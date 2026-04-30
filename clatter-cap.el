;;; clatter-cap.el --- IRCv3 CAP negotiation and SASL -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Glenn Thompson
;; Author: Glenn Thompson
;; License: MIT

;;; Commentary:

;; IRCv3 capability negotiation and SASL authentication for clatter.el.
;; Ported from CLatter's net/irc.lisp CAP handling.

;;; Code:

(require 'cl-lib)
(require 'clatter-config)
(require 'clatter-protocol)
(require 'clatter-connection)
(require 'clatter-sasl-scram)
(require 'clatter-sts)

;; --- CAP LS / REQ / ACK / NAK ---

(defun clatter-cap--parse-list (caps-string)
  "Parse a space-separated capability list from CAPS-STRING.
Strips =value suffixes (e.g., \"sasl=PLAIN,EXTERNAL\" -> \"sasl\")."
  (when caps-string
    (mapcar (lambda (cap)
              (let ((eq-pos (cl-position ?= cap)))
                (if eq-pos (substring cap 0 eq-pos) cap)))
            (split-string caps-string))))

(defun clatter-cap--find-matching (available wanted)
  "Find capabilities from WANTED that appear in AVAILABLE."
  (cl-remove-if-not (lambda (w)
                      (cl-member w available :test #'string-equal))
                    wanted))

(defun clatter-cap-handle (conn params)
  "Handle a CAP response on CONN with PARAMS.
Dispatches based on subcommand (LS, ACK, NAK)."
  (let* ((subcommand (nth 1 params))
         ;; Handle multi-line CAP LS (second param is *)
         (is-multiline (string= (nth 1 params) "*"))
         (caps-string (if is-multiline (nth 3 params) (nth 2 params))))
    (cond
     ;; CAP LS - server lists available capabilities
     ((or (string-equal subcommand "LS")
          (and is-multiline (string-equal (nth 2 params) "LS")))
      (clatter-cap--handle-ls conn caps-string))

     ;; CAP ACK - capabilities accepted
     ((string-equal subcommand "ACK")
      (clatter-cap--handle-ack conn caps-string))

     ;; CAP NAK - capabilities rejected
     ((string-equal subcommand "NAK")
      (clatter-cap--handle-nak conn caps-string)))))

(defun clatter-cap--handle-ls (conn caps-string)
  "Handle CAP LS response on CONN with CAPS-STRING."
  ;; Check STS before processing capabilities
  (let* ((config (process-get (clatter-connection-process conn) :clatter-config))
         (hostname (plist-get config :server))
         (is-tls (plist-get config :tls))
         (sts-result (clatter-sts-check-cap caps-string hostname is-tls)))
    (when (and sts-result (eq (plist-get sts-result :action) 'upgrade))
      ;; Must disconnect and reconnect with TLS
      (let ((new-port (plist-get sts-result :port)))
        (clatter--debug "STS: upgrading to TLS on port %d" new-port)
        (clatter-send conn "CAP END")
        ;; Schedule reconnect with TLS
        (run-at-time 0.1 nil
                     (lambda ()
                       (clatter-connect
                        (clatter-connection-network-id conn)
                        :server hostname
                        :port new-port
                        :tls t
                        :nick (clatter-connection-nick conn))))
        (delete-process (clatter-connection-process conn))
        (cl-return-from clatter-cap--handle-ls nil))))
  (let* ((available (clatter-cap--parse-list caps-string))
         (config (process-get (clatter-connection-process conn) :clatter-config))
         (sasl-type (plist-get config :sasl))
         (client-cert (plist-get config :client-cert))
         (want-sasl-external (and (eq sasl-type 'external)
                                   client-cert
                                   (file-exists-p client-cert)))
         (want-sasl-plain (and (memq sasl-type '(plain scram-sha-256))
                                (clatter-get-password
                                 (clatter-connection-network-id conn))))
         (caps-to-request (clatter-cap--find-matching
                           available clatter-wanted-capabilities)))
    ;; Add SASL if wanted and available
    (when (and (or want-sasl-external want-sasl-plain)
               (cl-member "sasl" available :test #'string-equal))
      (push "sasl" caps-to-request)
      (setf (clatter-connection-sasl-state conn) :requested))
    (if caps-to-request
        (progn
          (clatter--debug "Requesting caps: %s" (string-join caps-to-request ", "))
          (clatter-send conn (format "CAP REQ :%s"
                                     (string-join caps-to-request " "))))
      (progn
        (clatter--debug "No IRCv3 capabilities to request")
        (clatter-send conn "CAP END")
        (clatter-cap--send-registration conn)))))

(defun clatter-cap--handle-ack (conn caps-string)
  "Handle CAP ACK response on CONN with CAPS-STRING."
  (let* ((acked (clatter-cap--parse-list caps-string))
         (config (process-get (clatter-connection-process conn) :clatter-config))
         (sasl-type (plist-get config :sasl)))
    (setf (clatter-connection-cap-enabled conn)
          (append (clatter-connection-cap-enabled conn) acked))
    (clatter--debug "Enabled caps: %s" (string-join acked ", "))
    ;; If SASL was ACKed, start authentication
    (if (cl-member "sasl" acked :test #'string-equal)
        (cond
         ;; SASL EXTERNAL
         ((eq sasl-type 'external)
          (clatter--debug "Starting SASL EXTERNAL")
          (setf (clatter-connection-sasl-state conn) :authenticating)
          (clatter-send conn "AUTHENTICATE EXTERNAL"))
         ;; SASL SCRAM-SHA-256
         ((eq sasl-type 'scram-sha-256)
          (clatter--debug "Starting SASL SCRAM-SHA-256")
          (setf (clatter-connection-sasl-state conn) :authenticating)
          (clatter-send conn "AUTHENTICATE SCRAM-SHA-256"))
         ;; SASL PLAIN
         ((eq sasl-type 'plain)
          (setf (clatter-connection-sasl-state conn) :authenticating)
          (clatter-send conn "AUTHENTICATE PLAIN"))
         ;; Fallback
         (t
          (clatter-send conn "CAP END")
          (clatter-cap--send-registration conn)))
      ;; No SASL - finish CAP and register
      (progn
        (clatter-send conn "CAP END")
        (clatter-cap--send-registration conn)))))

(defun clatter-cap--handle-nak (conn caps-string)
  "Handle CAP NAK response on CONN with CAPS-STRING."
  (clatter--debug "Capabilities rejected: %s" caps-string)
  (clatter-send conn "CAP END")
  (clatter-cap--send-registration conn))

;; --- SASL Authentication ---

(defun clatter-cap-handle-authenticate (conn params)
  "Handle AUTHENTICATE response on CONN with PARAMS.
For PLAIN: sends base64-encoded credentials.
For EXTERNAL: sends + (empty, cert already presented at TLS).
For SCRAM-SHA-256: multi-step challenge-response."
  (let* ((response (car params))
         (config (process-get (clatter-connection-process conn) :clatter-config))
         (sasl-type (plist-get config :sasl)))
    (cond
     ;; EXTERNAL
     ((and (string= response "+") (eq sasl-type 'external))
      (clatter-send conn "AUTHENTICATE +"))
     ;; SCRAM-SHA-256: server ready for client-first
     ((and (string= response "+") (eq sasl-type 'scram-sha-256))
      (clatter-cap--scram-client-first conn))
     ;; SCRAM-SHA-256: server-first or server-final response
     ((and (not (string= response "+")) (eq sasl-type 'scram-sha-256))
      (clatter-cap--scram-continue conn response))
     ;; PLAIN
     ((and (string= response "+") (eq sasl-type 'plain))
      (clatter-cap--sasl-plain-authenticate conn)))))

(defun clatter-cap--scram-client-first (conn)
  "Send SCRAM-SHA-256 client-first-message on CONN."
  (let* ((nick (clatter-connection-nick conn))
         (password (clatter-get-password (clatter-connection-network-id conn)))
         (result (clatter-scram-client-first nick password))
         (state (car result))
         (message-b64 (cdr result)))
    ;; Store SCRAM state on the connection process
    (process-put (clatter-connection-process conn) :scram-state state)
    (clatter-send conn (format "AUTHENTICATE %s" message-b64))))

(defun clatter-cap--scram-continue (conn server-response)
  "Handle SCRAM server response on CONN.
SERVER-RESPONSE is base64-encoded server message."
  (let ((state (process-get (clatter-connection-process conn) :scram-state)))
    (if (clatter-scram-state-auth-message state)
        ;; We already sent client-final, this is server-final
        (if (clatter-scram-verify-server state server-response)
            (clatter--debug "SCRAM: server signature verified")
          (clatter--debug "SCRAM: server signature INVALID (possible MITM)"))
      ;; This is server-first, send client-final
      (let ((client-final (clatter-scram-client-final state server-response)))
        (clatter-send conn (format "AUTHENTICATE %s" client-final))))))

(defun clatter-cap--sasl-plain-authenticate (conn)
  "Send SASL PLAIN authentication on CONN."
  (let* ((network-id (clatter-connection-network-id conn))
         (nick (clatter-connection-nick conn))
         (password (clatter-get-password network-id))
         ;; SASL PLAIN format: \0username\0password
         (auth-string (format "%c%s%c%s" 0 nick 0 password))
         (encoded (base64-encode-string auth-string t)))
    (setf (clatter-connection-sasl-state conn) :authenticating)
    (clatter-send conn (format "AUTHENTICATE %s" encoded))))

(defun clatter-cap-handle-sasl-success (conn)
  "Handle 903 RPL_SASLSUCCESS on CONN."
  (setf (clatter-connection-sasl-state conn) :done)
  (clatter--debug "SASL authentication successful")
  (clatter-send conn "CAP END")
  (clatter-cap--send-registration conn))

(defun clatter-cap-handle-sasl-failure (conn params)
  "Handle 904/905 SASL failure on CONN with PARAMS."
  (clatter--debug "SASL authentication failed: %s" (nth 1 params))
  (clatter-send conn "CAP END")
  (clatter-cap--send-registration conn))

;; --- Registration (NICK/USER/PASS) ---

(defun clatter-cap--send-registration (conn)
  "Send NICK and USER registration commands on CONN."
  (let* ((config (process-get (clatter-connection-process conn) :clatter-config))
         (nick (clatter-connection-nick conn))
         (username (or (plist-get config :username) nick))
         (realname (or (plist-get config :realname) clatter-default-realname))
         (password (plist-get config :password)))
    ;; Server password (PASS) must come before NICK/USER
    (when password
      (clatter-send conn (clatter-irc-pass password)))
    (clatter-send conn (clatter-irc-nick nick))
    (clatter-send conn (clatter-irc-user username realname))))

(provide 'clatter-cap)

;;; clatter-cap.el ends here
