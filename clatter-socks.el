;;; clatter-socks.el --- SOCKS5 proxy support for clatter.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Glenn Thompson
;; Author: Glenn Thompson <glenn@paren.works>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; SOCKS5 (RFC 1928) proxy support for clatter.el, with username/password
;; authentication (RFC 1929).  Provides pure codec functions and a session
;; based asynchronous handshake driver that runs over an already-open process.
;; The CONNECT request always uses ATYP=domain (remote DNS / SOCKS5h), so the
;; proxy resolves the target hostname; this avoids DNS leaks and supports
;; .onion addresses.

;;; Code:

(require 'cl-lib)
(require 'auth-source)
(require 'clatter-config)

;; --- Codec (pure) ---

(defun clatter-socks--encode-greeting (methods)
  "Encode a SOCKS5 client greeting offering auth METHODS (list of bytes)."
  (apply #'unibyte-string 5 (length methods) methods))

(defun clatter-socks--parse-method-selection (bytes)
  "Return the method byte from a method-selection reply BYTES, or nil if short."
  (when (>= (length bytes) 2)
    (aref bytes 1)))

(defun clatter-socks--encode-auth (user pass)
  "Encode an RFC 1929 username/password auth request for USER and PASS."
  (let ((u (encode-coding-string (or user "") 'utf-8))
        (p (encode-coding-string (or pass "") 'utf-8)))
    (when (or (> (length u) 255) (> (length p) 255))
      (error "SOCKS5: username or password too long (max 255 bytes)"))
    (concat (unibyte-string 1 (length u)) u
            (unibyte-string (length p)) p)))

(defun clatter-socks--parse-auth-status (bytes)
  "Return :ok or :fail from an auth status reply BYTES, or nil if short."
  (when (>= (length bytes) 2)
    (if (= (aref bytes 1) 0) :ok :fail)))

(defun clatter-socks--encode-connect (host port)
  "Encode a SOCKS5 CONNECT request to HOST:PORT using ATYP=domain."
  (let ((h (encode-coding-string host 'utf-8)))
    (when (> (length h) 255)
      (error "SOCKS5: hostname too long (%d > 255 bytes)" (length h)))
    (concat (unibyte-string 5 1 0 3 (length h))
            h
            (unibyte-string (logand (ash port -8) 255) (logand port 255)))))

(defun clatter-socks--reply-length (bytes)
  "Total expected length of a SOCKS5 reply in BYTES.
Return nil if not enough bytes yet to know, or `invalid' for a bad ATYP."
  (when (>= (length bytes) 4)
    (pcase (aref bytes 3)
      (1 (+ 4 4 2))                                  ; IPv4
      (4 (+ 4 16 2))                                 ; IPv6
      (3 (when (>= (length bytes) 5)                 ; domain: header+len+name+port
           (+ 4 1 (aref bytes 4) 2)))
      (_ 'invalid))))

(defconst clatter-socks--rep-messages
  '((0 . "succeeded")
    (1 . "general SOCKS server failure")
    (2 . "connection not allowed by ruleset")
    (3 . "network unreachable")
    (4 . "host unreachable")
    (5 . "connection refused")
    (6 . "TTL expired")
    (7 . "command not supported")
    (8 . "address type not supported"))
  "Map of SOCKS5 REP reply codes to human-readable messages.")

(defun clatter-socks--rep-message (code)
  "Human-readable message for SOCKS5 reply CODE."
  (or (cdr (assq code clatter-socks--rep-messages))
      (format "unknown reply code %d" code)))

;; --- Handshake session driver ---

(cl-defstruct (clatter-socks--session (:constructor clatter-socks--session-create))
  "State for one in-progress SOCKS5 handshake.
STATE is one of :method :auth :reply :done :failed.  BUFFER accumulates raw
input from the proxy.  SEND-FN is called with a unibyte string to transmit."
  state buffer host port user pass send-fn on-success on-failure)

(defun clatter-socks--consume (session n)
  "Drop the first N bytes from SESSION's input buffer."
  (setf (clatter-socks--session-buffer session)
        (substring (clatter-socks--session-buffer session) n)))

(defun clatter-socks--send (session bytes)
  "Transmit BYTES via SESSION's send function."
  (funcall (clatter-socks--session-send-fn session) bytes))

(defun clatter-socks--succeed (session)
  "Mark SESSION done and invoke its success continuation."
  (setf (clatter-socks--session-state session) :done)
  (funcall (clatter-socks--session-on-success session)))

(defun clatter-socks--fail (session reason)
  "Mark SESSION failed and invoke its failure continuation with REASON."
  (setf (clatter-socks--session-state session) :failed)
  (funcall (clatter-socks--session-on-failure session) reason))

(defun clatter-socks--send-auth (session)
  "Send the RFC 1929 auth request and move SESSION to :auth."
  (setf (clatter-socks--session-state session) :auth)
  (clatter-socks--send session
                       (clatter-socks--encode-auth
                        (clatter-socks--session-user session)
                        (clatter-socks--session-pass session))))

(defun clatter-socks--send-connect (session)
  "Send the CONNECT request, move SESSION to :reply, then drain the buffer."
  (setf (clatter-socks--session-state session) :reply)
  (clatter-socks--send session
                       (clatter-socks--encode-connect
                        (clatter-socks--session-host session)
                        (clatter-socks--session-port session)))
  (clatter-socks--advance session))

(defun clatter-socks--advance (session)
  "Advance SESSION's handshake using whatever input is buffered."
  (unless (memq (clatter-socks--session-state session) '(:done :failed))
    (let ((buf (clatter-socks--session-buffer session)))
      (pcase (clatter-socks--session-state session)
        (:method
         (let ((method (clatter-socks--parse-method-selection buf)))
           (when method
             (clatter-socks--consume session 2)
             (pcase method
               (0 (clatter-socks--send-connect session))
               (2 (clatter-socks--send-auth session))
               (_ (clatter-socks--fail
                   session
                   "no acceptable auth method (does the proxy require credentials?)"))))))
        (:auth
         (let ((status (clatter-socks--parse-auth-status buf)))
           (pcase status
             ('nil nil)
             (:ok (clatter-socks--consume session 2)
                  (clatter-socks--send-connect session))
             (_ (clatter-socks--consume session 2)
                (clatter-socks--fail session "proxy authentication failed")))))
        (:reply
         (let ((total (clatter-socks--reply-length buf)))
           (cond
            ((eq total 'invalid)
             (clatter-socks--fail session "malformed SOCKS reply (bad address type)"))
            ((null total) nil)
            ((< (length buf) total) nil)
            (t (let ((rep (aref buf 1)))
                 (if (= rep 0)
                     (clatter-socks--succeed session)
                   (clatter-socks--fail session (clatter-socks--rep-message rep))))))))))))

;; --- Proxy password ---

(defun clatter-socks--password (proxy)
  "Return the password for PROXY plist, from :pass or auth-source."
  (or (plist-get proxy :pass)
      (when (and clatter-use-auth-source (plist-get proxy :host))
        (let ((found (car (auth-source-search
                           :host (plist-get proxy :host)
                           :user (plist-get proxy :user)
                           :port (plist-get proxy :port)
                           :max 1))))
          (when found
            (let ((secret (plist-get found :secret)))
              (if (functionp secret) (funcall secret) secret)))))))

;; --- Process glue ---

(defun clatter-socks--filter (proc data)
  "Process filter driving the SOCKS5 handshake on PROC with incoming DATA."
  (when-let* ((session (process-get proc :socks-session)))
    (setf (clatter-socks--session-buffer session)
          (concat (clatter-socks--session-buffer session) data))
    (clatter-socks--advance session)))

(defun clatter-socks-begin (proc host port proxy on-success on-failure)
  "Begin a SOCKS5 handshake on open PROC to reach HOST:PORT through PROXY.
PROXY is a plist (:host :port [:user] [:pass]).  Call ON-SUCCESS (no args) once
the tunnel is established, or ON-FAILURE with a reason string on any error.
PROC must use binary coding for the duration of the handshake."
  (let* ((user (plist-get proxy :user))
         (auth (and user (> (length user) 0)))
         (pass (and auth (clatter-socks--password proxy)))
         (methods (if auth '(0 2) '(0)))
         (session (clatter-socks--session-create
                   :state :method :buffer (unibyte-string)
                   :host host :port port :user user :pass pass
                   :send-fn (lambda (s) (process-send-string proc s))
                   :on-success on-success :on-failure on-failure)))
    (process-put proc :socks-session session)
    (set-process-filter proc #'clatter-socks--filter)
    (clatter-socks--send session (clatter-socks--encode-greeting methods))))

(provide 'clatter-socks)

;;; clatter-socks.el ends here
