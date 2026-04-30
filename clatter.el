;;; clatter.el --- An IRCv3 client for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Glenn Thompson
;; Author: Glenn Thompson
;; URL: https://src.paren.works/glenn/clatter.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: comm, irc

;;; Commentary:

;; clatter.el is a dedicated, IRCv3-compliant IRC client for Emacs.
;; Pure Elisp with no external dependencies beyond Emacs 29.1+.
;;
;; Spiritual successor to CLatter (Common Lisp TUI IRC client),
;; redesigned from scratch for Emacs.
;;
;; Features:
;; - Full IRC protocol (RFC 1459/2812)
;; - IRCv3 capability negotiation (CAP LS 302)
;; - SASL authentication (PLAIN and EXTERNAL)
;; - Message tags, batch, labeled-response, chathistory
;; - MONITOR, typing indicators, CTCP
;; - TLS/SSL with client certificate support
;; - Buffer-per-channel with nick colorization
;; - auth-source integration for passwords
;; - Reconnection with exponential backoff
;;
;; Quick start:
;;
;;   (require 'clatter)
;;   (setq clatter-networks
;;     '(("libera"
;;        :server "irc.libera.chat"
;;        :port 6697
;;        :tls t
;;        :nick "yournick"
;;        :sasl plain
;;        :autojoin ("#emacs" "#commonlisp"))))
;;   (clatter-connect "libera")

;;; Code:

(require 'clatter-config)
(require 'clatter-protocol)
(require 'clatter-connection)
(require 'clatter-cap)
(require 'clatter-handlers)
(require 'clatter-model)
(require 'clatter-ui)
(require 'clatter-commands)
(require 'clatter-actions)
(require 'clatter-track)
(require 'clatter-notify)
(require 'clatter-completion)
(require 'clatter-rawlog)
(require 'clatter-chathistory)
(require 'clatter-read-marker)
(require 'clatter-nicklist)
(require 'clatter-log)
(require 'clatter-url-preview)

;; --- Autoload entry points ---

;;;###autoload
(defun clatter (network)
  "Connect to IRC NETWORK.
NETWORK should be the name of a network defined in `clatter-networks'.
Interactively, prompts with completion."
  (interactive
   (list (completing-read "Connect to network: "
                          (mapcar #'car clatter-networks)
                          nil t)))
  (clatter-connect network))

;;;###autoload
(defun clatter-quick-connect (server nick &optional port tls)
  "Quickly connect to SERVER as NICK.
PORT defaults to 6697, TLS defaults to t.
Creates a transient network entry."
  (interactive
   (list (read-string "Server: ")
         (read-string "Nick: " (user-login-name))
         (read-number "Port: " 6697)
         (y-or-n-p "Use TLS? ")))
  (let ((network-id (format "quick-%s" server)))
    (clatter-connect network-id
                     :server server
                     :nick nick
                     :port (or port 6697)
                     :tls (if (null tls) t tls))))

;; --- Disconnect ---

;;;###autoload
(defun clatter-disconnect (network &optional quit-message)
  "Disconnect from IRC NETWORK with optional QUIT-MESSAGE."
  (interactive
   (list (completing-read "Disconnect from: "
                          (let (ids)
                            (maphash (lambda (id _conn) (push id ids))
                                     clatter-connections)
                            ids)
                          nil t)
         (read-string "Quit message (empty for default): " nil nil "CLatter")))
  (let ((conn (clatter-get-connection network)))
    (when conn
      (clatter-send conn (format "QUIT :%s" (or quit-message "CLatter")))
      (when (clatter-connection-process conn)
        (delete-process (clatter-connection-process conn)))
      (setf (clatter-connection-state conn) :disconnected)
      (message "[clatter] Disconnected from %s" network))))

;; --- Status ---

(defun clatter-status ()
  "Display status of all connections."
  (interactive)
  (let ((lines nil))
    (maphash (lambda (id conn)
               (push (format "%-20s %-15s nick: %s  caps: %s"
                             id
                             (clatter-connection-state conn)
                             (or (clatter-connection-nick conn) "-")
                             (length (clatter-connection-cap-enabled conn)))
                     lines))
             clatter-connections)
    (if lines
        (message "clatter connections:\n%s" (string-join (nreverse lines) "\n"))
      (message "No clatter connections"))))

(provide 'clatter)

;;; clatter.el ends here
