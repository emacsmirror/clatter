;;; clatter.el --- An IRCv3-compliant IRC client -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Glenn Thompson
;; Author: Glenn Thompson
;; License: MIT
;; URL: https://github.com/parenworks/clatter.el
;; Version: 0.3.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: comm, irc

;;; Commentary:

;; clatter.el is a dedicated, IRCv3-compliant IRC client for Emacs.
;; Pure Elisp, requiring only Emacs 30.1+ and curl (for async URL/image fetching).
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
;;        :autojoin ("#systemcrafters" "#commonlisp"))))
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

;; `clatter-disconnect' is defined in clatter-connection.el which properly
;; disables auto-reconnect before killing the process.

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

(defun clatter--on-kill-buffer ()
  "Close this clatter buffer."
  ;; Avoid infinite recursion
  (remove-hook 'kill-buffer-hook #'clatter--on-kill-buffer t)
  (cond
   ((not (boundp 'clatter--buffer-type)) nil)
   ((eq 'channel clatter--buffer-type)
    (clatter-cmd-close nil))
   ((and (eq 'server clatter--buffer-type)
         (boundp 'clatter--network)
         clatter--network)
    (clatter-disconnect clatter--network))))

(defun clatter--setup-kill-buffer-hook ()
  "Install hooks that clean-up killed clatter buffers."
  (add-hook 'kill-buffer-hook #'clatter--on-kill-buffer nil t))

(defun clatter--on-disconnect (network _event)
  "Remove dead network buffers from the buffer list."
  (let ((buf (clatter-get-server-buffer network)))
    (when (and buf (not (buffer-live-p buf)))
      (clatter-remove-buffer network "*server*"))))

(add-hook 'clatter-mode-hook #'clatter--setup-kill-buffer-hook)
(add-hook 'clatter-disconnect-hook #'clatter--on-disconnect)

(provide 'clatter)

;;; clatter.el ends here
