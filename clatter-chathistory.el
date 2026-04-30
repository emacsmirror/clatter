;;; clatter-chathistory.el --- IRCv3 CHATHISTORY support -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Glenn Thompson
;; Author: Glenn Thompson
;; License: MIT

;;; Commentary:

;; IRCv3 CHATHISTORY extension for clatter.el.
;; Fetches message backlog on join/reconnect.
;; Works with servers/bouncers that support:
;;   chathistory or draft/chathistory
;; Requires: server-time, batch, message-tags capabilities.

;;; Code:

(require 'cl-lib)
(require 'clatter-config)
(require 'clatter-connection)
(require 'clatter-model)

;; --- Configuration ---

(defcustom clatter-chathistory-enabled t
  "Enable automatic chathistory fetch on join."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-chathistory-limit 50
  "Maximum number of messages to fetch per target."
  :type 'integer
  :group 'clatter)

(defcustom clatter-chathistory-on-join t
  "Fetch chathistory automatically when joining a channel."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-chathistory-on-reconnect t
  "Fetch chathistory automatically on reconnection."
  :type 'boolean
  :group 'clatter)

;; --- State ---

(defvar-local clatter-chathistory--last-timestamp nil
  "Last known message timestamp for this buffer.
Used to request messages since this time on reconnect.")

;; --- Capability check ---

(defun clatter-chathistory--available-p (conn)
  "Return non-nil if CONN supports chathistory."
  (let ((caps (clatter-connection-cap-enabled conn)))
    (or (cl-member "chathistory" caps :test #'string-equal)
        (cl-member "draft/chathistory" caps :test #'string-equal))))

(defun clatter-chathistory--cap-name (conn)
  "Return the chathistory capability name supported by CONN."
  (let ((caps (clatter-connection-cap-enabled conn)))
    (cond
     ((cl-member "chathistory" caps :test #'string-equal) "chathistory")
     ((cl-member "draft/chathistory" caps :test #'string-equal) "draft/chathistory")
     (t nil))))

;; --- Request commands ---

(defun clatter-chathistory--format-time (time)
  "Format TIME as an IRCv3 server-time string (ISO 8601).
TIME is an Emacs time value."
  (format-time-string "%Y-%m-%dT%H:%M:%S.000Z" time t))

(defun clatter-chathistory-fetch-latest (conn target &optional limit)
  "Fetch the latest LIMIT messages for TARGET via CONN."
  (when (clatter-chathistory--available-p conn)
    (let ((n (or limit clatter-chathistory-limit)))
      (clatter-send conn
                    (format "CHATHISTORY LATEST %s * %d" target n)))))

(defun clatter-chathistory-fetch-before (conn target timestamp &optional limit)
  "Fetch LIMIT messages before TIMESTAMP for TARGET via CONN."
  (when (clatter-chathistory--available-p conn)
    (let ((n (or limit clatter-chathistory-limit))
          (ts (clatter-chathistory--format-time timestamp)))
      (clatter-send conn
                    (format "CHATHISTORY BEFORE %s timestamp=%s %d"
                            target ts n)))))

(defun clatter-chathistory-fetch-after (conn target timestamp &optional limit)
  "Fetch LIMIT messages after TIMESTAMP for TARGET via CONN."
  (when (clatter-chathistory--available-p conn)
    (let ((n (or limit clatter-chathistory-limit))
          (ts (clatter-chathistory--format-time timestamp)))
      (clatter-send conn
                    (format "CHATHISTORY AFTER %s timestamp=%s %d"
                            target ts n)))))

(defun clatter-chathistory-fetch-since (conn target timestamp &optional limit)
  "Fetch messages since TIMESTAMP for TARGET via CONN.
Used on reconnect to fill in gaps."
  (when (clatter-chathistory--available-p conn)
    (let ((n (or limit clatter-chathistory-limit))
          (ts (clatter-chathistory--format-time timestamp)))
      (clatter-send conn
                    (format "CHATHISTORY AFTER %s timestamp=%s %d"
                            target ts n)))))

;; --- Automatic fetch hooks ---

(defun clatter-chathistory--on-join (conn _channel target _account _realname)
  "Fetch chathistory when joining TARGET on CONN."
  (when (and clatter-chathistory-enabled
             clatter-chathistory-on-join
             (clatter-chathistory--available-p conn))
    (let* ((network (clatter-connection-network-id conn))
           (buf (clatter-get-buffer network target)))
      (if (and buf
               (buffer-local-value 'clatter-chathistory--last-timestamp buf))
          ;; Reconnect: fetch since last known message
          (when clatter-chathistory-on-reconnect
            (clatter-chathistory-fetch-since
             conn target
             (buffer-local-value 'clatter-chathistory--last-timestamp buf)))
        ;; First join: fetch latest
        (clatter-chathistory-fetch-latest conn target)))))

(defun clatter-chathistory--track-timestamp (_conn _sender _target _text server-time)
  "Track the latest message timestamp for chathistory gaps.
SERVER-TIME is the IRCv3 server-time value."
  (when server-time
    (setq-local clatter-chathistory--last-timestamp server-time)))

;; --- Interactive commands ---

(defun clatter-chathistory-request (&optional count)
  "Manually request chathistory for the current buffer.
COUNT defaults to `clatter-chathistory-limit'."
  (interactive "P")
  (let ((conn (clatter-get-connection clatter--network))
        (target clatter--target)
        (n (or count clatter-chathistory-limit)))
    (if (and conn target)
        (if (clatter-chathistory--available-p conn)
            (progn
              (clatter-chathistory-fetch-latest conn target n)
              (message "Requested %d messages for %s" n target))
          (message "Server does not support chathistory"))
      (message "Not in a clatter buffer"))))

(defun clatter-chathistory-more (&optional count)
  "Fetch older messages (before the earliest in this buffer)."
  (interactive "P")
  (let ((conn (clatter-get-connection clatter--network))
        (target clatter--target)
        (n (or count clatter-chathistory-limit)))
    (if (and conn target)
        (if (clatter-chathistory--available-p conn)
            (let ((earliest clatter-chathistory--last-timestamp))
              (if earliest
                  (progn
                    (clatter-chathistory-fetch-before conn target earliest n)
                    (message "Requested %d older messages for %s" n target))
                (clatter-chathistory-fetch-latest conn target n)))
          (message "Server does not support chathistory"))
      (message "Not in a clatter buffer"))))

;; --- Enable/disable ---

(defun clatter-chathistory-enable ()
  "Enable chathistory hooks."
  (interactive)
  (add-hook 'clatter-join-hook #'clatter-chathistory--on-join)
  (add-hook 'clatter-privmsg-hook #'clatter-chathistory--track-timestamp)
  (message "[clatter-chathistory] Enabled"))

(defun clatter-chathistory-disable ()
  "Disable chathistory hooks."
  (interactive)
  (remove-hook 'clatter-join-hook #'clatter-chathistory--on-join)
  (remove-hook 'clatter-privmsg-hook #'clatter-chathistory--track-timestamp)
  (message "[clatter-chathistory] Disabled"))

;; Auto-enable
(when clatter-chathistory-enabled
  (clatter-chathistory-enable))

(provide 'clatter-chathistory)

;;; clatter-chathistory.el ends here
