;;; clatter-log.el --- Channel logging to file -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Glenn Thompson
;; Author: Glenn Thompson <glenn@paren.works>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Channel and query logging for clatter.el.
;; Writes plain-text logs to disk, organized by network and channel,
;; with optional daily rotation.  Comparable to ERC's erc-log module.

;;; Code:

(require 'cl-lib)
(require 'clatter-config)
(require 'clatter-connection)
(require 'clatter-protocol)
(require 'clatter-model)

;; --- Configuration ---

(defcustom clatter-log-enable nil
  "Enable channel logging to file."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-log-directory
  (expand-file-name "clatter/logs/" user-emacs-directory)
  "Directory for IRC log files.
Logs are stored as NETWORK/TARGET.log or NETWORK/TARGET-YYYY-MM-DD.log
depending on `clatter-log-rotate-daily'."
  :type 'directory
  :group 'clatter)

(defcustom clatter-log-rotate-daily t
  "If non-nil, create a new log file each day.
Files are named TARGET-YYYY-MM-DD.log.
If nil, all logs append to TARGET.log."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-log-timestamp-format "%Y-%m-%d %H:%M:%S"
  "Timestamp format for log entries.
See `format-time-string' for format specifiers."
  :type 'string
  :group 'clatter)

(defcustom clatter-log-system-messages t
  "If non-nil, log system messages (joins, parts, quits, etc)."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-log-server-buffer nil
  "If non-nil, also log the server buffer."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-log-exclude-targets nil
  "List of targets (channels/nicks) to exclude from logging.
Case-insensitive.  Example: (\"#spam\" \"#bots\")."
  :type '(repeat string)
  :group 'clatter)

(defcustom clatter-log-flush-interval 30
  "Seconds between automatic flushes of log buffers to disk.
Set to nil to write immediately (slower but no data loss)."
  :type '(choice integer (const nil))
  :group 'clatter)

;; --- Internal State ---

(defvar clatter-log--buffers (make-hash-table :test 'equal)
  "Hash table mapping (network . target) to log file buffer.")

(defvar clatter-log--flush-timer nil
  "Timer for periodic log flushing.")

;; --- File Paths ---

(defun clatter-log--sanitize-filename (name)
  "Sanitize NAME for use as a filename.
Replaces characters that are problematic on common filesystems."
  (replace-regexp-in-string "[/\\\\<>:\"|?*]" "_" name))

(defun clatter-log--file-path (network target &optional time)
  "Return the log file path for NETWORK and TARGET.
TIME is used for daily rotation (defaults to current time)."
  (let* ((net-dir (expand-file-name
                   (clatter-log--sanitize-filename network)
                   clatter-log-directory))
         (safe-target (clatter-log--sanitize-filename target))
         (filename (if clatter-log-rotate-daily
                       (format "%s-%s.log"
                               safe-target
                               (format-time-string "%Y-%m-%d" time))
                     (format "%s.log" safe-target))))
    (expand-file-name filename net-dir)))

;; --- Writing ---

(defun clatter-log--ensure-directory (file)
  "Ensure the directory for FILE exists."
  (let ((dir (file-name-directory file)))
    (unless (file-directory-p dir)
      (make-directory dir t))))

(defun clatter-log--write (network target text &optional time)
  "Write TEXT as a log line for NETWORK TARGET.
TIME overrides current time for the timestamp."
  (when (and clatter-log-enable
             (not (member (downcase target)
                          (mapcar #'downcase clatter-log-exclude-targets))))
    (let* ((ts (format-time-string clatter-log-timestamp-format time))
           (line (format "[%s] %s\n" ts text))
           (file (clatter-log--file-path network target time)))
      (clatter-log--ensure-directory file)
      (if clatter-log-flush-interval
          ;; Buffered: append to an internal buffer, flush periodically
          (clatter-log--buffer-append network target file line)
        ;; Immediate: write directly to file
        (clatter-log--append-to-file file line)))))

(defun clatter-log--append-to-file (file line)
  "Append LINE to FILE without visiting it."
  (let ((coding-system-for-write 'utf-8))
    (write-region line nil file t 'quiet)))

;; --- Buffered writing ---

(defun clatter-log--buffer-append (network target file line)
  "Append LINE to the in-memory buffer for NETWORK TARGET.
FILE is stored for flushing."
  (let* ((key (cons network (downcase target)))
         (entry (gethash key clatter-log--buffers)))
    (if entry
        (setcdr (cdr entry) (cons line (cddr entry)))
      (puthash key (list file 'lines line) clatter-log--buffers))))

(defun clatter-log-flush ()
  "Flush all buffered log entries to disk."
  (maphash
   (lambda (_key entry)
     (let ((file (car entry))
           (lines (cddr entry)))
       (when lines
         (clatter-log--ensure-directory file)
         (let ((text (apply #'concat (nreverse lines))))
           (clatter-log--append-to-file file text))
         ;; Clear the buffer
         (setcdr (cdr entry) nil))))
   clatter-log--buffers))

(defun clatter-log--start-flush-timer ()
  "Start the periodic flush timer."
  (when (and clatter-log-flush-interval
             (null clatter-log--flush-timer))
    (setq clatter-log--flush-timer
          (run-at-time clatter-log-flush-interval
                       clatter-log-flush-interval
                       #'clatter-log-flush))))

(defun clatter-log--stop-flush-timer ()
  "Stop the periodic flush timer."
  (when clatter-log--flush-timer
    (cancel-timer clatter-log--flush-timer)
    (setq clatter-log--flush-timer nil)))

;; --- Event Handlers ---

(defun clatter-log--on-privmsg (conn sender target text server-time)
  "Log PRIVMSG from SENDER to TARGET with TEXT on CONN."
  (let* ((network (clatter-connection-network-id conn))
         (sender-nick (clatter-prefix-nick sender))
         (my-nick (clatter-connection-nick conn))
         (log-target (if (clatter-channel-name-p target)
                         target
                       (if (string-equal target my-nick) sender-nick target))))
    (clatter-log--write network log-target
                         (format "<%s> %s" sender-nick text)
                         server-time)))

(defun clatter-log--on-action (conn sender target text server-time)
  "Log ACTION from SENDER to TARGET with TEXT on CONN."
  (let* ((network (clatter-connection-network-id conn))
         (my-nick (clatter-connection-nick conn))
         (sender-nick (clatter-prefix-nick sender))
         (log-target (if (clatter-channel-name-p target)
                         target
                       (if (string-equal target my-nick) sender-nick target))))
    (clatter-log--write network log-target
                        (format "* %s %s" sender-nick text)
                        server-time)))

(defun clatter-log--on-notice (conn sender target text)
  "Log NOTICE from SENDER to TARGET on CONN."
  (let* ((network (clatter-connection-network-id conn))
         (sender-nick (or (clatter-prefix-nick sender) "*"))
         (log-target (if (or (string= target "*")
                             (not (clatter-channel-name-p target)))
                         (if clatter-log-server-buffer "*server*" nil)
                       target)))
    (when log-target
      (clatter-log--write network log-target
                           (format "-%s- %s" sender-nick text)))))

(defun clatter-log--on-join (conn sender channel _account realname)
  "Log JOIN of SENDER to CHANNEL on CONN."
  (when clatter-log-system-messages
    (let ((sender-nick (clatter-prefix-nick sender)))
      (clatter-log--write (clatter-connection-network-id conn) channel
                          (if (and realname (not (string= sender-nick realname)))
                              (format "%s (%s) has joined %s" sender-nick realname channel)
                            (format "%s has joined %s" sender-nick channel))))))

(defun clatter-log--on-part (conn sender channel message)
  "Log PART of SENDER from CHANNEL on CONN."
  (when clatter-log-system-messages
    (let ((sender-nick (clatter-prefix-nick sender)))
      (clatter-log--write (clatter-connection-network-id conn) channel
                          (format "*** %s has left %s%s" sender-nick channel
                                  (if message (format " (%s)" message) ""))))))

(defun clatter-log--on-quit (conn sender message)
  "Log QUIT of SENDER on CONN."
  (when clatter-log-system-messages
    (let ((network (clatter-connection-network-id conn))
          (sender-nick (clatter-prefix-nick sender)))
      (dolist (buf (clatter-channel-buffers network))
        (when (gethash (downcase sender-nick)
                       (buffer-local-value 'clatter--nick-list buf))
          (clatter-log--write network
                              (buffer-local-value 'clatter--target buf)
                              (format "*** %s has quit%s" sender-nick
                                      (if message (format " (%s)" message) ""))))))))

(defun clatter-log--on-nick (conn sender new-nick)
  "Log SENDER changing their nick to NEW-NICK on CONN."
  (when clatter-log-system-messages
    (let ((network (clatter-connection-network-id conn))
          (sender-nick (clatter-prefix-nick sender)))
      (dolist (buf (clatter-channel-buffers network))
        (when (gethash (downcase sender-nick)
                       (buffer-local-value 'clatter--nick-list buf))
          (clatter-log--write network
                               (buffer-local-value 'clatter--target buf)
                               (format "*** %s is now known as %s"
                                       sender-nick new-nick)))))))

(defun clatter-log--on-topic (conn channel sender topic at)
  "Log TOPIC change in CHANNEL on CONN."
  (when clatter-log-system-messages
    (let ((sender-nick (clatter-prefix-nick sender)))
      (clatter-log--write (clatter-connection-network-id conn) channel
                          (let ((prefix "Topic"))
                            (cond
                             ((and sender-nick at)
                              (setq prefix (format "%s set at %s by %s"
                                                   prefix
                                                   (format-time-string "%F %T" at)
                                                   sender-nick)))
                             (sender-nick (setq prefix (format "%s set by %s" prefix sender-nick))))
                            (format "*** %s: %s" prefix topic))))))

(defun clatter-log--on-kick (conn channel sender kicked reason)
  "Log KICK of KICKED by SENDER in CHANNEL on CONN."
  (when clatter-log-system-messages
    (let ((sender-nick (clatter-prefix-nick sender)))
      (clatter-log--write (clatter-connection-network-id conn) channel
                          (format "*** %s was kicked by %s%s" kicked sender-nick
                                  (if reason (format " (%s)" reason) ""))))))

(defun clatter-log--on-mode (conn target setter modes)
  "Log MODE change on TARGET by SETTER on CONN."
  (when clatter-log-system-messages
    (let* ((network (clatter-connection-network-id conn))
           (setter-nick (clatter-prefix-nick setter))
           (log-target (if (clatter-channel-name-p target)
                           target
                         (if clatter-log-server-buffer "*server*" nil))))
      (when log-target
        (clatter-log--write network log-target
                             (format "*** %s sets mode %s"
                                     setter-nick (string-join modes " ")))))))

;; --- Hook Registration ---

(defun clatter-log-init ()
  "Register logging hooks and start flush timer.  Call after loading clatter."
  (add-hook 'clatter-privmsg-hook #'clatter-log--on-privmsg)
  (add-hook 'clatter-action-hook #'clatter-log--on-action)
  (add-hook 'clatter-notice-hook #'clatter-log--on-notice)
  (add-hook 'clatter-join-hook #'clatter-log--on-join)
  (add-hook 'clatter-part-hook #'clatter-log--on-part)
  (add-hook 'clatter-quit-hook #'clatter-log--on-quit)
  (add-hook 'clatter-nick-hook #'clatter-log--on-nick)
  (add-hook 'clatter-topic-hook #'clatter-log--on-topic)
  (add-hook 'clatter-kick-hook #'clatter-log--on-kick)
  (add-hook 'clatter-irc-mode-hook #'clatter-log--on-mode)
  ;; Flush any buffered lines when Emacs exits.
  (add-hook 'kill-emacs-hook #'clatter-log-flush)
  (clatter-log--start-flush-timer))

(defun clatter-log-teardown ()
  "Remove logging hooks and flush remaining data."
  (remove-hook 'clatter-privmsg-hook #'clatter-log--on-privmsg)
  (remove-hook 'clatter-action-hook #'clatter-log--on-action)
  (remove-hook 'clatter-notice-hook #'clatter-log--on-notice)
  (remove-hook 'clatter-join-hook #'clatter-log--on-join)
  (remove-hook 'clatter-part-hook #'clatter-log--on-part)
  (remove-hook 'clatter-quit-hook #'clatter-log--on-quit)
  (remove-hook 'clatter-nick-hook #'clatter-log--on-nick)
  (remove-hook 'clatter-topic-hook #'clatter-log--on-topic)
  (remove-hook 'clatter-kick-hook #'clatter-log--on-kick)
  (remove-hook 'clatter-irc-mode-hook #'clatter-log--on-mode)
  (remove-hook 'kill-emacs-hook #'clatter-log-flush)
  (clatter-log-flush)
  (clatter-log--stop-flush-timer)
  (clrhash clatter-log--buffers))

;; --- User Commands ---

(defun clatter-log-open ()
  "Open the log file for the current clatter buffer."
  (interactive)
  (if (and clatter--network clatter--target)
      (let ((file (clatter-log--file-path clatter--network clatter--target)))
        (if (file-exists-p file)
            (find-file-other-window file)
          (message "No log file yet for %s/%s" clatter--network clatter--target)))
    (message "Not in a clatter buffer")))

(defun clatter-log-open-directory ()
  "Open the log directory in Dired."
  (interactive)
  (if (file-directory-p clatter-log-directory)
      (dired clatter-log-directory)
    (message "Log directory does not exist yet: %s" clatter-log-directory)))

;; Logging is started by `clatter-setup' when `clatter-log-enable' is
;; non-nil.  `clatter-log-init' also installs the `kill-emacs-hook'
;; flush, so that merely loading this file has no side effects.

(provide 'clatter-log)

;;; clatter-log.el ends here
