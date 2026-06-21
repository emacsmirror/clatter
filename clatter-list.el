;;; clatter-list.el --- Channel list browser -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Glenn Thompson
;; Author: Glenn Thompson <glenn@paren.works>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Interactive channel list browser for clatter.el.
;; Sends LIST, accumulates RPL_LIST (322), and displays results
;; in a tabulated-list buffer with filtering and join-on-RET.

;;; Code:

(require 'cl-lib)
(require 'clatter-model)

;; --- State ---

(defvar clatter-list--entries nil
  "Accumulated channel list entries.
Each entry is (CHANNEL USERS TOPIC).")

(defvar clatter-list--conn nil
  "Connection associated with the current list request.")

(defvar clatter-list--filter ""
  "Current filter string for the channel list.")

(defvar clatter-list--message "LIST"
  "Message used to fetch the list; Defaults to LIST.")

;; --- Buffer-local state ---

(defvar-local clatter-list--local-entries nil
  "Channel list entries displayed by the list buffer.")

(defvar-local clatter-list--local-conn nil
  "Connection associated with the channel list buffer.")

(defvar-local clatter-list--local-filter nil
  "Filter string currently active within the channel list buffer.")

(defvar-local clatter-list--local-message nil
  "Message used to fetch the list displayed in the channel list buffer.")

;; --- Accumulation (called from handlers) ---

(defun clatter-list--on-entry (conn channel users topic)
  "Accumulate a LIST entry: CHANNEL with USERS and TOPIC on CONN."
  (when (eq conn clatter-list--conn)
    (push (list channel (string-to-number users) topic)
          clatter-list--entries)))

(defun clatter-list--on-end (conn)
  "LIST complete on CONN - display results."
  (when (eq conn clatter-list--conn)
    (clatter-list--display)))

;; --- Display ---

(defvar clatter-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'clatter-list-join)
    (define-key map (kbd "g") #'clatter-list-refresh)
    (define-key map (kbd "f") #'clatter-list-filter)
    (define-key map (kbd "/") #'clatter-list-filter)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `clatter-list-mode'.")

(define-derived-mode clatter-list-mode tabulated-list-mode "Clatter-List"
  "Major mode for browsing IRC channel lists."
  (setq tabulated-list-format
        [("Channel" 25 t)
         ("Users" 7 (lambda (a b) (> (string-to-number (aref (cadr a) 1))
                                     (string-to-number (aref (cadr b) 1)))))
         ("Topic" 0 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key '("Users" . t))
  (tabulated-list-init-header))

(defun clatter-list--display ()
  "Display the accumulated channel list in a buffer."
  (let* ((network (clatter-connection-network-id clatter-list--conn))
         (buf-name (format "*clatter-list: %s*" network))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (clatter-list-mode)
      ;; --- >> Set buffer local state ---
      (setq clatter-list--local-entries
            (sort (copy-sequence clatter-list--entries)
                  (lambda (a b) (> (nth 1 a) (nth 1 b)))))
      (setq clatter-list--local-conn clatter-list--conn)
      (setq clatter-list--local-filter clatter-list--filter)
      (setq clatter-list--local-message clatter-list--message)
      ;; --- << Set buffer local state ---
      (clatter-list--refresh-display)
      (goto-char (point-min)))
    (pop-to-buffer buf)
    (message "%d channels found. RET=join, f=filter, g=refresh"
             (length clatter-list--entries))))

(defun clatter-list--refresh-display ()
  "Refresh the tabulated list display with current filter."
  (let* ((entries (buffer-local-value 'clatter-list--local-entries
                                      (current-buffer)))
         (filter clatter-list--local-filter)
         (filtered (if (string-empty-p filter)
                       entries
                     (cl-remove-if-not
                      (lambda (e)
                        (or (string-match-p filter (nth 0 e))
                            (string-match-p filter (nth 2 e))))
                      entries))))
    (setq tabulated-list-entries
          (mapcar (lambda (e)
                    (list (nth 0 e)
                          (vector (nth 0 e)
                                  (number-to-string (nth 1 e))
                                  (nth 2 e))))
                  filtered))
    (tabulated-list-print t)))

;; --- Commands ---

(defun clatter-list-join ()
  "Join the channel at point."
  (interactive)
  (let ((channel (tabulated-list-get-id)))
    (when channel
      (let ((conn (buffer-local-value 'clatter-list--local-conn
                                      (current-buffer))))
        (when conn ;; When the current buffer is the buffer list
          (clatter-send conn (format "JOIN %s" channel))
          (message "Joining %s..." channel))))))

(defun clatter-list-refresh ()
  "Re-send LIST and refresh the channel list."
  (interactive)
  (let ((conn (buffer-local-value 'clatter-list--local-conn
                                  (current-buffer)))
        (message (buffer-local-value 'clatter-list--local-message
                                     (current-buffer))))
    (when (and conn message) ;; When the current buffer is the buffer list
      (setq clatter-list--entries nil) ;; Reset accumulator
      (setq clatter-list--filter (or clatter-list--local-filter ""))
      (clatter-send conn message)
      (message "Refreshing channel list..."))))

(defun clatter-list-filter ()
  "Filter the channel list by pattern."
  (interactive)
  ;; When the current buffer is the buffer list
  (when clatter-list--local-filter
    (setq clatter-list--local-filter
          (read-string "Filter (regexp): " clatter-list--local-filter))
    (clatter-list--refresh-display)
    (message "Showing %d channels" (length tabulated-list-entries))))

;; --- Entry point ---

(defun clatter-list-request (conn &optional arg)
  "Send LIST on CONN and prepare to display results; ARG filters server-side."
  (setq clatter-list--entries nil)
  (setq clatter-list--conn conn)
  (setq clatter-list--filter "")
  (setq clatter-list--message (if (or (null arg) (string-empty-p arg))
                                  "LIST"
                                (format "LIST %s" arg)))
  (clatter-send conn clatter-list--message)
  (message "Fetching channel list..."))

(provide 'clatter-list)

;;; clatter-list.el ends here
