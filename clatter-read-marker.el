;;; clatter-read-marker.el --- IRCv3 read-marker support -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Glenn Thompson
;; Author: Glenn Thompson <glenn@paren.works>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; IRCv3 read-marker (MARKREAD) extension for clatter.el.
;; Syncs read position across clients via server.
;; When you read messages in one client, other clients see the marker.
;; Spec: https://ircv3.net/specs/extensions/read-marker

;;; Code:

(require 'cl-lib)
(require 'clatter-config)
(require 'clatter-connection)
(require 'clatter-model)

;; --- Configuration ---

(defcustom clatter-read-marker-enabled t
  "Enable read-marker sync with the server."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-read-marker-auto-send t
  "Automatically send MARKREAD when switching to a buffer.
If nil, only syncs when you explicitly call `clatter-read-marker-mark'."
  :type 'boolean
  :group 'clatter)

;; --- Faces ---

(defface clatter-read-marker-line
  '((t :strike-through "#7c7c7c" :extend t))
  "Face for the visual read-marker separator line."
  :group 'clatter)

;; --- State ---

(defvar-local clatter-read-marker--msgid nil
  "The msgid of the last read message in this buffer, per the server.")

(defvar-local clatter-read-marker--local-msgid nil
  "The msgid of the last message we've seen (local tracking).")

(defvar-local clatter-read-marker--overlay nil
  "Overlay for the visual read-marker line.")

;; --- Capability check ---

(defun clatter-read-marker--available-p (conn)
  "Return non-nil if CONN supports read-marker."
  (let ((caps (clatter-connection-cap-enabled conn)))
    (or (cl-member "read-marker" caps :test #'string-equal)
        (cl-member "draft/read-marker" caps :test #'string-equal))))

;; --- Send MARKREAD ---

(defun clatter-read-marker--send (conn target &optional msgid)
  "Send MARKREAD for TARGET on CONN, optionally with MSGID.
If MSGID is nil, queries the current read position."
  (when (clatter-read-marker--available-p conn)
    (if msgid
        (clatter-send conn (format "MARKREAD %s timestamp=%s" target msgid))
      (clatter-send conn (format "MARKREAD %s" target)))))

;; --- Handle incoming MARKREAD ---

(defun clatter-read-marker--handle (conn _tags params)
  "Handle incoming MARKREAD on CONN with PARAMS.
PARAMS format: (target timestamp=TIMESTAMP)"
  (let* ((target (nth 0 params))
         (ts-param (nth 1 params))
         (network (clatter-connection-network-id conn))
         (buf (clatter-get-buffer network target)))
    (when (and buf ts-param)
      (with-current-buffer buf
        (setq clatter-read-marker--msgid ts-param)
        (clatter-read-marker--update-visual)))))

;; --- Visual marker ---

(defun clatter-read-marker--update-visual ()
  "Update the visual read-marker overlay in the current buffer."
  (when clatter-read-marker--overlay
    (delete-overlay clatter-read-marker--overlay))
  ;; Place marker after the last read message
  ;; For now, place at the boundary between read and unread
  (when (and clatter-read-marker--msgid
             (> clatter--unread-count 0))
    (save-excursion
      (if (eq clatter-message-order 'oldest-first)
          ;; In oldest-first, unread messages are at the bottom
          (progn
            (goto-char (point-max))
            (forward-line (- clatter--unread-count)))
        ;; In newest-first, unread messages are at the top (after prompt)
        (goto-char (or clatter--messages-marker (point-min)))
        (forward-line clatter--unread-count))
      (let ((ov (make-overlay (line-beginning-position)
                              (1+ (line-beginning-position)))))
        (overlay-put ov 'before-string
                     (propertize (concat (make-string 40 ?\x2500) "\n")
                                 'face 'clatter-read-marker-line))
        (overlay-put ov 'clatter-read-marker t)
        (setq clatter-read-marker--overlay ov)))))

;; --- Auto-mark on buffer focus ---

(defun clatter-read-marker--on-buffer-switch ()
  "Send MARKREAD when switching to a clatter buffer."
  (when (and clatter-read-marker-enabled
             clatter-read-marker-auto-send
             (derived-mode-p 'clatter-mode)
             clatter--target
             clatter--network
             clatter-read-marker--local-msgid)
    (let ((conn (clatter-get-connection clatter--network)))
      (when (and conn (clatter-read-marker--available-p conn))
        (clatter-read-marker--send conn clatter--target
                                   clatter-read-marker--local-msgid)
        ;; Clear the visual marker since everything is now read
        (when clatter-read-marker--overlay
          (delete-overlay clatter-read-marker--overlay)
          (setq clatter-read-marker--overlay nil))))))

;; --- Track msgids ---

(defun clatter-read-marker--track-msgid (_conn _sender _target _text server-time)
  "Track the latest msgid for read-marker purposes.
Uses SERVER-TIME as a proxy when msgid tags are not available."
  (when server-time
    (setq-local clatter-read-marker--local-msgid
                (format-time-string "%Y-%m-%dT%H:%M:%S.000Z" server-time t))))

;; --- Interactive ---

(defun clatter-read-marker-mark ()
  "Manually mark the current position as read on the server."
  (interactive)
  (when (and clatter--network clatter--target)
    (let ((conn (clatter-get-connection clatter--network)))
      (if (and conn (clatter-read-marker--available-p conn))
          (progn
            (clatter-read-marker--send conn clatter--target
                                       clatter-read-marker--local-msgid)
            (message "Marked %s as read" clatter--target))
        (message "Read-marker not available")))))

(defun clatter-read-marker-query ()
  "Query the server for the current read position."
  (interactive)
  (when (and clatter--network clatter--target)
    (let ((conn (clatter-get-connection clatter--network)))
      (if (and conn (clatter-read-marker--available-p conn))
          (clatter-read-marker--send conn clatter--target)
        (message "Read-marker not available")))))

;; --- Enable/disable ---

(defun clatter-read-marker--window-change (&rest _)
  "Send MARKREAD when the selected window's buffer changes.
Ignores its arguments; suitable for `window-buffer-change-functions'."
  (clatter-read-marker--on-buffer-switch))

(defun clatter-read-marker-enable ()
  "Enable read-marker hooks."
  (interactive)
  (add-hook 'window-buffer-change-functions #'clatter-read-marker--window-change)
  (add-hook 'clatter-privmsg-hook #'clatter-read-marker--track-msgid)
  (when (called-interactively-p 'interactive)
    (message "[clatter-read-marker] Enabled")))

(defun clatter-read-marker-disable ()
  "Disable read-marker hooks."
  (interactive)
  (remove-hook 'window-buffer-change-functions #'clatter-read-marker--window-change)
  (remove-hook 'clatter-privmsg-hook #'clatter-read-marker--track-msgid)
  (when (called-interactively-p 'interactive)
    (message "[clatter-read-marker] Disabled")))

;; Enabled by `clatter-setup' when `clatter-read-marker-enabled' is
;; non-nil, so that merely loading this file has no side effects.

(provide 'clatter-read-marker)

;;; clatter-read-marker.el ends here
