;;; clatter-actions.el --- Message actions at point -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Glenn Thompson
;; Author: Glenn Thompson <glenn@paren.works>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Operate on IRC messages at point in clatter buffers.
;; Provides a context-aware action menu with reply, copy,
;; inspect, nick operations, URL handling, and more.

;;; Code:

(require 'cl-lib)
(require 'clatter-config)
(require 'clatter-connection)
(require 'clatter-protocol)
(require 'clatter-model)
(require 'clatter-ui)

;; --- Get message properties at point ---

(defun clatter-action--msg-at-point ()
  "Return plist of message properties at point, or nil."
  (let ((type (get-text-property (point) 'clatter-msg-type))
        (sender (get-text-property (point) 'clatter-sender))
        (text (get-text-property (point) 'clatter-text))
        (url (get-text-property (point) 'clatter-url))
        (nick (get-text-property (point) 'clatter-nick)))
    (when (or type sender url nick)
      (list :type type :sender sender :text text :url url :nick nick))))

(defun clatter-action--line-text ()
  "Return the visible text of the current message line."
  (buffer-substring-no-properties
   (line-beginning-position) (line-end-position)))

;; --- Actions ---

(defun clatter-select-message ()
  "Select the message at point."
  (interactive)
  (when-let* ((msgid (get-text-property (point) 'clatter-msgid))
              (begin (previous-single-property-change (point) 'clatter-msgid))
              (end (next-single-property-change (point) 'clatter-msgid)))
    ;; Ensure the message marker is outside the boundaries of the region,
    ;; so that newly-added messages are not included in the selection.
    (when (and (= begin clatter--messages-marker)
               (< begin (point-max)))
      (setq begin (1+ begin)))
    (when (and (= end clatter--messages-marker)
               (> end (point-min)))
      (setq end (1- end)))
    ;; Create the region and convert it into a secondary selection.
    (prog1 msgid
      (save-mark-and-excursion
        (goto-char begin)
        (set-mark end)
        (activate-mark)
        (secondary-selection-from-region)))))

(defun clatter-action-reply (&optional arg)
  "Reply to the message at point.
Inserts the sender's nick at the input prompt.
With a prefix argument ARG, uses a /reply command."
  (interactive "P")
  (let* ((props (clatter-action--msg-at-point))
         (sender (plist-get props :sender)))
    (if (and sender (or (not arg) (clatter-select-message)))
        (progn
          (goto-char clatter--input-marker)
          (goto-char (save-excursion
                       (goto-char clatter--input-marker)
                       (line-end-position)))
          (let ((inhibit-read-only t))
            (if arg (insert "/reply " sender ": ") (insert sender ": "))))
      (message "No message at point"))))

(defun clatter-action-react ()
  "React to the message at point."
  (interactive)
  (if (clatter-select-message)
      (progn
        (goto-char clatter--input-marker)
        (goto-char (save-excursion
                     (goto-char clatter--input-marker)
                     (line-end-position)))
        (let ((inhibit-read-only t))
          (insert "/react ")))
    (message "No message at point")))

(defun clatter-action-copy-message ()
  "Copy the message text at point to kill ring."
  (interactive)
  (let* ((props (clatter-action--msg-at-point))
         (text (plist-get props :text)))
    (if text
        (progn
          (kill-new text)
          (message "Copied: %s" (truncate-string-to-width text 60)))
      (let ((line (clatter-action--line-text)))
        (kill-new line)
        (message "Copied line")))))

(defun clatter-action-copy-nick ()
  "Copy the sender's nick at point to kill ring."
  (interactive)
  (let* ((props (clatter-action--msg-at-point))
         (sender (or (plist-get props :sender)
                     (plist-get props :nick))))
    (if sender
        (progn
          (kill-new sender)
          (message "Copied nick: %s" sender))
      (message "No nick at point"))))

(defun clatter-action-copy-url ()
  "Copy the URL at point to kill ring."
  (interactive)
  (let* ((props (clatter-action--msg-at-point))
         (url (plist-get props :url)))
    (if url
        (progn
          (kill-new url)
          (message "Copied URL: %s" url))
      (message "No URL at point"))))

(defun clatter-action-open-url ()
  "Open the URL at point in browser."
  (interactive)
  (let* ((props (clatter-action--msg-at-point))
         (url (plist-get props :url)))
    (if url
        (browse-url url)
      (message "No URL at point"))))

(defun clatter-action-whois ()
  "WHOIS the sender of the message at point."
  (interactive)
  (let* ((props (clatter-action--msg-at-point))
         (sender (or (plist-get props :sender)
                     (plist-get props :nick)))
         (conn (clatter-get-connection clatter--network)))
    (if (and sender conn)
        (clatter-send conn (format "WHOIS %s" sender))
      (message "No nick at point"))))

(defun clatter-action-query ()
  "Open a private message buffer with the sender at point."
  (interactive)
  (let* ((props (clatter-action--msg-at-point))
         (sender (or (plist-get props :sender)
                     (plist-get props :nick))))
    (if sender
        (let ((buf (clatter-get-or-create-buffer clatter--network sender)))
          (switch-to-buffer buf))
      (message "No nick at point"))))

(defun clatter-action-ignore ()
  "Toggle ignore on the sender at point."
  (interactive)
  (let* ((props (clatter-action--msg-at-point))
         (sender (or (plist-get props :sender)
                     (plist-get props :nick))))
    (if sender
        (if (member (downcase sender) (mapcar #'downcase clatter-ignore-list))
            (progn
              (setq clatter-ignore-list
                    (cl-remove sender clatter-ignore-list
                               :test #'string-equal-ignore-case))
              (message "Unignored %s" sender))
          (push sender clatter-ignore-list)
          (message "Ignoring %s (messages hidden)" sender))
      (message "No nick at point"))))

(defun clatter-action-inspect ()
  "Show raw message properties at point."
  (interactive)
  (let ((props (clatter-action--msg-at-point))
        (all-props (text-properties-at (point))))
    (if props
        (with-current-buffer (get-buffer-create "*clatter-inspect*")
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert "Message Properties at Point\n")
            (insert (make-string 40 ?-) "\n\n")
            (let ((type (plist-get props :type))
                  (sender (plist-get props :sender))
                  (text (plist-get props :text))
                  (url (plist-get props :url))
                  (nick (plist-get props :nick)))
              (when type (insert (format "Type:   %s\n" type)))
              (when sender (insert (format "Sender: %s\n" sender)))
              (when nick (insert (format "Nick:   %s\n" nick)))
              (when url (insert (format "URL:    %s\n" url)))
              (when text (insert (format "\nText:\n%s\n" text))))
            (insert (format "\nAll text properties:\n%S\n" all-props))
            (goto-char (point-min))
            (special-mode))
          (display-buffer (current-buffer)))
      (message "No message properties at point"))))

(defun clatter-action-collect-urls ()
  "Collect all URLs in the current buffer and display them."
  (interactive)
  (let ((urls nil))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((url (get-text-property (point) 'clatter-url)))
          (when (and url (not (member url urls)))
            (push url urls)))
        (goto-char (or (next-single-property-change (point) 'clatter-url)
                       (point-max)))))
    (if urls
        (with-current-buffer (get-buffer-create "*clatter-urls*")
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (format "URLs from %s\n" (buffer-name)))
            (insert (make-string 40 ?-) "\n\n")
            (dolist (url (nreverse urls))
              (insert (propertize url
                                  'face '(:foreground "#89ddff" :underline t)
                                  'mouse-face 'highlight
                                  'clatter-url url)
                      "\n"))
            (goto-char (point-min))
            (special-mode))
          (display-buffer (current-buffer)))
      (message "No URLs found"))))

;; --- Action menu ---

(defvar clatter-action-map
  (let ((map (make-sparse-keymap "Clatter Actions")))
    (define-key map (kbd "r") #'clatter-action-reply)
    (define-key map (kbd "e") #'clatter-action-react)
    (define-key map (kbd "c") #'clatter-action-copy-message)
    (define-key map (kbd "n") #'clatter-action-copy-nick)
    (define-key map (kbd "u") #'clatter-action-copy-url)
    (define-key map (kbd "o") #'clatter-action-open-url)
    (define-key map (kbd "w") #'clatter-action-whois)
    (define-key map (kbd "q") #'clatter-action-query)
    (define-key map (kbd "i") #'clatter-action-inspect)
    (define-key map (kbd "I") #'clatter-action-ignore)
    (define-key map (kbd "l") #'clatter-action-collect-urls)
    map)
  "Keymap for message actions at point.")

(defun clatter-action-menu ()
  "Show the message action menu for the message at point.
Key bindings:
  r  Reply (insert nick at prompt)
  e  React (insert /react at prompt)
  c  Copy message text
  n  Copy nick
  u  Copy URL at point
  o  Open URL at point
  w  WHOIS sender
  q  Open query/DM with sender
  i  Inspect raw message
  I  Toggle ignore on sender
  l  List all URLs in buffer"
  (interactive)
  (let* ((props (clatter-action--msg-at-point))
         (sender (plist-get props :sender))
         (url (get-text-property (point) 'clatter-url))
         (parts nil))
    (push "[r]eply" parts)
    (push "r[e]act" parts)
    (push "[c]opy msg" parts)
    (when sender (push (format "[n]ick(%s)" sender) parts))
    (when url (push "[u]rl copy" parts))
    (when url (push "[o]pen url" parts))
    (when sender (push "[w]hois" parts))
    (when sender (push "[q]uery" parts))
    (push "[i]nspect" parts)
    (when sender (push "[I]gnore" parts))
    (push "[l]ist urls" parts)
    (message "%s" (string-join (nreverse parts) " "))
    (set-transient-map clatter-action-map)))

;; --- Bind into clatter-mode ---

(defun clatter-actions-setup-keys ()
  "Add action keybindings to `clatter-mode-map'."
  (define-key clatter-mode-map (kbd "C-c C-a") #'clatter-action-menu)
  (define-key clatter-mode-map (kbd "C-c C-r") #'clatter-action-reply)
  (define-key clatter-mode-map (kbd "C-c C-e") #'clatter-action-react)
  (define-key clatter-mode-map (kbd "C-c C-u") #'clatter-action-collect-urls)
  (define-key clatter-mode-map (kbd "C-c C-w") #'clatter-action-whois))

(clatter-actions-setup-keys)

(provide 'clatter-actions)

;;; clatter-actions.el ends here
