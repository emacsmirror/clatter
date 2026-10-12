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

(defvar clatter-action--menu-active nil
  "Non-nil while dispatching an action at the menu's captured position.")

(defun clatter-action--target-position ()
  "Return the message position at point, or under the mouse in this buffer.
A message without a server ID still takes precedence over the mouse."
  (or (and (or (get-text-property (point) 'clatter-message-generation)
               (get-text-property (point) 'clatter-msg-type)
               (get-text-property (point) 'clatter-msgid))
           (point))
      (unless clatter-action--menu-active
        (let* ((mouse (mouse-pixel-position))
               (frame (car mouse))
               (x (cadr mouse))
               (y (cddr mouse)))
          (when (and (frame-live-p frame) (numberp x) (numberp y))
            (let* ((hit (posn-at-x-y x y frame))
                   (window (and hit (posn-window hit)))
                   (position (and hit (posn-point hit))))
              (when (and (window-live-p window)
                         (eq (window-buffer window) (current-buffer))
                         (integerp position)
                         (<= (point-min) position) (< position (point-max))
                         (or (get-text-property position 'clatter-message-generation)
                             (get-text-property position 'clatter-msg-type)
                             (get-text-property position 'clatter-msgid)))
                position)))))))

(defun clatter-action--line-text ()
  "Return the visible text of the current message line."
  (buffer-substring-no-properties
   (line-beginning-position) (line-end-position)))

;; --- Actions ---

(defun clatter-select-message ()
  "Select the message at point."
  (interactive)
  (when-let* ((msgid (get-text-property (point) 'clatter-msgid))
              (begin (previous-single-property-change
                      (min (1+ (point)) (point-max))
                      'clatter-msgid nil (point-min)))
              (end (next-single-property-change
                    (point) 'clatter-msgid nil (point-max))))
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
  "Reply to the message at point, falling back to the mouse in this chat.
Uses a threaded reply when `message-tags' and a message ID are available.
With a prefix argument ARG, requires a threaded reply."
  (interactive "P")
  (let* ((position (clatter-action--target-position))
         (sender (and position
                      (get-text-property position 'clatter-sender)))
         (conn (and clatter--network
                    (clatter-get-connection clatter--network)))
         (threaded-p
          (or arg
              (and conn
                   (member "message-tags"
                           (clatter-connection-cap-enabled conn))
                   position
                   (get-text-property position 'clatter-msgid)))))
    (if (and sender
             (or (not threaded-p)
                 (save-excursion
                   (goto-char position)
                   (clatter-select-message))))
        (progn
          (goto-char clatter--input-marker)
          (goto-char (save-excursion
                       (goto-char clatter--input-marker)
                       (line-end-position)))
          (let ((inhibit-read-only t))
            (if threaded-p
                (insert "/reply " sender ": ")
              (insert sender ": "))))
      (message "No message at point"))))

(defun clatter-action-react ()
  "React to the message at point, falling back to the mouse in this chat.
The message must have a server-provided message ID."
  (interactive)
  (let ((position (clatter-action--target-position)))
    (cond
     ((not position)
      (message "No message at point"))
     ((not (get-text-property position 'clatter-msgid))
      (message "Message has no server-provided message ID"))
     ((save-excursion
        (goto-char position)
        (clatter-select-message))
      (goto-char clatter--input-marker)
      (goto-char (save-excursion
                   (goto-char clatter--input-marker)
                   (line-end-position)))
      (let ((inhibit-read-only t))
        (insert "/react "))))))

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
                                  'face 'link
                                  'mouse-face 'highlight
                                  'clatter-url url)
                      "\n"))
            (goto-char (point-min))
            (special-mode))
          (display-buffer (current-buffer)))
      (message "No URLs found"))))

;; --- Action menu ---

(defun clatter-action-menu ()
  "Show actions for the message at point or under the mouse in this chat.
Keep that target while waiting for a choice; do not retarget a removed row."
  (interactive)
  (let* ((buffer (current-buffer))
         (target (clatter-action--target-position))
         (marker (copy-marker (or target (point)) t))
         (generation (get-text-property marker 'clatter-message-generation))
         (msgid (get-text-property marker 'clatter-msgid))
         input-position)
    (unwind-protect
        (save-excursion
          (let* ((props (save-excursion
                          (goto-char marker)
                          (clatter-action--msg-at-point)))
                 (sender (plist-get props :sender))
                 (url (plist-get props :url))
                 (choices
                  (delq nil
                        (list '(?r "reply" "Reply (insert nick at prompt)")
                              '(?e "react" "React (insert /react at prompt)")
                              '(?c "copy msg" "Copy message text")
                              (when sender `(?n "nick" ,(format "Copy nick (\"%s\")" sender)))
                              (when url '(?u "url copy" "Copy URL at point"))
                              (when url '(?o "open url" "Open URL at point"))
                              (when sender '(?w "WHOIS" "WHOIS sender"))
                              (when sender '(?q "query" "Open query/DM with sender"))
                              '(?i "inspect" "Inspect raw message")
                              (when sender '(?I "Ignore" "Toggle ignore on sender"))
                              '(?l "list urls" "List all URLs in buffer"))))
                 (choice (car (read-multiple-choice "Pick: " choices)))
                 (action (pcase choice
                           (?r #'clatter-action-reply)
                           (?e #'clatter-action-react)
                           (?c #'clatter-action-copy-message)
                           (?n #'clatter-action-copy-nick)
                           (?u #'clatter-action-copy-url)
                           (?o #'clatter-action-open-url)
                           (?w #'clatter-action-whois)
                           (?q #'clatter-action-query)
                           (?i #'clatter-action-inspect)
                           (?I #'clatter-action-ignore)
                           (?l #'clatter-action-collect-urls))))
            (if (and (buffer-live-p buffer)
                     (eq (marker-buffer marker) buffer)
                     (with-current-buffer buffer
                       (or (eq choice ?l)
                           (and (<= (point-min) marker (point-max))
                                (or (not target)
                                    (and (< marker (point-max))
                                         (equal generation
                                                (get-text-property marker 'clatter-message-generation))
                                         (equal msgid
                                                (get-text-property marker 'clatter-msgid))))))))
                (with-current-buffer buffer
                  (unless (eq choice ?l)
                    (goto-char marker))
                  (let ((clatter-action--menu-active t))
                    (call-interactively action))
                  ;; Only staging a reply/reaction intentionally moves the
                  ;; editing cursor. Other actions and failures preserve it.
                  (when (and target
                             (memq action '(clatter-action-reply clatter-action-react))
                             clatter--input-marker
                             (<= clatter--input-marker (point) (clatter--input-end)))
                    (setq input-position (point))))
              (message "Message is no longer available"))))
      (set-marker marker nil))
    (when (and input-position (buffer-live-p buffer))
      (with-current-buffer buffer
        (goto-char input-position)))))

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
