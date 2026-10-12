;;; clatter-nicklist.el --- Channel member sidebar for clatter -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Glenn Thompson
;; Author: Glenn Thompson <glenn@paren.works>
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Provides a side window displaying channel members with nick colors
;; and mode prefixes.  Toggle with `clatter-nicklist-toggle'.
;; An open sidebar follows the selected channel.

;;; Code:

(require 'clatter-model)
(require 'clatter-config)

(defcustom clatter-nicklist-width 20
  "Width of the nicklist sidebar window."
  :type 'integer
  :group 'clatter)

(defcustom clatter-nicklist-side 'right
  "Side of the frame for the nicklist window."
  :type '(choice (const left) (const right))
  :group 'clatter)

(defconst clatter-nicklist--buffer-name "*clatter-nicks*"
  "Buffer name of the singleton nicklist sidebar.")

(defvar-local clatter-nicklist--source-buffer nil
  "The channel buffer this nicklist is associated with.")

(defvar-local clatter-nicklist--refresh-timer nil
  "Pending refresh timer owned by this sidebar.")

(defvar clatter-nicklist-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'clatter-nicklist-close)
    (define-key map (kbd "g") #'clatter-nicklist-refresh)
    (define-key map (kbd "RET") #'clatter-nicklist-query)
    map)
  "Keymap for `clatter-nicklist-mode'.")

(define-derived-mode clatter-nicklist-mode special-mode "NickList"
  "Major mode for the clatter nicklist sidebar."
  (setq-local revert-buffer-function #'clatter-nicklist--revert)
  (add-hook 'kill-buffer-hook #'clatter-nicklist--cancel-refresh nil t)
  (setq buffer-read-only t))

(defun clatter-nicklist--revert (_ignore-auto _noconfirm)
  "Revert function for nicklist buffer."
  (clatter-nicklist-refresh))

(defun clatter-nicklist--render (buf)
  "Render the nicklist for source buffer BUF into the current buffer."
  (let* ((inhibit-read-only t)
         (nicks (clatter-nick-list buf))
         (conn (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (when clatter--network
                     (clatter-get-connection clatter--network)))))
         (rank (or (let ((isup (clatter-connection-isupport conn)))
                     (when isup
                       (let ((prefix (gethash "PREFIX" isup)))
                         (and prefix
                              (string-match (rx bol ?\( (+ alpha) ?\) (group (+ anything)) eol)
                                            prefix)
                              (match-string 1 prefix)))))
                   clatter-prefix-rank)))
    (erase-buffer)
    (insert (propertize (format " %d members\n"
                                (length nicks))
                        'face 'bold))
    (insert (propertize (make-string (1- clatter-nicklist-width) ?-) 'face 'font-lock-doc-face)
            "\n")
    ;; Display sorted nicks by rank
    (setq nicks
          (sort nicks
                :key (lambda (n) (cons (car n)
                                  (or (cl-loop for p being the elements of (cdr n)
                                               for idx = (string-search (char-to-string p) rank)
                                               maximize (if idx (- (length rank) idx) 0))
                                      0)))
                :lessp (lambda (a b)
                         (if (= (cdr a) (cdr b))
                             (string< (car a) (car b))
                           (> (cdr a) (cdr b))))
                :in-place t))
    (dolist (n nicks)
      (clatter-nicklist--insert-nick (car n) (cdr n) conn))
    (goto-char (point-min))))

(defun clatter-nicklist--insert-nick (nick prefix _conn)
  "Insert NICK with PREFIX into the nicklist buffer."
  (let* ((prefix-face (cond
                       ((string-search "@" prefix) 'clatter-system)
                       ((string-search "+" prefix) 'clatter-notice)
                       (t nil)))
         (prefix-str (if (string-empty-p prefix) " " prefix)))
    (insert (if prefix-face
                (propertize prefix-str 'face prefix-face)
              prefix-str)
            " "
            (propertize nick 'face (clatter-hl-nick-face-symbol nick)
                        'clatter-nick nick)
            "\n")))

(defun clatter-nicklist-refresh ()
  "Refresh the nicklist sidebar."
  (interactive)
  (let ((buf clatter-nicklist--source-buffer))
    (when (and buf (buffer-live-p buf))
      (clatter-nicklist--render buf))))

(defun clatter-nicklist--cancel-refresh ()
  "Cancel this sidebar's pending refresh."
  (when clatter-nicklist--refresh-timer
    (cancel-timer clatter-nicklist--refresh-timer)
    (setq clatter-nicklist--refresh-timer nil)))

(defun clatter-nicklist-close ()
  "Close the nicklist sidebar."
  (interactive)
  (when-let* ((existing (get-buffer clatter-nicklist--buffer-name)))
    (when-let* ((win (get-buffer-window existing)))
      (delete-window win))
    (kill-buffer existing)))

(defun clatter-nicklist-query ()
  "Open a query (DM) with the nick at point."
  (interactive)
  (let ((nick (get-text-property (point) 'clatter-nick)))
    (when nick
      (let ((source clatter-nicklist--source-buffer))
        (when (and source (buffer-live-p source))
          (with-current-buffer source
            (when (and clatter--network
                       (clatter-get-connection clatter--network))
              (clatter-get-or-create-buffer
               clatter--network nick 'query)
              (pop-to-buffer
               (clatter-get-buffer clatter--network nick)))))))))

(defun clatter-nicklist-toggle ()
  "Toggle the nicklist sidebar.
An open list follows the selected channel."
  (interactive)
  (unless (and clatter--target clatter--nick-list)
    (user-error "Not in a channel buffer"))
  (if-let* ((existing (get-buffer clatter-nicklist--buffer-name))
            ((get-buffer-window existing)))
      (clatter-nicklist-close)
    (let ((source (current-buffer))
          (nl-buf (get-buffer-create clatter-nicklist--buffer-name)))
      (with-current-buffer nl-buf
        (clatter-nicklist--cancel-refresh)
        (clatter-nicklist-mode)
        (setq clatter-nicklist--source-buffer source)
        (clatter-nicklist--render source))
      (display-buffer-in-side-window
       nl-buf
       `((side . ,clatter-nicklist-side)
         (window-width . ,clatter-nicklist-width)
         (slot . 0)
         (dedicated . t))))))

(defun clatter-nicklist--follow (frame)
  "Retarget the open nicklist to FRAME's selected channel."
  (when-let* ((nl (get-buffer clatter-nicklist--buffer-name))
              ((get-buffer-window nl))
              (buf (window-buffer (frame-selected-window frame)))
              ((not (eq buf nl))))
    (with-current-buffer buf
      (when (and (derived-mode-p 'clatter-mode)
                 clatter--target
                 clatter--nick-list
                 (not (eq buf (buffer-local-value
                               'clatter-nicklist--source-buffer nl))))
        (with-current-buffer nl
          (clatter-nicklist--cancel-refresh)
          (setq clatter-nicklist--source-buffer buf)
          (clatter-nicklist--render buf))))))

;; --- Auto-refresh hooks ---

(defun clatter-nicklist--auto-refresh (channel-buffer)
  "Schedule one refresh for the visible sidebar showing CHANNEL-BUFFER."
  (when-let* (((buffer-live-p channel-buffer))
              (nl-buf (get-buffer clatter-nicklist--buffer-name))
              ((get-buffer-window nl-buf))
              ((eq channel-buffer
                   (buffer-local-value 'clatter-nicklist--source-buffer nl-buf))))
    (with-current-buffer nl-buf
      (unless clatter-nicklist--refresh-timer
        (let (timer)
          (setq timer
                (run-at-time
                 0.2 nil
                 (lambda ()
                   (when (buffer-live-p nl-buf)
                     (with-current-buffer nl-buf
                       (when (eq timer clatter-nicklist--refresh-timer)
                         (setq clatter-nicklist--refresh-timer nil)
                         (when (and (get-buffer-window nl-buf)
                                    (buffer-live-p channel-buffer)
                                    (eq channel-buffer clatter-nicklist--source-buffer))
                           (clatter-nicklist-refresh))))))))
          (setq clatter-nicklist--refresh-timer timer))))))

(defun clatter-nicklist--on-join (conn _nick channel _account _realname)
  "Refresh nicklist on CONN when someone joins CHANNEL."
  (let ((buf (clatter-get-buffer
              (clatter-connection-network-id conn) channel)))
    (when buf
      (clatter-nicklist--auto-refresh buf))))

(defun clatter-nicklist--on-part (conn _nick channel _message)
  "Refresh nicklist on CONN when someone parts CHANNEL."
  (let ((buf (clatter-get-buffer
              (clatter-connection-network-id conn) channel)))
    (when buf
      (clatter-nicklist--auto-refresh buf))))

(defun clatter-nicklist--source-on-network (network-id)
  "Return the visible nicklist source buffer when it is on NETWORK-ID."
  (when-let* ((nl (get-buffer clatter-nicklist--buffer-name))
              ((get-buffer-window nl))
              (src (buffer-local-value 'clatter-nicklist--source-buffer nl))
              ((buffer-live-p src))
              ((equal (buffer-local-value 'clatter--network src) network-id)))
    src))

(defun clatter-nicklist--on-quit (conn _sender _message)
  "Refresh nicklist on CONN after a quit."
  (when-let* ((src (clatter-nicklist--source-on-network
                   (clatter-connection-network-id conn))))
    (clatter-nicklist--auto-refresh src)))

(defun clatter-nicklist--on-nick (conn _old-nick _new-nick)
  "Refresh nicklist on CONN when someone changes nick."
  (when-let* ((src (clatter-nicklist--source-on-network
                   (clatter-connection-network-id conn))))
    (clatter-nicklist--auto-refresh src)))

(defun clatter-nicklist--on-names (conn channel _names-str)
  "Refresh nicklist on CONN after NAMES reply for CHANNEL."
  (let ((buf (clatter-get-buffer
              (clatter-connection-network-id conn) channel)))
    (when buf
      (clatter-nicklist--auto-refresh buf))))

(defun clatter-nicklist--on-disconnect (network-id _event)
  "Refresh the matching sidebar after NETWORK-ID's membership is cleared."
  (when-let* ((src (clatter-nicklist--source-on-network network-id)))
    (clatter-nicklist--auto-refresh src)))

(defun clatter-nicklist-init ()
  "Register nicklist auto-refresh and follow hooks."
  (add-hook 'clatter-join-hook #'clatter-nicklist--on-join)
  (add-hook 'clatter-part-hook #'clatter-nicklist--on-part)
  (add-hook 'clatter-quit-hook #'clatter-nicklist--on-quit)
  (add-hook 'clatter-nick-hook #'clatter-nicklist--on-nick)
  (add-hook 'clatter-names-hook #'clatter-nicklist--on-names)
  (add-hook 'clatter-disconnect-hook #'clatter-nicklist--on-disconnect)
  (add-hook 'window-buffer-change-functions #'clatter-nicklist--follow)
  (add-hook 'window-selection-change-functions #'clatter-nicklist--follow))

;; Auto-init when loaded
(clatter-nicklist-init)

(provide 'clatter-nicklist)

;;; clatter-nicklist.el ends here
