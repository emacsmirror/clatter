;;; clatter-nicklist.el --- Channel member sidebar for clatter -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Glenn Thompson
;; Author: Glenn Thompson <glenn@paren.works>
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Provides a side window displaying channel members with nick colors
;; and mode prefixes.  Toggle with `clatter-nicklist-toggle'.

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

(defvar-local clatter-nicklist--source-buffer nil
  "The channel buffer this nicklist is associated with.")

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
  (setq buffer-read-only t))

(defun clatter-nicklist--revert (_ignore-auto _noconfirm)
  "Revert function for nicklist buffer."
  (clatter-nicklist-refresh))

(defun clatter-nicklist--buffer-name (channel)
  "Return nicklist buffer name for CHANNEL."
  (format "*clatter-nicks: %s*" channel))

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
    (insert (propertize (make-string (1- clatter-nicklist-width) ?-) 'face 'shadow)
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
  (let* ((color (clatter-hl-nick-color nick))
         (prefix-face (cond
                       ((string-search "@" prefix) 'clatter-system)
                       ((string-search "+" prefix) 'clatter-notice)
                       (t nil)))
         (prefix-str (if (string-empty-p prefix) " " prefix)))
    (insert (if prefix-face
                (propertize prefix-str 'face prefix-face)
              prefix-str)
            " "
            (propertize nick 'face (list :foreground color)
                        'clatter-nick nick)
            "\n")))

(defun clatter-nicklist-refresh ()
  "Refresh the nicklist sidebar."
  (interactive)
  (let ((buf clatter-nicklist--source-buffer))
    (when (and buf (buffer-live-p buf))
      (clatter-nicklist--render buf))))

(defun clatter-nicklist-close ()
  "Close the nicklist sidebar."
  (interactive)
  (let ((source (or clatter-nicklist--source-buffer (current-buffer))))
    (when source
      (cond
       ((buffer-live-p source)
        (with-current-buffer source
          (when (and clatter--target clatter--nick-list)
            (let* ((target clatter--target)
                   (nl-name (clatter-nicklist--buffer-name target))
                   (existing (get-buffer nl-name)))
              (when (and existing (get-buffer-window existing))
                (delete-window (get-buffer-window existing))
                (kill-buffer existing))))))
       ((eq source clatter-nicklist--source-buffer)
        (delete-window (get-buffer-window (current-buffer))))))))

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
  "Toggle the nicklist sidebar for the current channel."
  (interactive)
  (unless (and clatter--target clatter--nick-list)
    (user-error "Not in a channel buffer"))
  (let* ((target clatter--target)
         (nl-name (clatter-nicklist--buffer-name target))
         (existing (get-buffer nl-name)))
    (if (and existing (get-buffer-window existing))
        ;; Close it
        (progn
          (delete-window (get-buffer-window existing))
          (kill-buffer existing))
      ;; Open it
      (let ((source (current-buffer))
            (nl-buf (get-buffer-create nl-name)))
        (with-current-buffer nl-buf
          (clatter-nicklist-mode)
          (setq clatter-nicklist--source-buffer source)
          (clatter-nicklist--render source))
        (display-buffer-in-side-window
         nl-buf
         `((side . ,clatter-nicklist-side)
           (window-width . ,clatter-nicklist-width)
           (slot . 0)
           (dedicated . t)))))))

;; --- Auto-refresh hooks ---

(defun clatter-nicklist--auto-refresh (channel-buffer)
  "Refresh any open nicklist for CHANNEL-BUFFER."
  (when (buffer-live-p channel-buffer)
    (with-current-buffer channel-buffer
      (when clatter--target
        (let ((nl-buf (get-buffer
                       (clatter-nicklist--buffer-name clatter--target))))
          (when (and nl-buf (get-buffer-window nl-buf))
            (with-current-buffer nl-buf
              (clatter-nicklist-refresh))))))))

(defun clatter-nicklist--on-join (conn _nick channel _account _realname)
  "Refresh nicklist on CONN when someone joins CHANNEL."
  (let ((buf (clatter-get-buffer
              (clatter-connection-network-id conn) channel)))
    (when buf
      (run-at-time 0.2 nil #'clatter-nicklist--auto-refresh buf))))

(defun clatter-nicklist--on-part (conn _nick channel _message)
  "Refresh nicklist on CONN when someone parts CHANNEL."
  (let ((buf (clatter-get-buffer
              (clatter-connection-network-id conn) channel)))
    (when buf
      (run-at-time 0.2 nil #'clatter-nicklist--auto-refresh buf))))

(defun clatter-nicklist--on-quit (conn nick _message)
  "Refresh nicklist on CONN for all channels NICK was in."
  (let ((network (clatter-connection-network-id conn)))
    (dolist (buf (clatter-all-buffers))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when (and (equal clatter--network network)
                     clatter--nick-list
                     (gethash (downcase nick) clatter--nick-list))
            (run-at-time 0.2 nil #'clatter-nicklist--auto-refresh buf)))))))

(defun clatter-nicklist--on-nick (conn _old-nick _new-nick)
  "Refresh nicklist on CONN when someone changes nick."
  (let ((network (clatter-connection-network-id conn)))
    (dolist (buf (clatter-all-buffers))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when (equal clatter--network network)
            (run-at-time 0.2 nil #'clatter-nicklist--auto-refresh buf)))))))

(defun clatter-nicklist--on-names (conn channel _names-str)
  "Refresh nicklist on CONN after NAMES reply for CHANNEL."
  (let ((buf (clatter-get-buffer
              (clatter-connection-network-id conn) channel)))
    (when buf
      (run-at-time 0.2 nil #'clatter-nicklist--auto-refresh buf))))

(defun clatter-nicklist-init ()
  "Register nicklist auto-refresh hooks."
  (add-hook 'clatter-join-hook #'clatter-nicklist--on-join)
  (add-hook 'clatter-part-hook #'clatter-nicklist--on-part)
  (add-hook 'clatter-quit-hook #'clatter-nicklist--on-quit)
  (add-hook 'clatter-nick-hook #'clatter-nicklist--on-nick)
  (add-hook 'clatter-names-hook #'clatter-nicklist--on-names))

;; Auto-init when loaded
(clatter-nicklist-init)

(provide 'clatter-nicklist)

;;; clatter-nicklist.el ends here
