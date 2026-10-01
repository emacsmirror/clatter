;;; clatter-feed.el --- Feed inbox buffer for clatter -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Glenn Thompson
;; Author: Glenn Thompson <glenn@paren.works>
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Collects live messages from every channel and query, across all
;; networks, into a single `*clatter-feed*' buffer.  Lines are rendered
;; by `clatter-insert-generic', the same entry point the per-buffer UI
;; uses, so sender colors, mentions, grouping and filtering behave as they
;; do in a channel.  (Nicks inside message text are not highlighted: that
;; keys off a channel's member list, which this buffer has none of.)
;; Enable with `clatter-feed-enabled'; RET on a message jumps to it in
;; its source buffer.  `clatter-feed-hide-visible' and
;; `clatter-feed-hide-channels' collapse sources the same way fools
;; do: text stays, the spec hides it.

;;; Code:

;; Defined below with their `:set' machinery; declared here for the
;; functions that machinery calls.
(defvar clatter-feed-hide-visible)
(defvar clatter-feed-hide-channels)

(require 'clatter-config)
(require 'clatter-protocol)
(require 'clatter-connection)
(require 'clatter-model)
(require 'clatter-handlers)
(require 'clatter-smart)
(require 'clatter-pals)
(require 'clatter-ui)

(defcustom clatter-feed-enabled nil
  "Collect messages from every channel and query into one inbox buffer."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-feed-autoscroll t
  "When non-nil, follow new messages while at the bottom of the inbox.
Windows (and the buffer's point) sitting at the end of the feed
buffer advance with each new message; anywhere else they stay put, so
scrolling back to read is never interrupted."
  :type 'boolean
  :group 'clatter)

(defconst clatter-feed--buffer-name "*clatter-feed*"
  "Name of the feed inbox buffer.")

(defvar-local clatter-feed--last-source nil
  "Source (NETWORK . TARGET) of the previously captured message.
Used to decide when to insert a channel separator line.")

(defvar-local clatter-feed--hidden-atoms nil
  "Hide atoms currently present in this buffer's invisibility spec.")

(defvar clatter-feed--hide-atom-cache
  (make-hash-table :test #'equal :weakness 'value)
  "Weak cache of source keys to uninterned invisibility symbols.")

(defun clatter-feed--hide-atom (network target)
  "Return the invisibility atom for NETWORK and TARGET."
  (let ((key (format "%s/%s" network (downcase target))))
    (or (gethash key clatter-feed--hide-atom-cache)
        (puthash key (make-symbol (concat "clatter-feed-hide:" key))
                 clatter-feed--hide-atom-cache))))

(defun clatter-feed--combine-invisible (base atom)
  "Return a flat invisibility value combining BASE with hide ATOM."
  (cond
   ((null base) atom)
   ((listp base) (append base (list atom)))
   (t (list base atom))))

(defun clatter-feed--visible-hide-atoms ()
  "Return hide atoms for channel and query buffers shown in any window."
  (let (atoms)
    (walk-windows
     (lambda (window)
       (let ((buf (window-buffer window)))
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (when (and (derived-mode-p 'clatter-mode)
                        (memq clatter--buffer-type '(channel query))
                        clatter--network
                        clatter--target)
               (push (clatter-feed--hide-atom clatter--network
                                               clatter--target)
                     atoms))))))
     nil t)
    (delete-dups atoms)))

(defun clatter-feed--target-listed-p (target patterns)
  "Return non-nil if TARGET matches a string in PATTERNS, ignoring case."
  (seq-some (lambda (pat)
              (and (stringp pat)
                   (string-equal-ignore-case pat target)))
            patterns))

(defun clatter-feed--listed-hide-atoms (patterns)
  "Return hide atoms for PATTERNS across connections, buffers, and the inbox."
  (let (atoms)
    (maphash
     (lambda (network _conn)
       (dolist (pat patterns)
         (when (stringp pat)
           (push (clatter-feed--hide-atom network pat) atoms))))
     clatter-connections)
    (dolist (buf (clatter-all-buffers))
      (with-current-buffer buf
        (when (and clatter--network clatter--target
                   (memq clatter--buffer-type '(channel query))
                   (clatter-feed--target-listed-p clatter--target patterns))
          (push (clatter-feed--hide-atom clatter--network clatter--target)
                atoms))))
    (when-let* ((inbox (get-buffer clatter-feed--buffer-name))
                ((buffer-live-p inbox)))
      (with-current-buffer inbox
        (let ((pos (point-min)))
          (while (< pos (point-max))
            (let ((network (get-text-property pos 'clatter-feed-network))
                  (target (get-text-property pos 'clatter-feed-target)))
              (when (and network target
                         (clatter-feed--target-listed-p target patterns))
                (push (clatter-feed--hide-atom network target) atoms)))
            (setq pos (or (next-single-property-change
                           pos 'clatter-feed-target nil (point-max))
                          (point-max)))))))
    (delete-dups atoms)))

(defun clatter-feed--desired-hide-atoms ()
  "Return hide atoms for the current visible and channel-list options."
  (delete-dups
   (append (and clatter-feed-hide-visible
                (clatter-feed--visible-hide-atoms))
           (and clatter-feed-hide-channels
                (clatter-feed--listed-hide-atoms
                 clatter-feed-hide-channels)))))

(defun clatter-feed--reconcile-hide ()
  "Sync hide atoms in the feed buffer's invisibility spec."
  (when-let* ((buf (get-buffer clatter-feed--buffer-name))
              ((buffer-live-p buf)))
    (let ((want (clatter-feed--desired-hide-atoms)))
      (with-current-buffer buf
        (dolist (atom clatter-feed--hidden-atoms)
          (unless (memq atom want)
            (remove-from-invisibility-spec atom)))
        (dolist (atom want)
          (add-to-invisibility-spec atom))
        (setq clatter-feed--hidden-atoms want)
        (force-window-update buf)))))

(defun clatter-feed--clear-hide-atoms ()
  "Remove hide atoms from the feed buffer's invisibility spec."
  (when-let* ((buf (get-buffer clatter-feed--buffer-name))
              ((buffer-live-p buf)))
    (with-current-buffer buf
      (dolist (atom clatter-feed--hidden-atoms)
        (remove-from-invisibility-spec atom))
      (setq clatter-feed--hidden-atoms nil)
      (force-window-update buf))))

(defun clatter-feed--window-change (_frame)
  "Reconcile hide atoms after a window buffer change on FRAME."
  (clatter-feed--reconcile-hide))

(defun clatter-feed--sync-hide-hook ()
  "Install or remove the window hook for `clatter-feed-hide-visible'.
The hook is only live while feed capture is enabled."
  (if (and clatter-feed-hide-visible
           (memq #'clatter-feed--on-privmsg clatter-privmsg-hook))
      (add-hook 'window-buffer-change-functions
                #'clatter-feed--window-change)
    (remove-hook 'window-buffer-change-functions
                 #'clatter-feed--window-change)))

(defun clatter-feed--set-hide (symbol value)
  "Set SYMBOL to VALUE and resync the feed hide state."
  (set-default symbol value)
  (clatter-feed--sync-hide-hook)
  (clatter-feed--reconcile-hide))

(defcustom clatter-feed-hide-visible nil
  "When non-nil, hide sources that currently have a live window."
  :type 'boolean
  :set #'clatter-feed--set-hide
  :group 'clatter)

(defcustom clatter-feed-hide-channels nil
  "Channel or query names to hide in the feed inbox buffer.
Case-insensitive; matches every network.  Combined with
`clatter-feed-hide-visible' when that is also set."
  :type '(repeat string)
  :set #'clatter-feed--set-hide
  :group 'clatter)

(defvar clatter-feed-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'clatter-feed-visit)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `clatter-feed-mode'.")

(define-derived-mode clatter-feed-mode clatter-mode "CLatter-Feed"
  "Major mode for the feed inbox buffer."
  ;; The insert path reads `clatter-message-order' in the destination
  ;; buffer.  This buffer has no prompt and no markers, so insertion
  ;; always appends at point-max; if the user selects `newest-first',
  ;; truncation would delete the newest lines and grouping would inspect
  ;; the wrong neighbor.
  (setq-local clatter-message-order 'oldest-first)
  ;; `clatter-mode' leaves buffers writable for their input area; there is
  ;; none here, so stray typing must be blocked.
  (setq buffer-read-only t)
  ;; Seed the invisibility spec the way `clatter-ui-setup-buffer' does,
  ;; otherwise mute and fool properties have no effect here.
  (setq buffer-invisibility-spec (copy-sequence clatter-suppress-messages))
  (when (and clatter-smart-enabled clatter-smart-noise)
    (add-to-invisibility-spec 'noise))
  (unless clatter-fools-visible
    (add-to-invisibility-spec 'clatter-fool)))

(defun clatter-feed--buffer ()
  "Return the feed buffer, creating it on first use."
  (or (get-buffer clatter-feed--buffer-name)
      (with-current-buffer (get-buffer-create clatter-feed--buffer-name)
        (clatter-feed-mode)
        (clatter-feed--reconcile-hide)
        (current-buffer))))

;; --- Capture ---

(defun clatter-feed--capture (msg-type conn sender target text server-time)
  "Insert SENDER's MSG-TYPE TEXT to TARGET on CONN into the feed buffer.
SERVER-TIME is the IRCv3 server-time of the message, if any."
  (let* ((network (clatter-connection-network-id conn))
         (my-nick (clatter-connection-nick conn))
         (isupport (clatter-connection-isupport conn))
         (case-mapping (and isupport (gethash "CASEMAPPING" isupport)))
         (sender-nick (clatter-prefix-nick sender))
         (buf-target (if (clatter-channel-name-p target)
                         target
                       (if (clatter-nick-equal-p target my-nick case-mapping)
                           sender-nick target)))
         (sender-inv (clatter-sender-invisibility sender network))
         (invisible (clatter-feed--combine-invisible
                     sender-inv
                     (clatter-feed--hide-atom network buf-target)))
         (buf (clatter-feed--buffer))
         (last (buffer-local-value 'clatter-feed--last-source buf))
         ;; Note who is tailing the buffer before inserting: windows (and
         ;; the buffer point) at the bottom follow the new message, anyone
         ;; scrolled back stays put.  "At the bottom" is anywhere on the
         ;; last message line, not just point-max: evil's normal state
         ;; never rests point at point-max.
         (tail-floor (when clatter-feed-autoscroll
                       (with-current-buffer buf
                         (save-excursion
                           (goto-char (point-max))
                           (forward-line -1)
                           (point)))))
         (tailing (when tail-floor
                    (seq-filter (lambda (w) (>= (window-point w) tail-floor))
                                (get-buffer-window-list buf nil t))))
         (point-tailing (and tail-floor
                             (with-current-buffer buf
                               (>= (point) tail-floor)))))
    (unless (and last
                 (equal (car last) network)
                 (string-equal-ignore-case (cdr last) buf-target))
      ;; Spell the label out rather than using `clatter-buffer-name': its
      ;; `channel' style drops the network, which is the whole point here.
      ;; The separator shares the message's invisibility, else a hidden
      ;; muted/fool message would leave a bare visible separator behind.
      (clatter--insert-message
       buf
       (propertize (format "── %s/%s ──" network buf-target)
                   'face 'font-lock-doc-face)
       t nil nil invisible))
    (with-current-buffer buf
      ;; The source buffer already scanned this message for inline images;
      ;; scanning again would fetch every URL twice.  Inserting with BUF
      ;; current also prevents this aggregate view from accruing activity.
      (let ((clatter--suppress-image-scan t))
        (clatter-insert-generic msg-type buf sender-nick text conn
                                server-time invisible
                                (list 'clatter-feed-network network
                                      'clatter-feed-target buf-target)))
      ;; Mute/fool must not own separator context.  Source-hide is
      ;; temporary, so those lines still do.
      (unless sender-inv
        (setq clatter-feed--last-source (cons network buf-target)))
      (when point-tailing
        (goto-char (point-max)))
      (dolist (w tailing)
        (set-window-point w (point-max))))))

(defun clatter-feed--on-privmsg (conn sender target text server-time)
  "Capture SENDER's PRIVMSG TEXT to TARGET on CONN at SERVER-TIME."
  (clatter-feed--capture 'privmsg conn sender target text server-time))

(defun clatter-feed--on-action (conn sender target text server-time)
  "Capture SENDER's ACTION TEXT to TARGET on CONN at SERVER-TIME."
  (clatter-feed--capture 'action conn sender target text server-time))

;; --- Jump back to the source ---

(defun clatter-feed--find-message (buffer sender text server-time)
  "Return the position of SENDER's TEXT in BUFFER, or nil.
Fallback for servers without message IDs.  With several matches, prefer
the one nearest SERVER-TIME, else the newest."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let ((matches nil))
        (while (not (eobp))
          (when (and (equal sender (get-text-property (point) 'clatter-sender))
                     (equal text (get-text-property (point) 'clatter-text)))
            (push (point) matches))
          (forward-line 1))
        (cond
         ((null matches) nil)
         ((null server-time)
          ;; MATCHES is in reverse document order.  Which end is newest
          ;; depends on this buffer's message order: `newest-first' puts
          ;; new messages at the top.
          (if (eq clatter-message-order 'newest-first)
              (car (last matches))
            (car matches)))
         (t
          (let ((best nil)
                (best-delta nil))
            (dolist (pos matches best)
              (let* ((time (get-text-property pos 'clatter-server-time))
                     ;; Named delta, not distance: package-lint reads a
                     ;; `distance' binding as the function Emacs 25 removed.
                     (delta (if time
                                (abs (float-time
                                      (time-subtract server-time time)))
                              most-positive-fixnum)))
                (when (or (null best-delta) (< delta best-delta))
                  (setq best pos
                        best-delta delta)))))))))))

(defun clatter-feed-visit ()
  "Jump to the source buffer of the message at point."
  (interactive)
  (let ((network (get-text-property (point) 'clatter-feed-network))
        (target (get-text-property (point) 'clatter-feed-target))
        (msgid (get-text-property (point) 'clatter-msgid))
        (sender (get-text-property (point) 'clatter-sender))
        (text (get-text-property (point) 'clatter-text))
        (server-time (get-text-property (point) 'clatter-server-time)))
    (unless (and network target)
      (user-error "No message at point"))
    (let ((buf (clatter-get-buffer network target)))
      (unless (buffer-live-p buf)
        (user-error "No buffer for %s/%s" network target))
      ;; Resolve the position before showing the buffer, so a message the
      ;; source has truncated away errors out instead of leaving the user
      ;; at an arbitrary point.  A stale msgid falls through to the
      ;; sender+text match.
      (let ((pos (or (clatter--find-message-position-by-msgid buf msgid)
                     (clatter-feed--find-message buf sender text
                                                    server-time))))
        (unless pos
          (user-error "Message not found in %s" (buffer-name buf)))
        ;; Keep the inbox window: the jump target opens elsewhere, like a
        ;; compilation or occur match.
        (pop-to-buffer buf '(nil (inhibit-same-window . t)))
        (goto-char pos)))))

;; --- Setup ---

(defun clatter-feed-enable ()
  "Start collecting messages into the feed buffer."
  (interactive)
  (add-hook 'clatter-privmsg-hook #'clatter-feed--on-privmsg)
  (add-hook 'clatter-action-hook #'clatter-feed--on-action)
  (clatter-feed--sync-hide-hook)
  (clatter-feed--reconcile-hide))

(defun clatter-feed-disable ()
  "Stop collecting messages into the feed buffer."
  (interactive)
  (remove-hook 'clatter-privmsg-hook #'clatter-feed--on-privmsg)
  (remove-hook 'clatter-action-hook #'clatter-feed--on-action)
  (remove-hook 'window-buffer-change-functions
               #'clatter-feed--window-change)
  (clatter-feed--clear-hide-atoms))

(defun clatter-feed ()
  "Display the feed inbox buffer."
  (interactive)
  (pop-to-buffer (clatter-feed--buffer)))

(provide 'clatter-feed)

;;; clatter-feed.el ends here
