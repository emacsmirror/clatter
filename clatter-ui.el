;;; clatter-ui.el --- Buffer rendering, faces, and input -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Glenn Thompson
;; Author: Glenn Thompson
;; License: MIT

;;; Commentary:

;; UI layer for clatter.el.  Renders messages into Emacs buffers,
;; defines faces for nick colorization, handles user input from the
;; prompt, and manages mode-line display.

;;; Code:

(require 'cl-lib)
(require 'clatter-config)
(require 'clatter-protocol)
(require 'clatter-connection)
(require 'clatter-model)
(require 'clatter-handlers)
(require 'clatter-hl-nicks)

;; --- Faces ---

(defface clatter-timestamp
  '((t :foreground "#7c7c7c"))
  "Face for message timestamps."
  :group 'clatter)

(defface clatter-nick
  '((t :weight bold))
  "Default face for nicks."
  :group 'clatter)

(defface clatter-my-nick
  '((t :foreground "#c792ea" :weight bold))
  "Face for your own nick."
  :group 'clatter)

(defface clatter-action
  '((t :foreground "#c3e88d" :slant italic))
  "Face for /me action messages."
  :group 'clatter)

(defface clatter-notice
  '((t :foreground "#ffcb6b"))
  "Face for NOTICE messages."
  :group 'clatter)

(defface clatter-system
  '((t :foreground "#546e7a"))
  "Face for system/status messages."
  :group 'clatter)

(defface clatter-error
  '((t :foreground "#ff5370" :weight bold))
  "Face for error messages."
  :group 'clatter)

(defface clatter-prompt
  '((t :foreground "#82aaff" :weight bold))
  "Face for the input prompt."
  :group 'clatter)

(defface clatter-mention
  '((t :foreground "#ff5370" :weight bold))
  "Face for highlighted mentions of your nick."
  :group 'clatter)

(defface clatter-channel
  '((t :foreground "#89ddff"))
  "Face for channel names."
  :group 'clatter)

;; --- Nick color palette (hash-based consistent colors) ---

(defcustom clatter-nick-colors
  '("#f78c6c" "#c3e88d" "#89ddff" "#c792ea" "#ffcb6b"
    "#ff5370" "#82aaff" "#f07178" "#babed8" "#a6accd"
    "#e2b93d" "#addb67" "#7fdbca" "#ef5350" "#80cbc4"
    "#b2ccd6" "#eeffff" "#f78c6c" "#c792ea" "#ff5370")
  "Color palette for nick colorization."
  :type '(repeat color)
  :group 'clatter)

(defun clatter-nick-color (nick)
  "Return a consistent color for NICK based on hash."
  (let* ((hash (cl-reduce #'+ (mapcar #'identity nick)))
         (idx (mod hash (length clatter-nick-colors))))
    (nth idx clatter-nick-colors)))

(defun clatter-nick-face (nick conn)
  "Return face properties for NICK on CONN."
  (if (string-equal nick (clatter-connection-nick conn))
      'clatter-my-nick
    (list :foreground (clatter-nick-color nick) :weight 'bold)))

;; --- Message insertion ---

(defvar-local clatter--prompt-marker nil
  "Marker for the start of the input prompt.")

(defvar-local clatter--input-marker nil
  "Marker for the start of user input (after prompt text).")

(defvar-local clatter--messages-marker nil
  "Marker for the start of the message area (below the input line).")

(defun clatter--format-nick-column (nick-str &optional face sender)
  "Right-align NICK-STR within `clatter-nick-column-width'.
Apply FACE and set clatter-sender property to SENDER if provided."
  (let* ((width clatter-nick-column-width)
         (nick-len (length nick-str))
         (pad (max 0 (- width nick-len)))
         (padded (concat (make-string pad ?\s)
                         (if face
                             (propertize nick-str 'face face)
                           nick-str))))
    (when sender
      (setq padded (propertize padded 'clatter-sender sender)))
    padded))

(defun clatter--format-system-prefix (prefix-str)
  "Right-align PREFIX-STR (e.g. \"***\") within the nick column."
  (let* ((width clatter-nick-column-width)
         (plen (length prefix-str))
         (pad (max 0 (- width plen))))
    (concat (make-string pad ?\s)
            (propertize prefix-str 'face 'clatter-system))))

(defun clatter--insert-message (buffer text &optional no-timestamp msg-props time invisible)
  "Insert formatted TEXT into BUFFER.
Adds timestamp unless NO-TIMESTAMP is non-nil.
MSG-PROPS is an optional plist of extra text properties for the message line.
TIME is an optional Emacs time value (from IRCv3 server-time) for the timestamp.
When `clatter-message-order' is `newest-first', messages appear directly below
the input line with older ones scrolling down.  When `oldest-first', messages
append at the bottom like a traditional IRC client."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (oldest-first (eq clatter-message-order 'oldest-first)))
        (save-excursion
          (goto-char (if oldest-first
                         (point-max)
                       (or clatter--messages-marker (point-max))))
          (let* ((ts-str (unless no-timestamp
                           (format-time-string clatter-timestamp-format time)))
                 (wrap-col (1+ clatter-nick-column-width))
                 (wrap-prefix (make-string wrap-col ?\s))
                 (start (point)))
            (insert text "\n")
            (when ts-str
              (let ((ov (make-overlay start (1+ start) nil t)))
                (overlay-put ov 'before-string
                             (propertize " " 'display
                                         `((margin right-margin)
                                           ,(propertize ts-str 'face 'clatter-timestamp))))
                (overlay-put ov 'clatter-timestamp t)
                (overlay-put ov 'invisible invisible)))
            (add-text-properties start (point)
                                 (list 'read-only t
                                       'front-sticky t
                                       'wrap-prefix wrap-prefix
                                       'line-prefix ""))
            (when msg-props
              (add-text-properties start (point) msg-props))
            (put-text-property start (point) 'invisible invisible)))
        (clatter--maybe-truncate buffer)
        ;; Auto-scroll in oldest-first mode
        (when oldest-first
          (dolist (win (get-buffer-window-list buffer nil t))
            (with-selected-window win
              (goto-char (point-max))
              (recenter -1))))))))

(defun clatter--maybe-truncate (buffer)
  "Truncate BUFFER if it exceeds `clatter-buffer-max-lines'.
Removes oldest messages from the appropriate end of the buffer."
  (when (and clatter-buffer-max-lines
             (> (count-lines (point-min) (point-max)) clatter-buffer-max-lines))
    (let ((inhibit-read-only t)
          (target-lines (- (count-lines (point-min) (point-max))
                           clatter-buffer-max-lines)))
      (save-excursion
        (if (eq clatter-message-order 'oldest-first)
            ;; Oldest messages are near the top (after the prompt)
            (let ((start (or clatter--messages-marker (point-min))))
              (goto-char start)
              (forward-line target-lines)
              (dolist (ov (overlays-in start (point)))
                (delete-overlay ov))
              (delete-region start (point)))
          ;; newest-first: oldest messages are at the bottom
          (goto-char (point-max))
          (forward-line (- target-lines))
          (dolist (ov (overlays-in (point) (point-max)))
            (delete-overlay ov))
          (delete-region (point) (point-max)))))))

(defun clatter--find-message-by-msgid (buffer msgid)
  "Find message text and sender in BUFFER by MSGID.
Returns (sender . text) or nil."
  (when (and (buffer-live-p buffer) msgid)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (let ((found nil))
          (while (and (not found) (not (eobp)))
            (when (string= msgid (get-text-property (point) 'clatter-msgid))
              (setq found (cons (get-text-property (point) 'clatter-sender)
                                (get-text-property (point) 'clatter-text))))
            (forward-line 1))
          found)))))

(defun clatter-insert-privmsg (buffer sender text conn &optional server-time)
  "Insert a PRIVMSG from SENDER with TEXT into BUFFER using CONN context.
SERVER-TIME overrides the current time for the timestamp."
  (let* ((nick-face (clatter-hl-nick-face sender conn))
         (my-nick (clatter-connection-nick conn))
         (is-mention (and my-nick
                          (string-match-p
                           (regexp-quote (downcase my-nick))
                           (downcase text))))
         (reply-to (get-text-property 0 'clatter-reply-to text))
         (msgid (get-text-property 0 'clatter-msgid text))
         (reply-context (when reply-to
                          (clatter--find-message-by-msgid buffer reply-to)))
         (hl-text (clatter-hl-format-text text buffer conn))
         (bot-tag (if (get-text-property 0 'clatter-bot sender)
                     (propertize "[bot]" 'face 'clatter-notice) ""))
         (nick-col (clatter--format-nick-column
                    (concat (format "<%s>" sender) bot-tag) nick-face sender))
         (msg-text (if is-mention
                       (propertize hl-text 'face 'clatter-mention)
                     hl-text))
         ;; Prepend reply context if available
         (reply-line (when reply-context
                       (let* ((ref-sender (car reply-context))
                              (ref-text (cdr reply-context))
                              (preview (if (> (length ref-text) 60)
                                           (concat (substring ref-text 0 57) "...")
                                         ref-text)))
                         (concat (propertize (format "↳ %s: %s" ref-sender preview)
                                             'face 'shadow)
                                 "\n"))))
         (formatted (concat (or reply-line "") nick-col " " msg-text))
         (props (list 'clatter-msg-type 'privmsg
                      'clatter-sender sender
                      'clatter-text text)))
    (when msgid
      (setq props (plist-put props 'clatter-msgid msgid)))
    (clatter--insert-message buffer formatted nil props server-time)
    (when (fboundp 'clatter-image--scan-message)
      (let ((img-marker (with-current-buffer buffer
                          (copy-marker
                           (if (eq clatter-message-order 'oldest-first)
                               (point-max)
                             (or clatter--messages-marker (point-max)))))))
        (clatter-image--scan-message text buffer img-marker)))
    (unless (eq buffer (current-buffer))
      (clatter-mark-activity buffer is-mention))))

(defun clatter-insert-action (buffer sender text conn)
  "Insert a /me ACTION from SENDER with TEXT into BUFFER."
  (let* ((hl-text (clatter-hl-format-text text buffer conn))
         (prefix (clatter--format-nick-column "*" 'clatter-action sender))
         (formatted (concat prefix " "
                            (propertize (concat sender " " hl-text)
                                        'face 'clatter-action))))
    (clatter--insert-message buffer formatted nil
                              (list 'clatter-msg-type 'action
                                    'clatter-sender sender
                                    'clatter-text text))
    (unless (eq buffer (current-buffer))
      (clatter-mark-activity buffer nil))))

(defun clatter-insert-notice (buffer sender text)
  "Insert a NOTICE from SENDER with TEXT into BUFFER."
  (let* ((prefix (clatter--format-nick-column
                  (format "-%s-" sender) 'clatter-notice))
         (formatted (concat prefix " "
                            (propertize text 'face 'clatter-notice))))
    (clatter--insert-message buffer formatted)))

(defun clatter-insert-system (buffer text &optional invisible)
  "Insert a system message TEXT into BUFFER."
  (let* ((prefix (clatter--format-system-prefix "***"))
         (formatted (concat prefix " "
                            (propertize text 'face 'clatter-system))))
    (clatter--insert-message buffer formatted nil nil nil invisible)))

(defun clatter-insert-error (buffer text)
  "Insert an error message TEXT into BUFFER."
  (let* ((prefix (clatter--format-nick-column "!!!" 'clatter-error))
         (formatted (concat prefix " "
                            (propertize text 'face 'clatter-error))))
    (clatter--insert-message buffer formatted)))

;; --- Input prompt ---

(defun clatter--setup-prompt (buffer)
  "Set up the input prompt at the top of BUFFER."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (clatter-input-ring-setup)
      (goto-char (point-min))
      (setq clatter--prompt-marker (point-marker))
      (set-marker-insertion-type clatter--prompt-marker nil)
      (insert (propertize (concat (or clatter--target "clatter") "> ")
                          'face 'clatter-prompt
                          'read-only t
                          'front-sticky t
                          'rear-nonsticky t))
      (setq clatter--input-marker (point-marker))
      (set-marker-insertion-type clatter--input-marker nil)
      ;; Newline separates input line from messages
      (save-excursion
        (goto-char clatter--input-marker)
        (insert (propertize "\n" 'read-only t 'rear-nonsticky t))
        (setq clatter--messages-marker (point-marker))
        (set-marker-insertion-type clatter--messages-marker nil)))))

(defun clatter--get-input ()
  "Get user input text from the prompt."
  (when (and clatter--input-marker clatter--messages-marker)
    (buffer-substring-no-properties
     clatter--input-marker
     (1- (marker-position clatter--messages-marker)))))

(defun clatter--clear-input ()
  "Clear the user input area."
  (when (and clatter--input-marker clatter--messages-marker)
    (let ((inhibit-read-only t))
      (delete-region clatter--input-marker
                     (1- (marker-position clatter--messages-marker))))))

(defun clatter--set-input (input)
  (clatter--clear-input)
  (insert input))

(defun clatter-set-prev-input ()
  "Insert the previous (older) input history item at the prompt."
  (interactive)
  (let ((item (clatter-input-ring-nth clatter-input-ring-index)))
    (when item
      (clatter--set-input item)
      (setq clatter-input-ring-index
            (min (1+ clatter-input-ring-index)
                 (1- (ring-length clatter-input-ring)))))))

(defun clatter-set-next-input ()
  "Insert the next (newer) input history item at the prompt."
  (interactive)
  (when (and (ring-p clatter-input-ring)
             (not (ring-empty-p clatter-input-ring)))
    (setq clatter-input-ring-index (max 0 (1- clatter-input-ring-index)))
    (let ((item (clatter-input-ring-nth clatter-input-ring-index)))
      (when item
        (clatter--set-input item)))))

;; --- Input handling ---

(defun clatter-send-input ()
  "Send the current input line.
If the input contains multiple lines and exceeds
`clatter-paste-flood-threshold', prompt before sending."
  (interactive)
  (let ((input (string-trim (or (clatter--get-input) ""))))
    (when (> (length input) 0)
      (clatter-input-ring-add input)
      (let* ((lines (split-string input "\n"))
             (nlines (length lines))
             (flood (and clatter-paste-flood-threshold
                         (> nlines clatter-paste-flood-threshold))))
        (if (and flood
                 (not (y-or-n-p
                       (format "Paste %d lines to %s? "
                               nlines (or clatter--target "?")))))
            (message "[clatter] Paste cancelled")
          (clatter--clear-input)
          (if (string-prefix-p "/" (car lines))
              (clatter--handle-command (car lines))
            (dolist (line lines)
              (let ((trimmed (string-trim line)))
                (when (> (length trimmed) 0)
                  (clatter--send-message trimmed)))))
          (clatter--send-typing-done))))))

(defun clatter--send-message (text)
  "Send TEXT as a PRIVMSG to the current target."
  (let* ((network clatter--network)
         (target clatter--target)
         (conn (clatter-get-connection network)))
    (when (and conn target (not (string= target "*server*")))
      (let ((parts (clatter-split-long-message target text)))
        (dolist (part parts)
          (clatter-send conn (clatter-irc-privmsg target part))
          ;; Echo our own message if echo-message not enabled
          (unless (member "echo-message" (clatter-connection-cap-enabled conn))
            (clatter-insert-privmsg (current-buffer)
                                    (clatter-connection-nick conn)
                                    part conn)))))))

(defun clatter--handle-command (input)
  "Parse and execute INPUT as a /command."
  ;; Forward to clatter-commands.el
  (clatter-execute-command input))

;; Forward declaration
(declare-function clatter-execute-command "clatter-commands")

;; --- Mode-line ---

(defvar clatter-mode-line-format
  '(:eval (clatter--mode-line-string))
  "Mode-line construct for clatter buffers.")

(defun clatter--mode-line-string ()
  "Generate mode-line string for current clatter buffer."
  (when clatter--network
    (let* ((conn (clatter-get-connection clatter--network))
           (nick (if conn (clatter-connection-nick conn) "?"))
           (nicks (clatter-nick-count (current-buffer)))
           (topic-str (if clatter--topic
                          (truncate-string-to-width clatter--topic 40 nil nil "...")
                        "")))
      (format " [%s/%s] %s%s%s"
              clatter--network
              (or clatter--target "")
              nick
              (if (> nicks 0) (format " (%d)" nicks) "")
              (if (> (length topic-str) 0)
                  (format " - %s" topic-str)
                "")))))

;; --- Hook into clatter-mode ---

(defun clatter-ui-setup-buffer (buffer)
  "Set up UI elements for a new clatter BUFFER."
  (with-current-buffer buffer
    ;; Seed the buffer-local invisibility spec from the global default.
    ;; Use a fresh copy so per-buffer /suppress and /unsuppress edits
    ;; never mutate the shared clatter-suppress-messages list.
    (setq buffer-invisibility-spec (copy-sequence clatter-suppress-messages))
    (clatter--setup-prompt buffer)
    ;; Add mode-line
    (setq-local mode-line-format
                (list " " 'mode-line-buffer-identification
                      clatter-mode-line-format
                      '(:eval (clatter--typing-mode-line))
                      " " 'mode-line-end-spaces))
    ;; Key bindings for input
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map clatter-mode-map)
      (define-key map (kbd "RET") #'clatter-send-input)
      (define-key map (kbd "TAB") #'completion-at-point)
      (define-key map (kbd "M-p") #'clatter-set-prev-input)
      (define-key map (kbd "M-n") #'clatter-set-next-input)
      (use-local-map map))
    ;; Ensure window margins are synced for timestamp display
    (add-hook 'window-configuration-change-hook
              #'clatter--sync-window-margins nil t)
    ;; Outbound typing notifications
    (clatter--setup-outbound-typing buffer)))

(defun clatter--sync-window-margins ()
  "Ensure the current window has correct margins for timestamp display.
Emacs requires `set-window-margins' on the window, not just
`right-margin-width' on the buffer."
  (when (and (derived-mode-p 'clatter-mode)
             (eq (current-buffer) (window-buffer)))
    (let ((ts-width (1+ (length (format-time-string clatter-timestamp-format)))))
      (set-window-margins (selected-window)
                          (car (window-margins))
                          ts-width))))

;; --- Nick completion ---

(defun clatter-complete-nick ()
  "Complete nick at point using the channel's nick list."
  (interactive)
  (when clatter--nick-list
    (let* ((end (point))
           (start (save-excursion
                    (skip-chars-backward "^ \t\n")
                    (point)))
           (prefix (buffer-substring-no-properties start end))
           (nicks (cl-loop for k being the hash-keys of clatter--nick-list
                           when (string-prefix-p (downcase prefix) k)
                           collect (cdr (gethash k clatter--nick-list)))))
      (cond
       ((null nicks)
        (message "No matching nicks"))
       ((= (length nicks) 1)
        (delete-region start end)
        (insert (car nicks)
                (if (= start (marker-position clatter--input-marker)) ": " " ")))
       (t
        (let ((common (try-completion prefix
                                      (mapcar #'list nicks))))
          (when (stringp common)
            (delete-region start end)
            (insert common))
          (message "Nicks: %s" (string-join nicks ", "))))))))

;; --- Wire up event hooks ---

(defun clatter-ui--on-privmsg (conn sender target text server-time)
  "Handle PRIVMSG event for UI."
  (unless (clatter-ignored-p sender)
    (let* ((network (clatter-connection-network-id conn))
           (my-nick (clatter-connection-nick conn))
           (buf-target (if (clatter-channel-name-p target)
                           target
                         (if (string-equal target my-nick) sender target)))
           (buf (clatter-get-or-create-buffer network buf-target)))
      (clatter-ui-setup-buffer-if-needed buf)
      (clatter-insert-privmsg buf sender text conn server-time))))

(defun clatter-ui--on-action (conn sender target text _server-time)
  "Handle ACTION event for UI."
  (unless (clatter-ignored-p sender)
    (let* ((network (clatter-connection-network-id conn))
           (my-nick (clatter-connection-nick conn))
           (buf-target (if (clatter-channel-name-p target)
                           target
                         (if (string-equal target my-nick) sender target)))
           (buf (clatter-get-or-create-buffer network buf-target)))
      (clatter-ui-setup-buffer-if-needed buf)
      (clatter-insert-action buf sender text conn))))

(defun clatter-ui--on-notice (conn sender target text)
  "Handle NOTICE event for UI."
  (let* ((network (clatter-connection-network-id conn))
         (buf (or (clatter-get-buffer network target)
                  (clatter-get-server-buffer network)
                  (clatter-get-or-create-buffer network "*server*" 'server))))
    (clatter-ui-setup-buffer-if-needed buf)
    (clatter-insert-notice buf sender text)))

(defun clatter-ui--on-join (conn nick channel _account _realname)
  "Handle JOIN event for UI."
  (let* ((network (clatter-connection-network-id conn))
         (my-nick (clatter-connection-nick conn))
         (buf (clatter-get-or-create-buffer network channel)))
    (clatter-ui-setup-buffer-if-needed buf)
    (clatter-nick-add buf nick)
    (when (string-equal nick my-nick)
      (clatter-send conn (clatter-irc-names channel))
      (display-buffer buf))
    (clatter-insert-system buf (format "%s has joined %s" nick channel) 'join)))

(defun clatter-ui--on-part (conn nick channel message)
  "Handle PART event for UI."
  (let* ((network (clatter-connection-network-id conn))
         (buf (clatter-get-buffer network channel)))
    (when buf
      (clatter-nick-remove buf nick)
      (clatter-insert-system buf
                             (format "%s has left %s%s" nick channel
                                     (if message (format " (%s)" message) ""))
                             'part))))

(defun clatter-ui--on-quit (conn nick message)
  "Handle QUIT event for UI."
  (let ((network (clatter-connection-network-id conn)))
    (dolist (buf (clatter-channel-buffers network))
      (when (gethash (downcase nick)
                     (buffer-local-value 'clatter--nick-list buf))
        (clatter-nick-remove buf nick)
        (clatter-insert-system buf
                               (format "%s has quit%s" nick
                                       (if message (format " (%s)" message) ""))
                               'quit)))))

(defun clatter-ui--on-nick (conn old-nick new-nick)
  "Handle NICK change event for UI."
  (let ((network (clatter-connection-network-id conn)))
    (dolist (buf (clatter-channel-buffers network))
      (when (gethash (downcase old-nick)
                     (buffer-local-value 'clatter--nick-list buf))
        (clatter-nick-rename buf old-nick new-nick)
        (clatter-insert-system buf
                               (format "%s is now known as %s"
                                       old-nick new-nick)
                               'nick)))))

(defun clatter-ui--on-topic (conn channel _nick topic)
  "Handle TOPIC event for UI."
  (let* ((network (clatter-connection-network-id conn))
         (buf (clatter-get-buffer network channel)))
    (when buf
      (clatter-set-topic buf topic)
      (clatter-insert-system buf (format "Topic: %s" topic) 'topic))))

(defun clatter-ui--on-kick (conn channel nick kicked reason)
  "Handle KICK event for UI."
  (let* ((network (clatter-connection-network-id conn))
         (buf (clatter-get-buffer network channel)))
    (when buf
      (clatter-nick-remove buf kicked)
      (clatter-insert-system buf
                             (format "%s was kicked by %s%s" kicked nick
                                     (if reason (format " (%s)" reason) ""))
                             'kick))))

(defun clatter-ui--on-names (conn channel names-str)
  "Handle NAMES reply for UI."
  (let* ((network (clatter-connection-network-id conn))
         (buf (clatter-get-buffer network channel)))
    (when buf
      (dolist (entry (clatter-parse-names names-str))
        (clatter-nick-add buf (car entry) (cdr entry))))))

(defun clatter-ui--on-system (conn text)
  "Handle system message for UI."
  (let* ((network (clatter-connection-network-id conn))
         (buf (or (clatter-get-server-buffer network)
                  (clatter-get-or-create-buffer network "*server*" 'server))))
    (clatter-ui-setup-buffer-if-needed buf)
    (clatter-insert-system buf text)))

(defun clatter-ui--on-welcome (conn _nick)
  "Handle 001 welcome for UI."
  (let* ((network (clatter-connection-network-id conn))
         (buf (clatter-get-or-create-buffer network "*server*" 'server)))
    (clatter-ui-setup-buffer-if-needed buf)
    (clatter-insert-system buf
                           (format "Connected to %s as %s"
                                   network (clatter-connection-nick conn)))
    (display-buffer buf)))

(defun clatter-ui-setup-buffer-if-needed (buf)
  "Set up UI for BUF if not already done."
  (with-current-buffer buf
    (unless clatter--prompt-marker
      (clatter-ui-setup-buffer buf))))

;; --- Register hooks ---

(defun clatter-ui--on-away (conn nick away-msg)
  "Handle AWAY event for UI."
  (let ((network (clatter-connection-network-id conn)))
    (dolist (buf (clatter-channel-buffers network))
      (when (gethash (downcase nick)
                     (buffer-local-value 'clatter--nick-list buf))
        (clatter-insert-system buf
                               (if away-msg
                                   (format "%s is away: %s" nick away-msg)
                                 (format "%s is back" nick))
                               'away)))))

(defun clatter-ui--on-mode (conn target setter modes)
  "Handle MODE event for UI."
  (let* ((network (clatter-connection-network-id conn))
         (buf (clatter-get-buffer network target)))
    (when buf
      (clatter-insert-system buf
                             (format "%s sets mode %s"
                                     setter (string-join modes " "))
                             'mode))))

(defun clatter-ui--on-motd (conn lines)
  "Handle MOTD for UI: display in server buffer."
  (let* ((network (clatter-connection-network-id conn))
         (buf (or (clatter-get-server-buffer network)
                  (clatter-get-or-create-buffer network "*server*" 'server))))
    (clatter-ui-setup-buffer-if-needed buf)
    (clatter-insert-system buf "--- MOTD ---")
    (dolist (line lines)
      (clatter-insert-system buf line))
    (clatter-insert-system buf "--- End of MOTD ---")))

(defun clatter-ui--on-whois (_conn nick data)
  "Handle WHOIS reply for UI: display formatted info in current buffer."
  (let ((buf (current-buffer))
        (parts nil))
    (push (format "WHOIS %s (%s@%s)"
                  nick
                  (or (plist-get data :user) "?")
                  (or (plist-get data :host) "?"))
          parts)
    (when (plist-get data :realname)
      (push (format "  Realname: %s" (plist-get data :realname)) parts))
    (when (plist-get data :account)
      (push (format "  Account: %s" (plist-get data :account)) parts))
    (when (plist-get data :server)
      (push (format "  Server: %s (%s)"
                    (plist-get data :server)
                    (or (plist-get data :server-info) "")) parts))
    (when (plist-get data :channels)
      (push (format "  Channels: %s" (plist-get data :channels)) parts))
    (when (plist-get data :idle)
      (let* ((idle-secs (string-to-number (plist-get data :idle)))
             (idle-str (cond
                        ((< idle-secs 60) (format "%ds" idle-secs))
                        ((< idle-secs 3600) (format "%dm" (/ idle-secs 60)))
                        (t (format "%dh %dm" (/ idle-secs 3600)
                                   (/ (mod idle-secs 3600) 60))))))
        (push (format "  Idle: %s" idle-str) parts)))
    (when (plist-get data :signon)
      (let ((time (seconds-to-time
                   (string-to-number (plist-get data :signon)))))
        (push (format "  Signon: %s" (format-time-string "%F %T" time)) parts)))
    (when (plist-get data :secure)
      (push "  Secure connection (TLS)" parts))
    (when (plist-get data :oper)
      (push "  IRC Operator" parts))
    (when (plist-get data :away)
      (push (format "  Away: %s" (plist-get data :away)) parts))
    (dolist (line (nreverse parts))
      (clatter-insert-system buf line))))

(defun clatter-ui--on-disconnect (network-id event)
  "Handle disconnect for UI: show message in all NETWORK-ID buffers."
  (dolist (buf (clatter-all-buffers network-id))
    (clatter-insert-error buf
                           (format "Disconnected: %s" (string-trim event)))))

(defun clatter-ui--on-reconnect (network-id delay attempt)
  "Handle reconnect scheduling for UI: show in all NETWORK-ID buffers."
  (dolist (buf (clatter-all-buffers network-id))
    (clatter-insert-system buf
                            (format "Reconnecting in %ds (attempt %d)..."
                                    delay attempt))))

(defun clatter-ui--on-react (conn nick target emoji msgid)
  "Handle reaction: display EMOJI from NICK on message MSGID in TARGET."
  (let* ((network (clatter-connection-network-id conn))
         (buf (clatter-get-buffer network target)))
    (when (and buf (buffer-live-p buf))
      (with-current-buffer buf
        (save-excursion
          (goto-char (point-min))
          (let ((found nil))
            (while (and (not found) (not (eobp)))
              (when (equal msgid (get-text-property (point) 'clatter-msgid))
                (setq found (point)))
              (forward-line 1))
            (when found
              (goto-char found)
              (end-of-line)
              (let ((inhibit-read-only t)
                    (existing (get-text-property found 'clatter-reactions)))
                (unless existing (setq existing nil))
                ;; Add this reaction
                (let* ((key emoji)
                       (entry (assoc key existing))
                       (new-reactions
                        (if entry
                            (progn (setcdr entry (cons nick (cdr entry)))
                                   existing)
                          (append existing (list (list key nick)))))
                       (display (mapconcat
                                 (lambda (r)
                                   (format "%s %d" (car r) (length (cdr r))))
                                 new-reactions " ")))
                  ;; Remove old reaction overlay if any
                  (dolist (ov (overlays-at found))
                    (when (overlay-get ov 'clatter-reaction)
                      (delete-overlay ov)))
                  ;; Add new overlay showing reactions
                  (let ((ov (make-overlay (line-beginning-position)
                                          (line-end-position))))
                    (overlay-put ov 'clatter-reaction t)
                    (overlay-put ov 'after-string
                                 (concat "\n"
                                         (make-string clatter-nick-column-width ?\s)
                                         " "
                                         (propertize display 'face 'clatter-notice))))
                  ;; Store reactions as property
                  (add-text-properties found (1+ found)
                                       (list 'clatter-reactions new-reactions)))))))))))

(defun clatter-ui--on-batch-complete (conn batch-type target messages)
  "Handle completed batch: render MESSAGES for TARGET on CONN.
BATCH-TYPE is the IRC batch type (e.g. chathistory, znc.in/playback).
Renders a visual separator before and after history playback."
  (let* ((network (clatter-connection-network-id conn))
         (buf (clatter-get-buffer network target)))
    (when (and buf (buffer-live-p buf) messages)
      ;; Insert separator before history
      (let* ((sep-text (propertize
                        (concat " " (make-string 30 ?-) " history "
                                (make-string 30 ?-) " ")
                        'face 'shadow))
             (count (length messages)))
        (clatter--insert-message buf sep-text t)
        ;; Insert each message with dimmed style
        (dolist (msg messages)
          (let ((sender (plist-get msg :sender))
                (text (plist-get msg :text))
                (time (plist-get msg :time)))
            (clatter-insert-privmsg buf sender text conn time)))
        ;; Insert end separator
        (clatter--insert-message
         buf
         (propertize (format " %s end of history (%d messages) %s "
                             (make-string 20 ?-)
                             count
                             (make-string 20 ?-))
                     'face 'shadow)
         t)))))

;; --- CTCP replies ---

(defun clatter-ui--on-ctcp-reply (conn sender command reply-text)
  "Display CTCP reply from SENDER in the current clatter buffer.
COMMAND is the CTCP type (VERSION, PING, etc.), REPLY-TEXT is the response."
  (let* ((network (clatter-connection-network-id conn))
         (buf (or (clatter-get-buffer network sender)
                  (when-let* ((win (selected-window)))
                    (with-current-buffer (window-buffer win)
                      (when (derived-mode-p 'clatter-mode)
                        (current-buffer))))
                  (clatter-get-server-buffer network))))
    (when (and buf (buffer-live-p buf))
      (clatter-insert-system
       buf (format "CTCP %s reply from %s: %s" command sender reply-text)))))

;; --- Channel preview on hover (eldoc) ---

(defun clatter-ui--eldoc-function (callback &rest _)
  "Eldoc function for clatter buffers.
Shows channel topic and user count when point is on a #channel name.
Shows sender info when point is on a message."
  (let ((channel (clatter-ui--channel-at-point))
        (sender (get-text-property (point) 'clatter-sender))
        (msgid (get-text-property (point) 'clatter-msgid)))
    (cond
     ;; Channel name at point
     (channel
      (let* ((network clatter--network)
             (buf (clatter-get-buffer network channel)))
        (when (and buf (buffer-live-p buf))
          (let ((topic (buffer-local-value 'clatter--topic buf))
                (nick-list (buffer-local-value 'clatter--nick-list buf)))
            (let ((parts nil))
              (when (and nick-list (hash-table-p nick-list))
                (push (format "%d users" (hash-table-count nick-list)) parts))
              (when topic
                (push (if (> (length topic) 60)
                          (concat (substring topic 0 57) "...")
                        topic)
                      parts))
              (when parts
                (funcall callback
                         (mapconcat #'identity parts " - ")
                         :thing channel
                         :face 'clatter-notice)))))))
     ;; Message at point - show sender and msgid
     (sender
      (let ((text (get-text-property (point) 'clatter-text)))
        (funcall callback
                 (concat sender
                         (when msgid (format "  [msgid: %s]" msgid)))
                 :thing "message"
                 :face 'shadow))))
    nil))

(defun clatter-ui--channel-at-point ()
  "Return channel name at point, or nil.
Scans around point for a channel name starting with #, &, !, or +."
  (save-excursion
    (let ((orig (point)))
      ;; Move backward over valid channel-name chars
      (skip-chars-backward "a-zA-Z0-9_#&!+\\-\\[\\]\\\\`^{}|.")
      ;; Check if we're now on a channel prefix
      (when (memq (char-after) '(?# ?& ?! ?+))
        (let ((start (point)))
          (forward-char 1)
          (skip-chars-forward "a-zA-Z0-9_\\-\\[\\]\\\\`^{}|.")
          ;; Only return if original point was within the channel name
          (when (and (> (point) start)
                     (>= (point) orig)
                     (<= start orig))
            (buffer-substring-no-properties start (point))))))))

(defun clatter-ui--setup-eldoc ()
  "Set up eldoc for clatter buffers."
  (require 'eldoc)
  (when (boundp 'eldoc-documentation-functions)
    (add-hook 'eldoc-documentation-functions
              #'clatter-ui--eldoc-function nil t)
    (eldoc-mode 1)))

;; --- Typing indicators ---

(defcustom clatter-typing-timeout 6
  "Seconds after which a typing indicator expires.
If no update is received within this time, the indicator is cleared."
  :type 'integer
  :group 'clatter)

(defvar-local clatter--typing-nicks nil
  "Hash table of nicks currently typing in this buffer.
Keys are nick strings, values are timer objects.")

(defun clatter-ui--on-typing (conn nick target state)
  "Handle typing indicator from NICK in TARGET with STATE on CONN.
STATE is \"active\", \"paused\", or \"done\"."
  (let* ((network (clatter-connection-network-id conn))
         (buf (clatter-get-buffer network target)))
    (when (and buf (buffer-live-p buf))
      (with-current-buffer buf
        (unless clatter--typing-nicks
          (setq clatter--typing-nicks (make-hash-table :test 'equal)))
        ;; Cancel any existing timer for this nick
        (let ((existing-timer (gethash nick clatter--typing-nicks)))
          (when (timerp existing-timer)
            (cancel-timer existing-timer)))
        (if (string-equal state "done")
            ;; Remove typing indicator
            (remhash nick clatter--typing-nicks)
          ;; Set typing indicator with auto-expiry
          (let ((the-buf (current-buffer))
                (the-nick nick))
            (puthash nick
                     (run-at-time clatter-typing-timeout nil
                                  (lambda ()
                                    (when (buffer-live-p the-buf)
                                      (with-current-buffer the-buf
                                        (when clatter--typing-nicks
                                          (remhash the-nick clatter--typing-nicks))
                                        (force-mode-line-update)))))
                     clatter--typing-nicks)))
        (force-mode-line-update)))))

(defun clatter--typing-mode-line ()
  "Return a mode-line string showing who is typing, or nil."
  (when (and clatter--typing-nicks
             (> (hash-table-count clatter--typing-nicks) 0))
    (let ((nicks nil))
      (maphash (lambda (k _v) (push k nicks)) clatter--typing-nicks)
      (propertize
       (concat " "
               (pcase (length nicks)
                 (1 (format "%s is typing" (car nicks)))
                 (2 (format "%s and %s are typing" (car nicks) (cadr nicks)))
                 (_ (format "%d people typing" (length nicks))))
               "...")
       'face 'shadow))))

;; --- Outbound typing notifications ---

(defcustom clatter-send-typing t
  "Whether to send typing indicators to the server.
Requires the server to support the message-tags capability."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-typing-throttle 3
  "Minimum seconds between outbound typing notifications."
  :type 'number
  :group 'clatter)

(defvar-local clatter--typing-last-sent nil
  "Time (float) when the last typing notification was sent.")

(defun clatter--typing-capable-p ()
  "Return non-nil if we can send typing notifications."
  (and clatter-send-typing
       clatter--network
       clatter--target
       (not (string= clatter--target "*server*"))
       (let ((conn (clatter-get-connection clatter--network)))
         (and conn
              (member "message-tags"
                      (clatter-connection-cap-enabled conn))))))

(defun clatter--maybe-send-typing (&rest _)
  "Send a typing notification if in the input area and throttle allows."
  (when (and clatter--input-marker
             (>= (point) (marker-position clatter--input-marker))
             (clatter--typing-capable-p))
    (let ((now (float-time)))
      (when (or (null clatter--typing-last-sent)
                (> (- now clatter--typing-last-sent) clatter-typing-throttle))
        (setq clatter--typing-last-sent now)
        (let ((conn (clatter-get-connection clatter--network)))
          (when conn
            (clatter-send conn
                          (clatter-irc-typing clatter--target "active"))))))))

(defun clatter--send-typing-done ()
  "Send typing=done notification after sending a message."
  (when (clatter--typing-capable-p)
    (let ((conn (clatter-get-connection clatter--network)))
      (when conn
        (clatter-send conn
                      (clatter-irc-typing clatter--target "done"))))
    (setq clatter--typing-last-sent nil)))

(defun clatter--setup-outbound-typing (buffer)
  "Set up outbound typing notifications for BUFFER."
  (with-current-buffer buffer
    (add-hook 'post-self-insert-hook #'clatter--maybe-send-typing nil t)))

(defun clatter-ui-init ()
  "Register UI hooks.  Call this after loading clatter."
  (add-hook 'clatter-privmsg-hook #'clatter-ui--on-privmsg)
  (add-hook 'clatter-action-hook #'clatter-ui--on-action)
  (add-hook 'clatter-notice-hook #'clatter-ui--on-notice)
  (add-hook 'clatter-join-hook #'clatter-ui--on-join)
  (add-hook 'clatter-part-hook #'clatter-ui--on-part)
  (add-hook 'clatter-quit-hook #'clatter-ui--on-quit)
  (add-hook 'clatter-nick-hook #'clatter-ui--on-nick)
  (add-hook 'clatter-topic-hook #'clatter-ui--on-topic)
  (add-hook 'clatter-kick-hook #'clatter-ui--on-kick)
  (add-hook 'clatter-away-hook #'clatter-ui--on-away)
  (add-hook 'clatter-irc-mode-hook #'clatter-ui--on-mode)
  (add-hook 'clatter-names-hook #'clatter-ui--on-names)
  (add-hook 'clatter-system-hook #'clatter-ui--on-system)
  (add-hook 'clatter-welcome-hook #'clatter-ui--on-welcome)
  (add-hook 'clatter-disconnect-hook #'clatter-ui--on-disconnect)
  (add-hook 'clatter-reconnect-hook #'clatter-ui--on-reconnect)
  (add-hook 'clatter-motd-hook #'clatter-ui--on-motd)
  (add-hook 'clatter-whois-hook #'clatter-ui--on-whois)
  (add-hook 'clatter-react-hook #'clatter-ui--on-react)
  (add-hook 'clatter-batch-complete-hook #'clatter-ui--on-batch-complete)
  (add-hook 'clatter-ctcp-reply-hook #'clatter-ui--on-ctcp-reply)
  (add-hook 'clatter-typing-hook #'clatter-ui--on-typing)
  (add-hook 'clatter-mode-hook #'clatter-ui--setup-eldoc))

;; Auto-init when loaded
(clatter-ui-init)

(provide 'clatter-ui)

;;; clatter-ui.el ends here
