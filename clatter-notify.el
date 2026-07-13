;;; clatter-notify.el --- Desktop notifications -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Glenn Thompson
;; Author: Glenn Thompson <glenn@paren.works>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Desktop notifications for clatter.el with IRC-specific rules.
;; Supports mentions, DMs, keyword matching, muted channels,
;; and current-buffer suppression.
;; Uses Emacs-native notification APIs, with terminal-notifier retained
;; as the macOS backend.

;;; Code:

(require 'cl-lib)
(require 'xml)
(require 'clatter-config)
(require 'clatter-protocol)
(require 'clatter-connection)
(require 'clatter-model)
(require 'clatter-pals)

(declare-function notifications-notify "notifications" (&rest params))
(declare-function android-notifications-notify "androidselect.c" (&rest params))
(declare-function haiku-notifications-notify "haikuselect.c" (&rest params))
(declare-function w32-notification-notify "w32notify.c" (&rest params))

;; --- Configuration ---

(defcustom clatter-notify-enabled t
  "Enable desktop notifications."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-notify-on-mention t
  "Send notification when your nick is mentioned."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-notify-on-dm t
  "Send notification for private messages."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-notify-on-invite t
  "Send notification for channel invitations."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-notify-on-keyword t
  "Send notification when a keyword from `clatter-notify-keywords' matches."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-notify-keywords nil
  "List of keywords that trigger notifications.
Case-insensitive matching.
Example: (\"parenworks\" \"fluxion\" \"lattice\" \"sextant\")"
  :type '(repeat string)
  :group 'clatter)

(defcustom clatter-notify-muted-channels nil
  "List of channels that never trigger notifications.
Example: (\"#spam\" \"#bots\")"
  :type '(repeat (choice (string :tag "Channel Only")
                         (cons :tag "Channel and Network"
                               (string :tag "Channel")
                               (string :tag "Network"))))
  :group 'clatter)

(defcustom clatter-notify-muted-nicks nil
  "List of patterns or pattern-network pairs that never trigger
notifications.
Useful for bots.
Example: (\"ChanServ\" (\"SaslServ\". \"Libera.Chat\") \"NickServ\" \"github-bot\")"
  :type '(repeat (choice (string :tag "Pattern Only")
                         (cons :tag "Pattern and Network"
                               (string :tag "Pattern")
                               (string :tag "Network"))))
  :group 'clatter)

(defcustom clatter-notify-current-buffer nil
  "If nil, suppress notifications for the currently visible buffer.
Most users do not want notifications for messages they can already see."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-notify-timeout 5000
  "Notification timeout in milliseconds."
  :type 'integer
  :group 'clatter)

(defcustom clatter-notify-icon nil
  "Path to icon for notifications, or nil for default."
  :type '(choice (const nil) string)
  :group 'clatter)

(defcustom clatter-notify-urgency 'normal
  "Notification urgency level."
  :type '(choice (const :tag "Low" low)
                 (const :tag "Normal" normal)
                 (const :tag "Critical" critical))
  :group 'clatter)

(defcustom clatter-notify-sound nil
  "If non-nil, play this sound file with notifications.
Set to t for system default, or a path to a specific sound."
  :type '(choice (const :tag "None" nil)
                 (const :tag "System default" t)
                 string)
  :group 'clatter)

(defcustom clatter-notify-max-length 80
  "Maximum length of message text in notifications."
  :type 'integer
  :group 'clatter)

(defcustom clatter-notify-echo-area nil
  "When non-nil, echo a notification after native delivery fails.
This fallback is disabled by default so mentions and DMs do not spam
the echo area when no desktop notification service is available."
  :type 'boolean
  :group 'clatter)

;; --- Smart notification rules ---

(defcustom clatter-notify-rules nil
  "Per-channel/nick notification rules.
Each entry is a plist with:
  :target   - channel or nick name (string, case-insensitive)
  :level    - notification level: all, mentions, dms, none
  :schedule - optional (START-HOUR . END-HOUR) for active hours (24h)
              Notifications only fire during these hours.
              nil means always active.

Example:
  \\='((:target \"#emacs\" :level mentions :schedule (9 . 22))
    (:target \"#bots\" :level none)
    (:target \"#systemcrafters\" :level all)
    (:target \"friend\" :level all))"
  :type '(repeat plist)
  :group 'clatter)

(defcustom clatter-notify-dm-priority 'always
  "Priority for DM notifications.
The value is one of:
always - always notify for DMs regardless of rules
rules  - apply rules to DMs as well
never  - never notify for DMs"
  :type '(choice (const :tag "Always notify" always)
                 (const :tag "Follow rules" rules)
                 (const :tag "Never" never))
  :group 'clatter)

(defun clatter-notify--find-rule (target)
  "Find the notification rule matching TARGET."
  (cl-find-if (lambda (rule)
                (string-equal-ignore-case
                 (plist-get rule :target) target))
              clatter-notify-rules))

(defun clatter-notify--schedule-active-p (schedule)
  "Return non-nil if SCHEDULE is currently active.
SCHEDULE is (START-HOUR . END-HOUR) or nil (always active)."
  (if (null schedule)
      t
    (let* ((hour (string-to-number (format-time-string "%H")))
           (start (car schedule))
           (end (cdr schedule)))
      (if (<= start end)
          (and (>= hour start) (< hour end))
        ;; Wraps midnight (e.g. 22 . 8)
        (or (>= hour start) (< hour end))))))

(defun clatter-notify--rule-allows-p (target reason)
  "Return non-nil if smart rules allow notification for TARGET and REASON."
  (let ((rule (clatter-notify--find-rule target)))
    (if (null rule)
        t  ;; No rule = use default behavior
      (let ((level (plist-get rule :level))
            (schedule (plist-get rule :schedule)))
        (and (clatter-notify--schedule-active-p schedule)
             (pcase level
               ('all t)
               ('mentions (memq reason '(mention keyword)))
               ('dms (eq reason 'dm))
               ('none nil)
               (_ t)))))))

;; --- Rate limiting ---

(defcustom clatter-notify-cooldown 3
  "Minimum seconds between notifications from the same source.
Prevents notification spam from rapid messages."
  :type 'integer
  :group 'clatter)

(defvar clatter-notify--last-times (make-hash-table :test 'equal)
  "Hash table of source -> last notification time for rate limiting.")

(defun clatter-notify--rate-ok-p (source)
  "Return non-nil if SOURCE has not been notified within the cooldown period."
  (let* ((last (gethash source clatter-notify--last-times 0))
         (now (float-time))
         (elapsed (- now last)))
    (when (>= elapsed clatter-notify-cooldown)
      (puthash source now clatter-notify--last-times)
      t)))

;; --- Notification dispatch ---

(defun clatter-notify--plain-text (text)
  "Return notification TEXT without properties or IRC formatting."
  (clatter-strip-irc-formatting (substring-no-properties text)))

(defun clatter-notify--truncate (text)
  "Return plain notification TEXT truncated to the configured limit."
  (let* ((plain (clatter-notify--plain-text text))
         (limit (max 0 clatter-notify-max-length)))
    (cond
     ((<= (length plain) limit) plain)
     ((zerop limit) "")
     (t (concat (substring plain 0 (1- limit)) "…")))))

(defun clatter-notify--action-params (buffer)
  "Return native notification action parameters for BUFFER."
  (when (buffer-live-p buffer)
    (list :actions '("default" "Open buffer")
          :on-action (lambda (&rest _)
                       (when (buffer-live-p buffer)
                         (pop-to-buffer buffer))))))

(defun clatter-notify--dbus-sound-params ()
  "Return D-Bus sound parameters for the configured notification sound."
  (cond
   ((eq clatter-notify-sound t)
    (list :sound-name "message-new-instant"))
   ((and (stringp clatter-notify-sound)
         (file-exists-p clatter-notify-sound))
    (list :sound-file (expand-file-name clatter-notify-sound)))))

(defun clatter-notify--dbus-send (title body buffer)
  "Send TITLE and BODY over D-Bus, associated with BUFFER."
  (condition-case nil
      (when (require 'notifications nil t)
        (let ((params (list :title (xml-escape-string title t)
                            :body (xml-escape-string body t)
                            :app-name "CLatter"
                            :timeout clatter-notify-timeout
                            :urgency clatter-notify-urgency
                            :category "im.received")))
          (when clatter-notify-icon
            (setq params (append params (list :app-icon clatter-notify-icon))))
          (setq params (append params
                               (clatter-notify--dbus-sound-params)
                               (clatter-notify--action-params buffer)))
          ;; `notifications-notify' demotes D-Bus failures through `message'.
          ;; Keep automatic delivery silent and let our own fallback policy
          ;; decide whether anything belongs in the echo area.
          (let ((inhibit-message t)
                (message-log-max nil))
            (apply #'notifications-notify params))))
    (error nil)))

(defun clatter-notify--android-send (title body buffer)
  "Send an Android notification with TITLE and BODY for BUFFER."
  (when (fboundp 'android-notifications-notify)
    (apply #'android-notifications-notify
           (append (list :title title
                         :body body
                         :timeout clatter-notify-timeout
                         :urgency clatter-notify-urgency
                         :group "CLatter")
                   (clatter-notify--action-params buffer)))))

(defun clatter-notify--haiku-send (title body)
  "Send a Haiku notification with TITLE and BODY."
  (when (fboundp 'haiku-notifications-notify)
    (let ((params (list :title title :body body
                        :urgency clatter-notify-urgency)))
      (when clatter-notify-icon
        (setq params (append params (list :app-icon clatter-notify-icon))))
      (apply #'haiku-notifications-notify params))))

(defun clatter-notify--w32-send (title body)
  "Send a native Windows notification with TITLE and BODY."
  (when (fboundp 'w32-notification-notify)
    (let ((params (list :title title :body body
                        :level (if (eq clatter-notify-urgency 'critical)
                                   'error
                                 'info))))
      (when clatter-notify-icon
        (setq params (append params (list :icon clatter-notify-icon))))
      (apply #'w32-notification-notify params))))

(defun clatter-notify--mac-send (title body)
  "Send a macOS notification with TITLE and BODY."
  (when (executable-find "terminal-notifier")
    (let ((args (list "clatter-notify" nil
                      "terminal-notifier"
                      "-title" title
                      "-message" body
                      "-sender" "org.gnu.Emacs"
                      "-group" "clatter")))
      (when (eq clatter-notify-sound t)
        (setq args (append args '("-sound" "default"))))
      (apply #'start-process args))))

(defun clatter-notify--send-native (title body buffer)
  "Send TITLE and BODY through the native backend for BUFFER."
  (condition-case nil
      (cond
       ((featurep 'android) (clatter-notify--android-send title body buffer))
       ((featurep 'haiku) (clatter-notify--haiku-send title body))
       ((eq system-type 'windows-nt) (clatter-notify--w32-send title body))
       ((eq system-type 'darwin) (clatter-notify--mac-send title body))
       (t (clatter-notify--dbus-send title body buffer)))
    (error nil)))

(defun clatter-notify--send (title body &optional buffer)
  "Send a desktop notification with TITLE and BODY for BUFFER.
Return the backend result, `echo' for an echo-area fallback, or nil."
  (let ((plain-title (clatter-notify--plain-text title))
        (plain-body (clatter-notify--truncate body)))
    (or (clatter-notify--send-native plain-title plain-body buffer)
        (when clatter-notify-echo-area
          (message "[CLatter] %s: %s" plain-title plain-body)
          'echo))))

;; --- Notification logic ---

(defun clatter-notify--should-notify-p (sender target text conn &optional server-time)
  "Determine if SENDER's TEXT to TARGET on CONN should notify.
Returns a symbol indicating the reason: mention, dm, keyword, or nil."
  (when clatter-notify-enabled
    (let* ((my-nick (clatter-connection-nick conn))
           (sender-nick (clatter-prefix-nick sender))
           (network (clatter-connection-network-id conn))
           (is-self (and my-nick (string-equal-ignore-case sender-nick my-nick)))
           (channel-prefixes (let ((isup (clatter-connection-isupport conn)))
                               (or (and isup (gethash "CHANTYPES" isup))
                                   "#&!")))
           (is-channel (and target (seq-contains-p channel-prefixes (aref target 0))))
           (is-dm (not is-channel))
           (buf (clatter-get-buffer
                 (clatter-connection-network-id conn)
                 (if is-dm sender-nick target)))
           (is-current (and buf (eq buf (window-buffer (selected-window)))
                            (frame-focus-state (window-frame (selected-window)))))
           (is-muted-channel (and is-channel
                                  (clatter-notify-muted-channel-p target network)))
           (is-muted-nick (or (clatter-notify-muted-p sender network)
                              (clatter-muted-p sender network)))
           (is-reply-to-me (get-text-property 0 'clatter-reply-to-me text))
           (text-lower (downcase (or text ""))))
      (let ((reason
             (cond
              ;; Already-read history has already been seen elsewhere.
              ((and buf (clatter-read-state-message-read-p buf server-time)) nil)
              ;; Own messages (echo-message)
              (is-self nil)
              ;; Muted
              (is-muted-channel nil)
              (is-muted-nick nil)
              ;; Current buffer suppression
              ((and is-current (not clatter-notify-current-buffer)) nil)
              ;; DM
              ((and is-dm clatter-notify-on-dm) 'dm)
              ;; Mention
              ((and clatter-notify-on-mention
                    (or is-reply-to-me
                        (and my-nick
                             (clatter-mention-p (downcase my-nick) text-lower))))
               'mention)
              ;; Keyword
              ((and clatter-notify-on-keyword
                    clatter-notify-keywords
                    (cl-some (lambda (kw)
                               (string-match-p (regexp-quote (downcase kw)) text-lower))
                             clatter-notify-keywords))
               'keyword)
              (t nil))))
        ;; Apply smart rules
        (when reason
          ;; DM priority override
          (cond
           ((and (eq reason 'dm) (eq clatter-notify-dm-priority 'never))
            (setq reason nil))
           ((and (eq reason 'dm) (eq clatter-notify-dm-priority 'always))
            ;; Always notify for DMs, skip rule check
            nil)
           ;; Check per-target rules
           (t
            (unless (clatter-notify--rule-allows-p
                     (if is-dm sender-nick target) reason)
              (setq reason nil)))))
        reason))))

(defun clatter-notify--format-title (reason sender target)
  "Format notification title based on REASON, SENDER, and TARGET."
  (pcase reason
    ('dm (format "DM from %s" sender))
    ('mention (format "Mention from %s in %s" sender target))
    ('invite (format "Invite from %s" sender))
    ('keyword (format "Keyword from %s in %s" sender target))
    (_ (format "%s in %s" sender target))))

;; --- Hooks ---

(defun clatter-notify--on-privmsg (conn sender target text &optional server-time)
  "Notify for SENDER's PRIVMSG TEXT to TARGET on CONN."
  (let ((reason (clatter-notify--should-notify-p sender target text conn server-time)))
    (when reason
      (let* ((channel-prefixes (let ((isup (clatter-connection-isupport conn)))
                                 (or (and isup (gethash "CHANTYPES" isup))
                                     "#&!")))
             (is-channel (and target (seq-contains-p channel-prefixes (aref target 0))))
             (sender-nick (clatter-prefix-nick sender))
             (source (if is-channel target sender-nick)))
        (when (clatter-notify--rate-ok-p source)
          (clatter-notify--send
           (clatter-notify--format-title reason sender-nick target)
           text
           (clatter-get-buffer (clatter-connection-network-id conn)
                               (if is-channel target sender-nick))))))))

(defun clatter-notify--on-action (conn sender target text &optional server-time)
  "Notify for SENDER's ACTION TEXT to TARGET on CONN."
  (let ((reason (clatter-notify--should-notify-p sender target text conn server-time)))
    (when reason
      (let* ((channel-prefixes (let ((isup (clatter-connection-isupport conn)))
                                 (or (and isup (gethash "CHANTYPES" isup))
                                     "#&!")))
             (is-channel (and target (seq-contains-p channel-prefixes (aref target 0))))
             (sender-nick (clatter-prefix-nick sender))
             (source (if is-channel target sender-nick)))
        (when (clatter-notify--rate-ok-p source)
          (clatter-notify--send
           (clatter-notify--format-title reason sender-nick target)
           (format "* %s" text)
           (clatter-get-buffer (clatter-connection-network-id conn)
                               (if is-channel target sender-nick))))))))

(defun clatter-notify--on-invite (conn sender nick channel)
  "Notify when SENDER invites NICK to CHANNEL on CONN."
  (when (and clatter-notify-on-invite
             (string-equal (clatter-connection-nick conn) nick))
    (let ((sender-nick (clatter-prefix-nick sender)))
      (when (clatter-notify--rate-ok-p sender-nick)
        (clatter-notify--send
         (clatter-notify--format-title 'invite sender-nick nick)
         (format "Invitation to join %s" channel)
         (or (clatter-get-buffer (clatter-connection-network-id conn) channel)
             (clatter-get-server-buffer
              (clatter-connection-network-id conn))))))))

;; --- Interactive commands ---

(defun clatter-notify-toggle ()
  "Toggle desktop notifications on/off."
  (interactive)
  (setq clatter-notify-enabled (not clatter-notify-enabled))
  (message "[clatter-notify] Notifications %s"
           (if clatter-notify-enabled "enabled" "disabled")))

(defun clatter-notify-add-keyword (keyword)
  "Add KEYWORD to the notification keyword list."
  (interactive "sKeyword to add: ")
  (unless (member keyword clatter-notify-keywords)
    (push keyword clatter-notify-keywords)
    (message "Added keyword: %s" keyword)))

(defun clatter-notify-remove-keyword (keyword)
  "Remove KEYWORD from the notification keyword list."
  (interactive
   (list (completing-read "Remove keyword: " clatter-notify-keywords)))
  (setq clatter-notify-keywords (delete keyword clatter-notify-keywords))
  (message "Removed keyword: %s" keyword))

(defun clatter-notify-test ()
  "Send a test notification."
  (interactive)
  (unless (clatter-notify--send "CLatter Test" "Notifications are working")
    (message "[clatter-notify] Native notification delivery failed")))

(defun clatter-notify-mute-nick (pattern)
  "Add PATTERN to the muted nicks list."
  (interactive "sNick to mute: ")
  (unless (or (seq-contains-p pattern ?\*)
              (seq-contains-p pattern ?\?)
              (seq-contains-p pattern ?\[))
    (setq pattern (format "%s!*@*" pattern)))
  (unless (member pattern clatter-notify-muted-nicks)
    (push pattern clatter-notify-muted-nicks)
    (message "Muted notifications from %s" pattern)))

(defun clatter-notify-unmute-nick (pattern)
  "Remove PATTERN from the muted nicks list."
  (interactive
   (list (completing-read "Unmute nick: " clatter-notify-muted-nicks)))
  (unless (or (seq-contains-p pattern ?\*)
              (seq-contains-p pattern ?\?)
              (seq-contains-p pattern ?\[))
    (setq pattern (format "%s!*@*" pattern)))
  (setq clatter-notify-muted-nicks (delete pattern clatter-notify-muted-nicks))
  (message "Unmuted notifications from %s" pattern))

(defun clatter-notify-muted-p (sender &optional network)
  "Returns whether SENDER or the (SENDER . NETWORK) pair is muted."
  (when (clatter-prefix-p sender)
    (setq sender (clatter-join-prefix sender)))
  (and sender
       (progn
         (setq sender (downcase sender))
         (cl-some (lambda (elt)
                    (pcase elt
                      (`(,pat . ,in)
                       (and (string-match-p (wildcard-to-regexp (downcase pat)) sender)
                            network (string-equal network in) t))
                      (pat
                       (string-match-p (wildcard-to-regexp (downcase pat)) sender))))
                  clatter-notify-muted-nicks))))

(defun clatter-notify-muted-channel-p (channel &optional network)
  "Returns whether CHANNEL or the (CHANNEL . NETWORK) pair
is in the muted channels list."
  (cl-some (lambda (elt)
             (pcase elt
               (`(,c . ,in)
                (and (string-equal-ignore-case c channel)
                     network (string-equal network in) t))
               (c
                (string-equal-ignore-case c channel))))
           clatter-notify-muted-channels))

;; --- Enable/disable ---

(defun clatter-notify-enable ()
  "Enable notification hooks."
  (interactive)
  (add-hook 'clatter-privmsg-hook #'clatter-notify--on-privmsg)
  (add-hook 'clatter-action-hook #'clatter-notify--on-action)
  (add-hook 'clatter-invite-hook #'clatter-notify--on-invite)
  (when (called-interactively-p 'interactive)
    (message "[clatter-notify] Notification hooks enabled")))

(defun clatter-notify-disable ()
  "Disable notification hooks."
  (interactive)
  (remove-hook 'clatter-privmsg-hook #'clatter-notify--on-privmsg)
  (remove-hook 'clatter-action-hook #'clatter-notify--on-action)
  (remove-hook 'clatter-invite-hook #'clatter-notify--on-invite)
  (message "[clatter-notify] Notification hooks disabled"))

;; Enabled by `clatter-setup' when `clatter-notify-enabled' is non-nil,
;; so that merely loading this file has no side effects.

(provide 'clatter-notify)

;;; clatter-notify.el ends here
