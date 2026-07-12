;;; test-ui.el --- Tests for clatter-ui.el -*- lexical-binding: t; -*-

;;; Code:

(require 'test-helper)
(require 'clatter-ui)
(require 'clatter-commands)
(require 'clatter-nicklist)
(require 'clatter-track)
(require 'clatter-notify)

;; --- Timestamp margins ---

(ert-deftest clatter-test-timestamp-side-left-margin ()
  "Left timestamp side configures the left margin."
  (let ((clatter-timestamp-side 'left)
        (clatter-timestamp-format "%H:%M"))
    (with-temp-buffer
      (clatter-mode)
      (should (= left-margin-width 6))
      (should (= right-margin-width 0)))))

(ert-deftest clatter-test-timestamp-side-right-margin ()
  "Right timestamp side configures the right margin."
  (let ((clatter-timestamp-side 'right)
        (clatter-timestamp-format "%H:%M"))
    (with-temp-buffer
      (clatter-mode)
      (should (= left-margin-width 0))
      (should (= right-margin-width 6)))))

(ert-deftest clatter-test-timestamp-side-sync-clears-stale-window-margin ()
  "Window margin sync clears the previous timestamp side."
  (let ((clatter-timestamp-format "%H:%M"))
    (with-temp-buffer
      (clatter-mode)
      (switch-to-buffer (current-buffer))
      (let ((clatter-timestamp-side 'right))
        (clatter--sync-window-margins)
        (should-not (car (window-margins)))
        (should (= (cdr (window-margins)) 6)))
      (let ((clatter-timestamp-side 'left))
        (clatter--sync-window-margins)
        (should (= (car (window-margins)) 6))
        (should-not (cdr (window-margins))))
      (let ((clatter-timestamp-side nil))
        (clatter--sync-window-margins)
        (should-not (car (window-margins)))
        (should-not (cdr (window-margins)))))))

;; --- Message filling ---

(ert-deftest clatter-test-insert-message-fills-at-fill-column ()
  "Inserted messages are hard-wrapped at `clatter-fill-column'."
  (let ((clatter-fill-column 40)
        (clatter-nick-column-width 7))
    (with-temp-buffer
      (clatter--insert-message
       (current-buffer)
       "<alice> this is a long message that should wrap around the configured fill column"
       t)
      (should
       (equal (buffer-string)
              "<alice> this is a long message that\n        should wrap around the\n        configured fill column\n")))))

;; --- Fool visibility ---

(ert-deftest clatter-test-fool-message-gets-dim-face ()
  "Messages with the fool invisibility category get `clatter-fool'."
  (with-temp-buffer
    (clatter--insert-message (current-buffer) "<fool> no thanks" t nil nil 'clatter-fool)
    (should (eq (get-text-property (point-min) 'invisible) 'clatter-fool))
    (should (memq 'clatter-fool (ensure-list (get-text-property (point-min) 'face))))))

(ert-deftest clatter-test-fool-face-takes-priority ()
  "The `clatter-fool' face is prepended to existing faces."
  (with-temp-buffer
    (clatter--insert-message
     (current-buffer)
     (propertize "<fool> no thanks" 'face 'clatter-notice)
     t nil nil 'clatter-fool)
    (should (equal (ensure-list (get-text-property (point-min) 'face))
                   '(clatter-fool clatter-notice)))))

(ert-deftest clatter-test-fool-visibility-seeds-buffer-invisibility ()
  "New clatter buffers hide fools unless `clatter-fools-visible' is non-nil."
  (let ((clatter-fools-visible nil))
    (with-temp-buffer
      (clatter-mode)
      (clatter-ui-setup-buffer (current-buffer))
      (should (memq 'clatter-fool buffer-invisibility-spec))))
  (let ((clatter-fools-visible t))
    (with-temp-buffer
      (clatter-mode)
      (clatter-ui-setup-buffer (current-buffer))
      (should-not (memq 'clatter-fool buffer-invisibility-spec)))))

(ert-deftest clatter-test-toggle-fools-updates-existing-buffer ()
  "Toggling fool visibility updates existing clatter buffers."
  (let ((old clatter-fools-visible))
    (unwind-protect
        (with-temp-buffer
          (clatter-mode)
          (setq buffer-invisibility-spec '(clatter-fool muted))
          (clatter-toggle-fools 1)
          (should clatter-fools-visible)
          (should-not (memq 'clatter-fool buffer-invisibility-spec))
          (clatter-toggle-fools -1)
          (should-not clatter-fools-visible)
          (should (memq 'clatter-fool buffer-invisibility-spec)))
      (setq clatter-fools-visible old))))

(ert-deftest clatter-test-suppress-preserves-fool-visibility ()
  "Generic suppression commands keep fool visibility independent."
  (let ((clatter-fools-visible nil))
    (with-temp-buffer
      (clatter-mode)
      (setq buffer-invisibility-spec '(clatter-fool muted))
      (clatter-cmd-suppress "none")
      (should (equal buffer-invisibility-spec '(clatter-fool)))
      (clatter-cmd-suppress "all")
      (should (memq 'clatter-fool buffer-invisibility-spec))))
  (let ((clatter-fools-visible t))
    (with-temp-buffer
      (clatter-mode)
      (setq buffer-invisibility-spec '(clatter-fool muted))
      (clatter-cmd-suppress "none")
      (should-not (memq 'clatter-fool buffer-invisibility-spec))
      (clatter-cmd-suppress "all")
      (should-not (memq 'clatter-fool buffer-invisibility-spec)))))

(ert-deftest clatter-test-suppress-cannot-desync-fool-visibility ()
  "Explicit generic suppressions cannot override the fool toggle state."
  (let ((clatter-fools-visible nil))
    (with-temp-buffer
      (clatter-mode)
      (setq buffer-invisibility-spec '(clatter-fool muted))
      (clatter-cmd-unsuppress "clatter-fool")
      (should (memq 'clatter-fool buffer-invisibility-spec))))
  (let ((clatter-fools-visible t))
    (with-temp-buffer
      (clatter-mode)
      (setq buffer-invisibility-spec '(muted))
      (clatter-cmd-suppress "clatter-fool")
      (should-not (memq 'clatter-fool buffer-invisibility-spec)))))

;; --- Channel-at-point detection ---

(ert-deftest clatter-test-channel-at-point-hash ()
  "Detects #channel at point."
  (with-temp-buffer
    (insert "hello #emacs world")
    (goto-char 8)  ; on the #
    (should (equal (clatter-ui--channel-at-point) "#emacs"))))

(ert-deftest clatter-test-channel-at-point-middle ()
  "Detects channel when cursor is in the middle."
  (with-temp-buffer
    (insert "see #emacs for help")
    (goto-char 10)  ; on 'a' in emacs
    (should (equal (clatter-ui--channel-at-point) "#emacs"))))

(ert-deftest clatter-test-channel-at-point-ampersand ()
  "Detects &channel prefix."
  (with-temp-buffer
    (insert "join &local")
    (goto-char 6)
    (should (equal (clatter-ui--channel-at-point) "&local"))))

(ert-deftest clatter-test-channel-at-point-none ()
  "Returns nil when no channel at point."
  (with-temp-buffer
    (insert "just normal text")
    (goto-char 5)
    (should-not (clatter-ui--channel-at-point))))

(ert-deftest clatter-test-channel-at-point-hyphen ()
  "Detects channels with hyphens."
  (with-temp-buffer
    (insert "try #system-crafters")
    (goto-char 10)
    (should (equal (clatter-ui--channel-at-point) "#system-crafters"))))

(ert-deftest clatter-test-channel-at-point-start-of-line ()
  "#channel at start of line."
  (with-temp-buffer
    (insert "#emacs is great")
    (goto-char 3)
    (should (equal (clatter-ui--channel-at-point) "#emacs"))))

;; --- Eldoc function ---

(ert-deftest clatter-test-eldoc-channel-with-nicks ()
  "Eldoc shows user count for known channel."
  (let* ((network "testnet")
         (channel "#emacs")
         (buf (clatter-get-or-create-buffer network channel)))
    (unwind-protect
        (progn
          ;; Populate nick list
          (with-current-buffer buf
            (let ((ht (make-hash-table :test 'equal)))
              (puthash "alice" "@" ht)
              (puthash "bob" "" ht)
              (puthash "carol" "+" ht)
              (setq-local clatter--nick-list ht)
              (setq-local clatter--topic "Welcome to #emacs")))
          ;; Simulate being in a clatter buffer on this network
          (with-temp-buffer
            (setq-local clatter--network network)
            (insert "#emacs")
            (goto-char 3)
            (let ((result nil))
              (clatter-ui--eldoc-function
               (lambda (text &rest _) (setq result text)))
              (should result)
              (should (string-match-p "3 users" result))
              (should (string-match-p "Welcome to #emacs" result)))))
      (kill-buffer buf)
      (clatter-remove-buffer network channel))))

(ert-deftest clatter-test-eldoc-channel-no-data ()
  "Eldoc returns nothing for unknown channel."
  (with-temp-buffer
    (setq-local clatter--network "testnet")
    (insert "#nonexistent")
    (goto-char 3)
    (let ((result nil))
      (clatter-ui--eldoc-function
       (lambda (text &rest _) (setq result text)))
      (should-not result))))

(ert-deftest clatter-test-eldoc-sender ()
  "Eldoc shows sender info on message."
  (with-temp-buffer
    (setq-local clatter--network "testnet")
    (insert (propertize "hello world"
                        'clatter-sender "alice"
                        'clatter-msgid "msg123"))
    (goto-char 3)
    (let ((result nil))
      (clatter-ui--eldoc-function
       (lambda (text &rest _) (setq result text)))
      (should result)
      (should (string-match-p "alice" result))
      (should (string-match-p "msg123" result)))))

;; --- Read state ---

(ert-deftest clatter-test-read-state-clear-persists-and-restores ()
  "Clearing activity persists and restores the latest read timestamp."
  (let* ((file (make-temp-file "clatter-read-state"))
         (clatter-read-state-enabled t)
         (clatter-read-state-file file)
         (clatter-read-state--table (make-hash-table :test 'equal))
         (clatter-read-state--loaded t)
         (clatter-read-state--save-timer nil)
         (time (encode-time 0 0 12 1 1 2026 t)))
    (unwind-protect
        (progn
          (with-temp-buffer
            (clatter-mode)
            (setq-local clatter--network "libera")
            (setq-local clatter--target "#emacs")
            (setq-local clatter--latest-message-time time)
            (clatter-clear-activity (current-buffer)))
          (clatter-read-state--save-now)
          (setq clatter-read-state--table (make-hash-table :test 'equal))
          (setq clatter-read-state--loaded nil)
          (with-temp-buffer
            (clatter-mode)
            (setq-local clatter--network "libera")
            (setq-local clatter--target "#emacs")
            (clatter-read-state-restore-buffer (current-buffer))
            (should (equal clatter--last-read-time time))))
      (when clatter-read-state--save-timer
        (cancel-timer clatter-read-state--save-timer))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest clatter-test-read-state-suppresses-read-history-activity ()
  "History at or before the last-read timestamp does not mark activity."
  (let ((conn (clatter-test-make-connection "libera" "me"))
        (buf nil)
        (clatter-read-state-enabled t))
    (unwind-protect
        (let* ((last-read (encode-time 0 0 12 1 1 2026 t))
               (old (encode-time 0 59 11 1 1 2026 t))
               (equal-time (encode-time 0 0 12 1 1 2026 t))
               (new (encode-time 0 1 12 1 1 2026 t)))
          (setq buf (clatter-get-or-create-buffer "libera" "#emacs" 'channel))
          (with-current-buffer buf
            (clatter-ui-setup-buffer buf)
            (setq-local clatter--last-read-time last-read))
          (with-temp-buffer
            (clatter-insert-privmsg buf "alice" "old" conn old)
            (clatter-insert-privmsg buf "alice" "equal" conn equal-time)
            (clatter-insert-privmsg buf "alice" "new" conn new))
          (with-current-buffer buf
            (should (= clatter--unread-count 1)))
          (let* ((infos (clatter-track--collect))
                 (info (cl-find buf infos
                                :key (lambda (entry)
                                       (plist-get entry :buffer)))))
            (should info)
            (should (= (plist-get info :unread) 1))))
      (clatter-test-cleanup)
      (when buf
        (clatter-remove-buffer "libera" "#emacs")
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

;; --- Typing mode-line ---

(ert-deftest clatter-test-typing-mode-line-empty ()
  "No typing nicks returns nil."
  (with-temp-buffer
    (setq-local clatter--typing-nicks nil)
    (should-not (clatter--typing-mode-line))))

(ert-deftest clatter-test-typing-mode-line-one ()
  "One nick typing shows name."
  (with-temp-buffer
    (setq-local clatter--typing-nicks (make-hash-table :test 'equal))
    (puthash "alice" t clatter--typing-nicks)
    (let ((result (clatter--typing-mode-line)))
      (should result)
      (should (string-match-p "alice is typing" result)))))

(ert-deftest clatter-test-typing-mode-line-two ()
  "Two nicks typing shows both names."
  (with-temp-buffer
    (setq-local clatter--typing-nicks (make-hash-table :test 'equal))
    (puthash "alice" t clatter--typing-nicks)
    (puthash "bob" t clatter--typing-nicks)
    (let ((result (clatter--typing-mode-line)))
      (should result)
      (should (string-match-p "are typing" result)))))

(ert-deftest clatter-test-typing-mode-line-many ()
  "Three+ nicks shows count."
  (with-temp-buffer
    (setq-local clatter--typing-nicks (make-hash-table :test 'equal))
    (puthash "alice" t clatter--typing-nicks)
    (puthash "bob" t clatter--typing-nicks)
    (puthash "carol" t clatter--typing-nicks)
    (let ((result (clatter--typing-mode-line)))
      (should result)
      (should (string-match-p "3 people typing" result)))))

;; --- Outbound typing throttle ---

(ert-deftest clatter-test-typing-capable-no-network ()
  "Not typing-capable without network."
  (with-temp-buffer
    (setq-local clatter--network nil)
    (setq-local clatter--target "#test")
    (should-not (clatter--typing-capable-p))))

(ert-deftest clatter-test-typing-capable-no-target ()
  "Not typing-capable without target."
  (with-temp-buffer
    (setq-local clatter--network "testnet")
    (setq-local clatter--target nil)
    (should-not (clatter--typing-capable-p))))

(ert-deftest clatter-test-typing-capable-server-buffer ()
  "Not typing-capable in server buffer."
  (with-temp-buffer
    (setq-local clatter--network "testnet")
    (setq-local clatter--target "*server*")
    (should-not (clatter--typing-capable-p))))

(ert-deftest clatter-test-typing-capable-disabled ()
  "Not typing-capable when disabled."
  (with-temp-buffer
    (setq-local clatter--network "testnet")
    (setq-local clatter--target "#test")
    (let ((clatter-send-typing nil))
      (should-not (clatter--typing-capable-p)))))

(ert-deftest clatter-test-typing-capable-with-caps ()
  "Typing-capable when message-tags enabled."
  (let ((conn (clatter-test-make-connection)))
    (unwind-protect
        (with-temp-buffer
          (setq-local clatter--network "testnet")
          (setq-local clatter--target "#test")
          (let ((clatter-send-typing t))
            (should (clatter--typing-capable-p))))
      (clatter-test-cleanup))))

;; --- Notifications and read state ---

(ert-deftest clatter-test-read-state-suppresses-read-notification ()
  "Messages at or before the last-read timestamp do not notify."
  (let ((clatter-notify-enabled t)
        (clatter-notify-on-mention t)
        (clatter-notify-current-buffer t)
        (clatter-notify-cooldown 0)
        (clatter-read-state-enabled t)
        (clatter--buffer-alist nil)
        (clatter-notify--last-times (make-hash-table :test 'equal))
        (sent nil)
        (conn (clatter-test-make-connection "testnet" "trev"))
        (last-read (encode-time 0 0 12 1 1 2026 t)))
    (unwind-protect
        (let ((buf (clatter-get-or-create-buffer "testnet" "#test")))
          (with-current-buffer buf
            (setq-local clatter--last-read-time last-read))
          (cl-letf (((symbol-function 'clatter-notify--send)
                     (lambda (title body)
                       (push (list title body) sent))))
            (clatter-notify--on-privmsg
             conn '("alice" nil nil) "#test" "trev: already saw this" last-read)
            (should-not sent)))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-read-state-allows-unread-notification ()
  "Messages after the last-read timestamp can still notify."
  (let ((clatter-notify-enabled t)
        (clatter-notify-on-mention t)
        (clatter-notify-current-buffer t)
        (clatter-notify-cooldown 0)
        (clatter-read-state-enabled t)
        (clatter--buffer-alist nil)
        (clatter-notify--last-times (make-hash-table :test 'equal))
        (sent nil)
        (conn (clatter-test-make-connection "testnet" "trev"))
        (last-read (encode-time 0 0 12 1 1 2026 t))
        (unread-time (encode-time 1 0 12 1 1 2026 t)))
    (unwind-protect
        (let ((buf (clatter-get-or-create-buffer "testnet" "#test")))
          (with-current-buffer buf
            (setq-local clatter--last-read-time last-read))
          (cl-letf (((symbol-function 'clatter-notify--send)
                     (lambda (title body)
                       (push (list title body) sent))))
            (clatter-notify--on-privmsg
             conn '("alice" nil nil) "#test" "trev: new message" unread-time)
            (should sent)))
      (clatter-test-cleanup))))

;; --- Nicklist hooks registered ---

(ert-deftest clatter-test-nicklist-hooks-registered ()
  "Nicklist auto-refresh hooks are registered."
  (should (memq #'clatter-nicklist--on-join (default-value 'clatter-join-hook)))
  (should (memq #'clatter-nicklist--on-part (default-value 'clatter-part-hook)))
  (should (memq #'clatter-nicklist--on-quit (default-value 'clatter-quit-hook)))
  (should (memq #'clatter-nicklist--on-nick (default-value 'clatter-nick-hook)))
  (should (memq #'clatter-nicklist--on-names (default-value 'clatter-names-hook))))

(provide 'test-ui)

;;; test-ui.el ends here
