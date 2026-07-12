;;; test-notify.el --- Native desktop notification tests -*- lexical-binding: t; -*-

;;; Code:

(require 'test-helper)
(require 'clatter-notify)

(ert-deftest clatter-notify-dbus-sanitizes-and-opens-buffer ()
  "D-Bus delivery strips IRC controls, escapes markup, and opens its buffer."
  (let ((clatter-notify-max-length 200)
        (clatter-notify-timeout 1234)
        (clatter-notify-urgency 'critical)
        (clatter-notify-sound t)
        (real-require (symbol-function 'require))
        captured opened delivery-silenced)
    (with-temp-buffer
      (let ((buffer (current-buffer)))
        (cl-letf (((symbol-function 'require)
                   (lambda (feature &optional filename noerror)
                     (if (eq feature 'notifications)
                         t
                       (funcall real-require feature filename noerror))))
                  ((symbol-function 'notifications-notify)
                   (lambda (&rest params)
                     (setq captured params
                           delivery-silenced
                           (and inhibit-message (null message-log-max)))
                     42))
                  ((symbol-function 'start-process)
                   (lambda (&rest _) (ert-fail "External notifier used")))
                  ((symbol-function 'pop-to-buffer)
                   (lambda (target &rest _) (setq opened target))))
          (should (= (clatter-notify--send "A & B" "\x02<tag>\x02" buffer) 42))
          (should (equal (plist-get captured :title) "A &amp; B"))
          (should (equal (plist-get captured :body) "&lt;tag&gt;"))
          (should-not (plist-member captured :transient))
          (should (= (plist-get captured :timeout) 1234))
          (should (eq (plist-get captured :urgency) 'critical))
          (should (equal (plist-get captured :sound-name)
                         "message-new-instant"))
          (should delivery-silenced)
          (funcall (plist-get captured :on-action) 42 "default")
          (should (eq opened buffer)))))))

(ert-deftest clatter-notify-dbus-maps-sound-file ()
  "A configured sound file is passed to the native D-Bus backend."
  (let ((sound (make-temp-file "clatter-sound")))
    (unwind-protect
        (let ((clatter-notify-sound sound)
              (real-require (symbol-function 'require))
              captured)
          (cl-letf (((symbol-function 'require)
                     (lambda (feature &optional filename noerror)
                       (if (eq feature 'notifications)
                           t
                         (funcall real-require feature filename noerror))))
                    ((symbol-function 'notifications-notify)
                     (lambda (&rest params) (setq captured params) 1)))
            (should (clatter-notify--dbus-send "title" "body" nil))
            (should (equal (plist-get captured :sound-file)
                           (expand-file-name sound)))))
      (delete-file sound))))

(ert-deftest clatter-notify-failure-is-silent-unless-enabled ()
  "Native failures only reach the echo area when explicitly configured."
  (let (messages)
    (cl-letf (((symbol-function 'clatter-notify--send-native)
               (lambda (&rest _) nil))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (let ((clatter-notify-echo-area nil))
        (should-not (clatter-notify--send "title" "body"))
        (should-not messages))
      (let ((clatter-notify-echo-area t))
        (should (eq (clatter-notify--send "title" "body") 'echo))
        (should (equal (car messages) "[CLatter] title: body"))))))

(ert-deftest clatter-notify-test-reports-delivery-failure ()
  "The explicit notification test reports failure despite silent fallback."
  (let (reported)
    (cl-letf (((symbol-function 'clatter-notify--send)
               (lambda (&rest _) nil))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq reported (apply #'format format-string args)))))
      (clatter-notify-test)
      (should (equal reported
                     "[clatter-notify] Native notification delivery failed")))))

(ert-deftest clatter-notify-routes-native-platforms ()
  "Dispatcher selects Android, Haiku, Windows, and macOS backends."
  (let ((real-featurep (symbol-function 'featurep))
        platform)
    (cl-letf (((symbol-function 'clatter-notify--android-send)
               (lambda (&rest _) 'android))
              ((symbol-function 'clatter-notify--haiku-send)
               (lambda (&rest _) 'haiku))
              ((symbol-function 'clatter-notify--w32-send)
               (lambda (&rest _) 'windows))
              ((symbol-function 'clatter-notify--mac-send)
               (lambda (&rest _) 'mac))
              ((symbol-function 'featurep)
               (lambda (feature &optional subfeature)
                 (if (memq feature '(android haiku))
                     (eq feature platform)
                   (funcall real-featurep feature subfeature)))))
      (setq platform 'android)
      (should (eq (clatter-notify--send-native "t" "b" nil) 'android))
      (setq platform 'haiku)
      (should (eq (clatter-notify--send-native "t" "b" nil) 'haiku))
      (setq platform nil)
      (let ((system-type 'windows-nt))
        (should (eq (clatter-notify--send-native "t" "b" nil) 'windows)))
      (let ((system-type 'darwin))
        (should (eq (clatter-notify--send-native "t" "b" nil) 'mac))))))

(ert-deftest clatter-notify-macos-uses-terminal-notifier-only ()
  "The macOS backend retains terminal-notifier and native sound selection."
  (let ((clatter-notify-sound t)
        captured)
    (cl-letf (((symbol-function 'executable-find)
               (lambda (program) (and (equal program "terminal-notifier") program)))
              ((symbol-function 'start-process)
               (lambda (&rest args) (setq captured args) 'process)))
      (should (eq (clatter-notify--mac-send "title" "body") 'process))
      (should (equal (nth 2 captured) "terminal-notifier"))
      (should (member "-sound" captured))
      (should-not (member "notify-send" captured))
      (should-not (member "paplay" captured)))))

(ert-deftest clatter-notify-truncates-plain-text-safely ()
  "Notification truncation counts plain text and handles tiny limits."
  (let ((clatter-notify-max-length 8))
    (should (equal (clatter-notify--truncate "abcdefghijk") "abcdefg…")))
  (let ((clatter-notify-max-length 2))
    (should (equal (clatter-notify--truncate "abcdef") "a…")))
  (let ((clatter-notify-max-length 1))
    (should (equal (clatter-notify--truncate "abcdef") "…")))
  (let ((clatter-notify-max-length 0))
    (should (equal (clatter-notify--truncate "abcdef") ""))))

(ert-deftest clatter-notify-hook-formats-sender-in-title ()
  "Message hooks put the sender in the title, not the body."
  (let ((conn (clatter-test-make-connection "testnet" "trev"))
        (clatter-notify-cooldown 0)
        captured-title
        captured-body
        captured-buffer)
    (unwind-protect
        (let ((buffer (clatter-get-or-create-buffer "testnet" "#test")))
          (cl-letf (((symbol-function 'clatter-notify--should-notify-p)
                     (lambda (&rest _) 'mention))
                    ((symbol-function 'clatter-notify--send)
                     (lambda (title body &optional target-buffer)
                       (setq captured-title title
                             captured-body body)
                       (setq captured-buffer target-buffer)
                       t)))
            (clatter-notify--on-privmsg
             conn '("alice" nil nil) "#test" "trev: hello" nil)
            (should (equal captured-title "Mention from alice in #test"))
            (should (equal captured-body "trev: hello"))
            (should-not (string-match-p "alice" captured-body))
            (should (eq captured-buffer buffer))))
      (clatter-test-cleanup))))

(ert-deftest clatter-notify-action-and-invite-omit-sender-from-body ()
  "Action and invite titles carry the sender without repeating it."
  (let ((conn (clatter-test-make-connection "testnet" "trev"))
        (clatter-notify-cooldown 0)
        calls)
    (cl-letf (((symbol-function 'clatter-notify--should-notify-p)
               (lambda (&rest _) 'mention))
              ((symbol-function 'clatter-notify--send)
               (lambda (title body &optional _buffer)
                 (push (cons title body) calls)
                 t)))
      (clatter-notify--on-action
       conn '("alice" nil nil) "#test" "waves" nil)
      (clatter-notify--on-invite
       conn '("bob" nil nil) "trev" "#clatter")
      (should (equal (nreverse calls)
                     '(("Mention from alice in #test" . "* waves")
                       ("Invite from bob" . "Invitation to join #clatter")))))))

(provide 'test-notify)

;;; test-notify.el ends here
