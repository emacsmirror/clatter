;;; test-track-styling.el --- Tracker presentation tests -*- lexical-binding: t; -*-

;;; Code:

(require 'test-helper)
(require 'clatter-track)

(defun clatter-track-styling-test--info (&rest overrides)
  "Return tracker info with OVERRIDES applied."
  (let ((info (list :buffer (current-buffer)
                    :name "#test"
                    :unread 3
                    :mention nil
                    :muted nil
                    :dm nil)))
    (while overrides
      (setq info (plist-put info (pop overrides) (pop overrides))))
    info))

(ert-deftest clatter-track-styling-defaults-preserve-output ()
  "Default tracker styling reproduces the legacy strings and faces."
  (let ((clatter-track-indicators '((mention . "@") (dm . "*") (activity . "")))
        (clatter-track-count-style 'suffix)
        (clatter-track-show-counts t))
    (let ((mention (clatter-track--format-entry
                    (clatter-track-styling-test--info :mention t)))
          (dm (clatter-track--format-entry
               (clatter-track-styling-test--info :name "alice" :unread 1 :dm t)))
          (activity (clatter-track--format-entry
                     (clatter-track-styling-test--info :unread 5))))
      (should (equal (substring-no-properties mention) "@#test:3"))
      (should (equal (substring-no-properties dm) "*alice:1"))
      (should (equal (substring-no-properties activity) "#test:5"))
      (should (eq (get-text-property 0 'face mention) 'clatter-track-mention))
      (should (eq (get-text-property 0 'face dm) 'clatter-track-dm)))))

(ert-deftest clatter-track-styling-indicators-support-nil-and-fallbacks ()
  "Explicit nil hides an indicator while missing entries use legacy values."
  (let ((clatter-track-indicators '((mention . nil)))
        (clatter-track-count-style 'suffix))
    (should
     (equal (substring-no-properties
             (clatter-track--format-entry
              (clatter-track-styling-test--info :mention t)))
            "#test:3"))
    (should
     (equal (substring-no-properties
             (clatter-track--format-entry
              (clatter-track-styling-test--info :name "alice" :unread 1 :dm t)))
            "*alice:1"))))

(ert-deftest clatter-track-styling-count-display-styles ()
  "Raised, lowered, and disabled count styles preserve exact counts."
  (dolist (case '((superscript (raise 0.3))
                  (subscript (raise -0.3))))
    (let* ((clatter-track-count-style (car case))
           (entry (clatter-track--format-entry
                   (clatter-track-styling-test--info)))
           (count-pos (1- (length entry))))
      (should (equal (substring-no-properties entry) "#test3"))
      (should (equal (get-text-property count-pos 'display entry) (cadr case)))))
  (let ((clatter-track-count-style 'none))
    (should
     (equal (substring-no-properties
             (clatter-track--format-entry (clatter-track-styling-test--info)))
            "#test")))
  (let ((clatter-track-count-style 'parens))
    (should
     (equal (substring-no-properties
             (clatter-track--format-entry (clatter-track-styling-test--info)))
            "#test (3)")))
  (let ((clatter-track-count-style 'suffix)
        (clatter-track-show-counts nil))
    (should
     (equal (substring-no-properties
             (clatter-track--format-entry (clatter-track-styling-test--info)))
            "#test"))))

(ert-deftest clatter-track-styling-glyph-counts-are-exact ()
  "Glyph counts use compact marks and retain exact larger values."
  (let ((clatter-track-count-style 'glyph))
    (dolist (case '((1 "#test·") (2 "#test:") (3 "#test⋮") (12 "#test+12")))
      (let ((entry (clatter-track--format-entry
                    (clatter-track-styling-test--info :unread (car case)))))
        (should (equal (substring-no-properties entry) (cadr case)))
        (when (> (car case) 3)
          (should (equal (get-text-property (- (length entry) 2) 'display entry)
                         '(raise 0.3))))))))

(ert-deftest clatter-track-styling-uses-configured-faces ()
  "The public face alist controls entries and safely falls back."
  (let ((clatter-track-faces-alist '((mention . bold))))
    (let ((mention (clatter-track--format-entry
                    (clatter-track-styling-test--info :mention t)))
          (dm (clatter-track--format-entry
               (clatter-track-styling-test--info :name "alice" :dm t)))
          (muted (clatter-track--format-entry
                  (clatter-track-styling-test--info :muted t))))
      (should (eq (get-text-property 0 'face mention) 'bold))
      (should (eq (get-text-property 0 'face dm) 'clatter-track-dm))
      (should (eq (get-text-property 0 'face muted) 'clatter-track-muted)))))

(ert-deftest clatter-track-styling-preserves-interaction-properties ()
  "Styled entries retain help, mouse, and click-map properties."
  (let ((entry (clatter-track--format-entry
                (clatter-track-styling-test--info :mention t))))
    (should (stringp (get-text-property 0 'help-echo entry)))
    (should (eq (get-text-property 0 'mouse-face entry) 'highlight))
    (should (keymapp (get-text-property 0 'local-map entry)))))

;;; Target-name shortening

(ert-deftest clatter-track-shorten-truncate ()
  "An integer N keeps the prefix plus the first N chars of the body."
  (let ((clatter-track-shorten 3))
    (should (equal (clatter-track--shorten-target "#systemcrafters") "#sys"))
    (should (equal (clatter-track--shorten-target "#emacs") "#ema"))))

(ert-deftest clatter-track-shorten-large-cap-preserves-name ()
  "A cap larger than the body leaves the full target unchanged."
  (let ((clatter-track-shorten 100))
    (should (equal (clatter-track--shorten-target "#systemcrafters")
                   "#systemcrafters"))))

(ert-deftest clatter-track-shorten-drop-vowels ()
  "The `drop-vowels' style strips vowels from the body."
  (let ((clatter-track-shorten 'drop-vowels))
    (should (equal (clatter-track--shorten-target "#systemcrafters")
                   "#systmcrftrs"))))

(ert-deftest clatter-track-shorten-syllable ()
  "The `syllable' style takes the first char of each segment, lowercased."
  (let ((clatter-track-shorten 'syllable))
    (should (equal (clatter-track--shorten-target "#system-crafters") "#sc"))
    (should (equal (clatter-track--shorten-target "#LiberaChat") "#lc"))))

(ert-deftest clatter-track-shorten-leaves-non-channels ()
  "Query nicks and the server buffer are never shortened."
  (let ((clatter-track-shorten 2))
    (should (equal (clatter-track--shorten-target "alice") "alice"))
    (should (equal (clatter-track--shorten-target "*server*") "*server*"))))

(ert-deftest clatter-track-shorten-uniquifies-collisions ()
  "Colliding shortened channel names are extended until unique."
  (let ((clatter-track-shorten 3))
    (unwind-protect
        (let ((crafters (clatter-get-or-create-buffer
                         "net" "#systemcrafters" 'channel))
              (syslog (clatter-get-or-create-buffer "net" "#syslog" 'channel)))
          (with-current-buffer crafters (setq clatter--unread-count 1))
          (with-current-buffer syslog (setq clatter--unread-count 1))
          (let ((names (mapcar (lambda (i) (plist-get i :name))
                               (clatter-track--collect))))
            ;; Both start as #sys; uniquify extends them to differ.
            (should (equal (sort names #'string<) '("#sysl" "#syst")))))
      (clatter-test-cleanup))))

(ert-deftest clatter-track-shorten-tooltip-shows-full-name ()
  "Hovering a shortened entry reveals the unabbreviated channel name."
  (let ((clatter-track-shorten 3))
    (unwind-protect
        (let ((buf (clatter-get-or-create-buffer
                    "net" "#systemcrafters" 'channel)))
          (with-current-buffer buf (setq clatter--unread-count 2))
          (let* ((info (car (clatter-track--collect)))
                 (entry (clatter-track--format-entry info)))
            (should (equal (plist-get info :name) "#sys"))
            (should (equal (get-text-property 0 'help-echo entry)
                           "#systemcrafters - 2 unread"))))
      (clatter-test-cleanup))))

(provide 'test-track-styling)

;;; test-track-styling.el ends here
