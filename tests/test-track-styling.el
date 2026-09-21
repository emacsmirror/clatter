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

;;; Count face and adaptive strip

(defun clatter-track-styling-test--strip-entry (label &rest overrides)
  "Return a prepared strip entry for LABEL with OVERRIDES applied."
  (let ((entry (list :buffer (current-buffer)
                     :label label
                     :suffix ""
                     :count-str (clatter-track--format-count 3)
                     :marker ""
                     :face nil
                     :type 'activity
                     :unread 3
                     :mention nil
                     :full-name label
                     :help "help")))
    (while overrides
      (setq entry (plist-put entry (pop overrides) (pop overrides))))
    entry))

(defun clatter-track-styling-test--face-members (face)
  "Return FACE as a list of face names for membership checks."
  (if (listp face) face (list face)))

(ert-deftest clatter-track-styling-count-face-applies-to-count-only ()
  "The count face leads urgency faces on only the count span."
  (let ((entry (clatter-track--format-entry
                (clatter-track-styling-test--info :mention t))))
    (let ((count-face (clatter-track-styling-test--face-members
                       (get-text-property (1- (length entry)) 'face entry)))
          (name-face (clatter-track-styling-test--face-members
                      (get-text-property 0 'face entry))))
      (should (eq (car count-face) 'clatter-track-count))
      (should (memq 'clatter-track-mention count-face))
      (should (memq 'clatter-track-mention name-face))
      (should-not (memq 'clatter-track-count name-face))))
  (let* ((entry (clatter-track-styling-test--strip-entry
                 "#emacs" :face 'clatter-track-mention
                 :count-str " (3)"))
         (span (clatter-track--strip-entry-span entry "#emacs"))
         (count-pos (- (length span) 2))
         (count-face (clatter-track-styling-test--face-members
                      (get-text-property count-pos 'face span))))
    (should (eq (car count-face) 'clatter-track-count))
    (should (memq 'clatter-track-mention count-face))
    (should-not
     (memq 'clatter-track-count
           (clatter-track-styling-test--face-members
            (get-text-property 0 'face span))))))

(ert-deftest clatter-track-styling-count-face-keeps-display-and-click ()
  "Composing the count face preserves display and interaction properties."
  (let ((clatter-track-count-style 'superscript))
    (let* ((entry (clatter-track--format-entry
                   (clatter-track-styling-test--info :unread 7)))
           (count-pos (1- (length entry))))
      (should (equal (get-text-property count-pos 'display entry)
                     '(raise 0.3)))
      (should (memq 'clatter-track-count
                    (clatter-track-styling-test--face-members
                     (get-text-property count-pos 'face entry))))
      (should (stringp (get-text-property count-pos 'help-echo entry)))
      (should (keymapp (get-text-property count-pos 'local-map entry))))))

(ert-deftest clatter-track-styling-update-detects-property-changes ()
  "Character-identical property changes replace the legacy cache."
  (let* ((clatter-track-layout 'legacy)
         (old (propertize " [#a:1]" 'face 'bold))
         (new (propertize " [#a:1]" 'face 'italic))
         (clatter-track--string old)
         (forced nil))
    (cl-letf (((symbol-function 'clatter-track--format-string)
               (lambda () new))
              ((symbol-function 'force-mode-line-update)
               (lambda (&optional _all) (setq forced t))))
      (clatter-track--update))
    (should (equal-including-properties clatter-track--string new))
    (should forced)))

(ert-deftest clatter-track-styling-strip-terminal-budgets ()
  "Terminal-fixture budgets render the attention-first strip exactly."
  (let ((clatter-track-count-style 'parens)
        (clatter-track-indicators
         '((mention . "@") (dm . "*") (activity . ""))))
    (let ((entries
           (list (clatter-track-styling-test--strip-entry
                  "#emacs" :type 'mention :marker "@" :mention t :unread 10
                  :count-str " (10)")
                 (clatter-track-styling-test--strip-entry
                  "alice" :type 'dm :marker "*" :dm t :unread 2
                  :count-str " (2)")
                 (clatter-track-styling-test--strip-entry
                  "#guix" :unread 3 :count-str " (3)"))))
      (dolist (case '((40 " [@#emacs (10) · *alice (2) · #guix (3)]")
                      (32 " [@#emacs (10) · *+2]")
                      (20 " [@+3]")
                      (3 "@+3")
                      (2 "+3")
                      (1 "+")
                      (0 "")))
        (should (equal (substring-no-properties
                        (clatter-track--format-strip entries (car case)))
                       (cadr case)))))))

(ert-deftest clatter-track-styling-strip-shaped-overflow-can-hide ()
  "A final shaping overshoot returns empty when even bare + is too wide."
  (let ((entries (list (clatter-track-styling-test--strip-entry "#one"))))
    (cl-letf (((symbol-function 'clatter-track--strip-measure)
               (lambda (string)
                 (let ((plain (substring-no-properties string)))
                   (cond
                    ;; Component measurements admit the prefix.
                    ((member plain '(" [" "]" "#one:3")) 0)
                    ;; Final shaped strings and every fallback exceed 1.
                    (t 2))))))
      (let ((result (clatter-track--format-strip entries 1)))
        (should (stringp result))
        (should (equal result ""))))))

(ert-deftest clatter-track-styling-strip-keeps-full-name-when-narrower ()
  "A styled full name no wider than its capped form is not ellipsized."
  (let ((name "abcdefghijklmnopqrst")
        (saw-entry-face nil))
    (cl-letf (((symbol-function 'clatter-track--frame-graphic-p)
               (lambda () t))
              ((symbol-function 'string-pixel-width)
               (lambda (string)
                 (when (memq 'clatter-track-mention
                             (clatter-track-styling-test--face-members
                              (get-text-property 0 'face string)))
                   (setq saw-entry-face t))
                 (let ((width 0))
                   (dotimes (i (length string) width)
                     (setq width
                           (+ width (if (= (aref string i) ?…) 12 3))))))))
      ;; At cap 18, truncation is 17 three-pixel glyphs plus a
      ;; twelve-pixel ellipsis (63px); the full name is only 60px.
      (should (equal (clatter-track--strip-cap-name
                      name 18 'clatter-track-mention)
                     name))
      ;; The same comparison must happen at the initial eight-column cap:
      ;; eight glyphs plus the wide ellipsis cost more than nine glyphs.
      (should (equal (clatter-track--strip-cap-name
                      "abcdefghi" 8 'clatter-track-mention)
                     "abcdefghi"))
      (should saw-entry-face)))
  ;; Naturally short names bypass truncation and stay complete.
  (should (equal (clatter-track--strip-cap-name "#emacs" 8) "#emacs"))
  (should (equal (clatter-track--strip-cap-name "alice" 8) "alice")))

(ert-deftest clatter-track-styling-strip-count-growth-and-unicode-fit ()
  "Counts grow exactly; Unicode labels fit without detached combining marks."
  (let ((clatter-track-count-style 'parens)
        (buf (generate-new-buffer " *clatter-strip-boundary*")))
    (unwind-protect
        (let* ((info (list :buffer buf :name "#emacs" :full-name "#emacs"
                           :unread 99 :mention nil :muted nil :dm nil))
               (entry99 (car (clatter-track--prepare-strip-entries
                              (list info))))
               (entry100
                (car (clatter-track--prepare-strip-entries
                      (list (plist-put (copy-sequence info) :unread 100)))))
               (shown99 (clatter-track--format-strip (list entry99) 14))
               (shown100 (clatter-track--format-strip (list entry100) 15))
               (overflow100
                (clatter-track--format-strip (list entry100) 14)))
          (should (equal (substring-no-properties shown99)
                         " [#emacs (99)]"))
          (should (equal (substring-no-properties shown100)
                         " [#emacs (100)]"))
          (should (<= (string-width shown99) 14))
          (should (<= (string-width shown100) 15))
          ;; The wider fixed count moves the whole conversation to overflow.
          (should (equal (substring-no-properties overflow100) " [+1]"))
          (let* ((target "#12345élong-channel")
                 (unicode
                  (car
                   (clatter-track--prepare-strip-entries
                    (list (list :buffer buf :name target :full-name target
                                :unread 3 :mention nil :muted nil :dm nil)))))
                 ;; Budget 15 leaves exactly the initial eight-column name cap.
                 (rendered (clatter-track--format-strip (list unicode) 15))
                 (plain (substring-no-properties rendered))
                 (ellipsis (string-match "…" plain)))
            (should (<= (string-width rendered) 15))
            (should (string-match-p "(3)" plain))
            (should (string-match-p
                     (regexp-quote target)
                     (get-text-property 2 'help-echo rendered)))
            (should ellipsis)
            ;; The input mark sits at the cap boundary.  If retained, it
            ;; must still follow its base; it must never trail the ellipsis.
            (should (cl-some
                     (lambda (char)
                       (memq (get-char-code-property char 'general-category)
                             '(Mn Mc Me)))
                     (string-to-list target)))
            (dotimes (i (length plain))
              (when (memq (get-char-code-property
                           (aref plain i) 'general-category)
                          '(Mn Mc Me))
                (should (> i 0))
                (should (/= (aref plain (1- i)) ?…))))))
      (kill-buffer buf))))

(ert-deftest clatter-track-styling-strip-overflow-shares-click-identity ()
  "Overflow uses prepared marker, independent DM stats, and shared map."
  (let* ((hidden (list (clatter-track-styling-test--strip-entry
                        "alice" :unread 5 :type 'mention :mention t
                        :dm t :marker "!" :face 'clatter-track-mention)))
         (clatter-track-indicators '((mention . nil) (dm . nil)))
         (ovf (clatter-track--strip-overflow-data hidden))
         (span (clatter-track--strip-overflow-span ovf)))
    (should (equal (substring-no-properties span) "!+1"))
    (should (= (plist-get ovf :dms) 1))
    (should (= (plist-get ovf :mentions) 1))
    (should (eq (get-text-property 0 'clatter-track-target span) 'overflow))
    (should (keymapp (get-text-property 0 'local-map span)))
    (should (stringp (get-text-property 0 'help-echo span)))))

(ert-deftest clatter-track-styling-strip-labels-use-raw-targets ()
  "Colliding shortened labels fall back to distinct raw targets."
  (let ((clatter-track-shorten 2)
        (a (generate-new-buffer " *clatter-strip-a*"))
        (b (generate-new-buffer " *clatter-strip-b*")))
    (unwind-protect
        (let* ((infos (list (list :buffer a :name "#sa"
                                  :full-name "#same-alpha" :unread 1)
                            (list :buffer b :name "#sa"
                                  :full-name "#same-beta" :unread 1)))
               (entries (clatter-track--prepare-strip-entries infos)))
          (should (equal (mapcar (lambda (entry)
                                   (plist-get entry :label))
                                 entries)
                         '("#same-alpha" "#same-beta")))
          (should (equal (mapcar (lambda (entry)
                                   (plist-get entry :suffix))
                                 entries)
                         '("" ""))))
      (kill-buffer a)
      (kill-buffer b))))

(ert-deftest clatter-track-styling-strip-labels-add-network-suffixes ()
  "The same raw target on different networks gets intact suffixes."
  (let ((a (generate-new-buffer " *clatter-strip-net-a*"))
        (b (generate-new-buffer " *clatter-strip-net-b*")))
    (unwind-protect
        (progn
          (with-current-buffer a (setq-local clatter--network "net-a"))
          (with-current-buffer b (setq-local clatter--network "net-b"))
          (let* ((infos (list (list :buffer a :name "#emacs"
                                    :full-name "#emacs" :unread 1)
                              (list :buffer b :name "#emacs"
                                    :full-name "#emacs" :unread 1)))
                 (entries (clatter-track--prepare-strip-entries infos)))
            (should (equal (mapcar (lambda (entry)
                                     (concat (plist-get entry :label)
                                             (plist-get entry :suffix)))
                                   entries)
                           '("#emacs@net-a" "#emacs@net-b")))
            (should-not (eq (plist-get (car entries) :buffer)
                            (plist-get (cadr entries) :buffer)))))
      (kill-buffer a)
      (kill-buffer b))))

(ert-deftest clatter-track-styling-strip-skips-dead-buffers ()
  "Preparation skips stale buffers and keeps live malformed plists safe."
  (let ((live (generate-new-buffer " *clatter-strip-live*"))
        (dead (generate-new-buffer " *clatter-strip-dead*")))
    (kill-buffer dead)
    (unwind-protect
        (let ((entries
               (clatter-track--prepare-strip-entries
                (list (list :buffer dead :name "#dead" :unread 1)
                      (list :buffer live :name "#live" :unread 2)))))
          (should (= (length entries) 1))
          (should (equal (plist-get (car entries) :label) "#live")))
      (kill-buffer live))))

(ert-deftest clatter-track-styling-strip-overflow-ignores-count-suppression ()
  "Production count suppression does not suppress conversation overflow."
  (let ((clatter-track-show-counts nil)
        (a (generate-new-buffer " *clatter-strip-count-a*"))
        (b (generate-new-buffer " *clatter-strip-count-b*")))
    (unwind-protect
        (let ((entries
               (clatter-track--prepare-strip-entries
                (list (list :buffer a :name "#one" :full-name "#one"
                            :unread 4 :mention nil :muted nil :dm nil)
                      (list :buffer b :name "#two" :full-name "#two"
                            :unread 5 :mention nil :muted nil :dm nil)))))
          (should (cl-every
                   (lambda (entry)
                     (string-empty-p (plist-get entry :count-str)))
                   entries))
          (should (string-match-p
                   "\\+2"
                   (substring-no-properties
                    (clatter-track--format-strip entries 6)))))
      (kill-buffer a)
      (kill-buffer b))))

(provide 'test-track-styling)

;;; test-track-styling.el ends here
