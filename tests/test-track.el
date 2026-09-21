;;; test-track.el --- Tests for clatter activity tracking -*- lexical-binding: t; -*-

;;; Code:

(require 'test-helper)
(require 'clatter-track)
(require 'clatter-ui)

(defmacro clatter-track-test--with-buffer (target &rest body)
  "Run BODY in a temporary clatter buffer for TARGET."
  (declare (indent 1))
  `(clatter-test-with-buffer
     (setq-local clatter--target ,target)
     (setq-local clatter--buffer-type
                 (if (string= ,target "*server*") 'server 'channel))
     ,@body))

(ert-deftest clatter-track-exclude-targets-omits-buffer-info ()
  "Excluded targets are absent from the tracker collection."
  (let ((clatter-track-exclude-targets '("#quiet")))
    (clatter-track-test--with-buffer "#quiet"
      (setq-local clatter--unread-count 2)
      (should-not (clatter-track--buffer-info (current-buffer)))
      (should-not (clatter-track--collect)))))

(ert-deftest clatter-track-muted-channels-remain-visible ()
  "Muted targets remain visible and use the muted tracker face."
  (let ((clatter-track-muted-channels '("#bots")))
    (clatter-track-test--with-buffer "#bots"
      (setq-local clatter--unread-count 1)
      (let* ((info (clatter-track--buffer-info (current-buffer)))
             (entry (clatter-track--format-entry info)))
        (should info)
        (should (plist-get info :muted))
        (should (eq (get-text-property 0 'face entry)
                    'clatter-track-muted))))))

(ert-deftest clatter-track-clears-only-the-selected-split-window ()
  "Selecting a visible chat clears it without clearing another split."
  (let ((first (generate-new-buffer " *clatter-track-first*"))
        (second (generate-new-buffer " *clatter-track-second*")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (with-current-buffer first
            (clatter-mode)
            (setq-local clatter--target "#first")
            (setq-local clatter--unread-count 2))
          (with-current-buffer second
            (clatter-mode)
            (setq-local clatter--target "#second")
            (setq-local clatter--unread-count 3))
          (let* ((first-window (selected-window))
                 (second-window (split-window-right)))
            (set-window-buffer first-window first)
            (set-window-buffer second-window second)
            (select-window first-window)
            (clatter-track--window-change (selected-frame))
            (with-current-buffer first
              (should (zerop clatter--unread-count)))
            (with-current-buffer second
              (should (= clatter--unread-count 3)))
            ;; Merely changing an unselected split does not mark it read.
            (clatter-track--window-change second-window)
            (with-current-buffer second
              (should (= clatter--unread-count 3)))
            ;; Selecting that already-visible split clears it.
            (select-window second-window)
            (clatter-track--selection-change (selected-frame))
            (with-current-buffer second
              (should (zerop clatter--unread-count)))))
      (when (buffer-live-p first) (kill-buffer first))
      (when (buffer-live-p second) (kill-buffer second)))))

(ert-deftest clatter-track-clear-all-includes-filtered-targets ()
  "Clear-all resets and records every target, including excluded ones."
  (let ((clatter-track-exclude-targets '("#hidden"))
        (clatter-track-muted-channels '("#muted"))
        recorded
        (updates 0))
    (unwind-protect
        (let ((hidden (clatter-get-or-create-buffer
                       "track-clear" "#hidden" 'channel))
              (muted (clatter-get-or-create-buffer
                      "track-clear" "#muted" 'channel)))
          (dolist (buffer (list hidden muted))
            (with-current-buffer buffer
              (setq-local clatter--unread-count 3)
              (setq-local clatter--has-mention t)))
          (cl-letf (((symbol-function 'clatter-read-state-record-buffer)
                     (lambda (buffer) (push buffer recorded)))
                    ((symbol-function 'clatter-track--update)
                     (lambda () (cl-incf updates))))
            (should (= (clatter-track-clear-all) 2)))
          (dolist (buffer (list hidden muted))
            (with-current-buffer buffer
              (should (zerop clatter--unread-count))
              (should-not clatter--has-mention)))
          (should (= updates 1))
          (should (equal (sort recorded
                               (lambda (a b)
                                 (string< (buffer-name a) (buffer-name b))))
                         (sort (list hidden muted)
                               (lambda (a b)
                                 (string< (buffer-name a) (buffer-name b)))))))
      (clatter-test-cleanup))))

(defmacro clatter-activity-test--cleanup (&rest body)
  "Run BODY then clean up clatter buffers and the activity list buffer."
  (declare (indent 0))
  `(unwind-protect
       (progn ,@body)
     (when-let* ((b (get-buffer "*clatter-activity*"))) (kill-buffer b))
     (clatter-test-cleanup)))

(ert-deftest clatter-activity-list-renders-tabulated-entries ()
  "`*clatter-activity*' is a tabulated list whose entry id is the clatter buffer."
  (let ((clatter-track-shorten 100)
        (clatter-track-exclude-targets nil)
        (clatter-track-muted-channels '("#muted")))
    (clatter-activity-test--cleanup
      (let ((alpha (clatter-get-or-create-buffer "act" "#alpha" 'channel))
            (beta (clatter-get-or-create-buffer "act" "#beta" 'channel))
            (bob (clatter-get-or-create-buffer "act" "bob" 'query))
            (muted (clatter-get-or-create-buffer "act" "#muted" 'channel)))
        (with-current-buffer alpha (setq clatter--unread-count 3))
        (with-current-buffer beta
          (setq clatter--unread-count 5 clatter--has-mention t))
        (with-current-buffer bob (setq clatter--unread-count 2))
        (with-current-buffer muted (setq clatter--unread-count 1))
        (clatter-track-list)
        (with-current-buffer (get-buffer "*clatter-activity*")
          (should (eq major-mode 'clatter-activity-mode))
          (should (eq revert-buffer-function #'tabulated-list-revert))
          (should (eq tabulated-list-entries #'clatter--activity-entries))
          ;; Mentions sort first, so the top entry resolves to #beta.
          (goto-char (point-min))
          (should (eq (tabulated-list-get-id) beta))
          ;; Status column surfaces mention, DM, and muted flags.
          (let ((contents (buffer-string)))
            (should (string-match-p "mention" contents))
            (should (string-match-p "DM" contents))
            (should (string-match-p "muted" contents)))
          ;; Revert drops buffers whose activity has been cleared.
          (with-current-buffer alpha (setq clatter--unread-count 0))
          (funcall revert-buffer-function nil t)
          (should-not (string-match-p "#alpha" (buffer-string)))
          (should (string-match-p "#beta" (buffer-string))))))))

(ert-deftest clatter-activity-list-ignores-shorten-config ()
  "`*clatter-activity*' shows full buffer names regardless of `clatter-track-shorten'.
The compact mode-line indicator honors shortening; the activity list does not,
so a small `clatter-track-shorten' value must not truncate the Buffer column."
  (let ((clatter-track-shorten 3)
        (clatter-track-exclude-targets nil))
    (clatter-activity-test--cleanup
      (let ((chan (clatter-get-or-create-buffer "full" "#longchannel" 'channel)))
        (with-current-buffer chan (setq clatter--unread-count 1))
        (clatter-track-list)
        (with-current-buffer (get-buffer "*clatter-activity*")
          (let ((contents (buffer-string)))
            ;; Full buffer name (clatter:full/#longchannel) is shown...
            (should (string-match-p "#longchannel" contents))
            ;; ...and the 3-char truncation that the mode-line would apply is not.
            (should-not (string-match-p "#lon\\b" contents))))))))

(ert-deftest clatter-activity-jump-switches-to-buffer-and-clears ()
  "`clatter-activity-jump' selects the entry's clatter buffer and clears it."
  (let ((clatter-track-shorten 100)
        (clatter-track-exclude-targets nil)
        recorded)
    (clatter-activity-test--cleanup
      (cl-letf (((symbol-function 'clatter-read-state-record-buffer)
                 (lambda (buffer) (push buffer recorded))))
        (let ((chan (clatter-get-or-create-buffer "jump" "#chan" 'channel)))
          (with-current-buffer chan (setq clatter--unread-count 4))
          (clatter-track-list)
          (with-current-buffer (get-buffer "*clatter-activity*")
            (goto-char (point-min))
            (should (eq (tabulated-list-get-id) chan))
            (clatter-activity-jump))
          ;; `clatter-activity-jump' switches the selected window to chan and
          ;; clears its activity (`current-buffer' is restored by the
          ;; `with-current-buffer' above, so check the selected window).
          (should (eq (window-buffer (selected-window)) chan))
          (with-current-buffer chan (should (zerop clatter--unread-count)))
          (should (memq chan recorded)))))))

(ert-deftest clatter-activity-clear-clears-entry-at-point ()
  "`clatter-activity-clear' clears the entry at point and refreshes the list."
  (let ((clatter-track-shorten 100)
        (clatter-track-exclude-targets nil)
        recorded)
    (clatter-activity-test--cleanup
      (cl-letf (((symbol-function 'clatter-read-state-record-buffer)
                 (lambda (buffer) (push buffer recorded))))
        (let ((first (clatter-get-or-create-buffer "clear" "#first" 'channel))
              (second (clatter-get-or-create-buffer "clear" "#second" 'channel)))
          ;; Give #first a higher unread count so it sorts above #second,
          ;; making it the entry at point after `goto-char (point-min)'.
          (with-current-buffer first (setq clatter--unread-count 5))
          (with-current-buffer second (setq clatter--unread-count 1))
          (clatter-track-list)
          (with-current-buffer (get-buffer "*clatter-activity*")
            (goto-char (point-min))
            (should (eq (tabulated-list-get-id) first))
            (clatter-activity-clear)
            (should (with-current-buffer first (zerop clatter--unread-count)))
            (should (memq first recorded))
            (should-not (string-match-p "#first" (buffer-string)))
            (should (string-match-p "#second" (buffer-string)))))))))

(ert-deftest clatter-activity-mute-and-unmute-toggle-target-at-point ()
  "`clatter-activity-mute' and `clatter-activity-unmute' toggle the point target."
  (let ((clatter-track-shorten 100)
        (clatter-track-exclude-targets nil)
        (clatter-track-muted-channels nil))
    (clatter-activity-test--cleanup
      (let ((chan (clatter-get-or-create-buffer "mute" "#chan" 'channel)))
        (with-current-buffer chan (setq clatter--unread-count 2))
        (clatter-track-list)
        (with-current-buffer (get-buffer "*clatter-activity*")
          (goto-char (point-min))
          (should (eq (tabulated-list-get-id) chan))
          (clatter-activity-mute)
          (should (member "#chan" clatter-track-muted-channels))
          (should (string-match-p "muted" (buffer-string)))
          (clatter-activity-unmute)
          (should-not (member "#chan" clatter-track-muted-channels))
          (should-not (string-match-p "muted" (buffer-string))))))))

;;; clatter-track-switch return-to-origin

(ert-deftest clatter-track-switch-returns-to-origin-on-exhaustion ()
  "After visiting the last active buffer, the next switch returns to origin."
  (let ((clatter-track-switch-return-to-origin t)
        (clatter-track-shorten 100))
    (clatter-activity-test--cleanup
      (cl-letf (((symbol-function 'clatter-read-state-record-buffer)
                 (lambda (_buffer))))
        (let ((origin (generate-new-buffer " *origin*"))
              (chan (clatter-get-or-create-buffer "sw" "#chan" 'channel)))
          (with-current-buffer chan (setq clatter--unread-count 2))
          (unwind-protect
              (progn
                (switch-to-buffer origin)
                (setq clatter-track--switch-origin nil)
                ;; First switch jumps to the active buffer and captures origin.
                (clatter-track-switch)
                (should (eq (window-buffer (selected-window)) chan))
                (should (eq clatter-track--switch-origin origin))
                ;; Activity is now exhausted: return to origin.
                (clatter-track-switch)
                (should (eq (window-buffer (selected-window)) origin))
                (should (null clatter-track--switch-origin)))
            (kill-buffer origin)))))))

(ert-deftest clatter-track-switch-no-activity-when-nothing-active ()
  "With no active buffers and no prior switch, it messages and stays put."
  (let ((clatter-track-switch-return-to-origin t))
    (clatter-activity-test--cleanup
      (let ((origin (generate-new-buffer " *origin*")))
        (unwind-protect
            (progn
              (switch-to-buffer origin)
              (setq clatter-track--switch-origin nil)
              (clatter-track-switch)
              (should (eq (window-buffer (selected-window)) origin))
              (should (null clatter-track--switch-origin)))
          (kill-buffer origin))))))

(ert-deftest clatter-track-switch-cycles-then-returns-to-origin ()
  "Multiple active buffers are visited by priority, then origin is restored."
  (let ((clatter-track-switch-return-to-origin t)
        (clatter-track-shorten 100))
    (clatter-activity-test--cleanup
      (cl-letf (((symbol-function 'clatter-read-state-record-buffer)
                 (lambda (_buffer))))
        (let ((origin (generate-new-buffer " *origin*"))
              (alpha (clatter-get-or-create-buffer "sw" "#alpha" 'channel))
              (beta (clatter-get-or-create-buffer "sw" "#beta" 'channel)))
          (with-current-buffer alpha (setq clatter--unread-count 5))
          (with-current-buffer beta (setq clatter--unread-count 1))
          (unwind-protect
              (progn
                (switch-to-buffer origin)
                (setq clatter-track--switch-origin nil)
                ;; Higher unread first.
                (clatter-track-switch)
                (should (eq (window-buffer (selected-window)) alpha))
                (clatter-track-switch)
                (should (eq (window-buffer (selected-window)) beta))
                ;; Exhausted: return to origin.
                (clatter-track-switch)
                (should (eq (window-buffer (selected-window)) origin))
                (should (null clatter-track--switch-origin)))
            (kill-buffer origin)))))))

;; --- Mode-line integration ---

(ert-deftest clatter-track-old-buffer-mode-line-option-is-an-alias ()
  "The old option name remains compatible with existing configurations."
  (should (eq (indirect-variable 'clatter-track-in-buffer-mode-line)
              'clatter-track-show-in-clatter-buffers)))

(ert-deftest clatter-track-mode-line-item-present-p-finds-nested-item ()
  "A track item nested inside another construct counts as present."
  (let ((format (list "%e" (list "" 'clatter-track-mode-line-item)
                      'mode-line-end-spaces)))
    (should (clatter-track--mode-line-item-present-p format))
    (should-not (clatter-track--mode-line-item-present-p
                 (list "%e" 'mode-line-end-spaces)))))

(ert-deftest clatter-track-insert-mode-line-item-skips-nested-copy ()
  "Inserting into a format that already nests the item changes nothing."
  (let ((format (list "%e" (list "" 'clatter-track-mode-line-item)
                      'mode-line-end-spaces)))
    (should (equal (clatter-track--insert-mode-line-item format) format))))

(ert-deftest clatter-track-sync-global-mode-line-respects-opt-out ()
  "With the global option off, the item is never appended to the default."
  (let ((clatter-track-global-mode-line nil)
        (mode-line-format (default-value 'mode-line-format)))
    (unwind-protect
        (progn
          (set-default 'mode-line-format (list "%e" 'mode-line-end-spaces))
          (clatter-track--sync-global-mode-line)
          (should-not (clatter-track--mode-line-item-present-p
                       (default-value 'mode-line-format))))
      (set-default 'mode-line-format mode-line-format))))

(ert-deftest clatter-track-sync-global-mode-line-appends-when-on ()
  "With the global option on, the item is appended once and idempotently."
  (let ((clatter-track-global-mode-line t)
        (mode-line-format (default-value 'mode-line-format)))
    (unwind-protect
        (progn
          (set-default 'mode-line-format (list "%e" 'mode-line-end-spaces))
          (clatter-track--sync-global-mode-line)
          (should (clatter-track--mode-line-item-present-p
                   (default-value 'mode-line-format)))
          ;; A second sync does not duplicate the item.
          (clatter-track--sync-global-mode-line)
          (should (equal (member 'clatter-track-mode-line-item
                                 (default-value 'mode-line-format))
                         '(clatter-track-mode-line-item))))
      (set-default 'mode-line-format mode-line-format))))

(ert-deftest clatter-track-sync-global-mode-line-removes-when-toggled-off ()
  "Toggling the option off strips a previously appended item from the default."
  (let ((mode-line-format (default-value 'mode-line-format)))
    (unwind-protect
        (progn
          (set-default 'mode-line-format (list "%e" 'clatter-track-mode-line-item
                                               'mode-line-end-spaces))
          (let ((clatter-track-global-mode-line nil))
            (clatter-track--sync-global-mode-line))
          (should-not (clatter-track--mode-line-item-present-p
                       (default-value 'mode-line-format))))
      (set-default 'mode-line-format mode-line-format))))

;;; Adaptive strip layout

(ert-deftest clatter-track-mode-line-item-evals-renderer ()
  "The public item delegates to the renderer function."
  (should (equal clatter-track-mode-line-item
                 '(:eval (clatter-track--mode-line)))))

(ert-deftest clatter-track-strip-update-refreshes-snapshot-and-renders ()
  "The strip snapshot is refreshed on update and rendered per window.
Also covers changing layout while active, and empty activity."
  (let ((clatter-track-layout 'legacy))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (let ((buf (clatter-get-or-create-buffer "net" "#emacs" 'channel)))
            (with-current-buffer buf
              (setq clatter--unread-count 2)
              (setq clatter--has-mention t))
            (clatter-track--update)
            ;; Legacy string is populated.
            (should (string-match-p "#emacs" clatter-track--string))
            ;; Switching layout while active refreshes without new activity.
            (let ((clatter-track-layout 'strip))
              (clatter-track--update)
              (should clatter-track--strip-entries)
              ;; The renderer fits the snapshot in the selected window.
              (let ((rendered (clatter-track--mode-line)))
                (should (stringp rendered))
                (should (string-match-p "#emacs" rendered)))
              ;; A wider allocation shows the same entry.
              (let ((clatter-track-max-width 1.0))
                (should (string-match-p
                         "#emacs" (clatter-track--mode-line))))
              ;; Zero budget hides the strip.
              (let ((clatter-track-max-width 0.0))
                (should (equal (clatter-track--mode-line) ""))))
            ;; Clearing activity empties the snapshot.
            (with-current-buffer buf
              (clatter-clear-activity buf))
            (let ((clatter-track-layout 'strip))
              (clatter-track--update)
              (should (equal (clatter-track--mode-line) "")))))
      (clatter-test-cleanup))))

(ert-deftest clatter-track-layout-changes-force-legacy-redisplay ()
  "Strip-to-legacy changes force redisplay through setters and `setq'."
  (let ((old-layout clatter-track-layout)
        (old-rendered clatter-track--rendered-layout)
        (old-string clatter-track--string)
        (old-timer clatter-track--timer)
        (forced nil))
    (unwind-protect
        (progn
          (setq clatter-track-layout 'strip
                clatter-track--rendered-layout 'strip
                clatter-track--timer t
                clatter-track--string "same")
          (cl-letf (((symbol-function 'clatter-track--format-string)
                     (lambda () "same"))
                    ((symbol-function 'force-mode-line-update)
                     (lambda (&optional _all) (setq forced t))))
            (funcall (get 'clatter-track-layout 'custom-set)
                     'clatter-track-layout 'legacy)
            (should forced)
            (setq forced nil
                  clatter-track-layout 'legacy
                  clatter-track--rendered-layout 'strip)
            (clatter-track--update)
            (should forced)
            (should (eq clatter-track--rendered-layout 'legacy))))
      (setq clatter-track-layout old-layout
            clatter-track--rendered-layout old-rendered
            clatter-track--string old-string
            clatter-track--timer old-timer))))

(ert-deftest clatter-track-disable-clears-both-caches ()
  "Disabling clears the legacy string and the strip snapshot.
The strip renderer must not recollect and resurrect cleared state."
  (let ((clatter-track-layout 'strip))
    (unwind-protect
        (let ((buf (clatter-get-or-create-buffer "net" "#emacs" 'channel)))
          (with-current-buffer buf (setq clatter--unread-count 1))
          (clatter-track--update)
          (should clatter-track--strip-entries)
          (clatter-track-disable)
          (should (equal clatter-track--string ""))
          (should-not clatter-track--strip-entries)
          ;; Rendering after disable stays empty even on redisplay.
          (should (equal (clatter-track--mode-line) "")))
      (clatter-test-cleanup))))

(ert-deftest clatter-track-disable-removes-all-activity-hooks ()
  "Disabling removes PRIVMSG, ACTION, and NOTICE update hooks."
  (let ((clatter-privmsg-hook '(clatter-track--on-activity))
        (clatter-action-hook '(clatter-track--on-activity-action))
        (clatter-notice-hook '(clatter-track--on-activity-notice))
        (clatter-track--timer nil))
    (clatter-track-disable)
    (should-not (memq 'clatter-track--on-activity clatter-privmsg-hook))
    (should-not
     (memq 'clatter-track--on-activity-action clatter-action-hook))
    (should-not
     (memq 'clatter-track--on-activity-notice clatter-notice-hook))))

(ert-deftest clatter-track-strip-click-acts-in-event-window ()
  "A strip click switches the event window to its distinct target."
  (let ((clatter-track-layout 'strip)
        (first (generate-new-buffer " *clatter-track-click-a*"))
        (second (generate-new-buffer " *clatter-track-click-b*")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (dolist (buf (list first second))
            (with-current-buffer buf
              (clatter-mode)
              (setq-local clatter--network "net")
              (setq-local clatter--target
                          (if (eq buf first) "#first" "#second"))
              (setq-local clatter--unread-count 3)))
          (let* ((first-window (selected-window))
                 (second-window (split-window-right)))
            (set-window-buffer first-window first)
            (set-window-buffer second-window second)
            (select-window first-window)
            (clatter-track--update)
            (let* ((entry
                    (cl-find-if
                     (lambda (item)
                       (eq (plist-get item :buffer) first))
                     clatter-track--strip-entries))
                   (span (clatter-track--strip-entry-span
                          entry (plist-get entry :label)))
                   (event (list 'mode-line
                                (list second-window 'mode-line 1 0
                                      (cons span 1) 0 0))))
              (should entry)
              (clatter-track--strip-click event)
              (should (eq (selected-window) second-window))
              (should (eq (window-buffer second-window) first))
              (with-current-buffer first
                (should (zerop clatter--unread-count)))
              (with-current-buffer second
                (should (= clatter--unread-count 3))))))
      (when (buffer-live-p first) (kill-buffer first))
      (when (buffer-live-p second) (kill-buffer second))
      (clatter-test-cleanup))))

(ert-deftest clatter-track-strip-click-dead-window-does-not-navigate ()
  "A stale event window refreshes without switching a live target."
  (let ((clatter-track-layout 'strip)
        (target (generate-new-buffer " *clatter-track-click-target*"))
        (origin (current-buffer)))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (with-current-buffer target
            (clatter-mode)
            (setq-local clatter--target "#target")
            (setq-local clatter--unread-count 2))
          (let* ((dead-window (split-window-right))
                 (entry (list :buffer target :label "#target" :suffix ""
                              :count-str ":2" :marker "" :face nil
                              :type 'activity :unread 2 :help "target"))
                 (span (clatter-track--strip-entry-span entry "#target"))
                 (event (list 'mode-line
                              (list dead-window 'mode-line 1 0
                                    (cons span 1) 0 0))))
            (delete-window dead-window)
            (clatter-track--strip-click event)
            (should (eq (current-buffer) origin))
            (with-current-buffer target
              (should (= clatter--unread-count 2)))))
      (when (buffer-live-p target) (kill-buffer target)))))

(ert-deftest clatter-track-strip-click-dead-buffer-does-not-navigate ()
  "Clicking a stale target never selects an unrelated buffer."
  (let ((clatter-track-layout 'strip)
        (victim (generate-new-buffer " *clatter-track-dead*"))
        (origin (generate-new-buffer " *clatter-track-origin*")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (set-window-buffer (selected-window) origin)
          (select-window (selected-window))
          (with-current-buffer victim
            (clatter-mode)
            (setq-local clatter--network "net")
            (setq-local clatter--target "#victim")
            (setq-local clatter--unread-count 1))
          (clatter-track--update)
          (let* ((entry (car clatter-track--strip-entries))
                 (span (clatter-track--strip-entry-span
                        entry (plist-get entry :label))))
            (kill-buffer (plist-get entry :buffer))
            (let ((event (list 'mode-line
                                (list (selected-window) 'mode-line 1 0
                                      (cons span 1) 0 0))))
              (clatter-track--strip-click event)
              ;; No wrong-buffer navigation: the origin stays selected.
              (should (eq (current-buffer) origin))
              ;; The refresh cleared the stale entry from the snapshot.
              (should-not
               (cl-some (lambda (e)
                          (eq (plist-get e :buffer) (plist-get entry :buffer)))
                        clatter-track--strip-entries)))))
      (when (buffer-live-p origin) (kill-buffer origin))
      (clatter-test-cleanup))))

(ert-deftest clatter-track-strip-click-overflow-opens-activity-list ()
  "An overflow click opens the activity list with all current entries."
  (let ((clatter-track-layout 'strip))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (let ((a (clatter-get-or-create-buffer "net" "#alpha" 'channel))
                (b (clatter-get-or-create-buffer "net" "#beta-long-name" 'channel))
                (c (clatter-get-or-create-buffer "net" "#gamma-longer-name" 'channel)))
            (dolist (buf (list a b c))
              (with-current-buffer buf (setq clatter--unread-count 1)))
            (select-window (selected-window))
            (clatter-track--update)
            ;; Build an overflow span and click it.
            (let* ((entries clatter-track--strip-entries)
                   (ovf (clatter-track--strip-overflow-data entries))
                   (span (clatter-track--strip-overflow-span ovf))
                   (event (list 'mode-line
                                (list (selected-window) 'mode-line 1 0
                                      (cons span 1) 0 0))))
              (clatter-track--strip-click event)
              (let ((list-buf (get-buffer "*clatter-activity*")))
                (should list-buf)
                (with-current-buffer list-buf
                  (let ((text (buffer-substring (point-min) (point-max))))
                    (should (string-match-p "#alpha" text))
                    (should (string-match-p "#beta" text))
                    (should (string-match-p "#gamma" text))))))))
      (dolist (name '("*clatter:net/#alpha*" "*clatter:net/#beta-long-name*"
                      "*clatter:net/#gamma-longer-name*" "*clatter-activity*"))
        (let ((buf (get-buffer name)))
          (when (buffer-live-p buf) (kill-buffer buf))))
      (clatter-test-cleanup))))

(ert-deftest clatter-track-max-width-setter-validates-range ()
  "The Custom setter rejects invalid allocations without applying them."
  (let ((old-width (default-value 'clatter-track-max-width))
        (old-timer clatter-track--timer))
    (unwind-protect
        (progn
          (setq clatter-track--timer nil)
          (dolist (invalid '(-0.1 1.5 -1))
            (let ((before (default-value 'clatter-track-max-width)))
              (should-error
               (funcall (get 'clatter-track-max-width 'custom-set)
                        'clatter-track-max-width invalid))
              (should (equal (default-value 'clatter-track-max-width)
                             before))))
          (funcall (get 'clatter-track-max-width 'custom-set)
                   'clatter-track-max-width 0.25)
          (should (equal clatter-track-max-width 0.25))
          (funcall (get 'clatter-track-max-width 'custom-set)
                   'clatter-track-max-width 12)
          (should (equal clatter-track-max-width 12)))
      (set-default 'clatter-track-max-width old-width)
      (setq clatter-track--timer old-timer))))

(provide 'test-track)

;;; test-track.el ends here
