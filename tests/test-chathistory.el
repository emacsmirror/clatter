;;; test-chathistory.el --- Tests for clatter chathistory -*- lexical-binding: t; -*-

;;; Code:

(require 'test-helper)
(require 'clatter-chathistory)

(ert-deftest clatter-chathistory-batch-tracks-latest-timestamp ()
  "Completed playback advances the history cursor to its newest message."
  (let ((conn (clatter-test-make-connection "testnet" "testnick"))
        (clatter-read-state-enabled nil)
        buf)
    (unwind-protect
        (let ((old (encode-time 0 0 12 1 1 2026 t))
              (latest (encode-time 0 1 12 1 1 2026 t)))
          (setq buf (clatter-get-or-create-buffer "testnet" "#emacs" 'channel))
          (clatter-chathistory--track-batch-timestamp
           conn "chathistory" "#emacs"
           (list (list :time old)
                 (list :time latest)))
          (with-current-buffer buf
            (should (equal clatter-chathistory--last-timestamp latest))))
      (clatter-test-cleanup)
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(ert-deftest clatter-chathistory-more-pages-before-earliest ()
  "clatter-chathistory-more fetches BEFORE the earliest message, not latest."
  (let ((conn (clatter-test-make-connection-with-caps
               '("server-time" "batch" "message-tags" "chathistory")
               "testnet" "testnick"))
        buf)
    (unwind-protect
        (progn
          (setq buf (clatter-get-or-create-buffer "testnet" "#emacs" 'channel))
          (with-current-buffer buf
            (setq-local clatter--network "testnet")
            (setq-local clatter--target "#emacs")
            (setq-local clatter-chathistory--last-timestamp
                        (encode-time 0 0 12 1 1 2026 t))
            (setq-local clatter-chathistory--earliest-timestamp
                        (encode-time 0 0 10 1 1 2026 t))
            (clatter-test-with-mock-send
              (clatter-chathistory-more 10)
              (should (clatter-test-sent-matching
                       "^CHATHISTORY BEFORE #emacs timestamp=2026-01-01T10:00:00.000Z 10$"))
              ;; The reply must render at the buffer's oldest end.
              (should clatter--backlog-page-pending))))
      (clatter-test-cleanup)
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(ert-deftest clatter-chathistory-batch-ignores-nil-target ()
  "A target-less batch (e.g. soju.im/bouncer-networks) is a no-op.
It must not error on (downcase nil) and must not touch any buffer."
  (let ((conn (clatter-test-make-connection "testnet" "testnick")))
    (unwind-protect
        (progn
          ;; No error, no crash, no buffer created.
          (clatter-chathistory--track-batch-timestamp
           conn "soju.im/bouncer-networks" nil
           (list (list :time (encode-time 0 0 12 1 1 2026 t))))
          (should-not (clatter-get-buffer "testnet" nil)))
      (clatter-test-cleanup))))


;; --- CHATHISTORY TARGETS tests ---

(ert-deftest clatter-chathistory-fetch-targets-sends-command ()
  "fetch-targets sends CHATHISTORY TARGETS with the limit."
  (let ((conn (clatter-test-make-connection-with-caps
               '("server-time" "batch" "message-tags" "chathistory"))))
    (unwind-protect
        (clatter-test-with-mock-send
          (clatter-chathistory-fetch-targets conn 50)
          (should (clatter-test-sent-matching
                   "^CHATHISTORY TARGETS timestamp=9999-12-31T23:59:59.999Z timestamp=1970-01-01T00:00:00.000Z 50$")))
      (clatter-test-cleanup))))

(ert-deftest clatter-chathistory-fetch-targets-no-cap-no-op ()
  "fetch-targets is a no-op when chathistory cap is not enabled."
  (let ((conn (clatter-test-make-connection-with-caps
               '("server-time" "batch" "message-tags"))))
    (unwind-protect
        (clatter-test-with-mock-send
          (clatter-chathistory-fetch-targets conn)
          (should-not clatter-test--sent-lines))
      (clatter-test-cleanup))))

(ert-deftest clatter-chathistory-on-welcome-sends-targets ()
  "Welcome hook fires TARGETS request when chathistory is available."
  (let ((conn (clatter-test-make-connection-with-caps
               '("server-time" "batch" "message-tags" "chathistory"))))
    (unwind-protect
        (clatter-test-with-mock-send
          (clatter-chathistory--on-welcome conn "testnick")
          (should (clatter-test-sent-matching
                   "^CHATHISTORY TARGETS ")))
      (clatter-test-cleanup))))

(ert-deftest clatter-chathistory-on-welcome-disabled-no-op ()
  "Welcome hook does nothing when chathistory is disabled."
  (let ((conn (clatter-test-make-connection-with-caps
               '("server-time" "batch" "message-tags" "chathistory")))
        (clatter-chathistory-enabled nil))
    (unwind-protect
        (clatter-test-with-mock-send
          (clatter-chathistory--on-welcome conn "testnick")
          (should-not clatter-test--sent-lines))
      (clatter-test-cleanup))))

(ert-deftest clatter-chathistory-targets-batch-fetches-latest-for-new-dm ()
  "TARGETS batch with a new DM target triggers CHATHISTORY LATEST."
  (let ((conn (clatter-test-make-connection-with-caps
               '("server-time" "batch" "message-tags" "chathistory"))))
    (unwind-protect
        (clatter-test-with-mock-send
          (clatter-chathistory--on-targets-batch
           conn "chathistory-targets" nil
           (list (list :target "alcor" :time (encode-time 0 0 12 1 1 2026 t))))
          (should (clatter-test-sent-matching
                   "^CHATHISTORY LATEST alcor \\* 50$")))
      (clatter-test-cleanup))))

(ert-deftest clatter-chathistory-targets-batch-fetches-since-for-existing-dm ()
  "TARGETS batch with an existing DM buffer triggers CHATHISTORY AFTER."
  (let ((conn (clatter-test-make-connection-with-caps
               '("server-time" "batch" "message-tags" "chathistory")))
        (clatter-read-state-enabled nil)
        buf)
    (unwind-protect
        (progn
          (setq buf (clatter-get-or-create-buffer "testnet" "alcor" 'query))
          (with-current-buffer buf
            (setq clatter-chathistory--last-timestamp
                  (encode-time 0 0 10 1 1 2026 t)))
          (clatter-test-with-mock-send
            (clatter-chathistory--on-targets-batch
             conn "chathistory-targets" nil
             (list (list :target "alcor" :time (encode-time 0 0 12 1 1 2026 t))))
            (should (clatter-test-sent-matching
                     "^CHATHISTORY AFTER alcor timestamp="))))
      (clatter-test-cleanup)
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(ert-deftest clatter-chathistory-targets-batch-skips-channels ()
  "TARGETS batch skips channel targets (channels are handled on JOIN)."
  (let ((conn (clatter-test-make-connection-with-caps
               '("server-time" "batch" "message-tags" "chathistory"))))
    (unwind-protect
        (clatter-test-with-mock-send
          (clatter-chathistory--on-targets-batch
           conn "chathistory-targets" nil
           (list (list :target "#emacs" :time (encode-time 0 0 12 1 1 2026 t))))
          (should-not clatter-test--sent-lines))
      (clatter-test-cleanup))))

(ert-deftest clatter-chathistory-targets-batch-skips-own-nick ()
  "TARGETS batch skips entries where the target is our own nick."
  (let ((conn (clatter-test-make-connection-with-caps
               '("server-time" "batch" "message-tags" "chathistory")
               "testnet" "testnick")))
    (unwind-protect
        (clatter-test-with-mock-send
          (clatter-chathistory--on-targets-batch
           conn "chathistory-targets" nil
           (list (list :target "testnick" :time (encode-time 0 0 12 1 1 2026 t))))
          (should-not clatter-test--sent-lines))
      (clatter-test-cleanup))))

(ert-deftest clatter-chathistory-targets-batch-multiple-dm-targets ()
  "TARGETS batch with multiple DM targets fetches history for each."
  (let ((conn (clatter-test-make-connection-with-caps
               '("server-time" "batch" "message-tags" "chathistory"))))
    (unwind-protect
        (clatter-test-with-mock-send
          (clatter-chathistory--on-targets-batch
           conn "chathistory-targets" nil
           (list (list :target "alcor" :time (encode-time 0 0 12 1 1 2026 t))
                 (list :target "jazzah" :time (encode-time 0 0 11 1 1 2026 t))
                 (list :target "#emacs" :time (encode-time 0 0 10 1 1 2026 t))))
          (should (clatter-test-sent-matching "^CHATHISTORY LATEST alcor "))
          (should (clatter-test-sent-matching "^CHATHISTORY LATEST jazzah "))
          ;; Channel target should NOT trigger a fetch
          (should-not (clatter-test-sent-matching "^CHATHISTORY.*#emacs")))
      (clatter-test-cleanup))))

(ert-deftest clatter-chathistory-targets-batch-empty-messages ()
  "TARGETS batch with no messages is a harmless no-op."
  (let ((conn (clatter-test-make-connection-with-caps
               '("server-time" "batch" "message-tags" "chathistory"))))
    (unwind-protect
        (clatter-test-with-mock-send
          (clatter-chathistory--on-targets-batch
           conn "chathistory-targets" nil nil)
          (should-not clatter-test--sent-lines))
      (clatter-test-cleanup))))

(ert-deftest clatter-chathistory-fetch-targets-bounds-are-not-zero-time ()
  "TARGETS bounds must never be the zero time, which soju rejects.
soju parses each bound into a Go time value and answers
FAIL CHATHISTORY INVALID_PARAMS ... \"Invalid first bound\" for the zero
value, which is exactly 0001-01-01T00:00:00.000Z."
  (let ((conn (clatter-test-make-connection-with-caps
               '("server-time" "batch" "message-tags" "chathistory"))))
    (unwind-protect
        (clatter-test-with-mock-send
          (clatter-chathistory-fetch-targets conn 50)
          (let ((line (clatter-test-last-sent)))
            (should-not (string-match-p "0001-01-01" line))
            ;; Newest bound first: a truncating server keeps recent targets.
            (should (string-match-p
                     "TARGETS timestamp=9999-.* timestamp=1970-" line))))
      (clatter-test-cleanup))))

(ert-deftest clatter-chathistory-targets-batch-ignores-other-batch-types ()
  "A non-targets batch never triggers target fetches."
  (let ((conn (clatter-test-make-connection-with-caps
               '("server-time" "batch" "message-tags" "chathistory"))))
    (unwind-protect
        (clatter-test-with-mock-send
          (clatter-chathistory--on-targets-batch
           conn "chathistory" "alcor"
           (list (list :target "alcor" :time (encode-time 0 0 12 1 1 2026 t))))
          (should-not clatter-test--sent-lines))
      (clatter-test-cleanup))))

(ert-deftest clatter-chathistory-targets-response-parses-target-and-time ()
  "A CHATHISTORY TARGETS batch line yields the target name and its time.
The name is the second parameter, not the TARGETS subcommand, and the
timestamp is a parameter rather than a server-time tag."
  (let ((conn (clatter-test-make-connection-with-caps
               '("server-time" "batch" "message-tags" "chathistory")))
        (entries nil))
    (unwind-protect
        (let ((clatter-batch-complete-hook
               (list (lambda (_conn batch-type _target messages)
                       (when (clatter-chathistory--targets-batch-p batch-type)
                         (setq entries messages))))))
          (clatter-dispatch-message
           conn (clatter-test-parse "BATCH +tgt draft/chathistory-targets"))
          (clatter-dispatch-message
           conn (clatter-test-parse
                 "@batch=tgt CHATHISTORY TARGETS alcor 2026-01-01T12:00:00.000Z"))
          (clatter-dispatch-message conn (clatter-test-parse "BATCH -tgt"))
          (should (= (length entries) 1))
          (should (equal (plist-get (car entries) :target) "alcor"))
          (should (equal (plist-get (car entries) :time)
                         (clatter-parse-iso8601 "2026-01-01T12:00:00.000Z"))))
      (clatter-test-cleanup))))

(ert-deftest clatter-chathistory-targets-response-fetches-dm-history ()
  "An end-to-end TARGETS batch fetches history for the DM target."
  (let ((conn (clatter-test-make-connection-with-caps
               '("server-time" "batch" "message-tags" "chathistory"))))
    (unwind-protect
        (let ((clatter-batch-complete-hook
               (list #'clatter-chathistory--on-targets-batch)))
          (clatter-test-with-mock-send
            (clatter-dispatch-message
             conn (clatter-test-parse "BATCH +tgt draft/chathistory-targets"))
            (clatter-dispatch-message
             conn (clatter-test-parse
                   "@batch=tgt CHATHISTORY TARGETS alcor 2026-01-01T12:00:00.000Z"))
            (clatter-dispatch-message conn (clatter-test-parse "BATCH -tgt"))
            (should (clatter-test-sent-matching "^CHATHISTORY LATEST alcor \\* 50$"))
            (should-not (clatter-test-sent-matching "TARGETS TARGETS"))))
      (clatter-test-cleanup))))

(ert-deftest clatter-chathistory-targets-browser-groups-and-opens-targets ()
  "The browser queries every server and exposes targets as navigable children."
  (let ((alpha (clatter-test-make-connection-with-caps
                '("server-time" "batch" "message-tags" "chathistory")
                "alpha"))
        (zeta (clatter-test-make-connection-with-caps
               '("server-time" "batch" "message-tags" "chathistory")
               "zeta"))
        browser)
    (unwind-protect
        (clatter-test-with-mock-send
          (save-window-excursion
            (clatter-chathistory-targets))
          (setq browser
                (get-buffer clatter-chathistory--targets-buffer-name))
          (should (= 2
                     (cl-count-if
                      (lambda (line)
                        (string-prefix-p "CHATHISTORY TARGETS " line))
                      clatter-test--sent-lines)))
          (clatter-chathistory--record-targets
           zeta "chathistory-targets" nil
           (list (list :target "alice")))
          (clatter-chathistory--record-targets
           alpha "draft/chathistory-targets" nil
           (list (list :target "#emacs")
                 (list :target "bob")))
          (with-current-buffer browser
            (should
             (equal
              (mapcar #'car (funcall tabulated-list-entries))
              '(("alpha") ("alpha" . "#emacs") ("alpha" . "bob")
                ("zeta") ("zeta" . "alice"))))
            (should (eq (lookup-key clatter-chathistory-targets-mode-map
                                    (kbd "RET"))
                        #'clatter-chathistory-targets-visit))
            (should (eq (lookup-key clatter-chathistory-targets-mode-map
                                    (kbd "o"))
                        #'clatter-chathistory-targets-visit-other-window))
            (should (eq (lookup-key clatter-chathistory-targets-mode-map
                                    (kbd "a"))
                        #'clatter-chathistory-targets-visit-quit))
            (goto-char (point-min))
            (search-forward "#emacs")
            (let ((name-start (- (point) (length "#emacs"))))
              (should (eq (get-text-property name-start 'face) 'link))
              (should (button-at name-start))
              (should-not (eq (get-text-property (1- name-start) 'face)
                              'link))
              (should-not (button-at (1- name-start))))
            (goto-char (point-min))
            (while (and (not (equal (tabulated-list-get-id)
                                    '("alpha" . "#emacs")))
                        (= (forward-line 1) 0)))
            (should (equal (tabulated-list-get-id)
                           '("alpha" . "#emacs")))
            (save-window-excursion
              (clatter-chathistory-targets-visit)))
          (should (clatter-get-buffer "alpha" "#emacs"))
          (should (clatter-test-sent-matching
                   "^CHATHISTORY LATEST #emacs \\* 50$")))
      (when (buffer-live-p browser)
        (kill-buffer browser))
      (clatter-test-cleanup))))

(provide 'test-chathistory)

;;; test-chathistory.el ends here
