;;; test-url-preview.el --- Tests for URL title preview placement -*- lexical-binding: t; -*-

;;; Code:

(require 'test-helper)
(require 'clatter-ui)
(require 'clatter-url-preview)

(defmacro clatter-test-preview--with-processes (&rest body)
  "Use real Emacs pipes while replacing only curl startup in BODY."
  (declare (indent 0) (debug t))
  `(let ((clatter-privmsg-hook nil)
         processes commands)
     (cl-letf (((symbol-function 'make-process)
                (lambda (&rest args)
                  (push (plist-get args :command) commands)
                  (let ((process
                         (make-pipe-process
                          :name (plist-get args :name)
                          :buffer (plist-get args :buffer)
                          :coding (plist-get args :coding) :noquery t
                          :filter (plist-get args :filter)
                          :sentinel (plist-get args :sentinel))))
                    (push process processes)
                    process))))
       (unwind-protect
           (progn ,@body)
         (clatter-url-preview-teardown)
         (dolist (process processes)
           (when (process-live-p process) (delete-process process)))))))

(defun clatter-test-preview--source (&optional count)
  "Insert a source message and return its owned preview anchor."
  (let* ((text "https://example.com/one")
         (start (point)))
    (insert (propertize (concat text "\n") 'clatter-text text))
    (clatter-url-preview--make-anchor-record
     :buffer (current-buffer) :start (copy-marker start t)
     :end (copy-marker (point)) :tail (copy-marker (point))
     :text text :length (- (point) start) :remaining (or count 1))))

(defun clatter-test-preview--complete (process)
  "Deliver a successful completion to PROCESS without running curl."
  (let ((real-status (symbol-function 'process-status))
        (real-exit (symbol-function 'process-exit-status)))
    (cl-letf (((symbol-function 'process-status)
               (lambda (other)
                 (if (eq other process) 'exit (funcall real-status other))))
              ((symbol-function 'process-exit-status)
               (lambda (other)
                 (if (eq other process) 0 (funcall real-exit other)))))
      (funcall (process-sentinel process) process "finished"))))

(ert-deftest clatter-url-preview-skips-fools ()
  "Links from fools are not fetched or rendered."
  (let ((clatter-url-preview-enable t)
        (clatter-fools '("alice"))
        fetched)
    (cl-letf (((symbol-function 'clatter-get-buffer)
               (lambda (&rest _) (current-buffer)))
              ((symbol-function 'clatter-url-preview--fetch)
               (lambda (&rest _) (setq fetched t))))
      (clatter-url-preview--on-privmsg
       (clatter-test-make-connection "preview-fool" "me")
       '("alice" nil nil) "#chat" "https://example.com/one" nil))
    (should-not fetched)))

(ert-deftest clatter-url-preview-hook-renders-after-message-in-both-orders ()
  "URL previews follow their source message for either layout and order."
  (dolist (layout '(mode-line input-separator))
    (dolist (order '(newest-first oldest-first))
      (let ((clatter-message-order order)
            (clatter-typing-indicator-location layout)
            (clatter-url-preview-enable t)
            (clatter-url-preview--cache (make-hash-table :test 'equal))
            (clatter-url-preview--pending (make-hash-table :test 'equal))
            (clatter-privmsg-hook (list #'clatter-ui--on-privmsg)))
        (puthash "https://example.com/one" "Example title"
                 clatter-url-preview--cache)
        (clatter-url-preview-init)
        (with-temp-buffer
          (clatter-mode)
          (setq-local clatter--network "preview-order")
          (setq-local clatter--target "#chat")
          (setq-local clatter--buffer-type 'channel)
          (let ((conn (clatter-test-make-connection "preview-order" "me")))
            (cl-letf (((symbol-function 'clatter-get-or-create-buffer)
                       (lambda (&rest _) (current-buffer)))
                      ((symbol-function 'clatter-get-buffer)
                       (lambda (&rest _) (current-buffer))))
              (run-hook-with-args 'clatter-privmsg-hook
                                  conn '("alice" nil nil) "#chat"
                                  "https://example.com/one" nil)))
          (when (eq layout 'input-separator)
            (should (overlayp clatter--typing-indicator-overlay)))
          (let* ((contents (buffer-substring-no-properties
                            (point-min) (point-max)))
                 (message-pos (string-match "https://example.com/one" contents))
                 (preview-pos (string-match "↳ Example title" contents))
                 (prompt-pos (string-match "#chat>" contents)))
            (should message-pos)
            (should preview-pos)
            (should prompt-pos)
            (should (< message-pos preview-pos))
            (if (eq order 'oldest-first)
                (should (< preview-pos prompt-pos))
              (should (< prompt-pos message-pos)))))))))

(ert-deftest clatter-url-preview-message-anchor-finds-newest-message-end ()
  "A newest-first messages marker is before the message, not after it."
  (with-temp-buffer
    (insert "<alice> https://example.com/one\n<older> previous\n")
    (add-text-properties (point-min) (+ (point-min) 32)
                         '(clatter-text "https://example.com/one"))
    (let ((clatter--messages-marker (copy-marker (point-min))))
      (should (= (marker-position
                  (clatter-url-preview--message-end-marker
                   (current-buffer) "https://example.com/one"))
                 (+ (point-min) 32))))))

(ert-deftest clatter-url-preview-cached-title-uses-message-anchor ()
  "Cached previews use the same anchor as asynchronous previews."
  (let ((clatter-url-preview-enable t)
        (clatter-url-preview--cache (make-hash-table :test 'equal))
        (clatter-url-preview--pending (make-hash-table :test 'equal)))
    (puthash "https://example.com/one" "Cached title" clatter-url-preview--cache)
    (with-temp-buffer
      (insert "<alice> https://example.com/one\n")
      (let ((clatter--messages-marker (copy-marker (point) t))
            (conn (clatter-test-make-connection "preview-cache" "me")))
        (cl-letf (((symbol-function 'clatter-get-buffer)
                   (lambda (&rest _) (current-buffer))))
          (clatter-url-preview--on-privmsg
           conn '("alice" nil nil) "#chat" "https://example.com/one" nil))
        (should (equal (buffer-string)
                       "<alice> https://example.com/one\n↳ Cached title\n"))))))

(ert-deftest clatter-url-preview-fetch-start-failure-cleans-up ()
  "A failed curl start clears pending state without signaling repeatedly."
  (let ((clatter-url-preview--pending (make-hash-table :test 'equal))
        (clatter-url-preview--cache (make-hash-table :test 'equal))
        output)
    (with-temp-buffer
      (let ((start (copy-marker (point-min)))
            (end (copy-marker (point-max)))
            (tail (copy-marker (point-max))))
        (let ((anchor
               (clatter-url-preview--make-anchor-record
                :buffer (current-buffer)
                :start start
                :end end
                :tail tail
                :text nil
                :remaining 1)))
          (cl-letf (((symbol-function 'make-process)
                     (lambda (&rest args)
                       (setq output (plist-get args :buffer))
                       (signal 'file-missing '("curl")))))
            (clatter-url-preview--fetch
             "https://example.com/failure" (current-buffer) anchor))
          (should-not (gethash "https://example.com/failure"
                               clatter-url-preview--pending))
          (should-not (buffer-live-p output))
          (should-not (marker-buffer start))
          (should-not (marker-buffer end))
          (should-not (marker-buffer tail)))))))

(ert-deftest clatter-url-preview-fetch-sentinel-cleans-up-once ()
  "Completion renders a title and repeated notifications cannot release twice."
  (let ((clatter-url-preview--pending (make-hash-table :test 'equal))
        (clatter-url-preview--cache (make-hash-table :test 'equal)))
    (with-temp-buffer
      (let ((anchor (clatter-test-preview--source)))
        (clatter-test-preview--with-processes
          (clatter-url-preview--fetch
           "https://example.com/one" (current-buffer) anchor)
          (let* ((process (car processes))
                 (output (process-buffer process))
                 (sentinel (process-sentinel process)))
            (funcall (process-filter process) process "<title>Example title</title>")
            (clatter-test-preview--complete process)
            (funcall sentinel process "finished again")
            (should (equal (buffer-string)
                           "https://example.com/one\n↳ Example title\n"))
            (should (equal (gethash "https://example.com/one"
                                    clatter-url-preview--cache)
                           "Example title"))
            (should (= (hash-table-count clatter-url-preview--pending) 0))
            (should (= (clatter-url-preview--anchor-remaining anchor) 0))
            (should-not (process-live-p process))
            (should-not (buffer-live-p output))
            (dolist (marker (list (clatter-url-preview--anchor-start anchor)
                                  (clatter-url-preview--anchor-end anchor)
                                  (clatter-url-preview--anchor-tail anchor)))
              (should-not (marker-buffer marker)))))))))

(ert-deftest clatter-url-preview-stale-anchor-is-safe-at-buffer-end ()
  "A truncated anchor is rejected without reading past point-max."
  (with-temp-buffer
    (insert "message")
    (let ((start (copy-marker (point-max)))
          (end (copy-marker (point-max)))
          (tail (copy-marker (point-max))))
      (let ((anchor
             (clatter-url-preview--make-anchor-record
              :buffer (current-buffer)
              :start start
              :end end
              :tail tail
              :text "message"
              :remaining 1)))
        (should-not (clatter-url-preview--anchor-live-p anchor))))))

(ert-deftest clatter-url-preview-fetch-restricts-schemes ()
  "Curl keeps its timeout, scheme and redirect restrictions."
  (let ((clatter-url-preview--pending (make-hash-table :test 'equal)))
    (clatter-test-preview--with-processes
      (clatter-url-preview--fetch "https://example.com/" nil nil)
      (let ((args (car commands)))
        (should (member "--proto" args))
        (should (member "=http,https" args))
        (should (member "--proto-redir" args))
        (should (member "--max-redirs" args))
        (should (member "-m" args))))))

(ert-deftest clatter-url-preview-burst-deduplicates-and-teardown-cancels ()
  "A burst starts only four distinct fetches and teardown releases every owner."
  (let ((clatter-url-preview-enable t)
        (clatter-url-preview-max-concurrent-fetches 4)
        (clatter-url-preview--pending (make-hash-table :test 'equal))
        (clatter-url-preview--cache (make-hash-table :test 'equal)))
    (with-temp-buffer
      (let* ((urls (cl-loop for n below 24 collect (format "https://example.com/%d" n)))
             (text (mapconcat #'identity (append urls (list (car urls))) " ")))
        (insert (propertize (concat text "\n") 'clatter-text text))
        (let ((clatter--messages-marker (copy-marker (point) t)))
          (clatter-test-preview--with-processes
            (cl-letf (((symbol-function 'clatter-get-buffer)
                       (lambda (&rest _) (current-buffer))))
              (clatter-url-preview--on-privmsg
               (clatter-test-make-connection "preview-burst" "me")
               '("alice" nil nil) "#chat" text nil))
            (should (= (length processes) 4))
            (should (= (hash-table-count clatter-url-preview--pending) 4))
            (let* ((request (gethash (car urls) clatter-url-preview--pending))
                   (anchor (clatter-url-preview--request-anchor request))
                   (outputs (mapcar #'process-buffer processes))
                   (sentinels (mapcar #'process-sentinel processes)))
              (should (= (clatter-url-preview--anchor-remaining anchor) 4))
              (clatter-url-preview-teardown)
              (cl-mapc (lambda (sentinel process)
                         (funcall sentinel process "repeated"))
                       sentinels processes)
              (should (= (hash-table-count clatter-url-preview--pending) 0))
              (should (= (clatter-url-preview--anchor-remaining anchor) 0))
              (should-not (marker-buffer (clatter-url-preview--anchor-tail anchor)))
              (should-not (cl-some #'process-live-p processes))
              (should-not (cl-some #'buffer-live-p outputs)))))))))

(ert-deftest clatter-url-preview-stream-byte-boundaries ()
  "Exact byte limits succeed; max+1 and UTF-8 character-sized limits fail."
  (dolist (case '((ascii nil) (ascii extra) (unicode nil) (unicode characters)))
    (let* ((html (if (eq (car case) 'unicode) "<title>é</title>" "<title>A</title>"))
           (body (encode-coding-string html 'utf-8 t))
           (reject (cadr case))
           (clatter-url-preview-max-response-bytes
            (if (eq reject 'characters) (length html) (string-bytes body)))
           (clatter-url-preview--pending (make-hash-table :test 'equal))
           (clatter-url-preview--cache (make-hash-table :test 'equal)))
      (with-temp-buffer
        (let ((anchor (clatter-test-preview--source)))
          (clatter-test-preview--with-processes
            (clatter-url-preview--fetch "https://example.com/one" (current-buffer) anchor)
            (let* ((process (car processes))
                   (request (gethash "https://example.com/one" clatter-url-preview--pending))
                   (output (process-buffer process))
                   (filter (process-filter process)))
              ;; No Content-Length: arbitrary streamed chunks are counted.
              (dotimes (n (length body))
                (funcall filter process (substring body n (1+ n))))
              (when (eq reject 'extra) (funcall filter process "x"))
              (if reject
                  (progn
                    (should (clatter-url-preview--request-done request))
                    (should-not (process-live-p process))
                    (should-not (buffer-live-p output))
                    (should-not (gethash "https://example.com/one"
                                         clatter-url-preview--cache)))
                (should (= (buffer-size output) clatter-url-preview-max-response-bytes))
                (should (= (clatter-url-preview--request-bytes request)
                           clatter-url-preview-max-response-bytes))
                (clatter-test-preview--complete process)
                (should (equal (gethash "https://example.com/one"
                                        clatter-url-preview--cache)
                               (if (eq (car case) 'unicode) "é" "A"))))
              (should (= (hash-table-count clatter-url-preview--pending) 0))
              (should-not (marker-buffer (clatter-url-preview--anchor-start anchor))))))))))

(ert-deftest clatter-url-preview-large-chunk-is-never-retained ()
  "An oversized unknown-length chunk stops the fetch without growing its buffer."
  (let ((clatter-url-preview-max-response-bytes 32)
        (clatter-url-preview--pending (make-hash-table :test 'equal)))
    (with-temp-buffer
      (let ((anchor (clatter-test-preview--source)))
        (clatter-test-preview--with-processes
          (clatter-url-preview--fetch "https://example.com/one" (current-buffer) anchor)
          (let* ((process (car processes))
                 (output (process-buffer process))
                 (request (gethash "https://example.com/one" clatter-url-preview--pending)))
            (funcall (process-filter process) process (make-string 1024 ?x))
            (should (= (clatter-url-preview--request-bytes request) 0))
            (should-not (buffer-live-p output))
            (should-not (process-live-p process))
            (should-not (marker-buffer (clatter-url-preview--anchor-tail anchor)))))))))

(ert-deftest clatter-url-preview-abort-releases-once ()
  "Aborted processes remove pending state, output buffers and anchor ownership."
  (let ((clatter-url-preview--pending (make-hash-table :test 'equal)))
    (with-temp-buffer
      (let ((anchor (clatter-test-preview--source)))
        (clatter-test-preview--with-processes
          (clatter-url-preview--fetch "https://example.com/one" (current-buffer) anchor)
          (let* ((process (car processes))
                 (output (process-buffer process))
                 (sentinel (process-sentinel process)))
            (delete-process process)
            (funcall sentinel process "again")
            (should (= (hash-table-count clatter-url-preview--pending) 0))
            (should (= (clatter-url-preview--anchor-remaining anchor) 0))
            (should-not (buffer-live-p output))
            (should-not (marker-buffer (clatter-url-preview--anchor-tail anchor)))))))))

(ert-deftest clatter-url-preview-stale-completion-cannot-clobber-new-fetch ()
  "An old URL completion cannot cache a title or remove a newer URL owner."
  (let ((clatter-url-preview--pending (make-hash-table :test 'equal))
        (clatter-url-preview--cache (make-hash-table :test 'equal)))
    (with-temp-buffer
      (let ((old-anchor (clatter-test-preview--source)))
        (clatter-test-preview--with-processes
          (clatter-url-preview--fetch "https://example.com/one" (current-buffer) old-anchor)
          (let* ((old (car processes))
                 (old-output (process-buffer old)))
            (funcall (process-filter old) old "<title>Stale title</title>")
            (remhash "https://example.com/one" clatter-url-preview--pending)
            (let ((new-anchor (clatter-test-preview--source)))
              (clatter-url-preview--fetch "https://example.com/one" (current-buffer) new-anchor)
              (let ((new (car processes))
                    (owner (gethash "https://example.com/one" clatter-url-preview--pending)))
                (clatter-test-preview--complete old)
                (should (eq (gethash "https://example.com/one" clatter-url-preview--pending)
                            owner))
                (should-not (gethash "https://example.com/one" clatter-url-preview--cache))
                (should-not (buffer-live-p old-output))
                (should-not (marker-buffer (clatter-url-preview--anchor-tail old-anchor)))
                (funcall (process-filter new) new "<title>Current title</title>")
                (clatter-test-preview--complete new)
                (should (string-match-p "↳ Current title" (buffer-string)))
                (should-not (string-match-p "Stale title" (buffer-string)))))))))))

(ert-deftest clatter-url-preview-cached-duplicates-render-once ()
  "Repeated URLs in one message share only one cached title insertion."
  (let ((clatter-url-preview-enable t)
        (clatter-url-preview--pending (make-hash-table :test 'equal))
        (clatter-url-preview--cache (make-hash-table :test 'equal)))
    (puthash "https://example.com/one" "Only once" clatter-url-preview--cache)
    (with-temp-buffer
      (let ((text "https://example.com/one https://example.com/one"))
        (insert (propertize (concat text "\n") 'clatter-text text))
        (let ((clatter--messages-marker (copy-marker (point) t)))
          (cl-letf (((symbol-function 'clatter-get-buffer)
                     (lambda (&rest _) (current-buffer))))
            (clatter-url-preview--on-privmsg
             (clatter-test-make-connection "preview-duplicate" "me")
             '("alice" nil nil) "#chat" text nil))
          (should (= (count-lines (point-min) (point-max)) 2)))))))

(ert-deftest clatter-url-preview-cached-cap-preserves-draft-undo ()
  "Cached titles obey the physical cap without undoing history, prompt or padding."
  (dolist (layout '(mode-line input-separator))
    (dolist (order '(oldest-first newest-first))
      (let* ((clatter-typing-indicator-location layout)
             (clatter-message-order order)
             (clatter-fill-column nil)
             (clatter-buffer-max-lines 1)
             (clatter-url-preview-enable t)
             (clatter-url-preview--pending (make-hash-table :test 'equal))
             (clatter-url-preview--cache (make-hash-table :test 'equal))
             (urls (cl-loop for n below 10 collect (format "https://example.com/%d" n)))
             (text (mapconcat #'identity urls " ")))
        (dolist (url urls) (puthash url "Cached title" clatter-url-preview--cache))
        (with-temp-buffer
          (clatter-mode)
          (setq-local clatter--target "#chat")
          (setq-local clatter--buffer-type 'channel)
          (clatter--setup-prompt (current-buffer))
          (clatter--ensure-input-padding 3)
          (buffer-enable-undo)
          (setq buffer-undo-list nil)
          (goto-char (clatter--input-end))
          (insert "draft")
          (undo-boundary)
          (delete-region (1- (point)) (point))
          (clatter--insert-message (current-buffer) text t (list 'clatter-text text))
          (let ((prompt (buffer-substring clatter--prompt-marker clatter--input-marker))
                (padding (and clatter--input-padding-end
                              (buffer-substring (point-min) clatter--input-padding-end)))
                (typing-overlay clatter--typing-indicator-overlay))
            (setq-local flyspell-mode t)
            (setq-local flyspell-changes nil)
            (add-hook 'after-change-functions
                      (lambda (beg end _old) (push (cons beg end) flyspell-changes))
                      nil t)
            (cl-letf (((symbol-function 'clatter-get-buffer)
                       (lambda (&rest _) (current-buffer))))
              (clatter-url-preview--on-privmsg
               (clatter-test-make-connection "preview-cap" "me")
               '("alice" nil nil) "#chat" text nil))
            (should (= (if (eq order 'oldest-first)
                           (count-lines clatter--input-padding-end clatter--messages-marker)
                         (count-lines clatter--messages-marker (point-max)))
                       1))
            (should-not flyspell-changes)
            (should (equal (clatter--get-input) "draf"))
            (should (equal (cl-loop for entry in buffer-undo-list
                                    when (and (consp entry) (stringp (car entry)))
                                    collect (car entry))
                           '("t")))
            (let ((history (buffer-string))
                  (pending (primitive-undo 1 buffer-undo-list)))
              (should (equal (clatter--get-input) "draft"))
              (primitive-undo 1 pending)
              (should (equal (clatter--get-input) ""))
              (should (equal (replace-regexp-in-string "draf" "" history)
                             (buffer-string))))
            (should (equal (buffer-substring clatter--prompt-marker clatter--input-marker)
                           prompt))
            (should (get-text-property clatter--prompt-marker 'read-only))
            (when padding
              (should (equal (buffer-substring (point-min) clatter--input-padding-end)
                             padding))
              (should (get-text-property (point-min) 'clatter-input-padding))
              (should (get-text-property (point-min) 'read-only)))
            (when typing-overlay
              (should (eq (overlay-buffer typing-overlay) (current-buffer))))))))))

(ert-deftest clatter-url-preview-retains-and-rebases-pending-input-spelling ()
  "Preview edits discard history spelling changes but retain the pending draft."
  (dolist (order '(oldest-first newest-first))
    (let ((clatter-message-order order)
          (clatter-fill-column nil)
          (clatter-buffer-max-lines 2))
      (with-temp-buffer
        (clatter-mode)
        (setq-local clatter--target "#chat")
        (setq-local clatter--buffer-type 'channel)
        (clatter--setup-prompt (current-buffer))
        (clatter--insert-message (current-buffer) "older" t)
        (let ((text "https://example.com/one"))
          (clatter--insert-message (current-buffer) text t (list 'clatter-text text))
          (goto-char (clatter--input-end))
          (insert "draft")
          (let ((record (cons (1+ (marker-position clatter--input-marker))
                              (+ 4 (marker-position clatter--input-marker))))
                (anchor (clatter-url-preview--message-anchor (current-buffer) text 1)))
            (setq-local flyspell-mode t)
            (setq-local flyspell-changes (list (cons 1 2) record))
            (add-hook 'after-change-functions
                      (lambda (beg end _old) (push (cons beg end) flyspell-changes))
                      nil t)
            (unwind-protect
                (clatter-url-preview--insert "Title" (current-buffer) anchor)
              (clatter-url-preview--release-anchor anchor))
            (should (equal (clatter--get-input) "draft"))
            (should (equal flyspell-changes (list record)))
            (should (eq (car flyspell-changes) record))
            (should (= (car record) (1+ (marker-position clatter--input-marker))))
            (should (= (cdr record) (+ 4 (marker-position clatter--input-marker))))))))))

(provide 'test-url-preview)

;;; test-url-preview.el ends here
