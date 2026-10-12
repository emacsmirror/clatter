;;; test-image.el --- Tests for clatter-image URL scanning -*- lexical-binding: t; -*-

;;; Commentary:

;; Regression tests for inline image URL detection and scanning.

;;; Code:

(require 'ert)
(require 'test-helper)
(require 'clatter-commands)
(require 'clatter-image)

(ert-deftest clatter-image-url-p-basic ()
  "Direct image links are recognized, non-image links are not."
  (should (clatter-image--url-p "https://example.com/a.png"))
  (should (clatter-image--url-p "http://example.com/path/photo.JPG"))
  (should-not (clatter-image--url-p "https://example.com/page.html"))
  (should-not (clatter-image--url-p "https://example.com/")))

(ert-deftest clatter-image-url-p-query-and-fragment ()
  "Query strings and fragments are stripped before checking the extension."
  (should (clatter-image--url-p "https://example.com/a.png?size=large"))
  (should (clatter-image--url-p "https://example.com/a.png#section"))
  (should (clatter-image--url-p "https://example.com/a.png?v=2#x")))

(defmacro clatter-test-image--with-processes (&rest body)
  "Replace curl startup with real Emacs pipes; do not require a GUI decoder."
  (declare (indent 0) (debug t))
  `(let (processes commands)
     (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
               ((symbol-function 'make-process)
                (lambda (&rest args)
                  (push (plist-get args :command) commands)
                  (let ((process
                         (make-pipe-process
                          :name (plist-get args :name)
                          :buffer (plist-get args :buffer)
                          :coding (plist-get args :coding) :noquery t
                          :sentinel (plist-get args :sentinel))))
                    (push process processes)
                    process))))
       (unwind-protect
           (progn ,@body)
         (dolist (process processes)
           (when (process-live-p process) (delete-process process)))))))

(defun clatter-test-image--complete (process)
  "Deliver a successful completion without downloading or decoding an image."
  (let ((real-status (symbol-function 'process-status))
        (real-exit (symbol-function 'process-exit-status)))
    (cl-letf (((symbol-function 'process-status)
               (lambda (other)
                 (if (eq other process) 'exit (funcall real-status other))))
              ((symbol-function 'process-exit-status)
               (lambda (other)
                 (if (eq other process) 0 (funcall real-exit other)))))
      (funcall (process-sentinel process) process "finished"))))

(ert-deftest clatter-image-extract-urls-basic ()
  "All URLs in a message are returned in order."
  (should (equal (clatter-image--extract-urls "see http://a.com/x.png here")
                 '("http://a.com/x.png")))
  (should (equal (clatter-image--extract-urls
                  "http://a.com/1.png and http://b.com/2.jpg")
                 '("http://a.com/1.png" "http://b.com/2.jpg")))
  (should (null (clatter-image--extract-urls "no links at all"))))

(ert-deftest clatter-image-extract-urls-terminates-with-query ()
  "A URL with a query string preceded by text must not loop forever.

This reproduces the freeze where `match-end' was read after
`clatter-image--url-p' clobbered the match data: the search position
reset behind the URL and the same URL matched endlessly.  We bound the
work with a timer so a regression fails fast instead of hanging the
test run."
  (let ((text "Check out this cool pic everyone: https://example.com/a.png?v=2")
        (result nil)
        (timed-out nil))
    (with-timeout (5 (setq timed-out t))
      (setq result (clatter-image--extract-urls text)))
    (should-not timed-out)
    (should (equal result '("https://example.com/a.png?v=2")))))

(ert-deftest clatter-image-extract-urls-multiple-with-queries ()
  "Several query-string URLs with leading text all terminate and return."
  (let ((text (concat "first look here https://a.com/1.png?x=1 "
                      "and also there https://b.com/2.jpg?y=2#frag "
                      "plus https://c.com/3.gif"))
        (result nil)
        (timed-out nil))
    (with-timeout (5 (setq timed-out t))
      (setq result (clatter-image--extract-urls text)))
    (should-not timed-out)
    (should (equal result
                   '("https://a.com/1.png?x=1"
                     "https://b.com/2.jpg?y=2#frag"
                     "https://c.com/3.gif")))))

(ert-deftest clatter-image-fetch-restricts-schemes ()
  "Image curl keeps its scheme, redirect, timeout and download restrictions."
  (let ((clatter-image-enable t)
        (clatter-image--active-fetches 0))
    (clatter-test-image--with-processes
      (with-temp-buffer
        (insert "source\n")
        (clatter-image--fetch "https://example.com/a.png"
                              (current-buffer) (point-marker)))
      (let ((args (car commands)))
        (should (member "--proto" args))
        (should (member "=http,https" args))
        (should (member "--proto-redir" args))
        (should (member "--max-redirs" args))
        (should (member "--max-filesize" args))
        (should (member "-m" args))))))

(ert-deftest clatter-image-skips-and-start-failures-detach-markers ()
  "Every skip and failed startup releases its marker and download resources."
  (dolist (reason '(disabled terminal full invalid start-failure))
    (let ((clatter-image-enable (not (eq reason 'disabled)))
          (clatter-image--active-fetches 0)
          (clatter-image-max-concurrent-fetches (if (eq reason 'full) 0 4))
          output started)
      (with-temp-buffer
        (insert "source\n")
        (let ((marker (point-marker)))
          (cl-letf (((symbol-function 'display-graphic-p)
                     (lambda (&rest _) (not (eq reason 'terminal))))
                    ((symbol-function 'make-process)
                     (lambda (&rest args)
                       (setq started t output (plist-get args :buffer))
                       (signal 'file-missing '("curl")))))
            (clatter-image--fetch
             (if (eq reason 'invalid) "https://example.com/page" "https://example.com/a.png")
             (current-buffer) marker))
          (should (eq started (eq reason 'start-failure)))
          (should (= clatter-image--active-fetches 0))
          (should-not (marker-buffer marker))
          (should-not (buffer-live-p output)))))))

(ert-deftest clatter-image-stale-source-is-discarded-before-decoding ()
  "Clearing or truncating a source prevents image insertion in either order."
  (dolist (order '(oldest-first newest-first))
    (dolist (removal '(clear truncate partial))
      (let ((clatter-message-order order)
            (clatter-fill-column nil)
            (clatter-buffer-max-lines nil)
            (clatter-image-enable t)
            (clatter-image--active-fetches 0)
            decoded)
        (with-temp-buffer
          (clatter-mode)
          (setq-local clatter--target "#chat")
          (setq-local clatter--buffer-type 'channel)
          (clatter--setup-prompt (current-buffer))
          (let ((text "https://example.com/a.png"))
            (clatter--insert-message (current-buffer) text t (list 'clatter-text text))
            (clatter-test-image--with-processes
              (let ((marker (copy-marker clatter--messages-marker)))
                (clatter-image--scan-message text (current-buffer) marker)
                (should-not (marker-buffer marker)))
              (let* ((process (car processes))
                     (output (process-buffer process))
                     (sentinel (process-sentinel process)))
                (with-current-buffer output (insert "GIF89aXYZ"))
                (pcase removal
                  ('clear (clatter-cmd-clear nil))
                  ('truncate
                   (let ((clatter-buffer-max-lines 1))
                     (clatter--insert-message (current-buffer) "replacement" t)))
                  ('partial
                   (let ((inhibit-read-only t)
                         (buffer-undo-list t))
                     (goto-char (point-min))
                     (search-forward text)
                     (delete-region (match-beginning 0) (1+ (match-beginning 0))))))
                (cl-letf (((symbol-function 'create-image)
                           (lambda (&rest _) (setq decoded t) nil)))
                  (clatter-test-image--complete process))
                (funcall sentinel process "again")
                (should-not decoded)
                (should (= clatter-image--active-fetches 0))
                (should-not (buffer-live-p output))
                (should-not (string-match-p "\\[image:" (buffer-string)))))))))))

(ert-deftest clatter-image-insertion-cap-preserves-draft-undo ()
  "Images cap history immediately while retaining input undo and protected layout."
  (dolist (layout '(mode-line input-separator))
    (dolist (order '(oldest-first newest-first))
      (let ((clatter-typing-indicator-location layout)
            (clatter-message-order order)
            (clatter-fill-column nil)
            (clatter-buffer-max-lines 2)
            (clatter-image-enable t)
            (clatter-image--active-fetches 0))
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
          (clatter--insert-message (current-buffer) "older-message" t)
          (let ((text "https://example.com/a.png"))
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
              ;; Only GUI decoder/display primitives are replaced; all source,
              ;; insertion, truncation and undo bookkeeping is the real path.
              (clatter-test-image--with-processes
                (clatter-image--scan-message
                 text (current-buffer) (copy-marker clatter--messages-marker))
                (let* ((process (car processes))
                       (output (process-buffer process))
                       (sentinel (process-sentinel process)))
                  (with-current-buffer output (insert "GIF89aXYZ"))
                  (cl-letf (((symbol-function 'create-image) (lambda (&rest _) 'test-image))
                            ((symbol-function 'image-size) (lambda (&rest _) '(20 . 10)))
                            ((symbol-function 'insert-image)
                             (lambda (_image alt &rest _) (insert alt))))
                    (clatter-test-image--complete process))
                  (funcall sentinel process "again")
                  (should (= clatter-image--active-fetches 0))
                  (should-not (buffer-live-p output))))
              (should (= (if (eq order 'oldest-first)
                             (count-lines clatter--input-padding-end clatter--messages-marker)
                           (count-lines clatter--messages-marker (point-max)))
                         2))
              (should (string-match-p "\\[image: https://example.com/a.png\\]"
                                      (buffer-string)))
              (should-not (string-match-p "older-message" (buffer-string)))
              (save-excursion
                (goto-char (point-min))
                (search-forward "[image:")
                (should (get-text-property (match-beginning 0) 'read-only)))
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
                (should (eq (overlay-buffer typing-overlay) (current-buffer)))))))))))

(ert-deftest clatter-image-insertion-keeps-visible-input-pinned ()
  "A real history image insertion refreshes the oldest-first input viewport."
  (let ((clatter-message-order 'oldest-first)
        (clatter-typing-indicator-location 'input-separator)
        (clatter-fill-column nil)
        (clatter-buffer-max-lines nil))
    (save-window-excursion
      (with-temp-buffer
        (switch-to-buffer (current-buffer))
        (clatter-mode)
        (setq-local clatter--target "#chat")
        (setq-local clatter--buffer-type 'channel)
        (clatter--setup-prompt (current-buffer))
        (clatter--insert-message (current-buffer) "source" t)
        (let ((marker (copy-marker clatter--messages-marker))
              (window (selected-window)))
          (goto-char (clatter--input-end))
          (cl-letf (((symbol-function 'image-size) (lambda (&rest _) '(20 . 10)))
                    ((symbol-function 'insert-image)
                     (lambda (_image alt &rest _) (insert alt))))
            (clatter-image--insert 'test-image (current-buffer) marker
                                   "https://example.com/a.png"))
          (set-marker marker nil)
          (should (= (count-screen-lines (window-start window) (point-max) nil window)
                     (window-body-height window))))))))

(ert-deftest clatter-image-retains-and-rebases-pending-input-spelling ()
  "Image history edits preserve and reposition pending draft spelling records."
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
        (let ((text "https://example.com/a.png"))
          (clatter--insert-message (current-buffer) text t (list 'clatter-text text))
          (let* ((position (marker-position clatter--messages-marker))
                 (end (if (eq order 'newest-first)
                          (next-single-property-change position 'clatter-text nil (point-max))
                        position))
                 (marker (copy-marker end)))
            (goto-char (clatter--input-end))
            (insert "draft")
            (let ((record (cons (1+ (marker-position clatter--input-marker))
                                (+ 4 (marker-position clatter--input-marker)))))
              (setq-local flyspell-mode t)
              (setq-local flyspell-changes (list (cons 1 2) record))
              (add-hook 'after-change-functions
                        (lambda (beg finish _old) (push (cons beg finish) flyspell-changes))
                        nil t)
              (unwind-protect
                  (cl-letf (((symbol-function 'image-size) (lambda (&rest _) '(20 . 10)))
                            ((symbol-function 'insert-image)
                             (lambda (_image alt &rest _) (insert alt))))
                    (clatter-image--insert 'test-image (current-buffer) marker text))
                (set-marker marker nil))
              (should (equal (clatter--get-input) "draft"))
              (should (equal flyspell-changes (list record)))
              (should (eq (car flyspell-changes) record))
              (should (= (car record) (1+ (marker-position clatter--input-marker))))
              (should (= (cdr record) (+ 4 (marker-position clatter--input-marker)))))))))))

(provide 'test-image)

;;; test-image.el ends here
