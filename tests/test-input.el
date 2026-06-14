;;; test-input.el --- Tests for prompt placement and input handling -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the bottom-anchored prompt (oldest-first), the top prompt
;; (newest-first), input get/clear, and jump-to-prompt-on-type.

;;; Code:

(require 'ert)
(require 'clatter-ui)

(defmacro clatter-input-test--with (order &rest body)
  "Run BODY in a fresh clatter-mode buffer with message ORDER and a prompt."
  (declare (indent 1))
  `(let ((clatter-message-order ,order))
     (with-temp-buffer
       (clatter-mode)
       (setq-local clatter--target "#test")
       (clatter--setup-prompt (current-buffer))
       ,@body)))

(defun clatter-input-test--last-line ()
  "Return the text of the buffer's last line."
  (save-excursion
    (goto-char (point-max))
    (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

(defun clatter-input-test--first-line ()
  "Return the text of the buffer's first line."
  (save-excursion
    (goto-char (point-min))
    (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

(ert-deftest clatter-input-oldest-first-prompt-at-bottom ()
  "Oldest-first: prompt is on the last line; messages accumulate above it."
  (clatter-input-test--with 'oldest-first
    (should (string-prefix-p "#test>" (clatter-input-test--last-line)))
    (clatter--insert-message (current-buffer) "first")
    (clatter--insert-message (current-buffer) "second")
    ;; Prompt still on the last line.
    (should (string-prefix-p "#test>" (clatter-input-test--last-line)))
    ;; Chronological order above the prompt: first, then second, then prompt.
    (should (string-match-p
             "first\nsecond\n#test>"
             (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest clatter-input-newest-first-prompt-at-top ()
  "Newest-first: prompt is on the first line; newest message sits just below."
  (clatter-input-test--with 'newest-first
    (should (string-prefix-p "#test>" (clatter-input-test--first-line)))
    (clatter--insert-message (current-buffer) "first")
    (clatter--insert-message (current-buffer) "second")
    (should (string-prefix-p "#test>" (clatter-input-test--first-line)))
    ;; Newest (second) is directly below the prompt, older (first) below it.
    (should (string-match-p
             "#test>.*\nsecond\nfirst\n"
             (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest clatter-input-get-and-clear-oldest ()
  "Input get/clear work with a bottom prompt, even after messages arrive."
  (clatter-input-test--with 'oldest-first
    (clatter--insert-message (current-buffer) "noise")
    (goto-char (point-max))
    (insert "hello world")
    (should (equal (clatter--get-input) "hello world"))
    (should (= (clatter--input-end) (point-max)))
    (clatter--clear-input)
    (should (equal (clatter--get-input) ""))
    ;; The message above the prompt is untouched.
    (should (string-match-p "noise" (buffer-string)))))

(ert-deftest clatter-input-get-and-clear-newest ()
  "Input get/clear work with a top prompt."
  (clatter-input-test--with 'newest-first
    (clatter--insert-message (current-buffer) "noise")
    (goto-char (marker-position clatter--input-marker))
    (insert "hello")
    (should (equal (clatter--get-input) "hello"))
    (clatter--clear-input)
    (should (equal (clatter--get-input) ""))))

(ert-deftest clatter-input-move-to-prompt-oldest ()
  "Self-inserting from the message area jumps to the input."
  (clatter-input-test--with 'oldest-first
    (clatter--insert-message (current-buffer) "noise")
    (goto-char (point-min))                 ; up in the messages
    (let ((this-command 'self-insert-command)
          (clatter-move-to-prompt t))
      (clatter--move-to-prompt)
      (should (= (point) (clatter--input-end))))))

(ert-deftest clatter-input-move-to-prompt-disabled ()
  "With `clatter-move-to-prompt' nil, point is left alone."
  (clatter-input-test--with 'oldest-first
    (clatter--insert-message (current-buffer) "noise")
    (goto-char (point-min))
    (let ((this-command 'self-insert-command)
          (clatter-move-to-prompt nil))
      (clatter--move-to-prompt)
      (should (= (point) (point-min))))))

(ert-deftest clatter-input-move-to-prompt-not-self-insert ()
  "Non-self-insert commands never move point."
  (clatter-input-test--with 'oldest-first
    (clatter--insert-message (current-buffer) "noise")
    (goto-char (point-min))
    (let ((this-command 'next-line)
          (clatter-move-to-prompt t))
      (clatter--move-to-prompt)
      (should (= (point) (point-min))))))

(provide 'test-input)

;;; test-input.el ends here
