;;; test-input.el --- Tests for prompt placement and input handling -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the bottom-anchored prompt (oldest-first), the top prompt
;; (newest-first), input get/clear, and jump-to-prompt-on-type.

;;; Code:

(require 'ert)
(require 'test-helper)
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

(defmacro clatter-input-test--with-window (order &rest body)
  "Display a temporary Clatter buffer with message ORDER, then run BODY.
Within BODY, `buffer' and `window' name the temporary buffer and its window."
  (declare (indent 1) (debug t))
  `(let* ((clatter-message-order ,order)
          (buffer (generate-new-buffer " *clatter-input-window-test*"))
          (window (selected-window))
          (original-buffer (window-buffer window)))
     (unwind-protect
         (progn
           (set-window-buffer window buffer)
           (with-current-buffer buffer
             (clatter-mode)
             (setq-local clatter--target "#test")
             (clatter--setup-prompt buffer)
             (clatter--refresh-input-spacers buffer)
             ,@body))
       (when (window-live-p window)
         (set-window-buffer window original-buffer))
       (when (buffer-live-p buffer)
         (kill-buffer buffer)))))

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

(defun clatter-input-test--prompt ()
  "Return the current prompt without text properties."
  (buffer-substring-no-properties clatter--prompt-marker
                                  clatter--input-marker))

(defun clatter-input-test--spacer-lines (window)
  "Return the number of protected layout lines available to WINDOW."
  (ignore window)
  (when clatter--input-padding-end
    (count-lines (point-min) clatter--input-padding-end)))

(ert-deftest clatter-prompt-format-expands-placeholders ()
  "String prompt formats expand target, nick, network, and percent."
  (let ((clatter-prompt-format "%N/%n:%t %% "))
    (with-temp-buffer
      (clatter-mode)
      (setq-local clatter--network "testnet")
      (setq-local clatter--target "#test")
      (let ((conn (clatter-test-make-connection "testnet" "alice")))
        (unwind-protect
            (progn
              (clatter--setup-prompt (current-buffer))
              (should (string-suffix-p "testnet/alice:#test % "
                                       (clatter-input-test--prompt))))
          (remhash (clatter-connection-network-id conn) clatter-connections))))))

(ert-deftest clatter-prompt-format-function-receives-context ()
  "Function prompt formats receive the Clatter buffer."
  (let ((clatter-prompt-format
         (lambda (buffer)
           (with-current-buffer buffer
             (let ((conn (clatter-get-connection clatter--network)))
               (format "%s@%s/%s>"
                       (clatter-connection-nick conn)
                       clatter--network
                       clatter--target))))))
    (with-temp-buffer
      (clatter-mode)
      (setq-local clatter--network "testnet")
      (setq-local clatter--target "#test")
      (let ((conn (clatter-test-make-connection "testnet" "alice")))
        (unwind-protect
            (progn
              (clatter--setup-prompt (current-buffer))
              (should (string-suffix-p "alice@testnet/#test>"
                                       (clatter-input-test--prompt))))
          (remhash (clatter-connection-network-id conn) clatter-connections))))))

(ert-deftest clatter-prompt-format-needs-nick-detects-unescaped-specifier ()
  "Nick prompt detection ignores literal percent escapes."
  (let ((clatter-prompt-format "%n> "))
    (should (clatter--prompt-format-needs-nick-p)))
  (let ((clatter-prompt-format "%t %%n> "))
    (should-not (clatter--prompt-format-needs-nick-p)))
  (let ((clatter-prompt-format (lambda (_buffer) "prompt> ")))
    (should (clatter--prompt-format-needs-nick-p))))

(ert-deftest clatter-prompt-nick-hides-mode-line-nick ()
  "Prompts that display the current nick do not repeat it in the mode-line."
  (let ((clatter-prompt-format "%n> "))
    (with-temp-buffer
      (clatter-mode)
      (setq-local clatter--network "testnet")
      (setq-local clatter--target "#test")
      (let ((conn (clatter-test-make-connection "testnet" "alice")))
        (unwind-protect
            (progn
              (clatter--setup-prompt (current-buffer))
              (should (string-suffix-p "alice> "
                                       (clatter-input-test--prompt)))
              (should-not (string-match-p "alice" (clatter--mode-line-string))))
          (remhash (clatter-connection-network-id conn) clatter-connections))))))

(ert-deftest clatter-prompt-target-keeps-mode-line-nick ()
  "Prompts without the current nick keep the nick in the mode-line."
  (let ((clatter-prompt-format "%t> "))
    (with-temp-buffer
      (clatter-mode)
      (setq-local clatter--network "testnet")
      (setq-local clatter--target "#test")
      (let ((conn (clatter-test-make-connection "testnet" "alice")))
        (unwind-protect
            (progn
              (clatter--setup-prompt (current-buffer))
              (should (string-suffix-p "#test> "
                                       (clatter-input-test--prompt)))
              (should (string-match-p "alice" (clatter--mode-line-string))))
          (remhash (clatter-connection-network-id conn) clatter-connections))))))

(ert-deftest clatter-prompt-refresh-preserves-pending-input ()
  "Refreshing a nick-based prompt retains typed input and input point."
  (let ((clatter-prompt-format "%n> "))
    (with-temp-buffer
      (clatter-mode)
      (setq-local clatter--network "testnet")
      (setq-local clatter--target "#test")
      (let ((conn (clatter-test-make-connection "testnet" "alice")))
        (unwind-protect
            (progn
              (clatter--setup-prompt (current-buffer))
              (goto-char (clatter--input-end))
              (insert "draft")
              (goto-char (+ (marker-position clatter--input-marker) 2))
              (setf (clatter-connection-nick conn) "bob")
              (clatter--refresh-prompt)
              (should (string-suffix-p "bob> "
                                       (clatter-input-test--prompt)))
              (should (equal (clatter--get-input) "draft"))
              (should (= (point) (+ (marker-position clatter--input-marker) 2))))
          (remhash (clatter-connection-network-id conn) clatter-connections))))))

(ert-deftest clatter-prompt-nick-hook-refreshes-nick-prompts-only ()
  "Own nick changes refresh nick prompts and preserve other prompt formats."
  (let ((conn (clatter-test-make-connection "testnet" "alice"))
        nick-buffer
        target-buffer)
    (unwind-protect
        (progn
          (setq nick-buffer (clatter-get-or-create-buffer "testnet" "#nick" 'channel))
          (with-current-buffer nick-buffer
            (setq-local clatter-prompt-format "%n> ")
            (clatter--setup-prompt nick-buffer)
            (goto-char (clatter--input-end))
            (insert "draft"))
          (setq target-buffer (clatter-get-or-create-buffer "testnet" "#target" 'channel))
          (with-current-buffer target-buffer
            (setq-local clatter-prompt-format "%t> ")
            (clatter--setup-prompt target-buffer)
            (should (string-suffix-p "#target> "
                                     (clatter-input-test--prompt))))
          (setf (clatter-connection-nick conn) "bob")
          (clatter-ui--on-nick conn (clatter-parse-prefix "alice!u@h") "bob")
          (with-current-buffer nick-buffer
            (should (string-suffix-p "bob> "
                                     (clatter-input-test--prompt)))
            (should (equal (clatter--get-input) "draft")))
          (with-current-buffer target-buffer
            (should (string-suffix-p "#target> "
                                     (clatter-input-test--prompt)))))
      (when nick-buffer
        (clatter-remove-buffer "testnet" "#nick")
        (when (buffer-live-p nick-buffer)
          (kill-buffer nick-buffer)))
      (when target-buffer
        (clatter-remove-buffer "testnet" "#target")
        (when (buffer-live-p target-buffer)
          (kill-buffer target-buffer)))
      (remhash (clatter-connection-network-id conn) clatter-connections))))

(ert-deftest clatter-prompt-default-preserves-historical-layout ()
  "The default prompt layout remains unpadded."
  (let ((clatter-message-order 'oldest-first)
        (clatter-nick-column-width 10)
        (clatter-prompt-format "%t> ")
        (clatter-prompt-alignment nil))
    (with-temp-buffer
      (clatter-mode)
      (setq-local clatter--target "#test")
      (clatter--setup-prompt (current-buffer))
      (should (equal (clatter-input-test--prompt) "#test> "))
      (should (= (current-column) 7)))))

(ert-deftest clatter-prompt-is-right-aligned-to-nick-column ()
  "A prompt's visible text ends at the nick column boundary."
  (let ((clatter-message-order 'oldest-first)
        (clatter-nick-column-width 10)
        (clatter-prompt-format "%t> ")
        (clatter-prompt-alignment 'right))
    (with-temp-buffer
      (clatter-mode)
      (setq-local clatter--target "#test")
      (clatter--setup-prompt (current-buffer))
      (should (equal (clatter-input-test--prompt)
                     "    #test> "))
      ;; The prompt's trailing space occupies the same separator column as a
      ;; rendered message, so typed input starts at column 11.
      (should (= (current-column) 11)))))

(ert-deftest clatter-prompt-long-text-is-preserved ()
  "Overlong nick and target prompts are neither truncated nor padded."
  (let ((clatter-message-order 'oldest-first)
        (clatter-nick-column-width 10)
        (clatter-prompt-alignment 'right)
        (conn (clatter-test-make-connection
               "testnet" "very-long-nickname")))
    (unwind-protect
        (dolist (case '(("%t> " . "#very-long-channel-name> ")
                        ("%n> " . "very-long-nickname> ")))
          (let ((clatter-prompt-format (car case)))
            (with-temp-buffer
              (clatter-mode)
              (setq-local clatter--network "testnet")
              (setq-local clatter--target "#very-long-channel-name")
              (clatter--setup-prompt (current-buffer))
              (should (equal (clatter-input-test--prompt) (cdr case)))
              (should (= (current-column) (string-width (cdr case)))))))
      (remhash (clatter-connection-network-id conn) clatter-connections))))

(ert-deftest clatter-input-oldest-first-prompt-at-bottom ()
  "Oldest-first: prompt is on the last line; messages accumulate above it."
  (clatter-input-test--with 'oldest-first
    (should (string-suffix-p "#test> " (clatter-input-test--last-line)))
    (clatter--insert-message (current-buffer) "first")
    (clatter--insert-message (current-buffer) "second")
    ;; Prompt still on the last line.
    (should (string-suffix-p "#test> " (clatter-input-test--last-line)))
    ;; Chronological order above the prompt: first, then second, then prompt.
    (should (string-match-p
             "first\nsecond\n *#test>"
             (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest clatter-input-oldest-first-pins-short-buffer-to-window-bottom ()
  "History grows upward without moving the oldest-first input row."
  (clatter-input-test--with-window 'oldest-first
    (let ((height (window-body-height window)))
      (should (> height 3))
      (should (= (clatter-input-test--spacer-lines window)
                 (1- height)))
      (should (get-text-property (point-min) 'clatter-input-padding))
      (clatter--insert-message buffer "first" t)
      (clatter--insert-message buffer "second" t)
      (should (= (clatter-input-test--spacer-lines window)
                 (1- height)))
      ;; The window start advances through protected padding as messages stack
      ;; upward, while real history begins at the padding marker.
      (let ((first-position (marker-position clatter--input-padding-end)))
        (save-excursion
          (goto-char first-position)
          (should (looking-at-p "first"))))
      (should (= (count-screen-lines (window-start window)
                                     (point-max) nil window)
                 height)))))

(ert-deftest clatter-input-newest-first-does-not-create-window-spacer ()
  "Top-prompt buffers retain their existing window layout."
  (clatter-input-test--with-window 'newest-first
    (should-not clatter--input-padding-end)
    (clatter--insert-message buffer "message" t)
    (should-not clatter--input-padding-end)))

(ert-deftest clatter-input-oldest-first-window-starts-are-independent ()
  "Split windows independently bottom-align against shared real padding."
  (clatter-input-test--with-window 'oldest-first
    (let ((other-window (split-window window nil 'below)))
      (unwind-protect
          (progn
            (set-window-buffer other-window buffer)
            (set-window-point window clatter--input-marker)
            (set-window-point other-window clatter--input-marker)
            (clatter--refresh-input-spacers buffer)
            (dolist (candidate (list window other-window))
              (should (= (count-screen-lines (window-start candidate)
                                             (point-max) nil candidate)
                         (window-body-height candidate)))))
        (when (window-live-p other-window)
          (delete-window other-window))))))

(ert-deftest clatter-input-oldest-first-short-history-stays-bottom-pinned ()
  "Moving point into a short history does not dislodge the input row."
  (clatter-input-test--with-window 'oldest-first
    (dotimes (index 4)
      (clatter--insert-message buffer (format "message-%d" index) t))
    (set-window-point window clatter--input-padding-end)
    (set-window-start window (point-min))
    (clatter--refresh-input-spacers buffer)
    (should clatter--input-padding-end)
    (clatter--insert-message buffer "incoming" t)
    (should (= (count-screen-lines (window-start window)
                                   (point-max) nil window)
               (window-body-height window)))
    (should (= (window-point window) clatter--input-padding-end))))

(ert-deftest clatter-input-oldest-first-overflowing-history-retains-viewport ()
  "A window deliberately reading overflowing history does not recenter."
  (clatter-input-test--with-window 'oldest-first
    (dotimes (index (+ (window-body-height window) 4))
      (clatter--insert-message buffer (format "message-%02d" index) t))
    (set-window-point window clatter--input-padding-end)
    (set-window-start window clatter--input-padding-end)
    (clatter--refresh-input-spacers buffer)
    (let ((start (window-start window)))
      (clatter--insert-message buffer "incoming" t)
      (should (= (window-start window) start))
      (should (= (window-point window) clatter--input-padding-end)))))

(ert-deftest clatter-input-oldest-first-overflow-keeps-input-point-at-bottom ()
  "A full following window scrolls minimally without disturbing draft input."
  (clatter-input-test--with-window 'oldest-first
    (goto-char (clatter--input-end))
    (insert "draft")
    (clatter--refresh-input-spacers buffer)
    (let ((height (window-body-height window)))
      (dotimes (index (+ height 3))
        (clatter--insert-message buffer (format "message-%02d" index) t))
      (should (equal (clatter--get-input) "draft"))
      (should (= (- (window-point window)
                    (marker-position clatter--input-marker))
                 (length "draft")))
      (should (= (count-screen-lines (window-start window)
                                     (point-max) nil window)
                 height)))))

(ert-deftest clatter-input-newest-first-prompt-at-top ()
  "Newest-first: prompt is on the first line; newest message sits just below."
  (clatter-input-test--with 'newest-first
    (should (string-suffix-p "#test> " (clatter-input-test--first-line)))
    (clatter--insert-message (current-buffer) "first")
    (clatter--insert-message (current-buffer) "second")
    (should (string-suffix-p "#test> " (clatter-input-test--first-line)))
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

(ert-deftest clatter-input-undo-survives-incoming-message ()
  "Undo of typed input is not corrupted by a message inserted above it.
Regression for the rcirc-style bug: a bottom-anchored prompt shifts the
input down when messages arrive, so undo must shift its recorded
positions or it deletes the wrong (message) text."
  (clatter-input-test--with 'oldest-first
    (buffer-enable-undo)
    (goto-char (clatter--input-end))
    (setq buffer-undo-list nil)
    (insert "hello world")
    (should (equal (clatter--get-input) "hello world"))
    ;; A message arrives and pushes the input down.
    (clatter--insert-message (current-buffer) "<bob> incoming line here")
    (should (equal (clatter--get-input) "hello world"))
    ;; Undo the typing: it must remove the input and leave the message.
    (primitive-undo 1 buffer-undo-list)
    (should (equal (clatter--get-input) ""))
    (should (string-match-p "incoming line here" (buffer-string)))))

(ert-deftest clatter-update-undo-list-shifts-positions ()
  "`clatter--update-undo-list' shifts integer positions and (BEG . END)."
  (with-temp-buffer
    (let ((buffer-undo-list (list 10 (cons 5 8) (cons "txt" 12) nil)))
      (clatter--update-undo-list 3)
      (should (equal (nth 0 buffer-undo-list) 13))      ; POSITION
      (should (equal (nth 1 buffer-undo-list) (cons 8 11)))  ; (BEG . END)
      (should (equal (nth 2 buffer-undo-list) (cons "txt" 15))) ; (TEXT . POS)
      (should (null (nth 3 buffer-undo-list))))))       ; boundary untouched

(ert-deftest clatter-update-undo-list-noop-on-zero ()
  "A zero shift leaves the undo list untouched."
  (let ((buffer-undo-list (list 10 (cons 5 8))))
    (clatter--update-undo-list 0)
    (should (equal buffer-undo-list (list 10 (cons 5 8))))))

(provide 'test-input)

;;; test-input.el ends here
