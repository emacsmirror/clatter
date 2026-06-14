;; -*- lexical-binding: t; -*-

(defcustom clatter-smart-noise '(join part quit nick away)
  "Message types considered to be noise in clatter's smart filter."
  :type '(repeat (choice (const :tag "JOIN" join)
                         (const :tag "PART" part)
                         (const :tag "QUIT" quit)
                         (const :tag "NICK" nick)
                         (const :tag "MODE" mode)
                         (const :tag "AWAY" away)))
  :group 'clatter)

(defcustom clatter-smart-threshold 0.5
  "SNR threshold under which noisy message types are hidden."
  :type 'number
  :group 'clatter)

(defvar clatter-smart-data nil
  "Count of noisy and non-noisy messages, keyed by nick.")

(defun clatter-smart-on (buf)
  "Get smart filter data for BUF."
  (with-current-buffer buf
    (or clatter-smart-data
        (setq-local clatter-smart-data
                    (make-hash-table :test 'equal)))))

(defun clatter-smart-put (buf nick elt)
  "Record ELT for NICK in BUF and return the SNR value."
  (let* ((is-nick-change (and (stringp nick)
                              (stringp elt)))
         (data (clatter-smart-on buf))
         (signal-noise (gethash nick data))
         (signal-count (or (car signal-noise) 1))
         (noise-count (or (cdr signal-noise) 1))
         (is-noise (memq (if is-nick-change 'nick elt) clatter-smart-noise)))
    (when is-nick-change
      (remhash nick data)
      (setq nick elt))
    (setq signal-count (+ signal-count (if is-noise 0 1)))
    (setq noise-count (+ noise-count (if is-noise 1 0)))
    (puthash nick (cons signal-count noise-count) data)
    (/ signal-count noise-count)))

(defun clatter-smart-eval (buf nick elt)
  "Record ELT for NICK in BUF and return whether NICK is noisy."
  (< (clatter-smart-put buf nick elt) clatter-smart-threshold))

(provide 'clatter-smart)

;;; clatter-smart.el ends here
