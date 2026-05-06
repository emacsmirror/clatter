;;; clatter-image.el --- Inline image preview for clatter -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Glenn Thompson
;; Author: Glenn Thompson
;; License: MIT

;;; Commentary:

;; Async inline image preview for URLs in messages.
;; Opt-in via `clatter-image-enable' (default nil).
;; Only works in GUI Emacs frames; graceful no-op in terminal.
;; Uses external curl to avoid blocking Emacs on DNS/TLS.

;;; Code:

(require 'cl-lib)

;; --- Configuration ---

(defcustom clatter-image-enable nil
  "Enable inline image previews for URLs in messages.
When non-nil, recognized image URLs are fetched asynchronously
and displayed inline below the message."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-image-max-width 400
  "Maximum width in pixels for inline image previews."
  :type 'integer
  :group 'clatter)

(defcustom clatter-image-max-height 300
  "Maximum height in pixels for inline image previews."
  :type 'integer
  :group 'clatter)

(defcustom clatter-image-max-size (* 5 1024 1024)
  "Maximum file size in bytes for image downloads.
Images larger than this are skipped.  Default: 5 MB."
  :type 'integer
  :group 'clatter)

(defcustom clatter-image-extensions
  '("png" "jpg" "jpeg" "gif" "webp" "bmp" "svg")
  "File extensions recognized as images for inline preview."
  :type '(repeat string)
  :group 'clatter)

;; --- URL detection ---

(defun clatter-image--url-p (url)
  "Return non-nil if URL looks like a direct image link.
Uses simple string parsing to avoid `url-generic-parse-url' which can block."
  (let* ((clean (substring-no-properties url))
         ;; Strip fragment
         (no-frag (car (split-string clean "#")))
         ;; Strip query string
         (base (downcase (car (split-string no-frag "?")))))
    (cl-some (lambda (ext)
               (string-suffix-p (concat "." ext) base))
             clatter-image-extensions)))

;; --- Async fetch and display ---

(defun clatter-image--fetch (url buffer insert-marker)
  "Fetch image at URL and insert into BUFFER at INSERT-MARKER.
Uses curl subprocess to avoid blocking Emacs on DNS/TLS."
  (when (and clatter-image-enable
             (display-graphic-p)
             (clatter-image--url-p url))
    (let* ((clean-url (substring-no-properties url))
           (proc-buf (generate-new-buffer " *clatter-image-fetch*"))
           (proc (start-process
                  "clatter-image" proc-buf
                  "curl" "-sL"
                  "-m" "15"
                  "--max-filesize" (number-to-string clatter-image-max-size)
                  "-o" "-"
                  clean-url)))
      (set-process-coding-system proc 'binary 'binary)
      (set-process-sentinel
       proc
       (lambda (process _event)
         (when (memq (process-status process) '(exit signal))
           (let ((pbuf (process-buffer process)))
             (when (and (eq (process-exit-status process) 0)
                        (buffer-live-p pbuf))
               (condition-case nil
                   (let ((data (with-current-buffer pbuf
                                 (buffer-string))))
                     (when (and data (> (length data) 0)
                                (buffer-live-p buffer))
                       (let ((image (create-image data nil t
                                                  :max-width clatter-image-max-width
                                                  :max-height clatter-image-max-height)))
                         (when image
                           (with-current-buffer buffer
                             (let ((inhibit-read-only t))
                               (save-excursion
                                 (goto-char insert-marker)
                                 (end-of-line)
                                 (insert "\n")
                                 (insert-image image
                                               (format "[image: %s]" clean-url))
                                 (insert "\n"))))))))
                 (error nil)))
             (when (buffer-live-p pbuf)
               (kill-buffer pbuf)))))))))

;; --- Hook into message insertion ---

(defun clatter-image--scan-message (text buffer)
  "Scan TEXT for image URLs and fetch them for inline display in BUFFER.
Should be called after message insertion."
  (when (and clatter-image-enable (display-graphic-p))
    (let ((pos 0))
      (while (string-match "https?://[^ \t\n]+" text pos)
        (let ((url (match-string 0 text))
              (marker (with-current-buffer buffer
                        (save-excursion
                          (goto-char (point-max))
                          (point-marker)))))
          (when (clatter-image--url-p url)
            (clatter-image--fetch url buffer marker)))
        (setq pos (match-end 0))))))

(provide 'clatter-image)

;;; clatter-image.el ends here
