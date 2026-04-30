;;; clatter-image.el --- Inline image preview for clatter -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Glenn Thompson
;; Author: Glenn Thompson
;; License: MIT

;;; Commentary:

;; Async inline image preview for URLs in messages.
;; Opt-in via `clatter-image-enable' (default nil).
;; Only works in GUI Emacs frames; graceful no-op in terminal.
;; No external dependencies - uses built-in `url-retrieve' and `create-image'.

;;; Code:

(require 'cl-lib)
(require 'url)

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
  "Return non-nil if URL looks like a direct image link."
  (let ((path (downcase (or (url-filename (url-generic-parse-url url)) ""))))
    ;; Strip query string for extension check
    (let ((base (car (split-string path "?"))))
      (cl-some (lambda (ext)
                 (string-suffix-p (concat "." ext) base))
               clatter-image-extensions))))

;; --- Async fetch and display ---

(defun clatter-image--fetch (url buffer insert-marker)
  "Fetch image at URL and insert into BUFFER at INSERT-MARKER."
  (when (and clatter-image-enable
             (display-graphic-p)
             (clatter-image--url-p url))
    (url-retrieve
     url
     (lambda (status buf marker url-str)
       (unless (plist-get status :error)
         (condition-case nil
             (let* ((size (buffer-size))
                    (data (progn
                            (goto-char (point-min))
                            (re-search-forward "\n\n" nil t)
                            (buffer-substring-no-properties (point) (point-max)))))
               (when (and data
                          (> (length data) 0)
                          (<= size clatter-image-max-size))
                 (let ((image (create-image data nil t
                                            :max-width clatter-image-max-width
                                            :max-height clatter-image-max-height)))
                   (when (and image (buffer-live-p buf))
                     (with-current-buffer buf
                       (let ((inhibit-read-only t))
                         (save-excursion
                           (goto-char marker)
                           (end-of-line)
                           (insert "\n")
                           (insert-image image (format "[image: %s]" url-str))
                           (insert "\n"))))))))
           (error nil)))
       (when (buffer-live-p (current-buffer))
         (kill-buffer (current-buffer))))
     (list buffer insert-marker url)
     t t)))

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
