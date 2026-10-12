;;; clatter-image.el --- Inline image preview for clatter -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Glenn Thompson
;; Author: Glenn Thompson <glenn@paren.works>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Async inline image preview for URLs in messages.
;; Opt-in via `clatter-image-enable' (default nil).
;; Only works in GUI Emacs frames; graceful no-op in terminal.
;; Uses external curl to avoid blocking Emacs on DNS/TLS.

;;; Code:

(require 'cl-lib)
(require 'image)
(require 'clatter-ui)

;; History edits isolate Flyspell's dynamically scoped change records.
(defvar flyspell-changes)

(declare-function image-size "image.c" (spec &optional pixels frame))

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

(defcustom clatter-image-max-concurrent-fetches 4
  "Maximum number of image downloads to run at once.
Image URLs beyond this limit are skipped rather than queued, so a burst
of URLs (for example when a bouncer replays a large backlog) cannot spawn
a flood of curl subprocesses."
  :type 'integer
  :group 'clatter)

(defvar clatter-image--active-fetches 0
  "Number of image download subprocesses currently in flight.")

;; --- URL normalization ---

(defun clatter-image--normalize-url (url)
  "Convert URL to a direct image link where possible.
Handles GitHub and GitLab blob URLs."
  (let ((u (substring-no-properties url)))
    (cond
     ;; GitHub blob -> raw.githubusercontent.com
     ((string-match
       "https://github\\.com/\\([^/]+/[^/]+\\)/blob/\\(.+\\)" u)
      (format "https://raw.githubusercontent.com/%s/%s"
              (match-string 1 u) (match-string 2 u)))
     ;; GitLab blob -> raw
     ((string-match
       "\\(https://[^/]+\\)/\\([^/]+/[^/]+\\)/-/blob/\\(.+\\)" u)
      (format "%s/%s/-/raw/%s"
              (match-string 1 u) (match-string 2 u) (match-string 3 u)))
     (t u))))

;; --- Data validation ---

(defun clatter-image--valid-data-p (data)
  "Return non-nil if DATA starts with a known image magic byte sequence."
  (and (> (length data) 8)
       (or (string-prefix-p "\x89PNG" data)
           (string-prefix-p "\xFF\xD8" data)
           (string-prefix-p "GIF8" data)
           (string-prefix-p "BM" data)
           (string-prefix-p "RIFF" data))))

;; --- URL detection ---

(defun clatter-image--url-p (url)
  "Return non-nil if URL looks like a direct image link.
Uses simple string parsing to avoid `url-generic-parse-url' which can block."
  (let* ((clean (substring-no-properties url))
         ;; Strip fragment
         (no-frag (car (split-string clean "#")))
         ;; Strip query string
         (base (downcase (car (split-string no-frag "?"))))
         (exts clatter-image-extensions)
         (match nil))
    ;; Plain loop instead of `cl-some' with a lambda: this runs once per
    ;; URL in every incoming message, and an interpreted lambda here forces
    ;; `cconv-make-interpreted-closure' (free-variable analysis + GC) on
    ;; every call, which pegs the CPU during bouncer playback.
    (while (and exts (not match))
      (when (string-suffix-p (concat "." (car exts)) base)
        (setq match t))
      (setq exts (cdr exts)))
    match))

;; --- Async fetch and display ---

(defun clatter-image--insert (image buffer marker url-str)
  "Insert IMAGE into BUFFER at MARKER, centered horizontally.
URL-STR is used for the alt-text fallback."
  (let* ((img-width (or (car (image-size image t)) 0))
         (win (get-buffer-window buffer))
         (win-width (if win
                        (* (window-width win)
                           (frame-char-width (window-frame win)))
                      (* 80 (frame-char-width))))
         (pad-px (max 0 (/ (- win-width img-width) 2)))
         (pad-cols (/ pad-px (frame-char-width)))
         (prefix (make-string pad-cols ?\s)))
    (with-current-buffer buffer
      (clatter--flyspell-prune-changes)
      (let ((pre-input (and clatter--input-marker
                            (marker-position clatter--input-marker))))
        (let ((inhibit-read-only t)
              (buffer-undo-list t)
              (flyspell-changes nil))
          (save-excursion
            (goto-char marker)
            (let ((start (point)))
              (insert-image image (format "[image: %s]" url-str))
              (insert "\n")
              (add-text-properties start (point)
                                   (list 'line-prefix prefix 'read-only t
                                         'front-sticky t))))
          (clatter--maybe-truncate buffer)
          (clatter--refresh-input-spacers buffer))
        (when (and pre-input clatter--input-marker)
          (let ((shift (- (marker-position clatter--input-marker) pre-input)))
            (clatter--update-undo-list shift)
            (clatter--flyspell-update-changes shift)))))))

(defun clatter-image--source-live-p (buffer start end text length)
  "Return non-nil if START and END still delimit the original source message."
  (and (buffer-live-p buffer)
       (eq (marker-buffer start) buffer)
       (eq (marker-buffer end) buffer)
       (with-current-buffer buffer
         (let ((beg (marker-position start))
               (finish (marker-position end)))
           (and (< beg finish)
                (= (- finish beg) length)
                (or (null text)
                    (equal (get-text-property beg 'clatter-text) text)))))))

(defun clatter-image--fetch (url buffer insert-marker)
  "Fetch URL and insert its image only while its source message still exists.
Own INSERT-MARKER, detaching it on completion, startup failure or skip."
  (if (not (and clatter-image-enable
                (display-graphic-p)
                (< clatter-image--active-fetches clatter-image-max-concurrent-fetches)
                (clatter-image--url-p url)
                (buffer-live-p buffer)
                (eq (marker-buffer insert-marker) buffer)))
      (set-marker insert-marker nil)
    (let* ((clean-url (clatter-image--normalize-url url))
           (end (marker-position insert-marker))
           (start (with-current-buffer buffer
                    (copy-marker
                     (or (previous-single-property-change
                          end 'clatter-text nil (point-min))
                         (point-min))
                     t)))
           (source-text (with-current-buffer buffer
                          (get-text-property start 'clatter-text)))
           (source-length (- end (marker-position start)))
           (proc-buf (generate-new-buffer " *clatter-image-fetch*"))
           process done)
      (set-marker-insertion-type insert-marker nil)
      (with-current-buffer proc-buf
        (set-buffer-multibyte nil))
      (cl-incf clatter-image--active-fetches)
      (cl-labels
          ((dispose ()
             (unless done
               (setq done t)
               (cl-decf clatter-image--active-fetches)
               (when (and process (process-live-p process))
                 (delete-process process))
               (when (buffer-live-p proc-buf)
                 (kill-buffer proc-buf))
               (set-marker start nil)
               (set-marker insert-marker nil)))
           (sentinel (proc _event)
             (when (and (not done)
                        (memq (process-status proc) '(exit signal failed closed)))
               (setq process proc)
               (unwind-protect
                   (when (and (eq (process-exit-status proc) 0)
                              (buffer-live-p proc-buf)
                              (clatter-image--source-live-p
                               buffer start insert-marker source-text source-length))
                     (condition-case nil
                         (let ((data (with-current-buffer proc-buf
                                       (buffer-substring-no-properties
                                        (point-min) (point-max)))))
                           (when (and (> (length data) 0)
                                      (clatter-image--valid-data-p data))
                             (let ((image
                                    (create-image data nil t
                                                  :max-width clatter-image-max-width
                                                  :max-height clatter-image-max-height)))
                               (when image
                                 (clatter-image--insert
                                  image buffer insert-marker clean-url)))))
                       (error nil)))
                 (dispose)))))
        (condition-case nil
            (setq process
                  (make-process
                   :name "clatter-image" :buffer proc-buf :noquery t
                   :coding 'binary :connection-type 'pipe :sentinel #'sentinel
                   :command
                   (list
                    "curl" "-sL" "-m" "15"
                    "--proto" "=http,https" "--proto-redir" "=http,https"
                    "--max-redirs" "5"
                    "--max-filesize" (number-to-string clatter-image-max-size)
                    "-o" "-" clean-url)))
          (error (dispose)))))))

;; --- Hook into message insertion ---

(defun clatter-image--extract-urls (text)
  "Return the list of http(s) URLs found in TEXT, in order of appearance.
This scan reads the match end and the matched substring before doing
anything else, so it never relies on global match data surviving a call
to another function.  An earlier version advanced the loop with
`match-end' *after* calling `clatter-image--url-p' (which runs
`split-string' on the URL and clobbers the match data); when a URL
contained a query string or fragment and was preceded by other text,
the position reset behind the URL and the same URL matched forever,
spinning the CPU at 100%."
  (let ((pos 0)
        (urls nil))
    (while (string-match "https?://[^ \t\n]+" text pos)
      (setq pos (match-end 0))
      (push (match-string 0 text) urls))
    (nreverse urls)))

(defun clatter-image--scan-message (text buffer &optional insert-marker)
  "Scan rendered TEXT for image URLs and fetch them for display in BUFFER.
INSERT-MARKER locates the source message in either layout and is consumed."
  (unwind-protect
      (when (and clatter-image-enable (display-graphic-p) (buffer-live-p buffer))
        (with-current-buffer buffer
          (let* ((position (or (and insert-marker
                                    (marker-position insert-marker))
                               (and clatter--messages-marker
                                    (marker-position clatter--messages-marker))
                               (point-max)))
                 (end (cond
                       ((and (< position (point-max))
                             (equal (get-text-property position 'clatter-text) text))
                        (next-single-property-change
                         position 'clatter-text nil (point-max)))
                       (t position))))
            (dolist (url (clatter-image--extract-urls text))
              (when (clatter-image--url-p url)
                (clatter-image--fetch url buffer (copy-marker end)))))))
    (when (markerp insert-marker)
      (set-marker insert-marker nil))))

(provide 'clatter-image)

;;; clatter-image.el ends here
