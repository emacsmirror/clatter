;;; clatter-url-preview.el --- URL title preview -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Glenn Thompson
;; Author: Glenn Thompson <glenn@paren.works>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Asynchronous URL title fetching for clatter.el.
;; When a URL is posted in a channel, fetches the page title
;; and displays it inline as a system message.

;;; Code:

(require 'cl-lib)
(require 'clatter-config)
(require 'clatter-connection)
(require 'clatter-protocol)
(require 'clatter-model)
(require 'clatter-pals)
(require 'clatter-ui)

;; History edits isolate Flyspell's dynamically scoped change records.
(defvar flyspell-changes)

;; --- Configuration ---

(defface clatter-url-preview-title
  '((t :inherit font-lock-doc-face :slant italic))
  "Face for the title line of a rendered link preview."
  :group 'clatter)

(defcustom clatter-url-preview-enable nil
  "Enable automatic URL title preview."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-url-preview-max-length 200
  "Maximum length for displayed URL titles."
  :type 'integer
  :group 'clatter)

(defcustom clatter-url-preview-timeout 10
  "Timeout in seconds for URL fetching."
  :type 'integer
  :group 'clatter)

(defcustom clatter-url-preview-max-concurrent-fetches 4
  "Maximum number of distinct URL title fetches to run at once.
Excess URLs are skipped, not queued."
  :type 'integer
  :group 'clatter)

(defcustom clatter-url-preview-max-response-bytes (* 64 1024)
  "Hard byte limit for a URL title response, including streamed responses.
Responses exceeding this limit are discarded even if the server ignores Range."
  :type 'integer
  :group 'clatter)

(defcustom clatter-url-preview-exclude-patterns
  '("\\.png$" "\\.jpg$" "\\.jpeg$" "\\.gif$" "\\.webp$" "\\.svg$"
    "\\.pdf$" "\\.zip$" "\\.tar" "\\.mp3$" "\\.mp4$" "\\.mkv$"
    "\\.iso$" "\\.deb$" "\\.rpm$")
  "URL patterns to skip title fetching for (binary/media files)."
  :type '(repeat regexp)
  :group 'clatter)

;; --- Internal ---

(defvar clatter-url-preview--cache (make-hash-table :test 'equal)
  "Cache of URL -> title mappings to avoid re-fetching.")

(defvar clatter-url-preview--cache-max 500
  "Maximum number of cached URL titles.")

(defvar clatter-url-preview--pending (make-hash-table :test 'equal)
  "Clean URLs mapped to their owned in-flight request records.")

(cl-defstruct clatter-url-preview--request
  url destination anchor process output (bytes 0) done)

(cl-defstruct (clatter-url-preview--anchor
               (:constructor clatter-url-preview--make-anchor-record))
  "Insertion state shared by every preview belonging to one message.

START and END delimit the original rendered message.  TAIL is advanced after
each preview, so synchronous cache hits retain URL order and later previews
are not inserted before earlier ones.  REMAINING owns the markers: the last
cache hit or asynchronous request detaches them."
  buffer start end tail text length remaining)

(defun clatter-url-preview--should-fetch-p (url)
  "Return non-nil if URL should be fetched for title preview."
  (and clatter-url-preview-enable
       (string-match-p "^https?://" url)
       (not (gethash url clatter-url-preview--pending))
       (not (cl-some (lambda (pat) (string-match-p pat url))
                     clatter-url-preview-exclude-patterns))))

(defun clatter-url-preview--extract-title (html)
  "Extract the <title> content from HTML string."
  (when (string-match "<title[^>]*>\\([^<]*\\)</title>" html)
    (let ((raw (match-string 1 html)))
      (setq raw (replace-regexp-in-string "[ \n\r\t]+" " " raw))
      (setq raw (string-trim raw))
      ;; Decode HTML entities
      (setq raw (replace-regexp-in-string "&amp;" "&" raw))
      (setq raw (replace-regexp-in-string "&lt;" "<" raw))
      (setq raw (replace-regexp-in-string "&gt;" ">" raw))
      (setq raw (replace-regexp-in-string "&quot;" "\"" raw))
      (setq raw (replace-regexp-in-string "&#39;" "'" raw))
      (setq raw (replace-regexp-in-string "&nbsp;" " " raw))
      (when (> (length raw) 0)
        (if (> (length raw) clatter-url-preview-max-length)
            (concat (substring raw 0 clatter-url-preview-max-length) "...")
          raw)))))

(defun clatter-url-preview--anchor-live-p (anchor)
  "Return non-nil when ANCHOR still refers to its source message."
  (and (clatter-url-preview--anchor-p anchor)
       (let ((buffer (clatter-url-preview--anchor-buffer anchor))
             (start (clatter-url-preview--anchor-start anchor))
             (end (clatter-url-preview--anchor-end anchor)))
         (and (buffer-live-p buffer)
              (markerp start) (markerp end)
              (eq (marker-buffer start) buffer)
              (eq (marker-buffer end) buffer)
              (with-current-buffer buffer
                (let ((start-pos (marker-position start))
                      (end-pos (marker-position end)))
                  (and (<= (point-min) start-pos)
                       (< start-pos (point-max))
                       (<= end-pos (point-max))
                       (< start-pos end-pos)
                       (or (null (clatter-url-preview--anchor-length anchor))
                           (= (- end-pos start-pos)
                              (clatter-url-preview--anchor-length anchor)))
                       ;; Actual UI messages carry this property.  The
                       ;; fallback keeps the helper usable in minimal tests.
                       (or (null (clatter-url-preview--anchor-text anchor))
                           (equal (get-text-property start-pos 'clatter-text)
                                  (clatter-url-preview--anchor-text anchor))))))))))

(defun clatter-url-preview--dispose-anchor (anchor)
  "Detach every marker owned by ANCHOR."
  (when (clatter-url-preview--anchor-p anchor)
    (dolist (marker (list (clatter-url-preview--anchor-start anchor)
                          (clatter-url-preview--anchor-end anchor)
                          (clatter-url-preview--anchor-tail anchor)))
      (when (markerp marker) (set-marker marker nil)))))

(defun clatter-url-preview--release-anchor (anchor)
  "Release one URL request's ownership of ANCHOR."
  (when (clatter-url-preview--anchor-p anchor)
    (setf (clatter-url-preview--anchor-remaining anchor)
          (1- (clatter-url-preview--anchor-remaining anchor)))
    (when (<= (clatter-url-preview--anchor-remaining anchor) 0)
      (clatter-url-preview--dispose-anchor anchor))))

(defun clatter-url-preview--insert (title buffer anchor)
  "Insert TITLE below ANCHOR's original message.

ANCHOR's tail marker is advanced after insertion, retaining cached URL order.
No text is inserted if truncation or deletion has removed the source message."
  (when (and (clatter-url-preview--anchor-p anchor)
             (eq buffer (clatter-url-preview--anchor-buffer anchor))
             (clatter-url-preview--anchor-live-p anchor))
    (with-current-buffer buffer
      (clatter--flyspell-prune-changes)
      (let ((pre-input (and clatter--input-marker
                            (marker-position clatter--input-marker))))
        (let ((inhibit-read-only t)
              (buffer-undo-list t)
              (flyspell-changes nil)
              (prefix (make-string (1+ clatter-nick-column-width) ?\s)))
          (save-excursion
            (goto-char (clatter-url-preview--anchor-tail anchor))
            (insert (propertize (format "↳ %s\n" title)
                                'face 'clatter-url-preview-title
                                'read-only t
                                'front-sticky t
                                'line-prefix prefix
                                'wrap-prefix prefix))
            ;; Advance only our local tail, not the original message end.
            (set-marker (clatter-url-preview--anchor-tail anchor) (point)))
          (clatter--maybe-truncate buffer)
          (clatter--refresh-input-spacers buffer))
        (when (and pre-input clatter--input-marker)
          (let ((shift (- (marker-position clatter--input-marker) pre-input)))
            (clatter--update-undo-list shift)
            (clatter--flyspell-update-changes shift)))))))

(defun clatter-url-preview--dispose-request (request)
  "Idempotently release resources owned by REQUEST, never a newer request."
  (unless (clatter-url-preview--request-done request)
    (setf (clatter-url-preview--request-done request) t)
    (when (eq (gethash (clatter-url-preview--request-url request)
                      clatter-url-preview--pending)
              request)
      (remhash (clatter-url-preview--request-url request)
               clatter-url-preview--pending))
    (let ((process (clatter-url-preview--request-process request))
          (output (clatter-url-preview--request-output request)))
      (when (and process (process-live-p process))
        (delete-process process))
      (when (buffer-live-p output)
        (kill-buffer output)))
    (clatter-url-preview--release-anchor
     (clatter-url-preview--request-anchor request))))

(defun clatter-url-preview--filter (request process chunk)
  "Append CHUNK only while REQUEST owns PROCESS and stays within its byte limit."
  (unless (clatter-url-preview--request-process request)
    (setf (clatter-url-preview--request-process request) process))
  (unless (clatter-url-preview--request-done request)
    (let ((size (+ (clatter-url-preview--request-bytes request)
                   (string-bytes chunk)))
          (output (clatter-url-preview--request-output request)))
      (if (or (not (eq (gethash (clatter-url-preview--request-url request)
                               clatter-url-preview--pending)
                       request))
              (not (buffer-live-p output))
              (> size clatter-url-preview-max-response-bytes))
          (clatter-url-preview--dispose-request request)
        (setf (clatter-url-preview--request-bytes request) size)
        (with-current-buffer output
          (goto-char (point-max))
          (insert chunk))))))

(defun clatter-url-preview--sentinel (request process _event)
  "Render a completed owned REQUEST and always dispose its resources."
  (when (and (not (clatter-url-preview--request-done request))
             (memq (process-status process) '(exit signal failed closed)))
    (setf (clatter-url-preview--request-process request) process)
    (unwind-protect
        (when (and (eq (gethash (clatter-url-preview--request-url request)
                               clatter-url-preview--pending)
                      request)
                   (eq (process-exit-status process) 0)
                   (buffer-live-p (clatter-url-preview--request-output request)))
          ;; Copy only the title, never the entire response at completion.
          (let ((title
                 (with-current-buffer (clatter-url-preview--request-output request)
                   (goto-char (point-min))
                   (when (re-search-forward "<title[^>]*>[^<]*</title>" nil t)
                     (clatter-url-preview--extract-title
                      (decode-coding-string (match-string-no-properties 0)
                                            'utf-8 t))))))
            (when title
              (when (> (hash-table-count clatter-url-preview--cache)
                       clatter-url-preview--cache-max)
                (clrhash clatter-url-preview--cache))
              (puthash (clatter-url-preview--request-url request)
                       title clatter-url-preview--cache)
              (clatter-url-preview--insert
               title (clatter-url-preview--request-destination request)
               (clatter-url-preview--request-anchor request)))))
      (clatter-url-preview--dispose-request request))))

(defun clatter-url-preview--fetch (url buffer anchor)
  "Fetch URL asynchronously within the concurrency and streamed byte limits.
Skipped URLs release ANCHOR immediately; no queue or retry is retained."
  (let ((clean-url (substring-no-properties url)))
    (if (or (gethash clean-url clatter-url-preview--pending)
            (>= (hash-table-count clatter-url-preview--pending)
                clatter-url-preview-max-concurrent-fetches))
        (clatter-url-preview--release-anchor anchor)
      (let* ((output (generate-new-buffer " *clatter-url-fetch*"))
             (request (make-clatter-url-preview--request
                       :url clean-url :destination buffer :anchor anchor
                       :output output)))
        (with-current-buffer output
          (set-buffer-multibyte nil))
        (puthash clean-url request clatter-url-preview--pending)
        (condition-case nil
            (setf (clatter-url-preview--request-process request)
                  (make-process
                   :name "clatter-url-preview" :buffer output :noquery t
                   :coding 'binary :connection-type 'pipe
                   :filter (lambda (process chunk)
                             (clatter-url-preview--filter request process chunk))
                   :sentinel (lambda (process event)
                               (clatter-url-preview--sentinel request process event))
                   :command
                   (list
                    ;; Keep schemes, redirects and timeout restricted.
                    "curl" "-sL" "-m" (number-to-string clatter-url-preview-timeout)
                    "--proto" "=http,https" "--proto-redir" "=http,https"
                    "--max-redirs" "5" "-o" "-" "-r" "0-16384"
                    "-H" "User-Agent: Mozilla/5.0 (compatible; clatter.el)"
                    "-H" "Accept: text/html" clean-url)))
          (error (clatter-url-preview--dispose-request request)))))))

;; --- Hook Handler ---

(defun clatter-url-preview--message-end-marker (buffer text)
  "Return a fixed marker immediately after TEXT's rendered message in BUFFER.

For `newest-first', the messages marker is immediately before the newly
inserted message; for `oldest-first', it is immediately after it.  The UI
stores the unformatted message text as `clatter-text', which lets this work
for either order and for wrapped messages."
  (with-current-buffer buffer
    (let* ((messages-marker (or clatter--messages-marker (point-max)))
           (position (marker-position messages-marker))
           (before (and (> position (point-min)) (1- position)))
           (end (cond
                 ((equal (get-text-property position 'clatter-text) text)
                  (next-single-property-change position 'clatter-text nil (point-max)))
                 ((and before
                       (equal (get-text-property before 'clatter-text) text))
                  position)
                 (t position))))
      (copy-marker end))))

(defun clatter-url-preview--message-anchor (buffer text request-count)
  "Return a shared anchor for REQUEST-COUNT previews of TEXT in BUFFER.

The rendered message's `clatter-text' property is retained as an identity
check, preventing a delayed curl sentinel from inserting at a marker left
behind by buffer truncation."
  (with-current-buffer buffer
    (let* ((end (clatter-url-preview--message-end-marker buffer text))
           (end-pos (marker-position end))
           (start (or (previous-single-property-change
                       end-pos 'clatter-text nil (point-min))
                      (point-min)))
           ;; `previous-single-property-change' returns the beginning when
           ;; END is just after a property run; use the property's value at
           ;; the first character to distinguish real UI messages from a
           ;; minimal buffer used by a low-level caller.
           (source-text (and (< start end-pos)
                             (get-text-property start 'clatter-text)))
           (tail (copy-marker end-pos)))
      (set-marker-insertion-type end nil)
      (set-marker-insertion-type tail nil)
      (clatter-url-preview--make-anchor-record
       :buffer buffer :start (copy-marker start t) :end end :tail tail
       :text source-text :length (- end-pos start) :remaining request-count))))

(defun clatter-url-preview--on-privmsg (conn sender target text _server-time)
  "Check TEXT from SENDER for URLs and fetch titles for TARGET buffer on CONN."
  (when clatter-url-preview-enable
    (let* ((network (clatter-connection-network-id conn))
           (my-nick (clatter-connection-nick conn))
           (isupport (clatter-connection-isupport conn))
           (case-mapping (and isupport (gethash "CASEMAPPING" isupport)))
           (sender-nick (clatter-prefix-nick sender))
           (buf-target (if (clatter-channel-name-p target)
                           target
                         (if (clatter-nick-equal-p target my-nick case-mapping)
                             sender-nick target)))
           (buf (unless (clatter-fool-p sender-nick network)
                  (clatter-get-buffer network buf-target)))
           (pos 0)
           requests)
      (when buf
        (while (string-match
                ;; ponytail: (string 34) = double-quote, kept out of the
                ;; string literal so melpazoid's shlex header parser copes.
                (concat "https?://[^ \n\r\t<>'" (string 34) "]+")
                text pos)
          (let ((url (match-string 0 text)))
            (setq pos (match-end 0))
            (when (and (clatter-url-preview--should-fetch-p url)
                       (not (member url requests)))
              (push url requests))))
        ;; Build exactly one tail per source message.  In particular, do not
        ;; make a fresh marker for each cache hit: nil-insertion markers at
        ;; the same position otherwise reverse source URL order.
        (when requests
          (setq requests (nreverse requests))
          (let ((anchor (clatter-url-preview--message-anchor
                         buf text (length requests))))
            (dolist (url requests)
              (let ((cached (gethash url clatter-url-preview--cache)))
                (if cached
                    (unwind-protect
                        (clatter-url-preview--insert cached buf anchor)
                      (clatter-url-preview--release-anchor anchor))
                  (clatter-url-preview--fetch url buf anchor))))))))))

;; --- Init/Teardown ---

(defun clatter-url-preview-init ()
  "Register URL preview hooks."
  ;; The UI hook must render the source message first.  The preview anchor is
  ;; located relative to that rendered message, and a default-depth hook would
  ;; run before the UI because `add-hook' prepends same-depth functions.
  (add-hook 'clatter-privmsg-hook #'clatter-url-preview--on-privmsg 90))

(defun clatter-url-preview-teardown ()
  "Remove URL preview hooks and cancel every owned fetch."
  (remove-hook 'clatter-privmsg-hook #'clatter-url-preview--on-privmsg)
  (let (requests)
    (maphash (lambda (_url request) (push request requests))
             clatter-url-preview--pending)
    (dolist (request requests)
      (clatter-url-preview--dispose-request request))))

;; Enabled by `clatter-setup' when `clatter-url-preview-enable' is
;; non-nil, so that merely loading this file has no side effects.

(provide 'clatter-url-preview)

;;; clatter-url-preview.el ends here
