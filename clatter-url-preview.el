;;; clatter-url-preview.el --- URL title preview -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Glenn Thompson
;; Author: Glenn Thompson
;; License: MIT

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
(require 'clatter-ui)

;; --- Configuration ---

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
  "URLs currently being fetched (to avoid duplicate requests).")

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
      (setq raw (replace-regexp-in-string "[\n\r\t]+" " " raw))
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

(defun clatter-url-preview--fetch (url buffer)
  "Asynchronously fetch URL and display its title in BUFFER.
Uses curl subprocess to avoid blocking Emacs on DNS/TLS."
  (let ((clean-url (substring-no-properties url)))
    (puthash clean-url t clatter-url-preview--pending)
    (condition-case nil
        (let* ((proc-buf (generate-new-buffer " *clatter-url-fetch*"))
               (proc (start-process
                      "clatter-url-preview" proc-buf
                      "curl" "-sL" "-m" (number-to-string clatter-url-preview-timeout)
                      "-o" "-" "-r" "0-16384"
                      "-H" "User-Agent: Mozilla/5.0 (compatible; clatter.el)"
                      "-H" "Accept: text/html"
                      clean-url)))
          (set-process-sentinel
           proc
           (lambda (process _event)
             (when (memq (process-status process) '(exit signal))
               (let ((proc-buffer (process-buffer process)))
                 (remhash clean-url clatter-url-preview--pending)
                 (when (and (eq (process-exit-status process) 0)
                            (buffer-live-p proc-buffer))
                   (let ((body (with-current-buffer proc-buffer
                                 (buffer-string))))
                     (let ((title (clatter-url-preview--extract-title body)))
                       (when (and title (buffer-live-p buffer))
                         (when (> (hash-table-count clatter-url-preview--cache)
                                  clatter-url-preview--cache-max)
                           (clrhash clatter-url-preview--cache))
                         (puthash clean-url title clatter-url-preview--cache)
                         (with-current-buffer buffer
                           (clatter-insert-system
                            buffer (propertize (format "\u21b3 %s" title)
                                              'face '(:foreground "#89ddff" :slant italic))))))))
                 (when (buffer-live-p proc-buffer)
                   (kill-buffer proc-buffer)))))))
      (error
       (remhash clean-url clatter-url-preview--pending)))))

;; --- Hook Handler ---

(defun clatter-url-preview--on-privmsg (conn _sender target text _server-time)
  "Check TEXT for URLs and fetch titles for TARGET buffer on CONN."
  (when clatter-url-preview-enable
    (let* ((network (clatter-connection-network-id conn))
           (my-nick (clatter-connection-nick conn))
           (buf-target (if (clatter-channel-name-p target)
                           target
                         (if (string-equal target my-nick) _sender target)))
           (buf (clatter-get-buffer network buf-target))
           (pos 0))
      (when buf
        (while (string-match "https?://[^ \t\n\r<>\"']+" text pos)
          (let ((url (match-string 0 text)))
            (setq pos (match-end 0))
            (when (clatter-url-preview--should-fetch-p url)
              ;; Check cache first
              (let ((cached (gethash url clatter-url-preview--cache)))
                (if cached
                    (clatter-insert-system
                     buf (propertize (format "↳ %s" cached)
                                    'face '(:foreground "#89ddff" :slant italic)))
                  (clatter-url-preview--fetch url buf))))))))))

;; --- Init/Teardown ---

(defun clatter-url-preview-init ()
  "Register URL preview hooks."
  (add-hook 'clatter-privmsg-hook #'clatter-url-preview--on-privmsg))

(defun clatter-url-preview-teardown ()
  "Remove URL preview hooks."
  (remove-hook 'clatter-privmsg-hook #'clatter-url-preview--on-privmsg))

;; --- Auto-init ---

(when clatter-url-preview-enable
  (clatter-url-preview-init))

(provide 'clatter-url-preview)

;;; clatter-url-preview.el ends here
