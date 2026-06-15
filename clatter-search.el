;;; clatter-search.el --- Full-text search across IRC chat history -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Glenn Thompson
;; Author: Glenn Thompson <glenn@paren.works>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Semantic search across all clatter IRC logs.
;; Uses `grep' for fast searching across log files with results
;; displayed via `completing-read' with timestamps and context.
;; No external dependencies.

;;; Code:

(require 'cl-lib)
(require 'clatter-config)
(require 'clatter-model)

;; --- Configuration ---

(defcustom clatter-search-max-results 500
  "Maximum number of search results to display."
  :type 'integer
  :group 'clatter)

(defcustom clatter-search-context-lines 0
  "Number of context lines to include around each match."
  :type 'integer
  :group 'clatter)

(defface clatter-search-match
  '((t :foreground "#ffcb6b" :weight bold))
  "Face for highlighted search matches."
  :group 'clatter)

(defface clatter-search-file
  '((t :foreground "#82aaff" :slant italic))
  "Face for file/channel names in search results."
  :group 'clatter)

(defface clatter-search-timestamp
  '((t :foreground "#676e95"))
  "Face for timestamps in search results."
  :group 'clatter)

;; --- Search Engine ---

(defun clatter-search--log-directory ()
  "Return the log directory path."
  (expand-file-name "clatter/logs/" user-emacs-directory))

(defun clatter-search--parse-result (line)
  "Parse a grep result LINE into (file lnum text).
LINE format: /path/to/file:linenum:content"
  (when (string-match "\\`\\(.+?\\):\\([0-9]+\\):\\(.*\\)\\'" line)
    (list (match-string 1 line)
          (string-to-number (match-string 2 line))
          (match-string 3 line))))

(defun clatter-search--file-to-channel (filepath)
  "Extract network/channel from log FILEPATH."
  (let* ((logdir (clatter-search--log-directory))
         (rel (file-relative-name filepath logdir)))
    (if (string-match "\\`\\(.+?\\)/\\(.+?\\)\\(?:-[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)?\\.log\\'" rel)
        (format "%s/%s" (match-string 1 rel) (match-string 2 rel))
      rel)))

(defun clatter-search--run-grep (query &optional network channel)
  "Run grep for QUERY across log files.
Optionally filter by NETWORK and/or CHANNEL.
Returns list of (display-string file lnum)."
  (let* ((logdir (clatter-search--log-directory))
         (search-dir (cond
                      ((and network channel)
                       (expand-file-name
                        (concat network "/" channel "*.log")
                        logdir))
                      (network
                       (expand-file-name (concat network "/") logdir))
                      (t logdir)))
         (grep-args (list "-rn" "-i"
                          "--include=*.log"
                          "-m" (number-to-string clatter-search-max-results)
                          query))
         ;; When searching specific files, use glob; otherwise recursive dir
         (effective-dir (if (and network channel)
                            (file-name-directory search-dir)
                          search-dir))
         (effective-args (if (and network channel)
                             (append (list "-n" "-i"
                                           "-m" (number-to-string clatter-search-max-results)
                                           query)
                                     (file-expand-wildcards search-dir))
                           grep-args))
         (output (with-temp-buffer
                   (apply #'call-process "grep" nil t nil
                          (if (and network channel)
                              effective-args
                            (append grep-args (list effective-dir))))
                   (buffer-string)))
         (lines (split-string output "\n" t))
         (results nil))
    (dolist (line lines)
      (let ((parsed (clatter-search--parse-result line)))
        (when parsed
          (let* ((file (nth 0 parsed))
                 (lnum (nth 1 parsed))
                 (text (nth 2 parsed))
                 (chan (clatter-search--file-to-channel file))
                 (display (concat
                           (propertize chan 'face 'clatter-search-file)
                           " "
                           (propertize text 'face nil))))
            (push (list display file lnum text) results)))))
    (nreverse results)))

;; --- Interactive Commands ---

(defun clatter-search (query)
  "Search all IRC logs for QUERY and display results.
With prefix arg, prompt for network to filter."
  (interactive "sSearch IRC logs: ")
  (when (string-empty-p query)
    (user-error "Empty search query"))
  (let* ((network (when current-prefix-arg
                    (completing-read "Network (blank=all): "
                                    (clatter-search--networks)
                                    nil nil)))
         (network (if (and network (not (string-empty-p network))) network nil))
         (results (clatter-search--run-grep query network)))
    (if (null results)
        (message "No results for: %s" query)
      (clatter-search--display-results results query))))

(defun clatter-search-channel (query)
  "Search logs for QUERY in the current channel only."
  (interactive "sSearch this channel: ")
  (unless (and clatter--network clatter--target)
    (user-error "Not in a clatter buffer"))
  (let* ((network clatter--network)
         (target (clatter-log--sanitize-filename clatter--target))
         (results (clatter-search--run-grep query network target)))
    (if (null results)
        (message "No results for: %s in %s/%s" query network clatter--target)
      (clatter-search--display-results results query))))

(defun clatter-search--networks ()
  "Return list of network names from log directory."
  (let ((logdir (clatter-search--log-directory)))
    (when (file-directory-p logdir)
      (cl-remove-if-not
       (lambda (f) (file-directory-p (expand-file-name f logdir)))
       (directory-files logdir nil "\\`[^.]")))))

;; --- Results Display ---

(defvar clatter-search-results-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'clatter-search-visit-result)
    (define-key map (kbd "o") #'clatter-search-visit-result-other-window)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    (define-key map (kbd "g") #'clatter-search-refresh)
    map)
  "Keymap for `clatter-search-results-mode'.")

(define-derived-mode clatter-search-results-mode special-mode "IRC-Search"
  "Major mode for clatter search results."
  (setq-local truncate-lines t)
  (setq-local revert-buffer-function #'clatter-search--revert))

(defvar-local clatter-search--query nil
  "The query used for the current search results.")
(defvar-local clatter-search--results nil
  "The current search results.")

(defun clatter-search--display-results (results query)
  "Display RESULTS for QUERY in a search results buffer."
  (let ((buf (get-buffer-create "*clatter-search*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (clatter-search-results-mode)
        (setq clatter-search--query query)
        (setq clatter-search--results results)
        (insert (propertize (format "Search: %s (%d results)\n\n"
                                    query (length results))
                            'face 'bold))
        (dolist (result results)
          (let* ((display (nth 0 result))
                 (file (nth 1 result))
                 (lnum (nth 2 result)))
            (insert (propertize display
                                'clatter-search-file file
                                'clatter-search-lnum lnum)
                    "\n")))))
    (pop-to-buffer buf)
    (goto-char (point-min))
    (forward-line 2)))

(defun clatter-search-visit-result ()
  "Visit the log file at the current search result."
  (interactive)
  (let ((file (get-text-property (point) 'clatter-search-file))
        (lnum (get-text-property (point) 'clatter-search-lnum)))
    (if file
        (progn
          (find-file-other-window file)
          (goto-char (point-min))
          (forward-line (1- lnum))
          (recenter))
      (message "No result at point"))))

(defun clatter-search-visit-result-other-window ()
  "Visit the log file at point in other window."
  (interactive)
  (clatter-search-visit-result))

(defun clatter-search-refresh ()
  "Re-run the current search."
  (interactive)
  (when clatter-search--query
    (let ((results (clatter-search--run-grep clatter-search--query)))
      (clatter-search--display-results results clatter-search--query))))

(defun clatter-search--revert (_ignore-auto _noconfirm)
  "Revert function for search results."
  (clatter-search-refresh))

;; --- Completing-read interface ---

(defun clatter-search-completing (query)
  "Search IRC logs for QUERY with a `completing-read' interface.
Select a result to jump to it in the log file."
  (interactive "sSearch IRC logs: ")
  (when (string-empty-p query)
    (user-error "Empty search query"))
  (let* ((results (clatter-search--run-grep query))
         (candidates (mapcar (lambda (r)
                               (cons (nth 0 r) r))
                             results)))
    (if (null candidates)
        (message "No results for: %s" query)
      (let* ((choice (completing-read
                      (format "Results for \"%s\": " query)
                      candidates nil t))
             (result (cdr (assoc choice candidates))))
        (when result
          (find-file-other-window (nth 1 result))
          (goto-char (point-min))
          (forward-line (1- (nth 2 result)))
          (recenter))))))

(provide 'clatter-search)

;;; clatter-search.el ends here
