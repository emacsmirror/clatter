;;; clatter-org.el --- Org-mode integration for clatter -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Glenn Thompson
;; Author: Glenn Thompson
;; License: MIT

;;; Commentary:

;; Org-mode integration for clatter.el IRC client:
;; - `org-store-link' support: store links to IRC messages
;; - `org-capture' template helpers: capture messages to org
;; - Log export: convert IRC logs to org-mode format

;;; Code:

(require 'cl-lib)
(require 'clatter-model)

;; --- org-store-link support ---

(defun clatter-org-store-link ()
  "Store an org link to the IRC message at point.
Creates a link in the format irc:network/channel with description
including sender and message text."
  (when (derived-mode-p 'clatter-mode)
    (let* ((network clatter--network)
           (target clatter--target)
           (sender (get-text-property (point) 'clatter-sender))
           (text (get-text-property (point) 'clatter-text))
           (msgid (get-text-property (point) 'clatter-msgid))
           (link (format "irc:%s/%s%s"
                         network target
                         (if msgid (format "#%s" msgid) "")))
           (desc (cond
                  ((and sender text)
                   (format "IRC: <%s> %s (%s/%s)"
                           sender
                           (if (> (length text) 80)
                               (concat (substring text 0 77) "...")
                             text)
                           network target))
                  (target
                   (format "IRC: %s/%s" network target))
                  (t
                   (format "IRC: %s" network)))))
      (org-link-store-props
       :type "irc"
       :link link
       :description desc
       :network network
       :channel target
       :sender sender
       :message text
       :msgid msgid)
      link)))

;; --- org-capture helpers ---

(defun clatter-org-capture-message ()
  "Return formatted message at point for org-capture template.
For use in capture templates as %(clatter-org-capture-message)."
  (if (derived-mode-p 'clatter-mode)
      (let ((sender (get-text-property (point) 'clatter-sender))
            (text (get-text-property (point) 'clatter-text))
            (network clatter--network)
            (target clatter--target))
        (if (and sender text)
            (format "<%s> %s\n  /in %s/%s at %s/"
                    sender text network target
                    (format-time-string "%Y-%m-%d %H:%M"))
          (format "%s/%s at %s"
                  (or network "unknown") (or target "unknown")
                  (format-time-string "%Y-%m-%d %H:%M"))))
    "Not in a clatter buffer"))

(defun clatter-org-capture-channel ()
  "Return current channel/network for org-capture template.
For use as %(clatter-org-capture-channel)."
  (if (derived-mode-p 'clatter-mode)
      (format "%s/%s" (or clatter--network "unknown")
              (or clatter--target "unknown"))
    "unknown"))

;; --- Log export to org ---

(defun clatter-org-export-log (log-file output-file)
  "Export IRC LOG-FILE to org format in OUTPUT-FILE."
  (interactive
   (list (read-file-name "IRC log file: "
                         (expand-file-name "clatter/logs/" user-emacs-directory))
         (read-file-name "Output org file: ")))
  (let ((network nil)
        (channel nil))
    ;; Try to extract network/channel from path
    (when (string-match "\\(?:logs/\\)\\(.+?\\)/\\(.+?\\)\\(?:-[0-9]\\{4\\}\\)?\\.log\\'"
                        log-file)
      (setq network (match-string 1 log-file))
      (setq channel (match-string 2 log-file)))
    (with-temp-buffer
      (insert (format "#+TITLE: IRC Log - %s/%s\n"
                      (or network "unknown") (or channel "unknown")))
      (insert (format "#+DATE: %s\n" (format-time-string "%Y-%m-%d")))
      (insert "#+STARTUP: showall\n\n")
      (insert "* Messages\n\n")
      (with-temp-buffer
        (insert-file-contents log-file)
        (goto-char (point-min))
        (let ((current-date nil)
              (lines nil))
          (while (not (eobp))
            (let ((line (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position))))
              ;; Parse [timestamp] content
              (when (string-match "\\`\\[\\([0-9-]+\\) \\([0-9:]+\\)\\] \\(.*\\)\\'" line)
                (let ((date (match-string 1 line))
                      (time (match-string 2 line))
                      (content (match-string 3 line)))
                  (push (list date time content) lines))))
            (forward-line 1))
          ;; Write in org format, grouped by date
          (let ((output-buf (current-buffer)))
            (dolist (entry (nreverse lines))
              (let ((date (nth 0 entry))
                    (_time (nth 1 entry))
                    (_content (nth 2 entry)))
                (with-current-buffer output-buf
                  (when (not (equal date current-date))
                    (setq current-date date)
                    (goto-char (point-max))))
                ;; Write to outer buffer
                (with-temp-message ""
                  nil))))))
      ;; Actually write the export properly
      (erase-buffer)
      (insert (format "#+TITLE: IRC Log - %s/%s\n"
                      (or network "unknown") (or channel "unknown")))
      (insert (format "#+DATE: %s\n" (format-time-string "%Y-%m-%d")))
      (insert "#+STARTUP: showall\n\n")
      (let ((log-lines nil)
            (current-date nil))
        (with-temp-buffer
          (insert-file-contents log-file)
          (goto-char (point-min))
          (while (not (eobp))
            (let ((line (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position))))
              (push line log-lines))
            (forward-line 1)))
        (dolist (line (nreverse log-lines))
          (when (string-match "\\`\\[\\([0-9-]+\\) \\([0-9:]+\\)\\] \\(.*\\)\\'" line)
            (let ((date (match-string 1 line))
                  (time (match-string 2 line))
                  (content (match-string 3 line)))
              (unless (equal date current-date)
                (setq current-date date)
                (insert (format "* %s\n\n" date)))
              (insert (format "- =%s= %s\n" time content))))))
      (write-region (point-min) (point-max) output-file))
    (message "Exported to %s" output-file)
    (find-file-other-window output-file)))

;; --- Registration ---

(declare-function org-link-set-parameters "ol")
(declare-function org-link-store-props "ol")

(with-eval-after-load 'org
  (org-link-set-parameters
   "irc"
   :store #'clatter-org-store-link
   :follow (lambda (path)
             (message "IRC link: %s (use M-x clatter to connect)" path))))

(provide 'clatter-org)

;;; clatter-org.el ends here
