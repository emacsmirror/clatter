;;; clatter-rawlog.el --- Raw IRC protocol inspector -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Glenn Thompson
;; Author: Glenn Thompson <glenn@paren.works>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Raw protocol log for clatter.el.
;; Shows incoming and outgoing IRC lines with timestamps,
;; parsed structure, and capability state.
;; Essential for debugging IRCv3 compliance.

;;; Code:

(require 'cl-lib)
(require 'clatter-config)
(require 'clatter-connection)
(require 'clatter-protocol)

;; --- Configuration ---

(defcustom clatter-rawlog-enabled nil
  "Enable raw protocol logging.
When non-nil, all IRC traffic is logged to rawlog buffers."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-rawlog-max-lines 5000
  "Maximum number of lines to keep in rawlog buffers."
  :type 'integer
  :group 'clatter)

(defcustom clatter-rawlog-show-parsed t
  "Show parsed message structure alongside raw lines."
  :type 'boolean
  :group 'clatter)

;; --- Faces ---

(defface clatter-rawlog-incoming
  '((t :foreground "#c3e88d"))
  "Face for incoming (server to client) raw lines."
  :group 'clatter)

(defface clatter-rawlog-outgoing
  '((t :foreground "#82aaff"))
  "Face for outgoing (client to server) raw lines."
  :group 'clatter)

(defface clatter-rawlog-timestamp
  '((t :foreground "#7c7c7c"))
  "Face for timestamps in rawlog."
  :group 'clatter)

(defface clatter-rawlog-tag
  '((t :foreground "#c792ea"))
  "Face for IRCv3 message tags."
  :group 'clatter)

(defface clatter-rawlog-command
  '((t :foreground "#ffcb6b" :weight bold))
  "Face for IRC command names."
  :group 'clatter)

(defface clatter-rawlog-prefix
  '((t :foreground "#89ddff"))
  "Face for message prefix/source."
  :group 'clatter)

;; --- Buffer management ---

(defun clatter-rawlog-buffer-name (network)
  "Return the rawlog buffer name for NETWORK."
  (format "*clatter-rawlog:%s*" network))

(defun clatter-rawlog-get-buffer (network)
  "Get or create the rawlog buffer for NETWORK."
  (let ((name (clatter-rawlog-buffer-name network)))
    (or (get-buffer name)
        (with-current-buffer (get-buffer-create name)
          (clatter-rawlog-mode)
          (setq-local clatter-rawlog--network network)
          (current-buffer)))))

(defvar-local clatter-rawlog--network nil
  "Network ID for this rawlog buffer.")

;; --- Logging ---

(defun clatter-rawlog--insert (network direction line)
  "Insert a raw LINE into the rawlog buffer for NETWORK.
DIRECTION is :in or :out."
  (when clatter-rawlog-enabled
    (let ((buf (clatter-rawlog-get-buffer network)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (let ((inhibit-read-only t)
                (at-end (eobp))
                (ts (propertize (format-time-string "%H:%M:%S ")
                                'face 'clatter-rawlog-timestamp))
                (dir-str (if (eq direction :in)
                             (propertize "<< " 'face 'clatter-rawlog-incoming)
                           (propertize ">> " 'face 'clatter-rawlog-outgoing)))
                (line-face (if (eq direction :in)
                               'clatter-rawlog-incoming
                             'clatter-rawlog-outgoing)))
            (save-excursion
              (goto-char (point-max))
              (insert ts dir-str
                      (propertize (string-trim-right line) 'face line-face)
                      "\n")
              ;; Show parsed structure if enabled
              (when (and clatter-rawlog-show-parsed (eq direction :in))
                (clatter-rawlog--insert-parsed line))
              ;; Trim old lines
              (clatter-rawlog--trim))
            (when at-end
              (goto-char (point-max)))))))))

(defun clatter-rawlog--insert-parsed (line)
  "Insert parsed structure of raw LINE."
  (let ((parsed (ignore-errors (clatter-parse-line line))))
    (when parsed
      (let* ((tags (clatter-message-tags parsed))
             (prefix (clatter-message-prefix parsed))
             (command (clatter-message-command parsed))
             (params (clatter-message-params parsed)))
        (when (or tags prefix)
          (insert "         "
                  (if tags
                    (propertize (format "tags:%S " tags) 'face 'clatter-rawlog-tag) "")
                  (if prefix
                    (propertize (format "from:%s " prefix) 'face 'clatter-rawlog-prefix) "")
                  (propertize (format "cmd:%s " command) 'face 'clatter-rawlog-command)
                  (format "params:%S" params)
                  "\n"))))))

(defun clatter-rawlog--trim ()
  "Trim rawlog buffer to `clatter-rawlog-max-lines'."
  (let ((lines (count-lines (point-min) (point-max))))
    (when (> lines clatter-rawlog-max-lines)
      (save-excursion
        (goto-char (point-min))
        (forward-line (- lines clatter-rawlog-max-lines))
        (delete-region (point-min) (point))))))

;; --- Hook into connection layer ---

(defvar clatter-rawlog--original-send nil
  "Storage for the original `clatter-send' function when wrapping.")

(defun clatter-rawlog--on-incoming (network line)
  "Log incoming LINE from NETWORK."
  (clatter-rawlog--insert network :in line))

(defun clatter-rawlog--on-outgoing (network line)
  "Log outgoing LINE to NETWORK."
  (clatter-rawlog--insert network :out line))

;; These hooks should be called from clatter-connection.el
(defvar clatter-rawlog-incoming-hook nil
  "Hook called with (NETWORK LINE) for each incoming raw IRC line.")

(defvar clatter-rawlog-outgoing-hook nil
  "Hook called with (NETWORK LINE) for each outgoing raw IRC line.")

;; --- Major mode ---

(defvar clatter-rawlog-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "c") #'clatter-rawlog-clear)
    (define-key map (kbd "t") #'clatter-rawlog-toggle-parsed)
    (define-key map (kbd "f") #'clatter-rawlog-filter)
    (define-key map (kbd "s") #'clatter-rawlog-send-raw)
    (define-key map (kbd "C") #'clatter-rawlog-show-caps)
    map)
  "Keymap for clatter rawlog mode.")

(define-derived-mode clatter-rawlog-mode special-mode "CLatter-Raw"
  "Major mode for viewing raw IRC protocol traffic.
\\{clatter-rawlog-mode-map}")

;; --- Interactive commands ---

(defun clatter-rawlog-open (network)
  "Open the rawlog buffer for NETWORK."
  (interactive
   (list (completing-read "Network: "
                          (let (ids)
                            (maphash (lambda (id _conn) (push id ids))
                                     clatter-connections)
                            ids)
                          nil t)))
  (clatter-rawlog-enable)
  (switch-to-buffer (clatter-rawlog-get-buffer network)))

(defun clatter-rawlog-clear ()
  "Clear the current rawlog buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (message "Rawlog cleared")))

(defun clatter-rawlog-toggle-parsed ()
  "Toggle showing parsed message structure."
  (interactive)
  (setq clatter-rawlog-show-parsed (not clatter-rawlog-show-parsed))
  (message "Parsed display: %s" (if clatter-rawlog-show-parsed "on" "off")))

(defun clatter-rawlog-filter (pattern)
  "Show only lines matching PATTERN in a new buffer."
  (interactive "sFilter pattern: ")
  (let ((matches nil)
        (inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
          (when (string-match-p pattern line)
            (push line matches)))
        (forward-line 1)))
    (with-current-buffer (get-buffer-create
                          (format "*clatter-rawlog-filter:%s*"
                                  (or clatter-rawlog--network "?")))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Filter: %s\n%s\n\n" pattern (make-string 40 ?-)))
        (dolist (line (nreverse matches))
          (insert line "\n"))
        (goto-char (point-min))
        (special-mode))
      (display-buffer (current-buffer)))))

(defun clatter-rawlog-send-raw (line)
  "Send a raw IRC LINE to the network for this rawlog buffer."
  (interactive "sRaw IRC: ")
  (when clatter-rawlog--network
    (let ((conn (clatter-get-connection clatter-rawlog--network)))
      (when conn
        (clatter-send conn line)
        (message "Sent: %s" line)))))

(defun clatter-rawlog-show-caps ()
  "Show capability negotiation state for this network."
  (interactive)
  (when clatter-rawlog--network
    (let ((conn (clatter-get-connection clatter-rawlog--network)))
      (if conn
          (with-current-buffer (get-buffer-create "*clatter-caps*")
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert (format "CAP State for %s\n" clatter-rawlog--network))
              (insert (make-string 40 ?-) "\n\n")
              (insert (format "Enabled: %s\n\n"
                              (string-join
                               (clatter-connection-cap-enabled conn) ", ")))
              (insert (format "Available: %s\n"
                              (string-join
                               (clatter-connection-cap-available conn) ", ")))
              (goto-char (point-min))
              (special-mode))
            (display-buffer (current-buffer)))
        (message "No connection for %s" clatter-rawlog--network)))))

;; --- Enable/disable ---

(defun clatter-rawlog-enable ()
  "Enable raw protocol logging."
  (interactive)
  (setq clatter-rawlog-enabled t)
  (add-hook 'clatter-rawlog-incoming-hook #'clatter-rawlog--on-incoming)
  (add-hook 'clatter-rawlog-outgoing-hook #'clatter-rawlog--on-outgoing)
  (message "[clatter-rawlog] Enabled"))

(defun clatter-rawlog-disable ()
  "Disable raw protocol logging."
  (interactive)
  (setq clatter-rawlog-enabled nil)
  (message "[clatter-rawlog] Disabled"))

(provide 'clatter-rawlog)

;;; clatter-rawlog.el ends here
