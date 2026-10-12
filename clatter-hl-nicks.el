;;; clatter-hl-nicks.el --- Nick highlighting in message text -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Glenn Thompson
;; Author: Glenn Thompson <glenn@paren.works>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Enhanced nick colorization for clatter.el.
;; Highlights nicknames wherever they appear in message text,
;; using stable hash-based colors consistent across sessions.
;; Inspired by erc-hl-nicks but built-in and IRCv3-aware.

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'elisp-mode)
(require 'clatter-config)
(require 'clatter-model)
(require 'clatter-format)
(require 'clatter-pals)

;; --- Configuration ---

(defcustom clatter-hl-nicks-enabled t
  "Enable in-text nick highlighting."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-hl-elisp-symbols-enabled t
  "Turn quoted Elisp symbol names into links to their help pages."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-hl-nicks-minimum-length 3
  "Minimum nick length to highlight in message text.
Short nicks produce too many false positives."
  :type 'integer
  :group 'clatter)

(defcustom clatter-hl-nicks-skip-nicks nil
  "List of nicks to never highlight in message text."
  :type '(repeat string)
  :group 'clatter)

(defcustom clatter-hl-nicks-skip-faces '(clatter-timestamp clatter-system clatter-notice)
  "Faces that should not receive nick highlighting."
  :type '(repeat symbol)
  :group 'clatter)

(defcustom clatter-hl-nicks-alias-alist nil
  "Alist mapping nick aliases to canonical nicks for color consistency.
Example: ((\"beach_\" . \"beach\") (\"jackdaniel_\" . \"jackdaniel\"))
Both the alias and canonical nick will get the same color."
  :type '(alist :key-type string :value-type string)
  :group 'clatter)

;; --- Keyword Highlighting ---

(defcustom clatter-hl-keywords nil
  "List of keywords to highlight in message text.
Each entry is a string.  Matching is case-insensitive."
  :type '(repeat string)
  :group 'clatter)

(defface clatter-hl-keyword
  '((t :inherit match :weight bold))
  "Face for highlighted keywords in messages."
  :group 'clatter)

(defun clatter-hl-keywords-in-string (text)
  "Highlight keywords from `clatter-hl-keywords' in TEXT."
  (if (or (null clatter-hl-keywords) (string-empty-p text))
      text
    (let ((result text))
      (dolist (keyword clatter-hl-keywords)
        (let ((case-fold-search t)
              (pattern (regexp-quote keyword))
              (pos 0)
              (new-result ""))
          (while (string-match (concat "\\b" pattern "\\b") result pos)
            (let ((start (match-beginning 0))
                  (end (match-end 0)))
              (setq new-result (concat new-result
                                       (substring result pos start)
                                       (propertize (substring result start end)
                                                   'face 'clatter-hl-keyword)))
              (setq pos end)))
          (setq new-result (concat new-result (substring result pos)))
          (setq result new-result)))
      result)))

;; --- Nick color palette (theme-driven) ---
;;
;; Nicks inherit from a rotating list of standard Emacs faces (font-lock-* and
;; friends) so their colors follow the active theme and stay legible on both
;; light and dark backgrounds.  Each palette entry becomes a real, named face
;; `clatter-nick-color-N' (see `clatter-hl-rebuild-nick-faces') that inherits
;; from the Nth base face here.

(defcustom clatter-hl-nick-base-faces
  '(font-lock-keyword-face
    font-lock-string-face
    font-lock-type-face
    font-lock-function-name-face
    font-lock-variable-name-face
    font-lock-constant-face
    font-lock-builtin-face
    font-lock-preprocessor-face
    font-lock-warning-face
    font-lock-comment-delimiter-face
    font-lock-doc-face
    font-lock-negation-char-face)
  "Faces nicks inherit from, rotated by a hash of the nick.
Each entry is a standard theme face (typically a `font-lock-*' face) whose
foreground the active theme defines with a legible color.  Nicks are assigned
deterministically by `clatter-hl-nick-index'.  Customize this list to change
the palette; rebuild with `clatter-hl-rebuild-nick-faces' (prefix arg to
refresh existing faces)."
  :type '(repeat face)
  :group 'clatter)

(defun clatter-hl-nick-index (nick)
  "Return the palette index for NICK (deterministic, hash-based).
Uses the canonical nick from `clatter-hl-nicks-alias-alist' if present."
  (let* ((canonical (or (cdr (assoc nick clatter-hl-nicks-alias-alist))
                        nick))
         (hash (cl-reduce #'+ (mapcar #'identity (downcase canonical)))))
    (mod hash (length clatter-hl-nick-base-faces))))

(defun clatter-hl-nick-color (nick)
  "Return the resolved foreground color of NICK's highlight face.
This is a compatibility helper for callers that want a literal color; prefer
`clatter-hl-nick-face-symbol' for text properties so the face (and thus the
theme) is honored.  Returns `unspecified' when the active theme gives the
underlying base face no explicit foreground."
  (face-foreground (clatter-hl-nick-face-symbol nick) nil t))

;; --- Named nick faces ---
;;
;; Each palette color is exposed as a real, named face
;; (`clatter-nick-color-0' ... `clatter-nick-color-N') so nick colors can
;; be themed and customized like any other face, instead of being applied
;; as anonymous (:foreground ...) specs.

(defun clatter-hl--nick-face-name (idx)
  "Return the face symbol for palette index IDX."
  (intern (format "clatter-nick-color-%d" idx)))

(defun clatter-hl-rebuild-nick-faces (&optional force)
  "Define a named face for each entry in `clatter-hl-nick-base-faces'.
Face `clatter-nick-color-N' inherits from the Nth base face and is bold.
Existing faces are left alone unless FORCE is non-nil (as when called
interactively), so user or theme customizations are preserved across
reloads; call with a prefix arg to refresh them after changing the palette."
  (interactive (list t))
  (let ((idx 0))
    (dolist (base clatter-hl-nick-base-faces)
      (let ((face (clatter-hl--nick-face-name idx)))
        (cond
         ;; New face: declare it so it is themeable and shows up in Customize.
         ((not (facep face))
          (custom-declare-face
           ;; :slant normal prevents italic leaking in from italic base
           ;; faces (font-lock-string-face, font-lock-doc-face, etc.).
           face `((t (:inherit ,base :weight bold :slant normal)))
           (format "Clatter nick highlight color %d (inherits %s)." idx base)
           :group 'clatter))
         ;; Existing face: only overwrite when explicitly forced, so user or
         ;; theme customizations survive normal reloads.
         (force
          (set-face-attribute face nil :inherit base :weight 'bold :slant 'normal))))
      (setq idx (1+ idx)))))

(defun clatter-hl-nick-face-symbol (nick)
  "Return the named face symbol used to highlight NICK.
A pal (see `clatter-pals') gets the `clatter-pal' face; otherwise the
deterministic, hash-based `clatter-nick-color-N' palette face."
  (if (clatter-pal-p nick)
      'clatter-pal
    (clatter-hl--nick-face-name (clatter-hl-nick-index nick))))

;; --- In-text highlighting ---

(defun clatter-hl-nicks-in-string (text buffer)
  "Return TEXT with nicks from BUFFER's nick list highlighted.
Only highlights nicks that are currently in the channel.
Return TEXT itself if no eligible match needs highlighting."
  (if (or (not clatter-hl-nicks-enabled)
          (not (buffer-live-p buffer)))
      text
    (with-current-buffer buffer
      (when clatter--nick-list
        (let ((nicks (clatter-hl--collect-nicks buffer)))
          (with-syntax-table clatter-nick-syntax-table
            (dolist (nick nicks)
              (let ((re (rx bow (literal nick) eow)))
                (setq text (clatter-hl--propertize-matches text re nick)))))))
      text)))

(defun clatter-hl--collect-nicks (buffer)
  "Collect eligible nicks from BUFFER's nick list."
  (with-current-buffer buffer
    (let ((skip-nicks (mapcar #'downcase clatter-hl-nicks-skip-nicks))
          nicks)
      (when clatter--nick-list
        (maphash (lambda (nick _prefix)
                   (when (and (>= (length nick) clatter-hl-nicks-minimum-length)
                              (not (member (downcase nick) skip-nicks)))
                     (push nick nicks)))
                 clatter--nick-list))
      nicks)))

(defun clatter-hl--propertize-matches (text regexp nick)
  "Highlight matches of REGEXP in TEXT as NICK.
Skip regions that already have a face from `clatter-hl-nicks-skip-faces'.
Copy TEXT only on the first eligible match, leaving the input unchanged."
  (let ((pos 0)
        (result text)
        props)
    (while (string-match regexp text pos)
      (let* ((start (match-beginning 0))
             (end (match-end 0))
             (existing-face (get-text-property start 'face text)))
        (unless (and existing-face
                     (or (memq existing-face clatter-hl-nicks-skip-faces)
                         (and (listp existing-face)
                              (cl-intersection existing-face
                                               clatter-hl-nicks-skip-faces))))
          (when (eq result text)
            (setq result (copy-sequence text)
                  props (list 'face (clatter-hl-nick-face-symbol nick)
                              'clatter-nick nick)))
          (add-text-properties start end props result))
        (setq pos end)))
    result))

;; --- Highlight nicks in the <nick> prefix too ---

(defun clatter-hl-nick-face (nick conn)
  "Return the face used to highlight NICK on CONN.
Returns `clatter-my-nick' for our own nick, otherwise a named
`clatter-nick-color-N' face from the palette."
  (if (string-equal nick (clatter-connection-nick conn))
      'clatter-my-nick
    (clatter-hl-nick-face-symbol nick)))

;; --- Elisp symbol and URL links ---

(defvar clatter-hl-elisp-symbol-regexp
  "[`‘]\\(\\(?:\\sw\\|\\s_\\)+\\)['’]"
  "Regexp matching doc-quoted Elisp symbol names.")

(defun clatter-hl--describe-symbol-button (button)
  "Describe the Elisp symbol named by BUTTON, or show apropos results."
  (let* ((name (button-get button 'clatter-elisp-symbol))
         (symbol (intern-soft name)))
    (if (and symbol
             (or (boundp symbol) (fboundp symbol) (facep symbol)))
        (describe-symbol symbol)
      (apropos name))))

(defun clatter-hl-elisp-symbols-in-string (text)
  "Turn doc-quoted Elisp symbol names in TEXT into help buttons."
  (if (not clatter-hl-elisp-symbols-enabled)
      text
    (let ((pos 0)
          (result ""))
      (with-syntax-table emacs-lisp-mode-syntax-table
        (while (string-match clatter-hl-elisp-symbol-regexp text pos)
          (let ((start (match-beginning 1))
                (end (match-end 1))
                (name (match-string-no-properties 1 text)))
            (setq result
                  (concat result
                          (substring text pos start)
                          (propertize (substring text start end)
                                      'button '(t)
                                      'category 'default-button
                                      'follow-link t
                                      'clatter-elisp-symbol name
                                      'face 'link
                                      'help-echo (format "Describe `%s'" name)
                                      'action #'clatter-hl--describe-symbol-button)))
            (setq pos end))))
      (concat result (substring text pos)))))

(defvar clatter-url-regexp
  "https?://[^ \t\n\r<>\"']+"
  "Regexp matching URLs in IRC messages.")

(defun clatter-hl-urls-in-string (text)
  "Highlight URLs in TEXT as standard Emacs buttons."
  (let ((pos 0)
        (result ""))
    (while (string-match clatter-url-regexp text pos)
      (let ((start (match-beginning 0))
            (end (match-end 0))
            (url (match-string 0 text)))
        (setq result (concat result (substring text pos start)))
        (setq result
              (concat
               result
               (propertize url
                           'button '(t)
                           'category 'default-button
                           'follow-link t
                           'clatter-url url
                           'face 'link
                           'help-echo url
                           'action (lambda (button)
                                     (browse-url
                                      (button-get button 'clatter-url))))))
        (setq pos end)))
    (concat result (substring text pos))))

(defvar clatter-hl-url-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'clatter-hl-open-url-at-point)
    (define-key map [mouse-1] #'clatter-hl-open-url-at-point)
    map)
  "Keymap for clickable URLs in clatter buffers.")

(defun clatter-hl-open-url-at-point ()
  "Open the URL at point."
  (interactive)
  (let ((url (get-text-property (point) 'clatter-url)))
    (if url
        (browse-url url)
      (message "No URL at point"))))

(defun clatter-hl-open-url-nearest ()
  "Open the nearest URL on the current line, searching forward then backward."
  (interactive)
  (let ((url (get-text-property (point) 'clatter-url)))
    (if url
        (browse-url url)
      ;; Search forward on line
      (let ((found nil)
            (pos (point))
            (eol (line-end-position))
            (bol (line-beginning-position)))
        (save-excursion
          ;; Forward
          (while (and (not found) (< pos eol))
            (setq pos (next-single-property-change pos 'clatter-url nil eol))
            (when (and pos (get-text-property pos 'clatter-url))
              (setq found (get-text-property pos 'clatter-url))))
          ;; Backward
          (unless found
            (setq pos (point))
            (while (and (not found) (> pos bol))
              (setq pos (previous-single-property-change pos 'clatter-url nil bol))
              (when (and pos (get-text-property pos 'clatter-url))
                (setq found (get-text-property pos 'clatter-url))))))
        (if found
            (browse-url found)
          (message "No URL on this line"))))))

;; --- URL collector ---

(defun clatter-hl--collect-urls ()
  "Collect all URLs from the current buffer with context.
Returns a list of (DISPLAY-STRING . URL) pairs, newest first."
  (let (urls)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((url (get-text-property (point) 'clatter-url)))
          (when url
            (let* ((bol (line-beginning-position))
                   (sender (get-text-property bol 'clatter-sender))
                   (display (if sender
                                (format "%s - %s" sender url)
                              (format "%s" url))))
              (unless (cl-find url urls :key #'cdr :test #'string-equal)
                (push (cons display url) urls)))))
        ;; Jump to next url property change
        (let ((next (next-single-property-change (point) 'clatter-url)))
          (if next
              (goto-char next)
            (goto-char (point-max))))))
    urls))

(defun clatter-hl-browse-urls ()
  "Show all URLs from the current buffer and open the selected one."
  (interactive)
  (let ((urls (clatter-hl--collect-urls)))
    (if (null urls)
        (message "No URLs found in this buffer")
      (let* ((selection (completing-read "Open URL: " urls nil t))
             (url (cdr (assoc selection urls))))
        (when url
          (browse-url url))))))

(defun clatter-hl-copy-url ()
  "Show all URLs from the current buffer and copy the selected one."
  (interactive)
  (let ((urls (clatter-hl--collect-urls)))
    (if (null urls)
        (message "No URLs found in this buffer")
      (let* ((selection (completing-read "Copy URL: " urls nil t))
             (url (cdr (assoc selection urls))))
        (when url
          (kill-new url)
          (message "Copied: %s" url))))))

;; --- Integration: apply all highlighting to message text ---

(defun clatter-hl-format-text (text buffer conn)
  "Apply all highlighting to message TEXT for BUFFER using CONN.
Applies mIRC formatting, URLs, nicks, keywords, and Elisp symbol links."
  (ignore conn)
  (let ((result text))
    (setq result (clatter-format-parse result))
    (setq result (clatter-hl-urls-in-string result))
    (setq result (clatter-hl-nicks-in-string result buffer))
    (setq result (clatter-hl-keywords-in-string result))
    (setq result (clatter-hl-elisp-symbols-in-string result))
    result))

;; Build the named nick faces once at load time.
(clatter-hl-rebuild-nick-faces)

(provide 'clatter-hl-nicks)

;;; clatter-hl-nicks.el ends here
