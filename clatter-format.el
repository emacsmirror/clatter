;;; clatter-format.el --- mIRC color/formatting parser -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Glenn Thompson
;; Author: Glenn Thompson <glenn@paren.works>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Parses and renders mIRC formatting codes in IRC messages.
;; Handles: bold (\x02), italic (\x1D), underline (\x1F),
;; strikethrough (\x1E), reverse (\x16), monospace (\x11),
;; color (\x03 fg[,bg]), hex color (\x04), and reset (\x0F).

;;; Code:

(require 'cl-lib)

;; --- Configuration ---

(defcustom clatter-format-enable t
  "Enable mIRC color and formatting code rendering.
When nil, formatting codes are stripped but not rendered."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-format-strip-only nil
  "If non-nil, strip all formatting codes without rendering.
Overrides `clatter-format-enable'."
  :type 'boolean
  :group 'clatter)

;; --- mIRC Color Palette ---
;; Standard 16-color mIRC palette

(defconst clatter-format--mirc-colors
  ["#ffffff"   ; 0  white
   "#000000"   ; 1  black
   "#00007f"   ; 2  blue (navy)
   "#009300"   ; 3  green
   "#ff0000"   ; 4  red
   "#7f0000"   ; 5  brown (maroon)
   "#9c009c"   ; 6  purple
   "#fc7f00"   ; 7  orange (olive)
   "#ffff00"   ; 8  yellow
   "#00fc00"   ; 9  light green
   "#009393"   ; 10 teal (cyan)
   "#00ffff"   ; 11 light cyan (aqua)
   "#0000fc"   ; 12 light blue (royal)
   "#ff00ff"   ; 13 pink (light purple)
   "#7f7f7f"   ; 14 grey
   "#d2d2d2"]  ; 15 light grey (silver)
  "Standard 16-color mIRC palette.")

;; Extended 99-color palette (indices 16-98)
(defconst clatter-format--mirc-colors-extended
  ["#470000" "#472100" "#474700" "#324700" "#004700" "#00472c"
   "#004747" "#002747" "#000047" "#2e0047" "#470047" "#47002a"
   "#740000" "#743a00" "#747400" "#517400" "#007400" "#007449"
   "#007474" "#004074" "#000074" "#4b0074" "#740074" "#740045"
   "#b50000" "#b56300" "#b5b500" "#7db500" "#00b500" "#00b571"
   "#00b5b5" "#0063b5" "#0000b5" "#7500b5" "#b500b5" "#b5006b"
   "#ff0000" "#ff8c00" "#ffff00" "#b2ff00" "#00ff00" "#00ffa0"
   "#00ffff" "#008cff" "#0000ff" "#a500ff" "#ff00ff" "#ff0098"
   "#ff5959" "#ffb459" "#ffff71" "#cfff60" "#6fff6f" "#65ffc9"
   "#6dffff" "#59b4ff" "#5959ff" "#c459ff" "#ff66ff" "#ff59bc"
   "#ff9c9c" "#ffd39c" "#ffff9c" "#e2ff9c" "#9cff9c" "#9cffdb"
   "#9cffff" "#9cd3ff" "#9c9cff" "#dc9cff" "#ff9cff" "#ff94d3"
   "#000000" "#131313" "#282828" "#363636" "#4d4d4d" "#656565"
   "#818181" "#9f9f9f" "#bcbcbc" "#e2e2e2" "#ffffff"]
  "Extended mIRC color palette (indices 16-98).")

(defun clatter-format--color-for-index (idx)
  "Return hex color string for mIRC color index IDX."
  (cond
   ((and (>= idx 0) (< idx 16))
    (aref clatter-format--mirc-colors idx))
   ((and (>= idx 16) (< idx 99))
    (aref clatter-format--mirc-colors-extended (- idx 16)))
   (t nil)))

;; --- Formatting code constants ---

(defconst clatter-format--bold      ?\x02)
(defconst clatter-format--color     ?\x03)
(defconst clatter-format--hex-color ?\x04)
(defconst clatter-format--reset     ?\x0F)
(defconst clatter-format--reverse   ?\x16)
(defconst clatter-format--italic    ?\x1D)
(defconst clatter-format--strikethrough ?\x1E)
(defconst clatter-format--underline ?\x1F)
(defconst clatter-format--monospace ?\x11)

;; --- Parser ---

(defun clatter-format-parse (text)
  "Parse mIRC formatting codes in TEXT and return propertized string.
If `clatter-format-strip-only' is non-nil, strips codes without rendering.
If `clatter-format-enable' is nil, returns TEXT unchanged."
  (if (not (string-match-p "[\x02\x03\x04\x0F\x11\x16\x1D\x1E\x1F]" text))
      text  ; fast path: no formatting codes
    (if clatter-format-strip-only
        (clatter-format--strip text)
      (if clatter-format-enable
          (clatter-format--render text)
        text))))

(defun clatter-format--strip (text)
  "Strip all mIRC formatting codes from TEXT, returning plain string."
  (let ((result (replace-regexp-in-string
                 "\x03\\([0-9]\\{1,2\\}\\(,[0-9]\\{1,2\\}\\)?\\)?" "" text)))
    (setq result (replace-regexp-in-string
                  "\x04\\([0-9a-fA-F]\\{6\\}\\(,[0-9a-fA-F]\\{6\\}\\)?\\)?" "" result))
    (replace-regexp-in-string "[\x02\x0F\x11\x16\x1D\x1E\x1F]" "" result)))

(defun clatter-format--render (text)
  "Render mIRC formatting codes in TEXT as Emacs face properties."
  (let ((pos 0)
        (len (length text))
        (result "")
        ;; Active state
        (bold nil)
        (italic nil)
        (underline nil)
        (strikethrough nil)
        (reverse-video nil)
        (monospace nil)
        (fg-color nil)
        (bg-color nil))
    (while (< pos len)
      (let ((ch (aref text pos)))
        (cond
         ;; Bold toggle
         ((= ch clatter-format--bold)
          (setq bold (not bold))
          (cl-incf pos))

         ;; Italic toggle
         ((= ch clatter-format--italic)
          (setq italic (not italic))
          (cl-incf pos))

         ;; Underline toggle
         ((= ch clatter-format--underline)
          (setq underline (not underline))
          (cl-incf pos))

         ;; Strikethrough toggle
         ((= ch clatter-format--strikethrough)
          (setq strikethrough (not strikethrough))
          (cl-incf pos))

         ;; Reverse toggle
         ((= ch clatter-format--reverse)
          (setq reverse-video (not reverse-video))
          (cl-incf pos))

         ;; Monospace toggle
         ((= ch clatter-format--monospace)
          (setq monospace (not monospace))
          (cl-incf pos))

         ;; Reset all
         ((= ch clatter-format--reset)
          (setq bold nil italic nil underline nil
                strikethrough nil reverse-video nil
                monospace nil fg-color nil bg-color nil)
          (cl-incf pos))

         ;; Color code: \x03[fg[,bg]]
         ((= ch clatter-format--color)
          (cl-incf pos)
          (if (and (< pos len) (cl-digit-char-p (aref text pos)))
              ;; Parse foreground
              (let ((fg-start pos))
                (while (and (< pos len) (cl-digit-char-p (aref text pos))
                            (< (- pos fg-start) 2))
                  (cl-incf pos))
                (setq fg-color (clatter-format--color-for-index
                                (string-to-number (substring text fg-start pos))))
                ;; Optional background
                (if (and (< pos len) (= (aref text pos) ?,)
                         (< (1+ pos) len) (cl-digit-char-p (aref text (1+ pos))))
                    (progn
                      (cl-incf pos) ; skip comma
                      (let ((bg-start pos))
                        (while (and (< pos len) (cl-digit-char-p (aref text pos))
                                    (< (- pos bg-start) 2))
                          (cl-incf pos))
                        (setq bg-color (clatter-format--color-for-index
                                        (string-to-number
                                         (substring text bg-start pos))))))
                  ;; No background specified
                  nil))
            ;; Bare \x03 with no number = reset colors
            (setq fg-color nil bg-color nil)))

         ;; Hex color: \x04[RRGGBB[,RRGGBB]]
         ((= ch clatter-format--hex-color)
          (cl-incf pos)
          (if (and (<= (+ pos 6) len)
                   (string-match-p "\\`[0-9a-fA-F]\\{6\\}"
                                   (substring text pos (min (+ pos 6) len))))
              (progn
                (setq fg-color (concat "#" (substring text pos (+ pos 6))))
                (cl-incf pos 6)
                (when (and (< pos len) (= (aref text pos) ?,)
                           (<= (+ pos 7) len)
                           (string-match-p "\\`[0-9a-fA-F]\\{6\\}"
                                           (substring text (1+ pos) (+ pos 7))))
                  (cl-incf pos) ; skip comma
                  (setq bg-color (concat "#" (substring text pos (+ pos 6))))
                  (cl-incf pos 6)))
            ;; Bare \x04 = reset colors
            (setq fg-color nil bg-color nil)))

         ;; Normal character - apply current formatting
         (t
          (let ((face (clatter-format--build-face
                       bold italic underline strikethrough
                       reverse-video monospace fg-color bg-color))
                (char-str (char-to-string ch)))
            (when face
              (setq char-str (propertize char-str 'face face)))
            (setq result (concat result char-str)))
          (cl-incf pos)))))
    result))

(defun clatter-format--build-face (bold italic underline strikethrough
                                        reverse-video monospace fg-color bg-color)
  "Build a face spec from the current formatting state.
BOLD, ITALIC, UNDERLINE, STRIKETHROUGH, REVERSE-VIDEO, MONOSPACE,
FG-COLOR and BG-COLOR are the active formatting attributes.
Returns nil if no formatting is active."
  (let ((face nil))
    (when bold (push :weight face) (push 'bold face))
    (when italic (push :slant face) (push 'italic face))
    (when underline (push :underline face) (push t face))
    (when strikethrough (push :strike-through face) (push t face))
    (when monospace (push :family face) (push "Monospace" face))
    (when (and fg-color (not reverse-video))
      (push :foreground face) (push fg-color face))
    (when (and bg-color (not reverse-video))
      (push :background face) (push bg-color face))
    (when (and reverse-video fg-color)
      (push :background face) (push fg-color face))
    (when (and reverse-video bg-color)
      (push :foreground face) (push bg-color face))
    (when (and reverse-video (not fg-color) (not bg-color))
      (push :inverse-video face) (push t face))
    (when face
      (nreverse face))))

(provide 'clatter-format)

;;; clatter-format.el ends here
