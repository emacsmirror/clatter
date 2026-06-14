;;; clatter-pals.el --- Pals and fools lists for clatter -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Glenn Thompson
;; Author: Glenn Thompson
;; License: MIT
;; URL: https://github.com/parenworks/clatter.el

;;; Commentary:

;; Two complementary nick lists:
;;
;; - Pals (friends): their nick is highlighted with the `clatter-pal'
;;   face wherever it appears, so you never miss them.
;; - Fools: their messages are muted (suppressed) much like the ignore
;;   list, but kept in a separate, easily toggled list.
;;
;; Both are matched case-insensitively and managed with the /pal,
;; /unpal, /pals, /fool, /unfool and /fools commands.

;;; Code:

(require 'cl-lib)
(require 'clatter-config)

;; --- Lists ---

(defcustom clatter-pals nil
  "List of nicks treated as pals (friends).
Their nick is highlighted with the `clatter-pal' face wherever it
appears.  Matched case-insensitively."
  :type '(repeat string)
  :group 'clatter)

(defcustom clatter-fools nil
  "List of nicks treated as fools.
Messages from a fool are suppressed, like `clatter-ignore-list', but
kept in a separate list that can be toggled with the /fool and /unfool
commands.  Matched case-insensitively."
  :type '(repeat string)
  :group 'clatter)

;; --- Faces ---

(defface clatter-pal
  '((t :foreground "#42be65" :weight bold))
  "Face used to highlight a pal's nick."
  :group 'clatter)

;; --- Membership (pure) ---

(defun clatter--nick-member-p (nick list)
  "Return non-nil if NICK is in LIST, compared case-insensitively."
  (and nick
       (let ((n (downcase nick)))
         (cl-some (lambda (x) (string= n (downcase x))) list))))

(defun clatter-pal-p (nick)
  "Return non-nil if NICK is a pal."
  (clatter--nick-member-p nick clatter-pals))

(defun clatter-fool-p (nick)
  "Return non-nil if NICK is a fool."
  (clatter--nick-member-p nick clatter-fools))

(defun clatter-muted-p (sender)
  "Return non-nil if SENDER's messages should be hidden.
True when SENDER is on the ignore list or the fools list."
  (or (clatter-ignored-p sender)
      (clatter-fool-p sender)))

;; --- Pure add/remove helpers ---

(defun clatter--nick-list-add (nick list)
  "Return LIST with NICK added unless already present (case-insensitive)."
  (if (clatter--nick-member-p nick list)
      list
    (cons nick list)))

(defun clatter--nick-list-remove (nick list)
  "Return LIST with NICK removed (case-insensitive)."
  (let ((n (downcase nick)))
    (cl-remove-if (lambda (x) (string= n (downcase x))) list)))

(provide 'clatter-pals)

;;; clatter-pals.el ends here
