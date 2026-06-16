;;; clatter-pals.el --- Pals and fools lists for clatter -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Glenn Thompson
;; Author: Glenn Thompson <glenn@paren.works>
;; SPDX-License-Identifier: MIT
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
  :type '(repeat (choice (string :tag "Nick Only")
                         (cons :tag "Nick and Network"
                               (string :tag "Nick")
                               (string :tag "Network"))))
  :group 'clatter)

(defcustom clatter-fools nil
  "List of nicks treated as fools.
Messages from a fool are suppressed, like `clatter-ignore-list', but
kept in a separate list that can be toggled with the /fool and /unfool
commands.  Matched case-insensitively."
  :type '(repeat (choice (string :tag "Nick Only")
                         (cons :tag "Nick and Network"
                               (string :tag "Nick")
                               (string :tag "Network"))))
  :group 'clatter)

;; --- Faces ---

(defface clatter-pal
  '((t :foreground "#42be65" :weight bold))
  "Face used to highlight a pal's nick."
  :group 'clatter)

;; --- Membership (pure) ---

(defun clatter--nick-member-p (nick list &optional network)
  "Return non-nil if NICK is in LIST, compared case-insensitively.
Also matches against NETWORK if given."
  (and nick
       (cl-some (lambda (elt)
                  (pcase elt
                    (`(,x . ,in) (and (string-equal-ignore-case nick x)
                                      network (string-equal network in)))
                    (x (string-equal-ignore-case nick x))))
                list)))

(defun clatter-pal-p (nick &optional network)
  "Return non-nil if NICK is a pal.
Also matches against NETWORK if given."
  (clatter--nick-member-p nick clatter-pals network))

(defun clatter-fool-p (nick &optional network)
  "Return non-nil if NICK is a fool.
Also matches against NETWORK if given."
  (clatter--nick-member-p nick clatter-fools network))

(defun clatter-muted-p (sender &optional network)
  "Return non-nil if SENDER's messages should be hidden.
Also matches against NETWORK if given.
True when SENDER is on the ignore list or the fools list."
  (or (clatter-ignored-p sender network)
      (clatter-fool-p sender network)))

;; --- Pure add/remove helpers ---

(defun clatter--nick-list-add (nick list &optional network)
  "Return LIST with NICK added unless already present (case-insensitive)."
  (if (clatter--nick-member-p nick list network)
      list
    (if network
        (cons (cons nick network) list)
      (cons nick (cl-remove-if (lambda (elt)
                                 (pcase elt
                                   (`(,n . ,_) (string-equal-ignore-case nick n))))
                               list)))))

(defun clatter--nick-list-remove (nick list &optional network)
  "Return LIST with NICK removed (case-insensitive)."
  (cl-remove-if (lambda (elt)
                  (pcase elt
                    (`(,x . ,net) (and (string-equal-ignore-case nick x)
                                       (if network (string-equal network net) t)))
                    (x (string-equal-ignore-case nick x))))
                list))

(provide 'clatter-pals)

;;; clatter-pals.el ends here
