;;; clatter-completion.el --- Modern completion for clatter -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Glenn Thompson
;; Author: Glenn Thompson <glenn@paren.works>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Completion-at-point for clatter.el buffers.
;; Provides completion for nicks, channels, /commands, and emoji.
;; Integrates with Emacs native completion framework:
;; Corfu, Vertico, Orderless, Cape, etc.

;;; Code:

(require 'cl-lib)
(require 'clatter-config)
(require 'clatter-model)
(require 'clatter-commands)

;; --- Configuration ---

(defcustom clatter-completion-add-colon t
  "Add \": \" after nick completion at the start of input."
  :type 'boolean
  :group 'clatter)

;; --- Nick completion ---

(defun clatter-completion--nicks ()
  "Return list of nicks from current buffer's nick list."
  (when clatter--nick-list
    (let (nicks)
      (maphash (lambda (_nick prefix-and-nick) (push (cdr prefix-and-nick) nicks))
               clatter--nick-list)
      (sort nicks #'string<))))

(defun clatter-completion--nick-capf ()
  "Completion-at-point function for nicks.
Completes nick names from the current channel."
  (when (and clatter--nick-list
             clatter--input-marker
             (>= (point) clatter--input-marker))
    (let* ((end (point))
           (start (save-excursion
                    (skip-chars-backward "^ \t\n")
                    (point)))
           (prefix (buffer-substring-no-properties start end)))
      (when (> (length prefix) 0)
        (let ((nicks (clatter-completion--nicks))
              (at-start (= start (marker-position clatter--input-marker))))
          (list start end nicks
                :exclusive 'no
                :annotation-function
                (lambda (nick)
                  (when clatter--nick-list
                    (let* ((prefix-and-nick (gethash nick clatter--nick-list))
                           (prefix-char (car prefix-and-nick)))
                      (when (and prefix-char (not (string= prefix-char "")))
                        (format " [%s]" prefix-char)))))
                :exit-function
                (lambda (_nick status)
                  (when (eq status 'finished)
                    (when (and at-start clatter-completion-add-colon)
                      (insert ": "))))))))))

;; --- /command completion ---

(defun clatter-completion--commands ()
  "Return list of available /commands."
  (let (cmds)
    (maphash (lambda (name _func) (push (concat "/" name) cmds))
             clatter-command-table)
    (sort cmds #'string<)))

(defun clatter-completion--command-capf ()
  "Completion-at-point function for /commands.
Active when input starts with /."
  (when (and clatter--input-marker
             (>= (point) clatter--input-marker))
    (let* ((input-start (marker-position clatter--input-marker))
           (line-text (buffer-substring-no-properties
                       input-start
                       (save-excursion
                         (goto-char input-start)
                         (line-end-position)))))
      (when (string-prefix-p "/" line-text)
        (let* ((end (point))
               (start (save-excursion
                        (skip-chars-backward "^ \t\n")
                        (max (point) input-start))))
          (when (= start input-start)
            (list start end (clatter-completion--commands)
                  :exclusive 'no
                  :annotation-function
                  (lambda (cmd)
                    (pcase (downcase (substring cmd 1))
                      ("join" " Join a channel")
                      ("part" " Leave a channel")
                      ("msg" " Send private message")
                      ("me" " Send action")
                      ("nick" " Change nick")
                      ("topic" " View/set topic")
                      ("quit" " Disconnect")
                      ("whois" " Query user info")
                      ("kick" " Kick user")
                      ("ban" " Ban user")
                      ("mode" " Set mode")
                      ("suppress" " Suppress message type")
                      ("unsuppress" " Unsuppress message type")
                      (_ ""))))))))))

;; --- Channel completion ---

(defun clatter-completion--channels ()
  "Return list of known channel names across all connections."
  (let (channels)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (derived-mode-p 'clatter-mode)
                   clatter--target
                   (string-match-p "^[#&!+]" clatter--target)
                   (not (member clatter--target channels)))
          (push clatter--target channels))))
    (sort channels #'string<)))

(defun clatter-completion--channel-capf ()
  "Completion-at-point function for channel names.
Active when the word at point starts with #."
  (when (and clatter--input-marker
             (>= (point) clatter--input-marker))
    (let* ((end (point))
           (start (save-excursion
                    (skip-chars-backward "^ \t\n")
                    (point)))
           (prefix (buffer-substring-no-properties start end)))
      (when (string-prefix-p "#" prefix)
        (list start end (clatter-completion--channels)
              :exclusive 'no)))))

;; --- Combined CAPF ---

(defun clatter-completion-at-point ()
  "Main `completion-at-point' function for clatter buffers.
Dispatches to command, channel, or nick completion as appropriate."
  (or (clatter-completion--command-capf)
      (clatter-completion--channel-capf)
      (clatter-completion--nick-capf)))

;; --- Setup ---

(defun clatter-completion-setup ()
  "Set up `completion-at-point' in the current clatter buffer."
  (setq-local completion-ignore-case t)
  (add-hook 'completion-at-point-functions
            #'clatter-completion-at-point nil t))

;; `clatter-completion-setup' is called from the `clatter-mode' body
;; (see clatter-model.el) so that merely loading this file installs no
;; global hooks.

(provide 'clatter-completion)

;;; clatter-completion.el ends here
