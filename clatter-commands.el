;;; clatter-commands.el --- User commands for clatter.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Glenn Thompson
;; Author: Glenn Thompson
;; License: MIT

;;; Commentary:

;; User slash-commands for clatter.el (/join, /part, /msg, /me, etc).
;; Ported from CLatter's core/commands.lisp.

;;; Code:

(require 'cl-lib)
(require 'clatter-config)
(require 'clatter-protocol)
(require 'clatter-connection)
(require 'clatter-model)
(require 'clatter-ui)
(require 'clatter-list)
(require 'clatter-search)

;; --- Command dispatch ---

(defvar clatter-command-table (make-hash-table :test 'equal)
  "Hash table mapping command names to handler functions.")

(defun clatter-defcommand (name func &rest aliases)
  "Register FUNC as handler for command NAME.
ALIASES are alternative names for the same command."
  (puthash (downcase name) func clatter-command-table)
  (dolist (alias aliases)
    (puthash (downcase alias) func clatter-command-table)))

(defun clatter-execute-command (input)
  "Parse and execute INPUT as a /command.
INPUT is the full string including the leading /."
  (let* ((no-slash (substring input 1))
         (space-pos (cl-position ?\s no-slash))
         (cmd (downcase (if space-pos
                            (substring no-slash 0 space-pos)
                          no-slash)))
         (args (if space-pos
                   (string-trim-left (substring no-slash (1+ space-pos)))
                 ""))
         (handler (gethash cmd clatter-command-table)))
    (if handler
        (funcall handler args)
      (clatter-insert-error (current-buffer)
                            (format "Unknown command: /%s" cmd)))))

;; --- Helper to get current connection ---

(defun clatter--current-conn ()
  "Get the connection for the current buffer."
  (when clatter--network
    (clatter-get-connection clatter--network)))

(defun clatter--require-conn ()
  "Get current connection or signal an error message."
  (or (clatter--current-conn)
      (progn
        (clatter-insert-error (current-buffer) "Not connected")
        nil)))

;; --- Commands ---

(defun clatter-cmd-join (args)
  "Handle /join CHANNEL [KEY]."
  (let ((conn (clatter--require-conn)))
    (when conn
      (let* ((parts (split-string args))
             (channel (car parts))
             (key (cadr parts)))
        (if (and channel (> (length channel) 0))
            (clatter-send conn (clatter-irc-join channel key))
          (clatter-insert-error (current-buffer) "Usage: /join #channel [key]"))))))

(defun clatter-cmd-part (args)
  "Handle /part [CHANNEL] [MESSAGE]."
  (let ((conn (clatter--require-conn)))
    (when conn
      (let* ((parts (split-string args " " t))
             (channel (if (and (car parts)
                               (clatter-channel-name-p (car parts)))
                          (car parts)
                        clatter--target))
             (message (if (and (car parts)
                               (clatter-channel-name-p (car parts)))
                          (string-join (cdr parts) " ")
                        args)))
        (clatter-send conn (clatter-irc-part channel
                                             (unless (string-empty-p message)
                                               message)))))))

(defun clatter-cmd-msg (args)
  "Handle /msg TARGET TEXT."
  (let ((conn (clatter--require-conn)))
    (when conn
      (let ((space-pos (cl-position ?\s args)))
        (if space-pos
            (let ((target (substring args 0 space-pos))
                  (text (string-trim-left (substring args (1+ space-pos)))))
              (clatter-send conn (clatter-irc-privmsg target text))
              ;; Open query buffer and show the message
              (let ((buf (clatter-get-or-create-buffer clatter--network target)))
                (clatter-ui-setup-buffer-if-needed buf)
                (clatter-insert-privmsg buf (clatter-connection-nick conn) text conn)))
          (clatter-insert-error (current-buffer) "Usage: /msg target message"))))))

(defun clatter-cmd-me (args)
  "Handle /me ACTION."
  (let ((conn (clatter--require-conn)))
    (when conn
      (when (and clatter--target (not (string= clatter--target "*server*")))
        (let ((ctcp-msg (format "\C-aACTION %s\C-a" args)))
          (clatter-send conn (clatter-irc-privmsg clatter--target ctcp-msg))
          (unless (member "echo-message" (clatter-connection-cap-enabled conn))
            (clatter-insert-action (current-buffer)
                                   (clatter-connection-nick conn)
                                   args conn)))))))

(defun clatter-cmd-nick (args)
  "Handle /nick NEWNICK."
  (let ((conn (clatter--require-conn)))
    (when conn
      (let ((new-nick (car (split-string args))))
        (if (and new-nick (> (length new-nick) 0))
            (clatter-send conn (clatter-irc-nick new-nick))
          (clatter-insert-error (current-buffer) "Usage: /nick newnick"))))))

(defun clatter-cmd-topic (args)
  "Handle /topic [NEWTOPIC]."
  (let ((conn (clatter--require-conn)))
    (when conn
      (when clatter--target
        (if (> (length args) 0)
            (clatter-send conn (clatter-irc-topic clatter--target args))
          (clatter-send conn (clatter-irc-topic clatter--target)))))))

(defun clatter-cmd-kick (args)
  "Handle /kick NICK [REASON]."
  (let ((conn (clatter--require-conn)))
    (when conn
      (when clatter--target
        (let* ((parts (split-string args " " t))
               (nick (car parts))
               (reason (string-join (cdr parts) " ")))
          (if nick
              (clatter-send conn (clatter-irc-kick clatter--target nick
                                                   (unless (string-empty-p reason)
                                                     reason)))
            (clatter-insert-error (current-buffer) "Usage: /kick nick [reason]")))))))

(defun clatter-cmd-mode (args)
  "Handle /mode [MODE] [ARGS]."
  (let ((conn (clatter--require-conn)))
    (when conn
      (let ((target (or clatter--target "")))
        (if (> (length args) 0)
            (clatter-send conn (format "MODE %s %s" target args))
          (clatter-send conn (clatter-irc-mode target)))))))

(defun clatter-cmd-whois (args)
  "Handle /whois NICK."
  (let ((conn (clatter--require-conn)))
    (when conn
      (let ((nick (car (split-string args))))
        (if (and nick (> (length nick) 0))
            (clatter-send conn (clatter-irc-whois nick))
          (clatter-insert-error (current-buffer) "Usage: /whois nick"))))))

(defun clatter-cmd-away (args)
  "Handle /away [MESSAGE].  No message clears away."
  (let ((conn (clatter--require-conn)))
    (when conn
      (clatter-send conn (clatter-irc-away
                          (unless (string-empty-p args) args))))))

(defun clatter-cmd-quit (args)
  "Handle /quit [MESSAGE]."
  (let ((network clatter--network))
    (clatter-disconnect network (if (string-empty-p args) nil args))))

(defun clatter-cmd-raw (args)
  "Handle /raw - send raw IRC line."
  (let ((conn (clatter--require-conn)))
    (when conn
      (if (> (length args) 0)
          (clatter-send conn args)
        (clatter-insert-error (current-buffer) "Usage: /raw IRC-COMMAND")))))

(defun clatter-cmd-query (args)
  "Handle /query NICK - open a query buffer."
  (let* ((nick (car (split-string args)))
         (network clatter--network))
    (if (and nick (> (length nick) 0))
        (let ((buf (clatter-get-or-create-buffer network nick 'query)))
          (clatter-ui-setup-buffer-if-needed buf)
          (switch-to-buffer buf))
      (clatter-insert-error (current-buffer) "Usage: /query nick"))))

(defun clatter-cmd-names (args)
  "Handle /names [CHANNEL]."
  (let ((conn (clatter--require-conn)))
    (when conn
      (let ((channel (if (> (length args) 0)
                         (car (split-string args))
                       clatter--target)))
        (when channel
          (clatter-send conn (clatter-irc-names channel)))))))

(defun clatter-cmd-invite (args)
  "Handle /invite NICK [CHANNEL]."
  (let ((conn (clatter--require-conn)))
    (when conn
      (let* ((parts (split-string args))
             (nick (car parts))
             (channel (or (cadr parts) clatter--target)))
        (if nick
            (clatter-send conn (clatter-irc-invite nick channel))
          (clatter-insert-error (current-buffer)
                                "Usage: /invite nick [channel]"))))))

(defun clatter-cmd-ctcp (args)
  "Handle /ctcp TARGET COMMAND [ARGS]."
  (let ((conn (clatter--require-conn)))
    (when conn
      (let* ((parts (split-string args " " t))
             (target (nth 0 parts))
             (command (upcase (or (nth 1 parts) "")))
             (rest (string-join (nthcdr 2 parts) " ")))
        (if (and target (> (length command) 0))
            (clatter-send conn (clatter-irc-ctcp-request target command
                                                         (unless (string-empty-p rest)
                                                           rest)))
          (clatter-insert-error (current-buffer)
                                "Usage: /ctcp target command [args]"))))))

(defun clatter-cmd-monitor (args)
  "Handle /monitor +nick,-nick,C,L,S."
  (let ((conn (clatter--require-conn)))
    (when conn
      (cond
       ((string-prefix-p "+" args)
        (clatter-send conn (clatter-irc-monitor-add (substring args 1))))
       ((string-prefix-p "-" args)
        (clatter-send conn (clatter-irc-monitor-remove (substring args 1))))
       ((string-equal (upcase args) "C")
        (clatter-send conn (clatter-irc-monitor-clear)))
       ((string-equal (upcase args) "L")
        (clatter-send conn (clatter-irc-monitor-list)))
       ((string-equal (upcase args) "S")
        (clatter-send conn (clatter-irc-monitor-status)))
       (t
        (clatter-insert-error (current-buffer)
                              "Usage: /monitor +nick / -nick / C / L / S"))))))

(defun clatter-cmd-clear (_args)
  "Handle /clear - clear current buffer."
  (let ((inhibit-read-only t))
    (when clatter--prompt-marker
      (delete-region (point-min) clatter--prompt-marker))))

(defun clatter-cmd-buffers (_args)
  "Handle /buffers - list all buffers."
  (let ((lines nil))
    (dolist (buf (clatter-all-buffers))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (push (format "  %s (%s)" (buffer-name) clatter--buffer-type) lines))))
    (clatter-insert-system (current-buffer)
                           (format "Buffers:\n%s"
                                   (string-join (nreverse lines) "\n")))))

;; --- NickServ shortcuts ---

(defun clatter-cmd-ns (args)
  "Handle /ns - shortcut for /msg NickServ."
  (let ((conn (clatter--require-conn)))
    (when conn
      (clatter-send conn (clatter-irc-privmsg "NickServ" args)))))

(defun clatter-cmd-cs (args)
  "Handle /cs - shortcut for /msg ChanServ."
  (let ((conn (clatter--require-conn)))
    (when conn
      (clatter-send conn (clatter-irc-privmsg "ChanServ" args)))))

(defun clatter-cmd-suppress (args)
  "Handle /suppress [TYPE...] - suppress message types.
With no args, show current suppressions.
With 'all', suppress join/part/quit/nick/mode/away.
With 'none', clear all suppressions."
  (let ((types (split-string (string-trim args) " " t)))
    (cond
     ((null types)
      (clatter-insert-system (current-buffer)
                             (format "Suppressed: %s"
                                     (if clatter-suppress-messages
                                         (mapconcat #'symbol-name
                                                    clatter-suppress-messages " ")
                                       "none"))))
     ((string-equal (car types) "all")
      (setq clatter-suppress-messages '(join part quit nick mode away))
      (clatter-insert-system (current-buffer) "Suppressing all join/part/quit/nick/mode/away"))
     ((string-equal (car types) "none")
      (setq clatter-suppress-messages nil)
      (clatter-insert-system (current-buffer) "Showing all messages"))
     (t
      (dolist (type types)
        (let ((sym (intern type)))
          (unless (memq sym clatter-suppress-messages)
            (push sym clatter-suppress-messages))))
      (clatter-insert-system (current-buffer)
                             (format "Suppressed: %s"
                                     (mapconcat #'symbol-name
                                                clatter-suppress-messages " ")))))))

(defun clatter-cmd-unsuppress (args)
  "Handle /unsuppress TYPE... - stop suppressing message types."
  (let ((types (split-string (string-trim args) " " t)))
    (dolist (type types)
      (setq clatter-suppress-messages
            (delq (intern type) clatter-suppress-messages)))
    (clatter-insert-system (current-buffer)
                           (format "Suppressed: %s"
                                   (if clatter-suppress-messages
                                       (mapconcat #'symbol-name
                                                  clatter-suppress-messages " ")
                                     "none")))))

;; --- Register all commands ---

(clatter-defcommand "join" #'clatter-cmd-join "j")
(clatter-defcommand "part" #'clatter-cmd-part "leave")
(clatter-defcommand "msg" #'clatter-cmd-msg)
(clatter-defcommand "me" #'clatter-cmd-me)
(clatter-defcommand "nick" #'clatter-cmd-nick)
(clatter-defcommand "topic" #'clatter-cmd-topic)
(clatter-defcommand "kick" #'clatter-cmd-kick)
(clatter-defcommand "mode" #'clatter-cmd-mode)
(clatter-defcommand "whois" #'clatter-cmd-whois)
(clatter-defcommand "away" #'clatter-cmd-away)
(clatter-defcommand "quit" #'clatter-cmd-quit "q")
(clatter-defcommand "raw" #'clatter-cmd-raw)
(clatter-defcommand "query" #'clatter-cmd-query)
(clatter-defcommand "names" #'clatter-cmd-names "members")
(clatter-defcommand "invite" #'clatter-cmd-invite)
(clatter-defcommand "ctcp" #'clatter-cmd-ctcp)
(clatter-defcommand "monitor" #'clatter-cmd-monitor)
(clatter-defcommand "clear" #'clatter-cmd-clear)
(clatter-defcommand "buffers" #'clatter-cmd-buffers)
(clatter-defcommand "suppress" #'clatter-cmd-suppress)
(clatter-defcommand "unsuppress" #'clatter-cmd-unsuppress)
(clatter-defcommand "ns" #'clatter-cmd-ns)
(clatter-defcommand "cs" #'clatter-cmd-cs)

;; --- Ignore commands ---

(defun clatter-cmd-ignore (args)
  "Ignore a nick or pattern.  Usage: /ignore NICK-OR-PATTERN
Supports glob wildcards (* and ?).
With no argument, shows the current ignore list."
  (let ((pattern (string-trim args)))
    (if (string-empty-p pattern)
        (clatter-insert-system
         (current-buffer)
         (if clatter-ignore-list
             (format "Ignore list: %s" (string-join clatter-ignore-list ", "))
           "Ignore list is empty"))
      (if (member (downcase pattern) (mapcar #'downcase clatter-ignore-list))
          (clatter-insert-system (current-buffer)
                                  (format "%s is already ignored" pattern))
        (push pattern clatter-ignore-list)
        (clatter-insert-system (current-buffer)
                                (format "Now ignoring %s" pattern))))))

(defun clatter-cmd-unignore (args)
  "Remove a nick or pattern from the ignore list.  Usage: /unignore NICK-OR-PATTERN"
  (let ((pattern (string-trim args)))
    (if (string-empty-p pattern)
        (clatter-insert-error (current-buffer) "Usage: /unignore NICK-OR-PATTERN")
      (let ((before (length clatter-ignore-list)))
        (setq clatter-ignore-list
              (cl-remove pattern clatter-ignore-list
                         :test #'string-equal-ignore-case))
        (if (< (length clatter-ignore-list) before)
            (clatter-insert-system (current-buffer)
                                    (format "No longer ignoring %s" pattern))
          (clatter-insert-system (current-buffer)
                                  (format "%s was not in the ignore list" pattern)))))))

(defun clatter-cmd-close (_args)
  "Close (kill) the current clatter buffer.
For channels, sends PART first."
  (let ((network clatter--network)
        (target clatter--target)
        (buf-type clatter--buffer-type)
        (buf (current-buffer)))
    (when (and network target)
      ;; PART from channel if needed
      (when (eq buf-type 'channel)
        (let ((conn (clatter-get-connection network)))
          (when (and conn (clatter-connection-process conn)
                     (process-live-p (clatter-connection-process conn)))
            (clatter-send conn (clatter-irc-part target)))))
      ;; Remove from registry and kill buffer
      (clatter-remove-buffer network target)
      (kill-buffer buf))))

(clatter-defcommand "close" #'clatter-cmd-close "wc")
(clatter-defcommand "ignore" #'clatter-cmd-ignore)
(clatter-defcommand "unignore" #'clatter-cmd-unignore)

(defun clatter-cmd-list (_conn _args)
  "Open the interactive channel list browser."
  (let ((conn (clatter-current-connection)))
    (if conn
        (clatter-list-request conn)
      (message "Not connected."))))

(clatter-defcommand "list" #'clatter-cmd-list)

(defun clatter-cmd-search (args)
  "Search all IRC logs.  Usage: /search QUERY"
  (let ((query (string-trim args)))
    (if (string-empty-p query)
        (call-interactively #'clatter-search)
      (clatter-search query))))

(defun clatter-cmd-searchhere (args)
  "Search current channel logs.  Usage: /searchhere QUERY"
  (let ((query (string-trim args)))
    (if (string-empty-p query)
        (call-interactively #'clatter-search-channel)
      (clatter-search-channel query))))

(clatter-defcommand "search" #'clatter-cmd-search "grep" "find")
(clatter-defcommand "searchhere" #'clatter-cmd-searchhere)

(defun clatter-cmd-reply (args)
  "Reply to the message at point.  Usage: /reply TEXT
Uses +draft/reply tag to thread the response."
  (let* ((text (string-trim args))
         (msgid (get-text-property (point) 'clatter-msgid))
         (target clatter--target)
         (conn (clatter-current-connection)))
    (cond
     ((string-empty-p text)
      (message "Usage: /reply <message>"))
     ((null msgid)
      (message "No message at point to reply to (move cursor to a message first)"))
     ((null conn)
      (message "Not connected"))
     (t
      (clatter-send conn (format "@+draft/reply=%s PRIVMSG %s :%s"
                                 msgid target text))
      ;; Echo if echo-message not enabled
      (unless (member "echo-message" (clatter-connection-cap-enabled conn))
        (clatter-insert-privmsg (current-buffer)
                                (clatter-connection-nick conn)
                                text conn))))))

(clatter-defcommand "reply" #'clatter-cmd-reply "r")

(defun clatter-cmd-react (args)
  "React to the message at point with an emoji.  Usage: /react EMOJI
Uses +draft/react tag via TAGMSG."
  (let* ((emoji (string-trim args))
         (msgid (get-text-property (point) 'clatter-msgid))
         (target clatter--target)
         (conn (clatter-current-connection)))
    (cond
     ((string-empty-p emoji)
      (message "Usage: /react <emoji>"))
     ((null msgid)
      (message "No message at point to react to (move cursor to a message first)"))
     ((null conn)
      (message "Not connected"))
     (t
      (clatter-send conn (format "@+draft/react=%s;+draft/reply=%s TAGMSG %s"
                                 emoji msgid target))))))

(clatter-defcommand "react" #'clatter-cmd-react)

(provide 'clatter-commands)

;;; clatter-commands.el ends here
