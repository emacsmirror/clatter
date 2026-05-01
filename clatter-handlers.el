;;; clatter-handlers.el --- IRC message dispatch and handling -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Glenn Thompson
;; Author: Glenn Thompson
;; License: MIT

;;; Commentary:

;; IRC message dispatch for clatter.el.
;; Routes parsed messages to appropriate handlers.
;; Ported from CLatter's net/irc.lisp irc-handle-message.

;;; Code:

(require 'cl-lib)
(require 'clatter-config)
(require 'clatter-protocol)
(require 'clatter-connection)
(require 'clatter-cap)
(require 'clatter-list)

;; --- Event hooks for UI layer ---

(defvar clatter-privmsg-hook nil
  "Hook for PRIVMSG events.
Called with (CONN SENDER TARGET TEXT SERVER-TIME).")

(defvar clatter-notice-hook nil
  "Hook for NOTICE events.
Called with (CONN SENDER TARGET TEXT).")

(defvar clatter-join-hook nil
  "Hook for JOIN events.
Called with (CONN NICK CHANNEL ACCOUNT REALNAME).")

(defvar clatter-part-hook nil
  "Hook for PART events.
Called with (CONN NICK CHANNEL MESSAGE).")

(defvar clatter-quit-hook nil
  "Hook for QUIT events.
Called with (CONN NICK MESSAGE).")

(defvar clatter-nick-hook nil
  "Hook for NICK change events.
Called with (CONN OLD-NICK NEW-NICK).")

(defvar clatter-irc-mode-hook nil
  "Hook for MODE events.
Called with (CONN TARGET SETTER MODES).")

(defvar clatter-topic-hook nil
  "Hook for TOPIC events.
Called with (CONN CHANNEL NICK TOPIC).")

(defvar clatter-kick-hook nil
  "Hook for KICK events.
Called with (CONN CHANNEL NICK KICKED-NICK REASON).")

(defvar clatter-away-hook nil
  "Hook for AWAY events (IRCv3 away-notify).
Called with (CONN NICK AWAY-MSG).")

(defvar clatter-typing-hook nil
  "Hook for typing indicator events.
Called with (CONN NICK TARGET STATE).")

(defvar clatter-react-hook nil
  "Hook for reaction events (draft/react).
Called with (CONN NICK TARGET EMOJI MSGID).")

(defvar clatter-names-hook nil
  "Hook for NAMES reply events.
Called with (CONN CHANNEL NAMES-STRING).")

(defvar clatter-numeric-hook nil
  "Hook for unhandled numeric replies.
Called with (CONN NUMERIC PARAMS).")

(defvar clatter-whois-hook nil
  "Hook for WHOIS reply events.
Called with (CONN NICK INFO-ALIST).
INFO-ALIST keys: :user :host :realname :server :server-info
:channels :idle :signon :account :secure :away.")

(defvar clatter-motd-hook nil
  "Hook for MOTD display.
Called with (CONN MOTD-LINES) where MOTD-LINES is a list of strings.")

(defvar clatter-system-hook nil
  "Hook for system/log messages.
Called with (CONN TEXT).")

(defvar clatter-ctcp-hook nil
  "Hook for CTCP events (non-ACTION).
Called with (CONN SENDER TARGET COMMAND ARGS).")

(defvar clatter-ctcp-reply-hook nil
  "Hook for CTCP replies received via NOTICE.
Called with (CONN SENDER COMMAND REPLY-TEXT).")

(defvar clatter-action-hook nil
  "Hook for CTCP ACTION (/me) events.
Called with (CONN SENDER TARGET TEXT SERVER-TIME).")

(defvar clatter-welcome-hook nil
  "Hook for 001 RPL_WELCOME.
Called with (CONN NICK).")

(defvar clatter-batch-complete-hook nil
  "Hook for completed batch delivery.
Called with (CONN BATCH-TYPE TARGET MESSAGES).")

;; --- Main Dispatch ---

(defun clatter-dispatch-message (conn msg)
  "Dispatch parsed MSG on CONN to the appropriate handler."
  (let ((command (clatter-message-command msg))
        (params (clatter-message-params msg))
        (prefix (clatter-message-prefix msg))
        (tags (clatter-message-tags msg)))
    ;; Check for labeled-response
    (let* ((parsed-tags (clatter-parse-tags tags))
           (label (cdr (assoc "label" parsed-tags))))
      (when label
        (clatter--handle-labeled-response conn label msg)))
    (pcase command
      ;; --- Core protocol ---
      ("PING"
       (setf (clatter-connection-last-activity conn) (float-time))
       (clatter-send conn (clatter-irc-pong (or (car params) ""))))

      ("PONG"
       (setf (clatter-connection-last-activity conn) (float-time))
       (setf (clatter-connection-ping-sent-time conn) nil))

      ;; --- CAP / SASL ---
      ("CAP"
       (clatter-cap-handle conn params))

      ("AUTHENTICATE"
       (clatter-cap-handle-authenticate conn params))

      ("903"  ; RPL_SASLSUCCESS
       (clatter-cap-handle-sasl-success conn))

      ((or "904" "905")  ; SASL failures
       (clatter-cap-handle-sasl-failure conn params))

      ;; --- Standard Replies (IRCv3) ---
      ("FAIL"
       (let* ((cmd (nth 0 params))
              (code (nth 1 params))
              (description (car (last params)))
              (context (when (> (length params) 3)
                         (string-join (cl-subseq params 2 (1- (length params))) " "))))
         (run-hook-with-args 'clatter-system-hook conn
                             (propertize
                              (if context
                                  (format "FAIL [%s/%s] %s: %s" cmd code context description)
                                (format "FAIL [%s/%s] %s" cmd code description))
                              'face 'clatter-error))))

      ("WARN"
       (let* ((cmd (nth 0 params))
              (code (nth 1 params))
              (description (car (last params)))
              (context (when (> (length params) 3)
                         (string-join (cl-subseq params 2 (1- (length params))) " "))))
         (run-hook-with-args 'clatter-system-hook conn
                             (propertize
                              (if context
                                  (format "WARN [%s/%s] %s: %s" cmd code context description)
                                (format "WARN [%s/%s] %s" cmd code description))
                              'face 'clatter-notice))))

      ("NOTE"
       (let* ((cmd (nth 0 params))
              (code (nth 1 params))
              (description (car (last params)))
              (context (when (> (length params) 3)
                         (string-join (cl-subseq params 2 (1- (length params))) " "))))
         (run-hook-with-args 'clatter-system-hook conn
                             (if context
                                 (format "NOTE [%s/%s] %s: %s" cmd code context description)
                               (format "NOTE [%s/%s] %s" cmd code description)))))

      ;; --- Registration complete ---
      ("001"
       (setf (clatter-connection-state conn) :connected)
       ;; Reset reconnect attempts after 60s of stable connection
       (let ((this-conn conn))
         (run-at-time 60 nil
                      (lambda ()
                        (when (eq (clatter-connection-state this-conn) :connected)
                          (setf (clatter-connection-reconnect-attempts this-conn) 0)))))
       (let ((config (process-get (clatter-connection-process conn) :clatter-config)))
         ;; NickServ identify if not using SASL
         (let ((password (clatter-get-password (clatter-connection-network-id conn))))
           (when (and password
                      (not (eq (clatter-connection-sasl-state conn) :done)))
             (clatter-send conn (clatter-irc-privmsg
                                 "NickServ"
                                 (format "IDENTIFY %s" password)))))
         ;; Autojoin - stagger joins to avoid overwhelming Emacs with
         ;; NAMES/WHOX responses for all channels at once
         (let ((channels (plist-get config :autojoin))
               (delay 0))
           (dolist (ch channels)
             (if (zerop delay)
                 (clatter-send conn (clatter-irc-join ch))
               (let ((channel ch))
                 (run-at-time delay nil
                              (lambda ()
                                (when (eq (clatter-connection-state conn) :connected)
                                  (clatter-send conn (clatter-irc-join channel)))))))
             (setq delay (+ delay 2)))))
       (run-hook-with-args 'clatter-welcome-hook conn (clatter-connection-nick conn))
       (run-hook-with-args 'clatter-connect-hook (clatter-connection-network-id conn))
       (message "[clatter] Connected to %s as %s"
                (clatter-connection-network-id conn)
                (clatter-connection-nick conn)))

      ;; --- 005 RPL_ISUPPORT ---
      ("005"
       (unless (clatter-connection-isupport conn)
         (setf (clatter-connection-isupport conn)
               (make-hash-table :test 'equal)))
       (let ((isup (clatter-connection-isupport conn)))
         (dolist (param (cdr params))  ; skip first (nick) and last (:are supported...)
           (unless (string-prefix-p ":" param)
             (let ((eq-pos (cl-position ?= param)))
               (if eq-pos
                   (puthash (upcase (substring param 0 eq-pos))
                            (substring param (1+ eq-pos))
                            isup)
                 (puthash (upcase param) t isup)))))))

      ;; --- Banned ---
      ("465"
       (setf (clatter-connection-reconnect-enabled conn) nil)
       (let ((reason (or (car (last params)) "No reason given")))
         (message "[clatter] BANNED from %s: %s (auto-reconnect disabled)"
                  (clatter-connection-network-id conn) reason)
         (run-hook-with-args 'clatter-system-hook conn
                             (format "BANNED: %s" reason))))

      ;; --- Nick in use ---
      ("433"
       (let ((new-nick (concat (clatter-connection-nick conn) "_")))
         (setf (clatter-connection-nick conn) new-nick)
         (clatter-send conn (clatter-irc-nick new-nick))
         (run-hook-with-args 'clatter-system-hook conn
                             (format "Nick in use, trying %s" new-nick))))

      ;; --- PRIVMSG ---
      ("PRIVMSG"
       (let* ((target (nth 0 params))
              (raw-text (nth 1 params))
              (text (clatter-strip-irc-formatting raw-text))
              (parsed-prefix (clatter-parse-prefix prefix))
              (sender-nick (clatter-prefix-nick parsed-prefix))
              (server-time (clatter-get-server-time tags))
              (parsed-tags (clatter-parse-tags tags))
              (batch-id (cdr (assoc "batch" parsed-tags)))
              (is-bot (assoc "bot" parsed-tags))
              (msgid (cdr (assoc "msgid" parsed-tags)))
              (reply-to (or (cdr (assoc "+draft/reply" parsed-tags))
                            (cdr (assoc "draft/reply" parsed-tags))))
              ;; STATUSMSG: detect prefix like @#channel or +#channel
              (statusmsg-chars (let ((isup (clatter-connection-isupport conn)))
                                 (when isup (gethash "STATUSMSG" isup))))
              (status-prefix nil))
         ;; Strip STATUSMSG prefix from target
         (when (and statusmsg-chars
                    (> (length target) 1)
                    (cl-find (aref target 0) statusmsg-chars))
           (setq status-prefix (string (aref target 0)))
           (setq target (substring target 1))
           (let ((label (cond ((string= status-prefix "@") "[ops]")
                              ((string= status-prefix "+") "[voiced]")
                              (t (format "[%s]" status-prefix)))))
             (setq text (concat (propertize label 'face 'clatter-notice) " " text))))
         ;; Mark sender as bot if draft/bot tag present
         (when is-bot
           (setq sender-nick (propertize sender-nick 'clatter-bot t)))
         ;; Attach msgid and reply-to as text properties
         (when msgid
           (setq text (propertize text 'clatter-msgid msgid)))
         (when reply-to
           (setq text (propertize text 'clatter-reply-to reply-to)))
         (cond
          ;; CTCP
          ((and (> (length raw-text) 1)
                (= (aref raw-text 0) 1)
                (= (aref raw-text (1- (length raw-text))) 1))
           (clatter--handle-ctcp conn sender-nick target raw-text))
          ;; Batched message
          (batch-id
           (clatter--accumulate-batch conn batch-id sender-nick text server-time))
          ;; Normal message
          (t
           (run-hook-with-args 'clatter-privmsg-hook
                               conn sender-nick target text server-time)))))

      ;; --- NOTICE ---
      ("NOTICE"
       (let* ((target (nth 0 params))
              (raw-text (nth 1 params))
              (text (clatter-strip-irc-formatting raw-text))
              (parsed-prefix (clatter-parse-prefix prefix))
              (sender-nick (or (clatter-prefix-nick parsed-prefix) "*")))
         ;; Check for CTCP reply in NOTICE (don't respond)
         (if (and (> (length raw-text) 1)
                  (= (aref raw-text 0) 1)
                  (= (aref raw-text (1- (length raw-text))) 1))
             (let* ((ctcp-content (substring raw-text 1 (1- (length raw-text))))
                    (space-pos (cl-position ?\s ctcp-content))
                    (ctcp-cmd (if space-pos (substring ctcp-content 0 space-pos) ctcp-content))
                    (ctcp-args (if space-pos (substring ctcp-content (1+ space-pos)) "")))
               (run-hook-with-args 'clatter-ctcp-reply-hook
                                   conn sender-nick ctcp-cmd ctcp-args))
           (run-hook-with-args 'clatter-notice-hook
                               conn sender-nick target text))))

      ;; --- JOIN ---
      ("JOIN"
       (let* ((channel (nth 0 params))
              (account (nth 1 params))
              (realname (nth 2 params))
              (parsed-prefix (clatter-parse-prefix prefix))
              (nick (clatter-prefix-nick parsed-prefix)))
         (run-hook-with-args 'clatter-join-hook
                             conn nick channel account realname)))

      ;; --- PART ---
      ("PART"
       (let* ((channel (nth 0 params))
              (message (nth 1 params))
              (parsed-prefix (clatter-parse-prefix prefix))
              (nick (clatter-prefix-nick parsed-prefix)))
         (run-hook-with-args 'clatter-part-hook conn nick channel message)))

      ;; --- QUIT ---
      ("QUIT"
       (let* ((message (nth 0 params))
              (parsed-prefix (clatter-parse-prefix prefix))
              (nick (clatter-prefix-nick parsed-prefix)))
         (run-hook-with-args 'clatter-quit-hook conn nick message)))

      ;; --- NICK ---
      ("NICK"
       (let* ((new-nick (nth 0 params))
              (parsed-prefix (clatter-parse-prefix prefix))
              (old-nick (clatter-prefix-nick parsed-prefix)))
         (when (string-equal old-nick (clatter-connection-nick conn))
           (setf (clatter-connection-nick conn) new-nick))
         (run-hook-with-args 'clatter-nick-hook conn old-nick new-nick)))

      ;; --- MODE ---
      ("MODE"
       (let* ((target (nth 0 params))
              (modes (cdr params))
              (parsed-prefix (clatter-parse-prefix prefix))
              (setter (or (clatter-prefix-nick parsed-prefix) target)))
         (run-hook-with-args 'clatter-irc-mode-hook conn target setter modes)))

      ;; --- TOPIC ---
      ("TOPIC"
       (let* ((channel (nth 0 params))
              (topic (nth 1 params))
              (parsed-prefix (clatter-parse-prefix prefix))
              (nick (clatter-prefix-nick parsed-prefix)))
         (run-hook-with-args 'clatter-topic-hook conn channel nick topic)))

      ;; --- KICK ---
      ("KICK"
       (let* ((channel (nth 0 params))
              (kicked (nth 1 params))
              (reason (nth 2 params))
              (parsed-prefix (clatter-parse-prefix prefix))
              (nick (clatter-prefix-nick parsed-prefix)))
         (run-hook-with-args 'clatter-kick-hook conn channel nick kicked reason)))

      ;; --- RENAME (draft/channel-rename) ---
      ("RENAME"
       (let* ((old-channel (nth 0 params))
              (new-channel (nth 1 params))
              (reason (nth 2 params))
              (network (clatter-connection-network-id conn))
              (buf (clatter-get-buffer network old-channel)))
         (when buf
           ;; Update buffer-local target
           (with-current-buffer buf
             (setq clatter--target new-channel)
             (rename-buffer (format "*clatter: %s/%s*" network new-channel) t))
           ;; Update buffer registry: remove old, add new
           (clatter-remove-buffer network old-channel)
           (let ((new-key (cons network (downcase new-channel))))
             (setf (alist-get new-key clatter--buffer-alist nil nil #'equal) buf))
           ;; Notify
           (run-hook-with-args 'clatter-system-hook conn
                               (format "Channel %s renamed to %s%s"
                                       old-channel new-channel
                                       (if reason (format " (%s)" reason) ""))))))

      ;; --- AWAY (IRCv3 away-notify) ---
      ("AWAY"
       (let* ((parsed-prefix (clatter-parse-prefix prefix))
              (nick (clatter-prefix-nick parsed-prefix))
              (away-msg (nth 0 params)))
         (run-hook-with-args 'clatter-away-hook conn nick away-msg)))

      ;; --- TAGMSG (typing indicators + reactions) ---
      ("TAGMSG"
       (let* ((parsed-prefix (clatter-parse-prefix prefix))
              (nick (clatter-prefix-nick parsed-prefix))
              (target (nth 0 params))
              (parsed-tags (clatter-parse-tags tags))
              (typing-state (cdr (assoc "+typing" parsed-tags)))
              (react-emoji (or (cdr (assoc "+draft/react" parsed-tags))
                               (cdr (assoc "draft/react" parsed-tags))))
              (react-msgid (or (cdr (assoc "+draft/reply" parsed-tags))
                               (cdr (assoc "draft/reply" parsed-tags)))))
         ;; Typing indicator
         (when (and typing-state
                    (not (string-equal nick (clatter-connection-nick conn))))
           (run-hook-with-args 'clatter-typing-hook conn nick target typing-state))
         ;; Reaction
         (when (and react-emoji react-msgid)
           (run-hook-with-args 'clatter-react-hook
                               conn nick target react-emoji react-msgid))))

      ;; --- BATCH ---
      ("BATCH"
       (clatter--handle-batch conn tags params))

      ;; --- 353 RPL_NAMREPLY ---
      ("353"
       (let ((channel (nth 2 params))
             (names-str (nth 3 params)))
         (when (and channel names-str)
           (run-hook-with-args 'clatter-names-hook conn channel names-str))))

      ;; --- 366 RPL_ENDOFNAMES ---
      ("366"
       ;; Send WHOX to get account names after NAMES completes
       (let ((channel (nth 1 params)))
         (when (and channel (string-prefix-p "#" channel))
           (clatter-send-whox conn channel))))

      ;; --- 354 RPL_WHOSPCRPL (WHOX reply) ---
      ("354"
       ;; Format: 354 <me> <token> <channel> <nick> <user> <host> <realname> <account> <flags>
       (let ((token (nth 1 params))
             (channel (nth 2 params))
             (nick (nth 3 params))
             (account (nth 7 params)))
         (when (and (string= token clatter-whox-token)
                    nick account
                    (not (string= account "0")))  ; "0" means no account
           (let ((buf (clatter-get-buffer
                       (clatter-connection-network-id conn) channel)))
             (when buf
               (clatter-nick-set-account buf nick account))))))

      ;; --- MOTD numerics ---
      ("375"  ; RPL_MOTDSTART
       (setf (clatter-connection--motd-lines conn) nil))
      ("372"  ; RPL_MOTD
       (push (or (car (last params)) "") (clatter-connection--motd-lines conn)))
      ("376"  ; RPL_ENDOFMOTD
       (let ((lines (nreverse (clatter-connection--motd-lines conn))))
         (run-hook-with-args 'clatter-motd-hook conn lines)
         (setf (clatter-connection--motd-lines conn) nil)))
      ("422"  ; ERR_NOMOTD
       (run-hook-with-args 'clatter-system-hook conn "No MOTD"))

      ;; --- WHOIS numerics ---
      ("311"  ; RPL_WHOISUSER
       (let ((nick (nth 1 params))
             (user (nth 2 params))
             (host (nth 3 params))
             (realname (nth 5 params)))
         (setf (clatter-connection--whois-data conn)
               (list :nick nick :user user :host host :realname realname))))
      ("312"  ; RPL_WHOISSERVER
       (let ((data (clatter-connection--whois-data conn)))
         (when data
           (plist-put data :server (nth 2 params))
           (plist-put data :server-info (nth 3 params)))))
      ("313"  ; RPL_WHOISOPERATOR
       (let ((data (clatter-connection--whois-data conn)))
         (when data
           (plist-put data :oper t))))
      ("319"  ; RPL_WHOISCHANNELS
       (let ((data (clatter-connection--whois-data conn)))
         (when data
           (plist-put data :channels (nth 2 params)))))
      ("317"  ; RPL_WHOISIDLE
       (let ((data (clatter-connection--whois-data conn)))
         (when data
           (plist-put data :idle (nth 2 params))
           (plist-put data :signon (nth 3 params)))))
      ("330"  ; RPL_WHOISACCOUNT
       (let ((data (clatter-connection--whois-data conn)))
         (when data
           (plist-put data :account (nth 2 params)))))
      ("671"  ; RPL_WHOISSECURE
       (let ((data (clatter-connection--whois-data conn)))
         (when data
           (plist-put data :secure t))))
      ("301"  ; RPL_AWAY
       (let ((data (clatter-connection--whois-data conn)))
         (when data
           (plist-put data :away (nth 2 params)))))
      ("318"  ; RPL_ENDOFWHOIS
       (let ((data (clatter-connection--whois-data conn)))
         (when data
           (run-hook-with-args 'clatter-whois-hook conn
                               (plist-get data :nick) data)
           (setf (clatter-connection--whois-data conn) nil))))

      ;; --- LIST numerics ---
      ("322"  ; RPL_LIST
       (let ((channel (nth 1 params))
             (users (nth 2 params))
             (topic (or (nth 3 params) "")))
         (clatter-list--on-entry conn channel users topic)))
      ("323"  ; RPL_LISTEND
       (clatter-list--on-end conn))

      ;; --- MONITOR numerics ---
      ("730"  ; RPL_MONONLINE
       (run-hook-with-args 'clatter-system-hook conn
                           (format "Online: %s" (nth 1 params))))
      ("731"  ; RPL_MONOFFLINE
       (run-hook-with-args 'clatter-system-hook conn
                           (format "Offline: %s" (nth 1 params))))
      ("732" nil)  ; RPL_MONLIST (ignore)
      ("733" nil)  ; RPL_ENDOFMONLIST
      ("734"       ; ERR_MONLISTFULL
       (run-hook-with-args 'clatter-system-hook conn
                           (format "Monitor list full (limit: %s)" (nth 1 params))))

      ;; --- Other numerics ---
      (_
       (if (and (> (length command) 0)
                (cl-every #'cl-digit-char-p command))
           (run-hook-with-args 'clatter-numeric-hook conn command params)
         (run-hook-with-args 'clatter-system-hook conn
                             (format "%s %s" command
                                     (string-join params " "))))))))

;; --- CTCP Handling ---

(defun clatter--handle-ctcp (conn sender-nick target raw-text)
  "Handle CTCP request on CONN from SENDER-NICK to TARGET with RAW-TEXT."
  ;; Don't respond to our own CTCP
  (unless (string-equal sender-nick (clatter-connection-nick conn))
  (let* ((ctcp-content (substring raw-text 1 (1- (length raw-text))))
         (space-pos (cl-position ?\s ctcp-content))
         (ctcp-cmd (upcase (if space-pos
                               (substring ctcp-content 0 space-pos)
                             ctcp-content)))
         (ctcp-args (if space-pos
                        (substring ctcp-content (1+ space-pos))
                      "")))
    (pcase ctcp-cmd
      ("ACTION"
       (run-hook-with-args 'clatter-action-hook
                           conn sender-nick target ctcp-args
                           (clatter-get-server-time (clatter-message-tags
                                                     (clatter-parse-line "")))))
      ("VERSION"
       (clatter-send conn (clatter-irc-ctcp-reply
                           sender-nick "VERSION" "clatter.el 0.1.0"))
       (run-hook-with-args 'clatter-system-hook conn
                           (format "CTCP VERSION from %s" sender-nick)))
      ("PING"
       (clatter-send conn (clatter-irc-ctcp-reply sender-nick "PING" ctcp-args))
       (run-hook-with-args 'clatter-system-hook conn
                           (format "CTCP PING from %s" sender-nick)))
      ("TIME"
       (clatter-send conn (clatter-irc-ctcp-reply
                           sender-nick "TIME" (format-time-string "%F %T")))
       (run-hook-with-args 'clatter-system-hook conn
                           (format "CTCP TIME from %s" sender-nick)))
      (_
       (run-hook-with-args 'clatter-ctcp-hook
                           conn sender-nick target ctcp-cmd ctcp-args))))))

;; --- Batch Handling ---

(defun clatter--accumulate-batch (conn batch-id sender text server-time)
  "Accumulate a message into active batch BATCH-ID on CONN."
  (let ((batch (gethash batch-id (clatter-connection-active-batches conn))))
    (when batch
      (push (list :sender sender :text text :time server-time)
            (plist-get batch :messages)))))

(defun clatter--handle-batch (conn _tags params)
  "Handle BATCH command on CONN with PARAMS."
  (let* ((ref (nth 0 params))
         (starting (and (> (length ref) 0) (= (aref ref 0) ?+)))
         (batch-id (substring ref 1)))
    (if starting
        ;; Start batch
        (puthash batch-id
                 (list :type (nth 1 params) :target (nth 2 params) :messages nil)
                 (clatter-connection-active-batches conn))
      ;; End batch
      (let ((batch (gethash batch-id (clatter-connection-active-batches conn))))
        (when batch
          (let ((messages (nreverse (plist-get batch :messages))))
            (run-hook-with-args 'clatter-batch-complete-hook conn
                                (plist-get batch :type)
                                (plist-get batch :target)
                                messages))
          (remhash batch-id (clatter-connection-active-batches conn)))))))

;; --- Labeled Response ---

(defun clatter--handle-labeled-response (conn label _msg)
  "Handle labeled response on CONN for LABEL."
  (let ((callback (gethash label (clatter-connection-pending-labels conn))))
    (when callback
      (funcall callback _msg)
      (remhash label (clatter-connection-pending-labels conn)))))

(provide 'clatter-handlers)

;;; clatter-handlers.el ends here
