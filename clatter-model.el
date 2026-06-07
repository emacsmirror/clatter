;;; clatter-model.el --- Buffer and channel state management -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Glenn Thompson
;; Author: Glenn Thompson
;; License: MIT

;;; Commentary:

;; Channel, nick, and buffer state management for clatter.el.
;; Maps IRC channels/queries to Emacs buffers and tracks nick lists,
;; topics, and channel modes.

;;; Code:

(require 'cl-lib)
(require 'ring)
(require 'clatter-config)
(require 'clatter-protocol)
(require 'clatter-connection)

;; --- Buffer naming ---

(defun clatter-buffer-name (network target)
  "Generate buffer name for NETWORK and TARGET (channel or nick).
Server buffers use TARGET \"*server*\"."
  (format "*clatter:%s/%s*" network target))

(defun clatter-server-buffer-name (network)
  "Generate server buffer name for NETWORK."
  (clatter-buffer-name network "*server*"))

;; --- Channel state (stored as buffer-local variables) ---

(defvar-local clatter--network nil
  "Network ID for this clatter buffer.")

(defvar-local clatter--target nil
  "Target (channel name, nick, or \"*server*\") for this buffer.")

(defvar-local clatter--nick-list nil
  "Hash table of nicks in this channel.
Keys are nicks (downcased), values are prefix chars (@ + etc).")

(defvar-local clatter--nick-accounts nil
  "Hash table mapping nicks (downcased) to account names.
Populated by WHOX (extended WHO) replies.")

(defvar-local clatter--topic nil
  "Current topic for this channel.")

(defvar-local clatter--channel-modes nil
  "Current channel modes string.")

(defvar-local clatter--buffer-type nil
  "Buffer type: server, channel, or query.")

;; --- Buffer registry ---

(defvar clatter--buffer-alist nil
  "Alist mapping (network . target) to buffer objects.")

(defun clatter-get-buffer (network target)
  "Get existing clatter buffer for NETWORK and TARGET, or nil."
  (let ((key (cons network (downcase target))))
    (alist-get key clatter--buffer-alist nil nil #'equal)))

(defun clatter-get-server-buffer (network)
  "Get server buffer for NETWORK."
  (clatter-get-buffer network "*server*"))

(defun clatter-get-or-create-buffer (network target &optional type)
  "Get or create a clatter buffer for NETWORK and TARGET.
TYPE is server, channel, or query (auto-detected if nil)."
  (or (clatter-get-buffer network target)
      (let* ((buf-name (clatter-buffer-name network target))
             (buf (get-buffer-create buf-name))
             (buf-type (or type
                           (cond
                            ((string= target "*server*") 'server)
                            ((clatter-channel-name-p target) 'channel)
                            (t 'query)))))
        (with-current-buffer buf
          (clatter-mode)
          (setq clatter--network network)
          (setq clatter--target target)
          (setq clatter--buffer-type buf-type)
          (when (eq buf-type 'channel)
            (setq clatter--nick-list (make-hash-table :test 'equal))))
        ;; Register
        (let ((key (cons network (downcase target))))
          (setf (alist-get key clatter--buffer-alist nil nil #'equal) buf))
        buf)))

(defun clatter-remove-buffer (network target)
  "Remove buffer for NETWORK and TARGET from registry."
  (let ((key (cons network (downcase target))))
    (setf clatter--buffer-alist
          (cl-remove key clatter--buffer-alist :key #'car :test #'equal))))

(defun clatter-all-buffers (&optional network)
  "Return all clatter buffers, optionally filtered by NETWORK."
  (cl-loop for (key . buf) in clatter--buffer-alist
           when (and (buffer-live-p buf)
                     (or (null network)
                         (string= (car key) network)))
           collect buf))

(defun clatter-channel-buffers (network)
  "Return all channel buffers for NETWORK."
  (cl-loop for buf in (clatter-all-buffers network)
           when (with-current-buffer buf
                  (eq clatter--buffer-type 'channel))
           collect buf))

;; --- Input ring ---

(defvar-local clatter-input-ring nil
  "Ring object for input history.")

(defvar-local clatter-input-ring-index 0
  "Current position in the input history.")

(defun clatter-input-ring-setup ()
  (setq clatter-input-ring
        (if (and (ring-p clatter-input-ring)
                 (= (ring-size clatter-input-ring)
                    clatter-input-ring-size))
            clatter-input-ring
          (make-ring clatter-input-ring-size))))

(defun clatter-input-ring-add (input)
  "Add INPUT to the input history ring."
  (ring-insert clatter-input-ring input)
  (setq clatter-input-ring-index 0))

(defun clatter-input-ring-nth (n)
  "Get element N steps back in the input history ring.
Return nil when the ring is empty or N is out of range."
  (when (and (ring-p clatter-input-ring)
             (not (ring-empty-p clatter-input-ring))
             (>= n 0)
             (< n (ring-length clatter-input-ring)))
    (ring-ref clatter-input-ring n)))

;; --- Nick list management ---

(defun clatter-nick-add (buffer nick &optional prefix)
  "Add NICK with optional PREFIX (@, +, etc) to BUFFER's nick list."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when clatter--nick-list
        (puthash (downcase nick) (or prefix "") clatter--nick-list)))))

(defun clatter-nick-remove (buffer nick)
  "Remove NICK from BUFFER's nick list."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when clatter--nick-list
        (remhash (downcase nick) clatter--nick-list)))))

(defun clatter-nick-rename (buffer old-nick new-nick)
  "Rename OLD-NICK to NEW-NICK in BUFFER's nick list."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when clatter--nick-list
        (let ((prefix (gethash (downcase old-nick) clatter--nick-list)))
          (remhash (downcase old-nick) clatter--nick-list)
          (puthash (downcase new-nick) (or prefix "") clatter--nick-list))))))

(defun clatter-nick-list (buffer)
  "Return sorted list of (nick . prefix) pairs for BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when clatter--nick-list
        (let ((nicks nil))
          (maphash (lambda (k v) (push (cons k v) nicks)) clatter--nick-list)
          (sort nicks (lambda (a b) (string< (car a) (car b)))))))))

(defun clatter-nick-count (buffer)
  "Return number of nicks in BUFFER's nick list."
  (if (and (buffer-live-p buffer)
           (buffer-local-value 'clatter--nick-list buffer))
      (hash-table-count (buffer-local-value 'clatter--nick-list buffer))
    0))

(defun clatter-nick-set-account (buffer nick account)
  "Set ACCOUNT name for NICK in BUFFER's account table."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (unless clatter--nick-accounts
        (setq clatter--nick-accounts (make-hash-table :test 'equal)))
      (puthash (downcase nick) account clatter--nick-accounts))))

(defun clatter-nick-get-account (buffer nick)
  "Get account name for NICK in BUFFER, or nil."
  (when (and (buffer-live-p buffer)
             (buffer-local-value 'clatter--nick-accounts buffer))
    (gethash (downcase nick)
             (buffer-local-value 'clatter--nick-accounts buffer))))

(defconst clatter-whox-token "645"
  "Token used in WHOX queries to identify our replies.")

(defun clatter-send-whox (conn channel)
  "Send WHOX query for CHANNEL on CONN if WHOX is supported.
Uses format: WHO #channel %tcnuhraf,TOKEN to get account names."
  (clatter-send conn (format "WHO %s %%tcnuhraf,%s"
                              channel clatter-whox-token)))

(defun clatter-parse-names (names-str)
  "Parse NAMES reply string into list of (nick . prefix).
Handles prefixes like @nick, +nick, ~nick."
  (mapcar (lambda (entry)
            (let ((entry (string-trim entry)))
              (if (and (> (length entry) 0)
                       (memq (aref entry 0) '(?@ ?+ ?~ ?& ?%)))
                  (cons (substring entry 1) (string (aref entry 0)))
                (cons entry ""))))
          (split-string names-str)))

;; --- Topic management ---

(defun clatter-set-topic (buffer topic)
  "Set TOPIC for BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq clatter--topic topic))))

;; --- Channel modes ---

(defun clatter-set-channel-modes (buffer modes)
  "Set channel MODES for BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq clatter--channel-modes modes))))

;; --- Activity tracking ---

(defvar-local clatter--unread-count 0
  "Number of unread messages in this buffer.")

(defvar-local clatter--has-mention nil
  "Non-nil if there is an unread mention in this buffer.")

(defun clatter-mark-activity (buffer &optional mention)
  "Mark BUFFER as having activity.  If MENTION, also mark as mentioned."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (cl-incf clatter--unread-count)
      (when mention
        (setq clatter--has-mention t)))))

(defun clatter-clear-activity (buffer)
  "Clear activity tracking for BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq clatter--unread-count 0)
      (setq clatter--has-mention nil))))

;; --- Major mode (minimal, UI fills in details) ---

(defvar clatter-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `clatter-mode'.")

(defvar clatter-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Make channel prefixes part of symbol syntax so
    ;; (thing-at-point 'symbol) picks up #channel names.
    (modify-syntax-entry ?# "_" st)
    (modify-syntax-entry ?& "_" st)
    (modify-syntax-entry ?! "_" st)
    (modify-syntax-entry ?+ "_" st)
    ;; Common nick/channel chars
    (modify-syntax-entry ?- "_" st)
    (modify-syntax-entry ?_ "_" st)
    (modify-syntax-entry ?. "_" st)
    (modify-syntax-entry ?\[ "_" st)
    (modify-syntax-entry ?\] "_" st)
    (modify-syntax-entry ?\\ "_" st)
    (modify-syntax-entry ?` "_" st)
    (modify-syntax-entry ?^ "_" st)
    (modify-syntax-entry ?{ "_" st)
    (modify-syntax-entry ?} "_" st)
    (modify-syntax-entry ?| "_" st)
    st)
  "Syntax table for `clatter-mode'.
Makes IRC channel prefixes and nick characters part of symbol syntax.")

(define-derived-mode clatter-mode fundamental-mode "CLatter"
  "Major mode for clatter.el IRC buffers."
  (setq-local scroll-conservatively 101)
  (setq-local scroll-step 1)
  (setq buffer-read-only nil)
  (setq-local word-wrap t)
  (setq-local truncate-lines nil)
  (setq-local wrap-prefix (make-string (1+ clatter-nick-column-width) ?\s))
  ;; Right margin for timestamps (always on first visual line)
  (let ((ts-width (1+ (length (format-time-string clatter-timestamp-format)))))
    (setq-local right-margin-width ts-width))
  ;; Flyspell: only check words in the input area (not read-only messages)
  (when clatter-flyspell-enable
    (require 'flyspell)
    (setq-local flyspell-generic-check-word-predicate
                #'clatter--flyspell-predicate)
    (flyspell-mode 1)))

(defun clatter--flyspell-predicate ()
  "Return non-nil if point is in the input area (not read-only message text)."
  (and clatter--input-marker
       clatter--messages-marker
       (>= (point) (marker-position clatter--input-marker))
       (< (point) (marker-position clatter--messages-marker))))

(provide 'clatter-model)

;;; clatter-model.el ends here
