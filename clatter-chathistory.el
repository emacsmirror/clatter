;;; clatter-chathistory.el --- IRCv3 CHATHISTORY support -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Glenn Thompson
;; Author: Glenn Thompson <glenn@paren.works>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; IRCv3 CHATHISTORY extension for clatter.el.
;; Fetches message backlog on join/reconnect.
;; Works with servers/bouncers that support:
;;   chathistory or draft/chathistory
;; Requires: server-time, batch, message-tags capabilities.

;;; Code:

(require 'cl-lib)
(require 'tabulated-list)
(require 'clatter-config)
(require 'clatter-connection)
(require 'clatter-model)
(require 'clatter-protocol)
(require 'clatter-ui)

;; --- Configuration ---

(defcustom clatter-chathistory-enabled t
  "Enable automatic chathistory fetch on join."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-chathistory-limit 50
  "Maximum number of messages to fetch per target."
  :type 'integer
  :group 'clatter)

(defcustom clatter-chathistory-on-join t
  "Fetch chathistory automatically when joining a channel."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-chathistory-on-reconnect t
  "Fetch chathistory automatically on reconnection."
  :type 'boolean
  :group 'clatter)

;; Sentinel bounds for CHATHISTORY TARGETS.  Both bounds must be timestamps
;; (the `*' wildcard is only valid for LATEST), and they carry BETWEEN
;; semantics: the newest bound comes first so that a truncating limit keeps
;; the most recently active targets.  The oldest bound must not be the zero
;; time (0001-01-01T00:00:00.000Z): soju parses bounds into Go time values
;; and rejects the zero value with "Invalid first bound".
(defconst clatter-chathistory--newest-bound "9999-12-31T23:59:59.999Z"
  "Newest timestamp sentinel for CHATHISTORY TARGETS requests.")
(defconst clatter-chathistory--oldest-bound "1970-01-01T00:00:00.000Z"
  "Oldest timestamp sentinel for CHATHISTORY TARGETS requests.")

;; --- State ---

(defvar-local clatter-chathistory--last-timestamp nil
  "Last known message timestamp for this buffer.
Used to request messages since this time on reconnect.")

(defvar-local clatter-chathistory--earliest-timestamp nil
  "Earliest known message timestamp for this buffer.
Used by `clatter-chathistory-more' to page further back.")

;; --- Capability check ---

(defun clatter-chathistory--available-p (conn)
  "Return non-nil if CONN supports chathistory."
  (let ((caps (clatter-connection-cap-enabled conn)))
    (or (cl-member "chathistory" caps :test #'string-equal)
        (cl-member "draft/chathistory" caps :test #'string-equal))))

(defun clatter-chathistory--cap-name (conn)
  "Return the chathistory capability name supported by CONN."
  (let ((caps (clatter-connection-cap-enabled conn)))
    (cond
     ((cl-member "chathistory" caps :test #'string-equal) "chathistory")
     ((cl-member "draft/chathistory" caps :test #'string-equal) "draft/chathistory")
     (t nil))))

;; --- Request commands ---

(defun clatter-chathistory--format-time (time)
  "Format TIME as an IRCv3 server-time string (ISO 8601).
TIME is an Emacs time value."
  (format-time-string "%Y-%m-%dT%H:%M:%S.000Z" time t))

(defun clatter-chathistory-fetch-latest (conn target &optional limit)
  "Fetch the latest LIMIT messages for TARGET via CONN."
  (when (clatter-chathistory--available-p conn)
    (let ((n (or limit clatter-chathistory-limit)))
      (clatter-send conn
                    (format "CHATHISTORY LATEST %s * %d" target n)))))

(defun clatter-chathistory-fetch-before (conn target timestamp &optional limit)
  "Fetch LIMIT messages before TIMESTAMP for TARGET via CONN."
  (when (clatter-chathistory--available-p conn)
    (let ((n (or limit clatter-chathistory-limit))
          (ts (clatter-chathistory--format-time timestamp)))
      (clatter-send conn
                    (format "CHATHISTORY BEFORE %s timestamp=%s %d"
                            target ts n)))))

(defun clatter-chathistory-fetch-since (conn target timestamp &optional limit)
  "Fetch messages since TIMESTAMP for TARGET via CONN.
Used on reconnect to fill in gaps."
  (when (clatter-chathistory--available-p conn)
    (let ((n (or limit clatter-chathistory-limit))
          (ts (clatter-chathistory--format-time timestamp)))
      (clatter-send conn
                    (format "CHATHISTORY AFTER %s timestamp=%s %d"
                            target ts n)))))

;; --- TARGETS (DM discovery) ---

(defun clatter-chathistory-fetch-targets (conn &optional limit)
  "Request CHATHISTORY TARGETS on CONN to discover DM targets with history.
LIMIT defaults to `clatter-chathistory-limit'.  The server responds with a
batch of type `chathistory-targets' listing channels and DM partners that
have history, each with the timestamp of their latest message.

Both bounds MUST be timestamps; the `*' wildcard is not valid for TARGETS
(unlike LATEST), and servers reject it with CHATHISTORY/INVALID_PARAMS.
The newest sentinel bound is sent first so that servers truncating the
list at LIMIT keep the most recently active targets."
  (when (clatter-chathistory--available-p conn)
    (let ((n (or limit clatter-chathistory-limit)))
      (clatter-send conn
                    (format
                     "CHATHISTORY TARGETS timestamp=%s timestamp=%s %d"
                     clatter-chathistory--newest-bound
                     clatter-chathistory--oldest-bound n)))))

(defun clatter-chathistory--on-welcome (conn _nick)
  "On welcome (001), request CHATHISTORY TARGETS to discover missed DMs.
This is essential when connecting through a bouncer: channels get their
history fetched on JOIN, but DM buffers are only created on demand.
Without TARGETS, DMs received while offline are never fetched."
  (when (and clatter-chathistory-enabled
             (clatter-chathistory--available-p conn))
    (clatter-chathistory-fetch-targets conn)))

(defun clatter-chathistory--targets-batch-p (batch-type)
  "Return non-nil if BATCH-TYPE names a chathistory-targets batch.
Servers send either `chathistory-targets' or the draft name
`draft/chathistory-targets'."
  (and (stringp batch-type)
       (member batch-type '("chathistory-targets" "draft/chathistory-targets"))
       t))

(defun clatter-chathistory--on-targets-batch (conn batch-type _target messages)
  "Process a completed chathistory-targets batch.
BATCH-TYPE must name a targets batch; other batches are ignored so that
ordinary history playback never triggers another round of fetches.
MESSAGES is a list of plists with :target and :time keys.  For each
target that is a DM (not a channel), fetch latest history if we don't
already have a buffer for it, or fetch since the last known timestamp
if we do."
  (when (and clatter-chathistory-enabled
             (clatter-chathistory--targets-batch-p batch-type)
             (clatter-chathistory--available-p conn))
    (let ((network (clatter-connection-network-id conn))
          (my-nick (clatter-connection-nick conn)))
      (dolist (entry messages)
        (let ((target (plist-get entry :target)))
          (when (and target
                     (not (clatter-channel-name-p target))
                     (not (string-equal target my-nick)))
            (let ((buf (clatter-get-buffer network target)))
              (if (and buf
                       (buffer-local-value 'clatter-chathistory--last-timestamp buf))
                  ;; Existing buffer: fetch since last known message
                  (when clatter-chathistory-on-reconnect
                    (clatter-chathistory-fetch-since
                     conn target
                     (buffer-local-value 'clatter-chathistory--last-timestamp buf)))
                ;; New DM target: fetch latest
                (clatter-chathistory-fetch-latest conn target)))))))))

;; --- TARGETS browser ---

(defconst clatter-chathistory--targets-buffer-name
  "*clatter-chathistory-targets*"
  "Name of the chathistory targets browser buffer.")

(defvar-local clatter-chathistory--target-results nil
  "Hash table mapping network names to their CHATHISTORY TARGETS entries.")

(defun clatter-chathistory--target-connections ()
  "Return connected chathistory-capable connections, sorted by network."
  (let (connections)
    (maphash
     (lambda (_network conn)
       (when (and (eq (clatter-connection-state conn) :connected)
                  (clatter-chathistory--available-p conn))
         (push conn connections)))
     clatter-connections)
    (sort connections
          (lambda (a b)
            (string-lessp (clatter-connection-network-id a)
                          (clatter-connection-network-id b))))))

(defun clatter-chathistory--target-entries ()
  "Return tabulated tree entries for the targets browser."
  (let (entries)
    (dolist (conn (clatter-chathistory--target-connections))
      (let* ((network (clatter-connection-network-id conn))
             (targets (gethash network clatter-chathistory--target-results)))
        (push
         (list (list network)
               (vector
                (cons network
                      (list 'face 'bold
                            'mouse-face 'highlight
                            'help-echo (format "Visit %s server buffer" network)
                            'action #'clatter-chathistory-targets-visit))))
         entries)
        (while targets
          (let* ((target (plist-get (car targets) :target))
                 (branch (if (cdr targets) "  ├─ " "  └─ ")))
            (when target
              (push
               (list (cons network target)
                     (vector
                      (concat
                       branch
                       (propertize
                        target
                        'button '(t)
                        'category 'default-button
                        'follow-link t
                        'face 'link
                        'mouse-face 'highlight
                        'help-echo (format "Visit %s on %s" target network)
                        'action #'clatter-chathistory-targets-visit))))
               entries)))
          (setq targets (cdr targets)))))
    (nreverse entries)))

(defun clatter-chathistory-targets--visit (display &optional quit)
  "Visit the target at point using DISPLAY.
When QUIT is non-nil, bury the targets browser first."
  (let* ((id (tabulated-list-get-id))
         (network (car-safe id))
         (history-target (cdr-safe id))
         (target (or history-target "*server*"))
         (conn (and network (clatter-get-connection network))))
    (if (not conn)
        (message "No connected clatter server at point")
      (let* ((existing (clatter-get-buffer network target))
             (buffer (clatter-get-or-create-buffer
                      network target (unless history-target 'server))))
        (clatter-ui-setup-buffer-if-needed buffer)
        (when (and history-target (not existing))
          (clatter-chathistory-fetch-latest conn target))
        (when quit
          (quit-window))
        (funcall display buffer)))))

(defun clatter-chathistory-targets-visit (&optional _button)
  "Visit the server or target buffer at point."
  (interactive)
  (clatter-chathistory-targets--visit #'switch-to-buffer))

(defun clatter-chathistory-targets-visit-other-window ()
  "Visit the server or target buffer at point in another window."
  (interactive)
  (clatter-chathistory-targets--visit #'switch-to-buffer-other-window))

(defun clatter-chathistory-targets-visit-quit ()
  "Visit the server or target buffer at point and bury the browser."
  (interactive)
  (clatter-chathistory-targets--visit #'switch-to-buffer t))

(defun clatter-chathistory-targets-visit-mouse (event)
  "Visit the server or target clicked at EVENT."
  (interactive "e")
  (let* ((position (event-start event))
         (window (posn-window position)))
    (when (window-live-p window)
      (with-current-buffer (window-buffer window)
        (goto-char (posn-point position))
        (clatter-chathistory-targets-visit)))))

(defvar clatter-chathistory-targets-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'clatter-chathistory-targets-visit)
    (define-key map (kbd "o") #'clatter-chathistory-targets-visit-other-window)
    (define-key map (kbd "a") #'clatter-chathistory-targets-visit-quit)
    (define-key map [mouse-2] #'clatter-chathistory-targets-visit-mouse)
    map)
  "Keymap for `clatter-chathistory-targets-mode'.")

(defun clatter-chathistory--record-targets
    (conn batch-type _target messages)
  "Record a completed targets BATCH-TYPE from CONN containing MESSAGES."
  (when (clatter-chathistory--targets-batch-p batch-type)
    (when-let* ((buffer (get-buffer clatter-chathistory--targets-buffer-name)))
      (with-current-buffer buffer
        (when (derived-mode-p 'clatter-chathistory-targets-mode)
          (puthash (clatter-connection-network-id conn)
                   messages clatter-chathistory--target-results)
          (tabulated-list-print t))))))

(defun clatter-chathistory-targets--stop ()
  "Stop collecting target batches when the browser closes."
  (remove-hook 'clatter-batch-complete-hook
               #'clatter-chathistory--record-targets))

(define-derived-mode clatter-chathistory-targets-mode
  tabulated-list-mode "Clatter Targets"
  "Major mode for browsing every server's available history targets."
  (setq tabulated-list-format [("Server / target" 0 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-entries #'clatter-chathistory--target-entries)
  (setq revert-buffer-function #'clatter-chathistory-targets-refresh)
  (setq-local clatter-chathistory--target-results
              (make-hash-table :test 'equal))
  (add-hook 'clatter-batch-complete-hook
            #'clatter-chathistory--record-targets)
  (add-hook 'change-major-mode-hook
            #'clatter-chathistory-targets--stop nil t)
  (add-hook 'kill-buffer-hook
            #'clatter-chathistory-targets--stop nil t)
  (tabulated-list-init-header))

(defun clatter-chathistory-targets-refresh (&rest _ignore)
  "Clear and re-query all connected servers for available history targets."
  (interactive)
  (setq-local clatter-chathistory--target-results
              (make-hash-table :test 'equal))
  (let ((connections (clatter-chathistory--target-connections)))
    (dolist (conn connections)
      (clatter-chathistory-fetch-targets conn))
    (tabulated-list-print t)
    (message "Requested history targets from %d server%s"
             (length connections)
             (if (= (length connections) 1) "" "s"))))

;;;###autoload
(defun clatter-chathistory-targets ()
  "Browse available history targets grouped under their servers.
Re-query every connected chathistory-capable server with `g'."
  (interactive)
  (let ((buffer (get-buffer-create clatter-chathistory--targets-buffer-name)))
    (with-current-buffer buffer
      (clatter-chathistory-targets-mode)
      (clatter-chathistory-targets-refresh)
      (goto-char (point-min)))
    (display-buffer buffer)))

;; --- Automatic fetch hooks ---

(defun clatter-chathistory--on-join (conn sender channel _account _realname)
  "Fetch chathistory when we join CHANNEL on CONN.
SENDER is who joined; we only fetch if it's our own nick."
  (when (and clatter-chathistory-enabled
             clatter-chathistory-on-join
             (string-equal (clatter-prefix-nick sender) (clatter-connection-nick conn))
             (clatter-chathistory--available-p conn))
    (let* ((target channel)
           (network (clatter-connection-network-id conn))
           (buf (clatter-get-buffer network target)))
      (if (and buf
               (buffer-local-value 'clatter-chathistory--last-timestamp buf))
          ;; Reconnect: fetch since last known message
          (when clatter-chathistory-on-reconnect
            (clatter-chathistory-fetch-since
             conn target
             (buffer-local-value 'clatter-chathistory--last-timestamp buf)))
        ;; First join: fetch latest
        (clatter-chathistory-fetch-latest conn target)))))

(defun clatter-chathistory--track-timestamp (_conn _sender _target _text server-time)
  "Track the latest message timestamp for chathistory gaps.
SERVER-TIME is the IRCv3 server-time value.  Also seed the earliest
timestamp when unset, so `clatter-chathistory-more' has a cursor."
  (when server-time
    (setq-local clatter-chathistory--last-timestamp server-time)
    (when (or (null clatter-chathistory--earliest-timestamp)
              (time-less-p server-time clatter-chathistory--earliest-timestamp))
      (setq-local clatter-chathistory--earliest-timestamp server-time))))

(defun clatter-chathistory--track-batch-timestamp (conn _batch-type target messages)
  "Track the newest timestamp in completed history MESSAGES for TARGET.
Only channel/query batches carry a string TARGET; target-less batches
such as `soju.im/bouncer-networks' are ignored (they have no buffer to
track a cursor for)."
  (when (stringp target)
    (let ((buf (clatter-get-buffer (clatter-connection-network-id conn) target))
          latest earliest)
      (dolist (message messages)
        (let ((timestamp (plist-get message :time)))
          (when (and timestamp
                     (or (null latest) (time-less-p latest timestamp)))
            (setq latest timestamp))
          (when (and timestamp
                     (or (null earliest) (time-less-p timestamp earliest)))
            (setq earliest timestamp))))
      (when (and buf latest)
        (with-current-buffer buf
          (when (or (null clatter-chathistory--last-timestamp)
                    (time-less-p clatter-chathistory--last-timestamp latest))
            (setq-local clatter-chathistory--last-timestamp latest))))
      (when (and buf earliest)
        (with-current-buffer buf
          (when (or (null clatter-chathistory--earliest-timestamp)
                    (time-less-p earliest clatter-chathistory--earliest-timestamp))
            (setq-local clatter-chathistory--earliest-timestamp earliest)))))))

;; --- Interactive commands ---

(defun clatter-chathistory-request (&optional count)
  "Manually request chathistory for the current buffer.
COUNT defaults to `clatter-chathistory-limit'."
  (interactive "P")
  (let ((conn (clatter-get-connection clatter--network))
        (target clatter--target)
        (n (or count clatter-chathistory-limit)))
    (if (and conn target)
        (if (clatter-chathistory--available-p conn)
            (progn
              (clatter-chathistory-fetch-latest conn target n)
              (message "Requested %d messages for %s" n target))
          (message "Server does not support chathistory"))
      (message "Not in a clatter buffer"))))

(defun clatter-chathistory-more (&optional count)
  "Fetch COUNT older messages (before the earliest in this buffer)."
  (interactive "P")
  (let ((conn (clatter-get-connection clatter--network))
        (target clatter--target)
        (n (or count clatter-chathistory-limit)))
    (if (and conn target)
        (if (clatter-chathistory--available-p conn)
            (let ((earliest clatter-chathistory--earliest-timestamp))
              (if earliest
                  (progn
                    ;; The reply is older than everything on screen: tell the
                    ;; renderer to place it at the buffer's oldest end.
                    (setq-local clatter--backlog-page-pending t)
                    (clatter-chathistory-fetch-before conn target earliest n)
                    (message "Requested %d older messages for %s" n target))
                (clatter-chathistory-fetch-latest conn target n)))
          (message "Server does not support chathistory"))
      (message "Not in a clatter buffer"))))

;; --- Enable/disable ---

(defun clatter-chathistory-enable ()
  "Enable chathistory hooks."
  (interactive)
  (add-hook 'clatter-join-hook #'clatter-chathistory--on-join)
  (add-hook 'clatter-privmsg-hook #'clatter-chathistory--track-timestamp)
  (add-hook 'clatter-batch-complete-hook
            #'clatter-chathistory--track-batch-timestamp)
  (add-hook 'clatter-welcome-hook #'clatter-chathistory--on-welcome)
  (add-hook 'clatter-batch-complete-hook
            #'clatter-chathistory--on-targets-batch)
  (when (called-interactively-p 'interactive)
    (message "[clatter-chathistory] Enabled")))

(defun clatter-chathistory-disable ()
  "Disable chathistory hooks."
  (interactive)
  (remove-hook 'clatter-join-hook #'clatter-chathistory--on-join)
  (remove-hook 'clatter-privmsg-hook #'clatter-chathistory--track-timestamp)
  (remove-hook 'clatter-batch-complete-hook
               #'clatter-chathistory--track-batch-timestamp)
  (remove-hook 'clatter-welcome-hook #'clatter-chathistory--on-welcome)
  (remove-hook 'clatter-batch-complete-hook
               #'clatter-chathistory--on-targets-batch)
  (message "[clatter-chathistory] Disabled"))

;; Enabled by `clatter-setup' when `clatter-chathistory-enabled' is
;; non-nil, so that merely loading this file has no side effects.

(provide 'clatter-chathistory)

;;; clatter-chathistory.el ends here
