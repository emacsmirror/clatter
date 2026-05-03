;;; clatter-connection.el --- IRC network connection management -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Glenn Thompson
;; Author: Glenn Thompson
;; License: MIT

;;; Commentary:

;; Network connection management for clatter.el.
;; Handles TCP/TLS connections, async I/O via process filters,
;; reconnection with exponential backoff, and health monitoring.

;;; Code:

(require 'cl-lib)
(require 'clatter-config)
(require 'clatter-protocol)

;; --- Connection Structure ---

(cl-defstruct (clatter-connection (:constructor clatter-connection--create))
  "An IRC network connection."
  network-id           ; string: network name from config
  process              ; network process
  state                ; :disconnected :connecting :registering :connected
  nick                 ; current nickname
  ;; SASL
  sasl-state           ; nil :requested :authenticating :done
  cap-negotiating      ; t during CAP negotiation
  cap-enabled          ; list of enabled capability strings
  ;; IRCv3 batch/label tracking
  active-batches       ; hash-table: batch-id -> plist
  pending-labels       ; hash-table: label -> callback
  label-counter        ; integer
  ;; Reconnection
  reconnect-enabled    ; t to auto-reconnect
  reconnect-attempts   ; integer
  reconnect-timer      ; timer object
  desired-nick         ; string: the configured nick we want to reclaim
  nick-reclaim-timer   ; timer for periodic nick reclaim attempts
  ;; Health monitoring
  last-activity        ; float-time of last activity
  ping-sent-time       ; float-time of last health ping, nil if no pending
  health-timer         ; timer for periodic pings
  ;; Receive buffer (partial line accumulation)
  recv-buffer          ; string: incomplete data from last read
  ;; ISUPPORT (005) parameters
  isupport             ; hash-table: param -> value (from RPL_ISUPPORT)
  ;; MOTD/WHOIS accumulation
  -motd-lines          ; list: accumulating MOTD lines
  -whois-data)         ; plist: accumulating WHOIS data

;; --- Active connections registry ---

(defvar clatter-connections (make-hash-table :test 'equal)
  "Hash table mapping network-id to `clatter-connection'.")

(defun clatter-get-connection (network-id)
  "Get the connection for NETWORK-ID."
  (gethash network-id clatter-connections))

;; --- Debug Logging ---

(defun clatter--debug (format-string &rest args)
  "Log to *clatter-debug* buffer if `clatter-log-raw-protocol' is non-nil."
  (when clatter-log-raw-protocol
    (with-current-buffer (get-buffer-create "*clatter-debug*")
      (goto-char (point-max))
      (insert (format-time-string "[%H:%M:%S] ")
              (apply #'format format-string args)
              "\n"))))

(defvar clatter-watchdog-log
  (expand-file-name "clatter/watchdog.log" user-emacs-directory)
  "File path for connection watchdog log.
This log persists across Emacs restarts to help diagnose lockups.")

(defun clatter--watchdog (format-string &rest args)
  "Write a timestamped line to the watchdog log file.
Always writes regardless of `clatter-log-raw-protocol' setting."
  (let ((dir (file-name-directory clatter-watchdog-log))
        (line (concat (format-time-string "[%F %T] ")
                      (apply #'format format-string args)
                      "\n")))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (write-region line nil clatter-watchdog-log 'append 'silent)))

;; --- Send ---

(defun clatter-send (conn line)
  "Send LINE to the IRC server via CONN.
Appends CR-LF automatically."
  (let ((proc (clatter-connection-process conn)))
    (when (and proc (process-live-p proc)
               (memq (process-status proc) '(open run)))
      (clatter--debug ">> %s" line)
      (run-hook-with-args 'clatter-rawlog-outgoing-hook
                          (clatter-connection-network-id conn) line)
      (process-send-string proc (concat line "\r\n")))))

;; --- Receive (Process Filter) ---

(defun clatter--process-filter (proc string)
  "Process filter for incoming IRC data from PROC.
Accumulates partial lines and dispatches complete ones."
  (let* ((network-id (process-get proc :clatter-network-id))
         (conn (clatter-get-connection network-id)))
    (when conn
      (setf (clatter-connection-last-activity conn) (float-time))
      (let ((data (concat (string-to-multibyte
                          (or (clatter-connection-recv-buffer conn) ""))
                         (string-to-multibyte string))))
        ;; Process complete lines (terminated by \r\n or \n)
        (while (string-match "\r?\n" data)
          (let ((line (substring data 0 (match-beginning 0))))
            (setq data (substring data (match-end 0)))
            (when (> (length line) 0)
              (clatter--debug "<< %s" line)
              (run-hook-with-args 'clatter-rawlog-incoming-hook
                                  network-id line)
              (clatter--handle-line conn line))))
        ;; Save any remaining partial data
        (setf (clatter-connection-recv-buffer conn) data)))))

(defun clatter--handle-line (conn line)
  "Parse and handle a single IRC LINE on CONN.
This is the main entry point from the process filter into the handler layer."
  (let ((msg (clatter-parse-line line)))
    (when msg
      ;; Dispatch to handlers (defined in clatter-handlers.el)
      (clatter-dispatch-message conn msg))))

;; Forward declaration - implemented in clatter-handlers.el
(declare-function clatter-dispatch-message "clatter-handlers")

;; --- Process Sentinel (disconnect handling) ---

(defun clatter--process-sentinel (proc event)
  "Process sentinel for PROC handling disconnect EVENT."
  (let* ((network-id (process-get proc :clatter-network-id))
         (conn (clatter-get-connection network-id)))
    (when conn
      (clatter--debug "sentinel: %s %s" network-id (string-trim event))
      (clatter--watchdog "DISCONNECT %s event=%s" network-id (string-trim event))
      (message "[clatter] Disconnected from %s: %s" network-id (string-trim event))
      (setf (clatter-connection-state conn) :disconnected)
      (setf (clatter-connection-process conn) nil)
      ;; Cancel health timer
      (when (clatter-connection-health-timer conn)
        (cancel-timer (clatter-connection-health-timer conn))
        (setf (clatter-connection-health-timer conn) nil))
      ;; Notify UI
      (run-hook-with-args 'clatter-disconnect-hook network-id event)
      ;; Auto-reconnect
      (when (clatter-connection-reconnect-enabled conn)
        (clatter--watchdog "RECONNECT-SCHEDULE %s" network-id)
        (clatter--schedule-reconnect conn)))))

;; --- Connect ---

(defun clatter-connect (network-id &rest args)
  "Connect to IRC network NETWORK-ID.
ARGS are keyword arguments that override `clatter-networks' config:
  :server :port :tls :nick :username :realname :sasl :client-cert
  :autojoin :password"
  (interactive
   (list (completing-read "Network: "
                          (mapcar #'car clatter-networks))))
  ;; Merge args with stored config
  (let* ((stored (cdr (assoc network-id clatter-networks #'equal)))
         (config (if args (append args stored) stored))
         (server (plist-get config :server))
         (port (or (plist-get config :port) clatter-default-port))
         (use-tls (if (plist-member config :tls)
                      (plist-get config :tls)
                    clatter-default-tls))
         (nick (or (plist-get config :nick) clatter-default-nick)))
    (unless server
      (error "No server specified for network %s" network-id))
    (unless nick
      (error "No nick specified for network %s" network-id))

    ;; STS policy enforcement: force TLS if cached policy exists
    (when (and (fboundp 'clatter-sts-lookup)
               (not use-tls))
      (let ((sts-policy (clatter-sts-lookup server)))
        (when sts-policy
          (setq use-tls t)
          (setq port (plist-get sts-policy :port))
          (clatter--debug "STS: enforcing TLS on port %d for %s" port server))))

    ;; Create or reuse connection struct
    (let ((conn (or (clatter-get-connection network-id)
                    (let ((new-conn (clatter-connection--create
                                     :network-id network-id
                                     :state :disconnected
                                     :nick nick
                                     :reconnect-enabled t
                                     :reconnect-attempts 0
                                     :active-batches (make-hash-table :test 'equal)
                                     :pending-labels (make-hash-table :test 'equal)
                                     :label-counter 0)))
                      (puthash network-id new-conn clatter-connections)
                      new-conn))))

      ;; Disconnect existing connection if any
      (when (clatter-connection-process conn)
        (delete-process (clatter-connection-process conn)))

      ;; Cancel any pending reconnect
      (when (clatter-connection-reconnect-timer conn)
        (cancel-timer (clatter-connection-reconnect-timer conn))
        (setf (clatter-connection-reconnect-timer conn) nil))

      (setf (clatter-connection-state conn) :connecting)
      (setf (clatter-connection-nick conn) nick)
      (setf (clatter-connection-desired-nick conn) nick)
      (setf (clatter-connection-recv-buffer conn) (decode-coding-string "" 'utf-8))
      (setf (clatter-connection-cap-enabled conn) nil)
      (setf (clatter-connection-sasl-state conn) nil)
      (setf (clatter-connection-ping-sent-time conn) nil)
      (clrhash (clatter-connection-active-batches conn))
      (clrhash (clatter-connection-pending-labels conn))

      (message "[clatter] Connecting to %s:%d%s..."
               server port (if use-tls " (TLS)" ""))
      (clatter--watchdog "CONNECT %s %s:%d tls=%s" network-id server port use-tls)

      (condition-case err
          (let* ((client-cert (plist-get config :client-cert))
                 ;; Always connect plain+nowait first — TLS handshake
                 ;; happens async in the sentinel to avoid blocking Emacs
                 (proc (make-network-process
                        :name (format "clatter-%s" network-id)
                        :host server
                        :service port
                        :nowait t
                        :coding '(utf-8 . utf-8)
                        :filter #'clatter--process-filter
                        :sentinel
                        (lambda (p e)
                          (clatter--watchdog "SENTINEL %s event=%s status=%s"
                                             network-id (string-trim e)
                                             (process-status p))
                          (cond
                           ;; TCP connected — upgrade to TLS if needed
                           ((string-match-p "open" e)
                            (when use-tls
                              (clatter--watchdog "TLS-START %s" network-id)
                              (condition-case tls-err
                                  (gnutls-negotiate
                                   :process p
                                   :hostname server
                                   :keylist (when (and client-cert
                                                      (file-exists-p client-cert))
                                              (list (list client-cert client-cert))))
                                (error
                                 (clatter--watchdog "TLS-FAIL %s %s"
                                                     network-id
                                                     (error-message-string tls-err))
                                 (delete-process p)
                                 (setf (clatter-connection-state conn) :disconnected)
                                 (message "[clatter] TLS failed: %s"
                                          (error-message-string tls-err))
                                 (when (clatter-connection-reconnect-enabled conn)
                                   (clatter--schedule-reconnect conn)))))
                            (when (process-live-p p)
                              (clatter--watchdog "TLS-OK %s" network-id)
                              (set-process-sentinel p #'clatter--process-sentinel)
                              (clatter--begin-registration conn config)))
                           ;; Connection failed or closed
                           (t
                            (clatter--process-sentinel p e)))))))
            (setf (clatter-connection-process conn) proc)
            (process-put proc :clatter-network-id network-id)
            ;; Watchdog: kill process if still connecting after 15 seconds
            (let ((watchdog-proc proc))
              (run-at-time 15 nil
                           (lambda ()
                             (when (and (process-live-p watchdog-proc)
                                        (eq (clatter-connection-state conn) :connecting))
                               (clatter--watchdog "WATCHDOG-KILL %s (stuck connecting)" network-id)
                               (delete-process watchdog-proc)))))
            conn)
        (error
         (clatter--watchdog "CONNECT-FAIL %s %s" network-id (error-message-string err))
         (setf (clatter-connection-state conn) :disconnected)
         (message "[clatter] Connection failed: %s" (error-message-string err))
         nil)))))

;; --- Registration ---

(defun clatter--begin-registration (conn config)
  "Begin IRC registration sequence on CONN with CONFIG.
Always starts with CAP LS 302 for IRCv3 negotiation."
  (setf (clatter-connection-state conn) :registering)
  (setf (clatter-connection-cap-negotiating conn) t)
  (setf (clatter-connection-last-activity conn) (float-time))
  ;; Start health monitor
  (clatter--start-health-timer conn)
  ;; Store config on connection for CAP handler to reference
  (process-put (clatter-connection-process conn) :clatter-config config)
  ;; Always start with CAP negotiation
  (clatter-send conn "CAP LS 302"))

;; --- Disconnect ---

(defun clatter-disconnect (network-id &optional message)
  "Disconnect from NETWORK-ID with optional quit MESSAGE."
  (interactive
   (list (completing-read "Disconnect: "
                          (cl-loop for k being the hash-keys of clatter-connections
                                   collect k))))
  (let ((conn (clatter-get-connection network-id)))
    (when conn
      (setf (clatter-connection-reconnect-enabled conn) nil)
      (when (clatter-connection-reconnect-timer conn)
        (cancel-timer (clatter-connection-reconnect-timer conn)))
      (when (clatter-connection-nick-reclaim-timer conn)
        (cancel-timer (clatter-connection-nick-reclaim-timer conn))
        (setf (clatter-connection-nick-reclaim-timer conn) nil))
      (let ((proc (clatter-connection-process conn)))
        (when (and proc (process-live-p proc))
          (clatter-send conn (clatter-irc-quit (or message "clatter.el")))
          ;; Give the QUIT time to send, then delete async
          (run-at-time 0.5 nil
                       (lambda ()
                         (when (process-live-p proc)
                           (delete-process proc))))))
      (setf (clatter-connection-state conn) :disconnected)
      (message "[clatter] Disconnected from %s" network-id))))

;; --- Reconnection ---

(defun clatter--schedule-reconnect (conn)
  "Schedule a reconnection attempt for CONN with exponential backoff."
  (unless (eq (clatter-connection-state conn) :connecting)
    (let* ((attempts (clatter-connection-reconnect-attempts conn))
           (delay (min (* clatter-reconnect-initial-delay (expt 2 attempts))
                       clatter-reconnect-max-delay))
           (network-id (clatter-connection-network-id conn)))
      (message "[clatter] Reconnecting to %s in %ds (attempt %d)..."
               network-id delay (1+ attempts))
      (run-hook-with-args 'clatter-reconnect-hook network-id delay (1+ attempts))
      (setf (clatter-connection-reconnect-attempts conn) (1+ attempts))
      (setf (clatter-connection-reconnect-timer conn)
            (run-at-time delay nil
                         (lambda ()
                           (setf (clatter-connection-reconnect-timer conn) nil)
                           (clatter-connect network-id)))))))

;; --- Health Monitoring ---

(defun clatter--start-health-timer (conn)
  "Start periodic health check timer for CONN."
  (when (clatter-connection-health-timer conn)
    (cancel-timer (clatter-connection-health-timer conn)))
  (setf (clatter-connection-health-timer conn)
        (run-at-time clatter-ping-interval clatter-ping-interval
                     #'clatter--health-check conn)))

(defun clatter--health-check (conn)
  "Check connection health for CONN.  Send PING if idle, disconnect if timed out."
  (let ((proc (clatter-connection-process conn)))
    (when (and proc (process-live-p proc)
               (memq (process-status proc) '(open run)))
      (let ((idle (- (float-time) (or (clatter-connection-last-activity conn) 0)))
            (ping-age (when (clatter-connection-ping-sent-time conn)
                        (- (float-time) (clatter-connection-ping-sent-time conn)))))
        (clatter--debug "health: %s idle=%.0fs ping-pending=%s"
                       (clatter-connection-network-id conn) idle
                       (if ping-age (format "%.0fs" ping-age) "no"))
        (cond
         ;; Pending ping timed out
         ((and ping-age (> ping-age clatter-ping-timeout))
          (message "[clatter] Health check: ping timeout for %s (%.0fs > %ds)"
                   (clatter-connection-network-id conn) ping-age clatter-ping-timeout)
          (delete-process proc))
         ;; Idle too long, send ping
         ((> idle clatter-ping-interval)
          (clatter--debug "health: sending PING to %s (idle %.0fs)"
                          (clatter-connection-network-id conn) idle)
          (setf (clatter-connection-ping-sent-time conn) (float-time))
          (clatter-send conn (clatter-irc-ping "clatter"))))))))

;; --- Hooks ---

(defvar clatter-connect-hook nil
  "Hook run after successfully connecting (001 received).
Called with NETWORK-ID.")

(defvar clatter-disconnect-hook nil
  "Hook run after disconnecting.
Called with NETWORK-ID and EVENT string.")

(defvar clatter-reconnect-hook nil
  "Hook run when scheduling a reconnection attempt.
Called with NETWORK-ID, DELAY (seconds), and ATTEMPT (integer).")

;; --- Labeled Responses ---

(defun clatter-generate-label (conn)
  "Generate a unique label for CONN."
  (format "clatter%d" (cl-incf (clatter-connection-label-counter conn))))

(defun clatter-send-labeled (conn command &optional callback)
  "Send COMMAND on CONN with a label tag for labeled-response.
CALLBACK is called with the response message when received.
Returns the label used, or nil if labeled-response not available."
  (if (member "labeled-response" (clatter-connection-cap-enabled conn))
      (let ((label (clatter-generate-label conn)))
        (when callback
          (puthash label callback (clatter-connection-pending-labels conn)))
        (clatter-send conn (format "@label=%s %s" label command))
        label)
    (progn
      (clatter-send conn command)
      nil)))

(provide 'clatter-connection)

;;; clatter-connection.el ends here
