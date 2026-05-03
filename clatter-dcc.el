;;; clatter-dcc.el --- DCC file transfer and chat for clatter -*- lexical-binding: t; -*-

;;; Commentary:
;; Direct Client-to-Client (DCC) protocol support.
;; - DCC SEND: receive files from IRC users and XDCC bots
;; - DCC RESUME/ACCEPT: resume interrupted file transfers
;; - DCC CHAT: direct peer-to-peer chat (planned)

;;; Code:

(require 'cl-lib)

(declare-function clatter-send "clatter-connection")
(declare-function clatter-get-connection "clatter-connection")
(declare-function clatter--watchdog "clatter-connection")
(declare-function clatter-connection-network-id "clatter-connection")
(declare-function clatter-irc-privmsg "clatter-protocol")
(declare-function clatter--current-conn "clatter-commands")

;;; --- Configuration ---

(defgroup clatter-dcc nil
  "DCC file transfer and chat settings."
  :group 'clatter
  :prefix "clatter-dcc-")

(defcustom clatter-dcc-download-directory "~/Downloads/"
  "Directory for received DCC files."
  :type 'directory
  :group 'clatter-dcc)

(defcustom clatter-dcc-auto-accept nil
  "When non-nil, automatically accept incoming DCC SEND offers.
When nil, prompt for confirmation."
  :type 'boolean
  :group 'clatter-dcc)

(defcustom clatter-dcc-max-file-size 0
  "Maximum file size in bytes to accept (0 = unlimited)."
  :type 'integer
  :group 'clatter-dcc)

(defcustom clatter-dcc-progress-interval 2
  "Seconds between progress updates during transfers."
  :type 'number
  :group 'clatter-dcc)

;;; --- Data Structures ---

(cl-defstruct (clatter-dcc-transfer
               (:constructor clatter-dcc-transfer--create))
  "An active or pending DCC transfer."
  (id 0)
  (type 'send)          ; send or chat
  (direction 'incoming) ; incoming or outgoing
  nick
  network-id
  filename
  (size 0)
  (received 0)
  host                   ; dotted-quad IP string
  (port 0)
  token                  ; for passive/reverse DCC
  process                ; network process
  (state :pending)       ; :pending :connecting :active :complete :failed :cancelled
  start-time
  output-path            ; full path to output file
  progress-timer)

(defvar clatter-dcc--transfers nil
  "Alist of (ID . transfer) for all DCC transfers.")

(defvar clatter-dcc--next-id 0
  "Next unique transfer ID.")

;;; --- Hooks ---

(defvar clatter-dcc-offer-hook nil
  "Hook called when a DCC offer is received.
Called with (TRANSFER).")

(defvar clatter-dcc-complete-hook nil
  "Hook called when a DCC transfer completes.
Called with (TRANSFER).")

;;; --- IP Encoding ---

(defun clatter-dcc--decode-ip (ip-int)
  "Convert DCC IP integer IP-INT to dotted-quad string."
  (let ((n (if (stringp ip-int) (string-to-number ip-int) ip-int)))
    (format "%d.%d.%d.%d"
            (logand (ash n -24) 255)
            (logand (ash n -16) 255)
            (logand (ash n -8) 255)
            (logand n 255))))

(defun clatter-dcc--encode-ip (ip-str)
  "Convert dotted-quad IP-STR to DCC integer."
  (let ((parts (mapcar #'string-to-number (split-string ip-str "\\."))))
    (+ (ash (nth 0 parts) 24)
       (ash (nth 1 parts) 16)
       (ash (nth 2 parts) 8)
       (nth 3 parts))))

;;; --- Parsing ---

(defun clatter-dcc--parse-send (args)
  "Parse DCC SEND arguments string ARGS.
Returns plist (:filename :ip :port :size :token) or nil.
Handles both quoted and unquoted filenames."
  (when (and args (> (length args) 0))
    (let (filename rest)
      (if (string-prefix-p "\"" args)
          ;; Quoted filename: "my file.mkv" ip port size [token]
          (when (string-match "\"\\([^\"]+\\)\"\\s-+\\(.*\\)" args)
            (setq filename (match-string 1 args)
                  rest (match-string 2 args)))
        ;; Unquoted filename: file.mkv ip port size [token]
        (let ((space (cl-position ?\s args)))
          (when space
            (setq filename (substring args 0 space)
                  rest (substring args (1+ space))))))
      (when (and filename rest)
        (let ((parts (split-string rest)))
          (when (>= (length parts) 3)
            (list :filename filename
                  :ip (clatter-dcc--decode-ip (nth 0 parts))
                  :port (string-to-number (nth 1 parts))
                  :size (string-to-number (nth 2 parts))
                  :token (nth 3 parts))))))))

(defun clatter-dcc--parse-resume (args)
  "Parse DCC RESUME/ACCEPT arguments ARGS.
Returns plist (:filename :port :position) or nil."
  (when args
    (let ((parts (split-string args)))
      (when (>= (length parts) 3)
        (list :filename (nth 0 parts)
              :port (string-to-number (nth 1 parts))
              :position (string-to-number (nth 2 parts)))))))

;;; --- Formatting ---

(defun clatter-dcc--format-size (bytes)
  "Format BYTES as human-readable size string."
  (cond
   ((>= bytes (* 1024 1024 1024))
    (format "%.1f GB" (/ (float bytes) (* 1024.0 1024.0 1024.0))))
   ((>= bytes (* 1024 1024))
    (format "%.1f MB" (/ (float bytes) (* 1024.0 1024.0))))
   ((>= bytes 1024)
    (format "%.1f KB" (/ (float bytes) 1024.0)))
   (t (format "%d B" bytes))))

(defun clatter-dcc--format-speed (bytes-per-sec)
  "Format BYTES-PER-SEC as human-readable transfer speed."
  (concat (clatter-dcc--format-size (round bytes-per-sec)) "/s"))

(defun clatter-dcc--format-progress (transfer)
  "Format a progress string for TRANSFER."
  (let* ((received (clatter-dcc-transfer-received transfer))
         (size (clatter-dcc-transfer-size transfer))
         (elapsed (when (clatter-dcc-transfer-start-time transfer)
                    (- (float-time) (clatter-dcc-transfer-start-time transfer))))
         (speed (when (and elapsed (> elapsed 0))
                  (/ (float received) elapsed)))
         (pct (if (and size (> size 0))
                  (format " %d%%" (/ (* received 100) size))
                "")))
    (format "%s/%s%s%s"
            (clatter-dcc--format-size received)
            (if (and size (> size 0))
                (clatter-dcc--format-size size)
              "?")
            pct
            (if speed
                (format " @ %s" (clatter-dcc--format-speed speed))
              ""))))

;;; --- Transfer Management ---

(defun clatter-dcc--new-id ()
  "Generate a unique transfer ID."
  (cl-incf clatter-dcc--next-id))

(defun clatter-dcc--get-transfer (id)
  "Get transfer by ID."
  (cdr (assq id clatter-dcc--transfers)))

(defun clatter-dcc--register (transfer)
  "Register TRANSFER in the transfer list."
  (push (cons (clatter-dcc-transfer-id transfer) transfer)
        clatter-dcc--transfers))

(defun clatter-dcc--output-path (filename)
  "Compute output path for FILENAME, avoiding overwrites."
  (let* ((dir (expand-file-name clatter-dcc-download-directory))
         (path (expand-file-name filename dir)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    ;; Avoid overwriting existing files
    (let ((base (file-name-sans-extension path))
          (ext (file-name-extension path t))
          (n 1))
      (while (file-exists-p path)
        (setq path (format "%s_%d%s" base n ext))
        (cl-incf n)))
    path))

;;; --- Binary Ack ---

(defun clatter-dcc--make-ack (bytes)
  "Create a 4-byte big-endian acknowledgment for BYTES received."
  (let ((b (logand bytes #xFFFFFFFF)))
    (unibyte-string
     (logand (ash b -24) 255)
     (logand (ash b -16) 255)
     (logand (ash b -8) 255)
     (logand b 255))))

;;; --- DCC SEND Receive ---

(defun clatter-dcc--recv-filter (proc data)
  "Process filter for incoming DCC SEND data from PROC."
  (let* ((id (process-get proc :clatter-dcc-id))
         (transfer (clatter-dcc--get-transfer id)))
    (when transfer
      (let ((output (clatter-dcc-transfer-output-path transfer)))
        ;; Append binary data to file
        (let ((coding-system-for-write 'binary))
          (write-region data nil output 'append 'silent))
        ;; Update byte count
        (cl-incf (clatter-dcc-transfer-received transfer)
                 (string-bytes data))
        ;; Send 4-byte ack
        (when (process-live-p proc)
          (process-send-string
           proc
           (clatter-dcc--make-ack (clatter-dcc-transfer-received transfer))))
        ;; Check if complete
        (let ((size (clatter-dcc-transfer-size transfer))
              (received (clatter-dcc-transfer-received transfer)))
          (when (and (> size 0) (>= received size))
            (clatter-dcc--complete transfer)))))))

(defun clatter-dcc--recv-sentinel (proc event)
  "Process sentinel for DCC receive from PROC with EVENT."
  (let* ((id (process-get proc :clatter-dcc-id))
         (transfer (clatter-dcc--get-transfer id)))
    (when transfer
      (cond
       ;; TCP connected - start receiving
       ((string-match-p "open" event)
        (setf (clatter-dcc-transfer-state transfer) :active)
        (message "[clatter] DCC connected: receiving %s from %s"
                 (clatter-dcc-transfer-filename transfer)
                 (clatter-dcc-transfer-nick transfer)))
       ;; Connection closed normally
       ((string-match-p "\\`\\(finished\\|deleted\\|connection broken\\)" event)
        (if (or (= (clatter-dcc-transfer-size transfer) 0)
                (>= (clatter-dcc-transfer-received transfer)
                    (clatter-dcc-transfer-size transfer)))
            (clatter-dcc--complete transfer)
          (clatter-dcc--fail transfer
                             (format "Connection lost at %s"
                                     (clatter-dcc--format-progress transfer)))))
       ;; Connection failed
       ((string-match-p "failed" event)
        (clatter-dcc--fail transfer (string-trim event)))))))

(defun clatter-dcc--complete (transfer)
  "Mark TRANSFER as complete."
  (unless (eq (clatter-dcc-transfer-state transfer) :complete)
    (setf (clatter-dcc-transfer-state transfer) :complete)
    (when (clatter-dcc-transfer-progress-timer transfer)
      (cancel-timer (clatter-dcc-transfer-progress-timer transfer)))
    (let ((proc (clatter-dcc-transfer-process transfer)))
      (when (and proc (process-live-p proc))
        (delete-process proc)))
    (message "[clatter] DCC complete: %s (%s) → %s"
             (clatter-dcc-transfer-filename transfer)
             (clatter-dcc--format-size (clatter-dcc-transfer-received transfer))
             (clatter-dcc-transfer-output-path transfer))
    (run-hook-with-args 'clatter-dcc-complete-hook transfer)))

(defun clatter-dcc--fail (transfer reason)
  "Mark TRANSFER as failed with REASON."
  (unless (memq (clatter-dcc-transfer-state transfer) '(:complete :failed :cancelled))
    (setf (clatter-dcc-transfer-state transfer) :failed)
    (when (clatter-dcc-transfer-progress-timer transfer)
      (cancel-timer (clatter-dcc-transfer-progress-timer transfer)))
    (let ((proc (clatter-dcc-transfer-process transfer)))
      (when (and proc (process-live-p proc))
        (delete-process proc)))
    (message "[clatter] DCC failed: %s - %s"
             (clatter-dcc-transfer-filename transfer)
             reason)))

;;; --- Accept / Reject ---

(defun clatter-dcc-accept (transfer)
  "Accept and start receiving a pending DCC SEND TRANSFER."
  (let* ((host (clatter-dcc-transfer-host transfer))
         (port (clatter-dcc-transfer-port transfer))
         (output (clatter-dcc--output-path
                  (clatter-dcc-transfer-filename transfer))))
    (setf (clatter-dcc-transfer-output-path transfer) output)
    (setf (clatter-dcc-transfer-state transfer) :connecting)
    (setf (clatter-dcc-transfer-start-time transfer) (float-time))
    ;; Create empty output file
    (let ((coding-system-for-write 'binary))
      (write-region "" nil output nil 'silent))
    (condition-case err
        (let ((proc (make-network-process
                     :name (format "clatter-dcc-%d"
                                   (clatter-dcc-transfer-id transfer))
                     :host host
                     :service port
                     :nowait t
                     :coding 'binary
                     :filter #'clatter-dcc--recv-filter
                     :sentinel #'clatter-dcc--recv-sentinel)))
          (process-put proc :clatter-dcc-id (clatter-dcc-transfer-id transfer))
          (setf (clatter-dcc-transfer-process transfer) proc)
          ;; Progress timer
          (setf (clatter-dcc-transfer-progress-timer transfer)
                (run-at-time clatter-dcc-progress-interval
                             clatter-dcc-progress-interval
                             (lambda ()
                               (when (eq (clatter-dcc-transfer-state transfer)
                                         :active)
                                 (message "[clatter] DCC %s: %s"
                                          (clatter-dcc-transfer-filename transfer)
                                          (clatter-dcc--format-progress transfer))))))
          (message "[clatter] DCC connecting to %s:%d for %s..."
                   host port (clatter-dcc-transfer-filename transfer)))
      (error
       (clatter-dcc--fail transfer (error-message-string err))))))

(defun clatter-dcc-reject (transfer)
  "Reject a pending DCC SEND TRANSFER."
  (setf (clatter-dcc-transfer-state transfer) :cancelled)
  (message "[clatter] DCC rejected: %s from %s"
           (clatter-dcc-transfer-filename transfer)
           (clatter-dcc-transfer-nick transfer)))

(defun clatter-dcc--prompt-accept (transfer)
  "Prompt user to accept or reject TRANSFER."
  (let* ((filename (clatter-dcc-transfer-filename transfer))
         (nick (clatter-dcc-transfer-nick transfer))
         (size (clatter-dcc-transfer-size transfer))
         (size-str (if (> size 0)
                       (clatter-dcc--format-size size)
                     "unknown size")))
    (if (and (> clatter-dcc-max-file-size 0)
             (> size clatter-dcc-max-file-size))
        (progn
          (clatter-dcc-reject transfer)
          (message "[clatter] DCC rejected %s from %s: exceeds max size (%s > %s)"
                   filename nick size-str
                   (clatter-dcc--format-size clatter-dcc-max-file-size)))
      (if (yes-or-no-p
           (format "[clatter] Accept DCC SEND \"%s\" (%s) from %s? "
                   filename size-str nick))
          (clatter-dcc-accept transfer)
        (clatter-dcc-reject transfer)))))

;;; --- DCC RESUME ---

(defun clatter-dcc-resume (transfer)
  "Request resuming a partially downloaded TRANSFER.
Sends CTCP DCC RESUME to the sender."
  (let* ((conn (clatter-get-connection
                (clatter-dcc-transfer-network-id transfer)))
         (output (clatter-dcc-transfer-output-path transfer))
         (position (if (and output (file-exists-p output))
                       (file-attribute-size (file-attributes output))
                     0)))
    (when (and conn (> position 0))
      (setf (clatter-dcc-transfer-received transfer) position)
      (clatter-send conn
                    (format "PRIVMSG %s :\C-aDCC RESUME %s %d %d\C-a"
                            (clatter-dcc-transfer-nick transfer)
                            (clatter-dcc-transfer-filename transfer)
                            (clatter-dcc-transfer-port transfer)
                            position))
      (message "[clatter] DCC resume requested for %s at %s"
               (clatter-dcc-transfer-filename transfer)
               (clatter-dcc--format-size position)))))

;;; --- CTCP DCC Handler ---

(defun clatter-dcc--handle-ctcp (conn sender-nick _target ctcp-cmd ctcp-args)
  "Handle DCC CTCP on CONN from SENDER-NICK.
CTCP-CMD should be \"DCC\", CTCP-ARGS is the rest of the CTCP message."
  (when (string-equal ctcp-cmd "DCC")
    (let* ((space (cl-position ?\s ctcp-args))
           (dcc-cmd (upcase (if space
                                (substring ctcp-args 0 space)
                              ctcp-args)))
           (dcc-args (if space
                         (substring ctcp-args (1+ space))
                       ""))
           (network-id (clatter-connection-network-id conn)))
      (pcase dcc-cmd
        ("SEND"
         (let ((parsed (clatter-dcc--parse-send dcc-args)))
           (when parsed
             (let* ((id (clatter-dcc--new-id))
                    (transfer (clatter-dcc-transfer--create
                               :id id
                               :type 'send
                               :direction 'incoming
                               :nick sender-nick
                               :network-id network-id
                               :filename (plist-get parsed :filename)
                               :size (plist-get parsed :size)
                               :host (plist-get parsed :ip)
                               :port (plist-get parsed :port)
                               :token (plist-get parsed :token))))
               (clatter-dcc--register transfer)
               (run-hook-with-args 'clatter-dcc-offer-hook transfer)
               (if clatter-dcc-auto-accept
                   (clatter-dcc-accept transfer)
                 (clatter-dcc--prompt-accept transfer))))))

        ("ACCEPT"
         (let ((parsed (clatter-dcc--parse-resume dcc-args)))
           (when parsed
             ;; Find matching pending transfer for this resume
             (let ((match (cl-find-if
                           (lambda (pair)
                             (let ((tr (cdr pair)))
                               (and (eq (clatter-dcc-transfer-state tr) :pending)
                                    (string-equal (clatter-dcc-transfer-nick tr)
                                                  sender-nick)
                                    (string-equal (clatter-dcc-transfer-filename tr)
                                                  (plist-get parsed :filename)))))
                           clatter-dcc--transfers)))
               (when match
                 (setf (clatter-dcc-transfer-received (cdr match))
                       (plist-get parsed :position))
                 (clatter-dcc-accept (cdr match)))))))))))

;;; --- Interactive Commands ---

(defun clatter-dcc-list ()
  "Display a list of all DCC transfers."
  (interactive)
  (if (null clatter-dcc--transfers)
      (message "[clatter] No DCC transfers")
    (let ((buf (get-buffer-create "*clatter-dcc*")))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert "DCC Transfers\n")
          (insert (make-string 70 ?─) "\n")
          (dolist (pair (reverse clatter-dcc--transfers))
            (let ((tr (cdr pair)))
              (insert (format "#%-3d  %-10s  %-15s  %-25s  %s\n"
                              (clatter-dcc-transfer-id tr)
                              (clatter-dcc-transfer-state tr)
                              (clatter-dcc-transfer-nick tr)
                              (clatter-dcc-transfer-filename tr)
                              (clatter-dcc--format-progress tr)))
              (when (and (eq (clatter-dcc-transfer-state tr) :complete)
                         (clatter-dcc-transfer-output-path tr))
                (insert (format "      → %s\n"
                                (clatter-dcc-transfer-output-path tr))))))))
      (pop-to-buffer buf))))

(defun clatter-dcc-cancel (id)
  "Cancel DCC transfer with ID."
  (interactive "nTransfer ID: ")
  (let ((transfer (clatter-dcc--get-transfer id)))
    (if transfer
        (progn
          (setf (clatter-dcc-transfer-state transfer) :cancelled)
          (when (clatter-dcc-transfer-progress-timer transfer)
            (cancel-timer (clatter-dcc-transfer-progress-timer transfer)))
          (let ((proc (clatter-dcc-transfer-process transfer)))
            (when (and proc (process-live-p proc))
              (delete-process proc)))
          (message "[clatter] DCC cancelled: %s"
                   (clatter-dcc-transfer-filename transfer)))
      (message "[clatter] No transfer with ID %d" id))))

;;; --- Slash Commands ---

(defun clatter-cmd-dcc (args)
  "Handle /dcc commands.
Usage:
  /dcc list           - show all transfers
  /dcc accept [ID]    - accept pending transfer
  /dcc cancel ID      - cancel a transfer
  /dcc get BOT #PACK  - request pack from XDCC bot"
  (let* ((parts (split-string (string-trim args)))
         (subcmd (downcase (or (car parts) "list")))
         (rest (cdr parts)))
    (pcase subcmd
      ("list" (clatter-dcc-list))
      ("accept"
       (let* ((id (when rest (string-to-number (car rest))))
              (transfer (if id
                            (clatter-dcc--get-transfer id)
                          ;; Accept first pending
                          (cdr (cl-find-if
                                (lambda (pair)
                                  (eq (clatter-dcc-transfer-state (cdr pair))
                                      :pending))
                                clatter-dcc--transfers)))))
         (if transfer
             (clatter-dcc-accept transfer)
           (message "[clatter] No pending DCC transfer to accept"))))
      ("cancel"
       (if rest
           (clatter-dcc-cancel (string-to-number (car rest)))
         (message "[clatter] Usage: /dcc cancel ID")))
      ("get"
       (if (>= (length rest) 2)
           (let* ((bot (nth 0 rest))
                  (pack (nth 1 rest))
                  (conn (clatter--current-conn)))
             (if conn
                 (progn
                   (clatter-send conn
                                (clatter-irc-privmsg
                                 bot (format "xdcc send %s" pack)))
                   (message "[clatter] Requested pack %s from %s" pack bot))
               (message "[clatter] Not connected")))
         (message "[clatter] Usage: /dcc get BOT #PACK")))
      (_
       (message "[clatter] Unknown DCC command: %s. Try: list, accept, cancel, get"
                subcmd)))))

;;; --- Setup ---

(defun clatter-dcc-setup ()
  "Enable DCC support.  Call this after loading clatter."
  (interactive)
  (add-hook 'clatter-ctcp-hook #'clatter-dcc--handle-ctcp))

(provide 'clatter-dcc)
;;; clatter-dcc.el ends here
