;;; clatter-sts.el --- IRCv3 Strict Transport Security -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Glenn Thompson
;; Author: Glenn Thompson
;; License: MIT

;;; Commentary:

;; IRCv3 STS (Strict Transport Security) for clatter.el.
;; When a server advertises STS during CAP LS on a plaintext connection,
;; the client disconnects and reconnects on the TLS port.
;; STS policies are persisted and enforced on future connections.

;;; Code:

(require 'cl-lib)

;; --- Configuration ---

(defcustom clatter-sts-enable t
  "Enable IRCv3 Strict Transport Security.
When non-nil, plaintext connections are auto-upgraded to TLS
if the server advertises STS."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-sts-policy-file
  (expand-file-name "clatter/sts-policies.el"
                    user-emacs-directory)
  "File to persist STS policies."
  :type 'file
  :group 'clatter)

;; --- Policy Storage ---

(defvar clatter-sts--policies (make-hash-table :test 'equal)
  "Hash table of STS policies.
Key: server hostname (string).
Value: plist (:port PORT :expiry FLOAT-TIME).")

(defun clatter-sts--load-policies ()
  "Load STS policies from disk."
  (when (file-exists-p clatter-sts-policy-file)
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents clatter-sts-policy-file)
          (let ((data (read (current-buffer))))
            (when (hash-table-p data)
              (setq clatter-sts--policies data))))
      (error nil)))
  ;; Prune expired policies
  (let ((now (float-time))
        (to-remove nil))
    (maphash (lambda (host policy)
               (when (< (plist-get policy :expiry) now)
                 (push host to-remove)))
             clatter-sts--policies)
    (dolist (host to-remove)
      (remhash host clatter-sts--policies))))

(defun clatter-sts--save-policies ()
  "Save STS policies to disk."
  (let ((dir (file-name-directory clatter-sts-policy-file)))
    (unless (file-directory-p dir)
      (make-directory dir t)))
  (with-temp-file clatter-sts-policy-file
    (let ((print-level nil)
          (print-length nil))
      (prin1 clatter-sts--policies (current-buffer)))))

(defun clatter-sts-store-policy (hostname port duration)
  "Store an STS policy for HOSTNAME: TLS PORT, valid for DURATION seconds."
  (let ((expiry (+ (float-time) duration)))
    (puthash hostname (list :port port :expiry expiry)
             clatter-sts--policies)
    (clatter-sts--save-policies)))

(defun clatter-sts-remove-policy (hostname)
  "Remove STS policy for HOSTNAME (duration=0 means remove)."
  (remhash hostname clatter-sts--policies)
  (clatter-sts--save-policies))

(defun clatter-sts-lookup (hostname)
  "Look up STS policy for HOSTNAME.
Returns plist (:port PORT :expiry FLOAT-TIME) or nil."
  (let ((policy (gethash hostname clatter-sts--policies)))
    (when policy
      (if (< (plist-get policy :expiry) (float-time))
          (progn (remhash hostname clatter-sts--policies) nil)
        policy))))

;; --- CAP LS STS Parsing ---

(defun clatter-sts--parse-value (sts-value)
  "Parse STS capability value string.
STS-VALUE is like \"port=6697,duration=2592000\".
Returns plist (:port PORT :duration DURATION)."
  (let ((parts (split-string sts-value ","))
        (result nil))
    (dolist (part parts)
      (let ((kv (split-string part "=")))
        (when (= (length kv) 2)
          (cond
           ((string= (car kv) "port")
            (setq result (plist-put result :port (string-to-number (cadr kv)))))
           ((string= (car kv) "duration")
            (setq result (plist-put result :duration (string-to-number (cadr kv)))))))))
    result))

(defun clatter-sts-check-cap (caps-string hostname is-tls)
  "Check CAP LS response CAPS-STRING for STS on HOSTNAME.
IS-TLS indicates whether the current connection uses TLS.
Returns (:action upgrade :port PORT) if upgrade needed,
        (:action store :port PORT :duration DURATION) if policy should be stored,
        nil if no STS or not applicable."
  (when (and clatter-sts-enable caps-string)
    (let ((caps (split-string caps-string))
          (result nil))
      (dolist (cap caps)
        (when (and (not result) (string-prefix-p "sts=" cap))
          (let* ((value (substring cap 4))
                 (parsed (clatter-sts--parse-value value))
                 (port (plist-get parsed :port))
                 (duration (plist-get parsed :duration)))
            (cond
             ;; On plaintext connection: upgrade required
             ((and (not is-tls) port)
              (setq result (list :action 'upgrade :port port)))
             ;; On TLS connection: store/refresh policy
             ((and is-tls duration (> duration 0))
              (clatter-sts-store-policy hostname (or port 6697) duration)
              (setq result (list :action 'store :port (or port 6697)
                                 :duration duration)))
             ;; Duration 0 on TLS: remove policy
             ((and is-tls duration (= duration 0))
              (clatter-sts-remove-policy hostname))))))
      result)))

;; --- Initialize ---

(defun clatter-sts-init ()
  "Initialize STS subsystem, loading persisted policies."
  (clatter-sts--load-policies))

;; Load on require
(clatter-sts-init)

(provide 'clatter-sts)

;;; clatter-sts.el ends here
