;;; test-connection.el --- Tests for clatter-connection -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-helper)
(require 'clatter-connection)
(require 'clatter-rawlog)

(ert-deftest clatter-test-tls-external-args-openssl-verifies ()
  "openssl s_client must abort on verify failure and check the hostname."
  (let ((clatter-tls-external-command "openssl"))
    (let ((args (clatter--tls-external-args "irc.example.com" 6697 nil)))
      (should (member "-verify_return_error" args))
      (should (member "-verify_hostname" args))
      (should (member "-servername" args)))))

(ert-deftest clatter-test-tls-external-args-gnutls-unchanged ()
  "gnutls-cli aborts on verification failure by default; keep args as-is."
  (let ((clatter-tls-external-command "gnutls-cli"))
    (let ((args (clatter--tls-external-args "irc.example.com" 6697 nil)))
      (should (member "--port" args))
      (should (equal (car (last args)) "irc.example.com")))))

(ert-deftest clatter-test-process-filter-chunks-and-crlf ()
  "Complete lines retain logging order and partial CRLF across chunks."
  (let* ((clatter-connections (make-hash-table :test 'equal))
         (conn (clatter-test-make-connection))
         (proc (make-pipe-process :name "clatter-test-filter" :noquery t))
         events
         (clatter-rawlog-incoming-hook
          (list (lambda (network-id line)
                  (should (equal network-id "testnet"))
                  (push (list :raw line) events)))))
    (unwind-protect
        (progn
          (setf (clatter-connection-process conn) proc)
          (process-put proc :clatter-network-id "testnet")
          (cl-letf (((symbol-function 'clatter--debug)
                     (lambda (format-string line)
                       (should (equal format-string "<< %s"))
                       (push (list :debug line) events)))
                    ((symbol-function 'clatter--handle-line)
                     (lambda (received-conn line)
                       (should (eq received-conn conn))
                       (push (list :handle line) events))))
            (clatter--process-filter proc "one\r\ntwo\n\n\r\npartial\r")
            (should (equal (clatter-connection-recv-buffer conn) "partial\r"))
            (should (equal (reverse events)
                           '((:debug "one") (:raw "one") (:handle "one")
                             (:debug "two") (:raw "two") (:handle "two"))))
            (clatter--process-filter proc "\nthrée\r\r\nfour\r")
            (should (equal (clatter-connection-recv-buffer conn) "four\r"))
            (clatter--process-filter proc "\n")
            (should (equal (clatter-connection-recv-buffer conn) ""))
            (should-not (process-get proc :clatter-filter-busy))
            (should
             (equal (reverse events)
                    (cl-loop for line in '("one" "two" "partial" "thrée" "four")
                             append (list (list :debug line) (list :raw line)
                                          (list :handle line)))))))
      (delete-process proc))))

(ert-deftest clatter-test-process-filter-reentrant-order ()
  "Reentrant input follows queued lines and completes their partial tail."
  (let* ((clatter-connections (make-hash-table :test 'equal))
         (conn (clatter-test-make-connection))
         (proc (make-pipe-process :name "clatter-test-filter" :noquery t))
         (clatter-rawlog-incoming-hook nil)
         lines)
    (unwind-protect
        (progn
          (setf (clatter-connection-process conn) proc)
          (process-put proc :clatter-network-id "testnet")
          (cl-letf (((symbol-function 'clatter--debug) (lambda (&rest _)))
                    ((symbol-function 'clatter--handle-line)
                     (lambda (_conn line)
                       (should (process-get proc :clatter-filter-busy))
                       (push line lines)
                       (pcase line
                         ("one"
                          (clatter--process-filter proc "tial\r")
                          (clatter--process-filter proc "\nthree\nlast"))
                         ("two" (clatter--process-filter proc "more\n"))
                         ("partial" (clatter--process-filter proc "four\nend"))))))
            (clatter--process-filter proc "one\ntwo\npar")
            (should (equal (reverse lines)
                           '("one" "two" "partial" "three" "lastmore" "four")))
            (should (equal (clatter-connection-recv-buffer conn) "end"))
            (clatter--process-filter proc "ing\n")
            (should (equal (reverse lines)
                           '("one" "two" "partial" "three" "lastmore"
                             "four" "ending")))
            (should (equal (clatter-connection-recv-buffer conn) ""))
            (should-not (process-get proc :clatter-filter-busy))))
      (delete-process proc))))

(ert-deftest clatter-test-process-filter-error-releases-busy ()
  "An error consumes its line but preserves queued and reentrant input."
  (let* ((clatter-connections (make-hash-table :test 'equal))
         (conn (clatter-test-make-connection))
         (proc (make-pipe-process :name "clatter-test-filter" :noquery t))
         (clatter-rawlog-incoming-hook nil)
         lines)
    (unwind-protect
        (progn
          (setf (clatter-connection-process conn) proc)
          (process-put proc :clatter-network-id "testnet")
          (cl-letf (((symbol-function 'clatter--debug) (lambda (&rest _)))
                    ((symbol-function 'clatter--handle-line)
                     (lambda (_conn line)
                       (push line lines)
                       (when (equal line "one")
                         (clatter--process-filter proc "tial\nthree\n")
                         (error "Handler failure")))))
            (should-error (clatter--process-filter proc "one\ntwo\npar"))
            (should (equal lines '("one")))
            (should-not (process-get proc :clatter-filter-busy))
            (should (equal (clatter-connection-recv-buffer conn)
                           "two\npartial\nthree\n"))
            (clatter--process-filter proc "four\n")
            (should (equal (reverse lines)
                           '("one" "two" "partial" "three" "four")))
            (should (equal (clatter-connection-recv-buffer conn) ""))
            (should-not (process-get proc :clatter-filter-busy))))
      (delete-process proc))))

(ert-deftest clatter-test-process-filter-owner-loss-during-dispatch ()
  "Disconnects and replacements discard captured data, even on error."
  (dolist (replace-p '(nil t))
    (dolist (fail-p '(nil t))
      (let* ((clatter-connections (make-hash-table :test 'equal))
             (conn (clatter-test-make-connection))
             (proc (make-pipe-process :name "clatter-test-filter" :noquery t))
             (replacement (make-pipe-process :name "clatter-test-replacement"
                                             :noquery t))
             (clatter-rawlog-incoming-hook nil)
             lines)
        (unwind-protect
            (progn
              (setf (clatter-connection-process conn) proc)
              (process-put proc :clatter-network-id "testnet")
              (process-put replacement :clatter-network-id "testnet")
              (cl-letf (((symbol-function 'clatter--debug) (lambda (&rest _)))
                        ((symbol-function 'clatter--handle-line)
                         (lambda (_conn line)
                           (push line lines)
                           (when (equal line "one")
                             (clatter--process-filter proc "stale\n")
                             (setf (clatter-connection-process conn)
                                   (and replace-p replacement)
                                   (clatter-connection-recv-buffer conn) "")
                             (when fail-p
                               (error "Disconnected handler failure"))))))
                (if fail-p
                    (should-error (clatter--process-filter proc "one\ntwo\ntail"))
                  (clatter--process-filter proc "one\ntwo\ntail"))
                (should (equal lines '("one")))
                (should (equal (clatter-connection-recv-buffer conn) ""))
                (should-not (process-get proc :clatter-filter-busy))
                (clatter--process-filter proc "late\n")
                (should (equal lines '("one")))
                (should (equal (clatter-connection-recv-buffer conn) ""))
                (when replace-p
                  (clatter--process-filter replacement "fresh\n")
                  (should (equal (reverse lines) '("one" "fresh")))
                  (should (equal (clatter-connection-recv-buffer conn) "")))))
          (delete-process proc)
          (delete-process replacement))))))

(defmacro clatter-test--with-filter-session (conn proc &rest body)
  "Run BODY with an owned filter process PROC and connection CONN."
  (declare (indent 2))
  `(let* ((clatter-connections (make-hash-table :test 'equal))
          (,conn (clatter-test-make-connection))
          (,proc (make-pipe-process :name "clatter-test-resource" :noquery t))
          (clatter-rawlog-incoming-hook nil)
          (clatter-disconnect-hook nil))
     (setf (clatter-connection-process ,conn) ,proc
           (clatter-connection-reconnect-enabled ,conn) nil)
     (process-put ,proc :clatter-network-id "testnet")
     (set-process-sentinel ,proc #'clatter--process-sentinel)
     (unwind-protect
         (cl-letf (((symbol-function 'clatter--watchdog) (lambda (&rest _)))
                   ((symbol-function 'clatter--debug) (lambda (&rest _))))
           ,@body)
       (when (process-live-p ,proc)
         (setf (clatter-connection-reconnect-enabled ,conn) nil)
         (delete-process ,proc)))))

(ert-deftest clatter-test-receive-line-boundary-and-large-chunk ()
  "Byte-exact lines and tails fit; many short lines are not a large line."
  (clatter-test--with-filter-session conn proc
    (let ((clatter-max-receive-line-bytes 8)
          lines)
      (cl-letf (((symbol-function 'clatter--handle-line)
                 (lambda (_conn line) (push line lines))))
        (clatter--process-filter proc "12345678\n")
        (clatter--process-filter proc (apply #'concat (make-list 1000 "x\n")))
        (clatter--process-filter proc "éééé\n")
        (clatter--process-filter proc "1234")
        (clatter--process-filter proc "5678")
        (should (= (clatter-connection-recv-tail-bytes conn) 8))
        (should (equal (clatter-connection-recv-buffer conn) "12345678"))
        (clatter--process-filter proc "\nabcdefg\r")
        (should (= (clatter-connection-recv-tail-bytes conn) 8))
        (clatter--process-filter proc "\n")
        (should (process-live-p proc))
        (should (= (length lines) 1004))
        (should (equal (cl-subseq lines 0 3)
                       '("abcdefg" "12345678" "éééé")))
        (should (zerop (clatter-connection-recv-tail-bytes conn)))))))

(ert-deftest clatter-test-receive-line-overflow-complete-and-partial ()
  "Max+1 complete, unfinished, split, and multibyte lines abort visibly."
  (dolist (chunks '(("123456789\n") ("123456789")
                    ("12345678" "9") ("1234" "56789\n")
                    ("ééééé\n") ("12345678" "\r\n")))
    (clatter-test--with-filter-session conn proc
      (let ((clatter-max-receive-line-bytes 8)
            lines reason)
        (let ((clatter-disconnect-hook
               (list (lambda (_network why) (setq reason why)))))
          (cl-letf (((symbol-function 'clatter--handle-line)
                     (lambda (_conn line) (push line lines))))
            (dolist (chunk chunks)
              (clatter--process-filter proc chunk))
            (should-not lines)
            (should (string-match-p "incoming line bytes.*limit 8" reason))
            (should-not (clatter-connection-process conn))
            (should-not (clatter-connection-recv-buffer conn))
            (should (zerop (clatter-connection-recv-tail-bytes conn)))
            (should-not (process-get proc :clatter-filter-busy))))))))

(ert-deftest clatter-test-receive-reentrant-line-bound ()
  "Reentrant input includes the captured tail and cannot restore overflow."
  (dolist (overflow-p '(nil t))
    (dolist (fail-p '(nil t))
      (clatter-test--with-filter-session conn proc
        (let ((clatter-max-receive-line-bytes 8)
              lines)
          (cl-letf (((symbol-function 'clatter--handle-line)
                     (lambda (_conn line)
                       (push line lines)
                       (when (equal line "go")
                         (clatter--process-filter
                          proc (if overflow-p "56789\n" "5678\n"))
                         (when fail-p (error "Hook failure"))))))
            (if fail-p
                (should-error (clatter--process-filter proc "go\n1234"))
              (clatter--process-filter proc "go\n1234"))
            (should-not (process-get proc :clatter-filter-busy))
            (if overflow-p
                (progn
                  (should (equal lines '("go")))
                  (should-not (clatter-connection-process conn))
                  (should-not (clatter-connection-recv-buffer conn))
                  (should (zerop (clatter-connection-recv-tail-bytes conn))))
              (when fail-p
                (should (equal (clatter-connection-recv-buffer conn)
                               "12345678\n"))
                (clatter--process-filter proc ""))
              (should (equal (reverse lines) '("go" "12345678")))
              (should (zerop (clatter-connection-recv-tail-bytes conn))))))))))

(ert-deftest clatter-test-receive-error-preserves-tail-bound ()
  "An error leaves the final byte count valid for the next continuation."
  (dolist (overflow-p '(nil t))
    (clatter-test--with-filter-session conn proc
      (let ((clatter-max-receive-line-bytes 8)
            lines)
        (cl-letf (((symbol-function 'clatter--handle-line)
                   (lambda (_conn line)
                     (push line lines)
                     (when (equal line "go")
                       (clatter--process-filter proc "5678")
                       (error "Hook failure")))))
          (should-error (clatter--process-filter proc "go\n1234"))
          (should (equal (clatter-connection-recv-buffer conn) "12345678"))
          (should (= (clatter-connection-recv-tail-bytes conn) 8))
          (clatter--process-filter proc (if overflow-p "9" "\n"))
          (if overflow-p
              (progn
                (should (equal lines '("go")))
                (should-not (clatter-connection-recv-buffer conn)))
            (should (equal (reverse lines) '("go" "12345678")))
            (should (process-live-p proc))))))))

(ert-deftest clatter-test-receive-maximum-standard-tagged-line ()
  "The default receive ceiling accepts 8191 tag bytes plus a full message."
  (clatter-test--with-filter-session conn proc
    (let* ((message-prefix ":server NOTICE testnick :")
           (line (concat "@x=" (make-string 8188 ?a) " "
                         message-prefix
                         (make-string (- 510 (length message-prefix)) ?x)))
           received)
      (cl-letf (((symbol-function 'clatter--handle-line)
                 (lambda (_conn value) (setq received value))))
        (clatter--process-filter proc (concat line "\r\n"))
        (should (equal received line))
        (should (process-live-p proc))))))

(ert-deftest clatter-test-terminal-sentinel-releases-session-data ()
  "Terminal cleanup runs with or without reconnect and preserves user data."
  (dolist (reconnect-p '(nil t))
    (clatter-test--with-filter-session conn proc
      (let ((buffer (generate-new-buffer " clatter-test-history"))
            (config '(:server "irc.example" :nick "me"))
            scheduled)
        (unwind-protect
            (progn
              (with-current-buffer buffer (insert "history\ndraft"))
              (setf (clatter-connection-config conn) config
                    (clatter-connection-connect-overrides conn) '(:nick "me")
                    (clatter-connection-reconnect-enabled conn) reconnect-p
                    (clatter-connection-cap-negotiating conn) t
                    (clatter-connection-recv-buffer conn) "unfinished"
                    (clatter-connection-recv-tail-bytes conn) 10
                    (clatter-connection--motd-lines conn) '("motd")
                    (clatter-connection--whois-data conn) '(:nick "alice")
                    (clatter-connection-pending-whox conn) '("#chan")
                    (clatter-connection-last-query-buffer conn) buffer)
              (puthash "label" (lambda (_message) buffer)
                       (clatter-connection-pending-labels conn))
              (clatter--handle-batch conn nil '("+done" "chathistory" "#chan"))
              (clatter--accumulate-batch conn "done" "alice" "history" nil)
              (clatter--handle-batch conn nil '("-done"))
              (clatter--handle-batch conn nil '("+active" "chathistory" "#chan"))
              (clatter--accumulate-batch conn "active" "alice" "pending" nil)
              (cl-letf (((symbol-function 'clatter--schedule-reconnect)
                         (lambda (_conn) (setq scheduled t))))
                (clatter--process-sentinel proc "connection broken by peer\n"))
              (should (eq scheduled reconnect-p))
              (should (eq (clatter-get-connection "testnet") conn))
              (should (eq (clatter-connection-config conn) config))
              (should (equal (clatter-connection-connect-overrides conn)
                             '(:nick "me")))
              (should-not (clatter-connection-recv-buffer conn))
              (should (zerop (clatter-connection-recv-tail-bytes conn)))
              (should (zerop (hash-table-count
                             (clatter-connection-active-batches conn))))
              (should-not (clatter-connection-deferred-batches conn))
              (should (zerop (hash-table-count
                             (clatter-connection-pending-labels conn))))
              (should (zerop (clatter-connection-batch-bytes conn)))
              (should (zerop (clatter-connection-batch-message-count conn)))
              (should (zerop (clatter-connection-batch-count conn)))
              (should-not (clatter-connection--motd-lines conn))
              (should-not (clatter-connection--whois-data conn))
              (should-not (clatter-connection-pending-whox conn))
              (should-not (clatter-connection-last-query-buffer conn))
              (should (buffer-live-p buffer))
              (should (equal (with-current-buffer buffer (buffer-string))
                             "history\ndraft")))
          (kill-buffer buffer))))))

(ert-deftest clatter-test-stale-sentinel-preserves-session-data ()
  "A superseded process cannot release the replacement session's payload."
  (clatter-test--with-filter-session conn proc
    (let ((old (make-pipe-process :name "clatter-test-old" :noquery t)))
      (unwind-protect
          (progn
            (process-put old :clatter-network-id "testnet")
            (setf (clatter-connection-recv-buffer conn) "new-tail"
                  (clatter-connection-recv-tail-bytes conn) 8
                  (clatter-connection--motd-lines conn) '("new-motd")
                  (clatter-connection--whois-data conn) '(:nick "new")
                  (clatter-connection-pending-whox conn) '("#new")
                  (clatter-connection-cap-negotiating conn) t)
            (puthash "new" #'ignore (clatter-connection-pending-labels conn))
            (clatter--handle-batch conn nil '("+new" "chathistory" "#new"))
            (clatter--accumulate-batch conn "new" "sender" "text" nil)
            (clatter--handle-batch conn nil '("-new"))
            (clatter--handle-batch conn nil '("+active" "chathistory" "#new"))
            (let ((bytes (clatter-connection-batch-bytes conn)))
              (clatter--process-sentinel old "deleted process\n")
              (should (eq (clatter-connection-process conn) proc))
              (should (equal (clatter-connection-recv-buffer conn) "new-tail"))
              (should (= (clatter-connection-recv-tail-bytes conn) 8))
              (should (= (clatter-connection-batch-bytes conn) bytes))
              (should (= (clatter-connection-batch-message-count conn) 1))
              (should (= (clatter-connection-batch-count conn) 2))
              (should (clatter-connection-deferred-batches conn))
              (should (gethash "active" (clatter-connection-active-batches conn)))
              (should (gethash "new" (clatter-connection-pending-labels conn)))
              (should (equal (clatter-connection--motd-lines conn) '("new-motd")))
              (should (equal (clatter-connection--whois-data conn) '(:nick "new")))
              (should (equal (clatter-connection-pending-whox conn) '("#new")))))
        (delete-process old)))))

(ert-deftest clatter-test-reconnect-releases-session-data ()
  "Replacing a live session clears all retained resources before new input."
  (clatter-test--with-filter-session conn proc
    (let ((replacement (make-pipe-process :name "clatter-test-new" :noquery t)))
      (unwind-protect
          (progn
            (setf (clatter-connection-cap-negotiating conn) t
                  (clatter-connection--motd-lines conn) '("old")
                  (clatter-connection--whois-data conn) '(:nick "old")
                  (clatter-connection-pending-whox conn) '("#old"))
            (clatter--process-filter proc "partial")
            (clatter--handle-batch conn nil '("+old" "chathistory" "#old"))
            (clatter--accumulate-batch conn "old" "sender" "text" nil)
            (clatter--handle-batch conn nil '("-old"))
            (puthash "old" #'ignore (clatter-connection-pending-labels conn))
            (cl-letf (((symbol-function 'make-network-process)
                       (lambda (&rest _) replacement))
                      ((symbol-function 'run-at-time) (lambda (&rest _) nil)))
              (clatter-connect "testnet" :server "irc.example" :tls nil))
            (should (eq (clatter-connection-process conn) replacement))
            (should-not (clatter-connection-recv-buffer conn))
            (should (zerop (clatter-connection-recv-tail-bytes conn)))
            (should-not (clatter-connection-deferred-batches conn))
            (should (zerop (clatter-connection-batch-bytes conn)))
            (should (zerop (clatter-connection-batch-message-count conn)))
            (should (zerop (clatter-connection-batch-count conn)))
            (should (zerop (hash-table-count
                           (clatter-connection-pending-labels conn))))
            (should-not (clatter-connection--motd-lines conn))
            (should-not (clatter-connection--whois-data conn))
            (should-not (clatter-connection-pending-whox conn)))
        (delete-process replacement)))))

;;; test-connection.el ends here
