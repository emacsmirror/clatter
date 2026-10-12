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

;;; test-connection.el ends here
