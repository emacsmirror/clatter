;;; test-sts.el --- Tests for clatter-sts.el -*- lexical-binding: t; -*-

;;; Code:

(require 'test-helper)
(require 'clatter-sts)

;; --- STS value parsing ---

(ert-deftest clatter-test-sts-parse-value ()
  "Parse STS capability value string."
  (let ((result (clatter-sts--parse-value "port=6697,duration=2592000")))
    (should (= (plist-get result :port) 6697))
    (should (= (plist-get result :duration) 2592000))))

(ert-deftest clatter-test-sts-parse-value-port-only ()
  "Parse STS value with only port."
  (let ((result (clatter-sts--parse-value "port=6697")))
    (should (= (plist-get result :port) 6697))
    (should-not (plist-get result :duration))))

(ert-deftest clatter-test-sts-parse-value-duration-only ()
  "Parse STS value with only duration."
  (let ((result (clatter-sts--parse-value "duration=86400")))
    (should-not (plist-get result :port))
    (should (= (plist-get result :duration) 86400))))

;; --- STS CAP check ---

(ert-deftest clatter-test-sts-check-plaintext-upgrade ()
  "STS on plaintext triggers upgrade."
  (let ((clatter-sts-enable t))
    (let ((result (clatter-sts-check-cap
                   "server-time sts=port=6697,duration=2592000 multi-prefix"
                   "irc.example.com" nil)))
      (should result)
      (should (eq (plist-get result :action) 'upgrade))
      (should (= (plist-get result :port) 6697)))))

(ert-deftest clatter-test-sts-check-tls-stores-policy ()
  "STS on TLS stores policy."
  (let ((clatter-sts-enable t)
        (clatter-sts--policies (make-hash-table :test 'equal)))
    (let ((result (clatter-sts-check-cap
                   "server-time sts=port=6697,duration=2592000"
                   "irc.example.com" t)))
      (should result)
      (should (eq (plist-get result :action) 'store))
      ;; Policy should be persisted in memory
      (should (gethash "irc.example.com" clatter-sts--policies)))))

(ert-deftest clatter-test-sts-check-no-sts ()
  "No STS in caps returns nil."
  (let ((clatter-sts-enable t))
    (should-not (clatter-sts-check-cap
                 "server-time multi-prefix away-notify"
                 "irc.example.com" nil))))

(ert-deftest clatter-test-sts-check-disabled ()
  "STS check returns nil when disabled."
  (let ((clatter-sts-enable nil))
    (should-not (clatter-sts-check-cap
                 "sts=port=6697,duration=86400"
                 "irc.example.com" nil))))

;; --- Policy storage ---

(ert-deftest clatter-test-sts-store-and-lookup ()
  "Store and retrieve STS policy."
  (let ((clatter-sts--policies (make-hash-table :test 'equal)))
    (clatter-sts-store-policy "test.example.com" 6697 86400)
    (let ((policy (clatter-sts-lookup "test.example.com")))
      (should policy)
      (should (= (plist-get policy :port) 6697)))))

(ert-deftest clatter-test-sts-remove-policy ()
  "Remove STS policy."
  (let ((clatter-sts--policies (make-hash-table :test 'equal)))
    (clatter-sts-store-policy "test.example.com" 6697 86400)
    (clatter-sts-remove-policy "test.example.com")
    (should-not (clatter-sts-lookup "test.example.com"))))

(ert-deftest clatter-test-sts-expired-policy ()
  "Expired policy returns nil."
  (let ((clatter-sts--policies (make-hash-table :test 'equal)))
    ;; Store a policy that expired 1 second ago
    (puthash "test.example.com"
             (list :port 6697 :expiry (- (float-time) 1))
             clatter-sts--policies)
    (should-not (clatter-sts-lookup "test.example.com"))))

(ert-deftest clatter-test-sts-duration-zero-removes ()
  "STS duration=0 on TLS removes policy."
  (let ((clatter-sts-enable t)
        (clatter-sts--policies (make-hash-table :test 'equal)))
    (clatter-sts-store-policy "irc.example.com" 6697 86400)
    (clatter-sts-check-cap "sts=duration=0" "irc.example.com" t)
    (should-not (gethash "irc.example.com" clatter-sts--policies))))

(provide 'test-sts)

;;; test-sts.el ends here
