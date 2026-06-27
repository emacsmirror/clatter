;;; test-cap.el --- Tests for clatter-cap.el -*- lexical-binding: t; -*-

;;; Code:

(require 'test-helper)

(ert-deftest clatter-test-get-password-prefers-explicit-password ()
  "Explicit network :password wins over auth-source."
  (let ((clatter-networks
         '(("znc"
            :server "192.0.2.1"
            :port 7777
            :nick "testnick"
            :password "explicit"))))
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest _)
                 (list (list :secret "auth-source")))))
      (should (equal (clatter-get-password "znc") "explicit")))))

(ert-deftest clatter-test-get-password-searches-network-id-and-port ()
  "Auth-source lookup can match the Clatter network id and port."
  (let ((clatter-networks
         '(("znc"
            :server "192.0.2.1"
            :port 7777
            :nick "testnick")))
        calls)
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest plist)
                 (push plist calls)
                 (when (and (equal (plist-get plist :host) "znc")
                            (equal (plist-get plist :port) "7777")
                            (equal (plist-get plist :user) "testnick"))
                   (list (list :secret "from-network-id"))))))
      (should (equal (clatter-get-password "znc") "from-network-id"))
      (should calls))))

(ert-deftest clatter-test-get-password-uses-default-port ()
  "Auth-source lookup includes `clatter-default-port' when no port is set."
  (let ((clatter-networks
         '(("znc"
            :server "znc.example"
            :nick "testnick")))
        (clatter-default-port 6697))
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest plist)
                 (when (and (equal (plist-get plist :host) "znc.example")
                            (equal (plist-get plist :port) "6697")
                            (equal (plist-get plist :user) "testnick"))
                   (list (list :secret "from-default-port"))))))
      (should (equal (clatter-get-password "znc") "from-default-port")))))

(ert-deftest clatter-test-get-password-prefers-live-config-password ()
  "Explicit :password from a live config wins over saved network config."
  (let ((clatter-networks
         '(("znc"
            :server "znc.example"
            :nick "testnick"
            :password "saved"))))
    (should (equal (clatter-get-password
                    "znc"
                    '(:server "temporary.example"
                      :nick "testnick"
                      :password "live"))
                   "live"))))

(ert-deftest clatter-test-registration-sends-auth-source-pass ()
  "Registration sends PASS from auth-source before NICK and USER."
  (let* ((clatter-networks
          '(("znc"
             :server "znc.example"
             :nick "testnick")))
         (conn (clatter-test-make-connection "znc" "testnick"))
         (proc (make-pipe-process :name "clatter-test-cap"
                                  :buffer nil)))
    (unwind-protect
        (progn
          (setf (clatter-connection-process conn) proc)
          (process-put proc :clatter-config
                       '(:username "testnick" :realname "Test User"))
          (cl-letf (((symbol-function 'auth-source-search)
                     (lambda (&rest _)
                       (list (list :secret "znc-password")))))
            (clatter-test-with-mock-send
             (clatter-cap--send-registration conn)
             (should (equal (nreverse clatter-test--sent-lines)
                            '("PASS znc-password"
                              "NICK testnick"
                              "USER testnick 0 * :Test User"))))))
      (delete-process proc)
      (clatter-test-cleanup))))

(ert-deftest clatter-test-registration-prefers-live-config-password ()
  "Registration sends PASS from the live process config when present."
  (let* ((clatter-networks
          '(("znc"
             :server "znc.example"
             :nick "testnick"
             :password "saved")))
         (conn (clatter-test-make-connection "znc" "testnick"))
         (proc (make-pipe-process :name "clatter-test-cap"
                                  :buffer nil)))
    (unwind-protect
        (progn
          (setf (clatter-connection-process conn) proc)
          (process-put proc :clatter-config
                       '(:server "temporary.example"
                         :nick "testnick"
                         :username "testnick"
                         :realname "Test User"
                         :password "live"))
          (clatter-test-with-mock-send
           (clatter-cap--send-registration conn)
           (should (equal (car (nreverse clatter-test--sent-lines))
                          "PASS live"))))
      (delete-process proc)
      (clatter-test-cleanup))))

(provide 'test-cap)
;;; test-cap.el ends here
