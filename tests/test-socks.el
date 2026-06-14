;;; test-socks.el --- SOCKS5 tests for clatter.el -*- lexical-binding: t; -*-
;;; Commentary:
;; ERT tests for clatter-socks.el (SOCKS5 codec and handshake driver).
;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'clatter-socks)

;; --- Greeting ---

(ert-deftest clatter-socks-test-greeting-noauth ()
  (should (equal (clatter-socks--encode-greeting '(0))
                 (unibyte-string 5 1 0))))

(ert-deftest clatter-socks-test-greeting-userpass ()
  (should (equal (clatter-socks--encode-greeting '(0 2))
                 (unibyte-string 5 2 0 2))))

;; --- Method selection ---

(ert-deftest clatter-socks-test-method-incomplete ()
  (should (null (clatter-socks--parse-method-selection (unibyte-string 5)))))

(ert-deftest clatter-socks-test-method-noauth ()
  (should (= 0 (clatter-socks--parse-method-selection (unibyte-string 5 0)))))

(ert-deftest clatter-socks-test-method-userpass ()
  (should (= 2 (clatter-socks--parse-method-selection (unibyte-string 5 2)))))

(ert-deftest clatter-socks-test-method-rejected ()
  (should (= 255 (clatter-socks--parse-method-selection (unibyte-string 5 255)))))

;; --- Auth (RFC 1929) ---

(ert-deftest clatter-socks-test-encode-auth ()
  (should (equal (clatter-socks--encode-auth "ab" "xyz")
                 (concat (unibyte-string 1 2) "ab" (unibyte-string 3) "xyz"))))

(ert-deftest clatter-socks-test-encode-auth-too-long ()
  (should-error (clatter-socks--encode-auth (make-string 256 ?a) "x")))

(ert-deftest clatter-socks-test-auth-status ()
  (should (null (clatter-socks--parse-auth-status (unibyte-string 1))))
  (should (eq :ok (clatter-socks--parse-auth-status (unibyte-string 1 0))))
  (should (eq :fail (clatter-socks--parse-auth-status (unibyte-string 1 1)))))

;; --- CONNECT request ---

(ert-deftest clatter-socks-test-encode-connect ()
  ;; VER=5 CMD=1 RSV=0 ATYP=3 LEN host PORT(6697=0x1A29)
  (should (equal (clatter-socks--encode-connect "a.bc" 6697)
                 (concat (unibyte-string 5 1 0 3 4) "a.bc"
                         (unibyte-string #x1a #x29)))))

(ert-deftest clatter-socks-test-encode-connect-onion ()
  (let* ((h (concat (make-string 16 ?a) ".onion"))
         (out (clatter-socks--encode-connect h 6667)))
    (should (= (aref out 3) 3))                 ; domain ATYP
    (should (= (aref out 4) (length h)))        ; length byte
    (should (equal (substring out 5 (+ 5 (length h))) h))))

(ert-deftest clatter-socks-test-encode-connect-host-too-long ()
  (should-error (clatter-socks--encode-connect (make-string 256 ?a) 1)))

;; --- Reply length / parse ---

(ert-deftest clatter-socks-test-reply-length ()
  (should (= 10 (clatter-socks--reply-length (unibyte-string 5 0 0 1)))) ; IPv4
  (should (= 22 (clatter-socks--reply-length (unibyte-string 5 0 0 4)))) ; IPv6
  (should (= 11 (clatter-socks--reply-length (unibyte-string 5 0 0 3 4)))) ; domain len 4
  (should (null (clatter-socks--reply-length (unibyte-string 5 0 0 3)))) ; domain, no len yet
  (should (null (clatter-socks--reply-length (unibyte-string 5 0))))     ; too short
  (should (eq 'invalid (clatter-socks--reply-length (unibyte-string 5 0 0 9)))))

(ert-deftest clatter-socks-test-rep-message ()
  (should (equal "connection refused" (clatter-socks--rep-message 5)))
  (should (string-match-p "unknown" (clatter-socks--rep-message 42))))

;; --- Driver (session-based, no real process) ---

(defun clatter-socks-test--session (&rest overrides)
  "Make a test session capturing sent bytes and the terminal result.
Returns (cons session state-box) where state-box is a plist with
:sent (list, newest-first) and :result (:ok or (cons :fail reason))."
  (let* ((box (list :sent nil :result nil))
         (session (apply #'clatter-socks--session-create
                         :state :method :buffer (unibyte-string)
                         :host "irc.example.org" :port 6697
                         :send-fn (lambda (s) (push s (plist-get box :sent)))
                         :on-success (lambda () (plist-put box :result :ok))
                         :on-failure (lambda (r) (plist-put box :result (cons :fail r)))
                         overrides)))
    (cons session box)))

(defun clatter-socks-test--feed (session bytes)
  "Append BYTES to SESSION's buffer and advance the state machine."
  (setf (clatter-socks--session-buffer session)
        (concat (clatter-socks--session-buffer session) bytes))
  (clatter-socks--advance session))

(ert-deftest clatter-socks-test-driver-noauth-success ()
  (pcase-let ((`(,session . ,box) (clatter-socks-test--session)))
    (clatter-socks-test--feed session (unibyte-string 5 0))
    (should (eq (clatter-socks--session-state session) :reply))
    (should (equal (car (plist-get box :sent))
                   (clatter-socks--encode-connect "irc.example.org" 6697)))
    (clatter-socks-test--feed session (unibyte-string 5 0 0 1 0 0 0 0 0 0))
    (should (eq (plist-get box :result) :ok))))

(ert-deftest clatter-socks-test-driver-userpass-success ()
  (pcase-let ((`(,session . ,box)
               (clatter-socks-test--session :user "u" :pass "p")))
    (clatter-socks-test--feed session (unibyte-string 5 2))
    (should (eq (clatter-socks--session-state session) :auth))
    (should (equal (car (plist-get box :sent)) (clatter-socks--encode-auth "u" "p")))
    (clatter-socks-test--feed session (unibyte-string 1 0))
    (should (eq (clatter-socks--session-state session) :reply))
    (clatter-socks-test--feed session (unibyte-string 5 0 0 3 1 ?x 0 0))
    (should (eq (plist-get box :result) :ok))))

(ert-deftest clatter-socks-test-driver-auth-failure ()
  (pcase-let ((`(,session . ,box)
               (clatter-socks-test--session :user "u" :pass "p")))
    (clatter-socks-test--feed session (unibyte-string 5 2))
    (clatter-socks-test--feed session (unibyte-string 1 1))
    (should (equal (plist-get box :result) '(:fail . "proxy authentication failed")))))

(ert-deftest clatter-socks-test-driver-no-acceptable-method ()
  (pcase-let ((`(,session . ,box) (clatter-socks-test--session)))
    (clatter-socks-test--feed session (unibyte-string 5 255))
    (should (eq (car (plist-get box :result)) :fail))
    (should (string-match-p "no acceptable auth method"
                            (cdr (plist-get box :result))))))

(ert-deftest clatter-socks-test-driver-connect-refused ()
  (pcase-let ((`(,session . ,box) (clatter-socks-test--session)))
    (clatter-socks-test--feed session (unibyte-string 5 0))
    (clatter-socks-test--feed session (unibyte-string 5 5 0 1 0 0 0 0 0 0))
    (should (equal (plist-get box :result) '(:fail . "connection refused")))))

(ert-deftest clatter-socks-test-driver-partial-reply ()
  (pcase-let ((`(,session . ,box) (clatter-socks-test--session)))
    (clatter-socks-test--feed session (unibyte-string 5 0))
    (dolist (b '(5 0 0 1 0 0 0 0 0))
      (clatter-socks-test--feed session (unibyte-string b))
      (should (null (plist-get box :result))))
    (clatter-socks-test--feed session (unibyte-string 0))
    (should (eq (plist-get box :result) :ok))))

(ert-deftest clatter-socks-test-driver-bad-atyp ()
  (pcase-let ((`(,session . ,box) (clatter-socks-test--session)))
    (clatter-socks-test--feed session (unibyte-string 5 0))
    (clatter-socks-test--feed session (unibyte-string 5 0 0 9 0 0))
    (should (string-match-p "malformed" (cdr (plist-get box :result))))))

(ert-deftest clatter-socks-test-driver-no-reentry-after-terminal ()
  (pcase-let ((`(,session . ,box) (clatter-socks-test--session)))
    (clatter-socks-test--feed session (unibyte-string 5 0))
    (clatter-socks-test--feed session (unibyte-string 5 0 0 1 0 0 0 0 0 0))
    (should (eq (plist-get box :result) :ok))
    (should (eq (clatter-socks--session-state session) :done))
    ;; trailing bytes after a completed handshake must be ignored
    (clatter-socks-test--feed session (unibyte-string 1 2 3 4))
    (should (eq (plist-get box :result) :ok))
    (should (eq (clatter-socks--session-state session) :done))))

;; Hardening: explicit big-endian port packing (catches byte-swap regressions)
(ert-deftest clatter-socks-test-encode-connect-port-bytes ()
  (let ((out (clatter-socks--encode-connect "h" 256)))   ; 256 = 0x0100
    (should (equal (substring out (- (length out) 2)) (unibyte-string 1 0))))
  (let ((out (clatter-socks--encode-connect "h" 65535))) ; 0xFFFF
    (should (equal (substring out (- (length out) 2)) (unibyte-string 255 255)))))

;; --- Password helper ---

(ert-deftest clatter-socks-test-password-explicit ()
  (should (equal "sekret"
                 (clatter-socks--password '(:host "h" :user "u" :pass "sekret")))))

(ert-deftest clatter-socks-test-password-none-without-auth-source ()
  (let ((clatter-use-auth-source nil))
    (should (null (clatter-socks--password '(:host "h" :user "u"))))))

;; --- begin: greeting is sent and a session is stored on the process ---

(ert-deftest clatter-socks-test-begin-sends-greeting ()
  (let ((proc (make-network-process
               :name "clatter-socks-test" :server t :host 'local :service t)))
    (unwind-protect
        (let (sent)
          (cl-letf (((symbol-function 'process-send-string)
                     (lambda (_p s) (push s sent))))
            (clatter-socks-begin proc "irc.example.org" 6697
                                 '(:host "127.0.0.1" :port 1080)
                                 #'ignore #'ignore)
            ;; greeting sent, session stored, the real filter installed
            (should (equal (car (last sent)) (unibyte-string 5 1 0)))
            (should (clatter-socks--session-p (process-get proc :socks-session)))
            (should (eq (process-filter proc) #'clatter-socks--filter))
            ;; drive one step through the real filter: method selection -> CONNECT
            (funcall (process-filter proc) proc (unibyte-string 5 0))
            (should (equal (car sent)
                           (clatter-socks--encode-connect "irc.example.org" 6697)))))
      (delete-process proc))))

;; --- Proxy config resolution ---

(ert-deftest clatter-socks-test-config-tor-sugar ()
  (let ((clatter-proxy nil))
    (should (equal (clatter-proxy-config '(:tor t))
                   '(:type socks5 :host "127.0.0.1" :port 9050)))))

(ert-deftest clatter-socks-test-config-explicit-overrides-tor ()
  (let ((clatter-proxy nil)
        (p '(:type socks5 :host "10.0.0.1" :port 1080)))
    (should (equal (clatter-proxy-config (list :proxy p :tor t)) p))))

(ert-deftest clatter-socks-test-config-global-fallback ()
  (let ((clatter-proxy '(:type socks5 :host "g" :port 1)))
    (should (equal (clatter-proxy-config nil) clatter-proxy))))

(ert-deftest clatter-socks-test-config-direct ()
  (let ((clatter-proxy nil))
    (should (null (clatter-proxy-config '(:server "x"))))))

(ert-deftest clatter-socks-test-config-proxy-overrides-global ()
  (let ((clatter-proxy '(:type socks5 :host "g" :port 1))
        (p '(:type socks5 :host "10.0.0.1" :port 1080)))
    (should (equal (clatter-proxy-config (list :proxy p)) p))))

(ert-deftest clatter-socks-test-config-tor-overrides-global ()
  (let ((clatter-proxy '(:type socks5 :host "g" :port 1)))
    (should (equal (clatter-proxy-config '(:tor t))
                   '(:type socks5 :host "127.0.0.1" :port 9050)))))

;; --- Connection-level guards ---

(ert-deftest clatter-socks-test-external-tls-proxy-refused ()
  "Configuring a proxy with external TLS must error, not connect."
  (let ((clatter-tls-method 'external)
        (clatter-networks nil))
    (unwind-protect
        (should-error
         (clatter-connect "x-socks-test"
                          :server "irc.example.org" :port 6697 :tls t
                          :nick "n"
                          :proxy '(:type socks5 :host "127.0.0.1" :port 9050)))
      (clatter-test-cleanup))))

(ert-deftest clatter-socks-test-invalid-proxy-refused ()
  "A proxy missing :host or :port must error."
  (let ((clatter-tls-method 'builtin)
        (clatter-networks nil))
    (unwind-protect
        (should-error
         (clatter-connect "x-socks-test2"
                          :server "irc.example.org" :port 6697 :tls t
                          :nick "n"
                          :proxy '(:type socks5 :host "127.0.0.1")))
      (clatter-test-cleanup))))

;;; test-socks.el ends here
