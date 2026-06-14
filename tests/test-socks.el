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

;;; test-socks.el ends here
