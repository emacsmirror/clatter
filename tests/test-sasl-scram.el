;;; test-sasl-scram.el --- Tests for SCRAM-SHA-256 -*- lexical-binding: t; -*-

;;; Code:

(require 'test-helper)
(require 'clatter-sasl-scram)

;; --- HMAC-SHA256 ---

(ert-deftest clatter-test-hmac-sha256-basic ()
  "HMAC-SHA256 produces 32-byte result."
  (let ((result (clatter-scram--hmac-sha256 "key" "message")))
    (should (stringp result))
    (should (= (length result) 32))))

(ert-deftest clatter-test-hmac-sha256-deterministic ()
  "Same inputs produce same HMAC."
  (let ((r1 (clatter-scram--hmac-sha256 "secret" "data"))
        (r2 (clatter-scram--hmac-sha256 "secret" "data")))
    (should (equal r1 r2))))

(ert-deftest clatter-test-hmac-sha256-varies ()
  "Different keys produce different HMACs."
  (let ((r1 (clatter-scram--hmac-sha256 "key1" "data"))
        (r2 (clatter-scram--hmac-sha256 "key2" "data")))
    (should-not (equal r1 r2))))

;; --- PBKDF2-SHA256 ---

(ert-deftest clatter-test-pbkdf2-basic ()
  "PBKDF2 produces 32-byte key."
  (let ((result (clatter-scram--pbkdf2-sha256 "password" "salt" 1)))
    (should (stringp result))
    (should (= (length result) 32))))

(ert-deftest clatter-test-pbkdf2-iterations-matter ()
  "Different iteration counts produce different keys."
  (let ((r1 (clatter-scram--pbkdf2-sha256 "pass" "salt" 1))
        (r2 (clatter-scram--pbkdf2-sha256 "pass" "salt" 2)))
    (should-not (equal r1 r2))))

;; --- XOR strings ---

(ert-deftest clatter-test-xor-strings ()
  "XOR of identical strings is all zeros."
  (let ((s "abcd"))
    (should (equal (clatter-scram--xor-strings s s)
                   (make-string 4 0)))))

(ert-deftest clatter-test-xor-strings-identity ()
  "XOR with zero string returns original."
  (let ((s "test")
        (z (make-string 4 0)))
    (should (equal (clatter-scram--xor-strings s z) s))))

;; --- Nonce ---

(ert-deftest clatter-test-nonce-length ()
  "Generated nonce is 24 characters."
  (should (= (length (clatter-scram--generate-nonce)) 24)))

(ert-deftest clatter-test-nonce-unique ()
  "Successive nonces differ."
  (should-not (equal (clatter-scram--generate-nonce)
                     (clatter-scram--generate-nonce))))

;; --- Attribute parsing ---

(ert-deftest clatter-test-parse-attributes ()
  "Parse SCRAM attribute string."
  (let ((attrs (clatter-scram--parse-attributes "r=nonce123,s=c2FsdA==,i=4096")))
    (should (equal (cdr (assoc ?r attrs)) "nonce123"))
    (should (equal (cdr (assoc ?s attrs)) "c2FsdA=="))
    (should (equal (cdr (assoc ?i attrs)) "4096"))))

;; --- Client-first message ---

(ert-deftest clatter-test-client-first ()
  "Client-first produces state and base64 message."
  (let ((result (clatter-scram-client-first "user" "password")))
    (should (consp result))
    (should (clatter-scram-state-p (car result)))
    (should (stringp (cdr result)))
    ;; Base64 message should decode to something starting with "n,,"
    (let ((decoded (decode-coding-string
                    (base64-decode-string (cdr result)) 'utf-8)))
      (should (string-prefix-p "n,," decoded)))))

(ert-deftest clatter-test-client-first-contains-username ()
  "Client-first bare message contains the username."
  (let* ((result (clatter-scram-client-first "testuser" "pass"))
         (state (car result)))
    (should (string-match-p "n=testuser" (clatter-scram-state-client-first-bare state)))))

;; --- Full SCRAM round-trip (synthetic) ---

(ert-deftest clatter-test-scram-round-trip ()
  "Simulate a complete SCRAM exchange with known values."
  (let* ((username "user")
         (password "pencil")
         ;; Step 1: client-first
         (cf-result (clatter-scram-client-first username password))
         (state (car cf-result))
         (client-nonce (clatter-scram-state-client-nonce state))
         ;; Simulate server-first: server appends to our nonce
         (server-nonce (concat client-nonce "SERVERPART"))
         (salt (encode-coding-string "testsalt" 'utf-8))
         (salt-b64 (base64-encode-string salt t))
         (server-first (format "r=%s,s=%s,i=4096" server-nonce salt-b64))
         (server-first-b64 (base64-encode-string server-first t)))
    ;; Step 2: client-final
    (let ((client-final-b64 (clatter-scram-client-final state server-first-b64)))
      (should (stringp client-final-b64))
      ;; Should decode to something with c= and r= and p=
      (let ((decoded (decode-coding-string
                      (base64-decode-string client-final-b64) 'utf-8)))
        (should (string-match-p "c=" decoded))
        (should (string-match-p "r=" decoded))
        (should (string-match-p "p=" decoded))))
    ;; State should now have auth-message set
    (should (clatter-scram-state-auth-message state))
    (should (clatter-scram-state-salted-password state))))

(ert-deftest clatter-test-scram-bad-nonce ()
  "Server nonce not starting with client nonce signals error."
  (let* ((cf-result (clatter-scram-client-first "user" "pass"))
         (state (car cf-result))
         (server-first "r=TOTALLYDIFFERENT,s=c2FsdA==,i=4096")
         (server-first-b64 (base64-encode-string server-first t)))
    (should-error (clatter-scram-client-final state server-first-b64))))

(provide 'test-sasl-scram)

;;; test-sasl-scram.el ends here
