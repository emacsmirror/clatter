;;; clatter-sasl-scram.el --- SASL SCRAM-SHA-256 -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Glenn Thompson
;; Author: Glenn Thompson <glenn@paren.works>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; SASL SCRAM-SHA-256 implementation for clatter.el.
;; Implements RFC 5802 (SCRAM) with SHA-256 as the hash function.
;; This is the recommended SASL mechanism for Libera.Chat and modern
;; IRC networks.

;;; Code:

(require 'cl-lib)

;; --- HMAC-SHA256 ---

(defun clatter-scram--hmac-sha256 (key message)
  "Compute HMAC-SHA256 of MESSAGE with KEY.
KEY and MESSAGE are unibyte strings.  Returns unibyte string."
  (let* ((block-size 64)
         (key-padded (if (> (length key) block-size)
                         (secure-hash 'sha256 key nil nil t)
                       key))
         (key-len (length key-padded))
         (ipad (make-string block-size ?\x36))
         (opad (make-string block-size ?\x5c)))
    ;; XOR key into ipad and opad
    (dotimes (i key-len)
      (aset ipad i (logxor (aref key-padded i) ?\x36))
      (aset opad i (logxor (aref key-padded i) ?\x5c)))
    ;; HMAC = H(opad || H(ipad || message))
    (let ((inner (secure-hash 'sha256 (concat ipad message) nil nil t)))
      (secure-hash 'sha256 (concat opad inner) nil nil t))))

;; --- PBKDF2-SHA256 ---

(defun clatter-scram--pbkdf2-sha256 (password salt iterations)
  "Derive key using PBKDF2 with SHA-256.
PASSWORD and SALT are unibyte strings.  ITERATIONS is integer.
Returns 32-byte unibyte string (dkLen=32)."
  (let* ((u1 (clatter-scram--hmac-sha256
              password (concat salt (unibyte-string 0 0 0 1))))
         (result u1)
         (u-prev u1))
    (dotimes (_ (1- iterations))
      (let ((u-next (clatter-scram--hmac-sha256 password u-prev)))
        (setq result (clatter-scram--xor-strings result u-next))
        (setq u-prev u-next)))
    result))

(defun clatter-scram--xor-strings (a b)
  "XOR two equal-length unibyte strings A and B."
  (let* ((len (length a))
         (result (make-string len 0)))
    (dotimes (i len)
      (aset result i (logxor (aref a i) (aref b i))))
    result))

;; --- Nonce generation ---

(defun clatter-scram--generate-nonce ()
  "Generate a random nonce for SCRAM authentication."
  (let ((chars "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
        (result ""))
    (dotimes (_ 24)
      (setq result (concat result
                           (char-to-string
                            (aref chars (random (length chars)))))))
    result))

;; --- SASLprep (simplified) ---

(defun clatter-scram--saslprep (str)
  "Simplified SASLprep normalization of STR.
Full SASLprep (RFC 4013) is complex; this handles the common case."
  (encode-coding-string str 'utf-8))

;; --- SCRAM State Machine ---

(cl-defstruct (clatter-scram-state (:constructor clatter-scram-state--create))
  "State for an ongoing SCRAM-SHA-256 exchange."
  username
  password
  client-nonce
  client-first-bare
  server-first
  auth-message
  salted-password)

(defun clatter-scram-client-first (username password)
  "Create SCRAM client-first-message for USERNAME and PASSWORD.
Returns (STATE . BASE64-MESSAGE) cons."
  (let* ((nonce (clatter-scram--generate-nonce))
         (bare (format "n=%s,r=%s" username nonce))
         (full (concat "n,," bare))
         (state (clatter-scram-state--create
                 :username username
                 :password (clatter-scram--saslprep password)
                 :client-nonce nonce
                 :client-first-bare bare)))
    (cons state (base64-encode-string full t))))

(defun clatter-scram-client-final (state server-first-b64)
  "Process server-first-message and produce client-final-message.
STATE is the SCRAM state from client-first.
SERVER-FIRST-B64 is the base64-encoded server response.
Returns BASE64-MESSAGE string, or signals error on failure."
  (let* ((server-first (decode-coding-string
                        (base64-decode-string server-first-b64) 'utf-8))
         (attrs (clatter-scram--parse-attributes server-first))
         (server-nonce (cdr (assoc ?r attrs)))
         (salt-b64 (cdr (assoc ?s attrs)))
         (iterations (string-to-number (cdr (assoc ?i attrs)))))
    ;; Validate server nonce starts with our client nonce
    (unless (string-prefix-p (clatter-scram-state-client-nonce state)
                             server-nonce)
      (error "SCRAM: server nonce does not start with client nonce"))
    (unless (and salt-b64 (> iterations 0))
      (error "SCRAM: invalid server-first-message"))
    (let* ((salt (base64-decode-string salt-b64))
           (salted-password (clatter-scram--pbkdf2-sha256
                             (clatter-scram-state-password state)
                             salt iterations))
           (client-key (clatter-scram--hmac-sha256
                        salted-password "Client Key"))
           (stored-key (secure-hash 'sha256 client-key nil nil t))
           (channel-binding (base64-encode-string "n,," t))
           (client-final-without-proof
            (format "c=%s,r=%s" channel-binding server-nonce))
           (auth-message (concat (clatter-scram-state-client-first-bare state)
                                 "," server-first
                                 "," client-final-without-proof))
           (client-signature (clatter-scram--hmac-sha256
                              stored-key auth-message))
           (client-proof (clatter-scram--xor-strings
                          client-key client-signature))
           (proof-b64 (base64-encode-string client-proof t))
           (client-final (format "%s,p=%s"
                                 client-final-without-proof proof-b64)))
      ;; Store for server verification
      (setf (clatter-scram-state-salted-password state) salted-password)
      (setf (clatter-scram-state-server-first state) server-first)
      (setf (clatter-scram-state-auth-message state) auth-message)
      (base64-encode-string client-final t))))

(defun clatter-scram-verify-server (state server-final-b64)
  "Verify server-final-message in SERVER-FINAL-B64 against STATE.
Returns t if server proof is valid, nil otherwise."
  (let* ((server-final (decode-coding-string
                        (base64-decode-string server-final-b64) 'utf-8))
         (attrs (clatter-scram--parse-attributes server-final))
         (verifier-b64 (cdr (assoc ?v attrs))))
    (when verifier-b64
      (let* ((server-key (clatter-scram--hmac-sha256
                          (clatter-scram-state-salted-password state)
                          "Server Key"))
             (server-signature (clatter-scram--hmac-sha256
                                server-key
                                (clatter-scram-state-auth-message state)))
             (expected-b64 (base64-encode-string server-signature t)))
        (string= verifier-b64 expected-b64)))))

;; --- Attribute parsing ---

(defun clatter-scram--parse-attributes (msg)
  "Parse SCRAM attribute=value pairs from MSG.
Returns alist of (CHAR . VALUE) pairs."
  (let ((parts (split-string msg ","))
        (result nil))
    (dolist (part parts)
      (when (>= (length part) 2)
        (let ((key (aref part 0))
              (val (substring part 2)))
          (push (cons key val) result))))
    (nreverse result)))

(provide 'clatter-sasl-scram)

;;; clatter-sasl-scram.el ends here
