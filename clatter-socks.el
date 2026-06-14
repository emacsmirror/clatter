;;; clatter-socks.el --- SOCKS5 proxy support for clatter.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Glenn Thompson
;; Author: Glenn Thompson
;; License: MIT

;;; Commentary:

;; SOCKS5 (RFC 1928) proxy support for clatter.el, with username/password
;; authentication (RFC 1929).  Provides pure codec functions and a session
;; based asynchronous handshake driver that runs over an already-open process.
;; The CONNECT request always uses ATYP=domain (remote DNS / SOCKS5h), so the
;; proxy resolves the target hostname; this avoids DNS leaks and supports
;; .onion addresses.

;;; Code:

(require 'cl-lib)
(require 'auth-source)
(require 'clatter-config)

;; --- Codec (pure) ---

(defun clatter-socks--encode-greeting (methods)
  "Encode a SOCKS5 client greeting offering auth METHODS (list of bytes)."
  (apply #'unibyte-string 5 (length methods) methods))

(defun clatter-socks--parse-method-selection (bytes)
  "Return the method byte from a method-selection reply BYTES, or nil if short."
  (when (>= (length bytes) 2)
    (aref bytes 1)))

(defun clatter-socks--encode-auth (user pass)
  "Encode an RFC 1929 username/password auth request for USER and PASS."
  (let ((u (encode-coding-string (or user "") 'utf-8))
        (p (encode-coding-string (or pass "") 'utf-8)))
    (when (or (> (length u) 255) (> (length p) 255))
      (error "SOCKS5: username or password too long (max 255 bytes)"))
    (concat (unibyte-string 1 (length u)) u
            (unibyte-string (length p)) p)))

(defun clatter-socks--parse-auth-status (bytes)
  "Return :ok or :fail from an auth status reply BYTES, or nil if short."
  (when (>= (length bytes) 2)
    (if (= (aref bytes 1) 0) :ok :fail)))

(defun clatter-socks--encode-connect (host port)
  "Encode a SOCKS5 CONNECT request to HOST:PORT using ATYP=domain."
  (let ((h (encode-coding-string host 'utf-8)))
    (when (> (length h) 255)
      (error "SOCKS5: hostname too long (%d > 255 bytes)" (length h)))
    (concat (unibyte-string 5 1 0 3 (length h))
            h
            (unibyte-string (logand (ash port -8) 255) (logand port 255)))))

(defun clatter-socks--reply-length (bytes)
  "Total expected length of a SOCKS5 reply in BYTES.
Return nil if not enough bytes yet to know, or `invalid' for a bad ATYP."
  (when (>= (length bytes) 4)
    (pcase (aref bytes 3)
      (1 (+ 4 4 2))                                  ; IPv4
      (4 (+ 4 16 2))                                 ; IPv6
      (3 (when (>= (length bytes) 5)                 ; domain: header+len+name+port
           (+ 4 1 (aref bytes 4) 2)))
      (_ 'invalid))))

(defconst clatter-socks--rep-messages
  '((0 . "succeeded")
    (1 . "general SOCKS server failure")
    (2 . "connection not allowed by ruleset")
    (3 . "network unreachable")
    (4 . "host unreachable")
    (5 . "connection refused")
    (6 . "TTL expired")
    (7 . "command not supported")
    (8 . "address type not supported"))
  "Map of SOCKS5 REP reply codes to human-readable messages.")

(defun clatter-socks--rep-message (code)
  "Human-readable message for SOCKS5 reply CODE."
  (or (cdr (assq code clatter-socks--rep-messages))
      (format "unknown reply code %d" code)))

(provide 'clatter-socks)

;;; clatter-socks.el ends here
