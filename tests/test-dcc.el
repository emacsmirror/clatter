;;; test-dcc.el --- Tests for clatter-dcc -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-helper)
(require 'clatter-dcc)

;;; --- IP Encoding/Decoding ---

(ert-deftest clatter-test-dcc-decode-ip ()
  "Decode a standard DCC IP integer."
  (should (string= (clatter-dcc--decode-ip 3232235777) "192.168.1.1")))

(ert-deftest clatter-test-dcc-decode-ip-string ()
  "Decode a DCC IP integer passed as string."
  (should (string= (clatter-dcc--decode-ip "3232235777") "192.168.1.1")))

(ert-deftest clatter-test-dcc-decode-ip-localhost ()
  "Decode 127.0.0.1."
  (should (string= (clatter-dcc--decode-ip 2130706433) "127.0.0.1")))

(ert-deftest clatter-test-dcc-encode-ip ()
  "Encode a dotted-quad IP to DCC integer."
  (should (= (clatter-dcc--encode-ip "192.168.1.1") 3232235777)))

(ert-deftest clatter-test-dcc-encode-ip-roundtrip ()
  "Encode and decode should roundtrip."
  (should (string= (clatter-dcc--decode-ip (clatter-dcc--encode-ip "10.0.0.1"))
                    "10.0.0.1")))

;;; --- DCC SEND Parsing ---

(ert-deftest clatter-test-dcc-parse-send-basic ()
  "Parse a standard DCC SEND."
  (let ((result (clatter-dcc--parse-send "file.mkv 3232235777 4000 1048576")))
    (should result)
    (should (string= (plist-get result :filename) "file.mkv"))
    (should (string= (plist-get result :ip) "192.168.1.1"))
    (should (= (plist-get result :port) 4000))
    (should (= (plist-get result :size) 1048576))
    (should-not (plist-get result :token))))

(ert-deftest clatter-test-dcc-parse-send-quoted ()
  "Parse DCC SEND with quoted filename containing spaces."
  (let ((result (clatter-dcc--parse-send
                 "\"my file.mkv\" 3232235777 4000 1048576")))
    (should result)
    (should (string= (plist-get result :filename) "my file.mkv"))
    (should (= (plist-get result :port) 4000))))

(ert-deftest clatter-test-dcc-parse-send-passive ()
  "Parse passive DCC SEND with token."
  (let ((result (clatter-dcc--parse-send "file.mkv 0 0 1048576 abc123")))
    (should result)
    (should (= (plist-get result :port) 0))
    (should (string= (plist-get result :token) "abc123"))))

(ert-deftest clatter-test-dcc-parse-send-empty ()
  "Parse empty string returns nil."
  (should-not (clatter-dcc--parse-send "")))

(ert-deftest clatter-test-dcc-parse-send-nil ()
  "Parse nil returns nil."
  (should-not (clatter-dcc--parse-send nil)))

(ert-deftest clatter-test-dcc-parse-send-too-few-args ()
  "Parse with too few arguments returns nil."
  (should-not (clatter-dcc--parse-send "file.mkv 1234")))

;;; --- DCC RESUME Parsing ---

(ert-deftest clatter-test-dcc-parse-resume ()
  "Parse DCC RESUME arguments."
  (let ((result (clatter-dcc--parse-resume "file.mkv 4000 524288")))
    (should result)
    (should (string= (plist-get result :filename) "file.mkv"))
    (should (= (plist-get result :port) 4000))
    (should (= (plist-get result :position) 524288))))

(ert-deftest clatter-test-dcc-parse-resume-nil ()
  "Parse nil RESUME returns nil."
  (should-not (clatter-dcc--parse-resume nil)))

;;; --- Size Formatting ---

(ert-deftest clatter-test-dcc-format-size-bytes ()
  "Format small byte count."
  (should (string= (clatter-dcc--format-size 500) "500 B")))

(ert-deftest clatter-test-dcc-format-size-kb ()
  "Format kilobytes."
  (should (string= (clatter-dcc--format-size 2048) "2.0 KB")))

(ert-deftest clatter-test-dcc-format-size-mb ()
  "Format megabytes."
  (should (string= (clatter-dcc--format-size (* 1024 1024 5)) "5.0 MB")))

(ert-deftest clatter-test-dcc-format-size-gb ()
  "Format gigabytes."
  (should (string= (clatter-dcc--format-size (* 1024 1024 1024 2)) "2.0 GB")))

;;; --- Ack Generation ---

(ert-deftest clatter-test-dcc-ack-zero ()
  "Ack for 0 bytes."
  (should (equal (clatter-dcc--make-ack 0) (unibyte-string 0 0 0 0))))

(ert-deftest clatter-test-dcc-ack-small ()
  "Ack for 256 bytes."
  (should (equal (clatter-dcc--make-ack 256) (unibyte-string 0 0 1 0))))

(ert-deftest clatter-test-dcc-ack-1mb ()
  "Ack for 1 MB."
  (should (equal (clatter-dcc--make-ack (* 1024 1024))
                 (unibyte-string 0 #x10 0 0))))

(ert-deftest clatter-test-dcc-ack-4-bytes ()
  "Ack is always exactly 4 bytes."
  (should (= (length (clatter-dcc--make-ack 999999999)) 4)))

;;; --- Transfer Management ---

(ert-deftest clatter-test-dcc-new-id-increments ()
  "Transfer IDs increment."
  (let ((clatter-dcc--next-id 0))
    (should (= (clatter-dcc--new-id) 1))
    (should (= (clatter-dcc--new-id) 2))
    (should (= (clatter-dcc--new-id) 3))))

(ert-deftest clatter-test-dcc-register-and-get ()
  "Register and retrieve a transfer."
  (let ((clatter-dcc--transfers nil)
        (tr (clatter-dcc-transfer--create
             :id 42 :nick "bot" :filename "test.mkv")))
    (clatter-dcc--register tr)
    (should (eq (clatter-dcc--get-transfer 42) tr))))

(ert-deftest clatter-test-dcc-get-missing ()
  "Get nonexistent transfer returns nil."
  (let ((clatter-dcc--transfers nil))
    (should-not (clatter-dcc--get-transfer 999))))

(ert-deftest clatter-test-dcc-output-path ()
  "Output path places file in download directory."
  (let ((clatter-dcc-download-directory "/tmp/clatter-test-dcc/"))
    (when (file-directory-p clatter-dcc-download-directory)
      (delete-directory clatter-dcc-download-directory t))
    (unwind-protect
        (let ((path (clatter-dcc--output-path "test.mkv")))
          (should (string-match-p "test\\.mkv\\'" path))
          (should (string-prefix-p "/tmp/clatter-test-dcc/" path)))
      (when (file-directory-p "/tmp/clatter-test-dcc/")
        (delete-directory "/tmp/clatter-test-dcc/" t)))))

(ert-deftest clatter-test-dcc-output-path-no-overwrite ()
  "Output path avoids overwriting existing files."
  (let ((clatter-dcc-download-directory "/tmp/clatter-test-dcc/"))
    (when (file-directory-p clatter-dcc-download-directory)
      (delete-directory clatter-dcc-download-directory t))
    (make-directory "/tmp/clatter-test-dcc/" t)
    (unwind-protect
        (progn
          ;; Create existing file
          (write-region "" nil "/tmp/clatter-test-dcc/test.mkv" nil 'silent)
          (let ((path (clatter-dcc--output-path "test.mkv")))
            (should (string-match-p "test_1\\.mkv\\'" path))))
      (when (file-directory-p "/tmp/clatter-test-dcc/")
        (delete-directory "/tmp/clatter-test-dcc/" t)))))

;;; --- Transfer State ---

(ert-deftest clatter-test-dcc-reject ()
  "Rejecting sets state to cancelled."
  (let ((tr (clatter-dcc-transfer--create
             :id 1 :nick "bot" :filename "f.mkv" :state :pending)))
    (clatter-dcc-reject tr)
    (should (eq (clatter-dcc-transfer-state tr) :cancelled))))

(ert-deftest clatter-test-dcc-complete-idempotent ()
  "Completing a transfer is idempotent."
  (let ((clatter-dcc--transfers nil)
        (tr (clatter-dcc-transfer--create
             :id 1 :nick "bot" :filename "f.mkv"
             :size 100 :received 100 :state :active)))
    (clatter-dcc--register tr)
    (clatter-dcc--complete tr)
    (should (eq (clatter-dcc-transfer-state tr) :complete))
    ;; Calling again should not error
    (clatter-dcc--complete tr)
    (should (eq (clatter-dcc-transfer-state tr) :complete))))

(ert-deftest clatter-test-dcc-fail-sets-state ()
  "Failing a transfer sets state to failed."
  (let ((clatter-dcc--transfers nil)
        (tr (clatter-dcc-transfer--create
             :id 1 :nick "bot" :filename "f.mkv" :state :active)))
    (clatter-dcc--register tr)
    (clatter-dcc--fail tr "connection lost")
    (should (eq (clatter-dcc-transfer-state tr) :failed))))

(ert-deftest clatter-test-dcc-fail-no-override-complete ()
  "Failing does not override a completed transfer."
  (let ((clatter-dcc--transfers nil)
        (tr (clatter-dcc-transfer--create
             :id 1 :nick "bot" :filename "f.mkv" :state :complete)))
    (clatter-dcc--register tr)
    (clatter-dcc--fail tr "oops")
    (should (eq (clatter-dcc-transfer-state tr) :complete))))

;;; --- Format Progress ---

(ert-deftest clatter-test-dcc-format-progress-known-size ()
  "Progress with known file size shows percentage."
  (let ((tr (clatter-dcc-transfer--create
             :id 1 :filename "f.mkv" :size (* 1024 1024)
             :received (* 512 1024))))
    (let ((str (clatter-dcc--format-progress tr)))
      (should (string-match-p "50%" str)))))

(ert-deftest clatter-test-dcc-format-progress-unknown-size ()
  "Progress with unknown file size shows ?."
  (let ((tr (clatter-dcc-transfer--create
             :id 1 :filename "f.mkv" :size 0 :received 1024)))
    (let ((str (clatter-dcc--format-progress tr)))
      (should (string-match-p "\\?" str)))))

(provide 'test-dcc)
;;; test-dcc.el ends here
