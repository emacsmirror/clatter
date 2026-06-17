;;; test-pr-integration.el --- Tests for integrated PR features -*- lexical-binding: t; -*-

;;; Commentary:

;; Regression and feature tests for the batch of PRs (#7-#24) integrated
;; into clatter.el: /list args, nicklist multi-prefix parsing, RPL_WHOISBOT,
;; channel INVITE, RPL_TOPIC/TOPICWHOTIME, case-preserving nick lists,
;; /say, and informational/MODE numerics.

;;; Code:

(require 'test-helper)
(require 'clatter-ui)
(require 'clatter-list)
(require 'clatter-commands)

;; --- #15: NAMES parsing with multi-prefix and userhost-in-names ---

(ert-deftest clatter-test-parse-names-single-prefix ()
  "Parse NAMES entries with a single prefix character."
  (should (equal (clatter-parse-names "@alice +bob carol")
                 '(("alice" . "@") ("bob" . "+") ("carol" . "")))))

(ert-deftest clatter-test-parse-names-multi-prefix ()
  "Parse NAMES entries carrying multiple stacked prefixes."
  (should (equal (clatter-parse-names "@+alice ~&bob")
                 '(("alice" . "@+") ("bob" . "~&")))))

(ert-deftest clatter-test-parse-names-userhost-in-names ()
  "Strip the user@host portion when userhost-in-names is active."
  (should (equal (clatter-parse-names "@alice!~a@host bob!b@h")
                 '(("alice" . "@") ("bob" . "")))))

(ert-deftest clatter-test-parse-names-custom-prefixes ()
  "Honour an explicit PREFIXES ranking string."
  ;; With only "@" recognised, "+" is treated as part of the nick.
  (should (equal (clatter-parse-names "@alice +bob" "@")
                 '(("alice" . "@") ("+bob" . "")))))

;; --- #20: case-preserving nick list ---

(ert-deftest clatter-test-nick-list-preserves-case ()
  "Nick list keeps the original case while keying case-insensitively."
  (clatter-test-with-buffer
    (setq-local clatter--nick-list (make-hash-table :test 'equal))
    (clatter-nick-add (current-buffer) "AliceB" "@")
    (clatter-nick-add (current-buffer) "bOb" "")
    ;; gethash uses downcased key, value is (prefix . original-nick)
    (should (equal (gethash "aliceb" clatter--nick-list) '("@" . "AliceB")))
    ;; clatter-nick-list returns (original-nick . prefix) pairs
    (should (equal (clatter-nick-list (current-buffer))
                   '(("AliceB" . "@") ("bOb" . ""))))))

(ert-deftest clatter-test-nick-rename-preserves-case ()
  "Renaming a nick preserves the new nick's case and its prefix."
  (clatter-test-with-buffer
    (setq-local clatter--nick-list (make-hash-table :test 'equal))
    (clatter-nick-add (current-buffer) "Alice" "@")
    (clatter-nick-rename (current-buffer) "Alice" "AliceAway")
    (should-not (gethash "alice" clatter--nick-list))
    (should (equal (gethash "aliceaway" clatter--nick-list)
                   '("@" . "AliceAway")))))

;; --- #14: channel INVITE ---

(ert-deftest clatter-test-dispatch-invite ()
  "INVITE dispatches to clatter-invite-hook with sender, nick, channel."
  (let ((conn (clatter-test-make-connection "testnet" "testnick")))
    (unwind-protect
        (let ((calls (clatter-test-capture-hook clatter-invite-hook
                       (clatter-dispatch-message
                        conn (clatter-test-parse
                              ":bob!~b@host INVITE testnick #secret")))))
          (should (= (length calls) 1))
          (let ((args (car calls)))
            (should (equal (nth 1 args) '("bob" "~b" "host")))
            (should (equal (nth 2 args) "testnick"))
            (should (equal (nth 3 args) "#secret"))))
      (clatter-test-cleanup))))

;; --- #9: RPL_WHOISBOT (335) ---

(ert-deftest clatter-test-whoisbot-sets-bot-flag ()
  "RPL_WHOISBOT (335) marks the current whois data as a bot."
  (let ((conn (clatter-test-make-connection)))
    (unwind-protect
        (progn
          (setf (clatter-connection--whois-data conn) (list :nick "botnick"))
          (clatter-dispatch-message
           conn (clatter-test-parse
                 ":server 335 testnick botnick :is a bot"))
          (should (eq (plist-get (clatter-connection--whois-data conn) :bot) t)))
      (clatter-test-cleanup))))

;; --- #18: RPL_TOPIC / RPL_TOPICWHOTIME ---

(ert-deftest clatter-test-topicwhotime-dispatch ()
  "RPL_TOPICWHOTIME (333) dispatches topic-hook with setter and time."
  (let ((conn (clatter-test-make-connection)))
    (unwind-protect
        (let ((calls (clatter-test-capture-hook clatter-topic-hook
                       (clatter-dispatch-message
                        conn (clatter-test-parse
                              ":server 333 testnick #emacs setter!x@host 1700000000")))))
          (should (= (length calls) 1))
          (let ((args (car calls)))
            (should (equal (nth 1 args) "#emacs"))
            (should (equal (nth 2 args) '("setter" "x" "host")))
            (should (equal (nth 4 args) 1700000000))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-topicwhotime-missing-time ()
  "RPL_TOPICWHOTIME without a timestamp does not error (nil time)."
  (let ((conn (clatter-test-make-connection)))
    (unwind-protect
        (let ((calls (clatter-test-capture-hook clatter-topic-hook
                       (clatter-dispatch-message
                        conn (clatter-test-parse
                              ":server 333 testnick #emacs setter")))))
          (should (= (length calls) 1))
          (should-not (nth 4 (car calls))))
      (clatter-test-cleanup))))

;; --- #7: /list optional argument ---

(ert-deftest clatter-test-list-request-no-arg ()
  "LIST without an argument sends a bare LIST."
  (let ((conn (clatter-test-make-connection)))
    (clatter-test-with-mock-send
     (clatter-list-request conn)
     (should (equal (clatter-test-last-sent) "LIST")))))

(ert-deftest clatter-test-list-request-empty-arg ()
  "LIST with an empty-string argument sends a bare LIST."
  (let ((conn (clatter-test-make-connection)))
    (clatter-test-with-mock-send
     (clatter-list-request conn "")
     (should (equal (clatter-test-last-sent) "LIST")))))

(ert-deftest clatter-test-list-request-with-arg ()
  "LIST with an argument forwards it to the server."
  (let ((conn (clatter-test-make-connection)))
    (clatter-test-with-mock-send
     (clatter-list-request conn ">100")
     (should (equal (clatter-test-last-sent) "LIST >100")))))

;; --- #22: /say command ---

(ert-deftest clatter-test-cmd-say-sends-text ()
  "/say forwards trimmed text via clatter--send-message."
  (let ((sent nil))
    (cl-letf (((symbol-function 'clatter--send-message)
               (lambda (text) (setq sent text))))
      (clatter-cmd-say "  hello world  ")
      (should (equal sent "hello world")))))

(ert-deftest clatter-test-cmd-say-ignores-empty ()
  "/say with only whitespace sends nothing."
  (let ((called nil))
    (cl-letf (((symbol-function 'clatter--send-message)
               (lambda (_text) (setq called t))))
      (clatter-cmd-say "   ")
      (should-not called))))

;; --- #24: informational / MODE numerics ---

(ert-deftest clatter-test-numeric-umodeis ()
  "RPL_UMODEIS (221) inserts the user mode line into the server buffer."
  (let* ((conn (clatter-test-make-connection "testnet" "testnick"))
         (buf (clatter-get-or-create-buffer "testnet" "*server*" 'server)))
    (unwind-protect
        (progn
          (clatter-ui--on-numeric conn "221" '("testnick" "+iw"))
          (with-current-buffer buf
            (should (string-match-p "testnick is \\+iw" (buffer-string)))))
      (kill-buffer buf)
      (clatter-remove-buffer "testnet" "*server*")
      (clatter-test-cleanup))))

(ert-deftest clatter-test-numeric-no-server-buffer ()
  "Numeric handling is a no-op (no error) when no server buffer exists."
  (let ((conn (clatter-test-make-connection "no-buf-net" "testnick")))
    (unwind-protect
        (should-not (clatter-ui--on-numeric conn "221" '("testnick" "+iw")))
      (clatter-test-cleanup))))

(provide 'test-pr-integration)

;;; test-pr-integration.el ends here
