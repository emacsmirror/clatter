;;; test-pals.el --- Tests for pals and fools lists -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for clatter-pals.el: membership, add/remove helpers, muting,
;; and pal nick-face highlighting.

;;; Code:

(require 'ert)
(require 'clatter-pals)
(require 'clatter-hl-nicks)

(ert-deftest clatter-pals-membership-case-insensitive ()
  (let ((clatter-pals '("Alice" "bob")))
    (should (clatter-pal-p "alice"))
    (should (clatter-pal-p "ALICE"))
    (should (clatter-pal-p "Bob"))
    (should-not (clatter-pal-p "carol"))
    (should-not (clatter-pal-p nil))))

(ert-deftest clatter-fools-membership-case-insensitive ()
  (let ((clatter-fools '("knighthk")))
    (should (clatter-fool-p "KnightHK"))
    (should-not (clatter-fool-p "someone"))))

(ert-deftest clatter-nick-list-add-idempotent ()
  (should (equal (clatter--nick-list-add "alice" '("alice")) '("alice")))
  (should (equal (clatter--nick-list-add "ALICE" '("alice")) '("alice")))
  (should (equal (clatter--nick-list-add "bob" '("alice")) '("bob" "alice"))))

(ert-deftest clatter-nick-list-remove-case-insensitive ()
  (should (equal (clatter--nick-list-remove "ALICE" '("alice" "bob")) '("bob")))
  (should (equal (clatter--nick-list-remove "carol" '("alice" "bob"))
                 '("alice" "bob"))))

(ert-deftest clatter-muted-combines-ignore-and-fools ()
  (let ((clatter-ignore-list '("spammer"))
        (clatter-fools '("knighthk")))
    (should (clatter-muted-p "spammer"))
    (should (clatter-muted-p "KnightHK"))
    (should-not (clatter-muted-p "friend"))))

(ert-deftest clatter-pal-gets-pal-face ()
  "A pal's nick maps to the `clatter-pal' face; others to a palette face."
  (let ((clatter-pals '("alice")))
    (should (eq (clatter-hl-nick-face-symbol "alice") 'clatter-pal))
    (should (eq (clatter-hl-nick-face-symbol "ALICE") 'clatter-pal))
    (should-not (eq (clatter-hl-nick-face-symbol "bob") 'clatter-pal))
    (should (facep (clatter-hl-nick-face-symbol "bob")))))

(provide 'test-pals)

;;; test-pals.el ends here
