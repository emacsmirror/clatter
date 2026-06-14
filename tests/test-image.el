;;; test-image.el --- Tests for clatter-image URL scanning -*- lexical-binding: t; -*-

;;; Commentary:

;; Regression tests for inline image URL detection and scanning.

;;; Code:

(require 'ert)
(require 'clatter-image)

(ert-deftest clatter-image-url-p-basic ()
  "Direct image links are recognized, non-image links are not."
  (should (clatter-image--url-p "https://example.com/a.png"))
  (should (clatter-image--url-p "http://example.com/path/photo.JPG"))
  (should-not (clatter-image--url-p "https://example.com/page.html"))
  (should-not (clatter-image--url-p "https://example.com/")))

(ert-deftest clatter-image-url-p-query-and-fragment ()
  "Query strings and fragments are stripped before checking the extension."
  (should (clatter-image--url-p "https://example.com/a.png?size=large"))
  (should (clatter-image--url-p "https://example.com/a.png#section"))
  (should (clatter-image--url-p "https://example.com/a.png?v=2#x")))

(ert-deftest clatter-image-extract-urls-basic ()
  "All URLs in a message are returned in order."
  (should (equal (clatter-image--extract-urls "see http://a.com/x.png here")
                 '("http://a.com/x.png")))
  (should (equal (clatter-image--extract-urls
                  "http://a.com/1.png and http://b.com/2.jpg")
                 '("http://a.com/1.png" "http://b.com/2.jpg")))
  (should (null (clatter-image--extract-urls "no links at all"))))

(ert-deftest clatter-image-extract-urls-terminates-with-query ()
  "A URL with a query string preceded by text must not loop forever.

This reproduces the freeze where `match-end' was read after
`clatter-image--url-p' clobbered the match data: the search position
reset behind the URL and the same URL matched endlessly.  We bound the
work with a timer so a regression fails fast instead of hanging the
test run."
  (let ((text "Check out this cool pic everyone: https://example.com/a.png?v=2")
        (result nil)
        (timed-out nil))
    (with-timeout (5 (setq timed-out t))
      (setq result (clatter-image--extract-urls text)))
    (should-not timed-out)
    (should (equal result '("https://example.com/a.png?v=2")))))

(ert-deftest clatter-image-extract-urls-multiple-with-queries ()
  "Several query-string URLs with leading text all terminate and return."
  (let ((text (concat "first look here https://a.com/1.png?x=1 "
                      "and also there https://b.com/2.jpg?y=2#frag "
                      "plus https://c.com/3.gif"))
        (result nil)
        (timed-out nil))
    (with-timeout (5 (setq timed-out t))
      (setq result (clatter-image--extract-urls text)))
    (should-not timed-out)
    (should (equal result
                   '("https://a.com/1.png?x=1"
                     "https://b.com/2.jpg?y=2#frag"
                     "https://c.com/3.gif")))))

(provide 'test-image)

;;; test-image.el ends here
