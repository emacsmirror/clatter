;;; test-feed.el --- Tests for clatter-feed.el -*- lexical-binding: t; -*-

;;; Code:

(require 'test-helper)
(require 'clatter-ui)
(require 'clatter-pals)
(require 'clatter-feed)

;;; Helpers

(defun clatter-test-feed--buffer ()
  "Return the live feed buffer, or nil."
  (when-let* ((buf (get-buffer clatter-feed--buffer-name))
              ((buffer-live-p buf)))
    buf))

(defun clatter-test-feed--kill-buffer ()
  "Kill the feed buffer if it exists."
  (when-let* ((buf (get-buffer clatter-feed--buffer-name))
              ((buffer-live-p buf)))
    (let ((kill-buffer-query-functions nil))
      (kill-buffer buf))))

(defmacro clatter-test-feed--with-capture (conn &rest body)
  "Run BODY with CONN bound to a mock connection, then clean up.

Removes the feed buffer and any clatter buffers BODY created, and
unregisters the feed hooks whether or not BODY enabled them."
  (declare (indent 1))
  `(let ((initial-buffers clatter--buffer-alist)
         (,conn (clatter-test-make-connection)))
     (clatter-test-feed--kill-buffer)
     (unwind-protect
         (progn ,@body)
       (clatter-feed-disable)
       (clatter-test-feed--kill-buffer)
       (dolist (entry clatter--buffer-alist)
         (when (and (not (memq entry initial-buffers))
                    (buffer-live-p (cdr entry)))
           (kill-buffer (cdr entry))))
       (setq clatter--buffer-alist initial-buffers)
       (clatter-test-cleanup))))

(defun clatter-test-feed--line-bol (buffer text)
  "Return the bol of the line in BUFFER whose text contains TEXT, or nil."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (when (search-forward text nil t)
        (beginning-of-line)
        (point)))))

(defun clatter-test-feed--nick-column-at (buffer bol)
  "Return the rendered nick column at BOL in BUFFER."
  (with-current-buffer buffer
    (let* ((line-end (save-excursion
                       (goto-char bol)
                       (line-end-position)))
           (end (next-single-property-change
                 bol 'clatter-nick-column nil line-end))
           (display (get-text-property bol 'display)))
      (if (get-text-property bol 'clatter-grouped)
          display
        (buffer-substring-no-properties bol end)))))

(defun clatter-test-feed--string (buffer)
  "Return the unpropertized contents of BUFFER."
  (with-current-buffer buffer
    (buffer-substring-no-properties (point-min) (point-max))))

(defun clatter-test-feed--count (buffer regexp)
  "Return the number of lines in BUFFER matching REGEXP."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let ((count 0))
        (while (re-search-forward regexp nil t)
          (setq count (1+ count)))
        count))))

(defun clatter-test-feed--separator-pos (buffer label)
  "Return the position of the LABEL text inside a separator line in BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward (concat "──[^\n]*" (regexp-quote label)) nil t)
        (match-beginning 0)))))

;;; Hook registration

(ert-deftest clatter-test-feed-enable-registers-hooks ()
  "Enabling adds both live message hooks; disabling removes them."
  (unwind-protect
      (progn
        (clatter-feed-enable)
        (should (memq #'clatter-feed--on-privmsg
                      (default-value 'clatter-privmsg-hook)))
        (should (memq #'clatter-feed--on-action
                      (default-value 'clatter-action-hook)))
        (clatter-feed-disable)
        (should-not (memq #'clatter-feed--on-privmsg
                          (default-value 'clatter-privmsg-hook)))
        (should-not (memq #'clatter-feed--on-action
                          (default-value 'clatter-action-hook))))
    (clatter-feed-disable)))

(ert-deftest clatter-test-feed-disabled-by-default ()
  "The feed buffer is opt-in."
  (should-not (default-value 'clatter-feed-enabled)))

;;; Rendering

(ert-deftest clatter-test-feed-channel-privmsg-renders ()
  "A channel PRIVMSG creates the buffer lazily and renders with a separator."
  (clatter-test-feed--with-capture conn
    (should-not (clatter-test-feed--buffer))
    (clatter-feed--on-privmsg conn '("alice" "user" "host") "#emacs"
                                 "hello there" nil)
    (let ((buf (clatter-test-feed--buffer)))
      (should buf)
      (should (= 0 (buffer-local-value 'clatter--unread-count buf)))
      (let ((text (clatter-test-feed--string buf)))
        (should (string-match-p "──[^\n]*testnet/#emacs" text))
        (should (string-match-p "<alice>" text))
        (should (string-match-p "hello there" text)))
      ;; The separator is doc-faced and carries no sender, so grouping
      ;; and navigation both skip it.
      (let ((sep (clatter-test-feed--separator-pos buf "testnet/#emacs")))
        (should sep)
        (with-current-buffer buf
          (should (memq 'font-lock-doc-face
                        (ensure-list (get-text-property sep 'face))))
          (should-not (get-text-property sep 'clatter-sender))))
      ;; The message line carries its source for jump-back.
      (let ((bol (clatter-test-feed--line-bol buf "hello there")))
        (should bol)
        (with-current-buffer buf
          (should (equal (get-text-property bol 'clatter-feed-network)
                         "testnet"))
          (should (equal (get-text-property bol 'clatter-feed-target)
                         "#emacs"))
          (should (equal (get-text-property bol 'clatter-sender) "alice"))
          (should (eq (get-text-property bol 'clatter-msg-type) 'privmsg))))
      ;; The sender in the nick column is nick-face colored.
      (with-current-buffer buf
        (goto-char (point-min))
        (should (search-forward "<alice>" nil t))
        (let ((nick-pos (- (point) 6)))
          (should (memq (clatter-hl-nick-face "alice" conn)
                        (ensure-list
                         (get-text-property nick-pos 'face)))))))))

(ert-deftest clatter-test-feed-action-renders ()
  "A CTCP ACTION renders with action formatting and its source props."
  (clatter-test-feed--with-capture conn
    (clatter-feed--on-action conn '("alice" "user" "host") "#emacs"
                                "waves" nil)
    (let* ((buf (clatter-test-feed--buffer))
           (bol (and buf (clatter-test-feed--line-bol buf "waves"))))
      (should buf)
      (should bol)
      (should (string-match-p "\\*" (clatter-test-feed--nick-column-at
                                     buf bol)))
      (with-current-buffer buf
        (should (eq (get-text-property bol 'clatter-msg-type) 'action))
        (should (equal (get-text-property bol 'clatter-feed-target)
                       "#emacs"))))))

;;; Grouping and separators

(ert-deftest clatter-test-feed-groups-consecutive-same-sender ()
  "Two messages from one nick in one channel show the nick once."
  (let ((clatter-group-messages-by-nick t))
    (clatter-test-feed--with-capture conn
      (clatter-feed--on-privmsg conn '("alice" "user" "host") "#emacs"
                                   "first" nil)
      (clatter-feed--on-privmsg conn '("alice" "user" "host") "#emacs"
                                   "second" nil)
      (let* ((buf (clatter-test-feed--buffer))
             (first-bol (clatter-test-feed--line-bol buf "first"))
             (second-bol (clatter-test-feed--line-bol buf "second")))
        (should first-bol)
        (should second-bol)
        (should (string-match-p
                 "<alice>" (clatter-test-feed--nick-column-at
                            buf first-bol)))
        (should (string-match-p
                 "\\` *\\'" (clatter-test-feed--nick-column-at
                             buf second-bol)))
        ;; The blanked line keeps its sender metadata.
        (with-current-buffer buf
          (should (equal (get-text-property second-bol 'clatter-sender)
                         "alice")))
        ;; One channel run, one separator.
        (should (= 1 (clatter-test-feed--count
                      buf "──[^\n]*testnet/#emacs")))))))

(ert-deftest clatter-test-feed-separator-breaks-grouping-across-channels ()
  "The same nick alternating between channels gets a separator each time."
  (let ((clatter-group-messages-by-nick t))
    (clatter-test-feed--with-capture conn
      (clatter-feed--on-privmsg conn '("alice" "user" "host") "#emacs"
                                   "first" nil)
      (clatter-feed--on-privmsg conn '("alice" "user" "host") "#lisp"
                                   "second" nil)
      (clatter-feed--on-privmsg conn '("alice" "user" "host") "#emacs"
                                   "third" nil)
      (let ((buf (clatter-test-feed--buffer)))
        ;; Each source change emits its own separator.
        (should (= 2 (clatter-test-feed--count
                      buf "──[^\n]*testnet/#emacs")))
        (should (= 1 (clatter-test-feed--count
                      buf "──[^\n]*testnet/#lisp")))
        ;; The separator broke the burst: every line shows its nick.
        (dolist (text '("first" "second" "third"))
          (let ((bol (clatter-test-feed--line-bol buf text)))
            (should bol)
            (should (string-match-p
                     "<alice>" (clatter-test-feed--nick-column-at
                                buf bol)))))
        ;; Each line points back at its own channel.
        (with-current-buffer buf
          (should (equal (get-text-property
                          (clatter-test-feed--line-bol buf "second")
                          'clatter-feed-target)
                         "#lisp"))
          (should (equal (get-text-property
                          (clatter-test-feed--line-bol buf "third")
                          'clatter-feed-target)
                         "#emacs")))))))

(ert-deftest clatter-test-feed-query-keyed-by-sender ()
  "A message addressed to my nick is keyed by the sender's nick."
  (clatter-test-feed--with-capture conn
    ;; TARGET is my nick, so the source is the query with alice.
    (clatter-feed--on-privmsg conn '("alice" "user" "host") "testnick"
                                 "psst" nil)
    (let* ((buf (clatter-test-feed--buffer))
           (bol (and buf (clatter-test-feed--line-bol buf "psst"))))
      (should buf)
      (should bol)
      (should (clatter-test-feed--separator-pos buf "testnet/alice"))
      (should-not (string-match-p "testnet/testnick"
                                  (clatter-test-feed--string buf)))
      (with-current-buffer buf
        (should (equal (get-text-property bol 'clatter-feed-target)
                       "alice"))
        (should (equal (get-text-property bol 'clatter-feed-network)
                       "testnet"))))))

;;; Highlighting and filtering

(ert-deftest clatter-test-feed-mention-gets-mention-face ()
  "Text containing my nick for that connection is mention-faced."
  (clatter-test-feed--with-capture conn
    (clatter-feed--on-privmsg conn '("alice" "user" "host") "#emacs"
                                 "testnick: ping" nil)
    (let ((buf (clatter-test-feed--buffer)))
      (should buf)
      (with-current-buffer buf
        (goto-char (point-min))
        (should (search-forward "ping" nil t))
        (should (memq 'clatter-mention
                      (ensure-list (get-text-property (match-beginning 0)
                                                      'face))))))))

(ert-deftest clatter-test-feed-muted-sender-line-is-invisible ()
  "A fool's captured line is actually hidden, not merely propertied.
The mode must seed `buffer-invisibility-spec' itself: the feed buffer
never goes through `clatter-ui-setup-buffer'."
  (let ((clatter-fools '("troll")))
    (clatter-test-feed--with-capture conn
      (clatter-feed--on-privmsg conn '("troll" "user" "host") "#emacs"
                                   "bait" nil)
      (let* ((buf (clatter-test-feed--buffer))
             (bol (and buf (clatter-test-feed--line-bol buf "bait"))))
        (should buf)
        (should bol)
        (with-current-buffer buf
          (let ((invisible (get-text-property bol 'invisible)))
            (should invisible)
            (should (memq 'clatter-fool (ensure-list invisible))))
          (should (memq 'clatter-fool buffer-invisibility-spec))
          (should (memq 'muted buffer-invisibility-spec))
          (should (invisible-p bol)))))))

(ert-deftest clatter-test-feed-ignored-sender-uses-muted-category ()
  "An ignore-list sender's line is hidden under the `muted' category."
  ;; Ignore patterns match the full nick!user@host prefix.
  (let ((clatter-ignore-list '("troll!*")))
    (clatter-test-feed--with-capture conn
      (clatter-feed--on-privmsg conn '("troll" "user" "host") "#emacs"
                                   "bait" nil)
      (let* ((buf (clatter-test-feed--buffer))
             (bol (and buf (clatter-test-feed--line-bol buf "bait"))))
        (should buf)
        (should bol)
        (with-current-buffer buf
          (should (memq 'muted (ensure-list (get-text-property bol 'invisible))))
          (should (invisible-p bol)))))))

(ert-deftest clatter-test-feed-hidden-sender-separator-is-hidden ()
  "A hidden sender's separator is hidden too, and does not claim the source.
The next visible message from that channel still gets a visible separator."
  (let ((clatter-fools '("troll")))
    (clatter-test-feed--with-capture conn
      (clatter-feed--on-privmsg conn '("alice" "user" "host") "#alpha"
                                   "hi" nil)
      (clatter-feed--on-privmsg conn '("troll" "user" "host") "#beta"
                                   "bait" nil)
      (clatter-feed--on-privmsg conn '("bob" "user" "host") "#beta"
                                   "real talk" nil)
      (let ((buf (clatter-test-feed--buffer)))
        (should (= 2 (clatter-test-feed--count buf "──[^\n]*testnet/#beta")))
        (with-current-buffer buf
          (goto-char (point-min))
          (re-search-forward "──[^\n]*testnet/#beta")
          (should (invisible-p (match-beginning 0)))
          (re-search-forward "──[^\n]*testnet/#beta")
          (should-not (invisible-p (match-beginning 0))))))))

(ert-deftest clatter-test-feed-image-scan-suppressed ()
  "Capture never re-scans message text for inline images.
The source channel buffer already scanned; a second scan would fetch
every URL twice."
  (let ((calls 0))
    (cl-letf (((symbol-function 'clatter-image--scan-message)
               (lambda (&rest _) (cl-incf calls))))
      (clatter-test-feed--with-capture conn
        (clatter-feed--on-privmsg conn '("alice" "user" "host") "#emacs"
                                     "look https://example.com/cat.png" nil)
        (should (string-match-p
                 "cat\\.png" (clatter-test-feed--string
                              (clatter-test-feed--buffer))))
        (should (= calls 0))))))

;;; Buffer properties

(ert-deftest clatter-test-feed-buffer-is-read-only ()
  "The feed buffer has no input area, so typing into it is blocked."
  (clatter-test-feed--with-capture conn
    (clatter-feed--on-privmsg conn '("alice" "user" "host") "#emacs"
                                 "hello" nil)
    (let ((buf (clatter-test-feed--buffer)))
      (should buf)
      (should (buffer-local-value 'buffer-read-only buf))
      (should-error (with-current-buffer buf
                      (goto-char (point-max))
                      (insert "stray"))
                    :type 'buffer-read-only))))

(ert-deftest clatter-test-feed-truncation-keeps-newest ()
  "Truncation deletes the oldest lines and keeps the newest capture.

Regression: the buffer is oldest-first regardless of the user's global
`clatter-message-order', so truncation must cut from the top."
  (let ((clatter-buffer-max-lines 4)
        (clatter-message-order 'newest-first))
    (clatter-test-feed--with-capture conn
      (dotimes (i 12)
        (clatter-feed--on-privmsg conn '("alice" "user" "host") "#emacs"
                                     (format "line-%02d" i) nil))
      (let* ((buf (clatter-test-feed--buffer))
             (text (clatter-test-feed--string buf)))
        (should (string-match-p "line-11" text))
        (should-not (string-match-p "line-00" text))))))

;;; Batch safety

(ert-deftest clatter-test-feed-batched-messages-do-not-appear ()
  "Batched (chathistory) traffic never reaches the feed buffer."
  (clatter-test-feed--with-capture conn
    (clatter-feed-enable)
    (clatter-dispatch-message
     conn (clatter-test-parse ":server BATCH +history chathistory #emacs"))
    (clatter-dispatch-message
     conn (clatter-test-parse
           "@batch=history :alice!~a@host PRIVMSG #emacs :old message"))
    (clatter-dispatch-message
     conn (clatter-test-parse ":server BATCH -history"))
    ;; Nothing was captured, so the buffer was never even created.
    (should-not (clatter-test-feed--buffer))))

(ert-deftest clatter-test-feed-live-message-reaches-buffer-through-hook ()
  "An unbatched PRIVMSG dispatched normally is captured via the hook."
  (clatter-test-feed--with-capture conn
    (clatter-feed-enable)
    (run-hook-with-args 'clatter-privmsg-hook
                        conn '("alice" "user" "host") "#emacs" "live" nil)
    (let ((buf (clatter-test-feed--buffer)))
      (should buf)
      (should (string-match-p "live" (clatter-test-feed--string buf))))))

;;; Jump back to the source message

(ert-deftest clatter-test-feed-visit-jumps-to-source-message ()
  "RET on a captured line lands on that message in the source buffer."
  (clatter-test-feed--with-capture conn
    (let ((text (propertize "jumpable" 'clatter-msgid "msg-42")))
      (clatter-ui--on-privmsg conn '("alice" "user" "host") "#emacs" text nil)
      (clatter-feed--on-privmsg conn '("alice" "user" "host") "#emacs"
                                   text nil))
    (let ((chan (clatter-get-buffer "testnet" "#emacs"))
          (feed (clatter-test-feed--buffer)))
      (should chan)
      (should feed)
      (should (clatter--find-message-position-by-msgid chan "msg-42"))
      (save-window-excursion
        (with-current-buffer feed
          (goto-char (clatter-test-feed--line-bol feed "jumpable"))
          (clatter-feed-visit)
          ;; Visiting pops to the source buffer...
          (should (eq (current-buffer) chan))))
      ;; ...and leaves point on the referenced message.
      (with-current-buffer chan
        (should (equal (get-text-property (point) 'clatter-msgid) "msg-42"))))))

(ert-deftest clatter-test-feed-visit-dead-source-signals-user-error ()
  "Visiting a line whose source buffer is gone reports an error, not a jump."
  (clatter-test-feed--with-capture conn
    (let ((text (propertize "orphan" 'clatter-msgid "msg-99")))
      (clatter-ui--on-privmsg conn '("alice" "user" "host") "#emacs" text nil)
      (clatter-feed--on-privmsg conn '("alice" "user" "host") "#emacs"
                                   text nil))
    (let ((chan (clatter-get-buffer "testnet" "#emacs"))
          (feed (clatter-test-feed--buffer)))
      (should chan)
      (should feed)
      (kill-buffer chan)
      (clatter-remove-buffer "testnet" "#emacs")
      (save-window-excursion
        (with-current-buffer feed
          (goto-char (clatter-test-feed--line-bol feed "orphan"))
          (should-error (clatter-feed-visit) :type 'user-error))))))

;;; Autoscroll

(ert-deftest clatter-test-feed-autoscroll-follows-at-bottom ()
  "A window sitting at end-of-buffer advances with each new message."
  (clatter-test-feed--with-capture conn
    (clatter-feed--on-privmsg conn '("alice" "user" "host") "#emacs"
                                 "first" nil)
    (let ((buf (clatter-test-feed--buffer)))
      (save-window-excursion
        (set-window-buffer (selected-window) buf)
        (set-window-point (selected-window) (with-current-buffer buf
                                              (point-max)))
        (clatter-feed--on-privmsg conn '("alice" "user" "host") "#emacs"
                                     "second" nil)
        (should (= (window-point (selected-window))
                   (with-current-buffer buf (point-max))))))))

(ert-deftest clatter-test-feed-autoscroll-follows-on-last-line ()
  "Point at the beginning of the last message line counts as tailing.
Evil's normal state never rests point at point-max, so bottom detection
must accept anywhere on the last line."
  (clatter-test-feed--with-capture conn
    (clatter-feed--on-privmsg conn '("alice" "user" "host") "#emacs"
                                 "first" nil)
    (let ((buf (clatter-test-feed--buffer)))
      (save-window-excursion
        (set-window-buffer (selected-window) buf)
        ;; Land on the last message line's bol, the way evil G does.
        (set-window-point (selected-window)
                          (with-current-buffer buf
                            (save-excursion
                              (goto-char (point-max))
                              (forward-line -1)
                              (point))))
        (clatter-feed--on-privmsg conn '("alice" "user" "host") "#emacs"
                                     "second" nil)
        (should (= (window-point (selected-window))
                   (with-current-buffer buf (point-max))))))))

(ert-deftest clatter-test-feed-autoscroll-leaves-scrolled-back-window ()
  "A window scrolled away from the bottom stays where it is."
  (clatter-test-feed--with-capture conn
    (clatter-feed--on-privmsg conn '("alice" "user" "host") "#emacs"
                                 "first" nil)
    (let ((buf (clatter-test-feed--buffer)))
      (save-window-excursion
        (set-window-buffer (selected-window) buf)
        (set-window-point (selected-window) (point-min))
        (clatter-feed--on-privmsg conn '("alice" "user" "host") "#emacs"
                                     "second" nil)
        (should (= (window-point (selected-window)) (point-min)))))))

(ert-deftest clatter-test-feed-autoscroll-disabled-stays-put ()
  "With `clatter-feed-autoscroll' nil, even a bottom window stays."
  (let ((clatter-feed-autoscroll nil))
    (clatter-test-feed--with-capture conn
      (clatter-feed--on-privmsg conn '("alice" "user" "host") "#emacs"
                                   "first" nil)
      (let* ((buf (clatter-test-feed--buffer))
             (end (with-current-buffer buf (point-max))))
        (save-window-excursion
          (set-window-buffer (selected-window) buf)
          (set-window-point (selected-window) end)
          (clatter-feed--on-privmsg conn '("alice" "user" "host") "#emacs"
                                       "second" nil)
          (should (= (window-point (selected-window)) end))
          (should (< end (with-current-buffer buf (point-max)))))))))

(ert-deftest clatter-test-feed-autoscroll-buffer-point-follows ()
  "With no window, the buffer point tails so the first view shows the newest."
  (clatter-test-feed--with-capture conn
    (dotimes (i 3)
      (clatter-feed--on-privmsg conn '("alice" "user" "host") "#emacs"
                                   (format "msg-%d" i) nil))
    (with-current-buffer (clatter-test-feed--buffer)
      (should (= (point) (point-max))))))

(ert-deftest clatter-test-feed-visit-falls-back-without-msgid ()
  "Without a msgid, RET matches the message by sender and text."
  (clatter-test-feed--with-capture conn
    (clatter-ui--on-privmsg conn '("alice" "user" "host") "#emacs"
                            "findme" nil)
    (clatter-feed--on-privmsg conn '("alice" "user" "host") "#emacs"
                                 "findme" nil)
    (let ((chan (clatter-get-buffer "testnet" "#emacs"))
          (feed (clatter-test-feed--buffer)))
      (save-window-excursion
        (with-current-buffer feed
          (goto-char (clatter-test-feed--line-bol feed "findme"))
          (should-not (get-text-property (point) 'clatter-msgid))
          (clatter-feed-visit)
          (should (eq (current-buffer) chan))))
      (with-current-buffer chan
        (should (equal (get-text-property (point) 'clatter-text) "findme"))))))

(ert-deftest clatter-test-feed-visit-stale-msgid-falls-back-to-text ()
  "A msgid the source buffer no longer has falls back to the text match."
  (clatter-test-feed--with-capture conn
    ;; The channel line has no msgid; the feed line carries a stale one.
    (clatter-ui--on-privmsg conn '("alice" "user" "host") "#emacs"
                            "stale" nil)
    (clatter-feed--on-privmsg conn '("alice" "user" "host") "#emacs"
                                 (propertize "stale" 'clatter-msgid "gone-1")
                                 nil)
    (let ((chan (clatter-get-buffer "testnet" "#emacs"))
          (feed (clatter-test-feed--buffer)))
      (save-window-excursion
        (with-current-buffer feed
          (goto-char (clatter-test-feed--line-bol feed "stale"))
          (should (equal (get-text-property (point) 'clatter-msgid) "gone-1"))
          (clatter-feed-visit)
          (should (eq (current-buffer) chan))))
      (with-current-buffer chan
        (should (equal (get-text-property (point) 'clatter-text) "stale"))))))

(ert-deftest clatter-test-feed-visit-missing-message-signals-user-error ()
  "A message the source buffer never had (or truncated) errors out."
  (clatter-test-feed--with-capture conn
    ;; The channel buffer exists but never received the message.
    (clatter-get-or-create-buffer "testnet" "#emacs" 'channel)
    (clatter-feed--on-privmsg conn '("alice" "user" "host") "#emacs"
                                 "vanished" nil)
    (let ((feed (clatter-test-feed--buffer)))
      (save-window-excursion
        (with-current-buffer feed
          (goto-char (clatter-test-feed--line-bol feed "vanished"))
          (should-error (clatter-feed-visit) :type 'user-error))))))

(ert-deftest clatter-test-feed-fallback-prefers-newest-match ()
  "With duplicate sender+text and no server-time, the newest match wins.
Both message orders must find the newest duplicate, not just the last line."
  (dolist (clatter-message-order '(oldest-first newest-first))
    (clatter-test-feed--with-capture conn
      (clatter-ui--on-privmsg conn '("alice" "user" "host") "#emacs" "dup" nil)
      (clatter-ui--on-privmsg conn '("alice" "user" "host") "#emacs" "dup" nil)
      (let* ((chan (clatter-get-buffer "testnet" "#emacs"))
             (matches (with-current-buffer chan
                        (save-excursion
                          (goto-char (point-min))
                          (let (acc)
                            (while (not (eobp))
                              (when (equal "dup" (get-text-property
                                                  (point) 'clatter-text))
                                (push (point) acc))
                              (forward-line 1))
                            (nreverse acc))))))
        (should (= 2 (length matches)))
        (should (= (clatter-feed--find-message chan "alice" "dup" nil)
                   (if (eq clatter-message-order 'newest-first)
                       (car matches)
                     (car (last matches)))))))))

;;; Hide

(ert-deftest clatter-test-feed-hide-atoms-are-stable-and-uninterned ()
  "Source atoms remain stable without leaking into the global obarray."
  (let* ((name "clatter-feed-hide:testnet/#fresh")
         (atom (clatter-feed--hide-atom "testnet" "#fresh")))
    (should (eq atom (clatter-feed--hide-atom "testnet" "#FRESH")))
    (should-not (intern-soft name))))

(ert-deftest clatter-test-feed-hide-nil-stamps-atom-but-shows ()
  "Every captured line carries a hide atom even when nothing is hidden."
  (clatter-test-feed--with-capture conn
    (clatter-feed--on-privmsg conn '("alice" "user" "host") "#emacs"
                                 "hello" nil)
    (let* ((buf (clatter-test-feed--buffer))
           (bol (clatter-test-feed--line-bol buf "hello"))
           (atom (clatter-feed--hide-atom "testnet" "#emacs")))
      (should bol)
      (with-current-buffer buf
        (should (memq atom (ensure-list (get-text-property bol 'invisible))))
        (should-not (invisible-p bol))))))

(ert-deftest clatter-test-feed-hide-list-hides-matching-target ()
  "A hide list conceals matching targets and unhides when cleared."
  (let ((clatter-feed-hide-channels '("#EMACS")))
    (clatter-test-feed--with-capture conn
      (clatter-feed--on-privmsg conn '("alice" "user" "host") "#emacs"
                                   "hidden" nil)
      (clatter-feed--on-privmsg conn '("bob" "user" "host") "#other"
                                   "shown" nil)
      (let* ((buf (clatter-test-feed--buffer))
             (hidden (clatter-test-feed--line-bol buf "hidden"))
             (shown (clatter-test-feed--line-bol buf "shown")))
        (with-current-buffer buf
          (should (invisible-p hidden))
          (should-not (invisible-p shown)))
        (setq clatter-feed-hide-channels nil)
        (clatter-feed--reconcile-hide)
        (with-current-buffer buf
          (should-not (invisible-p hidden))
          (should-not (invisible-p shown)))))))

(ert-deftest clatter-test-feed-hide-list-still-updates-last-source ()
  "Source-hide is temporary, so a hidden run still owns separator context."
  (let ((clatter-feed-hide-channels '("#emacs")))
    (clatter-test-feed--with-capture conn
      (clatter-feed--on-privmsg conn '("alice" "user" "host") "#emacs"
                                   "one" nil)
      (clatter-feed--on-privmsg conn '("bob" "user" "host") "#emacs"
                                   "two" nil)
      (should (= 1 (clatter-test-feed--count
                    (clatter-test-feed--buffer)
                    "──[^\n]*testnet/#emacs"))))))

(ert-deftest clatter-test-feed-hide-list-hides-separator ()
  "A hidden source's separator shares the hide atom."
  (let ((clatter-feed-hide-channels '("#emacs")))
    (clatter-test-feed--with-capture conn
      (clatter-feed--on-privmsg conn '("alice" "user" "host") "#emacs"
                                   "x" nil)
      (let ((buf (clatter-test-feed--buffer)))
        (with-current-buffer buf
          (goto-char (point-min))
          (re-search-forward "──[^\n]*testnet/#emacs")
          (should (invisible-p (match-beginning 0))))))))

(ert-deftest clatter-test-feed-hide-visible-follows-windows ()
  "Visible mode hides a source while it has a window, then shows it again."
  (let ((clatter-feed-hide-visible t))
    (clatter-test-feed--with-capture conn
      (let ((chan (clatter-get-or-create-buffer "testnet" "#emacs" 'channel))
            (scratch (get-buffer-create " *clatter-hide-scratch*")))
        (clatter-feed--on-privmsg conn '("alice" "user" "host") "#emacs"
                                     "here" nil)
        (clatter-feed--on-privmsg conn '("alice" "user" "host") "#other"
                                     "away" nil)
        (let* ((buf (clatter-test-feed--buffer))
               (here (clatter-test-feed--line-bol buf "here"))
               (away (clatter-test-feed--line-bol buf "away")))
          (save-window-excursion
            (delete-other-windows)
            (set-window-buffer (selected-window) chan)
            (clatter-feed--reconcile-hide)
            (with-current-buffer buf
              (should (invisible-p here))
              (should-not (invisible-p away)))
            (set-window-buffer (selected-window) scratch)
            (clatter-feed--reconcile-hide)
            (with-current-buffer buf
              (should-not (invisible-p here))))
          (when (buffer-live-p scratch)
            (kill-buffer scratch)))))))

(ert-deftest clatter-test-feed-hide-visible-and-channels-combine ()
  "On-screen sources and the denylist hide together."
  (let ((clatter-feed-hide-visible t)
        (clatter-feed-hide-channels '("#other")))
    (clatter-test-feed--with-capture conn
      (let ((chan (clatter-get-or-create-buffer "testnet" "#emacs" 'channel))
            (scratch (get-buffer-create " *clatter-hide-scratch*")))
        (clatter-feed--on-privmsg conn '("alice" "user" "host") "#emacs"
                                     "here" nil)
        (clatter-feed--on-privmsg conn '("bob" "user" "host") "#other"
                                     "listed" nil)
        (clatter-feed--on-privmsg conn '("carol" "user" "host") "#keep"
                                     "shown" nil)
        (let* ((buf (clatter-test-feed--buffer))
               (here (clatter-test-feed--line-bol buf "here"))
               (listed (clatter-test-feed--line-bol buf "listed"))
               (shown (clatter-test-feed--line-bol buf "shown")))
          (save-window-excursion
            (delete-other-windows)
            (set-window-buffer (selected-window) chan)
            (clatter-feed--reconcile-hide)
            (with-current-buffer buf
              (should (invisible-p here))
              (should (invisible-p listed))
              (should-not (invisible-p shown))))
          (when (buffer-live-p scratch)
            (kill-buffer scratch)))))))

(ert-deftest clatter-test-feed-disable-clears-hide-atoms ()
  "Disabling capture unhides retained lines."
  (let ((clatter-feed-hide-channels '("#emacs")))
    (clatter-test-feed--with-capture conn
      (clatter-feed--on-privmsg conn '("alice" "user" "host") "#emacs"
                                   "x" nil)
      (let* ((buf (clatter-test-feed--buffer))
             (bol (clatter-test-feed--line-bol buf "x")))
        (with-current-buffer buf
          (should (invisible-p bol)))
        (clatter-feed-disable)
        (with-current-buffer buf
          (should-not (invisible-p bol)))))))

(ert-deftest clatter-test-feed-hide-list-covers-disconnected-sources ()
  "A hide list still conceals inbox lines after the connection is gone."
  (clatter-test-feed--with-capture conn
    (clatter-feed--on-privmsg conn '("alice" "user" "host") "#emacs"
                                 "old" nil)
    (let* ((buf (clatter-test-feed--buffer))
           (bol (clatter-test-feed--line-bol buf "old"))
           (clatter-feed-hide-channels '("#emacs")))
      (clrhash clatter-connections)
      (clatter-feed--reconcile-hide)
      (with-current-buffer buf
        (should (invisible-p bol))))))

(ert-deftest clatter-test-feed-hide-visible-hook-waits-for-enable ()
  "Customizing hide to visible does not install the window hook until enable."
  (unwind-protect
      (progn
        (clatter-feed-disable)
        (clatter-feed--set-hide 'clatter-feed-hide-visible t)
        (should-not (memq #'clatter-feed--window-change
                          (default-value 'window-buffer-change-functions)))
        (clatter-feed-enable)
        (should (memq #'clatter-feed--window-change
                      (default-value 'window-buffer-change-functions))))
    (clatter-feed-disable)
    (clatter-feed--set-hide 'clatter-feed-hide-visible nil)))

(ert-deftest clatter-test-feed-hide-visible-registers-hook ()
  "Visible mode installs the window hook; disable removes it."
  (let ((clatter-feed-hide-visible t))
    (unwind-protect
        (progn
          (clatter-feed-enable)
          (should (memq #'clatter-feed--window-change
                        (default-value 'window-buffer-change-functions)))
          (clatter-feed-disable)
          (should-not (memq #'clatter-feed--window-change
                            (default-value 'window-buffer-change-functions))))
      (clatter-feed-disable))))

(provide 'test-feed)

;;; test-feed.el ends here
