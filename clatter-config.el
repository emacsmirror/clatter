;;; clatter-config.el --- Configuration for clatter.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Glenn Thompson
;; Author: Glenn Thompson
;; License: MIT

;;; Commentary:

;; User-facing configuration variables and auth-source integration
;; for clatter.el IRC client.

;;; Code:

(require 'auth-source)

(defgroup clatter nil
  "An IRCv3-compliant IRC client for Emacs."
  :group 'communication
  :prefix "clatter-")

(defcustom clatter-networks nil
  "List of IRC networks to connect to.
Each entry is a list of (NAME . PLIST) where PLIST contains:
  :server    - Server hostname (required)
  :port      - Server port (default 6697)
  :tls       - Use TLS (default t)
  :nick      - Nickname (required)
  :username  - Username (default: nick)
  :realname  - Real name (default: nick)
  :sasl      - SASL type: nil, \\='plain, or \\='external
  :client-cert - Path to client certificate for SASL EXTERNAL
  :autojoin  - List of channels to join on connect
  :password  - Server password (or use auth-source)

Example:
  \\='((\"libera\"
     :server \"irc.libera.chat\"
     :port 6697
     :tls t
     :nick \"yournick\"
     :sasl plain
     :autojoin (\"#emacs\" \"#commonlisp\")))"
  :type '(repeat (cons string plist))
  :group 'clatter)

(defcustom clatter-default-nick (user-login-name)
  "Default nickname if not specified per-network."
  :type 'string
  :group 'clatter)

(defcustom clatter-default-realname "clatter.el user"
  "Default real name if not specified per-network."
  :type 'string
  :group 'clatter)

(defcustom clatter-default-port 6697
  "Default server port."
  :type 'integer
  :group 'clatter)

(defcustom clatter-default-tls t
  "Use TLS by default."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-timestamp-format "%H:%M"
  "Format string for message timestamps.
See `format-time-string' for format specifiers."
  :type 'string
  :group 'clatter)

(defcustom clatter-fill-column 80
  "Column at which to wrap messages in channel buffers."
  :type 'integer
  :group 'clatter)

(defcustom clatter-nick-column-width 20
  "Width of the nick column for right-aligned nick display.
Nicks are right-aligned within this column.  Increase if you
have channels with very long nicknames."
  :type 'integer
  :group 'clatter)

(defcustom clatter-max-line-length 400
  "Maximum length of a single IRC message (excluding protocol overhead)."
  :type 'integer
  :group 'clatter)

(defcustom clatter-reconnect-max-delay 300
  "Maximum delay in seconds between reconnection attempts."
  :type 'integer
  :group 'clatter)

(defcustom clatter-reconnect-initial-delay 10
  "Initial delay in seconds before first reconnection attempt."
  :type 'integer
  :group 'clatter)

(defcustom clatter-ping-interval 30
  "Seconds between sending keepalive pings to the server.
Matches ERC default.  Lower values keep the connection alive through
NAT timeouts but produce more traffic."
  :type 'integer
  :group 'clatter)

(defcustom clatter-ping-timeout 120
  "Seconds with no data received before considering connection dead.
If no data (including PONG replies) has been received from the server
for this many seconds, the connection is killed and a reconnect is
scheduled.  Must be greater than `clatter-ping-interval'.
Matches ERC default."
  :type 'integer
  :group 'clatter)

(defcustom clatter-use-auth-source t
  "Use `auth-source' to look up passwords.
Passwords are looked up by :host (server) and :user (nick)."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-nick-reclaim-enabled t
  "Whether to periodically try to reclaim the configured nick.
When non-nil, if you connect and your desired nick is in use (so you
land on a fallback like \"nick_\"), clatter periodically sends NICK to
reclaim it.  Disable this if another long-lived client (such as a
bouncer) legitimately holds your nick, to avoid fighting over it."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-nick-reclaim-use-regain nil
  "Reclaim the nick as the identified account owner via NickServ REGAIN.
When non-nil and the connection is SASL-identified, automatic reclaim
attempts use \"REGAIN\" on NickServ rather than a bare NICK command.

This defaults to nil deliberately: REGAIN forcibly kills whatever session
holds the nick.  If a legitimate second client (a bouncer, or the same
account connected from another host) holds it, auto-REGAIN turns a
harmless fallback nick into a mutual kill loop, since both sides keep
regaining and killing each other.  With nil, automatic reclaim only
issues a passive NICK and waits, which never kills anyone; use the
explicit `/regain' command when you actually want to seize the nick.

When non-nil and not SASL-identified, a bare NICK is used as a fallback."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-nick-reclaim-initial-delay 8
  "Seconds to wait after connecting before the first nick reclaim attempt.
Gives services time to fully associate your SASL login with your account
and to settle any ENFORCE hold on the nick, so the first REGAIN succeeds
cleanly instead of racing the enforcement timer."
  :type 'integer
  :group 'clatter)

(defcustom clatter-regain-kill-backoff 120
  "Minimum reconnect delay in seconds after a services nick-regain kill.
When the server kills the connection with a \"regained by services\"
message, an immediate reconnect tends to trigger another collision and
another kill.  After such a kill, clatter waits at least this many
seconds before reconnecting to break the ping-pong loop."
  :type 'integer
  :group 'clatter)

(defcustom clatter-quit-on-exit t
  "Send a QUIT to all connected networks when Emacs exits.
This prevents leaving orphaned ghost sessions on the server that hold
your nick until the server times them out."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-quit-message "clatter.el"
  "Default QUIT message sent when disconnecting or exiting Emacs."
  :type 'string
  :group 'clatter)

(defcustom clatter-log-raw-protocol nil
  "Log raw IRC protocol lines to *clatter-debug* buffer."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-flyspell-enable t
  "Enable flyspell in the clatter input area.
Uses `flyspell-mode' which is built into Emacs."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-paste-flood-threshold 3
  "Number of lines above which a paste triggers a flood warning.
If input contains more than this many lines, the user is prompted
before sending.  Set to nil to disable the warning."
  :type '(choice integer (const nil))
  :group 'clatter)

(defcustom clatter-input-ring-size 1024
  "Size of the input history ring."
  :type 'integer
  :group 'clatter)

(defcustom clatter-message-order 'newest-first
  "Order in which messages appear in channel buffers.
`newest-first' places new messages directly below the input line
with older messages scrolling downward (default).
`oldest-first' places new messages at the bottom of the buffer,
like a traditional IRC client."
  :type '(choice (const :tag "Newest first (below input)" newest-first)
                 (const :tag "Oldest first (traditional)" oldest-first))
  :group 'clatter)

(defcustom clatter-buffer-max-lines 10000
  "Maximum number of lines to keep in a clatter buffer.
When exceeded, oldest messages are removed.
Set to nil to disable truncation."
  :type '(choice integer (const nil))
  :group 'clatter)

(defcustom clatter-suppress-messages nil
  "Default message types hidden in new channel buffers.
Valid values: join, part, quit, nick, mode, away, kick, topic.
Example: \\='(join part quit away) to hide join/part/quit/away noise.

This list seeds each new buffer's `buffer-invisibility-spec'.
Suppression is non-destructive: matching messages are still
inserted but hidden, so they can be revealed again later.  Use
the per-buffer `/suppress' and `/unsuppress' commands to toggle
types at runtime without losing any history."
  :type '(repeat (choice (const :tag "JOIN" join)
                         (const :tag "PART" part)
                         (const :tag "QUIT" quit)
                         (const :tag "NICK" nick)
                         (const :tag "MODE" mode)
                         (const :tag "AWAY" away)
                         (const :tag "KICK" kick)
                         (const :tag "TOPIC" topic)))
  :group 'clatter)

(defcustom clatter-prefix-rank "~&@%+"
  "Default prefix ranking.  This may be overridden by the server."
  :type 'string
  :group 'clatter)

;; --- IRCv3 capabilities we want to negotiate ---

(defconst clatter-wanted-capabilities
  '("server-time"
    "multi-prefix"
    "away-notify"
    "account-notify"
    "extended-join"
    "chghost"
    "invite-notify"
    "setname"
    "account-tag"
    "message-tags"
    "batch"
    "labeled-response"
    "echo-message"
    "draft/chathistory"
    "chathistory"
    "cap-notify"
    "userhost-in-names"
    "typing"
    "draft/typing"
    "monitor"
    "draft/read-marker"
    "read-marker"
    "standard-replies"
    "draft/bot"
    "draft/channel-rename"
    "draft/reply"
    "+draft/reply"
    "draft/react"
    "+draft/react"
    "message-tags")
  "IRCv3 capabilities to request during CAP negotiation.")

;; --- Ignore list ---

(defcustom clatter-ignore-list nil
  "List of ignored nick patterns.
Each entry is a string.  Entries are matched case-insensitively.
Glob-style wildcards (* and ?) are supported.
Example: (\"spambot\" \"*!*@bad.host.example.com\")"
  :type '(repeat string)
  :group 'clatter)

(defun clatter-ignored-p (sender)
  "Return non-nil if SENDER should be ignored.
Matches against `clatter-ignore-list' case-insensitively.
Supports glob wildcards (* and ?)."
  (let ((sender-down (downcase sender)))
    (cl-some (lambda (pattern)
               (let* ((pat-down (downcase pattern))
                      (re (concat "\\`"
                                  (replace-regexp-in-string
                                   "\\?" "."
                                   (replace-regexp-in-string
                                    "\\*" ".*"
                                    (regexp-quote pat-down)))
                                  "\\'")))
                 (string-match-p re sender-down)))
             clatter-ignore-list)))

;; --- IRC protocol constants ---

(defconst clatter-max-irc-line-length 510
  "Maximum IRC line length (512 minus CR LF).")

;; --- Helper functions ---

(defun clatter-network-get (network key &optional default)
  "Get KEY from NETWORK config plist, or DEFAULT."
  (let ((config (cdr (assoc network clatter-networks #'equal))))
    (or (plist-get config key) default)))

(defun clatter-get-password (network)
  "Get the password for NETWORK.
Checks the network config first, then auth-source."
  (let* ((config (cdr (assoc network clatter-networks #'equal)))
         (server (plist-get config :server))
         (nick (or (plist-get config :nick) clatter-default-nick))
         (explicit-pw (plist-get config :password)))
    (cond
     (explicit-pw explicit-pw)
     (clatter-use-auth-source
      (let ((found (car (auth-source-search :host server
                                            :user nick
                                            :max 1))))
        (when found
          (let ((secret (plist-get found :secret)))
            (if (functionp secret) (funcall secret) secret)))))
     (t nil))))

(provide 'clatter-config)

;;; clatter-config.el ends here
