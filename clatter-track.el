;;; clatter-track.el --- Buffer activity tracking -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Glenn Thompson
;; Author: Glenn Thompson <glenn@paren.works>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Smart activity tracker for clatter.el buffers.
;; Tracks unread messages and mentions per channel,
;; displays activity in the global mode-line,
;; and integrates with consult for buffer switching.

;;; Code:

(require 'cl-lib)
(require 'clatter-config)
(require 'clatter-model)

;; --- Configuration ---

(defcustom clatter-track-enabled t
  "Enable activity tracking in the global mode-line."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-track-position 'after-modes
  "Where to place the activity indicator in the global mode-line.
Valid values: before-modes, after-modes, end."
  :type '(choice (const :tag "Before modes" before-modes)
                 (const :tag "After modes" after-modes)
                 (const :tag "End" end))
  :group 'clatter)

(defcustom clatter-track-muted-channels nil
  "List of targets to dim, but keep, in the activity tracker.
Muted targets still appear in the mode-line indicator, activity list,
activity switch command, and Consult activity source.  They use the
`clatter-track-muted' face instead of their normal activity face.

Despite the historical variable name, this list may contain any target,
including the server target \"*server*\".  Use
`clatter-track-exclude-targets' when a target should not appear in any
tracker surface.

Example: (\"*server*\" \"#spam\" \"#bots\")"
  :type '(repeat string)
  :group 'clatter)

(defcustom clatter-track-exclude-targets nil
  "List of targets to hide completely from the activity tracker.
Excluded targets do not appear in the mode-line indicator, activity list,
activity switch command, or Consult activity source.  Exclusion only affects
the tracker: messages still appear in the target buffer and retain their
normal unread state.

This differs from `clatter-track-muted-channels', which keeps targets in the
tracker and merely dims them.  Target names use the same spelling as
`clatter--target'.  For example, use (\"*server*\") to omit server activity,
or (\"#spam\" \"#bots\") to omit selected channels."
  :type '(repeat string)
  :group 'clatter)

(defcustom clatter-track-faces-alist
  '((mention . clatter-track-mention)
    (dm . clatter-track-dm)
    (activity . clatter-track-activity)
    (muted . clatter-track-muted))
  "Alist mapping activity types to faces for the track indicator."
  :type '(alist :key-type symbol :value-type face)
  :group 'clatter)

(defcustom clatter-track-shorten nil
  "Shorten channel names in the track indicator.
nil shows the full buffer name in the legacy layout and the raw target
in the strip layout.  An integer N truncates the channel name body to
N chars; e.g. 5 turns #systemcrafters into #syst.  `drop-vowels'
strips vowels from the body.  `syllable' keeps the first char of each
CamelCase or delimiter segment, lowercased (so #system-crafters
becomes #sc).  Only channel targets (#, &, !, +) are shortened; query
nicks and `*server*' are unchanged.  Collisions are disambiguated."
  :type '(choice (const :tag "Off (full name)" nil)
                 (integer :tag "Truncate to N chars")
                 (const :tag "Drop vowels" drop-vowels)
                 (const :tag "Syllable / CamelHump" syllable))
  :group 'clatter)

(defcustom clatter-track-switch-return-to-origin t
  "When non-nil, `clatter-track-switch' returns to the buffer you started
switching from once all active clatter buffers have been visited.  The
origin is captured on the first switch of a sequence and cleared on
return, so each switching session starts a fresh origin."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-track-show-counts t
  "Show unread message counts in the track indicator."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-track-indicators
  '((mention . "@")
    (dm . "*")
    (activity . ""))
  "Alist mapping activity types to prefix indicators.
An explicit nil or empty value hides that indicator.  Missing entries
fall back to the legacy indicator for their activity type."
  :type '(alist :key-type (choice (const mention)
                                  (const dm)
                                  (const activity))
                :value-type (choice (const :tag "No indicator" nil)
                                    string))
  :group 'clatter)

(defcustom clatter-track-count-style 'suffix
  "Style used to display unread counts in the activity tracker.
The value `suffix' renders the legacy :N form.  `parens' renders the
count in parentheses, e.g. #chan (10).  `superscript' and
`subscript' raise or lower the exact count.  `glyph' renders one as ·,
two as :, three as ⋮, and larger counts as a raised +N.  `none' hides
the count.  `clatter-track-show-counts' remains the master switch."
  :type '(choice (const :tag "Colon suffix (:N)" suffix)
                 (const :tag "Parenthesized count" parens)
                 (const :tag "Raised number" superscript)
                 (const :tag "Lowered number" subscript)
                 (const :tag "Compact glyphs" glyph)
                 (const :tag "No count" none))
  :group 'clatter)

(defcustom clatter-track-layout 'legacy
  "Tracker mode-line presentation.
`legacy' renders the single cached string with full buffer-name labels.
`strip' renders a bracketed, space-efficient strip that fits a budget
derived from `clatter-track-max-width', overflowing hidden
conversations as an attention-first `+N' summary."
  :type '(choice (const :tag "Legacy cached string" legacy)
                 (const :tag "Adaptive strip" strip))
  :set (lambda (sym val)
         (set-default sym val)
         (when (and (fboundp 'clatter-track--layout-active-p)
                    (clatter-track--layout-active-p))
           (clatter-track--update)
           (force-mode-line-update t)))
  :group 'clatter)

(defcustom clatter-track-max-width 0.4
  "Maximum width of the adaptive tracker strip.
Only used when `clatter-track-layout' is `strip'.  A float in [0.0,1.0]
allocates that fraction of the rendering window's body width; zero
hides the strip.  A nonnegative integer allocates that many columns
\(converted to pixels on graphical frames using the mode-line base
face).  Values outside these ranges are rejected by the Custom setter."
  :type '(choice (float :tag "Window width fraction")
                 (integer :tag "Columns"))
  :set (lambda (sym val)
         (unless (or (and (floatp val) (>= val 0.0) (<= val 1.0))
                     (and (integerp val) (>= val 0)))
           (signal 'args-out-of-range
                   (list sym val "float in [0.0,1.0] or nonnegative integer")))
         (set-default sym val)
         (when (and (fboundp 'clatter-track--layout-active-p)
                    (clatter-track--layout-active-p))
           (clatter-track--update)))
  :group 'clatter)

(defcustom clatter-track-global-mode-line t
  "Install the activity indicator into the global `mode-line-format'.
When non-nil (the default), `clatter-track-mode' appends
`clatter-track-mode-line-item' to the default `mode-line-format' so the
crumbs are visible in every window.

Set this to nil when you embed `clatter-track-mode-line-item' in a
custom mode line of your own (for example a `doom-modeline' segment).
clatter then leaves the global `mode-line-format' untouched, avoiding a
duplicate indicator.  You are responsible for adding the item to your
modeline yourself.  Setting this through Customize or `setopt' adds or
removes the item from the global format immediately."
  :type 'boolean
  :group 'clatter
  :set (lambda (sym val)
         (set-default sym val)
         (when (fboundp 'clatter-track--sync-global-mode-line)
           (clatter-track--sync-global-mode-line))))

(define-obsolete-variable-alias 'clatter-track-in-buffer-mode-line
  'clatter-track-show-in-clatter-buffers "0.9.0")

(defcustom clatter-track-show-in-clatter-buffers nil
  "Show activity crumbs in each clatter buffer's own mode line.
By default the track indicator is appended to the global
`mode-line-format', which clatter buffers override with their own
buffer-local mode line, so the crumbs are not visible while you are in a
clatter buffer.  When this is non-nil, the indicator is also inserted
into each clatter buffer's mode line (just before the trailing spaces),
so the crumbs appear everywhere.  Setting this through Customize or
`setopt' updates all existing clatter buffers immediately."
  :type 'boolean
  :group 'clatter
  :set (lambda (sym val)
         (set-default sym val)
         (when (fboundp 'clatter-track--refresh-mode-lines)
           (clatter-track--refresh-mode-lines))))

;; --- Faces ---
(defface clatter-track-count
  '((t :height 1.0))
  "Face applied to the unread-count span in both tracker layouts.
Composed over the entry's urgency or muted face, so only the count's
typography changes; relative `:height' only has a visual effect on
graphical frames."
  :group 'clatter)

(defface clatter-track-separator
  '((t :inherit shadow))
  "Face used only for strip brackets and separators."
  :group 'clatter)

(defface clatter-track-mention
  '((t :inherit error :weight bold))
  "Face for channels with unread mentions."
  :group 'clatter)

(defface clatter-track-activity
  '((t :inherit font-lock-string-face))
  "Face for channels with unread messages."
  :group 'clatter)

(defface clatter-track-muted
  '((t :inherit font-lock-doc-face))
  "Face for muted channels with activity."
  :group 'clatter)

(defface clatter-track-dm
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for DM buffers with unread messages."
  :group 'clatter)

;; --- Track state ---

(defvar clatter-track--timer nil
  "Timer for periodic mode-line updates.")

(defvar clatter-track--string ""
  "Current track string for the mode-line.")
(defvar clatter-track--strip-entries nil
  "Prepared strip-entry plists for the adaptive layout, or nil.
Refreshed by `clatter-track--update'; never collected during redisplay.
Keys: :buffer :label :suffix :count-str :marker :face :type :help
:unread :mention :dm :full-name.")

(defvar clatter-track--rendered-layout 'legacy
  "Layout used by the most recent `clatter-track--update'.")

(defvar clatter-track--switch-origin nil
  "Buffer the user was in before the current `clatter-track-switch' sequence.
Captured on the first successful switch of a sequence and cleared when
the sequence returns to this buffer on activity exhaustion.")

;; --- Track info collection ---

(defun clatter-track--drop-vowels (body)
  "Return BODY with ASCII vowels removed, keeping at least one char."
  (let (chars)
    (dotimes (i (length body))
      (let ((c (aref body i)))
        (unless (memq c '(?a ?e ?i ?o ?u ?A ?E ?I ?O ?U))
          (push c chars))))
    (let ((res (apply #'string (nreverse chars))))
      (if (string-empty-p res)
          (substring body 0 (min 1 (length body)))
        res))))

(defun clatter-track--syllable-abbrev (body)
  "Return the first char of each segment of BODY.
Segments split on `-', `_', `/' and on uppercase (CamelCase) boundaries."
  (let (segs cur)
    (dotimes (i (length body))
      (let ((c (aref body i)))
        (cond
         ((memq c '(?- ?_ ?/))
          (when cur (push cur segs))
          (setq cur nil))
         ((and (> i 0) (<= ?A c) (<= c ?Z))
          (when cur (push cur segs))
          (setq cur (string c)))
         (t
          (setq cur (concat (or cur "") (string c)))))))
    (when cur (push cur segs))
    (let ((abbr (apply #'concat (mapcar (lambda (s) (substring s 0 1))
                                        (nreverse segs)))))
      (if (string-empty-p abbr) body abbr))))

(defun clatter-track--style-body (body)
  "Apply the active shortening style to a channel name BODY (no prefix).
The style is selected by `clatter-track-shorten': `drop-vowels' or
`syllable' transform the body; an integer (truncate) or nil leaves it
intact, since truncation is applied separately as a cap."
  (pcase clatter-track-shorten
    ('drop-vowels (clatter-track--drop-vowels body))
    ('syllable (downcase (clatter-track--syllable-abbrev body)))
    (_ body)))

(defun clatter-track--shorten-target (target &optional cap)
  "Shorten channel TARGET per `clatter-track-shorten', capped at CAP chars.
CAP defaults to `clatter-track-shorten' when that is an integer, or the
full styled length otherwise.  Non-channel targets (nicks, `*server*')
are returned unchanged."
  (if (or (null target) (not (string-match-p "^[#&!+]" target)))
      target
    (let* ((prefix (substring target 0 1))
           (styled (clatter-track--style-body (substring target 1)))
           (limit (or cap
                      (and (integerp clatter-track-shorten)
                           clatter-track-shorten)
                      (length styled))))
      (concat prefix (substring styled 0 (min limit (length styled)))))))

(defun clatter-track--uniquify-short-names (infos)
  "Disambiguate colliding shortened channel names in INFOS.
Extends each colliding channel's cap by one char until its short name is
unique or the full styled body is exhausted.  Mutates each channel info's
:name; non-channel infos are left alone.  Returns INFOS."
  (let ((entries
         (delq nil
          (mapcar
           (lambda (info)
             (let ((raw (with-current-buffer (plist-get info :buffer)
                          clatter--target)))
               (when (and raw (string-match-p "^[#&!+]" raw))
                 (let* ((styled (clatter-track--style-body (substring raw 1)))
                        (base (or (and (integerp clatter-track-shorten)
                                       clatter-track-shorten)
                                  (length styled))))
                   (list info raw (substring raw 0 1) styled base)))))
           infos))))
    (when entries
      (let (changed)
        (while (progn
                 (setq changed nil)
                 (let ((names (mapcar (lambda (e)
                                        (clatter-track--shorten-target
                                         (nth 1 e) (nth 4 e)))
                                      entries)))
                   (dotimes (i (length entries))
                     (let* ((e (nth i entries))
                            (name (nth i names))
                            (styled (nth 3 e))
                            (cap (nth 4 e)))
                       (when (and (< cap (length styled))
                                  (cl-some (lambda (j)
                                             (and (/= j i)
                                                  (equal (nth j names) name)))
                                           (number-sequence
                                            0 (1- (length entries)))))
                         (setf (nth 4 e) (1+ cap))
                         (setq changed t))))
                   changed))))
      (dolist (e entries)
        (setf (plist-get (nth 0 e) :name)
              (clatter-track--shorten-target (nth 1 e) (nth 4 e)))))
    infos))

(defun clatter-track--buffer-info (buf)
  "Return activity info for BUF as plist, or nil if no activity.
Plist keys: :buffer :name :full-name :unread :mention :muted :dm"
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (and (derived-mode-p 'clatter-mode)
                 clatter--target
                 (not (member clatter--target clatter-track-exclude-targets))
                 (> clatter--unread-count 0))
        (let* ((target clatter--target)
               (is-channel (and target (string-match-p "^[#&!+]" target)))
               (is-muted (member target clatter-track-muted-channels))
               (display-name (if clatter-track-shorten
                                 (clatter-track--shorten-target target)
                               (buffer-name buf))))
          (list :buffer buf
                :name display-name
                ;; Keep the unabbreviated target so tooltips can show the full
                ;; channel name even when `clatter-track-shorten' truncates it.
                :full-name target
                :unread clatter--unread-count
                :mention clatter--has-mention
                :muted is-muted
                :dm (not is-channel)))))))

(defun clatter-track--collect ()
  "Collect activity info from all clatter buffers.
Returns list of plists sorted by priority: mentions > DMs > activity."
  (let ((infos nil))
    (dolist (buf (buffer-list))
      (let ((info (clatter-track--buffer-info buf)))
        (when info
          (push info infos))))
    ;; Sort: mentions first, then DMs, then regular activity
    (let ((sorted (sort infos
                        (lambda (a b)
                          (let ((a-mention (plist-get a :mention))
                                (b-mention (plist-get b :mention))
                                (a-dm (plist-get a :dm))
                                (b-dm (plist-get b :dm)))
                            (cond
                             ((and a-mention (not b-mention)) t)
                             ((and b-mention (not a-mention)) nil)
                             ((and a-dm (not b-dm)) t)
                             ((and b-dm (not a-dm)) nil)
                             (t (> (plist-get a :unread)
                                   (plist-get b :unread)))))))))
      ;; Disambiguate colliding shortened channel names after sorting.
      (when clatter-track-shorten
        (clatter-track--uniquify-short-names sorted))
      sorted)))

;; --- Format track string ---

(defun clatter-track--entry-type (info)
  "Return the primary activity type represented by INFO."
  (cond
   ((plist-get info :mention) 'mention)
   ((plist-get info :dm) 'dm)
   (t 'activity)))

(defun clatter-track--legacy-indicator (type)
  "Return the legacy tracker indicator for TYPE."
  (pcase type
    ('mention "@")
    ('dm "*")
    (_ "")))

(defun clatter-track--indicator (type)
  "Return the configured tracker indicator for TYPE."
  (let ((entry (assq type clatter-track-indicators)))
    (if entry
        (or (cdr entry) "")
      (clatter-track--legacy-indicator type))))

(defun clatter-track--legacy-face (type)
  "Return the legacy tracker face for TYPE."
  (pcase type
    ('mention 'clatter-track-mention)
    ('dm 'clatter-track-dm)
    ('muted 'clatter-track-muted)
    (_ 'clatter-track-activity)))

(defun clatter-track--face (type muted)
  "Return the configured tracker face for TYPE, respecting MUTED."
  (let* ((face-type (if muted 'muted type))
         (entry (assq face-type clatter-track-faces-alist)))
    (or (cdr entry) (clatter-track--legacy-face face-type))))

(defun clatter-track--format-count (unread)
  "Format UNREAD according to the configured tracker count style."
  (if (or (not clatter-track-show-counts)
          (<= unread 0)
          (eq clatter-track-count-style 'none))
      ""
    (pcase clatter-track-count-style
      ('parens (format " (%d)" unread))
      ('superscript
       (propertize (number-to-string unread) 'display '(raise 0.3)))
      ('subscript
       (propertize (number-to-string unread) 'display '(raise -0.3)))
      ('glyph
       (pcase unread
         (1 "·")
         (2 ":")
         (3 "⋮")
         (_ (propertize (format "+%d" unread) 'display '(raise 0.3)))))
      (_ (format ":%d" unread)))))

(defun clatter-track--format-entry (info)
  "Format a single track INFO plist into a propertized string."
  (let* ((name (plist-get info :name))
         (full-name (or (plist-get info :full-name) name))
         (unread (plist-get info :unread))
         (mention (plist-get info :mention))
         (muted (plist-get info :muted))
         (type (clatter-track--entry-type info))
         (face (clatter-track--face type muted))
         (prefix (clatter-track--indicator type))
         (count-str (clatter-track--format-count unread)))
    (let ((entry (propertize (format "%s%s%s" prefix name count-str)
                             'face face
                             'help-echo (format "%s - %d unread%s"
                                                full-name unread
                                                (if mention " (mentioned)" ""))
                             'mouse-face 'highlight
                             'local-map (clatter-track--make-click-map
                                         (plist-get info :buffer)))))
      ;; Compose the count face over the entry face on the count span
      ;; only; the name and indicator keep the entry's own styling.
      (when (> (length count-str) 0)
        (add-face-text-property (- (length entry) (length count-str))
                                (length entry)
                                'clatter-track-count nil entry))
      entry)))

(defun clatter-track--make-click-map (buffer)
  "Return a keymap that switches to BUFFER on click."
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1]
      (lambda (_event)
        (interactive "e")
        (when (buffer-live-p buffer)
          (switch-to-buffer buffer)
          (clatter-clear-activity buffer))))
    map))

(defun clatter-track--format-string ()
  "Build the full track indicator string."
  (let ((infos (clatter-track--collect)))
    (if infos
        (concat " ["
                (mapconcat #'clatter-track--format-entry infos " ")
                "]")
      "")))

;; --- Adaptive strip layout ---

;; Widths are measured in pixels on GUI frames (native
;; `string-pixel-width') and in display columns on terminal frames,
;; where face heights are ignored.
(require 'subr-x)                  ; `string-pixel-width'
(require 'mule-util)               ; `truncate-string-to-width'

(defun clatter-track--layout-active-p ()
  "Return non-nil when the tracker update timer is running."
  (and clatter-track--timer t))

(defun clatter-track--frame-graphic-p ()
  "Return non-nil when the selected frame uses a window system."
  (display-graphic-p))

(defun clatter-track--strip-measure (string)
  "Return the width of STRING in pixels on GUI, columns otherwise."
  (if (clatter-track--frame-graphic-p)
      (string-pixel-width string)
    (string-width string)))

(defun clatter-track--strip-ellipsis ()
  "Return the ellipsis string displayable on the selected frame."
  (if (char-displayable-p ?…) "…" "..."))

(defun clatter-track--strip-separator ()
  "Return the separator string displayable on the selected frame."
  (if (char-displayable-p ?·) " · " " | "))

(defun clatter-track--strip-face-list (&rest faces)
  "Return FACES composed over the window's mode-line base face."
  (let ((base (if (mode-line-window-selected-p)
                  'mode-line-active
                'mode-line-inactive)))
    (nconc (delq nil faces) (list base))))

(defun clatter-track--strip-open-str ()
  "Return the styled strip opening bracket with its leading space."
  (propertize " [" 'face
              (clatter-track--strip-face-list
               'clatter-track-separator)))

(defun clatter-track--strip-close-str ()
  "Return the styled strip closing bracket."
  (propertize "]" 'face
              (clatter-track--strip-face-list
               'clatter-track-separator)))

(defun clatter-track--strip-sep-str ()
  "Return the styled strip separator."
  (propertize (clatter-track--strip-separator) 'face
              (clatter-track--strip-face-list
               'clatter-track-separator)))

(defun clatter-track--strip-cap-name (name cap &optional face)
  "Return NAME cut to CAP columns using its rendered FACE.
Keep the full name whenever it is no wider than its capped form; a
wide ellipsis can cost more pixels than the letters it replaces."
  (if (< (string-width name) cap)
      name
    (let* ((capped (truncate-string-to-width
                    name cap 0 nil (clatter-track--strip-ellipsis)))
           (faces (clatter-track--strip-face-list face))
           (full-rendered (propertize (copy-sequence name) 'face faces))
           (capped-rendered
            (propertize (copy-sequence capped) 'face faces)))
      (if (<= (clatter-track--strip-measure full-rendered)
              (clatter-track--strip-measure capped-rendered))
          name
        capped))))

(defun clatter-track--strip-help (info)
  "Return the full-identity tooltip for collector INFO."
  (format "%s (%s) - %d unread%s"
          (or (plist-get info :full-name) (plist-get info :name))
          (or (and (buffer-live-p (plist-get info :buffer))
                   (buffer-name (plist-get info :buffer)))
              "dead buffer")
          (or (plist-get info :unread) 0)
          (if (plist-get info :mention) " (mentioned)" "")))

(defun clatter-track--prepare-strip-entries (infos)
  "Prepare strip presentation data from collector INFOS.
Returns a list of plists with keys :buffer :label :suffix
:count-str :marker :face :type :unread :mention :dm :full-name :help.
Called from `clatter-track--update' outside redisplay."
  (clatter-track--strip-uniquify-entries
   (delq nil
         (mapcar
          (lambda (info)
            (let ((buf (plist-get info :buffer)))
              (when (buffer-live-p buf)
                (let* ((raw (plist-get info :full-name))
                       (name (plist-get info :name))
                       (label (if clatter-track-shorten
                                  (or name raw)
                                (or raw name)))
                       (type (clatter-track--entry-type info)))
                  (list :buffer buf
                        :label label
                        :suffix ""
                        :count-str (clatter-track--format-count
                                    (plist-get info :unread))
                        :marker (clatter-track--indicator type)
                        :face (clatter-track--face
                               type (plist-get info :muted))
                        :type type
                        :unread (plist-get info :unread)
                        :mention (plist-get info :mention)
                        :dm (plist-get info :dm)
                        :full-name raw
                        :help (clatter-track--strip-help info))))))
          infos))))

(defun clatter-track--strip-uniquify-entries (entries)
  "Disambiguate colliding strip labels in ENTRIES.
Distinct raw targets with the same shortened label use their raw labels.
Remaining collisions get distinct @NETWORK suffixes when possible, or
@BUFFER-NAME suffixes otherwise."
  (setq entries
        (cl-remove-if-not
         (lambda (entry) (buffer-live-p (plist-get entry :buffer)))
         entries))
  (let ((indices (number-sequence 0 (1- (length entries)))))
    ;; Distinct raw targets with the same shortened label use raw labels.
    (dolist (i indices)
      (let* ((entry (nth i entries))
             (label (plist-get entry :label))
             (same (cl-remove-if-not
                    (lambda (j)
                      (equal label (plist-get (nth j entries) :label)))
                    indices))
             (raws (delete-dups
                    (mapcar (lambda (j)
                              (or (plist-get (nth j entries) :full-name)
                                  (plist-get (nth j entries) :label)))
                            same))))
        (when (> (length raws) 1)
          (dolist (j same)
            (setf (plist-get (nth j entries) :label)
                  (or (plist-get (nth j entries) :full-name)
                      (plist-get (nth j entries) :label)))))))
    ;; Remaining collisions get a stable identity suffix.
    (let (handled)
      (dolist (i indices)
        (unless (memq i handled)
          (let* ((label (plist-get (nth i entries) :label))
                 (same (cl-remove-if-not
                        (lambda (j)
                          (equal label (plist-get (nth j entries) :label)))
                        indices)))
            (setq handled (append same handled))
            (when (> (length same) 1)
              (let* ((networks
                      (mapcar
                       (lambda (j)
                         (buffer-local-value
                          'clatter--network
                          (plist-get (nth j entries) :buffer)))
                       same))
                     (network-suffixes-p
                      (and (cl-every
                            (lambda (network)
                              (and (stringp network)
                                   (not (string-empty-p network))))
                            networks)
                           (= (length (delete-dups
                                       (copy-sequence networks)))
                              (length networks)))))
                (dolist (j same)
                  (let* ((entry (nth j entries))
                         (buf (plist-get entry :buffer))
                         (identity
                          (if network-suffixes-p
                              (buffer-local-value 'clatter--network buf)
                            (buffer-name buf))))
                    (setf (plist-get entry :suffix)
                          (concat "@" identity)))))))))))
  entries)

(defvar clatter-track--strip-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] 'clatter-track--strip-click)
    map)
  "Shared keymap for every strip span.
Click targets are resolved from the `clatter-track-target' text
property, so no per-entry closures or keymaps are allocated.")

(defun clatter-track--strip-click (event)
  "Act on mouse-1 EVENT over a strip span.
Switches to the target buffer and clears its activity, or opens the
activity list for an overflow span.  Acts only in the clicked window."
  (interactive "e")
  (let* ((posn (event-start event))
         (window (posn-window posn))
         (posn-str (posn-string posn))
         (target (and (consp posn-str)
                      (stringp (car posn-str))
                      (get-text-property
                       (or (cdr posn-str) 0)
                       'clatter-track-target (car posn-str)))))
    (if (not (window-live-p window))
        (clatter-track--update)
      (select-window window)
      (cond
       ((bufferp target)
        (if (buffer-live-p target)
            (progn
              (switch-to-buffer target)
              (clatter-clear-activity target)
              (clatter-track--update))
          (clatter-track--update)))
       ((eq target 'overflow)
        (clatter-track-list))
       (t (clatter-track--update))))))

(defun clatter-track--strip-span (string face help target)
  "Return propertized STRING with FACE, HELP and click TARGET.
TARGET is a buffer or the symbol `overflow'."
  (propertize string
              'face face
              'help-echo help
              'mouse-face 'highlight
              'clatter-track-target target
              'local-map clatter-track--strip-keymap))

(defun clatter-track--strip-marker-span (entry)
  "Return the styled indicator span for strip ENTRY."
  (clatter-track--strip-span
   (or (plist-get entry :marker) "")
   (clatter-track--strip-face-list (plist-get entry :face))
   (plist-get entry :help)
   (plist-get entry :buffer)))

(defun clatter-track--strip-name-span (entry name)
  "Return the styled name span for ENTRY, label NAME plus suffix."
  (clatter-track--strip-span
   (concat name (or (plist-get entry :suffix) ""))
   (clatter-track--strip-face-list (plist-get entry :face))
   (plist-get entry :help)
   (plist-get entry :buffer)))

(defun clatter-track--strip-count-span (entry)
  "Return the styled count span for strip ENTRY.
The count face composes ahead of the entry's urgency or muted face."
  (clatter-track--strip-span
   (or (plist-get entry :count-str) "")
   (clatter-track--strip-face-list 'clatter-track-count
                                   (plist-get entry :face))
   (plist-get entry :help)
   (plist-get entry :buffer)))

(defun clatter-track--strip-entry-span (entry name)
  "Return the full styled span for ENTRY with label NAME."
  (concat (clatter-track--strip-marker-span entry)
          (clatter-track--strip-name-span entry name)
          (clatter-track--strip-count-span entry)))

(defun clatter-track--make-overflow-data
    (convs unread mentions dms top)
  "Return overflow data from aggregate counts and highest-priority TOP."
  (let ((top-type (and top (plist-get top :type))))
    (list :convs convs
          :unread unread
          :mentions mentions
          :dms dms
          :marker (and top (memq top-type '(mention dm))
                       (plist-get top :marker))
          :face (and top (plist-get top :face))
          :help (format
                 "Hidden: %d conversation%s, %d unread message%s total (%d with mentions, %d DM%s). Click to open the activity list."
                 convs (if (= convs 1) "" "s")
                 unread (if (= unread 1) "" "s")
                 mentions
                 dms (if (= dms 1) "" "s")))))

(defun clatter-track--strip-overflow-data (entries)
  "Return overflow summary data for hidden strip ENTRIES."
  (let ((convs 0)
        (unread 0)
        (mentions 0)
        (dms 0))
    (dolist (entry entries)
      (cl-incf convs)
      (cl-incf unread (or (plist-get entry :unread) 0))
      (when (plist-get entry :mention) (cl-incf mentions))
      (when (plist-get entry :dm) (cl-incf dms)))
    (clatter-track--make-overflow-data
     convs unread mentions dms (car entries))))

(defun clatter-track--strip-overflow-span (data)
  "Return the styled +N overflow span for overflow DATA."
  (let* ((marker (or (plist-get data :marker) ""))
         (body (concat "+" (number-to-string (plist-get data :convs)))))
    (clatter-track--strip-span
     (if (string-empty-p marker) body (concat marker body))
     (clatter-track--strip-face-list (plist-get data :face))
     (plist-get data :help)
     'overflow)))

(defun clatter-track--strip-overflow-forms (data budget)
  "Return the first overflow form of DATA fitting BUDGET, or nil.
Wrapped, marked, plain and bare forms are tried in order; every
nonempty form stays clickable with the full summary tooltip."
  (let* ((plain (concat "+" (number-to-string (plist-get data :convs))))
         (marker (or (plist-get data :marker) ""))
         (marked (if (string-empty-p marker) plain (concat marker plain)))
         (face (clatter-track--strip-face-list (plist-get data :face)))
         (help (plist-get data :help))
         (span (lambda (s)
                 (clatter-track--strip-span s face help 'overflow))))
    (cl-some
     (lambda (form)
       (and (<= (clatter-track--strip-measure form) budget) form))
     (list (concat (clatter-track--strip-open-str)
                   (funcall span marked)
                   (clatter-track--strip-close-str))
           (funcall span marked)
           (funcall span plain)
           (funcall span "+")))))

(defun clatter-track--strip-total-width (strings sep-w)
  "Sum measured widths of STRINGS with SEP-W between each."
  (let ((acc 0)
        (first t))
    (dolist (s strings acc)
      (setq acc (+ acc (clatter-track--strip-measure s)
                   (if first 0 sep-w)))
      (setq first nil))))

(defun clatter-track--strip-prefix-count (entries budget)
  "Return the largest leading prefix of ENTRIES that fits BUDGET.
Each entry uses its eight-column name candidate.  Visible widths and
hidden overflow aggregates are each scanned once."
  (let* ((n (length entries))
         (entryv (vconcat entries))
         (spans (make-vector n nil))
         (suffixes (make-vector (1+ n) nil))
         (open-w (clatter-track--strip-measure
                  (clatter-track--strip-open-str)))
         (close-w (clatter-track--strip-measure
                   (clatter-track--strip-close-str)))
         (sep-w (clatter-track--strip-measure
                 (clatter-track--strip-sep-str)))
         (best 0)
         (acc 0))
    (dotimes (i n)
      (let ((entry (aref entryv i)))
        (aset spans i
              (clatter-track--strip-entry-span
               entry
               (clatter-track--strip-cap-name
                (plist-get entry :label) 8 (plist-get entry :face))))))
    (let ((convs 0)
          (unread 0)
          (mentions 0)
          (dms 0))
      (dotimes (offset n)
        (let* ((i (- n offset 1))
               (entry (aref entryv i)))
          (cl-incf convs)
          (cl-incf unread (or (plist-get entry :unread) 0))
          (when (plist-get entry :mention) (cl-incf mentions))
          (when (plist-get entry :dm) (cl-incf dms))
          (aset suffixes i
                (clatter-track--make-overflow-data
                 convs unread mentions dms entry)))))
    (dotimes (i n)
      (setq acc (+ acc (clatter-track--strip-measure (aref spans i))
                   (if (zerop i) 0 sep-w)))
      (let* ((hidden (aref suffixes (1+ i)))
             (ovf-w
              (and hidden
                   (clatter-track--strip-measure
                    (clatter-track--strip-overflow-span hidden)))))
        (when (<= (+ open-w acc close-w
                     (if hidden (+ sep-w ovf-w) 0))
                  budget)
          (setq best (1+ i)))))
    best))

(defun clatter-track--strip-name-caps (visible width)
  "Return per-entry name caps fitting WIDTH, or nil at cap eight.
Finds the largest common display-column cap by binary search from
eight columns to the widest full name, then spends remaining width
expanding names in priority order."
  (let* ((n (length visible))
         (full-ws (mapcar (lambda (e) (string-width (plist-get e :label)))
                           visible))
         ;; The caller has already accounted for brackets, separators
         ;; and overflow; only name-span widths are fitted here.
         (width-at
          (lambda (caps)
            (apply #'+
                   (cl-mapcar (lambda (e cap)
                                (clatter-track--strip-measure
                                 (clatter-track--strip-entry-span
                                  e (clatter-track--strip-cap-name
                                     (plist-get e :label) cap
                                     (plist-get e :face)))))
                              visible caps))))
         (fits (lambda (caps) (<= (funcall width-at caps) width))))
    (when (funcall fits (make-list n 8))
      (let ((lo 8)
            (hi (apply #'max (cons 8 full-ws)))
            (cap 8))
        (while (<= lo hi)
          (let ((mid (floor (+ lo hi) 2)))
            (if (funcall fits (make-list n mid))
                (progn (setq cap mid) (setq lo (1+ mid)))
              (setq hi (1- mid)))))
        (let ((caps (make-list n cap)))
          ;; Priority distribution: expand each name as far as it fits.
          (dotimes (i n)
            (let ((ilo cap)
                  (ihi (nth i full-ws))
                  (ibest (nth i caps)))
              (while (<= ilo ihi)
                (let ((mid (floor (+ ilo ihi) 2)))
                  (let ((trial (append (cl-subseq caps 0 i)
                                       (list mid)
                                       (nthcdr (1+ i) caps))))
                    (if (funcall fits trial)
                        (progn (setq ibest mid ilo (1+ mid))
                               (setq caps trial))
                      (setq ihi (1- mid))))))
              (setf (nth i caps) ibest)))
          caps)))))

(defun clatter-track--strip-build (entries k budget)
  "Return the assembled strip for the first K ENTRIES within BUDGET.
Expands name caps to spend remaining space, then re-measures the
final assembled string.  Returns nil when the prefix cannot fit; the
caller demotes its last visible entry to overflow."
  (let* ((visible (cl-subseq entries 0 k))
         (hidden (nthcdr k entries))
         (ovf (and hidden (clatter-track--strip-overflow-data hidden)))
         (open (clatter-track--strip-open-str))
         (close (clatter-track--strip-close-str))
         (sep (clatter-track--strip-sep-str))
         (ovf-span (and ovf (clatter-track--strip-overflow-span ovf)))
         (sep-w (clatter-track--strip-measure sep))
         (fixed-w (+ (clatter-track--strip-measure open)
                     (clatter-track--strip-measure close)
                     (* (1- k) sep-w)
                     (if ovf-span
                         (+ sep-w (clatter-track--strip-measure ovf-span))
                       0))))
    (when (<= fixed-w budget)
      (let ((caps (clatter-track--strip-name-caps
                   visible (- budget fixed-w))))
        (when caps
          (let* ((spans (cl-mapcar (lambda (e cap)
                                   (clatter-track--strip-entry-span
                                    e (clatter-track--strip-cap-name
                                       (plist-get e :label) cap
                                       (plist-get e :face))))
                                 visible caps))
                 (final (concat open
                                (mapconcat #'identity spans sep)
                                (if ovf-span (concat sep ovf-span) "")
                                close)))
            (when (<= (clatter-track--strip-measure final) budget)
              final)))))))

(defun clatter-track--strip-final (entries k budget)
  "Return the strip for prefix K of ENTRIES within BUDGET.
When font shaping makes an assembled string exceed the budget, moves
the last visible entry into overflow and retries; falls back to
summary-only forms when no entry fits."
  (let ((result nil)
        (k k))
    (while (and (not result) (> k 0))
      (setq result (clatter-track--strip-build entries k budget))
      (unless result (cl-decf k)))
    (or result
        (clatter-track--strip-overflow-forms
         (clatter-track--strip-overflow-data entries) budget)
        "")))

(defun clatter-track--format-strip (entries budget)
  "Return the fitted strip string for prepared ENTRIES within BUDGET.
BUDGET is in pixels on graphical frames and display columns on
terminal frames.  Returns \"\" for empty activity or a zero budget."
  (if (or (null entries) (<= budget 0))
      ""
    (let ((k (clatter-track--strip-prefix-count entries budget)))
      (if (zerop k)
          (or (clatter-track--strip-overflow-forms
               (clatter-track--strip-overflow-data entries) budget)
              "")
        (clatter-track--strip-final entries k budget)))))

(defun clatter-track--strip-budget ()
  "Return the strip budget for the selected window.
Pixels on graphical frames, display columns on terminal frames,
clamped to the window body width."
  (let* ((window (selected-window))
         (graphic (clatter-track--frame-graphic-p))
         (window-w (if graphic
                       (window-body-width window t)
                     (window-body-width window)))
         (budget
          (cond
           ((and (floatp clatter-track-max-width)
                 (>= clatter-track-max-width 0.0)
                 (<= clatter-track-max-width 1.0))
            (floor (* clatter-track-max-width window-w)))
           ((integerp clatter-track-max-width)
            (if graphic
                (floor
                 (* clatter-track-max-width
                    (clatter-track--strip-measure
                     (propertize
                      "0" 'face
                      (if (mode-line-window-selected-p)
                          'mode-line-active
                        'mode-line-inactive)))))
              clatter-track-max-width))
           (t 0))))
    (max 0 (min budget window-w))))

(defun clatter-track--mode-line ()
  "Return the tracker string for the selected window.
The legacy layout returns the cached global string; the strip layout
fits the prepared snapshot to the current window's budget.  Resizing
therefore needs no extra timer, hook or per-window cache."
  (if (eq clatter-track-layout 'strip)
      (clatter-track--format-strip
       clatter-track--strip-entries (clatter-track--strip-budget))
    clatter-track--string))

;; --- Mode-line integration ---

(defvar clatter-track-mode-line-item
  '(:eval (clatter-track--mode-line))
  "Mode-line construct showing clatter activity.
Renders the legacy cached string or the adaptive strip according to
`clatter-track-layout'.  Add this to a custom mode line (for example a
`doom-modeline' segment) and set `clatter-track-global-mode-line' to
nil so clatter does not also append it to the global
`mode-line-format'.")

(put 'clatter-track-mode-line-item 'risky-local-variable t)

(defun clatter-track--mode-line-item-present-p (format)
  "Return non-nil when the track item appears anywhere in mode-line FORMAT.
The search descends into nested constructs, so an item another package
embedded inside its own format still counts as present.  Items produced
by a function call inside an `:eval' form cannot be found this way;
users who embed the item that way should set
`clatter-track-global-mode-line' to nil to avoid a duplicate."
  (cond
   ((eq format 'clatter-track-mode-line-item) t)
   ((consp format)
    (or (clatter-track--mode-line-item-present-p (car format))
        (clatter-track--mode-line-item-present-p (cdr format))))))

(defun clatter-track--insert-mode-line-item (format)
  "Return mode-line FORMAT with the track item before the trailing spaces.
If the item is already present, FORMAT is returned unchanged."
  (if (clatter-track--mode-line-item-present-p format)
      format
    (let ((tail (member 'mode-line-end-spaces format)))
      (if tail
          (append (butlast format (length tail))
                  (list 'clatter-track-mode-line-item)
                  tail)
        (append format (list 'clatter-track-mode-line-item))))))

(defun clatter-track--sync-global-mode-line ()
  "Add or remove the track item from the global `mode-line-format'.
Adds the item when `clatter-track-global-mode-line' is non-nil and it is
not already present; removes it when the option is nil.  Used both when
`clatter-track-mode' is enabled and when the option is changed live."
  (let ((fmt (default-value 'mode-line-format)))
    (set-default 'mode-line-format
                 (if clatter-track-global-mode-line
                     (if (clatter-track--mode-line-item-present-p fmt)
                         fmt
                       (append fmt (list 'clatter-track-mode-line-item)))
                   (delq 'clatter-track-mode-line-item
                         (copy-sequence fmt)))))
  (force-mode-line-update t))

(defun clatter-track--refresh-mode-lines ()
  "Add or remove the track item in all clatter buffers' mode lines.
The presence of the item follows `clatter-track-show-in-clatter-buffers'."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (derived-mode-p 'clatter-mode)
        (setq-local mode-line-format
                    (if clatter-track-show-in-clatter-buffers
                        (clatter-track--insert-mode-line-item mode-line-format)
                      (delq 'clatter-track-mode-line-item
                            (copy-sequence mode-line-format))))
        (force-mode-line-update)))))

(defun clatter-track--update ()
  "Refresh tracker caches and force mode-line redisplay.
The legacy string is skipped when unchanged by characters and
properties; the strip snapshot is always replaced, since buffer
identity, tooltips, muting and count-style properties can change
without changing visible characters.  A layout transition always
forces redisplay, including transitions configured with ordinary
`setq'."
  (let ((layout-changed
         (not (eq clatter-track-layout clatter-track--rendered-layout))))
    (if (eq clatter-track-layout 'strip)
        (progn
          (setq clatter-track--strip-entries
                (clatter-track--prepare-strip-entries
                 (clatter-track--collect)))
          (force-mode-line-update t))
      (let* ((new-string (clatter-track--format-string))
             (string-changed
              (not (equal-including-properties
                    new-string clatter-track--string))))
        (when string-changed
          (setq clatter-track--string new-string))
        (when (or string-changed layout-changed)
          (force-mode-line-update t))))
    (setq clatter-track--rendered-layout clatter-track-layout)))

;; --- Auto-clear on buffer switch ---

(defun clatter-track--selected-window (context)
  "Return CONTEXT's selected live window, or nil.
CONTEXT may be a frame or window as supplied by the window change hooks."
  (let ((window
         (cond
          ((framep context) (frame-selected-window context))
          ((windowp context) context)
          (t (selected-window)))))
    (when (and (window-live-p window)
               (eq window (frame-selected-window (window-frame window))))
      window)))

(defun clatter-track--on-buffer-switch (window)
  "Clear activity for the Clatter buffer selected in WINDOW."
  (when-let* ((window (clatter-track--selected-window window))
              (buffer (window-buffer window)))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'clatter-mode)
                 (> clatter--unread-count 0))
        (clatter-clear-activity buffer)
        (clatter-track--update)))))

;; --- Consult integration ---

(defun clatter-track-buffer-source ()
  "Consult buffer source for clatter buffers with activity.
Use with `consult-buffer' by adding to `consult-buffer-sources'."
  (let ((infos (clatter-track--collect)))
    (mapcar (lambda (info)
              (buffer-name (plist-get info :buffer)))
            infos)))

(defvar clatter-track--consult-source
  (when (featurep 'consult)
    (list :name "IRC Activity"
          :narrow ?i
          :category 'buffer
          :face 'clatter-track-activity
          :items #'clatter-track-buffer-source
          :action (lambda (name)
                    (let ((buf (get-buffer name)))
                      (when buf
                        (switch-to-buffer buf)
                        (clatter-clear-activity buf))))))
  "Consult source for clatter buffers with activity.
Add to `consult-buffer-sources' to enable.")

;; --- Interactive commands ---

(defun clatter-track-clear-all ()
  "Clear activity and record the latest read time for every Clatter target.
Return the number of live target buffers processed.  Muted and excluded
targets are included because this command clears Clatter's entire read state."
  (interactive)
  (let ((buffers (clatter-all-buffers)))
    (dolist (buffer buffers)
      (clatter-clear-activity buffer))
    (clatter-track--update)
    (when (called-interactively-p 'interactive)
      (message "Cleared Clatter activity in %d target%s"
               (length buffers) (if (= (length buffers) 1) "" "s")))
    (length buffers)))

(defun clatter-track-switch ()
  "Switch to the clatter buffer with the most urgent activity.
Priority: mentions > DMs > highest unread count.  When all active
clatter buffers have been visited, the next invocation returns to the
buffer you started switching from (see
`clatter-track-switch-return-to-origin')."
  (interactive)
  (let ((infos (clatter-track--collect)))
    (if infos
        (let ((buf (plist-get (car infos) :buffer)))
          ;; Capture the origin on the first switch of a sequence.
          (when (and clatter-track-switch-return-to-origin
                     (null clatter-track--switch-origin))
            (setq clatter-track--switch-origin (current-buffer)))
          (switch-to-buffer buf)
          (clatter-clear-activity buf))
      (if (and clatter-track-switch-return-to-origin
               clatter-track--switch-origin
               (buffer-live-p clatter-track--switch-origin))
          (let ((origin clatter-track--switch-origin))
            (setq clatter-track--switch-origin nil)
            (switch-to-buffer origin)
            (message "Returned to %s" (buffer-name origin)))
        (setq clatter-track--switch-origin nil)
        (message "No clatter activity")))))

(defun clatter--activity-entries ()
  "Return `tabulated-list-entries' for the `*clatter-activity*' buffer.
Each entry id is the clatter buffer object itself, so the commands
below resolve the target buffer at point without parsing the displayed
name."
  (let ((infos (clatter-track--collect))
        entries)
    (dolist (info infos)
      (let* ((buf (plist-get info :buffer))
             ;; The activity list always shows the full buffer name
             ;; (clatter:network/#channel), ignoring `clatter-track-shorten';
             ;; shortening only applies to the compact mode-line indicator.
             (name (buffer-name buf))
             (full-name (or (plist-get info :full-name) name))
             (unread (plist-get info :unread))
             (mention (plist-get info :mention))
             (dm (plist-get info :dm))
             (muted (plist-get info :muted))
             (type (clatter-track--entry-type info))
             (face (clatter-track--face type muted))
             (status (cond
                      ((and mention muted) "mention (muted)")
                      (mention "mention")
                      ((and dm muted) "DM (muted)")
                      (dm "DM")
                      (muted "muted")
                      (t ""))))
        (push (list buf
                    (vector (cons name
                                  (list 'face face
                                        'mouse-face 'highlight
                                        'help-echo
                                        (format "%s - %d unread%s"
                                                full-name unread
                                                (if mention " (mentioned)" ""))
                                        'action
                                        #'clatter-activity--button-action))
                            (number-to-string unread)
                            status))
              entries)))
    (nreverse entries)))

(defun clatter-activity--button-action (_button)
  "Jump to the clatter buffer for the activity entry button at point.
Button activation target for the `Buffer' column of `*clatter-activity*'."
  (clatter-activity-jump))

(defun clatter-activity-jump ()
  "Switch to the clatter buffer for the activity entry at point.
Like `clatter-track-switch', selects the buffer and clears its activity."
  (interactive)
  (let ((buf (tabulated-list-get-id)))
    (if (buffer-live-p buf)
        (progn
          (switch-to-buffer buf)
          (clatter-clear-activity buf))
      (message "No clatter buffer at point"))))

(defun clatter-activity-jump-other-window ()
  "Switch to the clatter buffer at point in another window."
  (interactive)
  (let ((buf (tabulated-list-get-id)))
    (if (buffer-live-p buf)
        (progn
          (switch-to-buffer-other-window buf)
          (clatter-clear-activity buf))
      (message "No clatter buffer at point"))))

(defun clatter-activity-jump-quit ()
  "Jump to the clatter buffer at point and bury the activity list."
  (interactive)
  (let ((buf (tabulated-list-get-id)))
    (if (buffer-live-p buf)
        (progn
          (quit-window)
          (switch-to-buffer buf)
          (clatter-clear-activity buf))
      (message "No clatter buffer at point"))))

(defun clatter-activity-clear ()
  "Clear activity for the clatter buffer at point and refresh the list."
  (interactive)
  (let ((buf (tabulated-list-get-id)))
    (if (buffer-live-p buf)
        (progn
          (clatter-clear-activity buf)
          (tabulated-list-print t))
      (message "No clatter buffer at point"))))

(defun clatter-activity-clear-all ()
  "Clear activity for every clatter buffer and refresh the list."
  (interactive)
  (clatter-track-clear-all)
  (tabulated-list-print t))

(defun clatter-activity--target-at-point ()
  "Return the `clatter--target' of the clatter buffer at point, or nil."
  (let ((buf (tabulated-list-get-id)))
    (and (buffer-live-p buf)
         (with-current-buffer buf
           (and (derived-mode-p 'clatter-mode) clatter--target)))))

(defun clatter-activity-mute ()
  "Mute the target of the clatter buffer at point and refresh the list."
  (interactive)
  (let ((target (clatter-activity--target-at-point)))
    (if target
        (progn
          (unless (member target clatter-track-muted-channels)
            (push target clatter-track-muted-channels))
          (clatter-track--update)
          (tabulated-list-print t)
          (message "Muted %s" target))
      (message "No clatter buffer at point"))))

(defun clatter-activity-unmute ()
  "Unmute the target of the clatter buffer at point and refresh the list."
  (interactive)
  (let ((target (clatter-activity--target-at-point)))
    (if target
        (progn
          (setq clatter-track-muted-channels
                (delete target clatter-track-muted-channels))
          (clatter-track--update)
          (tabulated-list-print t)
          (message "Unmuted %s" target))
      (message "No clatter buffer at point"))))

(defun clatter-activity-jump-mouse (event)
  "Jump to the clatter buffer for the activity entry clicked at EVENT."
  (interactive "e")
  (let* ((pos (event-start event))
         (win (posn-window pos)))
    (when (window-live-p win)
      (with-current-buffer (window-buffer win)
        (goto-char (posn-point pos))
        (clatter-activity-jump)))))

(defvar clatter-activity-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'clatter-activity-jump)
    (define-key map (kbd "o")   #'clatter-activity-jump-other-window)
    (define-key map (kbd "a")   #'clatter-activity-jump-quit)
    (define-key map (kbd "c")   #'clatter-activity-clear)
    (define-key map (kbd "C")   #'clatter-activity-clear-all)
    (define-key map (kbd "m")   #'clatter-activity-mute)
    (define-key map (kbd "u")   #'clatter-activity-unmute)
    (define-key map [mouse-2]   #'clatter-activity-jump-mouse)
    map)
  "Keymap for `clatter-activity-mode'.
Inherits `tabulated-list-mode', which supplies `n'/`p' (line motion),
`g' (revert/refresh), `S' (sort by column), `TAB'/`S-TAB' (next/previous
button = entry), `<'/`>' (first/last), `SPC'/`DEL' (scroll), `h'/`?'
(help) and `q' (quit).  On top of that:

  \\[clatter-activity-jump]        jump to the buffer at point
  \\[clatter-activity-jump-other-window]  open it in another window
  \\[clatter-activity-jump-quit]   jump to it and bury the list
  \\[clatter-activity-clear]      clear its activity
  \\[clatter-activity-clear-all]  clear activity for every buffer
  \\[clatter-activity-mute]       mute its target
  \\[clatter-activity-unmute]     unmute its target
  \\[clatter-activity-jump-mouse] (mouse-2) jump to the clicked entry")

(define-derived-mode clatter-activity-mode tabulated-list-mode "Clatter Activity"
  "Major mode for the `*clatter-activity*' buffer.
\\{clatter-activity-mode-map}
Lists clatter buffers that currently have unread activity, pre-sorted by
priority (mentions > DMs > unread count).  The buffer reverts with `g'
(\\[revert-buffer]); each revert re-runs `clatter--activity-entries' so the
list always reflects current unread state.  The `Buffer' cell of each row
is also a button: `RET' (or mouse-2) on it jumps to that clatter buffer."
  (setq tabulated-list-format
        ;; NAME WIDTH SORT . PROPS.  Buffer sorts by name; Unread and Status
        ;; keep the natural priority order (nil = not a sort column).
        [("Buffer" 32 t)
         ("Unread" 8 nil :right-align t)
         ("Status" 0 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-entries #'clatter--activity-entries)
  (tabulated-list-init-header))

(defun clatter-track-list ()
  "Display all clatter buffer activity in the `*clatter-activity*' buffer.
Renders the activity summary as a tabulated list; revert it with `g'
\(\\[revert-buffer]) to refresh."
  (interactive)
  (let ((infos (clatter-track--collect)))
    (if (not infos)
        (message "No clatter activity")
      (with-current-buffer (get-buffer-create "*clatter-activity*")
        (clatter-activity-mode)
        (tabulated-list-print t)
        (goto-char (point-min))
        (display-buffer (current-buffer))))))

(defun clatter-track-mute (channel)
  "Add CHANNEL to the muted list."
  (interactive
   (list (completing-read "Mute channel: "
                          (let (channels)
                            (dolist (buf (buffer-list))
                              (with-current-buffer buf
                                (when (and (derived-mode-p 'clatter-mode)
                                           clatter--target
                                           (string-match-p "^[#&!+]" clatter--target))
                                  (push clatter--target channels))))
                            channels))))
  (unless (member channel clatter-track-muted-channels)
    (push channel clatter-track-muted-channels)
    (message "Muted %s" channel)
    (clatter-track--update)))

(defun clatter-track-unmute (channel)
  "Remove CHANNEL from the muted list."
  (interactive
   (list (completing-read "Unmute channel: " clatter-track-muted-channels)))
  (setq clatter-track-muted-channels
        (delete channel clatter-track-muted-channels))
  (message "Unmuted %s" channel)
  (clatter-track--update))

;; --- Enable/disable ---

(defun clatter-track-enable ()
  "Enable the activity tracker."
  (interactive)
  ;; Install mode-line item (honors `clatter-track-global-mode-line')
  (clatter-track--sync-global-mode-line)
  ;; Populate the chosen renderer immediately after installation.
  (clatter-track--update)
  ;; Start update timer
  (when clatter-track--timer
    (cancel-timer clatter-track--timer))
  (setq clatter-track--timer
        (run-with-timer 1 2 #'clatter-track--update))
  ;; Hook into buffer switches
  (add-hook 'window-buffer-change-functions #'clatter-track--window-change)
  (add-hook 'window-selection-change-functions
            #'clatter-track--selection-change)
  ;; Hook into clatter activity
  (add-hook 'clatter-privmsg-hook #'clatter-track--on-activity)
  (add-hook 'clatter-action-hook #'clatter-track--on-activity-action)
  (add-hook 'clatter-notice-hook #'clatter-track--on-activity-notice)
  ;; Register consult source if available
  (when (and (featurep 'consult)
             (boundp 'consult-buffer-sources)
             clatter-track--consult-source)
    (add-to-list 'consult-buffer-sources clatter-track--consult-source))
  (when (called-interactively-p 'interactive)
    (message "[clatter-track] Activity tracking enabled")))

(defun clatter-track-disable ()
  "Disable the activity tracker."
  (interactive)
  (when clatter-track--timer
    (cancel-timer clatter-track--timer)
    (setq clatter-track--timer nil))
  (remove-hook 'window-buffer-change-functions #'clatter-track--window-change)
  (remove-hook 'window-selection-change-functions
               #'clatter-track--selection-change)
  (remove-hook 'clatter-privmsg-hook #'clatter-track--on-activity)
  (remove-hook 'clatter-action-hook #'clatter-track--on-activity-action)
  (remove-hook 'clatter-notice-hook #'clatter-track--on-activity-notice)
  (setq clatter-track--string ""
        clatter-track--strip-entries nil)
  (force-mode-line-update t)
  (when (called-interactively-p 'interactive)
    (message "[clatter-track] Activity tracking disabled")))

(defun clatter-track--window-change (context)
  "Clear activity when CONTEXT's selected window changes buffers."
  (clatter-track--on-buffer-switch context))

(defun clatter-track--selection-change (context)
  "Clear activity when CONTEXT selects an already-visible chat window."
  (clatter-track--on-buffer-switch context))

;; --- Activity hooks ---

(defun clatter-track--on-activity (_conn _sender _target _text &rest _args)
  "Update track on PRIVMSG activity."
  (clatter-track--update))

(defun clatter-track--on-activity-action (_conn _sender _target _text &rest _args)
  "Update track on ACTION activity."
  (clatter-track--update))

(defun clatter-track--on-activity-notice (_conn _sender _target _text &rest _args)
  "Update track on NOTICE activity."
  (clatter-track--update))

;; Tracking is enabled by `clatter-setup' when `clatter-track-enabled'
;; is non-nil, so that merely loading this file has no side effects.

(provide 'clatter-track)

;;; clatter-track.el ends here
