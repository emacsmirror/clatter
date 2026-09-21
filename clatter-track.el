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
nil shows the full buffer name, including the clatter:network/ prefix.
An integer N truncates the channel name body to N chars; e.g. 5 turns
#systemcrafters into #syst.  `drop-vowels' strips vowels from the body.
`syllable' keeps the first char of each CamelCase or delimiter segment,
lowercased (so #system-crafters becomes #sc).  Only channel targets
(#, &, !, +) are shortened; query nicks and `*server*' are unchanged.
Collisions are disambiguated by extending each name until unique."
  :type '(choice (const :tag "Off (full buffer name)" nil)
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
    (propertize (format "%s%s%s" prefix name count-str)
                'face face
                'help-echo (format "%s - %d unread%s"
                                   full-name unread
                                   (if mention " (mentioned)" ""))
                'mouse-face 'highlight
                'local-map (clatter-track--make-click-map (plist-get info :buffer)))))

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

;; --- Mode-line integration ---

(defvar clatter-track-mode-line-item
  '(:eval clatter-track--string)
  "Mode-line construct showing clatter activity.
Add this to a custom mode line (for example a `doom-modeline' segment)
and set `clatter-track-global-mode-line' to nil so clatter does not
also append it to the global `mode-line-format'.")

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
  "Update the track string and force mode-line refresh."
  (let ((new-string (clatter-track--format-string)))
    (unless (string= new-string clatter-track--string)
      (setq clatter-track--string new-string)
      (force-mode-line-update t))))

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
  (setq clatter-track--string "")
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
