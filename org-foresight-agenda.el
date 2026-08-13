;;; org-foresight-agenda.el --- Derived rows inside Org's own agenda -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yoshzucker

;;; Commentary:

;; Org's agenda already draws the day as a column: the time grid interleaves
;; its rules with every timed entry, in order, with `← now' in its place.  What
;; it cannot draw is the part of the day that is not an entry -- the journey to
;; a meeting, the stretch between two of them, the hour the working day is
;; supposed to end.  Those are derived, and until now org-foresight drew them
;; in a grid of its own, which meant drawing the day twice.
;;
;; This file draws them once, in Org's agenda, by handing Org extra items.
;;
;; The mechanism is Org's own.  `org-agenda-add-time-grid-maybe' builds the
;; grid rules by calling `org-agenda-format-item' with a time and no entry
;; behind it; a derived row is the same thing with a different string.  Adding
;; ours to the list before `org-agenda-finalize-entries' sorts it means Org
;; puts each one where its clock says it goes -- we never place anything
;; ourselves, and never write to a file.
;;
;; Two traps, both worth stating because they are quiet and both cost the row
;; its place in the day.
;;
;; The DOTIME argument is concatenated with the item's text before the time is
;; looked for (`org-agenda-search-headline-for-time', on by default), so a row
;; reading "free 2:15" lands at 02:15.  Every item made here is formatted with
;; that search off.
;;
;; And the time is matched with `word-start' and `word-end', which are read
;; against the current syntax table -- so a range stops parsing wherever `-'
;; has been made a word constituent.  Org never meets this, because it formats
;; entries inside the Org buffer they came from; these rows are built in the
;; agenda buffer, outside any such correction.  See
;; `org-foresight-agenda--syntax'.

;;; Code:

(require 'org-foresight-core)
(require 'org-foresight-report)
(require 'org-agenda)

;; Calendar's own unprefixed dynamic variable, which `org-agenda-list' assigns
;; once per day before finalising that day.  Declared rather than defined:
;; giving it a value here would shadow theirs.  Org declares it the same way,
;; warnings and all -- the name is calendar's, and cannot be prefixed.
(with-no-warnings (defvar date))

(defgroup org-foresight-agenda nil
  "Derived rows inside Org's agenda."
  :group 'org-foresight)

(defcustom org-foresight-agenda-inject t
  "Whether foresight adds its derived rows to the agenda.
Set to nil to see the agenda exactly as Org builds it."
  :type 'boolean
  :group 'org-foresight-agenda)

(defcustom org-foresight-gap-net t
  "Whether a gap is offered net of the reserve rather than by the clock.

Interruptions do not happen at a fixed hour, so putting the reserve down as a
block would be a claim about the day that is not true.  What is true is that
they scale with time worked -- so every gap is offered at the fraction of
itself that history says survives, and the reserve stops being an abstraction
subtracted somewhere else.

Nothing is placed either way: the gap is only being reported honestly, and
what goes in it remains entirely yours to decide.  Set to nil to be offered
the clock."
  :type 'boolean
  :group 'org-foresight-agenda)

(defface org-foresight-agenda-derived '((t :inherit italic))
  "Rows nobody wrote down: journeys, gaps, the edges of the working day.

Italic rather than a colour of its own, because the distinction is not what
kind of time it is -- the category column says that -- but whether it exists
in a file.  Nothing here can be edited where it appears, and the slant is the
warning.")

;;;; Making an item

(defconst org-foresight-agenda--syntax
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?- "." table)
    table)
  "The syntax table a derived row is formatted under.

`org-get-time-of-day\' matches with `word-start\' and `word-end\', which are
read against whatever syntax table is current.  A range like \"12:00-13:00\"
therefore stops parsing wherever `-\' has been made a word constituent: Emacs
sees no word boundary after the minutes, finds no time, and the row is filed
as having none -- and a row with no time sorts after everything, landing in a
heap at the foot of the buffer, outside the day it describes.

Org never meets this itself.  Its grid lines carry a single time, and real
entries arrive from timestamps that were parsed long before.  A range handed
to `org-agenda-format-item\' is the one path that reaches the regexp, so it is
this file\'s business to make the ground under it firm.  Inherits the standard
table, so every other character stays as the user has it.")

(defun org-foresight-agenda--item (txt &optional category dotime face marker
                                       stamp)
  "Return TXT as an agenda item at DOTIME, or nil where TXT is empty.

CATEGORY fills the agenda's category column; FACE, when given, is laid over
the whole row without disturbing what is already there.  MARKER makes the row
answer to the agenda's own commands, and STAMP points them at a timestamp
rather than a heading -- see `org-foresight-report--actionable'.

`org-agenda-search-headline-for-time' is bound off: it exists so that a
heading reading \"meet Bob at 10\" is understood, and here it would read the
duration in \"free 2:15\" as the time of day."
  (unless (or (null txt) (string-empty-p txt))
    (let* ((org-agenda-search-headline-for-time nil)
           (item (with-syntax-table org-foresight-agenda--syntax
                   (org-agenda-format-item nil txt nil (or category "") nil
                                           dotime))))
      (when face
        (add-face-text-property 0 (length item) face t item))
      (org-foresight-report--actionable item marker stamp))))

(defun org-foresight-agenda--hhmm (time)
  "Return TIME as \"HH:MM\", the form `org-agenda-format-item' reads."
  (format-time-string "%H:%M" time))

(defun org-foresight-agenda--span (start end)
  "Return START and END as the range `org-agenda-format-item' reads."
  (concat (org-foresight-agenda--hhmm start) "-"
          (org-foresight-agenda--hhmm end)))

;;;; The marks

(defconst org-foresight-agenda-wont-fit "⨯"
  "The mark for something the day cannot hold: one glyph, one meaning.

A journey that cannot have the hours it needs and a task with no gap big
enough to take it are the same news -- this does not fit -- and the row says
which is which without a second character being spent on it.

U+2A2F rather than any of the ballot or multiplication crosses.  PlemolJP has
no glyph for U+2715, U+2717 or U+2718, so each arrives from whatever font the
fallback finds at whatever width that font uses, and the column behind it
steps; U+00D7 is present but full width, which steps by a whole cell.")

(defvar org-foresight-agenda--marks nil
  "The marks the day last drawn actually used.

Read back off the finished rows by `org-foresight-agenda--augment' rather
than tracked while building them, so it cannot drift from what is on the
page.  One day\'s worth: the views that show a key show a single day.")

(defconst org-foresight-agenda--mark-meanings
  `((,org-foresight-agenda-wont-fit . "will not fit")
    ("↳" . "would fit in the gap above"))
  "What each mark means, in the order the key names them.")

(defun org-foresight-agenda-key ()
  "Return a line explaining the marks this agenda used, or nil for none.

Only the marks actually on the page.  A key describing a clash on a day that
has none is explaining a problem the reader does not have, which is a slower
way of saying nothing."
  (when-let ((used (seq-filter (lambda (m)
                                 (member (car m) org-foresight-agenda--marks))
                               org-foresight-agenda--mark-meanings)))
    (mapconcat (pcase-lambda (`(,mark . ,meaning))
                 (concat (propertize mark 'face
                                     'org-foresight-report-overcommitted)
                         " " meaning))
               used "   ")))

(add-to-list 'org-foresight-verdict-extras #'org-foresight-agenda-key t)

;;;; The derived rows

(defun org-foresight-agenda--travel (bands)
  "Return the rows for the journeys among BANDS.

A journey is the clearest case for this whole file: it takes an hour of the
day, it is nowhere in any file, and until it is drawn the day looks an hour
longer than it is."
  (seq-keep
   (lambda (b)
     (when (eq (plist-get b :kind) 'travel)
       (let ((trimmed (plist-get b :trimmed)))
         (org-foresight-agenda--item
          (concat (when trimmed
                    (concat (propertize org-foresight-agenda-wont-fit
                                        'face
                                        'org-foresight-report-overcommitted)
                            " "))
                  (or (plist-get b :title) "→ ?"))
          "travel"
          ;; A journey that was squeezed is drawn at the length it needs, not
          ;; at what was left for it: fifteen minutes shown for a forty-five
          ;; minute drive is the day telling you it works when it does not.
          ;; Org files it by that start, so it appears against the meeting it
          ;; collides with -- which is where the collision is.
          (org-foresight-agenda--span (or (and trimmed
                                               (plist-get b :full-start))
                                          (plist-get b :start))
                                      (plist-get b :end))
          'org-foresight-agenda-derived
          (plist-get b :marker)
          (plist-get b :stamp)))))
   bands))

(defun org-foresight-agenda--keep (cap)
  "Return the fraction of a gap that survives the reserve, given CAP."
  (let ((span (or (plist-get cap :span-min) 0.0))
        (surge (max 0.0 (or (plist-get cap :surge-min) 0.0))))
    (if (or (not org-foresight-gap-net) (<= span 0))
        1.0
      (max 0.0 (min 1.0 (- 1.0 (/ surge span)))))))

(defun org-foresight-agenda--gap (b keep ledger)
  "Return the rows for free band B: what it holds, and what would go in it.

KEEP is the fraction of it that survives the reserve.  The candidates are
LEDGER's unplaced work that would fit in what is left, largest first -- the
biggest thing that will go in is the one worth knowing about, since anything
smaller still fits afterwards.

Each candidate is its own row, carrying its own entry's marker, and shown at
the hour the gap opens.  That is not decoration: it means the answer to
\"where does this go\" is given by putting the cursor on the line and pressing
the key you would have pressed anyway."
  (let* ((start (plist-get b :start))
         (mins (/ (float-time (time-subtract (plist-get b :end) start)) 60.0))
         (usable (* mins keep))
         (at (org-foresight-agenda--hhmm start))
         (fits (seq-sort-by
                #'org-foresight-report--entry-minutes #'>
                (seq-filter
                 (lambda (e)
                   (and (eq (plist-get e :kind) 'promised)
                        (<= (org-foresight-report--entry-minutes e) usable)))
                 ledger))))
    (when (> mins 0)
      (cons
       ;; The length first, as the effort column reads: "0:30 free" is a
       ;; quantity of free time, where "free 0:30" invites the eye to take the
       ;; number for a clock.  What `usable' means is explained once, by the
       ;; key, rather than on every gap.
       (org-foresight-agenda--item
        (concat (org-duration-from-minutes mins) " free"
                (when (< keep 1.0)
                  (propertize (format " · %s usable"
                                      (org-duration-from-minutes usable))
                              'face 'shadow)))
        "" (org-foresight-agenda--span start (plist-get b :end))
        'org-foresight-agenda-derived nil nil)
       (seq-keep
        (lambda (e)
          (org-foresight-agenda--item
           (concat "↳ " (org-foresight-report--grid-todo (plist-get e :marker))
                   (or (plist-get e :title) "?")
                   " "
                   (org-duration-from-minutes
                    (org-foresight-report--entry-minutes e)))
           (or (plist-get e :category) "")
           at 'shadow (plist-get e :marker) nil))
        (if (natnump org-foresight-grid-suggest)
            (seq-take fits org-foresight-grid-suggest)
          fits))))))

(defun org-foresight-agenda--gaps (bands cap ledger)
  "Return the rows for every free stretch among BANDS, given CAP and LEDGER."
  (let ((keep (org-foresight-agenda--keep cap)))
    (mapcan (lambda (b)
              (when (eq (plist-get b :kind) 'available)
                (org-foresight-agenda--gap b keep ledger)))
            bands)))

(defun org-foresight-agenda--edges (cap)
  "Return the rows marking where CAP's working day opens and closes.

Drawn as a rule rather than as a slot, so they read as the edges of something
rather than as two more things in it.  Work sitting above or below them is
work that escaped the day, which is the whole reason to draw them."
  (when-let ((window (plist-get cap :window)))
    (seq-keep
     (pcase-lambda (`(,time . ,label))
       ;; Label first, rule after -- the order Org uses for `← now ────',
       ;; which is the other line of this kind on the page.  Two rules that
       ;; mean the same thing should be read the same way.
       (org-foresight-agenda--item
        (concat label " " (make-string 12 ?─))
        "" (org-foresight-agenda--hhmm time)
        'org-agenda-structure nil nil))
     (list (cons (car window) "work starts")
           (cons (cdr window) "work ends")))))

(defun org-foresight-agenda--mark-wont-fit (list bands cap ledger)
  "Return LIST with LEDGER's unplaceable work marked, given BANDS and CAP.

Whether a piece of work fits is a fact about the work and the day, not about
any arrangement of them: if its estimate is longer than the largest gap there
is, no ordering will find it a home.  So it can be said without placing
anything, which is what keeps the choice of where things go entirely with the
reader.

The mark goes in the first column, which the agenda's prefix leaves blank.
Appending it instead would put it past the tags and defeat
`org-agenda-align-tags', which looks for them at the end of the line."
  (let* ((keep (org-foresight-agenda--keep cap))
         (largest (* keep
                     (apply #'max 0.0
                            (seq-keep
                             (lambda (b)
                               (when (eq (plist-get b :kind) 'available)
                                 (/ (float-time
                                     (time-subtract (plist-get b :end)
                                                    (plist-get b :start)))
                                    60.0)))
                             bands))))
         (stuck (seq-keep
                 (lambda (e)
                   (and (eq (plist-get e :kind) 'promised)
                        (markerp (plist-get e :marker))
                        (> (org-foresight-report--entry-minutes e) largest)
                        (cons (marker-buffer (plist-get e :marker))
                              (marker-position (plist-get e :marker)))))
                 ledger)))
    (if (null stuck)
        list
      (mapcar
       (lambda (item)
         (let ((m (or (get-text-property 0 'org-hd-marker item)
                      (get-text-property 0 'org-marker item))))
           (if (and (markerp m)
                    (member (cons (marker-buffer m) (marker-position m)) stuck)
                    (> (length item) 0)
                    (eq (aref item 0) ?\s))
               (let ((marked (copy-sequence item)))
                 (aset marked 0 (string-to-char org-foresight-agenda-wont-fit))
                 (add-face-text-property
                  0 1 'org-foresight-report-overcommitted t marked)
                 marked)
             item)))
       list))))

(defun org-foresight-agenda--augment (list day &optional scan)
  "Return LIST marked and extended with what foresight knows about DAY.

Order is Org's business: every added row carries a time, and
`org-agenda-finalize-entries' sorts the whole list by it.  Rows sharing a
minute keep the order given here, because Emacs sorts lists stably -- which
is what puts a gap above the candidates hanging off it."
  (let* ((scan (or scan (org-foresight-scan 1 day)))
         (bands (org-foresight-day-blocks day scan))
         (cap (org-foresight-capacity day scan))
         (idx (org-foresight--day-of day (plist-get scan :from)))
         (ledger (and (>= idx 0) (< idx (plist-get scan :days))
                      (aref (plist-get scan :ledger) idx)))
         (all (append (org-foresight-agenda--mark-wont-fit list bands cap ledger)
                      (org-foresight-agenda--edges cap)
                      (org-foresight-agenda--travel bands)
                      (org-foresight-agenda--gaps bands cap ledger))))
    ;; Read back off the finished rows rather than tracked while building
    ;; them: what the key has to explain is what ended up on the page, and
    ;; this cannot drift from it.  One day's worth -- the views that show a
    ;; key show a single day.
    (setq org-foresight-agenda--marks
          (seq-filter (lambda (m)
                        (seq-some (lambda (r) (string-search m r)) all))
                      (mapcar #'car org-foresight-agenda--mark-meanings)))
    all))

;;;; Handing them to Org

(defvar org-foresight-agenda--in-progress nil
  "Non-nil while rows are being made, to stop advice recursing.")

(defun org-foresight-agenda--day ()
  "Return the day the agenda is currently finalising.

`org-agenda-list' assigns the unprefixed `date' -- calendar's own dynamic
variable -- once per day before finalising that day's entries, so a
multi-day span gets its rows day by day without this having to know the
span at all."
  (if (and (boundp 'date) (consp date) (= (length date) 3))
      (org-foresight--midnight
       (encode-time 0 0 0 (nth 1 date) (nth 0 date) (nth 2 date)))
    (org-foresight--day-start 0)))

(defun org-foresight-agenda--inject (args)
  "Add foresight's derived rows to `org-agenda-finalize-entries' ARGS.

`:filter-args' rather than `:filter-return': the rows go in before Org sorts,
so Org places them.  Anything done after the sort would have to reproduce
`org-entries-lessp', and would still be wrong the moment a setting changed."
  (pcase-let ((`(,list ,type) args))
    (if (or org-foresight-agenda--in-progress
            (not org-foresight-agenda-inject)
            (not (eq type 'agenda)))
        args
      (let ((org-foresight-agenda--in-progress t))
        (condition-case err
            (list (org-foresight-agenda--augment
                   list (org-foresight-agenda--day))
                  type)
          (error
           ;; A broken annotation must never cost the day its agenda: the
           ;; entries are the point, everything added here is commentary.
           (message "org-foresight: %s" (error-message-string err))
           args))))))

(advice-add 'org-agenda-finalize-entries :filter-args
            #'org-foresight-agenda--inject)

(provide 'org-foresight-agenda)

;;; org-foresight-agenda.el ends here
