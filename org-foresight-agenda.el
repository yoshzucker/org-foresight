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
  "Rows nobody wrote down: journeys and the edges of the working day.

Italic rather than a colour of its own, because the distinction is not what
kind of time it is -- the category column says that -- but whether it exists
in a file.  Nothing here can be edited where it appears, and the slant is the
warning.")

(defface org-foresight-agenda-free
  '((t :inherit (org-foresight-report-spare italic)))
  "A stretch of the day nothing has claimed.

Derived like the rest, so it keeps the slant, but in the colour of room --
the same blue the bar gives spare time and the gaps it is made of.  It is the
one thing on the page worth growing, and a day is scanned for it.")

(defface org-foresight-agenda-arrival '((t :inherit shadow))
  "The mark for work that arrived rather than was planned.

Grey, like the mark for shared time and for the same reason: it reports where
a row came from, not what to do about it.  The shape carries the difference,
and a reader who has learnt the two coloured marks is not asked to learn a
third colour for a fact that decides nothing on its own.")

(defface org-foresight-agenda-elsewhere
  '((t :inherit org-foresight-report-overcommitted))
  "The mark for work that needs a place today never goes to.

The overrun's colour, because it carries the same kind of news: this is not
going to happen today, and something has to change for it to.  Its own face
all the same -- what has to change is different.  An overrun is answered by
moving work about, and this by going somewhere, or by waiting until you are.")

(defface org-foresight-agenda-shared '((t :inherit shadow))
  "The mark for work that shares its hour without competing for it.

Grey, and deliberately not a colour.  The other marks are coloured because
each carries a decision -- the overrun's colour says this has to move, the
colour of room says here is somewhere it could go -- and a reader who has
learnt two colours can afford to learn no more.  This mark exists to say
there is nothing to decide, so it says it in the one shade that asks nothing
of the reader's memory.  Bright enough to read, which is the whole of the
requirement.")

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
                                       stamp mark edge journey)
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
      (when mark
        (put-text-property 0 (length item) 'org-foresight-mark mark item))
      ;; EDGE says this row opens or closes a stretch of working hours.  A
      ;; property rather than a shape of words: the spine is drawn later, off
      ;; the finished buffer, and matching text there would be matching a
      ;; label somebody may well translate.
      (when edge
        (put-text-property 0 (length item) 'org-foresight-edge edge item))
      ;; JOURNEY is where a derived leg goes, carried so the row can be turned
      ;; into an entry that says the same thing.
      (when journey
        (put-text-property 0 (length item) 'org-foresight-journey journey item))
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

(defconst org-foresight-agenda-arrived "↯"
  "The mark for work that landed on the day rather than was planned into it.

Its own glyph because it answers a question the others do not: not whether
this fits or clashes, but why it is here at all.  On a day that will not
close, which of the work was chosen and which arrived is the first thing
worth knowing -- what arrived is what the reserve was held for, and what
went unplanned is rarely what should be defended.

U+21AF, an arrow struck downwards: it came down on the day.  One cell wide in
PlemolJP, which the mark column depends on.")

(defconst org-foresight-agenda-elsewhere "@"
  "The mark for work that needs a place today never goes to.

Answers a question the day would otherwise leave hanging.  Work that names a
place is not offered in the gaps, because an hour free at home is no use to
something that can only be done at the office -- but a row that is quietly
never suggested looks like a row the tool forgot, and the reader goes looking
for the fault in the wrong place.  The mark says the omission was deliberate
and what decided it.

Nothing is marked on a day that does go there: the work can be done, it is
listed under `Here' on the board, and there is nothing to explain.

An ordinary at-sign, and the only mark here that is not a drawing.  It reads
as \"at\" in every language that has borrowed it, it is one cell wide in every
font that exists, and this column has already been broken twice by a
character some font was missing.")

(defconst org-foresight-agenda-alongside "╰"
  "The mark for work that happens at the same time and does not mind.

A call you only have to hear, or somebody else\'s fixture: both sit in the
day without competing for it, and a clash reported between them and real work
is a clash nobody has to resolve.")

(defconst org-foresight-agenda--spine-column 1
  "The column the working hours are bracketed in.

One, not zero, and for the same reason every block this package draws starts
there: the frame edge belongs to the badges, and an eye running down it
should meet section headings and nothing else.  The listing is content, so it
begins where content begins.

Every ordinary agenda prefix leaves this column blank -- Org's own default
starts with two spaces -- so the first bracket costs a reader nothing.")

(defconst org-foresight-agenda--place-column 3
  "The column the hours away from home are bracketed in.

Two to the right of the first, with a blank between them so the two read as
two.  A prefix therefore needs five leading spaces: one for the margin, two
brackets, the blank between them, and the usual gap before the text.")

(defvar-local org-foresight-agenda--gutter nil
  "Whether this buffer's prefix leaves room for the second bracket.

Measured by formatting a row rather than by reading
`org-agenda-prefix-format', which may be a function, differ per block, and in
any case says nothing about where the text lands.  Measured once for the
buffer rather than line by line, so the answer cannot be yes on a day of hour
lines and no on a day with entries on it -- a bracket that comes and goes
with what the day happened to contain reads as a fault rather than a
setting.")

(defvar-local org-foresight-agenda--places nil
  "Alist of (ABSOLUTE-DAY . SPANS) for the days drawn in this buffer.

Filled as each day is built and read once the buffer is finished, because
those are two different passes over two different things -- the rows Org is
about to sort, and the lines it ended up with.  Keyed by the day rather than
kept as one list, so a view of a week draws each of its days from its own
journeys.")

(defvar org-foresight-agenda--marks nil
  "The marks the day last drawn actually used.

Read back off the finished rows by `org-foresight-agenda--augment' rather
than tracked while building them, so it cannot drift from what is on the
page.  One day\'s worth: the views that show a key show a single day.")

(defconst org-foresight-agenda--mark-meanings
  `((,org-foresight-agenda-elsewhere "needs a place today does not go"
     org-foresight-agenda-elsewhere)
    (,org-foresight-agenda-wont-fit "will not fit, or is booked twice"
     org-foresight-report-overcommitted)
    ("↳" "would fit in the gap above" org-foresight-report-spare)
    (,org-foresight-agenda-arrived "arrived today"
     org-foresight-agenda-arrival)
    (,org-foresight-agenda-alongside "shares its time"
     org-foresight-agenda-shared))
  "Each mark: the glyph, what it means, and the face it is drawn in.

The face lives here rather than at each use, so the key and the rows cannot
disagree about what a mark looks like -- and they are not all one colour,
because they are not all one kind of news: what will not fit takes the
overrun\'s colour, what would fit takes the colour of room, and what merely
shares the hour is quiet.")

(defun org-foresight-agenda--mark (glyph)
  "Return GLYPH in the face its meaning is drawn in."
  (if-let ((entry (assoc glyph org-foresight-agenda--mark-meanings)))
      (propertize glyph 'face (nth 2 entry))
    glyph))

(defun org-foresight-agenda-key (&optional _scan)
  "Return a line explaining the marks this agenda used, or nil for none.

Only the marks actually on the page.  A key describing a clash on a day that
has none is explaining a problem the reader does not have, which is a slower
way of saying nothing."
  (when-let ((used (seq-filter (lambda (m)
                                 (member (car m) org-foresight-agenda--marks))
                               org-foresight-agenda--mark-meanings)))
    (mapconcat (pcase-lambda (`(,glyph ,meaning ,_))
                 (concat (org-foresight-agenda--mark glyph) " " meaning))
               used "   ")))

(add-to-list 'org-foresight-verdict-extras #'org-foresight-agenda-key t)

;;;; The derived rows

(defun org-foresight-agenda--travel (bands)
  "Return the rows for the journeys among BANDS.

A journey somebody wrote down is skipped: it is an entry, Org has already
drawn it, and a second row under a name this file invented would be the same
hour twice.

A journey is the clearest case for this whole file: it takes an hour of the
day, it is nowhere in any file, and until it is drawn the day looks an hour
longer than it is."
  (seq-keep
   (lambda (b)
     (when (and (eq (plist-get b :kind) 'travel)
                (not (plist-get b :written)))
       (let ((trimmed (plist-get b :trimmed)))
         (org-foresight-agenda--item
          (or (plist-get b :title) "→ ?")
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
          ;; No marker.  A derived journey answers to none of Org\'s commands
          ;; because there is no entry behind it, and the marker it used to
          ;; carry belonged to the *meeting it is for*: standing on the drive
          ;; and clocking in, scheduling, or noting reached the meeting
          ;; instead, from a row that looked like any other.
          ;;
          ;; `org-foresight-book-travel\' never wanted it -- it reads the
          ;; journey off `org-foresight-journey\' below, and its own docstring
          ;; says inheriting a neighbour\'s marker would reschedule the wrong
          ;; thing.  Making one real still needs a key of its own, which is
          ;; the point of that command.
          nil
          (plist-get b :stamp)
          (and trimmed org-foresight-agenda-wont-fit)
          nil
          ;; Where this leg goes and when, kept on the row so that
          ;; `org-foresight-book-travel' can write it down without being
          ;; told again what it is looking at.
          (list (plist-get b :place) (plist-get b :start) (plist-get b :end))))))
   bands))

(defun org-foresight-agenda--checks (ledger)
  "Return the rows for the two ends of the day among LEDGER.

Read from the ledger rather than from the day's bands, because a band is
trimmed to keep the day a partition and one lying entirely under something
else is dropped altogether.  That is right for a picture of the day and wrong
for this: a check with nowhere to go is the one a day most needs to be told
about, and it would have gone missing on exactly the days it mattered.  Drawn
at its full length and marked, like a journey that cannot be made.

The title goes through `substitute-command-keys', so a check can name the key
that performs it and go on naming the right one after the key has moved.  It
is the only text in this file that does: everything else here describes the
day, and this one row is an instruction."
  (seq-keep
   (lambda (b)
     (when (eq (plist-get b :kind) 'check)
       (org-foresight-agenda--item
        (substitute-command-keys (or (plist-get b :title) "check"))
        "check"
        (org-foresight-agenda--span (plist-get b :start) (plist-get b :end))
        'org-foresight-agenda-derived nil nil
        (and (plist-get b :wont-fit) org-foresight-agenda-wont-fit))))
   ledger))

(defun org-foresight-agenda--keep (cap)
  "Return the fraction of a gap that survives the reserve, given CAP.

All three reserves, not just the one for arriving work: an hour of gap is
worth less than an hour whether the difference goes to an interruption, to
time at the keyboard nobody clocked, or to being away from the desk.  Spread
evenly rather than placed, because none of them can be put at an hour -- the
honest statement is that every gap is a little thinner than it looks."
  (let ((span (or (plist-get cap :span-min) 0.0))
        (reserve (max 0.0 (or (plist-get cap :reserve-min)
                              (plist-get cap :surge-min)
                              0.0))))
    (if (or (not org-foresight-gap-net) (<= span 0))
        1.0
      (max 0.0 (min 1.0 (- 1.0 (/ reserve span)))))))

(defvaralias 'org-foresight-agenda--now 'org-foresight-now
  "The moment the day is being read at.

An alias since the backward half of the day needed the same seam: a test that
pins when \"now\" is must pin it for the gaps, the bars and the verdict at
once, or the two halves of the day are drawn at different instants and the
join between them stops meaning anything.")

(defun org-foresight-agenda--gap (b keep ledger &optional place now)
  "Return the rows for free band B: what it holds, and what would go in it.

KEEP is the fraction of it that survives the reserve.  The candidates are
LEDGER's unplaced work that would fit in what is left, largest first -- the
biggest thing that will go in is the one worth knowing about, since anything
smaller still fits afterwards.

NOW is what \"what is left\" is measured from.  A stretch that has already gone
holds nothing, whatever it held this morning, and one that is half gone holds
what is still in front of it -- an hour offered at two o\'clock for a gap that
opened at nine is not an offer, it is a reproach.

PLACE is where the body is while B lasts.  Work that named a place of its own
is offered only where that is the place: an hour at home is no use at all to
something that can only be done at the office, and a suggestion that ignores
this is worse than none -- it reads as an answer, gets acted on, and the hour
is spent finding out.  Work that named no place is offered anywhere, which is
most of it.

Each candidate is its own row, carrying its own entry's marker, and shown at
the hour the gap opens.  That is not decoration: it means the answer to
\"where does this go\" is given by putting the cursor on the line and pressing
the key you would have pressed anyway."
  (let* ((start (plist-get b :start))
         (now (or now org-foresight-agenda--now (current-time)))
         (mins (/ (float-time (time-subtract (plist-get b :end) start)) 60.0))
         ;; What is left of it, which is all that may be offered.  The row
         ;; above still reports the stretch as it was: the day's shape is what
         ;; it was, and only the offer is about what is still ahead.
         (left (/ (float-time (time-subtract (plist-get b :end)
                                             (org-foresight--max-time start now)))
                  60.0))
         (usable (* (max 0.0 left) keep))
         (at (org-foresight-agenda--hhmm start))
         (fits (seq-sort-by
                #'org-foresight-report--entry-minutes #'>
                (seq-filter
                 (lambda (e)
                   (and (eq (plist-get e :kind) 'promised)
                        (<= (org-foresight-report--entry-minutes e) usable)
                        ;; Nowhere is work offered in a place no work happens
                        ;; in.  The hour is free and no use, and the day is
                        ;; already leaving it as soon as it can.
                        (not (memq place org-foresight-unworkable-places))
                        (or (null place)
                            (null (plist-get e :place))
                            (eq (plist-get e :place) place))))
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
        'org-foresight-agenda-free nil nil)
       (seq-keep
        (lambda (e)
          (org-foresight-agenda--item
           (concat (org-foresight-report--grid-todo (plist-get e :marker))
                   (or (plist-get e :title) "?")
                   " "
                   (org-foresight-report--effort-run e))
           (or (plist-get e :category) "")
           at 'shadow (plist-get e :marker) nil "↳"))
        (if (natnump org-foresight-grid-suggest)
            (seq-take fits org-foresight-grid-suggest)
          fits))))))

(defun org-foresight-agenda--gaps (bands cap ledger &optional day)
  "Return the rows for every free stretch among BANDS, given CAP and LEDGER.

DAY, where given, is what lets each gap know where the body is while it lasts,
so work that needs a place is offered only in the hours spent there.  Omitted,
every gap is offered everything that fits."
  (let ((keep (org-foresight-agenda--keep cap))
        (now (or org-foresight-agenda--now (current-time))))
    (mapcan (lambda (b)
              (when (eq (plist-get b :kind) 'available)
                (org-foresight-agenda--gap
                 b keep ledger
                 (and day (org-foresight-place-at
                           day (plist-get b :start) bands))
                 now)))
            bands)))

(defcustom org-foresight-agenda-lands-minutes 5
  "How far the projected end must sit from the declared one to be drawn.

A projection that agrees with the declaration is not news, and a rule saying
so is a rule the eye learns to skip -- which costs the days it does have
something to say."
  :type 'integer
  :group 'org-foresight)

(defun org-foresight-agenda--edges (cap day)
  "Return the rows marking where CAP's working day opens, closes, and lands.

Drawn as rules rather than as slots, so they read as the edges of something
rather than as more things in it.  Work sitting above or below them is work
that escaped the day, which is the whole reason to draw them.

The starts and ends are declarations -- the hours being defended -- and the
landing is what the day is actually going to do with them.  Same subject,
same shape, one verb apart, because the difference between intending to stop
at six and stopping at six is the whole subject of this package.

A day that breaks gets a rule at every edge, and the inner ones say `pauses\='
and `resumes\=' rather than `ends\=' and `starts\=': the hour between them was
declared not to be work, and a break announced as the end of the day would be
read as one.

It takes the overrun's colour past the declared end and the colour of room
before it: landing early is not a lesser kind of news.  Nothing is drawn when
the work will not fit in the waking day either -- there is no hour to put a
rule at, and the verdict has already said so in words.

Drawn from where the work lands rather than from where it fits, which are
different questions on exactly the days worth asking: the second stops at the
edge of the working day by construction and so could never draw a rule past
it, which is the one place a rule was worth drawing."
  (when-let ((work (plist-get cap :work)))
    (let* ((ends (cdr (car (last work))))
           ;; Nothing owed has nothing to land: a rule at the hour you are
           ;; already free would be drawn on every empty day, which is the
           ;; surest way to stop it being read on a full one.
           (lands (and (> (or (plist-get cap :committed-min) 0.0) 0)
                       (plist-get cap :lands)))
           (over (or (plist-get cap :overflow-min) 0.0))
           (awake (plist-get (org-foresight-day-shape day) :awake))
           (drift (and lands
                       (/ (float-time (time-subtract lands ends)) 60.0))))
      (append
       (org-foresight-agenda--pinned-now day)
       (seq-keep
       (pcase-lambda (`(,time ,label ,face ,edge))
         ;; Label first, rule after -- the order Org uses for `← now ────',
         ;; which is the other line of this kind on the page.  Two rules that
         ;; mean the same thing should be read the same way.
         (org-foresight-agenda--item
          (concat label " " (make-string 12 ?─))
          "" (org-foresight-agenda--hhmm time)
          face nil nil nil edge))
       (append
        ;; One pair per interval: the first opens the day and the last closes
        ;; it, and every edge in between is a break beginning or ending.
        (let ((n (length work)) (i 0) out)
          (dolist (iv work (nreverse out))
            (push (list (car iv)
                        (if (zerop i) "work starts" "work resumes")
                        'org-agenda-structure 'open)
                  out)
            (push (list (cdr iv)
                        (if (= i (1- n)) "work ends" "work pauses")
                        'org-agenda-structure 'close)
                  out)
            (setq i (1+ i))))
        (cond
         ;; It ends somewhere today, and somewhere worth drawing a rule at.
         ((and drift (>= (abs drift) org-foresight-agenda-lands-minutes))
          (list (list lands "work lands"
                      (if (> drift 0)
                          'org-foresight-report-overcommitted
                        'org-foresight-report-spare))))
         ;; It does not end today at all.  The rule goes at the last hour
         ;; there is and says how much is still standing then -- the day on
         ;; which the projection matters most is exactly the day it used to
         ;; go silent, and an hour that cannot be drawn is no reason to draw
         ;; nothing.
         ;;
         ;; A quantity, not an hour: what is named is the work with nowhere
         ;; left to go, counting the hours after work in.  In the mark\='s own words,
         ;; because it is the mark\='s own meaning applied to the whole day
         ;; rather than to one entry -- and because "past today" read as a
         ;; time, which is the one thing it is not.
         ((and (> over 0) awake)
          (list (list (cdr awake)
                      (format "work lands · %s will not fit"
                              (org-duration-from-minutes over))
                      'org-foresight-report-overcommitted))))))))))

(defun org-foresight-agenda--pinned-now (day)
  "Return the `now\=' rule for DAY when the day is being read at a pinned hour.

Org draws its own, from the wall clock.  That is right whenever the two agree
and a contradiction whenever they do not: with `org-foresight-now\=' set, every
figure on the page -- what is behind, what is ahead, which gaps are still
usable -- is worked out from the pinned hour, while Org\='s rule goes on
pointing at the real one.  A page with two different present moments on it is
worse than a page with none.

So the rule is drawn here instead, in Org\='s own words, and whoever pins the
hour is expected to turn `org-agenda-show-current-time-in-grid\=' off --
`org-foresight-demo-mode\=' does.  Nil when nothing is pinned, which is the
ordinary case and leaves Org to it."
  (when (and org-foresight-now
             ;; On the day the pinned moment belongs to, which in a week view
             ;; is one day of seven.
             (= (time-to-days org-foresight-now) (time-to-days day)))
    (list (org-foresight-agenda--item
           org-agenda-current-time-string ""
           (org-foresight-agenda--hhmm org-foresight-now)
           'org-foresight-report-now))))

(defun org-foresight-agenda--clashes (ledger)
  "Return a hash of the LEDGER entries whose hour is claimed twice.

Two things booked over each other is a day that cannot happen, and Org draws
them one after the other as though it could.  The bands know -- the later of
the pair is trimmed there to keep the day a partition -- but that is a fact
about the drawing, and the trimming is exactly what makes the collision
invisible in the rows.

Both parties are marked, because it takes two to make one.  Which of them
ought to move is not a question this can answer, and a mark on the later one
alone would be answering it.

Only what actually competes: something you merely have to hear, or somebody
else\='s fixture, shares its hour by definition -- see
`org-foresight-attention-property\='.  Derived rows are left out too; a journey
with nowhere to go is already reported as one."
  (let ((timed (seq-filter
                (lambda (e)
                  (and (plist-get e :start) (plist-get e :end)
                       (markerp (plist-get e :marker))
                       (memq (plist-get e :kind) '(meeting task))
                       (not (memq (plist-get e :attention)
                                  '(background informational)))))
                ledger))
        (out (make-hash-table :test 'equal)))
    (dolist (a timed)
      (dolist (b timed)
        (unless (eq a b)
          (when (and (time-less-p (plist-get a :start) (plist-get b :end))
                     (time-less-p (plist-get b :start) (plist-get a :end)))
            (let ((m (plist-get a :marker)))
              (puthash (cons (marker-buffer m) (marker-position m)) t out))))))
    out))

(defun org-foresight-agenda--mark-rows (list bands cap ledger &optional day)
  "Return LIST with LEDGER's entries marked, given BANDS and CAP.

Two marks, both facts about an entry that no arrangement of the day changes.

Whether the day ever goes where it has to be done.  DAY, where given, is what
lets this be asked; without it the question goes unasked and no row is marked
for it.  It comes first because it is the one thing rearranging the day cannot
help with -- and because work that needs somewhere else is quietly left out of
every suggestion, which without a mark looks like the tool having missed it.

Whether a piece of work fits: if its estimate is longer than the largest gap
there is, no ordering will find it a home.  Saying so without placing
anything is what keeps the choice of where things go with the reader.

And whether it competes for its hour at all.  A call you only have to hear,
or somebody else's fixture, sits in the day beside real work rather than
against it -- so an overlap there is not a clash, and marking it stops the
reader resolving one that was never there.

And whether it was chosen at all.  Work that arrived is why the day is
fuller than it was planned to be, and on a day that will not close, telling
the two apart is the first thing worth knowing.

The mark is recorded on the row rather than drawn into it, because where the
marks go is a decision about the page as a whole and is taken once, by
`org-foresight-agenda--place-marks', for every row at the same time."
  (let* ((keep (org-foresight-agenda--keep cap))
         ;; On a day with no working hours every gap is nothing, so every
         ;; piece of work would be marked -- and a mark on every row is a
         ;; mark that says nothing.  The fact is about the day, not about any
         ;; one task, and the verdict states it there instead.
         ;;
         ;; A reserve grown past the span arrives at the same place by a
         ;; different road: `keep\=' reaches zero, no gap survives it, and every
         ;; row is marked.  That reads as a day in which nothing can be done,
         ;; when what is true is that the reserve has stopped describing the
         ;; day -- and the mark would point at the tasks rather than at the
         ;; figure that needs looking at.
         (fits (and (plist-get cap :work) (> keep 0.0)))
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
         (places (and day (org-foresight-day-places day bands)))
         (clashes (org-foresight-agenda--clashes ledger))
         (marks (make-hash-table :test 'equal)))
    (dolist (e ledger)
      (when-let* ((m (plist-get e :marker))
                  ((markerp m))
                  (key (cons (marker-buffer m) (marker-position m)))
                  (glyph
                   (cond
                    ((and places (plist-get e :place)
                          (not (memq (plist-get e :place) places)))
                     org-foresight-agenda-elsewhere)
                    ;; The same glyph and the same news: the day cannot hold
                    ;; this.  An hour claimed twice is one hour short exactly
                    ;; as an estimate with no gap to take it is.
                    ((gethash key clashes) org-foresight-agenda-wont-fit)
                    ((and fits
                          (eq (plist-get e :kind) 'promised)
                          (> (org-foresight-report--entry-minutes e) largest))
                     org-foresight-agenda-wont-fit)
                    ((plist-get e :arrived) org-foresight-agenda-arrived)
                    ((memq (plist-get e :attention) '(background informational))
                     org-foresight-agenda-alongside))))
        ;; The two kinds of "not today" are the loudest, and neither is
        ;; overwritten by an entry that also arrived or happens to share its
        ;; hour.  One column holds one glyph, and what cannot happen at all is
        ;; worth more than where it came from.
        (unless (member (gethash key marks)
                        (list org-foresight-agenda-elsewhere
                              org-foresight-agenda-wont-fit))
          (puthash key glyph marks))))
    (if (zerop (hash-table-count marks))
        list
      (mapcar
       (lambda (item)
         (let* ((m (or (get-text-property 0 'org-hd-marker item)
                       (get-text-property 0 'org-marker item)))
                (glyph (and (markerp m)
                            (gethash (cons (marker-buffer m)
                                           (marker-position m))
                                     marks))))
           (if glyph
               (let ((marked (copy-sequence item)))
                 (put-text-property 0 (length marked)
                                    'org-foresight-mark glyph marked)
                 marked)
             item)))
       list))))

(defun org-foresight-agenda--mark-column (list)
  "Return the column the marks on LIST line up in, or nil if there is none.

One column for every mark, so that a reader who has learnt where to look has
learnt it for the whole page.  Which column is not a constant: it is read off
the rows themselves, as the column the earliest heading begins at -- and that
is where the time field ends, so the mark sits between the clock and the
title it qualifies, to the left of `Scheduled:' and of the effort, rather
than wandering right by however long a leader that row happened to need.

`org-heading' is Org's own answer to where the prefix stops: it is laid over
the heading text and nothing else, so the column it starts at is the one
thing on a row that does not have to be counted out of a format string --
which means a reader who rearranges `org-agenda-prefix-format' still gets a
straight column.

Only rows that carry a time are asked.  A conditional time field like
`%?-12t' is dropped entirely from a row that has no time rather than padded,
so an undated task's heading begins where a timed row is still in the middle
of its clock; taking the earliest of all of them would put the whole page's
marks inside the hour.  The timed rows are the grid, and the grid is what a
day is scanned down."
  (when-let ((heads (seq-keep
                     (lambda (item)
                       (and (get-text-property 0 'time-of-day item)
                            (org-foresight-agenda--heading-column item)))
                     list)))
    (apply #'min heads)))

(defun org-foresight-agenda--heading-column (item)
  "Return the display column ITEM's heading text begins at, or nil for none.

Columns, not characters, because that is the arithmetic Org itself does: a
prefix field is padded to a width on the screen, so a category written in a
script whose characters are two columns wide fills the same field with half
as many of them.  `CATEGORY: 会議予定' fills eight columns with four
characters and needs no padding at all, which puts its heading four
characters -- and no columns -- ahead of every other row.

Counting characters instead is not a rounding error: `\\='min\\=' over the
rows then answers with that row, and every mark on the page is inserted four
places to the left of where the headings actually are, inside the dots of the
time field.  On a page of one language it never happens; on a mixed page it
happens to the whole page at once, on some days and not others, which is
exactly the shape of a bug nobody can reproduce."
  (when-let ((at (if (get-text-property 0 'org-heading item)
                     0
                   (next-single-property-change 0 'org-heading item))))
    (string-width (substring item 0 at))))

(defun org-foresight-agenda--column-index (item column)
  "Return the position in ITEM at display COLUMN.

The inverse of `org-foresight-agenda--heading-column', and needed for the
same reason: what lines up on the screen is a column, and what a string is
cut at is a position.  Where a two-column character straddles COLUMN the
position before it is returned -- half a character is not a place anything
can be inserted, and stopping short leaves the mark in the padding rather
than through the middle of a word."
  (let ((i 0) (w 0) (n (length item)) (out nil))
    (while (and (null out) (< i n))
      (let ((next (+ w (char-width (aref item i)))))
        (if (> next column)
            (setq out i)
          (setq w next i (1+ i)))))
    (or out n)))

(defun org-foresight-agenda--mark-index (item column own)
  "Return where in ITEM a mark aimed at display COLUMN should be inserted.

COLUMN is the page's shared column; OWN is where this row's own heading
begins.  The shared one is taken where the row has whitespace in front of it,
and the row's own where it has not.

Because the shared column is only known to be padding on the rows it was
measured from.  A conditional prefix field is dropped from a row that has
nothing to put in it rather than padded, so the column that is the tail of the
clock on one row is the middle of `Scheduled:' on the next, and a mark
inserted there comes out inside the word.

Falling back rather than widening every row: what has to line up is the marks,
and a row whose prefix is a different shape was never in that column to begin
with."
  (let ((at (org-foresight-agenda--column-index item column)))
    (if (or (zerop at) (eq (aref item (1- at)) ?\s))
        at
      (org-foresight-agenda--column-index item own))))

(defun org-foresight-agenda--place-marks (list)
  "Return LIST with each row's recorded mark drawn into it.

The mark and the space after it are inserted, not written over what is there.
The only blank going spare at that column is the single space the time field
pads out with, and spending it leaves the mark jammed against the clock on
one side or the title on the other.

Only a marked row is touched.  Widening every row to keep their titles in one
column would buy an alignment the agenda has never had -- a leader, an
effort, an undated task all move a title already -- and would pay for it with
two dead columns on every row of every day that has a single mark.  What has
to line up is the marks, and that is what the shared column is for.

Appending the mark to the end of the row is not an option; it would land past
the tags and defeat `org-agenda-align-tags', which looks for them there.  The
face is applied without appending, because appended the row's own face would
win the conflict and the mark would come out the colour of its row.  The
inserted cell inherits the prefix's properties rather than the heading's,
which keeps `org-heading' over the heading text and nothing else.

The shared column is a column on the screen; where to cut this particular row
to reach it is a question with a different answer on every row, and
`org-foresight-agenda--column-index' is what asks it."
  (let ((col (org-foresight-agenda--mark-column list)))
    (mapcar
     (lambda (item)
       (if-let* ((glyph (get-text-property 0 'org-foresight-mark item))
                 (own (org-foresight-agenda--heading-column item))
                 (at (org-foresight-agenda--mark-index
                      item (if col (min col own) own) own))
                 (face (nth 2 (assoc glyph
                                     org-foresight-agenda--mark-meanings)))
                 (cell (concat glyph " ")))
           (progn
             (set-text-properties 0 2 (text-properties-at (max 0 (1- at)) item)
                                  cell)
             (add-face-text-property 0 1 face nil cell)
             (concat (substring item 0 at) cell (substring item at)))
         item))
     list)))

(defun org-foresight-agenda--annotate-efforts (list ledger)
  "Return LIST with LEDGER\='s corrections written beside the efforts Org printed.

Org prints the estimate that is in the file, and the day is planned on a
corrected one.  Where those differ the row said the first and meant the
second, which is the one thing a number on a screen must never do.  Written
as \"6:00→10:00\" in the face the derived rows use, because the second figure
is not in any file and nothing about it can be edited where it appears.

The effort is found by searching back from the heading rather than by
counting columns: which field of the prefix it is depends on
`org-agenda-prefix-format\=', but that it is the last thing before the heading
that reads as a duration does not.  A row whose estimate cannot be found is
left exactly as it was; a wrong insertion is worse than a missing one."
  (let ((drifts (make-hash-table :test 'equal)))
    (dolist (e ledger)
      (when-let* ((m (plist-get e :marker))
                  ((markerp m))
                  (drift (org-foresight-report--effort-drift e)))
        (puthash (cons (marker-buffer m) (marker-position m)) drift drifts)))
    (if (zerop (hash-table-count drifts))
        list
      (mapcar
       (lambda (item)
         (let* ((m (or (get-text-property 0 'org-hd-marker item)
                       (get-text-property 0 'org-marker item)))
                (drift (and (markerp m)
                            (gethash (cons (marker-buffer m)
                                           (marker-position m))
                                     drifts)))
                (raw (and drift (get-text-property 0 'effort item)))
                (head (and raw (org-foresight-agenda--heading-column item)))
                (at (and head (string-search
                               raw (substring-no-properties item 0 head)))))
           (if (not at)
               item
             (let ((tail (+ at (length raw)))
                   (text (copy-sequence drift)))
               (set-text-properties 0 (length text)
                                    (text-properties-at at item) text)
               (add-face-text-property 0 (length text)
                                       'org-foresight-agenda-derived nil text)
               (concat (substring item 0 tail) text (substring item tail))))))
       list))))

(defun org-foresight-agenda--scan-for (day)
  "Return a survey covering DAY, shared with the rest of the redraw if it can be.

The redraw\='s own survey reaches a fortnight out, which is every day an
agenda span normally shows, so each day of the span is drawn from the one
walk.  A day beyond it -- the agenda jumped to next month -- gets a survey
of its own rather than being drawn from buckets that do not exist."
  (let ((shared (org-foresight-redraw-scan)))
    (if (org-foresight-scan-covers-p shared day)
        shared
      (org-foresight-scan 1 day))))

(defun org-foresight-agenda--augment (list day &optional scan)
  "Return LIST marked and extended with what foresight knows about DAY.

Order is Org's business: every added row carries a time, and
`org-agenda-finalize-entries' sorts the whole list by it.  Rows sharing a
minute keep the order given here, because Emacs sorts lists stably -- which
is what puts a gap above the candidates hanging off it."
  (let* ((scan (or scan (org-foresight-agenda--scan-for day)))
         (bands (org-foresight-day-blocks day scan))
         (cap (org-foresight-capacity day scan))
         (ledger (org-foresight-scan-day scan :ledger day))
         ;; Marks first, then the corrections.  The mark column is read off
         ;; where the headings start, and an estimate annotated before that is
         ;; measured would push its own heading right and take the column with
         ;; it; annotated after, the insertion lands to the right of the mark
         ;; and leaves it where every other row has one.
         (all (org-foresight-agenda--annotate-efforts
               (org-foresight-agenda--place-marks
                (append (org-foresight-agenda--mark-rows list bands cap ledger day)
                        (org-foresight-agenda--edges cap day)
                        (org-foresight-agenda--travel bands)
                        (org-foresight-agenda--checks ledger)
                        (org-foresight-agenda--gaps bands cap ledger day)))
               ledger)))
    ;; Where the body is, kept for the spine to draw when the buffer is
    ;; finished.  Handed over rather than recomputed there: this is the one
    ;; place the day's bands are already in hand, and asking again would mean
    ;; a fresh scan of every file for every day on the page.
    (setf (alist-get (time-to-days day) org-foresight-agenda--places nil nil #'=)
          (org-foresight-day-place-spans day bands))
    ;; And whether the page has a column to draw it in -- its own, and a blank
    ;; after it.  Asked of the formatter while it is still compiled for this
    ;; block rather than of the finished lines, where a day of nothing but
    ;; hour lines would answer yes.
    (setq org-foresight-agenda--gutter
          (let ((probe (org-foresight-agenda--item "x" "c" "00:00")))
            (and probe (> (or (string-match "[^ ]" probe) 0)
                          (1+ org-foresight-agenda--place-column)))))
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
  "Return the day the agenda is currently drawing.

`org-agenda-current-date' is Org's own answer to that question: it is set
once per file per day inside `org-agenda-get-day-entries', which runs
immediately before the day's rows are assembled.  So a multi-day span gets
its rows day by day without this having to know the span at all.

Deliberately not the `date' the loop itself uses.  org-agenda.el is
lexically bound and never declares `date' special, so from outside that
function the name does not exist -- and reading it, as this once did, meant
falling through to `today' on every single day.  On a one-day agenda that
answer is right by accident, which is exactly why it survived: the second day
of a two-day view was drawn with the first day's gaps, its own entries
ignored.  It is still tried first, for an Org old enough to have made `date'
special, and costs nothing when it is not."
  (let ((d (or (and (boundp 'date) (consp date) date)
               org-agenda-current-date)))
    (if (and (consp d) (= (length d) 3))
        (org-foresight--midnight
         (encode-time 0 0 0 (nth 1 d) (nth 0 d) (nth 2 d)))
      (org-foresight--day-start 0))))

(defun org-foresight-agenda--inject (list)
  "Add foresight's derived rows to one day's LIST of agenda entries.

Hooked onto `org-agenda-add-time-grid-maybe', which looks like the wrong
place until you read the day loop in `org-agenda-list':

  (setq rtnall (org-agenda-add-time-grid-maybe rtnall ndays todayp))
  (when rtnall (insert (org-agenda-finalize-entries rtnall \\='agenda) ...))

Finalize runs only `when rtnall'.  A day Org found nothing for is therefore
never finalized -- so hooking finalize, as this once did, meant the day whose
shape is most worth drawing was the one day nothing was drawn on.  An empty
working day should say when work starts and how much of it is free rather
than going blank, and in clockcheck mode -- where the list holds clock lines
and nothing else -- that was every day.

The grid function runs immediately before that test, once per day, with
`date' bound.  Adding here puts the rows in the list Org sorts, so Org still
places them, and leaves a day that was empty with entries to finalize.  The
grid's own `require-timed' decision is taken before this returns, so nothing
here changes when hour lines are drawn.

It also inherits the right silence: the whole block sits inside
`(when (or rtnall org-agenda-show-all-dates) ...)', so somebody who has asked
not to see empty days does not get them back full of derived rows."
  (if (or org-foresight-agenda--in-progress
          (not org-foresight-agenda-inject))
      list
    (let ((org-foresight-agenda--in-progress t))
      (condition-case err
          (org-foresight-agenda--augment list (org-foresight-agenda--day))
        (error
         ;; A broken annotation must never cost the day its agenda: the
         ;; entries are the point, everything added here is commentary.
         (message "org-foresight: %s" (error-message-string err))
         list)))))

(advice-add 'org-agenda-add-time-grid-maybe :filter-return
            #'org-foresight-agenda--inject)

;;;; The spine
;; Which hours are work, read at a glance rather than by following the rules
;; down the page.  A day that breaks has its working hours in two or three
;; pieces now, and "am I inside them" stopped being answerable from where a
;; row sits.
;;
;; Two brackets, side by side, answering the two questions a day of mixed
;; places raises: am I working, and am I there.  The second is drawn from the
;; journeys, so the hours in transit fall outside both ends of it -- being on
;; the way to the office is not being at the office, and a bracket that
;; included the journey would offer those hours for work that needs the place.

(defcustom org-foresight-agenda-spine t
  "When non-nil, bracket the working hours down the agenda's left edge."
  :type 'boolean
  :group 'org-foresight)

(defcustom org-foresight-agenda-place-spine t
  "When non-nil, bracket the hours spent away from home beside the first.

Drawn two columns to the right of the working hours, so the agenda's prefix
needs five leading spaces for it to have anywhere to go:

    (setq org-agenda-prefix-format \\='((agenda . \"     %-8.8c%?-12t% s%?-5e\")))

Where there is no room nothing is drawn, so a narrower prefix loses the
second bracket and keeps everything else."
  :type 'boolean
  :group 'org-foresight)

(defconst org-foresight-agenda-spine-glyphs
  '((open . ?┌) (mid . ?│) (close . ?└) (only . ?├))
  "The characters the spine is drawn with.

Box drawing rather than a block or a bar: the corners say where a stretch
begins and ends, so two stretches read as two brackets instead of as one
interrupted line.  All of them are one cell wide in the fonts this is meant
for, which the mark glyphs are chosen for too.

`only' is a stretch that begins and ends on the same row -- an hour somewhere
else, with nothing above or below it there.  A bracket cannot have both
corners in one cell, and `open' alone would say the stretch carries on down
the page, which is the one thing it does not do.")

(defface org-foresight-agenda-spine '((t :inherit shadow))
  "The bracket down the left edge marking the working hours.

Quiet on purpose.  It is a frame around the day, not a thing in it: the eye
should be able to find it when it looks and ignore it when it does not.")

(defun org-foresight-agenda--spine-glyph (kind)
  "Return the spine character for KIND, one of `open', `mid' or `close'."
  (cdr (assq kind org-foresight-agenda-spine-glyphs)))

(defun org-foresight-agenda--draw-spine ()
  "Bracket each stretch of working hours down the agenda's left edge.

Drawn on the finished buffer rather than on the rows, because the shape
depends on the order Org put them in, and that is not known until it has.
The state is taken from the `org-foresight-edge' property the rules carry --
not from their text, which is a label and may be reworded.

The glyph is *shown* in its column rather than written there: the space keeps
its place in the buffer and only its appearance changes.  Nothing about
the text moves, which matters twice over.  Every column in this package -- the
mark column above all -- is counted from the line\='s start, so an insertion
would leave the marks a column adrift.  And Org stamps the whole buffer with
the properties `org-agenda-redo\=' reads from under the cursor *before* this
hook runs; text put in afterwards has none of them, and a cursor resting on
such a character makes `r\=', `g\=' and every mode toggle quietly do nothing.
Where the column is not a blank, the line is left alone: something else is
using it, and this is only decoration.

The second bracket is drawn in the same pass, from the spans handed over by
`org-foresight-agenda--augment' and the time each line carries, rather than
from a property: which place a row belongs to is a question about when it is,
and the rows that most need answering -- the grid's own hour lines -- are
Org\='s and carry nothing of ours."
  (when (and org-foresight-agenda-spine
             org-foresight-agenda-inject
             (derived-mode-p 'org-agenda-mode))
    (let ((inhibit-read-only t)
          (open nil)                    ; row that began the current stretch
          (last nil)                    ; last row seen inside it
          (spans nil)                   ; the day's time away from home
          (span nil)                    ; the one the last row was inside
          (pfrom nil) (plast nil))      ; and the rows it has covered
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((edge (get-text-property (point) 'org-foresight-edge))
                 (ours (get-text-property (point) 'org-foresight-report))
                 (header (get-text-property (point) 'org-agenda-date-header))
                 (day (and header (get-text-property (point) 'day)))
                 (at (and (not ours)
                          (get-text-property (point) 'time-of-day)))
                 (now (and at spans (org-foresight-agenda--span-at at spans))))
            (cond
             ;; The report's own blocks are not part of the day's grid, and a
             ;; date line belongs to no stretch: either one ends whatever was
             ;; open, so a day that forgot to close cannot bleed into the next.
             ((or ours header) (setq open nil last nil))
             ((eq edge 'open) (setq open (point) last (point)))
             ((eq edge 'close)
              (when open
                (org-foresight-agenda--spine-region open (point))
                (setq open nil last nil)))
             (open (setq last (point))))
            ;; The place bracket runs while consecutive rows answer to the
            ;; same span, so a journey -- which belongs to none -- closes the
            ;; one before it and the arrival opens the next.
            (unless (eq now span)
              (when (and span pfrom)
                (org-foresight-agenda--spine-region
                 pfrom plast org-foresight-agenda--place-column))
              (setq span now pfrom (and now (point))))
            (when now (setq plast (point)))
            ;; A new day brings its own spans.  Read here rather than tracked
            ;; from the top, so a view of several days draws each from what
            ;; that day's journeys were, not from the first day's.
            (when header
              (setq spans (and day (alist-get day org-foresight-agenda--places
                                              nil nil #'=)))))
          (forward-line 1))
        ;; A stretch left open -- the day ended mid-bracket, which happens when
        ;; a filter hid the closing rule -- is still drawn, down to the last
        ;; row it reached.
        (when (and open last) (org-foresight-agenda--spine-region open last))
        (when (and span pfrom)
          (org-foresight-agenda--spine-region
           pfrom plast org-foresight-agenda--place-column))))))

(add-hook 'org-agenda-finalize-hook #'org-foresight-agenda--draw-spine)

(defun org-foresight-agenda--span-at (at spans)
  "Return the span among SPANS holding AT, the HHMM a row carries, or nil.

Closed at the start and open at the end, so a row at the minute a journey
leaves belongs to the road rather than to the place being left.  That is the
minute the two would otherwise both claim, and the road has the better
argument: by then you are on it."
  (when (and org-foresight-agenda-place-spine org-foresight-agenda--gutter)
    (seq-find (lambda (s)
                (let ((from (org-foresight-agenda--minutes-of (car s)))
                      (to (org-foresight-agenda--minutes-of (cadr s))))
                  (and (<= from at) (< at to))))
              spans)))

(defun org-foresight-agenda--minutes-of (time)
  "Return TIME as the HHMM integer Org files its rows under."
  (string-to-number (format-time-string "%H%M" time)))

(defun org-foresight-agenda--spine-room-p (open close column)
  "Return non-nil when every line from OPEN to CLOSE is blank at COLUMN."
  (save-excursion
    (goto-char open)
    (let ((room t) (done nil))
      (while (and room (not done))
        (setq done (>= (point) close))
        (let ((cell (+ (point) column)))
          (unless (and (< cell (line-end-position))
                       (eq (char-after cell) ?\s))
            (setq room nil)))
        (forward-line 1))
      room)))

(defun org-foresight-agenda--spine-region (open close &optional column)
  "Draw the bracket from the line at OPEN down to the line at CLOSE.

COLUMN defaults to `org-foresight-agenda--spine-column', which every agenda
prefix leaves blank; a line using it anyway is skipped, since this is only
decoration.

Any other column belongs to a page laid out for it, and there the bracket is
drawn only where the whole of it fits.  All or nothing: a prefix with no room
should lose the second bracket outright rather than show it on the few rows
whose text happens to start late, which reads as a bracket with holes in it
rather than as a setting that has not been turned on."
  (save-excursion
    (let ((column (or column org-foresight-agenda--spine-column)))
      (when (or (= column org-foresight-agenda--spine-column)
                (org-foresight-agenda--spine-room-p open close column))
        (goto-char open)
        (let ((done nil))
          (while (not done)
            (setq done (>= (point) close))
            (let ((cell (+ (point) column)))
              (when (and (< cell (line-end-position))
                         (eq (char-after cell) ?\s))
                (let ((glyph (org-foresight-agenda--spine-glyph
                              (cond ((and (= (point) open) done) 'only)
                                    ((= (point) open) 'open)
                                    (done 'close)
                                    (t 'mid)))))
                  (add-text-properties
                   cell (1+ cell)
                   (list 'display (propertize (string glyph)
                                              'face 'org-foresight-agenda-spine)
                         'org-foresight-spine t)))))
            (forward-line 1)))))))

(defun org-foresight-agenda--prefix-blanks ()
  "Return how many blank columns the agenda's prefix begins with, or nil.

Nil where the answer cannot be had by looking: a prefix may be a function,
and one that is has no leading blanks to count."
  (let ((prefix (if (stringp org-agenda-prefix-format)
                    org-agenda-prefix-format
                  (cdr (assq 'agenda org-agenda-prefix-format)))))
    (when (stringp prefix)
      (or (string-match "[^ ]" prefix) (length prefix)))))

(defun org-foresight-agenda--diagnose-gutter ()
  "Say when the page has no room for the second bracket, or nothing.

The one thing about this layer that fails silently.  Everything else either
appears or errors; a place spine with nowhere to go is simply not drawn --
deliberately, since a partial one reads as a fault -- and there is otherwise
no way to tell that apart from having nothing to draw."
  (when org-foresight-agenda-place-spine
    (let ((blanks (org-foresight-agenda--prefix-blanks))
          (needed (+ 2 org-foresight-agenda--place-column)))
      (when (and blanks (< blanks needed))
        (format (concat "`org-agenda-prefix-format' begins with %d space%s; "
                        "the hours away from home need %d to be bracketed")
                blanks (if (= blanks 1) "" "s") needed)))))

(add-to-list 'org-foresight-diagnose-extras
             #'org-foresight-agenda--diagnose-gutter t)

(defconst org-foresight-agenda-attentions
  '(("blocking" . "needs all of you")
    ("background" . "costs the hour but shares it")
    ("informational" . "takes none of your time")
    ("" . "whatever its category says"))
  "The values `org-foresight-attention-property\' takes, and what each means.
The empty string deletes the property, so the entry goes back to being
whatever its category makes it.")

;;;###autoload
(defun org-foresight-set-attention (&optional attention)
  "Set how much of the hour the entry at point demands.

Occupying time and demanding all of it are different things, and a day that
treats them alike reports clashes nobody has to resolve: a call you only have
to hear can be heard on the way to somewhere else, and a child\'s fixture is
a fact about the household rather than an hour of yours.

Works from an agenda line as well as from the entry itself, and refreshes the
view, so the day's arithmetic changes under the cursor rather than at the
next redraw.  ATTENTION is one of `org-foresight-agenda-attentions\'; the
empty string removes the property and lets the category decide again."
  (interactive)
  (let ((attention
         (or attention
             (completing-read
              "Attention: "
              (mapcar (lambda (kv)
                        (format "%-14s %s" (car kv) (cdr kv)))
                      org-foresight-agenda-attentions)
              nil t)))
        (marker (or (org-get-at-bol 'org-hd-marker)
                    (org-get-at-bol 'org-marker)
                    (and (derived-mode-p 'org-mode) (point-marker)))))
    (setq attention (car (split-string attention)))
    (unless marker (user-error "No entry here"))
    (org-with-point-at marker
      (org-back-to-heading t)
      (if (or (null attention) (string-empty-p attention))
          (org-entry-delete (point) org-foresight-attention-property)
        (org-entry-put (point) org-foresight-attention-property attention)))
    (org-foresight-report-refresh)
    (when (derived-mode-p 'org-agenda-mode) (org-agenda-redo))
    (message "Attention: %s"
             (if (string-empty-p (or attention ""))
                 "by category" attention))))

;;;###autoload
(defun org-foresight-mark-surge ()
  "Mark the entry at point as work that arrived, now.

For the interruption you did not capture as one -- you were already in the
file when it landed, or you only realised afterwards that it had.  The
property's value is the moment it arrived, which is what decides the day
whose reserve it spent and the day after which it stops counting as
unplanned.

Leaves an existing mark alone.  Re-marking would move that day, and with it
the one figure the reserve is learned from; a mark that can be reset by a
second keystroke is not a record of when anything happened.

Works from an agenda line as well as from the entry itself, and refreshes the
view, so the day's arithmetic changes under the cursor rather than at the
next redraw.  The mark is inherited, so marking a parent covers everything
broken out of it."
  (interactive)
  (let ((marker (or (org-get-at-bol 'org-hd-marker)
                    (org-get-at-bol 'org-marker)
                    (and (derived-mode-p 'org-mode) (point-marker)))))
    (unless marker (user-error "No entry here"))
    (let (had)
      (org-with-point-at marker
        (org-back-to-heading t)
        (setq had (org-entry-get (point) org-foresight-surge-property))
        (unless had
          (org-entry-put (point) org-foresight-surge-property
                         (format-time-string (org-time-stamp-format t t)))))
      (if had
          (message "Already arrived %s" had)
        (org-foresight-report-refresh)
        (when (derived-mode-p 'org-agenda-mode) (org-agenda-redo))
        (message "Marked as arrived now")))))

(provide 'org-foresight-agenda)

;;; org-foresight-agenda.el ends here
