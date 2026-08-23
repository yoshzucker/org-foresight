;;; org-foresight-core.el --- Capacity model  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yoshzucker

;; Author: yoshzucker
;; URL: https://github.com/yoshzucker/org-foresight
;; Package-Requires: ((emacs "29.1") (org "9.6"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The dependency root of org-foresight: everything else requires this file and
;; this file requires nothing of ours.
;;
;; What lives here is the *model* -- pure computation over time values and Org
;; data, with no rendering, no network access and no writes back to Org:
;;
;;   interval algebra  set operations on (START . END) time conses
;;   clock scan        what was actually spent, per day and per category
;;   day scan          what is already committed: event intervals + effort sums
;;   capacity          work hours - busy - committed - surge, and when that runs out
;;
;; The split between "busy" and "committed" is the one rule the whole package
;; rests on, so it is stated once here and referred to elsewhere:
;;
;;   a SCHEDULED entry *with* a time contributes an INTERVAL (it is busy)
;;   a SCHEDULED entry *without* a time contributes its EFFORT (it is committed)
;;
;; Never both, or the same work is counted twice and capacity silently shrinks.

;;; Code:

(require 'org)
(require 'org-duration)
(require 'org-element)
(require 'org-datetree)
(require 'seq)
(require 'cl-lib)

;; Not required: this file is the half that reads Org and knows nothing about
;; how a day is displayed.  The one place it calls back into the agenda is
;; guarded by `derived-mode-p', which cannot be true unless org-agenda is
;; already loaded.
(declare-function org-agenda-redo "org-agenda" (&optional all))

(defvar org-foresight-now nil
  "The moment the day is being read at, or nil for the present one.

A seam for tests rather than a setting.  Almost everything here is a statement
about what is still true, so a suite that asked the clock would pass in the
morning and fail in the afternoon -- and the interesting cases all live at an
hour somebody has to be able to name.

Bound rather than passed where the caller is Org's own machinery and there is
no argument to thread it through, which is why it is a variable at all.")

(defgroup org-foresight nil
  "Forward-looking capacity, signals and scheduling for Org."
  :group 'org
  :prefix "org-foresight-")

;;;; Interval algebra
;; Operations return intervals rather than totals, because callers need the
;; resulting spans themselves (to bin, to render, to place tasks into).

(defun org-foresight--overlap-seconds (s e intervals)
  "Seconds of [S,E] covered by sorted disjoint INTERVALS (list of (A . B))."
  (let ((sum 0.0))
    (dolist (iv intervals sum)
      (let ((a (car iv)) (b (cdr iv)))
        (when (and (time-less-p s b) (time-less-p a e))
          (let ((os (if (time-less-p s a) a s))
                (oe (if (time-less-p e b) e b)))
            (setq sum (+ sum (float-time (time-subtract oe os))))))))))

;; --- Interval set algebra (return intervals, not just seconds) ----------
;; `org-foresight--overlap-seconds' above only totals coverage; the coverage/leak
;; view needs the resulting intervals themselves (to bin per half-hour), so
;; these produce (START . END) time-cons lists.  Inputs need not be sorted or
;; disjoint -- each op normalizes first.

(defun org-foresight--duration-minutes (raw &optional fallback)
  "Return RAW read as minutes, or FALLBACK when it cannot be read.

`org-duration-to-minutes\=' signals on anything it does not recognise, and a
property typed by hand is exactly where that happens: \"2h\", \"soon\", a
stray space.  One mistyped `EFFORT\=' would otherwise take down every number
this package produces -- and the page it would have taken down is the page
that shows the mistake.

Unreadable is treated as absent, which is what it is: somebody meant to give
an estimate and did not manage to."
  (or (and raw (stringp raw) (ignore-errors (org-duration-to-minutes raw)))
      fallback))

(defun org-foresight--intervals-normalize (ivs)
  "Sort IVS (list of (START . END) time conses) and merge overlaps/adjacencies.
Returns a fresh, sorted, disjoint list; never mutates IVS."
  (let ((sorted (sort (mapcar (lambda (iv) (cons (car iv) (cdr iv))) ivs)
                      (lambda (a b) (time-less-p (car a) (car b)))))
        out)
    (dolist (iv sorted (nreverse out))
      (if (and out (not (time-less-p (cdar out) (car iv))))
          ;; prev-end >= cur-start: overlap/touch -> extend prev end if longer
          (when (time-less-p (cdar out) (cdr iv))
            (setcdr (car out) (cdr iv)))
        (push iv out)))))

(defun org-foresight--intervals-intersect (a b)
  "Return intervals covered by BOTH A and B (normalized internally)."
  (let ((a (org-foresight--intervals-normalize a))
        (b (org-foresight--intervals-normalize b))
        out)
    (while (and a b)
      (let* ((ae (cdar a)) (be (cdar b))
             (lo (if (time-less-p (caar a) (caar b)) (caar b) (caar a)))
             (hi (if (time-less-p ae be) ae be)))
        (when (time-less-p lo hi) (push (cons lo hi) out))
        (if (time-less-p ae be) (setq a (cdr a)) (setq b (cdr b)))))
    (nreverse out)))

(defun org-foresight--intervals-subtract (a b)
  "Return the parts of A not covered by B (normalized internally)."
  (let ((a (org-foresight--intervals-normalize a))
        (b (org-foresight--intervals-normalize b))
        out)
    (dolist (iv a (nreverse out))
      (let ((cur (car iv)) (end (cdr iv)) (bs b))
        ;; drop b-intervals ending at/before cur
        (while (and bs (not (time-less-p cur (cdar bs)))) (setq bs (cdr bs)))
        (while (and bs (time-less-p (caar bs) end))
          (let ((os (caar bs)) (oe (cdar bs)))
            (when (time-less-p cur os)
              (push (cons cur (if (time-less-p os end) os end)) out))
            (setq cur (if (time-less-p oe cur) cur oe))
            (setq bs (cdr bs))))
        (when (time-less-p cur end) (push (cons cur end) out))))))

(defun org-foresight--intervals-seconds (ivs)
  "Total seconds covered by interval list IVS."
  (let ((sum 0.0))
    (dolist (iv ivs sum)
      (setq sum (+ sum (float-time (time-subtract (cdr iv) (car iv))))))))

;;;; Clock scan

(defcustom org-foresight-private-categories nil
  "CATEGORY values whose entries are private commitments, not work.

They occupy the day but never count against work capacity.  Read by both
surveys -- the clock scan tells a private clock from a working one, and the
day scan tells a private band from a booked one -- which is why it sits above
them rather than with the options of either."
  :type '(repeat string)
  :group 'org-foresight)


(defun org-foresight--day-start (&optional day-offset)
  "Return the Emacs time value for local midnight, DAY-OFFSET days back."
  (let ((d (decode-time (current-time))))
    (encode-time 0 0 0 (- (nth 3 d) (or day-offset 0)) (nth 4 d) (nth 5 d))))

(defun org-foresight--clock-charge-task (table cat minutes iv &optional day)
  "Add MINUTES and the segment IV under CAT to TABLE for the entry point is in.

Keyed on the heading's position, so a drawer holding several CLOCK lines is
one task rather than several.  The heading's own facts -- what it is called,
what state it is in, what it was estimated at -- are read once, the first time
that heading is seen.

IV is kept as well as its length because minutes cannot be intersected with
anything.  Asking how much of the day was spent *inside the working hours*
means cutting the clock against them, and a total has already thrown away the
one thing that cut needs.

DAY, where given, is the day the question is being asked about, and decides
`:surge\=' -- whether this is work that arrived rather than was planned.  Read
here because point is already on the heading; asking again later would mean
opening every one of them a second time."
  (let* ((head (save-excursion (org-back-to-heading t) (point)))
         (key (cons (current-buffer) head))
         (task (gethash key table)))
    (if task
        (progn
          (plist-put task :minutes (+ minutes (plist-get task :minutes)))
          ;; Safe only because `:intervals\=' is in the plist below: `plist-put\='
          ;; returns a fresh list when the key is absent, and the return value
          ;; is dropped here -- exactly as the line above already relies on.
          (plist-put task :intervals (cons iv (plist-get task :intervals))))
      (puthash key
               (save-excursion
                 (goto-char head)
                 (list :title (org-get-heading t t t t)
                       :category cat
                       :todo (org-get-todo-state)
                       :effort (org-foresight--duration-minutes
                                (org-entry-get (point) "EFFORT"))
                       :marker (point-marker)
                       :surge (and day (org-foresight--entry-surge-p day))
                       :intervals (list iv)
                       :minutes minutes))
               table))))

(defun org-foresight-clock-scan (days &optional now)
  "Scan `org-agenda-files' LOGBOOK CLOCK lines over the last DAYS days
\(today inclusive) in one pass.  A running clock (no end timestamp) is
closed at NOW, the current time by default, so its elapsed-so-far time always
counts -- every
consumer built on this plist agrees on whether \"now\" is included, unlike
the three separate hand-rolled scans this replaces.  Return a plist:
:rows           (CATEGORY . MINUTES) alist for the whole window, desc
:total          whole-window total minutes
:byday          DAYS-length vector of per-day minutes, index 0 = oldest
:days           DAYS
:today-rows     (CATEGORY . MINUTES) alist for today only, desc
:today-total    today's total minutes
:today-segments today's clock-segment count (fragmentation)
:today-intervals  today's (START . END) time conses, every clock alike
:today-private-intervals  the subset of them clocked against a private
                category.  A clock is a clock -- the watcher's leak and lost
                are measured against all of them -- but only some of it is
                work, and telling which is what stops an hour at the dentist
                being reported as an hour of the day's work
:today-tasks    plists (:title :category :todo :effort :marker :minutes
                :intervals :surge) for every entry clocked today, desc by
                minutes.  EFFORT is the estimate in minutes or nil; MARKER
                points at the heading, so a row built from one of these answers
                to the agenda's commands; INTERVALS are the segments
                themselves, so the entry can be cut against the working hours;
                SURGE says the work arrived rather than was planned
:intervals-byday  DAYS-length vector of (START . END) lists, index 0 = oldest,
                  normalized; a segment is filed under the day it starts in,
                  matching how :byday attributes minutes.
Each segment is attributed once to its heading's inherited CATEGORY, so
:rows/:today-rows partition their window (minutes sum to :total/:today-total).
The org hierarchy depth is irrelevant: CATEGORY is inherited, so a GTD
project marked with `:CATEGORY:' at any level collects all descendant clocks."
  (let* ((today0 (org-foresight--day-start 0))
         (today1 (time-add today0 (days-to-time 1)))
         (from (org-foresight--day-start (1- days)))
         (now (or now (current-time)))
         (table (make-hash-table :test 'equal))
         (today-table (make-hash-table :test 'equal))
         (byday (make-vector days 0))
         (intervals-byday (make-vector days nil))
         (total 0) (today-total 0) (today-segments 0)
         ;; Per-entry totals for today, keyed on the heading itself so several
         ;; CLOCK lines in one drawer add up.  Gathered here rather than by a
         ;; second pass: the same LOGBOOK is already open under point, and the
         ;; heading's own data is one `org-back-to-heading' away.
         (today-tasks (make-hash-table :test 'equal))
         today-intervals today-private-intervals
         (re (concat "^[ \t]*" org-clock-string
                     "[ \t]*\\(\\[[^]\n]+\\]\\)\\(?:--\\(\\[[^]\n]+\\]\\)\\)?")))
    (dolist (file (org-agenda-files))
      (with-current-buffer (find-file-noselect file)
        (org-with-wide-buffer
         (goto-char (point-min))
         (while (re-search-forward re nil t)
           ;; Read both groups before converting: `org-time-string-to-time'
           ;; runs `string-match' internally and would clobber match data.
           (let* ((s-str (match-string-no-properties 1))
                  (e-str (match-string-no-properties 2))
                  (s (org-time-string-to-time s-str))
                  (e (if e-str (org-time-string-to-time e-str) now)))
             (when (and (time-less-p s e) (time-less-p from e) (time-less-p s today1))
               (let* ((cs (if (time-less-p s from) from s))
                      (ce (if (time-less-p today1 e) today1 e))
                      (dur (/ (float-time (time-subtract ce cs)) 60.0))
                      (cat (or (org-entry-get (point) "CATEGORY" t)
                               (org-get-category (point))
                               "?"))
                      (idx (min (1- days)
                                (floor (/ (float-time (time-subtract cs from)) 86400)))))
                 (setq total (+ total dur))
                 (puthash cat (+ dur (gethash cat table 0)) table)
                 (aset byday idx (+ dur (aref byday idx)))
                 (push (cons cs ce) (aref intervals-byday idx))
                 ;; The portion of this segment (if any) inside today.
                 (when (time-less-p today0 ce)
                   (let ((ts (if (time-less-p cs today0) today0 cs)))
                     (when (time-less-p ts ce)
                       (let ((today-dur (/ (float-time (time-subtract ce ts)) 60.0)))
                         (setq today-total (+ today-total today-dur)
                               today-segments (1+ today-segments))
                         (puthash cat (+ today-dur (gethash cat today-table 0))
                                  today-table)
                         (push (cons ts ce) today-intervals)
                         ;; Kept apart rather than dropped.  A clock running
                         ;; on a private entry is a clock running -- it is
                         ;; not leak, and the watcher's account must go on
                         ;; seeing it -- but it is not work, and the hours it
                         ;; covers are hours work lent out rather than spent.
                         (when (member cat org-foresight-private-categories)
                           (push (cons ts ce) today-private-intervals))
                         (org-foresight--clock-charge-task
                          today-tasks cat today-dur (cons ts ce)
                          today0))))))))))))
    (dotimes (i days)
      (aset intervals-byday i
            (org-foresight--intervals-normalize (aref intervals-byday i))))
    (let (rows today-rows tasks)
      (maphash (lambda (k v) (push (cons k v) rows)) table)
      (maphash (lambda (k v) (push (cons k v) today-rows)) today-table)
      (maphash (lambda (_ v) (push v tasks)) today-tasks)
      (list :rows (seq-sort-by #'cdr #'> rows)
            :total total :byday byday :days days
            :today-rows (seq-sort-by #'cdr #'> today-rows)
            :today-total today-total :today-segments today-segments
            :today-intervals (nreverse today-intervals)
            :today-private-intervals
            (org-foresight--intervals-normalize
             (nreverse today-private-intervals))
            :today-tasks (seq-sort-by (lambda (e) (plist-get e :minutes)) #'> tasks)
            :intervals-byday intervals-byday))))

;;;; Project scan
;; The outline axis.  The day scan asks when work is dated; this asks how the
;; work is shaped -- which headings are projects, which are the tasks inside
;; them, and which deadline each task is really working towards.
;;
;; A third walk of the agenda files, and the reason it cannot ride on either
;; of the other two: the day scan buckets by day and keeps nothing about
;; entries dated outside its horizon, while undated work under a deadline is
;; precisely what this has to see; the clock scan is a regexp sweep of
;; LOGBOOKs and never looks at a heading.  A shared `--walk-entries' that both
;; this and the signals walk consume would put the count back to two, and is
;; the next move here -- after the classification is known to be right.

(defun org-foresight--entry-deadline ()
  "Return when the entry at point is next due, or nil.

`org-get-deadline-time\=' returns the stamp as written, which for a repeating
deadline is whenever it was first set -- a weekly review written eighteen
months ago answers with a date eighteen months gone.  Read as an overdue
commitment that is the whole of the work demanded today, every day, forever.

So a repeater is walked forward to its next occurrence at or after today,
which is the only date it actually means.  The remaining effort is the
effort for one occurrence, so counting it once, there, is exactly right.

A `.+\=' restart repeater is left as written, for the reason
`org-foresight--ts-occurrences\=' gives: its next date depends on when the
work is finished, and inventing one would be invention."
  (when-let ((written (org-get-deadline-time (point))))
    (or (save-excursion
          (org-back-to-heading t)
          (let ((meta-end (save-excursion (org-end-of-meta-data t) (point))))
            (when (and (re-search-forward (concat "\\<" org-deadline-string)
                                          meta-end t)
                       (re-search-forward org-ts-regexp (line-end-position) t))
              (goto-char (match-beginning 0))
              (let* ((el (org-element-timestamp-parser))
                     (today (org-foresight--day-start 0))
                     (occ (car (org-foresight--ts-occurrences
                                el today
                                (time-add today (days-to-time 3660))))))
                (car occ)))))
        written)))

(defun org-foresight-project-p ()
  "Non-nil when the heading at point is a project: a TODO with TODO children.

The same rule `org-foresight-project-scan\=' applies over a whole corpus,
asked of one heading instead.  Two evaluations of one rule, and a test holds
them to the same answers -- what would be worse than either is two rules,
which is how \"is this a project\" comes to mean different things in
different blocks of the same report.

Descendants at any depth, so a grouping heading with no keyword is
transparent here as it is there.  Stops at the first one found: the question
is whether any exists, not how many."
  (save-excursion
    (org-back-to-heading t)
    (and (org-get-todo-state)
         (let ((end (save-excursion (org-end-of-subtree t t) (point)))
               found)
           (forward-line 1)
           (while (and (not found) (re-search-forward org-heading-regexp end t))
             (when (org-get-todo-state) (setq found t)))
           found))))

(defun org-foresight--project-record ()
  "Return the record for the TODO heading at point, or nil if it has no keyword.

A heading with no TODO keyword gets no record at all.  That absence is the
answer to \"is this a project\": it is scaffolding, a place to put things,
and the outline is full of it.

Structure only.  What a heading *is* -- its keyword, its level, its date, who
its parent is -- is read here, for every heading, because the shape of the
outline cannot be known from a part of it.  What a heading still *needs* is
not read here at all: that is a question about a handful of leaves, and which
handful is not known until the whole file has been seen.  See
`org-foresight--project-units', which asks it of the leaves that turn out to
matter.

The split is worth having for its own sake.  A record describes the outline;
a unit describes an amount.  Keeping the amount out of the record is what
stops the two being confused, and it happens to be most of the cost as
well -- on an ordinary corpus about one leaf in six answers to a deadline,
and the rest were being priced for nobody."
  (let ((todo (org-get-todo-state)))
    (when todo
      (let ((cat (org-entry-get (point) "CATEGORY" t)))
        (list :title (org-get-heading t t t t)
              :marker (point-marker)
              :level (org-current-level)
              :todo todo
              :done (org-entry-is-done-p)
              :deadline (org-foresight--entry-deadline)
              :category cat
              :private (and (member cat org-foresight-private-categories) t)
              ;; Filled in as the walk goes past: a heading learns it is a
              ;; project from its children, never from itself.
              :todo-parent nil
              :has-todo-child nil
              :child-deadlines nil)))))

(defun org-foresight--project-walk ()
  "Return a record for every TODO heading in `org-agenda-files', linked to its
nearest TODO ancestor.

One pass per file, in document order, carrying a stack of the TODO-keyworded
headings still open above the point.  The stack gives the parent link in one
step, where comparing subtree bounds across every pair of headings would cost
a walk of its own for each.

The rule the stack encodes has two halves and one line does both:

  the stack is popped by level, unconditionally, for every heading
  only TODO-keyworded headings are ever pushed

Popping by level for a heading with no keyword is what closes the subtree it
ends -- without it the next heading would find a stale ancestor from a
sibling tree on top and adopt it, which is wrong and silent.  Never pushing
it is what makes it transparent, so a TODO grandchild under a keyword-less
child still finds its TODO grandparent.  A grouping heading is a hole in the
outline for the purpose of asking who owns what, and a wall for the purpose
of asking where a subtree ends."
  (let (out)
    (dolist (file (org-agenda-files) (nreverse out))
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (org-with-wide-buffer
           ;; Per file: containment never crosses one.
           (let (stack)
             (org-map-entries
              (lambda ()
                (let ((level (org-current-level)))
                  (while (and stack (>= (car (car stack)) level))
                    (pop stack))
                  (when-let ((rec (org-foresight--project-record)))
                    (when-let ((parent (cdr (car stack))))
                      (plist-put parent :has-todo-child t)
                      (when-let ((d (plist-get rec :deadline)))
                        (plist-put parent :child-deadlines
                                   (cons d (plist-get parent :child-deadlines))))
                      (plist-put rec :todo-parent parent))
                    (push (cons level rec) stack)
                    (push rec out))))
              nil nil))))))))

(defun org-foresight--project-classify (records)
  "Set `:project-p', `:leaf-p' and `:deadline-project-p' on RECORDS.

A second pass, and it has to be: a heading becomes a deadline project when a
*later* child turns out to carry a DEADLINE, so anything decided at the
moment a heading is visited is decided too early.

  * NEXT P
  ** NEXT leaf one     <- P is not a deadline project yet
  ** NEXT leaf two
  DEADLINE: <...>      <- and now it is

`leaf one' belongs to P, and only a pass that runs after the whole file can
say so.

`:has-todo-child' is enough for \"has a TODO descendant at any depth\".  From
any descendant, walking `:todo-parent' strictly decreases the outline level
and stays inside the ancestor, so the chain reaches it in finitely many
steps and the last link before it is a TODO child of it.  Having a TODO
child and having a TODO descendant are therefore the same claim, and the
walk already recorded the cheap one."
  (dolist (rec records records)
    (let ((project (and (plist-get rec :has-todo-child) t)))
      (plist-put rec :project-p project)
      (plist-put rec :leaf-p (not project))
      (plist-put rec :deadline-project-p
                 (and project
                      (or (plist-get rec :deadline)
                          (plist-get rec :child-deadlines))
                      t)))))

(defun org-foresight--latest-time (times)
  "Return the latest of TIMES, or nil."
  (let (latest)
    (dolist (d times latest)
      (when (or (null latest) (time-less-p latest d))
        (setq latest d)))))

(defun org-foresight--project-due (rec)
  "Return when deadline project REC is due.

Its own DEADLINE if it carries one; otherwise the *latest* of its TODO
children's.  Latest, because a project is not finished until its parts are,
and the earliest would pull every undated sibling onto the tightest child's
date and report a shortfall the day does not have.  A view that cries wolf
stops being read."
  (or (plist-get rec :deadline)
      (org-foresight--latest-time (plist-get rec :child-deadlines))))

(defun org-foresight--project-unit-of (leaf)
  "Return the record whose deadline LEAF is working towards, or nil.

Up the `:todo-parent' chain to the first deadline project.  The *first*,
which is what makes a leaf count once: a deadline project nested inside
another takes its own leaves, and the outer one never sees them.

A done ancestor stops the walk with nothing.  Work filed under something
already closed is not work anybody is waiting for, and the outline is the
only place that says so."
  (let ((p (plist-get leaf :todo-parent))
        found)
    (while (and p (not found))
      (if (plist-get p :done)
          (setq p nil)
        (if (plist-get p :deadline-project-p)
            (setq found p)
          (setq p (plist-get p :todo-parent)))))
    found))

(defun org-foresight--leaf-cost (leaf now)
  "Read what LEAF still needs, at NOW, and remember it on the record.

Asked of the leaves that turned out to answer to a deadline, and of no
others.  Going back is a jump to a marker, not a walk: the position is
already known, so the file is not searched again and nothing is re-parsed
that was parsed on the way past."
  (unless (plist-member leaf :remaining)
    (org-with-point-at (plist-get leaf :marker)
      (plist-put leaf :estimated (and (org-entry-get (point) "EFFORT") t))
      (plist-put leaf :remaining (org-foresight--entry-remaining-minutes now))))
  leaf)

(defun org-foresight--project-units (records now)
  "Return the dated commitments among RECORDS, earliest due first.

A unit is one thing with one date and one figure for what it still needs:
either a deadline project, holding the leaves that answer to it, or a lone
TODO with a DEADLINE and no children of its own.  The second is not a
project and is counted anyway -- an invoice due Friday takes its hour out of
the same week whether or not anybody broke it into steps.

Each open leaf is charged to exactly one unit, so the totals may be added
without counting an hour twice.

  :remaining-min  what its leaves still need
  :unestimated    how many of them carry no EFFORT of their own, and so are
                  standing on the default rather than on anything measured
  :largest-min    what its largest single leaf still needs.  A leaf longer
                  than a day can hold is one nobody can report progress on
                  until it is finished, so the figure the verdict rests on
                  is the least checkable kind there is
  :overdue        its date has gone, and it is tested against today instead

An overdue unit is folded onto today rather than left in the past.  The work
is real and takes hours that exist; its window is not.  Tested where it was
written, the window from now to then is empty, the shortfall is the whole of
the demand, and it poisons every later date with a debt that can never be
paid -- so the view would be nailed to it forever.

A unit whose leaves are all done is dropped.  That is not a deadline which
lands, it is one which has landed, and counting it would pad every tally of
what still has to."
  (let ((today (org-foresight--day-start 0))
        (units (make-hash-table :test #'eq))
        out)
    ;; Every deadline project is a unit, whether or not it has open leaves --
    ;; the empty ones are dropped at the end, once their leaves are known.
    (dolist (rec records)
      (when (and (plist-get rec :deadline-project-p)
                 (not (plist-get rec :done))
                 (not (plist-get rec :private)))
        (puthash rec (list :kind 'project :record rec) units)))
    (dolist (leaf records)
      (when (and (plist-get leaf :leaf-p)
                 (not (plist-get leaf :done))
                 (not (plist-get leaf :private)))
        (let* ((owner (org-foresight--project-unit-of leaf))
               (unit (cond (owner (gethash owner units))
                           ;; No project is waiting on it, but a date is.
                           ((plist-get leaf :deadline)
                            (or (gethash leaf units)
                                (puthash leaf (list :kind 'task :record leaf)
                                         units))))))
          (when unit
            (plist-put unit :leaves (cons leaf (plist-get unit :leaves)))))))
    (maphash
     (lambda (rec unit)
       (let* ((leaves (mapcar (lambda (l) (org-foresight--leaf-cost l now))
                              (plist-get unit :leaves)))
              (remaining (apply #'+ (mapcar (lambda (l)
                                              (or (plist-get l :remaining) 0.0))
                                            leaves)))
              (largest (apply #'max 0.0
                              (mapcar (lambda (l) (or (plist-get l :remaining) 0.0))
                                      leaves)))
              (due (org-foresight--project-due rec))
              (due0 (and due (org-foresight--midnight due)))
              (overdue (and due0 (time-less-p due0 today))))
         (when (and due (> remaining 0.0))
           (push (list :kind (plist-get unit :kind)
                       :title (plist-get rec :title)
                       :marker (plist-get rec :marker)
                       :category (plist-get rec :category)
                       :due due
                       :due-day (if overdue today due0)
                       :overdue (and overdue t)
                       :remaining-min remaining
                       :largest-min largest
                       :leaves (length leaves)
                       :leaf-markers (mapcar (lambda (l) (plist-get l :marker))
                                             leaves)
                       :unestimated (seq-count
                                     (lambda (l) (not (plist-get l :estimated)))
                                     leaves))
                 out))))
     units)
    (seq-sort-by (lambda (u) (float-time (plist-get u :due-day))) #'< out)))

(defun org-foresight-project-scan (&optional now)
  "Return the shape of the work in `org-agenda-files'.

  :headings  a record per TODO heading, in document order.  Structure, not
             amounts: a record says what a heading is and where it sits.
             `:remaining' appears on the leaves a deadline turned out to
             need and on no others -- see `org-foresight--project-units'

  :units     the dated commitments, earliest due first -- see
             `org-foresight--project-units'
  :now       the moment it was read at

Each record carries, besides what was read off the heading:

  :todo-parent         the record of its nearest TODO ancestor, or nil
  :has-todo-child      whether any TODO descendant exists, at any depth
  :child-deadlines     the DEADLINEs of its TODO children
  :project-p           it has a TODO descendant
  :leaf-p              it has none
  :deadline-project-p  a project, and a DEADLINE is on it or on a TODO child

The three classes the outline sorts into, and the rules exactly:

  a heading with no TODO keyword   neither project nor task; no record
  a TODO with no TODO descendant   a task
  a TODO with a TODO descendant    a project

and a project is a *deadline* project when the DEADLINE is on the project
itself or on one of its TODO children.  It stops there.  A deadline on a
grandchild belongs to the child it sits under, which is a project in its own
right, and saying it also belongs to the grandparent would make one date the
due date of every tree it is filed in.

Projects nest, and that is ordinary: a heading with TODO children is a
project however small it looks and whatever its title calls it."
  (let* ((now (or now org-foresight-now (current-time)))
         (records (org-foresight--project-classify
                   (org-foresight--project-walk))))
    (list :headings records
          :units (org-foresight--project-units records now)
          :now now)))

;;;; Day scan
(defcustom org-foresight-surge-property "SURGE"
  "Property marking work that arrived rather than was planned.

Its value is when the work arrived.  One property carries both facts, and
they cannot drift apart -- which matters because the second is what decides
when the first stops applying: work that arrived on Monday and is still being
done on Thursday is only surge on Monday, if by Thursday it has been given a
date of its own.

Read with inheritance, so a task broken out of an interruption is still part
of that interruption without being marked again, and takes the arrival of the
entry it came from.  The inheritance is asked for explicitly, so it does not
depend on `org-use-property-inheritance\'.

Nothing writes it for you: it comes from whatever captures an interruption on
your machine, or from `org-foresight-mark-surge\' after the fact."
  :type 'string
  :group 'org-foresight)

;; What is already claimed.  One pass over the agenda files fills per-day
;; vectors, in the same spirit as `org-foresight-clock-scan': scan once, hand
;; the result to every consumer, so nobody disagrees about what today holds.

(defcustom org-foresight-default-effort "0:30"
  "Effort assumed for a task that carries no EFFORT property.
Estimating badly is better than estimating nothing: an unestimated task that
counts as zero would make the day look emptier than it is, which is the one
error a capacity figure must not make."
  :type 'string
  :group 'org-foresight)

(defcustom org-foresight-default-event-duration 60
  "Minutes an appointment occupies when its timestamp gives no end time.
Calendar imports always carry an explicit range, so this only affects
hand-written appointments such as `<2026-08-11 Tue 10:00>'."
  :type 'integer
  :group 'org-foresight)

(defun org-foresight--ts-encode (y m d h mi)
  "Return an Emacs time for Y-M-D at H:MI, treating a nil H as midnight."
  (encode-time 0 (or mi 0) (or h 0) d m y))

(defun org-foresight--ts-start (el)
  "Return the start time of timestamp element EL."
  (org-foresight--ts-encode
   (org-element-property :year-start el)
   (org-element-property :month-start el)
   (org-element-property :day-start el)
   (org-element-property :hour-start el)
   (org-element-property :minute-start el)))

(defun org-foresight--ts-end (el)
  "Return the end time of timestamp element EL.
Org reports a point in time (`<... 10:00>') with its end equal to its start,
so callers must not read a non-nil end as \"this has a duration\"; use
`org-foresight--ts-has-span-p' for that."
  (org-foresight--ts-encode
   (org-element-property :year-end el)
   (org-element-property :month-end el)
   (org-element-property :day-end el)
   (org-element-property :hour-end el)
   (org-element-property :minute-end el)))

(defun org-foresight--ts-timed-p (el)
  "Non-nil when timestamp EL names a time of day, not just a date."
  (and (org-element-property :hour-start el) t))

(defun org-foresight--ts-has-span-p (el)
  "Non-nil when timestamp EL covers a stretch rather than an instant."
  (time-less-p (org-foresight--ts-start el) (org-foresight--ts-end el)))

(defun org-foresight--time-shift (time n unit)
  "Return TIME moved forward by N UNITs (a repeater unit symbol)."
  (pcase unit
    ('hour (time-add time (* n 3600)))
    ('day (time-add time (days-to-time n)))
    ('week (time-add time (days-to-time (* 7 n))))
    ('month (let ((d (decode-time time)))
              (encode-time (nth 0 d) (nth 1 d) (nth 2 d) (nth 3 d)
                           (+ (nth 4 d) n) (nth 5 d))))
    ('year (let ((d (decode-time time)))
             (encode-time (nth 0 d) (nth 1 d) (nth 2 d) (nth 3 d)
                          (nth 4 d) (+ (nth 5 d) n))))
    (_ (time-add time (days-to-time n)))))

(defun org-foresight--ts-occurrences (el from to)
  "Return (START . END) pairs for timestamp EL that touch \[FROM, TO).
A repeater is expanded forward through the window.  A `.+' (restart) repeater
is not: its next date depends on when the task is actually finished, so
predicting occupancy from it would be invention rather than measurement."
  (let* ((start (org-foresight--ts-start el))
         (end (org-foresight--ts-end el))
         (span (float-time (time-subtract end start)))
         (rtype (org-element-property :repeater-type el))
         (rval (org-element-property :repeater-value el))
         (runit (org-element-property :repeater-unit el))
         out)
    ;; An all-day or untimed stamp has END equal to START, so the window test
    ;; must accept a zero-length occurrence sitting exactly on FROM -- testing
    ;; `from < end' would silently drop every untimed entry on the first day.
    (if (or (null rtype) (eq rtype 'restart) (null rval) (< rval 1))
        (when (and (time-less-p start to) (not (time-less-p end from)))
          (push (cons start end) out))
      ;; Jump straight to the first occurrence at or after FROM where the unit
      ;; has a fixed length; a daily repeater set years ago would otherwise be
      ;; stepped through one day at a time.
      (let ((cur start)
            (secs (pcase runit ('hour 3600) ('day 86400) ('week 604800) (_ nil)))
            (guard 0))
        (when (and secs (time-less-p cur from))
          (let ((k (floor (/ (float-time (time-subtract from cur))
                             (* rval secs)))))
            (setq cur (time-add cur (* k rval secs)))))
        (while (and (time-less-p cur to) (< guard 1000))
          (let ((cend (time-add cur span)))
            (when (not (time-less-p cend from))
              (push (cons cur cend) out)))
          (setq cur (org-foresight--time-shift cur rval runit)
                guard (1+ guard)))))
    (nreverse out)))

(defun org-foresight--entry-effort-minutes ()
  "Return the entry's EFFORT in minutes, falling back to the default."
  (org-foresight--duration-minutes
   (org-entry-get (point) "EFFORT")
   (org-foresight--duration-minutes org-foresight-default-effort 30.0)))

(defun org-foresight--entry-timestamps ()
  "Return the parsed active timestamps that place the entry at point in time.
That is the SCHEDULED stamp plus any plain active stamps in the entry's own
body, stopping before the first child.  DEADLINE is deliberately excluded: a
deadline says when work must be finished, not which stretch of a day it eats."
  (let (out)
    (save-excursion
      (org-back-to-heading t)
      (let* ((subtree-end (save-excursion (org-end-of-subtree t t) (point)))
             (meta-end (save-excursion (org-end-of-meta-data t) (point)))
             (body-limit (save-excursion
                           (goto-char meta-end)
                           (if (re-search-forward org-heading-regexp subtree-end t)
                               (line-beginning-position)
                             subtree-end))))
        (save-excursion
          (org-back-to-heading t)
          (when (re-search-forward (concat "\\<" org-scheduled-string) meta-end t)
            (when (re-search-forward org-ts-regexp (line-end-position) t)
              (goto-char (match-beginning 0))
              (push (org-element-timestamp-parser) out))))
        (goto-char meta-end)
        (while (re-search-forward org-ts-regexp body-limit t)
          (goto-char (match-beginning 0))
          (let ((el (org-element-timestamp-parser)))
            (push el out)
            (goto-char (max (1+ (point)) (org-element-property :end el)))))))
    (nreverse out)))

;;;; Places
;; Where an entry has to happen.  Only a place someone deliberately wrote down
;; counts: an imported meeting whose LOCATION is just a video-call link says
;; nothing about where the body has to be, and must not be allowed to invent a
;; journey.  Travel derived from these is added to the day in a later section.

(defcustom org-foresight-places nil
  "Alist (PLACE . REGEXP) resolving an entry's LOCATION to a place.
PLACE is a symbol used to look up travel times.  A LOCATION matching nothing
leaves the entry placeless, which means \"wherever you already were\"."
  :type '(alist :key-type symbol :value-type regexp)
  :group 'org-foresight)

(defcustom org-foresight-home-place 'home
  "Place the day starts from and returns to."
  :type 'symbol
  :group 'org-foresight)

(defcustom org-foresight-travel-matrix nil
  "Alist ((FROM . TO) . MINUTES) giving the time between two places.
Looked up in either direction, so only one of each pair need be listed."
  :type '(alist :key-type (cons symbol symbol) :value-type integer)
  :group 'org-foresight)

(defcustom org-foresight-travel-default 30
  "Minutes assumed between two places not listed in the travel matrix."
  :type 'integer
  :group 'org-foresight)

(defcustom org-foresight-travel-property "TRAVEL"
  "Property naming an entry that *is* a journey, and where it goes.

Journeys are normally derived: a meeting at the office implies getting there,
and nothing has to be written down.  Sometimes the derived one is wrong -- the
train you actually catch, an errand on the way -- and then the answer is to
write the journey down and have the derivation defer to it.

  * Drive in early
  <2026-08-19 Wed 07:10-08:00>
  :PROPERTIES:
  :TRAVEL: office
  :END:

The value is the place the journey ends at.  An entry carrying it is booked
time like any other, counted as travel rather than as work, and it puts you
where it says from the moment it ends -- so nothing is derived to take you
somewhere you have already said you are going.

`\[org-foresight-book-travel]' writes one from the derived row it replaces,
which is the only way it is worth doing by hand."
  :type 'string
  :group 'org-foresight)

(defcustom org-foresight-unworkable-places nil
  "Places nothing can be worked from, so the day does not linger in them.

A journey normally arrives just in time: leaving at the last possible moment
is what a person does, and the hours before it belong to wherever they were
already.  That is right for anywhere work can happen, and wrong for the gym.
An hour there is not an hour anything else can be done in, so the day does not
wait in it -- what took you there ends and you set off, and the hours that
frees land where they are worth something.

  \='(gym)

Not a ranking.  The only question a ranking would settle beyond this one is
where to go when nothing needs you anywhere, and that already has an answer:
you go back to where the day is worked from."
  :type '(repeat symbol)
  :group 'org-foresight)

(defun org-foresight--travel-minutes (from to)
  "Return the minutes needed to get from FROM to TO."
  (if (eq from to)
      0
    (or (cdr (assoc (cons from to) org-foresight-travel-matrix))
        (cdr (assoc (cons to from) org-foresight-travel-matrix))
        org-foresight-travel-default)))

(defun org-foresight--travel-blocks (day ledger)
  "Return the journeys DAY's placed entries in LEDGER imply.

Getting somewhere is work.  Whether an employer counts the commute as paid
time is beside the point: a meeting that can only happen at the office costs
the journey as well as the hour, and a day that hides that cost will keep
running over.  So travel is booked like any other occupation.

The outward legs arrive just in time -- but not necessarily immediately
before.  Leaving at the last possible moment is only the best plan when the
last possible moment is free; where something else already occupies it, the
journey moves earlier into whatever gap will take it, which is what a person
does.  Only when no gap will take it at all is the day genuinely impossible,
and that is left to be reported as a clash rather than smoothed over.

The journey back ends when the work does -- the commute is inside the working
day, not appended to it, so a day that ends at 17:30 means being back at
17:30.  On a day that breaks, that is the end of the last interval: you go
back once, not at every pause.

Where the day's work happens is not where the body starts.  A day worked from
the office is still begun and ended at home, so it carries a journey in
before its hours and a journey back inside them -- and that pair is the cost
that makes a token appearance at the office worth thinking twice about.  It
is the reason the day has a place of its own: an office day with nothing in
the calendar used to look like a day at home."
  (let* ((work (org-foresight-work-intervals day))
         ;; Journeys somebody wrote down.  They are not places the day has to
         ;; be taken to -- they are the taking -- so they are kept out of what
         ;; needs a leg derived for it, and walked alongside it instead.
         (written (seq-filter (lambda (e)
                                (and (plist-get e :start)
                                     (plist-get e :place)
                                     (eq (plist-get e :kind) 'travel)))
                              ledger))
         (placed (seq-filter (lambda (e)
                               (and (plist-get e :start)
                                    (plist-get e :place)
                                    (not (eq (plist-get e :kind) 'travel))
                                    ;; Somebody else going somewhere is not a
                                    ;; journey of yours to make.
                                    (not (eq (plist-get e :attention)
                                             'informational))))
                             ledger))
         ;; Both, in the order the day happens in: a written leg says where
         ;; you are from the moment it ends, and everything after it has to
         ;; know.  Nothing is emitted for one -- it is already an entry, and
         ;; drawing it twice is exactly what this is for.
         (stops (seq-sort-by (lambda (e) (float-time (plist-get e :start))) #'<
                             (append placed written)))
         (taken (seq-keep (lambda (e)
                            (and (plist-get e :start)
                                 (cons (plist-get e :start) (plist-get e :end))))
                          ledger))
         ;; The breaks: everything between the first start and the last end
         ;; that is not work.  Not the hours before work or after it -- a
         ;; journey to a nine o'clock meeting has to begin before nine, and
         ;; refusing to place it there would not stop it happening.
         (off (and (cdr work)
                   (org-foresight--intervals-subtract
                    (list (cons (car (car work)) (cdr (car (last work)))))
                    work)))
         ;; Two different places, and confusing them is how a commute
         ;; disappears.  The body starts and ends where it sleeps; the day's
         ;; own place is where its work is done.  On a day worked from the
         ;; office the first of those is home and the second is the office, so
         ;; there is a journey in before the day starts and a journey back
         ;; before it ends -- which is the cost that makes a token appearance
         ;; at the office worth thinking twice about.
         (base (org-foresight-day-place day))
         (here org-foresight-home-place)
         ;; When you become free to set off.  You cannot leave for the
         ;; afternoon's client while still sitting in the morning's meeting,
         ;; and without this the backward search happily puts the second
         ;; journey before the first.
         (since (car (car work)))
         out)
    ;; Going in.  Pinned by the earliest thing that actually needs you there:
    ;; a meeting at the place, if one comes early enough to matter, and
    ;; otherwise the working day itself.  In the first case the arrival is
    ;; what is fixed and the journey is the last slot that makes it; in the
    ;; second nothing needs you at any particular minute, so what is fixed is
    ;; the departure and the journey is the first thing the day does.
    ;;
    ;; Which is what puts it inside the working hours, and that is the point.
    ;; Travel is work here: an hour spent getting somewhere is an hour that
    ;; could have gone on something else.  Placed before the hours instead, it
    ;; would come out of the morning, and going in would cost the same working
    ;; day as staying home -- which is exactly the arithmetic that makes a
    ;; token appearance at the office look free.
    (when (and work (not (eq base here))
               ;; Unless the way in is already written down, in which case
               ;; the walk below picks it up and this would be the same
               ;; journey a second time.
               (not (seq-find (lambda (e) (eq (plist-get e :place) base))
                              written)))
      (let* ((mins (org-foresight--travel-minutes here base))
             (opens (car (car work)))
             ;; The first thing today that is at the day's own place.
             (needed-by
              (car (sort (seq-keep (lambda (e)
                                     (and (plist-get e :start)
                                          (eq (plist-get e :place) base)
                                          (plist-get e :start)))
                                   placed)
                         #'time-less-p))))
        (when (> mins 0)
          (let ((leg (if (and needed-by
                              (time-less-p needed-by (time-add opens (* 60 mins))))
                         ;; Something is there before you could be: the
                         ;; arrival is what is pinned, and the journey starts
                         ;; before the day if it has to.
                         (org-foresight--travel-slot needed-by mins taken nil off)
                       (org-foresight--travel-slot-from opens mins taken off))))
            (push (list :kind 'travel
                        :title (format "→ %s" base)
                        :marker nil
                        :effort (float mins)
                        :start (car leg) :end (cdr leg)
                        :place base :location nil :category nil)
                  out)
            (push leg taken))))
      (setq here base))
    (dolist (e stops)
      (let ((there (plist-get e :place)))
        (when (eq (plist-get e :kind) 'travel)
          ;; Written: it takes you there itself.  Say so and emit nothing.
          (setq here there))
        (unless (or (eq there here) (eq (plist-get e :kind) 'travel))
          (let ((mins (org-foresight--travel-minutes here there)))
            (when (> mins 0)
              (let ((leg (if (memq here org-foresight-unworkable-places)
                             ;; Nothing keeps you where nothing can be done.
                             ;; The departure is what is pinned, so the hours
                             ;; between are spent where they are of some use
                             ;; rather than sitting in a changing room.
                             (org-foresight--travel-slot-from
                              since mins taken off)
                           (org-foresight--travel-slot
                            (plist-get e :start) mins taken since off))))
                (push (list :kind 'travel
                            :title (format "→ %s" there)
                            :marker (plist-get e :marker)
                            :effort (float mins)
                            :start (car leg) :end (cdr leg)
                            :place there :location nil :category nil)
                      out)
                (push leg taken))))
          (setq here there))
        (setq since (if since
                        (org-foresight--max-time since (plist-get e :end))
                      (plist-get e :end)))))
    ;; Coming back from somewhere the day is not worked from.  What took you
    ;; there is over, so nothing keeps you: you leave when it ends.  Waiting
    ;; instead until the day closed -- which is what this used to do -- put you
    ;; at the office from noon until half four with nothing to be there for,
    ;; and offered those hours as though they could be worked.
    ;;
    ;; Where you go back to is where the day is worked from, not home: on a day
    ;; worked from the office an errand elsewhere is an excursion, and the rest
    ;; of the day still happens at the office.  On a day worked from home the
    ;; two are the same place, and this is the journey home.
    ;;
    ;; Skipped when there would be nothing left to come back for -- if getting
    ;; back lands after the moment you would have to set off home anyway, going
    ;; back is a journey to nowhere, and the leg below takes you straight home.
    (when (and work (not (eq here base)))
      (let ((mins (org-foresight--travel-minutes here base)))
        (when (> mins 0)
          (let* ((leg (org-foresight--travel-slot-from since mins taken off))
                 (home-mins (org-foresight--travel-minutes
                             base org-foresight-home-place))
                 (must-leave (time-subtract (cdr (car (last work)))
                                            (* 60 home-mins))))
            (when (time-less-p (cdr leg) must-leave)
              (push (list :kind 'travel
                          :title (format "→ %s" base)
                          :marker nil
                          :effort (float mins)
                          :start (car leg) :end (cdr leg)
                          :place base :location nil :category nil)
                    out)
              (push leg taken)
              (setq here base
                    since (cdr leg)))))))
    ;; And home, by the way in read from the other end.  What pins it is
    ;; normally the arrival: the day ends at half five and you are home then,
    ;; so the journey is the last slot that manages it and sits inside the
    ;; hours like the one that opened them.
    ;;
    ;; Unless something is still holding you there.  A meeting that runs to six
    ;; makes being home at half five impossible, and then the departure is what
    ;; is pinned -- exactly as a meeting early enough to matter pins the
    ;; arrival on the way in.  The journey runs past the end of the day and is
    ;; counted as borrowed, because that is what it is: an hour of the evening
    ;; the day took without asking.
    (when (and work (not (eq here org-foresight-home-place)))
      (let* ((mins (org-foresight--travel-minutes here org-foresight-home-place))
             (closes (cdr (car (last work))))
             (held (and since
                        (time-less-p (time-subtract closes (* 60 mins)) since)
                        since)))
        (when (> mins 0)
          (let ((leg (if held
                         (org-foresight--travel-slot-from held mins taken off)
                       (org-foresight--travel-slot closes mins taken since off))))
            (push (list :kind 'travel
                        :title (format "→ %s" org-foresight-home-place)
                        :marker nil
                        :effort (float mins)
                        :start (car leg) :end (cdr leg)
                        :place org-foresight-home-place
                        :location nil :category nil)
                  out)))))
    (nreverse out)))

(defun org-foresight--travel-slot-from (depart mins taken off)
  "Return (START . END) for a MINS journey that may begin at DEPART.

The other half of the same rule.  A journey is placed as close as it can be
to the moment that pins it; usually what is pinned is the arrival -- a
meeting needs you there -- and the journey is the last slot that gets you
there in time.  Here it is the departure that is pinned, because nothing in
particular needs you at nine o\='clock: what needs you is the day, and the
journey is the first thing the day does.

Searched forwards for the same reason the other is searched backwards: to sit
as near the pin as the day allows, sliding past whatever is already booked
instead of being drawn over it."
  (let* ((secs (* 60 mins))
         (busy (org-foresight--intervals-normalize (append taken off)))
         (start depart)
         (found nil)
         (guard 0))
    (while (and (not found) (< guard 64))
      (setq guard (1+ guard))
      (let* ((end (time-add start secs))
             (hit (seq-find (lambda (iv)
                              (and (time-less-p start (cdr iv))
                                   (time-less-p (car iv) end)))
                            busy)))
        (if hit
            ;; Slide to begin exactly when the obstruction ends.
            (setq start (cdr hit))
          (setq found (cons start end)))))
    (or found (cons depart (time-add depart secs)))))

(defun org-foresight--travel-slot (arrive mins taken earliest &optional off)
  "Return (START . END) for a MINS journey that has to be finished by ARRIVE.

The latest slot that does not run over anything in TAKEN, searched backwards
from ARRIVE and not before EARLIEST.  Leaving at the last moment is only
right when the last moment is free; otherwise a person goes earlier, and so
does this.

OFF are stretches of the day that are not working time -- a declared break
between two work intervals.  A journey is work, so it is searched past one of
those exactly as it is searched past a meeting: the hour set aside for lunch
was set aside, and a plan that quietly spends it on the motorway has planned
a day nobody agreed to.  The cost lands where it belongs instead, on the
working hour before the break.

When nothing fits, the last-moment slot is returned anyway -- a day where the
journey cannot be made is a fact about the day, and hiding it by inventing a
slot would be worse than showing the clash.  That is also what happens to a
journey that has to begin before work starts: it is real, it is shown, and it
is counted as borrowed rather than being forced into hours it cannot fit."
  (let* ((secs (* 60 mins))
         (latest (cons (time-subtract arrive secs) arrive))
         (busy (org-foresight--intervals-normalize (append taken off)))
         (end arrive)
         (found nil))
    (while (and (not found) end
                (or (null earliest)
                    (not (time-less-p (time-subtract end secs) earliest))))
      (let* ((start (time-subtract end secs))
             (hit (seq-find (lambda (iv)
                              (and (time-less-p start (cdr iv))
                                   (time-less-p (car iv) end)))
                            busy)))
        (if hit
            ;; Slide to finish exactly when the obstruction begins.
            (setq end (car hit))
          (setq found (cons start end)))))
    (or found latest)))

;;;; The two ends of the day
;; Looking at the day when you arrive, and looking at it again before you go,
;; are not overhead: they are the two moments the rest of this package is for.
;; They take ten minutes each and appear nowhere, which means those twenty
;; minutes are taken from something else every single day, silently -- the same
;; arithmetic that makes an unbooked commute turn into a day that overruns.
;;
;; Where they go follows from the journeys rather than competing with them: the
;; journeys are settled first, and a check takes the first or last ten minutes
;; the day has left.  That is all the rule there is, and it lands them where a
;; person would put them -- after arriving, and before setting off back -- for
;; the plain reason that the drive is one of the things they cannot be drawn
;; over.
;;
;; The other order was tried and is wrong.  Checks first, travel fitted around
;; them, and the check before leaving sits in the last minutes of the day and
;; pushes the journey home past the end of it: every office day would borrow an
;; hour of the evening for a commute that used to fit.

(defcustom org-foresight-check-in nil
  "The look at the day taken once you are where the day is worked from.
A plist of :minutes and :title, or nil -- the default -- to book no such
time.

    (setq org-foresight-check-in
          \\='(:minutes 10 :title \"look at the day \\\\[org-agenda-list]\"))

Off unless asked for, because unlike a journey this is not derived from
anything written down: whether the day opens with a ritual, and how long it
takes, is a fact about a person and not about their calendar.

The title is passed through `substitute-command-keys', so it can name the way
to do the thing rather than only the thing -- and go on naming the right key
after the key has moved."
  :type '(choice (const :tag "None" nil) plist)
  :group 'org-foresight)

(defcustom org-foresight-check-out nil
  "The look at the day taken in the last minutes before you go.
A plist of :minutes and :title, or nil -- the default -- to book no such
time.  The title is passed through `substitute-command-keys'; see
`org-foresight-check-in'."
  :type '(choice (const :tag "None" nil) plist)
  :group 'org-foresight)

(defun org-foresight--check-block (spec anchor forward taken off bound)
  "Return the block SPEC asks for, placed against ANCHOR, or nil.

FORWARD means the check begins at ANCHOR and slides later when something is
in the way; otherwise it ends at ANCHOR and slides earlier.  Which is the
same rule the journeys are placed by, and for the same reason: what is fixed
is the moment being sat against, and the search goes away from it only as far
as it must.

BOUND is how far that may go -- the other end of the working day.  A journey
is allowed past it, because a drive that runs into the evening is a drive
that happens; a check is not, because a look at the day taken after the day
is over is not a look at the day.

TAKEN and OFF are what may not be written over -- what is already booked, and
the breaks the day declared.  A check is work, so an hour set aside for lunch
is no more available to it than it is to a journey.

A day with nowhere to put one gets it anyway, sitting on whatever is in the
way and marked as not fitting.  That is the same answer a journey that cannot
be made gets, and for the stronger reason: the day this happens on is the day
the check would have caught something, and a package that quietly dropped it
would be silent exactly when it was needed."
  (when-let* ((spec)
              (mins (or (plist-get spec :minutes) 0))
              ((> mins 0)))
    (let* ((slot (if forward
                     (org-foresight--travel-slot-from anchor mins taken off)
                   (org-foresight--travel-slot anchor mins taken bound off)))
           ;; The forward search slides until it finds room and would happily
           ;; leave the day to do it.  Past the bound there is nothing left to
           ;; look at, so it goes back to sitting against the anchor and says
           ;; it does not fit -- which is what the backward search already
           ;; does when it runs out of day.
           (slot (if (and forward bound (time-less-p bound (cdr slot)))
                     (cons anchor (time-add anchor (* 60 mins)))
                   slot))
           (clash (seq-some (lambda (iv)
                              (and (time-less-p (car slot) (cdr iv))
                                   (time-less-p (car iv) (cdr slot))))
                            (append taken off))))
      (list :kind 'check
            :title (or (plist-get spec :title) "check")
            :marker nil
            :effort (float mins)
            :start (car slot) :end (cdr slot)
            :wont-fit (and clash t)
            :place nil :location nil :category nil))))

(defun org-foresight--check-blocks (day legs ledger)
  "Return DAY's two checks, given its journeys LEGS and its LEDGER.

The first ten minutes of the working day that are free, and the last ten that
are.  Nothing needs to know about arrivals and departures: the journeys are
already among the things a check may not be drawn over, so on a day worked
from the office the first free minutes are the ones after you get there and
the last are the ones before you set off back.  What has to be true is only
the order -- the journeys are settled first, and the checks take what is left
-- and that is a fact about when this is called."
  (let* ((work (org-foresight-work-intervals day)))
    (when work
      (let* ((opens (car (car work)))
             (closes (cdr (car (last work))))
             (taken (seq-keep (lambda (e)
                                (and (plist-get e :start)
                                     (cons (plist-get e :start)
                                           (plist-get e :end))))
                              (append ledger legs)))
             (off (and (cdr work)
                       (org-foresight--intervals-subtract
                        (list (cons opens closes)) work)))
             out)
        (when-let ((b (org-foresight--check-block
                       org-foresight-check-in opens t taken off closes)))
          (push b out)
          (push (cons (plist-get b :start) (plist-get b :end)) taken))
        (when-let ((b (org-foresight--check-block
                       org-foresight-check-out closes nil taken off
                       ;; Not before the working day began, and no nearer than
                       ;; that.  A day solid from one o'clock has its last
                       ;; free ten minutes at ten to twelve, and that is worth
                       ;; being told: after noon you are gone.  Stopping at
                       ;; the last stretch of work instead would have said it
                       ;; on a day that breaks for lunch and not on the same
                       ;; day undeclared, which is a rule about the setting
                       ;; rather than about the day.
                       opens)))
          (push b out))
        (nreverse out)))))

;;;; Attention
;; Occupying time and demanding all of it are not the same thing, and treating
;; them as one is what makes a day look impossible when it is merely full.
;;
;;   blocking       you must be there, doing that              (the default)
;;   background     your hour, but it will share -- a call you
;;                  only have to hear can happen while walking
;;   informational  not yours at all.  A child's fixture is a
;;                  fact about the household, not work; it says
;;                  when the house is empty, and takes nothing
;;
;; Capacity was already right about this: busy intervals are unioned, so an
;; hour spent on two things at once has always counted once.  What was wrong
;; was the display, which had to drop one of the pair, and the clash signal,
;; which called every overlap impossible.

(defcustom org-foresight-attention-property "ATTENTION"
  "Property naming how much of you an entry demands.
Its value is `blocking', `background' or `informational'; anything else, or
nothing at all, means `blocking'."
  :type 'string
  :group 'org-foresight)

(defcustom org-foresight-background-categories nil
  "CATEGORY values whose entries take your time but will share it."
  :type '(repeat string)
  :group 'org-foresight)

(defcustom org-foresight-informational-categories nil
  "CATEGORY values whose entries are somebody else's commitment.
They are shown for what they say about the day and take none of it."
  :type '(repeat string)
  :group 'org-foresight)

(defun org-foresight--entry-attention (&optional category)
  "Return how much of you the entry at point demands.
An explicit property wins over the category default, so \"this one I only
have to listen to\" can be said about a single meeting."
  (let ((explicit (org-entry-get (point) org-foresight-attention-property)))
    (cond
     ((member explicit '("background" "listen")) 'background)
     ((member explicit '("informational" "info" "context")) 'informational)
     ((equal explicit "blocking") 'blocking)
     ((member category org-foresight-informational-categories) 'informational)
     ((member category org-foresight-background-categories) 'background)
     (t 'blocking))))

(defcustom org-foresight-people-property "PEOPLE"
  "Property naming the people a piece of work involves.

Read as a multi-valued property, so `:PEOPLE: 佐藤 田中\=' is two people and
`:PEOPLE+:\=' appends to the list.

It says who, and nothing else.  In particular it does not say where: most
work involving somebody can be done by message, and what makes a
conversation need a room is a judgement about that conversation -- that it
would go wrong in writing -- which nothing here can infer.  When it does need
a room, say so the way anything else says so, with `:PLACE:\='.

What the relation is comes from the entry\='s own state: work in a state
listed in `org-foresight-followup-keywords\=' is with them, and anything else
needs them."
  :type 'string
  :group 'org-foresight)

(defun org-foresight--entry-people ()
  "Return the people the entry at point involves, as a list of strings."
  (when org-foresight-people-property
    (org-entry-get-multivalued-property (point) org-foresight-people-property)))

(defun org-foresight--entry-place ()
  "Return the place of the entry at point, or nil when it names none.
An explicit `:PLACE:' property wins over `:LOCATION:', so a meeting whose
imported location is unhelpful can be corrected without editing the text."
  (let ((explicit (org-entry-get (point) "PLACE")))
    (if explicit
        (intern explicit)
      (let ((location (org-entry-get (point) "LOCATION")))
        (when location
          (car (seq-find (lambda (cell)
                           (string-match-p (cdr cell) location))
                         org-foresight-places)))))))

(defun org-foresight--day-of (time from)
  "Return the whole-day offset of TIME from midnight of FROM."
  (floor (/ (float-time (time-subtract time (org-foresight--midnight from)))
            86400)))

(defun org-foresight--midnight (time)
  "Return local midnight at the start of TIME's day."
  (let ((d (decode-time time)))
    (encode-time 0 0 0 (nth 3 d) (nth 4 d) (nth 5 d))))

(defvar org-foresight--redraw-scan nil
  "The survey the redraw now in progress is working from, or nil.

Not a cache with a lifetime of its own.  A survey is only as true as the
settings it was taken under -- turn the estimate correction off and the same
files answer differently -- so one kept by the clock would hand the old
answer to the new question.  This one lives for exactly as long as the
redraw that took it, which is the one span over which nothing can change
underneath it.")

(defun org-foresight-scan-covers-p (scan day)
  "Non-nil when SCAN has a bucket for DAY."
  (let ((off (org-foresight--day-of day (plist-get scan :from))))
    (and (>= off 0) (< off (plist-get scan :days)))))

(defun org-foresight-invalidate-scan (&rest _)
  "Forget the redraw\='s survey, so the next reader takes a fresh one.

Called before every agenda build, which is what keeps a survey from
outliving the settings it was taken under.  A survey is only true of the
options in force when it ran -- turn the estimate correction off and the
same files answer differently -- and no fingerprint of the files can notice
that, because the files did not change.  So the survey is dropped wherever a
new question might be being asked, and only shared within the one build
where nothing can move underneath it."
  (setq org-foresight--redraw-scan nil))

(defun org-foresight-scan-day (scan key day)
  "Return SCAN\='s KEY vector for DAY, or nil when DAY is outside the survey.

A survey is a run of days and index 0 is whichever day it started at, which
is not always today: one survey now answers a whole redraw, and the day a
caller wants sits wherever its date puts it.  Reading index 0 and calling it
today is right until the day a wider survey is handed in, and then it is a
figure from the wrong date with nothing to mark it as one."
  (let ((idx (org-foresight--day-of day (plist-get scan :from))))
    (and (>= idx 0) (< idx (plist-get scan :days))
         (aref (plist-get scan key) idx))))

(defun org-foresight-scan (days &optional from now)
  "Survey what is already claimed over DAYS days starting at FROM (today).
NOW, the current time by default, closes any clock still running; passing it
makes the whole scan reproducible, which is what lets this be tested.
Return a plist of DAYS-length vectors, index 0 = FROM:

  :busy       per day, the (START . END) stretches that are spoken for
  :committed  per day, minutes of effort promised, not placed at a time, and
              not yet done -- what is left of it rather than what it began as
  :surged     per day, minutes that work arriving that day has already taken,
              finished or not: what the day\'s reserve for it has been spent on.
              A `promised\' row that is part of it carries `:arrived\'
  :allday     per day, titles of all-day events
  :ledger     per day, what produced the above, entry by entry:
              (:kind :title :marker :effort :effort-adj :clocked :remaining
              :start :end), where KIND is `meeting\' (no TODO keyword),
              `task\' (placed at a time) or `promised\' (accepted but not yet
              placed).  A promised row keeps all four figures: what was
              estimated, what history says that really means, what has gone
              into it, and what is therefore left
  :days :from as given

The ledger is what lets a total be traced back to its parts.  A capacity
figure nobody can take apart is a figure nobody can act on: the answer to
\"why is there no time today\" has to be a list of things, not a number.

The busy/committed split is the rule the whole package rests on.  A timed
entry contributes an INTERVAL; an untimed one contributes its EFFORT.  Never
both -- and never twice for one entry on one day, so an entry carrying an
untimed SCHEDULED alongside a timed range is counted once, as the range.

Entries in a done state are skipped: finished work makes no claim on the
future.  Because `org-done-keywords' is consulted rather than a fixed list,
a done-type keyword such as DELEG drops out too."
  (let* ((from (or from (org-foresight--day-start 0)))
         (now (or now (current-time)))
         (from0 (org-foresight--midnight from))
         (to (time-add from0 (days-to-time days)))
         (busy (make-vector days nil))
         (committed (make-vector days 0.0))
         (surged (make-vector days 0.0))
         (allday (make-vector days nil))
         (ledger (make-vector days nil))
         ;; The day index an entry arrived on, when it arrived at all: read
         ;; once per entry and carried into its ledger row, so a reader can
         ;; be told which of the day's work landed on it.
         arrived)
    (dolist (file (org-agenda-files))
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (org-with-wide-buffer
           (org-map-entries
            (lambda ()
              ;; Arriving work spends its day\'s reserve whether or not it is
              ;; finished: the reserve is capacity held for it, and once it
              ;; has landed the capacity is spoken for either way.  Read
              ;; before the done check, which the rest of the scan makes --
              ;; an interruption dealt with by lunch is still the reason the
              ;; afternoon has less in it.
              (setq arrived
                    (when-let* ((_ (org-entry-get (point)
                                                  org-foresight-surge-property t))
                                (arrival (org-foresight--entry-arrival))
                                (i (org-foresight--day-of arrival from0))
                                ((<= 0 i))
                                ((< i days))
                                ((org-foresight--entry-surge-p arrival)))
                      (aset surged i
                            (+ (aref surged i)
                               (if (org-entry-is-done-p)
                                   (org-foresight--entry-clocked-minutes now)
                                 (org-foresight--entry-remaining-minutes now))))
                      i))
              (unless (org-entry-is-done-p)
                (let* ((todo (org-get-todo-state))
                       (effort (org-foresight--entry-effort-minutes))
                       (clocked (org-foresight--entry-clocked-minutes now))
                       (title (org-get-heading t t t t))
                       (marker (point-marker))
                       (location (org-entry-get (point) "LOCATION"))
                       ;; A journey somebody wrote down, and where it goes.
                       ;; Read before the place, because it *is* the place:
                       ;; this entry does not happen somewhere, it is the
                       ;; getting there.
                       (booked-travel
                        (and org-foresight-travel-property
                             (org-entry-get (point)
                                            org-foresight-travel-property)))
                       (place (if booked-travel
                                  (intern booked-travel)
                                (org-foresight--entry-place)))
                       (category (org-entry-get (point) "CATEGORY" t))
                       (attention (org-foresight--entry-attention category))
                       ;; day index -> the kind of claim seen there, so one
                       ;; entry cannot be charged twice for the same day
                       (seen (make-hash-table :test 'eql)))
                  ;; Work that arrived today is today\'s work, dated or not.
                  ;; An interruption is captured without a date -- there was
                  ;; no deciding where to put it -- and it would otherwise
                  ;; land on no day at all: spending the reserve held for it
                  ;; while never being counted as the thing that spent it.
                  ;; Charged to the day it arrived, through the same path an
                  ;; undated SCHEDULED takes.
                  (when-let* (((org-foresight--entry-surge-p))
                              (arrival (org-foresight--entry-arrival))
                              (idx (org-foresight--day-of arrival from0))
                              ((<= 0 idx))
                              ((< idx days)))
                    (puthash idx 'untimed seen))
                  (dolist (el (org-foresight--entry-timestamps))
                    (dolist (occ (org-foresight--ts-occurrences el from0 to))
                      (let ((idx (org-foresight--day-of (car occ) from0)))
                        (when (and (>= idx 0) (< idx days))
                          (cond
                           ((org-foresight--ts-timed-p el)
                            (let ((end (if (org-foresight--ts-has-span-p el)
                                           (cdr occ)
                                         (time-add (car occ)
                                                   (* 60 (if todo
                                                             effort
                                                           org-foresight-default-event-duration))))))
                              ;; Somebody else's commitment takes none of your
                              ;; day, so it never reaches `busy'.  It is still
                              ;; worth knowing: it is why the house is empty.
                              (unless (eq attention 'informational)
                                (push (cons (car occ) end) (aref busy idx)))
                              (push (list :kind (cond (booked-travel 'travel)
                                                      (todo 'task)
                                                      (t 'meeting))
                                          ;; Org draws this row itself.  The
                                          ;; grid must not draw it a second
                                          ;; time under a name it invented.
                                          :written (and booked-travel t)
                                          :title title :marker marker
                                          ;; Where the stamp itself is, which
                                          ;; is what the agenda's own time
                                          ;; commands edit -- see
                                          ;; `org-agenda-date-later'.
                                          :stamp (copy-marker
                                                  (org-element-property
                                                   :begin el))
                                          :attention attention
                                          :effort (/ (float-time
                                                      (time-subtract end (car occ)))
                                                     60.0)
                                          :start (car occ) :end end
                                          :place place :location location
                                          :category category)
                                    (aref ledger idx))
                              (unless (eq attention 'informational)
                                (puthash idx 'timed seen))))
                           (todo
                            (unless (gethash idx seen)
                              (puthash idx 'untimed seen)))
                           (t
                            (push title (aref allday idx))
                            (push (list :kind 'allday :title title :marker marker
                                        :effort 0.0 :start nil :end nil
                                        :place place :location location
                                        :category category)
                                  (aref ledger idx))))))))
                  ;; Charge untimed effort only where nothing timed was found.
                  ;; What capacity spends is what is *left*: the corrected
                  ;; estimate less the hours already in it.  All three figures
                  ;; are kept, because a row that shows only the last of them
                  ;; can say neither what was estimated nor what it cost.
                  (maphash (lambda (idx kind)
                             (when (eq kind 'untimed)
                               (let* ((adj (* effort (org-foresight-bias-factor
                                                      category effort)))
                                      (left (max 0.0 (- adj clocked))))
                                 (aset committed idx (+ (aref committed idx) left))
                                 (push (list :kind 'promised :title title
                                             :marker marker :effort effort
                                             :effort-adj adj
                                             :clocked clocked
                                             :remaining left
                                             :arrived (eq idx arrived)
                                             :start nil :end nil
                                             :place place :location location
                                             :category category)
                                       (aref ledger idx)))))
                           seen))))
            nil nil)))))
    (dotimes (i days)
      ;; Journeys are derived last, from the placed entries of the day, and
      ;; then booked like anything else -- so free time, capacity and
      ;; placement all account for the commute without knowing about it.
      ;;
      ;; The checks come after the journeys and from them, because where they
      ;; belong is the inner end of each: you look at the day once you have
      ;; arrived, and again before you set off back.
      (let* ((day-time (time-add from0 (days-to-time i)))
             (entries (sort (copy-sequence (aref ledger i))
                            (lambda (a b)
                              (let ((sa (plist-get a :start))
                                    (sb (plist-get b :start)))
                                (and sa sb (time-less-p sa sb))))))
             (legs (org-foresight--travel-blocks day-time entries)))
        (dolist (tb (append legs (org-foresight--check-blocks
                                  day-time legs entries)))
          (push (cons (plist-get tb :start) (plist-get tb :end)) (aref busy i))
          (push tb (aref ledger i))))
      (aset busy i (org-foresight--intervals-normalize (aref busy i)))
      ;; Chronological, with the unplaced after everything that has a time.
      (aset ledger i
            (sort (nreverse (aref ledger i))
                  (lambda (a b)
                    (let ((sa (plist-get a :start)) (sb (plist-get b :start)))
                      (cond ((and sa sb) (time-less-p sa sb))
                            (sa t)
                            (t nil)))))))
    (list :busy busy :committed committed :surged surged
          :allday allday :ledger ledger
          :days days :from from0)))

;;;; The day
;; A working day is not a single bar from nine to half five.  Work and private
;; life are mixed in time, some of the day is spent getting somewhere, and the
;; hours that belong to neither are not simply absent -- they are private time
;; that happens to be unclaimed, which is a thing worth protecting rather than
;; a reservoir to draw on.
;;
;; So a day is modelled as bands filling the waking hours:
;;
;;   grey │ travel │ meeting │ available │ grey │ available │ private │ grey
;;         └───────── work ─────────┘          └─── work ───┘
;;
;; Work is a LIST of intervals, not one bar from nine to half five.  A day can
;; break for lunch, for a school run, for anything -- and the time between two
;; work intervals is not a lesser kind of working time, it is time that is not
;; work at all.  Whoever declared the break declared it for a reason.
;;
;; "I stop at 17:30" is not the end of a window; it is that the hours after it
;; are already spoken for.  Work that lands outside the intervals therefore
;; does not disappear -- it is borrowed from time meant for something else,
;; and is marked as such.
;;
;; The intervals themselves are DECLARED, never inferred from history.
;; Measuring when work actually stopped and calling that the working day would
;; teach the system to plan around the overruns it is supposed to prevent.

(defcustom org-foresight-awake '("07:00" . "23:00")
  "Waking hours as (START . END), each \"HH:MM\".
An END at or before START is taken as the following morning."
  :type '(cons string string)
  :group 'org-foresight)

(defcustom org-foresight-work '(("09:00" . "17:30"))
  "The hours meant for work, as a list of (START . END), each \"HH:MM\".

A list, because a day need not be one unbroken stretch:

  \\='((\"09:00\" . \"12:00\")
    (\"13:00\" . \"17:30\"))

The gaps between intervals are not quiet working time -- they are time that
is not work, declared as such, and nothing here will plan into them.  That is
the whole point of being able to name more than one.

A declaration, not an observation: these are the hours you intend to keep,
and the numbers exist to be defended.  Setting them to the hours you actually
tend to work would make every day fit by construction and defeat the point of
having them at all.  A day that is genuinely different is declared on its own
heading with \\[org-foresight-shape-day]."
  :type '(repeat (cons string string))
  :group 'org-foresight)

(defcustom org-foresight-workdays '(1 2 3 4 5)
  "Days of the week that have a work span, 0 being Sunday."
  :type '(repeat integer)
  :group 'org-foresight)

(defcustom org-foresight-horizon-days 14
  "How many days ahead this package looks.


One number for every forward question -- which signals are worth raising, how
far the forward load is costed, when you are next at a given place -- because
they are the same question asked three times, and a horizon that differed
between them would answer them inconsistently."
  :type 'integer
  :group 'org-foresight)

(defun org-foresight-redraw-scan (&optional now)
  "Return the survey this redraw is working from, taking one if it has none.

A redraw wants the files twice: once for the day it is drawing and once for
the fortnight the forward view reaches over.  Surveying a fortnight costs
what surveying one day costs -- the walk of every heading is the expense and
the day count only decides how many buckets it drops them into -- so the
fortnight is taken once and both readers are answered out of it.  On a real
journal the second walk was a fifth of the redraw, spent to learn what the
first had already found out.

Anchored at today, never earlier: index 0 stays the day everything else in
this package means by it."
  (let ((files (org-foresight--scan-files)))
    (or (and org-foresight--redraw-scan
             (equal files (car org-foresight--redraw-scan))
             (cdr org-foresight--redraw-scan))
        (let ((scan (org-foresight-scan (max 1 org-foresight-horizon-days)
                                        (org-foresight--day-start 0) now)))
          ;; Read after the walk, not before: it visits files nothing was
          ;; visiting, and a survey filed under the ticks from beforehand
          ;; would never match the ticks it is looked up with.
          (setq org-foresight--redraw-scan
                (cons (org-foresight--scan-files) scan))
          scan))))

(defun org-foresight--scan-files ()
  "Return the agenda files paired with how far each has been edited here.

`buffer-chars-modified-tick\=' of whatever visits the file, or nil where
nothing does.  No file is opened to answer.  The names travel with the ticks
because a different set of files is a different world, and two files can
easily stand at the same tick.

The second of the two things that bound a shared survey, and the one that
catches an edit: a redraw that follows a write must not be answered out of
the survey taken before it.  Settings are the other, and no fingerprint
reaches those -- see `org-foresight-invalidate-scan\='."
  (mapcar (lambda (f)
            (let ((buf (find-buffer-visiting f)))
              (cons f (and buf (buffer-chars-modified-tick buf)))))
          (org-agenda-files)))


(defcustom org-foresight-day-places nil
  "Alist of (WEEKDAY . PLACE) saying where an ordinary week is worked.

WEEKDAY is 0 for Sunday.  PLACE is one of the symbols
`org-foresight-places' names.  A day not listed is worked from
`org-foresight-home-place', and any day may say otherwise on its own heading
with \\[org-foresight-shape-day].

  \\='((1 . office) (3 . office))   ; in on Mondays and Wednesdays

This is what lets the day know something no entry can tell it: that tomorrow
is worked from somewhere else.  Work that needs a place is not late until the
next day at that place has gone, and until the day has a place of its own
there is no way to ask when that is."
  :type '(alist :key-type integer :value-type symbol)
  :group 'org-foresight)

(defcustom org-foresight-day-file nil
  "Org file whose date tree carries per-day overrides.
Nil means `org-default-notes-file'.  The properties are read from the day's
own heading, so a day nobody has said anything about needs no input at all."
  :type '(choice (const :tag "org-default-notes-file" nil) file)
  :group 'org-foresight)


(defun org-foresight--places-file ()
  "Return the file whose date tree carries per-day overrides, or nil."
  (or org-foresight-day-file
      (and (boundp 'org-default-notes-file) org-default-notes-file)))

(defvar org-foresight--shape-cache nil
  "Cons (TICK . TABLE) memoizing day shapes for one state of the day file.
`org-foresight-work-intervals' is asked for every day of the horizon, several
times per render; without this each call would search the whole file again.")

(defun org-foresight--day-properties (day names)
  "Return an alist of the properties NAMES on DAY's own heading in the day tree.

All of them in one visit, because they are all on the same heading: asked one
at a time this opened the file, searched it from the top and threw the
position away once per property, four times over, for a heading most days do
not have at all.  What is not there is simply absent from the answer."
  (let ((file (org-foresight--places-file)))
    (when (and file (file-exists-p file))
      (with-current-buffer (find-file-noselect file)
        (org-with-wide-buffer
         (goto-char (point-min))
         (when (re-search-forward
                (concat "^\\*+ +"
                        (regexp-quote (format-time-string "%Y-%m-%d" day)))
                nil t)
           (seq-keep (lambda (name)
                       (when-let ((v (org-entry-get (point) name)))
                         (cons name v)))
                     names)))))))

(defun org-foresight--shape-table ()
  "Return the shape cache, emptied whenever the day file has changed.

`get-file-buffer\=', not `find-file-noselect\=': this runs on every question
about every day -- a hundred times over while one agenda is drawn -- and all
it wants is a number that says whether the answers are still good.  Opening
the file to ask costs a stat every time, which on a synchronised drive is the
single most expensive thing this package does.  A file nobody has visited has
nothing cached to invalidate, and the first read of it visits it anyway."
  (let* ((file (org-foresight--places-file))
         (buf (and file (get-file-buffer file)))
         (tick (and buf (buffer-chars-modified-tick buf))))
    (unless (and org-foresight--shape-cache
                 (equal tick (car org-foresight--shape-cache)))
      (setq org-foresight--shape-cache
            (cons tick (make-hash-table :test 'equal))))
    (cdr org-foresight--shape-cache)))

(defun org-foresight--min-time (a b) (if (time-less-p a b) a b))
(defun org-foresight--max-time (a b) (if (time-less-p a b) b a))

(defun org-foresight-day-shape (day)
  "Return DAY's shape as a plist (:awake (S . E) :work LIST-OF-(S . E) :place P).

`:work' is nil on a day with no working hours -- which an empty list also is,
so a caller may test it either way.  `:place' is where the day is worked
from, and is never nil: a day nobody has said anything about is worked from
`org-foresight-home-place'.

Resolution order is the day's own `WAKE' / `SLEEP' / `WORK' / `PLACE'
properties, then the configured defaults.  `WORK' may be \"09:00-17:30\" to
move the hours, \"09:00-12:00 13:00-17:30\" to break them up, or \"none\" to
declare the day free of work entirely."
  (let ((key (format-time-string "%Y-%m-%d" day))
        (table (org-foresight--shape-table)))
    (or (gethash key table)
        (puthash key (org-foresight--day-shape-1 day) table))))

(defun org-foresight--parse-ranges (s)
  "Return the \"HH:MM-HH:MM\" ranges in S as a list of string conses.

Separated by whitespace or commas, so \"09:00-12:00, 13:00-17:30\" and
\"09:00-12:00 13:00-17:30\" both read.  Anything that is not a range -- most
importantly the word \"none\" -- contributes nothing, which is what lets a day
be declared free of work by saying so."
  (let ((start 0) out)
    (while (string-match "\\([0-9]+:[0-9]+\\)-\\([0-9]+:[0-9]+\\)" s start)
      (push (cons (match-string 1 s) (match-string 2 s)) out)
      (setq start (match-end 0)))
    (nreverse out)))

(defun org-foresight--day-shape-1 (day)
  "Work out DAY's shape from the day file and the defaults."
  (let* ((declared (org-foresight--day-properties
                    day '("WAKE" "SLEEP" "WORK" "PLACE")))
         (wake (or (cdr (assoc "WAKE" declared))
                   (car org-foresight-awake)))
         (sleep (or (cdr (assoc "SLEEP" declared))
                    (cdr org-foresight-awake)))
         (raw (cdr (assoc "WORK" declared)))
         (work
          (cond
           (raw (org-foresight--parse-ranges raw))  ; "none" parses to nothing
           ((memq (nth 6 (decode-time day)) org-foresight-workdays)
            org-foresight-work)
           (t nil)))
         (place (or (cdr (assoc "PLACE" declared))
                    (cdr (assq (nth 6 (decode-time day))
                               org-foresight-day-places))
                    org-foresight-home-place))
         (wake-t (org-foresight--hhmm-on day wake))
         (sleep-t (org-foresight--hhmm-on day sleep)))
    ;; A bedtime at or before waking means the small hours of the next day.
    (unless (time-less-p wake-t sleep-t)
      (setq sleep-t (time-add sleep-t (days-to-time 1))))
    (list :awake (cons wake-t sleep-t)
          :place (if (stringp place) (intern place) place)
          ;; Normalized, so everything downstream may assume the intervals are
          ;; in order and do not touch: two that overlap are one stretch of
          ;; work however they were written, and a caller that had to check
          ;; would be a caller that eventually forgot to.
          :work (org-foresight--intervals-normalize
                 (mapcar (lambda (iv)
                           (cons (org-foresight--hhmm-on day (car iv))
                                 (org-foresight--hhmm-on day (cdr iv))))
                         work)))))

(defun org-foresight--gap-bands (start end work)
  "Split the empty stretch \[START, END) by whether it falls in WORK.

WORK is the day's work intervals.  Inside them the time is available for
work; outside them it is time that merely happens to be unclaimed, and a
break declared between two work intervals is as much outside as the hours
after work are.

Set algebra rather than case analysis: with several intervals the cases
multiply, and every one of them is a chance to hand back an hour that was
declared not to be work."
  (let ((gap (list (cons start end))))
    (sort (append
           (mapcar (lambda (iv)
                     (list :kind 'available :start (car iv) :end (cdr iv)))
                   (org-foresight--intervals-intersect gap work))
           (mapcar (lambda (iv)
                     (list :kind 'grey :start (car iv) :end (cdr iv)))
                   (org-foresight--intervals-subtract gap work)))
          (lambda (a b) (time-less-p (plist-get a :start) (plist-get b :start))))))

(defun org-foresight--within-p (start end intervals)
  "Non-nil when \[START, END) lies wholly inside one of INTERVALS.

One of them, not their hull: a meeting from noon to one on a day that breaks
for lunch is not inside working hours merely by being between the first hour
of the morning and the last of the afternoon."
  (seq-some (lambda (iv)
              (and (not (time-less-p start (car iv)))
                   (not (time-less-p (cdr iv) end))))
            intervals))

(defun org-foresight-day-blocks (day &optional scan)
  "Return DAY as an ordered list of bands filling the waking hours.

Each band is a plist (:kind :start :end ...), where KIND is `meeting',
`task', `travel' or `private' for something that occupies the time,
`available' for work time nothing has claimed, and `grey' for private time
nothing has claimed.  Occupations outside the work span carry `:borrowed t':
they are being taken out of private time, which is worth saying rather than
quietly absorbing.

The bands partition the waking hours exactly -- no gaps, no overlaps -- so
any total taken over them is guaranteed to add up to the day."
  (let* ((shape (org-foresight-day-shape day))
         (awake (plist-get shape :awake))
         (work (plist-get shape :work))
         (scan (or scan (org-foresight-scan 1 day)))
         (ledger (org-foresight-scan-day scan :ledger day))
         occupations out (cursor (car awake)))
    ;; Timed entries, clipped to the waking hours.  Somebody else's commitment
    ;; is left out: it takes none of the day, so giving it a band would make it
    ;; displace work that is actually happening.
    (dolist (e ledger)
      (when (and (plist-get e :start)
                 (not (eq (plist-get e :attention) 'informational)))
        (let ((s (org-foresight--max-time (plist-get e :start) (car awake)))
              (n (org-foresight--min-time (plist-get e :end) (cdr awake))))
          (when (time-less-p s n)
            (push (list :kind (if (member (plist-get e :category)
                                          org-foresight-private-categories)
                                  'private
                                (plist-get e :kind))
                        :written (plist-get e :written)
                        :start s :end n
                        :title (plist-get e :title)
                        :marker (plist-get e :marker)
                        :stamp (plist-get e :stamp)
                        :attention (or (plist-get e :attention) 'blocking)
                        :category (plist-get e :category)
                        :place (plist-get e :place))
                  occupations)))))
    ;; Earliest first; where two start together the one that will not share
    ;; goes first, so the band shows what actually has to happen then and the
    ;; obliging one is the one reported alongside it.
    (setq occupations
          (sort (nreverse occupations)
                ;; `time-equal-p', not `equal': a derived start comes out of
                ;; `time-subtract' and need not share a representation with
                ;; one that came from `encode-time', so `equal' would call two
                ;; identical instants different and leave the order to chance.
                (lambda (a b)
                  (if (time-equal-p (plist-get a :start) (plist-get b :start))
                      (and (eq (plist-get a :attention) 'blocking)
                           (not (eq (plist-get b :attention) 'blocking)))
                    (time-less-p (plist-get a :start) (plist-get b :start))))))
    (dolist (occ occupations)
      (let ((s (plist-get occ :start))
            (n (plist-get occ :end)))
        (when (time-less-p cursor s)
          (setq out (nconc out (org-foresight--gap-bands cursor s work))))
        ;; Overlapping entries: keep the day a partition by trimming the
        ;; later one rather than emitting two bands over the same minutes.
        ;; What gets trimmed away is not lost -- the grid recovers it from the
        ;; ledger, because two things booked over each other is a day that
        ;; cannot happen and must not be quietly tidied into one that can.
        (let ((trimmed (org-foresight--max-time s cursor)))
          (when (time-less-p trimmed n)
            (setq out (nconc out (list (append
                                       ;; Only work can be borrowed; private
                                       ;; time outside the span is just life.
                                       (list :borrowed
                                             (and (memq (plist-get occ :kind)
                                                        '(meeting task travel
                                                                  check))
                                                  (not (org-foresight--within-p
                                                        trimmed n work)))
                                             ;; A band cut short to keep the
                                             ;; day a partition still needs to
                                             ;; say so: an hour's journey shown
                                             ;; as fifteen minutes is a day
                                             ;; that reads as workable and is
                                             ;; not.
                                             :trimmed (time-less-p s trimmed)
                                             :full-start s)
                                       (plist-put (copy-sequence occ)
                                                  :start trimmed)))))
            (setq cursor n)))))
    (when (time-less-p cursor (cdr awake))
      (setq out (nconc out (org-foresight--gap-bands cursor (cdr awake) work))))
    out))

(defun org-foresight--day-at-point ()
  "Return the day the cursor is on, or nil where it is not on one.

An agenda puts a `day' text property over the whole of each day it draws,
date header included, so a command run from an agenda line can act on the day
the reader is looking at.  Nowhere else is there a day under the cursor, and
nil is the answer that lets a caller fall back to today."
  (when-let* ((abs (org-get-at-bol 'day))
              ((numberp abs))
              (g (calendar-gregorian-from-absolute abs)))
    (org-foresight--midnight
     (encode-time 0 0 0 (nth 1 g) (nth 0 g) (nth 2 g)))))

;;;###autoload
(defun org-foresight-shape-day (&optional day)
  "Declare the shape of DAY on its own heading.

Asks when the day starts and ends and how much of it is work, and writes the
answers to DAY's heading in the day tree, where they can also be edited by
hand.  Only exceptional days need this: a day nobody has said anything about
simply takes the defaults.

DAY is the day under the cursor when run from an agenda line and today
otherwise, which is what makes this usable in advance: the day that goes
differently is almost never the one being lived, and a command that could
only shape today would have to be remembered on the morning it was too late
to plan around.

Specific private commitments are not entered here -- they are ordinary Org
entries in a category listed in `org-foresight-private-categories', so that a
dentist appointment lives where every other appointment lives."
  (interactive (list (org-foresight--day-at-point)))
  (require 'org-datetree)
  (let* ((day (or day (org-foresight--day-start 0)))
         (shape (org-foresight-day-shape day))
         (work (plist-get shape :work))
         (file (org-foresight--places-file))
         (wake (read-string
                "Awake from: "
                (format-time-string "%H:%M" (car (plist-get shape :awake)))))
         (sleep (read-string
                 "Until: "
                 (format-time-string "%H:%M" (cdr (plist-get shape :awake)))))
         (span (read-string
                "Work hours (HH:MM-HH:MM …, or \"none\"): "
                (if work
                    (mapconcat (lambda (iv)
                                 (concat (format-time-string "%H:%M" (car iv))
                                         "-"
                                         (format-time-string "%H:%M" (cdr iv))))
                               work " ")
                  "none")))
         ;; Where the body is that day.  Asked here rather than left to be
         ;; typed into a drawer, because a day worked from somewhere else is
         ;; exactly the day somebody is in a hurry on.
         (place (read-string
                 "Worked from: "
                 (symbol-name (plist-get shape :place)))))
    (unless file (user-error "Set `org-foresight-day-file' first"))
    (with-current-buffer (find-file-noselect file)
      (org-with-wide-buffer
       (org-datetree-find-date-create
        (calendar-gregorian-from-absolute (time-to-days day)))
       (org-entry-put (point) "WAKE" wake)
       (org-entry-put (point) "SLEEP" sleep)
       (org-entry-put (point) "WORK" span)
       (if (string-empty-p (string-trim place))
           (org-entry-delete (point) "PLACE")
         (org-entry-put (point) "PLACE" (string-trim place)))
       (save-buffer)))
    (setq org-foresight--shape-cache nil)
    ;; Shaping a day from the agenda is shaping the day on the screen, and the
    ;; drawing is now wrong everywhere at once: the gaps, the commute, the
    ;; reserve and what still fits all follow from the hours just changed.
    (when (derived-mode-p 'org-agenda-mode) (org-agenda-redo))
    (message "%s: awake %s–%s, work %s, from %s"
             (format-time-string "%Y-%m-%d %a" day) wake sleep span place)))

;;;; Capacity
;; Supply and demand for one day.  Supply is the working hours minus what is
;; already spoken for; demand is the effort promised plus a reserve for work
;; that has not arrived yet.  Whatever is left is what may still be promised.

(defcustom org-foresight-surge-default "1:00"
  "Reserve held back for work that has not arrived, before any is learned.
A day planned to the minute has nowhere to put the first interruption, so a
machine with no history still plans with a buffer rather than with none."
  :type 'string
  :group 'org-foresight)

(defcustom org-foresight-surge-window 20
  "How many days back `org-foresight-learn-surge\' looks.
Only days that produced a sample contribute one: a day nothing arrived on is
a day with no evidence either way, and counting it as zero would let a quiet
fortnight argue that interruptions have stopped."
  :type 'integer
  :group 'org-foresight)

(defcustom org-foresight-surge-cache-file
  (locate-user-emacs-file "org-foresight-surge.eld")
  "Where the learned surge reserve is cached.
Derived, machine-local data: the measurement it comes from is this machine's
own activity history, and it is cheap to recompute."
  :type 'file
  :group 'org-foresight)

(defcustom org-foresight-leak-cache-file
  (locate-user-emacs-file "org-foresight-leak.eld")
  "Where the learned leak and lost budgets are cached.
Derived, machine-local data: the measurement comes from this machine\'s own
activity history, and it is cheap to recompute."
  :type 'file
  :group 'org-foresight)

(defcustom org-foresight-leak-default "0:00"
  "Leak assumed per working day before any has been measured.

Zero, because leak is the one term here that no plan can be built without
evidence for.  A guessed reserve for unrecorded time would shrink every day
on the strength of nothing, and the honest failure is the day that runs long
for a reason the tool can then name."
  :type 'string
  :group 'org-foresight)

(defcustom org-foresight-lost-default "0:00"
  "Time away from the machine assumed per working day, before any is measured."
  :type 'string
  :group 'org-foresight)

(defun org-foresight--leak-data ()
  "Return the cached leak and lost budgets as a plist, or nil for none."
  (ignore-errors
    (when (file-readable-p org-foresight-leak-cache-file)
      (with-temp-buffer
        (insert-file-contents org-foresight-leak-cache-file)
        (read (current-buffer))))))

(defun org-foresight-leak-minutes ()
  "Return the leak to expect over a whole working day, in minutes."
  (or (plist-get (org-foresight--leak-data) :leak)
      (org-foresight--duration-minutes org-foresight-leak-default 0)))

(defun org-foresight-lost-minutes ()
  "Return the time away from the machine to expect over a working day."
  (or (plist-get (org-foresight--leak-data) :lost)
      (org-foresight--duration-minutes org-foresight-lost-default 0)))

(defun org-foresight-leak-samples ()
  "Return how many days the leak budgets were learned from, or nil."
  (plist-get (org-foresight--leak-data) :samples))

(defun org-foresight--hhmm-on (day hhmm)
  "Return the time on DAY at HHMM, a \"HH:MM\" string."
  (let ((d (decode-time day))
        (parts (split-string hhmm ":")))
    (encode-time 0 (string-to-number (or (nth 1 parts) "0"))
                 (string-to-number (car parts))
                 (nth 3 d) (nth 4 d) (nth 5 d))))

(defun org-foresight-work-intervals (day)
  "Return DAY's working hours as a sorted list of (START . END), or nil.

Delegates to `org-foresight-day-shape', so a day whose heading declares its
own hours is honoured everywhere they are consulted -- capacity, the forward
load and placement all follow the same answer.

A list, not a window: the day may break, and code that took the first start
and the last end would quietly hand back the break as working time."
  (plist-get (org-foresight-day-shape day) :work))

(defun org-foresight--longest-sitting (&optional days)
  "Return minutes in the longest unbroken stretch of working time.

Not the longest gap left today, which shrinks as the day is booked and as it
is spent, but the longest run the working hours themselves offer on the
emptiest imaginable day.  Work longer than this cannot be done in one
sitting whatever the calendar looks like, so the figure says something about
the task rather than about the hour it was asked in.

DAYS days are read, a week by default, and the longest run among them wins.
A single day would make the answer depend on which day it was asked: a half
Friday would shorten every verdict, and a day off would shorten it to
nothing.  Working hours are declared per weekday, so a week sees them all.

Zero when no day in the window has working hours at all.  Callers treat that
as no bound rather than as a bound of zero, which would condemn everything."
  (let ((days (or days 7))
        (best 0.0))
    (dotimes (i days)
      (dolist (iv (org-foresight-work-intervals (org-foresight--day-start (- i))))
        (setq best (max best (/ (float-time (time-subtract (cdr iv) (car iv)))
                                60.0)))))
    best))

(defun org-foresight-day-place (day)
  "Return the place DAY is worked from.

Never nil: a day nobody has declared is worked from
`org-foresight-home-place'.  What this answers is the question no entry can
-- where the body is that day -- which is what makes \"I am here now and will
not be again until Wednesday\" a thing the day can say."
  (plist-get (org-foresight-day-shape day) :place))

(defun org-foresight-day-place-spans (day &optional bands)
  "Return when DAY is spent away from home, as ((START END . PLACE) ...).

Where the body is, minute by minute, rather than where the day is worked
from: the two differ on any day with a journey in it, and it is the first
that answers \"can I do this now\".  Home is left out -- it is the state the
day returns to, and marking it would mark most of every day.

Read off the journeys, which is what makes the answer exact: a span runs from
one arrival to the next departure, so the hours in transit belong to neither
end.  An hour on a train is not an hour at the office, and a bracket drawn
around both would say you could have done something there that you could not.

BANDS default to DAY's, which costs a scan; pass them when they are already
to hand."
  (let* ((bands (or bands (org-foresight-day-blocks day)))
         (legs (seq-sort-by (lambda (b) (float-time (plist-get b :start))) #'<
                            (seq-filter (lambda (b)
                                          (eq (plist-get b :kind) 'travel))
                                        bands)))
         (work (org-foresight-work-intervals day))
         (base (org-foresight-day-place day))
         (home org-foresight-home-place)
         ;; Where the day begins.  Away from home it begins at home, because
         ;; that is where the body slept -- but only if the journey in was
         ;; actually made: where getting there costs nothing there is no leg
         ;; to wait for, and the day is spent at its own place throughout.
         (here (if (and legs (eq (plist-get (car legs) :place) base)
                        (not (eq base home)))
                   home
                 base))
         (from (if work (car (car work)) (car (plist-get (org-foresight-day-shape day)
                                                         :awake))))
         (until (if work (cdr (car (last work)))
                  (cdr (plist-get (org-foresight-day-shape day) :awake))))
         out)
    (dolist (leg legs)
      (let ((off (plist-get leg :start)))
        (unless (or (eq here home) (not (time-less-p from off)))
          (push (cons from (cons off here)) out))
        (setq here (plist-get leg :place)
              from (plist-get leg :end))))
    (unless (or (eq here home) (not (time-less-p from until)))
      (push (cons from (cons until here)) out))
    (nreverse out)))

(defun org-foresight-place-at (day time &optional bands)
  "Return where the body is on DAY at TIME.

`org-foresight-day-place' answers where the day is worked from, which is a
different question: on a day with a journey in it the morning is still spent
at home.  This one is what \"could I do that now\" has to ask, because work
that needs a place cannot be done in an hour spent somewhere else -- and an
hour that is free is not the same as an hour that is any use.

Minutes in transit answer `org-foresight-home-place', since a span runs from
one arrival to the next departure and the road belongs to neither end.  That
is never wrong in practice: a journey is not free time, so nothing asks.

BANDS default to DAY's, which costs a scan; pass them when they are to hand."
  (or (cddr (seq-find (lambda (s)
                        (and (not (time-less-p time (car s)))
                             (time-less-p time (cadr s))))
                      (org-foresight-day-place-spans day bands)))
      org-foresight-home-place))

(defun org-foresight-day-places (day &optional bands)
  "Return every place DAY is at, the one it is worked from first.

The day's own place is where it is based; a journey in it means the day is at
somewhere else too, for the hours between arriving and setting off again.
Both belong to the answer, because \"can this be done today\" is asked of the
day as a whole -- a home day with an appointment at the office is a day on
which the office errands can, in fact, be run.

BANDS default to DAY's, which costs a scan; pass them when they are to hand."
  (let ((bands (or bands (org-foresight-day-blocks day))))
    (delete-dups
     (append
      (list (org-foresight-day-place day))
      (mapcar #'cddr (org-foresight-day-place-spans day bands))
      ;; And wherever something is actually booked, which is not the same
      ;; list.  A span runs from an arrival to a departure, so it exists only
      ;; where a journey could be placed -- and where one could not, the day
      ;; still goes: the appointment is at nine whether or not the hour before
      ;; it was free to travel in.  Without this the entry that puts the day
      ;; somewhere is the entry told the day never goes there.
      (seq-keep (lambda (b) (plist-get b :place)) bands)))))

(defun org-foresight-next-day-at (place &optional from horizon)
  "Return the next day at PLACE after FROM, or nil within HORIZON days.

FROM defaults to today and is excluded: the question is always \"when am I
next there\", asked by somebody who is there now.  Past
`org-foresight-horizon-days' the honest answer is \"not soon\" rather than a
date nobody will keep."
  (let* ((from (or from (org-foresight--day-start 0)))
         (horizon (or horizon org-foresight-horizon-days))
         (found nil))
    (cl-loop for i from 1 to horizon
             for day = (time-add from (days-to-time i))
             when (eq place (org-foresight-day-place day))
             return (setq found day))
    found))

(defun org-foresight-work-ends (day)
  "Return when DAY's work is meant to be over, or nil if it has none.

The end of the last interval -- the hour being defended, and the one thing a
list of intervals is still asked for as a single time."
  (cdr (car (last (org-foresight-work-intervals day)))))

(defun org-foresight--surge-data ()
  "Return the cached surge reserve as a plist, or nil when there is none.

A file written before the reserve meant arriving work is not read.  It held
the median of time at the machine with no clock running, which is a
measurement of recording, not of demand -- carrying it forward would keep
planning around the wrong quantity under the right name."
  (ignore-errors
    (when (file-readable-p org-foresight-surge-cache-file)
      (with-temp-buffer
        (insert-file-contents org-foresight-surge-cache-file)
        (let ((data (read (current-buffer))))
          (and (plist-get data :version) data))))))

(defun org-foresight-surge-minutes ()
  "Return the reserve to hold back for work that has not arrived, in minutes.
Reads what `org-foresight-learn-surge\' cached; falls back to
`org-foresight-surge-default\' when nothing has been learned yet."
  (or (plist-get (org-foresight--surge-data) :minutes)
      (org-foresight--duration-minutes org-foresight-surge-default 0)))

;;;###autoload
(defun org-foresight-learn-surge (&optional days)
  "Learn how much of a day arriving work takes, and cache it.

Reads only Org.  Every entry marked with `org-foresight-surge-property\'
carries the day it arrived; the minutes clocked against it on that day are
what the interruption actually cost.  Summed per day and taken at the median
over working days, that is the reserve -- and it needs no ActivityWatch, so a
machine where the watcher is not running still plans with one.

The median rather than the mean: one afternoon that went entirely to somebody
else should not become the reserve every ordinary day is planned around."
  (interactive)
  (let* ((days (or days org-foresight-surge-window))
         (per-day (make-hash-table :test 'equal))
         (now (current-time)))
    (dolist (file (org-agenda-files))
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (org-with-wide-buffer
           (org-map-entries
            (lambda ()
              (when (org-entry-get (point) org-foresight-surge-property t)
                (when-let* ((arrival (org-foresight--entry-arrival))
                            (idx (org-foresight--day-of arrival
                                                        (org-foresight--day-start
                                                         (1- days))))
                            ((<= 0 idx))
                            ((< idx days))
                            ((org-foresight--entry-surge-p arrival)))
                  (let ((key (format-time-string "%Y-%m-%d" arrival)))
                    (puthash key
                             (+ (gethash key per-day 0.0)
                                (org-foresight--entry-clocked-minutes now))
                             per-day)))))
            nil nil)))))
    (let (samples)
      (maphash (lambda (_ mins) (push mins samples)) per-day)
      (if (null samples)
          (user-error
           "No work marked with `%s\' in the last %d days; is the capture wired up?"
           org-foresight-surge-property days)
        (let ((median (org-foresight--median samples)))
          (with-temp-file org-foresight-surge-cache-file
            (prin1 (list :version 2
                         :minutes median
                         :samples (length samples)
                         :window days
                         :updated (format-time-string "%Y-%m-%d"))
                   (current-buffer)))
          (message "Arriving work takes %s on a day it arrives, from %d day(s)"
                   (org-duration-from-minutes median) (length samples))
          median)))))

;;;; Landing
;; Whether the dated work will be finished by its dates.  The day asks whether
;; today's work fits in today; this asks whether the week's work fits before
;; the week's deadlines, which is a different question with a different answer:
;; a day that fits can be a day of admin, and a day that is OVER can be the
;; only arrangement that lands the one tree due on Friday.
;;
;; Not to be confused with `:lands' in `org-foresight-capacity', which is the
;; hour today's work runs out.  Same word, different question -- that one is
;; about a clock, this one about a calendar.

(defun org-foresight--landing-dated (units scan days)
  "Return a hash from each unit to a DAYS-vector of its own dated minutes.

The correction without which this whole comparison punishes good practice.

A day's `:spare-min' is already net of `:committed-min', so an hour a unit
has scheduled for Thursday has *left* Thursday's spare.  The same hour is in
the unit's remaining.  Compared naively, the question being asked is whether
a project fits in the hours not already set aside for it -- and a project
whose every leaf has been carefully placed inside its window reads as the
one most certain to fail.

So each unit's own dated hours are handed back to the supply on the day they
sit.  Two kinds, kept apart because they are given back on different terms:
untimed promises are deferrable and count towards both figures, while an
hour fixed at a time is in `:booked-min' and only the hard figure -- the one
that assumes everything moveable moves -- can claim it.

Joined on the marker, which both surveys make with `point-marker' at the
heading inside `org-map-entries', so the same entry has the same position in
both."
  (let ((where (make-hash-table :test #'equal))
        (out (make-hash-table :test #'eq)))
    (dolist (u units)
      (puthash u (cons (make-vector days 0.0) (make-vector days 0.0)) out)
      (dolist (m (plist-get u :leaf-markers))
        (when m
          (puthash (cons (marker-buffer m) (marker-position m)) u where))))
    (dotimes (i days)
      (dolist (e (aref (plist-get scan :ledger) i))
        (when-let* ((m (plist-get e :marker))
                    (u (gethash (cons (marker-buffer m) (marker-position m))
                                where))
                    (cell (gethash u out)))
          (pcase (plist-get e :kind)
            ('promised (aset (car cell) i
                             (+ (aref (car cell) i)
                                (or (plist-get e :remaining) 0.0))))
            ('task (aset (cdr cell) i
                         (+ (aref (cdr cell) i)
                            (or (plist-get e :effort) 0.0))))
            (_ nil)))))
    out))

(defun org-foresight--landing-window (caps dated units day last)
  "Return the hours available through day index LAST, for UNITS due by DAY.

  :soft       what is free beside today's other promises
  :hard       the same once anything without a deadline gives way
  :unclaimed  waking hours outside the working day that nothing has claimed

`:unclaimed' and not the whole of the day off.  The hours outside work are
mostly spoken for -- dinner, a fixture, an evening that belongs to somebody
else -- and offering those as somewhere to put late work would be answering
\"can I stay late\" with hours that are not available to stay in.  Only what
nothing has claimed is honestly free, which is the same figure the `Off' row
draws under that name.

Clipped as a sum rather than term by term: `:spare-min' alone goes negative
on a day already promised more than it holds, while spare plus what could be
deferred may still be hours somebody has.  Clipping at zero also stops an
overrun being charged twice -- once where it happened, and again against a
deadline it has nothing to do with."
  (let ((soft 0.0) (hard 0.0) (unclaimed 0.0))
    (dotimes (i (1+ last))
      (when-let ((cap (aref caps i)))
        (let ((spare (or (plist-get cap :spare-min) 0.0))
              (committed (or (plist-get cap :committed-min) 0.0))
              (own 0.0) (own-timed 0.0))
          ;; Only the units in play give their hours back: one due later has
          ;; not been asked for yet, and its Thursday is not this deadline's
          ;; to spend.
          (dolist (u units)
            (unless (time-less-p day (plist-get u :due-day))
              (when-let ((cell (gethash u dated)))
                (setq own (+ own (aref (car cell) i))
                      own-timed (+ own-timed (aref (cdr cell) i))))))
          (setq soft (+ soft (max 0.0 (+ spare own)))
                hard (+ hard (max 0.0 (+ spare committed own-timed)))
                unclaimed (+ unclaimed
                             (max 0.0 (or (plist-get cap :unclaimed-min)
                                          0.0)))))))
    (list :soft soft :hard hard :unclaimed unclaimed)))

(defun org-foresight--landing-deferrable (scan units last)
  "Return the promised work in the first LAST+1 days that owes nobody a date.

Work with no deadline is the only work a deadline can take hours from
without anything else giving way -- that is what having no deadline means.
So this is what `defer' is about, and naming it is the difference between
being told to rearrange the week and being shown what to move.

Anything belonging to a unit is excluded: its hours are the demand, and
offering the work as its own way out would be circular."
  (let ((theirs (make-hash-table :test #'equal))
        (out (make-hash-table :test #'equal)))
    (dolist (u units)
      (dolist (m (plist-get u :leaf-markers))
        (when m (puthash (cons (marker-buffer m) (marker-position m)) t theirs))))
    (dotimes (i (1+ last))
      (dolist (e (aref (plist-get scan :ledger) i))
        (when (eq (plist-get e :kind) 'promised)
          (when-let* ((m (plist-get e :marker))
                      (key (cons (marker-buffer m) (marker-position m)))
                      ((not (gethash key theirs))))
            (let ((cell (gethash key out)))
              (puthash key
                       (list :title (plist-get e :title)
                             :marker m
                             :minutes (+ (or (plist-get cell :minutes) 0.0)
                                         (or (plist-get e :remaining) 0.0)))
                       out))))))
    (let (rows)
      (maphash (lambda (_ v) (when (> (plist-get v :minutes) 0.0) (push v rows)))
               out)
      (seq-sort-by (lambda (r) (plist-get r :minutes)) #'< rows))))

(defun org-foresight--landing-enough (candidates short key)
  "Return CANDIDATES that would close a gap of SHORT, smallest first.

The smallest that is on its own enough, which is the order
`org-foresight-report--frees' already uses for the same question about a
single day: the point is to give up the least that still works.  Nil when
nothing alone would do it -- the question has stopped being which one and
become how many, and that is a different sentence."
  (when (> short 0)
    (seq-sort-by key #'<
                 (seq-filter (lambda (c) (>= (funcall key c) short))
                             candidates))))

(defun org-foresight--landing-verdict (demand soft hard covered)
  "Return what DEMAND against SOFT and HARD means.

  lands   it fits in the hours nothing else has claimed
  defer   it fits only if work without a deadline gives way
  over    it does not fit at all: overtime, delegation, or less of it
  beyond  more than the scanned horizon holds, and the horizon is not the
          deadline -- COVERED nil

The last is the one that has to be resisted.  Supply only grows as the
window lengthens, so a fortnight's pool is a *lower bound* for a deadline
six weeks out: falling short of it says nothing except that the answer is
further away than the scan can see.  Calling that a failure would be exactly
the lie this is built to stop telling -- and `lands' and `defer' remain
sound past the horizon for the same reason, since a window that already
holds the work will still hold it when it grows."
  (cond ((<= demand soft) 'lands)
        ((<= demand hard) 'defer)
        (covered 'over)
        (t 'beyond)))

(defun org-foresight-landing (&optional projects scan now days)
  "Return whether the dated work in PROJECTS will be finished in time.

Takes both surveys rather than making either, so it may be called wherever
they are already to hand -- and so that it costs nothing to ask twice.

The test is cumulative, and cumulative is the whole of it.  Deadlines are
taken in order and the demand is added up as they pass, so each date is
asked whether *everything* due by then fits in the hours before it.  Asking
each deadline about its own work alone would pass two projects that each fit
in the same fortnight and together need three weeks of it -- which is the
ordinary way a month goes wrong, not a corner case.

That the answer needs no schedule is a theorem rather than a shortcut: for
one resource that can be interrupted, if the cumulative test holds at every
deadline then some order of the work meets all of them.  So nothing here
allocates, nothing is written, and no day is told what to do.  It reports.

  :deadlines    one entry per distinct due day, earliest first
  :first-fail   the soonest entry that cannot be made to fit, or nil
  :first-defer  the soonest that fits only by deferring other work, or nil
  :tightest     among those that land, the one with least room to spare
  :count        how many due days were tested
  :overdue      units whose date has already gone
  :unestimated  leaves standing on the default rather than an estimate
  :beyond       due days past the end of the scan
  :horizon-day  the last day the scan covers

Each entry of `:deadlines':

  :day :demand-min :soft-min :hard-min :verdict
  :short-min       what is missing against the hard figure
  :soft-short-min  what is missing against the soft one
  :spare-min       hard less demand; negative when short
  :units           the units due exactly on that day, biggest first
  :count :unestimated  the same, cumulative to that day

and, where it does not fit, the four ways out -- each a subtraction on
figures already in hand, none of them a proposed schedule:

  :lands-day    when it would fit if nothing changed, or nil past the horizon
  :drop         units that would close the gap on their own, smallest first
  :move         work owing nobody a date that would close it, smallest first
  :unclaimed-min  hours off that nothing has claimed, in the same window

Nil when nothing is dated: a report with no deadlines in it has nothing to
say about deadlines, and saying so every morning is how a line stops being
read."
  (let* ((now (or now org-foresight-now (current-time)))
         (days (or days org-foresight-horizon-days))
         (today (org-foresight--day-start 0))
         (projects (or projects (org-foresight-project-scan now)))
         (scan (or scan (org-foresight-scan days today now)))
         (units (plist-get projects :units)))
    (when units
      (let* ((dated (org-foresight--landing-dated units scan days))
             (caps (make-vector days nil))
             (horizon (time-add today (days-to-time (1- days))))
             (demand 0.0) (unestimated 0) (count 0)
             (rest units)
             out)
        (dotimes (i days)
          (let ((day (time-add today (days-to-time i))))
            (aset caps i (and (org-foresight-work-intervals day)
                              (org-foresight-capacity day scan now)))))
        (while rest
          (let* ((day (plist-get (car rest) :due-day))
                 (here (seq-take-while
                        (lambda (u) (equal day (plist-get u :due-day))) rest))
                 (idx (org-foresight--day-of day today))
                 (covered (< idx days))
                 (last (min (1- days) (max -1 idx)))
                 (w (org-foresight--landing-window caps dated units day last))
                 (soft (plist-get w :soft))
                 (hard (plist-get w :hard))
                 (verdict nil))
            (setq rest (nthcdr (length here) rest))
            (dolist (u here)
              (setq demand (+ demand (plist-get u :remaining-min))
                    unestimated (+ unestimated (plist-get u :unestimated))
                    count (1+ count)))
            (setq verdict (org-foresight--landing-verdict
                           demand soft hard covered))
            (push (list :day day
                        :demand-min demand
                        :soft-min soft
                        :hard-min hard
                        :short-min (max 0.0 (- demand hard))
                        :soft-short-min (max 0.0 (- demand soft))
                        :spare-min (- hard demand)
                        :verdict verdict
                        :units (seq-sort-by (lambda (u)
                                              (plist-get u :remaining-min))
                                            #'> here)
                        :count count
                        :unestimated unestimated
                        ;; The ways out, worked out only where there is
                        ;; something to get out of.  Every one of them is a
                        ;; subtraction on figures already in hand -- nothing
                        ;; here proposes a schedule.
                        :unclaimed-min (plist-get w :unclaimed)
                        :lands-day
                        (when (memq verdict '(over defer))
                          (let ((k last) (found nil))
                            (while (and (not found) (< k (1- days)))
                              (setq k (1+ k))
                              (let ((wk (org-foresight--landing-window
                                         caps dated units day k)))
                                (when (<= demand (plist-get wk (if (eq verdict 'over)
                                                                   :hard :soft)))
                                  (setq found
                                        (time-add today (days-to-time k))))))
                            found))
                        :drop
                        (when (eq verdict 'over)
                          (org-foresight--landing-enough
                           (seq-filter (lambda (u)
                                         (not (time-less-p day
                                                           (plist-get u :due-day))))
                                       units)
                           (- demand hard)
                           (lambda (u) (plist-get u :remaining-min))))
                        :move
                        (when (eq verdict 'defer)
                          (org-foresight--landing-enough
                           (org-foresight--landing-deferrable scan units last)
                           (- demand soft)
                           (lambda (r) (plist-get r :minutes)))))
                  out)))
        (let ((entries (nreverse out)))
          (list :deadlines entries
                :first-fail (seq-find (lambda (e) (eq (plist-get e :verdict) 'over))
                                      entries)
                :first-defer (seq-find (lambda (e)
                                         (eq (plist-get e :verdict) 'defer))
                                       entries)
                :tightest (car (seq-sort-by
                                (lambda (e) (plist-get e :spare-min)) #'<
                                (seq-filter (lambda (e)
                                              (eq (plist-get e :verdict) 'lands))
                                            entries)))
                :count (length entries)
                :overdue (seq-count (lambda (u) (plist-get u :overdue)) units)
                :unestimated unestimated
                :beyond (seq-count
                         (lambda (e) (eq (plist-get e :verdict) 'beyond))
                         entries)
                :horizon-day horizon))))))

;;;; Estimate bias
;; Estimates are systematically wrong, and always in the same direction for
;; the same kind of work.  The evidence is already there: every finished task
;; that carried an EFFORT and was clocked is one estimate measured against its
;; outcome.  Reading those back is a way of improving the numbers without
;; asking for a single new keystroke.
;;
;; How wrong depends on how big the estimate was, which one multiplier cannot
;; say.  A ratio of outcome to estimate is a quotient by a small number when
;; the estimate is small: three minutes over a two-minute guess is x2.5, and
;; three minutes over a two-hour one is x1.025.  Pooling those and taking a
;; middle is answering two different questions with one number.
;;
;; So the line is fitted in log space -- ln(actual) = a + b*ln(estimate) --
;; which makes the multiplier a function of the estimate:
;;
;;   factor(est) = exp(a) * est^(b-1)
;;
;; b = 1 is a constant multiplier, and is what this reduces to when there is
;; not enough history to fit anything; b < 1 is the common case, where small
;; estimates are the broken ones.  The slope is shared across categories and
;; only the intercept is per-category: how far ahead of yourself you run is a
;; habit of estimating, while which work runs long is a fact about the work,
;; and the second needs far less evidence to place than the first.
;;
;; Applied to what is promised and to how long a task is given when placed,
;; so a 1:30 estimate that reliably runs to 2:06 is allotted 2:06.  The
;; correction is always shown, because a shrinking day must read as "my
;; estimates are optimistic", never as "the tool is being pessimistic".

(defcustom org-foresight-bias-enabled t
  "Whether learned estimate multipliers are applied to capacity and placement."
  :type 'boolean
  :group 'org-foresight)

(defcustom org-foresight-bias-window 90
  "How many days back `org-foresight-learn-bias' looks for finished work."
  :type 'integer
  :group 'org-foresight)

(defcustom org-foresight-bias-min-samples 3
  "Finished tasks a category needs before it gets its own multiplier.
Below this the overall figure is used: one unlucky task should not decide
how an entire category is planned for."
  :type 'integer
  :group 'org-foresight)

(defcustom org-foresight-bias-max-samples 600
  "How many finished tasks the slope is fitted from at most.

The slope is the median of every pair's slope, so the work grows with the
square of the sample: past a few hundred tasks the extra pairs buy accuracy
nobody can see and cost time somebody waits for.  Beyond this the sample is
thinned evenly across the window, which keeps the whole period represented
rather than only its most recent end."
  :type 'integer
  :group 'org-foresight)

(defcustom org-foresight-bias-abandoned-keywords '("CANCELLED" "CANCELED")
  "Done-type keywords whose entries say nothing about an estimate.

Org has one idea of finished, and it covers both the work that was carried
through and the work that was dropped.  Only the first measures an estimate:
an hour\'s job abandoned after ten minutes is not evidence that hours take
minutes.  Delegated work belongs here too, on any machine that has a keyword
for it -- add yours, since the names are yours."
  :type '(repeat string)
  :group 'org-foresight)

(defcustom org-foresight-bias-slope-range '(0.3 . 1.3)
  "The steepest and shallowest slope a fit is allowed to claim.

Outside this the fit is saying something no history really supports -- that
an hour\'s work takes minutes, or that ten minutes takes half a day -- and
the day would be planned around it.  Clamping loses a real extreme; not
clamping loses the day."
  :type '(cons number number)
  :group 'org-foresight)

(defcustom org-foresight-bias-factor-range '(0.5 . 4.0)
  "The smallest and largest multiplier the correction may ever apply.

The last guard, applied after the fit: whatever curve was learnt, no estimate
is quartered and none is quadrupled.  A correction that large is not a
correction, it is a different plan.

Set the lower bound to 1.0 for the cautious reading, where a correction may
only ever grow an estimate.  Being wrong pessimistically costs an afternoon
that turns out free; being wrong optimistically costs the hours after work."
  :type '(cons number number)
  :group 'org-foresight)

(defcustom org-foresight-bias-visible-minutes 5
  "How far a corrected estimate must move before the row says both figures.

Below this the correction is inside the rounding of the figures beside it,
and writing \"0:30→0:32\" spends five columns to report nothing.  Above it
the row is showing a number that is not the one in the file, and saying so is
the difference between a tool that corrects and a tool that quietly disagrees."
  :type 'integer
  :group 'org-foresight)

(defcustom org-foresight-bias-cache-file
  (locate-user-emacs-file "org-foresight-bias.eld")
  "Where the learned estimate curve is cached."
  :type 'file
  :group 'org-foresight)

(defvar org-foresight--bias-cache nil
  "Cons (MTIME . PLIST) memoizing the contents of the bias cache file.")

(defun org-foresight--bias-data ()
  "Return the learned bias as a plist, or nil when nothing is cached."
  (let ((mtime (and (file-readable-p org-foresight-bias-cache-file)
                    (file-attribute-modification-time
                     (file-attributes org-foresight-bias-cache-file)))))
    (cond
     ((null mtime) (setq org-foresight--bias-cache nil))
     ((and org-foresight--bias-cache
           (equal mtime (car org-foresight--bias-cache)))
      (cdr org-foresight--bias-cache))
     (t
      (let ((data (org-foresight--bias-modernize
                   (ignore-errors
                     (with-temp-buffer
                       (insert-file-contents org-foresight-bias-cache-file)
                       (read (current-buffer)))))))
        (setq org-foresight--bias-cache (cons mtime data))
        data)))))

(defun org-foresight--bias-modernize (data)
  "Return DATA as a fitted curve, converting a cache written before there was one.

The older file recorded multipliers directly, which is the same statement
with the slope pinned at 1 -- so it is read as exactly that rather than
discarded.  Somebody upgrading keeps the correction they had until the next
time they learn, instead of silently losing it for a week."
  (cond
   ((null data) nil)
   ((plist-get data :slope) data)
   ((plist-get data :overall)
    (list :version 2
          :slope 1.0
          :pivot 1.0
          :intercept (log (plist-get data :overall))
          :categories (mapcar (lambda (c) (cons (car c) (log (cdr c))))
                              (plist-get data :categories))
          :samples (plist-get data :samples)
          :updated (plist-get data :updated)))
   (t nil)))

(defun org-foresight-bias-factor (category &optional minutes)
  "Return the multiplier to apply to a MINUTES estimate in CATEGORY.

The multiplier is a function of the estimate, because how far an estimate
runs over depends on how big it was.  MINUTES omitted asks for the figure at
the size most often estimated, which is the one number worth quoting when
only one will fit.

Falls back to the overall intercept for a category with too little history,
and to 1.0 when nothing has been learned at all -- an unknown bias must never
make the numbers worse than not correcting them."
  (if (not org-foresight-bias-enabled)
      1.0
    (let ((data (org-foresight--bias-data)))
      (if (null data)
          1.0
        (let* ((a (or (and category
                           (cdr (assoc category (plist-get data :categories))))
                      (plist-get data :intercept)
                      0.0))
               (m (- (or (plist-get data :slope) 1.0) 1.0))
               (pivot (or (plist-get data :pivot) 1.0))
               ;; Held to the sizes actually seen.  A curve fitted on jobs
               ;; between five minutes and two hours says nothing about a
               ;; day-long one, and following it out there would shrink an
               ;; eight-hour estimate on no evidence at all -- an error in
               ;; the one direction this package exists to prevent.  Past
               ;; the ends of the evidence the correction goes flat.
               (size (and minutes (> minutes 0)
                          (if-let ((range (plist-get data :range)))
                              (min (cdr range) (max (car range) (float minutes)))
                            (float minutes))))
               (factor (if (and size (> pivot 0))
                           (* (exp a) (expt (/ size pivot) m))
                         (exp a))))
          (min (cdr org-foresight-bias-factor-range)
               (max (car org-foresight-bias-factor-range) factor)))))))

(defun org-foresight--entry-text ()
  "Return the text of the entry at point, excluding its heading and children.

Returned as a string rather than a pair of positions on purpose.  Callers run
inside `org-map-entries', where a bare `re-search-forward' happily walks past
any limit it is handed and into the following entry; searching a string
cannot leave the entry it came from."
  (save-excursion
    (org-back-to-heading t)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t t) (point)))
           (start (progn (forward-line 1) (point)))
           (end (if (re-search-forward org-heading-regexp subtree-end t)
                    (match-beginning 0)
                  subtree-end)))
      (if (< start end)
          (buffer-substring-no-properties start end)
        ""))))

(defun org-foresight--entry-clocked-minutes (&optional now)
  "Return the minutes clocked against the entry at point, its own only.

A clock still running has no end stamp; it is closed at NOW (the current time
by default) so that time spent on the task right now counts as spent.  Left
out, a task being worked on would keep its full estimate against the day
until the moment it was clocked out of -- which is exactly the stretch during
which the day most needs to be right."
  (let ((text (org-foresight--entry-text))
        (now (or now (current-time)))
        (re (concat "^[ \t]*" org-clock-string
                    "[ \t]*\\(\\[[^]\n]+\\]\\)\\(?:--\\(\\[[^]\n]+\\]\\)\\)?"))
        (pos 0)
        (total 0.0))
    (while (string-match re text pos)
      ;; Read every group out before converting: `org-time-string-to-time'
      ;; matches internally and would clobber the match data mid-expression.
      (let ((s-str (match-string 1 text))
            (e-str (match-string 2 text)))
        (setq pos (match-end 0))
        (let ((s (org-time-string-to-time s-str))
              (e (if e-str (org-time-string-to-time e-str) now)))
          (when (time-less-p s e)
            (setq total (+ total (/ (float-time (time-subtract e s)) 60.0)))))))
    total))

(defun org-foresight--entry-arrival ()
  "Return when the entry at point arrived, or nil when nothing says.

Three sources, in falling order of directness:

1. the value of `org-foresight-surge-property\', if it parses as a time --
   written by whatever captured the interruption, so it is the moment itself
2. the earliest state-change timestamp in the entry\'s log
3. the start of its earliest clock

The second takes the earliest rather than the first line.  The order inside a
log drawer is not reliable -- `org-log-states-order-reversed\' governs what is
written next, not what is already there, and a file that has lived through a
change of that setting has drawers in both orders.  A timestamp is a fact; a
position in a drawer is not.

The property is read with inheritance, so a task broken out of an
interruption is dated by the interruption rather than by itself.  The other
two are the entry\'s own, which is right for an entry that carries its own
mark and the best that can be had for one that does not."
  (or (org-foresight--parse-stamp
       (org-entry-get (point) org-foresight-surge-property t))
      (org-foresight--entry-earliest "^[ \t]*- State \"[^\"]*\"[ \t]*from[^[\n]*\\(\\[[^]\n]+\\]\\)")
      (org-foresight--entry-earliest
       (concat "^[ \t]*" org-clock-string "[ \t]*\\(\\[[^]\n]+\\]\\)"))))

(defun org-foresight--parse-stamp (text)
  "Return the time TEXT names, or nil when it names none."
  (when (and text (string-match "\\[[^]\n]+\\]\\|<[^>\n]+>" text))
    (ignore-errors (org-time-string-to-time (match-string 0 text)))))

(defun org-foresight--entry-earliest (regexp)
  "Return the earliest time REGEXP\'s first group finds in the entry at point.

Every match is read before any is compared, because the drawer\'s order says
nothing about the timestamps\' order."
  (let ((text (org-foresight--entry-text))
        (pos 0)
        best)
    (while (string-match regexp text pos)
      (let ((stamp (match-string 1 text)))
        (setq pos (match-end 0))
        (when-let ((time (ignore-errors (org-time-string-to-time stamp))))
          (when (or (null best) (time-less-p time best))
            (setq best time)))))
    best))

(defun org-foresight--entry-surge-p (&optional day)
  "Non-nil when the entry at point is work that arrived rather than was planned.

Marked (or descended from something marked) with
`org-foresight-surge-property\', and not since given a date of its own.  A
SCHEDULED on the day it arrived is not a plan -- it is where the capture put
it -- so it still counts; a SCHEDULED on any other day means the work has
been taken in hand, and from then it is ordinary promised work.

That is the whole point of dating the mark: an interruption absorbed over
three days should show as unplanned load on the first day only.  Leaving it
as surge for all three would hold a reserve against work already on the
calendar, and hold it three times over.

DAY, when given, asks whether it was surge *on that day* rather than at all."
  (when (org-entry-get (point) org-foresight-surge-property t)
    (let ((arrival (org-foresight--entry-arrival))
          (sched (org-get-scheduled-time (point))))
      (and arrival
           (or (null day) (= 0 (org-foresight--day-of day arrival)))
           (or (null sched)
               (= 0 (org-foresight--day-of sched arrival)))))))

(defun org-foresight--entry-remaining-minutes (&optional now)
  "Return what the entry at point still needs, in minutes.

The corrected estimate less what has already gone into it.  A day\'s capacity
is a question about what is left, and an entry half done that still weighs
its full estimate is the same error as an estimate that was never corrected --
it makes the afternoon look impossible on the strength of the morning.

Zero once the clock has passed the estimate.  That the work is not finished
is then a fact about the estimate rather than about the day, and the estimate
correction is where it belongs."
  (let* ((raw (org-foresight--entry-effort-minutes))
         (adj (* raw (org-foresight-bias-factor
                      (org-entry-get (point) "CATEGORY" t) raw))))
    (max 0.0 (- adj (org-foresight--entry-clocked-minutes now)))))

;;;###autoload
(defun org-foresight-learn-bias (&optional days)
  "Learn how far estimates run over, and by how much more when they are small.

Reads only what is already recorded -- an EFFORT and the clock beside it --
so this costs nothing to start using.  Work that was abandoned rather than
carried through is left out: its clock says nothing about its estimate.

Fits `ln(actual) = a + b*ln(estimate)\' and caches the two numbers.  Both are
medians rather than means, in keeping with the rest of this file: one task
that went badly wrong should not reshape the plan for every task like it."
  (interactive)
  (let* ((days (or days org-foresight-bias-window))
         (from (org-foresight--day-start (1- days)))
         samples)
    (dolist (file (org-agenda-files))
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (org-with-wide-buffer
           (org-map-entries
            (lambda ()
              (when (and (org-entry-is-done-p)
                         (not (member (org-get-todo-state)
                                      org-foresight-bias-abandoned-keywords)))
                ;; `org-entry-get\' would happily return the next heading\'s
                ;; text for a CLOSED that is not there; the planning API
                ;; knows the difference between a timestamp and a property.
                (let ((closed (org-entry-get (point) "CLOSED" nil t))
                      (effort (org-entry-get (point) "EFFORT")))
                  (when (and closed effort)
                    (let ((when (org-time-string-to-time closed)))
                      (when (time-less-p from when)
                        (let ((est (org-foresight--duration-minutes effort 0))
                              (act (org-foresight--entry-clocked-minutes)))
                          (when (and (> est 0) (> act 0))
                            (push (list est act
                                        (org-entry-get (point) "CATEGORY" t))
                                  samples)))))))))
            nil nil)))))
    (if (null samples)
        (user-error "No finished, estimated, clocked work in the last %d days"
                    days)
      (let* ((fit (org-foresight--bias-fit samples))
             (data (append fit
                           (list :version 2
                                 :by-effort (org-foresight--bias-by-effort
                                             samples)
                                 :samples (length samples)
                                 :window days
                                 :updated (format-time-string "%Y-%m-%d")))))
        (with-temp-file org-foresight-bias-cache-file
          (prin1 data (current-buffer)))
        (setq org-foresight--bias-cache nil)
        (message "%s" (org-foresight-bias-summary data))
        data))))

(defun org-foresight--bias-fit (samples)
  "Return the plist describing how SAMPLES miss, and by how much more when small.

Each sample is (ESTIMATE ACTUAL CATEGORY) in minutes.  What is fitted is the
overrun against the size of the estimate -- ln(actual/estimate) against
ln(estimate) -- rather than outcome against estimate, because the overrun is
the quantity anybody reasons about and it puts the answer in the units the
rest of this file speaks.  A slope of zero on that line is a constant
multiplier; a negative one is small estimates being missed by more.

The slope is Theil-Sen: the median of the slope between every pair of points.
That is the median used everywhere else here, lifted into two dimensions, and
it takes as many wrong tasks to move as it takes wrong days to move a median,
which is half of them.

The line is centred on the size actually estimated most -- the median
estimate -- so the intercept is the multiplier at a size that exists.
Centred at zero it would be the multiplier for a one-minute task, which is an
extrapolation past every sample and a nonsense to quote as a headline.

One slope serves every category and only the intercept is taken per category.
Running ahead of yourself is a habit of estimating and needs the whole corpus
to see; which work runs long is a fact about the work and shows in a handful
of tasks.  Fitting a slope per category would ask the smaller question of the
larger evidence.

Falls back to a flat line -- a plain constant multiplier, which is what this
did before it drew any line at all -- when there is too little to fit, or
when every estimate was the same size and there is no slope to see."
  (let* ((keep (org-foresight--bias-thin samples))
         (pivot (or (org-foresight--median
                     (mapcar (lambda (s) (float (nth 0 s))) keep))
                    1.0))
         (p (log pivot))
         (pts (mapcar (lambda (s)
                        (let ((x (log (float (nth 0 s)))))
                          (cons x (- (log (float (nth 1 s))) x))))
                      keep))
         (slopes nil))
    (dolist (a pts)
      (dolist (b pts)
        (when (< (car a) (car b))
          (push (/ (- (cdr b) (cdr a)) (- (car b) (car a))) slopes))))
    (let* ((raw (or (org-foresight--median slopes) 0.0))
           (m (if (< (length keep) org-foresight-bias-min-samples)
                  0.0
                (min (1- (cdr org-foresight-bias-slope-range))
                     (max (1- (car org-foresight-bias-slope-range)) raw))))
           (residual (lambda (s)
                       (let ((x (log (float (nth 0 s)))))
                         (- (log (float (nth 1 s))) x (* m (- x p))))))
           (by-category (make-hash-table :test 'equal))
           cats)
      (dolist (s samples)
        (when (nth 2 s)
          (push (funcall residual s) (gethash (nth 2 s) by-category))))
      (maphash (lambda (cat rs)
                 (when (>= (length rs) org-foresight-bias-min-samples)
                   (push (cons cat (org-foresight--median rs)) cats)))
               by-category)
      (list :slope (+ 1.0 m)
            :pivot pivot
            :range (cons (apply #'min (mapcar (lambda (s) (float (nth 0 s)))
                                              keep))
                         (apply #'max (mapcar (lambda (s) (float (nth 0 s)))
                                              keep)))
            :intercept (org-foresight--median (mapcar residual samples))
            :categories cats))))

(defun org-foresight--bias-thin (samples)
  "Return at most `org-foresight-bias-max-samples\' of SAMPLES, evenly spread.

Taken at a stride rather than from one end, so a year of history is still a
year of history after thinning: dropping the older half would fit the line to
the last few weeks and call it a habit."
  (let ((n (length samples)))
    (if (<= n org-foresight-bias-max-samples)
        samples
      (let ((stride (/ (float n) org-foresight-bias-max-samples))
            (i 0.0)
            out)
        (while (< (floor i) n)
          (push (nth (floor i) samples) out)
          (setq i (+ i stride)))
        (nreverse out)))))

(defun org-foresight--bias-by-effort (samples)
  "Return what SAMPLES actually did, grouped by the estimate they carried.

Each group is (MINUTES COUNT MEDIAN-RATIO).  Kept beside the fitted line
rather than derived from it, because a line is a claim and this is the
evidence: seeing both is what tells you whether to believe the first."
  (let ((by (make-hash-table :test 'eql))
        out)
    (dolist (s samples)
      (push (/ (float (nth 1 s)) (nth 0 s)) (gethash (nth 0 s) by)))
    (maphash (lambda (est ratios)
               (push (list est (length ratios)
                           (org-foresight--median ratios))
                     out))
             by)
    (sort out (lambda (a b) (< (car a) (car b))))))

(defun org-foresight-bias-summary (&optional data)
  "Return one line describing the learned curve in DATA, or nil for none.

Named at two sizes rather than as a slope, because nobody plans a day in
exponents: what a reader can act on is that a five-minute job takes twenty
and an hour-long one takes an hour and a bit."
  (when-let ((data (or data (org-foresight--bias-data))))
    (let ((small (org-foresight-bias-factor nil 5))
          (large (org-foresight-bias-factor nil 60)))
      (format "Estimates run ×%.1f at 0:05 and ×%.1f at 1:00 · slope %.2f · %d task(s)"
              small large (or (plist-get data :slope) 1.0)
              (or (plist-get data :samples) 0)))))

(defun org-foresight--median (numbers)
  "Return the median of NUMBERS, or nil when there are none.
The median rather than the mean, because one catastrophic day of firefighting
should not become the reserve every ordinary day is planned around."
  (when numbers
    (let* ((s (sort (copy-sequence numbers) #'<))
           (n (length s)))
      (if (cl-oddp n)
          (nth (/ n 2) s)
        (/ (+ (nth (1- (/ n 2)) s) (nth (/ n 2) s)) 2.0)))))

(defun org-foresight-surge-samples ()
  "Return how many days the cached surge reserve was learned from, or nil."
  (plist-get (org-foresight--surge-data) :samples))

(defun org-foresight--window-remaining (window now)
  "Return the part of WINDOW that has not already elapsed at NOW.
Capacity is about what can still be promised, so a morning that is already
over is not free time however empty the calendar looked at breakfast."
  (cond ((null window) nil)
        ((not (time-less-p (car window) now)) window)   ; wholly ahead
        ((time-less-p now (cdr window)) (cons now (cdr window)))
        (t nil)))                                       ; wholly past

(defun org-foresight--intervals-remaining (intervals now)
  "Return the parts of INTERVALS that have not already elapsed at NOW."
  (seq-keep (lambda (iv) (org-foresight--window-remaining iv now)) intervals))

(defun org-foresight--window-elapsed (window now)
  "Return the part of WINDOW that has already gone at NOW.

The exact complement of `org-foresight--window-remaining\=', written beside it
in the same three branches and the same order so that a reader can see the two
are total by looking rather than by reasoning: every branch that hands the
whole window to one hands nothing to the other, and the branch that splits it
splits it at the same instant.

Deliberately not \"intersect with midnight-to-now\": that needs a midnight it
has not been given, and it stops being a visible dual of the function above --
which is the only thing that keeps the two from drifting apart."
  (cond ((null window) nil)
        ((not (time-less-p (car window) now)) nil)      ; wholly ahead
        ((time-less-p now (cdr window)) (cons (car window) now))
        (t window)))                                    ; wholly past

(defun org-foresight--intervals-elapsed (intervals now)
  "Return the parts of INTERVALS that have already gone at NOW."
  (seq-keep (lambda (iv) (org-foresight--window-elapsed iv now)) intervals))

(defun org-foresight--day-busy (day scan)
  "Return what SCAN says is already taken on DAY."
  (let ((idx (org-foresight--day-of day (plist-get scan :from))))
    (and (>= idx 0)
         (< idx (plist-get scan :days))
         (aref (plist-get scan :busy) idx))))

(defun org-foresight-free-intervals (day &optional scan now)
  "Return the stretches of DAY's working hours that nothing has claimed.
SCAN is a `org-foresight-scan' result covering DAY; one is taken for DAY alone
when omitted.  NOW defaults to the current time and clips away the part of the
day that has already gone.

A declared break is not among them.  It is not free working time that happens
to be empty -- it is not working time."
  (let* ((now (or now (current-time)))
         (windows (org-foresight--intervals-remaining
                   (org-foresight-work-intervals day) now))
         (scan (or scan (org-foresight-scan 1 day))))
    (when windows
      (org-foresight--intervals-subtract
       windows (org-foresight--day-busy day scan)))))

(defun org-foresight-waking-free-intervals (day &optional scan now)
  "Return the stretches of DAY nothing has claimed, working hours or not.

The same subtraction as `org-foresight-free-intervals\=' over the whole waking
day rather than the working part of it.  Work that will not fit inside the
hours you meant to keep does not stop existing when they end; it runs on into
the time after them, and where it stops is a fact worth having.

Not capacity: nothing here may be promised away, and every minute of it was
meant for something else.  It exists to answer when, not to offer."
  (let* ((now (or now (current-time)))
         (awake (plist-get (org-foresight-day-shape day) :awake))
         (window (org-foresight--window-remaining awake now))
         (scan (or scan (org-foresight-scan 1 day))))
    (when window
      (org-foresight--intervals-subtract
       (list window) (org-foresight--day-busy day scan)))))

(defun org-foresight--run-out-intervals (day &optional scan now)
  "Return where DAY's work would actually go, in the order it would go there.

Two stretches, and the order is the whole point:

  first  what is left of the working hours themselves, break excluded
  then   whatever is still free once those hours are over

So work fills the hours meant for it, and only what will not fit runs on past
the end.  A declared break is in neither -- not the lunch hour, and not the
morning before work starts.  Pouring through a break would answer \"when will
this be over\" with an hour that assumes you worked through the one part of
the day you said you would not, which is the comfortable answer and the wrong
one.

On a day with a single unbroken stretch of work this is exactly the waking
day from NOW, which is what it has always been."
  (let* ((now (or now (current-time)))
         (scan (or scan (org-foresight-scan 1 day)))
         (ends (org-foresight-work-ends day))
         (after (if ends (org-foresight--max-time now ends) now)))
    (append (org-foresight-free-intervals day scan now)
            (org-foresight-waking-free-intervals day scan after))))

(defun org-foresight--bias-minutes (scan idx)
  "Return how many of day IDX\'s promised minutes are the estimate correction.

The difference between what the estimates said and what they are being
treated as, which is the one figure that says what the correction is costing
today.  Read off the ledger, where both numbers were kept side by side for
exactly this: a correction nobody can see the size of is a correction nobody
can argue with."
  (let ((ledger (and (>= idx 0) (< idx (plist-get scan :days))
                     (aref (plist-get scan :ledger) idx)))
        (total 0.0))
    (dolist (e ledger total)
      (when (and (eq (plist-get e :kind) 'promised)
                 (plist-get e :effort-adj))
        (setq total (+ total (- (plist-get e :effort-adj)
                                (or (plist-get e :effort) 0.0))))))))

(defun org-foresight--surge-left (scan idx ahead)
  "Return the reserve still held for work that has not arrived, in minutes.

Two ceilings, and the lower one wins.

The first is what the day\'s allowance has left in it.  Work that has arrived
is no longer hypothetical: it is an entry with an estimate, counted in what
the day owes.  Holding the whole reserve beside it would put the same hours
in the day twice.

The second is what could still arrive.  AHEAD is the fraction of the working
window still to come, and an allowance for a whole day cannot land in the
half hour that is left of one.

Both are ceilings on the same quantity, so neither is subtracted from the
other -- the answer is simply the smaller.  It is a different question from
the one leak asks: leak is a rate, and what has already leaked says nothing
about what the rest of the day will."
  (let* ((budget (org-foresight-surge-minutes))
         (spent (if (and (>= idx 0) (< idx (plist-get scan :days)))
                    (aref (plist-get scan :surged) idx)
                  0.0)))
    (max 0.0 (min (- budget spent) (* budget ahead)))))

(defun org-foresight--task-intervals (clock surge)
  "Return the clock segments of CLOCK's tasks whose arrived-ness is SURGE."
  (org-foresight--intervals-normalize
   (apply #'append
          (seq-keep (lambda (task)
                      (and (eq (and (plist-get task :surge) t) surge)
                           (copy-sequence (plist-get task :intervals))))
                    (and clock (plist-get clock :today-tasks))))))

(defun org-foresight-behind (day &optional clock coverage now)
  "Return how the elapsed part of DAY's working hours was actually spent.

The backward half of the day, and the counterweight to everything else here.
`org-foresight-capacity\=' answers what may still be promised, and it answers
it from what is *left* to do -- so a task marked DONE leaves it entirely and
the hours it took leave with it.  That is right for a forecast and wrong for a
picture of the day: by four in the afternoon a forecast alone says the day is
mostly empty, when what is true is that it is mostly gone.

Five segments, which divide the elapsed working span exactly:

  :borrowed-min   a clock running on a private entry inside the working
                  hours.  Not work, and not unrecorded either: the hour is
                  accounted for, it was simply lent out.  The mirror of the
                  `borrowed\=' the hours off report, which is work taking
                  their time -- each row names the hours it lent the other
  :baseline-min   clock time against work that did not arrive today: the
                  level the day was already running at before anything landed
                  on it.  Named for exactly what is checked, which is the
                  absence of a dated surge mark and nothing else.  Not
                  `planned\=', which would claim the work was on a plan when no
                  plan is consulted -- a task written this morning and clocked
                  at noon lands here, and so does effort spent long past its
                  estimate.  The sums the bar draws:

                    baseline + surge                = the clock, in hours
                    baseline + surge + :outside-min = the clock, all of it
  :surge-min      clock time against work that arrived unplanned
  :unclocked-min  elapsed, no clock running, and nothing says you were away
  :away-min       elapsed, no clock running, the watcher says afk
  :behind-min     the elapsed span itself; the four sum to it

  :outside-min    clock that has already run and fell outside the working
                  hours.  Not one of the four -- it is not part of the span
                  they divide -- and named
                  so that a `Clocked\=' total which disagrees with the bar can
                  be accounted for rather than doubted.  With a declared lunch
                  break this is an ordinary occurrence, not a corner.
  :measured       whether the watcher had anything to say, so that an `away\='
                  of zero can be told from an `away\=' nobody looked for
  :elapsed        the intervals themselves
  :unclocked-ivs
  :away-ivs       the two unrecorded segments as intervals rather than as
                  totals.  A total says how much of the day went unaccounted
                  for; these say *which* stretches did, which is what
                  `org-foresight-clock-fill\=' needs in order to ask about
                  them one at a time instead of making somebody type hours

The derivation has to run in this order, and each step earns its place:

  W  = C \\ P             the clock, less anything clocked as private
  B  = P ∩ E              private clock inside the working hours: lent out
  Cb = W ∩ E              the working clock, cut to the elapsed span
  Sb = (S ∩ Cb) \\ P       the same, and the baseline wins any overlap
  baseline  = Cb \\ Sb
  U         = E \\ Cb \\ B       elapsed, with no clock running at all
  away      = U ∩ F
  unclocked = U \\ F

`unclocked\=' is `U \\ F\=' and not `U ∩ active\='.  The difference looks
cosmetic and is not.  Splitting by \"active\" would make three sets -- active,
afk, and the time the watcher says nothing about (asleep, not yet started, a
hole in the bucket) -- and the third would have to be assigned by hand.  Every
such hand-assignment is a special case, and the special case is always wrong on
the machine with no watcher at all.  Taking the complement of afk instead means
only what the watcher positively vouches for becomes `away\=', the rest is
unrecorded time, and a day with no watcher falls to one segment through the
same arithmetic rather than through a branch.

Never returns nil.  Given no clock it reports the whole elapsed span as
unclocked, which is the truthful reading -- nothing is known about it -- and
one a reader can see, where a nil would quietly draw the day at half length."
  (let* ((now (or now org-foresight-now (current-time)))
         (work (org-foresight-work-intervals day))
         (elapsed (org-foresight--intervals-elapsed work now))
         (all (org-foresight--intervals-normalize
               (copy-sequence (and clock (plist-get clock :today-intervals)))))
         ;; A clock on a private entry covers the hour as surely as any
         ;; other, but it does not spend the hour on work.  Inside the
         ;; working day those hours are lent out, not worked; outside it they
         ;; are simply yours, and nothing here has anything to say about
         ;; them.
         (priv (and clock (plist-get clock :today-private-intervals)))
         (work-clock (org-foresight--intervals-subtract all priv))
         ;; Cut to the elapsed span first.  A clock may run before work
         ;; begins, through a declared break, or into the evening, and none of
         ;; that may enter a partition of the working hours.  It also clips a
         ;; hand-written CLOCK line that ends in the future, which looks like
         ;; a missing guard and is not: E stops at NOW by construction.
         (cb (org-foresight--intervals-intersect work-clock elapsed))
         (borrowed (org-foresight--intervals-intersect priv elapsed))
         (by-plan (org-foresight--task-intervals clock nil))
         (arrived (org-foresight--task-intervals clock t))
         ;; Intersected with Cb rather than with E: a per-task segment that
         ;; somehow escaped the day's own list cannot then break the sum -- it
         ;; is dropped rather than counted twice.  Overlaps go to planned,
         ;; which is the reading that does not spend the reserve on a minute
         ;; that had planned work in it too.
         (sb (org-foresight--intervals-subtract
              (org-foresight--intervals-intersect arrived cb) by-plan))
         (baseline (org-foresight--intervals-subtract cb sb))
         (unaccounted (org-foresight--intervals-subtract
                       (org-foresight--intervals-subtract elapsed cb)
                       borrowed))
         (afk (and coverage (plist-get coverage :afk-ivs)))
         (away (org-foresight--intervals-intersect unaccounted afk))
         (unclocked (org-foresight--intervals-subtract unaccounted afk))
         (mins (lambda (ivs) (/ (org-foresight--intervals-seconds ivs) 60.0))))
    (list :behind-min (funcall mins elapsed)
          :baseline-min (funcall mins baseline)
          :borrowed-min (funcall mins borrowed)
          :surge-min (funcall mins sb)
          :unclocked-min (funcall mins unclocked)
          :away-min (funcall mins away)
          ;; Clock that has already run and fell outside the working hours --
          ;; cut to NOW first, so a CLOCK line written for later today is not
          ;; reported as an hour spent somewhere it has not been spent yet.
          :outside-min (funcall mins
                                (org-foresight--intervals-subtract
                                 (org-foresight--intervals-elapsed work-clock
                                                                   now)
                                 elapsed))
          :measured (and afk t)
          :elapsed elapsed
          :unclocked-ivs unclocked
          :away-ivs away)))

(defun org-foresight-capacity (day &optional scan now)
  "Return a plist describing how much of DAY may still be promised.

Within the part of the work span that is still ahead, these divide it
exactly:

  :span-min   the whole span in minutes, either side of NOW alike
  :behind-min the part of it NOW has passed.  Nothing here divides that
              half; `org-foresight-behind\=' does, from what was clocked
  :ahead-min  the part still to come, which the rest of this group divides
  :booked-min meetings and work already placed at a time, less any of it
              that is already over
  :travel-min getting to and from them, on the same terms
  :private-min-in-span  life that happens to fall in working hours, likewise
  :committed-min  effort still owed on work promised for DAY but not placed
  :bias-min   how much of `:committed-min' is the estimate correction
  :surge-min  what is still held for work that has not arrived
  :leak-min   what is still expected to go unrecorded
  :lost-min   what is still expected to go away from the machine
  :reserve-min  the three of them together
  :reserve-day-min  the whole of today's allowance, before the day spent any
              of it: what `:reserve-min' was at the first minute of the span.
              The denominator that makes the remainder mean something -- a
              reserve down to nothing has been used, not abolished
  :spare-min  what survives all of it -- negative means overcommitted

Outside it, these divide the rest of the waking day:

  :off-min        the waking day less the work span, either side of NOW
  :off-behind-min the part of it NOW has passed.  Nothing here divides that
                  half either: what is known about it is what the clock says,
                  which `org-foresight-behind\=' reports as `:outside-min\='
  :off-ahead-min  the part still to come, which the rest of this group
                  divides
  :private-min    commitments that are life rather than work, still to come
  :borrowed-min   work that fell outside the span, taken from private time
  :unclaimed-min  waking hours nothing has claimed at all
  :grey-min       an alias of `:unclaimed-min'

And what is left of today, as opposed to what the day was shaped like:

  :free       stretches nothing has claimed yet, from NOW onwards
  :free-min   minutes in those stretches
  :needed-min `:committed-min' plus `:reserve-min' -- the wall clock the rest
              of the day has to find
  :headroom-min   `:free-min' less `:needed-min'
  :lands      when the day's work runs out, counting the hours past the end
              of work, or nil when it does not fit in today at all.  A day
              with nothing owed still lands -- at once -- and it is for the
              caller to decide that saying so is not worth a line
  :overflow-min   what will not fit in today at all, zero when it all does

These five close over the remaining working day, by construction:

  :free-min = :committed-min + :surge-min + :leak-min + :lost-min
              + :headroom-min

which is what lets an overrun be read as a list of terms rather than as a
verdict.  Every one of them is measured from NOW, so they answer what is
still true rather than what the morning looked like.

Everything above is measured from NOW.  A meeting held this morning is not
capacity this afternoon, and counting its hour here as well as in
`org-foresight-behind\=' would let the same hour be spent twice -- once as a
plan and once as a fact.  What the day looked like when it started is a
different question, and `:reserve-day-min\=' is the one figure kept for it.

NOW defaults to `org-foresight-now\=', and that to the current time.  Passing
it makes the whole calculation reproducible, which is what lets this be
tested at all -- and honouring the variable is what lets a whole render be
pinned to an hour at once, for a demonstration or a screenshot, without
every caller in the chain having to be told."
  (let* ((now (or now org-foresight-now (current-time)))
         (scan (or scan (org-foresight-scan 1 day now)))
         (idx (org-foresight--day-of day (plist-get scan :from)))
         (work (org-foresight-work-intervals day))
         ;; The hours themselves, not first-start to last-end: a day that
         ;; breaks for an hour has an hour less to promise, and a hull would
         ;; hand that hour back as capacity.
         (span (/ (org-foresight--intervals-seconds work) 60.0))
         (free (org-foresight-free-intervals day scan now))
         (free-min (/ (org-foresight--intervals-seconds free) 60.0))
         (committed (if (and (>= idx 0) (< idx (plist-get scan :days)))
                        (aref (plist-get scan :committed) idx)
                      0.0))
         (bias (org-foresight--bias-minutes scan idx))
         ;; What is left of the day to be interrupted in.  Every reserve is a
         ;; claim about the hours still ahead, so each is measured against
         ;; them rather than against the whole span -- which is what stops a
         ;; full day's allowance being held against the last half hour of it.
         (remaining (org-foresight--intervals-remaining work now))
         (rest (/ (org-foresight--intervals-seconds remaining) 60.0))
         (ahead (if (> span 0) (min 1.0 (/ rest span)) 0.0))
         (surge (org-foresight--surge-left scan idx ahead))
         (leak (* (org-foresight-leak-minutes) ahead))
         (lost (* (org-foresight-lost-minutes) ahead))
         (reserve (+ surge leak lost))
         ;; The same three at the top of the day: nothing arrived yet and the
         ;; whole span still ahead.  Kept beside the remainder so the two can
         ;; be shown together -- a reserve that has shrunk to nothing looks
         ;; identical to one that never existed, and they are opposites.
         (reserve-day (+ (org-foresight-surge-minutes)
                         (org-foresight-leak-minutes)
                         (org-foresight-lost-minutes)))
         (needed (+ committed reserve))
         ;; Where the work would actually go: the hours meant for it first,
         ;; then whatever is left once they are over.  Both `:lands' and
         ;; `:overflow-min' are read off this one list, so the hour named and
         ;; the amount said not to fit can never disagree.
         (run-out (org-foresight--run-out-intervals day scan now))
         (bands (org-foresight-day-blocks day scan))
         (awake (plist-get (org-foresight-day-shape day) :awake))
         ;; The waking hours that are not working hours, and the part of them
         ;; still to come.  The bands drawn against them are cut to it for
         ;; the same reason the working ones are: an evening that is over
         ;; cannot be spent again, and a row that goes on offering it decays
         ;; through the day exactly as the work row used to.
         (off-ivs (org-foresight--intervals-subtract
                   (list (cons (car awake) (cdr awake))) work))
         (off-left (org-foresight--intervals-remaining off-ivs now))
         (booked 0.0) (travel 0.0) (private-out 0.0) (private-in 0.0)
         (borrowed 0.0) (grey 0.0))
    (dolist (b bands)
      (let* ((iv (cons (plist-get b :start) (plist-get b :end)))
             ;; What the band still costs.  Every band is counted by what is
             ;; left of it rather than by its length: an hour that is over
             ;; cannot be spent again, and its time is already spoken for by
             ;; the record of what happened.  A row drawn from lengths decays
             ;; through the day -- by the evening it offers hours that have
             ;; gone -- which is true of the hours outside the working day
             ;; exactly as it is of the ones inside it.
             (left (/ (org-foresight--intervals-seconds
                       (org-foresight--intervals-intersect (list iv) remaining))
                      60.0))
             ;; The same, for the bands that lie outside the working hours:
             ;; they are measured against what is left of the evening rather
             ;; than of the working day.
             (off (/ (org-foresight--intervals-seconds
                      (org-foresight--intervals-intersect (list iv) off-left))
                     60.0)))
        (pcase (plist-get b :kind)
          ('grey (setq grey (+ grey off)))
          ;; A private commitment is not work and not empty time; without a
          ;; figure of its own it vanished from the account entirely, leaving
          ;; hours that were spoken for looking free.  One that falls in
          ;; working hours is counted apart from the rest, because it is time
          ;; the day cannot spend on work and must not be handed back as
          ;; spare.
          ('private
           (if (org-foresight--within-p (car iv) (cdr iv) work)
               (setq private-in (+ private-in left))
             (setq private-out (+ private-out off))))
          ('travel (if (plist-get b :borrowed)
                       (setq borrowed (+ borrowed off))
                     (setq travel (+ travel left))))
          ;; A check is booked work like any other: it is ten minutes of the
          ;; day that cannot be spent twice, and the whole reason to derive it
          ;; is that nothing was counting it.
          ((or 'meeting 'task 'check)
           (if (plist-get b :borrowed)
               (setq borrowed (+ borrowed off))
             (setq booked (+ booked left))))
          (_ nil))))
    (list :work work
          :span-min span
          :ahead-min rest
          :behind-min (- span rest)
          :booked-min booked
          :travel-min travel
          :private-min-in-span private-in
          :free free
          :free-min free-min
          :committed-min committed
          :bias-min bias
          :surge-min surge
          :leak-min leak
          :lost-min lost
          :reserve-min reserve
          :reserve-day-min reserve-day
          :needed-min needed
          :spare-min (- rest booked travel private-in committed reserve)
          :headroom-min (- free-min needed)
          :lands (org-foresight--pour run-out needed)
          :overflow-min (max 0.0 (- needed
                                    (/ (org-foresight--intervals-seconds run-out)
                                       60.0)))
          :off-min (- (/ (float-time (time-subtract (cdr awake) (car awake)))
                         60.0)
                      span)
          :off-ahead-min (/ (org-foresight--intervals-seconds off-left) 60.0)
          :off-behind-min (/ (org-foresight--intervals-seconds
                              (org-foresight--intervals-elapsed off-ivs now))
                             60.0)
          :private-min private-out
          :borrowed-min borrowed
          :unclaimed-min grey
          :grey-min grey)))

(defun org-foresight--pour (intervals minutes)
  "Return the time at which MINUTES of work poured into INTERVALS runs out.

Nothing to pour runs out immediately -- at the start of the first stretch
there is, which is when you would already be finished.  Nil is kept for the
one thing it should mean: that it does not fit at all."
  (let ((left (* 60.0 minutes))
        (result nil))
    (catch 'done
      (when (<= left 0)
        (throw 'done (setq result (car (car intervals)))))
      (dolist (iv intervals)
        (let ((span (float-time (time-subtract (cdr iv) (car iv)))))
          (if (<= left span)
              (throw 'done (setq result (time-add (car iv) left)))
            (setq left (- left span))))))
    result))

(provide 'org-foresight-core)

;;; org-foresight-core.el ends here
