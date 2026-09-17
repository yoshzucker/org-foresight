;;; org-foresight-plan.el --- Signals and placement  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yoshzucker

;; Author: yoshzucker
;; URL: https://github.com/yoshzucker/org-foresight

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The only part of org-foresight that writes back to Org, and the only part
;; that proposes rather than reports.
;;
;; Signals -- work that exists but is not yet in the plan:
;;
;;   meetings      an accepted invitation implies preparation and follow-up
;;                 time that nobody has budgeted for
;;   procrastination  repeated reschedules, already recorded in LOGBOOK by
;;                 `org-log-reschedule', are a measurement nobody reads
;;   unplannable   a near DEADLINE with no EFFORT cannot be placed, so it
;;                 silently drops out of every plan
;;   follow-ups    handed-off work whose check-in date has passed
;;   orphans       preparation for a meeting that no longer exists
;;
;; Two rules govern every write from this file:
;;
;;   1. Nothing is written without confirmation.  Proposals are shown first and
;;      applied only on an explicit command, so a bad estimate costs a
;;      keystroke rather than a corrupted agenda.
;;   2. Writes must not corrupt the measurements the signals depend on.  In
;;      particular `org-log-reschedule' is bound to nil while placing, or the
;;      procrastination signal would end up counting this package's own writes.

;;; Code:

(require 'org-foresight-core)
(require 'org-foresight-report)
;; The board is a page of rows carrying entries, which is what
;; org-foresight-agenda.el is about; it names its own rows the way a drawn
;; agenda names its own.  Not circular: that file knows about core and the
;; report, and nothing about this one.
(require 'org-foresight-agenda)
(require 'org-agenda)
(require 'org-datetree)
(require 'org-id)
(require 'seq)
(require 'cl-lib)

;;;; Options

(defcustom org-foresight-procrastination-threshold 3
  "How many reschedules before a task is called out.
Moving a task once is planning; moving it repeatedly is a decision that is
not being made, and that is what this number is meant to catch."
  :type 'integer
  :group 'org-foresight)

(defcustom org-foresight-followup-keywords '("WAIT")
  "TODO keywords whose SCHEDULED date is a check-in, not a start date.
An entry in one of these states whose date has passed is someone else's work
that has gone quiet."
  :type '(repeat string)
  :group 'org-foresight)

(defcustom org-foresight-meeting-categories nil
  "CATEGORY values whose events imply preparation and follow-up work.
Typically the category an imported work calendar is tagged with.  Nil
disables the meeting signal, since without it every birthday reminder would
be treated as a commitment."
  :type '(repeat string)
  :group 'org-foresight)

(defcustom org-foresight-meeting-prep "0:30"
  "Effort assumed for preparing for a meeting."
  :type 'string
  :group 'org-foresight)

(defcustom org-foresight-meeting-follow "0:15"
  "Effort assumed for writing up and following through after a meeting."
  :type 'string
  :group 'org-foresight)

(defcustom org-foresight-wip-keywords nil
  "TODO keywords meaning work is actually underway.
Nil disables the signal.  Set this to the keyword used for started work."
  :type '(repeat string)
  :group 'org-foresight)

(defcustom org-foresight-wip-limit 2
  "How many things may be in flight before it is worth saying so.
Every extra piece of started work costs the switch back into it, so a rising
count is a slowing day even when each item looks reasonable."
  :type 'integer
  :group 'org-foresight)

(defcustom org-foresight-leak-warn 90
  "Minutes a day the clock cannot account for before that is the problem.

Both halves together: time at the keyboard with no clock running, and time
away from the desk that nothing on the calendar explains.  They are recalled
differently but they cost the same, and the reserve is built from their sum
-- watching only the first leaves the larger one of the two unwatched on
exactly the days it matters.

Left alone it grows, and every future day is planned with that much less in
it, so past a point the answer is not a bigger allowance but a look at where
the hour goes."
  :type 'integer
  :group 'org-foresight)

(defcustom org-foresight-borrow-warn 180
  "Minutes of work taken from private time in a week before it is flagged."
  :type 'integer
  :group 'org-foresight)

(defconst org-foresight--borrow-days 7
  "How many days the borrowing signal looks over, and so how wide its survey is.")

(defcustom org-foresight-undecided-enabled nil
  "Whether to report captured items that have not been decided about.

Off by default, and deliberately so.  Measured against a real journal the
rule matches 17% of all headings -- date-tree scaffolding, ordinary diary
entries, notes that were never meant to become anything.  A board that
mostly lists things which are not problems stops being read, which costs
more than the signal is worth.  Turn it on only where captures live in a
file of their own."
  :type 'boolean
  :group 'org-foresight)

(defcustom org-foresight-undecided-files nil
  "Files whose entries the undecided signal considers, or nil for all of them."
  :type '(repeat file)
  :group 'org-foresight)

;;;; Signals

(defun org-foresight--log-prefix (kind)
  "Return the literal text Org writes at the head of a KIND log line.
Derived from `org-log-note-headings' rather than hardcoded, so a user who has
reworded their log entries still gets counted correctly."
  (let ((h (cdr (assq kind org-log-note-headings))))
    (when (and h (string-match "\\`\\([^%]+\\)" h))
      (match-string 1 h))))

(defun org-foresight--reschedule-count ()
  "Return how many times the entry at point has been rescheduled.
Reads what `org-log-reschedule' has already been recording, so this costs
nothing to start measuring -- the history is there the moment it is asked
for.  Returns 0 when reschedule logging is off."
  (let ((prefix (org-foresight--log-prefix 'reschedule)))
    (if (null prefix)
        0
      (let ((text (org-foresight--entry-text))
            (re (regexp-quote prefix))
            (pos 0) (n 0))
        (while (string-match re text pos)
          (setq pos (match-end 0) n (1+ n)))
        n))))

(defun org-foresight--entry-has-future-time-p (stamps now)
  "Non-nil when STAMPS contains a timed stamp at or after NOW."
  (seq-some (lambda (el)
              (and (org-foresight--ts-timed-p el)
                   (not (time-less-p (org-foresight--ts-start el) now))))
            stamps))

(defun org-foresight--outside-work-p (occ)
  "Non-nil when the interval OCC does not fit inside any of its day's work hours.

Any one of them: a meeting that starts before the lunch break and ends after
it is outside the working hours even though both ends of it are inside them,
because the middle is time that was declared not to be work."
  (not (org-foresight--within-p (car occ) (cdr occ)
                                (org-foresight-work-intervals (car occ)))))

(defun org-foresight--outside-work-hours (stamps now horizon)
  "Return the first occurrence in STAMPS that lands outside working hours.

Work parked in the lunch break, or at 19:00, is invisible to capacity -- the
working hours do not cover it, so it is subtracted from nothing and warns
about nothing.  That is precisely the work that stops the day ending on time,
so it is worth saying out loud rather than quietly excluding.

Not \"after hours\": that means after the close of business, and a day that
breaks in the middle has work escaping into hours it is nowhere near the end
of."
  (catch 'found
    (dolist (el stamps)
      (when (org-foresight--ts-timed-p el)
        (dolist (occ (org-foresight--ts-occurrences el now horizon))
          (when (org-foresight--outside-work-p occ)
            (throw 'found occ)))))
    nil))

(defun org-foresight--here-sort (rows)
  "Return ROWS ordered by how soon each is needed.

Deadlines first and earliest first; everything else after, in the order the
files gave it.  Sorted rather than filtered by deadline: a file that does not
use deadlines would show nothing at all under a filter, and the question
\"what can only be done here\" is worth answering whether or not anybody has
written a date on it."
  (let ((dated (seq-filter (lambda (r) (plist-get r :deadline)) rows))
        (undated (seq-remove (lambda (r) (plist-get r :deadline)) rows)))
    (append (sort dated (lambda (a b) (time-less-p (plist-get a :deadline)
                                                   (plist-get b :deadline))))
            undated)))

(defun org-foresight--finding (title note)
  "Build a finding for the entry at point, described by TITLE and NOTE."
  (list :file (buffer-file-name)
        :point (point)
        :marker (point-marker)
        :title title
        :note note))

(defun org-foresight--finding-at (marker title note)
  "Build a finding for the entry at MARKER, described by TITLE and NOTE.
`org-foresight--finding\=' reads the entry the walk is standing on; this one
is for findings decided after the walk, from a record it left behind."
  (list :file (buffer-file-name (marker-buffer marker))
        :point (marker-position marker)
        :marker marker
        :title title
        :note note))

(defun org-foresight--stalled-findings (headings)
  "Return findings for open projects in HEADINGS with no live step under them.

The one thing a review of a project list is for.  A project keeps its name
after its last step is finished -- `:project-p\=' says it has a TODO child,
and a finished child is still one -- so a plan does not announce that it has
run out; it simply stops having anything in it, and goes on looking like work
in hand."
  (mapcar (lambda (p)
            (let ((rec (plist-get p :record)))
              (org-foresight--finding-at
               (plist-get rec :marker) (plist-get rec :title)
               "nothing live under it")))
          (seq-filter (lambda (p) (null (plist-get p :next)))
                      (org-foresight-projects headings))))

(defvar org-foresight-signals-cache-ttl 3
  "Seconds a computed signal set is reused before the files are walked again.
Short enough that an edit shows on the next refresh, long enough that one
agenda render -- which asks twice, once for the summary line and once for the
board -- only pays for the walk once.")

(defvar org-foresight--signals-cache nil
  "Plist (:time T :files F :signals S) memoizing `org-foresight-signals'.")

(defun org-foresight-signals (&optional force scan)
  "Return an alist (LABEL . FINDINGS) of work that exists but is not planned.
Cached for `org-foresight-signals-cache-ttl' seconds unless FORCE.  SCAN, a
survey wide enough to cover a week, is used instead of taking one where the
caller already has it.

The cache is keyed on the agenda file list as well as the clock.  Time alone
would be wrong: switching which files are in play -- demo data in or out, a
narrowed set for one command -- changes the answer completely, and a stale
board describing the other corpus is worse than a slow one."
  (let ((files (org-agenda-files)))
    (if (and (not force)
             org-foresight--signals-cache
             (equal files (plist-get org-foresight--signals-cache :files))
             (< (float-time
                 (time-subtract (current-time)
                                (plist-get org-foresight--signals-cache :time)))
                org-foresight-signals-cache-ttl))
        (plist-get org-foresight--signals-cache :signals)
      (let ((result (org-foresight--signals-compute scan)))
        (setq org-foresight--signals-cache
              (append (list :time (current-time) :files files) result))
        (plist-get result :signals)))))

(defun org-foresight-here (&optional force)
  "Return the work that only where you are today can do, nearest need first.

Read off the same walk as `org-foresight-signals\=' and cached with it: they
are two questions about one pass over the files, and asking both should cost
what asking one costs."
  (let ((files (org-agenda-files)))
    (unless (and (not force)
                 org-foresight--signals-cache
                 (equal files (plist-get org-foresight--signals-cache :files))
                 (< (float-time
                     (time-subtract (current-time)
                                    (plist-get org-foresight--signals-cache :time)))
                    org-foresight-signals-cache-ttl))
      (org-foresight-signals force))
    (plist-get org-foresight--signals-cache :here)))

(defun org-foresight-outline-records (&optional force)
  "Return a record per TODO heading in `org-agenda-files\=', in document order.

Structure, not amounts: what each heading is, and which TODO heading it
answers to.  See `org-foresight--project-record\=' for a record, and
`org-foresight-project-scan\=' for what is made of them.

Read off the same walk as `org-foresight-signals\=' and cached with it, for
the reason `org-foresight-here\=' is: they are different questions about one
pass over the files, and a redraw that asked them separately walked every
heading in every file twice.  On a real journal that second walk was a
quarter of the redraw and it learned nothing the first had not seen.

Kept apart from the survey in `org-foresight-scan\=' all the same.  That one
answers what the days hold and is taken over a window; this one answers what
shape the work is in and has no window at all."
  (let ((files (org-agenda-files)))
    (unless (and (not force)
                 org-foresight--signals-cache
                 (equal files (plist-get org-foresight--signals-cache :files))
                 (< (float-time
                     (time-subtract (current-time)
                                    (plist-get org-foresight--signals-cache :time)))
                    org-foresight-signals-cache-ttl))
      (org-foresight-signals force))
    (plist-get org-foresight--signals-cache :headings)))

(defvar org-foresight-signal-functions nil
  "Functions contributing signals beyond the ones this file finds.

Each is called with the survey of the horizon -- which may be nil, and which
it is free to ignore -- and returns a list of (LABEL . FINDINGS), the same
shape the built-in signals have.  A finding is what
`org-foresight--finding\=' makes.

Their kinds come from `org-foresight-signal-kinds\=', which a contributor
adds to as well; a label registered nowhere is treated as `fix\='.

This is here so that a question this package has no business answering can
still be answered in the same place.  What is unsettled about a week is not
only a matter of whether it fits, and a file loaded later should not have to
edit this one to say so.  One that signals is dropped with a complaint rather
than taking the board down with it.")

(defun org-foresight--contributed-signals (scan)
  "Return the signals `org-foresight-signal-functions\=' finds in SCAN."
  (mapcan
   (lambda (fn)
     (copy-sequence
      (condition-case err (funcall fn scan)
        (error
         (message "org-foresight: signal source %s failed: %s"
                  fn (error-message-string err))
         nil))))
   org-foresight-signal-functions))

(defun org-foresight--signals-compute (&optional scan)
  "Walk the agenda files and return the signals.

One pass, in the same spirit as the other scans here: every signal is a
different reading of the same walk, so asking for all of them costs no more
than asking for one."
  (let* ((now (current-time))
         (today (org-foresight--day-start 0))
         (horizon (time-add today (days-to-time org-foresight-horizon-days)))
         (uids (make-hash-table :test 'equal))
         ;; The week, not the day: the borrowing signal asks about seven of
         ;; them and everything else asks about today, and one survey answers
         ;; both.  A survey of a week costs what a survey of a day costs --
         ;; the walk is the price, and the days are only how many buckets it
         ;; sorts the answers into.
         ;; The redraw\='s own survey when the caller brought none.  A redraw
         ;; hands one in from the report; the board and the outline records
         ;; arrive here without one, and taking a second survey of the same
         ;; files for them would give back exactly what sharing this walk was
         ;; worth.  It reaches further than the week wanted here, which costs
         ;; nothing: every reading below asks for the day it means.
         (scan (or scan (org-foresight-redraw-scan)))
         (places (org-foresight-day-places
                  today (org-foresight-day-blocks today scan)))
         here elsewhere records
         meetings procrastinated unplannable followups outside-work
         orphan-candidates undecided in-flight unreadable untimed-travel)
    (dolist (file (org-agenda-files))
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (org-with-wide-buffer
           ;; The TODO-keyworded headings still open above the point, deepest
           ;; first.  Per file: containment never crosses one.
           (let (stack)
           (org-map-entries
            (lambda ()
              (let* ((todo (org-get-todo-state))
                     (done (org-entry-is-done-p))
                     (title (org-foresight--entry-title))
                     (effort (org-entry-get (point) "EFFORT"))
                     (sched (org-get-scheduled-time (point)))
                     (dead (org-get-deadline-time (point)))
                     (cat (org-entry-get (point) "CATEGORY" t))
                     (stamps (unless done (org-foresight--entry-timestamps))))
                ;; The shape of the outline, read off the same visit as the
                ;; signals below -- two questions about one heading, where
                ;; the walk is the expensive part.
                ;;
                ;; The rule the stack encodes has two halves and one line
                ;; does both: the stack is popped by level for *every*
                ;; heading, and only TODO-keyworded headings are ever pushed.
                ;; Popping unconditionally is what closes the subtree a
                ;; keyword-less heading ends -- without it the next heading
                ;; would find a stale ancestor from a sibling tree on top and
                ;; adopt it, wrongly and silently.  Never pushing it is what
                ;; makes it transparent, so a TODO grandchild under a
                ;; keyword-less child still finds its TODO grandparent.  A
                ;; grouping heading is a hole in the outline for the purpose
                ;; of asking who owns what, and a wall for the purpose of
                ;; asking where a subtree ends.
                (let ((level (org-current-level)))
                  (while (and stack (>= (car (car stack)) level))
                    (pop stack))
                  (when-let ((rec (org-foresight--project-record
                                   (list :todo todo :done done
                                         :title title :category cat))))
                    (when-let ((parent (cdr (car stack))))
                      (plist-put parent :has-todo-child t)
                      (when-let ((d (plist-get rec :deadline)))
                        (plist-put parent :child-deadlines
                                   (cons d (plist-get parent :child-deadlines))))
                      (plist-put rec :todo-parent parent))
                    (push (cons level rec) stack)
                    (push rec records)))
                (when-let ((uid (org-entry-get (point) "UID")))
                  (puthash uid t uids))
                ;; (a) A meeting nobody has budgeted around.
                (when (and (not done)
                           org-foresight-meeting-categories
                           (member cat org-foresight-meeting-categories)
                           (null (org-entry-get (point) org-foresight-prep-property))
                           (org-foresight--entry-has-future-time-p stamps now))
                  (push (org-foresight--finding
                         title
                         (format "needs %s + %s"
                                 org-foresight-meeting-prep
                                 org-foresight-meeting-follow))
                        meetings))
                ;; A journey written down that no day can place.  Between
                ;; them a stamp and a clock are the only two ways an entry
                ;; says when it happened; with neither, the derivation goes
                ;; on believing you never left the house and draws the next
                ;; leg from there.  It used to do that in silence, which is
                ;; the part worth fixing -- a rule nobody can see broken is
                ;; a rule nobody can follow.
                (when (and (not done)
                           org-foresight-travel-property
                           (org-entry-get (point) org-foresight-travel-property)
                           (not (seq-find #'org-foresight--ts-timed-p stamps))
                           (not (org-foresight--entry-clocked-p)))
                  (push (org-foresight--finding
                         title "no time and no clock, so no day can place it")
                        untimed-travel))
                ;; (f) Work already parked outside the hours being defended.
                ;; Excluded: private commitments, because dinner at seven is
                ;; not work that escaped the day; and anything belonging to
                ;; somebody else, because a child's fixture is not overtime.
                ;; A board that says otherwise is telling its reader off for
                ;; having a life.
                (when-let ((occ (and stamps
                                     (not (member cat
                                                  org-foresight-private-categories))
                                     (eq (org-foresight--entry-attention cat)
                                         'blocking)
                                     (org-foresight--outside-work-hours
                                      stamps now horizon))))
                  (push (org-foresight--finding
                         title
                         ;; The group heading already says these are outside
                         ;; the working hours; the note only has to say when.
                         (format "%s %s–%s"
                                 (format-time-string "%a %m-%d" (car occ))
                                 (format-time-string "%H:%M" (car occ))
                                 (format-time-string "%H:%M" (cdr occ))))
                        outside-work))
                ;; (g) An estimate Org itself cannot read.  This one is
                ;; not about the plan at all -- it is about the tool: the
                ;; agenda calls `org-duration-to-minutes' on every EFFORT it
                ;; is asked to display, and that function signals rather than
                ;; shrugging.  So a single "2h" or "soon" takes down the
                ;; whole of `C-c a a' with a message naming neither the file
                ;; nor the heading.  This board is built by walking the files
                ;; and so still works when the agenda does not, which makes it
                ;; the one place that can say which heading to go and fix.
                (when (and effort (null (org-foresight--duration-minutes effort)))
                  (push (org-foresight--finding
                         title (format "%S is not a duration Org can read"
                                       effort))
                        unreadable))
                ;; (h) Work the place decides.  Most work goes anywhere, so
                ;; what lands here is the little that does not: a thing to
                ;; pick up, a conversation that would go wrong in writing.
                ;; Being here is the scarce part, and the question at the door
                ;; is what only being here can settle.
                (when (and todo (not done))
                  (let ((place (org-foresight--entry-place)))
                    (cond
                     ;; Every place the day is at, not the one it is worked
                     ;; from.  A day based at home with an appointment at the
                     ;; office is a day the office errands can be run on, and
                     ;; asking only for the base both hid them here and called
                     ;; them impossible below -- one mistake, read twice.
                     ((and place (memq place places))
                      (push (list :title title :marker (point-marker)
                                  :place place :deadline dead
                                  :people (org-foresight--entry-people))
                            here))
                     ;; The mirror image: work put on today that today cannot
                     ;; do.  A home day with an office errand on it is a plan
                     ;; that will not survive contact with the morning.
                     ((and place sched
                           (= 0 (org-foresight--day-of sched today)))
                      (push (org-foresight--finding
                             title
                             ;; Where today is actually being worked from is
                             ;; the same answer on every row of this
                             ;; section, and the section it is read under
                             ;; has already given it once.  Repeating it
                             ;; here cost more columns than the note has --
                             ;; three places and the place needed was cut
                             ;; off the end of the line.
                             (format "needs %s · not there today"
                                     (truncate-string-to-width
                                      (format "%s" place) 13 nil nil t)))
                            elsewhere)))))
                ;; (b) A decision that keeps not being made.
                (when (and todo (not done))
                  (let ((n (org-foresight--reschedule-count)))
                    (when (>= n org-foresight-procrastination-threshold)
                      (push (org-foresight--finding
                             title (format "rescheduled %d times" n))
                            procrastinated))))
                ;; (c) A deadline that cannot be planned for.
                ;;
                ;; Leaves only.  A project heading carries no EFFORT because
                ;; its estimate is its children's -- asking one for its own
                ;; would be asking for the same hours twice -- so a signal
                ;; that read a missing EFFORT there fired on every properly
                ;; decomposed tree in the file.  Naming correct work as a
                ;; problem is how a board teaches people to stop reading it.
                (when (and todo (not done) dead (null effort)
                           (time-less-p dead horizon)
                           (not (org-foresight-project-p)))
                  (push (org-foresight--finding
                         title
                         (format "due %s, no estimate"
                                 (format-time-string "%m-%d" dead)))
                        unplannable))
                ;; (d) Work with someone else that has gone quiet.
                (when (and todo
                           (member todo org-foresight-followup-keywords)
                           sched (time-less-p sched today))
                  (push (org-foresight--finding
                         title
                         (format "check-in was %s"
                                 (format-time-string "%m-%d" sched)))
                        followups))
                ;; (e) Prep for something that may have been cancelled.
                (when-let ((ref (org-entry-get
                                 (point) org-foresight-meeting-uid-property)))
                  (unless done
                    (push (cons ref (org-foresight--finding
                                     title "meeting no longer in the calendar"))
                          orphan-candidates)))
                ;; (f) Work that has been started but not finished.
                (when (and todo (member todo org-foresight-wip-keywords))
                  (push (org-foresight--finding title "in flight") in-flight))
                ;; (g) Something captured that was never decided about.
                (when (and org-foresight-undecided-enabled
                           (org-foresight--undecided-p todo stamps))
                  (push (org-foresight--finding title "captured, not decided")
                        undecided))))
            nil nil))))))
    ;; Orphans can only be judged once every UID in the agenda has been seen.
    (let ((orphans (seq-keep (lambda (c)
                               (unless (gethash (car c) uids) (cdr c)))
                             orphan-candidates))
          (fit (org-foresight--fit-findings scan)))
      (let ((headings (org-foresight--project-classify
                       ;; Document order, which the level stack above depended
                       ;; on and `org-foresight--project-classify\=' depends on
                       ;; in turn.  Classified here because a project is known
                       ;; from its children, so nothing before the end of the
                       ;; walk could have said which headings are projects.
                       (nreverse records))))
       (list
       :here (org-foresight--here-sort (nreverse here))
       :headings headings
       ;; Kept beside the rows it decided, so the section that shows them can
       ;; head itself without a second scan of every file for an answer this
       ;; pass already had.
       :places places
       :signals
       (seq-filter
       #'cdr
       (append
        (list (cons "Impossible (travel clashes with a meeting)"
                   (org-foresight--clash-findings scan))
             (cons "Meetings without prep" (nreverse meetings))
             (cons "Journey that cannot be placed"
                   (nreverse untimed-travel))
             (cons "Unreadable estimate (breaks the agenda itself)"
                   (nreverse unreadable))
             (cons "Outside work hours (invisible to capacity)"
                   (nreverse outside-work))
             (cons "Won't fit today" (plist-get fit :today))
             (cons "Too big for one sitting (needs breaking up)"
                   (plist-get fit :oversized))
             (cons "Unplannable (deadline, no estimate)" (nreverse unplannable))
             (cons "Gone quiet (follow-up overdue)" (nreverse followups))
             (cons "Kept moving (not really NEXT)" (nreverse procrastinated))
             (cons "Too much in flight"
                   (if (> (length in-flight) org-foresight-wip-limit)
                       (nreverse in-flight)
                     nil))
             (cons "Borrowed from private time" (org-foresight--borrow-findings scan))
             (cons "Leaking (unclocked work)" (org-foresight--leak-findings))
             (cons "Cannot be done from here" (nreverse elsewhere))
             (cons "Undecided (captured, not decided)" (nreverse undecided))
             (cons "Nothing to do next"
                   (org-foresight--stalled-findings headings))
             (cons "Orphaned prep" orphans))
        ;; And whatever else has been contributed.  After the ones found
        ;; here, and it makes no difference where: the groups are drawn in
        ;; the order of their kinds, not of who found what.
        (org-foresight--contributed-signals scan))))))))

(defun org-foresight--undecided-p (todo stamps)
  "Non-nil when the entry at point was captured but never decided about.

Deliberately narrow.  Anything with a state, a date, a clock, a child or a
timestamp in its own heading is already being handled, and a date-tree
heading is scaffolding rather than a thought.  What is left is a heading
someone wrote down and walked away from."
  (and (null todo)
       (null stamps)
       (not (save-excursion (org-goto-first-child)))
       ;; An inactive stamp in the heading is a note about when, which is a
       ;; decision of a kind.  The active ones STAMPS already carries.
       (not (string-match-p org-ts-regexp-inactive
                            (org-get-heading t t nil nil)))
       ;; `*** 2026-08-11 Tuesday' and friends are structure, not capture.
       (not (string-match-p "\\`[0-9]\\{4\\}\\(-[0-9]\\{2\\}\\)\\{0,2\\}\\b"
                            (org-foresight--entry-title)))
       (not (string-match-p "CLOCK:" (org-foresight--entry-text)))
       (or (null org-foresight-undecided-files)
           (member (buffer-file-name) org-foresight-undecided-files))))

(defun org-foresight--clash-findings (scan)
  "Return findings for journeys that overlap something else in SCAN.

Being in two places at once is not a scheduling preference to be weighed
against others -- the day as written cannot happen, and no amount of working
harder at it will help.  Worth saying before anything else on the board."
  (let ((ledger (org-foresight-scan-day scan :ledger
                                        (org-foresight--day-start 0)))
        (seen (make-hash-table :test 'equal))
        out)
    (dolist (tb (seq-filter (lambda (e) (eq (plist-get e :kind) 'travel)) ledger))
      (dolist (other ledger)
        (when (and (not (eq other tb))
                   (memq (plist-get other :kind) '(meeting task))
                   ;; A call you only have to hear can be heard on the way,
                   ;; and somebody else's fixture was never yours to attend.
                   ;; Neither is a day that cannot happen.
                   (eq (or (plist-get other :attention) 'blocking) 'blocking)
                   (plist-get other :start)
                   (time-less-p (plist-get tb :start) (plist-get other :end))
                   (time-less-p (plist-get other :start) (plist-get tb :end)))
          (let ((key (cons (plist-get other :title) (plist-get tb :title))))
            (unless (gethash key seen)
              (puthash key t seen)
              (push (list :file nil :point nil
                          :marker (plist-get other :marker)
                          :title (plist-get other :title)
                          :note (format "clashes with %s at %s"
                                        (plist-get tb :title)
                                        (format-time-string
                                         "%H:%M" (plist-get tb :start))))
                    out))))))
    (nreverse out)))

(defun org-foresight--fit-findings (scan &optional now)
  "Return work promised today that no gap can hold, split by what caused it.

  :today      it fits the working hours as they are declared, and no longer
              fits what is left of today
  :oversized  it does not fit the longest unbroken stretch of working time
              the week offers, so no day will hold it whole

The two wear the same symptom and are different problems.  The first is a
fact about the hour it is read at -- the morning held a run this long and
the afternoon does not -- and it clears itself overnight.  The second is a
fact about the task, as true next week as today; left in the first list it
would sit there every morning forever, and a list whose top entries never
change is one that stops being read.

Reported apart because the answers differ.  Work that ran out of day is
moved.  Work too big to sit down to once is broken up, and until it is, the
estimate on it cannot be checked against anything.

Measured from NOW, the current time by default: a two-hour job does not fit
in a day with ninety minutes left of it, whatever the morning looked like."
  (let* ((today (org-foresight--day-start 0))
         ;; A day with no working hours has no gap to measure against, so
         ;; every piece of work would be called out and the list would say
         ;; nothing.  That the day was not meant for work is one fact about
         ;; the day, and the verdict states it there.
         (work (org-foresight-work-intervals today))
         (free (and work (org-foresight-free-intervals today scan now)))
         (longest (if free
                      (/ (apply #'max
                                (mapcar (lambda (iv)
                                          (float-time (time-subtract (cdr iv)
                                                                     (car iv))))
                                        free))
                         60.0)
                    0.0))
         (sitting (org-foresight--longest-sitting))
         fits oversized)
    (dolist (e (and work (org-foresight-scan-day scan :ledger today)))
      (let ((need (or (plist-get e :effort-adj) (plist-get e :effort))))
        (when (and (eq (plist-get e :kind) 'promised) (> need longest))
          ;; A week with no working hours in it at all bounds nothing, and
          ;; comparing against zero would call every task oversized.
          (let* ((big (and (> sitting 0) (> need sitting)))
                 (f (list :file nil :point nil
                          :marker (plist-get e :marker)
                          :title (plist-get e :title)
                          ;; Two figures and the name of what the second
                          ;; one is, because the note has 36 columns and an
                          ;; estimate can spend fifteen of them on its own
                          ;; (`1d 2:00->1d 8:00\=' is a legal run).  Spelling
                          ;; out "longest gap" cost more than was left, and
                          ;; the figure it introduced was what got cut.
                          :note (format "needs %s · %s %s"
                                        (org-foresight-report--effort-run e)
                                        (if big "sitting" "gap")
                                        (org-duration-from-minutes
                                         (if big sitting longest))))))
            (if big (push f oversized) (push f fits))))))
    (list :today (nreverse fits) :oversized (nreverse oversized))))

(defun org-foresight--borrow-findings (&optional scan)
  "Return a finding when this week has taken too much from private time.

The week ahead, not the week behind.  What is counted is
`:borrowed-min\=', and that figure is what is *left* of an evening already
claimed by work -- measured from now forwards, so a day that has gone
reports nothing whatever was in it.  A loop walking backwards through seven
of those found six zeroes and called the answer today\='s.  The question the
model can actually answer is the useful one anyway: evenings already spoken
for can still be given back.

SCAN must cover `org-foresight--borrow-days\=' days from today.  Without one
each day asked for its own, and a survey is a walk of every entry in every
agenda file: seven of them, for one line that is usually not printed.  On a
slow machine that was four seconds every time the signals fell out of their
few-second cache, and a fifth of a second every other time -- which is what a
redraw that feels unpredictable turns out to be made of."
  (let* ((today (org-foresight--day-start 0))
         (last (time-add today (days-to-time (1- org-foresight--borrow-days))))
         (total 0.0)
         (days 0)
         ;; Forward, and the survey has to reach that far.  A caller may hand
         ;; in one that does not, so it is asked rather than assumed.
         (scan (if (and scan (org-foresight-scan-covers-p scan last))
                   scan
                 (org-foresight-scan org-foresight--borrow-days today))))
    (dotimes (i org-foresight--borrow-days)
      (let* ((day (time-add today (days-to-time i)))
             (cap (ignore-errors (org-foresight-capacity day scan))))
        (when-let ((borrowed (and cap (plist-get cap :borrowed-min))))
          (when (> borrowed 0)
            (setq total (+ total borrowed) days (1+ days))))))
    (when (> total org-foresight-borrow-warn)
      (list (list :file nil :point nil :marker nil
                  :title "Work in private time"
                  :note (format "%s over %d day(s) ahead"
                                (org-duration-from-minutes total) days))))))

(defun org-foresight--leak-findings ()
  "Return a finding when the measured leak has grown past what is tolerable.

Reads only the cached figure -- signals are computed while an agenda is being
drawn, and reaching for the network there would stall the display."
  (let ((leak (org-foresight-leak-minutes))
        (lost (org-foresight-lost-minutes)))
    (when (and (org-foresight-leak-samples)
               (> (+ leak lost) org-foresight-leak-warn))
      (list (list :file nil :point nil :marker nil
                  :title "Time the clock cannot account for"
                  ;; Both figures, because they are answered differently: one
                  ;; is remembered by what was on the screen, the other by
                  ;; where you went.
                  :note (format "%s unrecorded, %s away"
                                (org-duration-from-minutes leak)
                                (org-duration-from-minutes lost)))))))

;;;; The board

(defvar org-foresight-signal-commands
  '(("Meetings without prep" . org-foresight-prepare-meetings))
  "Signal groups a single command can settle, and which command that is.

Most groups are fixed one row at a time, on the entry the row points at: an
estimate is typed where the work is, and a task that keeps moving is a
decision nobody else can make.  The few that are not have to be said out
loud -- a board that names a problem and not the thing that answers it sends
its reader off to find one, and a reader who has to go looking stops reading.

A variable, like `org-foresight-signal-kinds\=', so a group contributed
through `org-foresight-signal-functions\=' can name its own answer.  A
contributor able to say what is wrong but not what settles it would be the
one group on the page sending its reader off to look.")

(defvar org-foresight-signal-summarised nil
  "Signal groups drawn as a heading and a count, without their rows.

For a group whose length is the news and whose members are not.  Fifty rows
nobody reads push the rest of the page off the screen, and a reader learns
to scroll past the section rather than read it -- which costs more than the
rows were worth.

Only worth it where the group also names the command that settles it: a
number with nowhere to go is a reproach.  See
`org-foresight-signal-commands\='.")

(defvar org-foresight-signal-kinds
  '(("Impossible (travel clashes with a meeting)"     . fix)
    ("Meetings without prep"                          . fix)
    ("Unreadable estimate (breaks the agenda itself)" . fix)
    ("Won't fit today"                                . fix)
    ("Too big for one sitting (needs breaking up)"    . fix)
    ("Unplannable (deadline, no estimate)"            . fix)
    ("Cannot be done from here"                       . fix)
    ("Undecided (captured, not decided)"              . fix)
    ("Nothing to do next"                             . fix)
    ("Orphaned prep"                                  . fix)
    ("Gone quiet (follow-up overdue)"                 . owed)
    ("Kept moving (not really NEXT)"                  . owed)
    ("Too much in flight"                             . owed)
    ("Outside work hours (invisible to capacity)"     . fact)
    ("Borrowed from private time"                     . fact)
    ("Leaking (unclocked work)"                       . fact))
  "What kind of thing each signal is, and so whether emptying it is the point.

A variable rather than a constant: a signal contributed through
`org-foresight-signal-functions\=' says what kind it is by adding to this,
and the frame stays here.

One question separates them: can it be settled by editing the plan, without
doing any of the work, and without writing anything untrue?

  fix   Yes.  A clash is rescheduled, an estimate is typed, a lump is broken
        into steps.  Minutes of work at most, and none of it the work
        itself.  This is the only kind with a target, and the target is
        none left.

  owed  No -- only finishing, dropping or handing on the work clears it.  A
        task that keeps moving is not answered by moving it again; that is
        the count going up.  Zero here would mean owing nobody anything,
        which is not a state a working week passes through.

  fact  No, and it is not a fault.  A call with another timezone is at
        seven in the evening because that is when the other end is awake,
        and last week\='s unclocked hours already happened.  Reported so the
        figures elsewhere can be read, and driving them to zero would mean
        refusing the call.

Naming the kinds is what gives the board an answer to \"what does good look
like\", which a flat list of fifteen headings does not have: emptying it is
impossible, so a reader who tries once learns the board cannot be satisfied
and stops reading it.

Anything not named here counts as `fix\=', so a signal added and left out of
this list is over-reported rather than quietly filed away as weather."
  )

(defconst org-foresight--signal-order '(fix owed fact)
  "The kinds of `org-foresight-signal-kinds\=', in the order they are read.

What can be settled now comes first, because it is the part with an end to
it.  What is owed comes next: still the reader\='s, but not answerable at a
keyboard.  What is merely true comes last.")

(defconst org-foresight--signal-banners
  '((owed . "below here, nothing clears without doing the work")
    (fact . "below here, nothing is a fault: these are facts about the week"))
  "The rule drawn where the board stops asking to be emptied.

Nothing above `owed\=', because the top of the list needs no explaining: it is
the part a reader is meant to drive to nothing, and the badge has said so.")

(defun org-foresight-signal-kind (title)
  "Return the kind of the signal group called TITLE.
See `org-foresight-signal-kinds\=' for what the kinds mean."
  (or (cdr (assoc title org-foresight-signal-kinds)) 'fix))

(defun org-foresight-signals-to-fix (&optional signals)
  "Return how many findings in SIGNALS could be settled by editing the plan.

The board\='s one figure, and the only one of the three kinds with a target.
Counts findings rather than groups: it is the number of entries somebody has
to go and touch, and five estimates missing from one group is five pieces of
work, not one."
  (let ((signals (or signals (org-foresight-signals))))
    (seq-reduce (lambda (n group)
                  (if (eq (org-foresight-signal-kind (car group)) 'fix)
                      (+ n (length (cdr group)))
                    n))
                signals 0)))

(defun org-foresight--signals-in-order (signals)
  "Return SIGNALS grouped by kind, in `org-foresight--signal-order\='.
Stable within a kind, so the order each group was written in survives."
  (apply #'append
         (mapcar (lambda (kind)
                   (seq-filter (lambda (g)
                                 (eq (org-foresight-signal-kind (car g)) kind))
                               signals))
                 org-foresight--signal-order)))

(defun org-foresight-report--signal-rule (text)
  "Return a full-width rule introducing TEXT."
  (let* ((lead (format " %s %s " (make-string 2 ?\u2500) text))
         (pad (max 2 (- org-foresight-report-columns (string-width lead)))))
    (propertize (concat lead (make-string pad ?\u2500)) 'face 'shadow)))

(defun org-foresight-report--count-rows (text)
  "Return how many lines of TEXT are rows an agenda command could act on.

Counted from the drawn text rather than from the data behind it.  A block
decides for itself what it draws -- a rule here, a continuation there -- and
a heading that said one number while the reader counted another would be
worse than a heading with no number at all."
  (if (null text)
      0
    (seq-count (lambda (line)
                 (and (> (length line) 0)
                      (get-text-property 0 'org-marker line)))
               (split-string text "\n"))))

(defun org-foresight-report--group-heading (title &optional count command)
  "Return the heading of a group called TITLE, at the margin.

A group heading belongs to the badge above it, so it sits at the margin
rather than at the frame edge: only a badge is outdented, or an eye running
down the left edge stops finding sections.

COUNT is part of the heading because a group is read to decide whether to
read it, and how many there are is most of that decision.  Left out where
the number would say nothing -- a horizon that is always the same length is
not news.  COMMAND, where a group has one that empties it, is named beside
the title: the answer and the question in one line.

Shared by every section that groups its rows -- the signals, the projects,
what has a date.  One badge may hold several of these, and two sections
whose subheadings looked different would read as two kinds of thing."
  (org-foresight-report--indent
   (concat
    (propertize (if count (format "%s (%d)" title count) title)
                'face 'org-agenda-structure)
    (when command
      (propertize (concat " · " (org-foresight-plan--command-hint command))
                  'face 'shadow)))))

(defun org-foresight-report--signal-group (group)
  "Return one signal GROUP: its heading, and a row per finding.

Or the heading alone, where the group is one of
`org-foresight-signal-summarised\=' and its length is the news."
  (if (member (car group) org-foresight-signal-summarised)
      (org-foresight-report--group-heading
       (car group) (length (cdr group))
       (cdr (assoc (car group) org-foresight-signal-commands)))
    (concat
          (org-foresight-report--group-heading
           (car group) (length (cdr group))
           (cdr (assoc (car group) org-foresight-signal-commands)))
          "\n"
          (mapconcat
           (lambda (f)
             ;; 2 + 40 + 2 + note, budgeted so the longest note a signal can
             ;; produce still lands inside 80 columns.  The row carries the
             ;; entry's marker, which is what lets it be fixed from here
             ;; rather than merely reported.
             (org-foresight-report--actionable
              (format "  %s  %s"
                      (truncate-string-to-width
                       (replace-regexp-in-string
                        "[\n\r]" " " (or (plist-get f :title) "?"))
                       40 0 ?\s)
                      (truncate-string-to-width
                       (propertize (plist-get f :note) 'face 'shadow) 36))
              (plist-get f :marker)))
           (cdr group) "\n"))))

(defun org-foresight-report--project-row (rec next)
  "Return the row for project REC, whose first live leaf is NEXT."
  (org-foresight-report--actionable
   (format "  %s  %s  %s"
           (truncate-string-to-width (or (plist-get rec :category) "") 12 0 ?\s)
           (truncate-string-to-width
            (replace-regexp-in-string "[\n\r]" " " (or (plist-get rec :title) "?"))
            28 0 ?\s)
           (if next
               (truncate-string-to-width
                (propertize (concat "\u2192 " (or (plist-get next :title) "?"))
                            'face 'shadow)
                32)
             (propertize "\u26a0 nothing live under it"
                         'face 'org-foresight-report-overcommitted)))
   (plist-get rec :marker)))

(defun org-foresight-report--plain-row (rec)
  "Return the row for REC where there is no next step to name.

The title is cut but not padded: nothing follows it, and padding a last
field leaves every row of the group carrying whitespace to the edge."
  (org-foresight-report--actionable
   (format "  %s  %s"
           (truncate-string-to-width (or (plist-get rec :category) "") 12 0 ?\s)
           (truncate-string-to-width
            (replace-regexp-in-string "[\n\r]" " " (or (plist-get rec :title) "?"))
            50 nil nil t))
   (plist-get rec :marker)))

(defun org-foresight-report--group (title rows)
  "Return a heading for TITLE over ROWS, or nil when there are none.

Nil rather than a note saying the group is empty.  Four groups that each
say \"(none)\" is a section four lines longer for four answers nobody
asked for -- the whole section already says what it is about, and a group
that is not drawn has said everything true about itself."
  (when rows
    (concat (org-foresight-report--group-heading title (length rows))
            "\n"
            (mapconcat #'identity rows "\n"))))

(defun org-foresight-report-projects (&optional records)
  "Return what is moving in RECORDS, and what is not, in four groups.

The list a weekly review is mostly made of, and the order is the argument.
What is moving is read first and quickest.  What has run out is the row
worth the visit: a project whose leaves are all finished still calls itself
a project, and that is the state a plan falls into without anybody deciding
to let it.  What hangs under no project is the task that was captured and
never filed, which no other section of this page can show -- the projects
are walked from the top down, so a leaf with nothing above it is reached by
neither pass.  What is parked is last, and is not a fault: the question is
whether it is still rightly down, and that is a question for a review and
no other day."
  (let* ((records (or records (org-foresight-outline-records)))
         (projects (org-foresight-projects records))
         (moving (seq-filter (lambda (p) (plist-get p :next)) projects))
         (drained (seq-remove (lambda (p) (plist-get p :next)) projects))
         (loose (org-foresight-loose-leaves records))
         (parked (seq-filter (lambda (r) (plist-get r :parked)) records))
         (groups
          (delq nil
                (list
                 (org-foresight-report--group
                  "moving"
                  (mapcar (lambda (p)
                            (org-foresight-report--project-row
                             (plist-get p :record) (plist-get p :next)))
                          moving))
                 (org-foresight-report--group
                  "nothing live under it"
                  (mapcar (lambda (p)
                            (org-foresight-report--project-row
                             (plist-get p :record) nil))
                          drained))
                 (org-foresight-report--group
                  "under no project"
                  (mapcar #'org-foresight-report--plain-row loose))
                 (org-foresight-report--group
                  "parked"
                  (mapcar #'org-foresight-report--plain-row parked))))))
    (if (null groups)
        (org-foresight-report--indent
         (propertize "(nothing open)" 'face 'shadow))
      (string-join groups "\n\n"))))

(defun org-foresight-report-signals (&optional signals)
  "Return the signal blocks, or a note when nothing is outstanding.

Grouped by kind rather than in the order the walk happened to find them, so
the part with an end to it comes first and a rule says where that part
stops.  See `org-foresight-signal-kinds\=' for why a flat list cannot be
read: three different things wearing one heading, only one of which anybody
is meant to empty."
  (let ((signals (org-foresight--signals-in-order
                  (or signals (org-foresight-signals)))))
    (if (null signals)
        (propertize "(nothing unaccounted for)" 'face 'org-table)
      (let (out (prev nil))
        (dolist (group signals)
          (let ((kind (org-foresight-signal-kind (car group))))
            (unless (eq kind prev)
              (when-let ((banner (cdr (assq kind org-foresight--signal-banners))))
                (push (org-foresight-report--signal-rule banner) out))
              (setq prev kind)))
          (push (org-foresight-report--signal-group group) out))
        (string-join (nreverse out) "\n\n")))))

(defconst org-foresight-here-urgent "⚠"
  "The mark for work whose need falls before you are next in this place.")

(defun org-foresight--places-phrase (places)
  "Return PLACES as a phrase: \"home\", or \"home, office\".

Commas rather than an \"and\", because this goes into a note with 36 columns
to live in.  A list that grows by two characters a place still says something
when it is cut off; one that saves its last word for the end loses the word
that mattered."
  (string-join (mapcar (lambda (p) (format "%s" p)) places) ", "))

(defun org-foresight-report-here (&optional rows day)
  "Return the work only where DAY goes can do, and when it goes there again.

The heading is the whole point of the section: \"next at the office on
Wednesday\" is what turns a list of errands into a decision, because it says
what the alternative to doing it now actually costs.

Where the day goes, not where it is based.  A day worked from home with an
appointment at the office is a day the office errands can be run on, and it is
the day to be told so -- by tomorrow the answer is Wednesday again."
  (let* ((day (or day (org-foresight--day-start 0)))
         (rows (or rows (org-foresight-here)))
         (base (org-foresight-day-place day))
         ;; From the same pass that produced ROWS, where there was one: the
         ;; question was already answered there, and answering it again costs
         ;; another walk of every agenda file.
         (places (or (plist-get org-foresight--signals-cache :places)
                     (org-foresight-day-places day)))
         (next (org-foresight-next-day-at base day)))
    (if (null rows)
        (propertize (format "(nothing that only %s can do)"
                            (org-foresight--places-phrase places))
                    'face 'org-table)
      (concat
       (org-foresight-report--indent
        (propertize
         (if next
             (format "%s · next %s day is %s"
                     (org-foresight--places-phrase places) base
                     (format-time-string "%a %m-%d" next))
           (format "%s · not %s again within the horizon"
                   (org-foresight--places-phrase places) base))
         'face 'org-agenda-structure))
       "\n"
       (mapconcat
        (lambda (r)
          (let* ((dead (plist-get r :deadline))
                 ;; Urgent means the deadline lands before you are next here:
                 ;; the place, not the clock, is what runs out.  Asked of the
                 ;; row's own place, not of the day's: on a day that visits
                 ;; somewhere the list holds work for both, and "next home
                 ;; day" says nothing at all about an office errand.
                 (again (org-foresight-next-day-at (plist-get r :place) day))
                 (urgent (and dead (or (null again) (time-less-p dead again))))
                 (people (plist-get r :people)))
            (org-foresight-report--actionable
             (format "  %s %s  %s"
                     (if urgent
                         (propertize org-foresight-here-urgent
                                     'face 'org-foresight-report-overcommitted)
                       " ")
                     (truncate-string-to-width
                      (replace-regexp-in-string "[\n\r]" " "
                                                (or (plist-get r :title) "?"))
                      40 0 ?\s)
                     (propertize
                      ;; The place only where the day has more than one: with
                      ;; one it is the heading, repeated on every row.
                      (string-join
                       (delq nil
                             (list
                              (when (cdr places)
                                (format "@%s" (plist-get r :place)))
                              (when dead
                                (format "due %s"
                                        (format-time-string "%a %m-%d" dead)))
                              (when people
                                (format "(%s)" (string-join people ", ")))))
                       " ")
                      'face 'shadow))
             (plist-get r :marker))))
        rows "\n")))))

(add-hook 'org-foresight-report-invalidate-functions
          #'org-foresight--invalidate-signals)

(defun org-foresight-plan--board-verdict (landing signals)
  "Return what good would look like on this board, and how far off it is.

The board had no such line, and without one it could not be finished.  The
day\='s views each say plainly when they are satisfied -- capacity when
nothing is over, the grid when the work lands before the evening -- and a
reader who has those turns to a list of fifteen headings and reasonably
asks what emptying it would mean.  Emptying it means nothing, because two
thirds of it is not the reader\='s fault to begin with: see
`org-foresight-signal-kinds\='.

So the line carries the two halves that do have an answer.  Every dated
commitment lands, and there is nothing left to fix.  Both are reachable on
an ordinary Tuesday, which is what makes them worth printing."
  (let* ((entries (plist-get landing :deadlines))
         (short (seq-count (lambda (e) (not (eq (plist-get e :verdict) 'lands)))
                           entries))
         (n (org-foresight-signals-to-fix signals))
         ;; Settled reads as plainly as the rest of the page; unsettled is
         ;; the only thing here worth a colour, and it wears the same one
         ;; the day\='s own figures use when they will not fit.
         (say (lambda (ok text)
                (propertize text 'face
                            (if ok 'shadow
                              'org-foresight-report-overcommitted)))))
    (string-join
     (delq nil
           (list
            (cond ((null entries) nil)
                  ((zerop short)
                   (funcall say t (format "all %d deadlines land"
                                          (length entries))))
                  (t (funcall say nil (format "%d of %d deadlines short"
                                              short (length entries)))))
            (if (zerop n)
                (funcall say t "nothing to fix")
              (funcall say nil (format "%d to fix" n)))))
     (propertize " · " 'face 'shadow))))

;;;###autoload
(defun org-foresight-board (&optional _match)
  "Show what has not been settled: what only here can do, and what is unplanned.

Not an agenda view.  The day has one of those and it is the day; this is the
other question, and it is not about the timeline at all -- which is why it
stopped being a second copy of the agenda with a different tail underneath.

Three sections, in the order the questions get asked.  The first decides
whether you can walk out: work this place, and only this place, can do, with
the day that place comes round again.  The second is everything with a date
on it, and where the week stops holding it -- the list the day\='s one-line
verdict is a summary of.  The third is everything that exists and has not
been planned for at all, which is the longest and the least urgent: a
deadline that will be missed outranks work nobody has looked at yet.

Every row carries its entry\'s marker, so \\[org-agenda-schedule] and the rest
of the agenda\'s vocabulary work here as they do in the agenda itself.

MATCH is taken and ignored, so this can be given to
`org-agenda-custom-commands\=' as the FUNCTION of an entry and reached from
the dispatcher:

    (\"b\" \"Board\" org-foresight-board \"\")

The dispatcher calls its function with the entry\='s match string, and a
command that refused one would need a wrapper in everybody\='s config that did
nothing but drop it."
  (interactive)
  (let ((buffer (get-buffer-create "*Org Foresight Board*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (unless (derived-mode-p 'org-agenda-mode) (org-agenda-mode))
        (setq-local org-agenda-type 'agenda)
        ;; Read once and handed to both the line that summarises them and the
        ;; sections that show them, or the board answers its own question
        ;; twice from two walks of every file.
        ;; Read once and handed to every section that wants them.  The
        ;; survey, the records and the signals are three questions about one
        ;; walk of the files, and a page that asked them section by section
        ;; would walk the journal five times to draw itself.
        (let* ((scan (org-foresight-redraw-scan))
               (landing (org-foresight-landing nil scan))
               (signals (org-foresight-signals nil scan))
               (records (org-foresight-outline-records)))
        (insert
                ;; The verdict opens the page without a badge of its own.
                ;; The badge column is this page's table of contents, and
                ;; what belongs in it is a section about the work; the
                ;; verdict is about the page.  Given one, an eye running
                ;; down the left edge would count five sections and find
                ;; four.
                (org-foresight-report--indent
                 (org-foresight-plan--board-verdict landing signals))
                "\n\n"
                (org-foresight-report--badge
                 "Here" "what only this place can do")
                "\n"
                (org-foresight-report-here)
                "\n\n"
                (org-foresight-report--badge
                 "Fit" "what is promised, and whether the days hold it")
                "\n"
                ;; Two questions about one thing, so one badge over both.
                ;; Whether a date will be met and what a day is shaped like
                ;; are the same arithmetic read from its two ends, and a
                ;; reader who has just been told a date is short is asking
                ;; which day to take the hours out of.
                (let ((rows (org-foresight-report-landing landing scan)))
                  (concat (org-foresight-report--group-heading
                           "dated commitments"
                           (org-foresight-report--count-rows rows))
                          "\n"
                          (org-foresight-report--indent-deeper rows)))
                "\n\n"
                (org-foresight-report--group-heading "the coming days")
                "\n"
                (org-foresight-report--indent-deeper
                 (org-foresight-report-load nil scan nil landing))
                "\n\n"
                (org-foresight-report--badge
                 "Projects" "what is moving, and what is not")
                "\n"
                (org-foresight-report-projects records)
                "\n\n"
                (org-foresight-report--badge
                 "Signals" "everything unsettled, the fixable part first")
                "\n"
                (org-foresight-report-signals signals)
                "\n\n"
                ;; The board is reached by a key somebody bound once and is
                ;; then read for minutes at a time, which is exactly the page
                ;; whose commands have been forgotten by the next visit.
                (or (org-foresight--legend
                     'board '(("RET" . "go to the entry on this row")))
                    "")))
        (put-text-property (point-min) (point-max) 'org-agenda-type 'agenda)
        ;; The board is not drawn through `org-agenda-finalize-hook\=', so it
        ;; names its own rows.  Without this its fifty-odd rows would be the
        ;; ones nothing was watching.
        (org-foresight-agenda--name-rows)
        (goto-char (point-min))
        (setq buffer-read-only t)))
    (pop-to-buffer buffer)))

(define-obsolete-function-alias 'org-foresight-signals-list
  'org-foresight-board "0.2")

(defun org-foresight-plan--command-hint (command)
  "Return how to run COMMAND: the key it is bound to, and its name.

Both, because they say different things.  The key is what the hand needs and
is read from the keymap rather than written down, so it stays true when the
binding changes.  The name is what the sentence needs: a command called
`org-foresight-board\=' tells a reader what pressing the key will get them,
and a bare `B\=' tells them nothing at all.

Where nothing is bound, `substitute-command-keys\=' already answers with
\\[execute-extended-command] and the name, and that answer is used as it
stands.  Where something is, the key goes in front of it rather than the name
into brackets behind: what follows is then the same words in the same order
either way, and only the shortcut has appeared."
  (let ((keys (substitute-command-keys (format "\\[%s]" command))))
    (if (string-prefix-p "M-x " keys)
        keys
      (format "%s M-x %s" keys command))))

(defun org-foresight-plan--verdict-line (&optional scan)
  "Return a one-line summary of what is unsettled, or nil when nothing is.

The daily agenda otherwise gives no hint that anything is outstanding, and a
signal nobody is prompted to look at is not really being caught -- so the
line names the way to look.

Work bound to where you are is counted separately even though the board holds
both.  It is the one kind that stops being possible when you stand up, so a
number that folded it into the rest would be a number that says the same
thing at half past nine and at half past five."
  (let ((n (apply #'+ (mapcar (lambda (g) (length (cdr g)))
                              (org-foresight-signals nil scan))))
        (here (length (org-foresight-here))))
    (when (> (+ n here) 0)
      (concat
       (when (> n 0)
         (format "%d signal%s unplanned" n (if (= n 1) "" "s")))
       (when (and (> n 0) (> here 0)) " · ")
       (when (> here 0)
         (format "%d need%s you here" here (if (= here 1) "s" "")))
       " · "
       (org-foresight-plan--command-hint 'org-foresight-board)))))

(add-to-list 'org-foresight-verdict-extras #'org-foresight-plan--verdict-line)

;;;; Filing new work
;; The only writes org-foresight makes.  They go through one function so there
;; is a single place where the conventions of the target file are known.

(defcustom org-foresight-task-file nil
  "File that generated tasks are written into.
Nil means `org-default-notes-file'."
  :type '(choice (const :tag "org-default-notes-file" nil) file)
  :group 'org-foresight)

(defcustom org-foresight-task-datetree t
  "When non-nil, file generated tasks under a date tree for their own date."
  :type 'boolean
  :group 'org-foresight)

(defcustom org-foresight-task-todo "NEXT"
  "TODO keyword given to generated tasks."
  :type 'string
  :group 'org-foresight)

(defun org-foresight--task-file ()
  "Return the file generated tasks are written into."
  (or org-foresight-task-file
      (and (boundp 'org-default-notes-file) org-default-notes-file)
      (user-error "Set `org-foresight-task-file' first")))

(defun org-foresight--file-open-day (when)
  "Move point in the current buffer to where an entry for WHEN belongs.
Returns the outline level it should be written at.  Past the day\='s existing
entries rather than above them: filing two related things in one go should
leave them in the order they were written, and each one landing on top
reverses that."
  (let ((level 1))
    (when org-foresight-task-datetree
      (org-datetree-find-date-create
       (calendar-gregorian-from-absolute (time-to-days when)))
      (setq level (1+ (org-current-level)))
      (org-end-of-subtree t t))
    (unless org-foresight-task-datetree
      (goto-char (point-max)))
    (unless (bolp) (insert "\n"))
    level))

(defun org-foresight--file-journey (title place from to)
  "File a journey TITLE to PLACE running FROM until TO.

An active range rather than a SCHEDULED stamp: a journey is an hour that
happens, not a task waiting to be started, and the day has to read it as
occupied time."
  (with-current-buffer (find-file-noselect (org-foresight--task-file))
    (org-with-wide-buffer
     (let ((level (org-foresight--file-open-day from)))
       (insert (make-string level ?*) " " title "\n"
               (format-time-string "<%Y-%m-%d %a %H:%M>--" from)
               (format-time-string "<%Y-%m-%d %a %H:%M>\n" to))
       (forward-line -2)
       (org-set-property org-foresight-travel-property (format "%s" place))
       (save-buffer)
       (point-marker)))))

(defun org-foresight--book-travel-read ()
  "Read which of today\='s derived journeys to write down.

Asked only where the cursor is on nothing, as
`org-foresight--clock-switch-read\=' is asked: the day derives the same legs
wherever the reader happens to be standing, and a command that can only be
pressed on one row is a command nobody can press from the buffer they were
reading when the train turned out to be wrong.

Returns what the agenda row carries, so both ways in answer the same shape."
  (let* ((journeys (org-foresight--derived-journeys))
         (choices (mapcar (lambda (j)
                            (cons (format "%s  %s-%s"
                                          (nth 0 j)
                                          (format-time-string "%H:%M" (nth 2 j))
                                          (format-time-string "%H:%M" (nth 3 j)))
                                  (cdr j)))
                          journeys)))
    (unless choices
      (user-error "No journey is derived for today"))
    (cdr (assoc (completing-read "Which journey? "
                                 (org-foresight--ordered choices) nil t)
                choices))))

;;;###autoload
(defun org-foresight-book-travel ()
  "Write down the journey on this agenda row, so it stops being derived.

A derived leg is a claim about the day, and a good one -- until the day
disagrees.  The train you actually catch, an errand on the way, going in early
because the road is quieter: none of that can be guessed from where a meeting
happens to be, and until now the only way to say it was to argue with the
arithmetic.

Writes an ordinary timed entry carrying `org-foresight-travel-property\='.
From then on the derivation defers to it -- the leg is yours, it is booked
time like any other, and nothing invents a second one to the same place.

Its own command rather than `\[org-agenda-schedule]\='.  A derived row
answers to none of Org\='s commands, because there is no entry behind it and
inheriting a neighbour\='s marker would quietly reschedule the wrong thing.
One key meaning \"make this real\" is honest; one key meaning two different
things depending on which row it is pressed on is not."
  (interactive)
  (let ((journey (or (org-get-at-bol 'org-foresight-journey)
                     (org-foresight--book-travel-read))))
    (pcase-let* ((`(,place ,start ,end) journey)
                 (mins (/ (float-time (time-subtract end start)) 60))
                 (title (read-string "Journey: " (format "\u2192 %s" place)))
                 (from (org-read-date t t nil "Leaving" start))
                 (to (time-add from (* 60 mins))))
      (org-foresight--file-journey title place from to)
      (org-foresight--invalidate-signals)
      (setq org-foresight--shape-cache nil)
      (when (derived-mode-p 'org-agenda-mode) (org-agenda-redo))
      (message "Booked %s, %s-%s" title
               (format-time-string "%H:%M" from)
               (format-time-string "%H:%M" to)))))

(defun org-foresight--file-task (title when effort props)
  "File a task TITLE scheduled at WHEN with EFFORT and PROPS.
WHEN is a time value; its time of day is kept only when it is not midnight,
so an all-day task does not acquire a spurious 00:00.  Returns the marker of
the new entry.  `org-log-reschedule' is bound off throughout: a task being
created has not been rescheduled, and letting Org think otherwise would
poison the very measurement `org-foresight--reschedule-count' reads."
  (let ((file (org-foresight--task-file))
        (org-log-reschedule nil)
        (org-log-redeadline nil))
    (with-current-buffer (find-file-noselect file)
      (org-with-wide-buffer
       (let ((level (org-foresight--file-open-day when)))
         (insert (make-string level ?*) " "
                 org-foresight-task-todo " " title "\n")
         (forward-line -1)
         (org-schedule nil (format-time-string
                            (if (org-foresight--midnight-p when)
                                "%Y-%m-%d %a"
                              "%Y-%m-%d %a %H:%M")
                            when))
         (when effort (org-set-property "EFFORT" effort))
         (pcase-dolist (`(,k . ,v) props) (org-set-property k v))
         (save-buffer)
         (org-foresight--invalidate-signals)
         (point-marker))))))

(defun org-foresight--invalidate-signals ()
  "Drop the memoized signals.
Anything that writes to Org must call this.  Acting on a cached view of what
still needs doing is how a command that is supposed to be idempotent ends up
doing the same work twice."
  (setq org-foresight--signals-cache nil))

(defun org-foresight--midnight-p (time)
  "Non-nil when TIME falls exactly on midnight."
  (let ((d (decode-time time)))
    (and (zerop (nth 0 d)) (zerop (nth 1 d)) (zerop (nth 2 d)))))

;;;; Filling in the clock
;; The other half of filing.  Above, work that has not happened yet; here,
;; work that has happened and left no record of it.

(defcustom org-foresight-clock-fill-minimum 5
  "Shortest unrecorded stretch, in minutes, worth being asked about.

A day is full of small holes -- a phone call, a walk to the printer, the
minute between finishing one thing and starting the next.  Listing every one
of them would bury the twenty minutes that actually went somewhere, and a
list nobody reads to the end is a list that loses its last item."
  :type 'integer
  :group 'org-foresight)

(defcustom org-foresight-clock-fill-kinds nil
  "Kinds of time that are filed under something rather than standing alone.

Offered first when `org-foresight-clock-fill\=' asks what a stretch was spent
on, and answering with one asks a second question -- what it was about --
instead of a name.  The clock then lands on a heading of that kind under the
work chosen, made the first time and found every time after.

For the hours that are real, recurring, and tedious to name: the chat, the
mail, the corridor answer.  Left unnamed they are indistinguishable from time
nobody can account for, and a day of them teaches the reserve that the whole
day leaks.

Empty by default.  Which kinds a day divides into is a fact about the work
someone does, not about Org, and a list guessed here would be a list nobody
recognised."
  :type '(repeat string)
  :group 'org-foresight)

(defconst org-foresight-prep-property "FORESIGHT_PLAN_PREP"
  "Property marking a meeting whose preparation has already been made.")

(defconst org-foresight-meeting-uid-property "FORESIGHT_PLAN_MEETING_UID"
  "Property on a preparation task, naming the meeting it was made for.

Constants rather than settings, for the reason the day\='s own properties are:
these are written and read by this package alone, so there is nobody above to
want a different word for them.")

(defcustom org-foresight-clock-fill-kind-property "FORESIGHT_KIND"
  "Property naming what a heading made by `org-foresight-clock-fill\=' records.

A property rather than a tag, for the reason the surge property is one: it is
a mark left for this package to read, not a word the writer files things
under.  It carries a value, so a second kind costs a value rather than
another mechanism."
  :type 'string
  :group 'org-foresight)

(defun org-foresight--clock-cuts (day scan)
  "Return the moments in DAY that something really happened at.

The edges of the meetings it holds.  A meeting is a hard fact about a day:
it began, it ended, and the hours either side of it are different hours --
which is what makes it a place to cut an unrecorded stretch in two.

Not the working hours, which are a decision about the day rather than
something that happens in it, and not a SCHEDULED time, which is a plan
somebody made for a task and is as easily kept as broken.  Times the
keyboard went quiet are real too, and `org-foresight-behind\=' has already
cut at those.

The meeting\='s own stretch becomes a candidate of its own by this, and its
title is already among the answers to what the stretch was spent on."
  (let (out)
    (dolist (e (org-foresight-scan-day scan :ledger day) (nreverse out))
      (when (eq (plist-get e :kind) 'meeting)
        (push (plist-get e :start) out)
        (push (plist-get e :end) out)))))

(defun org-foresight--clock-gaps (behind &optional cuts)
  "Return BEHIND's unrecorded stretches as (INTERVAL . KIND), earliest first.

KIND is `unclocked' or `away'.  It is the only thing that tells one stretch
from another, and it is worth carrying because the two are remembered
differently: what you were doing at the keyboard and what you were doing away
from it are not recalled by the same kind of effort.

CUTS are moments to divide a stretch at.  Without them a day with nothing
clocked in it is one hole from breakfast to bedtime, and the answer to
\"what was that?\" cannot be one thing.

The threshold is applied to the stretches and the cuts are made inside
what survives, in that order and not the other way about.  Cutting first
manufactures short pieces out of long ones -- a meeting beginning two
minutes after a clock stopped makes a two-minute piece -- and dropping
those takes minutes out of the total.  Then the bar above the agenda
reports time as unrecorded that this command will not offer to fill, and
nothing in the tool can say where it went."
  (let* ((least (* 60 org-foresight-clock-fill-minimum))
         (worth (lambda (ivs)
                  (seq-filter
                   (lambda (iv)
                     (>= (float-time (time-subtract (cdr iv) (car iv))) least))
                   ivs))))
    (seq-sort-by
     (lambda (gap) (float-time (car (car gap)))) #'<
     (append (mapcar (lambda (iv) (cons iv 'unclocked))
                     (org-foresight--intervals-split
                      (funcall worth (plist-get behind :unclocked-ivs))
                      cuts least))
             (mapcar (lambda (iv) (cons iv 'away))
                     (org-foresight--intervals-split
                      (funcall worth (plist-get behind :away-ivs))
                      cuts least))))))

(defun org-foresight--ordered (candidates)
  "Return a completion table offering CANDIDATES in the order given.

The order is the answer on every prompt this package puts up: the holes of
a day run from morning to night, the spells of it likewise, and the two
halves of a divided spell are the first and the second.  Left to the
frontend they are sorted by how recently each string was typed and then by
how long it is, which is an order about the words rather than about the
day -- and an order nobody can predict is one the eye has to read from the
top every time.

CANDIDATES is a list of strings, or an alist whose keys are the strings."
  (let ((names (mapcar (lambda (c) (if (consp c) (car c) c)) candidates)))
    (lambda (string pred action)
      (if (eq action 'metadata)
          '(metadata (display-sort-function . identity)
                     (cycle-sort-function . identity))
        (complete-with-action action names string pred)))))

(defun org-foresight--clock-gap-label (gap)
  "Return GAP as one line: when it ran, how long it was, and which kind."
  (let ((iv (car gap)))
    (format "%s-%s  %s  (%s)"
            (format-time-string "%H:%M" (car iv))
            (format-time-string "%H:%M" (cdr iv))
            (org-duration-from-minutes
             (/ (float-time (time-subtract (cdr iv) (car iv))) 60.0))
            (if (eq (cdr gap) 'away) "away" "at the keyboard"))))

(defun org-foresight--clock-fill-candidates (clock &optional scan day)
  "Return (TITLE . MARKER) for the work DAY already knows about.

Two sources, because they miss different things.  What has been clocked today
is where an interrupted task is found: it is on the list already and only
wants its missing half.  The day's own entries are where a task that was
never clocked at all is found -- the commoner case by far, and the one a list
built from the clock alone can never offer.

Three sources, then, because the first two miss the commonest thing of all:
work that is simply open.  An hour goes on whatever was in hand, and what
was in hand is often neither clocked today nor dated to today -- the task
picked up because it was next, the one nobody ever gave a date.  Offered
neither, the name typed at the prompt matched nothing and the hour was
filed under a *second* heading with the same words as the first, which
loses the connection between them for good.

Followups are left out of that third source.  A WAIT is time somebody else
is spending, and an hour of yours is not what it is waiting for.

Neither is required.  A stretch that went on something nobody had written
down is the whole reason the day has holes in it, and a prompt that refused
to accept one would send its answer somewhere else."
  (let* ((day (or day (org-foresight--day-start 0)))
         (scan (or scan (org-foresight-scan 1 day)))
         (ledger (org-foresight-scan-day scan :ledger day))
         (out nil))
    (dolist (task (plist-get clock :day-tasks))
      (when-let ((marker (plist-get task :marker)))
        (push (cons (plist-get task :title) marker) out)))
    (dolist (entry ledger)
      (when-let ((title (plist-get entry :title)))
        (unless (assoc title out)
          ;; A *derived* journey is offered by name and files nowhere.  Half
          ;; of them have no entry behind them at all, and the half that do
          ;; carry the marker of the meeting they are *for* -- clocking the
          ;; drive onto the meeting would put the road inside the room, and
          ;; the meeting would report an hour and a half of itself.
          ;;
          ;; A journey somebody wrote down is the opposite case and was being
          ;; treated as the same one.  It has a heading of its own, which is
          ;; the one place the hour belongs; offered without it the name fell
          ;; through to the fallback and a second heading was written under
          ;; the same title, so the trip that had been settled once was on
          ;; the day twice and neither copy held all of it.
          (let ((marker (unless (and (eq (plist-get entry :kind) 'travel)
                                     (not (plist-get entry :written)))
                          (plist-get entry :marker))))
            (when (or marker (eq (plist-get entry :kind) 'travel))
              (push (cons title marker) out))))))
    ;; Last, so that the day's own work stays at the top of the list where
    ;; it is likeliest to be the answer.
    (dolist (rec (org-foresight-outline-records))
      (let ((title (plist-get rec :title)))
        (when (and title
                   (markerp (plist-get rec :marker))
                   (not (plist-get rec :done))
                   (not (plist-get rec :parked))
                   (not (member (plist-get rec :todo)
                                org-foresight-followup-keywords))
                   (not (assoc title out)))
          (push (cons title (plist-get rec :marker)) out))))
    (nreverse out)))

(defun org-foresight--heading-named (title)
  "Return a marker on any heading called TITLE, or nil.

Asked only of a name the prompt did not offer, which after the three
sources above means a heading that is done, parked, or waiting on somebody
else.  Each of those is a perfectly ordinary thing to have spent an hour
on -- the task finished this morning and never clocked, the one that came
unstuck while you waited -- and writing a second heading with the same
words would be the one outcome nobody wants.

The first match: two headings may share a title, and there is nothing here
to choose between them with.  The question that follows shows which one it
found."
  (seq-some (lambda (rec)
              (and (equal (plist-get rec :title) title)
                   (markerp (plist-get rec :marker))
                   (plist-get rec :marker)))
            (org-foresight-outline-records)))

(defun org-foresight--derived-journeys (&optional scan day)
  "Return (TITLE PLACE START END) for DAY\='s journeys nobody wrote down.

One reading of what a derived journey is, for the commands that offer them
and for the one that makes them real.  A written journey is not here: it is
an entry already, with its own hours and its own heading, and offering it
would be offering to write down what is written."
  (let* ((day (or day (org-foresight--day-start 0)))
         (scan (or scan (org-foresight-scan 1 day)))
         (ledger (org-foresight-scan-day scan :ledger day))
         out)
    (dolist (entry ledger (nreverse out))
      (when (and (eq (plist-get entry :kind) 'travel)
                 (plist-get entry :place)
                 (plist-get entry :title)
                 (plist-get entry :start)
                 (not (plist-get entry :written)))
        (push (list (plist-get entry :title) (plist-get entry :place)
                    (plist-get entry :start) (plist-get entry :end))
              out)))))

(defun org-foresight--clock-fill-journeys (&optional scan day)
  "Return (TITLE . PLACE) for the journeys DAY derives but nobody wrote down.

A journey named at the prompt has to be written down *as* a journey.  Filed as
an ordinary entry it records the hour that was spent and leaves the derivation
untouched, so the day goes on reserving a second leg to the same door -- the
hour is counted twice, the free time is short by it, and what would fit is
answered against a trip already made.  Booked to its place, the derivation
defers to it, which is the mechanism `org-foresight-book-travel\=' already
uses."
  (mapcar (lambda (j) (cons (nth 0 j) (nth 1 j)))
          (org-foresight--derived-journeys scan day)))

(defun org-foresight--clock-fill-parents ()
  "Return (TITLE . MARKER) for the open work a kind can be filed under.

Every open TODO heading, not only the ones that are projects: what a
conversation belonged to is a judgement about the work, and a list that had
already made it would be missing the answer half the time."
  (let (out)
    (dolist (rec (org-foresight-outline-records) (nreverse out))
      (when (and (not (plist-get rec :done))
                 (markerp (plist-get rec :marker)))
        (push (cons (plist-get rec :title) (plist-get rec :marker)) out)))))

(defun org-foresight--child-heading (title level end)
  "Return a marker on the direct child named TITLE of the entry at point.

LEVEL is that entry\='s own level and END where its subtree stops.  Direct
children only: a `comms\=' heading two levels down belongs to something else,
and answering with it would file the hour under the wrong work."
  (save-excursion
    (let (found)
      (while (and (not found)
                  (outline-next-heading)
                  (< (point) end))
        (when (and (= (org-current-level) (1+ level))
                   (equal (org-foresight--entry-title) title))
          (setq found (point-marker))))
      found)))

(defun org-foresight--clock-fill-kind-marker (kind)
  "Return a marker on KIND\='s own heading under work the reader picks.

Found when it is already there and made when it is not, so the second
conversation about a thing lands where the first one did and the two are one
figure rather than two entries.

No TODO keyword, for the reason `org-foresight--file-clocked-entry\=' has
none: what is being recorded already happened, and a keyword would put it
back among the things still to do.  It is also what leaves the outline\='s
shape alone -- a heading with no keyword is scaffolding, so a task that grows
one of these is still a task and not suddenly a project."
  (let* ((parents (org-foresight--clock-fill-parents))
         (title (completing-read (format "%s under: " kind)
                                 (mapcar #'car parents) nil t))
         (parent (cdr (assoc title parents))))
    (unless parent (user-error "Nothing chosen, nothing written"))
    (org-with-point-at parent
      (org-with-wide-buffer
       (org-back-to-heading t)
       (let* ((level (org-current-level))
              (end (save-excursion (org-end-of-subtree t t))))
         (or (org-foresight--child-heading kind level end)
             (progn
               ;; Last child rather than first: the hours go underneath the
               ;; work, not in front of what is still to be done in it.
               (goto-char end)
               (unless (bolp) (insert "\n"))
               (insert (make-string (1+ level) ?*) " " kind "\n")
               (forward-line -1)
               (org-set-property org-foresight-clock-fill-kind-property kind)
               (point-marker))))))))

(defun org-foresight--file-clocked (marker from to)
  "Add a CLOCK line running FROM until TO to the entry at MARKER.

Positioned by `org-clock-find-position', which is what `org-clock-in' itself
uses.  Where a clock line goes -- whether the entry has a LOGBOOK, whether it
is folded, where a new line sits among the old ones -- is Org's convention
and moves with it, and a second implementation of it here would be a second
one to keep true."
  (org-with-point-at marker
    (org-with-wide-buffer
     (org-back-to-heading t)
     (org-clock-find-position nil)
     (insert-before-markers-and-inherit "\n")
     (backward-char 1)
     (insert-and-inherit org-clock-string " "
                         (format-time-string (org-time-stamp-format t t) from)
                         "--"
                         (format-time-string (org-time-stamp-format t t) to))
     ;; Org's own arithmetic for the `=> H:MM' that closes the line, for the
     ;; same reason as the position: it is the number every clock report adds
     ;; up, and one computed here would only be able to differ.
     (org-clock-update-time-maybe)
     (save-buffer))))

(defun org-foresight--file-clocked-entry (title from to surge)
  "File a new entry TITLE covering FROM until TO, and return its marker.

No TODO keyword.  What is being written down already happened, and a keyword
would put it back on the list of things to do -- the day would then carry it
twice, once as an hour that is gone and once as an hour still owed.

SURGE non-nil marks it as work that arrived rather than work that was
planned, which is what keeps tomorrow's allowance for interruptions honest:
an interruption nobody recorded teaches the reserve that there are none."
  (with-current-buffer (find-file-noselect (org-foresight--task-file))
    (org-with-wide-buffer
     (let ((level (org-foresight--file-open-day from)))
       (insert (make-string level ?*) " " title "\n")
       (forward-line -1)
       (when surge
         (org-set-property org-foresight-surge-property
                           (format-time-string (org-time-stamp-format t t)
                                               from)))
       (let ((marker (point-marker)))
         (org-foresight--file-clocked marker from to)
         marker)))))

;;;###autoload
(defun org-foresight-clock-fill (&optional day)
  "Say what an unrecorded stretch of DAY was spent on.

Every day leaves holes in its own record: the interruption taken without
stopping to start a timer, the hour the machine slept through, the task
finished before anybody remembered it was never clocked in.  Each one is time
the record says nothing about, and by the evening they are the whole
difference between a day that looks half spent and one that was.

Nothing here asks for a time.  Where the holes are is known already -- they
are what `org-foresight-behind\\=' measured in order to draw the elapsed bar
-- so the only thing left for a person is the one thing no machine can
supply, which is what they were doing in them.  Typing hours in by hand is
the reason the holes are still there at six o\\='clock: it is a small tax on
an act that is already an afterthought, and a small tax on an afterthought
collects nothing.

Pick a stretch, name the work, and the clock line is written where it
belongs: on the entry when the work is already in a file, in a new one under
the day when it is not.

DAY is the day under the cursor when run from an agenda line and today
otherwise, as `org-foresight-shape-day' reads it.  A record is corrected the
morning after at least as often as the evening of -- the clock nobody
started yesterday is noticed today -- and a command that could only be
pointed at today could only mend a mistake on the day it was made.  The
watcher is the one thing that does not follow: it answers for today alone,
so on any other day the keyboard going quiet is not among the cuts and a
stretch that would have arrived as two arrives as one.

The holes are the waking day\='s, not the working day\='s.  Work happens
outside the hours set aside for it -- an evening that ran long, a Saturday
morning, the half hour before nine -- and hours nobody planned to work are
exactly the ones no clock was started for.  Declared working hours are a
decision about a day, not something that happens in it, so they are not
allowed to decide what a day may be asked about.

Where a stretch is cut in two is the other half of this.  A meeting is a
hard fact -- it began, it ended, the hours either side of it are different
hours -- and so is the moment the keyboard went quiet.  A SCHEDULED time is
not: it is a plan for a task, as easily kept as broken.  Cut at the first
two and an afternoon nobody clocked arrives as the few stretches it
actually had; cut at none and it is one hole from lunch to bedtime, which
no single answer fits."
  (interactive (list (org-foresight--day-at-point)))
  (let* ((day (or day (org-foresight--day-start 0)))
         (today (equal day (org-foresight--day-start 0)))
         (clock (org-foresight-clock-scan 7 nil day))
         (scan (org-foresight-scan 1 day))
         (awake (plist-get (org-foresight-day-shape day) :awake))
         (span (and awake (list (cons (car awake) (cdr awake)))))
         (behind (org-foresight-behind
                  day clock
                  ;; The watcher answers for today and for no other day, so
                  ;; on any other the keyboard going quiet is not among the
                  ;; cuts.  A stretch that would have been two arrives as
                  ;; one, which is a coarser answer and not a wrong one.
                  (and today (org-foresight-observe-coverage clock))
                  nil span))
         (gaps (org-foresight--clock-gaps
                behind (org-foresight--clock-cuts day scan))))
    (unless gaps
      (user-error "Nothing %s is unrecorded for longer than %d minutes"
                  (downcase (org-foresight--plan-day-name day))
                  org-foresight-clock-fill-minimum))
    (let* ((choices (mapcar (lambda (gap)
                              (cons (org-foresight--clock-gap-label gap) gap))
                            gaps))
           (gap (cdr (assoc (completing-read "Unrecorded: "
                                             (org-foresight--ordered choices)
                                             nil t nil nil (car (car choices)))
                            choices)))
           (from (car (car gap)))
           (to (cdr (car gap)))
           (known (org-foresight--clock-fill-candidates clock scan day))
           (journeys (org-foresight--clock-fill-journeys scan day))
           ;; Kinds first.  They are the answer on the hours hardest to name,
           ;; which is exactly why those hours are the ones still unrecorded
           ;; at six o\'clock.  Then the journeys, gathered rather than left
           ;; where the day happened to put them: they arrive among the
           ;; day\'s entries already, but behind everything clocked so far,
           ;; and by the evening that is a long way down.
           (title (completing-read
                   "What were you doing? "
                   (org-foresight--ordered
                    (append org-foresight-clock-fill-kinds
                            (mapcar #'car journeys)
                            (seq-remove (lambda (name) (assoc name journeys))
                                        (mapcar #'car known))))))
           (marker (cdr (assoc title known)))
           ;; A name the list did not hold may still be a heading.
           (elsewhere (unless marker (org-foresight--heading-named title))))
      (when (string-empty-p (string-trim title))
        (user-error "Nothing named, nothing written"))
      (cond
       ;; Checked before the day\'s own entries, so a kind that happens to
       ;; share a name with something on today\'s list still asks what it was
       ;; about rather than silently filing it there.
       ((member title org-foresight-clock-fill-kinds)
        (org-foresight--file-clocked
         (org-foresight--clock-fill-kind-marker title) from to))
       ;; Before the fallback, and it has to be: a derived journey is offered
       ;; with no marker, so without this it would fall through and be written
       ;; as a plain entry -- leaving the day to reserve the trip all over
       ;; again.
       ((assoc title journeys)
        (org-foresight--file-clocked
         (org-foresight--file-journey title (cdr (assoc title journeys)) from to)
         from to)
        (setq org-foresight--shape-cache nil))
       (marker (org-foresight--file-clocked marker from to))
       ;; Offered rather than assumed: a name can be typed meaning the thing
       ;; that heading is about, and it can be typed meaning something else
       ;; that happens to be called the same.  Only the person knows which,
       ;; and the cost of guessing wrong is a duplicate nobody notices.
       ((and elsewhere
             (y-or-n-p (format "\"%s\" is already a heading; clock onto it? "
                               title)))
        (org-foresight--file-clocked elsewhere from to))
       (t (org-foresight--file-clocked-entry
           title from to
           ;; Asked only of a name that came from nowhere.  An answer the day
           ;; already knew about -- a journey it derived from the calendar --
           ;; arrived on the calendar, and calling it unplanned would teach the
           ;; reserve that the commute was an interruption.
           (and (not (assoc title known))
                (y-or-n-p "Arrived unplanned? ")))))
      (org-foresight--invalidate-signals)
      (when (derived-mode-p 'org-agenda-mode) (org-agenda-redo))
      (message "Clocked %s, %s-%s" title
               (format-time-string "%H:%M" from)
               (format-time-string "%H:%M" to)))))


;;;; Moving the clock, now or at a time that has not come

;; Declared here because `org-read-date' tells its caller through them, and
;; the bare `defvar' in org.el makes them special in org.el alone: unbound
;; where the call is, a time typed at the prompt parses and is then dropped
;; on the way out.
(defvar org-time-was-given)
(defvar org-end-time-was-given)

(defvar org-foresight--clock-pending nil
  "Switches waiting for their time, newest first.

Each is a plist of :at, :marker, :title and :timer.  Kept for this session
and no longer.  A switch scheduled for three o'clock is a fact about this
afternoon, and restoring one at tomorrow's startup would move somebody's
clock for a reason they had forgotten by then -- the kind of help that is
indistinguishable from a bug.")

(defun org-foresight--clock-move (marker at)
  "Close whatever clock is running at AT and open one on MARKER from AT.

Closed at AT rather than at the moment of asking, so the two spells meet:
the work being left off ran until the work being taken up began, and a
record with a hole or an overlap between them is one somebody has to
correct later."
  (when (org-clocking-p)
    (when (time-less-p at org-clock-start-time)
      (user-error "The clock running started at %s, after that"
                  (format-time-string "%H:%M" org-clock-start-time)))
    (org-clock-out nil t at))
  (org-with-point-at marker
    ;; `org-clock-continuously' would take the last clock-out time in place
    ;; of the one just named, which is the whole of what was asked for.
    (let ((org-clock-continuously nil))
      (org-clock-in nil at))))

(defun org-foresight--clock-arrive (entry)
  "Make the switch ENTRY was waiting for.  Run from its timer."
  (setq org-foresight--clock-pending (delq entry org-foresight--clock-pending))
  (let ((marker (plist-get entry :marker))
        (at (plist-get entry :at))
        (title (plist-get entry :title)))
    (cond
     ((not (and (markerp marker) (marker-buffer marker)))
      (message "org-foresight: \"%s\" is gone; the clock stays where it is"
               title))
     ;; Only when the timer ran late enough for a newer clock to have started
     ;; after the time it was waiting for -- a machine asleep at three
     ;; o'clock, woken at four.  Nothing is written: a spell that ends before
     ;; it began is worse than a switch that did not happen, and the person
     ;; is at the keyboard now to make it themselves.
     ((and (org-clocking-p) (time-less-p at org-clock-start-time))
      (message "org-foresight: \"%s\" was due at %s and the clock has moved on since; left alone"
               title (format-time-string "%H:%M" at)))
     (t
      (org-foresight--clock-move marker at)
      (org-foresight--invalidate-signals)
      (message "org-foresight: clock moved to \"%s\", from %s"
               title (format-time-string "%H:%M" at))))))

(defun org-foresight--clock-switch-read ()
  "Read which entry to clock into, from what today knows about.

Asked only where the cursor is on nothing.  The same list the other two
clock commands offer -- what has been clocked today and what the day holds
-- because the answer is the same kind of thing however the question was
reached, and a command that could only be pressed on the right line would
be a command nobody could press from the buffer they were reading when the
meeting was about to start."
  (let* ((clock (org-foresight-clock-scan 1))
         (known (seq-filter #'cdr (org-foresight--clock-fill-candidates clock)))
         (title (and known (completing-read "Clock in to: " known nil t))))
    (unless known
      (user-error "Nothing clocked today and nothing on the day to clock into"))
    (cdr (assoc title known))))

(defun org-foresight--clock-forget ()
  "Call off a switch that has not happened yet."
  (unless org-foresight--clock-pending
    (user-error "No switch is waiting"))
  (let* ((choices (mapcar (lambda (e)
                            (cons (format "%s  %s"
                                          (format-time-string "%H:%M"
                                                              (plist-get e :at))
                                          (plist-get e :title))
                                  e))
                          org-foresight--clock-pending))
         (entry (cdr (assoc (completing-read "Call off: "
                                             (org-foresight--ordered choices)
                                             nil t)
                            choices))))
    (when entry
      (when (timerp (plist-get entry :timer))
        (cancel-timer (plist-get entry :timer)))
      (setq org-foresight--clock-pending
            (delq entry org-foresight--clock-pending))
      (message "org-foresight: \"%s\" at %s called off"
               (plist-get entry :title)
               (format-time-string "%H:%M" (plist-get entry :at))))))

;;;###autoload
(defun org-foresight-clock-switch (&optional call-off)
  "Move the clock onto the entry at point, as of a time you name.

Two things a running clock cannot say for itself.  The first is that the
work changed a while ago: you have been in the meeting for ten minutes
before it occurs to anybody that the clock is still on what came before, and
the ten minutes belong to the meeting.  Name the time it started and both
spells are written to meet there -- what was running is closed at it, and
this entry is opened from it and left running.

The second is a time that has not come yet.  A meeting at three will start
at three whether or not you are at the keyboard to say so, and the clock
that should move then is one thing you can decide now, while looking at the
row.  Name a time in the future and the switch waits for it: whatever is
running goes on running until then, and is closed at the stroke.

Waiting is for this session only.  There is no file of pending switches to
restore from, deliberately -- see `org-foresight--clock-pending\\='.

The entry is the one under the cursor.  Where the cursor is on nothing --
the foot of the agenda, another buffer entirely -- it asks which, from what
today knows about: a meeting about to start is exactly the moment somebody
is looking at something else.

With a prefix argument CALL-OFF, pick a waiting switch and call it off.

The time is read the way Org reads every time: `14:00\\=', `2pm\\=', `+2h\\=',
`+1 09:00\\=' for tomorrow morning.  A date with no time in it is refused,
because a clock is a moment and midnight is not what anybody meant."
  (interactive "P")
  (if call-off
      (org-foresight--clock-forget)
    (let ((marker (or (org-get-at-bol 'org-hd-marker)
                      (org-get-at-bol 'org-marker)
                      (and (derived-mode-p 'org-mode)
                           (not (org-before-first-heading-p))
                           (point-marker))
                      (org-foresight--clock-switch-read))))
      (let (at title)
        (let (org-time-was-given org-end-time-was-given)
          (setq at (org-read-date t t nil "Clock in at"))
          (unless org-time-was-given
            (user-error "No time of day in that; a clock starts at a moment")))
        (org-with-point-at marker
          (org-back-to-heading t)
          (setq title (org-foresight--entry-title)))
        (if (time-less-p (current-time) at)
            (let ((entry (list :at at :marker (copy-marker marker)
                               :title title :timer nil)))
              (push entry org-foresight--clock-pending)
              (plist-put entry :timer
                         (run-at-time at nil #'org-foresight--clock-arrive
                                      entry))
              (message "org-foresight: the clock moves to \"%s\" at %s (%s to call it off)"
                       title (format-time-string "%H:%M" at)
                       (let ((key (org-foresight--command-key
                                   'org-foresight-clock-switch)))
                         (if key (format "C-u %s" key)
                           "C-u M-x org-foresight-clock-switch"))))
          (org-foresight--clock-move marker at)
          (org-foresight--invalidate-signals)
          (org-foresight-report-refresh)
          (message "org-foresight: clocked into \"%s\", from %s"
                   title (format-time-string "%H:%M" at)))))))

;;;; Dividing a spell that was two things

(defun org-foresight--clocked-spells (clock)
  "Return CLOCK's newest day of clocked segments, as plists.

Each is (:title :marker :from :to).  Which day that is was decided when
CLOCK was surveyed; this reads whatever it was asked about.

One per segment rather than one per entry.  An hour clocked in two sittings
is two things that can be corrected separately, and the sitting that was
really something else is a sitting, not a total."
  (let (out)
    (dolist (task (plist-get clock :day-tasks))
      (when-let ((marker (plist-get task :marker)))
        (dolist (iv (plist-get task :intervals))
          (push (list :title (plist-get task :title) :marker marker
                      :from (car iv) :to (cdr iv))
                out))))
    (sort out (lambda (a b) (time-less-p (plist-get a :from)
                                         (plist-get b :from))))))

(defun org-foresight--clock-spell-label (spell)
  "Return SPELL as one line: when it ran, how long it was, and what it was on."
  (format "%s-%s  %s  %s"
          (format-time-string "%H:%M" (plist-get spell :from))
          (format-time-string "%H:%M" (plist-get spell :to))
          (org-duration-from-minutes
           (/ (float-time (time-subtract (plist-get spell :to)
                                         (plist-get spell :from)))
              60.0))
          (plist-get spell :title)))

(defun org-foresight--same-minute-p (a b)
  "Return non-nil when times A and B fall in the same minute.
A clock line is written to the minute, so a comparison finer than that would
only be able to differ from what is on the page."
  (= (floor (float-time a) 60) (floor (float-time b) 60)))

(defun org-foresight--goto-clock-line (from to)
  "Put point on the CLOCK line of the entry at point running FROM to TO.
Return its position, or nil when the entry has no such line.

The timestamps are compared as times rather than as text: a timestamp carries
the day name of whatever locale wrote it, and these files are read on more
than one machine."
  (org-back-to-heading t)
  (let ((end (save-excursion (org-end-of-subtree t t) (point)))
        found)
    (save-excursion
      (while (and (not found)
                  (re-search-forward
                   (concat "^[ \t]*" (regexp-quote org-clock-string)) end t))
        (let ((eol (line-end-position)) stamps)
          (save-excursion
            (beginning-of-line)
            (while (re-search-forward org-ts-regexp-inactive eol t)
              (push (org-time-string-to-time (match-string 1)) stamps)))
          (setq stamps (nreverse stamps))
          (when (and (= 2 (length stamps))
                     (org-foresight--same-minute-p (nth 0 stamps) from)
                     (org-foresight--same-minute-p (nth 1 stamps) to))
            (setq found (line-beginning-position))))))
    (when found (goto-char found))
    found))

(defun org-foresight--reclock-line (from to)
  "Rewrite the CLOCK line at point to run FROM until TO.

The indentation is left alone -- the line sits in a drawer, and Org's own
`org-clock-update-time-maybe\=' closes it with the `=> H:MM\=' every clock
report adds up."
  (beginning-of-line)
  (skip-chars-forward " \t")
  (delete-region (point) (line-end-position))
  (insert-and-inherit org-clock-string " "
                      (format-time-string (org-time-stamp-format t t) from)
                      "--"
                      (format-time-string (org-time-stamp-format t t) to))
  (org-clock-update-time-maybe))

(defun org-foresight--clock-split-read (from to)
  "Read a time strictly inside FROM..TO and return it.
The complaint about a rejected answer goes in front of the next prompt, so
the spell being divided stays on screen while it is corrected."
  (let ((default (format-time-string
                  "%H:%M"
                  (time-add from (seconds-to-time
                                  (/ (float-time (time-subtract to from)) 2)))))
        (complaint "")
        at)
    (while (null at)
      (let* ((answer (read-string
                      (format "%sDivide %s-%s at (HH:MM): " complaint
                              (format-time-string "%H:%M" from)
                              (format-time-string "%H:%M" to))
                      default))
             (try (and (string-match-p "\\`[0-9]\\{1,2\\}:[0-5][0-9]\\'"
                                       (string-trim answer))
                       (org-foresight--hhmm-on from (string-trim answer)))))
        (cond
         ((null try) (setq complaint "A time like 14:30.  "))
         ((or (not (time-less-p from try)) (not (time-less-p try to)))
          (setq complaint "Inside the spell.  "))
         (t (setq at try)))))
    at))

;;;###autoload
(defun org-foresight-clock-split (&optional day)
  "Give part of a clocked spell of DAY to the work it actually was.

An hour goes down against one task and turns out to have been two: the call
that came in the middle of it, twenty minutes of somebody else's problem, the
half of a meeting that was really about the next project.  Left alone the
whole hour is charged to whichever task happened to be running, and every
figure built on the clock -- what that task cost, where the day went, what to
estimate next time -- is wrong by the same amount in both directions at once.

Nothing is invented here.  The time already exists and only changes hands, so
the day's total after this is the total it had before.

`org-foresight-clock-fill\=' is the other half of keeping the record honest
and cannot be asked these questions.  It is about the hours no clock covers,
so it offers the holes and never asks for a time; this is about an hour a
clock covers wrongly, so it offers the spells and must ask where to cut.

DAY is the day under the cursor when run from an agenda line and today
otherwise, for the reason `org-foresight-clock-fill' gives: an hour charged
to the wrong task is noticed when the week is read, not while it is
running."
  (interactive (list (org-foresight--day-at-point)))
  (let* ((day (or day (org-foresight--day-start 0)))
         (clock (org-foresight-clock-scan 1 nil day))
         (spells (org-foresight--clocked-spells clock)))
    (unless spells
      (user-error "Nothing is clocked %s, so there is nothing to divide"
                  (downcase (org-foresight--plan-day-name day))))
    (let* ((choices (mapcar (lambda (s)
                              (cons (org-foresight--clock-spell-label s) s))
                            spells))
           (spell (cdr (assoc (completing-read "Divide which spell? "
                                               (org-foresight--ordered choices)
                                               nil t nil nil (caar choices))
                              choices)))
           (from (plist-get spell :from))
           (to (plist-get spell :to))
           (at (org-foresight--clock-split-read from to))
           (parts (list (cons (format "%s-%s  the first part"
                                      (format-time-string "%H:%M" from)
                                      (format-time-string "%H:%M" at))
                              (cons from at))
                        (cons (format "%s-%s  the second part"
                                      (format-time-string "%H:%M" at)
                                      (format-time-string "%H:%M" to))
                              (cons at to))))
           ;; The second part is the default.  An interruption arrives during
           ;; the work and is often still going when the clock is finally
           ;; stopped, so the tail is the half that usually belongs elsewhere.
           (moved (cdr (assoc (completing-read "Which part moves? "
                                               (org-foresight--ordered parts)
                                               nil t nil nil (car (nth 1 parts)))
                              parts)))
           (kept (if (org-foresight--same-minute-p (car moved) from)
                     (cons at to)
                   (cons from at)))
           (scan (org-foresight-scan 1 day))
           (known (org-foresight--clock-fill-candidates clock scan day))
           (journeys (org-foresight--clock-fill-journeys scan day))
           ;; Journeys gathered ahead of the day's own entries, as
           ;; `org-foresight-clock-fill' gathers them and for its reason: they
           ;; are among those entries already, but behind everything clocked
           ;; so far, and by the time an hour is being divided that is a long
           ;; way down a list.
           (title (completing-read "And that part was? "
                                   (org-foresight--ordered
                                    (append org-foresight-clock-fill-kinds
                                            (mapcar #'car journeys)
                                            (seq-remove
                                             (lambda (name) (assoc name journeys))
                                             (mapcar #'car known))))))
           (marker (cdr (assoc title known)))
           (elsewhere (unless marker (org-foresight--heading-named title))))
      (when (string-empty-p (string-trim title))
        (user-error "Nothing named, nothing moved"))
      ;; The spell is shortened before the other half is written.  A failure
      ;; between the two then leaves minutes belonging to nobody, which the
      ;; day reports as a hole and this command can be run again to fill --
      ;; where the other order would charge the same minutes to two tasks and
      ;; report a day longer than it was.
      (org-with-point-at (plist-get spell :marker)
        (org-with-wide-buffer
         (unless (org-foresight--goto-clock-line from to)
           (user-error "That clock line is no longer where the scan found it"))
         (org-foresight--reclock-line (car kept) (cdr kept))
         (save-buffer)))
      (cond
       ((member title org-foresight-clock-fill-kinds)
        (org-foresight--file-clocked
         (org-foresight--clock-fill-kind-marker title) (car moved) (cdr moved)))
       ((assoc title journeys)
        (org-foresight--file-clocked
         (org-foresight--file-journey title (cdr (assoc title journeys))
                                      (car moved) (cdr moved))
         (car moved) (cdr moved))
        (setq org-foresight--shape-cache nil))
       (marker (org-foresight--file-clocked marker (car moved) (cdr moved)))
       ((and elsewhere
             (y-or-n-p (format "\"%s\" is already a heading; clock onto it? "
                               title)))
        (org-foresight--file-clocked elsewhere (car moved) (cdr moved)))
       (t (org-foresight--file-clocked-entry
           title (car moved) (cdr moved)
           (and (not (assoc title known)) (y-or-n-p "Arrived unplanned? ")))))
      (org-foresight--invalidate-signals)
      (when (derived-mode-p 'org-agenda-mode) (org-agenda-redo))
      (message "%s-%s went to %s; %s keeps %s-%s"
               (format-time-string "%H:%M" (car moved))
               (format-time-string "%H:%M" (cdr moved))
               title (plist-get spell :title)
               (format-time-string "%H:%M" (car kept))
               (format-time-string "%H:%M" (cdr kept))))))

;;;; Meeting preparation

(defun org-foresight--meeting-slots (start end)
  "Return (PREP-TIME . FOLLOW-TIME) around a meeting running START to END.
Preparation is placed to end when the meeting starts and follow-up to begin
when it ends, so both land in the working day the meeting already occupies
rather than in some abstract free slot."
  (cons (time-subtract start (* 60 (org-foresight--duration-minutes
                                    org-foresight-meeting-prep 30)))
        end))

;;;###autoload
(defun org-foresight-prepare-meetings ()
  "Create preparation and follow-up tasks for meetings that lack them.

Lists what it proposes and asks once, rather than filing silently: a meeting
whose preparation is genuinely nothing is common enough that a package which
invented work for every invitation would be turned off within a week.

Each meeting is marked with `:PLAN_PREP:' so this is safe to re-run, and each
generated task carries `:PLAN_MEETING_UID:' pointing back, which is what lets
the orphan signal notice if the meeting is later cancelled."
  (interactive)
  ;; Read past the cache: this command decides what to create, and a view of
  ;; the world from a few seconds ago may already have been acted on.
  (let ((found (cdr (assoc "Meetings without prep" (org-foresight-signals t)))))
    (if (null found)
        ;; Two different silences, and they were being reported as one.  With
        ;; no category declared, nothing in the calendar can ever be a
        ;; meeting here, so "none are missing preparation" is a sentence
        ;; about the settings wearing the clothes of a sentence about the
        ;; week -- and the reader it misleads is exactly the one who has not
        ;; got as far as `org-foresight-diagnose'.
        (message "%s"
                 (if org-foresight-meeting-categories
                     "No meetings are missing preparation"
                   (concat "No meeting categories are declared, so nothing "
                           "counts as a meeting; see "
                           "`org-foresight-meeting-categories'")))
      (let ((n 0))
        (when (yes-or-no-p
               (format "Create prep + follow-up for %d meeting(s)? " (length found)))
          (dolist (f found)
            (when (org-foresight--prepare-meeting (plist-get f :marker))
              (setq n (1+ n))))
          (message "Prepared %d meeting(s)" n))))))

(defun org-foresight--prepare-meeting (marker)
  "File preparation and follow-up for the meeting at MARKER.

Returns non-nil when it filed something.  Nothing is filed for a meeting
with no hour on it: the two tasks are placed either side of the meeting, and
there is no side of a date."
  (with-current-buffer (marker-buffer marker)
    (org-with-wide-buffer
     (goto-char marker)
     (org-back-to-heading t)
     (let* ((title (org-foresight--entry-title))
            (stamps (org-foresight--entry-timestamps))
            (el (seq-find #'org-foresight--ts-timed-p stamps)))
       (when el
         (let* ((uid (or (org-entry-get (point) "UID") (org-id-get-create)))
                (start (org-foresight--ts-start el))
                (end (org-foresight--ts-end el))
                (slots (org-foresight--meeting-slots start end))
                (props (list (cons org-foresight-meeting-uid-property uid))))
           (org-foresight--file-task
            (format "Prep: %s" title) (car slots)
            org-foresight-meeting-prep props)
           (org-foresight--file-task
            (format "Follow up: %s" title) (cdr slots)
            org-foresight-meeting-follow props)
           (org-entry-put (point) org-foresight-prep-property "t")
           (save-buffer)
           t))))))

;;;###autoload
(defun org-foresight-prepare-meeting ()
  "Create preparation and follow-up for the one meeting at point.

`org-foresight-prepare-meetings\=' offers every meeting that has none, which
is the right shape for a Monday morning and the wrong one for the invitation
that just arrived.  Most meetings need nothing; the few that do are picked
out by hand, and this is how they are picked.

Works from the agenda and from an Org file alike, on whichever entry the
cursor is on.  The category is not consulted: a meeting is whatever the
person choosing says it is, and a command that argued about it would send
them back to edit a property first."
  (interactive)
  (let ((marker (or (org-get-at-bol 'org-hd-marker)
                    (org-get-at-bol 'org-marker)
                    (and (derived-mode-p 'org-mode) (point-marker)))))
    (unless marker (user-error "No entry here"))
    (let (title already)
      (org-with-point-at marker
        (org-back-to-heading t)
        (setq title (org-foresight--entry-title)
              already (org-entry-get (point) org-foresight-prep-property)))
      (cond
       (already (message "\"%s\" already has preparation" title))
       ((org-foresight--prepare-meeting marker)
        (org-foresight-report-refresh)
        (message "Prepared \"%s\"" title))
       (t (user-error "\"%s\" has no time of day to prepare around" title))))))

;;;; Placement
;; Fitting what still has to be done into what is left of the day.
;;
;; The engine is deliberately dull -- deadline order, greedy fill, no
;; backtracking -- because a schedule that cannot be predicted cannot be
;; trusted, and a plan nobody trusts gets ignored whatever its optimality.
;; What matters more than the packing is the two rules around it: the surge
;; reserve is taken out before anything is placed, and nothing is written
;; until it has been looked at.

(defcustom org-foresight-slot-gap 5
  "Minutes left between consecutive placed tasks.
Switching between two pieces of work is never free; pretending it is fills
the day to the brim on paper and overruns it in practice."
  :type 'integer
  :group 'org-foresight)

(defcustom org-foresight-plan-min-slot 10
  "Free stretches shorter than this many minutes are not used for placement."
  :type 'integer
  :group 'org-foresight)

(defun org-foresight--todo-descendant-p ()
  "Return non-nil when the entry at point has a descendant carrying a keyword.

The same claim `:project-p\=' makes about a scanned record, asked at point
because placement walks the files itself.  A keyword of any kind counts,
DONE included: a heading whose children are all finished is still a heading
whose work was the children\='s."
  (save-excursion
    (org-back-to-heading t)
    (let ((end (save-excursion (org-end-of-subtree t t)))
          (found nil))
      (while (and (not found) (outline-next-heading) (< (point) end))
        (when (org-get-todo-state) (setq found t)))
      found)))

(defun org-foresight--candidate-at-point (day)
  "Return a placement candidate for the entry at point, or nil.
Placeable means: open, not a project, not already pinned to a time, and
either unscheduled or scheduled for DAY without one -- work that has been
accepted but not yet given a place to happen.

A project is left out because it is not a thing anybody sits down and does.
Its children are, and they are here on their own account; placing the
parent as well would spend an hour of the day twice and leave the half of
the list that is real short of budget for it."
  (unless (org-entry-is-done-p)
    (let* ((todo (org-get-todo-state))
           (sched (org-get-scheduled-time (point)))
           (timed (seq-some #'org-foresight--ts-timed-p
                            (org-foresight--entry-timestamps))))
      (when (and todo
                 (not (member todo org-foresight-followup-keywords))
                 (not timed)
                 (not (org-foresight--todo-descendant-p))
                 (or (null sched)
                     (= (org-foresight--day-of sched day) 0)
                     ;; And a date that has gone, when the day being planned
                     ;; is today -- the same fold the survey makes, or the
                     ;; work is counted against the day by one and offered a
                     ;; place by neither.  Only today: a plan for Thursday
                     ;; has no business collecting what Wednesday dropped.
                     (and (= 0 (org-foresight--day-of day
                                                      (org-foresight--day-start 0)))
                          (org-foresight--entry-schedule-gone-p))))
        (let* ((raw (org-foresight--entry-effort-minutes))
               (category (org-entry-get (point) "CATEGORY" t))
               (factor (org-foresight-bias-factor category raw)))
          (list :marker (point-marker)
                :title (org-foresight--entry-title)
                ;; The slot is sized by what the work actually takes, not by
                ;; what it was estimated at -- a plan built on estimates known
                ;; to run over is a plan that is wrong before the day begins.
                :effort (* raw factor)
                :estimate raw
                :estimated (and (org-entry-get (point) "EFFORT") t)
                :category category
                :deadline (org-get-deadline-time (point))
                :priority (org-entry-get (point) "PRIORITY")
                :scheduled sched))))))

(defun org-foresight--candidates (day)
  "Return the placement candidates for DAY, most pressing first."
  (let (out)
    (dolist (file (org-agenda-files))
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (org-with-wide-buffer
           (org-map-entries
            (lambda ()
              (when-let ((c (org-foresight--candidate-at-point day)))
                (push c out)))
            nil nil)))))
    (sort (nreverse out) #'org-foresight--candidate<)))

(defun org-foresight--candidate< (a b)
  "Order candidates: nearest deadline, then priority, then shortest first.
Shortest-first among equals is deliberate -- it clears the largest number of
open loops per hour, and open loops are what make a day feel uncontrolled."
  (let ((da (plist-get a :deadline))
        (db (plist-get b :deadline)))
    (cond
     ((and da db (not (equal da db))) (time-less-p da db))
     ((and da (not db)) t)
     ((and db (not da)) nil)
     (t (let ((pa (or (plist-get a :priority) ""))
              (pb (or (plist-get b :priority) "")))
          (if (not (equal pa pb))
              (string< (if (equal pa "") "~" pa) (if (equal pb "") "~" pb))
            (< (plist-get a :effort) (plist-get b :effort))))))))

(defun org-foresight--place (candidates free budget)
  "Fit CANDIDATES into FREE intervals within BUDGET minutes.
Returns (PLACED . SKIPPED), where PLACED is a list of candidates each given a
:start and :end, and SKIPPED is a list of (CANDIDATE . REASON).

A task is never split across a meeting: work broken in half by an interruption
is not the same work, and a plan that pretends otherwise is the reason the
estimate was wrong."
  (let ((slots (mapcar (lambda (iv) (cons (car iv) (cdr iv)))
                       (seq-filter
                        (lambda (iv)
                          (>= (/ (float-time (time-subtract (cdr iv) (car iv)))
                                 60.0)
                              org-foresight-plan-min-slot))
                        free)))
        (spent 0.0)
        placed skipped)
    (dolist (c candidates)
      (let ((mins (plist-get c :effort))
            (slot nil))
        (cond
         ((> (+ spent mins) budget)
          (push (cons c "no budget left") skipped))
         (t
          ;; first slot with room, keeping the gap after the task
          (setq slot (seq-find
                      (lambda (s)
                        (>= (/ (float-time (time-subtract (cdr s) (car s))) 60.0)
                            mins))
                      slots))
          (if (null slot)
              (push (cons c "no gap long enough") skipped)
            (let* ((start (car slot))
                   (end (time-add start (* 60 mins))))
              (push (append (list :start start :end end) c) placed)
              (setq spent (+ spent mins))
              (setcar slot (time-add end (* 60 org-foresight-slot-gap)))
              (when (time-less-p (cdr slot) (car slot))
                (setq slots (delq slot slots)))))))))
    (cons (nreverse placed) (nreverse skipped))))

;;;; The review buffer

(defvar org-foresight-plan--placed nil
  "Placements awaiting confirmation in the review buffer.")
(defvar org-foresight-plan--skipped nil
  "Candidates that could not be placed, with the reason.")
(defvar org-foresight-plan--day nil
  "The day the pending proposals are for.")

(defvar org-foresight-plan-review-mode-map
  (let ((map (make-sparse-keymap)))
    ;; `d\=' and `u\=' the way dired means them: one marks, the other takes the
    ;; mark off, and both move down so a run of lines is a run of presses.
    ;; One key that toggled was a key whose effect depended on a state the
    ;; reader had to read off the line first.
    (define-key map (kbd "d") #'org-foresight-plan-drop)
    (define-key map (kbd "u") #'org-foresight-plan-keep)
    ;; Motion, because the letters are commands here and `j\=' and `k\=' would
    ;; otherwise be two of the few that do nothing.  The same four the other
    ;; listings of this kind answer to.
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    (define-key map (kbd "j") #'next-line)
    (define-key map (kbd "k") #'previous-line)
    (define-key map (kbd "r") #'org-foresight-plan-redo)
    (define-key map (kbd "g") #'org-foresight-plan-redo)
    ;; TAB as well as RET: going to the entry and leaving the listing
    ;; standing is what the agenda puts on TAB, and this list is read the
    ;; way an agenda is.
    (define-key map (kbd "RET") #'org-foresight-plan-goto)
    (define-key map (kbd "TAB") #'org-foresight-plan-goto)
    (define-key map (kbd "C-c C-c") #'org-foresight-plan-apply)
    (define-key map (kbd "C-c C-k") #'org-foresight-plan-abort)
    (define-key map (kbd "q") #'org-foresight-plan-abort)
    map)
  "Keymap for `org-foresight-plan-review-mode'.")

(define-derived-mode org-foresight-plan-review-mode tabulated-list-mode
  "Foresight-Plan"
  "Review proposed placements before any of them are written.

\\{org-foresight-plan-review-mode-map}"
  (setq tabulated-list-format
        [("When" 13 nil) ("Effort" 7 nil) ("Task" 44 nil) ("Note" 12 nil)]
        tabulated-list-padding 2
        ;; The column titles go in the buffer, so that the header line can
        ;; hold what org-capture puts there: a buffer holding a change until
        ;; it is confirmed has to say so where the reader starts, not where
        ;; they stop.  Two lines of titles is what a listing costs anyway.
        tabulated-list-use-header-line nil)
  (tabulated-list-init-header)
  ;; The fake header the titles are drawn as is overlined and underlined by
  ;; default, which rules a buffer that has a rule in it already, at the
  ;; foot.  Bold says "these are the titles" on its own.
  (face-remap-add-relative 'tabulated-list-fake-header
                           '(:overline nil :underline nil :weight bold))
  ;; After `tabulated-list-init-header\=', which writes the header line itself
  ;; wherever it is still being used for the titles.
  ;;
  ;; What is written and how to stop it, and nothing else: the keys for the
  ;; rows are at the foot, where the rows are.  Somebody reading the top of
  ;; a buffer that is holding a change wants to know how to finish or get
  ;; out of it.
  (setq header-line-format
        (format "Proposed, not written.  Write %s, abort %s."
                (org-foresight-plan--key #'org-foresight-plan-apply "C-c C-c")
                (org-foresight-plan--key #'org-foresight-plan-abort "C-c C-k")))
  (when (and (boundp 'evil-state) (fboundp 'evil-emacs-state))
    (evil-emacs-state)))

;; Letters are commands here, and a modal editor's normal state is letters
;; meaning something else -- `d\=' deleting rather than dropping a proposal,
;; and RET doing nothing at all.  The same answer the rest of this kind of
;; buffer gives.
(when (fboundp 'evil-set-initial-state)
  (evil-set-initial-state 'org-foresight-plan-review-mode 'emacs))

(defun org-foresight-plan--key (command preferred)
  "Name PREFERRED when it still runs COMMAND here, else whatever key does.

Org writes `C-c C-c\=' and `C-c C-k\=' on every buffer that holds a change
until it is confirmed, and this is one, so those are the keys to name.  But
a configuration is free to move them, and a header line that stated them
regardless would be a printed lie at the top of the buffer."
  (if (eq (lookup-key org-foresight-plan-review-mode-map (kbd preferred))
          command)
      preferred
    ;; Read off this buffer\='s own map rather than out of whatever is active
    ;; where the question is asked: the header line is a statement about
    ;; this buffer, and the mode is not necessarily on yet when it is built.
    (if-let ((keys (where-is-internal command org-foresight-plan-review-mode-map
                                      t)))
        (key-description keys)
      (format "M-x %s" command))))

(defun org-foresight-plan--carried-note (candidate day)
  "Return the note saying CANDIDATE was written for a day before DAY, or \"\".

A day that has gone is folded onto today, so work promised for last Friday
is here among today\='s -- see `org-foresight--entry-schedule-gone-p\='.  It
is not today\='s in the same sense, and a list that cannot be told apart is
a list that quietly rewrites when each thing was promised: the one that has
been slipping for a fortnight looks exactly like the one accepted an hour
ago, and they are not the same decision."
  (let ((sched (plist-get candidate :scheduled)))
    (if (and sched (< (org-foresight--day-of sched day) 0))
        (format "for %s " (format-time-string "%-m/%-d" sched))
      "")))

(defun org-foresight-plan--refresh ()
  "Rebuild the review buffer from the pending proposals."
  (let* ((day (or org-foresight-plan--day (org-foresight--day-start 0)))
         (ends (org-foresight-work-ends day)))
    (setq tabulated-list-entries
        (append
         (mapcar
          (lambda (p)
            (list p (vector
                     (concat (format-time-string "%H:%M" (plist-get p :start))
                             "–"
                             (format-time-string "%H:%M" (plist-get p :end)))
                     (org-duration-from-minutes (plist-get p :effort))
                     (truncate-string-to-width
                      (replace-regexp-in-string "[\n\r]" " "
                                                (plist-get p :title))
                      44 0 ?\s)
                     ;; Say when a slot is longer than what was asked for, so
                     ;; a surprising length is explained where it appears.
                     (concat (org-foresight-plan--carried-note p day)
                             ;; Said where the row is, because a row that
                             ;; takes the evening is the one a reader is
                             ;; likeliest to want to decline -- and the only
                             ;; thing that separates it from the rest is an
                             ;; hour they would have to know the working day
                             ;; by heart to spot.
                             (if (and ends (not (time-less-p (plist-get p :start)
                                                             ends)))
                                 "after work " "")
                             (if (plist-get p :rejected) "skip" "")
                             (if (plist-get p :estimated) "" " est?")
                             (let ((raw (plist-get p :estimate))
                                   (adj (plist-get p :effort)))
                               (if (and raw (> (abs (- adj raw)) 1))
                                   (format " ←%s" (org-duration-from-minutes raw))
                                 ""))))))
          org-foresight-plan--placed)
         (mapcar
          (lambda (s)
            (list nil (vector
                       "—"
                       (org-duration-from-minutes (plist-get (car s) :effort))
                       (truncate-string-to-width
                        (replace-regexp-in-string "[\n\r]" " "
                                                  (plist-get (car s) :title))
                        44 0 ?\s)
                       (concat (org-foresight-plan--carried-note (car s) day)
                               (cdr s)))))
          org-foresight-plan--skipped))))
  (tabulated-list-print t)
  ;; After the print, which erases: the foot belongs to the buffer rather
  ;; than to the list, and the list is what was just rebuilt.
  (let ((inhibit-read-only t))
    (save-excursion
      ;; A badge on the body as well as on the foot.  The foot carries one,
      ;; and a page whose only badge is over its legend reads as a legend
      ;; with a list stuck above it.
      (goto-char (point-min))
      (insert (org-foresight-report--badge
               "Plan"
               (format "proposed for %s, nothing written yet"
                       (downcase (org-foresight--plan-day-name
                                  (or org-foresight-plan--day
                                      (org-foresight--day-start 0))))))
              "\n")
      (goto-char (point-max))
      (insert "\n" (org-foresight--legend 'plan)))))

(defun org-foresight-plan--set-rejected (rejected)
  "Mark the placement at point REJECTED or not, and step to the next line.

Down afterwards, as dired steps: a run of lines is then a run of presses.
A line carrying no proposal -- the titles, or one of the leftovers, which
is not going to be written whatever is done to it -- says so."
  (let ((p (or (tabulated-list-get-id)
               (user-error "No proposal on this line"))))
    (plist-put p :rejected rejected)
    (let ((line (line-number-at-pos)))
      (org-foresight-plan--refresh)
      (goto-char (point-min))
      (forward-line line))))

(defun org-foresight-plan-drop ()
  "Drop the placement at point from what will be written, and move down."
  (interactive)
  (org-foresight-plan--set-rejected t))

(defun org-foresight-plan-keep ()
  "Put the placement at point back among what will be written, and move down."
  (interactive)
  (org-foresight-plan--set-rejected nil))

(defun org-foresight-plan-redo ()
  "Propose again, from the files as they are now.

The answer to disagreeing with a time: go to the task, schedule it by hand,
and come back here.  The proposals in this buffer were worked out against
the files as they were when it opened, and the task you have just given a
time to is not work that still needs placing -- it is gone from the list on
the next pass, and the hour it was going to have is back in the budget for
whatever is still waiting.

Marks go, as they go on every listing that reads its data again: what they
pointed at may not be there any more."
  (interactive)
  (let ((day (or org-foresight-plan--day (org-foresight--day-start 0))))
    (if (org-foresight-plan--propose day)
        (progn
          (org-foresight-plan--refresh)
          (message "%s: %d placed, %d left over"
                   (org-foresight--plan-day-name day)
                   (length org-foresight-plan--placed)
                   (length org-foresight-plan--skipped)))
      ;; `--propose\=' has said why, and left what it had alone.  The buffer
      ;; would otherwise go on showing a plan for a day that has none.
      (setq org-foresight-plan--placed nil
            org-foresight-plan--skipped nil)
      (org-foresight-plan--refresh))))

(defun org-foresight-plan-abort ()
  "Leave without writing any of it."
  (interactive)
  (setq org-foresight-plan--placed nil
        org-foresight-plan--skipped nil)
  (quit-window)
  (message "Nothing written"))

(defun org-foresight-plan-goto ()
  "Visit the task at point."
  (interactive)
  (when-let* ((p (tabulated-list-get-id))
              (m (plist-get p :marker)))
    (pop-to-buffer (marker-buffer m))
    (goto-char m)
    (if (fboundp 'org-fold-show-entry)
        (org-fold-show-entry)
      (with-no-warnings (org-show-entry)))))

(defun org-foresight-plan--still-placeable-p (marker day)
  "Return non-nil when the entry at MARKER is still work DAY has to place."
  (and (markerp marker)
       (marker-buffer marker)
       (org-with-point-at marker
         (and (org-foresight--candidate-at-point day) t))))

(defun org-foresight-plan-apply ()
  "Write the accepted placements as timed SCHEDULED stamps.

Each entry is asked again, at the moment of writing, whether it is still
work that needs placing.  A buffer of proposals is read for minutes and
sometimes for longer, and the ordinary thing to do about a time you disagree
with is to go to the task and schedule it by hand -- after which this list
is out of date, and writing it out regardless would take back the time
somebody had just chosen.  What has moved on is left alone and counted."
  (interactive)
  (let ((n 0)
        (moved 0)
        (day (or org-foresight-plan--day (org-foresight--day-start 0))))
    (dolist (p org-foresight-plan--placed)
      (unless (plist-get p :rejected)
        (let ((m (plist-get p :marker)))
          (cond
           ((not (org-foresight-plan--still-placeable-p m day))
            (setq moved (1+ moved)))
           ((marker-buffer m)
            (with-current-buffer (marker-buffer m)
              (org-with-wide-buffer
               (goto-char m)
               ;; Logging off: this is the package placing work, not the user
               ;; putting it off, and `org-foresight--reschedule-count' must
               ;; not learn to count its own writes as procrastination.
               (let ((org-log-reschedule nil))
                 (org-schedule nil (format-time-string
                                    "%Y-%m-%d %a %H:%M"
                                    (plist-get p :start))))
               (setq n (1+ n)))))))))
    (dolist (file (org-agenda-files))
      (when-let ((buf (get-file-buffer file)))
        (with-current-buffer buf
          (when (buffer-modified-p) (save-buffer)))))
    (org-foresight--invalidate-signals)
    (quit-window)
    (if (> moved 0)
        (message "Placed %d task%s; %d had moved on and %s left alone"
                 n (if (= n 1) "" "s") moved (if (= moved 1) "was" "were"))
      (message "Placed %d task%s" n (if (= n 1) "" "s")))))

(defun org-foresight--plan-day-name (day)
  "Say which day DAY is, as a message would name it."
  (if (equal day (org-foresight--day-start 0))
      "Today"
    (format-time-string "%a %-d %b" day)))

(defun org-foresight-plan--propose (day)
  "Work out the proposals for DAY.  Return non-nil when there are any.

Says why not, and changes nothing, when there are none: a day with no
working hours in it, a day whose headroom is already spent, or a file with
nothing in it left to place."
  (let* ((day (or day (org-foresight--day-start 0)))
         (scan (org-foresight-scan 1 day))
         (cap (org-foresight-capacity day scan))
         ;; Where the work would actually go: the working hours first, and
         ;; only then the evening -- the same list, in the same order, that
         ;; `:lands\=' and `:overflow-min\=' are read off.  Taking only the
         ;; working hours made this command disagree with the line above it
         ;; on the same page: the day reported that three hours would be
         ;; finished by ten past seven and that none of it overflowed, and
         ;; this refused to place a minute of it for want of budget.  An
         ;; evening the arithmetic has already spent is an evening a plan may
         ;; propose; whether to spend it is the reader\='s call, and `d\=' is
         ;; how they decline.
         (free (org-foresight--run-out-intervals day scan))
         (budget (- (/ (org-foresight--intervals-seconds free) 60.0)
                    (plist-get cap :reserve-min))))
    ;; Nil in every branch that proposes nothing, and said out loud in each.
    ;; `message\=' answers with the line it printed, so a branch that ended on
    ;; one would report a proposal it had not made.
    (cond
     ((null (plist-get cap :work))
      (message "%s is not a working day" (org-foresight--plan-day-name day))
      nil)
     ((<= budget 0)
      (message "No time left %s: %s free, %s reserved for interruptions"
               (downcase (org-foresight--plan-day-name day))
               (org-duration-from-minutes
                (/ (org-foresight--intervals-seconds free) 60.0))
               (org-duration-from-minutes (plist-get cap :reserve-min)))
      nil)
     (t
      (pcase-let* ((candidates (org-foresight--candidates day))
                   (`(,placed . ,skipped)
                    (org-foresight--place candidates free budget)))
        (cond
         ((null candidates)
          (message "Nothing %s has to place"
                   (downcase (org-foresight--plan-day-name day)))
          nil)
         (t
          (setq org-foresight-plan--placed placed
                org-foresight-plan--skipped skipped
                org-foresight-plan--day day)
          t)))))))

;;;###autoload
(defun org-foresight-plan-fill (&optional day)
  "Propose times for work that has been accepted but not yet placed.

Fills what is left of DAY with unscheduled tasks, nearest deadline first,
after taking out the surge reserve -- so the plan it proposes is one that
survives an ordinary number of interruptions rather than one that only works
if nothing happens.

Fills the day under the cursor, and today from anywhere that is not an
agenda line.  The day that wants planning is often not today -- tomorrow is
planned the evening before -- and pressing the key while looking at it is
how anybody would expect to say which.

Nothing is written here.  The proposals open in a review buffer; \\`d' drops
one, \\`C-c C-c' writes the rest."
  (interactive (list (org-foresight--day-at-point)))
  (when (org-foresight-plan--propose (or day (org-foresight--day-start 0)))
    (let ((buf (get-buffer-create "*Foresight Plan*")))
      (with-current-buffer buf
        (org-foresight-plan-review-mode)
        (org-foresight-plan--refresh))
      (pop-to-buffer buf)
      ;; The counts only.  Which key does what is on the buffer now, at the
      ;; top and at the foot, and an echo-area line that repeats it is one
      ;; more thing to read before the list itself.
      (message "%s: %d placed, %d left over"
               (org-foresight--plan-day-name org-foresight-plan--day)
               (length org-foresight-plan--placed)
               (length org-foresight-plan--skipped)))))

(provide 'org-foresight-plan)

;;; org-foresight-plan.el ends here
