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
  "Minutes of daily leak above which the leak itself is the problem.

Leak is time at the keyboard with no clock running: work that happened and
went unrecorded, or the small unnamed handling a day fills with.  Left alone
it grows, and every future day is planned with that much less in it -- so
past a point the answer is not a bigger allowance but a look at where the
hour goes."
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
         orphan-candidates undecided in-flight unreadable)
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
                     (title (org-get-heading t t t t))
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
                           (null (org-entry-get (point) "PLAN_PREP"))
                           (org-foresight--entry-has-future-time-p stamps now))
                  (push (org-foresight--finding
                         title
                         (format "needs %s + %s"
                                 org-foresight-meeting-prep
                                 org-foresight-meeting-follow))
                        meetings))
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
                (when-let ((ref (org-entry-get (point) "PLAN_MEETING_UID")))
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
      (list
       :here (org-foresight--here-sort (nreverse here))
       ;; Document order, which the level stack above depended on and
       ;; `org-foresight--project-classify\=' depends on in turn.
       :headings (nreverse records)
       ;; Kept beside the rows it decided, so the section that shows them can
       ;; head itself without a second scan of every file for an answer this
       ;; pass already had.
       :places places
       :signals
       (seq-filter
       #'cdr
       (list (cons "Impossible (travel clashes with a meeting)"
                   (org-foresight--clash-findings scan))
             (cons "Meetings without prep" (nreverse meetings))
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
             (cons "Orphaned prep" orphans)))))))

(defun org-foresight--undecided-p (todo stamps)
  "Non-nil when the entry at point was captured but never decided about.

Deliberately narrow.  Anything with a state, a date, a clock, a child or a
timestamp in its own heading is already being handled, and a date-tree
heading is scaffolding rather than a thought.  What is left is a heading
someone wrote down and walked away from."
  (and (null todo)
       (null stamps)
       (not (save-excursion (org-goto-first-child)))
       ;; A timestamp written into the heading itself is an appointment.
       (not (string-match-p org-ts-regexp-both (org-get-heading t t nil nil)))
       ;; `*** 2026-08-11 Tuesday' and friends are structure, not capture.
       (not (string-match-p "\\`[0-9]\\{4\\}\\(-[0-9]\\{2\\}\\)\\{0,2\\}\\b"
                            (org-get-heading t t t t)))
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
  (let ((leak (org-foresight-leak-minutes)))
    (when (and (org-foresight-leak-samples) (> leak org-foresight-leak-warn))
      (list (list :file nil :point nil :marker nil
                  :title "Time worked without a clock"
                  :note (format "%s/day goes unrecorded"
                                (org-duration-from-minutes leak)))))))

;;;; The board

(defconst org-foresight-signal-commands
  '(("Meetings without prep" . org-foresight-prepare-meetings))
  "Signal groups a single command can settle, and which command that is.

Most groups are fixed one row at a time, on the entry the row points at: an
estimate is typed where the work is, and a task that keeps moving is a
decision nobody else can make.  The few that are not have to be said out
loud -- a board that names a problem and not the thing that answers it sends
its reader off to find one, and a reader who has to go looking stops reading.")

(defconst org-foresight-signal-kinds
  '(("Impossible (travel clashes with a meeting)"     . fix)
    ("Meetings without prep"                          . fix)
    ("Unreadable estimate (breaks the agenda itself)" . fix)
    ("Won't fit today"                                . fix)
    ("Too big for one sitting (needs breaking up)"    . fix)
    ("Unplannable (deadline, no estimate)"            . fix)
    ("Cannot be done from here"                       . fix)
    ("Undecided (captured, not decided)"              . fix)
    ("Orphaned prep"                                  . fix)
    ("Gone quiet (follow-up overdue)"                 . owed)
    ("Kept moving (not really NEXT)"                  . owed)
    ("Too much in flight"                             . owed)
    ("Outside work hours (invisible to capacity)"     . fact)
    ("Borrowed from private time"                     . fact)
    ("Leaking (unclocked work)"                       . fact))
  "What kind of thing each signal is, and so whether emptying it is the point.

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

(defun org-foresight-report--signal-group (group)
  "Return one signal GROUP: its heading, and a row per finding."
  (concat
          ;; A group heading belongs to the badge above it, so it sits at the
          ;; margin rather than at the frame edge: only a badge is outdented,
          ;; or an eye running down the left edge stops finding sections.
          (org-foresight-report--indent
           (concat
            (propertize (format "%s (%d)" (car group) (length (cdr group)))
                        'face 'org-agenda-structure)
            (when-let ((command (cdr (assoc (car group)
                                            org-foresight-signal-commands))))
              (propertize
               (concat " · " (org-foresight-plan--command-hint command))
               'face 'shadow))))
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
           (cdr group) "\n")))

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
        (let ((landing (org-foresight-landing))
              (signals (org-foresight-signals)))
        (insert (org-foresight-report--badge
                 "Board" "what settled would look like, and how far off it is")
                "\n\n"
                (org-foresight-report--indent
                 (org-foresight-plan--board-verdict landing signals))
                "\n\n"
                (org-foresight-report--badge
                 "Here" "what only this place can do")
                "\n\n"
                (org-foresight-report-here)
                "\n\n"
                (org-foresight-report--badge
                 "Landing" "what has a date, and whether it will be met")
                "\n\n"
                (org-foresight-report-landing landing)
                "\n\n"
                (org-foresight-report--badge
                 "Signals" "everything unsettled, the fixable part first")
                "\n\n"
                (org-foresight-report-signals signals)))
        (put-text-property (point-min) (point-max) 'org-agenda-type 'agenda)
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
  (let ((journey (org-get-at-bol 'org-foresight-journey)))
    (unless journey
      (user-error "No derived journey on this line"))
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

(defun org-foresight--clock-gaps (behind)
  "Return BEHIND's unrecorded stretches as (INTERVAL . KIND), earliest first.

KIND is `unclocked' or `away'.  It is the only thing that tells one stretch
from another, and it is worth carrying because the two are remembered
differently: what you were doing at the keyboard and what you were doing away
from it are not recalled by the same kind of effort."
  (let ((least (* 60 org-foresight-clock-fill-minimum)))
    (seq-sort-by
     (lambda (gap) (float-time (car (car gap)))) #'<
     (seq-filter
      (lambda (gap)
        (>= (float-time (time-subtract (cdr (car gap)) (car (car gap)))) least))
      (append (mapcar (lambda (iv) (cons iv 'unclocked))
                      (plist-get behind :unclocked-ivs))
              (mapcar (lambda (iv) (cons iv 'away))
                      (plist-get behind :away-ivs)))))))

(defun org-foresight--clock-gap-label (gap)
  "Return GAP as one line: when it ran, how long it was, and which kind."
  (let ((iv (car gap)))
    (format "%s-%s  %s  (%s)"
            (format-time-string "%H:%M" (car iv))
            (format-time-string "%H:%M" (cdr iv))
            (org-duration-from-minutes
             (/ (float-time (time-subtract (cdr iv) (car iv))) 60.0))
            (if (eq (cdr gap) 'away) "away" "at the keyboard"))))

(defun org-foresight--clock-fill-candidates (clock &optional scan)
  "Return (TITLE . MARKER) for the work today already knows about.

Two sources, because they miss different things.  What has been clocked today
is where an interrupted task is found: it is on the list already and only
wants its missing half.  The day's own entries are where a task that was
never clocked at all is found -- the commoner case by far, and the one a list
built from the clock alone can never offer.

Neither is required.  A stretch that went on something nobody had written
down is the whole reason the day has holes in it, and a prompt that refused
to accept one would send its answer somewhere else."
  (let* ((scan (or scan (org-foresight-scan 1 (org-foresight--day-start 0))))
         (ledger (org-foresight-scan-day scan :ledger
                                         (org-foresight--day-start 0)))
         (out nil))
    (dolist (task (plist-get clock :today-tasks))
      (when-let ((marker (plist-get task :marker)))
        (push (cons (plist-get task :title) marker) out)))
    (dolist (entry ledger)
      (when-let ((marker (plist-get entry :marker))
                 (title (plist-get entry :title)))
        (unless (assoc title out)
          (push (cons title marker) out))))
    (nreverse out)))

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
(defun org-foresight-clock-fill ()
  "Say what an unrecorded stretch of today was spent on.

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
today when it is not."
  (interactive)
  (let* ((day (org-foresight--day-start 0))
         (clock (org-foresight-clock-scan 7))
         (behind (org-foresight-behind
                  day clock (org-foresight-observe-coverage clock)))
         (gaps (org-foresight--clock-gaps behind)))
    (unless gaps
      (user-error "Nothing today is unrecorded for longer than %d minutes"
                  org-foresight-clock-fill-minimum))
    (let* ((choices (mapcar (lambda (gap)
                              (cons (org-foresight--clock-gap-label gap) gap))
                            gaps))
           (gap (cdr (assoc (completing-read "Unrecorded: " choices nil t nil
                                             nil (car (car choices)))
                            choices)))
           (from (car (car gap)))
           (to (cdr (car gap)))
           (known (org-foresight--clock-fill-candidates clock))
           (title (completing-read "What were you doing? " (mapcar #'car known)))
           (marker (cdr (assoc title known))))
      (when (string-empty-p (string-trim title))
        (user-error "Nothing named, nothing written"))
      (if marker
          (org-foresight--file-clocked marker from to)
        (org-foresight--file-clocked-entry
         title from to (y-or-n-p "Arrived unplanned? ")))
      (org-foresight--invalidate-signals)
      (when (derived-mode-p 'org-agenda-mode) (org-agenda-redo))
      (message "Clocked %s, %s-%s" title
               (format-time-string "%H:%M" from)
               (format-time-string "%H:%M" to)))))

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
        (message "No meetings are missing preparation")
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
     (let* ((title (org-get-heading t t t t))
            (stamps (org-foresight--entry-timestamps))
            (el (seq-find #'org-foresight--ts-timed-p stamps)))
       (when el
         (let* ((uid (or (org-entry-get (point) "UID") (org-id-get-create)))
                (start (org-foresight--ts-start el))
                (end (org-foresight--ts-end el))
                (slots (org-foresight--meeting-slots start end))
                (props (list (cons "PLAN_MEETING_UID" uid))))
           (org-foresight--file-task
            (format "Prep: %s" title) (car slots)
            org-foresight-meeting-prep props)
           (org-foresight--file-task
            (format "Follow up: %s" title) (cdr slots)
            org-foresight-meeting-follow props)
           (org-entry-put (point) "PLAN_PREP" "t")
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
        (setq title (org-get-heading t t t t)
              already (org-entry-get (point) "PLAN_PREP")))
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

(defun org-foresight--candidate-at-point (day)
  "Return a placement candidate for the entry at point, or nil.
Placeable means: open, not already pinned to a time, and either unscheduled
or scheduled for DAY without one -- work that has been accepted but not yet
given a place to happen."
  (unless (org-entry-is-done-p)
    (let* ((todo (org-get-todo-state))
           (sched (org-get-scheduled-time (point)))
           (timed (seq-some #'org-foresight--ts-timed-p
                            (org-foresight--entry-timestamps))))
      (when (and todo
                 (not (member todo org-foresight-followup-keywords))
                 (not timed)
                 (or (null sched)
                     (= (org-foresight--day-of sched day) 0)))
        (let* ((raw (org-foresight--entry-effort-minutes))
               (category (org-entry-get (point) "CATEGORY" t))
               (factor (org-foresight-bias-factor category raw)))
          (list :marker (point-marker)
                :title (org-get-heading t t t t)
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

(defvar org-foresight-plan-review-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "d") #'org-foresight-plan-toggle-reject)
    (define-key map (kbd "u") #'org-foresight-plan-toggle-reject)
    (define-key map (kbd "RET") #'org-foresight-plan-goto)
    (define-key map (kbd "C-c C-c") #'org-foresight-plan-apply)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `org-foresight-plan-review-mode'.")

(define-derived-mode org-foresight-plan-review-mode tabulated-list-mode
  "Foresight-Plan"
  "Review proposed placements before any of them are written.

\\{org-foresight-plan-review-mode-map}"
  (setq tabulated-list-format
        [("When" 13 nil) ("Effort" 7 nil) ("Task" 44 nil) ("Note" 12 nil)]
        tabulated-list-padding 2)
  (tabulated-list-init-header))

(defun org-foresight-plan--refresh ()
  "Rebuild the review buffer from the pending proposals."
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
                     (concat (if (plist-get p :rejected) "skip" "")
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
                       (cdr s))))
          org-foresight-plan--skipped)))
  (tabulated-list-print t))

(defun org-foresight-plan-toggle-reject ()
  "Drop the placement at point from what will be written, or put it back."
  (interactive)
  (when-let ((p (tabulated-list-get-id)))
    (plist-put p :rejected (not (plist-get p :rejected)))
    (org-foresight-plan--refresh)))

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

(defun org-foresight-plan-apply ()
  "Write the accepted placements as timed SCHEDULED stamps."
  (interactive)
  (let ((n 0))
    (dolist (p org-foresight-plan--placed)
      (unless (plist-get p :rejected)
        (let ((m (plist-get p :marker)))
          (when (marker-buffer m)
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
               (setq n (1+ n))))))))
    (dolist (file (org-agenda-files))
      (when-let ((buf (get-file-buffer file)))
        (with-current-buffer buf
          (when (buffer-modified-p) (save-buffer)))))
    (org-foresight--invalidate-signals)
    (quit-window)
    (message "Placed %d task%s" n (if (= n 1) "" "s"))))

;;;###autoload
(defun org-foresight-plan-fill (&optional day)
  "Propose times for work that has been accepted but not yet placed.

Fills what is left of DAY (today by default) with unscheduled tasks, nearest
deadline first, after taking out the surge reserve -- so the plan it proposes
is one that survives an ordinary number of interruptions rather than one that
only works if nothing happens.

Nothing is written here.  The proposals open in a review buffer; \\`d' drops
one, \\`C-c C-c' writes the rest."
  (interactive)
  (let* ((day (or day (org-foresight--day-start 0)))
         (scan (org-foresight-scan 1 day))
         (cap (org-foresight-capacity day scan))
         (free (plist-get cap :free))
         (budget (- (plist-get cap :free-min) (plist-get cap :reserve-min))))
    (cond
     ((null (plist-get cap :work))
      (message "Not a working day"))
     ((<= budget 0)
      (message "No headroom today: %s free, %s reserved for interruptions"
               (org-duration-from-minutes (plist-get cap :free-min))
               (org-duration-from-minutes (plist-get cap :reserve-min))))
     (t
      (pcase-let* ((candidates (org-foresight--candidates day))
                   (`(,placed . ,skipped)
                    (org-foresight--place candidates free budget)))
        (if (null candidates)
            (message "Nothing to place")
          (setq org-foresight-plan--placed placed
                org-foresight-plan--skipped skipped)
          (let ((buf (get-buffer-create "*Foresight Plan*")))
            (with-current-buffer buf
              (org-foresight-plan-review-mode)
              (org-foresight-plan--refresh))
            (pop-to-buffer buf)
            (message
             "%d placed, %d left over · d to drop, C-c C-c to write"
             (length placed) (length skipped)))))))))

(provide 'org-foresight-plan)

;;; org-foresight-plan.el ends here
