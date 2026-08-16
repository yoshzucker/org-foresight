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

(defun org-foresight-signals (&optional force)
  "Return an alist (LABEL . FINDINGS) of work that exists but is not planned.
Cached for `org-foresight-signals-cache-ttl' seconds unless FORCE.

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
      (let ((result (org-foresight--signals-compute)))
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

(defun org-foresight--signals-compute ()
  "Walk the agenda files and return the signals.

One pass, in the same spirit as the other scans here: every signal is a
different reading of the same walk, so asking for all of them costs no more
than asking for one."
  (let* ((now (current-time))
         (today (org-foresight--day-start 0))
         (horizon (time-add today (days-to-time org-foresight-horizon-days)))
         (uids (make-hash-table :test 'equal))
         (scan (org-foresight-scan 1 today))
         (base (org-foresight-day-place today))
         here elsewhere
         meetings procrastinated unplannable followups outside-work
         orphan-candidates undecided in-flight unreadable)
    (dolist (file (org-agenda-files))
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (org-with-wide-buffer
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
                     ((and place (eq place base))
                      (push (list :title title :marker (point-marker)
                                  :deadline dead :people (org-foresight--entry-people))
                            here))
                     ;; The mirror image: work put on today that today cannot
                     ;; do.  A home day with an office errand on it is a plan
                     ;; that will not survive contact with the morning.
                     ((and place sched
                           (= 0 (org-foresight--day-of sched today)))
                      (push (org-foresight--finding
                             title (format "needs %s; today is %s" place base))
                            elsewhere)))))
                ;; (b) A decision that keeps not being made.
                (when (and todo (not done))
                  (let ((n (org-foresight--reschedule-count)))
                    (when (>= n org-foresight-procrastination-threshold)
                      (push (org-foresight--finding
                             title (format "rescheduled %d times" n))
                            procrastinated))))
                ;; (c) A deadline that cannot be planned for.
                (when (and todo (not done) dead (null effort)
                           (time-less-p dead horizon))
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
            nil nil)))))
    ;; Orphans can only be judged once every UID in the agenda has been seen.
    (let ((orphans (seq-keep (lambda (c)
                               (unless (gethash (car c) uids) (cdr c)))
                             orphan-candidates)))
      (list
       :here (org-foresight--here-sort (nreverse here))
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
             (cons "Won't fit today" (org-foresight--wont-fit-findings scan))
             (cons "Unplannable (deadline, no estimate)" (nreverse unplannable))
             (cons "Gone quiet (follow-up overdue)" (nreverse followups))
             (cons "Kept moving (not really NEXT)" (nreverse procrastinated))
             (cons "Too much in flight"
                   (if (> (length in-flight) org-foresight-wip-limit)
                       (nreverse in-flight)
                     nil))
             (cons "Borrowed from private time" (org-foresight--borrow-findings))
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
  (let ((ledger (aref (plist-get scan :ledger) 0))
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

(defun org-foresight--wont-fit-findings (scan &optional now)
  "Return findings for work promised today that no remaining gap can hold.
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
         out)
    (dolist (e (and work (aref (plist-get scan :ledger) 0)))
      (when (and (eq (plist-get e :kind) 'promised)
                 (> (or (plist-get e :effort-adj) (plist-get e :effort)) longest))
        (push (list :file nil :point nil
                    :marker (plist-get e :marker)
                    :title (plist-get e :title)
                    :note (format "needs %s, longest gap %s"
                                  (org-foresight-report--effort-run e)
                                  (org-duration-from-minutes longest)))
              out)))
    (nreverse out)))

(defun org-foresight--borrow-findings ()
  "Return a finding when this week has taken too much from private time."
  (let ((total 0.0)
        (days 0))
    (dotimes (i 7)
      (let* ((day (org-foresight--day-start i))
             (cap (ignore-errors (org-foresight-capacity day))))
        (when-let ((borrowed (and cap (plist-get cap :borrowed-min))))
          (when (> borrowed 0)
            (setq total (+ total borrowed) days (1+ days))))))
    (when (> total org-foresight-borrow-warn)
      (list (list :file nil :point nil :marker nil
                  :title "Work in private time"
                  :note (format "%s over %d day(s) this week"
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

(defun org-foresight-report-signals (&optional signals)
  "Return the signal blocks, or a note when nothing is outstanding."
  (let ((signals (or signals (org-foresight-signals))))
    (if (null signals)
        (propertize "(nothing unaccounted for)" 'face 'org-table)
      (mapconcat
       (lambda (group)
         (concat
          ;; A group heading belongs to the badge above it, so it sits at the
          ;; margin rather than at the frame edge: only a badge is outdented,
          ;; or an eye running down the left edge stops finding sections.
          (org-foresight-report--indent
           (propertize (format "%s (%d)" (car group) (length (cdr group)))
                       'face 'org-agenda-structure))
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
       signals "\n\n"))))

(defconst org-foresight-here-urgent "⚠"
  "The mark for work whose need falls before you are next in this place.")

(defun org-foresight-report-here (&optional rows day)
  "Return the work only DAY's place can do, and when that place comes again.

The heading is the whole point of the section: \"next at the office on
Wednesday\" is what turns a list of errands into a decision, because it says
what the alternative to doing it now actually costs."
  (let* ((day (or day (org-foresight--day-start 0)))
         (rows (or rows (org-foresight-here)))
         (base (org-foresight-day-place day))
         (next (org-foresight-next-day-at base day)))
    (if (null rows)
        (propertize (format "(nothing that only %s can do)" base) 'face 'org-table)
      (concat
       (org-foresight-report--indent
        (propertize
         (if next
             (format "%s · next %s day is %s" base base
                     (format-time-string "%a %m-%d" next))
           (format "%s · not %s again within the horizon" base base))
         'face 'org-agenda-structure))
       "\n"
       (mapconcat
        (lambda (r)
          (let* ((dead (plist-get r :deadline))
                 ;; Urgent means the deadline lands before you are next here:
                 ;; the place, not the clock, is what runs out.
                 (urgent (and dead (or (null next) (time-less-p dead next))))
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
                      (concat
                       (if dead
                           (format "due %s" (format-time-string "%a %m-%d" dead))
                         "")
                       (when people
                         (format "%s(%s)" (if dead " " "")
                                 (string-join people ", "))))
                      'face 'shadow))
             (plist-get r :marker))))
        rows "\n")))))

(add-hook 'org-foresight-report-invalidate-functions
          #'org-foresight--invalidate-signals)

;;;###autoload
(defun org-foresight-board (&optional _match)
  "Show what has not been settled: what only here can do, and what is unplanned.

Not an agenda view.  The day has one of those and it is the day; this is the
other question, and it is not about the timeline at all -- which is why it
stopped being a second copy of the agenda with a different tail underneath.

Two sections.  The first is the one that decides whether you can walk out:
work this place, and only this place, can do, with the day that place comes
round again.  The second is everything that exists and has not been planned
for.

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
        (insert (org-foresight-report--badge
                 "Here" "what only this place can do")
                "\n\n"
                (org-foresight-report-here)
                "\n\n"
                (org-foresight-report--badge
                 "Signals" "work that exists but is not planned")
                "\n\n"
                (org-foresight-report-signals))
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

(defun org-foresight-plan--verdict-line ()
  "Return a one-line summary of what is unsettled, or nil when nothing is.

The daily agenda otherwise gives no hint that anything is outstanding, and a
signal nobody is prompted to look at is not really being caught -- so the
line names the way to look.

Work bound to where you are is counted separately even though the board holds
both.  It is the one kind that stops being possible when you stand up, so a
number that folded it into the rest would be a number that says the same
thing at half past nine and at half past five."
  (let ((n (apply #'+ (mapcar (lambda (g) (length (cdr g)))
                              (org-foresight-signals))))
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
       (let ((level 1))
         (when org-foresight-task-datetree
           (org-datetree-find-date-create
            (calendar-gregorian-from-absolute (time-to-days when)))
           (setq level (1+ (org-current-level)))
           ;; Past the day's existing entries, not above them: filing two
           ;; related tasks in one go should leave them in the order they
           ;; were written, and each one landing on top reverses that.
           (org-end-of-subtree t t))
         (unless org-foresight-task-datetree
           (goto-char (point-max)))
         (unless (bolp) (insert "\n"))
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
            (let ((marker (plist-get f :marker)))
              (with-current-buffer (marker-buffer marker)
                (org-with-wide-buffer
                 (goto-char marker)
                 (let* ((title (org-get-heading t t t t))
                        (uid (or (org-entry-get (point) "UID")
                                 (org-id-get-create)))
                        (stamps (org-foresight--entry-timestamps))
                        (el (seq-find #'org-foresight--ts-timed-p stamps)))
                   (when el
                     (let* ((start (org-foresight--ts-start el))
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
                       (setq n (1+ n)))))))))
          (message "Prepared %d meeting(s)" n))))))

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
