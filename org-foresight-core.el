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

(defun org-foresight--day-start (&optional day-offset)
  "Return the Emacs time value for local midnight, DAY-OFFSET days back."
  (let ((d (decode-time (current-time))))
    (encode-time 0 0 0 (- (nth 3 d) (or day-offset 0)) (nth 4 d) (nth 5 d))))

(defun org-foresight--clock-charge-task (table cat minutes)
  "Add MINUTES under CAT to TABLE for the entry point is inside.

Keyed on the heading's position, so a drawer holding several CLOCK lines is
one task rather than several.  The heading's own facts -- what it is called,
what state it is in, what it was estimated at -- are read once, the first time
that heading is seen."
  (let* ((head (save-excursion (org-back-to-heading t) (point)))
         (key (cons (current-buffer) head))
         (task (gethash key table)))
    (if task
        (plist-put task :minutes (+ minutes (plist-get task :minutes)))
      (puthash key
               (save-excursion
                 (goto-char head)
                 (list :title (org-get-heading t t t t)
                       :category cat
                       :todo (org-get-todo-state)
                       :effort (org-foresight--duration-minutes
                                (org-entry-get (point) "EFFORT"))
                       :marker (point-marker)
                       :minutes minutes))
               table))))

(defun org-foresight-clock-scan (days)
  "Scan `org-agenda-files' LOGBOOK CLOCK lines over the last DAYS days
\(today inclusive) in one pass.  A running clock (no end timestamp) is
closed at `current-time', so its elapsed-so-far time always counts -- every
consumer built on this plist agrees on whether \"now\" is included, unlike
the three separate hand-rolled scans this replaces.  Return a plist:
:rows           (CATEGORY . MINUTES) alist for the whole window, desc
:total          whole-window total minutes
:byday          DAYS-length vector of per-day minutes, index 0 = oldest
:days           DAYS
:today-rows     (CATEGORY . MINUTES) alist for today only, desc
:today-total    today's total minutes
:today-segments today's clock-segment count (fragmentation)
:today-intervals  today's (START . END) time conses
:today-tasks    plists (:title :category :todo :effort :marker :minutes) for
                every entry clocked today, desc by minutes.  EFFORT is the
                estimate in minutes or nil; MARKER points at the heading, so a
                row built from one of these answers to the agenda's commands
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
         (now (current-time))
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
         today-intervals
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
                         (org-foresight--clock-charge-task
                          today-tasks cat today-dur))))))))))))
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
            :today-tasks (seq-sort-by (lambda (e) (plist-get e :minutes)) #'> tasks)
            :intervals-byday intervals-byday))))

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
         (placed (seq-filter (lambda (e)
                               (and (plist-get e :start)
                                    (plist-get e :place)
                                    ;; Somebody else going somewhere is not a
                                    ;; journey of yours to make.
                                    (not (eq (plist-get e :attention)
                                             'informational))))
                             ledger))
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
    (when (and work (not (eq base here)))
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
    (dolist (e placed)
      (let ((there (plist-get e :place)))
        (unless (eq there here)
          (let ((mins (org-foresight--travel-minutes here there)))
            (when (> mins 0)
              (let ((leg (org-foresight--travel-slot
                          (plist-get e :start) mins taken since off)))
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
                       (place (org-foresight--entry-place))
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
                              (push (list :kind (if todo 'task 'meeting)
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
      (let ((day-time (time-add from0 (days-to-time i))))
        (dolist (tb (org-foresight--travel-blocks
                     day-time
                     (sort (copy-sequence (aref ledger i))
                           (lambda (a b)
                             (let ((sa (plist-get a :start))
                                   (sb (plist-get b :start)))
                               (and sa sb (time-less-p sa sb)))))))
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

(defcustom org-foresight-private-categories nil
  "CATEGORY values whose entries are private commitments, not work.
They occupy the day but never count against work capacity."
  :type '(repeat string)
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

(defun org-foresight--day-property (day name)
  "Return property NAME from DAY's own heading in the day tree, or nil."
  (let ((file (org-foresight--places-file)))
    (when (and file (file-exists-p file))
      (with-current-buffer (find-file-noselect file)
        (org-with-wide-buffer
         (goto-char (point-min))
         (when (re-search-forward
                (concat "^\\*+ +"
                        (regexp-quote (format-time-string "%Y-%m-%d" day)))
                nil t)
           (org-entry-get (point) name)))))))

(defun org-foresight--shape-table ()
  "Return the shape cache, emptied whenever the day file has changed."
  (let* ((file (org-foresight--places-file))
         (buf (and file (file-exists-p file) (find-file-noselect file)))
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
  (let* ((wake (or (org-foresight--day-property day "WAKE")
                   (car org-foresight-awake)))
         (sleep (or (org-foresight--day-property day "SLEEP")
                    (cdr org-foresight-awake)))
         (raw (org-foresight--day-property day "WORK"))
         (work
          (cond
           (raw (org-foresight--parse-ranges raw))  ; "none" parses to nothing
           ((memq (nth 6 (decode-time day)) org-foresight-workdays)
            org-foresight-work)
           (t nil)))
         (place (or (org-foresight--day-property day "PLACE")
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
         (idx (org-foresight--day-of day (plist-get scan :from)))
         (ledger (and (>= idx 0) (< idx (plist-get scan :days))
                      (aref (plist-get scan :ledger) idx)))
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
                                                        '(meeting task travel))
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

;;;###autoload
(defun org-foresight-shape-day (&optional day)
  "Declare the shape of DAY (today by default) on its own heading.

Asks when the day starts and ends and how much of it is work, and writes the
answers to DAY's heading in the day tree, where they can also be edited by
hand.  Only exceptional days need this: a day nobody has said anything about
simply takes the defaults.

Specific private commitments are not entered here -- they are ordinary Org
entries in a category listed in `org-foresight-private-categories', so that a
dentist appointment lives where every other appointment lives."
  (interactive)
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

(defun org-foresight-day-place (day)
  "Return the place DAY is worked from.

Never nil: a day nobody has declared is worked from
`org-foresight-home-place'.  What this answers is the question no entry can
-- where the body is that day -- which is what makes \"I am here now and will
not be again until Wednesday\" a thing the day can say."
  (plist-get (org-foresight-day-shape day) :place))

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

(defun org-foresight-capacity (day &optional scan now)
  "Return a plist describing how much of DAY may still be promised.

Within the work span, these divide it exactly:

  :span-min   the whole span in minutes
  :booked-min meetings and work already placed at a time
  :travel-min getting to and from them
  :private-min-in-span  life that happens to fall in working hours
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

  :off-min        the waking day less the work span
  :private-min    commitments that are life rather than work
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

The two groups answer different questions and must not be mixed: the first
describes the day that was planned, the second what can still be promised.

NOW defaults to the current time; passing it makes the whole calculation
reproducible, which is what lets this be tested at all."
  (let* ((now (or now (current-time)))
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
         (rest (/ (org-foresight--intervals-seconds
                   (org-foresight--intervals-remaining work now))
                  60.0))
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
         (booked 0.0) (travel 0.0) (private 0.0) (private-in 0.0)
         (borrowed 0.0) (grey 0.0))
    (dolist (b bands)
      (let ((mins (/ (float-time (time-subtract (plist-get b :end)
                                                (plist-get b :start)))
                     60.0)))
        (pcase (plist-get b :kind)
          ('grey (setq grey (+ grey mins)))
          ;; A private commitment is not work and not empty time; without a
          ;; figure of its own it vanished from the account entirely, leaving
          ;; hours that were spoken for looking free.  One that falls in
          ;; working hours is counted separately, because it is time the day
          ;; cannot spend on work and must not be handed back as spare.
          ('private
           (setq private (+ private mins))
           (when (org-foresight--within-p (plist-get b :start) (plist-get b :end)
                                          work)
             (setq private-in (+ private-in mins))))
          ('travel (if (plist-get b :borrowed)
                       (setq borrowed (+ borrowed mins))
                     (setq travel (+ travel mins))))
          ((or 'meeting 'task)
           (if (plist-get b :borrowed)
               (setq borrowed (+ borrowed mins))
             (setq booked (+ booked mins))))
          (_ nil))))
    (list :work work
          :span-min span
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
          :spare-min (- span booked travel private-in committed reserve)
          :headroom-min (- free-min needed)
          :lands (org-foresight--pour run-out needed)
          :overflow-min (max 0.0 (- needed
                                    (/ (org-foresight--intervals-seconds run-out)
                                       60.0)))
          :off-min (- (/ (float-time (time-subtract (cdr awake) (car awake)))
                         60.0)
                      span)
          :private-min (- private private-in)
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
