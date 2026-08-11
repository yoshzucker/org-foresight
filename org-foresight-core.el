;;; org-foresight-core.el --- Capacity model  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yoshzucker

;; Author: yoshzucker
;; URL: https://github.com/yoshzucker/org-foresight
;; Package-Requires: ((emacs "28.1") (org "9.6"))

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
;;   capacity          window - busy - committed - surge, and when that runs out
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
(require 'seq)
(require 'cl-lib)

(defgroup org-foresight nil
  "Forward-looking capacity, signals and scheduling for Org."
  :group 'org
  :prefix "org-foresight-")

;;;; Classification

(defvar org-foresight-app-categories
  '(("work" . ("Emacs" "Ghostty" "Terminal" "iTerm2" "Code" "Xcode"
               "プレビュー" "Preview" "Claude" "Grok" "ActivityWatch"))
    ("comms" . ("メール" "Mail" "カレンダー" "Calendar" "Slack"
                "メッセージ" "Messages" "Zoom"))
    ("distraction" . ("Safari" "Chrome" "Firefox" "YouTube" "X"
                      "Twitter" "Discord")))
  "Alist (CATEGORY . (APP-NAME...)) for the Observed table's Cat column.
Matched case-insensitively against the AW window `app' name; unmatched apps
fall into `other'.  Tune the app lists to taste.")

(defun org-foresight--app-category (app)
  "Return the category of APP, or \"other\" when it matches nothing.
Categories come from `org-foresight-app-categories'."
  (let ((a (downcase (or app ""))))
    (or (cl-loop for (cat . apps) in org-foresight-app-categories
                 when (member a (mapcar #'downcase apps)) return cat)
        "other")))

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
                         (push (cons ts ce) today-intervals))))))))))))
    (dotimes (i days)
      (aset intervals-byday i
            (org-foresight--intervals-normalize (aref intervals-byday i))))
    (let (rows today-rows)
      (maphash (lambda (k v) (push (cons k v) rows)) table)
      (maphash (lambda (k v) (push (cons k v) today-rows)) today-table)
      (list :rows (seq-sort-by #'cdr #'> rows)
            :total total :byday byday :days days
            :today-rows (seq-sort-by #'cdr #'> today-rows)
            :today-total today-total :today-segments today-segments
            :today-intervals (nreverse today-intervals)
            :intervals-byday intervals-byday))))

;;;; Day scan
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
  (let ((raw (org-entry-get (point) "EFFORT")))
    (org-duration-to-minutes (or raw org-foresight-default-effort))))

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

(defun org-foresight--day-of (time from)
  "Return the whole-day offset of TIME from midnight of FROM."
  (floor (/ (float-time (time-subtract time (org-foresight--midnight from)))
            86400)))

(defun org-foresight--midnight (time)
  "Return local midnight at the start of TIME's day."
  (let ((d (decode-time time)))
    (encode-time 0 0 0 (nth 3 d) (nth 4 d) (nth 5 d))))

(defun org-foresight-scan (days &optional from)
  "Survey what is already claimed over DAYS days starting at FROM (today).
Return a plist of DAYS-length vectors, index 0 = FROM:

  :busy       per day, the (START . END) stretches that are spoken for
  :committed  per day, minutes of effort promised but not placed at a time
  :allday     per day, titles of all-day events
  :days :from as given

The busy/committed split is the rule the whole package rests on.  A timed
entry contributes an INTERVAL; an untimed one contributes its EFFORT.  Never
both -- and never twice for one entry on one day, so an entry carrying an
untimed SCHEDULED alongside a timed range is counted once, as the range.

Entries in a done state are skipped: finished work makes no claim on the
future.  Because `org-done-keywords' is consulted rather than a fixed list,
a done-type keyword such as DELEG drops out too."
  (let* ((from (or from (org-foresight--day-start 0)))
         (from0 (org-foresight--midnight from))
         (to (time-add from0 (days-to-time days)))
         (busy (make-vector days nil))
         (committed (make-vector days 0.0))
         (allday (make-vector days nil)))
    (dolist (file (org-agenda-files))
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (org-with-wide-buffer
           (org-map-entries
            (lambda ()
              (unless (org-entry-is-done-p)
                (let* ((todo (org-get-todo-state))
                       (effort (org-foresight--entry-effort-minutes))
                       (title (org-get-heading t t t t))
                       ;; day index -> the kind of claim seen there, so one
                       ;; entry cannot be charged twice for the same day
                       (seen (make-hash-table :test 'eql)))
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
                              (push (cons (car occ) end) (aref busy idx))
                              (puthash idx 'timed seen)))
                           (todo
                            (unless (gethash idx seen)
                              (puthash idx 'untimed seen)))
                           (t
                            (push title (aref allday idx))))))))
                  ;; Charge untimed effort only where nothing timed was found.
                  (maphash (lambda (idx kind)
                             (when (eq kind 'untimed)
                               (aset committed idx (+ (aref committed idx) effort))))
                           seen))))
            nil nil)))))
    (dotimes (i days)
      (aset busy i (org-foresight--intervals-normalize (aref busy i))))
    (list :busy busy :committed committed :allday allday
          :days days :from from0)))

;;;; Capacity
;; Supply and demand for one day.  Supply is the working window minus what is
;; already spoken for; demand is the effort promised plus a reserve for work
;; that has not arrived yet.  Whatever is left is what may still be promised.

(defcustom org-foresight-workday-start "09:00"
  "Time the working window opens, as \"HH:MM\"."
  :type 'string
  :group 'org-foresight)

(defcustom org-foresight-workday-end "18:00"
  "Time the working window closes, as \"HH:MM\"."
  :type 'string
  :group 'org-foresight)

(defcustom org-foresight-workdays '(1 2 3 4 5)
  "Days of the week that have a working window, 0 being Sunday."
  :type '(repeat integer)
  :group 'org-foresight)

(defcustom org-foresight-surge-default "1:00"
  "Reserve held back for unplanned work before anything has been learned.
Also the fallback wherever ActivityWatch history is unavailable, so a machine
without it still plans with a buffer rather than with none."
  :type 'string
  :group 'org-foresight)

(defcustom org-foresight-surge-cache-file
  (locate-user-emacs-file "org-foresight-surge.eld")
  "Where the learned surge reserve is cached.
Derived, machine-local data: the measurement it comes from is this machine's
own activity history, and it is cheap to recompute."
  :type 'file
  :group 'org-foresight)

(defun org-foresight--hhmm-on (day hhmm)
  "Return the time on DAY at HHMM, a \"HH:MM\" string."
  (let ((d (decode-time day))
        (parts (split-string hhmm ":")))
    (encode-time 0 (string-to-number (or (nth 1 parts) "0"))
                 (string-to-number (car parts))
                 (nth 3 d) (nth 4 d) (nth 5 d))))

(defun org-foresight-workday-window (day)
  "Return (START . END) for DAY's working window, or nil if DAY is not one."
  (when (memq (nth 6 (decode-time day)) org-foresight-workdays)
    (cons (org-foresight--hhmm-on day org-foresight-workday-start)
          (org-foresight--hhmm-on day org-foresight-workday-end))))

(defun org-foresight-surge-minutes ()
  "Return the reserve to hold back for unplanned work, in minutes.
Reads what `org-foresight-learn-surge' cached; falls back to
`org-foresight-surge-default' when nothing has been learned yet."
  (or (ignore-errors
        (when (file-readable-p org-foresight-surge-cache-file)
          (with-temp-buffer
            (insert-file-contents org-foresight-surge-cache-file)
            (plist-get (read (current-buffer)) :minutes))))
      (org-duration-to-minutes org-foresight-surge-default)))

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
  (ignore-errors
    (when (file-readable-p org-foresight-surge-cache-file)
      (with-temp-buffer
        (insert-file-contents org-foresight-surge-cache-file)
        (plist-get (read (current-buffer)) :samples)))))

(defun org-foresight--window-remaining (window now)
  "Return the part of WINDOW that has not already elapsed at NOW.
Capacity is about what can still be promised, so a morning that is already
over is not free time however empty the calendar looked at breakfast."
  (cond ((null window) nil)
        ((not (time-less-p (car window) now)) window)   ; wholly ahead
        ((time-less-p now (cdr window)) (cons now (cdr window)))
        (t nil)))                                       ; wholly past

(defun org-foresight-free-intervals (day &optional scan now)
  "Return the stretches of DAY's working window that nothing has claimed.
SCAN is a `org-foresight-scan' result covering DAY; one is taken for DAY alone
when omitted.  NOW defaults to the current time and clips away the part of the
day that has already gone."
  (let* ((now (or now (current-time)))
         (window (org-foresight--window-remaining
                  (org-foresight-workday-window day) now))
         (scan (or scan (org-foresight-scan 1 day)))
         (idx (org-foresight--day-of day (plist-get scan :from)))
         (busy (and (>= idx 0)
                    (< idx (plist-get scan :days))
                    (aref (plist-get scan :busy) idx))))
    (when window
      (org-foresight--intervals-subtract (list window) busy))))

(defun org-foresight-capacity (day &optional scan now)
  "Return a plist describing how much of DAY may still be promised.

  :window     the working window, or nil on a non-working day
  :free       stretches nothing has claimed yet, from NOW onwards
  :free-min   minutes in those stretches
  :committed-min  effort promised for DAY but not placed at a time
  :surge-min  reserve held back for work that has not arrived
  :headroom-min   what is left after both -- negative means overcommitted
  :finish     when the remaining commitment runs out, or nil if it does not fit

NOW defaults to the current time; passing it makes the whole calculation
reproducible, which is what lets this be tested at all."
  (let* ((now (or now (current-time)))
         (scan (or scan (org-foresight-scan 1 day)))
         (idx (org-foresight--day-of day (plist-get scan :from)))
         (free (org-foresight-free-intervals day scan now))
         (free-min (/ (org-foresight--intervals-seconds free) 60.0))
         (committed (if (and (>= idx 0) (< idx (plist-get scan :days)))
                        (aref (plist-get scan :committed) idx)
                      0.0))
         (surge (org-foresight-surge-minutes)))
    (list :window (org-foresight-workday-window day)
          :free free
          :free-min free-min
          :committed-min committed
          :surge-min surge
          :headroom-min (- free-min committed surge)
          :finish (org-foresight--pour free (+ committed surge)))))

(defun org-foresight--pour (intervals minutes)
  "Return the time at which MINUTES of work poured into INTERVALS runs out.
Returns nil when it does not fit, which is the honest answer to \"when am I
done\" on a day that is already overcommitted."
  (let ((left (* 60.0 minutes))
        (result nil))
    (catch 'done
      (when (<= left 0)
        (throw 'done (car (car intervals))))
      (dolist (iv intervals)
        (let ((span (float-time (time-subtract (cdr iv) (car iv)))))
          (if (<= left span)
              (throw 'done (setq result (time-add (car iv) left)))
            (setq left (- left span))))))
    result))

(provide 'org-foresight-core)

;;; org-foresight-core.el ends here
