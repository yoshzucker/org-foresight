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
(require 'org-datetree)
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

The outward legs arrive just in time.  The journey home ends when the work
span does -- the commute is inside the working day, not appended to it, so a
day that ends at 17:30 means being home at 17:30.

A journey is not shortened to fit around whatever else was in the diary.  If
it runs over something already booked then the day genuinely cannot be done
as written, and that is reported as a clash rather than quietly smoothed
over -- moving the commute would only hide the problem."
  (let* ((work (org-foresight-workday-window day))
         (placed (seq-filter (lambda (e)
                               (and (plist-get e :start) (plist-get e :place)))
                             ledger))
         (here org-foresight-home-place)
         out)
    (dolist (e placed)
      (let ((there (plist-get e :place)))
        (unless (eq there here)
          (let ((mins (org-foresight--travel-minutes here there)))
            (when (> mins 0)
              (push (list :kind 'travel
                          :title (format "→ %s" there)
                          :marker (plist-get e :marker)
                          :effort (float mins)
                          :start (time-subtract (plist-get e :start) (* 60 mins))
                          :end (plist-get e :start)
                          :place there :location nil :category nil)
                    out)))
          (setq here there))))
    (when (and work (not (eq here org-foresight-home-place)))
      (let ((mins (org-foresight--travel-minutes here org-foresight-home-place)))
        (when (> mins 0)
          (push (list :kind 'travel
                      :title (format "→ %s" org-foresight-home-place)
                      :marker nil
                      :effort (float mins)
                      :start (time-subtract (cdr work) (* 60 mins))
                      :end (cdr work)
                      :place org-foresight-home-place :location nil :category nil)
                out))))
    (nreverse out)))

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

(defun org-foresight-scan (days &optional from)
  "Survey what is already claimed over DAYS days starting at FROM (today).
Return a plist of DAYS-length vectors, index 0 = FROM:

  :busy       per day, the (START . END) stretches that are spoken for
  :committed  per day, minutes of effort promised but not placed at a time
  :allday     per day, titles of all-day events
  :ledger     per day, what produced the above, entry by entry:
              (:kind :title :marker :effort :start :end), where KIND is
              `meeting' (no TODO keyword), `task' (placed at a time) or
              `promised' (accepted but not yet placed)
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
         (from0 (org-foresight--midnight from))
         (to (time-add from0 (days-to-time days)))
         (busy (make-vector days nil))
         (committed (make-vector days 0.0))
         (allday (make-vector days nil))
         (ledger (make-vector days nil)))
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
                       (marker (point-marker))
                       (location (org-entry-get (point) "LOCATION"))
                       (place (org-foresight--entry-place))
                       (category (org-entry-get (point) "CATEGORY" t))
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
                              (push (list :kind (if todo 'task 'meeting)
                                          :title title :marker marker
                                          :effort (/ (float-time
                                                      (time-subtract end (car occ)))
                                                     60.0)
                                          :start (car occ) :end end
                                          :place place :location location
                                          :category category)
                                    (aref ledger idx))
                              (puthash idx 'timed seen)))
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
                  ;; The adjusted figure is what capacity spends, but both are
                  ;; kept: the ledger shows the estimate beside what it is
                  ;; being treated as, so the correction is never invisible.
                  (maphash (lambda (idx kind)
                             (when (eq kind 'untimed)
                               (let ((adj (* effort (org-foresight-bias-factor
                                                     category))))
                                 (aset committed idx (+ (aref committed idx) adj))
                                 (push (list :kind 'promised :title title
                                             :marker marker :effort effort
                                             :effort-adj adj
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
    (list :busy busy :committed committed :allday allday :ledger ledger
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
;;   grey │ travel │ meeting │ available │ travel │ private │ grey
;;         └──────────── work span ────────────┘
;;
;; "I leave at 17:30" is not the end of a window; it is that the evening is
;; already spoken for.  Work that lands outside the span therefore does not
;; disappear -- it is borrowed from private time, and is marked as such.
;;
;; The span itself is DECLARED, never inferred from history.  Measuring when
;; work actually stopped and calling that the working day would teach the
;; system to plan around the late nights it is supposed to prevent.

(defcustom org-foresight-awake '("07:00" . "23:00")
  "Waking hours as (START . END), each \"HH:MM\".
An END at or before START is taken as the following morning."
  :type '(cons string string)
  :group 'org-foresight)

(defcustom org-foresight-workday-start "09:00"
  "Default time the work span opens, as \"HH:MM\".
A declaration, not an observation: this is the shape of day being defended,
which is why it is never derived from when work actually happened."
  :type 'string
  :group 'org-foresight)

(defcustom org-foresight-workday-end "17:30"
  "Default time the work span closes, as \"HH:MM\"."
  :type 'string
  :group 'org-foresight)

(defcustom org-foresight-workdays '(1 2 3 4 5)
  "Days of the week that have a work span, 0 being Sunday."
  :type '(repeat integer)
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
`org-foresight-workday-window' is asked for every day of the horizon, several
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
  "Return DAY's shape as a plist (:awake (S . E) :work (S . E)-or-nil).

Resolution order is the day's own `WAKE' / `SLEEP' / `WORK' properties, then
the configured defaults.  `WORK' may be \"09:00-17:30\" to move the span or
\"none\" to declare the day free of work entirely."
  (let ((key (format-time-string "%Y-%m-%d" day))
        (table (org-foresight--shape-table)))
    (or (gethash key table)
        (puthash key (org-foresight--day-shape-1 day) table))))

(defun org-foresight--day-shape-1 (day)
  "Work out DAY's shape from the day file and the defaults."
  (let* ((wake (or (org-foresight--day-property day "WAKE")
                   (car org-foresight-awake)))
         (sleep (or (org-foresight--day-property day "SLEEP")
                    (cdr org-foresight-awake)))
         (raw (org-foresight--day-property day "WORK"))
         (work
          (cond
           ((and raw (string-match "\\`\\([0-9]+:[0-9]+\\)-\\([0-9]+:[0-9]+\\)\\'" raw))
            (cons (match-string 1 raw) (match-string 2 raw)))
           (raw nil)                    ; "none", or anything unparseable
           ((memq (nth 6 (decode-time day)) org-foresight-workdays)
            (cons org-foresight-workday-start org-foresight-workday-end))
           (t nil)))
         (wake-t (org-foresight--hhmm-on day wake))
         (sleep-t (org-foresight--hhmm-on day sleep)))
    ;; A bedtime at or before waking means the small hours of the next day.
    (unless (time-less-p wake-t sleep-t)
      (setq sleep-t (time-add sleep-t (days-to-time 1))))
    (list :awake (cons wake-t sleep-t)
          :work (and work (cons (org-foresight--hhmm-on day (car work))
                                (org-foresight--hhmm-on day (cdr work)))))))

(defun org-foresight--gap-bands (start end work)
  "Split the empty stretch \[START, END) by whether it is inside WORK.
Inside the span the time is available for work; outside it is private time
that merely happens to be unclaimed."
  (if (null work)
      (list (list :kind 'grey :start start :end end))
    (let ((ws (car work)) (we (cdr work)) out)
      (let ((b (org-foresight--min-time end ws)))
        (when (time-less-p start b)
          (push (list :kind 'grey :start start :end b) out)))
      (let ((a (org-foresight--max-time start ws))
            (b (org-foresight--min-time end we)))
        (when (time-less-p a b)
          (push (list :kind 'available :start a :end b) out)))
      (let ((a (org-foresight--max-time start we)))
        (when (time-less-p a end)
          (push (list :kind 'grey :start a :end end) out)))
      (nreverse out))))

(defun org-foresight--within-p (start end window)
  "Non-nil when \[START, END) lies wholly inside WINDOW."
  (and window
       (not (time-less-p start (car window)))
       (not (time-less-p (cdr window) end))))

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
    ;; Timed entries, clipped to the waking hours and de-overlapped by
    ;; preferring whichever started first.
    (dolist (e ledger)
      (when (plist-get e :start)
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
                        :place (plist-get e :place))
                  occupations)))))
    (setq occupations
          (sort (nreverse occupations)
                (lambda (a b) (time-less-p (plist-get a :start)
                                           (plist-get b :start)))))
    (dolist (occ occupations)
      (let ((s (plist-get occ :start))
            (n (plist-get occ :end)))
        (when (time-less-p cursor s)
          (setq out (nconc out (org-foresight--gap-bands cursor s work))))
        ;; Overlapping entries: keep the day a partition by trimming the
        ;; later one rather than emitting two bands over the same minutes.
        (let ((s (org-foresight--max-time s cursor)))
          (when (time-less-p s n)
            (setq out (nconc out (list (append
                                       ;; Only work can be borrowed; private
                                       ;; time outside the span is just life.
                                       (list :borrowed
                                             (and (memq (plist-get occ :kind)
                                                        '(meeting task travel))
                                                  (not (org-foresight--within-p
                                                        s n work))))
                                       (plist-put (copy-sequence occ) :start s)))))
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
                "Work span (HH:MM-HH:MM, or \"none\"): "
                (if work
                    (concat (format-time-string "%H:%M" (car work)) "-"
                            (format-time-string "%H:%M" (cdr work)))
                  "none"))))
    (unless file (user-error "Set `org-foresight-day-file' first"))
    (with-current-buffer (find-file-noselect file)
      (org-with-wide-buffer
       (org-datetree-find-date-create
        (calendar-gregorian-from-absolute (time-to-days day)))
       (org-entry-put (point) "WAKE" wake)
       (org-entry-put (point) "SLEEP" sleep)
       (org-entry-put (point) "WORK" span)
       (save-buffer)))
    (setq org-foresight--shape-cache nil)
    (message "%s: awake %s–%s, work %s"
             (format-time-string "%Y-%m-%d %a" day) wake sleep span)))

;;;; Capacity
;; Supply and demand for one day.  Supply is the working window minus what is
;; already spoken for; demand is the effort promised plus a reserve for work
;; that has not arrived yet.  Whatever is left is what may still be promised.

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
  "Return (START . END) for DAY's working window, or nil if DAY has none.
Delegates to `org-foresight-day-shape', so a day whose heading declares its
own hours is honoured everywhere the window is consulted -- capacity, the
forward load and placement all follow the same answer."
  (plist-get (org-foresight-day-shape day) :work))

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

;;;; Estimate bias
;; Estimates are systematically wrong, and always in the same direction for
;; the same kind of work.  The evidence is already there: every finished task
;; that carried an EFFORT and was clocked is one estimate measured against its
;; outcome.  Reading those back gives a per-category multiplier, which is a
;; way of improving the numbers without asking for a single new keystroke.
;;
;; Applied to what is promised and to how long a task is given when placed,
;; so a 1:30 estimate that reliably runs to 2:06 is allotted 2:06.  The
;; multiplier is always shown, because a shrinking day must read as "my
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

(defcustom org-foresight-bias-cache-file
  (locate-user-emacs-file "org-foresight-bias.eld")
  "Where the learned estimate multipliers are cached."
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
      (let ((data (ignore-errors
                    (with-temp-buffer
                      (insert-file-contents org-foresight-bias-cache-file)
                      (read (current-buffer))))))
        (setq org-foresight--bias-cache (cons mtime data))
        data)))))

(defun org-foresight-bias-factor (category)
  "Return the multiplier to apply to an estimate in CATEGORY.
Falls back to the overall figure for a category with too little history, and
to 1.0 when nothing has been learned at all -- an unknown bias must never
make the numbers worse than not correcting them."
  (if (not org-foresight-bias-enabled)
      1.0
    (let ((data (org-foresight--bias-data)))
      (or (and data category
               (cdr (assoc category (plist-get data :categories))))
          (and data (plist-get data :overall))
          1.0))))

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

(defun org-foresight--entry-clocked-minutes ()
  "Return the minutes clocked against the entry at point, its own only."
  (let ((text (org-foresight--entry-text))
        (re (concat "^[ \t]*" org-clock-string
                    "[ \t]*\\(\\[[^]\n]+\\]\\)--\\(\\[[^]\n]+\\]\\)"))
        (pos 0)
        (total 0.0))
    (while (string-match re text pos)
      ;; Read every group out before converting: `org-time-string-to-time'
      ;; matches internally and would clobber the match data mid-expression.
      (let ((s-str (match-string 1 text))
            (e-str (match-string 2 text)))
        (setq pos (match-end 0))
        (let ((s (org-time-string-to-time s-str))
              (e (org-time-string-to-time e-str)))
          (when (time-less-p s e)
            (setq total (+ total (/ (float-time (time-subtract e s)) 60.0)))))))
    total))

;;;###autoload
(defun org-foresight-learn-bias (&optional days)
  "Learn how far estimates run over, per category, from finished work.

Reads only what is already recorded -- an EFFORT and the clock beside it --
so this costs nothing to start using.  The median is taken rather than the
mean: one task that went badly wrong should not reshape the plan for every
task like it."
  (interactive)
  (let* ((days (or days org-foresight-bias-window))
         (from (org-foresight--day-start (1- days)))
         (by-category (make-hash-table :test 'equal))
         all)
    (dolist (file (org-agenda-files))
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (org-with-wide-buffer
           (org-map-entries
            (lambda ()
              (when (org-entry-is-done-p)
                ;; `org-entry-get' would happily return the next heading's
                ;; text for a CLOSED that is not there; the planning API
                ;; knows the difference between a timestamp and a property.
                (let ((closed (org-entry-get (point) "CLOSED" nil t))
                      (effort (org-entry-get (point) "EFFORT")))
                  (when (and closed effort)
                    (let ((when (org-time-string-to-time closed)))
                      (when (time-less-p from when)
                        (let ((est (org-duration-to-minutes effort))
                              (act (org-foresight--entry-clocked-minutes)))
                          (when (and (> est 0) (> act 0))
                            (let ((ratio (/ act est))
                                  (cat (org-entry-get (point) "CATEGORY" t)))
                              (push ratio all)
                              (when cat
                                (push ratio (gethash cat by-category))))))))))))
            nil nil)))))
    (if (null all)
        (user-error "No finished, estimated, clocked work in the last %d days" days)
      (let ((cats nil)
            (overall (org-foresight--median all)))
        (maphash (lambda (cat ratios)
                   (when (>= (length ratios) org-foresight-bias-min-samples)
                     (push (cons cat (org-foresight--median ratios)) cats)))
                 by-category)
        (with-temp-file org-foresight-bias-cache-file
          (prin1 (list :overall overall
                       :categories cats
                       :samples (length all)
                       :updated (format-time-string "%Y-%m-%d"))
                 (current-buffer)))
        (setq org-foresight--bias-cache nil)
        (message "Estimates run ×%.2f overall, from %d task(s)%s"
                 overall (length all)
                 (if cats
                     (concat "; " (mapconcat (lambda (c)
                                               (format "%s ×%.2f" (car c) (cdr c)))
                                             (seq-take cats 4) ", "))
                   ""))
        overall))))

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

  :window     the work span, or nil on a day that has none
  :span-min   the whole span in minutes -- what every part below adds up to
  :booked-min meetings, travel and work already placed at a time
  :free       stretches nothing has claimed yet, from NOW onwards
  :free-min   minutes in those stretches
  :committed-min  effort promised for DAY but not placed at a time
  :surge-min  reserve held back for work that has not arrived
  :headroom-min   what is left after both -- negative means overcommitted
  :finish     when the remaining commitment runs out, or nil if it does not fit
  :borrowed-min   work that fell outside the span, taken from private time
  :grey-min   waking hours that are neither work nor a private commitment

`:span-min' and `:booked-min' cover the whole span rather than only what is
left of it, so a bar drawn from them shows the day that was planned; the
other figures answer what can still be promised from NOW.

NOW defaults to the current time; passing it makes the whole calculation
reproducible, which is what lets this be tested at all."
  (let* ((now (or now (current-time)))
         (scan (or scan (org-foresight-scan 1 day)))
         (idx (org-foresight--day-of day (plist-get scan :from)))
         (window (org-foresight-workday-window day))
         (free (org-foresight-free-intervals day scan now))
         (free-min (/ (org-foresight--intervals-seconds free) 60.0))
         (committed (if (and (>= idx 0) (< idx (plist-get scan :days)))
                        (aref (plist-get scan :committed) idx)
                      0.0))
         (surge (org-foresight-surge-minutes))
         (bands (org-foresight-day-blocks day scan))
         (booked 0.0) (borrowed 0.0) (grey 0.0))
    (dolist (b bands)
      (let ((mins (/ (float-time (time-subtract (plist-get b :end)
                                                (plist-get b :start)))
                     60.0)))
        (pcase (plist-get b :kind)
          ('grey (setq grey (+ grey mins)))
          ((or 'meeting 'task 'travel)
           (if (plist-get b :borrowed)
               (setq borrowed (+ borrowed mins))
             (setq booked (+ booked mins))))
          (_ nil))))
    (list :window window
          :span-min (if window
                        (/ (float-time (time-subtract (cdr window) (car window)))
                           60.0)
                      0.0)
          :booked-min booked
          :free free
          :free-min free-min
          :committed-min committed
          :surge-min surge
          :headroom-min (- free-min committed surge)
          :finish (org-foresight--pour free (+ committed surge))
          :borrowed-min borrowed
          :grey-min grey)))

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
