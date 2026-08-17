;;; org-foresight-test.el --- Tests for org-foresight  -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Run from the package root:
;;
;;   emacs --batch -Q -L . -l test/org-foresight-test.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; The model is pure, so most of it is tested without starting Org's agenda at
;; all.  Tests that do need Org data use a fixture buffer rather than the user's
;; real files, so a failing test can never depend on what is in the inbox today.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-foresight-core)
(require 'org-foresight-report)
(require 'org-foresight-agenda)
(require 'org-foresight-plan)
(require 'org-foresight-demo)

;;;; Helpers

(defun org-foresight-test--ts (h m &optional day)
  "Return a time value for hour H, minute M on DAY (default 2026-08-10)."
  (encode-time 0 m h (or day 10) 8 2026))

(defun org-foresight-test--ivs (&rest specs)
  "Build intervals from SPECS, each a list (H1 M1 H2 M2 &optional DAY)."
  (mapcar (lambda (s)
            (cons (org-foresight-test--ts (nth 0 s) (nth 1 s) (nth 4 s))
                  (org-foresight-test--ts (nth 2 s) (nth 3 s) (nth 4 s))))
          specs))

(defun org-foresight-test--hhmm (intervals)
  "Render INTERVALS as \"HH:MM-HH:MM\" strings, so failures are readable."
  (mapcar (lambda (iv)
            (concat (format-time-string "%H:%M" (car iv))
                    "-"
                    (format-time-string "%H:%M" (cdr iv))))
          intervals))

;;;; Interval algebra

(ert-deftest org-foresight-test-normalize-sorts-and-merges ()
  "Overlapping and touching spans collapse; the result comes out sorted."
  (should (equal (org-foresight-test--hhmm
                  (org-foresight--intervals-normalize
                   (org-foresight-test--ivs '(13 0 14 0) '(9 0 10 0) '(9 30 11 0))))
                 '("09:00-11:00" "13:00-14:00")))
  ;; Exactly touching (10:00 end meets 10:00 start) must merge, not stay split.
  (should (equal (org-foresight-test--hhmm
                  (org-foresight--intervals-normalize
                   (org-foresight-test--ivs '(9 0 10 0) '(10 0 11 0))))
                 '("09:00-11:00")))
  ;; A span wholly inside another must not extend it.
  (should (equal (org-foresight-test--hhmm
                  (org-foresight--intervals-normalize
                   (org-foresight-test--ivs '(9 0 12 0) '(10 0 11 0))))
                 '("09:00-12:00"))))

(ert-deftest org-foresight-test-normalize-does-not-mutate-input ()
  "The docstring promises a fresh list; a caller's own intervals must survive."
  (let* ((input (org-foresight-test--ivs '(9 0 10 0) '(9 30 11 0)))
         (before (org-foresight-test--hhmm input)))
    (org-foresight--intervals-normalize input)
    (should (equal (org-foresight-test--hhmm input) before))))

(ert-deftest org-foresight-test-normalize-empty ()
  (should (equal (org-foresight--intervals-normalize nil) nil)))

(ert-deftest org-foresight-test-intersect ()
  (should (equal (org-foresight-test--hhmm
                  (org-foresight--intervals-intersect
                   (org-foresight-test--ivs '(9 0 12 0))
                   (org-foresight-test--ivs '(10 0 11 0))))
                 '("10:00-11:00")))
  ;; Several bites out of one span.
  (should (equal (org-foresight-test--hhmm
                  (org-foresight--intervals-intersect
                   (org-foresight-test--ivs '(9 0 17 0))
                   (org-foresight-test--ivs '(10 0 11 0) '(13 0 14 30))))
                 '("10:00-11:00" "13:00-14:30")))
  ;; Disjoint, and merely touching, both yield nothing.
  (should (equal (org-foresight--intervals-intersect
                  (org-foresight-test--ivs '(9 0 10 0))
                  (org-foresight-test--ivs '(11 0 12 0)))
                 nil))
  (should (equal (org-foresight--intervals-intersect
                  (org-foresight-test--ivs '(9 0 10 0))
                  (org-foresight-test--ivs '(10 0 11 0)))
                 nil)))

(ert-deftest org-foresight-test-subtract ()
  "Subtraction is what turns a workday into free slots, so cover its shapes."
  ;; A meeting in the middle leaves two slots.
  (should (equal (org-foresight-test--hhmm
                  (org-foresight--intervals-subtract
                   (org-foresight-test--ivs '(9 0 17 0))
                   (org-foresight-test--ivs '(12 0 13 0))))
                 '("09:00-12:00" "13:00-17:00")))
  ;; Two meetings leave three.
  (should (equal (org-foresight-test--hhmm
                  (org-foresight--intervals-subtract
                   (org-foresight-test--ivs '(9 0 17 0))
                   (org-foresight-test--ivs '(10 0 11 0) '(14 0 15 0))))
                 '("09:00-10:00" "11:00-14:00" "15:00-17:00")))
  ;; Fully covered leaves nothing.
  (should (equal (org-foresight--intervals-subtract
                  (org-foresight-test--ivs '(9 0 17 0))
                  (org-foresight-test--ivs '(8 0 18 0)))
                 nil))
  ;; Nothing to subtract leaves the day intact.
  (should (equal (org-foresight-test--hhmm
                  (org-foresight--intervals-subtract
                   (org-foresight-test--ivs '(9 0 17 0)) nil))
                 '("09:00-17:00")))
  ;; Overlapping meetings must not double-cut.
  (should (equal (org-foresight-test--hhmm
                  (org-foresight--intervals-subtract
                   (org-foresight-test--ivs '(9 0 17 0))
                   (org-foresight-test--ivs '(10 0 12 0) '(11 0 13 0))))
                 '("09:00-10:00" "13:00-17:00"))))

(ert-deftest org-foresight-test-seconds ()
  (should (= (org-foresight--intervals-seconds
              (org-foresight-test--ivs '(9 0 10 30) '(13 0 14 0)))
             (+ 5400 3600)))
  (should (= (org-foresight--intervals-seconds nil) 0)))

(ert-deftest org-foresight-test-overlap-seconds ()
  (let ((ivs (org-foresight-test--ivs '(9 0 10 0) '(11 0 12 0))))
    (should (= (org-foresight--overlap-seconds
                (org-foresight-test--ts 9 30) (org-foresight-test--ts 11 30) ivs)
               (+ 1800 1800)))
    (should (= (org-foresight--overlap-seconds
                (org-foresight-test--ts 10 0) (org-foresight-test--ts 11 0) ivs)
               0))))

(ert-deftest org-foresight-test-subtract-is-inverse-of-intersect ()
  "For any day and busy set, free + busy-within-day must equal the whole day."
  (let* ((day (org-foresight-test--ivs '(9 0 17 0)))
         (busy (org-foresight-test--ivs '(8 0 9 30) '(12 0 13 0) '(16 30 18 0)))
         (free (org-foresight--intervals-subtract day busy))
         (busy-in-day (org-foresight--intervals-intersect day busy)))
    (should (= (+ (org-foresight--intervals-seconds free)
                  (org-foresight--intervals-seconds busy-in-day))
               (org-foresight--intervals-seconds day)))))

;;;; Classification

;;;; Rendering primitives

(ert-deftest org-foresight-test-spark-char ()
  "Empty bins read as a dot; a full bin takes the tallest glyph."
  (should (equal (org-foresight-report--spark-char 0.0) ?·))
  (should (equal (org-foresight-report--spark-char -1.0) ?·))
  (should (equal (org-foresight-report--spark-char 1.0)
                 (aref org-foresight-report--spark-chars
                       (1- (length org-foresight-report--spark-chars))))))

(ert-deftest org-foresight-test-bin-frac-clamps ()
  "A half-hour bin cannot be more than full, however the seconds land."
  (let ((v (make-vector 48 0.0)))
    (aset v 0 3600.0)                   ; over-full
    (aset v 1 900.0)
    (aset v 2 -5.0)                     ; defensive
    (should (= (org-foresight-report--bin-frac v 0) 1.0))
    (should (= (org-foresight-report--bin-frac v 1) 0.5))
    (should (= (org-foresight-report--bin-frac v 2) 0.0))))

(ert-deftest org-foresight-test-sparkline-width ()
  "The sparkline and its axis must stay exactly 48 cells wide."
  (let ((v (make-vector 48 900.0)))
    (should (= (length (org-foresight-report--sparkline v)) 48)))
  (should (= (length (org-foresight-report--hour-axis)) 48)))

;;;; Day scan
;; The scan is driven from a buffer built here rather than from the user's
;; files, so these assertions stay true whatever is in the inbox today.

(defmacro org-foresight-test--with-org (text &rest body)
  "Run BODY with TEXT as the only agenda file."
  (declare (indent 1))
  `(let ((file (make-temp-file "org-foresight-test" nil ".org" ,text)))
     (unwind-protect
         (let ((org-agenda-files (list file))
               (org-todo-keywords '((sequence "NEXT" "ONGO" "|" "DONE" "CANCEL")
                                    (sequence "WAIT" "|" "DELEG")))
               (org-foresight-default-effort "0:30")
               (org-foresight-default-event-duration 60))
           ,@body)
       (when (get-file-buffer file) (kill-buffer (get-file-buffer file)))
       (delete-file file))))

(defun org-foresight-test--busy-on (scan idx)
  "Readable busy intervals for day IDX of SCAN."
  (org-foresight-test--hhmm (aref (plist-get scan :busy) idx)))

(ert-deftest org-foresight-test-scan-timed-becomes-interval ()
  "A SCHEDULED time plus an EFFORT occupies exactly that stretch."
  (org-foresight-test--with-org
      "* NEXT write the report
SCHEDULED: <2026-08-10 Mon 10:00>
:PROPERTIES:
:EFFORT:   1:30
:END:
"
    (let ((scan (org-foresight-scan 1 (org-foresight-test--ts 0 0))))
      (should (equal (org-foresight-test--busy-on scan 0) '("10:00-11:30")))
      (should (= (aref (plist-get scan :committed) 0) 0.0)))))

(ert-deftest org-foresight-test-scan-untimed-becomes-effort ()
  "An undated-in-the-day task claims effort, not a stretch of the clock."
  (org-foresight-test--with-org
      "* NEXT think about it
SCHEDULED: <2026-08-10 Mon>
:PROPERTIES:
:EFFORT:   0:45
:END:
"
    (let ((scan (org-foresight-scan 1 (org-foresight-test--ts 0 0))))
      (should (equal (org-foresight-test--busy-on scan 0) nil))
      (should (= (aref (plist-get scan :committed) 0) 45.0)))))

(ert-deftest org-foresight-test-scan-never-counts-an-entry-twice ()
  "The rule the whole package rests on: timed wins, and effort is not re-added.
This entry carries an untimed SCHEDULED *and* a timed range on the same day,
which is the shape that would silently halve a day's capacity if both were
charged."
  (org-foresight-test--with-org
      "* NEXT review with the team
SCHEDULED: <2026-08-10 Mon>
:PROPERTIES:
:EFFORT:   2:00
:END:
<2026-08-10 Mon 11:00>--<2026-08-10 Mon 12:00>
"
    (let ((scan (org-foresight-scan 1 (org-foresight-test--ts 0 0))))
      (should (equal (org-foresight-test--busy-on scan 0) '("11:00-12:00")))
      (should (= (aref (plist-get scan :committed) 0) 0.0)))))

(ert-deftest org-foresight-test-scan-skips-done ()
  "Finished work makes no claim on the future -- including done-type DELEG."
  (org-foresight-test--with-org
      "* DONE finished
SCHEDULED: <2026-08-10 Mon 10:00>
* DELEG handed off
SCHEDULED: <2026-08-10 Mon 14:00>
* NEXT still mine
SCHEDULED: <2026-08-10 Mon 16:00>
"
    (let ((scan (org-foresight-scan 1 (org-foresight-test--ts 0 0))))
      (should (equal (org-foresight-test--busy-on scan 0) '("16:00-16:30"))))))

(ert-deftest org-foresight-test-scan-deadline-is-not-occupation ()
  "A DEADLINE says when work must end, not which hours it eats."
  (org-foresight-test--with-org
      "* NEXT ship it
DEADLINE: <2026-08-10 Mon 17:00>
"
    (let ((scan (org-foresight-scan 1 (org-foresight-test--ts 0 0))))
      (should (equal (org-foresight-test--busy-on scan 0) nil))
      (should (= (aref (plist-get scan :committed) 0) 0.0)))))

(ert-deftest org-foresight-test-scan-meeting-without-end ()
  "A bare appointment time falls back to the configured event duration."
  (org-foresight-test--with-org
      "* standup
<2026-08-10 Mon 09:00>
"
    (let ((scan (org-foresight-scan 1 (org-foresight-test--ts 0 0))))
      (should (equal (org-foresight-test--busy-on scan 0) '("09:00-10:00"))))))

(ert-deftest org-foresight-test-scan-allday-is-not-busy ()
  "An all-day event is noted but does not swallow the working day."
  (org-foresight-test--with-org
      "* school sports day
<2026-08-10 Mon>
"
    (let ((scan (org-foresight-scan 1 (org-foresight-test--ts 0 0))))
      (should (equal (org-foresight-test--busy-on scan 0) nil))
      (should (equal (aref (plist-get scan :allday) 0) '("school sports day"))))))

(ert-deftest org-foresight-test-scan-expands-repeater ()
  "A weekly meeting fills every week of the horizon, not just the first."
  (org-foresight-test--with-org
      "* weekly sync
<2026-08-10 Mon 10:00-11:00 +1w>
"
    (let ((scan (org-foresight-scan 14 (org-foresight-test--ts 0 0))))
      (should (equal (org-foresight-test--busy-on scan 0) '("10:00-11:00")))
      (should (equal (org-foresight-test--busy-on scan 7) '("10:00-11:00")))
      (should (equal (org-foresight-test--busy-on scan 3) nil)))))

(ert-deftest org-foresight-test-scan-ignores-restart-repeater ()
  "A `.+' repeater's next date depends on completion, so it is not projected."
  (org-foresight-test--with-org
      "* water the plants
<2026-08-10 Mon 10:00-11:00 .+2d>
"
    (let ((scan (org-foresight-scan 14 (org-foresight-test--ts 0 0))))
      (should (equal (org-foresight-test--busy-on scan 0) '("10:00-11:00")))
      (should (equal (org-foresight-test--busy-on scan 2) nil)))))

(ert-deftest org-foresight-test-scan-survives-the-fixture ()
  "Every timestamp shape the real files use must scan without error.
`dayflow-fixture.org' is kept out of the agenda but is exactly the corpus of
odd shapes this scan has to survive."
  (let ((fixture (expand-file-name "~/Documents/memex/dayflow-fixture.org")))
    (skip-unless (file-readable-p fixture))
    (let* ((org-agenda-files (list fixture))
           (scan (org-foresight-scan 14 (org-foresight-test--ts 0 0))))
      (should (= (length (plist-get scan :busy)) 14))
      ;; intervals must come out normalized: sorted and non-overlapping
      (dotimes (i 14)
        (let ((ivs (aref (plist-get scan :busy) i)))
          (should (equal ivs (org-foresight--intervals-normalize ivs))))))))

;;;; Capacity

(defmacro org-foresight-test--with-window (&rest body)
  "Run BODY with a fixed 09:00-18:00 Mon-Fri window and a 60 minute reserve."
  (declare (indent 0))
  `(let ((org-foresight-work '(("09:00" . "17:30")))
         (org-foresight-workdays '(1 2 3 4 5))
         (org-foresight-surge-cache-file "/nonexistent/org-foresight-surge.eld")
         (org-foresight-leak-cache-file "/nonexistent/org-foresight-leak.eld")
         (org-foresight-surge-default "1:00")
         (org-foresight-leak-default "0:00")
         (org-foresight-lost-default "0:00"))
     ,@body))

(defun org-foresight-test--work-ends (cap)
  "Return when CAP\='s work is meant to be over: the end of its last interval."
  (cdr (car (last (plist-get cap :work)))))

(ert-deftest org-foresight-test-work-intervals ()
  (org-foresight-test--with-window
    ;; 2026-08-10 is a Monday; 2026-08-09 a Sunday.
    (should (org-foresight-work-intervals (org-foresight-test--ts 12 0 10)))
    (should-not (org-foresight-work-intervals (org-foresight-test--ts 12 0 9)))))

(ert-deftest org-foresight-test-capacity-subtracts-meetings-and-effort ()
  "Free time is the window minus meetings; headroom also loses effort and surge."
  (org-foresight-test--with-window
    (org-foresight-test--with-org
        "* team meeting
<2026-08-10 Mon 10:00-11:00>
* NEXT loose task
SCHEDULED: <2026-08-10 Mon>
:PROPERTIES:
:EFFORT:   2:00
:END:
"
      (let* ((day (org-foresight-test--ts 0 0 10))
             ;; NOW is pinned before the window opens, or this test would give a
             ;; different answer depending on the hour it happened to run.
             (cap (org-foresight-capacity day nil (org-foresight-test--ts 6 0 10))))
        ;; 8.5 hours of window, 1 hour of meeting
        (should (= (plist-get cap :free-min) 450.0))
        (should (= (plist-get cap :committed-min) 120.0))
        (should (= (plist-get cap :reserve-min) 60.0))
        (should (= (plist-get cap :headroom-min) 270.0))))))

(ert-deftest org-foresight-test-capacity-shrinks-as-the-day-passes ()
  "Free time is what is LEFT: the morning stops counting once it is over."
  (org-foresight-test--with-window
    (org-foresight-test--with-org "* nothing scheduled\n"
      (let ((day (org-foresight-test--ts 0 0 10)))
        ;; before the window opens: the whole 09:00-17:30
        (should (= (plist-get (org-foresight-capacity
                               day nil (org-foresight-test--ts 6 0 10))
                              :free-min)
                   510.0))
        ;; at 15:00 only two and a half hours remain
        (should (= (plist-get (org-foresight-capacity
                               day nil (org-foresight-test--ts 15 0 10))
                              :free-min)
                   150.0))
        ;; after it closes there is nothing left to promise
        (should (= (plist-get (org-foresight-capacity
                               day nil (org-foresight-test--ts 20 0 10))
                              :free-min)
                   0.0))))))

(defun org-foresight-test--headings-where (predicate)
  "Return the headings of the only agenda file for which PREDICATE holds.
Walks the file the way the scan does, so `org-map-entries\' has a real Org
buffer under it rather than whatever the test happened to be in."
  (let (out)
    (dolist (file (org-agenda-files))
      (with-current-buffer (find-file-noselect file)
        (org-with-wide-buffer
         (org-map-entries
          (lambda ()
            (when (funcall predicate)
              (push (org-get-heading t t t t) out)))
          nil nil))))
    (nreverse out)))

(defun org-foresight-test--arrived (title arrival &optional scheduled)
  "Return org text for a task marked as having arrived at ARRIVAL.
SCHEDULED goes before the drawer, where Org insists a planning line lives."
  (concat "* ONGO " title "\n"
          (when scheduled (concat "SCHEDULED: " scheduled "\n"))
          ":PROPERTIES:\n:SURGE: " arrival "\n:END:\n"))

(ert-deftest org-foresight-test-pouring-nothing-lands-at-once ()
  "Nothing to pour runs out at the first free instant, not never.

Nil has to keep meaning \"it does not fit\", or a day with nothing left to do
reads as a day that cannot be finished."
  (let ((free (list (cons (org-foresight-test--ts 10 0 10)
                          (org-foresight-test--ts 11 0 10))
                    (cons (org-foresight-test--ts 13 0 10)
                          (org-foresight-test--ts 14 0 10)))))
    (should (time-equal-p (org-foresight-test--ts 10 0 10)
                          (org-foresight--pour free 0)))
    (should (time-equal-p (org-foresight-test--ts 10 30 10)
                          (org-foresight--pour free 30)))
    ;; across the gap, not through it
    (should (time-equal-p (org-foresight-test--ts 13 30 10)
                          (org-foresight--pour free 90)))
    ;; and nil only when it truly does not fit
    (should-not (org-foresight--pour free 200))))

(ert-deftest org-foresight-test-mark-surge-keeps-the-first-arrival ()
  "Marking dates the arrival, and re-marking does not move it.

The arrival decides which day\'s reserve the work spent.  Overwriting it later
would move that day, and with it the one figure the reserve is learned from."
  (org-foresight-test--with-org "* NEXT it turned out to be an interruption\n"
    (with-current-buffer (find-file-noselect (car org-agenda-files))
      (goto-char (point-min))
      (org-foresight-mark-surge)
      (let ((first (org-entry-get (point) org-foresight-surge-property)))
        (should first)
        (should (org-foresight--parse-stamp first))
        (org-foresight-mark-surge)
        (should (equal first
                       (org-entry-get (point)
                                      org-foresight-surge-property)))))))

(ert-deftest org-foresight-test-surge-ignores-a-cache-from-before ()
  "A file written when the reserve meant something else is not read.

It held the median of time at the machine with no clock running -- a
measurement of recording, not of demand.  Carrying it forward would keep
planning around the wrong quantity under the right name."
  (let ((cache (make-temp-file "org-foresight-surge" nil ".eld"
                               (prin1-to-string '(:minutes 120.0 :samples 12)))))
    (unwind-protect
        (let ((org-foresight-surge-cache-file cache)
              (org-foresight-surge-default "0:20"))
          (should-not (org-foresight-surge-samples))
          (should (= 20.0 (org-foresight-surge-minutes))))
      (delete-file cache))))

(ert-deftest org-foresight-test-surge-stops-once-it-is-planned ()
  "Work that arrived is unplanned load on the day it arrived, and no longer.

An interruption absorbed over three days should show as unplanned on the
first.  Leaving it as surge for all three would hold a reserve against work
already on the calendar, and hold it three times over -- so a date of its own
on any later day is the point at which it becomes ordinary promised work.

A date on the arrival day itself is not a plan; it is where the capture put
it."
  (org-foresight-test--with-window
    (let ((mon "[2026-08-10 Mon 10:00]"))
      (org-foresight-test--with-org
          (concat (org-foresight-test--arrived "no date at all" mon)
                  (org-foresight-test--arrived
                   "dated to the day it landed" mon "<2026-08-10 Mon>")
                  (org-foresight-test--arrived
                   "taken in hand for Tuesday" mon "<2026-08-11 Tue>")
                  "* NEXT nobody interrupted anybody\nSCHEDULED: <2026-08-10 Mon>\n")
        (should (equal '("no date at all" "dated to the day it landed")
                       (org-foresight-test--headings-where
                        #'org-foresight--entry-surge-p)))))))

(ert-deftest org-foresight-test-surge-is-inherited-with-its-arrival ()
  "A task broken out of an interruption is part of it, and dated by it.

The mark is read with inheritance, so the child needs no mark of its own --
and the arrival it is judged against is the one on the entry that carries the
mark, not anything of the child\'s."
  (org-foresight-test--with-window
    (org-foresight-test--with-org
        (concat "* ONGO the interruption\n"
                ":PROPERTIES:\n:SURGE: [2026-08-10 Mon 10:00]\n:END:\n"
                "** NEXT part of it\nSCHEDULED: <2026-08-10 Mon>\n"
                "** NEXT taken in hand later\nSCHEDULED: <2026-08-12 Wed>\n")
      (should (equal '("the interruption" "part of it")
                     (org-foresight-test--headings-where
                      #'org-foresight--entry-surge-p))))))

(ert-deftest org-foresight-test-arrival-falls-back-through-the-log ()
  "Three sources for when work arrived, in falling order of directness.

The log is read for its earliest timestamp rather than its first line: the
order inside a drawer is not reliable -- `org-log-states-order-reversed\'
governs what is written next, not what is already there -- and a file that
has outlived a change of that setting has drawers both ways round."
  (org-foresight-test--with-org
      (concat
       ;; the property says so itself
       "* ONGO marked\n:PROPERTIES:\n:SURGE: [2026-08-10 Mon 10:00]\n:END:\n"
       ;; only a log, and written newest-first
       "* ONGO logged\n:PROPERTIES:\n:SURGE: t\n:END:\n"
       ":LOGBOOK:\n"
       "- State \"DONE\"       from \"ONGO\"       [2026-08-12 Wed 18:00]\n"
       "- State \"ONGO\"       from              [2026-08-09 Sun 08:00]\n"
       ":END:\n"
       ;; only a clock
       "* ONGO clocked\n:PROPERTIES:\n:SURGE: t\n:END:\n"
       ":LOGBOOK:\n"
       "CLOCK: [2026-08-11 Tue 13:00]--[2026-08-11 Tue 14:00] =>  1:00\n"
       ":END:\n"
       ;; nothing to go on
       "* ONGO bare\n:PROPERTIES:\n:SURGE: t\n:END:\n")
    (let (out)
      (dolist (file (org-agenda-files))
        (with-current-buffer (find-file-noselect file)
          (org-with-wide-buffer
           (org-map-entries
            (lambda ()
              (push (cons (org-get-heading t t t t)
                          (when-let ((a (org-foresight--entry-arrival)))
                            (format-time-string "%Y-%m-%d %H:%M" a)))
                    out))
            nil nil))))
      (should (equal '(("marked" . "2026-08-10 10:00")
                       ("logged" . "2026-08-09 08:00")
                       ("clocked" . "2026-08-11 13:00")
                       ("bare" . nil))
                     (nreverse out))))))

(ert-deftest org-foresight-test-arriving-work-is-todays-work ()
  "An interruption is captured without a date, and is still today\'s work.

There was no deciding where to put it -- it landed.  Left to the ordinary
rule it would belong to no day at all, and would spend the reserve held for
it while never being counted as the thing that spent it."
  (org-foresight-test--with-window
    (org-foresight-test--with-org
        (concat "* ONGO an interruption, no date\n"
                ":PROPERTIES:\n:SURGE: [2026-08-10 Mon 09:00]\n"
                ":EFFORT:   0:45\n:CATEGORY: work\n:END:\n")
      (let* ((day (org-foresight-test--ts 0 0 10))
             (scan (org-foresight-scan 1 day (org-foresight-test--ts 10 0 10)))
             (promised (seq-filter (lambda (e) (eq (plist-get e :kind) 'promised))
                                   (aref (plist-get scan :ledger) 0))))
        (should (= 45.0 (aref (plist-get scan :committed) 0)))
        (should (equal '("an interruption, no date")
                       (mapcar (lambda (e) (plist-get e :title)) promised)))
        ;; and it is the same figure that came off the reserve, so the day is
        ;; not discounted twice for one interruption
        (should (= 45.0 (aref (plist-get scan :surged) 0)))))))

(ert-deftest org-foresight-test-arriving-work-spends-its-own-reserve ()
  "The reserve is capacity held for work that has not arrived.

Once it has, it is an entry with an estimate and the day already owes it.
Holding the whole reserve beside it would put the same hours in the day
twice -- so what has landed comes off the allowance, finished or not."
  (org-foresight-test--with-window
    (let ((org-foresight-surge-default "2:00")
          (org-foresight-leak-default "0:00")
          (org-foresight-lost-default "0:00")
          (day (org-foresight-test--ts 0 0 10))
          ;; at the top of the window, where the whole allowance is still
          ;; ahead -- the shrinking with the day is a separate fact, and a
          ;; separate test
          (now (org-foresight-test--ts 9 0 10)))
      ;; nothing has arrived: the whole allowance stands
      (org-foresight-test--with-org "* nothing\n"
        (should (= 120.0 (plist-get (org-foresight-capacity day nil now)
                                    :surge-min))))
      ;; an hour of it has, and is still going
      (org-foresight-test--with-org
          (concat "* ONGO an interruption\n"
                  ":PROPERTIES:\n:SURGE: [2026-08-10 Mon 09:00]\n"
                  ":EFFORT:   1:00\n:END:\n"
                  "SCHEDULED: <2026-08-10 Mon>\n")
        (should (= 60.0 (plist-get (org-foresight-capacity day nil now)
                                   :surge-min))))
      ;; and one already finished spent the allowance just the same
      (org-foresight-test--with-org
          (concat "* DONE an interruption, dealt with\n"
                  "CLOSED: [2026-08-10 Mon 09:30]\n"
                  ":PROPERTIES:\n:SURGE: [2026-08-10 Mon 09:00]\n"
                  ":EFFORT:   1:00\n:END:\n"
                  ":LOGBOOK:\n"
                  "CLOCK: [2026-08-10 Mon 09:00]--[2026-08-10 Mon 09:30]"
                  " =>  0:30\n:END:\n")
        (should (= 90.0 (plist-get (org-foresight-capacity day nil now)
                                   :surge-min)))))))

(ert-deftest org-foresight-test-what-is-left-is-what-is-charged ()
  "A task half done costs the day what is left of it, not what it began as.

Capacity asks what can still be promised, and an entry that keeps its whole
estimate after half the work is in it answers a question about this morning.
The clock still running counts too: the hour being spent right now is the one
the afternoon most needs to know about."
  (org-foresight-test--with-window
    (let* ((day (org-foresight-test--ts 0 0 10))
           (stamp (lambda (h m) (format-time-string
                                 "[%Y-%m-%d %a %H:%M]"
                                 (org-foresight-test--ts h m 10)))))
      (org-foresight-test--with-org
          (concat "* NEXT two hours of work\n"
                  "SCHEDULED: <2026-08-10 Mon>\n"
                  ":PROPERTIES:\n:EFFORT:   2:00\n:END:\n"
                  ":LOGBOOK:\n"
                  "CLOCK: " (funcall stamp 9 0) "--" (funcall stamp 10 30)
                  " =>  1:30\n:END:\n")
        (let ((cap (org-foresight-capacity day nil
                                           (org-foresight-test--ts 11 0 10))))
          (should (= 30.0 (plist-get cap :committed-min)))))
      ;; a clock with no end is closed at NOW, so the remaining figure moves
      ;; while the work is being done rather than when it is stopped
      (org-foresight-test--with-org
          (concat "* NEXT two hours of work\n"
                  "SCHEDULED: <2026-08-10 Mon>\n"
                  ":PROPERTIES:\n:EFFORT:   2:00\n:END:\n"
                  ":LOGBOOK:\nCLOCK: " (funcall stamp 10 0) "\n:END:\n")
        (let ((cap (org-foresight-capacity day nil
                                           (org-foresight-test--ts 10 45 10))))
          (should (= 75.0 (plist-get cap :committed-min)))))
      ;; and it never goes below nothing: past the estimate the work is not
      ;; free, but that it is not finished is a fact about the estimate
      (org-foresight-test--with-org
          (concat "* NEXT two hours of work\n"
                  "SCHEDULED: <2026-08-10 Mon>\n"
                  ":PROPERTIES:\n:EFFORT:   2:00\n:END:\n"
                  ":LOGBOOK:\n"
                  "CLOCK: " (funcall stamp 9 0) "--" (funcall stamp 12 0)
                  " =>  3:00\n:END:\n")
        (let ((cap (org-foresight-capacity day nil
                                           (org-foresight-test--ts 12 0 10))))
          (should (= 0.0 (plist-get cap :committed-min))))))))

(ert-deftest org-foresight-test-the-remaining-day-adds-up ()
  "The terms of the remaining day divide it exactly, at any hour.

That identity is the whole point of the block: an overrun can be read as a
list of things rather than as a verdict, and every term is measured from now
so it answers what is still true."
  (org-foresight-test--with-window
    (let ((org-foresight-surge-default "0:40")
          (org-foresight-leak-default "0:50")
          (org-foresight-lost-default "0:20"))
      (org-foresight-test--with-org
          "* team meeting
<2026-08-10 Mon 14:00-15:00>
* NEXT write it up
SCHEDULED: <2026-08-10 Mon>
:PROPERTIES:
:EFFORT:   1:30
:END:
"
        (dolist (hour '(7 9 11 13 16 18))
          (let* ((cap (org-foresight-capacity
                       (org-foresight-test--ts 0 0 10) nil
                       (org-foresight-test--ts hour 0 10)))
                 (parts (+ (plist-get cap :committed-min)
                           (plist-get cap :surge-min)
                           (plist-get cap :leak-min)
                           (plist-get cap :lost-min)
                           (plist-get cap :headroom-min))))
            (should (< (abs (- (plist-get cap :free-min) parts)) 0.001))
            ;; and the three reserves are exactly what `:reserve-min' names
            (should (< (abs (- (plist-get cap :reserve-min)
                               (+ (plist-get cap :surge-min)
                                  (plist-get cap :leak-min)
                                  (plist-get cap :lost-min))))
                       0.001))))))))

(ert-deftest org-foresight-test-the-reserve-shrinks-with-the-day ()
  "What is held back is held against the hours that are left, not the whole day.

An allowance for a whole day cannot land in the half hour that remains of
one, and holding it there turned every evening into an overrun."
  (org-foresight-test--with-window
    (let ((org-foresight-leak-default "1:00")
          (org-foresight-lost-default "0:00")
          (org-foresight-surge-default "0:00"))
      (org-foresight-test--with-org "* nothing\n"
        (let ((morning (plist-get (org-foresight-capacity
                                   (org-foresight-test--ts 0 0 10) nil
                                   (org-foresight-test--ts 9 0 10))
                                  :leak-min))
              (afternoon (plist-get (org-foresight-capacity
                                     (org-foresight-test--ts 0 0 10) nil
                                     (org-foresight-test--ts 15 0 10))
                                    :leak-min))
              (after (plist-get (org-foresight-capacity
                                 (org-foresight-test--ts 0 0 10) nil
                                 (org-foresight-test--ts 19 0 10))
                                :leak-min)))
          ;; the whole allowance at the top of the day
          (should (< (abs (- 60.0 morning)) 0.001))
          ;; 2:30 of a 8:30 window left
          (should (< (abs (- (* 60.0 (/ 150.0 510.0)) afternoon)) 0.001))
          ;; and nothing at all once the window has closed
          (should (= 0.0 after)))))))

(ert-deftest org-foresight-test-capacity-finish-starts-from-now ()
  "The landing estimate pours from NOW, not from the top of the working day.

The reserve is set aside here so that one fact is under test: two hours of
work started at one o\'clock end at three, not at eleven."
  (org-foresight-test--with-window
    (org-foresight-test--with-org
        "* NEXT two hours of work
SCHEDULED: <2026-08-10 Mon>
:PROPERTIES:
:EFFORT:   2:00
:END:
"
      (let* ((day (org-foresight-test--ts 0 0 10))
             ;; 2:00 promised and nothing held back, started at 13:00
             (org-foresight-surge-default "0:00")
             (cap (org-foresight-capacity day nil (org-foresight-test--ts 13 0 10))))
        (should (equal (format-time-string "%H:%M" (plist-get cap :lands))
                       "15:00"))))))

(ert-deftest org-foresight-test-capacity-reports-overcommitment ()
  "When the promises exceed the day, headroom goes negative rather than to zero."
  (org-foresight-test--with-window
    (org-foresight-test--with-org
        "* NEXT far too much
SCHEDULED: <2026-08-10 Mon>
:PROPERTIES:
:EFFORT:   12:00
:END:
"
      (let ((cap (org-foresight-capacity (org-foresight-test--ts 0 0 10) nil
                                         (org-foresight-test--ts 6 0 10))))
        (should (< (plist-get cap :headroom-min) 0))
        ;; nothing fits inside the hours meant to be kept ...
        (should (null (plist-get cap :finish)))
        ;; ... but the work does not stop existing at the edge of them, and
        ;; where it actually stops is the fact worth having
        (should (plist-get cap :lands))
        (should (time-less-p (org-foresight-test--work-ends cap)
                             (plist-get cap :lands)))))))

(ert-deftest org-foresight-test-a-day-off-with-work-on-it-says-so ()
  "A day with no working hours still says what is dated to it.

It has no capacity to divide and nothing to offer, so there are no bars and
no suggestions -- but ten hours of work on a Saturday is exactly the kind of
thing the block exists to notice, and it used to go silent on it.  A day off
with nothing on it says nothing at all."
  (org-foresight-test--with-window
    ;; 2026-08-09 is a Sunday
    (org-foresight-test--with-org
        "* NEXT work on a day off
SCHEDULED: <2026-08-09 Sun>
:PROPERTIES:
:EFFORT:   2:00
:END:
"
      (let* ((day (org-foresight-test--ts 0 0 9))
             (now (org-foresight-test--ts 8 0 9))
             (scan (org-foresight-scan 1 day now))
             (line (substring-no-properties
                    (org-foresight-report-capacity-line day scan now))))
        (should (string-match-p "Not a working day" line))
        (should (string-match-p "2:00 promised" line))
        ;; and where it would land if done, from the waking day
        (should (string-match-p "ends 10:00" line))
        ;; no bars: there is no span to divide
        (should-not (string-match-p "█" line))
        ;; nothing marked, either -- a mark on every row says nothing, and
        ;; the fact is about the day
        (let ((marked (org-foresight-agenda--mark-rows
                       (list (propertize " x" 'org-hd-marker
                                         (copy-marker (point-min))))
                       nil (org-foresight-capacity day scan now)
                       (aref (plist-get scan :ledger) 0))))
          (should-not (seq-find (lambda (r)
                                  (get-text-property 0 'org-foresight-mark r))
                                marked)))))
    ;; a day off with nothing dated to it has nothing to say
    (org-foresight-test--with-org "* nothing\n"
      (should-not (org-foresight-report-capacity-line
                   (org-foresight-test--ts 0 0 9) nil
                   (org-foresight-test--ts 8 0 9))))))

(ert-deftest org-foresight-test-capacity-non-workday ()
  "A day with no working window has no free time to offer."
  (org-foresight-test--with-window
    (org-foresight-test--with-org "* nothing\n"
      (let ((cap (org-foresight-capacity (org-foresight-test--ts 0 0 9) nil
                                         (org-foresight-test--ts 6 0 9))))
        (should (null (plist-get cap :work)))
        (should (= (plist-get cap :free-min) 0.0))))))

(ert-deftest org-foresight-test-report-capacity-line ()
  "The top-of-agenda line carries the whole answer and fits on one line."
  (org-foresight-test--with-window
    (org-foresight-test--with-org
        "* team meeting
<2026-08-10 Mon 10:00-11:00>
* NEXT write it up
SCHEDULED: <2026-08-10 Mon>
:PROPERTIES:
:EFFORT:   1:00
:END:
"
      (let* ((s (org-foresight-report-capacity-line
                 (org-foresight-test--ts 0 0 10) nil
                 (org-foresight-test--ts 6 0 10)))
             (verdict (car (split-string s "\n"))))
        (should (string-match-p "Work" verdict))
        (should (string-match-p "ends" verdict))
        (should (string-match-p "left to promise" verdict))
        ;; the verdict itself stays one line; the bar follows beneath it
        (should (org-foresight-test--within-80 s))
        (should (string-match-p "booked" s))))))

(ert-deftest org-foresight-test-frees-names-the-way-out ()
  "An overcommitted day has to say by what, not only by how much."
  (let ((ledger '((:kind promised :title "Annual report" :effort 360)
                  (:kind meeting  :title "Project review" :effort 90
                         :category "outlook")
                  (:kind task     :title "Quarterly summary" :effort 60
                         :category "reporting")
                  (:kind meeting  :title "Kids' basketball" :effort 120
                         :category "club" :attention informational)
                  (:kind meeting  :title "Dinner" :effort 90 :category "family")
                  (:kind travel   :title "→ office" :effort 60)))
        (org-foresight-private-categories '("family"))
        (org-foresight-grid-frees 3))
    ;; smallest sufficient first: give up the least that still works
    (let ((line (org-foresight-report--frees 45 ledger)))
      (should (string-match-p "any one of" line))
      (should (< (string-match "Quarterly summary" line)
                 (string-match "Project review" line)))
      (should (<= (string-width line) 80)))
    ;; what is not yours to move is not offered as a way out
    (let ((line (org-foresight-report--frees 500 ledger)))
      (should (string-match-p "needs all of" line))
      (should-not (string-match-p "basketball" line))   ; informational
      (should-not (string-match-p "Dinner" line))       ; private
      (should-not (string-match-p "office" line)))      ; travel follows
    ;; an overflow bigger than everything movable says so rather than going
    ;; quiet on the one day the answer matters most -- 6:00 + 1:30 + 1:00
    (let ((line (org-foresight-report--frees 9999 ledger)))
      (should (string-match-p "only 8:30 of it can move" line)))
    ;; a day that fits has nothing to give up, and neither has an empty one
    (should-not (org-foresight-report--frees 0 ledger))
    (should-not (org-foresight-report--frees 45 nil))
    (let ((org-foresight-grid-frees nil))
      (should-not (org-foresight-report--frees 45 ledger)))))

(defun org-foresight-test--bar-cells (bar)
  "Return how many columns BAR draws, whatever glyph each one uses.

Every character is a column of the day, including the spaces the reserve is
outlined around; only the ellipsis that ends a cut-off bar stands for
something other than time."
  (length (seq-remove (lambda (c) (eq c ?…))
                      (string-to-list (substring-no-properties bar)))))

(ert-deftest org-foresight-test-bar-fits-a-full-day ()
  "A day whose parts fill the span exactly is drawn at the configured width."
  (let ((cap '(:span-min 510.0 :booked-min 137.0 :travel-min 60.0
               :private-min-in-span 0.0 :committed-min 73.0
               :reserve-min 57.0 :spare-min 183.0
               :private-min 0.0 :borrowed-min 0.0 :unclaimed-min 0.0)))
    (should (= (org-foresight-test--bar-cells (org-foresight-report--bar cap))
               org-foresight-bar-width))))

(ert-deftest org-foresight-test-bars-share-one-scale ()
  "The two bars must be comparable, or setting them side by side says nothing.

Equal spans of time have to draw equal numbers of cells whichever bar they
are in -- otherwise a long evening could look shorter than a short workday."
  (let* ((cap '(:span-min 480.0 :booked-min 480.0 :travel-min 0.0
                :private-min-in-span 0.0 :committed-min 0.0
                :reserve-min 0.0 :spare-min 0.0
                :private-min 240.0 :borrowed-min 0.0 :unclaimed-min 240.0))
         (work (org-foresight-test--bar-cells (org-foresight-report--bar cap)))
         (off (org-foresight-test--bar-cells (org-foresight-report--off-bar cap))))
    ;; 8:00 of work against 8:00 off: the same length
    (should (= work off))))

(ert-deftest org-foresight-test-reserve-is-outlined-not-filled ()
  "The reserve is room kept open, so it is drawn as a rule around nothing.

Filling it in would claim it for something, and the mono ramp it used to
occupy is needed for the three kinds of claimed work.  The outline is a face
box, which Emacs draws once around the whole run rather than per cell."
  (let* ((cap '(:span-min 510.0 :booked-min 137.0 :travel-min 60.0
                :private-min-in-span 0.0 :committed-min 73.0
                :reserve-min 57.0 :spare-min 183.0
                :private-min 0.0 :borrowed-min 0.0 :unclaimed-min 0.0))
         (bar (org-foresight-report--bar cap))
         (at (string-match " " (substring-no-properties bar))))
    (should at)
    (should (eq (get-text-property at 'face bar) 'org-foresight-report-surge))
    (should (face-attribute 'org-foresight-report-surge :box nil t))
    ;; and the reserve still occupies its columns, or the bar would not sum
    (should (= (org-foresight-test--bar-cells bar) org-foresight-bar-width))
    ;; the key names it with the same glyph the bar drew
    (let ((key (org-foresight-report--bar-key cap)))
      (should (string-match-p "  reserve" (substring-no-properties key))))))

(ert-deftest org-foresight-test-bar-marks-the-overflow ()
  "An overcommitted day shows where the span ran out instead of clipping."
  (let ((over '(:span-min 510.0 :booked-min 420.0 :travel-min 0.0
                :private-min-in-span 0.0 :committed-min 240.0
                :reserve-min 60.0 :spare-min -210.0))
        (fits '(:span-min 510.0 :booked-min 60.0 :travel-min 0.0
                :private-min-in-span 0.0 :committed-min 60.0
                :reserve-min 60.0 :spare-min 330.0)))
    (should (string-match-p "┃" (org-foresight-report--bar over)))
    ;; The overflow is shown, not cut back to something that fits.  The mark
    ;; replaces the cell it stands on, so the count is the width itself.
    (should (>= (org-foresight-test--bar-cells (org-foresight-report--bar over))
                org-foresight-bar-width))
    (should-not (string-match-p "┃" (org-foresight-report--bar fits)))))

(ert-deftest org-foresight-test-bar-absent-without-a-span ()
  (should (null (org-foresight-report--bar
                 '(:span-min 0.0 :booked-min 0.0 :travel-min 0.0
                   :private-min-in-span 0.0 :committed-min 0.0
                   :reserve-min 0.0 :spare-min 0.0)))))

(ert-deftest org-foresight-test-private-in-span-is-not-spare ()
  "An appointment in working hours is time the span cannot spend.
Counted nowhere, it used to come back as time still available to promise."
  (org-foresight-test--with-day
      "* dentist
:PROPERTIES:
:CATEGORY: family
:END:
<2026-08-10 Mon 11:00-12:00>
"
    (let* ((org-foresight-surge-cache-file "/nonexistent/surge.eld")
           (org-foresight-surge-default "1:00")
           (cap (org-foresight-capacity (org-foresight-test--ts 0 0 10) nil
                                        (org-foresight-test--ts 6 0 10))))
      (should (= (plist-get cap :private-min-in-span) 60.0))
      ;; 8:30 of span, less the hour and the reserve
      (should (= (plist-get cap :spare-min) (- 510 60 60)))
      ;; and it is not double-counted as time off
      (should (= (plist-get cap :private-min) 0.0)))))

(ert-deftest org-foresight-test-ledger-agrees-with-the-verdict ()
  "The ledger must add up to the numbers stated above it.
If the parts and the total ever disagree, the ledger stops being evidence."
  (org-foresight-test--with-travel
      "* Project review
:PROPERTIES:
:LOCATION: 会議室A
:END:
<2026-08-10 Mon 14:00-15:00>
* NEXT reply to procurement
SCHEDULED: <2026-08-10 Mon>
:PROPERTIES:
:EFFORT:   0:30
:END:
"
    (let* ((day (org-foresight-test--ts 0 0 10))
           (scan (org-foresight-scan 1 day))
           (cap (org-foresight-capacity day scan (org-foresight-test--ts 6 0 10)))
           (ledger (aref (plist-get scan :ledger) 0))
           (booked 0.0) (travel 0.0) (promised 0.0))
      (dolist (e ledger)
        (pcase (plist-get e :kind)
          ((or 'meeting 'task) (setq booked (+ booked (plist-get e :effort))))
          ('travel (setq travel (+ travel (plist-get e :effort))))
          ('promised (setq promised (+ promised (plist-get e :effort))))
          (_ nil)))
      (should (= booked (plist-get cap :booked-min)))
      (should (= travel (plist-get cap :travel-min)))
      (should (= promised (plist-get cap :committed-min)))
      ;; and the bar's own parts fill the span exactly -- if the segments and
      ;; the whole ever disagree, the picture stops being evidence
      (should (= (+ (plist-get cap :booked-min)
                    (plist-get cap :travel-min)
                    (plist-get cap :committed-min)
                    (plist-get cap :reserve-min)
                    (plist-get cap :spare-min))
                 (plist-get cap :span-min))))))

(ert-deftest org-foresight-test-only-badges-touch-the-frame-edge ()
  "Column zero belongs to badges alone.

A badge is read by scanning rather than by reading, so an eye running down
the left edge has to hit section headings and nothing else.  Every block
therefore starts its lines at the margin -- verdicts, keys, bars and rows
alike, which is also what makes a block's picture line up with its words.
The badge is added by whatever assembles the blocks, so what is checked here
is that no block emits a line at the edge itself."
  (org-foresight-test--with-day
      "* Project review
<2026-08-10 Mon 14:00-15:00>
* NEXT Write the vendor comparison
SCHEDULED: <2026-08-10 Mon>
:PROPERTIES:
:EFFORT: 2:00
:END:
"
    (let ((day (org-foresight-test--ts 0 0 10))
          (now (org-foresight-test--ts 6 0 10)))
      (dolist (block (list (org-foresight-report-capacity-line day nil now)
                           (org-foresight-report-load 14 nil now)
                           (org-foresight-report-signals
                            '(("Something (1)"
                               . ((:title "a thing" :note "a note"
                                          :marker nil)))))))
        (dolist (line (split-string (substring-no-properties (or block "")) "\n"))
          (unless (string-empty-p line)
            (should (string-prefix-p org-foresight-report-margin line))))))))

(ert-deftest org-foresight-test-glyphs-are-single-cell ()
  "Every mark drawn into an agenda line must occupy exactly one cell.

A wider one shifts the column behind it, and the usual cause is a character
the font simply lacks: what appears then comes from the fallback, at whatever
width that font uses.  `⨯' U+2717 was one such -- absent from PlemolJP and
rendered slightly too wide -- and PlemolJP has none of U+2715, U+2717 or
U+2718, which is why the mark for what will not fit is U+2A2F."
  (dolist (s (append (list (string org-foresight-block)
                           org-foresight-agenda-wont-fit)
                     (mapcar #'string
                             (append org-foresight-report--partials nil))
                     '("·" "─" "┈" "→" "┃" "↳" "⨯")))
    (should (= (string-width s) 1))))

(ert-deftest org-foresight-test-off-bar ()
  "Unclaimed private time is reported, and borrowing from it is called out."
  (org-foresight-test--with-day
      "* NEXT evening call
<2026-08-10 Mon 19:00-20:00>
"
    (let* ((cap (org-foresight-capacity (org-foresight-test--ts 0 0 10) nil
                                        (org-foresight-test--ts 6 0 10)))
           (line (substring-no-properties
                  (org-foresight-report--bars cap))))
      (should (string-match-p "Off" line))
      (should (string-match-p "borrowed 1:00" line))))
  ;; a day that stays inside its span borrows nothing
  (org-foresight-test--with-day
      "* NEXT afternoon work
<2026-08-10 Mon 14:00-15:00>
"
    (let* ((cap (org-foresight-capacity (org-foresight-test--ts 0 0 10) nil
                                        (org-foresight-test--ts 6 0 10)))
           (line (substring-no-properties
                  (org-foresight-report--bars cap))))
      (should-not (string-match-p "borrowed" line)))))

(ert-deftest org-foresight-test-the-verdict-names-the-command-and-the-key ()
  "Both, because they say different things.

The key is what the hand needs and is read from the keymap, so it stays true
when the binding moves.  The name is what the sentence needs: a line ending
in a bare `B' says nothing about what pressing it would get you, and this one
exists to make somebody press it.

The key goes in front, so the tail of the line reads the same whether or not
anything is bound -- only the shortcut has appeared."
  (should (equal "M-x org-foresight-board"
                 (org-foresight-plan--command-hint 'org-foresight-board)))
  (with-temp-buffer
    (let ((map (make-sparse-keymap)))
      (define-key map "B" #'org-foresight-board)
      (use-local-map map)
      (should (equal "B M-x org-foresight-board"
                     (org-foresight-plan--command-hint 'org-foresight-board))))))

(ert-deftest org-foresight-test-a-signal-names-what-settles-it ()
  "A group a command can settle says so; one it cannot stays quiet.

`org-foresight-prepare-meetings' appeared nowhere in the interface it exists
to serve -- the board named the problem and left the reader to go and find
the answer in the source.  The silence on the other groups is half of it: a
hint on every heading, including the ones fixed a row at a time, teaches the
reader to stop believing the ones that mean something."
  (let ((rendered
         (substring-no-properties
          (org-foresight-report-signals
           '(("Meetings without prep"
              . ((:title "Review" :note "needs 0:30 + 0:15" :marker nil)))
             ("Unplannable (deadline, no estimate)"
              . ((:title "Report" :note "no estimate" :marker nil))))))))
    (should (string-match-p
             "Meetings without prep (1) · M-x org-foresight-prepare-meetings"
             rendered))
    (should-not (string-match-p "Unplannable[^\n]*·" rendered))
    (dolist (line (split-string rendered "\n"))
      (should (org-foresight-test--within-80 line)))))

(ert-deftest org-foresight-test-verdict-extras ()
  "The daily line must announce outstanding signals, and stay silent without.
A signal nobody is prompted to look at is not really being caught, but a
permanent nag about nothing is worse."
  (org-foresight-test--with-window
    (org-foresight-test--with-signals
        "* WAIT gone quiet
SCHEDULED: <2020-01-01 Wed>
"
      (let ((s (org-foresight-report-capacity-line
                (org-foresight-test--ts 0 0 10) nil
                (org-foresight-test--ts 6 0 10))))
        (should (string-match-p "1 signal unplanned" s))
        (should (org-foresight-test--within-80 s))))
    ;; a clean file adds nothing at all
    (org-foresight-test--with-signals
        "* NEXT ordinary
SCHEDULED: <2027-06-01 Tue>
:PROPERTIES:
:EFFORT:   1:00
:END:
"
      (let ((s (org-foresight-report-capacity-line
                (org-foresight-test--ts 0 0 10) nil
                (org-foresight-test--ts 6 0 10))))
        (should-not (string-match-p "signal" s))))))

(ert-deftest org-foresight-test-signals-cache ()
  "The cache must serve repeats, and FORCE must go back to the files."
  (org-foresight-test--with-signals "* WAIT quiet\nSCHEDULED: <2020-01-01 Wed>\n"
    (let ((org-foresight--signals-cache nil)
          (calls 0))
      (cl-letf* ((orig (symbol-function 'org-foresight--signals-compute))
                 ((symbol-function 'org-foresight--signals-compute)
                  (lambda (&rest args) (setq calls (1+ calls)) (apply orig args))))
        (org-foresight-signals)
        (org-foresight-signals)
        (org-foresight-signals)
        (should (= calls 1))
        (org-foresight-signals t)
        (should (= calls 2))))))

;;;; Rendering into an agenda buffer
;; The render hook is the one place the package writes into someone else's
;; buffer, so its manners are pinned here: it leaves point alone, it puts its
;; blocks on the side the style asked for, and running twice is the same as
;; running once.

(defmacro org-foresight-test--in-agenda (&rest body)
  "Run BODY in a buffer shaped like a finalized agenda."
  (declare (indent 0))
  `(with-temp-buffer
     (org-agenda-mode)
     (insert "Day-agenda (W33):\n"
             "Tuesday 11 August 2026\n"
             "  work: 14:00 Project review\n")
     (put-text-property (point-min) (point-max) 'org-agenda-type 'agenda)
     (goto-char (point-min))
     ,@body))

(ert-deftest org-foresight-test-render-leaves-point-alone ()
  "`org-agenda-list' positions point and then finalizes, so this hook runs
last; moving point here strands the cursor -- and the window -- at the end."
  (org-foresight-test--with-day "* NEXT something\nSCHEDULED: <2026-08-10 Mon>\n"
    (dolist (style '(daily plan))
      (org-foresight-test--in-agenda
        (let ((org-foresight-report-style style))
          (org-foresight-report-render)
          (should (= (point) (point-min))))))
    ;; Point further down stays on the same text.  Its offset changes, and
    ;; must: a block inserted above it moves that text down the buffer, and
    ;; the cursor is expected to travel with what it was resting on.
    (org-foresight-test--in-agenda
      (let ((org-foresight-report-style 'daily))
        (goto-char (point-min))
        (search-forward "Project review")
        (let ((before (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
          (org-foresight-report-render)
          (should (equal (buffer-substring-no-properties
                          (line-beginning-position) (line-end-position))
                         before)))))))

(ert-deftest org-foresight-test-render-places-by-style ()
  "The verdict leads and the tail follows the listing, in either view.

And within the daily tail, forward before backward: the verdict has just
said how far over the day is, and where the rest goes must not sit on the far
side of a block about what already happened."
  (org-foresight-test--with-day "* NEXT something\nSCHEDULED: <2026-08-10 Mon>\n"
    (org-foresight-test--in-agenda
      (let ((org-foresight-report-style 'daily))
        (org-foresight-report-render)
        (let ((text (substring-no-properties (buffer-string))))
          (should (< (string-search "Capacity" text)
                     (string-search "Day-agenda" text)))
          (should (< (string-search "Day-agenda" text)
                     (string-search "Load" text)))
          (should (< (string-search "Load" text)
                     (string-search "Spent" text))))))
    (org-foresight-test--in-agenda
      (let ((org-foresight-report-style 'review))
        (org-foresight-report-render)
        (let ((text (substring-no-properties (buffer-string))))
          (should (< (string-search "Day-agenda" text)
                     (string-search "Clocked" text))))))))

(ert-deftest org-foresight-test-render-is-idempotent ()
  "Rendering twice must replace, not accumulate."
  (org-foresight-test--with-day "* NEXT something\nSCHEDULED: <2026-08-10 Mon>\n"
    (org-foresight-test--in-agenda
      (let ((org-foresight-report-style 'daily))
        (org-foresight-report-render)
        (let ((once (buffer-string)))
          (org-foresight-report-render)
          (org-foresight-report-render)
          (should (equal (buffer-string) once)))))))

(ert-deftest org-foresight-test-refresh-follows-an-edit ()
  "Acting on a row must move the numbers above it.

Agenda edits finalize narrowed to the changed line, which the render hook
declines; without the refresh the row would update while the verdict went on
stating what was true before the edit."
  (org-foresight-test--with-day
      (concat "* NEXT a big job\nSCHEDULED: "
              ;; today, so the figures the render reports are about this entry
              (format-time-string "<%Y-%m-%d %a>" (org-foresight--day-start 0))
              "\n:PROPERTIES:\n:EFFORT:   4:00\n:END:\n")
    (org-foresight-test--in-agenda
      (let ((org-foresight-report-style 'daily))
        (org-foresight-report-render)
        (should (string-match-p "promised 4:00"
                                (substring-no-properties (buffer-string))))
        ;; the entry is re-estimated behind the agenda's back
        (with-current-buffer (find-file-noselect (car org-agenda-files))
          (org-with-wide-buffer
           (goto-char (point-min))
           (re-search-forward "^\\* NEXT a big job")
           (org-entry-put (point) "EFFORT" "1:00")))
        (org-foresight-report-refresh)
        (let ((text (substring-no-properties (buffer-string))))
          (should (string-match-p "promised 1:00" text))
          (should-not (string-match-p "promised 4:00" text)))))))

;;;; Diagnosis

(ert-deftest org-foresight-test-diagnose-notices-unplaced-meetings ()
  "A place that is configured but never matches is the quietest failure of
all: the numbers look right, and the commute is simply missing from them."
  (org-foresight-test--with-travel
      (concat "* at the office\n:PROPERTIES:\n:LOCATION: 会議室A\n:END:\n"
              (format-time-string "<%Y-%m-%d %a 10:00-11:00>"
                                  (org-foresight--day-start 0))
              "\n* on a call\n:PROPERTIES:\n:LOCATION: https://teams/x\n:END:\n"
              (format-time-string "<%Y-%m-%d %a 14:00-15:00>"
                                  (org-foresight--day-start 0))
              "\n")
    (let* ((day (org-foresight--day-start 0))
           (state (org-foresight--diagnose-state day))
           (advice (org-foresight--diagnose-advice day)))
      (should (string-match-p "1 of 2 timed entries"
                              (cdr (assoc "places" state))))
      (should (seq-find (lambda (s) (string-match-p "name no place" s))
                        advice)))))

(ert-deftest org-foresight-test-the-report-wires-itself-in ()
  "Loading the package is the whole installation.

The derived rows arrive by an advice on Org's own grid function and the spine
is drawn from this very hook, both installed by loading.  A report that alone
had to be added by hand was the odd one out -- and the one thing a second
machine would be set up without."
  (should (memq 'org-foresight-report-render org-agenda-finalize-hook))
  ;; Loading again adds nothing: the file is read twice on any upgrade.
  (add-hook 'org-agenda-finalize-hook #'org-foresight-report-render t)
  (should (= 1 (seq-count (lambda (f) (eq f 'org-foresight-report-render))
                          org-agenda-finalize-hook))))

(ert-deftest org-foresight-test-diagnose-says-when-the-gutter-is-too-narrow ()
  "The one thing this layer does silently is the one thing diagnose must say.

A place spine with nowhere to go is not drawn -- deliberately, since half of
one reads as a fault -- and nothing else distinguishes that from a day spent
at home."
  (org-foresight-test--with-day "* nothing\n"
    (let ((day (org-foresight--day-start 0)))
      (let ((org-agenda-prefix-format '((agenda . "  %-8.8c%?-12t% s"))))
        (should (seq-find (lambda (s) (string-match-p "away from home" s))
                          (org-foresight--diagnose-advice day))))
      (let ((org-agenda-prefix-format '((agenda . "     %-8.8c%?-12t% s"))))
        (should-not (seq-find (lambda (s) (string-match-p "away from home" s))
                              (org-foresight--diagnose-advice day))))
      ;; and nothing at all where the brackets are not wanted
      (let ((org-foresight-agenda-place-spine nil)
            (org-agenda-prefix-format '((agenda . "  %-8.8c%?-12t% s"))))
        (should-not (seq-find (lambda (s) (string-match-p "away from home" s))
                              (org-foresight--diagnose-advice day)))))))

(ert-deftest org-foresight-test-report-guarded-surfaces-errors ()
  "A failing block complains in place instead of disappearing silently."
  (let ((s (org-foresight-report--guarded
            (lambda () (error "boom")))))
    (should (string-match-p "org-foresight failed" s))
    (should (string-match-p "boom" s))))

(ert-deftest org-foresight-test-pour-finish-estimate ()
  "Work poured into the day steps over meetings instead of through them."
  (let ((free (org-foresight-test--ivs '(9 0 10 0) '(11 0 17 0))))
    ;; 30 minutes fits before the 10:00 meeting
    (should (equal (format-time-string "%H:%M" (org-foresight--pour free 30))
                   "09:30"))
    ;; 90 minutes must resume after it: 60 before, 30 after 11:00
    (should (equal (format-time-string "%H:%M" (org-foresight--pour free 90))
                   "11:30"))
    ;; more than the day holds does not fit at all
    (should (null (org-foresight--pour free 600)))))

(ert-deftest org-foresight-test-surge-falls-back-without-cache ()
  "With nothing learned the reserve is the configured default, never zero."
  (org-foresight-test--with-window
    (should (= (org-foresight-surge-minutes) 60.0))
    (should (null (org-foresight-surge-samples)))))

;;;; The day as bands
;; The whole model rests on these partitioning the waking hours exactly.  If
;; they ever gap or overlap, every total taken over them is quietly wrong.

(defmacro org-foresight-test--with-day (text &rest body)
  "Run BODY with TEXT as the only agenda file and a fixed default shape."
  (declare (indent 1))
  `(org-foresight-test--with-org ,text
     (let ((org-foresight-awake '("07:00" . "23:00"))
           (org-foresight-work '(("09:00" . "17:30")))
           ;; Every day is a working day here.  Tests built on "today" would
           ;; otherwise answer differently at the weekend -- a day with no
           ;; working window has no capacity, no gaps and nothing to mark --
           ;; and a suite that passes on Tuesday and fails on Saturday is
           ;; testing the calendar.  Non-working days have their own tests,
           ;; on dates chosen for it.
           (org-foresight-workdays '(0 1 2 3 4 5 6))
           (org-foresight-private-categories '("family"))
           (org-foresight-places '((office . "会議室\\|Office")))
           (org-foresight-day-file nil)
           (org-foresight--shape-cache nil))
       ,@body)))

(defun org-foresight-test--bands (day)
  "Render DAY's bands as \"KIND HH:MM-HH:MM\" strings."
  (mapcar (lambda (b)
            (format "%s %s-%s%s"
                    (plist-get b :kind)
                    (format-time-string "%H:%M" (plist-get b :start))
                    (format-time-string "%H:%M" (plist-get b :end))
                    (if (plist-get b :borrowed) " borrowed" "")))
          (org-foresight-day-blocks day)))

(ert-deftest org-foresight-test-bands-partition-the-day ()
  "Bands must tile the waking hours with no gap and no overlap."
  (org-foresight-test--with-day
      "* Project review
<2026-08-10 Mon 14:00-15:30>
* NEXT evening call
<2026-08-10 Mon 19:00-20:00>
"
    (let* ((day (org-foresight-test--ts 0 0 10))
           (bands (org-foresight-day-blocks day))
           (shape (org-foresight-day-shape day)))
      (should bands)
      ;; contiguous
      (let ((cursor (car (plist-get shape :awake))))
        (dolist (b bands)
          (should (equal (float-time (plist-get b :start)) (float-time cursor)))
          (setq cursor (plist-get b :end)))
        (should (equal (float-time cursor)
                       (float-time (cdr (plist-get shape :awake))))))
      ;; and they add up to the awake window
      (should (= (apply #'+ (mapcar (lambda (b)
                                      (float-time (time-subtract (plist-get b :end)
                                                                 (plist-get b :start))))
                                    bands))
                 (float-time (time-subtract (cdr (plist-get shape :awake))
                                            (car (plist-get shape :awake)))))))))

(ert-deftest org-foresight-test-bands-classify ()
  "Work inside the span is booked; work outside it is borrowed from private."
  (org-foresight-test--with-day
      "* Project review
<2026-08-10 Mon 14:00-15:30>
* NEXT evening call
<2026-08-10 Mon 19:00-20:00>
"
    (let ((bands (org-foresight-test--bands (org-foresight-test--ts 0 0 10))))
      (should (equal bands
                     '("grey 07:00-09:00"
                       "available 09:00-14:00"
                       "meeting 14:00-15:30"
                       "available 15:30-17:30"
                       "grey 17:30-19:00"
                       "task 19:00-20:00 borrowed"
                       "grey 20:00-23:00"))))))

(ert-deftest org-foresight-test-bands-private-is-not-borrowed ()
  "A private commitment outside the span is just life, not borrowed work."
  (org-foresight-test--with-day
      "* dinner with family
:PROPERTIES:
:CATEGORY: family
:END:
<2026-08-10 Mon 19:00-20:00>
"
    (let ((bands (org-foresight-test--bands (org-foresight-test--ts 0 0 10))))
      (should (member "private 19:00-20:00" bands))
      (should-not (seq-find (lambda (s) (string-match-p "borrowed" s)) bands)))))

(ert-deftest org-foresight-test-bands-non-working-day ()
  "With no work span the whole waking day is private time.
Which days are working days is stated here rather than inherited: this is the
one test that turns on a day not being one."
  (org-foresight-test--with-day "* nothing\n"
    (let ((org-foresight-workdays '(1 2 3 4 5))
          (org-foresight--shape-cache nil))
      ;; 2026-08-09 is a Sunday
      (should (equal (org-foresight-test--bands (org-foresight-test--ts 0 0 9))
                     '("grey 07:00-23:00"))))))

(ert-deftest org-foresight-test-bands-overlapping-entries ()
  "Two entries over the same minutes must not produce two bands for them."
  (org-foresight-test--with-day
      "* first
<2026-08-10 Mon 10:00-12:00>
* second
<2026-08-10 Mon 11:00-13:00>
"
    (let ((bands (org-foresight-day-blocks (org-foresight-test--ts 0 0 10))))
      (let ((cursor (car (plist-get (org-foresight-day-shape
                                     (org-foresight-test--ts 0 0 10))
                                    :awake))))
        (dolist (b bands)
          (should (equal (float-time (plist-get b :start)) (float-time cursor)))
          (setq cursor (plist-get b :end)))))))

;;;; Day shape

(defun org-foresight-test--work-string (shape)
  "Return SHAPE\='s working hours as \"HH:MM-HH:MM ...\", for readable assertions."
  (mapconcat (lambda (iv)
               (concat (format-time-string "%H:%M" (car iv)) "-"
                       (format-time-string "%H:%M" (cdr iv))))
             (plist-get shape :work) " "))

(ert-deftest org-foresight-test-day-shape-defaults ()
  (org-foresight-test--with-day "* nothing\n"
   (let ((org-foresight-workdays '(1 2 3 4 5))
         (org-foresight--shape-cache nil))
    (let ((shape (org-foresight-day-shape (org-foresight-test--ts 0 0 10))))
      (should (equal (format-time-string "%H:%M" (car (plist-get shape :awake)))
                     "07:00"))
      (should (equal (org-foresight-test--work-string shape)
                     "09:00-17:30")))
    ;; Sunday has no work span at all
    (should-not (plist-get (org-foresight-day-shape (org-foresight-test--ts 0 0 9))
                           :work)))))

(ert-deftest org-foresight-test-day-shape-honours-the-heading ()
  "Properties on the day's own heading beat the defaults."
  (let ((file (make-temp-file "org-foresight-day" nil ".org"
                              "* 2026\n** 2026-08 August\n*** 2026-08-10 Mon\n:PROPERTIES:\n:WAKE:  06:00\n:SLEEP: 22:00\n:WORK:  10:00-16:00\n:END:\n")))
    (unwind-protect
        (let ((org-foresight-day-file file)
              (org-foresight-awake '("07:00" . "23:00"))
              (org-foresight-work '(("09:00" . "17:30")))
              (org-foresight-workdays '(1 2 3 4 5))
              (org-foresight--shape-cache nil))
          (let ((shape (org-foresight-day-shape (org-foresight-test--ts 0 0 10))))
            (should (equal (format-time-string "%H:%M" (car (plist-get shape :awake)))
                           "06:00"))
            (should (equal (org-foresight-test--work-string shape) "10:00-16:00")))
          ;; a day with no heading falls back
          (should (equal (format-time-string
                          "%H:%M" (car (plist-get (org-foresight-day-shape
                                                   (org-foresight-test--ts 0 0 12))
                                                  :awake)))
                         "07:00")))
      (when (get-file-buffer file)
        (with-current-buffer (get-file-buffer file) (set-buffer-modified-p nil))
        (kill-buffer (get-file-buffer file)))
      (delete-file file))))

(ert-deftest org-foresight-test-day-shape-bedtime-after-midnight ()
  "A bedtime at or before waking belongs to the next morning, not this one."
  (let ((org-foresight-awake '("07:00" . "01:00"))
        (org-foresight-day-file nil)
        (org-foresight--shape-cache nil))
    (let* ((shape (org-foresight-day-shape (org-foresight-test--ts 0 0 10)))
           (awake (plist-get shape :awake)))
      (should (time-less-p (car awake) (cdr awake)))
      (should (= (/ (float-time (time-subtract (cdr awake) (car awake))) 3600.0)
                 18.0)))))

;;;; Places

(ert-deftest org-foresight-test-entry-place ()
  "Only a place someone wrote down counts; a bare call link is not a place."
  (org-foresight-test--with-day
      "* at the office
:PROPERTIES:
:LOCATION: 会議室A
:END:
* on a call
:PROPERTIES:
:LOCATION: https://teams.microsoft.com/l/meetup-join/xyz
:END:
* explicitly placed
:PROPERTIES:
:LOCATION: https://teams.microsoft.com/l/meetup-join/xyz
:PLACE:    office
:END:
* no location at all
"
    (let (places)
      (with-current-buffer (find-file-noselect (car org-agenda-files))
        (org-with-wide-buffer
         (org-map-entries
          (lambda () (push (cons (org-get-heading t t t t)
                                 (org-foresight--entry-place))
                           places))
          nil nil)))
      (setq places (nreverse places))
      (should (eq (cdr (assoc "at the office" places)) 'office))
      (should (null (cdr (assoc "on a call" places))))
      (should (eq (cdr (assoc "explicitly placed" places)) 'office))
      (should (null (cdr (assoc "no location at all" places)))))))

;;;; Travel
;; Getting somewhere is work, so a journey has to be booked like a meeting.
;; The two things that must never happen: inventing a journey from a video
;; call, and letting the commute push the end of the day outward.

(defmacro org-foresight-test--with-travel (text &rest body)
  "Run BODY over TEXT with a home/office/client travel matrix."
  (declare (indent 1))
  `(org-foresight-test--with-day ,text
     (let ((org-foresight-places '((office . "会議室\\|Office")
                                   (client . "様\\|Client")))
           (org-foresight-home-place 'home)
           (org-foresight-travel-matrix '(((home . office) . 60)
                                          ((office . client) . 30)
                                          ((client . home) . 90)))
           (org-foresight-travel-default 30))
       ,@body)))

(ert-deftest org-foresight-test-travel-out-and-back ()
  "One office meeting costs the hour plus both journeys.

Out arrives just in time, and back leaves as soon as the meeting is over --
nothing keeps you there once the thing that took you there has finished."
  (org-foresight-test--with-travel
      "* Project review
:PROPERTIES:
:LOCATION: 会議室A
:END:
<2026-08-10 Mon 14:00-15:00>
"
    (let ((bands (org-foresight-test--bands (org-foresight-test--ts 0 0 10))))
      (should (member "travel 13:00-14:00" bands))
      (should (member "meeting 14:00-15:00" bands))
      (should (member "travel 15:00-16:00" bands)))))

(ert-deftest org-foresight-test-travel-chains-places ()
  "home → office → client → home produces three journeys, not two."
  (org-foresight-test--with-travel
      "* Morning at the office
:PROPERTIES:
:LOCATION: 会議室A
:END:
<2026-08-10 Mon 10:00-11:00>
* Site visit
:PROPERTIES:
:LOCATION: 顧客様先
:END:
<2026-08-10 Mon 13:00-14:00>
"
    (let ((bands (org-foresight-test--bands (org-foresight-test--ts 0 0 10))))
      (should (member "travel 09:00-10:00" bands))   ; home → office, 60
      (should (member "travel 12:30-13:00" bands))   ; office → client, 30
      (should (member "travel 14:00-15:30" bands))))) ; client → home, 90

(ert-deftest org-foresight-test-travel-not-invented-by-a-call-link ()
  "A meeting whose only location is a video link must not move anyone."
  (org-foresight-test--with-travel
      "* Remote sync
:PROPERTIES:
:LOCATION: https://teams.microsoft.com/l/meetup-join/xyz
:END:
<2026-08-10 Mon 14:00-15:00>
"
    (let ((bands (org-foresight-test--bands (org-foresight-test--ts 0 0 10))))
      (should-not (seq-find (lambda (b) (string-prefix-p "travel" b)) bands)))))

(ert-deftest org-foresight-test-travel-same-place-twice ()
  "Two meetings at the same place are one trip out and one back."
  (org-foresight-test--with-travel
      "* First
:PROPERTIES:
:LOCATION: 会議室A
:END:
<2026-08-10 Mon 10:00-11:00>
* Second
:PROPERTIES:
:LOCATION: 会議室B
:END:
<2026-08-10 Mon 14:00-15:00>
"
    (let* ((bands (org-foresight-test--bands (org-foresight-test--ts 0 0 10)))
           (trips (seq-filter (lambda (b) (string-prefix-p "travel" b)) bands)))
      (should (= (length trips) 2)))))

(ert-deftest org-foresight-test-travel-eats-capacity ()
  "The commute has to come out of the day, or the day will run over."
  (org-foresight-test--with-travel
      "* Project review
:PROPERTIES:
:LOCATION: 会議室A
:END:
<2026-08-10 Mon 14:00-15:00>
"
    (let* ((day (org-foresight-test--ts 0 0 10))
           (morning (org-foresight-test--ts 6 0 10))
           (cap (org-foresight-capacity day nil morning)))
      ;; 8:30 of span, minus 1:00 meeting and 2:00 of travel
      (should (= (plist-get cap :free-min) (- 510 60 120))))))

(ert-deftest org-foresight-test-travel-outside-the-span-is-borrowed ()
  "An early start to reach a 09:30 meeting is taken from private time."
  (org-foresight-test--with-travel
      "* Early start
:PROPERTIES:
:LOCATION: 会議室A
:END:
<2026-08-10 Mon 09:30-10:30>
"
    (let ((bands (org-foresight-test--bands (org-foresight-test--ts 0 0 10))))
      ;; leaving at 08:30 is before the span opens
      (should (member "travel 08:30-09:30 borrowed" bands)))))

(ert-deftest org-foresight-test-travel-moves-out-of-the-way ()
  "Leaving at the last moment is only right when the last moment is free.

A journey that would run over something already booked slides earlier into
whatever gap will take it -- which is what a person does, and what keeps a
perfectly workable day from being reported as impossible."
  (org-foresight-test--with-travel
      "* At the office
:PROPERTIES:
:LOCATION: 会議室A
:END:
<2026-08-10 Mon 14:00-15:00>
* Something already at 13:00
<2026-08-10 Mon 13:00-14:00>
"
    (let ((bands (org-foresight-test--bands (org-foresight-test--ts 0 0 10))))
      ;; the hour before the meeting is taken, so the trip goes before that
      (should (member "travel 12:00-13:00" bands))
      (should-not (member "travel 13:00-14:00" bands)))))

(ert-deftest org-foresight-test-travel-still-arrives-in-time ()
  "Moving earlier must not mean arriving late: the slot still ends by the
meeting it serves, however far back it had to go."
  (org-foresight-test--with-travel
      "* At the office
:PROPERTIES:
:LOCATION: 会議室A
:END:
<2026-08-10 Mon 14:00-15:00>
* Busy all morning
<2026-08-10 Mon 09:00-13:30>
"
    (let* ((day (org-foresight-test--ts 0 0 10))
           (scan (org-foresight-scan 1 day))
           (trip (seq-find (lambda (e) (eq (plist-get e :kind) 'travel))
                           (aref (plist-get scan :ledger) 0))))
      (should trip)
      ;; 13:30–14:30 would overrun the meeting, so it must not be chosen
      (should (not (time-less-p (org-foresight-test--ts 14 0 10)
                                (plist-get trip :end)))))))

(ert-deftest org-foresight-test-travel-minutes-is-symmetric ()
  "Only one direction of a pair need be configured."
  (let ((org-foresight-travel-matrix '(((home . office) . 45)))
        (org-foresight-travel-default 30))
    (should (= (org-foresight--travel-minutes 'home 'office) 45))
    (should (= (org-foresight--travel-minutes 'office 'home) 45))
    (should (= (org-foresight--travel-minutes 'home 'nowhere) 30))
    (should (= (org-foresight--travel-minutes 'home 'home) 0))))

;;;; Attention
;; Occupying time and demanding all of it are two axes, and collapsing them is
;; what made a full day look impossible.  These pin both directions: a call you
;; only have to hear still costs the hour, and somebody else's fixture costs
;; nothing at all.

(defmacro org-foresight-test--with-attention (text &rest body)
  "Run BODY over TEXT with a background and an informational category."
  (declare (indent 1))
  `(org-foresight-test--with-day ,text
     (let ((org-foresight-background-categories '("listen"))
           (org-foresight-informational-categories '("club")))
       ,@body)))

(ert-deftest org-foresight-test-attention-resolution ()
  "An explicit property beats the category, so one meeting can be excepted."
  (org-foresight-test--with-attention
      "* explicitly listen-only
:PROPERTIES:
:ATTENTION: background
:END:
* by category
:PROPERTIES:
:CATEGORY: listen
:END:
* somebody else's
:PROPERTIES:
:CATEGORY: club
:END:
* an exception to its category
:PROPERTIES:
:CATEGORY: club
:ATTENTION: blocking
:END:
* ordinary
"
    (let (found)
      (with-current-buffer (find-file-noselect (car org-agenda-files))
        (org-with-wide-buffer
         (org-map-entries
          (lambda ()
            (push (cons (org-get-heading t t t t)
                        (org-foresight--entry-attention
                         (org-entry-get (point) "CATEGORY" t)))
                  found))
          nil nil)))
      (setq found (nreverse found))
      (should (eq (cdr (assoc "explicitly listen-only" found)) 'background))
      (should (eq (cdr (assoc "by category" found)) 'background))
      (should (eq (cdr (assoc "somebody else's" found)) 'informational))
      (should (eq (cdr (assoc "an exception to its category" found)) 'blocking))
      (should (eq (cdr (assoc "ordinary" found)) 'blocking)))))

(ert-deftest org-foresight-test-informational-takes-no-time ()
  "A child's fixture is a fact about the household, not an hour of work."
  (org-foresight-test--with-attention
      "* 子供の部活
:PROPERTIES:
:CATEGORY: club
:END:
<2026-08-10 Mon 10:00-12:00>
"
    (let* ((day (org-foresight-test--ts 0 0 10))
           (scan (org-foresight-scan 1 day))
           (cap (org-foresight-capacity day scan (org-foresight-test--ts 6 0 10))))
      ;; the whole span is still free
      (should (= (plist-get cap :free-min) 510.0))
      (should (= (plist-get cap :booked-min) 0.0))
      ;; and it claims no band, so it cannot displace work
      (should-not (seq-find (lambda (b) (equal (plist-get b :title) "子供の部活"))
                            (org-foresight-day-blocks day scan)))
      ;; but it is still in the ledger, because it is why the house is empty
      (should (seq-find
               (lambda (e) (and (equal (plist-get e :title) "子供の部活")
                                (eq (plist-get e :attention) 'informational)))
               (aref (plist-get scan :ledger) 0))))))

(ert-deftest org-foresight-test-background-costs-the-hour-but-shares-it ()
  "A call you only have to hear still takes the hour -- it just takes it
alongside whatever else is happening, rather than instead of it."
  (org-foresight-test--with-attention
      "* 全社定例
:PROPERTIES:
:CATEGORY: listen
:END:
<2026-08-10 Mon 10:00-11:00>
"
    (let* ((day (org-foresight-test--ts 0 0 10))
           (cap (org-foresight-capacity day nil (org-foresight-test--ts 6 0 10))))
      ;; unlike an informational entry, this one is on your clock
      (should (= (plist-get cap :booked-min) 60.0))
      (should (= (plist-get cap :free-min) 450.0)))))

(ert-deftest org-foresight-test-background-is-not-a-clash ()
  "Overlapping something that will share is not a day that cannot happen."
  (org-foresight-test--with-travel
      "* At the office
:PROPERTIES:
:LOCATION: 会議室A
:END:
<2026-08-10 Mon 14:00-15:00>
* 全社定例
:PROPERTIES:
:ATTENTION: background
:END:
<2026-08-10 Mon 14:00-15:00>
"
    (let ((bands (org-foresight-test--bands (org-foresight-test--ts 0 0 10))))
      ;; the band is the meeting that needs the hour; the one that will share
      ;; it does not displace anything and so gets no band of its own
      (should (= 1 (length (seq-filter
                            (lambda (b) (string-match-p "14:00-15:00" b))
                            bands)))))))

(ert-deftest org-foresight-test-informational-is-not-overtime ()
  "Somebody else's fixture must not be reported as your overrun."
  (org-foresight-test--with-signals
      "* 子供の部活
:PROPERTIES:
:CATEGORY: club
:END:
<2027-01-05 Tue 19:00-21:00>
"
    (let ((org-foresight-informational-categories '("club"))
          (org-foresight-horizon-days 400))
      (should-not (org-foresight-test--signal
                   "Outside work hours (invisible to capacity)")))
    ;; without the category it is ordinary work, and is reported
    (let ((org-foresight-informational-categories nil)
          (org-foresight-horizon-days 400))
      (should (org-foresight-test--signal
               "Outside work hours (invisible to capacity)")))))

;;;; Ledger

(ert-deftest org-foresight-test-ledger-explains-the-totals ()
  "Every total the scan reports must be reconstructible from the ledger."
  (org-foresight-test--with-day
      "* Project review
<2026-08-10 Mon 14:00-15:30>
* NEXT reply to procurement
SCHEDULED: <2026-08-10 Mon>
:PROPERTIES:
:EFFORT:   0:30
:END:
* NEXT draft the summary
SCHEDULED: <2026-08-10 Mon 10:00>
:PROPERTIES:
:EFFORT:   1:00
:END:
"
    (let* ((scan (org-foresight-scan 1 (org-foresight-test--ts 0 0 10)))
           (ledger (aref (plist-get scan :ledger) 0))
           (kinds (mapcar (lambda (e) (plist-get e :kind)) ledger)))
      (should (equal kinds '(task meeting promised)))
      ;; promised in the ledger reconstructs :committed exactly
      (should (= (apply #'+ (mapcar (lambda (e)
                                      (if (eq (plist-get e :kind) 'promised)
                                          (plist-get e :effort) 0))
                                    ledger))
                 (aref (plist-get scan :committed) 0)))
      ;; and the timed ones reconstruct the busy total
      (should (= (apply #'+ (mapcar (lambda (e)
                                      (if (memq (plist-get e :kind) '(task meeting))
                                          (plist-get e :effort) 0))
                                    ledger))
                 (/ (org-foresight--intervals-seconds
                     (aref (plist-get scan :busy) 0))
                    60.0))))))

;;;; Report blocks
;; Rendered from synthetic data, so these never touch the user's agenda files
;; and never wait on an ActivityWatch server.  The width assertion is the point:
;; the 80-column budget is the one contract every block shares, and multibyte
;; category names are exactly what breaks it.

(defconst org-foresight-test--clock
  (list :rows '(("work" . 540.0) ("会議" . 210.0) ("admin" . 150.0))
        :total 900.0
        :days 7
        :byday (vector 100.0 200.0 0.0 150.0 200.0 150.0 100.0)
        :today-rows '(("work" . 120.0) ("会議" . 45.0))
        :today-total 165.0
        :today-segments 6
        :today-intervals nil)
  "A clock plist shaped like `org-foresight-clock-scan' output.")

(defmacro org-foresight-test--without-aw (&rest body)
  "Run BODY with ActivityWatch reporting itself unavailable."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'org-foresight-observe-today) (lambda () nil))
             ((symbol-function 'org-foresight-observe-coverage) (lambda (_) nil)))
     ,@body))

(defun org-foresight-test--within-80 (s)
  "Non-nil when every line of S fits the 80-column budget."
  (seq-every-p (lambda (l) (<= (string-width l) 80))
               (split-string (substring-no-properties s) "\n")))

(ert-deftest org-foresight-test-report-spent ()
  "The retrospective block leads with what the clock says, in its own words."
  (org-foresight-test--without-aw
    (let ((s (org-foresight-report-spent org-foresight-test--clock)))
      ;; the clock's own total, named for what it is rather than for time at
      ;; the machine -- which is the number it gets measured against
      (should (string-match-p "Clocked" s))
      (should (string-match-p "spell" s))
      (should (string-match-p "vs 7d" s))
      (should (org-foresight-test--within-80 s)))))

(ert-deftest org-foresight-test-report-week ()
  (org-foresight-test--without-aw
    (let ((s (org-foresight-report-week org-foresight-test--clock)))
      (should (string-match-p "Week" s))
      (should (string-match-p "peak" s))
      (should (org-foresight-test--within-80 s)))))

(ert-deftest org-foresight-test-report-estimates ()
  "The review names the sizes actually estimated with, and only a few of them.

The claim and its evidence on one screen: the fitted line at the top, the
sizes it was fitted from underneath.  Capped, because a corpus of free-form
efforts has a long tail of sizes used once, and a row per singleton buries
the handful anybody could act on."
  (org-foresight-test--with-bias
      (mapconcat
       (lambda (est)
         (org-foresight-test--done-minutes
          (format "t%d" est) "work" est
          (max 1 (round (* 3 (expt (float est) 0.6))))))
       '(5 5 5 10 10 15 15 15 30 30 45 60 60 90 120 7 23 37 53 71 97)
       "")
    (org-foresight-learn-bias)
    (let* ((org-foresight-report-estimate-sizes 8)
           (s (org-foresight-report-estimates))
           (lines (split-string (substring-no-properties s) "\n")))
      (should (string-match-p "slope" (car lines)))
      ;; the header and no more sizes than were asked for
      (should (= 9 (length lines)))
      ;; in order of size, so the shape is the thing the eye follows
      (let ((sizes (mapcar (lambda (l)
                             (org-duration-to-minutes
                              (string-trim (substring l 0 6))))
                           (cdr lines))))
        (should (equal sizes (sort (copy-sequence sizes) #'<))))
      (should (org-foresight-test--within-80 s))
      ;; and it obeys the margin rule once the review has indented it
      (should (seq-every-p (lambda (l) (or (string-empty-p l)
                                           (string-prefix-p " " l)))
                           (split-string
                            (substring-no-properties
                             (org-foresight-report--indent s))
                            "\n"))))))

(ert-deftest org-foresight-test-report-empty-clock ()
  "A day with nothing clocked must render a message, not crash or blank out."
  (org-foresight-test--without-aw
    (let ((empty (list :rows nil :total 0 :days 7 :byday (make-vector 7 0)
                       :today-rows nil :today-total 0 :today-segments 0
                       :today-intervals nil)))
      (should (string-match-p "Clocked 0:00" (org-foresight-report-spent empty)))
      (should (string-match-p "no clocked time"
                              (org-foresight-report-week empty))))))

(ert-deftest org-foresight-test-report-spent-degrades ()
  "With no ActivityWatch the block drops the parts that need it and keeps the
rest, rather than signalling or blanking out."
  (org-foresight-test--without-aw
    (let ((s (org-foresight-report-spent org-foresight-test--clock)))
      (should (string-match-p "Clocked" s))
      ;; nothing that only the machine could have said
      (should-not (string-match-p "at the machine" s))
      (should-not (string-match-p "switching" s))
      (should-not (string-match-p "leak" s)))))

(ert-deftest org-foresight-test-category-table-truncates ()
  "An over-long category is cut to the column, not allowed to widen the table."
  (let ((s (org-foresight-report--category-table
            '(("a-very-long-project-name-that-overflows" . 60.0))
            60.0 60.0 "Project")))
    (should (org-foresight-test--within-80 s))))

(ert-deftest org-foresight-test-report-style-selects-block ()
  "`org-foresight-report-style' drives which block `--body' produces."
  (org-foresight-test--without-aw
    (cl-letf (((symbol-function 'org-foresight-clock-scan)
               (lambda (_) org-foresight-test--clock)))
      (let ((org-foresight-report-style 'review))
        (should (string-match-p "Week" (org-foresight-report--body))))
      (let ((org-foresight-report-style 'daily))
        (should (string-match-p "Clocked" (org-foresight-report--body))))
      (let ((org-foresight-report-style nil))
        (should (null (org-foresight-report--body)))))))

;;;; Signals
;; Every signal is a claim about the user's data, so each gets both a case that
;; must fire and a case that must not: a board that cries wolf is worse than no
;; board, because it stops being read.

(defun org-foresight-test--stamp (offset &optional from to)
  "Return an active timestamp OFFSET days from today, optionally FROM-TO.

Signals are always computed about today, so a test that names a date is a
test that stops meaning what it said the moment the date passes."
  (concat "<" (format-time-string
               "%Y-%m-%d %a"
               (time-add (org-foresight--day-start 0) (days-to-time offset)))
          (when from (concat " " from))
          (when (and from to) (concat "-" to))
          ">"))

(defun org-foresight-test--signal (label)
  "Return the findings filed under LABEL by `org-foresight-signals'.
Always recomputes: a test that changes a threshold and asks again must see
the new answer, not the one cached moments earlier."
  (cdr (assoc label (org-foresight-signals t))))

(defmacro org-foresight-test--with-signals (text &rest body)
  "Run BODY over TEXT with signal thresholds pinned."
  (declare (indent 1))
  `(org-foresight-test--with-org ,text
     (let ((org-foresight-procrastination-threshold 3)
           (org-foresight-horizon-days 14)
           (org-foresight-followup-keywords '("WAIT"))
           (org-foresight-meeting-categories '("outlook"))
           (org-foresight-wip-keywords nil)
           (org-foresight-wip-limit 2)
           (org-foresight-undecided-enabled nil)
           (org-foresight-undecided-files nil)
           (org-foresight-borrow-warn 180)
           (org-foresight-leak-warn 90)
           (org-foresight-awake '("07:00" . "23:00"))
           (org-foresight-work '(("09:00" . "17:30")))
           ;; See `org-foresight-test--with-day': signals are computed for
           ;; today, so today has to be a working day whatever day it is.
           (org-foresight-workdays '(0 1 2 3 4 5 6))
           (org-foresight-day-file nil)
           (org-foresight--shape-cache nil)
           (org-foresight-surge-cache-file "/nonexistent/surge.eld")
           ;; Signals are memoized; a test must never read what the previous
           ;; one computed, or the assertions describe someone else's corpus.
           (org-foresight--signals-cache nil)
           (org-log-note-headings org-log-note-headings))
       ,@body)))

(ert-deftest org-foresight-test-signal-procrastination ()
  "Three reschedules is a decision not being made; two is just planning."
  (org-foresight-test--with-signals
      "* NEXT keeps sliding
:LOGBOOK:
- Rescheduled from \"[2026-08-01 Sat]\" on [2026-08-02 Sun 09:00]
- Rescheduled from \"[2026-08-02 Sun]\" on [2026-08-03 Mon 09:00]
- Rescheduled from \"[2026-08-03 Mon]\" on [2026-08-04 Tue 09:00]
:END:
* NEXT moved once
:LOGBOOK:
- Rescheduled from \"[2026-08-01 Sat]\" on [2026-08-02 Sun 09:00]
:END:
"
    (let ((found (org-foresight-test--signal "Kept moving (not really NEXT)")))
      (should (= (length found) 1))
      (should (equal (plist-get (car found) :title) "keeps sliding"))
      (should (string-match-p "3 times" (plist-get (car found) :note))))))

(ert-deftest org-foresight-test-signal-procrastination-counts-own-entry-only ()
  "A child's reschedules must not be charged to its parent."
  (org-foresight-test--with-signals
      "* NEXT parent
:LOGBOOK:
- Rescheduled from \"[2026-08-01 Sat]\" on [2026-08-02 Sun 09:00]
:END:
** NEXT child
:LOGBOOK:
- Rescheduled from \"[2026-08-01 Sat]\" on [2026-08-02 Sun 09:00]
- Rescheduled from \"[2026-08-02 Sun]\" on [2026-08-03 Mon 09:00]
- Rescheduled from \"[2026-08-03 Mon]\" on [2026-08-04 Tue 09:00]
:END:
"
    (let ((found (org-foresight-test--signal "Kept moving (not really NEXT)")))
      (should (= (length found) 1))
      (should (equal (plist-get (car found) :title) "child")))))

(ert-deftest org-foresight-test-signal-unplannable ()
  "A near deadline with no estimate cannot be placed, so it must be surfaced."
  (org-foresight-test--with-signals
      (concat "* NEXT due soon, unestimated\nDEADLINE: "
              (org-foresight-test--stamp 2) "\n"
              "* NEXT due soon, estimated\nDEADLINE: "
              (org-foresight-test--stamp 2)
              "\n:PROPERTIES:\n:EFFORT:   1:00\n:END:\n"
              "* DONE already finished\nDEADLINE: "
              (org-foresight-test--stamp 2) "\n")
    (let ((found (org-foresight-test--signal
                  "Unplannable (deadline, no estimate)")))
      (should (= (length found) 1))
      (should (equal (plist-get (car found) :title) "due soon, unestimated")))))

(ert-deftest org-foresight-test-signal-followup-overdue ()
  "Handed-off work whose check-in has passed has gone quiet."
  (org-foresight-test--with-signals
      "* WAIT reply from vendor
SCHEDULED: <2026-08-05 Wed>
* WAIT check in next week
SCHEDULED: <2026-08-20 Thu>
* NEXT my own overdue task
SCHEDULED: <2026-08-05 Wed>
"
    (let ((found (org-foresight-test--signal "Gone quiet (follow-up overdue)")))
      (should (= (length found) 1))
      (should (equal (plist-get (car found) :title) "reply from vendor")))))

(ert-deftest org-foresight-test-signal-meetings ()
  "A future meeting in a watched category with no prep recorded is a signal."
  (org-foresight-test--with-signals
      "* review with the board
:PROPERTIES:
:CATEGORY: outlook
:END:
<2027-01-05 Tue 10:00-11:00>
* already prepared
:PROPERTIES:
:CATEGORY: outlook
:PLAN_PREP: t
:END:
<2027-01-06 Wed 10:00-11:00>
* private appointment
:PROPERTIES:
:CATEGORY: personal
:END:
<2027-01-07 Thu 10:00-11:00>
"
    (let ((found (org-foresight-test--signal "Meetings without prep")))
      (should (= (length found) 1))
      (should (equal (plist-get (car found) :title) "review with the board")))))

(ert-deftest org-foresight-test-signal-meetings-ignores-the-past ()
  "A meeting that already happened needs no preparation."
  (org-foresight-test--with-signals
      "* last year's kickoff
:PROPERTIES:
:CATEGORY: outlook
:END:
<2020-01-06 Mon 10:00-11:00>
"
    (should (null (org-foresight-test--signal "Meetings without prep")))))

(ert-deftest org-foresight-test-signal-orphans ()
  "Prep whose meeting has vanished from the calendar is probably dead work."
  (org-foresight-test--with-signals
      "* the meeting
:PROPERTIES:
:UID: still-here
:CATEGORY: outlook
:PLAN_PREP: t
:END:
<2027-01-05 Tue 10:00-11:00>
* NEXT prep for the live meeting
:PROPERTIES:
:PLAN_MEETING_UID: still-here
:END:
* NEXT prep for a cancelled meeting
:PROPERTIES:
:PLAN_MEETING_UID: long-gone
:END:
"
    (let ((found (org-foresight-test--signal "Orphaned prep")))
      (should (= (length found) 1))
      (should (equal (plist-get (car found) :title)
                     "prep for a cancelled meeting")))))

(ert-deftest org-foresight-test-signal-outside-work-hours ()
  "Work parked outside the working hours is subtracted from nothing, so it
must be named: it is the work that quietly stops the day ending on time."
  (org-foresight-test--with-window
    (org-foresight-test--with-signals
        "* NEXT evening call
<2027-01-05 Tue 19:00-20:30>
* NEXT during the day
<2027-01-05 Tue 10:00-11:00>
* NEXT saturday work
<2027-01-09 Sat 10:00-11:00>
"
      ;; Stated here rather than inherited: half of what this test asserts is
      ;; that a Saturday is not a working day.
      (let* ((org-foresight-workdays '(1 2 3 4 5))
             (org-foresight--shape-cache nil)
             (org-foresight-horizon-days 400)
             (found (org-foresight-test--signal
                     "Outside work hours (invisible to capacity)"))
             (titles (mapcar (lambda (f) (plist-get f :title)) found)))
        (should (member "evening call" titles))
        (should (member "saturday work" titles))
        (should-not (member "during the day" titles))))))

(ert-deftest org-foresight-test-signal-outside-work-hours-reports-once ()
  "A repeating out-of-hours meeting is one problem, not fifty."
  (org-foresight-test--with-window
    (org-foresight-test--with-signals
        "* NEXT nightly deploy watch
<2027-01-05 Tue 19:00-20:00 +1d>
"
      (let ((org-foresight-horizon-days 400))
        (should (= (length (org-foresight-test--signal
                            "Outside work hours (invisible to capacity)"))
                   1))))))

(ert-deftest org-foresight-test-signal-undecided ()
  "Only a heading nobody has decided anything about counts.

The exclusions are the whole point: measured against a real journal the naive
rule matched 17% of headings, mostly diary entries and date-tree scaffolding.
A board that lists things which are not problems stops being read."
  (org-foresight-test--with-signals
      "* a thought I never decided about
* 2026-08-11 Tuesday
* meeting notes <2026-08-11 Tue 14:00>
* NEXT already decided
* has a child
** NEXT the child
* was worked on
:LOGBOOK:
CLOCK: [2026-08-10 Mon 09:00]--[2026-08-10 Mon 10:00] =>  1:00
:END:
"
    (let* ((org-foresight-undecided-enabled t)
           (titles (mapcar (lambda (f) (plist-get f :title))
                           (org-foresight-test--signal
                            "Undecided (captured, not decided)"))))
      (should (equal titles '("a thought I never decided about"))))))

(ert-deftest org-foresight-test-signal-undecided-off-by-default ()
  "The signal stays silent unless it has been asked for."
  (org-foresight-test--with-signals "* a thought I never decided about\n"
    (should (null (org-foresight-test--signal
                   "Undecided (captured, not decided)")))))

(ert-deftest org-foresight-test-signal-wip ()
  "Started work is only worth reporting once there is too much of it."
  (org-foresight-test--with-signals
      "* ONGO one
* ONGO two
* ONGO three
"
    (let ((org-foresight-wip-keywords '("ONGO"))
          (org-foresight-wip-limit 2))
      (should (= (length (org-foresight-test--signal "Too much in flight")) 3)))
    ;; at or below the limit it says nothing
    (let ((org-foresight-wip-keywords '("ONGO"))
          (org-foresight-wip-limit 5))
      (should (null (org-foresight-test--signal "Too much in flight"))))
    ;; and nothing at all when no keyword has been nominated
    (let ((org-foresight-wip-keywords nil))
      (should (null (org-foresight-test--signal "Too much in flight"))))))

(ert-deftest org-foresight-test-signal-impossible ()
  "A journey that overlaps a meeting is a plan that cannot happen."
  (org-foresight-test--with-travel
      (concat "* At the office\n:PROPERTIES:\n:LOCATION: 会議室A\n:END:\n"
              (org-foresight-test--stamp 0 "10:00" "11:00") "\n"
              "* Call from home\n"
              (org-foresight-test--stamp 0 "09:30" "10:00") "\n")
    (let ((found (org-foresight-test--signal
                  "Impossible (travel clashes with a meeting)")))
      ;; the 09:00-10:00 journey runs straight through the 09:30 call
      (should found)
      (should (string-match-p "clashes with" (plist-get (car found) :note))))))

(ert-deftest org-foresight-test-signal-wont-fit ()
  "Work promised for today that no remaining gap can hold is called out.
NOW is pinned to the morning, or the answer would depend on the hour the
tests happen to run -- which is also the point of the signal: what fits
shrinks as the day goes on."
  (org-foresight-test--with-signals
      (let ((today (format-time-string "<%Y-%m-%d %a>"
                                       (org-foresight--day-start 0))))
        (concat "* NEXT a very long job\nSCHEDULED: " today
                "\n:PROPERTIES:\n:EFFORT:   12:00\n:END:\n"
                "* NEXT a short job\nSCHEDULED: " today
                "\n:PROPERTIES:\n:EFFORT:   0:15\n:END:\n"))
    (let* ((day (org-foresight--day-start 0))
           (morning (org-foresight--hhmm-on day "06:00"))
           (titles (mapcar (lambda (f) (plist-get f :title))
                           (org-foresight--wont-fit-findings
                            (org-foresight-scan 1 day) morning))))
      (should (member "a very long job" titles))
      (should-not (member "a short job" titles)))))

(ert-deftest org-foresight-test-signal-leaking-reads-only-the-cache ()
  "The leak signal must not reach for the network while an agenda is drawing."
  (let ((cache (make-temp-file "org-foresight-leak" nil ".eld"
                               (prin1-to-string '(:leak 120.0 :lost 30.0
                                                  :samples 12)))))
    (unwind-protect
        (let ((org-foresight-leak-cache-file cache)
              (org-foresight-leak-warn 90))
          (cl-letf (((symbol-function 'org-foresight-observe--get-json)
                     (lambda (&rest _) (error "network touched"))))
            (let ((found (org-foresight--leak-findings)))
              (should found)
              (should (string-match-p "2:00" (plist-get (car found) :note))))
            ;; and below the threshold it says nothing
            (let ((org-foresight-leak-warn 180))
              (should (null (org-foresight--leak-findings))))))
      (delete-file cache))))

(ert-deftest org-foresight-test-signals-quiet-when-nothing-is-wrong ()
  "A clean file must produce no groups at all, not empty ones."
  (org-foresight-test--with-signals
      "* NEXT a perfectly ordinary task
SCHEDULED: <2027-01-05 Tue>
:PROPERTIES:
:EFFORT:   1:00
:END:
"
    (should (null (org-foresight-signals)))
    (should (string-match-p "nothing unaccounted for"
                            (org-foresight-report-signals)))))

(ert-deftest org-foresight-test-signals-block-within-80 ()
  "Long titles must be cut to the column, not allowed to run off the board.
Multibyte titles are the real hazard: they are twice as wide as they look."
  (org-foresight-test--with-window
    (org-foresight-test--with-signals
        "* NEXT 非常に長い日本語のタスク名で桁あふれを起こしかねないもの、さらに続く
<2027-01-05 Tue 19:00-20:30>
DEADLINE: <2027-01-05 Tue>
"
      (let* ((org-foresight-horizon-days 400)
             (s (org-foresight-report-signals)))
        (should (org-foresight-test--within-80 s))))))

(ert-deftest org-foresight-test-log-prefix-follows-org ()
  "The reschedule text is read from Org's own settings, not hardcoded."
  (should (equal (org-foresight--log-prefix 'reschedule) "Rescheduled from "))
  (let ((org-log-note-headings '((reschedule . "Moved from %S on %t"))))
    (should (equal (org-foresight--log-prefix 'reschedule) "Moved from "))))

;;;; Meeting preparation
;; The only writes the package makes, so they are pinned hardest: what gets
;; created, that re-running creates nothing more, and that the write does not
;; corrupt the reschedule history the procrastination signal reads.

(defun org-foresight-test--count (needle haystack)
  "Return how many times NEEDLE occurs in HAYSTACK."
  (let ((n 0) (i 0))
    (while (setq i (string-search needle haystack i))
      (setq n (1+ n) i (1+ i)))
    n))

(defmacro org-foresight-test--with-meeting (calendar &rest body)
  "Run BODY with CALENDAR as the agenda and a scratch task file.
Binds `org-foresight-test-tasks' to the task file's path."
  (declare (indent 1))
  `(let ((cal (make-temp-file "org-foresight-cal" nil ".org" ,calendar))
         (org-foresight-test-tasks (make-temp-file "org-foresight-task" nil ".org" "")))
     (unwind-protect
         (let ((org-agenda-files (list cal))
               (org-todo-keywords '((sequence "NEXT" "ONGO" "|" "DONE")))
               (org-foresight-meeting-categories '("outlook"))
               (org-foresight-meeting-prep "0:30")
               (org-foresight-meeting-follow "0:15")
               (org-foresight-task-file org-foresight-test-tasks)
               (org-foresight-task-datetree t)
               (org-log-reschedule 'time))
           (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
             ,@body))
       (dolist (f (list cal org-foresight-test-tasks))
         (when (get-file-buffer f)
           (with-current-buffer (get-file-buffer f) (set-buffer-modified-p nil))
           (kill-buffer (get-file-buffer f)))
         (delete-file f)))))

(defvar org-foresight-test-tasks nil
  "Path of the scratch task file inside `org-foresight-test--with-meeting'.")

(ert-deftest org-foresight-test-prepare-meetings-creates-both-tasks ()
  "Preparation lands before the meeting and follow-up right after it."
  (org-foresight-test--with-meeting
      "* board review
:PROPERTIES:
:UID:      uid-1
:CATEGORY: outlook
:END:
<2027-01-05 Tue 10:00-11:00>
"
    (org-foresight-prepare-meetings)
    (let ((text (with-current-buffer
                    (find-file-noselect org-foresight-test-tasks)
                  (buffer-string))))
      (should (= 1 (org-foresight-test--count "Prep: board review" text)))
      (should (= 1 (org-foresight-test--count "Follow up: board review" text)))
      ;; prep ends when the meeting starts; follow-up begins when it ends
      (should (string-match-p "SCHEDULED: <2027-01-05 Tue 09:30>" text))
      (should (string-match-p "SCHEDULED: <2027-01-05 Tue 11:00>" text))
      (should (= 1 (org-foresight-test--count ":EFFORT:   0:30" text)))
      (should (= 1 (org-foresight-test--count ":EFFORT:   0:15" text)))
      ;; both point back at the meeting, which is what makes orphans findable
      (should (= 2 (org-foresight-test--count ":PLAN_MEETING_UID: uid-1" text)))
      ;; and they read in the order they happen, not upside down
      (should (< (string-search "Prep: board review" text)
                 (string-search "Follow up: board review" text))))))

(ert-deftest org-foresight-test-prepare-meetings-is-idempotent ()
  "Re-running must create nothing: the meeting is marked, not re-read."
  (org-foresight-test--with-meeting
      "* board review
:PROPERTIES:
:UID:      uid-1
:CATEGORY: outlook
:END:
<2027-01-05 Tue 10:00-11:00>
"
    (org-foresight-prepare-meetings)
    (org-foresight-prepare-meetings)
    (org-foresight-prepare-meetings)
    (let ((text (with-current-buffer
                    (find-file-noselect org-foresight-test-tasks)
                  (buffer-string))))
      (should (= 1 (org-foresight-test--count "Prep: board review" text))))
    ;; and the meeting itself now carries the marker
    (should (string-match-p
             ":PLAN_PREP:"
             (with-current-buffer (find-file-noselect (car org-agenda-files))
               (buffer-string))))))

(ert-deftest org-foresight-test-prepare-meetings-does-not-log-a-reschedule ()
  "Scheduling a brand-new task must not look like a reschedule.
If it did, every generated task would arrive pre-loaded with the very
evidence the procrastination signal counts."
  (org-foresight-test--with-meeting
      "* board review
:PROPERTIES:
:UID:      uid-1
:CATEGORY: outlook
:END:
<2027-01-05 Tue 10:00-11:00>
"
    (org-foresight-prepare-meetings)
    (let ((text (with-current-buffer
                    (find-file-noselect org-foresight-test-tasks)
                  (buffer-string))))
      (should (= 0 (org-foresight-test--count "Rescheduled from" text))))))

;;;; Forward load

(ert-deftest org-foresight-test-load-skips-non-working-days ()
  "A weekend offers no capacity, so it must not appear as a place to put work."
  (org-foresight-test--with-window
    (org-foresight-test--with-org "* nothing\n"
      (let* ((s (substring-no-properties
                 (org-foresight-report-load 14 nil
                                            (org-foresight-test--ts 6 0 10))))
             (today (org-foresight--day-start 0))
             (weekend 0))
        (dotimes (i 14)
          (let* ((day (time-add today (days-to-time i)))
                 (dow (nth 6 (decode-time day))))
            (when (memq dow '(0 6))
              (setq weekend (1+ weekend))
              (should-not (string-match-p
                           (regexp-quote (format-time-string "%a %m-%d" day))
                           s)))))
        (should (> weekend 0))
        ;; and it stops at the configured number of rows rather than the horizon
        (should (<= (length (split-string s "\n")) org-foresight-load-rows))))))

(ert-deftest org-foresight-test-load-speaks-the-capacity-vocabulary ()
  "The forward view has to be readable against today, or it is a second system.

One number, `:headroom-min', phrased as the verdict phrases it, and the same
stacked bar at the same scale -- so a day drawn in both blocks is drawn the
same length in both."
  (org-foresight-test--with-day
      (concat "* NEXT big piece of work
SCHEDULED: " (org-foresight-test--stamp 0) "
:PROPERTIES:
:EFFORT: 12:00
:END:
")
    (let* ((now (org-foresight--day-start 0))
           (s (org-foresight-report-load 14 nil now))
           (plain (substring-no-properties s)))
      ;; a day promised past its span says so in the verdict's own words
      (should (string-match-p "OVER by" plain))
      (should (string-match-p "to promise" plain))
      ;; the overflow is shown rather than clipped, exactly as the bar above
      (should (string-match-p "┃" plain))
      ;; today is drawn at the same length in both blocks
      (let* ((cap (org-foresight-capacity (org-foresight--day-start 0) nil now))
             (bar (org-foresight-report--bar cap))
             (row (car (split-string plain "\n"))))
        (should (string-match-p (regexp-quote (substring-no-properties bar))
                                row))))))

(ert-deftest org-foresight-test-report-load-within-80 ()
  (org-foresight-test--with-window
    (org-foresight-test--with-org "* nothing\n"
      (let ((s (org-foresight-report-load 14 nil (org-foresight-test--ts 6 0 10))))
        (should (org-foresight-test--within-80 s))))))

(ert-deftest org-foresight-test-styles-are-the-two-that-are-left ()
  "Two agenda views, and both put their tail after the listing.

There used to be a third that drew the same agenda with a different tail
underneath, and it was never opened: a view whose top half is identical to
another is not a place, it is a toggle.  What it had to say now lives on the
board, which is not an agenda view at all."
  (should (assq 'daily org-foresight-report-renderers))
  (should (assq 'review org-foresight-report-renderers))
  (should-not (assq 'plan org-foresight-report-renderers))
  (should (eq (org-foresight-report--place 'daily) 'bottom))
  (should (eq (org-foresight-report--place 'review) 'bottom)))

(ert-deftest org-foresight-test-signal-rows-are-actionable ()
  "A signal must be fixable from where it is reported.

This is what separates catching a problem from merely listing it: with the
entry's marker on the row, `s' schedules it and `e' estimates it without
leaving the board."
  (org-foresight-test--with-signals
      "* WAIT reply from vendor
SCHEDULED: <2020-01-01 Wed>
"
    (let* ((s (org-foresight-report-signals))
           (pos (string-match "reply from vendor" s)))
      (should pos)
      (should (markerp (get-text-property pos 'org-marker s)))
      (should (markerp (get-text-property pos 'org-hd-marker s)))
      (should (eq (get-text-property pos 'org-agenda-type s) 'agenda)))))

(ert-deftest org-foresight-test-summary-signals-are-inert ()
  "A finding that is a total, not an entry, has nothing to act on."
  (let ((cache (make-temp-file "org-foresight-leak" nil ".eld"
                               (prin1-to-string '(:leak 120.0 :lost 30.0
                                                  :samples 12)))))
    (unwind-protect
        (let* ((org-foresight-leak-cache-file cache)
               (org-foresight-leak-warn 90)
               (s (org-foresight-report-signals
                   (list (cons "Leaking (unclocked work)"
                               (org-foresight--leak-findings)))))
               (pos (string-match "Time worked without a clock" s)))
          (should pos)
          (should (null (get-text-property pos 'org-marker s))))
      (delete-file cache))))

;;;; Estimate bias

(defmacro org-foresight-test--with-bias (text &rest body)
  "Run BODY over TEXT with a scratch bias cache."
  (declare (indent 1))
  `(let ((cache (make-temp-file "org-foresight-bias" nil ".eld")))
     (unwind-protect
         (org-foresight-test--with-day ,text
           (let ((org-foresight-bias-cache-file cache)
                 (org-foresight-bias-enabled t)
                 (org-foresight-bias-window 90)
                 (org-foresight-bias-min-samples 3)
                 (org-foresight-bias-max-samples 600)
                 (org-foresight-bias-slope-range '(0.3 . 1.3))
                 (org-foresight-bias-factor-range '(0.5 . 4.0))
                 (org-foresight-bias-abandoned-keywords '("CANCEL"))
                 (org-foresight--bias-cache nil))
             ,@body))
       (delete-file cache))))

(defun org-foresight-test--done (title category est act-hours &optional day-offset)
  "Return org text for a finished task estimated EST and clocked ACT-HOURS.
Dated relative to today, so the entry stays inside the learning window
however long after it is written the tests are run."
  (let* ((day (org-foresight--day-start (or day-offset 1)))
         (stamp (lambda (h)
                  (format-time-string (format "[%%Y-%%m-%%d %%a %02d:00]" h) day))))
    (concat "* DONE " title "\n"
            "CLOSED: " (funcall stamp 18) "\n"
            ":PROPERTIES:\n"
            ":EFFORT:   " est "\n"
            ":CATEGORY: " category "\n"
            ":END:\n"
            ":LOGBOOK:\n"
            "CLOCK: " (funcall stamp 9) "--" (funcall stamp (+ 9 act-hours))
            (format " => %d:00\n" act-hours)
            ":END:\n")))

(ert-deftest org-foresight-test-learn-bias ()
  "The multiplier is the median ratio of clocked time to estimate."
  (org-foresight-test--with-bias
      (concat
       ;; three reporting tasks, each estimated 1:00 and taking 1, 2, 3 hours
       (org-foresight-test--done "r1" "reporting" "1:00" 1)
       (org-foresight-test--done "r2" "reporting" "1:00" 2)
       (org-foresight-test--done "r3" "reporting" "1:00" 3))
    (org-foresight-learn-bias)
    (should (= (org-foresight-bias-factor "reporting") 2.0))
    (should (= (org-foresight-bias-factor nil) 2.0))))

(ert-deftest org-foresight-test-bias-falls-back-to-overall ()
  "A category with too little history is planned by the overall figure."
  (org-foresight-test--with-bias
      (concat
       (org-foresight-test--done "r1" "reporting" "1:00" 2)
       (org-foresight-test--done "r2" "reporting" "1:00" 2)
       (org-foresight-test--done "r3" "reporting" "1:00" 2)
       ;; a single admin task is not enough to judge admin by
       (org-foresight-test--done "a1" "admin" "1:00" 5))
    (org-foresight-learn-bias)
    (should (= (org-foresight-bias-factor "reporting") 2.0))
    (should (= (org-foresight-bias-factor "admin")
               (org-foresight-bias-factor nil)))))

(defun org-foresight-test--done-minutes (title category est-min act-min
                                              &optional keyword)
  "Return org text for a KEYWORD task estimated EST-MIN and clocked ACT-MIN."
  (let* ((day (org-foresight--day-start 1))
         (stamp (lambda (mins)
                  (format-time-string "[%Y-%m-%d %a %H:%M]"
                                      (time-add day (seconds-to-time
                                                     (+ (* 9 3600)
                                                        (* 60 mins))))))))
    (concat "* " (or keyword "DONE") " " title "\n"
            "CLOSED: " (funcall stamp 600) "\n"
            ":PROPERTIES:\n"
            ":EFFORT:   " (org-duration-from-minutes est-min) "\n"
            ":CATEGORY: " category "\n"
            ":END:\n"
            ":LOGBOOK:\n"
            "CLOCK: " (funcall stamp 0) "--" (funcall stamp act-min) "\n"
            ":END:\n")))

(ert-deftest org-foresight-test-bias-recovers-a-slope ()
  "A corpus that really is a power law is read back as one.

The whole point of fitting rather than averaging: when small estimates are
missed by more than large ones, one multiplier cannot say so and a slope can.
Built from `act = 3 * est^0.6\=', which is the shape being claimed."
  (org-foresight-test--with-bias
      (mapconcat
       (lambda (est)
         (org-foresight-test--done-minutes
          (format "t%d" est) "work" est
          (max 1 (round (* 3 (expt (float est) 0.6))))))
       '(2 2 5 5 10 10 15 15 30 30 45 60 60 90 120 120)
       "")
    (org-foresight-learn-bias)
    (let ((b (plist-get (org-foresight--bias-data) :slope)))
      (should (< 0.5 b 0.7)))
    ;; and the multiplier it implies falls away as the estimate grows
    (should (> (org-foresight-bias-factor "work" 5)
               (org-foresight-bias-factor "work" 30)
               (org-foresight-bias-factor "work" 120)))
    ;; an estimate nobody ever wrote is answered between its neighbours
    (should (< (org-foresight-bias-factor "work" 30)
               (org-foresight-bias-factor "work" 20)
               (org-foresight-bias-factor "work" 15)))))

(ert-deftest org-foresight-test-bias-ignores-abandoned-work ()
  "Work that was dropped says nothing about the estimate it was given.

An hour\='s job abandoned after ten minutes is not evidence that hours take
minutes, and letting it count drags every estimate of that kind down."
  (org-foresight-test--with-bias
      (concat
       (org-foresight-test--done-minutes "d1" "work" 60 120)
       (org-foresight-test--done-minutes "d2" "work" 60 120)
       (org-foresight-test--done-minutes "d3" "work" 60 120)
       (org-foresight-test--done-minutes "x1" "work" 60 5 "CANCEL")
       (org-foresight-test--done-minutes "x2" "work" 60 5 "CANCEL"))
    (org-foresight-learn-bias)
    (should (= 3 (plist-get (org-foresight--bias-data) :samples)))
    (should (= 2.0 (org-foresight-bias-factor "work" 60)))))

(ert-deftest org-foresight-test-bias-is-clamped ()
  "However wild the history, the day is not planned around an absurdity.

A correction that quarters or quadruples an estimate is not a correction, it
is a different plan -- and one bad fortnight should not be allowed to make
one."
  (org-foresight-test--with-bias
      (mapconcat
       (lambda (est)
         (org-foresight-test--done-minutes (format "t%d" est) "work" est
                                           (* est 40)))
       '(2 2 5 5 10 15 30 60 120)
       "")
    (org-foresight-learn-bias)
    (let ((b (plist-get (org-foresight--bias-data) :slope)))
      (should (<= (car org-foresight-bias-slope-range) b
                  (cdr org-foresight-bias-slope-range))))
    (dolist (est '(2 5 15 60 120 480))
      (should (<= (car org-foresight-bias-factor-range)
                  (org-foresight-bias-factor "work" est)
                  (cdr org-foresight-bias-factor-range))))))

(ert-deftest org-foresight-test-bias-reads-a-cache-from-before-the-curve ()
  "A file written when the correction was one number still works.

It is the same statement with the slope pinned at 1, so it is read as that
rather than discarded: upgrading keeps the correction you had instead of
silently losing it until the next time you learn."
  (org-foresight-test--with-bias ""
    (with-temp-file org-foresight-bias-cache-file
      (prin1 '(:overall 2.0 :categories (("admin" . 3.0)) :samples 9
               :updated "2026-01-01")
             (current-buffer)))
    (setq org-foresight--bias-cache nil)
    (should (= 1.0 (plist-get (org-foresight--bias-data) :slope)))
    ;; a constant multiplier: the same whatever the estimate.  Compared
    ;; within a tolerance because the number makes a round trip through a
    ;; logarithm, and exp(log(3.0)) is 3.0000000000000004.
    (should (< (abs (- 2.0 (org-foresight-bias-factor nil 5))) 1e-9))
    (should (< (abs (- 2.0 (org-foresight-bias-factor nil 120))) 1e-9))
    (should (< (abs (- 3.0 (org-foresight-bias-factor "admin" 30))) 1e-9))))

(ert-deftest org-foresight-test-bias-minutes-is-what-the-correction-cost ()
  "The day says what the correction added to it, in the unit it added it in.

A multiplier stopped being a single number when the correction became a
curve, so the verdict names minutes instead -- which is the same unit as the
headroom beside it and therefore the only one worth comparing."
  (org-foresight-test--with-bias
      (concat
       (org-foresight-test--done-minutes "d1" "work" 60 120)
       (org-foresight-test--done-minutes "d2" "work" 60 120)
       (org-foresight-test--done-minutes "d3" "work" 60 120)
       "* NEXT write it up\n"
       "SCHEDULED: " (format-time-string "<%Y-%m-%d %a>"
                                         (org-foresight--day-start 0)) "\n"
       ":PROPERTIES:\n:EFFORT:   1:00\n:CATEGORY: work\n:END:\n")
    (org-foresight-learn-bias)
    (let* ((day (org-foresight--day-start 0))
           (scan (org-foresight-scan 1 day))
           (cap (org-foresight-capacity day scan))
           (ledger (aref (plist-get scan :ledger) 0))
           (sum 0.0))
      (dolist (e ledger)
        (when (eq (plist-get e :kind) 'promised)
          (setq sum (+ sum (- (plist-get e :effort-adj)
                              (plist-get e :effort))))))
      ;; an hour of work that reliably takes two is an hour of correction
      (should (< (abs (- 60.0 (plist-get cap :bias-min))) 0.001))
      (should (< (abs (- sum (plist-get cap :bias-min))) 0.001)))))

(ert-deftest org-foresight-test-a-corrected-estimate-says-so ()
  "A row showing a figure that is not the one in the file says both.

The estimate stays on the left because that is the number that was written
and the one being questioned; the arrow is the learning.  Below the
threshold nothing is said, because five columns spent reporting a two-minute
adjustment is five columns spent reporting nothing."
  (let ((org-foresight-bias-visible-minutes 5))
    (should (equal "2:00→2:48"
                   (org-foresight-report--effort-run
                    '(:effort 120.0 :effort-adj 168.0))))
    ;; pessimistic estimates are reported just as plainly
    (should (equal "1:00→0:40"
                   (org-foresight-report--effort-run
                    '(:effort 60.0 :effort-adj 40.0))))
    ;; inside the threshold, or inside a tenth, the row shows the figure the
    ;; day is planned around and says nothing further: a difference this
    ;; small is inside the rounding of the numbers beside it
    (should (equal "0:32"
                   (org-foresight-report--effort-run
                    '(:effort 30.0 :effort-adj 32.0))))
    (should (equal "8:20"
                   (org-foresight-report--effort-run
                    '(:effort 480.0 :effort-adj 500.0))))
    ;; and an entry nobody estimated has nothing to compare
    (should (equal "0:45"
                   (org-foresight-report--effort-run '(:effort-adj 45.0))))))

(ert-deftest org-foresight-test-bias-does-not-extrapolate ()
  "The curve stops where the evidence does.

A line fitted on jobs between five minutes and two hours says nothing about a
day-long one, and following it out there would shrink an eight-hour estimate
on no evidence at all -- wrong in the one direction this package exists to
prevent.  Past the ends of the evidence the correction goes flat."
  (org-foresight-test--with-bias
      (mapconcat
       (lambda (est)
         (org-foresight-test--done-minutes
          (format "t%d" est) "work" est
          (max 1 (round (* 3 (expt (float est) 0.6))))))
       '(5 5 10 10 15 15 30 30 60 60 120 120)
       "")
    (org-foresight-learn-bias)
    (should (equal '(5.0 . 120.0) (plist-get (org-foresight--bias-data) :range)))
    ;; beyond the largest estimate ever made, the factor stops moving
    (should (= (org-foresight-bias-factor "work" 120)
               (org-foresight-bias-factor "work" 480)))
    ;; and below the smallest, likewise
    (should (= (org-foresight-bias-factor "work" 5)
               (org-foresight-bias-factor "work" 1)))
    ;; inside it, the curve still falls away with size
    (should (> (org-foresight-bias-factor "work" 10)
               (org-foresight-bias-factor "work" 60)))))

(ert-deftest org-foresight-test-bias-without-history-is-neutral ()
  "With nothing learned the factor is 1.0: an unknown bias corrects nothing."
  (org-foresight-test--with-bias "* NEXT nothing finished yet\n"
    (should (= (org-foresight-bias-factor "reporting") 1.0))
    (should (= (org-foresight-bias-factor nil) 1.0))))

(ert-deftest org-foresight-test-bias-can-be-switched-off ()
  (org-foresight-test--with-bias
      (concat (org-foresight-test--done "r1" "reporting" "1:00" 2)
              (org-foresight-test--done "r2" "reporting" "1:00" 2)
              (org-foresight-test--done "r3" "reporting" "1:00" 2))
    (org-foresight-learn-bias)
    (should (= (org-foresight-bias-factor "reporting") 2.0))
    (let ((org-foresight-bias-enabled nil))
      (should (= (org-foresight-bias-factor "reporting") 1.0)))))

(ert-deftest org-foresight-test-bias-applies-to-promised ()
  "What is promised is charged at what the work actually takes."
  (org-foresight-test--with-bias
      (concat
       (org-foresight-test--done "r1" "reporting" "1:00" 2)
       (org-foresight-test--done "r2" "reporting" "1:00" 2)
       (org-foresight-test--done "r3" "reporting" "1:00" 2)
       "* NEXT write the report
SCHEDULED: <2026-08-10 Mon>
:PROPERTIES:
:EFFORT:   1:00
:CATEGORY: reporting
:END:
")
    (org-foresight-learn-bias)
    (let ((scan (org-foresight-scan 1 (org-foresight-test--ts 0 0 10))))
      (should (= (aref (plist-get scan :committed) 0) 120.0)))
    ;; and turning the correction off returns the raw estimate
    (let ((org-foresight-bias-enabled nil))
      (let ((scan (org-foresight-scan 1 (org-foresight-test--ts 0 0 10))))
        (should (= (aref (plist-get scan :committed) 0) 60.0))))))

(ert-deftest org-foresight-test-bias-ledger-keeps-both-figures ()
  "The ledger shows the estimate beside what it is being treated as."
  (org-foresight-test--with-bias
      (concat
       (org-foresight-test--done "r1" "reporting" "1:00" 2)
       (org-foresight-test--done "r2" "reporting" "1:00" 2)
       (org-foresight-test--done "r3" "reporting" "1:00" 2)
       "* NEXT write the report
SCHEDULED: <2026-08-10 Mon>
:PROPERTIES:
:EFFORT:   1:00
:CATEGORY: reporting
:END:
")
    (org-foresight-learn-bias)
    (let* ((scan (org-foresight-scan 1 (org-foresight-test--ts 0 0 10)))
           (e (seq-find (lambda (e) (eq (plist-get e :kind) 'promised))
                        (aref (plist-get scan :ledger) 0))))
      (should (= (plist-get e :effort) 60.0))
      (should (= (plist-get e :effort-adj) 120.0)))))

(ert-deftest org-foresight-test-bias-of-one-changes-nothing ()
  "A neutral multiplier must leave every figure exactly as it was.
This is the regression that keeps the correction honest: turning it on for a
person whose estimates are accurate should be invisible."
  (org-foresight-test--with-bias
      (concat
       (org-foresight-test--done "r1" "reporting" "1:00" 1)
       (org-foresight-test--done "r2" "reporting" "1:00" 1)
       (org-foresight-test--done "r3" "reporting" "1:00" 1)
       "* NEXT write the report
SCHEDULED: <2026-08-10 Mon>
:PROPERTIES:
:EFFORT:   1:30
:CATEGORY: reporting
:END:
")
    (org-foresight-learn-bias)
    (should (= (org-foresight-bias-factor "reporting") 1.0))
    (let ((with-bias
           (let ((org-foresight-bias-enabled t))
             (aref (plist-get (org-foresight-scan 1 (org-foresight-test--ts 0 0 10))
                              :committed)
                   0)))
          (without
           (let ((org-foresight-bias-enabled nil))
             (aref (plist-get (org-foresight-scan 1 (org-foresight-test--ts 0 0 10))
                              :committed)
                   0))))
      (should (= with-bias without))
      (should (= with-bias 90.0)))))

(ert-deftest org-foresight-test-bias-sizes-the-placement-slot ()
  "A task known to overrun is given the time it will actually need."
  (org-foresight-test--with-bias
      (concat
       (org-foresight-test--done "r1" "reporting" "1:00" 2)
       (org-foresight-test--done "r2" "reporting" "1:00" 2)
       (org-foresight-test--done "r3" "reporting" "1:00" 2)
       "* NEXT write the report
:PROPERTIES:
:EFFORT:   1:00
:CATEGORY: reporting
:END:
")
    (org-foresight-learn-bias)
    (let* ((cands (org-foresight--candidates (org-foresight-test--ts 0 0 10)))
           (c (seq-find (lambda (c) (equal (plist-get c :title) "write the report"))
                        cands)))
      (should c)
      (should (= (plist-get c :effort) 120.0))
      (should (= (plist-get c :estimate) 60.0)))))

;;;; Placement

(defun org-foresight-test--cand (title effort &optional deadline priority)
  "Build a placement candidate for tests."
  (list :marker nil :title title :effort effort :estimated t
        :deadline deadline :priority priority :scheduled nil))

(defun org-foresight-test--placed (result)
  "Render the placements of RESULT as \"HH:MM Title\" strings."
  (mapcar (lambda (p)
            (concat (format-time-string "%H:%M" (plist-get p :start))
                    " " (plist-get p :title)))
          (car result)))

(ert-deftest org-foresight-test-place-packs-in-order ()
  "Tasks go into the first slot that fits, in the order given, with a gap."
  (let* ((org-foresight-slot-gap 5)
         (free (org-foresight-test--ivs '(9 0 12 0)))
         (r (org-foresight--place
             (list (org-foresight-test--cand "a" 60)
                   (org-foresight-test--cand "b" 30)
                   (org-foresight-test--cand "c" 15))
             free 1000)))
    (should (equal (org-foresight-test--placed r)
                   '("09:00 a" "10:05 b" "10:40 c")))
    (should (null (cdr r)))))

(ert-deftest org-foresight-test-place-never-splits-across-a-meeting ()
  "A task that does not fit in the gap before a meeting moves after it,
rather than being cut in half by it."
  (let* ((org-foresight-slot-gap 5)
         ;; 09:00-10:00 then 11:00-17:00, i.e. a meeting at 10:00
         (free (org-foresight-test--ivs '(9 0 10 0) '(11 0 17 0)))
         (r (org-foresight--place
             (list (org-foresight-test--cand "long" 120)) free 1000)))
    (should (equal (org-foresight-test--placed r) '("11:00 long")))))

(ert-deftest org-foresight-test-place-respects-the-budget ()
  "The surge reserve is taken out first: placement stops at the budget even
when the calendar still looks empty."
  (let* ((org-foresight-slot-gap 0)
         (free (org-foresight-test--ivs '(9 0 17 0)))
         (r (org-foresight--place
             (list (org-foresight-test--cand "a" 60)
                   (org-foresight-test--cand "b" 60)
                   (org-foresight-test--cand "c" 60))
             free 120)))
    (should (= (length (car r)) 2))
    (should (equal (cdr (car (cdr r))) "no budget left"))))

(ert-deftest org-foresight-test-place-reports-what-would-not-fit ()
  "Anything left over must be named, not silently dropped."
  (let* ((free (org-foresight-test--ivs '(9 0 9 30)))
         (r (org-foresight--place
             (list (org-foresight-test--cand "too big" 120)) free 1000)))
    (should (null (car r)))
    (should (equal (mapcar (lambda (s) (plist-get (car s) :title)) (cdr r))
                   '("too big")))
    (should (equal (cdr (car (cdr r))) "no gap long enough"))))

(ert-deftest org-foresight-test-place-skips-slivers ()
  "Stretches too short to start anything in are not treated as capacity."
  (let* ((org-foresight-plan-min-slot 10)
         (free (org-foresight-test--ivs '(9 0 9 5) '(10 0 11 0)))
         (r (org-foresight--place
             (list (org-foresight-test--cand "a" 5)) free 1000)))
    (should (equal (org-foresight-test--placed r) '("10:00 a")))))

(ert-deftest org-foresight-test-candidate-order ()
  "Nearest deadline first, then priority, then the shortest job."
  (let* ((soon (org-foresight-test--ts 0 0 12))
         (late (org-foresight-test--ts 0 0 20))
         (sorted (sort (list (org-foresight-test--cand "none-long" 90)
                             (org-foresight-test--cand "late" 30 late)
                             (org-foresight-test--cand "none-short" 15)
                             (org-foresight-test--cand "soon" 60 soon))
                       #'org-foresight--candidate<)))
    (should (equal (mapcar (lambda (c) (plist-get c :title)) sorted)
                   '("soon" "late" "none-short" "none-long")))))

(ert-deftest org-foresight-test-candidate-order-priority ()
  "With equal deadlines, priority decides; unset priority sorts last."
  (let ((sorted (sort (list (org-foresight-test--cand "none" 10)
                            (org-foresight-test--cand "b" 10 nil "B")
                            (org-foresight-test--cand "a" 10 nil "A"))
                      #'org-foresight--candidate<)))
    (should (equal (mapcar (lambda (c) (plist-get c :title)) sorted)
                   '("a" "b" "none")))))

(ert-deftest org-foresight-test-candidates-exclude-the-unplaceable ()
  "Only open, unpinned work is a candidate."
  (org-foresight-test--with-org
      "* NEXT unscheduled backlog
:PROPERTIES:
:EFFORT:   0:30
:END:
* NEXT already at a time
SCHEDULED: <2026-08-10 Mon 10:00>
* NEXT scheduled today, no time
SCHEDULED: <2026-08-10 Mon>
* DONE finished
* WAIT someone else's
"
    (let* ((org-foresight-followup-keywords '("WAIT" "DELEG"))
           (titles (mapcar (lambda (c) (plist-get c :title))
                           (org-foresight--candidates
                            (org-foresight-test--ts 0 0 10)))))
      (should (member "unscheduled backlog" titles))
      (should (member "scheduled today, no time" titles))
      (should-not (member "already at a time" titles))
      (should-not (member "finished" titles))
      (should-not (member "someone else's" titles)))))

;;;; The demo corpus, end to end
;; The generated demo is built to contain one of everything, which makes it the
;; closest thing here to an integration test: if a signal stops firing on it,
;; something upstream has broken.  It also guards the generator itself, whose
;; whole point is that it stays correct as the calendar moves.

(defmacro org-foresight-test--with-demo (&rest body)
  "Generate the demo corpus into a temporary directory and run BODY over it."
  (declare (indent 0))
  `(let ((dir (file-name-as-directory (make-temp-file "org-foresight-demo" t))))
     (unwind-protect
         (let* ((org-foresight-demo-directory dir)
                (org-todo-keywords
                 '((sequence "NEXT" "ONGO" "|" "DONE" "CANCEL")
                   (sequence "WAIT" "|" "DELEG")))
                (org-foresight-work '(("09:00" . "17:30")))
                ;; The demo is written relative to today, so today has to be
                ;; a working day whatever day the tests are run on.  Its
                ;; after-hours example is an evening, not a weekend, so
                ;; nothing is lost by it.
                (org-foresight-workdays '(0 1 2 3 4 5 6))
                (org-foresight-followup-keywords '("WAIT" "DELEG"))
                ;; The same three `org-foresight-demo-mode' declares, because
                ;; the corpus is written for them: a category names what a
                ;; thing is, not which calendar it was read out of.
                (org-foresight-meeting-categories '("meeting"))
                (org-foresight-private-categories '("family" "personal"))
                (org-foresight-informational-categories '("club"))
                (org-foresight-places '((office . "本社\\|会議室")
                                        (client . "様")))
                (org-foresight-travel-matrix '(((home . office) . 60)
                                               ((home . client) . 75)
                                               ((office . client) . 45)))
                (org-foresight-travel-default 45)
                (org-foresight-wip-keywords '("ONGO"))
                (org-foresight-wip-limit 2)
                (org-foresight-awake '("06:30" . "23:00"))
                (org-foresight-day-file nil)
                (org-foresight--shape-cache nil)
                (org-foresight--signals-cache nil)
                (org-foresight-surge-cache-file (expand-file-name "s.eld" dir))
                (org-foresight-bias-cache-file (expand-file-name "b.eld" dir))
                (org-foresight--bias-cache nil)
                (org-agenda-files (org-foresight-demo-regenerate)))
           ,@body)
       (dolist (f (directory-files dir t "\\.org\\'"))
         (when (get-file-buffer f)
           (with-current-buffer (get-file-buffer f) (set-buffer-modified-p nil))
           (kill-buffer (get-file-buffer f))))
       (delete-directory dir t))))

(ert-deftest org-foresight-test-demo-is-valid-org ()
  "The generated files must parse, and be dated relative to today."
  (org-foresight-test--with-demo
    (dolist (f org-agenda-files)
      (with-current-buffer (find-file-noselect f)
        (should (derived-mode-p 'org-mode))
        (goto-char (point-min))
        (should (org-element-parse-buffer))
        ;; Org's planning line is one line.  Split across two, the second half
        ;; is body text -- and the PROPERTIES drawer beneath it silently stops
        ;; being a property drawer, so effort and category quietly vanish.
        (goto-char (point-min))
        (should-not (re-search-forward "^SCHEDULED:.*\n *DEADLINE:" nil t))
        (goto-char (point-min))
        (should-not (re-search-forward "^DEADLINE:.*\n *SCHEDULED:" nil t))))
    ;; and every entry that declares an effort is actually read as having one
    (with-current-buffer (find-file-noselect (cadr org-agenda-files))
      (org-with-wide-buffer
       (goto-char (point-min))
       (while (re-search-forward "^:EFFORT: " nil t)
         (org-back-to-heading t)
         (should (org-entry-get (point) "EFFORT"))
         (org-end-of-subtree t t))))
    ;; today must actually appear, or the corpus is describing another week
    (with-current-buffer (find-file-noselect (cadr org-agenda-files))
      (should (string-match-p (format-time-string "%Y-%m-%d")
                              (buffer-string))))))

(ert-deftest org-foresight-test-demo-mode-restores-what-it-changed ()
  "The demo puts everything back, including the calendar it borrowed.

Today is made a working day while it runs -- the data describes a full day of
work, and a Saturday would show a day that cannot happen -- and that is a
setting somebody lives by, so it has to come back exactly as it was."
  (let ((files org-agenda-files)
        (workdays org-foresight-workdays)
        (surge org-foresight-surge-cache-file)
        (leak org-foresight-leak-cache-file)
        (bias org-foresight-bias-cache-file)
        (org-foresight-demo-directory
         (file-name-as-directory (make-temp-file "org-foresight-demo" t))))
    (unwind-protect
        (progn
          (org-foresight-demo-mode 1)
          (should (equal '(0 1 2 3 4 5 6) org-foresight-workdays))
          ;; and every learned figure comes from the demo, not from a life
          (dolist (f (list org-foresight-surge-cache-file
                           org-foresight-leak-cache-file
                           org-foresight-bias-cache-file))
            (should (string-prefix-p org-foresight-demo-directory f))))
      (org-foresight-demo-mode -1)
      (delete-directory org-foresight-demo-directory t))
    (should (equal files org-agenda-files))
    (should (equal workdays org-foresight-workdays))
    (should (equal surge org-foresight-surge-cache-file))
    (should (equal leak org-foresight-leak-cache-file))
    (should (equal bias org-foresight-bias-cache-file))))

(ert-deftest org-foresight-test-demo-fires-every-signal ()
  "One of everything: each signal must find its example in the demo corpus."
  (org-foresight-test--with-demo
    (let ((labels (mapcar #'car (org-foresight-signals))))
      (dolist (expected '("Meetings without prep"
                          "Outside work hours (invisible to capacity)"
                          "Unplannable (deadline, no estimate)"
                          "Gone quiet (follow-up overdue)"
                          "Kept moving (not really NEXT)"
                          "Orphaned prep"
                          "Impossible (travel clashes with a meeting)"
                          "Won't fit today"
                          "Too much in flight"))
        (should (member expected labels))))))

(ert-deftest org-foresight-test-demo-has-travel-and-private-time ()
  "The demo must exercise the whole day model, not just the working part."
  (org-foresight-test--with-demo
    (let* ((day (org-foresight--day-start 0))
           (kinds (mapcar (lambda (b) (plist-get b :kind))
                          (org-foresight-day-blocks day))))
      ;; an office meeting, so there is a journey either side of it
      (should (memq 'travel kinds))
      ;; a private commitment, which occupies the evening without being work
      (should (memq 'private kinds))
      ;; and unclaimed private time around the edges
      (should (memq 'grey kinds)))))

(ert-deftest org-foresight-test-demo-teaches-the-bias ()
  "The demo's history must contain a real estimate/outcome gap to learn from."
  (org-foresight-test--with-demo
    (org-foresight-learn-bias)
    ;; reporting is written to run consistently over
    (should (> (org-foresight-bias-factor "reporting") 1.2))
    ;; while admin is estimated accurately
    (should (< (abs (- (org-foresight-bias-factor "admin") 1.0)) 0.2))))

(ert-deftest org-foresight-test-demo-has-a-workable-day ()
  "The demo day must have real busy intervals, real effort and real clocks,
or the capacity and clock blocks would render empty and prove nothing."
  (org-foresight-test--with-demo
    (let* ((day (org-foresight--day-start 0))
           (scan (org-foresight-scan 14 day))
           (clock (org-foresight-clock-scan 7)))
      (should (> (length (aref (plist-get scan :busy) 0)) 0))
      (should (> (aref (plist-get scan :committed) 0) 0))
      (should (> (plist-get clock :today-total) 0))
      (should (> (plist-get clock :total) (plist-get clock :today-total)))
      ;; the repeating meeting must reach the far end of the horizon
      (should (seq-some (lambda (i) (> (length (aref (plist-get scan :busy) i)) 0))
                        (number-sequence 7 13))))))

(ert-deftest org-foresight-test-demo-regenerate-is-repeatable ()
  "Regenerating twice must give the same corpus, not append to it."
  (org-foresight-test--with-demo
    (let ((first (with-temp-buffer
                   (insert-file-contents (cadr org-agenda-files))
                   (buffer-string))))
      (org-foresight-demo-regenerate)
      (should (equal first
                     (with-temp-buffer
                       (insert-file-contents (cadr org-agenda-files))
                       (buffer-string)))))))

;;;; Derived rows inside Org's agenda

(defun org-foresight-test--time-of-day (item)
  "Return the time Org will sort ITEM by."
  (and item (get-text-property 0 'time-of-day item)))

(ert-deftest org-foresight-test-item-takes-its-time-from-the-clock ()
  "A derived row's time must come from its time, not from its words.

`org-agenda-format-item' concatenates DOTIME with the text before looking for
a time, so that a heading reading \"call Bob at 10\" is understood.  Here that
is exactly wrong: a row saying a gap is 2:15 long would be filed at 02:15.
The search is bound off, and this is the guard on it."
  (should (= 945 (org-foresight-test--time-of-day
                  (org-foresight-agenda--item "free 2:15 · 1:59 usable"
                                              "" "09:45"))))
  (should (= 1200 (org-foresight-test--time-of-day
                   (org-foresight-agenda--item "→ office" "travel"
                                               "12:00-13:00"))))
  ;; and a row with nothing to say makes no line at all
  (should-not (org-foresight-agenda--item "" "" "09:00")))

(ert-deftest org-foresight-test-derived-rows-land-in-the-day ()
  "Journeys, gaps and the edges of the span are handed to Org with times on
them, and Org is what puts them in order."
  (org-foresight-test--with-travel
      "* At the office
:PROPERTIES:
:LOCATION: 会議室A
:END:
<2026-08-10 Mon 14:00-15:00>
"
    (let* ((day (org-foresight-test--ts 0 0 10))
           (rows (org-foresight-agenda--augment nil day))
           (plain (mapcar #'substring-no-properties rows)))
      ;; Every row carries a time, at position 1 as well as at 0.  This is
      ;; the whole contract with Org.  `org-cmp-time' reads position 1 and
      ;; treats a row without a time as later than everything; because the
      ;; sort is stable, such rows then come out in the order they were added
      ;; -- in a heap at the foot of the buffer, outside the day they
      ;; describe.  A row that cannot say when it happens has no business
      ;; being in an agenda at all.
      (should (seq-every-p #'org-foresight-test--time-of-day rows))
      (should (seq-every-p (lambda (r) (get-text-property 1 'time-of-day r))
                           rows))
      (should (seq-find (lambda (r) (string-match-p "→" r)) plain))
      (should (seq-find (lambda (r) (string-match-p "work starts" r)) plain))
      (should (seq-find (lambda (r) (string-match-p "work ends" r)) plain))
      ;; a journey answers to the agenda's commands through the meeting it
      ;; serves; an edge has no entry behind it and stays inert
      (should (markerp
               (get-text-property
                0 'org-marker
                (seq-find (lambda (r)
                            (string-match-p "→ office"
                                            (substring-no-properties r)))
                          rows))))
      (should-not (get-text-property
                   0 'org-marker
                   (seq-find (lambda (r)
                               (string-match-p "work starts"
                                               (substring-no-properties r)))
                             rows))))))

(ert-deftest org-foresight-test-gap-is-offered-net-of-the-reserve ()
  "A gap is offered at what survives interruption, not at what the clock says.

Nothing is placed either way -- the gap is only being reported honestly -- so
the choice of what goes in it stays entirely with the reader."
  (let ((cap '(:span-min 500.0 :surge-min 50.0)))
    (should (= 0.9 (org-foresight-agenda--keep cap)))
    (let ((org-foresight-gap-net nil))
      (should (= 1.0 (org-foresight-agenda--keep cap))))
    ;; a day with no span cannot have a ratio taken out of it
    (should (= 1.0 (org-foresight-agenda--keep '(:span-min 0.0 :surge-min 60.0))))
    ;; nor can more be reserved than there is
    (should (= 0.0 (org-foresight-agenda--keep
                    '(:span-min 60.0 :surge-min 600.0))))))

(ert-deftest org-foresight-test-what-will-not-fit-is-marked ()
  "Whether work fits is a fact about the work and the day, not about any
arrangement of them -- so it can be said without placing anything."
  (let* ((cap (list :span-min 510.0 :surge-min 0.0 :work t))
         (buf (get-buffer-create "*foresight-fit*"))
         ;; two distinct positions: a marker is identified by buffer and
         ;; offset, so an empty buffer would make every entry the same one
         (m (with-current-buffer buf
              (insert "* big\n* small\n") (copy-marker (point-min))))
         (other-m (with-current-buffer buf (copy-marker (point-max))))
         (ledger (list (list :kind 'promised :title "big" :effort 300
                             :marker m)))
         (item (org-foresight-test--row " reporting Scheduled: big" m))
         (other (org-foresight-test--row " admin     Scheduled: small" other-m))
         (small (list (list :kind 'available
                            :start (org-foresight-test--ts 9 0 10)
                            :end (org-foresight-test--ts 11 0 10))))
         (roomy (list (list :kind 'available
                            :start (org-foresight-test--ts 9 0 10)
                            :end (org-foresight-test--ts 18 0 10)))))
    (unwind-protect
        (let ((out (org-foresight-agenda--mark-rows
                    (list item other) small cap ledger)))
          ;; 5:00 of work against a 2:00 gap: no ordering finds it a home
          (should (equal org-foresight-agenda-wont-fit
                         (get-text-property 0 'org-foresight-mark (car out))))
          (should-not (get-text-property 0 'org-foresight-mark (cadr out)))
          ;; drawn, the mark goes into the row rather than after it: appended
          ;; it would land past the tags and defeat their alignment
          (let ((drawn (car (org-foresight-agenda--place-marks out))))
            (should (string-match-p org-foresight-agenda-wont-fit drawn)))
          ;; a day with a big enough gap marks nothing
          (should (equal (list item other)
                         (org-foresight-agenda--mark-rows
                          (list item other) roomy cap ledger))))
      (kill-buffer buf))))

(defun org-foresight-test--timed (text heading)
  "Return TEXT as an agenda row whose heading starts at HEADING, with a time."
  (let ((row (copy-sequence text)))
    (put-text-property 0 (length row) 'time-of-day 1500 row)
    (put-text-property heading (length row) 'org-heading t row)
    row))

(defun org-foresight-test--row (text marker)
  "Return TEXT as an agenda row for MARKER, with Org's own heading property."
  (let ((row (propertize text 'org-hd-marker marker)))
    (put-text-property (string-match "[^ ]+\\'" text) (length text)
                       'org-heading t row)
    row))

(ert-deftest org-foresight-test-work-lands-is-drawn-where-it-lands ()
  "The projected end of the day is a rule at its own hour, or nothing at all.

Two of the day\='s three rules are declarations and this one is a prediction,
so it is only worth a line when it disagrees with them: a projection that
lands on the hour you declared is not news, and a rule that is always there
is a rule the eye stops seeing.

It takes the overrun\='s colour past the declared end and the colour of room
before it -- landing early is the point of the whole exercise, not a lesser
kind of news."
  (let* ((window (cons (org-foresight-test--ts 9 0 10)
                       (org-foresight-test--ts 17 30 10)))
         (rows (lambda (finish)
                 (mapcar #'substring-no-properties
                         (org-foresight-agenda--edges
                          (list :work (list window) :lands finish :committed-min 120.0)
                          (org-foresight-test--ts 0 0 10)))))
         (row-face (lambda (finish)
                     (car (ensure-list
                           (get-text-property
                            0 'face
                            (seq-find
                             (lambda (r) (string-match-p
                                          "work lands"
                                          (substring-no-properties r)))
                             (org-foresight-agenda--edges
                              (list :work (list window) :lands finish :committed-min 120.0)
                              (org-foresight-test--ts 0 0 10)))))))))
    ;; the declared edges are always there
    (should (= 2 (length (funcall rows nil))))
    (should (seq-find (lambda (r) (string-match-p "work starts" r))
                      (funcall rows nil)))
    (should (seq-find (lambda (r) (string-match-p "work ends" r))
                      (funcall rows nil)))
    ;; agreeing with the declaration says nothing
    (should (= 2 (length (funcall rows (org-foresight-test--ts 17 32 10)))))
    ;; running past it is the overrun's news, filed at the hour it happens --
    ;; the hour is a text property rather than text, which is what lets Org
    ;; sort the row into the day rather than onto the end of it
    (let* ((over (org-foresight-agenda--edges
                  (list :work (list window) :lands (org-foresight-test--ts 19 20 10)
                        :committed-min 120.0)
                  (org-foresight-test--ts 0 0 10)))
           (lands (seq-find (lambda (r) (string-match-p
                                         "work lands"
                                         (substring-no-properties r)))
                            over)))
      (should (= 3 (length over)))
      (should lands)
      (should (= 1920 (get-text-property 0 'time-of-day lands)))
      (should (= 1920 (get-text-property 1 'time-of-day lands))))
    (should (eq 'org-foresight-report-overcommitted
                (funcall row-face (org-foresight-test--ts 19 20 10))))
    ;; landing early is room, and gets the colour of it
    (should (eq 'org-foresight-report-spare
                (funcall row-face (org-foresight-test--ts 15 40 10))))))

(ert-deftest org-foresight-test-work-lands-still-speaks-when-it-cannot-fit ()
  "A day too full to end is the day the projection matters most.

Drawing nothing there put the rule on every day except the ones worth a rule.
The line goes at the last hour there is and says how much is still standing
then -- an hour that cannot be drawn is no reason to draw nothing."
  (let* ((window (cons (org-foresight-test--ts 9 0 10)
                       (org-foresight-test--ts 17 30 10)))
         (day (org-foresight-test--ts 0 0 10))
         (rows (org-foresight-agenda--edges
                (list :work (list window) :lands nil :overflow-min 255.0 :committed-min 600.0)
                day))
         (lands (seq-find (lambda (r) (string-match-p
                                       "work lands"
                                       (substring-no-properties r)))
                          rows)))
    (should lands)
    (should (string-match-p "4:15 will not fit" (substring-no-properties lands)))
    (should (eq 'org-foresight-report-overcommitted
                (car (ensure-list (get-text-property 0 'face lands)))))
    ;; at the last hour of the waking day, which is where it is still true
    (should (= 2300 (get-text-property 1 'time-of-day lands)))
    ;; and nothing at all when there is nothing left over
    (should (= 2 (length (org-foresight-agenda--edges
                          (list :work (list window) :lands nil :overflow-min 0.0 :committed-min 600.0)
                          day))))))

(ert-deftest org-foresight-test-org-own-effort-column-is-corrected ()
  "Org prints the estimate in the file; the day is planned on a corrected one.

Where those differ the row said the first and meant the second, which is the
one thing a number on a screen must never do.  The correction is written
beside it in the derived face, since it is in no file and cannot be edited
where it appears."
  (let* ((buf (get-buffer-create "*foresight-effort*"))
         (m (with-current-buffer buf
              (insert "* big\n* small\n") (copy-marker (point-min))))
         (other-m (with-current-buffer buf (copy-marker (point-max))))
         (row (lambda (text marker effort)
                (let ((r (propertize text 'org-hd-marker marker
                                     'effort effort)))
                  (put-text-property (string-match "NEXT" text) (length text)
                                     'org-heading t r)
                  r)))
         (big (funcall row "  reporti Scheduled:  6:00 NEXT Rewrite" m "6:00"))
         (small (funcall row "  admin    Scheduled:  0:30 NEXT Reply"
                         other-m "0:30")))
    (unwind-protect
        (let* ((out (org-foresight-agenda--annotate-efforts
                     (list big small)
                     (list (list :kind 'promised :marker m
                                 :effort 360.0 :effort-adj 466.0)
                           ;; inside the threshold: nothing to say
                           (list :kind 'promised :marker other-m
                                 :effort 30.0 :effort-adj 31.0))))
               (flat (mapcar #'substring-no-properties out)))
          (should (equal "  reporti Scheduled:  6:00→7:46 NEXT Rewrite"
                         (nth 0 flat)))
          (should (equal small (nth 1 out)))
          ;; the added figure is marked as nobody's writing
          (let ((at (string-match "→" (nth 0 flat))))
            (should (memq 'org-foresight-agenda-derived
                          (ensure-list (get-text-property at 'face
                                                          (nth 0 out)))))))
      (kill-buffer buf))))

(ert-deftest org-foresight-test-arriving-work-is-marked ()
  "Which of the day\'s work was chosen and which landed on it.

On a day that will not close, that is the first thing worth knowing: what
arrived is what the reserve was held for, and it is rarely what should be
defended.  Not fitting stays the loudest mark -- one column holds one glyph,
and what has to move is worth more than where it came from."
  (org-foresight-test--with-window
    (org-foresight-test--with-org
        (concat "* ONGO an interruption\n"
                "SCHEDULED: <2026-08-10 Mon>\n"
                ":PROPERTIES:\n:SURGE: [2026-08-10 Mon 09:00]\n"
                ":EFFORT:   0:30\n:END:\n"
                "* NEXT work that was chosen\n"
                "SCHEDULED: <2026-08-10 Mon>\n"
                ":PROPERTIES:\n:EFFORT:   0:30\n:END:\n")
      (let* ((day (org-foresight-test--ts 0 0 10))
             (now (org-foresight-test--ts 9 30 10))
             (scan (org-foresight-scan 1 day now))
             (cap (org-foresight-capacity day scan now))
             (ledger (aref (plist-get scan :ledger) 0))
             (rows (mapcar (lambda (e)
                             (propertize (plist-get e :title)
                                         'org-hd-marker (plist-get e :marker)))
                           ledger))
             (marked (org-foresight-agenda--mark-rows
                      rows (org-foresight-day-blocks day scan) cap ledger))
             (mark (lambda (title)
                     (get-text-property
                      0 'org-foresight-mark
                      (seq-find (lambda (r)
                                  (equal title (substring-no-properties r)))
                                marked)))))
        (should (equal org-foresight-agenda-arrived
                       (funcall mark "an interruption")))
        (should-not (funcall mark "work that was chosen"))))))

(ert-deftest org-foresight-test-marks-share-one-column ()
  "Every mark in the grid goes in the same column, whatever the row.

A reader who has learnt where to look has learnt it once.  The column is the
one the earliest timed heading starts at, which is where the time field ends
-- so a mark does not wander right by however long a `Scheduled:' that row
happened to need, and stays beside the clock and the title it qualifies.

Only rows that carry a time are asked for the column.  A conditional time
field is dropped from an undated row rather than padded, so its heading
begins where a timed row is still in the middle of its clock; such a row
takes its own heading instead of being given a mark inside its title.

The mark is inserted rather than written over a blank -- the one blank going
spare is the space the time field pads out with -- and only into rows that
have one, because what has to line up is the marks and not the titles."
  ;; the prefix as `  %-8.8c%?-12t% s%?-5e' lays it out: two columns, then
  ;; eight of category, then twelve of clock -- so a heading starts at 22,
  ;; and a leader starts there and pushes its own heading further right
  (let* ((short (org-foresight-test--timed
                 (concat "  travel  " "15:00-15:45 " "→ client") 22))
         (leader (org-foresight-test--timed
                  (concat "  reportin" "13:00 ┄┄┄┄┄ "
                          "Scheduled:  1:00 " "NEXT Draft")
                  39))
         (quiet (org-foresight-test--timed
                 (concat "  day     " "18:00-20:00 " "Basketball") 22))
         ;; no clock at all: the time field is dropped, not padded
         (undated (concat "  admin   " "2:00 " "NEXT Reply"))
         (marked (lambda (row glyph)
                   (let ((r (copy-sequence row)))
                     (put-text-property 0 (length r) 'org-foresight-mark
                                        glyph r)
                     r))))
    (put-text-property 15 (length undated) 'org-heading t undated)
    (let* ((rows (list (funcall marked short org-foresight-agenda-wont-fit)
                       (funcall marked leader org-foresight-agenda-alongside)
                       quiet
                       (funcall marked undated org-foresight-agenda-wont-fit)))
           (out (org-foresight-agenda--place-marks rows))
           (flat (mapcar #'substring-no-properties out)))
      (should (= 22 (org-foresight-agenda--mark-column rows)))
      ;; one column for the grid, and it is left of the leader rather than
      ;; lost behind it
      (should (equal '(22 22) (list (string-match "⨯" (nth 0 flat))
                                    (string-match "╰" (nth 1 flat)))))
      (should (string-match-p "╰ Scheduled:" (nth 1 flat)))
      ;; a row with no clock in its prefix takes its own heading
      (should (equal "  admin   2:00 ⨯ NEXT Reply" (nth 3 flat)))
      ;; an unmarked row is left exactly as it was
      (should (equal quiet (nth 2 out)))
      ;; the mark and the space after it are inserted, not written over
      (should (equal '(2 2 0 2)
                     (seq-mapn (lambda (a b) (- (length b) (length a)))
                               rows out)))
      ;; the cell belongs to the prefix, not to the heading Org looks for
      (should-not (get-text-property 22 'org-heading (nth 1 out))))))

(ert-deftest org-foresight-test-squeezed-travel-keeps-its-length ()
  "A journey that was squeezed is drawn at the length it needs.

Fifteen minutes shown for a forty-five minute drive is the day claiming to
work when it does not.  Org files it by that start, so it lands against the
meeting it collides with, which is where the collision is."
  (let* ((bands (list (list :kind 'travel :title "→ client"
                            :start (org-foresight-test--ts 15 30 10)
                            :end (org-foresight-test--ts 15 45 10)
                            :trimmed t
                            :full-start (org-foresight-test--ts 15 0 10))))
         (row (car (org-foresight-agenda--travel bands)))
         (plain (substring-no-properties row)))
    ;; filed at the hour it must leave, not at what was left for it
    (should (= 1500 (get-text-property 1 'time-of-day row)))
    ;; the mark is recorded rather than written into the title, so that where
    ;; every mark on the page goes is decided in one place
    (should (equal org-foresight-agenda-wont-fit
                   (get-text-property 0 'org-foresight-mark row)))
    (should-not (string-match-p org-foresight-agenda-wont-fit plain))
    ;; a journey that fits says nothing extra and keeps its own start
    (let ((row (car (org-foresight-agenda--travel
                     (list (list :kind 'travel :title "→ office"
                                 :start (org-foresight-test--ts 12 0 10)
                                 :end (org-foresight-test--ts 13 0 10)))))))
      (should (= 1200 (get-text-property 1 'time-of-day row)))
      (should-not (string-match-p "⨯" (substring-no-properties row))))))

(ert-deftest org-foresight-test-edges-read-like-the-now-line ()
  "The edges of the day are labelled then ruled, as Org rules `← now'.
Two lines that mean the same kind of thing should be read the same way."
  (let* ((day (org-foresight-test--ts 0 0 10))
         (rows (org-foresight-agenda--edges
                (list :work (list (cons (org-foresight-test--ts 9 0 10)
                                        (org-foresight-test--ts 17 30 10))))
                day))
         (plain (mapcar #'substring-no-properties rows)))
    (should (seq-find (lambda (r) (string-match-p "work starts ─+$" r)) plain))
    (should (seq-find (lambda (r) (string-match-p "work ends ─+$" r)) plain))
    ;; and a day with no working window has no edges to draw
    (should-not (org-foresight-agenda--edges '(:work nil) day))))

(ert-deftest org-foresight-test-key-names-only-the-marks-used ()
  "A key describing a clash on a day that has none is explaining a problem
the reader does not have, which is a slower way of saying nothing."
  (let ((org-foresight-agenda--marks (list org-foresight-agenda-wont-fit)))
    (let ((key (substring-no-properties (org-foresight-agenda-key))))
      (should (string-match-p "⨯" key))
      (should-not (string-match-p "↳" key))))
  (let ((org-foresight-agenda--marks (list "↳")))
    (let ((key (substring-no-properties (org-foresight-agenda-key))))
      (should (string-match-p "↳" key))
      (should-not (string-match-p "⨯" key))))
  ;; a day that needed no marks gets no line at all
  (let ((org-foresight-agenda--marks nil))
    (should-not (org-foresight-agenda-key))))

(ert-deftest org-foresight-test-time-survives-a-word-dash ()
  "A derived row must find its time whatever the buffer calls a word.

`org-get-time-of-day' matches with `word-start' and `word-end', so a range
like \"12:00-13:00\" stops parsing wherever `-' has been made a word
constituent -- a common enough tweak, and one that costs the row its time and
therefore its place in the day.  Org never meets this: its grid lines carry a
single time, and real entries arrive from timestamps parsed long before."
  (with-temp-buffer
    (modify-syntax-entry ?- "w")
    ;; the hazard is real: org's own reading of the range fails here
    (should-not (org-get-time-of-day "12:00-13:00"))
    ;; and a row made under it still knows when it happens
    (should (= 1200 (get-text-property
                     0 'time-of-day
                     (org-foresight-agenda--item "→ office" "travel"
                                                 "12:00-13:00"))))
    (should (= 945 (get-text-property
                    0 'time-of-day
                    (org-foresight-agenda--item "2:15 free" "" "09:45-12:00"))))))

(defmacro org-foresight-test--with-clocked (text &rest body)
  "Run BODY over TEXT, whose CLOCK lines are rewritten to today."
  (declare (indent 1))
  `(org-foresight-test--with-org
       (replace-regexp-in-string
        "@" (format-time-string "%Y-%m-%d %a") ,text)
     ,@body))

(ert-deftest org-foresight-test-today-tasks-are-per-entry ()
  "One drawer is one task however many CLOCK lines it holds, and it carries
enough of the heading to be acted on."
  (org-foresight-test--with-clocked
      "* NEXT Draft the summary
:PROPERTIES:
:CATEGORY: reporting
:EFFORT:   1:00
:END:
:LOGBOOK:
CLOCK: [@ 09:00]--[@ 10:00] =>  1:00
CLOCK: [@ 13:00]--[@ 13:40] =>  0:40
:END:
* NEXT Something unestimated
:PROPERTIES:
:CATEGORY: admin
:END:
:LOGBOOK:
CLOCK: [@ 14:00]--[@ 14:20] =>  0:20
:END:
"
    (let* ((tasks (plist-get (org-foresight-clock-scan 7) :today-tasks))
           (first (car tasks)))
      (should (= 2 (length tasks)))
      ;; two CLOCK lines, one task, minutes added up -- and sorted longest first
      (should (= 100 (plist-get first :minutes)))
      (should (= 60 (plist-get first :effort)))
      (should (equal "reporting" (plist-get first :category)))
      (should (markerp (plist-get first :marker)))
      ;; work with no estimate still appears; it simply has none
      (should-not (plist-get (cadr tasks) :effort)))))

(ert-deftest org-foresight-test-spent-row-is-always-a-percentage ()
  "One notation for the whole column: mixing a multiplier with a percentage
would make two columns out of one and leave the eye to work out which it is
reading."
  (let ((over (org-foresight-report--spent-row
               (list :title "over" :category "a" :minutes 100 :effort 60)))
        (under (org-foresight-report--spent-row
                (list :title "under" :category "a" :minutes 50 :effort 120)))
        (exact (org-foresight-report--spent-row
                (list :title "exact" :category "a" :minutes 60 :effort 60)))
        (none (org-foresight-report--spent-row
               (list :title "none" :category "a" :minutes 30))))
    (should (string-match-p "167%" (substring-no-properties over)))
    (should (string-match-p " 42%" (substring-no-properties under)))
    (should (string-match-p "100%" (substring-no-properties exact)))
    (should (string-match-p "no estimate" (substring-no-properties none)))
    (should-not (string-match-p "×" (substring-no-properties over)))))

(ert-deftest org-foresight-test-leak-and-lost-divide-the-unclocked ()
  "Unclocked time divides by whether you were at the keyboard, not by app.

Leak is time that happened and went unrecorded; lost is time away from the
machine.  They partition the unclocked day between them, and neither asks
what was on screen -- what a window was called says nothing about whether the
hour was work."
  (let* ((iv (lambda (h1 m1 h2 m2)
               (cons (org-foresight-test--ts h1 m1 10)
                     (org-foresight-test--ts h2 m2 10))))
         (active (list (funcall iv 9 0 10 0) (funcall iv 11 0 12 0)))
         (idle (list (funcall iv 10 0 11 0)))
         (clocked (list (funcall iv 9 0 9 30))))
    (let ((leak (org-foresight--intervals-subtract active clocked))
          (lost (org-foresight--intervals-subtract idle clocked)))
      ;; 0:30 left of the first active stretch, plus the whole second one
      (should (= 90.0 (/ (org-foresight--intervals-seconds leak) 60.0)))
      (should (= 60.0 (/ (org-foresight--intervals-seconds lost) 60.0)))
      ;; and together with the clocked part they account for the whole day
      ;; the watcher saw -- nothing is counted twice and nothing is dropped
      (should (= 180.0
                 (/ (+ (org-foresight--intervals-seconds leak)
                       (org-foresight--intervals-seconds lost)
                       (org-foresight--intervals-seconds clocked))
                    60.0))))))

(ert-deftest org-foresight-test-switches-binned-sums-to-the-count ()
  "The row and the number must be the same fact seen two ways."
  (let* ((day (format-time-string "%Y-%m-%dT%H:%M:%S%z"))
         (at (lambda (h m app)
               (list (cons 'timestamp
                           (format-time-string
                            "%Y-%m-%dT%H:%M:%S%z"
                            (encode-time 0 m h (nth 3 (decode-time))
                                         (nth 4 (decode-time))
                                         (nth 5 (decode-time)))))
                     (cons 'duration 60)
                     (cons 'data (list (cons 'app app))))))
         (events (list (funcall at 9 0 "Emacs") (funcall at 9 10 "Safari")
                       (funcall at 9 40 "Emacs") (funcall at 10 0 "Emacs")
                       (funcall at 10 5 "Slack")))
         (binned (org-foresight-observe--switches-binned events)))
    (ignore day)
    (should (= 48 (length binned)))
    ;; four transitions: Emacs→Safari, Safari→Emacs, Emacs→Slack
    (should (= 3 (apply #'+ (append binned nil))))
    ;; and they fall in the half-hours they happened in
    (should (= 1 (aref binned 18)))   ; 09:00-09:30
    (should (= 1 (aref binned 19)))   ; 09:30-10:00
    (should (= 1 (aref binned 20)))))

(ert-deftest org-foresight-test-sparkline-survives-a-missing-afk-watcher ()
  "A machine with no afk watcher still gets a day drawn.

`aw-watcher-afk' is a separate module and is not always installed; without it
the activity intervals were empty and the sparkline came out blank, which
reads as \"you did nothing\" rather than as \"nobody was watching\".  Window
events are the evidence that remains, and the header says the picture is the
coarser kind."
  (let* ((h (nth 2 (decode-time)))
         (event (lambda (m app)
                  (list (cons 'timestamp
                              (format-time-string
                               "%Y-%m-%dT%H:%M:%S%z"
                               (encode-time 0 m h (nth 3 (decode-time))
                                            (nth 4 (decode-time))
                                            (nth 5 (decode-time)))))
                        (cons 'duration 600)
                        (cons 'data (list (cons 'app app))))))
         (win (list (funcall event 0 "Emacs") (funcall event 20 "Slack")))
         (org-foresight-observe--cache nil))
    (cl-letf (((symbol-function 'org-foresight-observe--get-json)
               (lambda (path)
                 (cond ((equal path "/buckets/") '((aw-watcher-window_h . t)))
                       (t win)))))
      (let ((data (org-foresight-observe-today)))
        (should data)
        (should (plist-get data :screen-only))
        ;; the day is drawn from what there is, rather than left blank
        (should (> (apply #'+ (append (plist-get data :binned) nil)) 0))
        (should (> (plist-get data :active) 0))
        (should (= 0.0 (plist-get data :afk)))
        (should-not (string-match-p
                     "\\`·+\\'"
                     (org-foresight-report--sparkline
                      (plist-get data :binned))))))))

(ert-deftest org-foresight-test-emptiness-is-drawn-one-way ()
  "The same absence looks the same wherever it is drawn.

One character, one weight: the spare stretch of a bar and a half hour the
machine saw nothing in are the same statement, and two glyphs that look alike
but are not is a difference nobody can name and everybody can see.  The
colour still belongs to the segment -- what is empty and what is available
are not the same news."
  ;; the sparkline's empty bin
  (should (= org-foresight-report-dot
             (org-foresight-report--spark-char 0.0)))
  ;; a bin with nothing in it is empty, not focused work: before this it fell
  ;; through to the first of the four faces, which is the one for clocked and
  ;; active time
  (let ((zero (make-vector 48 0)))
    (should (eq 'org-foresight-report-empty
                (org-foresight-report--dominant-face zero zero zero zero 0))))
  ;; and a dot on a bar takes its weight from the same place, its colour from
  ;; the segment
  (let ((dotted (list :face 'org-foresight-report-spare
                      :glyph org-foresight-report-dot))
        (solid (list :face 'org-foresight-report-booked)))
    (should (equal '(org-foresight-report-spare org-foresight-report-empty)
                   (org-foresight-report--glyph-face dotted)))
    (should (eq 'org-foresight-report-booked
                (org-foresight-report--glyph-face solid)))))

(ert-deftest org-foresight-test-marks-carry-their-own-face ()
  "A mark looks the same wherever it appears, and they are not all one colour.

The key and the rows read their face from one table, so they cannot drift
apart.  Each mark names its own face -- two of them may be given the same
colour, since a mark that reports where a row came from decides nothing and
the shape tells those apart, but which of them is which stays a decision a
reader can make."
  (pcase-dolist (`(,glyph ,_ ,face) org-foresight-agenda--mark-meanings)
    (should (eq face (get-text-property 0 'face
                                        (org-foresight-agenda--mark glyph)))))
  ;; one face per mark: a single face shared by all would be no key at all,
  ;; and one shared by two would put their colours beyond reach separately
  (should (= (length org-foresight-agenda--mark-meanings)
             (length (delete-dups
                      (mapcar (lambda (m) (nth 2 m))
                              org-foresight-agenda--mark-meanings)))))
  ;; and the key paints them the same way the rows do
  (let* ((org-foresight-agenda--marks
          (mapcar #'car org-foresight-agenda--mark-meanings))
         (key (org-foresight-agenda-key)))
    (pcase-dolist (`(,glyph ,_ ,face) org-foresight-agenda--mark-meanings)
      (should (eq face (get-text-property (string-search glyph key)
                                          'face key))))))

(ert-deftest org-foresight-test-sharing-an-hour-is-marked-not-clashed ()
  "Work that will share its hour is marked rather than reported as a clash.

An overlap between a call you only have to hear and something real is not a
day that cannot happen, so the mark says which it is instead of leaving the
reader to resolve a clash that was never there."
  (let* ((buf (get-buffer-create "*foresight-attention*"))
         (m (with-current-buffer buf
              (insert "* listen\n* work\n") (copy-marker (point-min))))
         (ledger (list (list :kind 'meeting :title "listen" :marker m
                             :attention 'informational)))
         (item (propertize " club      Kids' basketball" 'org-hd-marker m)))
    (unwind-protect
        (let ((out (car (org-foresight-agenda--mark-rows
                         (list item) nil (list :span-min 480.0 :surge-min 0.0 :work t)
                         ledger))))
          (should (equal org-foresight-agenda-alongside
                         (get-text-property 0 'org-foresight-mark out)))
          ;; blocking work is left alone
          (should (equal item
                         (car (org-foresight-agenda--mark-rows
                               (list item) nil (list :span-min 480.0 :surge-min 0.0 :work t)
                               (list (list :kind 'meeting :title "listen"
                                           :marker m :attention 'blocking)))))))
      (kill-buffer buf))))

;;;; A day that breaks
;; Work is a list of intervals, so a day may stop for lunch, for a school run,
;; for anything.  What these fix is that the break is *not* working time in
;; every place the day is measured: it is not capacity, it is not offered, it
;; is not planned into, and it is not quietly worked through in the
;; projection.  Each of those was a separate subtraction before, and each of
;; them could have been missed on its own.

(defmacro org-foresight-test--with-broken-day (text &rest body)
  "Run BODY over TEXT with work declared as 09:00-12:00 and 13:00-17:30."
  (declare (indent 1))
  `(org-foresight-test--with-day ,text
     (let ((org-foresight-work '(("09:00" . "12:00")
                                 ("13:00" . "17:30")))
           (org-foresight-surge-cache-file "/nonexistent/surge.eld")
           (org-foresight-leak-cache-file "/nonexistent/leak.eld")
           (org-foresight-surge-default "0:00")
           (org-foresight-leak-default "0:00")
           (org-foresight-lost-default "0:00")
           (org-foresight--shape-cache nil))
       ,@body)))

(ert-deftest org-foresight-test-broken-day-loses-the-break-from-the-span ()
  "The hour off is not capacity.

Nine to half five with an hour for lunch is seven and a half hours of work,
not eight and a half.  Taking first-start to last-end would hand the lunch
hour back as time that may be promised away -- which is exactly the hour that
was declared not to be available."
  (org-foresight-test--with-broken-day "* nothing\n"
    (let ((cap (org-foresight-capacity (org-foresight-test--ts 0 0 10) nil
                                       (org-foresight-test--ts 6 0 10))))
      (should (= 450.0 (plist-get cap :span-min)))
      ;; and the two intervals are what the day says its work is
      (should (= 2 (length (plist-get cap :work)))))))

(ert-deftest org-foresight-test-broken-day-does-not-offer-the-break ()
  "The break is not among the stretches nothing has claimed.

`free' means work time nothing has claimed.  An hour that is not work time
cannot be free in that sense, however empty it is."
  (org-foresight-test--with-broken-day "* nothing\n"
    (let ((free (org-foresight-free-intervals (org-foresight-test--ts 0 0 10)
                                              nil
                                              (org-foresight-test--ts 6 0 10))))
      (should (equal '("09:00-12:00" "13:00-17:30")
                     (mapcar (lambda (iv)
                               (concat (format-time-string "%H:%M" (car iv)) "-"
                                       (format-time-string "%H:%M" (cdr iv))))
                             free))))))

(ert-deftest org-foresight-test-broken-day-greys-the-break ()
  "The break is drawn as time off, not as time waiting to be filled.

Same glyph and same colour as the hours before work and after it, because it
is the same kind of thing: the day, not the job."
  (org-foresight-test--with-broken-day "* nothing\n"
    (let ((bands (org-foresight-test--bands (org-foresight-test--ts 0 0 10))))
      (should (member "grey 12:00-13:00" bands))
      (should (member "available 09:00-12:00" bands))
      (should (member "available 13:00-17:30" bands)))))

(ert-deftest org-foresight-test-broken-day-cannot-hold-work-across-the-break ()
  "A job longer than the longest interval does not fit, however long the day is.

Seven and a half hours of working time will not take a four-hour job when it
comes in two pieces of three and four and a half.  Measuring against the total
would promise a day that cannot happen."
  (org-foresight-test--with-broken-day "* nothing\n"
    (let* ((day (org-foresight-test--ts 0 0 10))
           (cap (org-foresight-capacity day nil (org-foresight-test--ts 6 0 10)))
           (bands (org-foresight-day-blocks day))
           (entry (list :kind 'promised :effort 240.0
                        :marker (point-marker) :title "long job"))
           (marked (org-foresight-agenda--mark-rows
                    nil bands cap (list entry))))
      (ignore marked)
      ;; the longest stretch of work available is the afternoon, not the day
      (should (= 270.0
                 (apply #'max
                        (mapcar (lambda (b)
                                  (/ (float-time
                                      (time-subtract (plist-get b :end)
                                                     (plist-get b :start)))
                                     60.0))
                                (seq-filter (lambda (b)
                                              (eq (plist-get b :kind) 'available))
                                            bands))))))))

(ert-deftest org-foresight-test-work-in-the-break-is-outside-work-hours ()
  "Work placed in the break is work that escaped the day.

The signal used to be called \"after hours\", which could only ever mean the
end of the day.  A day that breaks in the middle has work escaping into hours
it is nowhere near the end of, and that work is just as invisible to
capacity."
  (org-foresight-test--with-signals
      "* NEXT lunchtime call
<2027-01-05 Tue 12:15-12:45>
* NEXT during the morning
<2027-01-05 Tue 10:00-11:00>
"
    (let* ((org-foresight-work '(("09:00" . "12:00") ("13:00" . "17:30")))
           (org-foresight-workdays '(1 2 3 4 5))
           (org-foresight--shape-cache nil)
           (org-foresight-horizon-days 400)
           (titles (mapcar (lambda (f) (plist-get f :title))
                           (org-foresight-test--signal
                            "Outside work hours (invisible to capacity)"))))
      (should (member "lunchtime call" titles))
      (should-not (member "during the morning" titles)))))

(ert-deftest org-foresight-test-work-property-takes-several-ranges ()
  "A day may declare its own broken shape, and it survives the round trip."
  (let ((file (make-temp-file
               "org-foresight-day" nil ".org"
               "* 2026\n** 2026-08 August\n*** 2026-08-10 Mon\n:PROPERTIES:\n:WORK: 09:00-12:00 13:00-17:30\n:END:\n")))
    (unwind-protect
        (let* ((org-foresight-day-file file)
               (org-foresight-work '(("08:00" . "16:00")))
               (org-foresight-workdays '(1 2 3 4 5))
               (org-foresight--shape-cache nil)
               (shape (org-foresight-day-shape (org-foresight-test--ts 0 0 10))))
          (should (equal "09:00-12:00 13:00-17:30"
                         (org-foresight-test--work-string shape))))
      (delete-file file))))

;;;; Where the day lands

(ert-deftest org-foresight-test-lands-steps-over-the-break ()
  "The projection does not work through a break that was declared.

Two hours owed at eleven do not finish at one o'clock: one of those hours is
lunch.  Pouring through it answers \"when will this be over\" with the
comfortable number, on precisely the days somebody is deciding whether to
skip lunch."
  (org-foresight-test--with-broken-day
      "* NEXT write the report
SCHEDULED: <2026-08-10 Mon>
:PROPERTIES:
:EFFORT: 2:00
:END:
"
    (let ((cap (org-foresight-capacity (org-foresight-test--ts 0 0 10) nil
                                       (org-foresight-test--ts 11 0 10))))
      (should (equal "14:00" (format-time-string "%H:%M" (plist-get cap :lands)))))))

(ert-deftest org-foresight-test-lands-is-unchanged-on-an-unbroken-day ()
  "One interval, and the answer is what it always was.

The generalisation has to be conservative: on a day with no break there is
nothing to step over, and work runs straight from now to when it is done."
  (org-foresight-test--with-day
      "* NEXT write the report
SCHEDULED: <2026-08-10 Mon>
:PROPERTIES:
:EFFORT: 2:00
:END:
"
    (let* ((org-foresight-surge-cache-file "/nonexistent/surge.eld")
           (org-foresight-leak-cache-file "/nonexistent/leak.eld")
           (org-foresight-surge-default "0:00")
           (org-foresight-leak-default "0:00")
           (org-foresight-lost-default "0:00")
           (cap (org-foresight-capacity (org-foresight-test--ts 0 0 10) nil
                                        (org-foresight-test--ts 11 0 10))))
      (should (equal "13:00" (format-time-string "%H:%M" (plist-get cap :lands)))))))

(ert-deftest org-foresight-test-overflow-runs-past-the-end-not-into-the-break ()
  "What will not fit runs on after work ends -- it does not fill the break.

The break is not a reservoir the day may draw on once it is in trouble.  The
overrun goes where an overrun actually goes: after the hour work was meant to
be over."
  (org-foresight-test--with-broken-day
      "* NEXT the whole day and then some
SCHEDULED: <2026-08-10 Mon>
:PROPERTIES:
:EFFORT: 9:00
:END:
"
    (let ((cap (org-foresight-capacity (org-foresight-test--ts 0 0 10) nil
                                       (org-foresight-test--ts 9 0 10))))
      ;; 7:30 of work hours, so 1:30 of it lands after 17:30
      (should (equal "19:00" (format-time-string "%H:%M" (plist-get cap :lands))))
      (should (= 0.0 (plist-get cap :overflow-min))))))

(ert-deftest org-foresight-test-overflow-and-lands-never-disagree ()
  "Nothing may be said not to fit while an hour is named for it.

Both are read off the same list of stretches, so the invariant is structural:
a day that overflows has no landing, and a day that lands has no overflow."
  (org-foresight-test--with-broken-day
      "* NEXT far more than a day
SCHEDULED: <2026-08-10 Mon>
:PROPERTIES:
:EFFORT: 20:00
:END:
"
    (let ((cap (org-foresight-capacity (org-foresight-test--ts 0 0 10) nil
                                       (org-foresight-test--ts 9 0 10))))
      (should (> (plist-get cap :overflow-min) 0))
      (should (null (plist-get cap :lands))))))

;;;; The reserve is said even when it is spent

(ert-deftest org-foresight-test-reserve-is-said-against-the-day-s-allowance ()
  "The remainder means nothing without the allowance it came out of.

The reserve shrinks as the hours pass and as interruptions land, so by the
afternoon it is small -- and a figure that disappears when it gets small
disappears exactly when it is being spent."
  (org-foresight-test--with-day "* nothing\n"
    (let* ((org-foresight-surge-cache-file "/nonexistent/surge.eld")
           (org-foresight-leak-cache-file "/nonexistent/leak.eld")
           (org-foresight-surge-default "1:00")
           (org-foresight-leak-default "0:20")
           (org-foresight-lost-default "0:15")
           (day (org-foresight-test--ts 0 0 10))
           (cap (org-foresight-capacity day nil (org-foresight-test--ts 9 0 10))))
      ;; the whole allowance, before the day has spent any of it
      (should (= 95.0 (plist-get cap :reserve-day-min)))
      (should (string-match-p "reserve 1:35 of 1:35"
                              (substring-no-properties
                               (org-foresight-report--verdict cap)))))))

(ert-deftest org-foresight-test-reserve-survives-being-used-up ()
  "A reserve down to nothing is reported, not dropped.

\"0:00 of 1:35\" is the day saying the allowance was real and is now spent,
which is a different statement from silence -- and the more useful one, since
it is the day on which the next interruption has nowhere to go."
  (org-foresight-test--with-day "* nothing\n"
    (let* ((org-foresight-surge-cache-file "/nonexistent/surge.eld")
           (org-foresight-leak-cache-file "/nonexistent/leak.eld")
           (org-foresight-surge-default "1:00")
           (org-foresight-leak-default "0:20")
           (org-foresight-lost-default "0:15")
           (day (org-foresight-test--ts 0 0 10))
           ;; after the working hours are over: nothing is held back any more
           (cap (org-foresight-capacity day nil (org-foresight-test--ts 18 0 10))))
      (should (= 0.0 (plist-get cap :reserve-min)))
      (should (string-match-p "reserve 0:00 of 1:35"
                              (substring-no-properties
                               (org-foresight-report--verdict cap)))))))

;;;; The edges of a broken day

(ert-deftest org-foresight-test-edges-say-pauses-and-resumes ()
  "The inner edges are a break, and are labelled as one.

A break announced as \"work ends\" would be read as the end of the day, and
the day would look like it finished at noon."
  (let* ((day (org-foresight-test--ts 0 0 10))
         (rows (mapcar #'substring-no-properties
                       (org-foresight-agenda--edges
                        (list :work (org-foresight-test--ivs '(9 0 12 0)
                                                             '(13 0 17 30)))
                        day))))
    (should (= 4 (length rows)))
    (should (seq-find (lambda (r) (string-match-p "work starts" r)) rows))
    (should (seq-find (lambda (r) (string-match-p "work pauses" r)) rows))
    (should (seq-find (lambda (r) (string-match-p "work resumes" r)) rows))
    (should (seq-find (lambda (r) (string-match-p "work ends" r)) rows))
    ;; and each is filed at its own hour (HHMM, as Org writes it), so the
    ;; rules sort into the day rather than onto the end of it
    (should (equal '(900 1200 1300 1730)
                   (sort (mapcar (lambda (r) (get-text-property 1 'time-of-day r))
                                 (org-foresight-agenda--edges
                                  (list :work (org-foresight-test--ivs
                                               '(9 0 12 0) '(13 0 17 30)))
                                  day))
                         #'<)))))

;;;; Journeys and the break

(defmacro org-foresight-test--with-broken-travel (text &rest body)
  "Run BODY over TEXT with an hour to the client and a 12:00-13:00 break."
  (declare (indent 1))
  `(org-foresight-test--with-broken-day ,text
     (let ((org-foresight-places '((office . "Client")))
           (org-foresight-home-place 'home)
           (org-foresight-travel-matrix '(((home . office) . 60)))
           (org-foresight-travel-default 60))
       ,@body)))

(ert-deftest org-foresight-test-travel-is-not-planned-into-the-break ()
  "A journey is work, so it is not quietly put in the hour set aside for lunch.

Arriving just in time for a one o'clock meeting an hour away means leaving at
noon -- which is exactly the break.  The search steps past it as it steps past
a meeting, and the journey costs the working hour before it instead.  A plan
that spends the break on the motorway has planned a day nobody agreed to, and
it is the plan that should give, not the break."
  (org-foresight-test--with-broken-travel
      "* Client review
:PROPERTIES:
:LOCATION: Client site
:END:
<2026-08-10 Mon 13:00-14:00>
"
    (let ((bands (org-foresight-test--bands (org-foresight-test--ts 0 0 10))))
      (should (member "travel 11:00-12:00" bands))
      ;; and the break is still a break
      (should (member "grey 12:00-13:00" bands))
      (should-not (seq-find (lambda (b) (string-match-p "travel 12:00" b)) bands)))))

(ert-deftest org-foresight-test-travel-that-cannot-fit-is-still-shown ()
  "A journey that has to begin before work does not stop being necessary.

Nothing can be moved to make an hour appear before a nine o'clock meeting an
hour away, so the leg is drawn where it actually happens and counted as
borrowed.  Refusing to place it -- or forcing it into hours it cannot fit --
would hide the one journey the day cannot absorb."
  (org-foresight-test--with-broken-travel
      "* Early client review
:PROPERTIES:
:LOCATION: Client site
:END:
<2026-08-10 Mon 09:00-10:00>
"
    (let ((bands (org-foresight-day-blocks (org-foresight-test--ts 0 0 10))))
      (let ((leg (seq-find (lambda (b) (eq (plist-get b :kind) 'travel)) bands)))
        (should leg)
        (should (equal "08:00" (format-time-string "%H:%M" (plist-get leg :start))))
        (should (plist-get leg :borrowed))))))

(ert-deftest org-foresight-test-travel-costs-the-day-its-working-hour ()
  "The hour the journey takes comes out of capacity, not out of the break.

Which is the point of moving it: the day is an hour shorter for work, and the
verdict says so, rather than the day looking whole while lunch quietly paid
for it."
  (org-foresight-test--with-broken-travel
      "* Client review
:PROPERTIES:
:LOCATION: Client site
:END:
<2026-08-10 Mon 13:00-14:00>
"
    (let ((cap (org-foresight-capacity (org-foresight-test--ts 0 0 10) nil
                                       (org-foresight-test--ts 8 0 10))))
      ;; both legs are inside the working hours and are counted as travel
      (should (= 120.0 (plist-get cap :travel-min)))
      (should (= 0.0 (plist-get cap :borrowed-min)))
      ;; 7:30 of work, less the meeting and the two journeys
      (should (= (- 450.0 60.0 120.0) (plist-get cap :spare-min))))))

;;;; The agenda Org actually draws
;; Everything above tests the rows in isolation.  These build a real agenda
;; buffer with `org-agenda-list' and read what came out, because every bug
;; found in this layer so far lived in the seam rather than in either side of
;; it: which day Org thinks it is drawing, and whether it calls us at all.

(defvar org-foresight-test--e2e-span 'day
  "Span passed to `org-agenda-list' by `org-foresight-test--agenda'.")

(defmacro org-foresight-test--with-agenda (text &rest body)
  "Run BODY with TEXT as the only agenda file and the agenda machinery armed.

Dates in TEXT are written by the caller, so they are relative to nothing --
`org-foresight-test--stamp' builds them from today, which is what makes an
assertion about \"tomorrow\" survive being run tomorrow."
  (declare (indent 1))
  `(org-foresight-test--with-org ,text
     (let ((org-agenda-sticky nil)
           (org-agenda-buffer-name "*org-foresight-test-agenda*")
           (org-agenda-span 'day)
           (org-agenda-start-on-weekday nil)
           (org-agenda-prefix-format
            '((agenda . "  %-8.8c%?-12t% s%?-5e") (todo . "  %-8c %-7e")
              (tags . "  %i %-5c %-7e") (search . " %i %-12c")))
           (org-agenda-sorting-strategy
            '((agenda time-up category-keep) (todo habit-down category-up time-up)
              (tags priority-down category-keep) (search category-keep)))
           (org-agenda-start-with-log-mode '(state))
           (org-agenda-log-mode-items '(closed state))
           (org-agenda-start-with-clockreport-mode nil)
           (org-agenda-start-with-entry-text-mode nil)
           ;; Both, in the order a real session ends up with: the package
           ;; adds its own when it loads, and a config appends the report.
           (org-agenda-finalize-hook '(org-foresight-agenda--draw-spine
                                       org-foresight-report-render))
           (org-foresight-report-style 'daily)
           (org-foresight-awake '("07:00" . "22:00"))
           (org-foresight-work '(("09:00" . "12:00") ("13:00" . "17:30")))
           (org-foresight-workdays '(0 1 2 3 4 5 6))
           (org-foresight-private-categories '("family"))
           (org-foresight-surge-cache-file "/nonexistent/surge.eld")
           (org-foresight-leak-cache-file "/nonexistent/leak.eld")
           (org-foresight-bias-cache-file "/nonexistent/bias.eld")
           (org-foresight-surge-default "0:30")
           (org-foresight-leak-default "0:00")
           (org-foresight-lost-default "0:00")
           (org-foresight--shape-cache nil))
       (unwind-protect (progn ,@body)
         (when (get-buffer org-agenda-buffer-name)
           (kill-buffer org-agenda-buffer-name))))))

(defun org-foresight-test--logstamp (offset hhmm)
  "Return an inactive timestamp OFFSET days from today at HHMM.

Inactive on purpose: Org reads LOGBOOK state lines with `[...]', and a log
line written with `<...>' is a log line Org does not see -- which makes a
test of log mode pass while proving nothing."
  (concat "[" (format-time-string
               "%Y-%m-%d %a"
               (time-add (org-foresight--day-start 0) (days-to-time offset)))
          " " hhmm "]"))

(defun org-foresight-test--agenda ()
  "Draw today's agenda and return its lines, properties stripped.

ActivityWatch is answered as if it were not installed: what a machine without
it does is the case worth testing, and a suite that reaches the network is a
suite that fails on a train."
  (cl-letf (((symbol-function 'org-foresight-observe--get-json)
             (lambda (&rest _) nil)))
    (org-agenda-list nil nil org-foresight-test--e2e-span)
    (with-current-buffer org-agenda-buffer-name
      (split-string (buffer-substring-no-properties (point-min) (point-max))
                    "\n"))))

(defun org-foresight-test--day-section (lines day-name)
  "Return the LINES belonging to DAY-NAME's date block."
  (let (out (in nil))
    (dolist (l lines (nreverse out))
      (cond
       ((string-match-p (concat "^" day-name "\\b") l) (setq in t))
       ;; another date header ends the section
       ((and in (string-match-p
                 "^\\(Monday\\|Tuesday\\|Wednesday\\|Thursday\\|Friday\\|Saturday\\|Sunday\\)"
                 l))
        (setq in nil))
       (in (push l out))))))

(ert-deftest org-foresight-test-e2e-rows-reach-the-agenda ()
  "The rows arrive in the buffer Org draws, not merely in a list.

The whole layer hangs off one advice on an Org internal.  Testing the
functions it calls says nothing about whether Org still calls it."
  (org-foresight-test--with-agenda
      (concat "* Standup\n:PROPERTIES:\n:CATEGORY: meeting\n:END:\n"
              (org-foresight-test--stamp 0 "09:30" "09:45") "\n")
    (let ((lines (org-foresight-test--agenda)))
      (should (seq-find (lambda (l) (string-match-p "work starts" l)) lines))
      (should (seq-find (lambda (l) (string-match-p "work pauses" l)) lines))
      (should (seq-find (lambda (l) (string-match-p "work ends" l)) lines))
      ;; "free", not "free · 0:29 usable": the usable part is only shown while
      ;; a reserve is still being held, so asserting on it would make this test
      ;; pass in the afternoon and fail in the evening.
      (should (seq-find (lambda (l) (string-match-p "free" l)) lines))
      ;; and the report block, which is a separate hook
      (should (seq-find (lambda (l) (string-match-p "Capacity" l)) lines))
      ;; nothing blew up on the way
      (should-not (seq-find (lambda (l) (string-match-p "org-foresight failed" l))
                            lines)))))

(ert-deftest org-foresight-test-e2e-a-check-names-the-key-that-does-it ()
  "The row that says to look at the day says how, in the key bound now.

Written out it would be a keystroke frozen at the moment somebody typed the
setting, and the first thing rebinding breaks is the instruction telling you
what to press."
  (org-foresight-test--with-agenda "* nothing dated\n"
    (let* ((org-foresight-check-in
            '(:minutes 10 :title "look at the day \\[org-foresight-board]"))
           (org-foresight--shape-cache nil)
           (map (make-sparse-keymap)))
      (define-key map "B" #'org-foresight-board)
      (let ((org-agenda-mode-map map))
        (let ((lines (org-foresight-test--agenda)))
          (should (seq-find (lambda (l)
                              (string-match-p "check.*look at the day B" l))
                            lines)))))))

(ert-deftest org-foresight-test-e2e-an-empty-day-still-has-a-shape ()
  "A day Org found nothing for is still a day with hours in it.

`org-agenda-finalize-entries' is called only `(when rtnall)', so hooking it --
as this once did -- left exactly the emptiest day blank.  That is the day the
shape is most worth drawing: the answer to \"is there room this week\" is a
column of free stretches, and it used to be a column of bare dates."
  (org-foresight-test--with-agenda "* nothing dated\n"
    (let ((lines (org-foresight-test--agenda)))
      (should (seq-find (lambda (l) (string-match-p "work starts" l)) lines))
      (should (seq-find (lambda (l) (string-match-p "3:00 free" l)) lines))
      (should (seq-find (lambda (l) (string-match-p "4:30 free" l)) lines)))))

(ert-deftest org-foresight-test-e2e-each-day-is-drawn-from-its-own-entries ()
  "Every day of a span answers with its own gaps, not with today's.

org-agenda.el binds the loop's `date' lexically and never declares it
special, so an advice reading it saw nothing and fell through to today --
correct by accident on a one-day agenda, and wrong on every other day of a
two-day one.  Org's own `org-agenda-current-date' is the answer."
  (let ((org-foresight-test--e2e-span 2))
    (org-foresight-test--with-agenda
        (concat "* Standup\n:PROPERTIES:\n:CATEGORY: meeting\n:END:\n"
                (org-foresight-test--stamp 0 "09:30" "09:45") "\n"
                "* Review\n:PROPERTIES:\n:CATEGORY: meeting\n:END:\n"
                (org-foresight-test--stamp 1 "10:00" "11:00") "\n")
      (let* ((lines (org-foresight-test--agenda))
             (tomorrow (format-time-string
                        "%A" (time-add (current-time) (days-to-time 1))))
             (section (org-foresight-test--day-section lines tomorrow)))
        (should section)
        ;; tomorrow's gaps are cut by tomorrow's meeting at 10:00 ...
        (should (seq-find (lambda (l) (string-match-p "9:00-10:00" l)) section))
        (should (seq-find (lambda (l) (string-match-p "11:00-12:00" l)) section))
        ;; ... and not by today's, which starts at 9:30
        (should-not (seq-find (lambda (l) (string-match-p "9:00-9:30" l)) section))))))

(ert-deftest org-foresight-test-column-index-counts-columns ()
  "Cutting a row to reach a column is not cutting it at that position.
Where a two-column character straddles the target the position before it is
the answer: half a character is not a place anything can be inserted."
  (should (= 3 (org-foresight-agenda--column-index "abcdef" 3)))
  ;; 5 spaces, then two characters four columns wide
  (should (= 5 (org-foresight-agenda--column-index "     会議 x" 5)))
  (should (= 6 (org-foresight-agenda--column-index "     会議 x" 7)))
  (should (= 7 (org-foresight-agenda--column-index "     会議 x" 9)))
  ;; straddling: column 6 falls inside the first wide character
  (should (= 5 (org-foresight-agenda--column-index "     会議 x" 6)))
  ;; past the end is the end
  (should (= 6 (org-foresight-agenda--column-index "abcdef" 99))))

(ert-deftest org-foresight-test-e2e-marks-line-up-in-columns-not-characters ()
  "A category in a two-column script must not drag the marks left.

Org pads a prefix field to a width on the screen, so `CATEGORY: 会議予定'
fills eight columns with four characters and takes no padding at all.  Its
heading then begins four *characters* -- and no columns -- ahead of every
other row, and a `min' taken over character positions answers with that row:
every mark on the page is then inserted four places to the left of where the
headings are, through the middle of the time field's dots.

Which day it happened on was decided by whether anyone had written a category
in Japanese, so it looked random and survived every restart."
  (org-foresight-test--with-agenda
      (concat "* 会議\n:PROPERTIES:\n:CATEGORY: 会議予定\n:END:\n"
              (org-foresight-test--stamp 0 "15:30" "16:30") "\n"
              "* NEXT すぐやる\nSCHEDULED: " (org-foresight-test--stamp 0)
              "\n:PROPERTIES:\n:EFFORT: 0:45\n:CATEGORY: office\n:END:\n")
    (org-foresight-test--agenda)
    (with-current-buffer org-agenda-buffer-name
      (let (marks)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((b (line-beginning-position)) (e (line-end-position)))
            ;; Agenda rows only.  The report's own blocks explain the mark in
            ;; a legend and carry no heading, and a legend is not a column.
            (when (text-property-any b e 'org-heading t)
              (let ((line (buffer-substring-no-properties b e)))
                (when-let ((at (string-match "↳" line)))
                  (push (cons (string-width (substring line 0 at))
                              (aref line (1- at)))
                        marks)))))
          (forward-line 1))
        (should marks)
        ;; One column for the whole page ...
        (should (= 1 (length (seq-uniq (mapcar #'car marks)))))
        ;; ... and it is the padding after the clock, not a dot of it.
        (dolist (m marks) (should (eq (cdr m) ?\s)))))))

(ert-deftest org-foresight-test-e2e-the-day-under-the-cursor ()
  "The day a command acts on is the day being looked at, not today.

`org-foresight-shape-day' reads it from Org's own `day' property, which
covers the whole of each day's block, date header included.  A command that
could only shape today would be useless for the case it exists for: the day
that goes differently is almost never the one being lived, and by the morning
it is, there is nothing left to plan around it."
  (let ((org-foresight-test--e2e-span 2))
    (org-foresight-test--with-agenda
        (concat "* Standup\n:PROPERTIES:\n:CATEGORY: meeting\n:END:\n"
                (org-foresight-test--stamp 1 "10:00" "11:00") "\n")
      (org-foresight-test--agenda)
      (with-current-buffer org-agenda-buffer-name
        (let ((tomorrow (time-add (current-time) (days-to-time 1))))
          (goto-char (point-min))
          (should (re-search-forward
                   (concat "^" (format-time-string "%A" tomorrow)) nil t))
          ;; the date header itself
          (should (equal (format-time-string "%Y-%m-%d"
                                             (org-foresight--day-at-point))
                         (format-time-string "%Y-%m-%d" tomorrow)))
          ;; and a row inside the block
          (forward-line 1)
          (should (equal (format-time-string "%Y-%m-%d"
                                             (org-foresight--day-at-point))
                         (format-time-string "%Y-%m-%d" tomorrow)))
          ;; The report blocks belong to no day.  Nil there is what sends the
          ;; command back to today, rather than to whichever day happened to
          ;; be drawn nearest.
          (goto-char (point-min))
          (should-not (org-foresight--day-at-point)))))))

(ert-deftest org-foresight-test-no-day-outside-an-agenda ()
  "An ordinary buffer has no day under the cursor, and says so with nil."
  (with-temp-buffer
    (insert "not an agenda\n")
    (goto-char (point-min))
    (should-not (org-foresight--day-at-point))))

(ert-deftest org-foresight-test-e2e-clockcheck-keeps-the-day ()
  "Clock-check mode lists clock lines and nothing else, and that is a day too.

Its list holds no ordinary entries at all, so the day went blank under the
old hook -- the audit view lost the working hours it was auditing against."
  (org-foresight-test--with-agenda
      (concat "* Standup\n:PROPERTIES:\n:CATEGORY: meeting\n:END:\n"
              (org-foresight-test--stamp 0 "09:30" "09:45") "\n")
    (let* ((org-agenda-start-with-log-mode 'clockcheck)
           (lines (org-foresight-test--agenda)))
      (should (seq-find (lambda (l) (string-match-p "work starts" l)) lines))
      (should (seq-find (lambda (l) (string-match-p "work ends" l)) lines)))))

(ert-deftest org-foresight-test-e2e-coexists-with-org-s-own-modes ()
  "Log mode, the clock report and entry text all still draw what they draw.

Each of these inserts its own material around the entries; the rows added
here must not displace it, and must survive being drawn beside it."
  (org-foresight-test--with-agenda
      (concat "* Standup\n:PROPERTIES:\n:CATEGORY: meeting\n:END:\n"
              (org-foresight-test--stamp 0 "09:30" "09:45") "\n"
              "* DONE Finished thing\nCLOSED: " (org-foresight-test--logstamp 0 "11:00") "\n"
              ":LOGBOOK:\n- State \"DONE\"       from \"ONGO\"       "
              (org-foresight-test--logstamp 0 "11:00") "\n:END:\n")
    ;; log mode: Org's own State line survives beside ours
    (let ((lines (org-foresight-test--agenda)))
      (should (seq-find (lambda (l) (string-match-p "State:" l)) lines))
      (should (seq-find (lambda (l) (string-match-p "work starts" l)) lines)))
    ;; the clock report table is still written
    (let* ((org-agenda-start-with-clockreport-mode t)
           (lines (org-foresight-test--agenda)))
      (should (seq-find (lambda (l) (string-match-p "^|" l)) lines))
      (should (seq-find (lambda (l) (string-match-p "work starts" l)) lines)))
    ;; entry text mode adds its lines without disturbing ours
    (let* ((org-agenda-start-with-entry-text-mode t)
           (lines (org-foresight-test--agenda)))
      (should (seq-find (lambda (l) (string-match-p "work starts" l)) lines)))))

(ert-deftest org-foresight-test-e2e-every-injected-row-carries-its-hour ()
  "Rows are placed by Org, which can only place what carries a time.

The invariant is checked on the finished buffer rather than on the list,
because a row can lose its property on the way through the formatter."
  (org-foresight-test--with-agenda
      (concat "* Standup\n:PROPERTIES:\n:CATEGORY: meeting\n:END:\n"
              (org-foresight-test--stamp 0 "09:30" "09:45") "\n")
    (org-foresight-test--agenda)
    (with-current-buffer org-agenda-buffer-name
      (goto-char (point-min))
      (let ((bad '()))
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
            (when (and (string-match-p
                        "work starts\\|work ends\\|work pauses\\|work resumes\\|work lands\\|free · "
                        line)
                       (not (get-text-property (line-beginning-position) 'time-of-day))
                       (not (get-text-property (1+ (line-beginning-position))
                                               'time-of-day)))
              (push line bad)))
          (forward-line 1))
        (should-not bad)))))

;;;; Headings people actually write
;; A day's file is not a fixture.  It has a timestamp somebody typed backwards,
;; an estimate in the wrong units, a clock that is still running, a heading with
;; a link in it.  None of that should produce a wrong number, and none of it
;; should produce an error -- the block is drawn while the agenda is being
;; built, so an error here costs the whole page.

(defmacro org-foresight-test--with-day-at-8 (text &rest body)
  "Run BODY over TEXT with the day fixed and NOW at 08:00, before anything."
  (declare (indent 1))
  `(org-foresight-test--with-day ,text
     (let ((org-foresight-surge-cache-file "/nonexistent/surge.eld")
           (org-foresight-leak-cache-file "/nonexistent/leak.eld")
           (org-foresight-surge-default "0:00")
           (org-foresight-leak-default "0:00")
           (org-foresight-lost-default "0:00"))
       ,@body)))

(defun org-foresight-test--partition-holds-p (day)
  "Non-nil when DAY's bands tile the waking hours exactly."
  (let* ((bands (org-foresight-day-blocks day))
         (awake (plist-get (org-foresight-day-shape day) :awake))
         (cursor (car awake))
         (ok t))
    (dolist (b bands)
      (unless (equal (float-time cursor) (float-time (plist-get b :start)))
        (setq ok nil))
      (setq cursor (plist-get b :end)))
    (and ok (equal (float-time cursor) (float-time (cdr awake))))))

(ert-deftest org-foresight-test-hard-timestamps-do-not-break-the-day ()
  "A backwards or empty timestamp gives a duration, not a negative one.

Neither is a thing anybody means to write, and both survive in a file for
years.  What must not happen is a band of negative length, which would make
every total below it quietly wrong rather than visibly odd."
  (dolist (case '(("14:00" "14:00")     ; zero length
                  ("15:00" "14:00")))   ; end before start
    (org-foresight-test--with-day-at-8
        (format "* Odd\n<2026-08-10 Mon %s-%s>\n" (car case) (cadr case))
      (let* ((day (org-foresight-test--ts 0 0 10))
             (cap (org-foresight-capacity day nil (org-foresight-test--ts 8 0 10))))
        (should (>= (plist-get cap :booked-min) 0))
        (should (org-foresight-test--partition-holds-p day))))))

(ert-deftest org-foresight-test-hard-efforts-are-read-or-ignored ()
  "An estimate that cannot be read is treated as one that was not given.

Org's own reader signals on it, which is how a single mistyped EFFORT takes
down `C-c a a' with a message naming no heading.  Nothing here may do the
same: these numbers are drawn while the agenda is being built."
  (dolist (case '(("2:00"     120.0)    ; the ordinary form
                  ("90"        90.0)    ; bare minutes, which Org also reads
                  ("2h"       120.0)    ; and this, which looks wrong and is not
                  ("whenever"  30.0)    ; unreadable -> the default effort
                  ("-1:00"     30.0)))  ; negative -> likewise
    (org-foresight-test--with-day-at-8
        (concat "* NEXT A task\nSCHEDULED: <2026-08-10 Mon>\n"
                ":PROPERTIES:\n:EFFORT: " (car case) "\n:END:\n")
      (let ((cap (org-foresight-capacity (org-foresight-test--ts 0 0 10) nil
                                         (org-foresight-test--ts 8 0 10))))
        (should (= (cadr case) (plist-get cap :committed-min)))))))

(ert-deftest org-foresight-test-hard-an-unreadable-effort-is-named ()
  "The one thing that can still be said when the agenda itself will not draw.

The board is built by walking the files, so it works when `org-agenda-list'
does not -- which is exactly the situation a mistyped estimate creates.  It
has to name the heading, because Org's error names neither file nor heading."
  (org-foresight-test--with-signals
      "* NEXT Mistyped estimate
:PROPERTIES:
:EFFORT: whenever
:END:
* NEXT Fine estimate
:PROPERTIES:
:EFFORT: 2:00
:END:
"
    (let* ((found (org-foresight-test--signal
                   "Unreadable estimate (breaks the agenda itself)"))
           (titles (mapcar (lambda (f) (plist-get f :title)) found)))
      (should (member "Mistyped estimate" titles))
      (should-not (member "Fine estimate" titles))
      (should (string-match-p "not a duration" (plist-get (car found) :note))))))

(ert-deftest org-foresight-test-hard-a-running-clock-counts-so-far ()
  "Work in progress is owed less than it was, by exactly as much as has gone.

The clock has no end yet, so the elapsed part is measured against NOW -- and
the remainder is what the rest of the day still has to find."
  (org-foresight-test--with-day-at-8
      (concat "* ONGO In progress\nSCHEDULED: <2026-08-10 Mon>\n"
              ":PROPERTIES:\n:EFFORT: 2:00\n:END:\n"
              ":LOGBOOK:\nCLOCK: [2026-08-10 Mon 07:00]\n:END:\n")
    (let ((cap (org-foresight-capacity (org-foresight-test--ts 0 0 10) nil
                                       (org-foresight-test--ts 8 0 10))))
      ;; an hour of the two has been done
      (should (= 60.0 (plist-get cap :committed-min))))))

(ert-deftest org-foresight-test-hard-clocking-past-the-estimate-owes-nothing ()
  "Work already over its estimate is not owed backwards.

Four hours clocked against a half-hour guess means the estimate was wrong,
not that the day has minus three and a half hours of work left in it."
  (org-foresight-test--with-day-at-8
      (concat "* ONGO Overran\nSCHEDULED: <2026-08-10 Mon>\n"
              ":PROPERTIES:\n:EFFORT: 0:30\n:END:\n:LOGBOOK:\n"
              "CLOCK: [2026-08-10 Mon 05:00]--[2026-08-10 Mon 09:00] =>  4:00\n:END:\n")
    (let ((cap (org-foresight-capacity (org-foresight-test--ts 0 0 10) nil
                                       (org-foresight-test--ts 8 0 10))))
      (should (= 0.0 (plist-get cap :committed-min))))))

(ert-deftest org-foresight-test-hard-headings-survive-their-own-punctuation ()
  "A heading is text somebody wrote, and it may contain anything.

Links, emphasis, a pipe, a percent sign, the glyphs this package draws with,
and Japanese with its wide spaces: all of it goes through a formatter and a
column count, and none of it may throw."
  (org-foresight-test--with-day-at-8
      (concat "* Heading with [[https://example.com][a link]] and *bold*\n"
              "<2026-08-10 Mon 10:00-10:30>\n"
              "* 日本語の見出し・全角スペース　あり\n"
              "<2026-08-10 Mon 10:30-10:45>\n"
              "* A | pipe, 100% and a ⨯ of our own\n"
              "<2026-08-10 Mon 11:00-11:15>\n")
    (let ((day (org-foresight-test--ts 0 0 10)))
      (should (org-foresight-test--partition-holds-p day))
      (should (org-foresight-capacity day nil (org-foresight-test--ts 8 0 10)))
      ;; and the rows are made without complaint
      (should (org-foresight-agenda--augment
               nil day (org-foresight-scan 1 day))))))

(ert-deftest org-foresight-test-hard-an-empty-file-is-a-quiet-day ()
  "Nothing dated is not an error; it is a day with everything still free."
  (org-foresight-test--with-day-at-8 "* just a heading\nwith a body\n"
    (let ((cap (org-foresight-capacity (org-foresight-test--ts 0 0 10) nil
                                       (org-foresight-test--ts 8 0 10))))
      (should (= 0.0 (plist-get cap :booked-min)))
      (should (= 0.0 (plist-get cap :committed-min)))
      (should (= (plist-get cap :span-min) (plist-get cap :spare-min))))))

;;;; Living beside the rest of the agenda

(ert-deftest org-foresight-test-e2e-redrawing-does-not-double-the-rows ()
  "A sticky agenda redrawn is the same agenda, not two of them.

Every redraw runs the injection again on a buffer that already holds the last
one's output, which is precisely how derived rows accumulate."
  (org-foresight-test--with-agenda
      (concat "* Standup\n:PROPERTIES:\n:CATEGORY: meeting\n:END:\n"
              (org-foresight-test--stamp 0 "09:30" "09:45") "\n")
    (let ((org-agenda-sticky t))
      (org-foresight-test--agenda)
      (let ((first (length (seq-filter
                            (lambda (l) (string-match-p "work starts" l))
                            (with-current-buffer org-agenda-buffer-name
                              (split-string (buffer-string) "\n"))))))
        (should (= 1 first))
        (with-current-buffer org-agenda-buffer-name
          (cl-letf (((symbol-function 'org-foresight-observe--get-json)
                     (lambda (&rest _) nil)))
            (org-agenda-redo)
            (org-agenda-redo))
          (should (= 1 (length (seq-filter
                                (lambda (l) (string-match-p "work starts" l))
                                (split-string (buffer-string) "\n"))))))))))

(ert-deftest org-foresight-test-e2e-injected-rows-refuse-to-be-acted-on ()
  "A derived row is not an entry, and Org's commands must say so.

They are drawn in the same column as real rows, so the cursor lands on them
by accident constantly.  What must never happen is a keystroke meant for a
task silently doing something to a rule."
  (org-foresight-test--with-agenda
      (concat "* Standup\n:PROPERTIES:\n:CATEGORY: meeting\n:END:\n"
              (org-foresight-test--stamp 0 "09:30" "09:45") "\n")
    (org-foresight-test--agenda)
    (with-current-buffer org-agenda-buffer-name
      (goto-char (point-min))
      (should (re-search-forward "work starts" nil t))
      (beginning-of-line)
      (dolist (cmd '(org-agenda-todo org-agenda-goto org-agenda-set-effort))
        (should-error (call-interactively cmd))))))

(ert-deftest org-foresight-test-e2e-a-tag-filter-keeps-the-shape-of-the-day ()
  "Filtering narrows the entries, not the hours they sit in.

The rules and gaps carry no tags, so a tag filter has nothing to reject them
by -- which is the behaviour worth having: you filter to see less work, not
to lose the day around it."
  (org-foresight-test--with-agenda
      (concat "* NEXT Tagged work :work:\nSCHEDULED: "
              (org-foresight-test--stamp 0) "\n:PROPERTIES:\n:EFFORT: 1:00\n:END:\n"
              "* NEXT Something else :home:\nSCHEDULED: "
              (org-foresight-test--stamp 0) "\n:PROPERTIES:\n:EFFORT: 0:30\n:END:\n")
    (org-foresight-test--agenda)
    (with-current-buffer org-agenda-buffer-name
      (let ((visible-ours
             (lambda ()
               (goto-char (point-min))
               (let ((n 0))
                 (while (not (eobp))
                   (when (and (string-match-p
                               "work starts\\|work ends\\|free · "
                               (buffer-substring-no-properties
                                (line-beginning-position) (line-end-position)))
                              (not (get-char-property (line-beginning-position)
                                                      'invisible)))
                     (setq n (1+ n)))
                   (forward-line 1))
                 n))))
        (let ((before (funcall visible-ours)))
          (should (> before 0))
          (org-agenda-filter-apply '("+work") 'tag)
          (should (= before (funcall visible-ours)))
          (org-agenda-filter-show-all-tag))))))

(ert-deftest org-foresight-test-observed-block-says-when-there-is-no-watcher ()
  "A machine with no ActivityWatch gets a sentence, not an empty block.

The company machine will not have one for a while, and \"nothing here\" has
to read as \"nothing is watching\" rather than as \"you did nothing\"."
  (cl-letf (((symbol-function 'org-foresight-observe--get-json)
             (lambda (&rest _) nil)))
    (let* ((org-foresight-observe--cache nil)
           (clock (list :rows nil :total 0 :days 7 :byday (make-vector 7 0)
                        :today-rows nil :today-total 0 :today-segments 0
                        :today-intervals nil :today-tasks nil))
           (text (substring-no-properties (org-foresight-report-spent clock))))
      (should (string-match-p "ActivityWatch" text)))))

;;;; The spine

(defun org-foresight-test--spine-of (&optional buffer column)
  "Return what BUFFER's COLUMN *shows*, one character per line.

Read from the display property rather than from the text: the spine is drawn
by showing a glyph in the space that is already there, so the buffer's own
characters are unchanged -- which is the point of drawing it that way.

COLUMN defaults to the column the working hours are bracketed in; the hours
away from home are bracketed two to the right of it."
  (with-current-buffer (or buffer org-agenda-buffer-name)
    (save-excursion
      (goto-char (point-min))
      (let ((column (or column org-foresight-agenda--spine-column)) out)
        (while (not (eobp))
          (let* ((cell (+ (line-beginning-position) column))
                 (shown (and (< cell (line-end-position))
                             (get-text-property cell 'display))))
            (push (if (stringp shown) shown " ") out))
          (forward-line 1))
        (apply #'concat (nreverse out))))))

(defun org-foresight-test--shown-at (regexp &optional column)
  "Return what COLUMN shows on the first line matching REGEXP."
  (with-current-buffer org-agenda-buffer-name
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward regexp nil t)
        (let* ((cell (+ (line-beginning-position)
                        (or column org-foresight-agenda--spine-column)))
               (shown (and (< cell (line-end-position))
                           (get-text-property cell 'display))))
          (if (stringp shown) shown " "))))))

(ert-deftest org-foresight-test-spine-brackets-each-stretch-of-work ()
  "Two stretches of work read as two brackets, not one interrupted line.

The corners are the whole point: with the hours in pieces, `am I inside them\='
stopped being answerable from where a row sits, and a plain bar would say
only that some rows are work without saying where a stretch begins."
  (org-foresight-test--with-agenda
      (concat "* Standup\n:PROPERTIES:\n:CATEGORY: meeting\n:END:\n"
              (org-foresight-test--stamp 0 "09:30" "09:45") "\n"
              "* Call in the break\n:PROPERTIES:\n:CATEGORY: meeting\n:END:\n"
              (org-foresight-test--stamp 0 "12:15" "12:45") "\n")
    (let* ((lines (org-foresight-test--agenda))
           (spine (org-foresight-test--spine-of)))
      ;; one bracket per work interval
      (should (= 2 (seq-count (lambda (c) (= c ?┌)) spine)))
      (should (= 2 (seq-count (lambda (c) (= c ?└)) spine)))
      (should (> (seq-count (lambda (c) (= c ?│)) spine) 0))
      ;; the rules themselves are the corners
      (should (equal "┌" (org-foresight-test--shown-at "work starts")))
      (should (equal "└" (org-foresight-test--shown-at "work ends")))
      (should (equal "└" (org-foresight-test--shown-at "work pauses")))
      (should (equal "┌" (org-foresight-test--shown-at "work resumes")))
      ;; and what sits in the declared break is outside the bracket
      (should (equal " " (org-foresight-test--shown-at "Call in the break")))
      (ignore lines))))

(ert-deftest org-foresight-test-spine-leaves-every-column-where-it-was ()
  "The glyph replaces the first column; it does not push the line right.

Every column this package draws -- the mark column above all -- is counted
from the start of the line.  An insertion would move all of them by one and
leave the marks a column adrift from the headings they mark."
  (org-foresight-test--with-agenda
      (concat "* Standup\n:PROPERTIES:\n:CATEGORY: meeting\n:END:\n"
              (org-foresight-test--stamp 0 "09:30" "09:45") "\n")
    (let ((with (progn (org-foresight-test--agenda)
                       (with-current-buffer org-agenda-buffer-name
                         (goto-char (point-min))
                         (re-search-forward "Standup")
                         (- (match-beginning 0) (line-beginning-position)))))
          (without (let ((org-foresight-agenda-spine nil))
                     (org-foresight-test--agenda)
                     (with-current-buffer org-agenda-buffer-name
                       (goto-char (point-min))
                       (re-search-forward "Standup")
                       (- (match-beginning 0) (line-beginning-position))))))
      (should (= with without)))))

(ert-deftest org-foresight-test-spine-is-drawn-once-however-often-it-runs ()
  "Redrawing must not stack glyphs or eat the column.

The report block is re-rendered on every change made from the agenda, and the
whole buffer is finalized again on every redo, so this pass runs far more
often than it appears to."
  (org-foresight-test--with-agenda
      (concat "* Standup\n:PROPERTIES:\n:CATEGORY: meeting\n:END:\n"
              (org-foresight-test--stamp 0 "09:30" "09:45") "\n")
    (org-foresight-test--agenda)
    (with-current-buffer org-agenda-buffer-name
      (let ((before (buffer-substring-no-properties (point-min) (point-max))))
        (org-foresight-agenda--draw-spine)
        (org-foresight-agenda--draw-spine)
        (should (equal before (buffer-substring-no-properties
                               (point-min) (point-max))))))))

(ert-deftest org-foresight-test-spine-does-not-cross-the-date-line ()
  "Each day is bracketed on its own; a stretch cannot bleed into the next day."
  (let ((org-foresight-test--e2e-span 2))
    (org-foresight-test--with-agenda
        (concat "* Standup\n:PROPERTIES:\n:CATEGORY: meeting\n:END:\n"
                (org-foresight-test--stamp 0 "09:30" "09:45") "\n"
                "* Review\n:PROPERTIES:\n:CATEGORY: meeting\n:END:\n"
                (org-foresight-test--stamp 1 "10:00" "11:00") "\n")
      (let* ((lines (org-foresight-test--agenda))
             (spine (org-foresight-test--spine-of)))
        ;; two days, two intervals each
        (should (= 4 (seq-count (lambda (c) (= c ?┌)) spine)))
        (should (= 4 (seq-count (lambda (c) (= c ?└)) spine)))
        ;; and no date header wears one
        (should (equal " " (org-foresight-test--shown-at "August\\|September")))
        (ignore lines)))))

(ert-deftest org-foresight-test-spine-can-be-turned-off ()
  "Somebody who does not want it gets the page exactly as it was before."
  (org-foresight-test--with-agenda
      (concat "* Standup\n:PROPERTIES:\n:CATEGORY: meeting\n:END:\n"
              (org-foresight-test--stamp 0 "09:30" "09:45") "\n")
    (let ((org-foresight-agenda-spine nil))
      (org-foresight-test--agenda)
      (should (string-match-p "\\`[[:space:]]*\\'" (org-foresight-test--spine-of))))))

(defmacro org-foresight-test--with-wide-agenda (text &rest body)
  "Run BODY over TEXT with a prefix wide enough for the second bracket."
  (declare (indent 1))
  `(org-foresight-test--with-agenda ,text
     (let ((org-agenda-prefix-format
            '((agenda . "     %-8.8c%?-12t% s%?-5e") (todo . "  %-8c %-7e")
              (tags . "  %i %-5c %-7e") (search . " %i %-12c")))
           (org-foresight-places '((office . "本社")))
           (org-foresight-home-place 'home)
           (org-foresight-travel-matrix '(((home . office) . 60)))
           (org-foresight-travel-default 60)
           (org-foresight--shape-cache nil))
       ,@body)))

(ert-deftest org-foresight-test-place-spine-brackets-the-time-away ()
  "The second bracket says where the body is, and stops at the road.

Both journeys fall outside it: the hour spent getting somewhere is not an
hour spent there, and a bracket drawn around it would offer those minutes for
work that needs the place."
  (org-foresight-test--with-wide-agenda "* nothing dated\n"
    (let ((org-foresight-day-places
           (list (cons (nth 6 (decode-time (org-foresight--day-start 0)))
                       'office))))
      (org-foresight-test--agenda)
      (let ((place (org-foresight-test--spine-of nil org-foresight-agenda--place-column)))
        (should (= 1 (seq-count (lambda (c) (= c ?┌)) place)))
        (should (= 1 (seq-count (lambda (c) (= c ?└)) place))))
      ;; the journeys themselves are outside it, at either end
      (should (equal " " (org-foresight-test--shown-at "→ office" org-foresight-agenda--place-column)))
      (should (equal " " (org-foresight-test--shown-at "→ home" org-foresight-agenda--place-column)))
      ;; and the working hours are still bracketed in the first column
      (should (string-match-p "┌" (org-foresight-test--spine-of))))))

(ert-deftest org-foresight-test-place-spine-of-one-row-closes-itself ()
  "An hour somewhere else is a bracket with both corners in one cell.

Drawn as `open' it would say the stretch carries on down the page, which is
the one thing a single errand does not do."
  (org-foresight-test--with-wide-agenda
      (concat "* Review\n:PROPERTIES:\n:CATEGORY: meeting\n:LOCATION: 本社\n:END:\n"
              (org-foresight-test--stamp 0 "15:00" "16:00") "\n")
    (let ((org-foresight-day-places nil))
      (org-foresight-test--agenda)
      (should (equal "├" (org-foresight-test--shown-at "Review" org-foresight-agenda--place-column)))
      (should (equal " " (org-foresight-test--shown-at "→ office" org-foresight-agenda--place-column))))))

(ert-deftest org-foresight-test-place-spine-needs-a-column-of-its-own ()
  "A prefix with no room for it loses the whole bracket, not part of one.

Drawn where it fits and skipped where it does not, it would appear on the
grid's hour lines and vanish on every row with a category -- which reads as a
bracket with holes in it rather than as a setting nobody turned on."
  (org-foresight-test--with-agenda "* nothing dated\n"
    (let ((org-foresight-places '((office . "本社")))
          (org-foresight-home-place 'home)
          (org-foresight-travel-matrix '(((home . office) . 60)))
          (org-foresight--shape-cache nil)
          (org-foresight-day-places
           (list (cons (nth 6 (decode-time (org-foresight--day-start 0)))
                       'office))))
      (org-foresight-test--agenda)
      (should (string-match-p "\\`[[:space:]]*\\'"
                              (org-foresight-test--spine-of nil org-foresight-agenda--place-column)))
      ;; and the first bracket is untouched by its absence
      (should (string-match-p "┌" (org-foresight-test--spine-of))))))

(ert-deftest org-foresight-test-place-spine-is-drawn-day-by-day ()
  "A week of mixed places brackets each day from that day's journeys.

The spans are kept per day for the same reason the gaps are: a view of
several days that drew them all from the first would be confidently wrong
about every day but one."
  (let ((org-foresight-test--e2e-span 2))
    (org-foresight-test--with-wide-agenda "* nothing dated\n"
      (let* ((today (nth 6 (decode-time (org-foresight--day-start 0))))
             (org-foresight-day-places (list (cons today 'office))))
        (let* ((lines (org-foresight-test--agenda))
               (tomorrow (format-time-string
                          "%A" (time-add (current-time) (days-to-time 1)))))
          (ignore lines)
          ;; today is worked from the office and says so ...
          (should (string-match-p "┌" (org-foresight-test--spine-of nil org-foresight-agenda--place-column)))
          ;; ... and tomorrow, worked from home, carries no bracket at all
          (with-current-buffer org-agenda-buffer-name
            (goto-char (point-min))
            (should (re-search-forward (concat "^" tomorrow) nil t))
            (let ((rest (org-foresight-test--spine-of
                         (current-buffer)
                         org-foresight-agenda--place-column)))
              (should (string-match-p
                       "\\`[[:space:]]*\\'"
                       (substring rest (line-number-at-pos)))))))))))

(ert-deftest org-foresight-test-place-spine-can-be-turned-off ()
  "Off, the gutter is one bracket wide again."
  (org-foresight-test--with-wide-agenda "* nothing dated\n"
    (let ((org-foresight-agenda-place-spine nil)
          (org-foresight-day-places
           (list (cons (nth 6 (decode-time (org-foresight--day-start 0)))
                       'office))))
      (org-foresight-test--agenda)
      (should (string-match-p "\\`[[:space:]]*\\'"
                              (org-foresight-test--spine-of nil org-foresight-agenda--place-column))))))

(ert-deftest org-foresight-test-e2e-redo-works-wherever-the-cursor-is ()
  "Every line must answer to `r', including the ones this package drew.

`org-agenda-redo' takes the command to re-run from the text under point, and
Org stamps that property over the buffer just *before* the finalize hook
runs.  So anything inserted from that hook was born without it, and a cursor
resting there made `r', `g' and every view toggle -- which redraw by way of
redo -- do nothing at all.  Silently, and only sometimes: it depended on
where the cursor happened to be."
  (org-foresight-test--with-agenda
      (concat "* Standup\n:PROPERTIES:\n:CATEGORY: meeting\n:END:\n"
              (org-foresight-test--stamp 0 "09:30" "09:45") "\n")
    (org-foresight-test--agenda)
    (with-current-buffer org-agenda-buffer-name
      (goto-char (point-min))
      (let ((missing '()))
        (while (not (eobp))
          (unless (or (eolp)            ; a blank line belongs to nobody
                      (get-text-property (point) 'org-redo-cmd))
            (push (buffer-substring-no-properties
                   (line-beginning-position) (line-end-position))
                  missing))
          ;; the first column especially: `n' and `p' leave the cursor there,
          ;; and the spine draws exactly there
          (forward-line 1))
        (should-not missing)))))

(ert-deftest org-foresight-test-e2e-toggling-log-mode-takes-effect ()
  "Turning the log on and off redraws the day, with our blocks in place.

The toggle redraws through `org-agenda-redo', so this is the same defect as
above seen from the outside -- and the way it was actually noticed."
  (org-foresight-test--with-agenda
      (concat "* Standup\n:PROPERTIES:\n:CATEGORY: meeting\n:END:\n"
              (org-foresight-test--stamp 0 "09:30" "09:45") "\n"
              "* DONE Finished thing\nCLOSED: "
              (org-foresight-test--logstamp 0 "11:00") "\n"
              ":LOGBOOK:\n- State \"DONE\"       from \"ONGO\"       "
              (org-foresight-test--logstamp 0 "11:00") "\n:END:\n")
    (org-foresight-test--agenda)
    (cl-letf (((symbol-function 'org-foresight-observe--get-json)
               (lambda (&rest _) nil)))
      (with-current-buffer org-agenda-buffer-name
        (let ((states (lambda ()
                        (seq-count (lambda (l) (string-match-p "State:" l))
                                   (split-string (buffer-string) "\n")))))
          (should (= 1 (funcall states)))
          ;; from the top of the buffer, which is inside the Capacity block
          (goto-char (point-min))
          (org-agenda-log-mode)
          (should (= 0 (funcall states)))
          ;; and back on, from the first column of a row wearing the spine
          (goto-char (point-min))
          (re-search-forward "work starts")
          (beginning-of-line)
          (org-agenda-log-mode)
          (should (= 1 (funcall states))))))))

;;;; Where the day is worked from
;; The one thing no entry can say.  Work that needs a place is not late until
;; the next day at that place has gone, and asking when that is needs the day
;; to have a place of its own.

(ert-deftest org-foresight-test-day-place-falls-back-to-home ()
  "A day nobody has said anything about is worked from home.

Never nil: every caller asks \"is this entry bound to where I am\", and a nil
there would make every placeless entry look bound."
  (org-foresight-test--with-day "* nothing\n"
    (let ((org-foresight-day-places nil)
          (org-foresight-home-place 'home)
          (org-foresight--shape-cache nil))
      (should (eq 'home (org-foresight-day-place (org-foresight-test--ts 0 0 10)))))))

(ert-deftest org-foresight-test-day-place-comes-from-the-weekday-then-the-heading ()
  "A week with a shape needs no daily input; a day that breaks it says so."
  (let ((file (make-temp-file
               "org-foresight-day" nil ".org"
               "* 2026\n** 2026-08 August\n*** 2026-08-11 Tue\n:PROPERTIES:\n:PLACE: client\n:END:\n")))
    (unwind-protect
        (let ((org-foresight-day-file file)
              (org-foresight-home-place 'home)
              (org-foresight-day-places '((1 . office) (2 . office)))
              (org-foresight--shape-cache nil))
          ;; 2026-08-10 is a Monday, 08-11 a Tuesday, 08-12 a Wednesday
          (should (eq 'office (org-foresight-day-place (org-foresight-test--ts 0 0 10))))
          ;; the heading beats the weekday
          (should (eq 'client (org-foresight-day-place (org-foresight-test--ts 0 0 11))))
          ;; and an unlisted weekday is home
          (should (eq 'home (org-foresight-day-place (org-foresight-test--ts 0 0 12)))))
      (delete-file file))))

(ert-deftest org-foresight-test-next-day-at-skips-today ()
  "\"When am I next there\" is asked by somebody who is there now.

So today never answers it, and a place that does not come round inside the
horizon answers nil rather than a date nobody will keep."
  (org-foresight-test--with-day "* nothing\n"
    (let ((org-foresight-home-place 'home)
          (org-foresight-day-places '((1 . office) (3 . office)))
          (org-foresight--shape-cache nil)
          (monday (org-foresight-test--ts 0 0 10)))
      ;; Monday is an office day; the next one is Wednesday, not today
      (should (equal "2026-08-12"
                     (format-time-string
                      "%Y-%m-%d" (org-foresight-next-day-at 'office monday))))
      ;; nowhere in the horizon is nil, not an invention
      (should-not (org-foresight-next-day-at 'client monday)))))

(defun org-foresight-test--legs (day)
  "Return DAY's journeys as \"HH:MM-HH:MM\" strings, borrowed ones marked."
  (let ((org-foresight--shape-cache nil))
    (mapcar (lambda (b)
              (format "%s-%s%s"
                      (format-time-string "%H:%M" (plist-get b :start))
                      (format-time-string "%H:%M" (plist-get b :end))
                      (if (plist-get b :borrowed) " borrowed" "")))
            (seq-filter (lambda (b) (eq (plist-get b :kind) 'travel))
                        (org-foresight-day-blocks day)))))

(defmacro org-foresight-test--with-commute (text &rest body)
  "Run BODY over TEXT with the office an hour away."
  (declare (indent 1))
  `(org-foresight-test--with-day ,text
     (let ((org-foresight-places '((office . "本社")))
           (org-foresight-home-place 'home)
           (org-foresight-travel-matrix '(((home . office) . 60)))
           (org-foresight-travel-default 60))
       ,@body)))

(ert-deftest org-foresight-test-a-journey-is-pinned-by-what-needs-you-there ()
  "One rule, and it decides where every journey sits.

A journey is placed as close as it can be to the moment that pins it.
Usually what is pinned is the arrival -- something needs you there -- so the
journey is the last slot that gets you there in time.  A day worked from
somewhere else is the case that shows the rule has two ends: nothing needs
you at nine o'clock in particular, what needs you is the day, so what is
pinned is the departure and the journey is the first thing the day does."
  (let ((day (org-foresight-test--ts 0 0 10)))
    ;; nowhere to be: no journey at all
    (org-foresight-test--with-commute "* nothing dated\n"
      (let ((org-foresight-day-places nil))
        (should (null (org-foresight-test--legs day)))))
    ;; a day worked from the office, with nothing in the calendar: the
    ;; departure is pinned, so the journey opens the working day
    (org-foresight-test--with-commute "* nothing dated\n"
      (let ((org-foresight-day-places '((1 . office))))
        (should (equal '("09:00-10:00" "16:30-17:30")
                       (org-foresight-test--legs day)))))
    ;; a meeting there, early enough that leaving at nine would be too late:
    ;; the arrival is pinned instead, and the journey starts before the day
    (org-foresight-test--with-commute
        (concat "* Standup\n:PROPERTIES:\n:LOCATION: 本社\n:END:\n"
                "<2026-08-10 Mon 09:30-09:45>\n")
      (let ((org-foresight-day-places '((1 . office))))
        (should (equal '("08:30-09:30 borrowed" "16:30-17:30")
                       (org-foresight-test--legs day)))))
    ;; a meeting there later on does not change it: you go in for the day,
    ;; not for the meeting
    (org-foresight-test--with-commute
        (concat "* Review\n:PROPERTIES:\n:LOCATION: 本社\n:END:\n"
                "<2026-08-10 Mon 15:00-16:00>\n")
      (let ((org-foresight-day-places '((1 . office))))
        (should (equal '("09:00-10:00" "16:30-17:30")
                       (org-foresight-test--legs day)))))
    ;; and from home, the way out is unchanged: as late as it can be while
    ;; still arriving in time
    (org-foresight-test--with-commute
        (concat "* Review\n:PROPERTIES:\n:LOCATION: 本社\n:END:\n"
                "<2026-08-10 Mon 11:00-12:00>\n")
      (let ((org-foresight-day-places nil))
        (should (equal "10:00-11:00" (car (org-foresight-test--legs day))))))))

(ert-deftest org-foresight-test-the-way-back-is-the-same-rule-mirrored ()
  "Coming back is the way in read from the other end.

Normally the arrival is what is pinned -- the day ends at half five and you
are home then -- so the journey is the last slot that manages it and sits
inside the hours.  When something holds you there past that point the
departure is pinned instead, exactly as a meeting early enough to matter pins
the arrival on the way in, and the journey runs into the evening as borrowed.

Both halves of that were wrong before this: the journey home was placed by
arrival alone, so a meeting running to six put you home at half five and at
the office at six, and on a day out from home it sent you home in the middle
of the meeting."
  (let ((day (org-foresight-test--ts 0 0 10)))
    ;; held past the end of the day, from a day worked at the office
    (org-foresight-test--with-commute
        (concat "* Late review\n:PROPERTIES:\n:LOCATION: 本社\n:END:\n"
                "<2026-08-10 Mon 17:00-18:00>\n")
      (let ((org-foresight-day-places '((1 . office))))
        (should (equal '("09:00-10:00" "18:00-19:00 borrowed")
                       (org-foresight-test--legs day)))))
    ;; and from home, where the way in is pinned by the meeting instead
    (org-foresight-test--with-commute
        (concat "* Late review\n:PROPERTIES:\n:LOCATION: 本社\n:END:\n"
                "<2026-08-10 Mon 17:00-18:00>\n")
      (let ((org-foresight-day-places nil))
        (should (equal '("16:00-17:00" "18:00-19:00 borrowed")
                       (org-foresight-test--legs day)))))))

(ert-deftest org-foresight-test-you-come-back-when-the-errand-ends ()
  "Nothing keeps you somewhere once the thing that took you there is over.

Waiting until the day closed put you at the office from noon until half four
with nothing to be there for -- and offered those hours as though they could
be worked, which is the part that actually costs something."
  (let ((day (org-foresight-test--ts 0 0 10)))
    ;; an unbroken day: back as soon as the meeting ends
    (org-foresight-test--with-commute
        (concat "* Review\n:PROPERTIES:\n:LOCATION: 本社\n:END:\n"
                "<2026-08-10 Mon 11:00-12:00>\n")
      (let ((org-foresight-day-places nil))
        (should (equal '("10:00-11:00" "12:00-13:00")
                       (org-foresight-test--legs day)))))
    ;; a day that breaks for lunch: the way back cannot be planned into the
    ;; break any more than the way out could, so it waits for the afternoon
    (org-foresight-test--with-commute
        (concat "* Review\n:PROPERTIES:\n:LOCATION: 本社\n:END:\n"
                "<2026-08-10 Mon 11:00-12:00>\n")
      (let ((org-foresight-work '(("09:00" . "12:00") ("13:00" . "17:30")))
            (org-foresight-day-places nil))
        (should (equal '("10:00-11:00" "13:00-14:00")
                       (org-foresight-test--legs day)))))))

(ert-deftest org-foresight-test-an-excursion-returns-to-where-the-day-is-worked ()
  "On a day worked from the office, an errand elsewhere is an excursion.

You go back, because the rest of the day still happens there -- unless
getting back would land after the moment you would have to set off home, in
which case going back is a journey to nowhere and you go straight home."
  (let ((day (org-foresight-test--ts 0 0 10)))
    (org-foresight-test--with-commute
        (concat "* On site\n:PROPERTIES:\n:LOCATION: 顧客先\n:END:\n"
                "<2026-08-10 Mon 10:00-11:00>\n")
      (let ((org-foresight-places '((office . "本社") (client . "顧客")))
            (org-foresight-travel-matrix '(((home . office) . 60)
                                           ((office . client) . 45)
                                           ((home . client) . 75)))
            (org-foresight-day-places '((1 . office))))
        ;; in, out to the client, back to the office, home at the end
        (should (equal '("09:00-10:00" "11:00-11:45" "16:30-17:30")
                       (org-foresight-test--legs day)))))
    ;; the same errand at the end of the day: no point going back
    (org-foresight-test--with-commute
        (concat "* On site\n:PROPERTIES:\n:LOCATION: 顧客先\n:END:\n"
                "<2026-08-10 Mon 15:00-16:00>\n")
      (let ((org-foresight-places '((office . "本社") (client . "顧客")))
            (org-foresight-travel-matrix '(((home . office) . 60)
                                           ((office . client) . 45)
                                           ((home . client) . 75)))
            (org-foresight-day-places '((1 . office))))
        (should (equal '("09:00-10:00" "14:15-15:00" "16:15-17:30")
                       (org-foresight-test--legs day)))))))

(ert-deftest org-foresight-test-the-journey-home-waits-for-the-meeting ()
  "You cannot be at home and in the meeting, and the day must not say you are.

The bands partition the day, so an impossible pair does not merely look odd:
one of them is trimmed to make room, and the meeting came out shorter than it
is.  A day that quietly shortens a meeting to fit a journey is a day that
cannot be trusted about either."
  (org-foresight-test--with-commute
      (concat "* Late review\n:PROPERTIES:\n:LOCATION: 本社\n:END:\n"
              "<2026-08-10 Mon 17:00-18:00>\n")
    (let* ((org-foresight-day-places '((1 . office)))
           (org-foresight--shape-cache nil)
           (day (org-foresight-test--ts 0 0 10))
           (meeting (seq-find (lambda (b) (eq (plist-get b :kind) 'meeting))
                              (org-foresight-day-blocks day))))
      ;; the meeting keeps its full hour ...
      (should (equal "17:00" (format-time-string "%H:%M" (plist-get meeting :start))))
      (should (equal "18:00" (format-time-string "%H:%M" (plist-get meeting :end))))
      ;; ... and nothing else is drawn over it
      (should-not (seq-find (lambda (b)
                              (and (eq (plist-get b :kind) 'travel)
                                   (time-less-p (plist-get b :start)
                                                (plist-get meeting :end))
                                   (time-less-p (plist-get meeting :start)
                                                (plist-get b :end))))
                            (org-foresight-day-blocks day))))))

(ert-deftest org-foresight-test-going-in-costs-the-working-day ()
  "Travel is work, so an office day is two hours shorter for work.

Placed outside the hours instead, the journey would come out of the morning,
and going in would leave the same working day as staying home -- which is the
arithmetic that makes a token appearance at the office look free.  The whole
reason the day has a place of its own is to stop that being true."
  (let ((day (org-foresight-test--ts 0 0 10)))
    (org-foresight-test--with-commute "* nothing dated\n"
      (let* ((cap (lambda ()
                    (let ((org-foresight--shape-cache nil))
                      (org-foresight-capacity
                       day nil (org-foresight-test--ts 6 0 10)))))
             (home (let ((org-foresight-day-places nil)) (funcall cap)))
             (office (let ((org-foresight-day-places '((1 . office)))) (funcall cap))))
        ;; the span is the same declaration on both days
        (should (= (plist-get home :span-min) (plist-get office :span-min)))
        ;; but two hours of the office one are spent getting there and back
        (should (= 0.0 (plist-get home :travel-min)))
        (should (= 120.0 (plist-get office :travel-min)))
        (should (= (- (plist-get home :spare-min) 120.0)
                   (plist-get office :spare-min)))
        ;; and none of it is quietly taken out of the time off
        (should (= 0.0 (plist-get office :borrowed-min)))))))

(ert-deftest org-foresight-test-being-there-already-is-not-another-journey ()
  "The commute is made once, whatever is in the calendar that day.

A meeting at the place the day is already worked from adds nothing: the
journey in covers it, which is the difference between costing a day and
costing every entry in it."
  (org-foresight-test--with-commute
      (concat "* Office meeting\n:PROPERTIES:\n:LOCATION: 本社\n:END:\n"
              "<2026-08-10 Mon 11:00-12:00>\n")
    (let ((day (org-foresight-test--ts 0 0 10)))
      (let ((org-foresight-day-places nil))
        (should (= 2 (length (org-foresight-test--legs day)))))
      (let ((org-foresight-day-places '((1 . office))))
        (should (= 2 (length (org-foresight-test--legs day))))))))

;;;; The two ends of the day

(defmacro org-foresight-test--with-checks (text &rest body)
  "Run BODY over TEXT with the office an hour away and both checks asked for."
  (declare (indent 1))
  `(org-foresight-test--with-commute ,text
     (let ((org-foresight-check-in '(:minutes 10 :title "look at the day"))
           (org-foresight-check-out '(:minutes 10 :title "before you leave")))
       ,@body)))

(defun org-foresight-test--checks (day)
  "Return DAY's checks as \"HH:MM-HH:MM TITLE\" strings, clashes marked.

Read from the ledger, which is where the agenda reads them: a band lying
under something else is dropped when the day is made a partition, and a check
that will not fit is precisely the one worth asserting on."
  (let ((org-foresight--shape-cache nil))
    (mapcar (lambda (b)
              (format "%s-%s %s%s"
                      (format-time-string "%H:%M" (plist-get b :start))
                      (format-time-string "%H:%M" (plist-get b :end))
                      (plist-get b :title)
                      (if (plist-get b :wont-fit) " wont-fit" "")))
            (seq-filter (lambda (b) (eq (plist-get b :kind) 'check))
                        (append (aref (plist-get (org-foresight-scan 1 day) :ledger)
                                      0)
                                nil)))))

(ert-deftest org-foresight-test-checks-sit-inside-the-journeys ()
  "Arrive, look at the day; look at it again, then set off back.

The order matters and is the whole of the design.  Placing the checks first
and fitting the journeys around them would put the second in the last minutes
of the day and push the journey home past the end of it, so every office day
would borrow an hour of the evening for a commute that used to fit.  Derived
from the journeys instead, the day is unmoved: what the checks take is time
at the desk, which is what they actually take."
  (org-foresight-test--with-checks "* nothing dated\n"
    (let ((day (org-foresight-test--ts 0 0 10))
          (org-foresight-day-places '((1 . office))))
      (should (equal '("10:00-10:10 look at the day"
                       "16:20-16:30 before you leave")
                     (org-foresight-test--checks day)))
      ;; and the journeys are exactly where they were without them
      (should (equal '("09:00-10:00" "16:30-17:30")
                     (org-foresight-test--legs day))))))

(ert-deftest org-foresight-test-checks-fall-back-to-the-working-day ()
  "With no journey to sit inside, the checks take the day's own two ends."
  (org-foresight-test--with-checks "* nothing dated\n"
    (let ((day (org-foresight-test--ts 0 0 10))
          (org-foresight-day-places nil))
      (should (equal '("09:00-09:10 look at the day"
                       "17:20-17:30 before you leave")
                     (org-foresight-test--checks day))))))

(ert-deftest org-foresight-test-a-check-slides-past-what-is-in-the-way ()
  "The last ten minutes before leaving, or the last ten that are free.

Searched backwards from the departure exactly as the journey home is searched
backwards from the end of the day -- and, like it, shown where it lands
rather than dropped."
  (org-foresight-test--with-checks
      (concat "* Late review\n:PROPERTIES:\n:LOCATION: 本社\n:END:\n"
              "<2026-08-10 Mon 17:00-18:00>\n")
    (let ((day (org-foresight-test--ts 0 0 10))
          (org-foresight-day-places '((1 . office))))
      (should (equal "16:50-17:00 before you leave"
                     (cadr (org-foresight-test--checks day))))
      ;; the journey home is still the one the meeting forced, unmoved
      (should (equal '("09:00-10:00" "18:00-19:00 borrowed")
                     (org-foresight-test--legs day))))))

(ert-deftest org-foresight-test-a-check-is-not-planned-into-the-break ()
  "An hour set aside for lunch is no more available to a check than to a drive."
  (org-foresight-test--with-checks "* nothing dated\n"
    (let ((day (org-foresight-test--ts 0 0 10))
          (org-foresight-day-places nil)
          (org-foresight-work '(("09:00" . "12:00") ("13:00" . "17:30"))))
      (should (equal '("09:00-09:10 look at the day"
                       "17:20-17:30 before you leave")
                     (org-foresight-test--checks day))))))

(ert-deftest org-foresight-test-the-last-free-ten-minutes-are-the-check ()
  "An afternoon with nothing spare in it moves the check, and says when to.

\"Ten to twelve\" is the answer worth having on a day like this: after noon
you are in meetings and then on the way home, so that is the last moment
there is.  Searched back only as far as the working day began -- and to the
same place whether or not the day declares a lunch break, which was not true
when it stopped at the last stretch of work."
  (pcase-dolist (`(,hours . ,expected)
                 '(((("09:00" . "17:30")) . "12:50-13:00 before you leave")
                   ((("09:00" . "12:00") ("13:00" . "17:30"))
                    . "11:50-12:00 before you leave")))
    (org-foresight-test--with-checks
        (concat "* All-hands\n:PROPERTIES:\n:CATEGORY: meeting\n:END:\n"
                "<2026-08-10 Mon 13:00-15:30>\n"
                "* Client review\n:PROPERTIES:\n:CATEGORY: meeting\n:END:\n"
                "<2026-08-10 Mon 15:30-17:30>\n")
      (let ((day (org-foresight-test--ts 0 0 10))
            (org-foresight-work hours)
            (org-foresight-day-places nil))
        ;; The two answers differ by exactly the declared break, which is the
        ;; rule working rather than failing: an hour set aside for lunch is
        ;; not ten free minutes, so the day that declares one has its last
        ;; chance an hour earlier.
        (should (equal expected (cadr (org-foresight-test--checks day))))))))

(ert-deftest org-foresight-test-a-check-with-nowhere-to-go-is-still-shown ()
  "A day without ten free minutes anywhere is the day this must not go quiet.

It went quiet: the check landed on top of what filled the day, and a band
lying entirely under another is dropped when the day is made into a partition
-- so the row vanished from exactly the day worth telling about.  It comes
from the ledger now, drawn at its full length and marked like a journey that
cannot be made."
  (org-foresight-test--with-checks
      (concat "* Marathon\n:PROPERTIES:\n:CATEGORY: meeting\n:END:\n"
              "<2026-08-10 Mon 09:00-17:30>\n")
    (let ((day (org-foresight-test--ts 0 0 10))
          (org-foresight-day-places nil))
      (should (equal '("09:00-09:10 look at the day wont-fit"
                       "17:20-17:30 before you leave wont-fit")
                     (org-foresight-test--checks day)))
      ;; and the row reaches the agenda, which is where it went missing
      (should (seq-find (lambda (r) (string-match-p "before you leave" r))
                        (org-foresight-agenda--checks
                         (aref (plist-get (org-foresight-scan 1 day) :ledger)
                               0)))))))

(ert-deftest org-foresight-test-checks-are-off-until-they-are-asked-for ()
  "Nothing is booked by default.

Unlike a journey this is not derived from anything written down: whether the
day opens with a ritual is a fact about a person, and a package that assumed
one would be taking twenty minutes out of every stranger's day."
  (org-foresight-test--with-commute "* nothing dated\n"
    (let ((day (org-foresight-test--ts 0 0 10))
          (org-foresight-day-places '((1 . office))))
      (should (null (org-foresight-test--checks day))))))

(ert-deftest org-foresight-test-a-check-is-booked-work ()
  "Twenty minutes that cannot be spent twice, and are counted as such."
  (org-foresight-test--with-commute "* nothing dated\n"
    (let* ((day (org-foresight-test--ts 0 0 10))
           (org-foresight-day-places nil)
           (bare (let ((org-foresight--shape-cache nil))
                   (plist-get (org-foresight-capacity day) :booked-min))))
      (let ((org-foresight-check-in '(:minutes 10 :title "in"))
            (org-foresight-check-out '(:minutes 10 :title "out"))
            (org-foresight--shape-cache nil))
        (should (= (+ bare 20)
                   (plist-get (org-foresight-capacity day) :booked-min)))))))

;;;; Where the body is

(defun org-foresight-test--spans (day)
  "Return DAY's time away from home as \"HH:MM-HH:MM PLACE\" strings."
  (let ((org-foresight--shape-cache nil))
    (mapcar (pcase-lambda (`(,from ,to . ,place))
              (format "%s-%s %s"
                      (format-time-string "%H:%M" from)
                      (format-time-string "%H:%M" to)
                      place))
            (org-foresight-day-place-spans day))))

(ert-deftest org-foresight-test-a-span-runs-from-arrival-to-departure ()
  "Being on the way to the office is not being at the office.

The hours in transit belong to neither end, so a span starts where a journey
finishes and stops where the next one begins.  Anything else would offer the
drive as time at the place, which is the one hour it certainly is not."
  (org-foresight-test--with-commute "* nothing dated\n"
    (let ((day (org-foresight-test--ts 0 0 10))
          (org-foresight-day-places '((1 . office))))
      (should (equal '("10:00-16:30 office") (org-foresight-test--spans day))))))

(ert-deftest org-foresight-test-an-errand-is-a-span-of-its-own ()
  "A day worked from home is unmarked until the errand, and after it."
  (org-foresight-test--with-commute
      (concat "* Review\n:PROPERTIES:\n:LOCATION: 本社\n:END:\n"
              "<2026-08-10 Mon 11:00-12:00>\n")
    (let ((day (org-foresight-test--ts 0 0 10))
          (org-foresight-day-places nil))
      (should (equal '("11:00-12:00 office") (org-foresight-test--spans day))))))

(ert-deftest org-foresight-test-two-places-are-two-spans ()
  "Office, then a client, then home: two brackets with the road between them."
  (org-foresight-test--with-commute
      (concat "* Client visit\n:PROPERTIES:\n:LOCATION: 顧客\n:END:\n"
              "<2026-08-10 Mon 15:00-16:00>\n")
    (let ((day (org-foresight-test--ts 0 0 10))
          (org-foresight-places '((office . "本社") (client . "顧客")))
          (org-foresight-travel-matrix '(((home . office) . 60)
                                         ((office . client) . 45)
                                         ((home . client) . 75)))
          (org-foresight-day-places '((1 . office))))
      (should (equal '("10:00-14:15 office" "15:00-16:15 client")
                     (org-foresight-test--spans day))))))

(ert-deftest org-foresight-test-a-place-next-door-is-still-a-place ()
  "Where getting there costs nothing there is no journey to wait for.

The day is then spent at its own place from the first minute, and a span read
off the journeys alone would have said it was spent at home."
  (org-foresight-test--with-commute "* nothing dated\n"
    (let ((day (org-foresight-test--ts 0 0 10))
          (org-foresight-travel-matrix '(((home . office) . 0)))
          (org-foresight-travel-default 0)
          (org-foresight-day-places '((1 . office))))
      (should (null (org-foresight-test--legs day)))
      (should (equal '("09:00-17:30 office") (org-foresight-test--spans day))))))

;;;; What only being here can do

(defmacro org-foresight-test--with-places (text &rest body)
  "Run BODY over TEXT with today at the office and tomorrow at home."
  (declare (indent 1))
  `(org-foresight-test--with-signals ,text
     (let ((org-foresight-places '((office . "本社") (client . "顧客")))
           (org-foresight-home-place 'home)
           ;; today is whatever day the test runs on, so both are named
           (org-foresight-day-places
            (list (cons (nth 6 (decode-time (org-foresight--day-start 0))) 'office)))
           (org-foresight--shape-cache nil)
           (org-foresight--signals-cache nil))
       ,@body)))

(ert-deftest org-foresight-test-here-is-what-this-place-can-do ()
  "Bound to where you are, and nothing else.

Most work goes anywhere; what lands here is the little that does not, which
is the only reason the list is worth reading at the door."
  (org-foresight-test--with-places
      "* NEXT stamp the form
:PROPERTIES:
:PLACE: office
:END:
* NEXT ask about the spec
:PROPERTIES:
:PEOPLE: 佐藤
:END:
* NEXT measure on site
:PROPERTIES:
:PLACE: client
:END:
* NEXT write the report
"
    (let ((titles (mapcar (lambda (r) (plist-get r :title)) (org-foresight-here))))
      (should (member "stamp the form" titles))
      ;; a person is not a place: this one is a message, and messages travel
      (should-not (member "ask about the spec" titles))
      ;; somewhere else is not here
      (should-not (member "measure on site" titles))
      ;; and work that needs nowhere in particular is not the door's business
      (should-not (member "write the report" titles)))))

(ert-deftest org-foresight-test-here-puts-the-soonest-need-first ()
  "Deadlines first and earliest first; the rest keep their order.

Sorted rather than filtered by deadline: a file that does not use them would
show nothing at all under a filter, and \"what can only be done here\" is
worth answering either way."
  (org-foresight-test--with-places
      (concat "* NEXT no date\n:PROPERTIES:\n:PLACE: office\n:END:\n"
              "* NEXT later\nDEADLINE: " (org-foresight-test--stamp 9) "\n"
              ":PROPERTIES:\n:PLACE: office\n:END:\n"
              "* NEXT sooner\nDEADLINE: " (org-foresight-test--stamp 2) "\n"
              ":PROPERTIES:\n:PLACE: office\n:END:\n")
    (should (equal '("sooner" "later" "no date")
                   (mapcar (lambda (r) (plist-get r :title)) (org-foresight-here))))))

(ert-deftest org-foresight-test-here-marks-what-the-next-visit-is-too-late-for ()
  "The mark is about the place running out, not the clock.

A deadline that falls before you are next here is one this visit has to
settle; one that falls after it can wait for the next."
  (org-foresight-test--with-places
      (concat "* NEXT before I am back\nDEADLINE: " (org-foresight-test--stamp 1) "\n"
              ":PROPERTIES:\n:PLACE: office\n:END:\n")
    (let* ((rendered (substring-no-properties (org-foresight-report-here)))
           (next (org-foresight-next-day-at 'office)))
      ;; only one office day a week, so tomorrow's deadline cannot wait
      (should (or (null next) (string-match-p "⚠" rendered)))
      (should (string-match-p "next office day\\|not office again" rendered)))))

(ert-deftest org-foresight-test-here-names-who-it-needs ()
  "Who the conversation is with is part of deciding whether to have it now."
  (org-foresight-test--with-places
      "* NEXT talk about the review
:PROPERTIES:
:PLACE: office
:PEOPLE: 佐藤 田中
:END:
"
    (should (equal '("佐藤" "田中")
                   (plist-get (car (org-foresight-here)) :people)))
    (should (string-match-p "佐藤, 田中"
                            (substring-no-properties (org-foresight-report-here))))))

(ert-deftest org-foresight-test-work-that-today-cannot-do-is-a-signal ()
  "Work planned for today that today's place cannot do will not happen.

A home day with an office errand on it is a plan that does not survive
contact with the morning, and the morning is too late to find out."
  (org-foresight-test--with-places
      (concat "* NEXT measure on site\nSCHEDULED: " (org-foresight-test--stamp 0) "\n"
              ":PROPERTIES:\n:PLACE: client\n:END:\n")
    (let ((found (org-foresight-test--signal "Cannot be done from here")))
      (should found)
      (should (equal "measure on site" (plist-get (car found) :title)))
      (should (string-match-p "needs client" (plist-get (car found) :note))))))

(ert-deftest org-foresight-test-the-board-answers-the-dispatcher ()
  "It can be the FUNCTION of a custom agenda command, which hands it a match.

A command that refused the argument would need a wrapper in every config that
did nothing but drop it -- and taking one is not a personal setting, it is
the dispatcher's own contract."
  (unwind-protect
      (progn
        (should (commandp 'org-foresight-board))
        (org-foresight-board "")
        (should (get-buffer "*Org Foresight Board*")))
    (when (get-buffer "*Org Foresight Board*")
      (kill-buffer "*Org Foresight Board*"))))

(ert-deftest org-foresight-test-board-holds-both-questions ()
  "One buffer: what only here can do, and what is not planned at all."
  (org-foresight-test--with-places
      "* NEXT stamp the form
:PROPERTIES:
:PLACE: office
:END:
"
    (unwind-protect
        (progn
          (org-foresight-board)
          (with-current-buffer "*Org Foresight Board*"
            (let ((text (substring-no-properties (buffer-string))))
              (should (string-match-p "Here" text))
              (should (string-match-p "stamp the form" text))
              (should (string-match-p "Signals" text))
              ;; the rows answer to the agenda's own commands
              (goto-char (point-min))
              (should (re-search-forward "stamp the form" nil t))
              (should (get-text-property (line-beginning-position) 'org-marker)))))
      (when (get-buffer "*Org Foresight Board*")
        (kill-buffer "*Org Foresight Board*")))))

(provide 'org-foresight-test)

;;; org-foresight-test.el ends here
