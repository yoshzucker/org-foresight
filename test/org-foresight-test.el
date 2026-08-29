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
  `(let ((file (make-temp-file "org-foresight-test" nil ".org" ,text))
         ;; The redraw shares one survey between the agenda's two readers and
         ;; drops it when the redraw ends.  A test is not a redraw: it must
         ;; not be answered out of the corpus the last one was written
         ;; against, and every fixture here is a different corpus.
         (org-foresight--redraw-scan nil))
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

(ert-deftest org-foresight-test-parked-work-takes-no-time-out-of-the-day ()
  "Deciding not to do something now must cost less than not deciding.

Before there was a way to say it, the only way out of the undecided pile was
to make a heading look like work -- after which it took its estimate out of
every day it was not being done on.  Parked, it occupies nothing."
  (let ((text "* %s think about it
SCHEDULED: <2026-08-10 Mon>
:PROPERTIES:
:EFFORT:   0:45
:END:
"))
    (org-foresight-test--with-org (format text "NEXT")
      (let ((scan (org-foresight-scan 1 (org-foresight-test--ts 0 0))))
        (should (= 45.0 (aref (plist-get scan :committed) 0)))))
    (org-foresight-test--with-org (format text "SDAY")
      (let ((org-foresight-parked-keywords '("SDAY"))
            (org-todo-keywords '((sequence "NEXT" "SDAY" "|" "DONE"))))
        (let ((scan (org-foresight-scan 1 (org-foresight-test--ts 0 0))))
          (should (= 0.0 (aref (plist-get scan :committed) 0))))))))

(ert-deftest org-foresight-test-a-parked-leaf-is-owed-by-nobody ()
  "A project is not short of hours because of work nobody is doing.

Gated in the survey of the outline as well as in the survey of the day: a
deadline counts what its leaves still need, and a leaf that has been put down
needs nothing until it is picked up again."
  (let ((text "* NEXT the project
DEADLINE: <2026-09-30 Wed>
** %s the part nobody is doing
:PROPERTIES:
:EFFORT:   2:00
:END:
"))
    (org-foresight-test--with-org (format text "NEXT")
      (let* ((scan (org-foresight-project-scan))
             (unit (car (plist-get scan :units))))
        (should unit)
        (should (= 120.0 (plist-get unit :remaining-min)))))
    (org-foresight-test--with-org (format text "SDAY")
      (let ((org-foresight-parked-keywords '("SDAY"))
            (org-todo-keywords '((sequence "NEXT" "SDAY" "|" "DONE"))))
        (let* ((scan (org-foresight-project-scan))
               (unit (car (plist-get scan :units))))
          ;; the deadline is still there; nothing is owed against it
          (should (or (null unit) (= 0.0 (plist-get unit :remaining-min)))))))))

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
        (should (string-match-p "left of the day" verdict))
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

(ert-deftest org-foresight-test-the-verdict-holds-its-line ()
  "The line gives up its footnotes rather than wrapping.

Every term here is at its longest at once -- a twelve-hour span promised
twice over, the whole allowance still held, and a correction big enough to
show.  It is not a likely day, but it is a reachable one, and the failure it
would cause is not a wrong figure: it is a line that wraps and takes the bar
below it out of alignment with everything the block has said."
  (let ((cap (list :span-min 720.0 :ahead-min 720.0 :headroom-min -600.0
                   :committed-min 1320.0 :reserve-min 95.0
                   :reserve-day-min 95.0 :bias-min 150.0
                   :lands nil :work nil)))
    (should (org-foresight-test--within-80
             (concat org-foresight-report-margin
                     (org-foresight-report--verdict cap))))
    ;; and what it keeps is the answer, not whichever terms happened to be
    ;; short enough
    (let ((plain (substring-no-properties
                  (org-foresight-report--verdict cap))))
      (should (string-match-p "Work 12:00" plain))
      (should (string-match-p "left of the day" plain))
      (should (string-match-p "OVER by" plain)))))

(ert-deftest org-foresight-test-bar-fits-a-full-day ()
  "A day whose parts fill the span exactly is drawn at the configured width."
  (let ((cap '(:span-min 510.0 :ahead-min 510.0 :booked-min 137.0 :travel-min 60.0
               :private-min-in-span 0.0 :committed-min 73.0
               :reserve-min 57.0 :spare-min 183.0
               :private-min 0.0 :borrowed-min 0.0 :unclaimed-min 0.0)))
    (should (= (org-foresight-test--bar-cells (org-foresight-report--bar cap))
               org-foresight-bar-width))))

(ert-deftest org-foresight-test-bars-share-one-scale ()
  "The two bars must be comparable, or setting them side by side says nothing.

Equal spans of time have to draw equal numbers of cells whichever bar they
are in -- otherwise a long evening could look shorter than a short workday."
  (let* ((cap '(:span-min 480.0 :ahead-min 480.0 :booked-min 480.0 :travel-min 0.0
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
  (let* ((cap '(:span-min 510.0 :ahead-min 510.0 :booked-min 137.0 :travel-min 60.0
                :private-min-in-span 0.0 :committed-min 73.0
                :reserve-min 57.0 :spare-min 183.0
                :private-min 0.0 :borrowed-min 0.0 :unclaimed-min 0.0))
         (bar (org-foresight-report--bar cap))
         (at (string-match " " (substring-no-properties bar)))
         (face (and at (get-text-property at 'face bar))))
    (should at)
    (should (memq 'org-foresight-report-surge face))
    ;; bounded inside its own cell, so the row keeps the height of every
    ;; other row on the page
    (should (equal '(:box (:line-width -1)) (car face)))
    ;; and the reserve still occupies its columns, or the bar would not sum
    (should (= (org-foresight-test--bar-cells bar) org-foresight-bar-width))
    ;; the key names it with the same glyph the bar drew
    (let ((key (org-foresight-report--bar-key cap)))
      (should (string-match-p "  reserve" (substring-no-properties key))))))

(ert-deftest org-foresight-test-bar-marks-the-overflow ()
  "An overcommitted day shows where the span ran out instead of clipping."
  (let ((over '(:span-min 510.0 :ahead-min 510.0 :booked-min 420.0 :travel-min 0.0
                :private-min-in-span 0.0 :committed-min 240.0
                :reserve-min 60.0 :spare-min -210.0))
        (fits '(:span-min 510.0 :ahead-min 510.0 :booked-min 60.0 :travel-min 0.0
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
                 '(:span-min 0.0 :ahead-min 0.0 :booked-min 0.0 :travel-min 0.0
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
      ;; and the bar's own parts fill what is ahead exactly -- if the
      ;; segments and the whole ever disagree, the picture stops being
      ;; evidence.  Read at six in the morning that is the whole span, and
      ;; the two readings are asserted together so neither can drift.
      (should (= (plist-get cap :ahead-min) (plist-get cap :span-min)))
      (should (= (+ (plist-get cap :booked-min)
                    (plist-get cap :travel-min)
                    (plist-get cap :private-min-in-span)
                    (plist-get cap :committed-min)
                    (plist-get cap :reserve-min)
                    (plist-get cap :spare-min))
                 (plist-get cap :ahead-min))))))

(ert-deftest org-foresight-test-the-evening-is-divided-at-now-too ()
  "The hours off are split at NOW exactly as the working hours are.

The work row was made robust across the day and the off row was left as a
forecast, which meant it decayed in precisely the way the work row had
stopped doing: by the evening it was still offering hours that had gone.
Whatever is true of one row is true of the other -- they are two halves of
one waking day, and a day cannot be half measured."
  (org-foresight-test--with-day
      "* dinner
:PROPERTIES:
:CATEGORY: family
:END:
<2026-08-10 Mon 19:00-20:30>
"
    (let* ((org-foresight-surge-cache-file "/nonexistent/surge.eld")
           (org-foresight-leak-cache-file "/nonexistent/leak.eld")
           (org-foresight-surge-default "0:00")
           (org-foresight-leak-default "0:00")
           (org-foresight-lost-default "0:00")
           (day (org-foresight-test--ts 0 0 10))
           (cap (org-foresight-capacity day nil
                                        (org-foresight-test--ts 14 0 10))))
      ;; awake 07:00-23:00 less work 09:00-17:30 is 7:30 off, of which
      ;; 07:00-09:00 has gone by two in the afternoon
      (should (= 450.0 (plist-get cap :off-min)))
      (should (= 120.0 (plist-get cap :off-behind-min)))
      (should (= 330.0 (plist-get cap :off-ahead-min)))
      (should (= (plist-get cap :off-min)
                 (+ (plist-get cap :off-behind-min)
                    (plist-get cap :off-ahead-min))))
      ;; and the forecast terms divide what is left of the evening, not the
      ;; whole of it: the dinner is still to come, the morning is not
      (should (= 90.0 (plist-get cap :private-min)))
      (should (= (plist-get cap :off-ahead-min)
                 (+ (plist-get cap :private-min)
                    (plist-get cap :borrowed-min)
                    (plist-get cap :unclaimed-min)))))))

(ert-deftest org-foresight-test-capacity-counts-only-what-is-still-ahead ()
  "A meeting that is over takes nothing further from the day.

Capacity answers what may still be promised.  A band it has already lived
through cannot go on being subtracted: those minutes belong to the record of
what happened, and counting them here as well would spend the same hour
twice -- once as a plan, once as a fact -- which is exactly what makes a bar
drawn beside `org-foresight-behind\=' overlap it."
  (org-foresight-test--with-day
      "* morning review
<2026-08-10 Mon 10:00-11:00>
* across the hour
<2026-08-10 Mon 13:30-14:30>
* afternoon call
<2026-08-10 Mon 15:00-16:00>
"
    (let* ((org-foresight-surge-cache-file "/nonexistent/surge.eld")
           (org-foresight-leak-cache-file "/nonexistent/leak.eld")
           (org-foresight-surge-default "0:00")
           (org-foresight-leak-default "0:00")
           (org-foresight-lost-default "0:00")
           (day (org-foresight-test--ts 0 0 10))
           (cap (org-foresight-capacity day nil
                                        (org-foresight-test--ts 14 0 10))))
      ;; 09:00-17:30, read at 14:00: five hours gone, three and a half left
      (should (= 510.0 (plist-get cap :span-min)))
      (should (= 300.0 (plist-get cap :behind-min)))
      (should (= 210.0 (plist-get cap :ahead-min)))
      (should (= (plist-get cap :span-min)
                 (+ (plist-get cap :behind-min) (plist-get cap :ahead-min))))
      ;; the finished hour costs nothing, the one across NOW costs its half
      (should (= 90.0 (plist-get cap :booked-min)))
      (should (= 120.0 (plist-get cap :spare-min)))
      ;; and the terms still divide what is ahead exactly
      (should (= (+ (plist-get cap :booked-min)
                    (plist-get cap :travel-min)
                    (plist-get cap :private-min-in-span)
                    (plist-get cap :committed-min)
                    (plist-get cap :reserve-min)
                    (plist-get cap :spare-min))
                 (plist-get cap :ahead-min))))))

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
  (dolist (s (append (list (string org-foresight-block))
                     ;; Every mark, read from the key rather than listed here,
                     ;; so a mark added later cannot skip this.
                     (mapcar #'car org-foresight-agenda--mark-meanings)
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
        (format "* NEXT ordinary
SCHEDULED: %s
:PROPERTIES:
:EFFORT:   1:00
:END:
"
                (org-foresight-test--stamp (org-foresight-test--offset-to 2 28)))
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
      (format "* 子供の部活
:PROPERTIES:
:CATEGORY: club
:END:
%s
"
              (org-foresight-test--stamp (org-foresight-test--offset-to 2) "19:00" "21:00"))
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

(defun org-foresight-test--weeks-fixture ()
  "Five weeks of clock, built so each column has something to say.

comms   spends the same as it always has, every day of this week
admin   is new, and happened on one day
procure was there for four weeks and is not here now"
  (let (out)
    (cl-flet ((spell (title category offset minutes)
                (setq out
                      (concat out
                              (format "* NEXT %s\n:PROPERTIES:\n:CATEGORY: %s\n:END:\n:LOGBOOK:\nCLOCK: %s--%s =>  %s\n:END:\n"
                                      title category
                                      (org-foresight-test--logstamp offset "09:00")
                                      (org-foresight-test--logstamp
                                       offset (format "%02d:%02d"
                                                      (+ 9 (/ minutes 60))
                                                      (% minutes 60)))
                                      (org-duration-from-minutes minutes))))))
      ;; This week: comms every day (2:00 each, 14:00 in all), admin once.
      ;; Offsets are negative -- `org-foresight-test--logstamp' counts
      ;; forward, and a week that has happened is behind us.
      (dotimes (d 7) (spell (format "comms %d" d) "comms" (- d) 120))
      (spell "admin once" "admin" -2 60)
      ;; The four weeks before: comms 3:30 a week, so 14:00 in all and a
      ;; baseline of 3:30 -- this week is four times it, which reads +300%.
      ;; procurement was there and is not here now.
      (dolist (d '(7 14 21 28))
        (spell (format "comms back %d" d) "comms" (- d) 210)
        (spell (format "procure back %d" d) "procurement" (- d) 60)))
    out))

(defmacro org-foresight-test--with-weeks (&rest body)
  "Run BODY with the five-week fixture as the only agenda file.
WEEK is a survey of the seven days alone; LONG is the one the review reads,
covering the week and the four before it."
  (declare (indent 0))
  `(org-foresight-test--with-org (org-foresight-test--weeks-fixture)
     (let ((week (org-foresight-clock-scan 7))
           (long (org-foresight-clock-scan 35)))
       (ignore week long)
       ,@body)))

(defun org-foresight-test--weeks-row (text area)
  "Return the AREA row of the review table in TEXT, or nil."
  (seq-find (lambda (l) (string-match-p (concat "| " area " ") l))
            (split-string (substring-no-properties text) "\n")))

(ert-deftest org-foresight-test-the-week-is-measured-against-the-weeks-before ()
  "`vs 4w' compares a week with a week, not with the whole window.

The baseline is the earlier window divided by the weeks in it.  Compared with
its total instead, an area that has not changed at all would read as a
collapse of three quarters -- and every week would look like a bad one."
  (org-foresight-test--with-weeks
    (let* ((text (org-foresight-report-week long))
           (comms (org-foresight-test--weeks-row text "comms")))
      ;; 14:00 this week; 14:00 across the four before, so 3:30 a week
      (should comms)
      (should (string-match-p "+300%" comms)))))

(ert-deftest org-foresight-test-an-area-that-stopped-is-still-a-row ()
  "What was dropped is the thing worth seeing, so it keeps its line.

Built from the seven-day survey, an area with nothing in it this week is not
in the table at all -- and an absence nobody is shown is an absence nobody
notices."
  (org-foresight-test--with-weeks
    (let* ((text (org-foresight-report-week long))
           (gone (org-foresight-test--weeks-row text "procurement"))
           (fresh (org-foresight-test--weeks-row text "admin")))
      (should gone)
      (should (string-match-p "quiet" gone))
      (should (string-match-p "0:00" gone))
      (should fresh)
      (should (string-match-p "new" fresh)))))

(ert-deftest org-foresight-test-the-band-is-each-area-s-own-days ()
  "Every day or one day: the band answers it, and the week's total cannot.

Scaled per area, so a small area is a shape rather than a row of dots, and
two areas with the same total but different weeks do not draw the same band."
  (org-foresight-test--with-weeks
    (let* ((text (org-foresight-report-week long))
           (comms (org-foresight-test--weeks-row text "comms"))
           (admin (org-foresight-test--weeks-row text "admin"))
           (band (lambda (row) (nth 5 (split-string row "|" nil " ")))))
      ;; comms is every day of the week
      (should-not (cl-find org-foresight-report-dot (funcall band comms)))
      ;; admin is one day of it, and the rest are dots
      (should (= 6 (cl-count org-foresight-report-dot (funcall band admin))))
      (should (org-foresight-test--within-80 text)))))

(ert-deftest org-foresight-test-without-a-longer-window-the-week-is-unchanged ()
  "The extra columns arrive with the longer survey and not before it.
A caller that has not asked for them gets the table it always got."
  (org-foresight-test--with-weeks
    (let ((plain (org-foresight-report-week week)))
      (should-not (string-match-p "vs 4w" plain))
      (should-not (string-match-p "quiet" plain))
      ;; and the Share column keeps its full width
      (should (string-match-p "| Share              |" plain)))))

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

(defun org-foresight-test--offset-to (dow &optional least)
  "Return the smallest offset of at least LEAST days that lands on DOW.
DOW counts from Sunday, the way `decode-time' reports it.  Tests that turn
on a day being a working day -- or on a Saturday not being one -- need the
weekday to hold, and a named date stops holding it the moment it passes."
  (let ((n (or least 1)))
    (while (/= dow (nth 6 (decode-time
                           (time-add (org-foresight--day-start 0)
                                     (days-to-time n)))))
      (setq n (1+ n)))
    n))

(defun org-foresight-test--stamp (offset &optional from to repeater)
  "Return an active timestamp OFFSET days from today, optionally FROM-TO.
REPEATER, such as \"+1d\", is appended as written.

Signals are always computed about today, so a test that names a date is a
test that stops meaning what it said the moment the date passes."
  (concat "<" (format-time-string
               "%Y-%m-%d %a"
               (time-add (org-foresight--day-start 0) (days-to-time offset)))
          (when from (concat " " from))
          (when (and from to) (concat "-" to))
          (when repeater (concat " " repeater))
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

(ert-deftest org-foresight-test-a-signal-can-come-from-somewhere-else ()
  "A question this package has no business answering can still be asked here.

What is unsettled about a week is not only whether it fits, and a file loaded
later should not have to edit this one to say so.  The contributed signal is
read exactly as a built-in one is -- same shape, same kinds table, same
grouping -- so nothing about the page knows which is which."
  (org-foresight-test--with-signals "* NEXT something\n"
    (let ((org-foresight-signal-functions
           (list (lambda (_scan)
                   (list (cons "Pointed nowhere"
                               (list (org-foresight--finding
                                      "a goal" "nothing is moving toward it")))))))
          (org-foresight-signal-kinds
           (cons '("Pointed nowhere" . owed) org-foresight-signal-kinds)))
      (let ((found (org-foresight-test--signal "Pointed nowhere")))
        (should (= 1 (length found)))
        (should (equal "a goal" (plist-get (car found) :title))))
      ;; and it is grouped by what it says it is, not by who found it
      (should (eq 'owed (org-foresight-signal-kind "Pointed nowhere"))))))

(ert-deftest org-foresight-test-a-broken-signal-source-does-not-take-the-board ()
  "One contributor that signals must not cost the page everything else knows."
  (org-foresight-test--with-signals "* NEXT something\n"
    (let ((org-foresight-signal-functions
           (list (lambda (_scan) (error "no"))
                 (lambda (_scan)
                   (list (cons "Still counted"
                               (list (org-foresight--finding "x" "y"))))))))
      (should (= 1 (length (org-foresight-test--signal "Still counted")))))))

(ert-deftest org-foresight-test-a-plan-that-has-run-out-says-so ()
  "A project keeps its name after its last step is finished.

`:project-p' means a heading has a TODO child, and a finished child is still
one -- so a plan that has run out does not announce it.  It simply stops
having anything in it and goes on looking like work in hand, which is the
state a review exists to catch."
  (org-foresight-test--with-signals
      "* NEXT the plan that ran out
** DONE step one
** DONE step two
* NEXT the plan still moving
** DONE step one
** NEXT step two
"
    (let ((found (org-foresight-test--signal "Nothing to do next")))
      (should (= 1 (length found)))
      (should (equal "the plan that ran out" (plist-get (car found) :title)))
      ;; and it can be acted on from where it is reported
      (should (markerp (plist-get (car found) :marker))))))

(ert-deftest org-foresight-test-a-project-names-what-is-next-in-it ()
  "The next step is the first live leaf, in the order the file is written.

Not the nearest deadline or the largest: a review reads down a list, and the
first thing under a heading is what somebody would pick up if they opened it."
  (org-foresight-test--with-signals
      "* NEXT the plan
** DONE already done
** NEXT the one to pick up
** NEXT the one after that
"
    (let* ((projects (org-foresight-projects (org-foresight-outline-records)))
           (p (car projects)))
      (should (= 1 (length projects)))
      (should (equal "the plan" (plist-get (plist-get p :record) :title)))
      (should (equal "the one to pick up" (plist-get (plist-get p :next) :title)))
      (should (= 2 (plist-get p :live))))))

(ert-deftest org-foresight-test-a-parked-step-does-not-keep-a-plan-alive ()
  "Putting the only step down leaves the plan with nothing to do next.
Otherwise parking work would hide the plan that has stopped, which is the
opposite of what a review is for."
  (org-foresight-test--with-signals
      "* NEXT the plan
** SDAY the only step
"
    (let ((org-foresight-parked-keywords '("SDAY"))
          (org-todo-keywords '((sequence "NEXT" "SDAY" "|" "DONE"))))
      (let ((projects (org-foresight-projects (org-foresight-outline-records t))))
        (should (= 1 (length projects)))
        (should-not (plist-get (car projects) :next))))))

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
  "Handed-off work whose check-in has passed has gone quiet.

Relative dates, not written-out ones.  This test used to say
`<2026-08-20 Thu>' and mean \"next week\"; it passed until the day that date
arrived and then began reporting two findings where it wanted one.  A test
about what is overdue has to be told when today is."
  (org-foresight-test--with-signals
      (concat
       "* WAIT reply from vendor\nSCHEDULED: "
       (org-foresight-test--stamp -16) "\n"
       "* WAIT check in next week\nSCHEDULED: "
       (org-foresight-test--stamp 7) "\n"
       "* NEXT my own overdue task\nSCHEDULED: "
       (org-foresight-test--stamp -16) "\n")
    (let ((found (org-foresight-test--signal "Gone quiet (follow-up overdue)")))
      (should (= (length found) 1))
      (should (equal (plist-get (car found) :title) "reply from vendor")))))

(ert-deftest org-foresight-test-signal-meetings ()
  "A future meeting in a watched category with no prep recorded is a signal."
  (org-foresight-test--with-signals
      (format "* review with the board
:PROPERTIES:
:CATEGORY: outlook
:END:
%s
* already prepared
:PROPERTIES:
:CATEGORY: outlook
:PLAN_PREP: t
:END:
%s
* private appointment
:PROPERTIES:
:CATEGORY: personal
:END:
%s
"
              (org-foresight-test--stamp (org-foresight-test--offset-to 2) "10:00" "11:00")
              (org-foresight-test--stamp (org-foresight-test--offset-to 3) "10:00" "11:00")
              (org-foresight-test--stamp (org-foresight-test--offset-to 4) "10:00" "11:00"))
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
      (format "* the meeting
:PROPERTIES:
:UID: still-here
:CATEGORY: outlook
:PLAN_PREP: t
:END:
%s
* NEXT prep for the live meeting
:PROPERTIES:
:PLAN_MEETING_UID: still-here
:END:
* NEXT prep for a cancelled meeting
:PROPERTIES:
:PLAN_MEETING_UID: long-gone
:END:
"
              (org-foresight-test--stamp (org-foresight-test--offset-to 2) "10:00" "11:00"))
    (let ((found (org-foresight-test--signal "Orphaned prep")))
      (should (= (length found) 1))
      (should (equal (plist-get (car found) :title)
                     "prep for a cancelled meeting")))))

(ert-deftest org-foresight-test-signal-outside-work-hours ()
  "Work parked outside the working hours is subtracted from nothing, so it
must be named: it is the work that quietly stops the day ending on time."
  (org-foresight-test--with-window
    (org-foresight-test--with-signals
        (format "* NEXT evening call
%s
* NEXT during the day
%s
* NEXT saturday work
%s
"
                (org-foresight-test--stamp (org-foresight-test--offset-to 2) "19:00" "20:30")
                (org-foresight-test--stamp (org-foresight-test--offset-to 2) "10:00" "11:00")
                (org-foresight-test--stamp (org-foresight-test--offset-to 6) "10:00" "11:00"))
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
        (format "* NEXT nightly deploy watch
%s
"
                (org-foresight-test--stamp (org-foresight-test--offset-to 2) "19:00" "20:00" "+1d"))
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
  "Work promised for today is split by what stopped it fitting.

NOW is pinned, or the answer would depend on the hour the tests happen to
run -- which is also the point of the first list: what fits shrinks as the
day goes on, and by the evening everything is in it.  The second list does
not move with the clock, and that is the whole reason it is kept apart: an
entry in it is still there tomorrow morning, so it is asking to be broken
up rather than to be moved."
  (org-foresight-test--with-signals
      (let ((today (format-time-string "<%Y-%m-%d %a>"
                                       (org-foresight--day-start 0))))
        (concat "* NEXT longer than any day holds\nSCHEDULED: " today
                "\n:PROPERTIES:\n:EFFORT:   12:00\n:END:\n"
                "* NEXT ran out of afternoon\nSCHEDULED: " today
                "\n:PROPERTIES:\n:EFFORT:   4:00\n:END:\n"
                "* NEXT a short job\nSCHEDULED: " today
                "\n:PROPERTIES:\n:EFFORT:   0:15\n:END:\n"))
    (let* ((day (org-foresight--day-start 0))
           (scan (org-foresight-scan 1 day))
           (titles (lambda (at key)
                     (mapcar (lambda (f) (plist-get f :title))
                             (plist-get (org-foresight--fit-findings
                                         scan (org-foresight--hhmm-on day at))
                                        key)))))
      ;; 09:00-17:30 declared, so 2:30 of it is left and the longest a day
      ;; could ever offer is 8:30.  The four-hour job is between the two.
      (should (equal '("ran out of afternoon") (funcall titles "15:00" :today)))
      (should (equal '("longer than any day holds")
                     (funcall titles "15:00" :oversized)))
      ;; Before work starts the whole day is free, so nothing has run out of
      ;; it -- and the oversized job is oversized anyway.  The bound is the
      ;; working hours, not the hour it was asked at.
      (should-not (funcall titles "06:00" :today))
      (should (equal '("longer than any day holds")
                     (funcall titles "06:00" :oversized)))
      ;; The board gives a note 36 columns and truncates the overrun
      ;; silently, on the screen only, where nothing else here would see the
      ;; figure go.  What decides whether it fits is the widest estimate the
      ;; note can be handed, and no corpus reliably holds one -- a run is at
      ;; its widest when a correction and a whole day are both in it -- so
      ;; the widest is handed to the real formatter instead of hoped for.
      (cl-letf (((symbol-function 'org-foresight-report--effort-run)
                 (lambda (_) "1d 2:00→1d 8:00")))
        (let ((fit (org-foresight--fit-findings
                    scan (org-foresight--hhmm-on day "15:00"))))
          (dolist (key '(:today :oversized))
            (should (plist-get fit key))
            (dolist (f (plist-get fit key))
              (should (<= (string-width (plist-get f :note)) 36)))))))))

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
      (format "* NEXT a perfectly ordinary task
SCHEDULED: %s
:PROPERTIES:
:EFFORT:   1:00
:END:
"
              (org-foresight-test--stamp (org-foresight-test--offset-to 2)))
    (should (null (org-foresight-signals)))
    (should (string-match-p "nothing unaccounted for"
                            (org-foresight-report-signals)))))

(ert-deftest org-foresight-test-signals-block-within-80 ()
  "Long titles must be cut to the column, not allowed to run off the board.
Multibyte titles are the real hazard: they are twice as wide as they look."
  (org-foresight-test--with-window
    (org-foresight-test--with-signals
        (format "* NEXT 非常に長い日本語のタスク名で桁あふれを起こしかねないもの、さらに続く
%s
DEADLINE: %s
"
                (org-foresight-test--stamp (org-foresight-test--offset-to 2) "19:00" "20:30")
                (org-foresight-test--stamp (org-foresight-test--offset-to 2)))
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
      (format "* board review
:PROPERTIES:
:UID:      uid-1
:CATEGORY: outlook
:END:
%s
"
              (org-foresight-test--stamp (org-foresight-test--offset-to 2) "10:00" "11:00"))
    (org-foresight-prepare-meetings)
    (let ((text (with-current-buffer
                    (find-file-noselect org-foresight-test-tasks)
                  (buffer-string))))
      (should (= 1 (org-foresight-test--count "Prep: board review" text)))
      (should (= 1 (org-foresight-test--count "Follow up: board review" text)))
      ;; prep ends when the meeting starts; follow-up begins when it ends
      (should (string-match-p (format "SCHEDULED: %s"
                                      (org-foresight-test--stamp (org-foresight-test--offset-to 2) "09:30")) text))
      (should (string-match-p (format "SCHEDULED: %s"
                                      (org-foresight-test--stamp (org-foresight-test--offset-to 2) "11:00")) text))
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
      (format "* board review
:PROPERTIES:
:UID:      uid-1
:CATEGORY: outlook
:END:
%s
"
              (org-foresight-test--stamp (org-foresight-test--offset-to 2) "10:00" "11:00"))
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
      (format "* board review
:PROPERTIES:
:UID:      uid-1
:CATEGORY: outlook
:END:
%s
"
              (org-foresight-test--stamp (org-foresight-test--offset-to 2) "10:00" "11:00"))
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
stacked bar at the same scale -- so a day drawn in both blocks says the same
thing in both.

At the same scale, not at the same length.  This row's leader is longer than
the bars' own, so it has less of the line left and is cut sooner; a day
promised three times over used to be drawn here at the width that fitted the
other block, and ran off the end of this one.  What has to hold is that
neither bar disagrees with the other about how many columns an hour is."
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
      ;; today is drawn to the same scale in both blocks
      (let* ((cap (org-foresight-capacity (org-foresight--day-start 0) nil now))
             (bar (substring-no-properties (org-foresight-report--bar cap)))
             (row (car (split-string plain "\n")))
             (lead (string-width (format org-foresight-report--load-stub "" "")))
             (drawn (substring row lead))
             ;; The ellipsis stands for the cut, not for time, so it comes off
             ;; before the two are compared.
             (trim (lambda (b) (string-trim-right b "…"))))
        (should (> (length drawn) 0))
        (should (or (string-prefix-p (funcall trim bar) drawn)
                    (string-prefix-p (funcall trim drawn) bar)))
        ;; and each is cut to its own line rather than to the other's
        (should (org-foresight-test--within-80 row))))))

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
               (pos (string-match "Time the clock cannot account for" s)))
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
                ;; The same hours `org-foresight-demo-mode' declares, break
                ;; included.  A single unbroken block here would describe a
                ;; day the shipped corpus does not have, and the longest
                ;; sitting -- which decides what can be done in one go --
                ;; would come out nearly twice its real length.
                (org-foresight-work '(("09:00" . "12:00")
                                      ("13:00" . "17:30")))
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
        (keywords org-todo-keywords)
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
    (should (equal keywords org-todo-keywords))
    (should (equal surge org-foresight-surge-cache-file))
    (should (equal leak org-foresight-leak-cache-file))
    (should (equal bias org-foresight-bias-cache-file))))

(ert-deftest org-foresight-test-the-demo-brings-its-own-keywords ()
  "The demo is written in NEXT, so the demo has to declare NEXT.

Its one promise is that it can be looked at before anything is configured,
and `org-todo-keywords\=' defaults to TODO and DONE.  Without declaring them
every `NEXT\=' in the generated files reads as a heading whose first word is
odd -- not a task, not a project, nothing the package can see -- and the
demo comes up nearly empty on the machine it exists for.

Run through `org-foresight-demo-mode\=' with the stock keywords in force,
because that is the situation: the test fixture binds the keywords itself,
so anything written against the fixture would pass whether the mode declared
them or not.  Asserted through the classification rather than through the
variable -- what matters is that Org agrees these headings are work."
  (let ((org-todo-keywords '((sequence "TODO" "DONE")))
        (org-foresight-demo-directory
         (file-name-as-directory (make-temp-file "org-foresight-demo" t))))
    (unwind-protect
        (progn
          (org-foresight-demo-mode 1)
          (let ((recs (plist-get (org-foresight-project-scan) :headings)))
            (should (seq-some (lambda (r) (equal (plist-get r :todo) "NEXT"))
                              recs))
            (should (seq-some (lambda (r) (plist-get r :deadline-project-p))
                              recs))))
      (org-foresight-demo-mode -1)
      (delete-directory org-foresight-demo-directory t))))

(ert-deftest org-foresight-test-signal-notes-fit-their-column ()
  "No signal writes a note wider than the column that draws it.

The board gives a note 36 columns and truncates what overruns, silently and
only on the screen -- so a note one column too long loses the figure it
exists to carry and nothing else in this suite sees it go.

Asserted over the demo corpus rather than a fixture of its own, because
this is where the notes reach their full length.  A hand-written entry with
no history behind it is drawn as a plain `2:00\='; the same entry with the
estimate correction applied is drawn as `2:00->2:48\=', nearly twice as wide,
and it is the second form that overruns."
  (org-foresight-test--with-demo
    (let ((seen 0))
      (dolist (section (org-foresight-signals))
        (dolist (f (cdr section))
          (when-let ((note (plist-get f :note)))
            (setq seen (1+ seen))
            (should (<= (string-width note) 36)))))
      ;; Or the loop above passes by finding nothing to look at.
      (should (> seen 10)))))

(ert-deftest org-foresight-test-every-signal-declares-its-kind ()
  "No signal group reaches the board without being classified.

The kind decides whether the group is counted in the figure the board is
read for, so a group that never got one is a group silently counted as
work the reader failed to do.  `org-foresight-signal-kind\=' answers `fix\='
for anything unlisted, which is the safe way to be wrong -- over-reported
rather than hidden -- but it is still wrong, and this is what notices."
  (org-foresight-test--with-demo
    (let ((titles (mapcar #'car (org-foresight-signals))))
      (should (> (length titles) 5))
      (dolist (title titles)
        (should (assoc title org-foresight-signal-kinds))))))

(ert-deftest org-foresight-test-the-partition-is-the-one-that-was-decided ()
  "The three kinds, pinned, because nothing else can check them.

Which kind a signal belongs to is a judgement about what its reader can do
about it, and there is no measurement to derive it from.  An accidental
edit to the table would change the one figure the board is read for and
nothing would object, so the decision is written down twice: once where it
is used, once here.

The boundary that took deciding is `Kept moving\='.  It looks settleable --
the entry is right there and its date can be typed over -- and doing that
is precisely what does not settle it, because moving the task again is the
count going up.  It clears when the work is done, dropped or handed on,
which makes it owed rather than fixable.  `Gone quiet\=' sits beside it for
the same reason: the check-in date can be edited, but only chasing the
person makes the new date true.

`Nothing to do next\=' is `fix\=' by the same test read the other way: it is
settled by writing the next step under the project, which is editing the plan
and not doing any of the work."
  (should (equal '((fix . 10) (owed . 3) (fact . 3))
                 (mapcar (lambda (kind)
                           (cons kind
                                 (seq-count (lambda (c) (eq (cdr c) kind))
                                            org-foresight-signal-kinds)))
                         org-foresight--signal-order)))
  (should (eq 'owed (org-foresight-signal-kind "Kept moving (not really NEXT)")))
  (should (eq 'owed (org-foresight-signal-kind "Gone quiet (follow-up overdue)")))
  (should (eq 'fix  (org-foresight-signal-kind "Won't fit today")))
  (should (eq 'fact (org-foresight-signal-kind "Leaking (unclocked work)"))))

(ert-deftest org-foresight-test-signals-are-ordered-by-kind ()
  "What can be settled now is drawn first, then what is owed, then facts.

The order is the argument: a reader who starts at the top is reading the
part with an end to it, and the rule they meet is where that part stops."
  (org-foresight-test--with-demo
    (let* ((ordered (org-foresight--signals-in-order (org-foresight-signals)))
           (kinds (mapcar (lambda (g) (org-foresight-signal-kind (car g)))
                          ordered)))
      ;; Written out rather than sorted by `org-foresight--signal-order\=',
      ;; which is the constant under test: a check that sorts by it agrees
      ;; with any order that constant happens to hold, reversed included.
      (should (equal '(fix owed fact) (seq-uniq kinds)))
      ;; Within a kind the authored order survives, so a group does not move
      ;; about the page between redraws for reasons nobody can see.
      (should (equal (seq-filter (lambda (g)
                                   (eq (org-foresight-signal-kind (car g)) 'fix))
                                 (org-foresight-signals))
                     (seq-filter (lambda (g)
                                   (eq (org-foresight-signal-kind (car g)) 'fix))
                                 ordered))))))

(ert-deftest org-foresight-test-the-board-counts-only-what-can-be-fixed ()
  "The board\='s figure counts the settleable findings and nothing else.

Counting the rest would make the number unreachable, and a target nobody
can reach is one nobody aims at: see `org-foresight-signal-kinds\='."
  (org-foresight-test--with-demo
    (let* ((signals (org-foresight-signals))
           (fix (seq-filter (lambda (g)
                              (eq (org-foresight-signal-kind (car g)) 'fix))
                            signals)))
      (should (< (length fix) (length signals)))
      (should (= (org-foresight-signals-to-fix signals)
                 (apply #'+ (mapcar (lambda (g) (length (cdr g))) fix))))
      ;; Findings, not groups: five estimates missing from one heading are
      ;; five entries somebody has to open.
      (should (> (org-foresight-signals-to-fix signals) (length fix))))))

(ert-deftest org-foresight-test-the-board-says-when-it-is-satisfied ()
  "With nothing left to settle the board says so, rather than going quiet.

A view that prints a figure only when something is wrong cannot be used to
confirm that nothing is: the reader cannot tell it from a view that failed
to run."
  (should (string-match-p
           "nothing to fix"
           (org-foresight-plan--board-verdict nil nil)))
  (should (string-match-p
           "all 2 deadlines land"
           (org-foresight-plan--board-verdict
            (list :deadlines (list (list :verdict 'lands)
                                   (list :verdict 'lands)))
            nil)))
  (should (string-match-p
           "1 of 2 deadlines short"
           (org-foresight-plan--board-verdict
            (list :deadlines (list (list :verdict 'lands)
                                   (list :verdict 'over)))
            nil))))

(ert-deftest org-foresight-test-the-board-badge-holds-no-figures ()
  "A badge names its section; what changes with the data goes in the body.

Every other badge on the page is a fixed phrase, and a reader learns to
skim them as labels rather than read them as content.  One badge whose text
moved with the figures would be the single place that rule broke, and the
figure would be the one thing on the page nobody looked at twice."
  (org-foresight-test--with-demo
    (org-foresight-board)
    (unwind-protect
        (with-current-buffer "*Org Foresight Board*"
          (goto-char (point-min))
          (let ((badge (buffer-substring-no-properties
                        (line-beginning-position) (line-end-position))))
            (should (string-match-p "Board" badge))
            (should-not (string-match-p "[0-9]" badge)))
          ;; And the figure is there, one line down, where the other
          ;; sections put their contents.
          (should (re-search-forward "^ [^ ].*to fix" nil t)))
      (kill-buffer "*Org Foresight Board*"))))

(ert-deftest org-foresight-test-board-rules-reach-the-same-edge ()
  "Every full-width rule on the board ends in the same column.

Two rules a column apart read as a mistake on a page whose whole argument
is that the figures line up, and the eye finds the ragged one before it
finds anything the rules were drawn to say."
  (org-foresight-test--with-demo
    (org-foresight-board)
    (unwind-protect
        (with-current-buffer "*Org Foresight Board*"
          (let (widths)
            (goto-char (point-min))
            (while (re-search-forward "^ ──.*$" nil t)
              (push (string-width (substring-no-properties (match-string 0)))
                    widths))
            (should (> (length widths) 2))
            (should (equal (list org-foresight-report-columns)
                           (seq-uniq widths)))))
      (kill-buffer "*Org Foresight Board*"))))

(ert-deftest org-foresight-test-demo-fires-every-signal ()
  "One of everything: each signal must find its example in the demo corpus."
  (org-foresight-test--with-demo
    (let ((labels (mapcar #'car (org-foresight-signals))))
      (dolist (expected '("Meetings without prep"
                          "Too big for one sitting (needs breaking up)"
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
      ;; Neither a journey nor an edge answers to the agenda's commands.
      ;; A journey used to, through the marker of the meeting it serves --
      ;; and every command that took it acted on a heading whose name was
      ;; not on the row: clocking the drive clocked the meeting, and the
      ;; meeting reported an hour and a half of itself.  There is no entry
      ;; behind a derived journey, and `org-foresight-book-travel' is the
      ;; key that makes one, reading the leg off `org-foresight-journey'.
      (should-not
       (get-text-property
        0 'org-marker
        (seq-find (lambda (r)
                    (string-match-p "→ office" (substring-no-properties r)))
                  rows)))
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
          ;; a day with a big enough gap marks nothing.  Read from the
          ;; property: `equal' compares strings without their properties, so
          ;; a marked row would still come back looking untouched.
          (let ((out (org-foresight-agenda--mark-rows
                      (list item other) roomy cap ledger)))
            (should-not (get-text-property 0 'org-foresight-mark (car out)))
            (should-not (get-text-property 0 'org-foresight-mark (cadr out)))))
      (kill-buffer buf))))

(ert-deftest org-foresight-test-a-reserve-past-the-day-marks-nothing ()
  "A mark on every row is a mark that says nothing.

`keep' reaching zero leaves no gap wide enough for anything, so every piece
of promised work is marked as impossible -- on a day whose calendar is empty.
What is wrong is the reserve, and the verdict is where that gets said.  The
guard for a day with no working hours already knew this much; a reserve grown
past the span arrives at the same place by another road."
  (let* ((cap (list :span-min 480.0 :reserve-min 1020.0 :work t))
         (buf (get-buffer-create "*foresight-swamped*"))
         (m (with-current-buffer buf
              (insert "* one\n") (copy-marker (point-min))))
         (ledger (list (list :kind 'promised :title "one" :effort 30
                             :marker m)))
         (item (org-foresight-test--row " reporting Scheduled: one" m))
         (roomy (list (list :kind 'available
                            :start (org-foresight-test--ts 9 0 10)
                            :end (org-foresight-test--ts 17 0 10)))))
    (unwind-protect
        (progn
          ;; the reserve really has swallowed the span
          (should (= 0.0 (org-foresight-agenda--keep cap)))
          ;; and half an hour of work is still not marked impossible.
          ;; Asked of the text property, not of the row: `equal' compares
          ;; strings without their properties, so a row that had been marked
          ;; would still look untouched.
          (should-not (get-text-property
                       0 'org-foresight-mark
                       (car (org-foresight-agenda--mark-rows
                             (list item) roomy cap ledger)))))
      (kill-buffer buf))))

(ert-deftest org-foresight-test-verdict-blames-the-reserve-not-the-work ()
  "A reserve larger than the day is not an over-committed day.

`OVER by' names the work as the thing to cut, and says it on a day with
nothing on it at all.  What wants looking at is the figure that swallowed the
span -- and it has to be said in one of the three terms the line may never
trim, because the reserve figure further along is the first thing dropped."
  (let* ((org-foresight-report-columns 80)
         (cap (list :span-min 480.0 :ahead-min 480.0
                    :committed-min 0.0 :headroom-min -540.0
                    :reserve-min 1020.0 :reserve-day-min 1020.0
                    :bias-min 0.0 :lands nil :work t))
         (line (substring-no-properties (org-foresight-report--verdict cap))))
    (should (string-match-p "reserve 17:00 exceeds the day" line))
    (should-not (string-match-p "OVER by" line))
    ;; and a reserve that still fits inside the day is reported as before
    (let* ((ok (plist-put (copy-sequence cap) :reserve-min 60.0))
           (line (substring-no-properties (org-foresight-report--verdict ok))))
      (should (string-match-p "OVER by" line)))))

(ert-deftest org-foresight-test-diagnose-names-a-reserve-past-the-day ()
  "The command that explains the figures has to explain this one.

Before, it pushed the opposite advice -- run `org-foresight-learn-leak' --
and then had nothing to say once that had been run and had come back with a
budget larger than the day it is subtracted from."
  (let ((cache (make-temp-file "org-foresight-leak" nil ".eld"
                               (prin1-to-string '(:leak 360.0 :lost 600.0
                                                  :samples 12)))))
    (unwind-protect
        (let ((org-foresight-leak-cache-file cache)
              (org-foresight-surge-cache-file "/nonexistent/surge.eld")
              (org-foresight-surge-default "1:00")
              (org-foresight-work '(("09:00" . "17:00")))
              (org-foresight-workdays '(0 1 2 3 4 5 6))
              (org-foresight--shape-cache nil))
          (should (seq-find
                   (lambda (l) (string-match-p "larger than the working day" l))
                   (org-foresight--diagnose-advice (org-foresight--day-start 0)))))
      (delete-file cache))))

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

(ert-deftest org-foresight-test-today-tasks-carry-their-intervals ()
  "A task keeps the segments it was clocked over, not only their sum.

Minutes cannot be intersected with anything.  Asking how much of the elapsed
working day a task accounts for means cutting it against the working hours,
and a total has already thrown away what that cut needs.

The union of the per-task segments must also equal the day's own list, or the
arithmetic built on top of the two can double count."
  (org-foresight-test--with-clocked
      "* NEXT Draft the summary
:PROPERTIES:
:CATEGORY: reporting
:END:
:LOGBOOK:
CLOCK: [@ 09:00]--[@ 10:00] =>  1:00
CLOCK: [@ 13:00]--[@ 13:40] =>  0:40
:END:
* NEXT Something else
:PROPERTIES:
:CATEGORY: admin
:END:
:LOGBOOK:
CLOCK: [@ 14:00]--[@ 14:20] =>  0:20
:END:
"
    (let* ((clock (org-foresight-clock-scan 7))
           (tasks (plist-get clock :today-tasks))
           (first (car tasks)))
      ;; two CLOCK lines, one task, both segments kept
      (should (= 2 (length (plist-get first :intervals))))
      (should (< (abs (- (* 60 (plist-get first :minutes))
                         (org-foresight--intervals-seconds
                          (plist-get first :intervals))))
                 0.001))
      ;; and the parts cover the whole
      (let ((union (org-foresight--intervals-normalize
                    (apply #'append (mapcar (lambda (tk) (plist-get tk :intervals))
                                            tasks)))))
        (should (< (abs (- (org-foresight--intervals-seconds union)
                           (org-foresight--intervals-seconds
                            (plist-get clock :today-intervals))))
                   0.001))))))

(ert-deftest org-foresight-test-today-tasks-know-what-arrived ()
  "A clocked task says whether it was planned or landed on the day.

Read while point is already on the heading, during the walk that reads the
clock -- asking afterwards would mean opening every one of them again."
  (org-foresight-test--with-clocked
      (concat
       "* ONGO Interruption\n:PROPERTIES:\n:CATEGORY: admin\n:SURGE: ["
       (format-time-string "%Y-%m-%d %a 14:00") "]\n:END:\n"
       ":LOGBOOK:\nCLOCK: [@ 14:00]--[@ 14:40] =>  0:40\n:END:\n"
       "* NEXT Planned work\n:PROPERTIES:\n:CATEGORY: admin\n:END:\n"
       ":LOGBOOK:\nCLOCK: [@ 09:00]--[@ 10:00] =>  1:00\n:END:\n")
    (let ((tasks (plist-get (org-foresight-clock-scan 7) :today-tasks)))
      (should (= 2 (length tasks)))
      (let ((arrived (seq-find (lambda (tk) (plist-get tk :surge)) tasks))
            (planned (seq-find (lambda (tk) (not (plist-get tk :surge))) tasks)))
        (should arrived)
        (should planned)
        (should (equal "Interruption" (plist-get arrived :title)))
        (should (equal "Planned work" (plist-get planned :title)))))))

(ert-deftest org-foresight-test-a-scan-keeps-every-answer-it-gave-before ()
  "The survey's existing keys say what they have always said.

Written before anything is added to it.  Capacity, the day's report and the
watcher all read this one plist, so a key that quietly changed shape would go
wrong in three places at once and look like three faults."
  (org-foresight-test--with-clocked
      "* NEXT Draft the summary
:PROPERTIES:
:CATEGORY: reporting
:END:
:LOGBOOK:
CLOCK: [@ 09:00]--[@ 10:00] =>  1:00
CLOCK: [@ 13:00]--[@ 13:40] =>  0:40
:END:
* NEXT Something else
:PROPERTIES:
:CATEGORY: admin
:END:
:LOGBOOK:
CLOCK: [@ 14:00]--[@ 14:20] =>  0:20
:END:
"
    (let ((clock (org-foresight-clock-scan 7)))
      (should (equal '(("reporting" . 100.0) ("admin" . 20.0))
                     (plist-get clock :rows)))
      (should (= 120.0 (plist-get clock :total)))
      (should (= 7 (plist-get clock :days)))
      (should (vectorp (plist-get clock :byday)))
      (should (= 7 (length (plist-get clock :byday))))
      ;; today is the newest day, and the whole of it is today's
      (should (= 120.0 (aref (plist-get clock :byday) 6)))
      (should (equal '(("reporting" . 100.0) ("admin" . 20.0))
                     (plist-get clock :today-rows)))
      (should (= 120.0 (plist-get clock :today-total)))
      (should (= 3 (plist-get clock :today-segments)))
      (should (= 3 (length (plist-get clock :today-intervals))))
      (should (= 2 (length (plist-get clock :today-tasks))))
      (should (vectorp (plist-get clock :intervals-byday)))
      (should (= 3 (length (aref (plist-get clock :intervals-byday) 6)))))))

(ert-deftest org-foresight-test-clock-scan-takes-a-now ()
  "A running clock is closed at NOW, so a test of one is reproducible."
  (org-foresight-test--with-clocked
      "* NEXT Still going
:LOGBOOK:
CLOCK: [@ 09:00]
:END:
"
    ;; Today at 11:30.  `org-foresight-test--ts' answers on a fixed day in
    ;; August, which is the right default for the pure-model tests and the
    ;; wrong one here: `--with-clocked' rewrites the drawer to *today*.
    (let ((clock (org-foresight-clock-scan
                  7 (time-add (org-foresight--day-start 0)
                              (seconds-to-time (* 60 (+ (* 60 11) 30)))))))
      (should (< (abs (- 150 (plist-get clock :today-total))) 0.001)))))

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

(ert-deftest org-foresight-test-a-blank-segment-is-what-the-box-draws ()
  "Only the blank segments are boxed, and no two of them share a face.

A blank cell with no box is nothing at all: the box is the only thing drawing
it.  Ink and dots draw themselves, and bounding them would be bounding what
needs no bounding.

No two segments of one bar may share a face, because Emacs draws one box per
run of identical face -- two blank segments sharing one are bounded together
and the edge between them disappears.  That is not hypothetical: `unclocked'
wore the reserve's own face, and the elapsed bar drew it and surge inside a
single box."
  (dolist (segments (list org-foresight-report--bar-segments
                          org-foresight-report--behind-segments
                          org-foresight-report--off-segments
                          org-foresight-report--off-behind-segments))
    (dolist (seg segments)
      (let ((face (org-foresight-report--glyph-face seg)))
        ;; boxed exactly when the segment asked to be
        (should (eq (and (plist-get seg :box) t)
                    (equal '(:box (:line-width -1))
                           (and (consp face) (car face)))))
        ;; Boxed exactly when blank, both ways round.  A blank segment that
        ;; did not ask is not drawn at all; a filled one that did is drawn a
        ;; box in its own colour around a cell of that colour, which is a
        ;; mark that marks nothing -- `surge' carried one for a while, and
        ;; the only way to notice was to go looking for it on screen.
        (should (eq (and (plist-get seg :box) t)
                    (eq (plist-get seg :glyph) ?\s)))))
    (let ((faces (mapcar (lambda (s) (plist-get s :face)) segments)))
      (should (= (length faces) (length (delete-dups (copy-sequence faces))))))))

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
      (format "* NEXT lunchtime call
%s
* NEXT during the morning
%s
"
              (org-foresight-test--stamp (org-foresight-test--offset-to 2) "12:15" "12:45")
              (org-foresight-test--stamp (org-foresight-test--offset-to 2) "10:00" "11:00"))
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
     ;; Pinned, and not to the wall clock.  These tests write timestamps at
     ;; named hours -- a meeting at three, an errand at four -- and every
     ;; figure the package draws is measured from NOW.  Left to the real
     ;; time, such a test passes all morning and fails from three o'clock:
     ;; the meeting has started, the journey to it is behind us, the gap it
     ;; sat in has gone.  A suite that fails in the afternoon is a suite
     ;; nobody trusts in the afternoon.
     ;;
     ;; Org's own `now' rule goes with it, for the same reason
     ;; `org-foresight-demo-mode' turns it off: with an hour pinned there
     ;; would otherwise be two present moments on one page, one of them the
     ;; wall clock's.
     (let ((org-foresight-now (time-add (org-foresight--day-start 0) (* 3600 8)))
           (org-agenda-show-current-time-in-grid nil)
           (org-agenda-sticky nil)
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
           ;; Early, and fixed.  What a gap can still hold is measured from
           ;; now, so a suite that asked the clock would offer the morning's
           ;; candidates before lunch and none of them after it.
           (org-foresight-agenda--now
            (time-add (org-foresight--day-start 0) (seconds-to-time (* 60 60 7))))
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

(ert-deftest org-foresight-test-e2e-a-gap-only-offers-what-it-can-hold ()
  "Work that named a place is offered in the hours spent there, and nowhere else.

An hour free at home is no use at all to something that can only be done at
the office, and offering it anyway is worse than saying nothing: the row reads
as an answer and the hour goes on finding out that it was not one.

Both halves matter, and one day shows both.  Two meetings at the office keep
you there between them, so the middle of the day is free *and* in the right
place; the morning before you set off and the evening after you get back are
free and in the wrong one."
  (org-foresight-test--with-agenda
      (concat "* 朝の打合せ\n:PROPERTIES:\n:CATEGORY: meeting\n"
              ":LOCATION: 本社 会議室3\n:END:\n"
              (org-foresight-test--stamp 0 "10:00" "11:00") "\n"
              "* 午後の打合せ\n:PROPERTIES:\n:CATEGORY: meeting\n"
              ":LOCATION: 本社 会議室3\n:END:\n"
              (org-foresight-test--stamp 0 "14:00" "15:00") "\n"
              "* NEXT only at the office\nSCHEDULED: "
              (org-foresight-test--stamp 0)
              "\n:PROPERTIES:\n:EFFORT: 1:00\n:PLACE: office\n:END:\n"
              "* NEXT anywhere at all\nSCHEDULED: " (org-foresight-test--stamp 0)
              "\n:PROPERTIES:\n:EFFORT: 0:30\n:END:\n")
    (let* ((org-foresight-work '(("09:00" . "17:30")))
           (org-foresight-places '((office . "本社\\|会議室\\|オフィス")))
           (org-foresight-home-place 'home)
           (org-foresight-travel-matrix '(((home . office) . 60)))
           (org-foresight--shape-cache nil)
           (rows (seq-filter (lambda (l) (string-match-p "↳ NEXT" l))
                             (org-foresight-test--agenda)))
           (at (lambda (hhmm)
                 (seq-filter (lambda (l) (string-search hhmm l)) rows))))
      ;; The office hours hold both ...
      (should (seq-find (lambda (l) (string-match-p "only at the office" l))
                        (funcall at "11:00")))
      (should (seq-find (lambda (l) (string-match-p "anywhere at all" l))
                        (funcall at "11:00")))
      ;; ... and the hours at home hold only the one that can be done there.
      (should (funcall at "16:00"))
      (should-not (seq-find (lambda (l) (string-match-p "only at the office" l))
                            (funcall at "16:00")))
      (should (seq-find (lambda (l) (string-match-p "anywhere at all" l))
                        (funcall at "16:00"))))))

(ert-deftest org-foresight-test-e2e-an-hour-claimed-twice-is-marked ()
  "Two things booked over each other is a day that cannot happen, and it says so.

The bands trim the later of the pair to keep the day a partition, and that
trimming is exactly what made the collision invisible: Org draws the two rows
one after the other, and nothing on the page says they are the same hour.

Both are marked, because it takes two.  A commitment that shares its hour
without competing for it is not one of them."
  (org-foresight-test--with-agenda
      (concat "* the school run\n:PROPERTIES:\n:CATEGORY: family\n:END:\n"
              (org-foresight-test--stamp 0 "08:00" "09:30") "\n"
              "* standup\n:PROPERTIES:\n:CATEGORY: meeting\n:END:\n"
              (org-foresight-test--stamp 0 "09:00" "09:45") "\n"
              "* somebody else's fixture\n:PROPERTIES:\n:CATEGORY: meeting\n"
              ":ATTENTION: informational\n:END:\n"
              (org-foresight-test--stamp 0 "09:15" "10:00") "\n"
              "* an hour of its own\n:PROPERTIES:\n:CATEGORY: meeting\n:END:\n"
              (org-foresight-test--stamp 0 "11:00" "12:00") "\n")
    (let* ((lines (org-foresight-test--agenda))
           (marked (lambda (re)
                     (seq-find (lambda (l)
                                 (and (string-match-p re l)
                                      (string-search
                                       org-foresight-agenda-wont-fit l)))
                               lines))))
      ;; both sides of the collision, private or not
      (should (funcall marked "school run"))
      (should (funcall marked "standup"))
      ;; what shares its hour is not competing for it
      (should-not (funcall marked "fixture"))
      ;; and an hour nothing else wants is left alone
      (should-not (funcall marked "an hour of its own")))))

(ert-deftest org-foresight-test-e2e-a-journey-can-be-written-down ()
  "A derived leg can be made real, and once it is, nothing derives it again.

The derivation is a claim about the day, and a good one until the day
disagrees -- the train you actually catch cannot be guessed from where a
meeting happens to be.  Writing it down has to end the argument rather than
add to it: the entry is the journey, and a second one to the same place is
the same hour drawn twice."
  (org-foresight-test--with-agenda
      (concat "* office meeting\n:PROPERTIES:\n:CATEGORY: meeting\n"
              ":LOCATION: \u672c\u793e\n:END:\n"
              (org-foresight-test--stamp 0 "13:00" "14:00") "\n")
    (let* ((org-foresight-work '(("09:00" . "17:30")))
           (org-foresight-places '((office . "\u672c\u793e")))
           (org-foresight-home-place 'home)
           (org-foresight-travel-matrix '(((home . office) . 60)))
           (org-foresight-task-file (car org-agenda-files))
           (org-foresight-task-datetree nil)
           (org-foresight--shape-cache nil)
           (org-foresight--signals-cache nil))
      ;; the derived leg is there, and carries where it goes and when
      (org-foresight-test--agenda)
      (let ((journey
             (with-current-buffer org-agenda-buffer-name
               (goto-char (point-min))
               (catch 'found
                 (while (not (eobp))
                   (when-let ((j (org-get-at-bol 'org-foresight-journey)))
                     (throw 'found j))
                   (forward-line 1))
                 nil))))
        (should journey)
        (should (eq 'office (car journey)))
        ;; write it down, an hour earlier than the derivation chose
        (org-foresight--file-journey
         "\u2192 office" 'office
         (time-subtract (nth 1 journey) 3600)
         (time-subtract (nth 2 journey) 3600))
        (setq org-foresight--shape-cache nil)
        (let ((lines (org-foresight-test--agenda)))
          ;; the written one is drawn once, by Org, at the hour it says
          (should (= 1 (seq-count (lambda (l) (string-search "\u2192 office" l))
                                  lines)))
          (should (seq-find (lambda (l)
                              (and (string-search "\u2192 office" l)
                                   (string-search "11:00" l)))
                            lines))
          ;; and the way home is still derived, since nobody wrote that down
          (should (seq-find (lambda (l) (string-search "\u2192 home" l))
                            lines)))))))

(ert-deftest org-foresight-test-e2e-you-do-not-wait-where-you-cannot-work ()
  "A place nothing can be done in is left as soon as what took you there ends.

A journey normally arrives just in time, which is right everywhere work can
happen and wrong at the gym: the hours between the last press-up and setting
off for the office were spent in a changing room, and were offered as though
they could be worked.  Naming the place unworkable moves the departure to the
front of them, and the same free minutes land somewhere they are worth
something."
  (let ((body (concat "* \u30b8\u30e0\n:PROPERTIES:\n:CATEGORY: personal\n"
                      ":LOCATION: GYM\n:END:\n"
                      (org-foresight-test--stamp 0 "09:30" "10:30") "\n"
                      "* office meeting\n:PROPERTIES:\n:CATEGORY: meeting\n"
                      ":LOCATION: \u672c\u793e\n:END:\n"
                      (org-foresight-test--stamp 0 "15:00" "16:00") "\n"
                      "* NEXT ordinary work\nSCHEDULED: "
                      (org-foresight-test--stamp 0)
                      "\n:PROPERTIES:\n:EFFORT: 1:00\n:END:\n")))
    (cl-flet ((lines-with
                (unworkable re)
                (org-foresight-test--with-agenda body
                  (let ((org-foresight-work '(("09:00" . "17:30")))
                        (org-foresight-places
                         '((office . "\u672c\u793e") (gym . "GYM")))
                        (org-foresight-home-place 'home)
                        (org-foresight-unworkable-places unworkable)
                        (org-foresight-travel-matrix
                         '(((home . gym) . 20) ((gym . office) . 40)
                           ((home . office) . 60)))
                        (org-foresight--shape-cache nil))
                    (seq-filter (lambda (l) (string-match-p re l))
                                (org-foresight-test--agenda))))))
      ;; left as it is, the journey waits until the meeting needs it ...
      (should (seq-find (lambda (l) (string-search "14:20" l))
                        (lines-with nil "\u2192 office")))
      ;; ... and the hours at the gym are offered for work
      (should (seq-find (lambda (l) (string-search "10:30" l))
                        (lines-with nil "\u21b3 NEXT")))
      ;; named unworkable, you set off the moment the gym ends ...
      (should (seq-find (lambda (l) (string-search "10:30" l))
                        (lines-with '(gym) "\u2192 office")))
      ;; ... and what is offered is offered after you have arrived
      (should (seq-find (lambda (l) (string-search "11:10" l))
                        (lines-with '(gym) "\u21b3 NEXT")))
      (should-not (seq-find (lambda (l) (string-search "10:30" l))
                            (lines-with '(gym) "\u21b3 NEXT"))))))

(defun org-foresight-test--behind-fixture (clock-specs surge-titles afk-specs)
  "Build the (CLOCK COVERAGE) pair `org-foresight-behind' takes.
CLOCK-SPECS and AFK-SPECS are `org-foresight-test--ivs' specs; SURGE-TITLES
names which clock segments count as work that arrived."
  (let* ((ivs (apply #'org-foresight-test--ivs clock-specs))
         (tasks (seq-map-indexed
                 (lambda (iv i)
                   (list :title (format "task %d" i)
                         :minutes (/ (float-time (time-subtract (cdr iv) (car iv))) 60.0)
                         :surge (and (memq i surge-titles) t)
                         :intervals (list iv)))
                 ivs)))
    (list (list :today-intervals (org-foresight--intervals-normalize
                                  (copy-sequence ivs))
                :today-tasks tasks)
          (and afk-specs
               (list :afk-ivs (apply #'org-foresight-test--ivs afk-specs))))))

(ert-deftest org-foresight-test-behind-partitions-the-elapsed-span ()
  "The four measured segments divide the elapsed working day exactly.

This is the invariant the whole two-row bar rests on.  If the four do not sum
to the elapsed span, `Behind' is drawn at the wrong length, the join stops
falling on now, and the picture quietly lies about how much of the day is
gone -- which is the very thing it was built to stop doing.

Swept across the day rather than sampled at three convenient hours, because
the failures live at the boundaries: exactly at a work start, exactly at a
clock end, and inside the declared break."
  (let* ((org-foresight-work '(("09:00" . "12:00") ("13:00" . "17:30")))
         (org-foresight-workdays '(0 1 2 3 4 5 6))
         (org-foresight--shape-cache nil)
         (day (org-foresight-test--ts 0 0))
         (fix (org-foresight-test--behind-fixture
               ;; one before work, one over the break, one ordinary, one arrived
               '((7 30 8 30) (11 30 13 30) (14 0 15 0) (15 30 16 0))
               '(3)
               '((10 0 10 20) (16 30 17 0)))))
    (dotimes (i 29)
      (let* ((mins (+ (* 60 7) (* 30 i)))
             (now (time-add day (seconds-to-time (* 60 mins))))
             (b (org-foresight-behind day (nth 0 fix) (nth 1 fix) now))
             (parts (+ (plist-get b :baseline-min) (plist-get b :surge-min)
                       (plist-get b :unclocked-min) (plist-get b :away-min))))
        (should (< (abs (- (plist-get b :behind-min) parts)) 0.001))
        ;; and no segment may go negative on the way
        (dolist (k '(:baseline-min :surge-min :unclocked-min :away-min))
          (should (>= (plist-get b k) 0)))))))

(ert-deftest org-foresight-test-behind-keeps-the-clock-out-of-the-span ()
  "Clock outside the working hours is named, not silently folded in.

A clock at half past seven and one through the declared lunch break are real
time really spent, and neither is part of the span the four segments divide.
Dropping them without a word leaves the `Spent' block's total disagreeing
with the bar and no way to find out why."
  (let* ((org-foresight-work '(("09:00" . "12:00") ("13:00" . "17:30")))
         (org-foresight-workdays '(0 1 2 3 4 5 6))
         (org-foresight--shape-cache nil)
         (day (org-foresight-test--ts 0 0))
         (fix (org-foresight-test--behind-fixture
               '((7 30 8 30) (12 15 12 45)) nil nil))
         (b (org-foresight-behind day (nth 0 fix) (nth 1 fix)
                                  (org-foresight-test--ts 17 30))))
    ;; an hour before work and half an hour in the break
    (should (< (abs (- 90 (plist-get b :outside-min))) 0.001))
    ;; none of it inside
    (should (< (plist-get b :baseline-min) 0.001))
    (should (< (abs (- (plist-get b :behind-min)
                       (plist-get b :unclocked-min)))
               0.001))))

(ert-deftest org-foresight-test-behind-without-a-watcher ()
  "No ActivityWatch means one unrecorded segment, not a wrong one.

The collapse has to happen through the arithmetic rather than through a
branch: `unclocked' is the complement of afk, so an empty afk list leaves it
the whole of the unaccounted time with nothing to decide."
  (let* ((org-foresight-work '(("09:00" . "17:30")))
         (org-foresight-workdays '(0 1 2 3 4 5 6))
         (org-foresight--shape-cache nil)
         (day (org-foresight-test--ts 0 0))
         (fix (org-foresight-test--behind-fixture '((9 0 10 0)) nil nil))
         (b (org-foresight-behind day (nth 0 fix) (nth 1 fix)
                                  (org-foresight-test--ts 12 0))))
    (should-not (plist-get b :measured))
    (should (< (plist-get b :away-min) 0.001))
    (should (< (abs (- 60 (plist-get b :baseline-min))) 0.001))
    (should (< (abs (- 120 (plist-get b :unclocked-min))) 0.001))
    (should (< (abs (- (plist-get b :behind-min)
                       (+ (plist-get b :baseline-min)
                          (plist-get b :unclocked-min))))
               0.001))))

(ert-deftest org-foresight-test-behind-gives-a-shared-minute-to-planned ()
  "Where planned and arrived work overlap, the reserve is not charged.

Two clocks can cover the same minute -- a hand-edited drawer, two files.  The
conservative reading is the one that does not spend the interruption budget on
a minute that had planned work in it too, and it is order-independent, which
\"whichever task was seen first\" is not."
  (let* ((org-foresight-work '(("09:00" . "17:30")))
         (org-foresight-workdays '(0 1 2 3 4 5 6))
         (org-foresight--shape-cache nil)
         (day (org-foresight-test--ts 0 0))
         (fix (org-foresight-test--behind-fixture
               '((9 0 10 0) (9 45 10 15)) '(1) nil))
         (b (org-foresight-behind day (nth 0 fix) (nth 1 fix)
                                  (org-foresight-test--ts 12 0))))
    ;; the shared quarter hour goes to planned, so surge is only 10:00-10:15
    (should (< (abs (- 15 (plist-get b :surge-min))) 0.001))
    (should (< (abs (- 60 (plist-get b :baseline-min))) 0.001))
    (should (< (abs (- (plist-get b :behind-min)
                       (+ (plist-get b :baseline-min) (plist-get b :surge-min)
                          (plist-get b :unclocked-min) (plist-get b :away-min))))
               0.001))))

(ert-deftest org-foresight-test-behind-without-a-clock-says-so ()
  "No clock information means the whole elapsed day is unrecorded, not absent."
  (let* ((org-foresight-work '(("09:00" . "17:30")))
         (org-foresight-workdays '(0 1 2 3 4 5 6))
         (org-foresight--shape-cache nil)
         (day (org-foresight-test--ts 0 0))
         (b (org-foresight-behind day nil nil (org-foresight-test--ts 11 0))))
    (should b)
    (should (< (abs (- 120 (plist-get b :behind-min))) 0.001))
    (should (< (abs (- 120 (plist-get b :unclocked-min))) 0.001))))

(ert-deftest org-foresight-test-the-gaps-offered-are-the-gaps-measured ()
  "What the command offers to fill is exactly what the bar drew as unrecorded.

Let the two come apart and filling every hole offered would still leave the
bar reporting time nobody can account for, with nothing inside the tool able
to say why."
  (let* ((org-foresight-work '(("09:00" . "17:30")))
         (org-foresight-workdays '(0 1 2 3 4 5 6))
         (org-foresight--shape-cache nil)
         (day (org-foresight-test--ts 0 0 10))
         (now (org-foresight-test--ts 14 0 10))
         (pair (org-foresight-test--behind-fixture
                '((9 15 10 30 10)) nil '((12 0 13 0 10))))
         (behind (org-foresight-behind day (car pair) (cadr pair) now))
         (gaps (org-foresight--clock-gaps behind)))
    ;; 09:00-09:15 and 10:30-12:00 and 13:00-14:00 unrecorded, 12:00-13:00 away
    (should (equal '("09:00-09:15" "10:30-12:00" "12:00-13:00" "13:00-14:00")
                   (mapcar (lambda (g) (car (org-foresight-test--hhmm
                                             (list (car g)))))
                           gaps)))
    (should (equal '(unclocked unclocked away unclocked) (mapcar #'cdr gaps)))
    ;; and together they are the whole of what the bar called unaccounted for
    (should (= (+ (plist-get behind :unclocked-min) (plist-get behind :away-min))
               (/ (org-foresight--intervals-seconds
                   (org-foresight--intervals-normalize
                    (mapcar #'car gaps)))
                  60.0)))
    ;; the quarter of an hour goes when the threshold rises past it
    (let ((org-foresight-clock-fill-minimum 20))
      (should (equal '("10:30-12:00" "12:00-13:00" "13:00-14:00")
                     (mapcar (lambda (g) (car (org-foresight-test--hhmm
                                               (list (car g)))))
                             (org-foresight--clock-gaps behind)))))))

(defmacro org-foresight-test--with-task-file (text &rest body)
  "Run BODY with TEXT written to a real file bound as the task file."
  (declare (indent 1))
  `(let ((file (make-temp-file "org-foresight-clock" nil ".org" ,text)))
     (unwind-protect
         (let ((org-foresight-task-file file)
               (org-foresight-task-datetree nil)
               (org-agenda-files (list file))
               (org-todo-keywords '((sequence "NEXT" "ONGO" "|" "DONE"))))
           ,@body)
       (when (get-file-buffer file) (kill-buffer (get-file-buffer file)))
       (delete-file file))))

(defun org-foresight-test--task-file-text ()
  "Return the task file's contents."
  (with-current-buffer (find-file-noselect org-foresight-task-file)
    (substring-no-properties (buffer-string))))

(ert-deftest org-foresight-test-a-filled-clock-lands-on-the-entry ()
  "The line goes into the entry's own drawer, closed and totalled.

Totalled by Org rather than here: the `=> H:MM' is what every clock report
adds up, and a second piece of arithmetic for it could only ever disagree."
  (org-foresight-test--with-task-file "* ONGO the work\n"
    (let ((marker (with-current-buffer (find-file-noselect org-foresight-task-file)
                    (goto-char (point-min))
                    (point-marker))))
      (org-foresight--file-clocked marker
                                   (org-foresight-test--ts 14 20 10)
                                   (org-foresight-test--ts 14 50 10))
      (let ((text (org-foresight-test--task-file-text)))
        (should (string-match-p ":LOGBOOK:" text))
        (should (string-match-p
                 "CLOCK: \\[2026-08-10 [^]]*14:20\\]--\\[2026-08-10 [^]]*14:50\\] +=> +0:30"
                 text))))))

(ert-deftest org-foresight-test-work-nobody-wrote-down-gets-an-entry ()
  "A stretch spent on something not in any file becomes something in a file.

With no keyword: what is written down already happened, and a keyword would
put it back among the things still to do -- the day would carry the hour
twice, once as spent and once as owed.  The round trip is asserted rather
than the text, because an entry the clock survey cannot see afterwards has
recorded nothing."
  (org-foresight-test--with-task-file ""
    (let* ((from (time-add (org-foresight--day-start 0) (* 3600 14)))
           (to (time-add from (* 60 30))))
      (org-foresight--file-clocked-entry "a call from procurement" from to t)
      (let ((text (org-foresight-test--task-file-text)))
        (should (string-match-p "^\\* a call from procurement$" text))
        (should (string-match-p ":SURGE:" text)))
      ;; and the survey finds it, with its half hour and its arrived-ness
      (let* ((clock (org-foresight-clock-scan 1))
             (task (seq-find (lambda (task)
                               (equal (plist-get task :title)
                                      "a call from procurement"))
                             (plist-get clock :today-tasks))))
        (should task)
        (should (= 30.0 (plist-get task :minutes)))
        (should (plist-get task :surge))))))

(defun org-foresight-test--split-fixture ()
  "Two clocked spells today: alpha 09:00-11:00 and beta 13:00-13:30."
  (let ((day (org-foresight--day-start 0)))
    (cl-flet ((at (h m) (format-time-string
                         (format "[%%Y-%%m-%%d %%a %02d:%02d]" h m) day)))
      (concat "* ONGO alpha\n:LOGBOOK:\nCLOCK: "
              (at 9 0) "--" (at 11 0) " =>  2:00\n:END:\n"
              "* ONGO beta\n:LOGBOOK:\nCLOCK: "
              (at 13 0) "--" (at 13 30) " =>  0:30\n:END:\n"))))

(defmacro org-foresight-test--divide (part &rest body)
  "Divide alpha's spell at 10:00, moving PART to beta, then run BODY.
PART is 0 for the earlier half and 1 for the later one."
  (declare (indent 1))
  `(org-foresight-test--with-task-file (org-foresight-test--split-fixture)
     (let ((org-foresight-work '(("09:00" . "17:30")))
           (org-foresight-workdays '(0 1 2 3 4 5 6))
           (org-foresight-clock-fill-kinds nil)
           (org-foresight--shape-cache nil)
           (org-foresight-now (time-add (org-foresight--day-start 0) (* 3600 15))))
       (cl-letf (((symbol-function 'org-foresight-observe--get-json)
                  (lambda (&rest _) nil))
                 ((symbol-function 'read-string) (lambda (&rest _) "10:00"))
                 ((symbol-function 'completing-read)
                  (lambda (prompt collection &rest _)
                    (cond
                     ((string-prefix-p "Divide" prompt)
                      (car (seq-find (lambda (c) (string-match-p "alpha" (car c)))
                                     collection)))
                     ((string-prefix-p "Which part" prompt)
                      (car (nth ,part collection)))
                     (t "beta")))))
         (org-foresight-clock-split))
       ,@body)))

(ert-deftest org-foresight-test-dividing-a-spell-moves-time-and-makes-none ()
  "The minutes change hands; the day is exactly as long as it was.

Which is the whole of the arithmetic.  A division that shortened nothing
would charge the same hour to two tasks and report a day that never happened;
one that wrote nothing would lose the hour outright.  Both are worse than the
mistake being corrected, because both are invisible in a total."
  (let (before after)
    (org-foresight-test--divide 1
      (setq after (org-foresight-test--task-file-text))
      (setq before (plist-get (org-foresight-clock-scan 1) :today-total)))
    ;; alpha keeps the first half, beta gains the second, and beta's own
    ;; spell is untouched
    (should (string-match-p "09:00\\]--\\[[^]]*10:00\\]" after))
    (should (string-match-p "10:00\\]--\\[[^]]*11:00\\]" after))
    (should (string-match-p "13:00\\]--\\[[^]]*13:30\\]" after))
    (should-not (string-match-p "09:00\\]--\\[[^]]*11:00\\]" after))
    (should (= 3 (org-foresight-test--count "CLOCK: " after)))
    ;; 2:00 + 0:30 before, and after
    (should (= 150 before))))

(ert-deftest org-foresight-test-the-half-that-moves-is-the-half-that-was-named ()
  "Either end can be the one that was really something else.

The interruption that arrives mid-task is the common case and the second half
is the default, but the meeting that overran into the hour booked after it
puts the misfiled time at the front.  A command that could only move the tail
would send half of those to the wrong task by construction."
  (let (text)
    (org-foresight-test--divide 0
      (setq text (org-foresight-test--task-file-text)))
    ;; alpha keeps the later half this time
    (should (string-match-p "\\* ONGO alpha\n:LOGBOOK:\nCLOCK: \\[[^]]*10:00\\]--\\[[^]]*11:00\\]"
                            text))
    (should (string-match-p "09:00\\]--\\[[^]]*10:00\\]" text))
    (should (= 3 (org-foresight-test--count "CLOCK: " text)))))

(ert-deftest org-foresight-test-clock-fill-asks-for-nothing-but-the-name ()
  "Choose a stretch, name the work: no hour is ever typed.

The whole point of the command.  Typing times by hand is a tax on an act
that is already an afterthought, and a tax on an afterthought collects
nothing -- which is why the holes were still there at six o'clock."
  (org-foresight-test--with-task-file
      (concat "* ONGO the work\n:LOGBOOK:\nCLOCK: "
              (format-time-string "[%Y-%m-%d %a 09:15]"
                                  (org-foresight--day-start 0))
              "--"
              (format-time-string "[%Y-%m-%d %a 10:30]"
                                  (org-foresight--day-start 0))
              " =>  1:15\n:END:\n")
    (let ((org-foresight-work '(("09:00" . "17:30")))
          (org-foresight-workdays '(0 1 2 3 4 5 6))
          (org-foresight--shape-cache nil)
          (org-foresight-now (time-add (org-foresight--day-start 0) (* 3600 14)))
          (asked nil))
      (cl-letf (((symbol-function 'org-foresight-observe--get-json)
                 (lambda (&rest _) nil))
                ((symbol-function 'completing-read)
                 (lambda (prompt collection &rest _)
                   (push prompt asked)
                   (if (string-prefix-p "Unrecorded" prompt)
                       ;; the second hole, 10:30 onwards
                       (car (nth 1 collection))
                     "the work"))))
        (org-foresight-clock-fill))
      ;; nothing was asked but which stretch and what it was
      (should (= 2 (length asked)))
      (should (seq-every-p (lambda (p) (not (string-match-p "[0-9]:[0-9]" p)))
                           asked))
      ;; and the hole is now on the entry that was already there
      (let ((text (org-foresight-test--task-file-text)))
        (should (= 2 (org-foresight-test--count "CLOCK: " text)))
        (should (string-match-p "10:30\\]--\\[[^]]*14:00\\]" text))))))

(ert-deftest org-foresight-test-a-clock-outside-the-hours-is-named ()
  "A clock that ran through the lunch break is reported, not swallowed.

It is not one of the four -- it did not happen inside the span they divide --
so it cannot go in the bar without breaking the sum.  Leaving it unsaid is
worse: `[Spent]' would report a `Clocked' total larger than everything the
bar accounts for, with nothing on the page able to explain the difference,
and a reader who cannot explain it has to doubt both figures.  With a
declared lunch break this happens on ordinary days, not on strange ones."
  (let* ((org-foresight-work '(("09:00" . "12:00") ("13:00" . "17:30")))
         (org-foresight-workdays '(0 1 2 3 4 5 6))
         (org-foresight--shape-cache nil)
         (day (org-foresight-test--ts 0 0 10))
         (now (org-foresight-test--ts 15 0 10))
         ;; worked straight through the hour the day declared it would stop
         (pair (org-foresight-test--behind-fixture '((11 30 13 30 10)) nil nil))
         (behind (org-foresight-behind day (car pair) (cadr pair) now)))
    ;; the half hour either side counts, the hour between does not
    (should (= 60.0 (plist-get behind :baseline-min)))
    (should (= 60.0 (plist-get behind :outside-min)))
    ;; the four still divide the elapsed span exactly, without it
    (should (= (plist-get behind :behind-min)
               (+ (plist-get behind :baseline-min)
                  (plist-get behind :surge-min)
                  (plist-get behind :unclocked-min)
                  (plist-get behind :away-min))))
    ;; the elapsed legend keeps to its four, so nothing there reads as a
    ;; fifth division of the span
    ;; drawn as a segment of the elapsed evening rather than as a note about
    ;; it, with the rest of those hours reported as yours -- which is all
    ;; that is known about them
    (let ((key (substring-no-properties
                (org-foresight-report--off-key
                 (list :off-behind-min 120.0) behind))))
      (should (string-match-p "borrowed 1:00" key))
      (should (string-match-p "own 1:00" key)))))

(ert-deftest org-foresight-test-a-private-clock-is-not-work ()
  "An hour clocked against life is an hour work lent out, not an hour worked.

The clock is the record of where the hours went, not of which of them were
work.  Counting every clock line as work reported an hour at the dentist as
an hour of the day's work -- and the same clock running in the evening as an
hour borrowed from it -- both of which are the tool telling you the opposite
of what happened.

Each row names the hours it lent the other: private time taken by work is
`borrowed' on the hours off, and working time taken by life is `borrowed' on
the working day.  Same word, same rule, opposite directions."
  (let* ((org-foresight-work '(("09:00" . "17:30")))
         (org-foresight-workdays '(0 1 2 3 4 5 6))
         (org-foresight-private-categories '("family"))
         (org-foresight--shape-cache nil)
         (day (org-foresight-test--ts 0 0 10))
         (now (org-foresight-test--ts 15 0 10))
         (dentist (car (org-foresight-test--ivs '(10 0 11 0 10))))
         (real-work (car (org-foresight-test--ivs '(13 0 14 0 10))))
         (evening (car (org-foresight-test--ivs '(18 0 19 0 10))))
         (clock (list :today-intervals (list dentist real-work evening)
                      :today-private-intervals (list dentist evening)
                      :today-tasks nil))
         (behind (org-foresight-behind day clock nil now)))
    ;; the hour at the dentist is lent out, not worked
    (should (= 60.0 (plist-get behind :borrowed-min)))
    (should (= 60.0 (plist-get behind :baseline-min)))
    ;; and the private evening is nobody's business: it is not work escaping
    ;; the day, so the hours off must not report it as borrowed either
    (should (= 0.0 (plist-get behind :outside-min)))
    ;; the five still divide the elapsed working span exactly
    (should (= (plist-get behind :behind-min)
               (+ (plist-get behind :baseline-min)
                  (plist-get behind :surge-min)
                  (plist-get behind :borrowed-min)
                  (plist-get behind :unclocked-min)
                  (plist-get behind :away-min))))))

(defun org-foresight-test--two-bars (hour)
  "Return the bars of the surrounding day drawn at HOUR, as a list of lines.

The clock is arranged so the ahead row opens with a filled block.  The seam
is asserted by finding where the blank indent ends, and a row that began
with the reserve -- which is drawn blank on purpose -- would hide it."
  (let* ((org-foresight-surge-cache-file "/nonexistent/surge.eld")
         (org-foresight-leak-cache-file "/nonexistent/leak.eld")
         (org-foresight-surge-default "0:45")
         (org-foresight-leak-default "0:00")
         (org-foresight-lost-default "0:00")
         (day (org-foresight-test--ts 0 0 10))
         (now (org-foresight-test--ts hour 0 10))
         (cap (org-foresight-capacity day nil now))
         (pair (org-foresight-test--behind-fixture
                '((9 15 10 30 10) (10 30 12 0 10) (13 0 13 45 10))
                '(2) '((12 0 13 0 10))))
         (behind (org-foresight-behind day (car pair) (cadr pair) now)))
    (split-string (substring-no-properties
                   (org-foresight-report--bars cap behind))
                  "\n")))

(defmacro org-foresight-test--with-two-bars (&rest body)
  "Run BODY over a fixed day holding a morning meeting and an afternoon one."
  (declare (indent 0))
  `(org-foresight-test--with-day
       "* morning review
<2026-08-10 Mon 10:00-11:00>
* afternoon call
<2026-08-10 Mon 15:00-16:00>
* NEXT write the summary
SCHEDULED: <2026-08-10 Mon>
:PROPERTIES:
:EFFORT:   1:00
:END:
"
     ,@body))

(ert-deftest org-foresight-test-the-rule-falls-where-now-falls ()
  "The bar is ruled at the column NOW stands in, not near it.

One bar, one time axis, one moment dividing it.  The rule is the only thing
on the page saying which part of the day has already happened, so a rule a
column or two out is a picture that quietly disagrees with the figure beside
it.

Measured in display columns rather than characters: how wide the terminal
draws a block is what decides which column the rule lands in, and counting
characters would only be guessing at it -- the same guess that once put the
gap marker four columns left of the row it belonged to."
  (org-foresight-test--with-two-bars
    (let* ((lines (org-foresight-test--two-bars 14))
           (bar (seq-find (lambda (l) (string-prefix-p "Work" l)) lines))
           (col (org-foresight-report--bar-column))
           (day (org-foresight-test--ts 0 0 10))
           (now (org-foresight-test--ts 14 0 10))
           (cap (org-foresight-capacity day nil now))
           (per (org-foresight-report--bar-scale cap))
           ;; 09:00 to 14:00 of an 09:00-17:30 day, at the bar's own scale
           (expect (+ col (round (/ 300.0 per))))
           (rule (and bar (string-search (string org-foresight-report-rule)
                                         bar))))
      (should bar)
      (should rule)
      (should (= expect (string-width (substring bar 0 rule))))
      ;; a seam, not the end of the bar
      (should (> (string-width bar) (1+ expect))))))

(ert-deftest org-foresight-test-an-overcommitted-afternoon-holds-the-line ()
  "The indented row is cut to the line it has left, not to the full width.

An afternoon promised six hours it does not have draws its overflow past the
mark, and it draws it starting from wherever the elapsed row ended.  Given
the whole width from there it runs off the window -- and a bar that leaves
the window is not a long bar, it is a wrapped line that takes the row below
it out of alignment with everything above."
  (org-foresight-test--with-two-bars
    (let* ((org-foresight-surge-cache-file "/nonexistent/surge.eld")
           (org-foresight-leak-cache-file "/nonexistent/leak.eld")
           (org-foresight-surge-default "0:00")
           (org-foresight-leak-default "0:00")
           (org-foresight-lost-default "0:00")
           (day (org-foresight-test--ts 0 0 10))
           (now (org-foresight-test--ts 16 0 10))
           ;; twelve hours owed with one left to do them in
           (cap (append (list :committed-min 720.0)
                        (org-foresight-capacity day nil now)))
           (pair (org-foresight-test--behind-fixture
                  '((9 0 12 0 10) (13 0 16 0 10)) nil nil))
           (behind (org-foresight-behind day (car pair) (cadr pair) now))
           (lines (split-string
                   (substring-no-properties
                    (org-foresight-report--bars cap behind))
                   "\n")))
      ;; the overflow is still shown -- it is cut, not clipped back to fitting
      (should (seq-find (lambda (l) (string-match-p "…" l)) lines))
      (should (org-foresight-test--within-80
               (mapconcat (lambda (l) (concat org-foresight-report-margin l))
                          lines "\n"))))))

(ert-deftest org-foresight-test-the-elapsed-row-names-what-it-found ()
  "Behind is labelled with the elapsed span and divided into its four parts."
  (org-foresight-test--with-two-bars
    (let* ((lines (org-foresight-test--two-bars 14))
           (all (string-join lines "\n")))
      ;; one row for the whole span, whichever side of NOW it lies
      (should (seq-find
               (lambda (l)
                 (string-prefix-p
                  (format org-foresight-report--bar-stub "Work" "8:30") l))
               lines))
      (should-not (seq-find (lambda (l) (string-prefix-p "Behind" l)) lines))
      ;; the work legend sits above its own bar and carries the rule between
      ;; what was measured and what is forecast
      (let ((above (seq-take-while
                    (lambda (l) (not (string-prefix-p "Work" l))) lines)))
        (should above)
        (should (string-match-p (string org-foresight-report-rule)
                                (string-join above "\n")))
        ;; and the terms fall the right side of it
        (let* ((joined (string-join above " "))
               (at (string-search (string org-foresight-report-rule) joined)))
          (should (string-match-p "baseline" (substring joined 0 at)))
          (should (string-match-p "booked" (substring joined at)))))
      (dolist (word '("clocked" "surge" "unclocked" "away"))
        (should (string-match-p word all)))
      ;; the morning meeting is over: it is no longer part of what is ahead
      (should (string-match-p "booked 1:00" all))
      ;; and every row of the block holds the line once the margin is on it
      (should (org-foresight-test--within-80
               (mapconcat (lambda (l) (concat org-foresight-report-margin l))
                          lines "\n"))))))

(ert-deftest org-foresight-test-one-row-before-the-day-begins ()
  "Nothing has elapsed, so there is no seam and no row to draw for it.

The bar keeps its older name then.  `Ahead\=' means something only against a
`Behind\=', and a lone row labelled with half of a pair is a promise of a
second row that is not coming."
  (org-foresight-test--with-two-bars
    (let ((lines (org-foresight-test--two-bars 8)))
      (should (seq-find (lambda (l) (string-prefix-p "Work" l)) lines))
      (should-not (seq-find (lambda (l) (string-prefix-p "Behind" l)) lines))
      (should-not (seq-find (lambda (l) (string-prefix-p "Ahead" l)) lines)))))

(ert-deftest org-foresight-test-elapsed-and-remaining-divide-the-day ()
  "What has gone and what is left must be a partition, at every instant.

The whole two-row bar rests on this: `Behind' is drawn from the first and
`Ahead' from the second, and if they do not add up to the working day the two
rows either overlap or leave a gap, and the join stops meaning `now'.

Asserted branch by branch against `org-foresight--window-remaining', because
the two are written as one three-way split and a change to either alone is
exactly how that stops being true."
  (let* ((work (org-foresight-test--ivs '(9 0 12 0) '(13 0 17 30)))
         (span (org-foresight--intervals-seconds work)))
    (dolist (at '((7 0) (9 0) (10 30) (12 0) (12 30) (13 0) (15 0) (17 30) (20 0)))
      (let* ((now (org-foresight-test--ts (nth 0 at) (nth 1 at)))
             (gone (org-foresight--intervals-elapsed work now))
             (left (org-foresight--intervals-remaining work now)))
        (should (< (abs (- span (+ (org-foresight--intervals-seconds gone)
                                   (org-foresight--intervals-seconds left))))
                   0.001))
        ;; and neither may invent time the day never had
        (dolist (iv (append gone left))
          (should (not (time-less-p (cdr iv) (car iv)))))))
    ;; the three branches, named
    (let ((iv (car (org-foresight-test--ivs '(9 0 12 0)))))
      ;; wholly ahead
      (should-not (org-foresight--window-elapsed iv (org-foresight-test--ts 8 0)))
      (should (equal iv (org-foresight--window-remaining iv (org-foresight-test--ts 8 0))))
      ;; wholly past
      (should (equal iv (org-foresight--window-elapsed iv (org-foresight-test--ts 13 0))))
      (should-not (org-foresight--window-remaining iv (org-foresight-test--ts 13 0)))
      ;; split, at the same instant
      (let ((now (org-foresight-test--ts 10 0)))
        (should (equal (cdr (org-foresight--window-elapsed iv now))
                       (car (org-foresight--window-remaining iv now))))))))

(ert-deftest org-foresight-test-the-signals-survey-the-files-once ()
  "Working out the signals must walk the agenda files once, not eight times.

The borrowing signal asks about seven days, and asked each of them for its own
survey -- seven walks of every entry in every file, for one line that is
usually not printed, plus the walk the rest of the signals needed.  A survey
of a week costs what a survey of a day costs, so one serves them all.

Measured on a slow machine this was the whole of the difference between a
redraw that took four seconds and one that took nine tenths of a second: the
eight extra walks landed whenever the signals fell out of their few-second
cache, which is once in every four or five redraws."
  (org-foresight-test--with-signals
      (concat "* NEXT something\nSCHEDULED: " (org-foresight-test--stamp 0)
              "\n:PROPERTIES:\n:EFFORT: 1:00\n:END:\n"
              "* dinner\n:PROPERTIES:\n:CATEGORY: family\n:END:\n"
              (org-foresight-test--stamp 0 "19:00" "20:30") "\n")
    (let ((org-foresight--signals-cache nil)
          (org-foresight--shape-cache nil)
          (org-foresight-private-categories '("family"))
          (surveys 0))
      (cl-letf* ((real (symbol-function 'org-foresight-scan))
                 ((symbol-function 'org-foresight-scan)
                  (lambda (&rest args)
                    (setq surveys (1+ surveys))
                    (apply real args))))
        (org-foresight-signals t)
        (should (= 1 surveys))
        ;; And none at all when the caller has one to offer, which is the case
        ;; every time the verdict asks: the report has already surveyed the
        ;; horizon to draw its own blocks, and a week is inside a fortnight.
        (setq surveys 0)
        (org-foresight-signals t (funcall real 14 (org-foresight--day-start 0)))
        (should (= 0 surveys))))))

(ert-deftest org-foresight-test-one-meeting-can-be-prepared-alone ()
  "Preparation can be filed for the meeting under the cursor and no other.

Offering every unprepared meeting at once is right for a Monday morning and
wrong for the invitation that just arrived: most meetings need nothing, and
a command that files for all of them is one that gets answered `no\=' and
then never run.  So the same filing is reachable one meeting at a time."
  (org-foresight-test--with-signals
      (concat "* chosen\n:PROPERTIES:\n:CATEGORY: outlook\n:END:\n"
              (org-foresight-test--stamp 1 "10:00" "11:00") "\n"
              "* the other one\n:PROPERTIES:\n:CATEGORY: outlook\n:END:\n"
              (org-foresight-test--stamp 1 "14:00" "15:00") "\n")
    (let ((org-foresight-task-file (car org-agenda-files))
          (org-foresight-task-datetree nil))
      (with-current-buffer (find-file-noselect (car org-agenda-files))
        (org-with-wide-buffer
         (goto-char (point-min))
         (re-search-forward "^\\* chosen")
         (org-foresight-prepare-meeting))
        ;; The one chosen is marked and has its two tasks; the other is
        ;; untouched, which is the whole point of the command.
        (org-with-wide-buffer
         (goto-char (point-min))
         (should (re-search-forward "Prep: chosen" nil t))
         (should (re-search-forward "Follow up: chosen" nil t))
         (goto-char (point-min))
         (should-not (re-search-forward "Prep: the other one" nil t)))
        ;; And running it again says so rather than filing a second pair.
        (org-with-wide-buffer
         (goto-char (point-min))
         (re-search-forward "^\\* chosen")
         (org-foresight-prepare-meeting))
        (org-with-wide-buffer
         (goto-char (point-min))
         (should (= 1 (cl-loop while (re-search-forward "Prep: chosen" nil t)
                               count t))))))))

(ert-deftest org-foresight-test-a-meeting-without-an-hour-is-refused ()
  "A date with no time of day has no side to put preparation on.

The two tasks are placed before and after the meeting; an all-day entry
gives nothing to place them around, and filing them at an invented hour
would put work in the day that nobody agreed to."
  (org-foresight-test--with-signals
      (concat "* all day\n:PROPERTIES:\n:CATEGORY: outlook\n:END:\n"
              (org-foresight-test--stamp 1) "\n")
    (let ((org-foresight-task-file (car org-agenda-files))
          (org-foresight-task-datetree nil))
      (with-current-buffer (find-file-noselect (car org-agenda-files))
        (org-with-wide-buffer
         (goto-char (point-min))
         (re-search-forward "^\\* all day")
         (should-error (org-foresight-prepare-meeting) :type 'user-error))))))

(ert-deftest org-foresight-test-borrowing-reads-the-week-ahead ()
  "Evenings already claimed by work are counted across the week ahead.

The loop used to walk backwards through seven days while the survey it was
given started today, so six of the seven had nothing to find.  The direction
that was wrong is the loop\='s: `:borrowed-min\=' is what is left of an evening
from now onwards, so a day that has gone reports nothing whatever was put in
it, and no survey could have made the backward reading work.

Which is the useful direction in any case -- an evening still ahead can be
given back, and one that is over cannot."
  (org-foresight-test--with-signals
      (concat "* NEXT tuesday evening\nSCHEDULED: "
              (org-foresight-test--stamp 2 "19:00" "21:00")
              "\n* NEXT wednesday evening\nSCHEDULED: "
              (org-foresight-test--stamp 3 "19:00" "21:00")
              "\n")
    (let ((org-foresight-borrow-warn 60)
          (org-foresight--shape-cache nil))
      (let ((found (car (org-foresight--borrow-findings))))
        (should found)
        (should (string-match-p "4:00 over 2 day(s) ahead"
                                (plist-get found :note)))))))

(ert-deftest org-foresight-test-the-profile-says-whether-code-is-really-native ()
  "The header reports what is running, not what the machine could run.

`native-comp-available-p\=' answers whether libgccjit loads.  It can load and
still fail every compilation -- it goes on to invoke a gcc driver, and where
that driver is missing or belongs to another toolchain each file falls back
to byte-code with a warning nobody keeps.  A header that read the first
question and printed it as the second told a reader their timings were
native when they were several times slower than native, which is the one
misreading that makes the rest of the page useless."
  (require 'org-foresight-profile)
  (cl-letf (((symbol-function 'subr-native-elisp-p) (lambda (_) nil)))
    (should-not (org-foresight-profile--native-in-use-p)))
  (cl-letf (((symbol-function 'subr-native-elisp-p) (lambda (_) t)))
    (should (org-foresight-profile--native-in-use-p))))

(ert-deftest org-foresight-test-every-profiled-phase-names-a-real-function ()
  "The profiler measures functions that exist.

It measures by advising what it is pointed at, and a name that no longer
answers is advised silently and reports a flat zero -- so the phase that got
renamed is the one phase the profile swears costs nothing.  This is the tool
somebody reaches for when a redraw has gone slow, and the one reading it
cannot afford is a confident nothing."
  (require 'org-foresight-profile)
  (dolist (phase org-foresight-profile--phases)
    (should (fboundp (nth 1 phase)))))

(ert-deftest org-foresight-test-the-files-are-surveyed-once-a-redraw ()
  "One walk of the agenda files answers a whole redraw, span and all.

The redraw asks the same files two different questions -- what today holds,
and what the fortnight holds -- and used to walk them once for each.  A week
view walked them once a day on top: eight walks to learn what one could have
told it, and on a real journal that was a fifth of the redraw at a day\='s
span and most of it at a week\='s.

Both spans are checked because they fail differently: at a day\='s span the
extra walk is the forward view\='s, and at a week\='s it is one per column."
  (org-foresight-test--with-agenda
      (concat "* NEXT something\nSCHEDULED: " (org-foresight-test--stamp 0)
              "\n:PROPERTIES:\n:EFFORT: 1:00\n:END:\n")
    (dolist (span '(day week))
      (let ((org-agenda-span span)
            (walks 0))
        (cl-letf* ((real (symbol-function 'org-foresight-scan))
                   ((symbol-function 'org-foresight-scan)
                    (lambda (&rest args)
                      (setq walks (1+ walks))
                      (apply real args))))
          (org-foresight-test--agenda)
          (should (= 1 walks)))))
    ;; And the one walk reaches as far as the furthest reader needs, or the
    ;; saving is only that the forward view is answered out of buckets that
    ;; were never filled.
    (should (<= org-foresight-horizon-days
                (plist-get (org-foresight-redraw-scan) :days)))))

(ert-deftest org-foresight-test-a-shared-survey-notices-an-edit ()
  "A survey kept from before a write is not handed to a reader after it.

The build boundary drops the survey, and between builds a write can still
happen -- \[org-foresight-report-refresh] runs after an edit made from the
agenda itself, and the figures it draws must be of the file as it now
stands.  So the survey is also checked against how far each file has been
edited, and a moved tick is a fresh walk."
  (org-foresight-test--with-agenda
      (concat "* NEXT something\nSCHEDULED: " (org-foresight-test--stamp 0)
              "\n:PROPERTIES:\n:EFFORT: 1:00\n:END:\n")
    (let ((today (org-foresight--day-start 0)))
      (should (= 60.0 (org-foresight-scan-day (org-foresight-redraw-scan)
                                              :committed today)))
      (with-current-buffer (find-file-noselect (car org-agenda-files))
        (org-with-wide-buffer
         (goto-char (point-min))
         (re-search-forward "EFFORT: +1:00")
         (replace-match "EFFORT:   3:00")))
      (should (= 180.0 (org-foresight-scan-day (org-foresight-redraw-scan)
                                               :committed today))))))

(ert-deftest org-foresight-test-a-day-past-the-horizon-is-surveyed-on-its-own ()
  "A day the shared survey does not reach gets a survey of its own.

The redraw\='s survey covers the fortnight, which is every day an agenda
normally shows.  Jump a month ahead and the day is outside it -- and a day
drawn from buckets that were never filled is a day that quietly says nothing
is happening, which is the one answer worse than being slow."
  (let ((org-foresight-horizon-days 3))
    (org-foresight-test--with-agenda
        (concat "* meeting far off\n:PROPERTIES:\n:CATEGORY: meeting\n:END:\n"
                (org-foresight-test--stamp 30 "10:00" "11:00") "\n")
      (let ((far (org-foresight--day-start -30)))
        (should-not (org-foresight-scan-covers-p (org-foresight-redraw-scan) far))
        (should (org-foresight-scan-day
                 (org-foresight-agenda--scan-for far) :ledger far))))))

(ert-deftest org-foresight-test-a-shared-survey-does-not-outlive-its-settings ()
  "A survey answers only for the options it was taken under.

The files can be word for word the same and the answer still different: turn
the estimate correction off and what is promised is charged at the estimate
rather than at what the estimate has historically meant.  No fingerprint of
the files notices that, because the files did not change -- so the survey is
dropped at the head of every build instead, and this is what says so."
  (org-foresight-test--with-bias
      (concat
       (org-foresight-test--done "r1" "reporting" "1:00" 2)
       (org-foresight-test--done "r2" "reporting" "1:00" 2)
       (org-foresight-test--done "r3" "reporting" "1:00" 2)
       "* NEXT write the report\nSCHEDULED: " (org-foresight-test--stamp 0)
       "\n:PROPERTIES:\n:EFFORT: 1:00\n:CATEGORY: reporting\n:END:\n")
    (org-foresight-learn-bias)
    (let ((today (org-foresight--day-start 0)))
      (let ((corrected (org-foresight-scan-day (org-foresight-redraw-scan)
                                               :committed today)))
        (org-foresight-invalidate-scan)
        (let* ((org-foresight-bias-enabled nil)
               (raw (org-foresight-scan-day (org-foresight-redraw-scan)
                                            :committed today)))
          (should (= 120.0 corrected))
          (should (= 60.0 raw)))))))

(ert-deftest org-foresight-test-the-clock-is-surveyed-once-a-redraw ()
  "One walk of the logbooks per redraw, whichever blocks are being drawn.

The elapsed bar above the agenda and the block below it both read the last
seven days.  Two walks are two chances to disagree about one afternoon --
and on a slow machine the second one is paid for on every keypress, which is
the shape the last round of this cost four seconds a redraw."
  (org-foresight-test--with-agenda
      (concat "* NEXT something\nSCHEDULED: " (org-foresight-test--stamp 0)
              "\n:PROPERTIES:\n:EFFORT: 1:00\n:END:\n")
    (dolist (style '(daily review))
      (let ((org-foresight-report-style style)
            (walks 0))
        (cl-letf* ((real (symbol-function 'org-foresight-clock-scan))
                   ((symbol-function 'org-foresight-clock-scan)
                    (lambda (&rest args)
                      (setq walks (1+ walks))
                      (apply real args))))
          (org-foresight-test--agenda)
          (should (= 1 walks)))))))

(ert-deftest org-foresight-test-the-profile-carries-no-content ()
  "The profile must be sendable to somebody who may not see the calendar.

That is a property of the code, not of the reader remembering to strip
fields: the functions that assemble the report are handed durations, counts
and sizes, and are never handed a heading, a file name, a category, a place
or a tag.  This is the test that says so -- every distinctive string in the
fixture is looked for in the output, including the name of the file it was
all read out of."
  (require 'org-foresight-profile)
  (org-foresight-test--with-agenda
      (concat "* NEXT ACQUISITIONOFNORTHERNCO\n:PROPERTIES:\n"
              ":CATEGORY: SECRETPROJECTX\n:PLACE: UNDISCLOSEDSITE\n"
              ":EFFORT: 1:00\n:END:\n"
              "SCHEDULED: " (org-foresight-test--stamp 0) "\n"
              "* CONFIDENTIALMEETING\n:PROPERTIES:\n:CATEGORY: SECRETPROJECTX\n"
              ":LOCATION: HEADQUARTERSALPHA\n:END:\n"
              (org-foresight-test--stamp 0 "13:00" "14:00") " :SENSITIVETAG:\n")
    (let* ((org-foresight-places '((UNDISCLOSEDSITE . "HEADQUARTERSALPHA")))
           (org-foresight-profile-file
            (make-temp-file "org-foresight-profile" nil ".txt"))
           (org-foresight--shape-cache nil))
      (unwind-protect
          (progn
            (org-foresight-test--agenda)
            (with-current-buffer org-agenda-buffer-name
              (cl-letf (((symbol-function 'display-buffer) #'ignore)
                        ((symbol-function 'org-foresight-observe--get-json)
                         (lambda (&rest _) nil)))
                (org-foresight-profile 2)))
            (let ((report (with-temp-buffer
                            (insert-file-contents org-foresight-profile-file)
                            (buffer-string))))
              (should (string-match-p "org-foresight profile" report))
              ;; the numbers are there ...
              (should (string-match-p "headings" report))
              (should (string-match-p "scan" report))
              ;; ... and nothing that was read to produce them
              (dolist (secret '("ACQUISITIONOFNORTHERNCO" "SECRETPROJECTX"
                                "UNDISCLOSEDSITE" "CONFIDENTIALMEETING"
                                "HEADQUARTERSALPHA" "SENSITIVETAG"
                                "org-foresight-test"))
                (should-not (string-match-p (regexp-quote secret) report)))
              ;; nor the path of any file it was read from
              (dolist (file org-agenda-files)
                (should-not (string-match-p (regexp-quote file) report))
                (should-not (string-match-p
                             (regexp-quote (file-name-nondirectory file))
                             report)))))
        (delete-file org-foresight-profile-file)))))

(ert-deftest org-foresight-test-the-watcher-is-asked-once-an-hour ()
  "One fetch is four requests, paid inside a redraw somebody is waiting on.

What they buy is the account of hours already spent, so asking again ten
minutes later cannot change a decision.  Plain `r\=' therefore reuses the
answer; a prefix argument -- the reading of the key that means rebuild all of
it -- goes back to the watcher."
  (let ((fetches 0)
        (org-foresight-observe--cache nil))
    (cl-letf (((symbol-function 'org-foresight-observe--get-json)
               (lambda (&rest _) (setq fetches (1+ fetches)) nil)))
      (should (>= org-foresight-observe-cache-ttl 3600))
      (org-foresight-observe-today)
      (org-foresight-observe-today)
      (org-foresight-observe-today)
      (should (= 1 fetches))
      ;; a plain redraw leaves it alone ...
      (org-foresight-observe--redo-all nil)
      (org-foresight-observe-today)
      (should (= 1 fetches))
      ;; ... and rebuilding all of it asks again
      (org-foresight-observe--redo-all t)
      (org-foresight-observe-today)
      (should (= 2 fetches)))))

(ert-deftest org-foresight-test-the-day-file-is-not-opened-to-be-asked ()
  "Asking what shape a day is must not cost a look at the disk.

The question is put a hundred times over while one agenda is drawn -- every
call to `org-foresight-work-intervals\=' and `org-foresight-day-place\=' is
one -- and each one used to open the day file, once to read a modification
tick and four times more to read four properties off one heading.  On a
synchronised drive that was the most expensive thing the package did."
  (org-foresight-test--with-day
      (concat "* 2026\n** 2026-08\n*** "
              (format-time-string "%Y-%m-%d %a" (org-foresight--day-start 0))
              "\n:PROPERTIES:\n:WORK: 10:00-16:00\n:PLACE: office\n:END:\n")
    (let ((org-foresight-day-file (car org-agenda-files))
          (org-foresight--shape-cache nil)
          (opens 0))
      ;; Twice, as a session has: the first read visits the file, and the
      ;; second is the first to see a modification tick at all -- so it is the
      ;; second that settles.  From then on nothing is opened again until
      ;; something changes.
      (org-foresight-day-shape (org-foresight--day-start 0))
      (org-foresight-day-shape (org-foresight--day-start 0))
      (cl-letf* ((real (symbol-function 'find-file-noselect))
                 ((symbol-function 'find-file-noselect)
                  (lambda (&rest args)
                    (setq opens (1+ opens))
                    (apply real args))))
        (dotimes (_ 50)
          (org-foresight-work-intervals (org-foresight--day-start 0))
          (org-foresight-day-place (org-foresight--day-start 0)))
        (should (= 0 opens))))))

(ert-deftest org-foresight-test-e2e-a-gap-that-has-gone-offers-nothing ()
  "What is left of a stretch is what may be offered, not what it was.

An hour suggested at two o\'clock for a gap that opened at nine is not an
offer, it is a reproach -- and the morning it names was spent on something,
which is a different question and a different view.  A stretch half gone
offers what still fits in front of it."
  (let ((body (concat "* NEXT two hours\nSCHEDULED: "
                      (org-foresight-test--stamp 0)
                      "\n:PROPERTIES:\n:EFFORT: 2:00\n:END:\n"
                      "* NEXT twenty minutes\nSCHEDULED: "
                      (org-foresight-test--stamp 0)
                      "\n:PROPERTIES:\n:EFFORT: 0:20\n:END:\n")))
    (cl-flet ((offered-at
                (hh mm at)
                (org-foresight-test--with-agenda body
                  (let ((org-foresight-agenda--now
                         (time-add (org-foresight--day-start 0)
                                   (seconds-to-time (* 60 (+ (* 60 hh) mm))))))
                    (seq-filter (lambda (l)
                                  (and (string-search "\u21b3 NEXT" l)
                                       (string-search at l)))
                                (org-foresight-test--agenda))))))
      ;; first thing: the whole of the morning is ahead, so both are offered
      (should (= 2 (length (offered-at 7 0 " 9:00"))))
      ;; from half eleven only twenty minutes of it is left
      (should (= 1 (length (offered-at 11 30 " 9:00"))))
      (should (string-match-p "twenty minutes"
                              (car (offered-at 11 30 " 9:00"))))
      ;; and once it is over it holds nothing at all, whatever it held
      (should-not (offered-at 13 30 " 9:00"))
      ;; while the afternoon, still ahead, is untouched
      (should (= 2 (length (offered-at 13 30 "13:00")))))))

(ert-deftest org-foresight-test-e2e-work-in-the-wrong-place-says-so ()
  "Work left out of every gap must say why, and only where the reason holds.

The suggestion is right to leave it out -- an hour at home is no use to an
office errand -- but a row that is silently never offered looks like a row the
tool forgot, and the reader goes hunting for the fault in the wrong place.

On a day that does go there the mark is wrong and must not appear: the work
can be done, and the board lists it."
  (let ((body (concat "* NEXT only at the office\nSCHEDULED: "
                      (org-foresight-test--stamp 0)
                      "\n:PROPERTIES:\n:EFFORT: 1:00\n:PLACE: office\n:END:\n"
                      "* NEXT anywhere at all\nSCHEDULED: "
                      (org-foresight-test--stamp 0)
                      "\n:PROPERTIES:\n:EFFORT: 0:30\n:END:\n"))
        (trip (concat "* 朝の打合せ\n:PROPERTIES:\n:CATEGORY: meeting\n"
                      ":LOCATION: 本社 会議室3\n:END:\n"
                      (org-foresight-test--stamp 0 "10:00" "11:00") "\n"
                      "* 午後の打合せ\n:PROPERTIES:\n:CATEGORY: meeting\n"
                      ":LOCATION: 本社 会議室3\n:END:\n"
                      (org-foresight-test--stamp 0 "14:00" "15:00") "\n")))
    (cl-flet ((marked-p
                (text title)
                (org-foresight-test--with-agenda text
                  (let ((org-foresight-work '(("09:00" . "17:30")))
                        (org-foresight-places
                         '((office . "本社\\|会議室\\|オフィス")))
                        (org-foresight-home-place 'home)
                        (org-foresight-travel-matrix '(((home . office) . 60)))
                        (org-foresight--shape-cache nil))
                    (seq-find
                     (lambda (l)
                       (and (string-match-p title l)
                            (string-search org-foresight-agenda-elsewhere l)))
                     (org-foresight-test--agenda))))))
      ;; a day that goes nowhere: the office errand is marked, the other is not
      (should (marked-p body "only at the office"))
      (should-not (marked-p body "anywhere at all"))
      ;; a day that goes there: nothing to explain
      (should-not (marked-p (concat trip body) "only at the office")))))

(ert-deftest org-foresight-test-the-board-asks-where-the-day-goes ()
  "`Here' and `Cannot be done from here' read one fact, and it is not the base.

A day worked from home with an appointment at the office is a day the office
errands can be run on.  Asking `org-foresight-day-place' -- where the day is
worked *from* -- got this wrong twice over: the work was missing from `Here',
and the same work was reported as impossible below it."
  (org-foresight-test--with-signals
      (concat "* 打合せ\n:PROPERTIES:\n:CATEGORY: meeting\n"
              ":LOCATION: 本社 会議室3\n:END:\n"
              (org-foresight-test--stamp 0 "10:00" "11:00") "\n"
              "* NEXT only at the office\nSCHEDULED: "
              (org-foresight-test--stamp 0)
              "\n:PROPERTIES:\n:EFFORT: 1:00\n:PLACE: office\n:END:\n")
    (let* ((org-foresight-work '(("09:00" . "17:30")))
           (org-foresight-places '((office . "本社\\|会議室\\|オフィス")))
           (org-foresight-home-place 'home)
           (org-foresight-travel-matrix '(((home . office) . 60)))
           (org-foresight--shape-cache nil)
           (org-foresight--signals-cache nil)
           (day (org-foresight--day-start 0)))
      ;; the day is based at home and goes to the office
      (should (equal '(home office) (org-foresight-day-places day)))
      ;; Here holds the office work, and names both places
      (let ((here (substring-no-properties (org-foresight-report-here))))
        (should (string-match-p "only at the office" here))
        (should (string-match-p "home, office" here)))
      ;; and it is not also reported as impossible
      (should-not
       (assoc "Cannot be done from here" (org-foresight-signals t))))))

(ert-deftest org-foresight-test-place-at-is-not-the-day-s-place ()
  "Where the body is at an hour, which a day with a journey answers twice.
`org-foresight-day-place' says where the day is worked from; this says where
you actually are, and on an office day the morning is still spent at home."
  (org-foresight-test--with-day
      (concat "* 打合せ\n:PROPERTIES:\n:CATEGORY: meeting\n"
              ":LOCATION: 本社 会議室3\n:END:\n"
              (org-foresight-test--stamp 0 "10:00" "11:00") "\n")
    (let* ((org-foresight-work '(("09:00" . "17:30")))
           (org-foresight-places '((office . "本社\\|会議室\\|オフィス")))
           (org-foresight-home-place 'home)
           (org-foresight-travel-matrix '(((home . office) . 60)))
           (org-foresight--shape-cache nil)
           (day (org-foresight--day-start 0))
           (on (lambda (h m) (org-foresight-place-at
                              day (time-add day (seconds-to-time
                                                 (* 60 (+ (* 60 h) m))))))))
      ;; before setting off, on the road, there, and home again
      (should (eq 'home (funcall on 9 30)))
      (should (eq 'office (funcall on 10 30)))
      (should (eq 'home (funcall on 16 0))))))

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

(ert-deftest org-foresight-test-e2e-a-mark-never-lands-inside-a-word ()
  "The shared column is padding only on the rows it was measured from.

`%?-12t' is dropped from a row with no time rather than padded, so the column
that is the tail of the clock on the timed rows is somewhere in the middle of
the prefix on an undated one.  Put the effort in front of the leader and that
somewhere is inside `Scheduled:', and the mark comes out as `Schedul⨯ed:'.
Where the shared column is not preceded by whitespace the row falls back to
its own heading, which always is."
  (org-foresight-test--with-agenda
      (concat "* NEXT far too big for the day\nSCHEDULED: "
              (org-foresight-test--stamp 0)
              "\n:PROPERTIES:\n:EFFORT: 9:00\n:END:\n"
              "* NEXT this one fits\nSCHEDULED: " (org-foresight-test--stamp 0)
              "\n:PROPERTIES:\n:EFFORT: 1:00\n:END:\n")
    ;; The effort ahead of the leader, which is what makes an estimate a column
    ;; and is the arrangement the shared column cannot assume anything about.
    (let ((org-agenda-prefix-format
           '((agenda . "     %-8.8c%?-12t%?-5e% s") (todo . "  %-8c %-7e")
             (tags . "  %i %-5c %-7e") (search . " %i %-12c"))))
      (org-foresight-test--agenda)
      (with-current-buffer org-agenda-buffer-name
        (let ((seen 0))
          (goto-char (point-min))
          (while (not (eobp))
            (let ((b (line-beginning-position)) (e (line-end-position)))
              (when (text-property-any b e 'org-heading t)
                (let ((line (buffer-substring-no-properties b e)))
                  (dolist (glyph (mapcar #'car org-foresight-agenda--mark-meanings))
                    (when-let ((at (string-search glyph line)))
                      (setq seen (1+ seen))
                      (should (> at 0))
                      (should (eq (aref line (1- at)) ?\s)))))))
            (forward-line 1))
          ;; Both marks are on this page: the one that will not fit, and the
          ;; candidate rows for the one that does.
          (should (> seen 1)))))))

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

(ert-deftest org-foresight-test-a-kind-is-made-once-and-found-after ()
  "The second conversation about a thing lands where the first one did.

Made every time, the hours would scatter across identical headings and the
figure they add up to would never be anywhere.  The heading carries no TODO
keyword, which is also what leaves the outline alone: keyword-less headings
are scaffolding, so the work it hangs under is still a task rather than
having quietly become a project."
  (org-foresight-test--with-org
      "* NEXT migrate the service\n** NEXT write the runbook\n"
    (let ((org-foresight-clock-fill-kinds '("comms"))
          (org-foresight-clock-fill-kind-property "KIND")
          (org-foresight--signals-cache nil))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "migrate the service")))
        (let ((first (org-foresight--clock-fill-kind-marker "comms"))
              (again (org-foresight--clock-fill-kind-marker "comms")))
          (should (= (marker-position first) (marker-position again)))
          (with-current-buffer (marker-buffer first)
            (should (= 1 (how-many "^\\*+ comms$" (point-min) (point-max))))
            (org-with-point-at first
              (should (equal "comms" (org-get-heading t t t t)))
              (should (null (org-get-todo-state)))
              (should (equal "comms" (org-entry-get (point) "KIND")))
              ;; one level under the work it belongs to
              (should (= 2 (org-current-level))))))))))

(ert-deftest org-foresight-test-clock-fill-offers-a-kind-before-the-day ()
  "A kind is the first answer offered, and it asks what it was about.

The hours hardest to name are the ones still unrecorded at six o'clock, so
the answer for them has to be the one already under the cursor.  Routed
before the day's own entries too: a kind that shares a name with something on
today's list must still ask, or the hour is filed silently under whichever
happened to match."
  (org-foresight-test--with-org "* NEXT something\n"
    (let ((org-foresight-clock-fill-kinds '("comms"))
          (org-foresight--signals-cache nil)
          offered filed)
      (cl-letf (((symbol-function 'org-foresight-behind) (lambda (&rest _) nil))
                ((symbol-function 'org-foresight-observe-coverage)
                 (lambda (&rest _) nil))
                ((symbol-function 'org-foresight--clock-gaps)
                 (lambda (_) (list (cons (cons (current-time) (current-time))
                                         'unclocked))))
                ((symbol-function 'org-foresight--clock-gap-label)
                 (lambda (_) "a gap"))
                ((symbol-function 'completing-read)
                 (lambda (prompt collection &rest _)
                   (if (string-prefix-p "Unrecorded" prompt)
                       "a gap"
                     (setq offered collection)
                     "comms")))
                ((symbol-function 'org-foresight--clock-fill-kind-marker)
                 (lambda (kind) (list 'kind-marker kind)))
                ((symbol-function 'org-foresight--file-clocked)
                 (lambda (marker _from _to) (setq filed marker)))
                ((symbol-function 'org-foresight--invalidate-signals) #'ignore))
        (org-foresight-clock-fill)
        (should (equal "comms" (car offered)))
        (should (equal '(kind-marker "comms") filed))))))

(ert-deftest org-foresight-test-a-journey-is-offered-and-files-nowhere ()
  "The drive is offered by name, and the clock does not land on the meeting.

Half the journeys a day derives have no entry behind them at all; the half
that do carry the marker of the meeting they are *for*.  Filing the drive
there would put the road inside the room, and the meeting would report an
hour and a half of itself."
  (let ((org-foresight-work '(("09:00" . "17:00")))
        (org-foresight-workdays '(0 1 2 3 4 5 6))
        (org-foresight--shape-cache nil)
        (org-foresight-places '((office . "office")))
        (org-foresight-travel-matrix '(((home . office) . 30)
                                       ((office . home) . 30))))
    (org-foresight-test--with-org
        (concat "* Standup\n:PROPERTIES:\n:CATEGORY: meeting\n:LOCATION: office\n:END:\n"
                (org-foresight-test--stamp 0 "13:00" "14:00") "\n")
      (let* ((clock (list :today-tasks nil))
             (known (org-foresight--clock-fill-candidates clock))
             (journey (assoc "→ office" known)))
        ;; offered
        (should journey)
        ;; and offered as a name, not as somewhere to file
        (should (null (cdr journey)))
        ;; while the meeting itself is still somewhere to file
        (should (markerp (cdr (assoc "Standup" known))))))))

(ert-deftest org-foresight-test-a-journey-named-at-the-prompt-is-booked ()
  "Naming the drive writes a journey, so the day stops reserving another one.

Filed as an ordinary entry the hour would be recorded and the derivation left
alone, and the day would go on holding time for a trip already made: the hour
counted twice, the free time short by it, and what would fit answered against
a road already travelled.  The travel property is what the derivation defers
to, so it is the thing to check."
  (let ((org-foresight-work '(("09:00" . "17:00")))
        (org-foresight-workdays '(0 1 2 3 4 5 6))
        (org-foresight-clock-fill-kinds nil)
        (org-foresight-clock-fill-minimum 20)
        (org-foresight--shape-cache nil)
        (org-foresight-places '((office . "office")))
        (org-foresight-travel-matrix '(((home . office) . 30)
                                       ((office . home) . 30))))
    (org-foresight-test--with-task-file
        (concat "* Standup\n:PROPERTIES:\n:CATEGORY: meeting\n:LOCATION: office\n:END:\n"
                (org-foresight-test--stamp 0 "13:00" "14:00") "\n"
                "* ONGO the work\n:LOGBOOK:\nCLOCK: "
                (format-time-string "[%Y-%m-%d %a 09:00]" (org-foresight--day-start 0))
                "--"
                (format-time-string "[%Y-%m-%d %a 10:00]" (org-foresight--day-start 0))
                " =>  1:00\n:END:\n")
      (let ((org-foresight-now (time-add (org-foresight--day-start 0) (* 3600 12))))
        ;; the journey is derived to begin with, and offered by name
        (should (assoc "→ office" (org-foresight--clock-fill-journeys)))
        (cl-letf (((symbol-function 'org-foresight-observe--get-json)
                   (lambda (&rest _) nil))
                  ((symbol-function 'completing-read)
                   (lambda (prompt collection &rest _)
                     (if (string-prefix-p "Unrecorded" prompt)
                         (car (car collection))
                       "→ office")))
                  ((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
          (org-foresight-clock-fill))
        (let ((text (org-foresight-test--task-file-text)))
          ;; written as a journey to somewhere, and clocked
          (should (string-match-p "→ office" text))
          (should (string-match-p
                   (concat ":" (regexp-quote org-foresight-travel-property)
                           ": *office")
                   text))
          (should (string-match-p "CLOCK: .*→\\|→[^*]*CLOCK: " text)))
        ;; and the day no longer derives one: what is on the page is written
        (setq org-foresight--shape-cache nil)
        (should-not (assoc "→ office" (org-foresight--clock-fill-journeys)))))))

(ert-deftest org-foresight-test-only-a-name-from-nowhere-can-have-arrived ()
  "A journey the calendar derived is not an interruption.

The question exists for a stretch that went on something nobody had written
down.  Asked of an answer the day already knew about, a yes teaches the
reserve that the commute arrived unplanned -- and it will hold time back for
one tomorrow."
  (org-foresight-test--with-org "* NEXT something\n"
    (let ((org-foresight--signals-cache nil)
          asked filed)
      (cl-letf (((symbol-function 'org-foresight-behind) (lambda (&rest _) nil))
                ((symbol-function 'org-foresight-observe-coverage)
                 (lambda (&rest _) nil))
                ((symbol-function 'org-foresight--clock-gaps)
                 (lambda (_) (list (cons (cons (current-time) (current-time))
                                         'unclocked))))
                ((symbol-function 'org-foresight--clock-gap-label)
                 (lambda (_) "a gap"))
                ((symbol-function 'org-foresight--clock-fill-candidates)
                 (lambda (&rest _) '(("→ office" . nil))))
                ((symbol-function 'completing-read)
                 (lambda (prompt &rest _)
                   (if (string-prefix-p "Unrecorded" prompt) "a gap" "→ office")))
                ((symbol-function 'y-or-n-p)
                 (lambda (&rest _) (setq asked t) t))
                ((symbol-function 'org-foresight--file-clocked-entry)
                 (lambda (_title _from _to surge) (setq filed surge) nil))
                ((symbol-function 'org-foresight--invalidate-signals) #'ignore))
        (org-foresight-clock-fill)
        (should-not asked)
        (should-not filed)))))

(ert-deftest org-foresight-test-every-agenda-row-names-its-own-entry ()
  "A row's marker is the heading the row is about, injected rows and all.

Everything the agenda can do to an entry goes through that marker, so a row
whose marker points elsewhere acts on the wrong heading while looking
right -- and the rows this package inserts between Org's own are exactly the
kind of change that would shift one.

Injected rows carry no marker at all, which is the other half of the same
invariant: a journey and a stretch of free time answer to nothing, and a
command that needs an entry has to refuse rather than reach for a neighbour."
  (let ((org-foresight-work '(("09:00" . "17:00")))
        (org-foresight-workdays '(0 1 2 3 4 5 6))
        (org-foresight--shape-cache nil)
        (org-foresight-places '((office . "office")))
        (org-foresight-travel-matrix '(((home . office) . 30)
                                       ((office . home) . 30))))
    (org-foresight-test--with-agenda
        (concat "* Standup\n:PROPERTIES:\n:CATEGORY: meeting\n:LOCATION: office\n:END:\n"
                (org-foresight-test--stamp 0 "13:00" "14:00") "\n"
                "* NEXT alpha task\nSCHEDULED: "
                (org-foresight-test--stamp 0 "10:00" "11:00") "\n"
                "* NEXT beta task\nSCHEDULED: "
                (org-foresight-test--stamp 0 "15:00" "16:00") "\n")
      (org-foresight-test--agenda)
      (with-current-buffer org-agenda-buffer-name
        (goto-char (point-min))
        (let ((named 0) (injected 0))
          (while (not (eobp))
            (let* ((bol (line-beginning-position))
                   (line (buffer-substring-no-properties bol (line-end-position)))
                   (hd (get-text-property bol 'org-hd-marker)))
              (cond
               (hd
                (setq named (1+ named))
                ;; the heading the marker leads to is named on the row
                (should (string-match-p
                         (regexp-quote (org-with-point-at hd
                                         (org-get-heading t t t t)))
                         line)))
               ;; a row this package inserted, and it answers to nothing
               ((string-match-p "→ \\|free\\|work \\(starts\\|ends\\|pauses\\|resumes\\)" line)
                (setq injected (1+ injected)))))
            (forward-line 1))
          ;; the fixture really did produce both kinds
          (should (= named 3))
          (should (> injected 0)))))))

(defun org-foresight-test--cut-heading (marker heading)
  "Delete the subtree named HEADING from MARKER\='s buffer, as a refile would."
  (with-current-buffer (marker-buffer marker)
    (org-with-wide-buffer
     (goto-char (point-min))
     (re-search-forward (concat "^\\* .*" (regexp-quote heading)))
     (beginning-of-line)
     (delete-region (point) (progn (org-end-of-subtree t t) (point))))))

(ert-deftest org-foresight-test-a-drifted-marker-is-what-a-rebuild-cures ()
  "A row keeps naming an entry that has gone, and its marker names another.

An agenda buffer keeps `org-marker' properties pointing into the org files.
When an entry it shows leaves -- archived, refiled, cut -- the marker does
not die: it slides to where the text was, which is now the *next* heading.
The line on screen is unchanged, so every command that acts through the
marker acts on a heading whose name is not on the row, and nothing says so.
Clocking in from that line clocks the wrong task.

This is the failure the rebuild exists for, so the test makes it happen."
  (let ((org-foresight-work '(("09:00" . "17:00")))
        (org-foresight-workdays '(0 1 2 3 4 5 6))
        (org-foresight--shape-cache nil))
    (org-foresight-test--with-agenda
        (concat "* NEXT alpha task\nSCHEDULED: "
                (org-foresight-test--stamp 0 "10:00" "11:00") "\n"
                "* NEXT beta task\nSCHEDULED: "
                (org-foresight-test--stamp 0 "13:00" "14:00") "\n")
      (unwind-protect
          ;; Answered as if there were no watcher for the whole test, not just
          ;; for the draw: the rebuild below redoes the agenda, and a rebuild
          ;; outside the stub reaches a real ActivityWatch and caches the day
          ;; it finds -- which the next test then inherits as its own.
          (cl-letf (((symbol-function 'org-foresight-observe--get-json)
                     (lambda (&rest _) nil)))
            (let ((org-agenda-sticky t))
              (org-foresight-test--agenda)
              (let ((marker (with-current-buffer org-agenda-buffer-name
                              (goto-char (point-min))
                              (re-search-forward "alpha task")
                              (get-text-property (line-beginning-position)
                                                 'org-hd-marker))))
                ;; to begin with, the row and its marker agree
                (should (equal "alpha task"
                               (org-with-point-at marker
                                 (org-get-heading t t t t))))
                (org-foresight-test--cut-heading marker "alpha task")
                ;; and now they do not: the row says alpha, the marker beta
                (should (equal "beta task"
                               (org-with-point-at marker
                                 (org-get-heading t t t t))))
                (should (with-current-buffer org-agenda-buffer-name
                          (save-excursion (goto-char (point-min))
                                          (re-search-forward "alpha task" nil t))))
                ;; and the row says so when asked
                (should (eq 'lies (org-foresight-agenda--row-state)))
                ;; and the next command in the agenda is what cures it: the
                ;; row goes with the entry, and the keystroke is dropped
                ;; rather than run on whatever took its place
                (with-current-buffer org-agenda-buffer-name
                  (let ((this-command 'org-agenda-clock-in))
                    (org-foresight-agenda--freshen)
                    (should (eq this-command #'ignore)))
                  ;; and the page now answers for the files as they are
                  (should (eq 'no-entry (org-foresight-agenda--row-state)))
                  (save-excursion
                    (goto-char (point-min))
                    (should-not (re-search-forward "alpha task" nil t))
                    (goto-char (point-min))
                    (should (re-search-forward "beta task" nil t)))))))
        ;; A sticky agenda is meant to survive, so this one has to be killed
        ;; by hand: left alive it outlives the fixture it was built from.
        (dolist (buf (buffer-list))
          (when (and (buffer-live-p buf)
                     (with-current-buffer buf (derived-mode-p 'org-agenda-mode)))
            (kill-buffer buf)))))))

(ert-deftest org-foresight-test-the-cure-does-not-wait-for-a-sticky-agenda ()
  "A marker is stranded by the deletion, not by the agenda outliving a visit.

`org-agenda-sticky\=' decides whether a page is shown again instead of built
again, which decides how likely a stale one is to be in front of you -- not
whether it can happen.  An ordinary agenda buffer left in a window while an
Org file is edited holds exactly the same markers, so the watch goes on every
agenda and the cure answers to none of that setting."
  (let ((org-foresight-work '(("09:00" . "17:00")))
        (org-foresight-workdays '(0 1 2 3 4 5 6))
        (org-foresight--shape-cache nil))
    (org-foresight-test--with-agenda
        (concat "* NEXT alpha task\nSCHEDULED: "
                (org-foresight-test--stamp 0 "10:00" "11:00") "\n"
                "* NEXT beta task\nSCHEDULED: "
                (org-foresight-test--stamp 0 "13:00" "14:00") "\n")
      (cl-letf (((symbol-function 'org-foresight-observe--get-json)
                 (lambda (&rest _) nil)))
        (let ((org-agenda-sticky nil))
          (org-foresight-test--agenda)
          (with-current-buffer org-agenda-buffer-name
            (should (memq #'org-foresight-agenda--freshen pre-command-hook))
            (goto-char (point-min))
            (re-search-forward "alpha task")
            (org-foresight-test--cut-heading
             (get-text-property (line-beginning-position) 'org-hd-marker)
             "alpha task")
            (should (eq 'lies (org-foresight-agenda--row-state)))
            (let ((this-command 'org-agenda-clock-in))
              (org-foresight-agenda--freshen)
              (should (eq this-command #'ignore)))
            (goto-char (point-min))
            (should-not (re-search-forward "alpha task" nil t))
            (goto-char (point-min))
            (should (re-search-forward "beta task" nil t))))))))

(defmacro org-foresight-test--after-cutting (on cut &rest body)
  "Put point on the row matching ON, cut the entry matching CUT, cure, run BODY."
  (declare (indent 2))
  `(let ((org-foresight-work '(("09:00" . "17:00")))
         (org-foresight-workdays '(0 1 2 3 4 5 6))
         (org-foresight--shape-cache nil))
     (org-foresight-test--with-agenda
         (concat "* NEXT alpha task\nSCHEDULED: "
                 (org-foresight-test--stamp 0 "10:00" "11:00") "\n"
                 "* NEXT beta task\nSCHEDULED: "
                 (org-foresight-test--stamp 0 "13:00" "14:00") "\n"
                 "* NEXT gamma task\nSCHEDULED: "
                 (org-foresight-test--stamp 0 "15:00" "16:00") "\n")
       (unwind-protect
           (cl-letf (((symbol-function 'org-foresight-observe--get-json)
                      (lambda (&rest _) nil)))
             (let ((org-agenda-sticky nil))
               (org-foresight-test--agenda)
               (with-current-buffer org-agenda-buffer-name
                 (goto-char (point-min))
                 (re-search-forward ,on)
                 (beginning-of-line)
                 (let ((keep (point))
                       (doomed (save-excursion
                                 (goto-char (point-min))
                                 (re-search-forward ,cut)
                                 (get-text-property (line-beginning-position)
                                                    'org-hd-marker))))
                   (org-foresight-test--cut-heading doomed ,cut)
                   (goto-char keep))
                 (let ((this-command 'org-agenda-clock-in))
                   (org-foresight-agenda--freshen))
                 ,@body)))
         (dolist (buf (buffer-list))
           (when (and (buffer-live-p buf)
                      (with-current-buffer buf (derived-mode-p 'org-agenda-mode)))
             (kill-buffer buf)))))))

(ert-deftest org-foresight-test-a-cursor-on-what-has-gone-lands-on-nothing ()
  "When the entry under the cursor is the one that went, the cursor holds none.

There is no honest row to return to.  Leaving the cursor where the position
happens to fall would hand the next keystroke an entry the person never chose
-- so it goes to the top of the page, which carries no entry at all and where
a stray keystroke can do nothing."
  (org-foresight-test--after-cutting "beta task" "beta task"
    ;; The top, stated as the top: where the character position happens to
    ;; fall is a matter of how many characters the deletion took with it, and
    ;; a test that only asked whether that spot carried an entry would pass on
    ;; a page where it happened not to.
    (should (= (point) (point-min)))
    (should-not (org-get-at-bol 'org-hd-marker))))

(ert-deftest org-foresight-test-an-edited-heading-still-names-its-entry ()
  "Editing a heading does not make its agenda row disagree with it.

The row is compared with the entry it points at, so anything that changes
both together is invisible here: typing in a heading, cycling its keyword,
moving its date, editing the body underneath.  Only the entry leaving makes
them differ -- which is the whole reason to ask the row rather than to watch
for the things that might have gone wrong."
  (let ((org-foresight-work '(("09:00" . "17:00")))
        (org-foresight-workdays '(0 1 2 3 4 5 6))
        (org-foresight--shape-cache nil))
    (org-foresight-test--with-agenda
        (concat "* NEXT alpha task\nSCHEDULED: "
                (org-foresight-test--stamp 0 "10:00" "11:00") "\n"
                "* NEXT beta task\nSCHEDULED: "
                (org-foresight-test--stamp 0 "13:00" "14:00") "\n")
      (unwind-protect
          (cl-letf (((symbol-function 'org-foresight-observe--get-json)
                     (lambda (&rest _) nil)))
            (let ((org-agenda-sticky nil))
              (org-foresight-test--agenda)
              (cl-flet ((state ()
                          (with-current-buffer org-agenda-buffer-name
                            (goto-char (point-min))
                            (re-search-forward "beta task")
                            (beginning-of-line)
                            (org-foresight-agenda--row-state))))
                (should (eq 'agrees (state)))
                (with-current-buffer (find-file-noselect (car org-agenda-files))
                  ;; the keyword, cycled
                  (goto-char (point-min))
                  (re-search-forward "^\\* NEXT beta")
                  (replace-match "* ONGO beta")
                  (should (eq 'agrees (state)))
                  ;; the date, moved
                  (goto-char (point-min))
                  (re-search-forward "^\\* ONGO beta")
                  (org-schedule nil "+1d")
                  (should (eq 'agrees (state)))
                  ;; and now the one thing that does part them
                  (goto-char (point-min))
                  (re-search-forward "^\\* ONGO beta")
                  (beginning-of-line)
                  (delete-region (point) (progn (org-end-of-subtree t t) (point)))
                  (set-buffer-modified-p nil))
                (should (eq 'lies (state))))))
        (dolist (buf (buffer-list))
          (when (and (buffer-live-p buf)
                     (with-current-buffer buf (derived-mode-p 'org-agenda-mode)))
            (kill-buffer buf)))))))

(ert-deftest org-foresight-test-clocking-in-lands-on-the-task-the-row-names ()
  "The whole point, said end to end: the clock goes on what the line says.

Every other test here checks a marker or a row.  This one presses the key and
asks what is running afterwards, because that is the fault as it is met --
`C-c C-x C-i\\=' on a line reading one task, and the wrong task clocked in.

Nothing is taken from the keyboard on the way.  The entry above was deleted,
so the page is out of date, but this row is not one of the ones that went
wrong and there is nothing here to protect anybody from."
  (let ((org-foresight-work '(("09:00" . "17:00")))
        (org-foresight-workdays '(0 1 2 3 4 5 6))
        (org-foresight--shape-cache nil))
    (org-foresight-test--with-agenda
        (concat "* NEXT alpha task\nSCHEDULED: "
                (org-foresight-test--stamp 0 "10:00" "11:00") "\n"
                "* NEXT beta task\nSCHEDULED: "
                (org-foresight-test--stamp 0 "13:00" "14:00") "\n"
                "* NEXT gamma task\nSCHEDULED: "
                (org-foresight-test--stamp 0 "15:00" "16:00") "\n")
      (unwind-protect
          (cl-letf (((symbol-function 'org-foresight-observe--get-json)
                     (lambda (&rest _) nil)))
            (let ((org-agenda-sticky nil))
              (org-foresight-test--agenda)
              (with-current-buffer org-agenda-buffer-name
                (goto-char (point-min))
                (re-search-forward "gamma task")
                (beginning-of-line)
                (let ((keep (point))
                      (alpha (save-excursion
                               (goto-char (point-min))
                               (re-search-forward "alpha task")
                               (get-text-property (line-beginning-position)
                                                  'org-hd-marker))))
                  (org-foresight-test--cut-heading alpha "alpha task")
                  (goto-char keep))
                (let ((this-command 'org-agenda-clock-in))
                  (org-foresight-agenda--freshen)
                  (should (eq this-command 'org-agenda-clock-in)))
                (org-agenda-clock-in)
                (should (org-clocking-p))
                (should (equal "gamma task"
                               (org-with-point-at org-clock-marker
                                 (org-get-heading t t t t)))))))
        (when (org-clocking-p) (org-clock-out nil t))
        (dolist (buf (buffer-list))
          (when (and (buffer-live-p buf)
                     (with-current-buffer buf (derived-mode-p 'org-agenda-mode)))
            (kill-buffer buf)))))))

(ert-deftest org-foresight-test-a-row-that-has-gone-takes-the-keystroke ()
  "On the row that went wrong, the key buys the rebuild instead.

The row under the cursor is the only one a command can reach, so it is the
only one worth asking about -- and when it is the one that has gone, running
what was typed would act on whatever took its place."
  (let ((org-foresight-work '(("09:00" . "17:00")))
        (org-foresight-workdays '(0 1 2 3 4 5 6))
        (org-foresight--shape-cache nil))
    (org-foresight-test--with-agenda
        (concat "* NEXT alpha task\nSCHEDULED: "
                (org-foresight-test--stamp 0 "10:00" "11:00") "\n"
                "* NEXT beta task\nSCHEDULED: "
                (org-foresight-test--stamp 0 "13:00" "14:00") "\n")
      (unwind-protect
          (cl-letf (((symbol-function 'org-foresight-observe--get-json)
                     (lambda (&rest _) nil)))
            (let ((org-agenda-sticky nil))
              (org-foresight-test--agenda)
              (with-current-buffer org-agenda-buffer-name
                (goto-char (point-min))
                (re-search-forward "alpha task")
                (beginning-of-line)
                (let ((keep (point)))
                  (org-foresight-test--cut-heading
                   (get-text-property (point) 'org-hd-marker) "alpha task")
                  (goto-char keep))
                ;; the row now lies, and says so
                (should (eq 'lies (org-foresight-agenda--row-state)))
                (let ((this-command 'org-agenda-clock-in))
                  (org-foresight-agenda--freshen)
                  (should (eq this-command #'ignore)))
                ;; the ghost is gone and the cursor holds nothing
                (should (= (point) (point-min)))
                (goto-char (point-min))
                (should-not (re-search-forward "alpha task" nil t)))))
        (dolist (buf (buffer-list))
          (when (and (buffer-live-p buf)
                     (with-current-buffer buf (derived-mode-p 'org-agenda-mode)))
            (kill-buffer buf)))))))

(ert-deftest org-foresight-test-the-ways-an-entry-actually-leaves-are-noticed ()
  "Archiving and refiling are how entries leave, and the row says so after.

The mechanism is tested elsewhere against a bare `delete-region\\=', which is
not what anybody types.  These are the two commands that do it in practice."
  (let ((archive (make-temp-file "org-foresight-archive" nil ".org"))
        (org-foresight-work '(("09:00" . "17:00")))
        (org-foresight-workdays '(0 1 2 3 4 5 6))
        (org-foresight--shape-cache nil))
    (unwind-protect
        (dolist (leaving (list 'archive 'refile))
          (org-foresight-test--with-agenda
              (concat "* NEXT alpha task\nSCHEDULED: "
                      (org-foresight-test--stamp 0 "10:00" "11:00") "\n"
                      "* somewhere else\n")
            (unwind-protect
                (cl-letf (((symbol-function 'org-foresight-observe--get-json)
                           (lambda (&rest _) nil)))
                  (let ((org-agenda-sticky nil)
                        (org-archive-location (concat archive "::")))
                    (org-foresight-test--agenda)
                    (with-current-buffer (find-file-noselect (car org-agenda-files))
                      (goto-char (point-min))
                      (re-search-forward "^\\* NEXT alpha")
                      (beginning-of-line)
                      (pcase leaving
                        ('archive (org-archive-subtree))
                        ('refile
                         (org-refile
                          nil nil
                          (list "somewhere else" (buffer-file-name) nil
                                (save-excursion
                                  (goto-char (point-min))
                                  (re-search-forward "^\\* somewhere else")
                                  (line-beginning-position))))))
                      (set-buffer-modified-p nil))
                    (with-current-buffer org-agenda-buffer-name
                      (goto-char (point-min))
                      (re-search-forward "alpha task")
                      (beginning-of-line)
                      (should (eq 'lies (org-foresight-agenda--row-state))))))
              (dolist (buf (buffer-list))
                (when (and (buffer-live-p buf)
                           (with-current-buffer buf
                             (derived-mode-p 'org-agenda-mode)))
                  (kill-buffer buf))))))
      (when (get-file-buffer archive) (kill-buffer (get-file-buffer archive)))
      (delete-file archive))))

(ert-deftest org-foresight-test-finishing-a-task-costs-no-keystroke ()
  "Marking something done leaves the next key alone.

`org-todo\\=' rewrites a heading by deleting it and putting it back, which is
a deletion taking a heading with it and cannot be told from one at the moment
it happens.  Left there, every state change would eat the keystroke after it
and blame a page that is perfectly good.  The row is asked as well as the
count, and this row still names its own entry."
  (let ((org-foresight-work '(("09:00" . "17:00")))
        (org-foresight-workdays '(0 1 2 3 4 5 6))
        (org-log-done 'time)
        (org-foresight--shape-cache nil))
    (org-foresight-test--with-agenda
        (concat "* NEXT alpha task\nSCHEDULED: "
                (org-foresight-test--stamp 0 "10:00" "11:00") "\n"
                "* NEXT beta task\nSCHEDULED: "
                (org-foresight-test--stamp 0 "13:00" "14:00") "\n")
      (unwind-protect
          (cl-letf (((symbol-function 'org-foresight-observe--get-json)
                     (lambda (&rest _) nil)))
            (let ((org-agenda-sticky nil))
              (org-foresight-test--agenda)
              ;; beta is finished in the file
              (with-current-buffer (find-file-noselect (car org-agenda-files))
                (goto-char (point-min))
                (re-search-forward "^\\* NEXT beta")
                (org-todo "DONE")
                (set-buffer-modified-p nil))
              ;; the keystroke on alpha is the person's: alpha's row still
              ;; names alpha, whatever happened to beta
              (with-current-buffer org-agenda-buffer-name
                (goto-char (point-min))
                (re-search-forward "alpha task")
                (beginning-of-line)
                (let ((this-command 'org-agenda-clock-in))
                  (org-foresight-agenda--freshen)
                  (should (eq this-command 'org-agenda-clock-in))))))
        (dolist (buf (buffer-list))
          (when (and (buffer-live-p buf)
                     (with-current-buffer buf (derived-mode-p 'org-agenda-mode)))
            (kill-buffer buf)))))))

(defun org-foresight-test--stranded-marker ()
  "Cut the first heading of the agenda file and return a marker left behind.
The marker names the entry that took its place, which is the whole fault: a
row built from it would say one thing and act on another."
  (with-current-buffer (find-file-noselect (car org-agenda-files))
    (goto-char (point-min))
    (re-search-forward "^\\* NEXT alpha")
    (beginning-of-line)
    (let ((marker (point-marker)))
      (delete-region (point) (progn (org-end-of-subtree t t) (point)))
      (set-buffer-modified-p nil)
      marker)))

(defun org-foresight-test--lying-row (marker)
  "Make this buffer an agenda whose one row says alpha and points at MARKER."
  (org-agenda-mode)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize "  10:00 alpha\n" 'org-hd-marker marker)))
  (goto-char (point-min)))

(defun org-foresight-test--row-saying (text marker)
  "Make this buffer an agenda whose one row is TEXT and points at MARKER."
  (unless (derived-mode-p 'org-agenda-mode) (org-agenda-mode))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize (concat text "\n") 'org-hd-marker marker)))
  (goto-char (point-min)))

(ert-deftest org-foresight-test-a-shortened-row-still-owns-its-entry ()
  "A row shows as much of a heading as its columns allow, and no more.

Required to carry the whole name, every long heading would read as one that
had gone: the page would rebuild under the cursor and put it at the top, on
the way down a list.  Which is what happened -- three times, and then the
bound stopped it, which is the only reason it was survivable."
  (org-foresight-test--with-org
      "* NEXT Prepare the quarterly board pack\n* NEXT Something else\n"
    (let ((marker (with-current-buffer
                      (find-file-noselect (car org-agenda-files))
                    (goto-char (point-min))
                    (re-search-forward "^\\* NEXT Prepare")
                    (beginning-of-line)
                    (point-marker))))
      (with-temp-buffer
        ;; cut to a column, the way every section here cuts a long title
        (org-foresight-test--row-saying
         "  reporting   Prepare the quarterly board   → Draft the summary" marker)
        (should (eq 'agrees (org-foresight-agenda--row-state)))
        ;; cut with the ellipsis Org and the report both use
        (org-foresight-test--row-saying "↳ Prepare the quarterly… +2 by Mon" marker)
        (should (eq 'agrees (org-foresight-agenda--row-state)))
        ;; and a row about something else is still a row about something else
        (org-foresight-test--row-saying "  reporting   Something else" marker)
        (should (eq 'lies (org-foresight-agenda--row-state)))))))

(ert-deftest org-foresight-test-no-row-of-a-drawn-page-reads-as-lost ()
  "Nothing the package itself draws may look like work that has gone.

Every section here cuts titles to keep its columns, and a cut that the row
test cannot see past turns a healthy page into one that rebuilds under the
cursor.  This walks a real page of both kinds and asks every row."
  (org-foresight-test--with-demo
    (cl-letf (((symbol-function 'org-foresight-observe--get-json)
               (lambda (&rest _) nil)))
      (let ((org-agenda-sticky nil)
            (org-agenda-buffer-name "*org-foresight-test-rows*"))
        (unwind-protect
            (progn
              (org-agenda-list nil nil 'day)
              (with-current-buffer org-agenda-buffer-name
                (goto-char (point-min))
                (while (not (eobp))
                  (should-not (eq 'lies (org-foresight-agenda--row-state)))
                  (forward-line 1)))
              (setq org-foresight--signals-cache nil)
              (org-foresight-board)
              (with-current-buffer "*Org Foresight Board*"
                (goto-char (point-min))
                (while (not (eobp))
                  (should-not (eq 'lies (org-foresight-agenda--row-state)))
                  (forward-line 1))))
          (dolist (name (list org-agenda-buffer-name "*Org Foresight Board*"))
            (when (get-buffer name) (kill-buffer name))))))))

(ert-deftest org-foresight-test-a-rebuild-that-fails-is-said-once ()
  "A page that cannot be rebuilt says so, and then lets the keyboard work.

Asking again at every keystroke would put the same error in front of every
one of them, which is worse than the staleness: nothing could be done at all,
including pressing the redo key by hand."
  (org-foresight-test--with-org "* NEXT alpha\n* NEXT beta\n"
   (with-temp-buffer
    (let ((tried 0))
      (org-foresight-test--lying-row (org-foresight-test--stranded-marker))
      (cl-letf (((symbol-function 'org-agenda-redo)
                 (lambda (&rest _) (setq tried (1+ tried)) (error "no"))))
        (let ((this-command 'org-agenda-clock-in))
          (org-foresight-agenda--freshen))
        (should (= 1 tried))
        ;; the next keystroke is the person's again
        (let ((this-command 'org-agenda-clock-in))
          (org-foresight-agenda--freshen)
          (should (eq this-command 'org-agenda-clock-in)))
        (should (= 1 tried)))))))

(ert-deftest org-foresight-test-the-cure-can-be-switched-off ()
  "With `org-foresight-agenda-refresh-stale\\=' nil, nothing is taken over.
The keystroke runs as typed and the page is left as it is, which is the
setting for somebody who would rather press the redo key themselves."
  (org-foresight-test--with-org "* NEXT alpha\n* NEXT beta\n"
   (with-temp-buffer
    (let ((org-foresight-agenda-refresh-stale nil)
          (tried 0))
      (org-foresight-test--lying-row (org-foresight-test--stranded-marker))
      (cl-letf (((symbol-function 'org-agenda-redo)
                 (lambda (&rest _) (setq tried (1+ tried)))))
        (let ((this-command 'org-agenda-clock-in))
          (org-foresight-agenda--freshen)
          (should (eq this-command 'org-agenda-clock-in))))
      (should (= 0 tried))))))

(ert-deftest org-foresight-test-a-fresh-agenda-is-left-alone ()
  "A row that answers for itself is left alone, and so is the redo key."
  (org-foresight-test--with-org "* NEXT alpha\n* NEXT beta\n"
   (with-temp-buffer
     (org-agenda-mode)
     ;; a row carrying nothing claims nothing
     (let ((this-command 'org-agenda-clock-in))
       (org-foresight-agenda--freshen)
       (should (eq this-command 'org-agenda-clock-in)))
     ;; and the redo key is never dropped, or the cure could not be reached
     (org-foresight-test--lying-row (org-foresight-test--stranded-marker))
     (let ((tried 0))
       (cl-letf (((symbol-function 'org-agenda-redo)
                  (lambda (&rest _) (setq tried (1+ tried)))))
         (let ((this-command 'org-agenda-redo))
           (org-foresight-agenda--freshen)
           (should (eq this-command 'org-agenda-redo)))
         (should (= 0 tried)))))))

(ert-deftest org-foresight-test-a-page-that-cannot-be-mended-is-handed-back ()
  "After enough rebuilds that changed nothing, the page is left as it is.

The row test is exact about the fault and could still be wrong about a row.
Were it wrong about every row, every keystroke would buy a rebuild and the
page could never be used at all -- worse than the staleness, and harder to
see.  Three, a word about it, and the keyboard is the person's again."
  (org-foresight-test--with-org "* NEXT alpha\n* NEXT beta\n"
   (with-temp-buffer
     (let ((marker (org-foresight-test--stranded-marker))
           (tried 0))
       (org-foresight-test--lying-row marker)
       (cl-letf (((symbol-function 'org-agenda-redo)
                  ;; a rebuild that mends nothing, which is the case being
                  ;; guarded against
                  (lambda (&rest _) (setq tried (1+ tried))
                    (org-foresight-test--lying-row marker))))
         (dotimes (_ 6)
           (let ((this-command 'org-agenda-clock-in))
             (org-foresight-agenda--freshen)))
         (should (= org-foresight-agenda--cures-allowed tried))
         ;; and from here the keystroke is the person's
         (let ((this-command 'org-agenda-clock-in))
           (org-foresight-agenda--freshen)
           (should (eq this-command 'org-agenda-clock-in))))))))

(defun org-foresight-test--answer-afk (offset spans)
  "Return a stand-in for `org-foresight-observe--get-json\='.

SPANS describe the afk watcher\='s day OFFSET days back, each one a list
(STATUS H1 M1 H2 M2) with STATUS either \"afk\" or \"not-afk\".  Built against
the real day rather than a fixed date, because `org-foresight--day-start\='
reads the clock and a fixture pinned to August would line up with nothing."
  (let* ((day (org-foresight--day-start offset))
         (at (lambda (h m)
               (time-add day (seconds-to-time (+ (* 3600 h) (* 60 m))))))
         (events
          (mapcar
           (lambda (span)
             (let ((from (funcall at (nth 1 span) (nth 2 span)))
                   (to (funcall at (nth 3 span) (nth 4 span))))
               `((timestamp . ,(format-time-string "%Y-%m-%dT%H:%M:%S%:z" from))
                 (duration . ,(float-time (time-subtract to from)))
                 (data . ((status . ,(nth 0 span)))))))
           spans)))
    (lambda (path &rest _)
      (if (string-match-p "/events" path)
          events
        '((aw-watcher-afk_test . t))))))

(defun org-foresight-test--today-ivs (&rest specs)
  "Build intervals on today from SPECS, each a list (H1 M1 H2 M2)."
  (let ((day (org-foresight--day-start 0)))
    (mapcar (lambda (s)
              (cons (time-add day (seconds-to-time
                                   (+ (* 3600 (nth 0 s)) (* 60 (nth 1 s)))))
                    (time-add day (seconds-to-time
                                   (+ (* 3600 (nth 2 s)) (* 60 (nth 3 s)))))))
            specs)))

(ert-deftest org-foresight-test-day-split-measures-only-the-working-hours ()
  "A machine left on overnight is not a day that leaked sixteen hours.

What leak and lost become is a reserve held back from the working day, so an
hour that was never part of that day cannot be held back from it.  Measured
over the whole calendar day instead, an idle evening is indistinguishable
from an afternoon away from the desk, and the reserve grows past the span it
is subtracted from -- which is how it reached ten hours."
  (let ((org-foresight-work '(("09:00" . "12:00") ("13:00" . "17:00")))
        ;; Every weekday, so the test does not depend on which one it runs on.
        (org-foresight-workdays '(0 1 2 3 4 5 6))
        (org-foresight--shape-cache nil))
    (cl-letf (((symbol-function 'org-foresight-observe--get-json)
               (org-foresight-test--answer-afk
                0 '(("not-afk" 9 0 10 0)     ; working, unclocked
                     ("afk" 12 0 13 0)       ; lunch, outside the working hours
                     ("afk" 14 0 15 0)       ; away from the desk, in them
                     ("not-afk" 20 0 22 0))))) ; the evening, outside them
      (should (equal (org-foresight-observe-day-split 0 nil)
                     '(60.0 . 60.0))))))

(ert-deftest org-foresight-test-day-split-has-no-sample-without-a-watcher ()
  "No events at all is \"no sample\", not a day that leaked nothing.

Asked of the working hours after the cut, a day spent entirely away from the
desk and a day the server never ran would both come back empty, and the
median would learn from the second as if it were the first."
  (let ((org-foresight-work '(("09:00" . "17:00")))
        (org-foresight-workdays '(0 1 2 3 4 5 6))
        (org-foresight--shape-cache nil))
    (cl-letf (((symbol-function 'org-foresight-observe--get-json)
               (org-foresight-test--answer-afk 0 nil)))
      (should-not (org-foresight-observe-day-split 0 nil)))
    ;; Away all through the working hours is a sample, and says so.
    (cl-letf (((symbol-function 'org-foresight-observe--get-json)
               (org-foresight-test--answer-afk 0 '(("afk" 9 0 17 0)))))
      (should (equal (org-foresight-observe-day-split 0 nil) '(0.0 . 480.0))))))

(ert-deftest org-foresight-test-day-split-does-not-charge-a-meeting-twice ()
  "An hour the day had already spoken for is not also an hour that went missing.

A meeting is taken out of the span before any of this is asked.  Counted
again as time away from the desk it is held back twice -- once as the
appointment it was, once as an hour nobody can account for -- and the reserve
grows by exactly the calendar the day was planned around."
  (let ((org-foresight-work '(("09:00" . "17:00")))
        (org-foresight-workdays '(0 1 2 3 4 5 6))
        (org-foresight--shape-cache nil))
    (cl-letf (((symbol-function 'org-foresight-observe--get-json)
               (org-foresight-test--answer-afk
                0 '(("afk" 10 0 11 0)      ; in the meeting, away from the desk
                    ("afk" 14 0 15 0)))))  ; wandering, on nobody's calendar
      ;; Only the hour nothing was planned in is lost.
      (should (equal (org-foresight-observe-day-split
                      0 nil (org-foresight-test--today-ivs '(10 0 11 0)))
                     '(0.0 . 60.0)))
      ;; Without it, both hours are.
      (should (equal (org-foresight-observe-day-split 0 nil nil)
                     '(0.0 . 120.0))))))

(ert-deftest org-foresight-test-day-split-subtracts-an-overlap-once ()
  "A meeting that was also clocked is removed once, not twice.

Both are set differences against the same stretch, so the union is what is
taken out.  Subtracting each in turn would take an hour away twice and hand
back a negative minute count, which no median can survive."
  (let ((org-foresight-work '(("09:00" . "17:00")))
        (org-foresight-workdays '(0 1 2 3 4 5 6))
        (org-foresight--shape-cache nil))
    (cl-letf (((symbol-function 'org-foresight-observe--get-json)
               (org-foresight-test--answer-afk 0 '(("afk" 10 0 11 0)))))
      (should (equal (org-foresight-observe-day-split
                      0
                      (org-foresight-test--today-ivs '(10 0 11 0))
                      (org-foresight-test--today-ivs '(10 0 11 0)))
                     '(0.0 . 0.0))))))

(ert-deftest org-foresight-test-learn-leak-hands-over-what-the-day-had-booked ()
  "The window's own calendar reaches the measurement.

`org-foresight-observe-day-split' cannot know what a past day had booked --
it can see the clock and the watcher and nothing else.  The command that
calls it can, and a correction that stops inside the function it was written
in is a correction nobody ever gets."
  (org-foresight-test--with-org
      (concat "* Standup\n:PROPERTIES:\n:CATEGORY: meeting\n:END:\n"
              (org-foresight-test--stamp 0 "10:00" "11:00") "\n")
    (let ((org-foresight-work '(("09:00" . "17:00")))
          (org-foresight-workdays '(0 1 2 3 4 5 6))
          (org-foresight--shape-cache nil)
          (org-foresight-leak-cache-file (make-temp-file "org-foresight-leak" nil ".eld"))
          (seen 'never-called))
      (unwind-protect
          (cl-letf (((symbol-function 'org-foresight-observe-day-split)
                     (lambda (_offset _clocked &optional occupied)
                       (setq seen occupied)
                       (cons 0.0 0.0))))
            (org-foresight-learn-leak 1)
            (should (equal (org-foresight-test--hhmm seen) '("10:00-11:00"))))
        (delete-file org-foresight-leak-cache-file)))))

(ert-deftest org-foresight-test-day-split-is-silent-on-a-day-with-no-work ()
  "A day declaring no working hours has nothing to hold a reserve against."
  (let ((org-foresight-work nil)
        (org-foresight-workdays '(0 1 2 3 4 5 6))
        (org-foresight--shape-cache nil))
    (cl-letf (((symbol-function 'org-foresight-observe--get-json)
               (org-foresight-test--answer-afk 0 '(("not-afk" 9 0 17 0)))))
      (should-not (org-foresight-observe-day-split 0 nil)))))

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
           ;; Read at the day's own first minute.  Capacity counts what is
           ;; still ahead, so a day asked about from outside it has nothing
           ;; booked and nothing to say.
           (bare (let ((org-foresight--shape-cache nil))
                   (plist-get (org-foresight-capacity day nil day) :booked-min))))
      (let ((org-foresight-check-in '(:minutes 10 :title "in"))
            (org-foresight-check-out '(:minutes 10 :title "out"))
            (org-foresight--shape-cache nil))
        (should (= (+ bare 20)
                   (plist-get (org-foresight-capacity day nil day)
                              :booked-min)))))))

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

(ert-deftest org-foresight-test-the-board-keeps-its-sections-in-order ()
  "The board's sections, and the order the questions are asked in.

Written before anything is added to it.  The order is the argument -- can I
leave, is everything moving, will the dates be met, what is unsettled -- and
a section that drifted up or down the page would be a different argument
made by accident."
  (org-foresight-test--with-places
      "* NEXT stamp the form\n:PROPERTIES:\n:PLACE: office\n:END:\n"
    (unwind-protect
        (progn
          (org-foresight-board)
          (with-current-buffer "*Org Foresight Board*"
            (let* ((text (substring-no-properties (buffer-string)))
                   (at (lambda (name) (string-search name text))))
              (dolist (name '("Board" "Here" "Projects" "Landing"
                              "Load" "Parked" "Signals"))
                (should (funcall at name)))
              ;; can I leave · is everything moving · will the dates be met ·
              ;; what are the coming days like · is this still rightly down ·
              ;; what is unsettled
              (should (< (funcall at "Board") (funcall at "Here")))
              (should (< (funcall at "Here") (funcall at "Projects")))
              (should (< (funcall at "Projects") (funcall at "Landing")))
              (should (< (funcall at "Landing") (funcall at "Load")))
              (should (< (funcall at "Load") (funcall at "Parked")))
              (should (< (funcall at "Parked") (funcall at "Signals"))))))
      (when (get-buffer "*Org Foresight Board*")
        (kill-buffer "*Org Foresight Board*")))))

(ert-deftest org-foresight-test-the-board-walks-the-files-once ()
  "Seven sections, one walk.

Every section wants the same three answers -- the survey, the records, the
signals -- and a page that asked for them section by section would walk every
heading in every file once per section.  They are read at the top and handed
down."
  (org-foresight-test--with-places
      "* NEXT stamp the form\n:PROPERTIES:\n:PLACE: office\n:END:\n"
    (unwind-protect
        (let ((walks 0) (surveys 0))
          (cl-letf* ((compute (symbol-function 'org-foresight--signals-compute))
                     ((symbol-function 'org-foresight--signals-compute)
                      (lambda (&rest args)
                        (setq walks (1+ walks))
                        (apply compute args)))
                     (survey (symbol-function 'org-foresight-scan))
                     ((symbol-function 'org-foresight-scan)
                      (lambda (&rest args)
                        (setq surveys (1+ surveys))
                        (apply survey args))))
            ;; nothing carried over from another test's page
            (setq org-foresight--signals-cache nil)
            (org-foresight-board)
            (should (= 1 walks))
            (should (= 1 surveys))))
      (when (get-buffer "*Org Foresight Board*")
        (kill-buffer "*Org Foresight Board*")))))

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

;;;; Projects and deadlines

(defun org-foresight-test--classify ()
  "Return (TITLE . CLASS) for every heading of the current agenda files.

Keyword-less headings are in the list too, as `not-todo', read straight off
the buffer rather than off the scan -- the scan answers for them by leaving
them out, and a test that only saw the scan could not tell \"classified as
scaffolding\" from \"missed entirely\"."
  (let ((byline (make-hash-table :test #'equal)))
    (dolist (rec (plist-get (org-foresight-project-scan) :headings))
      (puthash (plist-get rec :title)
               (cond ((plist-get rec :deadline-project-p) 'deadline-project)
                     ((plist-get rec :project-p) 'project)
                     (t 'task))
               byline))
    (with-current-buffer (find-file-noselect (car org-agenda-files))
      (org-with-wide-buffer
       (let (out)
         (org-map-entries
          (lambda ()
            (let ((title (org-get-heading t t t t)))
              (push (cons title (gethash title byline 'not-todo)) out)))
          nil nil)
         (nreverse out))))))

(defconst org-foresight-test--project-fixture "\
* this is not project because no todo state

** NEXT this is not project because no childlen

** NEXT this is non deadline project because have todo state and child todo

*** NEXT task a

** NEXT this is deadline project
DEADLINE: %s

*** NEXT task b

** NEXT this is deadline project with grandchild (or more) todo
DEADLINE: %s

*** NEXT task c

**** NEXT task d

** NEXT this is deadline project because a child todo has deadline

*** NEXT task e
DEADLINE: %s

**** NEXT task f

** NEXT this is non deadline project because childlen todo does not have deadline 1

*** NEXT this is deadline project because a child todo has deadline

**** NEXT task g
DEADLINE: %s

** NEXT this is non deadline project because childlen todo does not have deadline 2

*** NEXT this is deadline project because a child project directly has deadline

**** NEXT this is also deadline project because has child todo and itself or the child has deadline
DEADLINE: %s

***** NEXT task h
DEADLINE: %s

** this is not project or task because no todo state

*** NEXT this is non deadline project because has grandchild (or more) todo or project

**** this is not project or task

***** NEXT task i
"
  "The author's own specification of the rules, as an Org file.

Verbatim but for two edits that cost nothing and buy a test: the repeated
title `task' is numbered, and the two identically-titled `non deadline
project' headings are, so that a classification can be keyed by title.  The
dates are `%s' for `org-foresight-test--stamp' -- a literal date in a test
is a test that expires.")

(ert-deftest org-foresight-test-the-fixture-classifies-every-heading ()
  "The author's specification, asserted heading by heading.

This is the only canonical statement of the rules; everything else in this
section tests one edge of what this states whole.  Asserted as a single
comparison so that a failure names every heading that moved, rather than
stopping at the first.

Two headings the author labelled `task' are projects, and the rule they
wrote is what makes them so: `task c' has `task d' under it, and `task e'
has `task f'.  A heading with TODO children is a project whatever its title
says.  `task e' is further a deadline project nested inside another one,
which is ordinary and not a special case."
  (let ((due (org-foresight-test--stamp 1)))
    (org-foresight-test--with-org
        (format org-foresight-test--project-fixture due due due due due due)
      (should (equal
               '(("this is not project because no todo state" . not-todo)
                 ("this is not project because no childlen" . task)
                 ("this is non deadline project because have todo state and child todo" . project)
                 ("task a" . task)
                 ("this is deadline project" . deadline-project)
                 ("task b" . task)
                 ("this is deadline project with grandchild (or more) todo" . deadline-project)
                 ("task c" . project)
                 ("task d" . task)
                 ("this is deadline project because a child todo has deadline" . deadline-project)
                 ("task e" . deadline-project)
                 ("task f" . task)
                 ("this is non deadline project because childlen todo does not have deadline 1" . project)
                 ("this is deadline project because a child todo has deadline" . deadline-project)
                 ("task g" . task)
                 ("this is non deadline project because childlen todo does not have deadline 2" . project)
                 ("this is deadline project because a child project directly has deadline" . deadline-project)
                 ("this is also deadline project because has child todo and itself or the child has deadline" . deadline-project)
                 ("task h" . task)
                 ("this is not project or task because no todo state" . not-todo)
                 ("this is non deadline project because has grandchild (or more) todo or project" . project)
                 ("this is not project or task" . not-todo)
                 ("task i" . task))
               (org-foresight-test--classify))))))

(ert-deftest org-foresight-test-a-keywordless-heading-is-transparent ()
  "A grouping heading is a hole for ownership and a wall for extent.

Both halves come from one line of the walk -- pop by level always, push only
what has a keyword -- and both are needed.  Transparent, so a TODO under a
grouping still finds its TODO grandparent; a wall, so the grouping's own
subtree ends where the outline says it does."
  (org-foresight-test--with-org
      "* NEXT root
** notes
*** NEXT buried
"
    (let* ((recs (plist-get (org-foresight-project-scan) :headings))
           (root (seq-find (lambda (r) (equal (plist-get r :title) "root")) recs))
           (buried (seq-find (lambda (r) (equal (plist-get r :title) "buried")) recs)))
      ;; the grouping got no record at all
      (should (= 2 (length recs)))
      (should (plist-get root :project-p))
      (should (plist-get buried :leaf-p))
      ;; and the link skipped straight past the grouping
      (should (eq root (plist-get buried :todo-parent))))))

(ert-deftest org-foresight-test-a-sibling-does-not-become-a-parent ()
  "The stack is popped by level for every heading, keyword or not.

Without the unconditional pop, a keyword-less heading closing one subtree
leaves that subtree's TODO on the stack, and a later heading takes it as a
parent -- adopting across two branches that share nothing at all.

`third' has to be *deeper* than the heading left stale for this to bite, or
its own pop clears the stack before it looks.  That is why it is a `**'
under a `*' grouping: written flat, the test passes against a walk that
never pops for keyword-less headings, and says nothing."
  (org-foresight-test--with-org
      "* NEXT first
** NEXT inside first
* second is not a todo
** NEXT third
"
    (let* ((recs (plist-get (org-foresight-project-scan) :headings))
           (third (seq-find (lambda (r) (equal (plist-get r :title) "third")) recs))
           (first (seq-find (lambda (r) (equal (plist-get r :title) "first")) recs)))
      (should (null (plist-get third :todo-parent)))
      (should (plist-get third :leaf-p))
      ;; and the one that really does have a child still has it
      (should (plist-get first :project-p)))))

(ert-deftest org-foresight-test-a-deadline-stops-one-generation-up ()
  "A DEADLINE reaches the project it is written under, and no further.

The grandparent is a container for a tree that has a date; it is not itself
the thing that is due.  Letting the date travel all the way up would make
one deadline the due date of every tree the work is filed in, and the
outermost heading of a file would end up due whenever anything in it was."
  (org-foresight-test--with-org
      (format "* NEXT outer
** NEXT middle
*** NEXT leaf
DEADLINE: %s
" (org-foresight-test--stamp 1))
    (let ((recs (plist-get (org-foresight-project-scan) :headings)))
      (pcase-let ((`(,outer ,middle ,leaf) recs))
        (should (plist-get outer :project-p))
        (should-not (plist-get outer :deadline-project-p))
        (should (plist-get middle :deadline-project-p))
        (should (plist-get leaf :leaf-p))))))

(ert-deftest org-foresight-test-a-child-project-s-own-deadline-does-reach-up ()
  "The other half of the same rule, and the case that fixes its shape.

Here the DEADLINE is written on a heading that is itself a project, and that
heading is a TODO child of the one above -- so the one above is a deadline
project.  Only one generation again; it is the same rule, and the pair of
tests is what says it is not `never travels' and not `always travels'."
  (org-foresight-test--with-org
      (format "* NEXT outer
** NEXT middle
DEADLINE: %s
*** NEXT leaf
" (org-foresight-test--stamp 1))
    (let ((recs (plist-get (org-foresight-project-scan) :headings)))
      (pcase-let ((`(,outer ,middle ,_leaf) recs))
        (should (plist-get outer :deadline-project-p))
        (should (plist-get middle :deadline-project-p))))))

(ert-deftest org-foresight-test-a-done-child-still-makes-a-project ()
  "Finishing the last child does not turn a project into a task.

The outline's shape is a fact about the file, not about progress.  A
classification that flickered as work was closed would be unusable as a
key -- the same heading would answer differently on Tuesday and Wednesday
having never been edited."
  (org-foresight-test--with-org
      "* NEXT parent
** DONE finished
"
    (let ((recs (plist-get (org-foresight-project-scan) :headings)))
      (should (plist-get (car recs) :project-p))
      (should (plist-get (cadr recs) :done)))))

(ert-deftest org-foresight-test-projects-do-not-cross-files ()
  "Containment is per file: the stack is emptied between them.

Two agenda files are two outlines, not one long one, and a heading at the
top of the second must not be adopted by whatever the first file happened to
end on."
  (let ((a (make-temp-file "org-foresight-a" nil ".org" "* NEXT deep in a\n** NEXT child\n"))
        (b (make-temp-file "org-foresight-b" nil ".org" "*** NEXT top of b\n")))
    (unwind-protect
        (let ((org-agenda-files (list a b))
              (org-todo-keywords '((sequence "NEXT" "|" "DONE"))))
          (let* ((recs (plist-get (org-foresight-project-scan) :headings))
                 (top (seq-find (lambda (r) (equal (plist-get r :title) "top of b"))
                                recs)))
            (should top)
            (should (null (plist-get top :todo-parent)))
            (should (plist-get top :leaf-p))))
      (dolist (f (list a b))
        (when (get-file-buffer f) (kill-buffer (get-file-buffer f)))
        (delete-file f)))))

(defmacro org-foresight-test--with-units (text &rest body)
  "Run BODY over TEXT with the estimate correction off.

The correction is learned from history and would put a multiplier on every
figure here; these tests are about which minutes are counted, not about how
far estimates run over, and that has its own tests."
  (declare (indent 1))
  `(org-foresight-test--with-org ,text
     (let ((org-foresight-bias-enabled nil))
       ,@body)))

(defun org-foresight-test--units ()
  "Return (TITLE MINUTES LEAVES) for each unit, earliest due first."
  (mapcar (lambda (u) (list (plist-get u :title)
                            (plist-get u :remaining-min)
                            (plist-get u :leaves)))
          (plist-get (org-foresight-project-scan) :units)))

(ert-deftest org-foresight-test-a-unit-sums-its-leaves-not-its-own-estimate ()
  "The demand is the work inside, not an estimate written on the container.

A project heading may carry an EFFORT -- people put one there as a note to
themselves -- and counting it as well as its children's would charge the
same work twice, once wholesale and once in parts."
  (org-foresight-test--with-units
      (format "* NEXT the project
DEADLINE: %s
:PROPERTIES:
:EFFORT:   9:00
:END:
** NEXT first
:PROPERTIES:
:EFFORT:   1:00
:END:
** NEXT second
:PROPERTIES:
:EFFORT:   0:30
:END:
" (org-foresight-test--stamp 3))
    (should (equal '(("the project" 90.0 2)) (org-foresight-test--units)))))

(ert-deftest org-foresight-test-a-leaf-belongs-to-its-nearest-deadline-project ()
  "Nested deadline projects each take their own leaves, and no leaf twice.

This is what lets the units be added up.  The outer project is due as well,
and its demand is what is filed directly under it -- the inner project's
hours answer to the inner project's date, which is the date that will
actually be missed if they are not done."
  (org-foresight-test--with-units
      (format "* NEXT outer
DEADLINE: %s
** NEXT directly under outer
:PROPERTIES:
:EFFORT:   1:00
:END:
** NEXT inner
DEADLINE: %s
*** NEXT under inner
:PROPERTIES:
:EFFORT:   2:00
:END:
" (org-foresight-test--stamp 5) (org-foresight-test--stamp 2))
    ;; inner is due first, so it sorts first
    (should (equal '(("inner" 120.0 1) ("outer" 60.0 1))
                   (org-foresight-test--units)))))

(ert-deftest org-foresight-test-done-work-is-not-demand ()
  "Three ways of being finished, and none of them is owed.

A done leaf, a leaf under a done project, and a project whose leaves are all
done.  The last is dropped entirely: it is not a deadline that will land, it
is one that has, and leaving it in would pad every count of what is still
outstanding."
  (org-foresight-test--with-units
      (format "* NEXT live
DEADLINE: %s
** NEXT open
:PROPERTIES:
:EFFORT:   1:00
:END:
** DONE closed
:PROPERTIES:
:EFFORT:   4:00
:END:
* DONE finished project
DEADLINE: %s
** NEXT stranded under a done parent
:PROPERTIES:
:EFFORT:   8:00
:END:
* NEXT all its work is done
DEADLINE: %s
** DONE the only leaf
:PROPERTIES:
:EFFORT:   8:00
:END:
" (org-foresight-test--stamp 3) (org-foresight-test--stamp 3)
  (org-foresight-test--stamp 3))
    (should (equal '(("live" 60.0 1)) (org-foresight-test--units)))))

(ert-deftest org-foresight-test-private-work-is-not-demand ()
  "Life is not a deliverable, and its hours are already spoken for.

Twice over, which is the arithmetic reason as well as the obvious one: the
supply side subtracts private commitments from the hours available before
anything is asked of them, so counting them as demand would charge the same
hours to both sides of the comparison."
  (org-foresight-test--with-units
      (format "* NEXT move house
DEADLINE: %s
:PROPERTIES:
:CATEGORY: family
:END:
** NEXT pack the kitchen
:PROPERTIES:
:EFFORT:   4:00
:END:
* NEXT dentist
DEADLINE: %s
:PROPERTIES:
:CATEGORY: family
:EFFORT:   2:00
:END:
* NEXT the audit
DEADLINE: %s
** NEXT read the file
:PROPERTIES:
:EFFORT:   1:00
:END:
** NEXT school run
:PROPERTIES:
:CATEGORY: family
:EFFORT:   3:00
:END:
" (org-foresight-test--stamp 3) (org-foresight-test--stamp 3)
  (org-foresight-test--stamp 3))
    (let ((org-foresight-private-categories '("family")))
      ;; A private tree is not a unit; a lone private commitment with a date
      ;; of its own is not one either; and a private errand filed inside a
      ;; work project does not add its hours to that project's demand.  The
      ;; last is the one that needs saying: the project is work, so it is
      ;; counted, and only the leaf's own category keeps the errand out.
      (should (equal '(("the audit" 60.0 1)) (org-foresight-test--units))))))

(ert-deftest org-foresight-test-a-leaf-with-no-estimate-is-counted-and-owned-up-to ()
  "An unestimated leaf is a guess, not a zero, and the guess is declared.

Falling back to the default keeps the arithmetic honest -- work with no
figure on it still takes hours -- but a total built out of defaults is a
different kind of number from one built out of estimates, and a reader who
is not told cannot tell them apart."
  (org-foresight-test--with-units
      (format "* NEXT the project
DEADLINE: %s
** NEXT measured
:PROPERTIES:
:EFFORT:   1:00
:END:
** NEXT guessed at
" (org-foresight-test--stamp 3))
    (let ((u (car (plist-get (org-foresight-project-scan) :units))))
      ;; 1:00 measured plus the 0:30 default, not 1:00 plus nothing
      (should (= 90.0 (plist-get u :remaining-min)))
      (should (= 1 (plist-get u :unestimated))))))

(ert-deftest org-foresight-test-a-lone-dated-task-is-its-own-unit ()
  "A commitment with a date does not need children to be a commitment.

It is not a project and it is counted anyway: an invoice due Friday takes
its hour out of the same week whether or not anybody broke it into steps.
Excluding it would make every answer about the week optimistic by exactly
the work nobody decomposed."
  (org-foresight-test--with-units
      (format "* NEXT send the invoice
DEADLINE: %s
:PROPERTIES:
:EFFORT:   1:00
:END:
* NEXT no date on this one
:PROPERTIES:
:EFFORT:   9:00
:END:
" (org-foresight-test--stamp 2))
    (should (equal '(("send the invoice" 60.0 1)) (org-foresight-test--units)))))

(ert-deftest org-foresight-test-an-overdue-unit-is-tested-against-today ()
  "A date that has gone is folded onto today, and says so.

Left where it was written, its window is empty: the shortfall is the whole
of the demand, and every later deadline inherits a debt that can never be
paid, so the view would report the same failure forever.  The work is real
and takes hours that exist -- it is the window that does not."
  (org-foresight-test--with-units
      (format "* NEXT late
DEADLINE: %s
** NEXT still owed
:PROPERTIES:
:EFFORT:   1:00
:END:
" (org-foresight-test--stamp -9))
    (let ((u (car (plist-get (org-foresight-project-scan) :units))))
      (should (plist-get u :overdue))
      (should (equal (org-foresight--day-start 0) (plist-get u :due-day)))
      ;; and the date it was written with is kept, for the row to name
      (should (time-less-p (plist-get u :due) (plist-get u :due-day))))))

(ert-deftest org-foresight-test-a-repeating-deadline-is-due-next-time ()
  "A repeater means the next occurrence, not the day it was first written.

`org-get-deadline-time' answers with the stamp as written.  A weekly review
set up eighteen months ago answers with a date eighteen months gone, which
the overdue rule then folds onto today -- so the whole of it would be
demanded this morning, and again tomorrow, forever.  The remaining effort is
the effort for one occurrence, so one occurrence is what it is tested at."
  (org-foresight-test--with-units
      (format "* NEXT weekly review
DEADLINE: %s
** NEXT write it up
:PROPERTIES:
:EFFORT:   1:00
:END:
" (concat (substring (org-foresight-test--stamp -70) 0 -1) " +1w>"))
    (let ((u (car (plist-get (org-foresight-project-scan) :units))))
      (should-not (plist-get u :overdue))
      (should-not (time-less-p (plist-get u :due-day)
                               (org-foresight--day-start 0))))))

(ert-deftest org-foresight-test-a-project-is-due-when-its-last-part-is ()
  "A project with no date of its own is due at the latest of its parts.

Not the earliest.  A project is not finished until all of it is, and taking
the earliest would pull every undated sibling onto the tightest child's date
and report a shortfall the week does not actually have."
  (org-foresight-test--with-units
      (format "* NEXT the project
** NEXT soon
DEADLINE: %s
:PROPERTIES:
:EFFORT:   1:00
:END:
** NEXT later
DEADLINE: %s
:PROPERTIES:
:EFFORT:   1:00
:END:
" (org-foresight-test--stamp 2) (org-foresight-test--stamp 9))
    (let ((u (car (plist-get (org-foresight-project-scan) :units))))
      (should (equal "the project" (plist-get u :title)))
      ;; `org-foresight--day-start' counts days *back*, so nine days ahead
      ;; is -9.  Written out because the sign is easy to get wrong and a
      ;; wrong sign here reads as the rule being broken.
      (should (equal (org-foresight--day-start -9) (plist-get u :due-day))))))

(defmacro org-foresight-test--with-landing (text &rest body)
  "Run BODY over TEXT with a short, fully predictable working day.

Four hours a day, every day, read from eight in the morning so today counts
whole; no reserve, no estimate correction.  Supply is then exactly four
hours per day between now and any deadline, which is what lets these tests
say what they mean in figures a reader can check by hand."
  (declare (indent 1))
  `(org-foresight-test--with-org ,text
     (let ((org-foresight-awake '("07:00" . "23:00"))
           (org-foresight-work '(("09:00" . "13:00")))
           (org-foresight-workdays '(0 1 2 3 4 5 6))
           (org-foresight--shape-cache nil)
           (org-foresight-day-file nil)
           (org-foresight-bias-enabled nil)
           (org-foresight-surge-cache-file "/nonexistent/surge.eld")
           (org-foresight-leak-cache-file "/nonexistent/leak.eld")
           (org-foresight-surge-default "0:00")
           (org-foresight-leak-default "0:00")
           (org-foresight-lost-default "0:00")
           (org-foresight-check-in nil)
           (org-foresight-check-out nil)
           (org-foresight-now (time-add (org-foresight--day-start 0)
                                        (* 3600 8))))
       ,@body)))

(defun org-foresight-test--verdicts ()
  "Return (DAY-OFFSET VERDICT DEMAND) per deadline, earliest first."
  (let ((today (org-foresight--day-start 0)))
    (mapcar (lambda (e)
              (list (org-foresight--day-of (plist-get e :day) today)
                    (plist-get e :verdict)
                    (plist-get e :demand-min)))
            (plist-get (org-foresight-landing) :deadlines))))

(defun org-foresight-test--dated-tree (title offset effort &rest leaves)
  "Return a project TITLE due OFFSET days out whose leaf needs EFFORT.
LEAVES are extra (TITLE EFFORT SCHEDULED-OFFSET) lists."
  (concat (format "* NEXT %s\nDEADLINE: %s\n" title
                  (org-foresight-test--stamp offset))
          (format "** NEXT %s work\n:PROPERTIES:\n:EFFORT: %s\n:END:\n"
                  title effort)
          (mapconcat
           (lambda (l)
             (format "** NEXT %s\n%s:PROPERTIES:\n:EFFORT: %s\n:END:\n"
                     (nth 0 l)
                     (if (nth 2 l)
                         (format "SCHEDULED: %s\n"
                                 (org-foresight-test--stamp (nth 2 l)))
                       "")
                     (nth 1 l)))
           leaves "")))

(ert-deftest org-foresight-test-longest-sitting-is-a-run-not-a-total ()
  "The bound is the longest unbroken run of working time, not the day's sum.

A day broken for lunch offers two runs, and work longer than the longer of
them has to be interrupted through however many hours the day adds up to.
Taking the total instead would call a seven-and-a-half-hour day room enough
for a six-hour job that no part of that day can actually hold."
  (let ((org-foresight-day-file nil)
        (org-foresight--shape-cache nil)
        (org-foresight-workdays '(0 1 2 3 4 5 6))
        (org-foresight-work '(("09:00" . "12:00") ("13:00" . "17:30"))))
    (should (= 270.0 (org-foresight--longest-sitting))))
  ;; A week is read rather than a day, so which day it is asked on cannot
  ;; change the answer.  Here only one weekday works at all, and the bound
  ;; is still that day's -- asked on any of the other six, a single-day
  ;; reading would return zero and condemn every task on the board.
  (let* ((today (string-to-number (format-time-string "%w")))
         (org-foresight-day-file nil)
         (org-foresight--shape-cache nil)
         ;; Deliberately not today, whatever day the tests are run on: a
         ;; reading that looked only at today would find no hours at all.
         (org-foresight-workdays (list (mod (1+ today) 7)))
         (org-foresight-work '(("09:00" . "12:00") ("13:00" . "17:30"))))
    (should (= 270.0 (org-foresight--longest-sitting)))))

(ert-deftest org-foresight-test-landing-says-which-remedy-a-shortfall-needs ()
  "Short of the time left and short of the whole week want different answers.

Both used to print the same word, which left the reader to work out for
themselves which week they were in -- and the cheaper of the two answers is
the one they would stop looking for.

Short of the hours not already spoken for is settled by moving other work:
the time exists, it is promised elsewhere.  Short of the week with
everything else already cleared out of it is settled by none of that,
because there is nothing left to clear -- it takes overtime, another pair
of hands, or less work."
  ;; Four hours a day and two days to the deadline: eight hours exist, four
  ;; of them are already promised to something else, and five are needed.
  (org-foresight-test--with-landing
      (concat (org-foresight-test--dated-tree "big" 1 "5:00")
              "* NEXT unrelated\nSCHEDULED: " (org-foresight-test--stamp 0)
              "\n:PROPERTIES:\n:EFFORT: 4:00\n:END:\n")
    (should (equal '(defer) (mapcar #'cadr (org-foresight-test--verdicts))))
    (should (string-match-p
             "5:00 owed · 4:00 free of 8:00 · 1:00 must move"
             (substring-no-properties (org-foresight-report-landing)))))
  ;; Twenty hours needed and eight in the window however it is arranged.
  ;; The same four hours are promised elsewhere, so that the two supplies
  ;; differ and the line can be caught showing the wrong one.
  (org-foresight-test--with-landing
      (concat (org-foresight-test--dated-tree "big" 1 "20:00")
              "* NEXT unrelated\nSCHEDULED: " (org-foresight-test--stamp 0)
              "\n:PROPERTIES:\n:EFFORT: 4:00\n:END:\n")
    (should (equal '(over) (mapcar #'cadr (org-foresight-test--verdicts))))
    (should (string-match-p
             "20:00 owed · 4:00 free of 8:00 · 12:00 short"
             (substring-no-properties (org-foresight-report-landing))))))

(ert-deftest org-foresight-test-landing-says-when-a-leaf-is-too-big ()
  "A verdict resting on a leaf nobody can check says so on its own row.

Four hours a day are declared here, so a five-hour leaf cannot be sat down
to once.  Until it is broken up there is no moment before it is finished at
which anybody can say how far along it is -- which makes the figure the
whole row rests on the least checkable kind there is, and the row is where
somebody arrives to fix it."
  (org-foresight-test--with-landing
      (org-foresight-test--dated-tree "big" 3 "5:00")
    (should (string-match-p
             "a leaf over 4:00"
             (substring-no-properties (org-foresight-report-landing)))))
  ;; And stays quiet otherwise: a note on every row is a note nobody reads.
  (org-foresight-test--with-landing
      (org-foresight-test--dated-tree "small" 3 "2:00")
    (should-not (string-match-p
                 "a leaf over"
                 (substring-no-properties (org-foresight-report-landing))))))

(ert-deftest org-foresight-test-landing-tests-cumulatively ()
  "Two commitments that each fit alone need not fit together.

This is the whole reason the test is cumulative, and it is the ordinary way
a fortnight goes wrong rather than a corner case: nothing is individually
impossible, and the month is still lost.  Asked one at a time both answer
yes -- seven hours by Tuesday out of eight available, seven by Wednesday out
of twelve.  Asked in order, Wednesday owes fourteen hours and has twelve."
  (org-foresight-test--with-landing
      (concat (org-foresight-test--dated-tree "first" 1 "7:00")
              (org-foresight-test--dated-tree "second" 2 "7:00"))
    (should (equal '((1 lands 420.0) (2 over 840.0))
                   (org-foresight-test--verdicts)))
    ;; and each one, alone, fits its own window -- which is what a
    ;; per-deadline test would have reported
    (let ((e (cadr (plist-get (org-foresight-landing) :deadlines))))
      (should (> (plist-get e :hard-min) 420.0)))))

(ert-deftest org-foresight-test-landing-reports-the-soonest-failure ()
  "The first date that cannot be met is the one to act on.

A later failure is a consequence of the earlier one as often as not, and the
decision -- work longer, hand some over, do less of it -- has to be taken
before the first date, not the worst."
  (org-foresight-test--with-landing
      (concat (org-foresight-test--dated-tree "early" 1 "9:00")
              (org-foresight-test--dated-tree "late" 5 "1:00"))
    (let* ((l (org-foresight-landing))
           (fail (plist-get l :first-fail)))
      (should fail)
      (should (= 1 (org-foresight--day-of (plist-get fail :day)
                                          (org-foresight--day-start 0))))
      ;; nine hours owed by tomorrow, eight available: one short
      (should (= 60.0 (plist-get fail :short-min)))
      (should (equal "early" (plist-get (car (plist-get fail :units)) :title))))))

(ert-deftest org-foresight-test-soft-and-hard-differ-by-what-can-be-deferred ()
  "Two answers, because there are two different things to do about a shortfall.

Work with no deadline can wait; that is what having no deadline means.  So a
commitment that does not fit beside today's other promises may still fit if
they move, and telling the two apart is the difference between rearranging a
week and giving work away."
  (org-foresight-test--with-landing
      (concat (org-foresight-test--dated-tree "the deadline" 1 "6:00")
              (format "* NEXT undated but promised
SCHEDULED: %s
:PROPERTIES:
:EFFORT: 4:00
:END:
" (org-foresight-test--stamp 0)))
    (let ((e (car (plist-get (org-foresight-landing) :deadlines))))
      ;; eight hours of working time; four are promised to work with no date
      (should (= 240.0 (plist-get e :soft-min)))
      (should (= 480.0 (plist-get e :hard-min)))
      (should (eq 'defer (plist-get e :verdict)))
      (should (= 120.0 (plist-get e :soft-short-min)))
      (should (= 0.0 (plist-get e :short-min))))))

(ert-deftest org-foresight-test-a-unit-s-own-dated-hours-are-not-counted-twice ()
  "A project that has been carefully scheduled must not read as the one in trouble.

`:spare-min' is already net of `:committed-min', so an hour a project has
put in Thursday has left Thursday's spare -- while the same hour is still in
what the project needs.  Compared as they stand, the question asked is
whether the work fits in the hours *not* set aside for it, and the better
the planning the worse the answer.

Here every hour of a nine-hour project is placed inside its own window, on
days that hold it.  It lands.  Without the correction the same arrangement
reads as three hours of room against nine hours of need, and the tool tells
somebody who has planned correctly to delegate."
  (org-foresight-test--with-landing
      (format "* NEXT placed
DEADLINE: %s
** NEXT monday
SCHEDULED: %s
:PROPERTIES:
:EFFORT: 3:00
:END:
** NEXT tuesday
SCHEDULED: %s
:PROPERTIES:
:EFFORT: 3:00
:END:
** NEXT wednesday
SCHEDULED: %s
:PROPERTIES:
:EFFORT: 3:00
:END:
" (org-foresight-test--stamp 2) (org-foresight-test--stamp 0)
  (org-foresight-test--stamp 1) (org-foresight-test--stamp 2))
    (let ((e (car (plist-get (org-foresight-landing) :deadlines))))
      (should (= 540.0 (plist-get e :demand-min)))
      (should (eq 'lands (plist-get e :verdict)))
      ;; the hours it placed are its own to spend, so they are in the soft
      ;; figure too -- not only in the one that assumes everything moves
      (should (>= (plist-get e :soft-min) 540.0)))))

(ert-deftest org-foresight-test-non-working-days-supply-nothing ()
  "A weekend adds no hours, and the deadline after it is no easier for it.

The sum still runs *through* the closed days to reach the deadline, so a
Sunday deadline means \"by Friday\" with no calendar arithmetic anywhere."
  (org-foresight-test--with-landing
      (org-foresight-test--dated-tree "over the weekend" 6 "20:00")
    (let* ((org-foresight-workdays '(1 2 3 4 5))
           (org-foresight--shape-cache nil)
           (open (seq-count
                  (lambda (i)
                    (org-foresight-work-intervals
                     (org-foresight--day-start (- i))))
                  (number-sequence 0 6)))
           (e (car (plist-get (org-foresight-landing) :deadlines))))
      ;; four hours for each working day in the window, and no more
      (should (= (* 240.0 open) (plist-get e :hard-min)))
      (should (< open 7)))))

(ert-deftest org-foresight-test-a-day-already-over-takes-nothing-from-later ()
  "An overcommitted day contributes no hours, and does not remove any either.

`:spare-min' goes negative when a day is promised more than it holds.  Added
up unclipped, that day would *subtract* from the pool a later deadline draws
on -- charging the overrun a second time, against work that has nothing to
do with it, on a day it has not reached.  The capacity verdict already
reports the overrun where it happened; here the day simply gives nothing.

Twelve hours of undated promises sit on a four-hour day.  Tomorrow is clear,
and the three hours due tomorrow fit in it."
  (org-foresight-test--with-landing
      (format "* NEXT the deadline
DEADLINE: %s
** NEXT its work
:PROPERTIES:
:EFFORT: 3:00
:END:
* NEXT far too much promised for today
SCHEDULED: %s
:PROPERTIES:
:EFFORT: 12:00
:END:
" (org-foresight-test--stamp 1) (org-foresight-test--stamp 0))
    (let ((e (car (plist-get (org-foresight-landing) :deadlines))))
      ;; today gives nothing, tomorrow gives its four hours
      (should (= 240.0 (plist-get e :soft-min)))
      (should (eq 'lands (plist-get e :verdict))))))

(ert-deftest org-foresight-test-a-deadline-today-shrinks-through-the-day ()
  "Today's supply is what is left of today, which is a moving figure.

No special case makes this work: capacity measures from NOW, so a commitment
that fits at nine and does not at noon is reported as fitting at nine and not
at noon.  That is the truth, and the model already told it."
  (org-foresight-test--with-landing
      (org-foresight-test--dated-tree "due today" 0 "3:00")
    (should (eq 'lands (plist-get (car (plist-get (org-foresight-landing)
                                                  :deadlines))
                                  :verdict)))
    (let ((org-foresight-now (time-add (org-foresight--day-start 0)
                                       (* 3600 12))))
      (should (eq 'over (plist-get (car (plist-get (org-foresight-landing)
                                                   :deadlines))
                                   :verdict))))))

(ert-deftest org-foresight-test-a-deadline-beyond-the-horizon-is-not-a-failure ()
  "Falling short of a fortnight says nothing about a date six weeks out.

Supply only grows as the window lengthens, so the scanned pool is a lower
bound past its end: work that does not fit in it may fit comfortably in week
five.  Calling that a failure would be the one lie this is built to avoid,
so it is `beyond' -- and a date nobody can compute must never be allowed to
become the failure the line reports, or it would own that line forever."
  (org-foresight-test--with-landing
      (org-foresight-test--dated-tree "far off" 40 "200:00")
    (let* ((l (org-foresight-landing))
           (e (car (plist-get l :deadlines))))
      (should (eq 'beyond (plist-get e :verdict)))
      (should (null (plist-get l :first-fail)))
      (should (= 1 (plist-get l :beyond))))))

(ert-deftest org-foresight-test-a-deadline-beyond-the-horizon-can-still-land ()
  "The other half: `lands' stays sound past the horizon, and is worth saying.

A window that already holds the work will still hold it when it grows, so a
fortnight's pool proving sufficient proves it for good.  Only the negative
answer is out of reach out there."
  (org-foresight-test--with-landing
      (org-foresight-test--dated-tree "far off" 40 "2:00")
    (let ((e (car (plist-get (org-foresight-landing) :deadlines))))
      (should (eq 'lands (plist-get e :verdict))))))

(ert-deftest org-foresight-test-landing-takes-no-survey-of-its-own ()
  "Handed both surveys, it walks no files.

That is what makes it safe to call from anywhere -- a verdict that quietly
re-read every agenda file would be a verdict nobody could afford to put in a
redraw."
  (org-foresight-test--with-landing
      (org-foresight-test--dated-tree "something" 3 "1:00")
    (let* ((today (org-foresight--day-start 0))
           (projects (org-foresight-project-scan))
           (scan (org-foresight-scan org-foresight-horizon-days today))
           (walks 0))
      (cl-letf* ((real-scan (symbol-function 'org-foresight-scan))
                 (real-proj (symbol-function 'org-foresight-project-scan))
                 ((symbol-function 'org-foresight-scan)
                  (lambda (&rest a) (setq walks (1+ walks)) (apply real-scan a)))
                 ((symbol-function 'org-foresight-project-scan)
                  (lambda (&rest a) (setq walks (1+ walks)) (apply real-proj a))))
        (should (org-foresight-landing projects scan))
        (should (= 0 walks))))))

(defun org-foresight-test--landing-lines ()
  "Return the Load block's landing lines, properties and all, or nil.

Everything from the first `↳' on: the verdict, and under it the ways out
where there are any.  The Load rows above carry no `↳', so the split is
unambiguous."
  (let* ((block (org-foresight-report-load
                 nil nil nil (org-foresight-landing)))
         (lines (split-string (or block "") "\n"))
         (tail (seq-drop-while (lambda (l) (not (string-match-p "↳" l))) lines)))
    (and tail (string-join tail "\n"))))

(defun org-foresight-test--landing-line ()
  "Return just the Load block's landing verdict line, or nil."
  (when-let ((all (org-foresight-test--landing-lines)))
    (car (split-string all "\n"))))

(ert-deftest org-foresight-test-the-landing-line-names-the-failure ()
  "It names one heading, dates it, and says how much is missing.

One heading, because the line exists to send somebody to it -- and it
carries that heading's marker so `RET' works, which is the difference
between reporting a problem and being able to act on it."
  (org-foresight-test--with-landing
      (org-foresight-test--dated-tree "the annual report" 1 "9:00")
    (let ((line (org-foresight-test--landing-line)))
      (should line)
      (should (string-match-p "the annual report" line))
      ;; the window, not the day: the figure is what everything due by then
      ;; needs against the hours before then
      (should (string-match-p " by " line))
      (should (string-match-p "1:00 short" line))
      ;; one commitment in the window, so nothing is counted beside it
      (should-not (string-match-p "\\+[0-9]" line))
      ;; and the agenda's own commands can act on it
      (should (get-text-property (1- (length line)) 'org-marker line))
      ;; and the way out is under it rather than crammed beside it
      (should (string-match-p "\n" (org-foresight-test--landing-lines))))))

(ert-deftest org-foresight-test-the-landing-line-owns-up-to-the-window ()
  "A shortfall belongs to the window, not to the heading the line names.

The test is cumulative, so the figure is what *everything* due by that date
needs against the hours before it.  The line names one heading because
somebody has to be sent somewhere, and naming one while stating a collective
figure beside it -- with nothing between them -- reads as a sentence about
that one project which the arithmetic never made.

Two commitments fall due the same day and neither is short on its own.  The
date is given as a window, and the second is counted."
  (org-foresight-test--with-landing
      (concat (org-foresight-test--dated-tree "the larger one" 1 "5:00")
              (org-foresight-test--dated-tree "the smaller one" 1 "4:00"))
    (let ((line (org-foresight-test--landing-line)))
      (should line)
      ;; the bigger of the two is named, and the other is counted
      (should (string-match-p "the larger one" line))
      (should (string-match-p "\\+1" line))
      (should (string-match-p " by " line))
      ;; nine hours owed against eight available: the pair is short, and
      ;; neither of them is short alone
      (should (string-match-p "1:00 short" line)))))

(ert-deftest org-foresight-test-the-landing-line-counts-what-lands ()
  "When nothing fails it says so, and says how much room the tightest has.

The good news is the point of the exercise: a day that is OVER can still be
a day whose deadlines are all safe, and knowing that is what lets somebody
stop working."
  (org-foresight-test--with-landing
      (concat (org-foresight-test--dated-tree "small one" 3 "1:00")
              (org-foresight-test--dated-tree "other one" 5 "1:00"))
    (let ((line (org-foresight-test--landing-line)))
      (should line)
      (should (string-match-p "2 deadlines land" line))
      (should (string-match-p "spare at the tightest" line)))))

(ert-deftest org-foresight-test-the-landing-line-separates-defer-from-over ()
  "Fitting only if other work moves is not the same as not fitting.

They lead to different actions -- rearranging a week against giving work
away -- so the line must not spend the same words on them.  A shortfall that
only the soft figure sees is said in the ordinary face and qualified; one
the hard figure sees too is said in the overrun's own colour."
  (org-foresight-test--with-landing
      (concat (org-foresight-test--dated-tree "the deadline" 1 "6:00")
              (format "* NEXT undated but promised
SCHEDULED: %s
:PROPERTIES:
:EFFORT: 4:00
:END:
" (org-foresight-test--stamp 0)))
    (let ((line (org-foresight-test--landing-line)))
      (should line)
      (should (string-match-p "2:00 more than is free" line)))))

(ert-deftest org-foresight-test-a-shortfall-comes-with-a-way-out ()
  "A figure with no lever is read once and then resented.

Four answers, all of them subtractions on figures already worked out: leave
it and it finishes on some later day; give up the smallest single thing that
would close the gap; or work into the evenings, if the evenings are even
long enough.  None of them proposes a schedule."
  (org-foresight-test--with-landing
      ;; Short titles on purpose: with long ones the line fills and the
      ;; last term is budgeted away, which is correct and would make this
      ;; test about `org-foresight-report--fit-terms' instead.
      (concat (org-foresight-test--dated-tree "big" 1 "7:00")
              (org-foresight-test--dated-tree "small" 1 "2:00"))
    (let ((out (cadr (split-string (org-foresight-test--landing-lines) "\n"))))
      (should out)
      ;; nine hours owed by tomorrow, eight available: it lands the day after
      (should (string-match-p "lands" out))
      ;; and the smallest that would close a one-hour gap on its own is the
      ;; two-hour project, not the seven-hour one
      (should (string-match-p "drop any one of small" out))
      ;; and the hours nothing has claimed are named, so staying late can be
      ;; weighed rather than guessed at.  Only what nothing has claimed:
      ;; dinner is not somewhere to put late work.
      (should (string-match-p "unclaimed before then" out))
      ;; the row points at what it says to drop, not at what is late
      (should (get-text-property (1- (length out)) 'org-marker out)))))

(ert-deftest org-foresight-test-the-defer-way-out-names-what-owes-nobody-a-date ()
  "`defer' has its own lever, and it is never the deadline's own work.

Work with no date is the only work a deadline can take hours from without
anything else giving way -- that is what having no deadline means.  Naming it
is the difference between being told to rearrange the week and being shown
what to move.

The deadline's own leaves must be excluded, and the exclusion only shows
where one of them has been *scheduled*: an unscheduled leaf is in no day's
promises and could never be offered anyway.  So the project here has placed
two of its hours inside the window, and those two hours are smaller than the
undated work -- meaning a version without the exclusion would pick them, and
answer \"move the deadline's own work\" to \"the deadline is short\"."
  (org-foresight-test--with-landing
      (format "* NEXT the deadline
DEADLINE: %s
** NEXT placed
SCHEDULED: %s
:PROPERTIES:
:EFFORT: 2:00
:END:
** NEXT the rest
:PROPERTIES:
:EFFORT: 4:00
:END:
* NEXT the undated one
SCHEDULED: %s
:PROPERTIES:
:EFFORT: 3:00
:END:
" (org-foresight-test--stamp 1) (org-foresight-test--stamp 0)
  (org-foresight-test--stamp 0))
    (let* ((e (car (plist-get (org-foresight-landing) :deadlines)))
           (out (cadr (split-string (org-foresight-test--landing-lines) "\n"))))
      (should (eq 'defer (plist-get e :verdict)))
      ;; the two placed hours are smaller and are still not offered
      (should (equal '("the undated one")
                     (mapcar (lambda (r) (plist-get r :title))
                             (plist-get e :move))))
      (should out)
      (should (string-match-p "move any one of the undated one" out))
      (should-not (string-match-p "placed" out))
      ;; and nothing about staying late: the hours exist inside the working
      ;; day, they are merely spoken for
      (should-not (string-match-p "unclaimed" out)))))

(ert-deftest org-foresight-test-when-it-would-land-answers-the-question-asked ()
  "`lands' is read against the figure that failed, not against the other one.

For `over' the shortfall is against the hard figure, so the day it lands is
the day the hard figure catches up.  For `defer' the work already fits the
hard figure -- that is what makes it `defer' rather than `over' -- so
answering with the hard figure would say \"tomorrow\" about a thing that will
not be done tomorrow unless something else moves.  It has to be read against
soft: leave everything as it is, and this is when it finishes.

Here the next three days are each mostly promised to work with no deadline,
so the soft figure creeps up an hour a day while the hard one is sufficient
from the start.  They answer different days, and only one of them is true of
somebody who changes nothing."
  (org-foresight-test--with-landing
      (format "* NEXT the deadline
DEADLINE: %s
** NEXT its work
:PROPERTIES:
:EFFORT: 6:00
:END:
%s" (org-foresight-test--stamp 1)
      (mapconcat
       (lambda (d)
         (format "* NEXT undated %d\nSCHEDULED: %s\n:PROPERTIES:\n:EFFORT: 3:00\n:END:\n"
                 d (org-foresight-test--stamp d)))
       '(0 1 2) ""))
    (let ((e (car (plist-get (org-foresight-landing) :deadlines))))
      (should (eq 'defer (plist-get e :verdict)))
      ;; the hard figure is already enough, and would have said the day after
      (should (>= (plist-get e :hard-min) (plist-get e :demand-min)))
      ;; soft only reaches it on the fourth day
      (should (= 3 (org-foresight--day-of (plist-get e :lands-day)
                                          (org-foresight--day-start 0)))))))

(ert-deftest org-foresight-test-a-report-row-answers-the-agenda-at-its-margin ()
  "A row that names a heading has to carry its marker at the first character.

`org-agenda-goto', `org-agenda-schedule' and the rest all read through
`org-get-at-bol', which looks at the *beginning of the line*.  The margin is
added last and is a bare space, so for a long time every row in every report
block was marked carefully and then answered nothing: `RET' and `TAB' found
no marker and reported an error on a line plainly about a heading.

Asserted at the margin rather than anywhere in the row, because anywhere in
the row is exactly what used to pass."
  (org-foresight-test--with-landing
      ;; Two, so the way out names a *different* heading from the verdict and
      ;; both rows have somewhere of their own to go
      (concat (org-foresight-test--dated-tree "big" 1 "7:00")
              (org-foresight-test--dated-tree "small" 1 "2:00"))
    (let* ((block (org-foresight-report-load
                   nil nil nil (org-foresight-landing)))
           (lines (seq-filter (lambda (l) (string-match-p "↳" l))
                              (split-string block "\n"))))
      (should (= 2 (length lines)))
      ;; and they point at different headings
      (should-not (equal (get-text-property 0 'org-marker (car lines))
                         (get-text-property 0 'org-marker (cadr lines))))
      (dolist (l lines)
        ;; column zero, not column one: the margin is the character the
        ;; agenda reads
        (should (get-text-property 0 'org-marker l))
        (should (get-text-property 0 'org-agenda-type l))))))

(ert-deftest org-foresight-test-the-way-out-counts-the-alternatives ()
  "The least that works is not always the one that can go.

Every candidate is on its own enough and they are sorted smallest-first, so
the named one is the cheapest way out -- but a project may be the cheapest
and still be the one thing that cannot be cut.  A reader who cannot see that
there are others will not go looking for them."
  (org-foresight-test--with-landing
      (concat (org-foresight-test--dated-tree "big" 1 "5:00")
              (org-foresight-test--dated-tree "mid" 1 "4:00")
              (org-foresight-test--dated-tree "sml" 1 "3:00"))
    (let ((out (cadr (split-string (org-foresight-test--landing-lines) "\n"))))
      (should out)
      ;; twelve owed against eight: four short, and both `mid' and `big'
      ;; would close it on their own -- `sml' would not
      (should (string-match-p "any one of mid" out))
      (should (string-match-p "\\+1" out)))))

(ert-deftest org-foresight-test-the-way-out-does-not-name-the-same-thing-twice ()
  "\"What is late is X, so drop X\" answers nothing.

The verdict names the biggest commitment due that day, and the way out names
the smallest one that would close the gap.  When those are the same heading
the line says the title twice and reads as a circle -- and it happens
whenever one large project is late on its own, which is not a rare shape.

What it actually means is worth keeping: nothing smaller would have done, so
there is no partial way out.  That is what is said instead."
  (org-foresight-test--with-landing
      (org-foresight-test--dated-tree "the only one" 1 "20:00")
    (let ((out (cadr (split-string (org-foresight-test--landing-lines) "\n"))))
      (should out)
      (should (string-match-p "only by dropping it" out))
      (should-not (string-match-p "drop any one of" out))
      ;; the title appears once, on the verdict above
      (should-not (string-match-p "the only one" out)))))

(ert-deftest org-foresight-test-nothing-alone-is-enough-says-nothing ()
  "Where no single thing closes the gap, none is named.

The question has stopped being which one and become how many, and that is a
different sentence than one line can hold.  Naming the biggest anyway would
read as a recommendation that does not work."
  (org-foresight-test--with-landing
      ;; eighteen hours owed by tomorrow against eight available: ten short,
      ;; and neither nine-hour project would close it on its own
      (concat (org-foresight-test--dated-tree "one" 1 "9:00")
              (org-foresight-test--dated-tree "two" 1 "9:00"))
    (let ((out (cadr (split-string (org-foresight-test--landing-lines) "\n"))))
      (should-not (and out (string-match-p "drop any one of" out)))
      ;; the levers that still apply are still offered
      (should out)
      (should (string-match-p "unclaimed before then" out)))))

(ert-deftest org-foresight-test-the-landing-line-is-silent-with-no-deadlines ()
  "Nothing dated, nothing said.

A block that reports zero every morning is a block that stops being read,
and the Load rows above it still answer their own question."
  (org-foresight-test--with-landing
      "* NEXT no date anywhere
:PROPERTIES:
:EFFORT: 2:00
:END:
"
    (should (null (org-foresight-landing)))
    (should (null (org-foresight-test--landing-line)))
    ;; and the block itself is still drawn
    (should (org-foresight-report-load nil nil nil (org-foresight-landing)))))

(ert-deftest org-foresight-test-the-landing-line-holds-eighty-columns ()
  "Every qualifier at once, on a title long enough to want the whole line.

The Load rows are wide already; a line that wrapped would take the block's
alignment with it.  The answer is kept and the footnotes are dropped, in
that order, which is what `org-foresight-report--fit-terms' is for."
  (org-foresight-test--with-landing
      (concat (org-foresight-test--dated-tree
               "a title long enough that it has to be cut somewhere" 1 "9:00")
              ;; overdue, and unestimated, and past the horizon
              (format "* NEXT gone by\nDEADLINE: %s\n** NEXT no estimate on this\n"
                      (org-foresight-test--stamp -3))
              (org-foresight-test--dated-tree "far off" 40 "300:00"))
    (let ((line (org-foresight-test--landing-line)))
      (should line)
      (should (org-foresight-test--within-80
               (org-foresight-test--landing-lines)))
      ;; the answer survives whatever else had to go
      (should (string-match-p "short" line)))))

(ert-deftest org-foresight-test-the-projects-are-surveyed-once-a-redraw ()
  "The outline is walked once a redraw, on the walk that finds the signals.

They were two walks of every heading in every file, asking different
questions of the same visit -- and on a real journal the second was a
quarter of the redraw.  Counted on the walk itself rather than on
`org-foresight-project-scan\=', which no longer walks anything: it
classifies and prices what the shared walk brought back, and would report
one call however many times the files had been read."
  (org-foresight-test--with-agenda
      (concat "* NEXT parent\n** NEXT child\nDEADLINE: "
              (org-foresight-test--stamp 2)
              "\n:PROPERTIES:\n:EFFORT: 1:00\n:END:\n")
    (dolist (style '(daily review))
      ;; From cold each time: the walk is memoized for a few seconds, and a
      ;; second style drawn inside that window would report a walk of zero
      ;; and pass whatever the first one had done.
      (let ((org-foresight-report-style style)
            (org-foresight--signals-cache nil)
            (walks 0))
        (cl-letf* ((real (symbol-function 'org-foresight--signals-compute))
                   ((symbol-function 'org-foresight--signals-compute)
                    (lambda (&rest args)
                      (setq walks (1+ walks))
                      (apply real args))))
          (org-foresight-test--agenda)
          (should (= 1 walks)))))))

(ert-deftest org-foresight-test-a-shared-heading-is-not-read-twice ()
  "What the walk has already read is passed on, not asked for again.

Sharing the walk saves the traversal; sharing the readings is the other
half, and it is invisible from the answers -- a second `org-get-heading\='
at the same point returns what the first did, so nothing is wrong, it is
only paid for.  Four readings times every heading in every agenda file is
what that comes to.  Asserted by making the re-read fail."
  (org-foresight-test--with-org
      "* NEXT something\n:PROPERTIES:\n:CATEGORY: work\n:END:\n"
    (with-current-buffer (find-file-noselect (car org-agenda-files))
      (org-with-wide-buffer
       (goto-char (point-min))
       (cl-letf (((symbol-function 'org-get-heading)
                  (lambda (&rest _) (error "the heading was read again")))
                 ((symbol-function 'org-get-todo-state)
                  (lambda (&rest _) (error "the keyword was read again"))))
         (let ((rec (org-foresight--project-record
                     (list :todo "NEXT" :done nil
                           :title "something" :category "work"))))
           (should (equal "something" (plist-get rec :title)))
           (should (equal "NEXT" (plist-get rec :todo)))
           (should (equal "work" (plist-get rec :category)))))
       ;; And without facts it still reads for itself, or every caller that
       ;; has not got them would get a record of nils.
       (should (equal "something"
                      (plist-get (org-foresight--project-record) :title)))))))

(ert-deftest org-foresight-test-the-outline-and-the-signals-agree ()
  "What the shared walk records is what a walk of its own recorded.

The stack that links a heading to its nearest TODO ancestor now runs inside
the signals lambda, beside a dozen other readings of the same heading.  The
risk of putting it there is that it stops running for every heading -- the
pop has to happen even where no record is made -- so the classification is
checked whole, against the shape the fixture actually has."
  (org-foresight-test--with-org
      "* NEXT top
** notes
*** NEXT buried
* NEXT sibling
** NEXT leaf
DEADLINE: <2030-01-01 Tue>
* plain
** NEXT orphaned by a wall
"
    (let ((recs (plist-get (org-foresight-project-scan) :headings)))
      (should (equal '("top" "buried" "sibling" "leaf" "orphaned by a wall")
                     (mapcar (lambda (r) (plist-get r :title)) recs)))
      ;; `notes' is transparent: `buried' answers to `top'.
      (should (equal "top" (plist-get (plist-get (nth 1 recs) :todo-parent) :title)))
      ;; `plain' is a wall: it closed `sibling', so nothing above adopts the
      ;; heading under it.
      (should-not (plist-get (nth 4 recs) :todo-parent))
      ;; And the deadline on a child makes a deadline project of its parent
      ;; and of nobody further up.
      (should (plist-get (nth 2 recs) :deadline-project-p))
      (should-not (plist-get (nth 0 recs) :deadline-project-p)))))


(ert-deftest org-foresight-test-the-board-lists-every-dated-commitment ()
  "The list the day's one-line verdict is a summary of.

The Load line names the soonest date that cannot be met and counts the
rest with `+N'.  That count is unreadable and unreachable on its own -- the
others have no name there and nowhere to go -- so the board holds the list,
in date order, one commitment a row."
  (org-foresight-test--with-landing
      (concat (org-foresight-test--dated-tree "first" 1 "2:00")
              (org-foresight-test--dated-tree "second" 1 "1:00")
              (org-foresight-test--dated-tree "later" 5 "1:00"))
    (let ((rows (split-string (substring-no-properties
                               (org-foresight-report-landing))
                              "\n")))
      ;; every unit is named, and in the order they fall due
      (should (seq-find (lambda (r) (string-match-p "first" r)) rows))
      (should (seq-find (lambda (r) (string-match-p "second" r)) rows))
      (should (seq-find (lambda (r) (string-match-p "later" r)) rows))
      (should (< (seq-position rows (seq-find (lambda (r)
                                                (string-match-p "first" r))
                                              rows))
                 (seq-position rows (seq-find (lambda (r)
                                                (string-match-p "later" r))
                                              rows)))))))

(ert-deftest org-foresight-test-every-board-commitment-can-be-acted-on ()
  "A list of deadlines you cannot act on from is one you have to find again.

The marker sits at the first character, because that is where the agenda
reads it -- see
`org-foresight-test-a-report-row-answers-the-agenda-at-its-margin'."
  (org-foresight-test--with-landing
      (org-foresight-test--dated-tree "something" 3 "1:00")
    (let ((rows (seq-filter (lambda (r) (string-match-p "something" r))
                            (split-string (org-foresight-report-landing) "\n"))))
      (should (= 1 (length rows)))
      (should (get-text-property 0 'org-marker (car rows)))
      (should (get-text-property 0 'org-agenda-type (car rows))))))

(ert-deftest org-foresight-test-the-board-rules-only-where-it-does-not-fit ()
  "A rule saying a date is comfortable explains a problem nobody has.

Drawn under the last commitment of a window that does not fit, and nowhere
else.  A board that comments on every line stops being read, which is the
same reason the agenda's own key names only the marks the day used."
  (org-foresight-test--with-landing
      (concat (org-foresight-test--dated-tree "tight" 1 "9:00")
              (org-foresight-test--dated-tree "roomy" 9 "1:00"))
    (let* ((rows (split-string (substring-no-properties
                                (org-foresight-report-landing))
                               "\n"))
           (rules (seq-filter (lambda (r) (string-match-p "owed" r)) rows)))
      (should (= 1 (length rules)))
      (should (string-match-p "1:00 short" (car rules)))
      ;; and it is under the window that failed, not under the other one
      (should (< (seq-position rows (car rules))
                 (seq-position rows (seq-find (lambda (r)
                                                (string-match-p "roomy" r))
                                              rows)))))))

(ert-deftest org-foresight-test-the-board-says-so-when-nothing-is-dated ()
  "An empty section says it is empty rather than being absent.

The board's other sections do the same: a heading with nothing under it
reads as something failing to load, and the reader cannot tell that from
having no deadlines."
  (org-foresight-test--with-landing
      "* NEXT no date anywhere\n:PROPERTIES:\n:EFFORT: 2:00\n:END:\n"
    (should (string-match-p "nothing is dated"
                            (substring-no-properties
                             (org-foresight-report-landing))))))

(ert-deftest org-foresight-test-the-board-holds-eighty-columns ()
  "Long titles are cut, and the rule fills the line without passing it."
  (org-foresight-test--with-landing
      (org-foresight-test--dated-tree
       "a title long enough that it has to be cut somewhere or it will run on"
       1 "9:00")
    (should (org-foresight-test--within-80
             (substring-no-properties (org-foresight-report-landing))))))

(ert-deftest org-foresight-test-the-two-project-tests-agree ()
  "One rule, evaluated two ways, held to the same answers.

The corpus-wide scan reads the outline once with a level stack; the
predicate reads one subtree at point.  Both are needed -- a signal cannot
afford a whole-corpus survey to ask about the heading it is standing on --
and the danger of having both is that they drift, so \"is this a project\"
quietly means two things in two blocks of the same report.

Run over the author's own specification, so the thing they agree on is the
rule as written rather than whatever they happen to share."
  (let ((due (org-foresight-test--stamp 1)))
    (org-foresight-test--with-org
        (format org-foresight-test--project-fixture due due due due due due)
      (let ((batch (make-hash-table :test #'equal)))
        (dolist (r (plist-get (org-foresight-project-scan) :headings))
          (puthash (plist-get r :title) (and (plist-get r :project-p) t) batch))
        (with-current-buffer (find-file-noselect (car org-agenda-files))
          (org-with-wide-buffer
           (org-map-entries
            (lambda ()
              (let ((title (org-get-heading t t t t)))
                ;; keyword-less headings are in neither: no record, and the
                ;; predicate says no
                (should (eq (gethash title batch)
                            (and (org-foresight-project-p) t)))))
            nil nil)))))))

(ert-deftest org-foresight-test-scaffolding-alone-does-not-make-a-project ()
  "Notes under a task are notes, not children.

A project is a TODO with *TODO* descendants.  A heading whose only
descendants carry no keyword has nothing under it that anybody is going to
do, and counting them would make a project of every task somebody wrote a
paragraph under -- which is most of them.

Asserted on both readings of the rule, because this is the case that tells
them apart: the author's own specification has no task with scaffolding
beneath it, so a version counting any heading at all passes it."
  (org-foresight-test--with-org
      "* NEXT has only notes under it
** notes
*** more notes
* NEXT has a real child
** NEXT the child
"
    (let ((recs (plist-get (org-foresight-project-scan) :headings)))
      (should-not (plist-get (car recs) :project-p))
      (should (plist-get (cadr recs) :project-p)))
    (with-current-buffer (find-file-noselect (car org-agenda-files))
      (org-with-wide-buffer
       (goto-char (point-min))
       (should-not (org-foresight-project-p))
       (re-search-forward "^\\* NEXT has a real child")
       (should (org-foresight-project-p))))))

(ert-deftest org-foresight-test-a-decomposed-project-is-not-unplannable ()
  "A project heading has no EFFORT because its children carry it.

Asking one for its own estimate would be asking for the same hours twice, so
a signal that read a missing EFFORT there fired on every properly decomposed
tree in the file -- naming correct work as a problem, which is how a board
teaches somebody to stop reading it.

The genuine case survives: a dated commitment nobody has broken down and
nobody has sized is a deadline that cannot be planned for, and that is what
the signal is for."
  (org-foresight-test--with-signals
      (format "* NEXT decomposed
DEADLINE: %s
** NEXT its work
:PROPERTIES:
:EFFORT: 2:00
:END:
* NEXT never broken down
DEADLINE: %s
" (org-foresight-test--stamp 2) (org-foresight-test--stamp 2))
    (let ((titles (mapcar (lambda (f) (plist-get f :title))
                          (org-foresight-test--signal
                           "Unplannable (deadline, no estimate)"))))
      (should (member "never broken down" titles))
      (should-not (member "decomposed" titles)))))

(ert-deftest org-foresight-test-a-guessed-figure-says-so-on-its-own-row ()
  "The row carrying a figure is the row that has to admit it is a guess.

The day's line counts them in total, which is enough to know the answer is
soft and not enough to know *which* answer.  This is the list somebody comes
to in order to go and put the estimate in, so it is the list that has to say
where the estimate is missing."
  (org-foresight-test--with-landing
      (format "* NEXT half sized
DEADLINE: %s
** NEXT measured
:PROPERTIES:
:EFFORT: 1:00
:END:
** NEXT not measured
* NEXT sized properly
DEADLINE: %s
** NEXT measured too
:PROPERTIES:
:EFFORT: 1:00
:END:
* NEXT nothing sized
DEADLINE: %s
" (org-foresight-test--stamp 2) (org-foresight-test--stamp 2)
  (org-foresight-test--stamp 2))
    (let ((rows (split-string (substring-no-properties
                               (org-foresight-report-landing))
                              "\n")))
      (should (seq-find (lambda (r) (string-match-p "half sized · 1 of 2 unestimated" r))
                        rows))
      (should (seq-find (lambda (r) (string-match-p "nothing sized · all unestimated" r))
                        rows))
      ;; and the one that is properly sized says nothing at all
      (should (seq-find (lambda (r)
                          (and (string-match-p "sized properly" r)
                               (not (string-match-p "unestimated" r))))
                        rows)))))

(provide 'org-foresight-test)

;;; org-foresight-test.el ends here
