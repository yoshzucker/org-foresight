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

(ert-deftest org-foresight-test-app-category ()
  (should (equal (org-foresight--app-category "Emacs") "work"))
  (should (equal (org-foresight--app-category "slack") "comms"))   ; case-folded
  (should (equal (org-foresight--app-category "Safari") "distraction"))
  (should (equal (org-foresight--app-category "SomeUnknownApp") "other"))
  (should (equal (org-foresight--app-category nil) "other")))

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
  `(let ((org-foresight-workday-start "09:00")
         (org-foresight-workday-end "17:30")
         (org-foresight-workdays '(1 2 3 4 5))
         (org-foresight-surge-cache-file "/nonexistent/org-foresight-surge.eld")
         (org-foresight-surge-default "1:00"))
     ,@body))

(ert-deftest org-foresight-test-workday-window ()
  (org-foresight-test--with-window
    ;; 2026-08-10 is a Monday; 2026-08-09 a Sunday.
    (should (org-foresight-workday-window (org-foresight-test--ts 12 0 10)))
    (should-not (org-foresight-workday-window (org-foresight-test--ts 12 0 9)))))

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
        (should (= (plist-get cap :surge-min) 60.0))
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

(ert-deftest org-foresight-test-capacity-finish-starts-from-now ()
  "The finish estimate pours from NOW, not from the top of the working day."
  (org-foresight-test--with-window
    (org-foresight-test--with-org
        "* NEXT two hours of work
SCHEDULED: <2026-08-10 Mon>
:PROPERTIES:
:EFFORT:   2:00
:END:
"
      (let* ((day (org-foresight-test--ts 0 0 10))
             ;; 2:00 promised + 1:00 surge = 3 hours, started at 13:00
             (cap (org-foresight-capacity day nil (org-foresight-test--ts 13 0 10))))
        (should (equal (format-time-string "%H:%M" (plist-get cap :finish))
                       "16:00"))))))

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
        (should (null (plist-get cap :finish)))))))

(ert-deftest org-foresight-test-capacity-non-workday ()
  "A day with no working window has no free time to offer."
  (org-foresight-test--with-window
    (org-foresight-test--with-org "* nothing\n"
      (let ((cap (org-foresight-capacity (org-foresight-test--ts 0 0 9) nil
                                         (org-foresight-test--ts 6 0 9))))
        (should (null (plist-get cap :window)))
        (should (= (plist-get cap :free-min) 0.0))))))

(ert-deftest org-foresight-test-report-capacity-line ()
  "The top-of-agenda line carries the whole answer and fits on one line."
  (org-foresight-test--with-window
    (org-foresight-test--with-org
        "* team meeting
<2026-08-10 Mon 10:00-11:00>
"
      (let ((s (org-foresight-report-capacity-line
                (org-foresight-test--ts 0 0 10) nil
                (org-foresight-test--ts 6 0 10))))
        (should (string-match-p "Free" s))
        (should (string-match-p "surge" s))
        (should (string-match-p "ends" s))
        (should-not (string-match-p "\n" s))
        (should (org-foresight-test--within-80 s))))))

(ert-deftest org-foresight-test-report-capacity-table ()
  "The lower block lists the free stretches with columns that line up.
It must not repeat the verdict: the number is stated once, at the top."
  (org-foresight-test--with-window
    (org-foresight-test--with-org
        "* team meeting
<2026-08-10 Mon 10:00-11:00>
"
      (let* ((day (org-foresight-test--ts 0 0 10))
             (s (org-foresight-report-capacity
                 day nil (org-foresight-test--ts 6 0 10))))
        (should-not (string-match-p "Free 9:00\\|spare\\|ends" s))
        (should (org-foresight-test--within-80 s))
        ;; the meeting splits the day, so both stretches are listed
        (should (string-match-p "09:00–10:00" s))
        (should (string-match-p "11:00–17:30" s))
        ;; every table row must be the same width as its separator
        (let* ((rows (seq-filter (lambda (l) (string-prefix-p "|" l))
                                 (split-string (substring-no-properties s) "\n")))
               (widths (seq-uniq (mapcar #'string-width rows))))
          (should (= (length widths) 1)))))))

(ert-deftest org-foresight-test-report-capacity-non-workday ()
  "A non-working day yields no top line at all, and says so in the block."
  (org-foresight-test--with-window
    (org-foresight-test--with-org "* nothing\n"
      (let ((day (org-foresight-test--ts 0 0 9))
            (now (org-foresight-test--ts 6 0 9)))
        (should (null (org-foresight-report-capacity-line day nil now)))
        (should (string-match-p
                 "not a working day"
                 (org-foresight-report-capacity day nil now)))))))

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
        (should-not (string-match-p "signal" s))
        (should-not (string-match-p "\n" s))))))

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

(ert-deftest org-foresight-test-report-clocked ()
  (org-foresight-test--without-aw
    (let ((s (org-foresight-report-clocked org-foresight-test--clock)))
      (should (string-match-p "Focus" s))
      (should (string-match-p "work" s))
      (should (string-match-p "会議" s))
      (should (org-foresight-test--within-80 s)))))

(ert-deftest org-foresight-test-report-week ()
  (org-foresight-test--without-aw
    (let ((s (org-foresight-report-week org-foresight-test--clock)))
      (should (string-match-p "Week" s))
      (should (string-match-p "peak" s))
      (should (org-foresight-test--within-80 s)))))

(ert-deftest org-foresight-test-report-empty-clock ()
  "A day with nothing clocked must render a message, not crash or blank out."
  (org-foresight-test--without-aw
    (let ((empty (list :rows nil :total 0 :days 7 :byday (make-vector 7 0)
                       :today-rows nil :today-total 0 :today-segments 0
                       :today-intervals nil)))
      (should (string-match-p "no clocked time"
                              (org-foresight-report-clocked empty)))
      (should (string-match-p "no clocked time"
                              (org-foresight-report-week empty))))))

(ert-deftest org-foresight-test-report-observed-degrades ()
  "With no ActivityWatch the Observed block says so rather than signalling."
  (org-foresight-test--without-aw
    (let ((s (org-foresight-report-observed org-foresight-test--clock)))
      (should (string-match-p "unavailable" s)))))

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
        (should (string-match-p "Focus" (org-foresight-report--body))))
      (let ((org-foresight-report-style nil))
        (should (null (org-foresight-report--body)))))))

;;;; Signals
;; Every signal is a claim about the user's data, so each gets both a case that
;; must fire and a case that must not: a board that cries wolf is worse than no
;; board, because it stops being read.

(defun org-foresight-test--signal (label)
  "Return the findings filed under LABEL by `org-foresight-signals'."
  (cdr (assoc label (org-foresight-signals))))

(defmacro org-foresight-test--with-signals (text &rest body)
  "Run BODY over TEXT with signal thresholds pinned."
  (declare (indent 1))
  `(org-foresight-test--with-org ,text
     (let ((org-foresight-procrastination-threshold 3)
           (org-foresight-horizon-days 14)
           (org-foresight-followup-keywords '("WAIT"))
           (org-foresight-meeting-categories '("outlook"))
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
      "* NEXT due soon, unestimated
DEADLINE: <2026-08-12 Wed>
* NEXT due soon, estimated
DEADLINE: <2026-08-12 Wed>
:PROPERTIES:
:EFFORT:   1:00
:END:
* DONE already finished
DEADLINE: <2026-08-12 Wed>
"
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

(ert-deftest org-foresight-test-signal-after-hours ()
  "Work parked outside the working window is subtracted from nothing, so it
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
      (let* ((org-foresight-horizon-days 400)
             (found (org-foresight-test--signal
                     "After hours (invisible to capacity)"))
             (titles (mapcar (lambda (f) (plist-get f :title)) found)))
        (should (member "evening call" titles))
        (should (member "saturday work" titles))
        (should-not (member "during the day" titles))))))

(ert-deftest org-foresight-test-signal-after-hours-reports-once ()
  "A repeating out-of-hours meeting is one problem, not fifty."
  (org-foresight-test--with-window
    (org-foresight-test--with-signals
        "* NEXT nightly deploy watch
<2027-01-05 Tue 19:00-20:00 +1d>
"
      (let ((org-foresight-horizon-days 400))
        (should (= (length (org-foresight-test--signal
                            "After hours (invisible to capacity)"))
                   1))))))

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
      (let* ((load (org-foresight-load 14 nil (org-foresight-test--ts 6 0 10)))
             (today (org-foresight--day-start 0))
             (weekend 0))
        (dotimes (i 14)
          (let ((dow (nth 6 (decode-time (time-add today (days-to-time i))))))
            (when (memq dow '(0 6))
              (setq weekend (1+ weekend))
              (should (= (cdr (aref load i)) 0.0)))))
        (should (> weekend 0))))))

(ert-deftest org-foresight-test-report-load-within-80 ()
  (org-foresight-test--with-window
    (org-foresight-test--with-org "* nothing\n"
      (let ((s (org-foresight-report-load 14 nil (org-foresight-test--ts 6 0 10))))
        (should (org-foresight-test--within-80 s))))))

(ert-deftest org-foresight-test-plan-style-is-registered ()
  "org-foresight-plan.el must register itself with the report dispatcher."
  (should (eq (cdr (assq 'plan org-foresight-report-renderers))
              'org-foresight-plan-report)))

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
                (org-foresight-workday-start "09:00")
                (org-foresight-workday-end "17:30")
                (org-foresight-workdays '(1 2 3 4 5))
                (org-foresight-followup-keywords '("WAIT" "DELEG"))
                (org-foresight-meeting-categories '("outlook"))
                (org-foresight-surge-cache-file (expand-file-name "s.eld" dir))
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
        (should (org-element-parse-buffer))))
    ;; today must actually appear, or the corpus is describing another week
    (with-current-buffer (find-file-noselect (cadr org-agenda-files))
      (should (string-match-p (format-time-string "%Y-%m-%d")
                              (buffer-string))))))

(ert-deftest org-foresight-test-demo-fires-every-signal ()
  "One of everything: each signal must find its example in the demo corpus."
  (org-foresight-test--with-demo
    (let ((labels (mapcar #'car (org-foresight-signals))))
      (dolist (expected '("Meetings without prep"
                          "After hours (invisible to capacity)"
                          "Unplannable (deadline, no estimate)"
                          "Gone quiet (follow-up overdue)"
                          "Kept moving (not really NEXT)"
                          "Orphaned prep"))
        (should (member expected labels))))))

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

(provide 'org-foresight-test)

;;; org-foresight-test.el ends here
