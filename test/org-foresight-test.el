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
               :surge-min 57.0 :spare-min 183.0
               :private-min 0.0 :borrowed-min 0.0 :unclaimed-min 0.0)))
    (should (= (org-foresight-test--bar-cells (org-foresight-report--bar cap))
               org-foresight-bar-width))))

(ert-deftest org-foresight-test-bars-share-one-scale ()
  "The two bars must be comparable, or setting them side by side says nothing.

Equal spans of time have to draw equal numbers of cells whichever bar they
are in -- otherwise a long evening could look shorter than a short workday."
  (let* ((cap '(:span-min 480.0 :booked-min 480.0 :travel-min 0.0
                :private-min-in-span 0.0 :committed-min 0.0
                :surge-min 0.0 :spare-min 0.0
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
                :surge-min 57.0 :spare-min 183.0
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
      (should (string-match-p "  surge" (substring-no-properties key))))))

(ert-deftest org-foresight-test-bar-marks-the-overflow ()
  "An overcommitted day shows where the span ran out instead of clipping."
  (let ((over '(:span-min 510.0 :booked-min 420.0 :travel-min 0.0
                :private-min-in-span 0.0 :committed-min 240.0
                :surge-min 60.0 :spare-min -210.0))
        (fits '(:span-min 510.0 :booked-min 60.0 :travel-min 0.0
                :private-min-in-span 0.0 :committed-min 60.0
                :surge-min 60.0 :spare-min 330.0)))
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
                   :surge-min 0.0 :spare-min 0.0)))))

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
                    (plist-get cap :surge-min)
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
  "Both styles lead with the verdict and follow the listing with their tail:
the day is the agenda, and everything else is commentary on it."
  (org-foresight-test--with-day "* NEXT something\nSCHEDULED: <2026-08-10 Mon>\n"
    (org-foresight-test--in-agenda
      (let ((org-foresight-report-style 'plan))
        (org-foresight-report-render)
        (let ((text (substring-no-properties (buffer-string))))
          (should (< (string-search "Day-agenda" text)
                     (string-search "Signals" text))))))
    (org-foresight-test--in-agenda
      (let ((org-foresight-report-style 'daily))
        (org-foresight-report-render)
        (let ((text (substring-no-properties (buffer-string))))
          ;; the verdict still leads, but the tables follow the listing
          (should (< (string-search "Capacity" text)
                     (string-search "Day-agenda" text)))
          (should (< (string-search "Day-agenda" text)
                     (string-search "Clocked" text))))))))

(ert-deftest org-foresight-test-render-is-idempotent ()
  "Rendering twice must replace, not accumulate."
  (org-foresight-test--with-day "* NEXT something\nSCHEDULED: <2026-08-10 Mon>\n"
    (org-foresight-test--in-agenda
      (let ((org-foresight-report-style 'plan))
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

(ert-deftest org-foresight-test-diagnose-reports-missing-wiring ()
  (org-foresight-test--with-day "* nothing\n"
    (let ((org-agenda-finalize-hook nil))
      (should (seq-find (lambda (s) (string-match-p "finalize-hook" s))
                        (org-foresight--diagnose-advice
                         (org-foresight--day-start 0)))))
    (let ((org-agenda-finalize-hook '(org-foresight-report-render)))
      (should-not (seq-find (lambda (s) (string-match-p "finalize-hook" s))
                            (org-foresight--diagnose-advice
                             (org-foresight--day-start 0)))))))

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
           (org-foresight-workday-start "09:00")
           (org-foresight-workday-end "17:30")
           (org-foresight-workdays '(1 2 3 4 5))
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
  "With no work span the whole waking day is private time."
  (org-foresight-test--with-day "* nothing\n"
    ;; 2026-08-09 is a Sunday
    (should (equal (org-foresight-test--bands (org-foresight-test--ts 0 0 9))
                   '("grey 07:00-23:00")))))

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

(ert-deftest org-foresight-test-day-shape-defaults ()
  (org-foresight-test--with-day "* nothing\n"
    (let ((shape (org-foresight-day-shape (org-foresight-test--ts 0 0 10))))
      (should (equal (format-time-string "%H:%M" (car (plist-get shape :awake)))
                     "07:00"))
      (should (equal (format-time-string "%H:%M" (car (plist-get shape :work)))
                     "09:00"))
      (should (equal (format-time-string "%H:%M" (cdr (plist-get shape :work)))
                     "17:30")))
    ;; Sunday has no work span at all
    (should-not (plist-get (org-foresight-day-shape (org-foresight-test--ts 0 0 9))
                           :work))))

(ert-deftest org-foresight-test-day-shape-honours-the-heading ()
  "Properties on the day's own heading beat the defaults."
  (let ((file (make-temp-file "org-foresight-day" nil ".org"
                              "* 2026\n** 2026-08 August\n*** 2026-08-10 Mon\n:PROPERTIES:\n:WAKE:  06:00\n:SLEEP: 22:00\n:WORK:  10:00-16:00\n:END:\n")))
    (unwind-protect
        (let ((org-foresight-day-file file)
              (org-foresight-awake '("07:00" . "23:00"))
              (org-foresight-workday-start "09:00")
              (org-foresight-workday-end "17:30")
              (org-foresight-workdays '(1 2 3 4 5))
              (org-foresight--shape-cache nil))
          (let ((shape (org-foresight-day-shape (org-foresight-test--ts 0 0 10))))
            (should (equal (format-time-string "%H:%M" (car (plist-get shape :awake)))
                           "06:00"))
            (should (equal (format-time-string "%H:%M" (car (plist-get shape :work)))
                           "10:00"))
            (should (equal (format-time-string "%H:%M" (cdr (plist-get shape :work)))
                           "16:00")))
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
  "One office meeting costs the hour plus both journeys."
  (org-foresight-test--with-travel
      "* Project review
:PROPERTIES:
:LOCATION: 会議室A
:END:
<2026-08-10 Mon 14:00-15:00>
"
    (let ((bands (org-foresight-test--bands (org-foresight-test--ts 0 0 10))))
      ;; out arrives just in time; home lands exactly when work ends
      (should (member "travel 13:00-14:00" bands))
      (should (member "meeting 14:00-15:00" bands))
      (should (member "travel 16:30-17:30" bands)))))

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
      (should (member "travel 16:00-17:30" bands))))) ; client → home, 90

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
  "Somebody else's evening fixture must not be reported as your late night."
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
                   "After hours (invisible to capacity)")))
    ;; without the category it is ordinary work, and is reported
    (let ((org-foresight-informational-categories nil)
          (org-foresight-horizon-days 400))
      (should (org-foresight-test--signal
               "After hours (invisible to capacity)")))))

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
           (org-foresight-workday-start "09:00")
           (org-foresight-workday-end "17:30")
           (org-foresight-workdays '(1 2 3 4 5))
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
  (let ((cache (make-temp-file "org-foresight-surge" nil ".eld"
                               (prin1-to-string '(:minutes 120.0 :samples 12)))))
    (unwind-protect
        (let ((org-foresight-surge-cache-file cache)
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

(ert-deftest org-foresight-test-plan-style-is-registered ()
  "org-foresight-plan.el must register itself with the report dispatcher.
The day being rearranged is the agenda itself, so what this style adds --
where else the work could go, and what has not been asked about -- follows
the listing rather than splitting it from the verdict above."
  (let ((entry (cdr (assq 'plan org-foresight-report-renderers))))
    (should (eq (plist-get entry :body) 'org-foresight-plan-report))
    (should (eq (plist-get entry :place) 'bottom))
    (should (eq (org-foresight-report--place 'plan) 'bottom))
    (should (eq (org-foresight-report--place 'daily) 'bottom))))

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
  (let ((cache (make-temp-file "org-foresight-surge" nil ".eld"
                               (prin1-to-string '(:minutes 120.0 :samples 12)))))
    (unwind-protect
        (let* ((org-foresight-surge-cache-file cache)
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
                (org-foresight-workday-start "09:00")
                (org-foresight-workday-end "17:30")
                (org-foresight-workdays '(1 2 3 4 5))
                (org-foresight-followup-keywords '("WAIT" "DELEG"))
                (org-foresight-meeting-categories '("outlook"))
                (org-foresight-private-categories '("family" "club"))
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

(ert-deftest org-foresight-test-demo-fires-every-signal ()
  "One of everything: each signal must find its example in the demo corpus."
  (org-foresight-test--with-demo
    (let ((labels (mapcar #'car (org-foresight-signals))))
      (dolist (expected '("Meetings without prep"
                          "After hours (invisible to capacity)"
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
  (let* ((cap '(:span-min 510.0 :surge-min 0.0))
         (buf (get-buffer-create "*foresight-fit*"))
         ;; two distinct positions: a marker is identified by buffer and
         ;; offset, so an empty buffer would make every entry the same one
         (m (with-current-buffer buf
              (insert "* big\n* small\n") (copy-marker (point-min))))
         (other-m (with-current-buffer buf (copy-marker (point-max))))
         (ledger (list (list :kind 'promised :title "big" :effort 300
                             :marker m)))
         (item (propertize " reporting Scheduled: big" 'org-hd-marker m))
         (other (propertize " admin     Scheduled: small"
                            'org-hd-marker other-m))
         (small (list (list :kind 'available
                            :start (org-foresight-test--ts 9 0 10)
                            :end (org-foresight-test--ts 11 0 10))))
         (roomy (list (list :kind 'available
                            :start (org-foresight-test--ts 9 0 10)
                            :end (org-foresight-test--ts 18 0 10)))))
    (unwind-protect
        (let ((out (org-foresight-agenda--mark-wont-fit
                    (list item other) small cap ledger)))
          ;; 5:00 of work against a 2:00 gap: no ordering finds it a home
          (should (string-prefix-p org-foresight-agenda-wont-fit (car out)))
          ;; the mark replaces the prefix's blank column rather than being
          ;; appended, which would put it past the tags and defeat their
          ;; alignment
          (should (= (length item) (length (car out))))
          (should (equal other (cadr out)))
          ;; a day with a big enough gap marks nothing
          (should (equal (list item other)
                         (org-foresight-agenda--mark-wont-fit
                          (list item other) roomy cap ledger))))
      (kill-buffer buf))))

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
    ;; the mark leads the title rather than trailing it, so the tags Org
    ;; aligns at the end of the line stay at the end of the line
    (should (string-prefix-p org-foresight-agenda-wont-fit plain))
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
  (let* ((rows (org-foresight-agenda--edges
                (list :window (cons (org-foresight-test--ts 9 0 10)
                                    (org-foresight-test--ts 17 30 10)))))
         (plain (mapcar #'substring-no-properties rows)))
    (should (seq-find (lambda (r) (string-match-p "work starts ─+$" r)) plain))
    (should (seq-find (lambda (r) (string-match-p "work ends ─+$" r)) plain))
    ;; and a day with no working window has no edges to draw
    (should-not (org-foresight-agenda--edges '(:window nil)))))

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

(provide 'org-foresight-test)

;;; org-foresight-test.el ends here
