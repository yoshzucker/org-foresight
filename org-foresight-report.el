;;; org-foresight-report.el --- Agenda rendering  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yoshzucker

;; Author: yoshzucker
;; URL: https://github.com/yoshzucker/org-foresight

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Everything that turns model output into text, and nothing that computes it.
;;
;; House rules for every block rendered here:
;;
;;   - lines stay within 80 columns, budgeted with `truncate-string-to-width'
;;     so multibyte titles cannot push a table out of alignment
;;   - bars are drawn with `orgtbl-ascii-draw' over `org-foresight-bar-chars'
;;   - a block is introduced by a two-segment badge (title chip + meaning chip)
;;     so a reader can tell at a glance what question the block answers
;;
;; Blocks are appended to an agenda buffer on `org-agenda-finalize-hook'.  Which
;; blocks appear is chosen by `org-foresight-report-style', so an agenda custom
;; command can bind that symbol in its general settings to get a different
;; report without knowing anything about the blocks themselves.

;;; Code:

(require 'org-foresight-core)
(require 'org-foresight-observe)
(require 'org-agenda)
(require 'org-clock)
(require 'org-table)
(require 'seq)
(require 'cl-lib)

;;;; Style and glyphs

(defconst org-foresight-block ?█
  "The block every bar and gutter is filled with: U+2588, the full block.

Named once so the drawing has a single shape to change.  Note that it is
taller than the line it sits on -- 1300 units against a 1175-unit line in
PlemolJP, since the glyph is designed to tile a screen without seams -- so
a column of these reads as one mass rather than as separate bars.")

(defconst org-foresight-report-dot ?·
  "The character emptiness is drawn with: U+00B7, the middle dot.

Named once because it appears in two places that must not drift -- the spare
and unclaimed stretches of the bars, and the half hours the machine saw
nothing in.  They are the same statement about the same kind of absence, and
two characters that look alike but are not is the sort of difference nobody
can name and everybody can see.")

(defvar org-foresight-bar-chars " ▏▎▍▌▋▊▉█"
  "Shades (empty..full) for `orgtbl-ascii-draw' bars; unicode block elements.
Shared by the three custom agenda time-viz tables.")

(defvar org-foresight-report-style 'daily
  "Which report `org-foresight-report-render' produces on agenda finalize:
`daily'  the three today tables (Clocked / Estimate / Observed)
`review' the week-by-area review table (`org-foresight-report-week')
nil      nothing.
Bound to `review' in the \"r\" command's general settings; defaults to `daily'
for the plain `a' agenda.")

;; agent-shell-style two-segment badge (cf. `agent-shell--make-button'):
;; a filled title chip followed by an outlined meaning chip, boxed.
;;;; Badges

(defface org-foresight-report-badge-title
  '((t :inherit org-agenda-structure))
  "Left badge (filled): muted agenda fg on a subtle bg.")
(defface org-foresight-report-badge-meaning
  '((t :inherit (org-agenda-structure highlight)))
  "Right badge (solid): `shadow' fg becomes the fill; text knocked out to
the page background via inverse-video.")
;; Right badge is a solid fill (inverse-video: `shadow' fg becomes the bg,
;; text drops to the page bg).  Both badges share one box color (= `shadow'
;; fg) so the left outline meets the right fill seamlessly (agent-shell
;; style).  Set via `set-face-attribute' so a plain reload re-applies these
;; (a bare `defface' would not touch an already-defined face).
(let ((frame (face-foreground 'shadow nil t)))
  (set-face-attribute 'org-foresight-report-badge-title nil
                      :inverse-video t
                      :box (list :line-width -1 :color frame))
  (set-face-attribute 'org-foresight-report-badge-meaning nil
                      :box (list :line-width -1 :color frame)))

(defun org-foresight-report--badge (title meaning)
  "Return a styled \"TITLE MEANING\" header as an agent-shell-like badge.
GUI: adjacent filled + outlined boxed chips.  TUI: bracketed fallback."
  (if (display-graphic-p)
      (concat (propertize (concat " " title " ") 'face 'org-foresight-report-badge-title)
              (propertize (concat " " meaning " ") 'face 'org-foresight-report-badge-meaning))
    (concat "[" title "] " meaning)))

;;;; Sparklines

(defface org-foresight-report-clocked-active '((t :inherit success))
  "Sparkline fill for clocked & active (declared focus).")
(defface org-foresight-report-clocked-afk '((t :inherit shadow))
  "Sparkline fill for clocked & afk (clock running while away).")
(defface org-foresight-report-unclocked-active '((t :inherit warning))
  "Sparkline fill for unclocked & active (on-PC leak -- the actionable one).")
(defface org-foresight-report-unclocked-afk '((t :inherit font-lock-comment-face))
  "Sparkline fill for unclocked & afk (off-PC leak).")


(defvar org-foresight-report--spark-chars "▁▂▃▄▅▆▇█"
  "Vertical bar glyphs (U+2581..U+2588) for `org-foresight-report--sparkline'.
Unlike `org-foresight-bar-chars' (horizontal fill, for table bars), these stack
from the baseline so per-cell height encodes intensity — a real sparkline.")

(defun org-foresight-report--spark-char (frac)
  "Return the glyph for FRAC (0.0-1.0), or the dot when FRAC is zero or less.
Glyphs are taken from `org-foresight-report--spark-chars'."
  (if (<= frac 0.0) org-foresight-report-dot
    (let ((n (1- (length org-foresight-report--spark-chars))))
      (aref org-foresight-report--spark-chars (max 0 (min n (round (* frac n))))))))

(defun org-foresight-report--bin-frac (binned i)
  "Active fraction (0.0-1.0) of half-hour bin I in BINNED (sec per bin)."
  (min 1.0 (max 0.0 (/ (aref binned i) 1800.0))))

(defun org-foresight-report--sparkline (binned)
  "Return a 48-char active-intensity sparkline for BINNED (active sec per
half-hour).  Each cell is normalized by the fixed 30-minute (1800s) bin length."
  (let ((out (make-string 48 ?\s t)))
    (dotimes (i 48)
      (aset out i (org-foresight-report--spark-char
                   (org-foresight-report--bin-frac binned i))))
    out))

(defface org-foresight-report-empty '((t :inherit shadow))
  "A stretch the machine saw nothing in, on a bar or in a sparkline.

Named once and used everywhere emptiness is drawn, so the same absence looks
the same wherever it is met.  Before it existed the sparkline\'s blank bins
fell through to the face for clocked, active time -- bold and green, for the
one thing that had not happened -- and the same dot in the capacity bar was
neither, which is how two identical characters came to look like different
sizes."
  :group 'org-foresight)

(defun org-foresight-report--dominant-face (ca cf ua uf i)
  "Face for half-hour bin I: whichever of CA/CF/UA/UF (clocked×active/afk
48-vectors from `org-foresight-observe-coverage\') has the most seconds there.

A bin with nothing in it takes the face for emptiness rather than the first
of the four: nothing happened, and saying it in the colour of focused work is
saying the opposite."
  (let ((best-v (aref ca i)) (best-f 'org-foresight-report-clocked-active))
    (when (> (aref cf i) best-v) (setq best-v (aref cf i) best-f 'org-foresight-report-clocked-afk))
    (when (> (aref ua i) best-v) (setq best-v (aref ua i) best-f 'org-foresight-report-unclocked-active))
    (when (> (aref uf i) best-v) (setq best-v (aref uf i) best-f 'org-foresight-report-unclocked-afk))
    (if (<= best-v 0) 'org-foresight-report-empty best-f)))

(defun org-foresight-report--sparkline-colored (binned ca cf ua uf)
  "Like `org-foresight-report--sparkline', but each cell is also colored.
CA/CF/UA/UF are the clocked×active/afk 48-vectors from
`org-foresight-observe-coverage'; whichever dominates a half-hour gives that
cell its face, so one row carries both activity density and clocked status."
  (mapconcat
   (lambda (i)
     (propertize (char-to-string
                  (org-foresight-report--spark-char (org-foresight-report--bin-frac binned i)))
                 'face (org-foresight-report--dominant-face ca cf ua uf i)))
   (number-sequence 0 47) ""))

(defun org-foresight-report--hour-axis ()
  "Return a 48-char axis line (30-min cells) with hour ticks at 0/6/12/18/23."
  (let ((s (make-string 48 ?\s)))
    (dolist (h '(0 6 12 18 23) s)
      (let* ((lbl (number-to-string h))
             (start (min (* 2 h) (- 48 (length lbl)))))
        (dotimes (i (length lbl)) (aset s (+ start i) (aref lbl i)))))))

;;;; Tables

(defun org-foresight-report--cat-abbrev (cat)
  "Return a 4-column display code for category CAT."
  (cond ((equal cat "comms") "comm")
        ((equal cat "distraction") "dist")
        ((equal cat "other") "misc")
        (t (truncate-string-to-width cat 4 0 ?\s))))

(defun org-foresight-report--spark-scaled (counts)
  "Return a 48-char sparkline of COUNTS, scaled to its own busiest bin.

Unlike the activity line, which is a fraction of a fixed half-hour, a count
has no natural ceiling -- so the tallest bar means \"the worst it got today\"
and the row answers when rather than how many."
  (let ((peak (apply #'max 1 (append counts nil))))
    (mapconcat (lambda (i)
                 (char-to-string
                  (org-foresight-report--spark-char
                   (/ (float (aref counts i)) peak))))
               (number-sequence 0 47) "")))

(defconst org-foresight-report--spark-key
  '((org-foresight-report-clocked-active . "clocked")
    (org-foresight-report-unclocked-active . "off the clock")
    (org-foresight-report-clocked-afk . "clock left running"))
  "Faces the activity line can use, and what each says.")

(defun org-foresight-report--spent-key (cov)
  "Return the line naming the colours the activity line used, given COV."
  (when cov
    (concat
     (mapconcat (pcase-lambda (`(,face . ,label))
                  (concat (propertize (string org-foresight-block) 'face face)
                          " " label))
                org-foresight-report--spark-key "  ")
     "  " (propertize "·" 'face 'org-foresight-report-unclocked-afk) " away")))

(defun org-foresight-report--longest-spell (intervals)
  "Return the longest single stretch in INTERVALS, in minutes."
  (if (null intervals) 0
    (/ (apply #'max (mapcar (lambda (iv)
                              (float-time (time-subtract (cdr iv) (car iv))))
                            intervals))
       60.0)))

(defun org-foresight-report--spent-header (clock aw)
  "Return the one-line summary of what today cost, from CLOCK and AW.

Every figure is named for what it actually is.  `Clocked\\' is the org clock's
own total -- not time at the machine, which is the number it is measured
against.  A spell is one unbroken run of it, and the longest is the honest
answer to whether the day held together.  The comparison is with the last
week\\'s daily average, which says whether today was ordinary; it is not the
reserve, and must not be read as one."
  (let* ((total (plist-get clock :today-total))
         (segments (plist-get clock :today-segments))
         (longest (org-foresight-report--longest-spell
                   (plist-get clock :today-intervals)))
         (active (and aw (/ (plist-get aw :active) 60.0)))
         (avg7 (/ (plist-get clock :total) 7.0)))
    (concat
     (format "Clocked %s" (org-duration-from-minutes total))
     (when (and active (> active 0))
       (format " of %s at the machine%s"
               (org-duration-from-minutes active)
               (if (plist-get aw :screen-only)
                   (propertize " (screen only)" 'face 'shadow) "")))
     (when (> segments 0)
       (format " · %d spell%s, longest %s" segments (if (= segments 1) "" "s")
               (org-duration-from-minutes longest)))
     (when (> avg7 0)
       (format " · vs 7d %+.0f%%" (* 100.0 (/ (- total avg7) avg7)))))))

(defun org-foresight-report--spent-row (task)
  "Return the row for TASK, a plist from `org-foresight-clock-scan\\''s
:today-tasks.

What was spent, against what was said.  A single notation throughout: the
percentage of the estimate the work has consumed, where 100% is exactly on
it.  Mixing that with a multiplier for the ones that ran over would make two
columns out of one, and the eye would have to decide which it was reading."
  (let* ((spent (plist-get task :minutes))
         (effort (plist-get task :effort))
         (pct (and effort (> effort 0) (* 100.0 (/ spent (float effort)))))
         (verdict
          (if (null pct)
              (propertize "no estimate" 'face 'shadow)
            (propertize (format "%s planned  %3.0f%%"
                                (org-duration-from-minutes effort) pct)
                        'face (if (> pct 100.0)
                                  'org-foresight-report-overcommitted
                                'shadow)))))
    (org-foresight-report--actionable
     (format "%-8s %5s  %-34s %s"
             (truncate-string-to-width (or (plist-get task :category) "") 8 0 ?\s)
             (org-duration-from-minutes spent)
             (truncate-string-to-width
              (concat (when-let ((kw (plist-get task :todo)))
                        (concat (propertize kw 'face (org-get-todo-face kw)) " "))
                      (replace-regexp-in-string
                       "[\n\r]" " " (or (plist-get task :title) "?")))
              34 nil nil "…")
             verdict)
     (plist-get task :marker))))

(defun org-foresight-report--spent-apps (lead apps)
  "Return LEAD followed by APPS, an alist of (NAME . SECONDS), or nil.

The parts add up to the total in LEAD -- that is what makes the line worth
reading rather than merely suggestive -- so what will not fit on the line is
counted with `+N\\' rather than quietly dropped."
  (when apps
    (when-let ((named (org-foresight-report--name-run
                       (mapcar (pcase-lambda (`(,name . ,sec))
                                 (list :title name :effort (/ sec 60.0)))
                               apps)
                       99 (org-foresight-report--budget lead))))
      (propertize (concat lead named) 'face 'shadow))))

(defun org-foresight-report-spent (clock)
  "Return today looked back on: where the hours went, against where they were
meant to go.

One block rather than three tables.  What was planned, what was spent and
what the machine saw are three views of a single question, and answering it
in three places meant nobody could see the answer.  CLOCK is the plist from
`org-foresight-clock-scan\\'."
  (let* ((aw (org-foresight-observe-today))
         (cov (and aw (org-foresight-observe-coverage clock)))
         (tasks (plist-get clock :today-tasks)))
    (string-join
     (delq nil
           (list
            (org-foresight-report--spent-header clock aw)
            (when aw (org-foresight-report--hour-axis))
            (when aw
              (concat (if cov
                          (org-foresight-report--sparkline-colored
                           (plist-get aw :binned)
                           (plist-get cov :ca) (plist-get cov :cf)
                           (plist-get cov :ua) (plist-get cov :uf))
                        (org-foresight-report--sparkline (plist-get aw :binned)))
                      (propertize "  at the machine" 'face 'shadow)))
            (when-let ((sw (and aw (plist-get aw :switches-binned))))
              (concat (propertize (org-foresight-report--spark-scaled sw)
                                  'face 'org-foresight-report-unclocked-active)
                      (propertize "  switching" 'face 'shadow)))
            (org-foresight-report--spent-key cov)
            (when (or tasks cov) "")
            (when tasks (mapconcat #'org-foresight-report--spent-row tasks "\n"))
            (and cov (org-foresight-report--spent-apps
                      (format "↳ leak %s · "
                              (org-duration-from-minutes
                               (/ (plist-get cov :leak-sec) 60.0)))
                      (plist-get cov :leak-apps)))
            (and cov (propertize
                      (format "↳ lost %s · away from the machine"
                              (org-duration-from-minutes
                               (/ (plist-get cov :lost-sec) 60.0)))
                      'face 'shadow))
            (unless (or tasks aw)
              (propertize "(nothing clocked, and ActivityWatch is not running)"
                          'face 'shadow))))
     "\n")))

(defun org-foresight-report--category-table (rows total maxmin col2-label)
  "Return a CATEGORY/Time/%/Share ASCII table (<=80 cols), shared by the
daily and weekly Clocked views.  ROWS is a (CATEGORY . MINUTES) alist; TOTAL
and MAXMIN scale the % and bar columns; COL2-LABEL names the category column
\(e.g. \"Project\" or \"Area\")."
  (if (null rows)
      (propertize "(no clocked time)" 'face 'org-table)
    (propertize
     (concat
      (format "| %-14s | %5s | %5s | %-18s |" col2-label "Time" "%" "Share")
      "\n|" (make-string 16 ?-) "+" (make-string 7 ?-) "+" (make-string 7 ?-)
      "+" (make-string 20 ?-) "|\n"
      (mapconcat
       (lambda (r)
         (let ((cat (car r)) (min (cdr r)))
           (format "| %-14s | %5s | %5.1f | %s |"
                   (truncate-string-to-width
                    (replace-regexp-in-string "[|\n\r]" " " cat) 14 0 ?\s)
                   (org-duration-from-minutes min)
                   (if (> total 0) (* 100.0 (/ (float min) total)) 0)
                   (truncate-string-to-width
                    (orgtbl-ascii-draw min 0 (max maxmin 1) 18
                                       org-foresight-bar-chars)
                    18 0 ?\s))))
       rows "\n"))
     'face 'org-table)))

(defcustom org-foresight-report-estimate-sizes 8
  "How many estimate sizes the weekly review names.

Enough to show the shape and few enough to read at a glance.  The sizes kept
are the ones used most, since a size written once is not a habit and changing
it is not a decision anybody can act on."
  :type 'integer
  :group 'org-foresight)

(cl-defun org-foresight-report-estimates ()
  "Return the learned estimate curve set against what it was learnt from.

The fitted line is a claim and the rows beneath it are the evidence, so both
are shown: a slope nobody can check against the tasks that produced it is a
number to be taken on faith, and this whole package is an argument against
planning on faith.

Grouped by the size of the estimate rather than by category, because that is
the axis the correction now turns on and the one a reader can do something
about.  That five-minute jobs run three times over while hour-long ones
barely do is not a fact about the tool -- it is a fact about how small things
get estimated, and it is actionable on the very next one."
  (when-let* ((data (org-foresight--bias-data))
              ;; A file written before there was a curve carries the
              ;; multipliers but not the tasks behind them, and there is no
              ;; honest way to draw the second from the first.  Saying so
              ;; beats a block that quietly is not there.
              (all (or (plist-get data :by-effort)
                       (cl-return-from org-foresight-report-estimates
                         (concat
                          (org-foresight-bias-summary data) "\n"
                          "run M-x org-foresight-learn-bias again to see"
                          " the sizes behind it"))))
              ;; The sizes actually estimated with, not every size ever
              ;; written: a corpus of free-form efforts has a long tail of
              ;; ones used once, and a row per singleton buries the handful
              ;; of sizes a reader could do anything about.  Chosen by how
              ;; often each was used, shown in order of size.
              (rows (sort (seq-take (seq-sort-by #'cadr #'> all)
                                    org-foresight-report-estimate-sizes)
                          (lambda (a b) (< (car a) (car b))))))
    (let ((worst (apply #'max 1.0 (mapcar (lambda (r) (nth 2 r)) rows))))
      (string-join
       (cons
        (concat (org-foresight-bias-summary data)
                (when-let ((w (plist-get data :window)))
                  (format ", %d days" w)))
        (mapcar
         (lambda (row)
           (pcase-let ((`(,est ,n ,ratio) row))
             (format "%6s  n=%-4d ×%-5.1f %s"
                     (org-duration-from-minutes est) n ratio
                     (propertize
                      (make-string (max 1 (round (* 24 (/ ratio worst)))) ?█)
                      'face (if (> ratio 1.0)
                                'org-foresight-report-overcommitted
                              'org-foresight-report-spare)))))
         rows))
       "\n"))))

(defun org-foresight-report-week (clock)
  "Return a week-by-CATEGORY review table with a rhythm header (<=80 cols).
Mirrors the daily `org-foresight-report-clocked' layout (Area/Time/%/Share)
but aggregates the past 7 days, and leads with weekly-review insight: total,
daily average, active-day count, and the busiest day.
CLOCK is the plist from `org-foresight-clock-scan'."
  (let* ((rows (plist-get clock :rows))
         (total (plist-get clock :total))
         (days (plist-get clock :days))
         (byday (plist-get clock :byday))
         (maxmin (if rows (apply #'max (mapcar #'cdr rows)) 1))
         (active 0) (peak 0) (peakmin 0))
    (dotimes (i days)
      (when (> (aref byday i) 0) (setq active (1+ active)))
      (when (> (aref byday i) peakmin) (setq peakmin (aref byday i) peak i)))
    (if (null rows)
        (concat "Week 0:00\n"
                (propertize "(no clocked time this week)" 'face 'org-table))
      (concat
       (format "Week %s · avg %s/day · %d/%d active · peak %s %s"
               (org-duration-from-minutes total)
               (org-duration-from-minutes (/ (float total) days))
               active days
               (format-time-string "%a" (org-foresight--day-start (- days 1 peak)))
               (org-duration-from-minutes peakmin))
       "\n" (org-foresight-report--category-table rows total maxmin "Area")))))

;;;; Capacity
;; The one block that looks forward.  Everything else here reports what has
;; already happened; this answers what can still be promised, and when the day
;; will actually end.

(defface org-foresight-report-overcommitted '((t :inherit warning))
  "Face for the verdict line when a day has been promised away.")

(defvar org-foresight-verdict-extras nil
  "Functions contributing extra lines beneath the capacity verdict.

Each is called with one argument -- a survey of the horizon, or nil -- and
returns a line, or nil to add nothing.  It is free to ignore the survey and
must work when it is nil, but one that wants a survey should take the offered
one: a survey is a walk of every entry in every agenda file, and the caller
has already paid for this one.

A registry rather than a fixed call, so a file loaded later can add to the
summary without this one having to know about it.")

(defun org-foresight-report--verdict-extras (&optional scan)
  "Return the extra verdict lines, newline-prefixed, or the empty string."
  (mapconcat (lambda (fn)
               (if-let ((line (org-foresight-report--guarded fn scan)))
                   (concat "\n" line)
                 ""))
             org-foresight-verdict-extras ""))

;; Every bar and gutter is drawn in full blocks and told apart by colour, not
;; by shading.  Mixing `█' with `▓ ▒ ░' is the obvious way to make a bar and
;; the wrong one: in several monospace fonts -- PlemolJP among them -- the
;; shades sit on a different box from the full block (74 units high and 55
;; left of it, on a 1000 em), so every seam between them visibly steps.  The
;; part-width blocks are kept for what they are actually for: the last cell of
;; a run, where the duration does not fill a whole one.
;;
;; The faces inherit from the semantic ones every theme defines distinctly,
;; rather than from `org-agenda-structure' and `org-scheduled', which many
;; themes -- reasonably -- give the same colour.

(defface org-foresight-report-booked
  '((t :inherit font-lock-function-name-face))
  "Meetings and work already placed at a time.")
(defface org-foresight-report-travel '((t :inherit font-lock-keyword-face))
  "Time spent getting somewhere: booked, but not spent working.")
(defface org-foresight-report-promised '((t :inherit warning))
  "Effort accepted but not yet placed.")
(defface org-foresight-report-surge '((t :inherit warning))
  "The reserve held back for interruptions: an outline around nothing.

Drawn as a rule rather than as a fill, because that is what it is -- room
kept open for work that has not arrived, and filling it in would claim it for
something.  The outline takes the colour that marks an overrun, since the
reserve is the last thing between a day and one: when it has to be spent, the
day is over its limit and simply has not admitted it yet.

Emacs draws a face box around each run of the face, so a segment becomes one
rectangle rather than a fence of separate cells.")
(defface org-foresight-report-spare '((t :inherit success))
  "Time that may still be promised, and the gaps it is made of.
The one quantity on the board worth growing, so it is worth a colour of its
own rather than another step down a grey ramp.")
(defface org-foresight-report-private '((t :inherit font-lock-string-face))
  "Life: commitments that are not work and are not capacity either.
Distinct from both the claimed hours and the empty ones, because it is
neither -- and a day where the two are told apart is a day where the
question \"can this move\" has an obvious answer.")

(defconst org-foresight-report-margin " "
  "The column every line but a badge starts at.

One rule, three levels.  A badge is the only thing outdented to the frame
edge, because a badge is the one thing read by scanning rather than by
reading -- an eye running down the left edge should hit section headings and
nothing else.  Everything a section contains sits at the margin: verdicts,
keys, bars and rows alike, so a block's picture lines up with the block's
words.  Anything indented further belongs to the line above it, which is the
only thing depth is allowed to mean here.

It is also what Org's agenda does, and a foresight row is meant to be
indistinguishable from an agenda line to the commands that act on it.")

(defconst org-foresight-report-columns 80
  "The width every line of a report block is written to fit.

Not a preference about screens.  The agenda is read in a window beside
something else as often as not, and a block whose lines wrap stops being a
block -- an eye scanning a column of figures is handed a ragged paragraph
instead.  A line that cannot be shortened gives up its least urgent term
rather than running past this.")

(defcustom org-foresight-bar-width 40
  "Width in columns of the capacity bar."
  :type 'integer
  :group 'org-foresight)

(defvar org-foresight-report--bar-segments
  `((:key :booked-min    :face org-foresight-report-booked   :label "booked")
    (:key :travel-min    :face org-foresight-report-travel   :label "travel")
    (:key :private-min-in-span :face org-foresight-report-private
          :label "borrowed")
    (:key :committed-min :face org-foresight-report-promised :label "promised")
    (:key :reserve-min   :face org-foresight-report-surge    :label "reserve"
          :glyph ?\s :box t)
    (:key :spare-min     :face org-foresight-report-spare    :label "spare"
          :glyph ,org-foresight-report-dot))
  "The bar's segments in order, each a plist of plist-key, face, label and glyph.

They divide the work span exactly: booked and travel are the hours already
spoken for, promised what is still owed on effort accepted without a time,
reserve what is held for the three ways the rest of the day goes elsewhere
-- work arriving, time unrecorded, time away from the desk -- and spare
whatever survives all four.  The words are the ones the grid below
uses for the same things, so the two blocks can be read as one account.

Claimed time is drawn in full blocks and told apart by colour.  Spare is
drawn in dots, as the grid draws a free stretch -- one word for one thing,
and the same reason it works there: a run of dots reads as absence in a way
that a coloured block, however pale, does not.  Nothing is lost by it, since
a horizontal bar carries its quantity in length rather than in ink.")

(defface org-foresight-report-unclocked
  '((t :inherit org-foresight-report-surge))
  "Elapsed working time no clock was running for, and no watcher explains.

The reserve's own look, because it is the reserve being spent: the outline
held open ahead of NOW, found empty behind it.  A face of its own all the
same, and not the reserve's face itself -- Emacs draws one box around each
run of identical face, so a segment sharing a face with the one beside it
loses the edge between them, and the bar stops saying where one ends.")

(defface org-foresight-report-away
  '((t :inherit org-foresight-report-unclocked-afk))
  "Elapsed working time the watcher says you were away from the machine for.

Dots, not an outline, and the distinction is the point.  `unclocked\=' is an
hour that was there and went unwritten -- a failure of the record, and an
outline says exactly that: the hour is present, nothing is inside it.  This
is an hour you were not present for, which is not a recording failure but an
absence, and absence is what the dot has meant everywhere else here.

It also means the two never have to be told apart by a hairline.  Set side by
side as blanks they are two boxes touching, and two touching boxes read as
one; a change of shape cannot be missed.

The colour is the one the sparkline gives the same hours in `[Spent]\='.")

(defvar org-foresight-report--behind-segments
  `((:key :baseline-min   :face org-foresight-report-booked :label "baseline")
    (:key :surge-min     :face org-foresight-report-surge  :label "surge")
    (:key :borrowed-min  :face org-foresight-report-private :label "borrowed")
    (:key :unclocked-min :face org-foresight-report-unclocked
          :label "unclocked" :glyph ?\s :box t)
    (:key :away-min      :face org-foresight-report-away
          :label "away" :glyph ,org-foresight-report-dot))
  "Segments of the elapsed bar, read from `org-foresight-behind\='.

They divide the working hours NOW has passed, as the bar above divides the
ones it has not.

`baseline\=' and `surge\=' are the two halves of the clock, told apart by the
one thing that is checked: whether the work carries a dated surge mark.  The
pair is named the way load is named everywhere -- the level a day was already
running at, and what landed on top of it -- because that is all the mark
records.  A label saying the work was *planned* would claim a plan had been
consulted, and none is: a task written this morning and clocked at noon is
baseline, and so is effort spent long past its estimate.

Their sum is the clock inside the working hours.  Add the time worked outside
them, named on the row below, and it is the `Clocked\=' total in `[Spent]\='.

The colours are chosen so the two halves can be read across the seam.  Work
clocked against a plan takes the same face as `booked\=', because it is the
same work -- only on the other side of NOW.  Work that
arrived takes the reserve\='s face for the same reason: the outline held for
an interruption and the interruption that came are one story told twice.

The two unrecorded segments are drawn as outlines, and `unclocked\=' is drawn
in the reserve's own face -- the same colour, the same box, the same blank
cell.  That is not a coincidence to be tidied away: the reserve is the
forecast of exactly these hours, held back for work arriving and time going
unrecorded, and these are that forecast coming true.  Read across the seam,
an outline held open ahead of NOW becomes, behind it, either a fill (the
interruption arrived and somebody clocked it) or the same outline again (the
hour went, and nothing was written in it).

Not dots.  A dot means an hour nobody wanted, which is what `spare\=' and
`unclaimed\=' are; an unrecorded hour was wanted and used, and only the record
of it is missing.  An outline says exactly that -- the hour is there, and
nothing is written inside it.

`away\=' keeps the colour the sparkline gives it in `[Spent]\=', because the
distinction is the same one: unrecorded at the keyboard is the actionable
half, unrecorded away from it is not.")

(defvar org-foresight-report--off-behind-segments
  `((:key :worked-min :face org-foresight-report-booked :label "borrowed")
    (:key :own-min    :face org-foresight-report-private :label "own"
          :glyph ,org-foresight-report-dot))
  "Segments of the elapsed part of the waking day outside the working hours.

Two, because two is all that is known.  The clock says which of those hours
were worked -- an early start, a evening picked back up, the twenty minutes a
declared lunch break did not stop -- and about the rest it says nothing,
which is the honest reading of an hour off: it was yours, and what you did
with it is not this tool's business.

`worked\=' is drawn in the same face as `booked\=' for the same reason the
elapsed working hours are: it is work, and work is one colour here wherever
it happens.  That is the whole point of drawing it on this row -- an hour of
work in the evening should look like the work it is, sitting in the hours it
took.")

(defvar org-foresight-report--off-segments
  `((:key :private-min  :face org-foresight-report-private  :label "private")
    (:key :borrowed-min :face org-foresight-report-booked   :label "borrowed")
    (:key :unclaimed-min :face org-foresight-report-spare   :label "unclaimed"
          :glyph ,org-foresight-report-dot))
  "Segments of the second bar, dividing what is left of the waking day outside
the work span.

`unclaimed' rather than `free': the grid uses `free' for work time nothing
has claimed, and an hour with no work in it and an hour off with no plans in
it are not the same kind of emptiness.")

(defun org-foresight-report--bar-scale (cap)
  "Return minutes per column, chosen so both bars can share a scale.

Two bars drawn to their own widths cannot be compared, and comparing them is
the point: a working day that dwarfs what is left of it is the thing worth
seeing.
So one scale serves both, taken from the longer of the two *spans*.

Deliberately not from the promises.  Scaling to those would shrink an
overcommitted day back until it fitted the width, which is the one day that
must not be made to look like it fits."
  (let ((longest (max (plist-get cap :span-min)
                      (+ (max 0.0 (or (plist-get cap :private-min) 0.0))
                         (max 0.0 (or (plist-get cap :borrowed-min) 0.0))
                         (max 0.0 (or (plist-get cap :unclaimed-min) 0.0)))
                      1.0)))
    (/ longest (float org-foresight-bar-width))))

(defconst org-foresight-report-rule ?\u2503
  "The glyph a bar is divided by: where NOW falls, and where the span runs out.

One glyph for both, in two colours.  They are the same kind of statement --
here is a line across the day -- and a second glyph would have to be found in
the same font, at the same width, for no gain: what separates them is not
their shape but which line they are, and colour is what this block already
uses to say which.")

(defface org-foresight-report-now
  '((t :inherit org-foresight-report-private))
  "The rule where NOW falls in the bar.

Life's own colour, and deliberately so.  Everything to the right of this mark
is the part of the day still to be decided, which is the part the whole block
exists to protect; drawing the mark in the colour of the hours being defended
says what the line is for rather than merely where it is.

The `\= now\=' rule in the agenda below is set to match, so the mark in the bar
and the line in the day are visibly one statement about one time.")

(defcustom org-foresight-bar-max-width 64
  "Widest a bar may be drawn, including any overflow past the span.
The line it sits on has to fit the screen; a day promised three times over
would otherwise run off the end of it."
  :type 'integer
  :group 'org-foresight)

(defun org-foresight-report--glyph-face (seg)
  "Return the face SEG\'s glyph is drawn in.

The box is a property of the segment, asked for by `:box\', and only a blank
segment may ask.  A box is drawn in the face\'s own foreground, so around a
cell filled with a block of that same colour it draws nothing at all -- it
was on `surge\' once, invisible from the day it went on, claiming to mark
something and marking nothing.  A blank cell is the opposite case: the box is
the only thing drawing it.

So the box means one thing exactly -- *this hour is here and there is nothing
inside it* -- and it is the reserve\'s: the cell held open ahead of NOW, and
the same cell found empty behind it.  When the interruption it was held for
does arrive, what marks it is the colour, which is the reserve\'s colour, on
a cell that is now full.

Dots never take a box either: a dot draws itself, and bounding it would bound
what needs no bounding.

Put on here rather than on any face, so that no theme can drop it and so
that two segments meant to look alike can still have a face each: Emacs draws
one box per run of identical face, and two of them sharing one would be
bounded together and lose the edge between them.

A line-width of -1 draws the box inside the character cell, so the row keeps
the height of every other row on the page.  It is the narrowest a box can be.

The box is also why the bar is drawn in whole cells.  A run ending in a
part-width block would have its ink stop partway across the last cell while
the box went on to the end of it, and the two edges would disagree; if
fractional cells are ever wanted back, the box is what has to go.

A segment drawn in dots keeps the sparkline\'s own dot: the weight comes from
`org-foresight-report-empty\' and the colour from the segment, so identical
characters look identical wherever they are met while still saying what they
mean.  Order matters -- the segment first so its foreground wins, and only
the weight is inherited."
  (let* ((face (plist-get seg :face))
         (glyph (plist-get seg :glyph)))
    (cond
     ((eq glyph org-foresight-report-dot)
      (list face 'org-foresight-report-empty))
     ((plist-get seg :box) (list '(:box (:line-width -1)) face))
     (t face))))

(defun org-foresight-report--draw-bar (cap segments per-column &optional limit
                                           max-width)
  "Return SEGMENTS of CAP drawn at PER-COLUMN minutes to the column.

LIMIT, when given, marks where the span runs out -- so an overcommitted day
shows its overflow rather than being clipped back to something that fits.
Runaway overflow is cut at MAX-WIDTH, or `org-foresight-bar-max-width\', and
ended with `…\': past a point the exact length of the impossible stops
mattering, and the figure above says it anyway.

MAX-WIDTH is a parameter because a bar that starts partway across the line
has less of it left.  The `Ahead\' row is indented by the whole of the row
above, and an overcommitted afternoon drawn to the full width from there
would run off the window -- which is the one failure the ellipsis exists to
prevent."
  (let ((bar "")
        (total 0.0))
    (dolist (seg segments)
      (let* ((mins (max 0.0 (or (plist-get cap (plist-get seg :key)) 0.0)))
             (n (round (/ mins per-column))))
        (setq total (+ total mins))
        (setq bar (concat bar (propertize
                               (make-string (max 0 n)
                                            (or (plist-get seg :glyph)
                                                org-foresight-block))
                               'face (org-foresight-report--glyph-face seg))))))
    (when (and limit (> total limit))
      (let ((mark (round (/ limit per-column))))
        (when (< mark (length bar))
          (setq bar (concat (substring bar 0 mark)
                            (propertize (string org-foresight-report-rule)
                                        'face
                                        'org-foresight-report-overcommitted)
                            (substring bar (1+ mark)))))))
    (let ((most (max 1 (or max-width org-foresight-bar-max-width))))
      (if (<= (length bar) most)
          bar
        (concat (substring bar 0 (1- most))
                (propertize "…" 'face
                            'org-foresight-report-overcommitted))))))

(defun org-foresight-report--bar-room (indent)
  "Return the columns a bar indented by INDENT has left on its line."
  (- org-foresight-report-columns
     (string-width org-foresight-report-margin)
     (org-foresight-report--bar-column)
     indent))

(defun org-foresight-report--bar (cap &optional indent)
  "Return the work span drawn as a stacked bar, or nil.

The whole bar is the work span, so meetings and journeys appear in it rather
than being silently deducted first: the question \"where did the day go\" is
answered by the same picture as \"what is left\".

INDENT is how far across the line it starts, which is what is left of the
line for it to use."
  (when (> (plist-get cap :span-min) 0)
    (org-foresight-report--draw-bar
     cap org-foresight-report--bar-segments
     (org-foresight-report--bar-scale cap)
     (plist-get cap :ahead-min)
     (min org-foresight-bar-max-width
          (org-foresight-report--bar-room (or indent 0))))))

(defun org-foresight-report--behind-bar (behind per-column)
  "Return BEHIND drawn at PER-COLUMN minutes to the column, or nil.

Nil before the working day has begun.  There is no elapsed span to divide
then, and a row labelled with a zero would say less than no row at all."
  (when (and behind (> (or (plist-get behind :behind-min) 0) 0))
    (org-foresight-report--draw-bar
     behind org-foresight-report--behind-segments per-column)))

(defun org-foresight-report--off-plist (cap behind)
  "Return what is known about the elapsed off hours of CAP, from BEHIND.

Built here rather than kept in either of them: it is one subtraction across
two answers -- how much of the evening has gone, and how much of that the
clock accounts for -- and neither is the place for a figure that needs the
other."
  (let* ((gone (max 0.0 (or (plist-get cap :off-behind-min) 0.0)))
         ;; Never more than has actually elapsed: the clock outside the
         ;; working hours may reach past the end of the waking day, and an
         ;; hour claimed twice would leave `own' negative.
         (worked (min gone (if behind
                               (max 0.0 (or (plist-get behind :outside-min) 0.0))
                             0.0))))
    (list :worked-min worked :own-min (- gone worked))))

(defun org-foresight-report--off-behind-bar (cap behind per-column)
  "Return CAP's elapsed off hours drawn at PER-COLUMN, or nil."
  (when (and behind (> (or (plist-get cap :off-behind-min) 0) 0))
    (org-foresight-report--draw-bar
     (org-foresight-report--off-plist cap behind)
     org-foresight-report--off-behind-segments per-column)))

(defun org-foresight-report--off-bar (cap &optional indent)
  "Return what is left of the waking day outside the work span, to scale.

INDENT is how far across the line it starts, which is what is left of the
line for it to use."
  (let ((total (+ (max 0.0 (or (plist-get cap :private-min) 0.0))
                  (max 0.0 (or (plist-get cap :borrowed-min) 0.0))
                  (max 0.0 (or (plist-get cap :unclaimed-min) 0.0)))))
    (when (> total 0)
      (org-foresight-report--draw-bar
       cap org-foresight-report--off-segments
       (org-foresight-report--bar-scale cap) nil
       (min org-foresight-bar-max-width
            (org-foresight-report--bar-room (or indent 0)))))))

(defconst org-foresight-report--bar-stub "%-4s %5s "
  "Format of the label a bar carries: what it is, and how long it is.
The total sits on the bar's own line rather than above it, which is what
lets the bars be drawn one under the other -- and drawn one under the
other is the only way a shared scale is any use, since comparing them is the
whole reason they share one.")

(defun org-foresight-report--bar-column ()
  "Return the column every bar starts at, and every legend lines up with."
  (length (format org-foresight-report--bar-stub "Work" "00:00")))

(defun org-foresight-report--bar-line (label total-min bar)
  "Return BAR led by LABEL and TOTAL-MIN, or nil without a BAR."
  (when bar
    (concat (format org-foresight-report--bar-stub label
                    (org-duration-from-minutes (max 0.0 (or total-min 0.0))))
            bar)))

(defun org-foresight-report--key (cap segments &optional extra suffix)
  "Return a legend naming each of SEGMENTS of CAP, aligned under its bar.

EXTRA are further terms that belong to the legend without belonging to the
bar, as an alist of (AFTER-KEY TERM...): the terms are placed directly after
the segment whose `:key\=' is AFTER-KEY, or at the end under a nil key.  Position
is how a term says what it qualifies -- a figure about the clock, put after
the two clocked segments, is read as a note on them; the same figure at the
end of the line is read as a fifth segment, which it is not.

They wrap with the rest rather than being stuck on the end, which is the only
way a line that is already full can take another term without running past
the window.

SUFFIX is glued to the final term instead of being one of its own, so that a
mark belonging to the whole legend cannot be wrapped onto a line by itself --
which is what it looks like when it happens, and it says nothing there.

Sits with its bar rather than between the two, so the bars stay adjacent.
Segments that are zero are left out: a day with no travel in it has nothing
to say about travel, and saying it anyway costs the width the rest needs.

Two spaces between terms rather than a `+' chain.  The total is on the bar's
own line now, so the legend has stopped being a sum to check and gone back to
being what it is -- a list of what the colours mean."
  (let* ((parts (append
                 (apply
                  #'append
                  (mapcar
                   (lambda (seg)
                     (if (eq seg 'rule)
                         ;; The seam, in the middle of the legend it divides:
                         ;; everything before it is measured and everything
                         ;; after it is forecast, which is the same thing the
                         ;; rule says about the bar itself.
                         (list (propertize (string org-foresight-report-rule)
                                           'face 'org-foresight-report-now))
                     (let* ((key (plist-get seg :key))
                            (mins (max 0.0 (or (plist-get cap key) 0.0)))
                            (term
                             (when (> mins 0)
                               (concat (propertize
                                        (string (or (plist-get seg :glyph)
                                                    org-foresight-block))
                                        'face (org-foresight-report--glyph-face
                                               seg))
                                       " " (plist-get seg :label) " "
                                       (org-duration-from-minutes mins)))))
                       (delq nil (cons term (cdr (assq key extra)))))))
                   segments))
                 (cdr (assq nil extra))))
         (parts (if (and suffix parts)
                    (append (butlast parts)
                            (list (concat (car (last parts)) " " suffix)))
                  parts))
         (indent (make-string (org-foresight-report--bar-column) ?\s))
         (line indent)
         out)
    (when parts
      ;; Wrapped rather than truncated: with every segment in play the legend
      ;; is simply longer than a line, and one missing its last term is worse
      ;; than one that takes two lines to finish.
      (while parts
        (let ((piece (pop parts)))
          (if (or (equal line indent)
                  ;; Less the margin, which is added to every line of the
                  ;; block after this has finished wrapping it: a legend
                  ;; measured without it fits the budget everywhere except
                  ;; the buffer.
                  (<= (+ (string-width line) 2 (string-width piece))
                      (- org-foresight-report-columns
                         (string-width org-foresight-report-margin))))
              (setq line (if (equal line indent)
                             (concat line piece)
                           (concat line "  " piece)))
            (push line out)
            (setq line (concat indent piece)))))
      (push line out)
      (string-join (nreverse out) "\n"))))

(defun org-foresight-report--bar-key (cap)
  "Return the legend naming each of the work bar's segments in CAP."
  (org-foresight-report--key cap org-foresight-report--bar-segments))

(defun org-foresight-report--legend (plist measured forecast ruled)
  "Return one legend for PLIST: its MEASURED terms, then its FORECAST ones.

RULED puts the rule between the two halves, which is the whole grammar of the
line: what is named before it was measured, what is named after it is
predicted, and that is the same statement the rule makes on the bar.  Nil
before anything has elapsed -- there is no measured half then, and a mark
dividing one thing from nothing says only that the reader has missed
something."
  (org-foresight-report--key
   plist (append (and ruled measured) (and ruled '(rule)) forecast)))

(defun org-foresight-report--work-key (cap behind)
  "Return the legend for the working day: what it has been, and what is left."
  (org-foresight-report--legend
   (append behind cap)
   org-foresight-report--behind-segments
   org-foresight-report--bar-segments
   (and behind (> (or (plist-get behind :behind-min) 0) 0))))

(defun org-foresight-report--off-key (cap behind)
  "Return the legend for the hours off: what they have been, and what is left."
  (org-foresight-report--legend
   (append (org-foresight-report--off-plist cap behind) cap)
   org-foresight-report--off-behind-segments
   org-foresight-report--off-segments
   (and behind (> (or (plist-get cap :off-behind-min) 0) 0))))
(defun org-foresight-report--join-at-now (elapsed ahead)
  "Return ELAPSED and AHEAD as one bar, ruled where NOW falls.

The rule replaces the cell it stands on rather than being inserted between
them, which is what the overflow rule already does and what keeps the bar the
length its scale says it is -- a bar one column longer than the day would
stop being comparable with the one drawn beneath it.

One minute is covered by the rule.  That is the price of drawing the seam at
all, and it is the same price the overflow pays: the alternative is a seam
that has to be inferred from an indent, which is what this replaced."
  (let ((rule (propertize (string org-foresight-report-rule)
                          'face 'org-foresight-report-now)))
    (cond
     ;; Nothing has elapsed: no seam, because there are not two halves yet.
     ((or (null elapsed) (string-empty-p elapsed)) ahead)
     ;; Nothing is left: the seam is the end of the day, and takes the last
     ;; cell of it rather than growing the bar past its own span.
     ((or (null ahead) (string-empty-p ahead))
      (concat (substring elapsed 0 -1) rule))
     (t (concat elapsed rule (substring ahead 1))))))

(defun org-foresight-report--fit-terms (terms keep width)
  "Return TERMS joined into a line of at most WIDTH columns.

Terms go from the end until it fits; the first KEEP that survive are never
dropped, being the answer itself, and a line that drops its answer to make
room for its footnotes has stopped being worth reading.

Dropped silently, because nothing that can go is only said here: the reserve
is drawn in the bar directly below and the estimate correction has a block
of its own in the weekly review.  The alternative is a line that wraps, and
a wrapped verdict does not read as a long verdict -- it reads as a short one
followed by a fragment of something else."
  (let ((terms (delq nil (copy-sequence terms))))
    (while (and (> (length terms) keep)
                (> (string-width (apply #'concat terms)) width))
      (setq terms (butlast terms)))
    (apply #'concat terms)))

(defun org-foresight-report--verdict (cap)
  "Return the one-line answer for capacity plist CAP.

Everything the block exists to say, in the width of a single line: how much
of the working day is left, whether what is owed still fits inside it, and
the hour it actually ends.

The hours left are the subject, rather than the hours that may still be
promised.  They are different quantities, both were once on this line, and
carrying both made it too long to read.  The one kept is the one that
decides anything after the middle of the afternoon: what to do next turns on
how much day there is, not on how much of it is still unspoken for.  What
may still be promised has not gone anywhere -- it is `spare' in the bar
below, which is where a quantity that wants comparing belongs.

An overrun and a finishing time are the same fact said two ways, so only one
of them is shown: the hour while the day still holds it, the overrun once it
does not."
  (let* ((headroom (plist-get cap :headroom-min))
         (committed (or (plist-get cap :committed-min) 0.0))
         ;; Where it lands rather than where it fits: a day whose work runs
         ;; past six does not stop having an end, and "will not fit" was
         ;; refusing to name one on exactly the days it mattered most.  Nil is
         ;; kept for what it now honestly means -- not today at all.
         (finish (plist-get cap :lands))
         (reserve (or (plist-get cap :reserve-min) 0.0))
         (budget (or (plist-get cap :reserve-day-min) 0.0))
         (span (or (plist-get cap :span-min) 0.0))
         (bias (or (plist-get cap :bias-min) 0.0)))
    (org-foresight-report--fit-terms
     (list
      (format "Work %s" (org-duration-from-minutes (plist-get cap :span-min)))
      (format " · %s left of the day"
              (org-duration-from-minutes
               (max 0.0 (or (plist-get cap :ahead-min) 0.0))))
      (cond
       ;; A reserve larger than the day it is held back from is not a day that
       ;; is over-committed -- an empty day would say the same -- it is a
       ;; reserve that has stopped describing the day.  `OVER by\=' would name
       ;; the wrong thing to fix, so name the right one, and name it here
       ;; where the line cannot be trimmed: the figure below is the third term
       ;; to be dropped, which is how the cause used to vanish and leave the
       ;; effect standing.
       ((and (> span 0) (>= reserve span))
        (propertize (format " · reserve %s exceeds the day"
                            (org-duration-from-minutes reserve))
                    'face 'org-foresight-report-overcommitted))
       ((< headroom 0)
        (propertize (format " · OVER by %s" (org-duration-from-minutes
                                             (- headroom)))
                    'face 'org-foresight-report-overcommitted))
       ;; A day with nothing owed has nothing to land; naming an hour for it
       ;; would be answering a question nobody asked.
       ((<= committed 0) nil)
       (finish
        (let* ((work (plist-get cap :work))
               ;; Late against the hour work was meant to be over -- the end
               ;; of the last interval, not of whichever one it ran past.
               (ends (cdr (car (last work))))
               (late (and ends (time-less-p ends finish))))
          (propertize (format " · ends %s" (format-time-string "%H:%M" finish))
                      'face (if late 'org-foresight-report-overcommitted
                              'default))))
       (t (propertize " · not today"
                      'face 'org-foresight-report-overcommitted)))
      ;; What the day is holding back, and why it is smaller than it looks.
      ;; Both in minutes, in the same unit as the hours beside them: a
      ;; multiplier cannot be compared with an hour, and these are the two
      ;; terms that decide whether the hour is there.
      ;;
      ;; Said against the whole day\='s allowance, and said even at nothing.
      ;; The reserve shrinks through the day, by the hours going past and by
      ;; the interruptions that have already landed -- so on the day it
      ;; matters most it is smallest, and a figure that disappears when it
      ;; gets small disappears exactly then.  "0:00 of 1:35" is not an empty
      ;; line: it is the day saying the allowance was real and is now spent.
      (when (> budget 0)
        (format " · reserve %s of %s"
                (org-duration-from-minutes (max 0.0 reserve))
                (org-duration-from-minutes budget)))
      ;; Shown whenever it is doing anything, so a day that has shrunk reads
      ;; as "my estimates are optimistic" rather than "the tool is harsh".
      (when (>= (abs bias) org-foresight-bias-visible-minutes)
        (format " · est %s%s" (if (> bias 0) "+" "−")
                (org-duration-from-minutes (abs bias)))))
     3
     (- org-foresight-report-columns
        (string-width org-foresight-report-margin)))))

(defun org-foresight-report--bars (cap &optional behind)
  "Return CAP's bars, with BEHIND from `org-foresight-behind\=' leading them.

Two bars: the working day, and the waking hours outside it.

The working day is one bar, not two.  It is one time axis -- what has gone
and what is left are the same day, divided by a moment -- so it is drawn as
one, ruled where NOW falls.  It was two rows once, the second indented by the
width of the first, and that made the seam something to be inferred from an
alignment rather than something on the page.  A reader should not have to
measure a picture to find out what it says.

The rule is measured in display columns, not characters: the blocks are drawn
from a font whose idea of their width is what decides where the seam lands,
and `length\=' would be guessing at it.

Two legends, one either side, and the upper one ends in the rule so it is
clear which half of the bar it names.  Above and below is not left and right,
and the mark is what makes the mapping something read rather than guessed.
Without BEHIND there is no seam and no upper legend: nothing has elapsed, and
a legend for an empty half is a heading over nothing.

The bars are set one directly under the other, which is the whole use of
their shared scale: a working day that dwarfs the hours it leaves behind is
exactly the thing worth seeing, and nothing between them would let it be
seen.

Unclaimed hours are not capacity waiting to be spent: the emptiness is what
makes room for anything new, and a day that quietly borrows from it should
have to say so."
  (let* ((per-column (org-foresight-report--bar-scale cap))
         (elapsed (org-foresight-report--behind-bar behind per-column))
         (used (if elapsed (string-width elapsed) 0))
         (ahead (org-foresight-report--bar cap used))
         (gone (org-foresight-report--off-behind-bar cap behind per-column))
         (spent (if gone (string-width gone) 0))
         (off (org-foresight-report--off-bar cap spent)))
    (when ahead
      (string-join
       (delq nil
             (list (org-foresight-report--work-key cap behind)
                   (org-foresight-report--bar-line
                    "Work" (plist-get cap :span-min)
                    (org-foresight-report--join-at-now elapsed ahead))
                   (org-foresight-report--bar-line
                    "Off" (plist-get cap :off-min)
                    (org-foresight-report--join-at-now gone off))
                   (org-foresight-report--off-key cap behind)))
       "\n"))))

;;;; Forward load

(defconst org-foresight-report--load-stub " %-9s %15s  "
  "Format of a forward-load row's leader: which day, and what it can take.
Wider than the bars' own stub above, because a date and a verdict take more
room than a word and a duration -- which is why the two blocks cut their
bars at different lengths, and why each has to be told its own.")

(defcustom org-foresight-load-rows 5
  "How many working days the forward-load block shows.

Enough to answer \"then when?\", and no more.  A fortnight of rows is a
fortnight of scrolling for a question that is nearly always settled by the
first day with room in it."
  :type 'integer
  :group 'org-foresight)

(defun org-foresight-report-load (&optional days scan now landing)
  "Return the coming days drawn as today is, so that they can be compared.

This is the block that turns \"I'm busy\" into a date.  Each row is one
working day: what may still be promised on it, and the same stacked bar the
capacity block draws above -- same segments, same colours, same scale.  The
point of a forward view is to hold it against today, and two pictures of the
same thing drawn differently cannot be held against each other.

The figure is `:headroom-min': free time less what is already promised and
the reserve held back for interruptions.  Positive is what may still be taken
on, negative is what would have to come off first.  It is the number the
verdict states for today, asked of each day in turn -- one definition, not a
second one that happens to live in a table.

Capacity is worked out only for the days that will be drawn.  Costing out a
fortnight to print five rows is the sort of expense that never shows in a
benchmark and always shows in a keystroke."
  (let* ((days (or days org-foresight-horizon-days))
         (today (org-foresight--day-start 0))
         (scan (or scan (org-foresight-scan days today)))
         today-cap rows)
    (catch 'enough
      (dotimes (i days)
        (let ((day (time-add today (days-to-time i))))
          (when (org-foresight-work-intervals day)
            (let ((cap (org-foresight-capacity day scan now)))
              (when (zerop i) (setq today-cap cap))
              (when (plist-get cap :work)
                (push (cons day cap) rows)
                (when (>= (length rows) org-foresight-load-rows)
                  (throw 'enough nil))))))))
    (setq rows (nreverse rows))
    (if (null rows)
        (propertize "(no working days in the horizon)" 'face 'org-table)
      ;; One scale for every row, and the same one the block above used, so a
      ;; day appearing in both is drawn at the same length in both.
      (let ((per-column
             (max (/ (apply #'max 1.0
                            (mapcar (lambda (r) (plist-get (cdr r) :span-min))
                                    rows))
                     (float org-foresight-bar-width))
                  (if today-cap
                      (org-foresight-report--bar-scale today-cap)
                    0.0))))
        (concat
         (mapconcat
         (lambda (r)
           (let ((head (plist-get (cdr r) :headroom-min)))
             (concat
              (format org-foresight-report--load-stub
                      (format-time-string "%a %m-%d" (car r))
                      (if (>= head 0)
                          (format "%s to promise" (org-duration-from-minutes head))
                        (propertize
                         (format "OVER by %s"
                                 (org-duration-from-minutes (- head)))
                         'face 'org-foresight-report-overcommitted)))
              ;; Cut to what is left of this row's line, exactly as the
               ;; capacity bar is.  The leader here is longer than the one
               ;; there, so a day promised three times over drew a bar that
               ;; fitted the capacity block and ran off the end of this one.
               (org-foresight-report--draw-bar
                (cdr r) org-foresight-report--bar-segments per-column
                (plist-get (cdr r) :ahead-min)
                (min org-foresight-bar-max-width
                     (- org-foresight-report-columns
                        (string-width (format org-foresight-report--load-stub
                                              "" ""))))))))
          rows "\n")
         (when-let ((line (org-foresight-report--landing-line landing)))
           (concat "\n" line)))))))

(defun org-foresight-report--due-text (day)
  "Return DAY as a due date: the weekday within the week, the date past it."
  (let ((off (org-foresight--day-of day (org-foresight--day-start 0))))
    (format-time-string (if (and (>= off 0) (< off 7)) "%a" "%a %m-%d") day)))

(defun org-foresight-report--landing-name (item width)
  "Return ITEM's title cut to WIDTH, carrying the whole of it as help.

A cut title is unreadable exactly when it matters -- two projects whose names
differ after the twentieth character read as the same row.  The full text
goes on as `help-echo', which is what the mode line and a tooltip show, so
the row stays one line and the name is still recoverable without leaving it."
  (let ((title (or (plist-get item :title) "?")))
    (propertize (truncate-string-to-width title width nil nil "…")
                'help-echo title)))

(defun org-foresight-report-landing (&optional landing)
  "Return the dated commitments, earliest first, as rows for the board.

The list the one-line verdict in `Load\=' is a summary of.  That line names
the soonest date that cannot be met and counts the rest; this is the rest --
every commitment with a date, what it still needs, and where the week stops
holding them.

Every row carries its own marker, so \[org-agenda-schedule] and the rest of
the agenda\='s vocabulary work here as they do everywhere else on the board.
That is the point of the section rather than a courtesy: a list of deadlines
you cannot act on from is a list you have to go and find again somewhere
else.

A rule is drawn under the last commitment of any window that does not fit,
carrying what is owed by then, what is free before then, and the difference.
Under the ones that do fit there is nothing: a rule saying a date is
comfortable is explaining a problem the reader does not have, and a board
that says so on every line stops being read."
  (let* ((landing (or landing (org-foresight-landing)))
         (entries (plist-get landing :deadlines))
         (sitting (org-foresight--longest-sitting)))
    (if (null entries)
        (propertize "(nothing is dated)" 'face 'org-table)
      (string-join
       (apply
        #'append
        (mapcar
         (lambda (e)
           (append
            (mapcar
             (lambda (u)
               (org-foresight-report--actionable
                (format " %-9s %6s  %s%s"
                        (format-time-string "%a %m-%d" (plist-get u :due-day))
                        (org-duration-from-minutes (plist-get u :remaining-min))
                        (org-foresight-report--landing-name u 40)
                        ;; Where the figure is partly a guess, the row that
                        ;; carries the figure says so.  The day's line counts
                        ;; them in total, which is enough to know the answer
                        ;; is soft but not enough to know which answer -- and
                        ;; this is the list somebody comes to in order to go
                        ;; and put the estimate in.
                        (let* ((n (plist-get u :unestimated))
                               (all (plist-get u :leaves))
                               ;; Two ways the same verdict can be soft, and
                               ;; they are not exclusive: nobody estimated
                               ;; this, and nobody can check the estimate.
                               (note
                                (concat
                                 (when (> n 0)
                                   (format " · %s unestimated"
                                           (if (= n all) "all"
                                             (format "%d of %d" n all))))
                                 (when (and (> sitting 0)
                                            (> (or (plist-get u :largest-min) 0)
                                               sitting))
                                   (format " · a leaf over %s"
                                           (org-duration-from-minutes sitting))))))
                          (if (string-empty-p note)
                              ""
                            (propertize
                             note 'face 'org-foresight-report-overcommitted))))
                (plist-get u :marker)))
             (plist-get e :units))
            (unless (eq (plist-get e :verdict) 'lands)
              (list (org-foresight-report--landing-rule e)))))
         entries))
       "\n"))))

(defun org-foresight-report--landing-rule (entry)
  "Return the rule drawn under ENTRY\='s commitments when they do not fit."
  (let* ((dur #'org-duration-from-minutes)
         (text
          (format
           ;; Both supplies, always, in the same shape.  Showing one of them
           ;; and naming it for the verdict made the reader learn which word
           ;; went with which arithmetic before the line said anything; with
           ;; the pair in front of them the verdict is visible instead --
           ;; owed above the first figure and under the second is work that
           ;; fits once something else gives way, owed above both is work
           ;; that does not fit at all.
           "%s owed · %s free of %s · %s"
           (funcall dur (plist-get entry :demand-min))
           ;; What nothing else has been promised.
           (funcall dur (plist-get entry :soft-min))
           ;; And the most the window could hold, once everything without a
           ;; date of its own has been moved out of the way.  Not the whole
           ;; of the working day: meetings, travel and work already put at
           ;; an hour stay where they are, because none of those move by
           ;; deciding to work on something else.
           (funcall dur (plist-get entry :hard-min))
           (pcase (plist-get entry :verdict)
             ('beyond "more than the fortnight holds")
             ;; Beaten by the window itself: no rearranging reaches it, so
             ;; the answer is overtime, another pair of hands, or less work.
             ('over (format "%s short" (funcall dur (plist-get entry :short-min))))
             ;; Beaten only by what is already promised elsewhere, which
             ;; says what to do rather than that there is nothing to do.
             (_ (format "%s must move"
                        (funcall dur (plist-get entry :soft-short-min)))))))
         (rule (make-string 2 ?─)))
    (concat " " (propertize rule 'face 'shadow) " "
            (propertize text 'face
                        (if (eq (plist-get entry :verdict) 'over)
                            'org-foresight-report-overcommitted
                          'shadow))
            " "
            (propertize
             (make-string
              ;; 5, not 6: the three spaces and the two opening dashes are
              ;; what precede TEXT, and counting a sixth left the rule a
              ;; column short of every other full-width line on the page.
              (max 2 (- org-foresight-report-columns 5 (string-width text)))
              ?─)
             'face 'shadow))))

(defun org-foresight-report--landing-out (worst over named)
  "Return the line under a failing deadline naming what can be done about it.

A figure with no lever is a figure that is read once and then resented.  The
verdict above says how much is missing; this says the four things that would
answer it, in the order somebody actually considers them:

  lands Wed        leave it and it finishes then -- the case for moving the
                   date, and the only one of the four that costs nothing
  drop any one of  the smallest single commitment that would close the gap,
                   which is what delegating or cutting scope means in hours
  move any one of  the same, for work that owes nobody a date -- the answer
                   to a shortfall only the soft figure sees, and only offered
                   there
  7:00 unclaimed before then   the hours outside work that nothing has
                   claimed, which is what staying late would have to come
                   out of.  Weighed against the shortfall on the line above,
                   which is why that figure is not repeated here

NAMED is the unit the verdict above already named.  Where the smallest thing
that would close the gap turns out to be that same unit, the title is not
repeated -- \"what is late is X, so drop X\" is a sentence that answers
nothing.  What it means is said instead: nothing smaller would have done, so
there is no partial way out of this one.

Each is a subtraction on figures already worked out; none of them proposes a
schedule or writes anything.  Ranked smallest-first for the same reason
`org-foresight-report--frees' is: the point is to give up the least that
still works.  Where nothing alone is enough, nothing is named -- the question
has stopped being which one and become how many, and that is a different
sentence than this line can hold."
  (let* ((candidates (if over (plist-get worst :drop) (plist-get worst :move)))
         (pick (car candidates))
         (itself (and pick named (eq pick named)))
         (unclaimed (or (plist-get worst :unclaimed-min) 0.0))
         (terms
          (delq nil
                (list
                 (when-let ((day (plist-get worst :lands-day)))
                   (concat "↳ lands "
                           (org-foresight-report--due-text day)))
                 (cond
                  ;; The smallest thing that would close the gap is the very
                  ;; commitment named above.  Saying its title again reads as
                  ;; a circle -- what is late is X, so drop X -- when what it
                  ;; means is that nothing short of the whole of it would do.
                  (itself (format " · only by %s it"
                                  (if over "dropping" "moving")))
                  (pick
                   ;; One is named and the rest are counted.  They are sorted
                   ;; smallest-first and every one of them is on its own
                   ;; enough, so the named one is the least that works -- but
                   ;; the least is not always the one that *can* go, and a
                   ;; reader who cannot see that there are others will not
                   ;; look for them.  `TAB' goes to the one named.
                   (format " · %s any one of %s %s%s"
                           (if over "drop" "move")
                           (org-foresight-report--landing-name pick 22)
                           (org-duration-from-minutes
                            (or (plist-get pick :remaining-min)
                                (plist-get pick :minutes) 0.0))
                           (let ((others (1- (length candidates))))
                             (if (> others 0) (format " +%d" others) "")))))
                 ;; The resource, not a comparison: what it is being weighed
                 ;; against is the shortfall on the line above, and repeating
                 ;; that here would spend the width saying it twice.
                 (when (and over (> unclaimed 0))
                   (format " · %s unclaimed before then"
                           (org-duration-from-minutes unclaimed)))))))
    (when terms
      ;; Nothing to lead with means nothing to say: a line that opened with a
      ;; qualifier would be a footnote to a sentence that was never written.
      (unless (string-prefix-p "↳" (car terms))
        (setq terms (cons "↳" terms)))
      (let ((line (propertize
                   (org-foresight-report--fit-terms
                    terms 1
                    (- org-foresight-report-columns
                       (string-width org-foresight-report-margin) 2))
                   'face 'shadow)))
        ;; Pointed at what it names, which is a different heading from the
        ;; one above: this row says what to give up, so `TAB' should arrive
        ;; at the thing to be given up.  The lead spaces carry the marker
        ;; too, because the agenda reads the first character of the line.
        (let* ((row (if (and pick (not itself) (plist-get pick :marker))
                        (org-foresight-report--actionable
                         line (plist-get pick :marker))
                      line))
               (lead (concat (org-foresight-report--margin-for row) "  ")))
          (concat (if (text-properties-at 0 row)
                      (apply #'propertize lead (text-properties-at 0 row))
                    lead)
                  row))))))

(defun org-foresight-report--landing-line (landing)
  "Return one line saying whether the dated work will be finished in time.

Nil when nothing is dated.  A block that reports \"0 deadlines\" every
morning is a block that stops being read.

The failing case names one heading and carries its marker, because the whole
purpose of the line is to send somebody to that heading -- `RET' should work
on it.

But the shortfall belongs to the *window*, not to that heading.  It is the
cumulative test: everything due on or before that date, against the hours
between now and it.  So the date is given as `by Mon' rather than `Mon', and
where more than one commitment falls inside the window the rest are counted
with `+N'.  Naming one and stating a collective figure beside it, with
nothing to say they are not the same claim, reads as \"this project is
eighteen minutes short\" -- which is a sentence about a project the
arithmetic never made.

Which of the three answers is being given is the first thing said, and the
qualifiers fall off the right as the line fills:

  a shortfall against the hard figure  overtime, delegation, or less of it
  a shortfall against the soft one     it fits if other promises move
  neither                              it fits as things stand

`beyond' is not among them.  A deadline past the end of the scan has no
answer here at all, only a count -- see `org-foresight-landing'."
  (when-let* ((landing landing)
              (entries (plist-get landing :deadlines)))
    (let* ((fail (plist-get landing :first-fail))
           (defer (and (not fail) (plist-get landing :first-defer)))
           (worst (or fail defer))
           (unit (car (plist-get worst :units)))
           (tight (plist-get landing :tightest))
           (head
            (cond
             (worst
              (concat
               (propertize (org-foresight-report--landing-name unit 22)
                           'face 'default)
               (let ((others (1- (or (plist-get worst :count) 1))))
                 (if (> others 0) (format " +%d" others) ""))
               " by " (org-foresight-report--due-text (plist-get worst :day))
               ;; "short of X" is English for "less than X", which is the
               ;; opposite of what the soft case means -- it read as though
               ;; the work fitted with time to spare.  The two are said in
               ;; parallel instead: one is missing hours outright, the other
               ;; needs more than is going spare.
               (propertize
                (if fail
                    (format " · %s short"
                            (org-duration-from-minutes
                             (plist-get worst :short-min)))
                  (format " · %s more than is free"
                          (org-duration-from-minutes
                           (plist-get worst :soft-short-min))))
                'face (if fail 'org-foresight-report-overcommitted 'shadow))))
             (t (format "%d deadline%s land" (plist-get landing :count)
                        (if (= 1 (plist-get landing :count)) "" "s")))))
           (terms
            (list (concat (propertize "↳ " 'face 'shadow) head)
                  (and (not worst) tight
                       (format " · %s spare at the tightest"
                               (org-duration-from-minutes
                                (max 0.0 (plist-get tight :spare-min)))))
                  (and (> (plist-get landing :overdue) 0)
                       (format " · %d overdue" (plist-get landing :overdue)))
                  (and (> (plist-get landing :unestimated) 0)
                       (format " · %d unestimated"
                               (plist-get landing :unestimated)))
                  (and (> (plist-get landing :beyond) 0)
                       (format " · %d beyond %s"
                               (plist-get landing :beyond)
                               (format-time-string
                                "%m-%d" (plist-get landing :horizon-day))))))
           (line (org-foresight-report--fit-terms
                  terms 1
                  (- org-foresight-report-columns
                     (string-width org-foresight-report-margin)))))
      (concat (let ((row (if (and worst (plist-get unit :marker))
                             (org-foresight-report--actionable
                              line (plist-get unit :marker))
                           line)))
                (concat (org-foresight-report--margin-for row) row))
              (when worst
                (when-let ((out (org-foresight-report--landing-out
                                 worst (and fail t) unit)))
                  (concat "\n" out)))))))

(defun org-foresight-report-capacity-line (&optional day scan now behind)
  "Return DAY's capacity verdict as one line, or nil on a non-working day.
This is the line that goes at the very top of the agenda: a number you should
not have to go looking for is a number you will not look at.

BEHIND, when given, draws the hours DAY has already spent under the ones it
has left.  Passed in rather than gathered here because gathering it reads
the clock history, and this is called once per day drawn."
  (let* ((day (or day (org-foresight--day-start 0)))
         (scan (or scan (org-foresight-scan 1 day)))
         (cap (org-foresight-capacity day scan now))
         (idx (org-foresight--day-of day (plist-get scan :from)))
         (ledger (and (>= idx 0) (< idx (plist-get scan :days))
                      (aref (plist-get scan :ledger) idx))))
    (cond
     ;; A day with no working hours has no capacity to divide, but it can
     ;; still have work dated to it -- and that is exactly the day worth
     ;; saying so on.  One line, because there is no span to draw and nothing
     ;; to offer: what is true is that the work exists and the day was not
     ;; meant for it.  A genuinely free day says nothing at all.
     ((null (plist-get cap :work))
      (when (> (plist-get cap :committed-min) 0)
        (org-foresight-report--indent
         (concat (org-foresight-report--rest-day cap)
                 (org-foresight-report--verdict-extras scan)))))
     (t
      (org-foresight-report--indent
       (concat (org-foresight-report--verdict cap)
               ;; Directly under the overflow, because the number and the way
               ;; out of it are one thought and reading them apart is what
               ;; makes an overcommitted day feel like weather.
               (when-let ((frees (org-foresight-report--frees
                                  (- (min 0.0 (plist-get cap :headroom-min)))
                                  ledger)))
                 (concat "\n" frees))
               (when-let ((bars (org-foresight-report--bars cap behind)))
                 (concat "\n" bars))
               (org-foresight-report--verdict-extras scan)))))))

(defun org-foresight-report--rest-day (cap)
  "Return the one line a day with no working hours has to say, from CAP.

What is owed, and when doing it would finish.  Not what may still be promised
-- nothing may, and the answer to a day off is not a smaller number but the
observation that work is dated to it at all."
  (concat
   (propertize "Not a working day" 'face 'org-foresight-report-overcommitted)
   (format " · %s promised"
           (org-duration-from-minutes (plist-get cap :committed-min)))
   (if-let ((lands (plist-get cap :lands)))
       (format " · ends %s" (format-time-string "%H:%M" lands))
     (propertize " · not today" 'face 'org-foresight-report-overcommitted))))

(defconst org-foresight-report--carried
  '(org-marker org-hd-marker org-agenda-type)
  "Text properties the margin has to carry for a row to answer to the agenda.

`org-agenda-goto\=', `org-agenda-schedule\=' and the rest all read through
`org-get-at-bol\=', which looks at the *first character of the line*.  A row
whose margin is a bare space therefore answers nothing, however carefully the
rest of it was marked -- the commands find no marker and report an error on a
line that is plainly about a heading.")

(defun org-foresight-report--margin-for (line)
  "Return the margin, carrying whatever LINE answers to the agenda with.

The margin is put on last and is a space, so it is easy to forget that it is
also the character every agenda command looks at.  Copying the row's own
markers onto it is what makes `RET\=' and `TAB\=' work where the row says
they should."
  (let ((props (text-properties-at 0 line))
        (out org-foresight-report-margin)
        carried)
    (dolist (key org-foresight-report--carried)
      (when-let ((v (plist-get props key)))
        (setq carried (append carried (list key v)))))
    (when carried
      (setq out (apply #'propertize out carried)))
    out))

(defun org-foresight-report--indent (text)
  "Return TEXT with every line moved off the frame edge by the margin.
Applied to the blocks whose lines do not already carry it, rather than to
the rows, which are built at the margin because that is where a line the
agenda can act on belongs."
  (when text
    (mapconcat (lambda (line)
                 ;; A blank line stays blank: padding it would leave trailing
                 ;; whitespace on a line that says nothing.
                 (if (string-empty-p line) line
                   (concat (org-foresight-report--margin-for line) line)))
               (split-string text "\n") "\n")))

(defun org-foresight-report--actionable (string marker &optional stamp)
  "Return STRING carrying the text properties Org's agenda commands look for.

Rather than inventing a keymap, a row says what it is in the vocabulary the
agenda already speaks: `org-marker' is what `org-agenda-schedule' reads,
`org-hd-marker' what `org-agenda-set-effort' reads, and `org-agenda-type'
what `org-agenda-check-type' gates every one of them on.  With all three in
place \\`s', \\`e', \\`t', \\`RET' and the rest work on a foresight row exactly
as they do on an agenda line -- which is the difference between a board that
reports a problem and one you can fix it from.

STAMP, where the row's timestamp lives, becomes `org-marker' instead of the
heading.  That is the agenda's own convention and not an incidental one:
\\`S-right' and \\`>' go to `org-marker' and demand `org-at-timestamp-p'
there, so pointing it at the heading is the difference between being able to
shift a meeting half an hour and being told there is no time stamp.  Every
other command reaches its heading from inside the entry regardless.

A nil MARKER leaves the row inert.  That matters: a finding that is a total
rather than an entry has nothing to act on, and inheriting the neighbouring
row's marker would quietly reschedule the wrong thing."
  (if (not (markerp marker))
      string
    (propertize string
                'org-marker (if (markerp stamp) stamp marker)
                'org-hd-marker marker
                'org-agenda-type 'agenda
                'help-echo (concat "s day · S-left/right day · "
                                   "C-u C-u S-left/right minutes · "
                                   "> prompt · e effort · t state · RET visit"))))

;;;; Naming work in a line
;; The day itself is drawn by Org's agenda, and org-foresight-agenda.el hands
;; it what it does not know.  What is left here is the vocabulary both blocks
;; share: how a piece of work is named, how long it is held to be, and how a
;; run of them is fitted into the width of a line.

(defun org-foresight-report--grid-todo (marker)
  "Return the TODO keyword of MARKER's entry, with a trailing space, or \"\".
Worth carrying on a row the agenda did not build: NEXT and WAIT are different
answers to \"can this move\"."
  (or (and (markerp marker)
           (marker-buffer marker)
           (with-current-buffer (marker-buffer marker)
             (org-with-wide-buffer
              (goto-char marker)
              (when-let ((kw (org-get-todo-state)))
                (concat (propertize kw 'face (org-get-todo-face kw)) " ")))))
      ""))

(defcustom org-foresight-grid-suggest 3
  "How many candidates a free stretch names, or nil to name none."
  :type '(choice (const :tag "none" nil) integer)
  :group 'org-foresight)

(defcustom org-foresight-grid-frees 3
  "How many ways out of an overcommitted day are named, or nil to name none."
  :type '(choice (const :tag "none" nil) integer)
  :group 'org-foresight)

(defun org-foresight-report--entry-minutes (e)
  "Return how many minutes ledger entry E still costs the day.

What is left of it, which is what capacity spends: the corrected estimate
less the hours already in it.  A row that answers with the whole estimate
after half the work is done is answering a question about this morning."
  (or (plist-get e :remaining) (plist-get e :effort-adj) (plist-get e :effort) 0))

(defun org-foresight-report--effort-run (e)
  "Return what ledger entry E costs, saying so when that is not what it said.

Written \"2:00→2:48\" where the two differ enough to matter, and as the plain
figure where they do not.  The estimate stays on the left because that is the
number in the file; the right is what the day is actually being planned
around.  Two things move it: the correction history applies to the estimate,
and the hours already spent on the task.  The row does not separate them --
it only promises that the left is what was written and the right is what is
being used."
  (let ((drift (org-foresight-report--effort-drift e)))
    (if drift
        (concat (org-duration-from-minutes (plist-get e :effort)) drift)
      (org-duration-from-minutes (org-foresight-report--entry-minutes e)))))

(defun org-foresight-report--effort-drift (e)
  "Return \"→2:48\" where E\='s correction is doing real work, else nil.

Split from `org-foresight-report--effort-run\=' so that a row which already
carries the estimate -- one Org printed in its own effort column -- can be
told what the estimate is being treated as without the figure being written
out twice."
  (let ((raw (plist-get e :effort))
        (adj (org-foresight-report--entry-minutes e)))
    (when (and raw (> raw 0)
               (>= (abs (- adj raw)) org-foresight-bias-visible-minutes)
               (> (abs (- (/ adj (float raw)) 1.0)) 0.1))
      (concat "→" (org-duration-from-minutes adj)))))

(defun org-foresight-report--budget (lead)
  "Return the columns a run may use on a line already carrying LEAD.

The margin is taken off as well: every block is indented by
`org-foresight-report-margin\' after it is built, so a line measured against
the full width would come out one column over."
  (- 80 (string-width lead) (string-width org-foresight-report-margin)))

(defun org-foresight-report--name-run (entries limit budget)
  "Return at most LIMIT of ENTRIES named within BUDGET columns, or nil.

Each is its title and what it costs.  What did not fit is counted rather than
dropped, because the difference between three candidates and thirty is the
whole answer on some days."
  (let ((pieces (mapcar (lambda (e)
                          (format "%s %s"
                                  (truncate-string-to-width
                                   (plist-get e :title) 22 nil nil "…")
                                  (org-foresight-report--effort-run e)))
                        entries))
        (total (length entries))
        shown)
    (setq shown (seq-take pieces (max 0 limit)))
    ;; Trim from the end until the whole line fits -- including the "+N", which
    ;; is part of the line and grows as pieces come off it.  Counting the
    ;; pieces alone and appending the count afterwards is how a run ends up one
    ;; column over the budget it was measured against.
    (while (and shown
                (> (string-width
                    (org-foresight-report--name-run-join shown total))
                   budget))
      (setq shown (butlast shown)))
    (and shown (org-foresight-report--name-run-join shown total))))

(defun org-foresight-report--name-run-join (shown total)
  "Join SHOWN, counting how many of TOTAL were left out."
  (concat (string-join shown " · ")
          (let ((more (- total (length shown))))
            (if (> more 0) (format " +%d" more) ""))))

(defun org-foresight-report--frees (over ledger)
  "Return the line naming what LEDGER can give up to win back OVER minutes.

The mirror of `org-foresight-report--grid-fits': that one says what goes into
a gap, this one what has to come out of a day with none.  Between them they
are the whole of rearranging a day, and neither question is answerable by
looking at a bar -- a bar says how far over, never by what.

Ranked by the smallest that is on its own enough, because the point is to
give up the least that still works.  That is the opposite order to the gap's,
and for the same reason.  Where nothing alone is enough the largest are taken
in turn until they are, since the question has stopped being which one and
become how many.  Where even everything movable falls short, that is said
outright: a day whose overflow is made of other people's meetings cannot be
rescheduled out of trouble, and knowing so early is the difference between
declining something and working late.

Meetings and dated tasks are candidates alongside promised work: a day is
rarely saved by moving only the things nobody else knows about.  Travel is
not, because it is not moved -- it follows whatever it was getting you to.
Neither is anything private or merely informational: a working day that has
been overfilled is not answered by moving the family, and a fixture nobody
asked you to attend was never yours to move in the first place."
  (when (and org-foresight-grid-frees (> over 0))
    (let* ((movable (seq-filter
                     (lambda (e)
                       (and (memq (plist-get e :kind) '(meeting task promised))
                            (not (eq (plist-get e :attention) 'informational))
                            (not (member (plist-get e :category)
                                         org-foresight-private-categories))
                            (> (org-foresight-report--entry-minutes e) 0)))
                     ledger))
           (enough (seq-sort-by #'org-foresight-report--entry-minutes #'<
                                (seq-filter
                                 (lambda (e)
                                   (>= (org-foresight-report--entry-minutes e)
                                       over))
                                 movable)))
           (biggest (seq-sort-by #'org-foresight-report--entry-minutes
                                 #'> movable))
           (combined (unless enough
                       (let ((left over) out)
                         (dolist (e biggest)
                           (when (> left 0)
                             (setq left (- left
                                           (org-foresight-report--entry-minutes e)))
                             (push e out)))
                         (and (<= left 0) (nreverse out)))))
           (picked (or enough combined biggest))
           (lead (cond
                  (enough "↳ any one of ")
                  (combined "↳ needs all of ")
                  (movable
                   (format "↳ only %s of it can move: "
                           (org-duration-from-minutes
                            (apply #'+ (mapcar
                                        #'org-foresight-report--entry-minutes
                                        movable)))))))
           (named (and lead
                       (org-foresight-report--name-run
                        picked
                        (if combined (length picked) org-foresight-grid-frees)
                        (org-foresight-report--budget lead)))))
      (when named
        (propertize (concat lead named) 'face 'shadow)))))

;;;; Agenda integration

(defvar org-foresight-report-renderers
  '((daily  :body org-foresight-report--daily  :place bottom)
    (review :body org-foresight-report--review :place bottom))
  "Alist of (STYLE :body FUNCTION :place top-or-bottom).

FUNCTION is called with one argument: a survey of the horizon, or nil.  It is
free to ignore it, and must work when it is nil -- but a renderer that wants
one should take the offered one rather than ask for its own, because a survey
is a walk of every entry in every agenda file and the caller has already paid
for this one.

A registry rather than a `pcase' so that a file loaded later can add a style
without this one having to know about it -- org-foresight-plan.el registers
its own board here, and it depends on this file, not the other way round.

`:place' decides which side of the agenda listing the report lands on, and
that is a statement about what the view is for.  `bottom' is for a report
consulted while working through the day; `top' is for one worked through in
its own right, where reaching the entries means scrolling past what you came
to act on.")

(defun org-foresight-report--renderer (&optional style)
  "Return the plist registered for STYLE, or nil."
  (cdr (assq (or style org-foresight-report-style)
             org-foresight-report-renderers)))

(defun org-foresight-report--place (&optional style)
  "Return where STYLE's report belongs, `top' or `bottom'."
  (or (plist-get (org-foresight-report--renderer style) :place) 'bottom))

(defun org-foresight-report--body (&optional scan clock landing)
  "Return the report text for the current `org-foresight-report-style'.

Dispatches through `org-foresight-report-renderers'; an unknown style simply
renders nothing.  SCAN is a survey of the horizon, CLOCK a survey of the
clock history and LANDING the deadline verdict, all offered so a renderer
reads them rather than taking its own -- see `org-foresight-report-render'.
A renderer must work when any of them is nil."
  (when-let ((fn (plist-get (org-foresight-report--renderer) :body)))
    (funcall fn scan clock landing)))

(defun org-foresight-report--daily (&optional scan clock landing)
  "Return the day looked back on: where the hours went, against where they were
meant to go.

What is still possible is answered above the agenda by the verdict and its
bars, and the shape of the day by the agenda itself.  What is left for the
foot of the buffer is the half no plan can supply -- the clock against the
estimate, and the machine against the clock.

CLOCK is the survey of the clock history, taken here only when the caller
had none to offer.  The elapsed bar above the agenda reads the same seven
days, and two walks of the same logbooks are two chances to disagree about
one afternoon."
  (let ((clock (or clock (org-foresight-clock-scan 7))))
    (concat "\n"
            ;; Forward first.  The verdict above has just said "OVER by 3:45",
            ;; and the next question is where the rest goes -- which should not
            ;; be on the far side of a block of sparklines about what already
            ;; happened.  What is done with is read last, and least often.
            (org-foresight-report--badge "Load" "when I could take this on")
            "\n"
            (org-foresight-report-load nil scan nil landing)
            "\n\n"
            (org-foresight-report--badge "Spent" "where the hours actually went")
            "\n"
            (org-foresight-report--indent (org-foresight-report-spent clock))
            "\n")))

(defun org-foresight-report--review (&optional _scan clock _landing)
  "Return the weekly review: where the last seven days went, and how far the
estimates that shaped them were out.

Two blocks rather than one, because they answer different questions over
different periods: the week says where the hours landed, the curve says what
to expect of the next estimate written.  The second is only here and not on
the daily view -- changing how you estimate is done on reflection, not
between two meetings."
  (concat "\n"
          (org-foresight-report--badge "Clocked" "by area · last 7 days")
          "\n"
          (org-foresight-report-week (or clock (org-foresight-clock-scan 7)))
          "\n"
          (when-let ((estimates (org-foresight-report-estimates)))
            (concat "\n"
                    (org-foresight-report--badge
                     "Estimates" "how far each size of estimate runs over")
                    "\n"
                    (org-foresight-report--indent estimates)
                    "\n"))))

(defun org-foresight-report--guarded (thunk &rest args)
  "Call THUNK on ARGS, returning its string or a visible complaint on failure.
A failure must never take the agenda down with it, but it must not vanish
either: silently swallowing the error leaves a missing block and no way to
find out why, which is far worse to live with than one ugly line."
  (condition-case err (apply thunk args)
    (error
     (message "org-foresight: %s" (error-message-string err))
     (propertize (format "(org-foresight failed: %s)"
                         (error-message-string err))
                 'face 'error))))

(defun org-foresight-report--clear ()
  "Remove anything a previous render left behind."
  (let ((pos (point-min)))
    (while (setq pos (text-property-any pos (point-max)
                                        'org-foresight-report t))
      (delete-region
       pos (or (next-single-property-change pos 'org-foresight-report)
               (point-max))))))

(defconst org-foresight-report--agenda-properties
  '(org-redo-cmd org-last-args org-series-cmd org-series-redo-cmd org-agenda-type)
  "The properties Org reads from the character under point to redraw a view.

`org-agenda-list' stamps them over the whole buffer just before the finalize
hook runs, so anything inserted from that hook is born without them.")

(defun org-foresight-report--inherit-agenda-properties (beg end)
  "Give the text in \[BEG, END) the redo properties Org gave the rest.

`org-agenda-redo' takes the command to re-run from the text under point --
which means `r', `g', and every view toggle that redraws by way of it.  A
block inserted here carries none of that, so a cursor left sitting in one made
the agenda stop responding: not with an error, and not always, but exactly
whenever the cursor happened to be resting on our text rather than Org's.

The donor is the character just past the block, falling back to the first
place in the buffer that has the property at all -- which is what the block at
the very foot of the page needs."
  (let ((donor (or (and (< end (point-max))
                        (get-text-property end 'org-redo-cmd)
                        end)
                   (text-property-not-all (point-min) (point-max)
                                          'org-redo-cmd nil))))
    (when donor
      (dolist (prop org-foresight-report--agenda-properties)
        (when-let ((value (get-text-property donor prop)))
          (put-text-property beg end prop value))))))

(defun org-foresight-report--insert (text)
  "Insert TEXT at point and mark it as ours, so a later render can reclaim it."
  (when (and text (not (string-empty-p text)))
    (let ((beg (point)))
      (insert text)
      (put-text-property beg (point) 'org-foresight-report t)
      (org-foresight-report--inherit-agenda-properties beg (point)))))

(defun org-foresight-report-render ()
  "Render the current style's report into an agenda view.

The capacity verdict always goes at the very top, where it is read before
anything is decided.  The body follows it or comes after the agenda listing
depending on the style's `:place' (see `org-foresight-report-renderers').
Everything is marked with one text property, so a later render reclaims it
whatever side it went on.

Point is left exactly where it was found.  `org-agenda-list' positions point
before calling `org-agenda-finalize', so this hook runs last and would
otherwise strand the cursor -- and the window -- at the end of the buffer.

`org-agenda-change-all-lines' (used by `org-agenda-todo') calls
`org-agenda-finalize' narrowed to the single changed line, and
`org-agenda-finalize' does not widen before running the hook; the insertions
below would then land inline after that line.  Skip unless the whole buffer
is accessible; `org-foresight-report-refresh' brings the totals back into
step afterwards."
  (when (and (derived-mode-p 'org-agenda-mode)
             org-foresight-report-style
             (not (buffer-narrowed-p)))
    (save-excursion
      (let* ((inhibit-read-only t)
             (top-p (eq (org-foresight-report--place) 'top))
             ;; One survey for both halves of the report.  The verdict asks
             ;; about today and the forward view about the fortnight, and a
             ;; survey is a walk of every entry in every agenda file -- so the
             ;; wider of the two questions is asked once and the narrower one
             ;; reads its answer out of it.  A survey of a fortnight costs what
             ;; a survey of one day costs: the walk is the price, and the days
             ;; are only how many buckets it sorts the answers into.
             (scan (org-foresight-report--guarded
                    #'org-foresight-redraw-scan))
             (scan (and (not (stringp scan)) scan))
             ;; The clock history, surveyed once for both halves as well.
             ;; The elapsed bar above the agenda and the `Spent' block below
             ;; it read the same seven days, and a second walk of the same
             ;; logbooks could only give them something to disagree about.
             (clock (org-foresight-report--guarded
                     (lambda () (org-foresight-clock-scan 7))))
             (clock (and (not (stringp clock)) clock))
             ;; What the elapsed half of the day was actually spent on.
             ;; Worked out here rather than in the verdict because the
             ;; watcher is asked as part of it, and the verdict is one line
             ;; that several blocks are drawn from.
             (behind (org-foresight-report--guarded
                      (lambda ()
                        (org-foresight-behind
                         (org-foresight--day-start 0) clock
                         (org-foresight-observe-coverage clock)))))
             (behind (and (not (stringp behind)) behind))
             ;; The outline axis: which headings are projects, what each
             ;; deadline still needs, and whether the hours before it exist.
             ;; A third walk of the files, and the one thing that cannot ride
             ;; on either of the others -- the day scan keeps nothing about
             ;; work dated outside its horizon, and undated work under a
             ;; deadline is precisely what this counts.
             (projects (org-foresight-report--guarded
                        (lambda () (org-foresight-project-scan))))
             (projects (and (not (stringp projects)) projects))
             (landing (org-foresight-report--guarded
                       (lambda () (org-foresight-landing projects scan))))
             (landing (and (not (stringp landing)) landing))
             (body (org-foresight-report--guarded
                    #'org-foresight-report--body scan clock landing))
             (line (org-foresight-report--guarded
                    #'org-foresight-report-capacity-line nil scan nil behind)))
        (org-foresight-report--clear)
        (goto-char (point-min))
        (when line
          (org-foresight-report--insert
           (concat (org-foresight-report--badge
                    "Capacity" "what today can still take")
                   "\n" line "\n\n")))
        ;; Insertion leaves point after the text, so a `top' body follows the
        ;; verdict and still precedes the agenda listing.
        (unless top-p
          (goto-char (point-max)))
        (org-foresight-report--insert body)))))

(add-hook 'org-foresight-report-invalidate-functions
          #'org-foresight-invalidate-scan)

(defvar org-foresight-report-invalidate-functions nil
  "Functions called before a refresh to drop anything memoized.
A registry so that a file loaded later can clear its own caches -- the signals
in org-foresight-plan.el are cached for a few seconds, which is long enough
to answer a refresh with the state of the world before the edit.")

(defun org-foresight-report-refresh (&rest _)
  "Redraw this buffer's report so the totals match what was just changed.

Editing from an agenda goes through `org-agenda-change-all-lines', which
finalizes with the buffer narrowed to the one line it rewrote.  The render
hook declines to run narrowed -- rightly, or the blocks would land inline --
so without this the row would update while the verdict above it went on
stating the old numbers, which is worse than not showing them at all.

Only redraws what this package owns, and keeps point, so the loop of reading
a signal and acting on it is not interrupted by the display jumping."
  (when (and (derived-mode-p 'org-agenda-mode)
             org-foresight-report-style)
    (run-hook-with-args 'org-foresight-report-invalidate-functions)
    (save-restriction
      (widen)
      (org-foresight-report-render))))

;; Installed by loading, like the rest of what this package does to the agenda:
;; the rows are added by an advice on Org's own grid function and the spine is
;; drawn from this same hook, both from `org-foresight-agenda', and a report
;; that alone had to be wired up by hand was the odd one out.  Nothing here is
;; conditional on it having been asked for -- `org-foresight-report-render'
;; declines unless it is in an agenda buffer and a style is set, and
;; `org-foresight-report-style' set to nil is how somebody turns it off.
;;
;; Appended, so it runs after the spine: `org-foresight-agenda' is loaded after
;; this file and adds its own hook at the front.
(add-hook 'org-agenda-finalize-hook #'org-foresight-report-render t)

;; Last, after everything that draws.  The survey the redraw shared is only
;; true of the settings it was taken under, so it must not outlive the redraw
;; and be handed to the next one -- which may be answering a different
;; question about the same files.
(add-hook 'org-agenda-finalize-hook #'org-foresight-invalidate-scan t)

;; And first, before anything reads.  `org-agenda-prepare' runs at the head of
;; every build, whether the buffer is new or being redrawn in place -- which
;; `org-agenda-mode-hook' does not, since the mode is only entered once and a
;; redraw of an existing agenda would have gone on reading the old survey.
(advice-add 'org-agenda-prepare :before #'org-foresight-invalidate-scan)

;; Everything that can change what a day costs.  The time commands are on the
;; list for the same reason as the rest: they edit the entry and leave the
;; agenda buffer alone, so without this the row moves while the figure above
;; it still describes the day before the move.
(defconst org-foresight-write-commands
  '(org-agenda-schedule
    org-agenda-deadline
    org-agenda-todo
    org-agenda-set-effort
    org-agenda-set-tags
    org-agenda-priority
    org-agenda-date-later
    org-agenda-date-earlier
    org-agenda-date-prompt)
  "Agenda commands after which a foresight block is out of date.")

(dolist (cmd org-foresight-write-commands)
  (advice-add cmd :after #'org-foresight-report-refresh))

(defun org-foresight--diagnose-timed (scan)
  "Return the entries of SCAN's first day that sit at a time of their own.

Derived blocks are left out: what this answers is whether the files say what
you think they say, and a journey or a check is this package talking to
itself."
  (seq-filter (lambda (e)
                (and (plist-get e :start)
                     (not (memq (plist-get e :kind) '(travel check)))))
              (and scan (org-foresight-scan-day
                         scan :ledger (org-foresight--day-start 0)))))

(defun org-foresight--diagnose-state (day)
  "Return DAY's configuration as an alist of (LABEL . TEXT) lines."
  (let* ((shape (org-foresight-day-shape day))
         (awake (plist-get shape :awake))
         (work (plist-get shape :work))
         (scan (ignore-errors (org-foresight-scan 1 day)))
         ;; Journeys are derived from the entries; counting them here would
         ;; be measuring the answer rather than the question.
         (timed (org-foresight--diagnose-timed scan))
         (placed (seq-filter (lambda (e) (plist-get e :place)) timed))
         (bias (org-foresight--bias-data)))
    (list
     (cons "wiring"
           (format "hook %s · style %s"
                   (if (memq 'org-foresight-report-render
                             org-agenda-finalize-hook)
                       "installed" "MISSING")
                   (or org-foresight-report-style "nil (nothing renders)")))
     (cons "agenda files" (format "%d" (length (org-agenda-files))))
     (cons "today"
           (format "%s · awake %s–%s · work %s"
                   (format-time-string "%a" day)
                   (format-time-string "%H:%M" (car awake))
                   (format-time-string "%H:%M" (cdr awake))
                   (if work
                       (mapconcat
                        (lambda (iv)
                          (concat (format-time-string "%H:%M" (car iv)) "–"
                                  (format-time-string "%H:%M" (cdr iv))))
                        work " ")
                     "none")))
     (cons "surge"
           (if-let ((n (org-foresight-surge-samples)))
               (format "%s/day of arriving work (learned from %d day(s))"
                       (org-duration-from-minutes (org-foresight-surge-minutes))
                       n)
             (format "%s (default, never learned)"
                     (org-duration-from-minutes (org-foresight-surge-minutes)))))
     (cons "leak"
           (if-let ((n (org-foresight-leak-samples)))
               (format "%s unrecorded, %s away, per day (from %d day(s))"
                       (org-duration-from-minutes (org-foresight-leak-minutes))
                       (org-duration-from-minutes (org-foresight-lost-minutes))
                       n)
             "not measured (no ActivityWatch history)"))
     (cons "estimates"
           (if bias
               (concat (org-foresight-bias-summary bias)
                       (if org-foresight-bias-enabled "" " (APPLYING DISABLED)"))
             "not learned (estimates taken at face value)"))
     (cons "places"
           (format "%d configured · %d travel pair(s) · %d of %d timed entries today"
                   (length org-foresight-places)
                   (length org-foresight-travel-matrix)
                   (length placed) (length timed))))))

(defvar org-foresight-diagnose-extras nil
  "Functions contributing extra lines to what `org-foresight-diagnose' advises.
Each is called with no arguments and returns a line, or nil to add nothing.
A registry rather than a fixed call, so a file loaded later can report a
precondition of its own -- the layout the agenda's brackets need, say --
without this one having to know what that file draws.")

(defun org-foresight--diagnose-advice (day)
  "Return what to do next, given DAY's configuration.
Looks past whether options are set to whether they are doing anything: a
setting that is present but never matches is invisible, and silence about it
is what lets a whole feature quietly not run."
  (let* ((scan (ignore-errors (org-foresight-scan 1 day)))
         (timed (org-foresight--diagnose-timed scan))
         (unplaced (seq-filter (lambda (e) (null (plist-get e :place))) timed))
         out)
    (when (null org-agenda-files)
      (push "`org-agenda-files' is empty; nothing can be seen" out))
    (when (null org-foresight-places)
      (push "`org-foresight-places' is unset; no journey is ever counted" out))
    (when (and org-foresight-places (null org-foresight-travel-matrix))
      (push (format "no travel times set; every journey assumes %d min"
                    org-foresight-travel-default)
            out))
    (when (and org-foresight-places unplaced)
      (push (format "%d of today's %d timed entries name no place -- travel is not counted for them"
                    (length unplaced) (length timed))
            out))
    (unless (org-foresight-surge-samples)
      (push (format
             "run `org-foresight-learn-surge' to measure arriving work; it reads the `%s' property, which something has to write"
             org-foresight-surge-property)
            out))
    (unless (org-foresight-leak-samples)
      (push "run `org-foresight-learn-leak' to measure what a day loses" out))
    ;; Learned, and larger than the thing it is held back from.  Nothing is
    ;; wrong with the arithmetic -- the reserve is exactly what the hours said
    ;; -- but hours the clock never accounted for cannot tell work from
    ;; absence, so what it measured was the record, not the day.
    (let* ((span (/ (org-foresight--intervals-seconds
                     (org-foresight-work-intervals day))
                    60.0))
           (budget (+ (org-foresight-surge-minutes)
                      (org-foresight-leak-minutes)
                      (org-foresight-lost-minutes))))
      (when (and (> span 0) (>= budget span))
        (push (format
               "the reserve (%s) is larger than the working day (%s); fill the unrecorded hours with `org-foresight-clock-fill' and learn again, or delete `%s' to go back to the defaults"
               (org-duration-from-minutes budget)
               (org-duration-from-minutes span)
               org-foresight-leak-cache-file)
              out)))
    (let ((bias (org-foresight--bias-data)))
      (cond
       ((null bias)
        (push "run `org-foresight-learn-bias' to see how far estimates run over"
              out))
       ;; Learnt, but by a version that recorded only the answer.  The
       ;; correction still works; what is missing is any way to check it.
       ((null (plist-get bias :by-effort))
        (push (concat "run `org-foresight-learn-bias' again; the cached "
                      "figures predate the estimate curve")
              out))))
    (unless (bound-and-true-p org-foresight-task-file)
      (push "`org-foresight-task-file' is unset; generated tasks have nowhere to go"
            out))
    (unless (bound-and-true-p org-foresight-meeting-categories)
      (push "`org-foresight-meeting-categories' is unset; no meeting implies prep"
            out))
    (dolist (fn org-foresight-diagnose-extras)
      (when-let ((line (org-foresight-report--guarded fn)))
        (push line out)))
    (nreverse out)))

;;;###autoload
(defun org-foresight-diagnose ()
  "Report what org-foresight is configured to do, and what it is not doing.

Answers the question a missing or surprising figure raises.  Reports both
what is set and whether it is having any effect -- an option that is present
but never matches looks exactly like a working one from the outside."
  (interactive)
  (let* ((day (org-foresight--day-start 0))
         (state (org-foresight--diagnose-state day))
         (advice (org-foresight--diagnose-advice day))
         (buf (get-buffer-create "*Org Foresight*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (dolist (row state)
          (insert (format "%-14s %s\n" (car row) (cdr row))))
        (if (null advice)
            (insert "\nNothing to set up.\n")
          (insert "\nNext:\n")
          (dolist (line advice)
            (insert "  · " line "\n"))))
      (goto-char (point-min))
      (special-mode))
    (display-buffer buf)))

(provide 'org-foresight-report)

;;; org-foresight-report.el ends here
