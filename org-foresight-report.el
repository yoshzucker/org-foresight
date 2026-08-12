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

;;;; Style and glyphs

(defconst org-foresight-block ?█
  "The block every bar and gutter is filled with: U+2588, the full block.

Named once so the drawing has a single shape to change.  Note that it is
taller than the line it sits on -- 1300 units against a 1175-unit line in
PlemolJP, since the glyph is designed to tile a screen without seams -- so
a column of these reads as one mass rather than as separate bars.")

(defconst org-foresight-report--partials [?▏ ?▎ ?▍ ?▌ ?▋ ?▊ ?▉]
  "Blocks filling 1/8 to 7/8 of a cell, for a run's last, partial cell.
These share `org-foresight-block' box exactly, and nothing follows them in a
run to be pushed out of line.")

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
  "Return the glyph for FRAC (0.0-1.0), or ?· when FRAC is zero or less.
Glyphs are taken from `org-foresight-report--spark-chars'."
  (if (<= frac 0.0) ?·
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

(defun org-foresight-report--dominant-face (ca cf ua uf i)
  "Face for half-hour bin I: whichever of CA/CF/UA/UF (clocked×active/afk
48-vectors from `org-foresight-observe-coverage') has the most seconds there."
  (let ((best-v (aref ca i)) (best-f 'org-foresight-report-clocked-active))
    (when (> (aref cf i) best-v) (setq best-v (aref cf i) best-f 'org-foresight-report-clocked-afk))
    (when (> (aref ua i) best-v) (setq best-v (aref ua i) best-f 'org-foresight-report-unclocked-active))
    (when (> (aref uf i) best-v) (setq best-f 'org-foresight-report-unclocked-afk))
    best-f))

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

(defun org-foresight-report-observed (clock)
  "Return today's ActivityWatch \"reality & rhythm\" block (lines <=80 cols).
Header (boundaries/active/afk/switches/clocked/leak) + hourly sparkline
(colored by clocked-status when CLOCK's coverage data is available) + a table
partitioning the observed day into active apps (with a category tag and, when
available, the unclocked/leak portion of that app's time) plus an away(afk)
row, then an emacs-project detail line.  CLOCK is the plist from
`org-foresight-clock-scan', passed through to `org-foresight-observe-coverage'."
  (let ((data (org-foresight-observe-today)))
    (if (not data)
        (propertize "(ActivityWatch unavailable)" 'face 'org-table)
      (let* ((active (plist-get data :active))
             (afk (plist-get data :afk))
             (full (max 1.0 (+ active afk)))
             (cov (org-foresight-observe-coverage clock))
             (leak-by-app (and cov
                                (let ((h (make-hash-table :test 'equal)))
                                  (dolist (kv (plist-get cov :leak-apps))
                                    (puthash (car kv) (cdr kv) h))
                                  h)))
             (apps (plist-get data :active-apps))
             (top (seq-take apps 8))
             (rest-sum (apply #'+ (mapcar #'cdr (seq-drop apps 8))))
             (rows (append
                    (mapcar (lambda (kv)
                              (list (org-foresight--app-category (car kv)) (car kv) (cdr kv)
                                    (and leak-by-app (gethash (car kv) leak-by-app))))
                            top)
                    (when (> rest-sum 60)
                      (list (list "other" "other apps" rest-sum nil)))
                    (list (list "idle" "away (afk)" (float afk) nil))))
             (rows (seq-filter (lambda (r) (> (nth 2 r) 0)) rows))
             (rows (sort rows (lambda (a b) (> (nth 2 a) (nth 2 b)))))
             (maxrow (if rows (apply #'max (mapcar (lambda (r) (nth 2 r)) rows)) 1))
             (first (plist-get data :first))
             (last (plist-get data :last)))
        (propertize
         (concat
          (format "Screen %s–%s · active %s · afk %s · %d switches%s"
                  (if first (format-time-string "%H:%M" first) "—")
                  (if last (format-time-string "%H:%M" last) "—")
                  (org-duration-from-minutes (/ active 60.0))
                  (org-duration-from-minutes (/ afk 60.0))
                  (plist-get data :switches)
                  (if cov
                      (format " · clocked %s · leak %s"
                              (org-duration-from-minutes (/ (plist-get cov :clocked-sec) 60.0))
                              (org-duration-from-minutes (/ (plist-get cov :leak-sec) 60.0)))
                    ""))
          "\n" (org-foresight-report--hour-axis)
          "\n" (if cov
                   (org-foresight-report--sparkline-colored (plist-get data :binned)
                                              (plist-get cov :ca) (plist-get cov :cf)
                                              (plist-get cov :ua) (plist-get cov :uf))
                 (org-foresight-report--sparkline (plist-get data :binned)))
          "\n" (format "| %s | %s | %5s | %5s | %6s | %s |"
                       (truncate-string-to-width "Cat" 4 0 ?\s)
                       (truncate-string-to-width "Activity" 12 0 ?\s)
                       "Time" "%" "Leak" (truncate-string-to-width "Share" 14 0 ?\s))
          "\n|------+--------------+-------+-------+--------+----------------|"
          (mapconcat
           (lambda (r)
             (let ((cat (nth 0 r)) (name (nth 1 r)) (sec (nth 2 r)) (leak (nth 3 r)))
               (format "\n| %s | %s | %5s | %5.1f | %6s | %s |"
                       (truncate-string-to-width (org-foresight-report--cat-abbrev cat) 4 0 ?\s)
                       (truncate-string-to-width
                        (replace-regexp-in-string "[|\n\r]" " " name) 12 0 ?\s)
                       (org-duration-from-minutes (/ sec 60.0))
                       (* 100.0 (/ sec full))
                       (if (and leak (> leak 0)) (org-duration-from-minutes (/ leak 60.0)) "")
                       (truncate-string-to-width
                        (orgtbl-ascii-draw sec 0 (max maxrow 1) 14
                                           org-foresight-bar-chars)
                        14 0 ?\s))))
           rows "")
          (let ((em (plist-get data :emacs-projects)))
            (if em
                (concat "\nemacs: "
                        (mapconcat
                         (lambda (kv)
                           (format "%s %.0f" (or (car kv) "?") (/ (cdr kv) 60.0)))
                         (seq-take em 5) " · "))
              "")))
         'face 'org-table)))))

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

(defun org-foresight-report-clocked (clock)
  "Return today's CATEGORY-share table with a focus-budget header (<=80 cols).
The bar column is scaled to the largest project; the % column carries the
exact share of today's clocked total (so the % values sum to 100).
CLOCK is the plist from `org-foresight-clock-scan', called with DAYS large
enough to also cover the vs-7-day comparison in the header."
  (let* ((rows (plist-get clock :today-rows))
         (total (plist-get clock :today-total))
         (segments (plist-get clock :today-segments))
         (avg7 (/ (plist-get clock :total) 7.0))
         (aw (org-foresight-observe-today))
         (active-min (and aw (/ (plist-get aw :active) 60.0)))
         (maxmin (if rows (apply #'max (mapcar #'cdr rows)) 1))
         (budget
          (concat
           (format "Focus %s" (org-duration-from-minutes total))
           (when (and active-min (> active-min 0))
             (format " · %.0f%% of active" (* 100.0 (/ total active-min))))
           (when (> segments 0)
             (format " · avg %.0fm ×%d" (/ (float total) segments) segments))
           (when (> avg7 0)
             (format " · vs7d %+.0f%%" (* 100.0 (/ (- total avg7) avg7)))))))
    (concat budget "\n" (org-foresight-report--category-table rows total maxmin "Project"))))

(defun org-foresight-report-estimate ()
  "Return today's EFFORT-vs-actual table (each line <=80) for estimated tasks."
  (let (rows)
    (dolist (file (org-agenda-files))
      (let ((entries
             (nth 2 (with-current-buffer (find-file-noselect file)
                      (ignore-errors
                        (org-clock-get-table-data
                         file '(:block today :properties ("Effort")
                                       :maxlevel 99)))))))
        (dolist (e entries)
          (let ((headline (nth 1 e))
                (time (nth 4 e))
                (effort (cdr (assoc "Effort" (nth 5 e)))))
            (when (and effort (> (or time 0) 0))
              (push (list headline (org-duration-to-minutes effort) time) rows))))))
    (propertize
     (if (null rows)
         "(no estimated tasks clocked today)"
       (concat
        (format "| %-28s | %5s | %5s | %-12s |%5s" "Task" "Plan" "Act" "Progress" "%")
        "\n|" (make-string 30 ?-) "+" (make-string 7 ?-) "+" (make-string 7 ?-)
        "+" (make-string 14 ?-) "+" (make-string 5 ?-) "\n"
        (mapconcat
         (lambda (r)
           (let ((plan (nth 1 r)) (act (nth 2 r)))
             (format "| %s | %5s | %5s | %s |%4.0f%%"
                     (truncate-string-to-width
                      (replace-regexp-in-string "[|\n\r]" " " (nth 0 r)) 28 0 ?\s)
                     (org-duration-from-minutes plan)
                     (org-duration-from-minutes act)
                     (truncate-string-to-width
                      (orgtbl-ascii-draw (min act plan) 0 (max plan 1) 12
                                         org-foresight-bar-chars)
                      12 0 ?\s)
                     (if (> plan 0) (* 100.0 (/ (float act) plan)) 0))))
         (sort rows (lambda (a b) (> (nth 2 a) (nth 2 b))))
         "\n"))) 'face 'org-table)))

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
Each is called with no arguments and returns a line, or nil to add nothing.
A registry rather than a fixed call, so a file loaded later can add to the
summary without this one having to know about it.")

(defun org-foresight-report--verdict-extras ()
  "Return the extra verdict lines, newline-prefixed, or the empty string."
  (mapconcat (lambda (fn)
               (if-let ((line (org-foresight-report--guarded fn)))
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
(defface org-foresight-report-surge '((t :inherit shadow))
  "The reserve held back for interruptions.")
(defface org-foresight-report-spare '((t :inherit success))
  "Time that may still be promised, and the gaps it is made of.
The one quantity on the board worth growing, so it is worth a colour of its
own rather than another step down a grey ramp.")
(defface org-foresight-report-private '((t :inherit font-lock-string-face))
  "Life: commitments that are not work and are not capacity either.
Distinct from both the claimed hours and the empty ones, because it is
neither -- and a day where the two are told apart is a day where the
question \"can this move\" has an obvious answer.")
(defface org-foresight-report-grey '((t :inherit font-lock-comment-face))
  "Waking hours that are neither work nor a private commitment.")

(defcustom org-foresight-bar-width 40
  "Width in columns of the capacity bar."
  :type 'integer
  :group 'org-foresight)

(defvar org-foresight-report--bar-segments
  '((:key :booked-min    :face org-foresight-report-booked   :label "booked")
    (:key :travel-min    :face org-foresight-report-travel   :label "travel")
    (:key :private-min-in-span :face org-foresight-report-private
          :label "private")
    (:key :committed-min :face org-foresight-report-promised :label "promised")
    (:key :surge-min     :face org-foresight-report-surge    :label "surge")
    (:key :spare-min     :face org-foresight-report-spare    :label "spare"))
  "The bar's segments in order, each a plist of plist-key, face and label.

They divide the work span exactly: booked and travel are the hours already
spoken for, promised the effort accepted without a time, surge the reserve,
and spare whatever survives all four.  The words are the ones the grid below
uses for the same things, so the two blocks can be read as one account.

All drawn in full blocks; the colour is what tells them apart.")

(defvar org-foresight-report--off-segments
  '((:key :private-min  :face org-foresight-report-private  :label "private")
    (:key :borrowed-min :face org-foresight-report-travel   :label "borrowed")
    (:key :unclaimed-min :face org-foresight-report-spare   :label "unclaimed"))
  "Segments of the second bar, dividing the waking day outside the work span.

`unclaimed' rather than `free': the grid uses `free' for work time nothing
has claimed, and an hour with no work in it and an evening with no plans in
it are not the same kind of emptiness.")

(defun org-foresight-report--bar-scale (cap)
  "Return minutes per column, chosen so both bars can share a scale.

Two bars drawn to their own widths cannot be compared, and comparing them is
the point: a working day that dwarfs the evening is the thing worth seeing.
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

(defcustom org-foresight-bar-max-width 64
  "Widest a bar may be drawn, including any overflow past the span.
The line it sits on has to fit the screen; a day promised three times over
would otherwise run off the end of it."
  :type 'integer
  :group 'org-foresight)

(defun org-foresight-report--draw-bar (cap segments per-column &optional limit)
  "Return SEGMENTS of CAP drawn at PER-COLUMN minutes to the column.

LIMIT, when given, marks where the span runs out -- so an overcommitted day
shows its overflow rather than being clipped back to something that fits.
Runaway overflow is cut at `org-foresight-bar-max-width' and ended with `…',
since past a point the exact length of the impossible stops mattering and the
figure above says it anyway."
  (let ((bar "")
        (total 0.0))
    (dolist (seg segments)
      (let* ((mins (max 0.0 (or (plist-get cap (plist-get seg :key)) 0.0)))
             (n (round (/ mins per-column))))
        (setq total (+ total mins))
        (setq bar (concat bar (propertize
                               (make-string (max 0 n) org-foresight-block)
                               'face (plist-get seg :face))))))
    (when (and limit (> total limit))
      (let ((mark (round (/ limit per-column))))
        (when (< mark (length bar))
          (setq bar (concat (substring bar 0 mark)
                            (propertize "┃" 'face
                                        'org-foresight-report-overcommitted)
                            (substring bar (1+ mark)))))))
    (if (<= (length bar) org-foresight-bar-max-width)
        bar
      (concat (substring bar 0 (1- org-foresight-bar-max-width))
              (propertize "…" 'face 'org-foresight-report-overcommitted)))))

(defun org-foresight-report--bar (cap)
  "Return the work span drawn as a stacked bar, or nil.

The whole bar is the work span, so meetings and journeys appear in it rather
than being silently deducted first: the question \"where did the day go\" is
answered by the same picture as \"what is left\"."
  (when (> (plist-get cap :span-min) 0)
    (org-foresight-report--draw-bar
     cap org-foresight-report--bar-segments
     (org-foresight-report--bar-scale cap)
     (plist-get cap :span-min))))

(defun org-foresight-report--off-bar (cap)
  "Return the waking day outside the work span, drawn to the same scale."
  (let ((total (+ (max 0.0 (or (plist-get cap :private-min) 0.0))
                  (max 0.0 (or (plist-get cap :borrowed-min) 0.0))
                  (max 0.0 (or (plist-get cap :unclaimed-min) 0.0)))))
    (when (> total 0)
      (org-foresight-report--draw-bar
       cap org-foresight-report--off-segments
       (org-foresight-report--bar-scale cap)))))

(defun org-foresight-report--key (cap segments total-key total-label)
  "Return a legend for SEGMENTS of CAP, led by TOTAL-LABEL and TOTAL-KEY.

Written as the sum it is, so the parts can be checked against the whole
without the reader having to add them up and hope.  Segments that are zero
are left out: a day with no travel in it has nothing to say about travel,
and saying it anyway costs the width the rest of the sum needs."
  (let* ((parts (seq-keep
                 (lambda (seg)
                   (let ((mins (max 0.0 (or (plist-get cap (plist-get seg :key))
                                            0.0))))
                     (when (> mins 0)
                       (concat (propertize (string org-foresight-block)
                                           'face (plist-get seg :face))
                               " " (plist-get seg :label) " "
                               (org-duration-from-minutes mins)))))
                 segments))
         (lead (format "%-4s %5s = " total-label
                       (org-duration-from-minutes
                        (max 0.0 (or (plist-get cap total-key) 0.0)))))
         (indent (make-string (length lead) ?\s))
         (line lead)
         out)
    (if (null parts)
        (concat lead "nothing")
      ;; Wrapped rather than truncated: with every segment in play the sum is
      ;; simply longer than a line, and a sum missing its last term is worse
      ;; than one that takes two lines to finish.
      (while parts
        (let* ((part (pop parts))
               (piece (concat part (and parts " +"))))
          (if (or (equal line lead)
                  (<= (+ (string-width line) 1 (string-width piece)) 80))
              (setq line (if (equal line lead)
                             (concat line piece)
                           (concat line " " piece)))
            (push line out)
            (setq line (concat indent piece)))))
      (push line out)
      (string-join (nreverse out) "\n"))))

(defun org-foresight-report--bar-key (cap)
  "Return the legend naming each of the work bar's segments in CAP."
  (org-foresight-report--key cap org-foresight-report--bar-segments
                             :span-min "Work"))

(defun org-foresight-report--off-key (cap)
  "Return the legend naming each of the off bar's segments in CAP."
  (org-foresight-report--key cap org-foresight-report--off-segments
                             :off-min "Off"))

(defun org-foresight-report--verdict (cap)
  "Return the one-line answer for capacity plist CAP.
Everything the block exists to say, in the width of a single line: what is
left, what has been promised away, and the hour the day actually ends."
  (let ((headroom (plist-get cap :headroom-min))
        (finish (plist-get cap :finish))
        (samples (org-foresight-surge-samples)))
    (concat
     (format "Work %s" (org-duration-from-minutes (plist-get cap :span-min)))
     (if (>= headroom 0)
         (format " · %s left to promise" (org-duration-from-minutes headroom))
       (propertize
        (format " · OVER by %s" (org-duration-from-minutes (- headroom)))
        'face 'org-foresight-report-overcommitted))
     (if finish
         (format " · ends %s" (format-time-string "%H:%M" finish))
       (propertize " · will not fit" 'face 'org-foresight-report-overcommitted))
     (when samples (format " · surge from %d day(s)" samples))
     ;; Shown whenever it is doing anything, so a day that has shrunk reads as
     ;; "my estimates are optimistic" rather than "the tool is being harsh".
     (let ((factor (org-foresight-bias-factor nil)))
       (if (> (abs (- factor 1.0)) 0.1)
           (format " · est ×%.1f" factor)
         "")))))

(defun org-foresight-report--grey-line (cap)
  "Return CAP's day outside the work span: its key and its bar, or nil.

Kept apart from the work bar deliberately, but drawn to the same scale, so
the two can be set against each other -- a working day that dwarfs the
evening is exactly the thing worth seeing.  Unclaimed evenings are not
capacity waiting to be spent: the emptiness is what makes room for anything
new, and a day that quietly borrows from it should have to say so."
  (when-let ((bar (org-foresight-report--off-bar cap)))
    (concat (org-foresight-report--off-key cap) "\n" bar)))

(defun org-foresight-report-capacity-line (&optional day scan now)
  "Return DAY's capacity verdict as one line, or nil on a non-working day.
This is the line that goes at the very top of the agenda: a number you should
not have to go looking for is a number you will not look at."
  (let* ((day (or day (org-foresight--day-start 0)))
         (scan (or scan (org-foresight-scan 1 day)))
         (cap (org-foresight-capacity day scan now))
         (idx (org-foresight--day-of day (plist-get scan :from)))
         (ledger (and (>= idx 0) (< idx (plist-get scan :days))
                      (aref (plist-get scan :ledger) idx))))
    (when (plist-get cap :window)
      (concat (org-foresight-report--verdict cap)
              ;; Directly under the overflow, because the number and the way
              ;; out of it are one thought and reading them apart is what
              ;; makes an overcommitted day feel like weather.
              (when-let ((frees (org-foresight-report--frees
                                 (- (min 0.0 (plist-get cap :headroom-min)))
                                 ledger)))
                (concat "\n" frees))
              (when-let ((bar (org-foresight-report--bar cap)))
                (concat "\n" (org-foresight-report--bar-key cap) "\n" bar))
              (when-let ((grey (org-foresight-report--grey-line cap)))
                (concat "\n" grey))
              (org-foresight-report--verdict-extras)))))

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

;;;; The day as a grid
;; The bands already are a time grid: `org-foresight-day-blocks' hands back the
;; waking hours cut into contiguous pieces, so putting the clock down the left
;; and drawing each piece in proportion is not a new view of the data -- it is
;; the data, finally shown in its own shape.
;;
;; The gutter is what a table of durations cannot do: a three-hour gap looks
;; like a three-hour gap.  Where the day actually went stops being a column of
;; numbers to add up and becomes something seen at a glance.

(defcustom org-foresight-grid-minutes-per-column 15
  "Minutes each column of the grid's gutter stands for."
  :type 'integer
  :group 'org-foresight)

(defcustom org-foresight-grid-gutter-width 8
  "Widest the grid's gutter may grow, in columns.

Eight quarter-hours is two hours, and two hours is about where a piece of
work stops being one piece: past that it wants breaking up rather than
drawing longer.  So the gutter saturates there instead of running on, and
the number in the effort column carries anything above it."
  :type 'integer
  :group 'org-foresight)

(defvar org-foresight-report--grid-faces
  '((meeting   . org-foresight-report-booked)
    (task      . org-foresight-report-booked)
    (travel    . org-foresight-report-travel)
    (promised  . org-foresight-report-promised)
    (private   . org-foresight-report-private)
    (context   . org-foresight-report-grey)
    (available . org-foresight-report-spare)
    (grey      . org-foresight-report-grey))
  "Alist of band kind to the face its gutter is drawn in.
One shape throughout, so nothing can step at a seam; the kind is carried by
colour, which is what colour is for.")

(defvar org-foresight-report--grid-glyphs
  '((available . ?·))
  "Kinds drawn with something other than a full block.
Unclaimed time is the exception worth making: a run of dots reads as absence
in a way that a coloured block, however pale, does not.")

(defvar org-foresight-report--grid-legend
  '((meeting . "booked") (travel . "travel") (promised . "promised")
    (private . "private") (available . "free"))
  "Kinds named in the grid's key, in the order they are shown.
The same words the capacity bar uses, for the same things.")

(defvar org-foresight-report--grid-flags
  '(("?" . "no time yet") ("!" . "due today")
    ("⨯" . "double-booked") ("╰" . "alongside"))
  "Marks in the grid's flag column and what each stands for.

Each has to occupy exactly one cell or the column behind it steps.  That
rules out characters a monospace font is likely to be missing -- a glyph it
does not have comes from wherever the fallback finds it, at whatever width
that font happens to use.  `✗' U+2717 is one such: absent from PlemolJP, and
rendered a little too wide by the substitute.  `⨯' U+2A2F says the same thing
and is present.")

(defun org-foresight-report--grid-key (&optional flags)
  "Return the grid's key: what the gutter shapes and the FLAGS mean.

The bar above the grid carries its own key, and without one here the same
shape would appear in two blocks with nothing saying whether it means the
same thing.  It does -- and saying so is cheaper than leaving it assumed.

Only the flags actually used are explained.  A key that lists what a clash
looks like on a day that has none is describing a problem the reader does
not have, which is a slower way of saying nothing."
  (concat
   (mapconcat (lambda (c)
                (concat (propertize
                         (char-to-string
                          (or (cdr (assq (car c) org-foresight-report--grid-glyphs))
                              org-foresight-block))
                         'face (cdr (assq (car c)
                                          org-foresight-report--grid-faces)))
                        " " (cdr c)))
              org-foresight-report--grid-legend "  ")
   (when flags
     (concat
      "\n"
      (propertize
       (mapconcat (lambda (f) (concat f " "
                                      (cdr (assoc f
                                                  org-foresight-report--grid-flags))))
                  flags "  ")
       'face 'shadow)))))

(defun org-foresight-report--grid-gutter (kind minutes)
  "Return the gutter for a band of KIND lasting MINUTES.

Whole cells are full blocks; what is left over draws the part-width block
nearest to it, so twenty minutes at a quarter-hour to the cell reads as one
cell and a third rather than being rounded to one or to two.  That is what
those characters are for, and the only place they are metrically safe to sit
beside a full block -- at the end of a run, where nothing follows them."
  (let* ((face (or (cdr (assq kind org-foresight-report--grid-faces)) 'default))
         (glyph (cdr (assq kind org-foresight-report--grid-glyphs)))
         (cells (/ minutes (float org-foresight-grid-minutes-per-column)))
         (full (min org-foresight-grid-gutter-width (floor cells)))
         (rest (- cells full))
         (eighths (round (* 8 rest)))
         (text
          (cond
           ;; A kind with a glyph of its own is a texture, not a measure.
           (glyph (make-string (max 1 (min org-foresight-grid-gutter-width
                                           (round cells)))
                               glyph))
           ((>= full org-foresight-grid-gutter-width)
            (make-string org-foresight-grid-gutter-width org-foresight-block))
           ((and (zerop full) (zerop eighths)) "▏")
           ((zerop eighths) (make-string full org-foresight-block))
           ((>= eighths 8) (make-string (1+ full) org-foresight-block))
           (t (concat (make-string full org-foresight-block)
                      (char-to-string
                       (aref org-foresight-report--partials (1- eighths))))))))
    (truncate-string-to-width (propertize text 'face face)
                              org-foresight-grid-gutter-width 0 ?\s)))

(defun org-foresight-report--grid-row (span flag gutter effort category title
                                            marker &optional day stamp)
  "Return one grid row, actionable when MARKER is a marker.

Columns are category, span, effort, flag, gutter, deadline, title -- close to
the order Org's own agenda puts them in (`org-agenda-prefix-format' reads
category, time, effort), so a day read here and a day read there scan the
same way.

SPAN is \"HH:MM-HH:MM\", or \"?\" where the work has no time yet.  The kind
of block gets no column of its own: the gutter says it in shape and colour,
and repeating that would cost width the title wants.  The deadline sits in a
column of its own so every one on the board lines up -- the question \"which
of these can move\" is asked of the whole day at once, not row by row.  TITLE
is last and is not truncated: nothing after it needs to line up, so cutting
it would only lose words to no purpose."
  (org-foresight-report--actionable
   (format " %s %-11s %5s %s %s %s %s"
           (truncate-string-to-width (or category "") 8 0 ?\s)
           (or span "")
           (or effort "")
           (or flag " ")
           gutter
           (org-foresight-report--grid-deadline marker day)
           (replace-regexp-in-string "[\n\r]" " " (or title "")))
   marker stamp))

(defun org-foresight-report--grid-span (start end)
  "Return START and END as \"HH:MM-HH:MM\"."
  (concat (format-time-string "%H:%M" start) "-"
          (format-time-string "%H:%M" end)))

(defun org-foresight-report--grid-todo (marker)
  "Return the TODO keyword of MARKER's entry, with a trailing space, or \"\".
The agenda shows the state beside the heading and it is worth having here for
the same reason: NEXT and WAIT are different answers to \"can this move\"."
  (or (and (markerp marker)
           (marker-buffer marker)
           (with-current-buffer (marker-buffer marker)
             (org-with-wide-buffer
              (goto-char marker)
              (when-let ((kw (org-get-todo-state)))
                (concat (propertize kw 'face (org-get-todo-face kw)) " ")))))
      ""))

(defun org-foresight-report--grid-deadline (marker &optional day)
  "Return MARKER's deadline relative to DAY, as a short right-aligned field.

Every row on this board invites the same question -- can this be moved to
another day -- and a deadline is the answer to it.  Given as days rather
than a date because that is the form the question takes: `+3d' can wait,
`0d' cannot, `-2d' should already have gone."
  (let ((day (or day (org-foresight--day-start 0))))
    (or (and (markerp marker)
             (marker-buffer marker)
             (with-current-buffer (marker-buffer marker)
               (org-with-wide-buffer
                (goto-char marker)
                (when-let ((dead (and (not (org-entry-is-done-p))
                                      (org-get-deadline-time (point)))))
                  (let ((n (org-foresight--day-of dead day)))
                    (propertize
                     (format "%4s" (if (zerop n) "0d" (format "%+dd" n)))
                     'face (cond ((< n 0) 'org-foresight-report-overcommitted)
                                 ((<= n 1) 'org-foresight-report-promised)
                                 (t 'shadow))))))))
        "    ")))

(defun org-foresight-report--grid-category (b)
  "Return the category column for band or ledger entry B.
Falls back to naming the kind for a derived block, which has no category of
its own but is not nothing either."
  (or (plist-get b :category)
      (pcase (plist-get b :kind)
        ('travel "travel")
        (_ ""))))

(defcustom org-foresight-grid-suggest 3
  "How many candidates a free stretch names, or nil to name none."
  :type '(choice (const :tag "none" nil) integer)
  :group 'org-foresight)

(defcustom org-foresight-grid-frees 3
  "How many ways out of an overcommitted day are named, or nil to name none."
  :type '(choice (const :tag "none" nil) integer)
  :group 'org-foresight)

(defun org-foresight-report--entry-minutes (e)
  "Return how many minutes ledger entry E costs the day.
The adjusted estimate where there is one, since that is what capacity spends."
  (or (plist-get e :effort-adj) (plist-get e :effort) 0))

(defun org-foresight-report--name-run (entries limit budget)
  "Return at most LIMIT of ENTRIES named within BUDGET columns, or nil.

Each is its title and what it costs.  What did not fit is counted rather than
dropped, because the difference between three candidates and thirty is the
whole answer on some days."
  (let ((used 0) (shown 0) parts)
    (dolist (e entries)
      (let* ((piece (format "%s %s"
                            (truncate-string-to-width
                             (plist-get e :title) 22 nil nil "…")
                            (org-duration-from-minutes
                             (org-foresight-report--entry-minutes e))))
             (cost (+ (string-width piece) (if parts 3 0))))
        (when (and (< shown limit) (<= (+ used cost) budget))
          (setq used (+ used cost) shown (1+ shown))
          (push piece parts))))
    (when parts
      (concat (string-join (nreverse parts) " · ")
              (let ((more (- (length entries) shown)))
                (if (> more 0) (format " +%d" more) ""))))))

(defun org-foresight-report--grid-fits (mins ledger)
  "Return the line naming LEDGER's unplaced work that fits in MINS, or nil.

The one thing a text grid can do that a picture of the day cannot: hold a
gap and a list of candidates in the same glance.  Sized by what the work
actually takes, so nothing is offered that would not really go in."
  (when (and org-foresight-grid-suggest (> mins 0))
    (let* ((fits (seq-filter
                  (lambda (e)
                    (and (eq (plist-get e :kind) 'promised)
                         (<= (org-foresight-report--entry-minutes e) mins)))
                  ledger))
           ;; Largest first: the biggest thing that will go in is the one worth
           ;; knowing about, since anything smaller will still fit afterwards.
           (ranked (seq-sort-by #'org-foresight-report--entry-minutes #'> fits))
           ;; Hung under the span column, not the gutter: the arrow points out
           ;; of the time slot, and the shallower indent is what makes room for
           ;; more than one candidate on the line.
           (indent (make-string 10 ?\s))
           (named (org-foresight-report--name-run
                   ranked org-foresight-grid-suggest
                   (- 80 (length indent) (length "↳ fits ")))))
      (when named
        (propertize (concat indent "↳ fits " named) 'face 'shadow)))))

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
                        (- 80 (length lead))))))
      (when named
        (propertize (concat lead named) 'face 'shadow)))))

(defun org-foresight-report--grid-band (b &optional day)
  "Return the grid row for band B, or nil when it is not worth a line."
  (let* ((kind (plist-get b :kind))
         (mins (/ (float-time (time-subtract (plist-get b :end)
                                             (plist-get b :start)))
                  60.0)))
    ;; Empty private time is not news -- the grey total under the bar already
    ;; says how much of it there is, and a row per stretch of nothing pushes
    ;; the day itself off the screen.
    (unless (eq kind 'grey)
      (let* ((trimmed (plist-get b :trimmed))
             ;; What it actually needs, not what was left for it.
             (wanted (if trimmed
                         (/ (float-time (time-subtract (plist-get b :end)
                                                       (plist-get b :full-start)))
                            60.0)
                       mins)))
        (org-foresight-report--grid-row
         (org-foresight-report--grid-span (if trimmed
                                              (plist-get b :full-start)
                                            (plist-get b :start))
                                          (plist-get b :end))
         (when trimmed
           (propertize "⨯" 'face 'org-foresight-report-overcommitted))
         (org-foresight-report--grid-gutter kind wanted)
         (org-duration-from-minutes wanted)
         (if (eq kind 'available) "" (org-foresight-report--grid-category b))
         (if (eq kind 'available)
             "free"
           (concat (org-foresight-report--grid-todo (plist-get b :marker))
                   (or (plist-get b :title) "?")))
         (plist-get b :marker)
         day
         (plist-get b :stamp))))))

(defun org-foresight-report--grid-alongside (e &optional day)
  "Return the row for ledger entry E, shown beneath whatever it shares with."
  (org-foresight-report--grid-row
   (org-foresight-report--grid-span (plist-get e :start) (plist-get e :end))
   (propertize "╰" 'face 'org-foresight-report-surge)
   (org-foresight-report--grid-gutter 'promised (plist-get e :effort))
   (org-duration-from-minutes (plist-get e :effort))
   (org-foresight-report--grid-category e)
   (or (plist-get e :title) "?")
   (plist-get e :marker)
   day
   (plist-get e :stamp)))

(defun org-foresight-report--grid-boundary (time label)
  "Return the rule drawn where the working day opens or closes.
Drawn the full width of the gutter, so it reads as a line across the day
rather than as another block in it."
  (org-foresight-report--grid-row
   (format-time-string "%H:%M" time)
   nil
   (propertize (make-string org-foresight-grid-gutter-width ?─)
               'face 'org-agenda-structure)
   "" "" label nil))

(defun org-foresight-report--grid-eclipsed (ledger bands &optional day)
  "Return rows for LEDGER entries that BANDS had no room to show.

The bands partition the day, so where two things are booked over the same
minutes only one of them survives.  The one that lost is the more important
of the pair to see: a day with two things in one slot cannot be worked, and
tidying it into a day that can is how the clash goes unnoticed until it
happens."
  ;; Matched on title and end rather than on the marker: a journey borrows the
  ;; marker of the meeting it serves, so by marker alone a trimmed-away trip
  ;; looks like the meeting that is still on screen.  A trimmed band keeps its
  ;; end; one that lost entirely has no band at all.
  (let ((shown (mapcar (lambda (b)
                         (cons (plist-get b :title) (plist-get b :end)))
                       bands)))
    (seq-keep
     (lambda (e)
       ;; Something that will share its hour is not competing for it; it is
       ;; reported under the band it shares, not as a clash.
       (when (and (plist-get e :start)
                  (memq (or (plist-get e :attention) 'blocking) '(blocking))
                  (not (member (cons (plist-get e :title) (plist-get e :end))
                               shown)))
         (org-foresight-report--grid-row
          (org-foresight-report--grid-span (plist-get e :start)
                                           (plist-get e :end))
          (propertize "⨯" 'face 'org-foresight-report-overcommitted)
          (org-foresight-report--grid-gutter
           (plist-get e :kind) (plist-get e :effort))
          (org-duration-from-minutes (plist-get e :effort))
          (org-foresight-report--grid-category e)
          (or (plist-get e :title) "?")
          (plist-get e :marker)
          day
          (plist-get e :stamp))))
     ledger)))

(defun org-foresight-report--grid-context (ledger &optional day)
  "Return rows for LEDGER entries that belong to somebody else.

They take none of the day, so they are not part of it -- but a child's
fixture is exactly the sort of thing that decides when the house is empty,
and a plan made without knowing it is a plan made blind."
  (seq-keep
   (lambda (e)
     (when (and (plist-get e :start)
                (eq (plist-get e :attention) 'informational))
       (org-foresight-report--grid-row
        (org-foresight-report--grid-span (plist-get e :start) (plist-get e :end))
        nil
        (org-foresight-report--grid-gutter 'context (plist-get e :effort))
        (org-duration-from-minutes (plist-get e :effort))
        (org-foresight-report--grid-category e)
        (or (plist-get e :title) "?")
        (plist-get e :marker)
        day
        (plist-get e :stamp))))
   ledger))

(defun org-foresight-report--grid-unplaced (ledger &optional day)
  "Return rows for LEDGER entries that have been promised but given no time."
  (seq-keep
   (lambda (e)
     (when (eq (plist-get e :kind) 'promised)
       (let ((est (plist-get e :effort))
             (adj (plist-get e :effort-adj)))
         ;; The question mark stands where the clock would be, because that is
         ;; precisely what is missing.  It still has a length, so it still
         ;; gets a gutter: how much of a day it wants is the reason to place
         ;; it at all.
         (org-foresight-report--grid-row
          (propertize "?" 'face 'org-foresight-report-promised)
          nil
          (org-foresight-report--grid-gutter 'promised (or adj est))
          (org-duration-from-minutes (or adj est))
          (org-foresight-report--grid-category e)
          (concat (org-foresight-report--grid-todo (plist-get e :marker))
                  (or (plist-get e :title) "?")
                  (when (and adj (> (abs (- adj est)) 1))
                    (format " (est %s)" (org-duration-from-minutes est))))
          (plist-get e :marker)
          day))))
   ledger))

(defun org-foresight-report--grid-due (day ledger)
  "Return rows for work due on DAY that occupies none of it.

A deadline takes no time and so appears nowhere in the bands, which is
exactly how a day gets planned without it.  Anything already on the board
is skipped: its own row now carries the deadline, and listing it twice
would say nothing the second time."
  (let ((shown (seq-keep (lambda (e) (plist-get e :title)) ledger))
        out)
    (dolist (file (org-agenda-files))
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (org-with-wide-buffer
           (org-map-entries
            (lambda ()
              (let ((dead (org-get-deadline-time (point)))
                    (title (org-get-heading t t t t)))
                (when (and dead
                           (not (org-entry-is-done-p))
                           (not (member title shown))
                           (= (org-foresight--day-of dead day) 0))
                  (push (org-foresight-report--grid-row
                         (propertize "?" 'face
                                     'org-foresight-report-overcommitted)
                         (propertize "!" 'face
                                     'org-foresight-report-overcommitted)
                         (make-string org-foresight-grid-gutter-width ?\s)
                         ""
                         (or (org-entry-get (point) "CATEGORY" t) "")
                         (concat (when-let ((kw (org-get-todo-state)))
                                   (concat (propertize kw 'face
                                                       (org-get-todo-face kw))
                                           " "))
                                 title)
                         (point-marker)
                         day)
                        out))))
            nil nil)))))
    (nreverse out)))

(defun org-foresight-report-capacity (&optional day scan now)
  "Return DAY drawn as a grid: when its hours go, and what is left over.

The verdict above states a number; this shows what the number is made of, and
every row leads back to the entry behind it.  A capacity figure nobody can
take apart is one nobody can act on -- the answer to \"why is there no time
today\" has to be a list of things, in the order they happen."
  (let* ((day (or day (org-foresight--day-start 0)))
         (scan (or scan (org-foresight-scan 1 day)))
         (cap (org-foresight-capacity day scan now))
         (work (plist-get cap :window))
         (idx (org-foresight--day-of day (plist-get scan :from)))
         (ledger (and (>= idx 0) (< idx (plist-get scan :days))
                      (aref (plist-get scan :ledger) idx)))
         (bands (org-foresight-day-blocks day scan))
         rows)
    (if (null work)
        (propertize "(not a working day)" 'face 'org-table)
      (let ((opened nil) (closed nil))
        (dolist (b bands)
          ;; The span's edges are drawn where they fall, so anything above or
          ;; below the rules is visibly work that escaped the working day.
          (unless (or opened (time-less-p (plist-get b :start) (car work)))
            (setq opened t)
            (push (org-foresight-report--grid-boundary (car work) "work starts")
                  rows))
          (unless (or closed (time-less-p (plist-get b :start) (cdr work)))
            (setq closed t opened t)
            (push (org-foresight-report--grid-boundary (cdr work) "work ends")
                  rows))
          (when-let ((row (org-foresight-report--grid-band b day)))
            (push row rows)
            ;; A gap says what would go in it, right where the gap is: the
            ;; whole difficulty of rearranging a day in a list is holding one
            ;; row in mind while reading another.
            (when (eq (plist-get b :kind) 'available)
              (when-let ((fits (org-foresight-report--grid-fits
                                (/ (float-time
                                    (time-subtract (plist-get b :end)
                                                   (plist-get b :start)))
                                   60.0)
                                ledger)))
                (push fits rows)))
            ;; Anything that was happening at the same time and did not need
            ;; all of you hangs below the band, so the clock still reads as
            ;; one line down the page.
            (dolist (a (org-foresight-report--grid-sharers ledger b day))
              (push a rows))))
        (unless opened
          (push (org-foresight-report--grid-boundary (car work) "work starts")
                rows))
        (unless closed
          (push (org-foresight-report--grid-boundary (cdr work) "work ends")
                rows)))
      (let* ((eclipsed (org-foresight-report--grid-eclipsed ledger bands day))
             (unplaced (org-foresight-report--grid-unplaced ledger day))
             (due (org-foresight-report--grid-due day ledger))
             (extra (append eclipsed unplaced due))
             (context (org-foresight-report--grid-context ledger day)))
        (mapconcat
         #'identity
         (append (list (org-foresight-report--grid-key
                        (delq nil
                              (list (and (or eclipsed
                                             (seq-find
                                              (lambda (r)
                                                (string-match-p "⨯" r))
                                              rows))
                                         "⨯")
                                    (and unplaced "?")
                                    (and due "!")
                                    (and (seq-find
                                          (lambda (r)
                                            (string-match-p "╰" r)) rows)
                                         "╰"))))
                       "")
                 (nreverse rows)
                 (and extra '(""))
                 extra
                 (and context
                      (list "" (org-foresight-report--badge
                                "Alongside" "happening, but not yours")))
                 context)
         "\n")))))

(defun org-foresight-report--grid-sharers (ledger band &optional day)
  "Return rows for LEDGER entries sharing BAND's time without needing all of it."
  (let ((s (plist-get band :start))
        (n (plist-get band :end)))
    (seq-keep
     (lambda (e)
       (when (and (eq (plist-get e :attention) 'background)
                  (plist-get e :start)
                  (not (equal (plist-get e :title) (plist-get band :title)))
                  (time-less-p (plist-get e :start) n)
                  (time-less-p s (plist-get e :end)))
         (org-foresight-report--grid-alongside e day)))
     ledger)))

;;;; Agenda integration

(defvar org-foresight-report-renderers
  '((daily  :body org-foresight-report--daily  :place bottom)
    (review :body org-foresight-report--review :place bottom))
  "Alist of (STYLE :body FUNCTION :place top-or-bottom).

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

(defun org-foresight-report--body ()
  "Return the report text for the current `org-foresight-report-style'.
Dispatches through `org-foresight-report-renderers'; an unknown style simply
renders nothing."
  (when-let ((fn (plist-get (org-foresight-report--renderer) :body)))
    (funcall fn)))

(defun org-foresight-report--daily ()
  "Return the daily report: capacity windows, then what has been spent.
Scans clock data once and threads the result to every table that needs it,
rather than each table re-scanning independently."
  (let ((clock (org-foresight-clock-scan 7)))
    (concat "\n"
            ;; Forward-looking first: what is still possible matters before
            ;; what has already been spent.
            (org-foresight-report--badge "Capacity" "when the day is still open")
            "\n"
            (org-foresight-report-capacity)
            "\n\n"
            (org-foresight-report--badge "Clocked" "share of focus today")
            "\n"
            (org-foresight-report-clocked clock)
            "\n\n"
            (org-foresight-report--badge "Estimate" "planned vs actual")
            "\n"
            (org-foresight-report-estimate)
            "\n\n"
            (org-foresight-report--badge "Observed"
                                         "reality & rhythm · ActivityWatch")
            "\n"
            (org-foresight-report-observed clock)
            "\n")))

(defun org-foresight-report--review ()
  "Return the weekly review report: where the last seven days actually went."
  (concat "\n"
          (org-foresight-report--badge "Clocked" "by area · last 7 days")
          "\n"
          (org-foresight-report-week (org-foresight-clock-scan 7))
          "\n"))

(defun org-foresight-report--guarded (thunk)
  "Call THUNK, returning its string or a visible complaint on failure.
A failure must never take the agenda down with it, but it must not vanish
either: silently swallowing the error leaves a missing block and no way to
find out why, which is far worse to live with than one ugly line."
  (condition-case err (funcall thunk)
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

(defun org-foresight-report--insert (text)
  "Insert TEXT at point and mark it as ours, so a later render can reclaim it."
  (when (and text (not (string-empty-p text)))
    (let ((beg (point)))
      (insert text)
      (put-text-property beg (point) 'org-foresight-report t))))

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
      (let ((inhibit-read-only t)
            (top-p (eq (org-foresight-report--place) 'top))
            (body (org-foresight-report--guarded #'org-foresight-report--body))
            (line (org-foresight-report--guarded
                   #'org-foresight-report-capacity-line)))
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
  "Return the entries of SCAN's first day that sit at a time of their own."
  (seq-filter (lambda (e)
                (and (plist-get e :start)
                     (not (eq (plist-get e :kind) 'travel))))
              (and scan (aref (plist-get scan :ledger) 0))))

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
                       (concat (format-time-string "%H:%M" (car work)) "–"
                               (format-time-string "%H:%M" (cdr work)))
                     "none")))
     (cons "surge"
           (if-let ((n (org-foresight-surge-samples)))
               (format "%s (learned from %d day(s))"
                       (org-duration-from-minutes (org-foresight-surge-minutes))
                       n)
             (format "%s (default, never learned)"
                     (org-duration-from-minutes (org-foresight-surge-minutes)))))
     (cons "estimates"
           (if bias
               (format "×%.2f overall from %d task(s)%s"
                       (or (plist-get bias :overall) 1.0)
                       (or (plist-get bias :samples) 0)
                       (if org-foresight-bias-enabled "" " (APPLYING DISABLED)"))
             "not learned (estimates taken at face value)"))
     (cons "places"
           (format "%d configured · %d travel pair(s) · %d of %d timed entries today"
                   (length org-foresight-places)
                   (length org-foresight-travel-matrix)
                   (length placed) (length timed))))))

(defun org-foresight--diagnose-advice (day)
  "Return what to do next, given DAY's configuration.
Looks past whether options are set to whether they are doing anything: a
setting that is present but never matches is invisible, and silence about it
is what lets a whole feature quietly not run."
  (let* ((scan (ignore-errors (org-foresight-scan 1 day)))
         (timed (org-foresight--diagnose-timed scan))
         (unplaced (seq-filter (lambda (e) (null (plist-get e :place))) timed))
         out)
    (unless (memq 'org-foresight-report-render org-agenda-finalize-hook)
      (push "add `org-foresight-report-render' to `org-agenda-finalize-hook'"
            out))
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
      (push "run `org-foresight-learn-surge' to measure the reserve" out))
    (unless (org-foresight--bias-data)
      (push "run `org-foresight-learn-bias' to see how far estimates run over"
            out))
    (unless (bound-and-true-p org-foresight-task-file)
      (push "`org-foresight-task-file' is unset; generated tasks have nowhere to go"
            out))
    (unless (bound-and-true-p org-foresight-meeting-categories)
      (push "`org-foresight-meeting-categories' is unset; no meeting implies prep"
            out))
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
