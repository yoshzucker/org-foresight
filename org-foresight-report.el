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

(defface org-foresight-report-booked '((t :inherit org-agenda-structure))
  "Bar segment for meetings, travel and work already placed at a time.")
(defface org-foresight-report-promised '((t :inherit org-scheduled))
  "Bar segment for effort accepted but not yet placed.")
(defface org-foresight-report-surge '((t :inherit shadow))
  "Bar segment for the reserve held back for interruptions.")
(defface org-foresight-report-spare '((t :inherit success))
  "Bar segment for time that may still be promised.")
(defface org-foresight-report-grey '((t :inherit font-lock-comment-face))
  "Bar for waking hours that are neither work nor a private commitment.")

(defcustom org-foresight-bar-width 40
  "Width in columns of the capacity bar."
  :type 'integer
  :group 'org-foresight)

(defvar org-foresight-report--bar-segments
  '((:key :booked-min   :char ?█ :face org-foresight-report-booked   :label "booked")
    (:key :committed-min :char ?▓ :face org-foresight-report-promised :label "promised")
    (:key :surge-min    :char ?▒ :face org-foresight-report-surge    :label "surge")
    (:key :headroom-min :char ?░ :face org-foresight-report-spare    :label "spare"))
  "The bar's segments in order, each a plist of plist-key, glyph, face, label.")

(defun org-foresight-report--bar (cap)
  "Return a stacked bar showing how CAP's day divides up, or nil.

The whole bar is the work span, so meetings and journeys appear in it rather
than being silently deducted first: the question \"where did the day go\" is
answered by the same picture as \"what is left\".

When more has been promised than the span holds, the bar is scaled to the
promises and a mark is placed where the span actually ends.  The overflow is
shown rather than clipped -- a day that does not fit is exactly the day worth
seeing."
  (let* ((span (plist-get cap :span-min))
         (parts (mapcar (lambda (seg)
                          (max 0.0 (or (plist-get cap (plist-get seg :key)) 0.0)))
                        org-foresight-report--bar-segments))
         (total (apply #'+ parts))
         (scale (max span total)))
    (when (> scale 0)
      (let* ((width org-foresight-bar-width)
             (cells (mapcar (lambda (m) (round (* width (/ m scale)))) parts))
             (bar "")
             (over (> total span)))
        (cl-loop for seg in org-foresight-report--bar-segments
                 for n in cells
                 do (setq bar (concat bar (propertize
                                           (make-string (max 0 n)
                                                        (plist-get seg :char))
                                           'face (plist-get seg :face)))))
        ;; Rounding must not change the bar's length, or columns drift.
        (setq bar (truncate-string-to-width bar width 0 ?\s))
        (when over
          (let ((mark (min (1- width) (round (* width (/ span scale))))))
            (setq bar (concat (substring bar 0 mark)
                              (propertize "┃" 'face
                                          'org-foresight-report-overcommitted)
                              (substring bar (1+ mark))))))
        bar))))

(defun org-foresight-report--bar-key (cap)
  "Return the legend naming each of the bar's segments in CAP, with its size.
The legend carries the breakdown so the verdict line above need only carry
the conclusion; stating both twice would cost a line and say nothing more."
  (mapconcat (lambda (seg)
               (concat (propertize (char-to-string (plist-get seg :char))
                                   'face (plist-get seg :face))
                       " " (plist-get seg :label) " "
                       (org-duration-from-minutes
                        (max 0.0 (or (plist-get cap (plist-get seg :key)) 0.0)))))
             org-foresight-report--bar-segments "  "))

(defun org-foresight-report--verdict (cap)
  "Return the one-line answer for capacity plist CAP.
Everything the block exists to say, in the width of a single line: what is
left, what has been promised away, and the hour the day actually ends."
  (let ((headroom (plist-get cap :headroom-min))
        (finish (plist-get cap :finish))
        (samples (org-foresight-surge-samples)))
    (concat
     (format "Free %s of %s"
             (org-duration-from-minutes (plist-get cap :free-min))
             (org-duration-from-minutes (plist-get cap :span-min)))
     (if (>= headroom 0)
         (format " · spare %s" (org-duration-from-minutes headroom))
       (propertize
        (format " · OVER by %s" (org-duration-from-minutes (- headroom)))
        'face 'org-foresight-report-overcommitted))
     (if finish
         (format " · ends %s" (format-time-string "%H:%M" finish))
       (propertize " · will not fit" 'face 'org-foresight-report-overcommitted))
     (format " · surge %s%s"
             (org-duration-from-minutes (plist-get cap :surge-min))
             (if samples (format " (n=%d)" samples) ""))
     ;; Shown whenever it is doing anything, so a day that has shrunk reads as
     ;; "my estimates are optimistic" rather than "the tool is being harsh".
     (let ((factor (org-foresight-bias-factor nil)))
       (if (> (abs (- factor 1.0)) 0.1)
           (format " · est ×%.1f" factor)
         "")))))

(defun org-foresight-report--grey-line (cap)
  "Return the line describing unclaimed private time in CAP, or nil.

Kept apart from the work bar deliberately.  Unclaimed evenings are not
capacity waiting to be spent: the emptiness is what makes room for anything
new, and a day that quietly borrows from it should have to say so."
  (let ((grey (or (plist-get cap :grey-min) 0))
        (borrowed (or (plist-get cap :borrowed-min) 0)))
    (when (> (+ grey borrowed) 0)
      (concat
       (format "Grey %s" (org-duration-from-minutes grey))
       (when (> borrowed 0)
         (propertize (format " · borrowed %s"
                             (org-duration-from-minutes borrowed))
                     'face 'org-foresight-report-overcommitted))))))

(defun org-foresight-report-capacity-line (&optional day scan now)
  "Return DAY's capacity verdict as one line, or nil on a non-working day.
This is the line that goes at the very top of the agenda: a number you should
not have to go looking for is a number you will not look at."
  (let* ((day (or day (org-foresight--day-start 0)))
         (cap (org-foresight-capacity day scan now)))
    (when (plist-get cap :window)
      (concat (org-foresight-report--verdict cap)
              (when-let ((bar (org-foresight-report--bar cap)))
                (concat "\n" (org-foresight-report--bar-key cap) "\n" bar))
              (when-let ((grey (org-foresight-report--grey-line cap)))
                (concat "\n" grey))
              (org-foresight-report--verdict-extras)))))

(defun org-foresight-report--actionable (string marker)
  "Return STRING carrying the text properties Org's agenda commands look for.

Rather than inventing a keymap, a row says what it is in the vocabulary the
agenda already speaks: `org-marker' is what `org-agenda-schedule' reads,
`org-hd-marker' what `org-agenda-set-effort' reads, and `org-agenda-type'
what `org-agenda-check-type' gates every one of them on.  With all three in
place \\`s', \\`e', \\`t', \\`RET' and the rest work on a foresight row exactly
as they do on an agenda line -- which is the difference between a board that
reports a problem and one you can fix it from.

A nil MARKER leaves the row inert.  That matters: a finding that is a total
rather than an entry has nothing to act on, and inheriting the neighbouring
row's marker would quietly reschedule the wrong thing."
  (if (not (markerp marker))
      string
    (propertize string
                'org-marker marker
                'org-hd-marker marker
                'org-agenda-type 'agenda
                'help-echo "s schedule · e effort · t state · RET visit")))

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

(defcustom org-foresight-grid-gutter-width 14
  "Widest the grid's gutter may grow, in columns.
A long empty evening would otherwise push the titles off the screen."
  :type 'integer
  :group 'org-foresight)

(defvar org-foresight-report--grid-glyphs
  '((meeting   . (?█ . org-foresight-report-booked))
    (task      . (?█ . org-foresight-report-booked))
    (travel    . (?▓ . org-foresight-report-promised))
    (private   . (?░ . org-foresight-report-grey))
    (available . (?· . org-foresight-report-spare))
    (grey      . (?░ . org-foresight-report-grey)))
  "Alist of band kind to (GLYPH . FACE) for the grid's gutter.")

(defun org-foresight-report--grid-gutter (kind minutes)
  "Return the gutter for a band of KIND lasting MINUTES."
  (let* ((cell (or (cdr (assq kind org-foresight-report--grid-glyphs))
                   (cons ?\s 'default)))
         (n (max 1 (min org-foresight-grid-gutter-width
                        (round (/ minutes
                                  (float org-foresight-grid-minutes-per-column)))))))
    (truncate-string-to-width
     (propertize (make-string n (car cell)) 'face (cdr cell))
     org-foresight-grid-gutter-width 0 ?\s)))

(defun org-foresight-report--grid-row (time gutter effort category title marker)
  "Return one grid row, actionable when MARKER is a marker.

Columns are time, gutter, effort, category, title.  The kind of block is not
among them on purpose: the gutter already says it in glyph and colour, and a
column repeating that would cost width the title needs.  CATEGORY is the one
thing about a block that nothing else on the row conveys."
  (org-foresight-report--actionable
   (format " %5s %s %5s %s %s"
           (or time "")
           gutter
           (or effort "")
           (truncate-string-to-width (or category "") 10 0 ?\s)
           (truncate-string-to-width
            (replace-regexp-in-string "[\n\r]" " " (or title "")) 32 0 ?\s))
   marker))

(defun org-foresight-report--grid-category (b)
  "Return the category column for band or ledger entry B.
Falls back to naming the kind for a derived block, which has no category of
its own but is not nothing either."
  (or (plist-get b :category)
      (pcase (plist-get b :kind)
        ('travel "travel")
        (_ ""))))

(defun org-foresight-report--grid-band (b)
  "Return the grid row for band B, or nil when it is not worth a line."
  (let* ((kind (plist-get b :kind))
         (mins (/ (float-time (time-subtract (plist-get b :end)
                                             (plist-get b :start)))
                  60.0)))
    ;; Empty private time is not news -- the grey total under the bar already
    ;; says how much of it there is, and a row per stretch of nothing pushes
    ;; the day itself off the screen.
    (unless (eq kind 'grey)
      (org-foresight-report--grid-row
       (format-time-string "%H:%M" (plist-get b :start))
       (org-foresight-report--grid-gutter kind mins)
       (org-duration-from-minutes mins)
       (if (eq kind 'available) "" (org-foresight-report--grid-category b))
       (if (eq kind 'available) "free" (or (plist-get b :title) "?"))
       (plist-get b :marker)))))

(defun org-foresight-report--grid-alongside (e)
  "Return the row for ledger entry E, shown beneath whatever it shares with."
  (org-foresight-report--grid-row
   nil
   (truncate-string-to-width
    (propertize "╰" 'face 'org-foresight-report-surge)
    org-foresight-grid-gutter-width 0 ?\s)
   (org-duration-from-minutes (plist-get e :effort))
   (org-foresight-report--grid-category e)
   (concat (or (plist-get e :title) "?") " (listen)")
   (plist-get e :marker)))

(defun org-foresight-report--grid-boundary (time label)
  "Return the rule drawn where the working day opens or closes."
  (org-foresight-report--grid-row
   (format-time-string "%H:%M" time)
   (truncate-string-to-width
    (propertize "─────" 'face 'org-agenda-structure)
    org-foresight-grid-gutter-width 0 ?\s)
   "" "" label nil))

(defun org-foresight-report--grid-eclipsed (ledger bands)
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
          (format-time-string "%H:%M" (plist-get e :start))
          (truncate-string-to-width
           (propertize "✗" 'face 'org-foresight-report-overcommitted)
           org-foresight-grid-gutter-width 0 ?\s)
          (org-duration-from-minutes (plist-get e :effort))
          (org-foresight-report--grid-category e)
          (or (plist-get e :title) "?")
          (plist-get e :marker))))
     ledger)))

(defun org-foresight-report--grid-context (ledger)
  "Return rows for LEDGER entries that belong to somebody else.

They take none of the day, so they are not part of it -- but a child's
fixture is exactly the sort of thing that decides when the house is empty,
and a plan made without knowing it is a plan made blind."
  (seq-keep
   (lambda (e)
     (when (and (plist-get e :start)
                (eq (plist-get e :attention) 'informational))
       (org-foresight-report--grid-row
        (format-time-string "%H:%M" (plist-get e :start))
        (truncate-string-to-width
         (propertize "┈" 'face 'org-foresight-report-grey)
         org-foresight-grid-gutter-width 0 ?\s)
        (org-duration-from-minutes (plist-get e :effort))
        (org-foresight-report--grid-category e)
        (or (plist-get e :title) "?")
        (plist-get e :marker))))
   ledger))

(defun org-foresight-report--grid-unplaced (ledger)
  "Return rows for LEDGER entries that have been promised but given no time."
  (seq-keep
   (lambda (e)
     (when (eq (plist-get e :kind) 'promised)
       (let ((est (plist-get e :effort))
             (adj (plist-get e :effort-adj)))
         (org-foresight-report--grid-row
          nil
          (truncate-string-to-width
           (propertize "?" 'face 'org-foresight-report-promised)
           org-foresight-grid-gutter-width 0 ?\s)
          (org-duration-from-minutes (or adj est))
          (org-foresight-report--grid-category e)
          (concat (or (plist-get e :title) "?")
                  (when (and adj (> (abs (- adj est)) 1))
                    (format " (est %s)" (org-duration-from-minutes est))))
          (plist-get e :marker)))))
   ledger))

(defun org-foresight-report--grid-due (day)
  "Return rows for work due on DAY that occupies none of it.
A deadline takes no time and so appears nowhere in the bands, which is
exactly how a day gets planned without it."
  (let (out)
    (dolist (file (org-agenda-files))
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (org-with-wide-buffer
           (org-map-entries
            (lambda ()
              (let ((dead (org-get-deadline-time (point))))
                (when (and dead
                           (not (org-entry-is-done-p))
                           (= (org-foresight--day-of dead day) 0))
                  (push (org-foresight-report--grid-row
                         nil
                         (truncate-string-to-width
                          (propertize "!" 'face
                                      'org-foresight-report-overcommitted)
                          org-foresight-grid-gutter-width 0 ?\s)
                         "due"
                         (or (org-entry-get (point) "CATEGORY" t) "")
                         (org-get-heading t t t t)
                         (point-marker))
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
            (when rows
              (push (org-foresight-report--grid-boundary (car work) "work starts")
                    rows)))
          (unless (or closed (time-less-p (plist-get b :start) (cdr work)))
            (setq closed t opened t)
            (push (org-foresight-report--grid-boundary (cdr work) "work ends")
                  rows))
          (when-let ((row (org-foresight-report--grid-band b)))
            (push row rows)
            ;; Anything that was happening at the same time and did not need
            ;; all of you hangs below the band, so the clock still reads as
            ;; one line down the page.
            (dolist (a (org-foresight-report--grid-sharers ledger b))
              (push a rows))))
        (unless closed
          (push (org-foresight-report--grid-boundary (cdr work) "work ends")
                rows)))
      (let* ((eclipsed (org-foresight-report--grid-eclipsed ledger bands))
             (unplaced (org-foresight-report--grid-unplaced ledger))
             (due (org-foresight-report--grid-due day))
             (extra (append eclipsed unplaced due))
             (context (org-foresight-report--grid-context ledger))
             ;; A key, rather than a word appended to each row: the suffix was
             ;; the first thing a long title truncated away, so exactly the
             ;; rows that most needed explaining lost the explanation.
             (legend (string-join
                      (delq nil
                            (list (and unplaced "? not yet placed")
                                  (and due "! due today")
                                  (and eclipsed "✗ double-booked")))
                      "   ")))
        (mapconcat
         #'identity
         (append (nreverse rows)
                 (and extra (list "" (propertize (concat "       " legend)
                                                 'face 'shadow)))
                 extra
                 (and context
                      (list "" (org-foresight-report--badge
                                "Alongside" "happening, but not yours")))
                 context)
         "\n")))))

(defun org-foresight-report--grid-sharers (ledger band)
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
         (org-foresight-report--grid-alongside e)))
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

(dolist (cmd '(org-agenda-schedule
               org-agenda-deadline
               org-agenda-todo
               org-agenda-set-effort
               org-agenda-set-tags
               org-agenda-priority))
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
