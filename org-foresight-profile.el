;;; org-foresight-profile.el --- What a redraw costs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yoshzucker

;; Author: yoshzucker
;; URL: https://github.com/yoshzucker/org-foresight

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A redraw that takes four seconds on one machine and a fifth of a second on
;; another cannot be argued about from the fast machine.  This writes down
;; where the time went on the slow one, in a form that can be sent to somebody
;; who is not allowed to see the calendar it was measured on.
;;
;; Confidentiality is a property of the code here, not of the reader
;; remembering to strip fields.  Every line of the report is assembled from
;; numbers -- durations, counts, sizes -- and the functions that assemble it
;; are never handed a heading, a file name, a category or a place.  There is
;; no code path by which any of those could reach the file, in the same way
;; that `org-calsync-busy--vevent' cannot leak a meeting subject.
;;
;; What that costs is precision: the report can say "the survey took 0.6s" but
;; not which file was slow.  That trade is the right way round.  A report
;; nobody is allowed to send is worth nothing at all.

;;; Code:

(require 'org-foresight-core)
(require 'org-foresight-report)
(require 'org-agenda)

(defcustom org-foresight-profile-file
  (locate-user-emacs-file "org-foresight-profile.txt")
  "Where `org-foresight-profile' writes its report."
  :type 'file
  :group 'org-foresight)

(defcustom org-foresight-profile-runs 5
  "How many redraws `org-foresight-profile' times.

More than one because the interesting number is the spread.  A first redraw
that pays for something the rest do not -- the watcher, a file the editor had
not visited, a collection that happened to fall there -- looks like an
unreliable machine until the runs are put side by side."
  :type 'integer
  :group 'org-foresight)

(defconst org-foresight-profile--phases
  '((scan    org-foresight-scan           "survey of the agenda files")
    (clock   org-foresight-clock-scan      "clock history")
    (project org-foresight-project-scan    "survey of the project trees")
    (watcher org-foresight-observe--get-json "requests to ActivityWatch")
    (spine   org-foresight-agenda--draw-spine "drawing the brackets"))
  "The parts of a redraw that are timed, and what each is.

Chosen so that no one of them runs inside another: their total may honestly be
taken from the whole and the remainder called everything else.  The two that
do contain the others -- building the rows and rendering the report -- are
timed as well but reported apart, since adding them in would count the same
seconds twice.")

(defconst org-foresight-profile--counted
  '((opens find-file-noselect "times a file was opened or checked"))
  "Parts that are counted rather than timed.

Opening a file that is already visited costs almost nothing on a local disk
and is not free at all on a synchronised one, where it verifies the copy
against a server.  The count is the same everywhere; what it costs is not, so
the count is what is worth carrying.")

(defconst org-foresight-profile--inclusive
  '((rows   org-foresight-agenda--augment  "building the agenda's rows")
    (report org-foresight-report-render     "rendering the report blocks"))
  "Parts that contain other parts, timed separately so nothing is counted twice.")

(defvar org-foresight-profile--tally nil
  "Alist of (KEY SECONDS . CALLS) while a run is being timed.")

(defun org-foresight-profile--note (key seconds)
  "Add SECONDS and one call to KEY's entry in the tally."
  (let ((cell (or (assq key org-foresight-profile--tally)
                  (car (push (list key 0.0 0) org-foresight-profile--tally)))))
    (setf (nth 1 cell) (+ (nth 1 cell) seconds))
    (setf (nth 2 cell) (1+ (nth 2 cell)))))

(defun org-foresight-profile--timer (key)
  "Return an :around advice that adds its own duration to KEY."
  (lambda (fn &rest args)
    (let ((start (float-time)))
      (unwind-protect (apply fn args)
        (org-foresight-profile--note key (- (float-time) start))))))

(defun org-foresight-profile--measure (thunk)
  "Call THUNK with the timers installed, and return what they saw.

The advice is removed however THUNK ends.  A profiler that survives its own
run would go on charging every later redraw for the privilege of having been
asked once."
  (let ((org-foresight-profile--tally nil)
        (installed nil))
    (unwind-protect
        (let ((gcs gcs-done) (gc gc-elapsed) (start (float-time)))
          (pcase-dolist (`(,key ,fn ,_)
                         (append org-foresight-profile--phases
                                 org-foresight-profile--counted
                                 org-foresight-profile--inclusive))
            (when (fboundp fn)
              (let ((advice (org-foresight-profile--timer key)))
                (push (cons fn advice) installed)
                (advice-add fn :around advice))))
            (funcall thunk)
            (org-foresight-profile--note 'total (- (float-time) start))
            (org-foresight-profile--note 'gc (- gc-elapsed gc))
            (setf (nth 2 (assq 'gc org-foresight-profile--tally))
                  (- gcs-done gcs))
            org-foresight-profile--tally)
      (pcase-dolist (`(,fn . ,advice) installed) (advice-remove fn advice)))))

(defun org-foresight-profile--get (run key)
  "Return (SECONDS . CALLS) for KEY in RUN, or (0 . 0)."
  (if-let ((cell (assq key run)))
      (cons (nth 1 cell) (nth 2 cell))
    (cons 0.0 0)))

(defun org-foresight-profile--median (numbers)
  "Return the median of NUMBERS, or 0."
  (if (null numbers)
      0.0
    (let ((sorted (sort (copy-sequence numbers) #'<)))
      (nth (/ (length sorted) 2) sorted))))

(defun org-foresight-profile--row (label runs key)
  "Return one report row: LABEL, KEY's seconds in each of RUNS, and the median."
  (let ((times (mapcar (lambda (r) (car (org-foresight-profile--get r key))) runs))
        (calls (mapcar (lambda (r) (cdr (org-foresight-profile--get r key))) runs)))
    (format "  %-14s%s  %6.2f   %s\n" label
            (mapconcat (lambda (s) (format "%7.2f" s)) times "")
            (org-foresight-profile--median times)
            (if (apply #'= calls)
                (format "%d" (car calls))
                (mapconcat #'number-to-string calls "/")))))

(defun org-foresight-profile--corpus ()
  "Return how much there is to read, as counts and sizes and nothing else.

Never a name.  How many files there are and how big they are is what decides
what a survey costs; which files they are is the reader's business and no part
of the answer."
  (let ((files 0) (bytes 0) (headings 0))
    (dolist (file (org-agenda-files))
      (when-let ((buf (get-file-buffer file)))
        (setq files (1+ files))
        (with-current-buffer buf
          (setq bytes (+ bytes (buffer-size)))
          (org-with-wide-buffer
           (goto-char (point-min))
           (setq headings (+ headings (count-matches org-outline-regexp-bol)))))))
    (list files bytes headings)))

(defun org-foresight-profile--environment ()
  "Return the report's opening section: the machine, and what it was asked."
  (pcase-let ((`(,files ,bytes ,headings) (org-foresight-profile--corpus)))
    (concat
     (format "  emacs           %s (%s)\n" emacs-version system-type)
     (format "  org             %s\n" (org-version))
     ;; Two facts, not one.  A build made with native compilation still
     ;; reports it unavailable when libgccjit cannot be loaded, and the two
     ;; cases want opposite answers: one needs a library, the other needs a
     ;; different Emacs.  Reporting only the second sends a reader after the
     ;; wrong one.
     (format "  native-comp     built in %s, usable %s\n"
             (if (featurep 'native-compile) "yes" "no")
             (if (and (fboundp 'native-comp-available-p)
                      (native-comp-available-p))
                 "yes" "no"))
     (format "  gc-cons-threshold %d\n" gc-cons-threshold)
     "\n"
     (format "  agenda files    %d (of %d listed, visited)\n"
             files (length (org-agenda-files)))
     (format "  headings        %d\n" headings)
     (format "  bytes           %d\n" bytes)
     (format "  agenda span     %s\n" org-agenda-span)
     (format "  horizon days    %d\n" org-foresight-horizon-days)
     (format "  work intervals  %d\n" (length org-foresight-work))
     (format "  places named    %d\n" (length org-foresight-places))
     (format "  watcher ttl     %ds\n"
             (if (boundp 'org-foresight-observe-cache-ttl)
                 org-foresight-observe-cache-ttl 0))
     (format "  report style    %s\n" org-foresight-report-style))))

(defun org-foresight-profile--render (runs)
  "Return the whole report for RUNS, a list of tallies."
  (let ((n (length runs)))
    (concat
     "org-foresight profile\n"
     "=====================\n\n"
     "Numbers only.  No file name, heading, category, place or tag appears\n"
     "anywhere below, and none is read by the code that wrote it.  Read it\n"
     "through before sending it on all the same.\n\n"
     (format "written %s\n\n" (format-time-string "%Y-%m-%d %H:%M"))
     "Environment\n"
     (org-foresight-profile--environment)
     "\n"
     (format "Redraw, %d runs, seconds\n" n)
     (format "  %-14s%s  %6s   %s\n" "" 
             (mapconcat (lambda (i) (format "%7d" (1+ i)))
                        (number-sequence 0 (1- n)) "")
             "median" "calls")
     (org-foresight-profile--row "total" runs 'total)
     (mapconcat (lambda (phase)
                  (org-foresight-profile--row
                   (format "  %s" (car phase)) runs (car phase)))
                org-foresight-profile--phases "")
     (org-foresight-profile--row "  elsewhere" runs 'elsewhere)
     "\n"
     (mapconcat (lambda (phase)
                  (org-foresight-profile--row
                   (format "%s" (car phase)) runs (car phase)))
                org-foresight-profile--counted "")
     (org-foresight-profile--row "gc" runs 'gc)
     "\n"
     "Inclusive -- these contain the parts above, so they are not added in\n"
     (mapconcat (lambda (phase)
                  (org-foresight-profile--row
                   (format "  %s" (car phase)) runs (car phase)))
                org-foresight-profile--inclusive "")
     "\n"
     "Key\n"
     (mapconcat (lambda (phase)
                  (format "  %-14s%s\n" (car phase) (nth 2 phase)))
                (append org-foresight-profile--phases
                        org-foresight-profile--counted
                        org-foresight-profile--inclusive)
                "")
     "  elsewhere     everything else a redraw does, Org's own work included\n"
     "                for `opens' read the call count; its seconds mean little\n"
     "  gc            garbage collection; the call count is collections\n")))

(defun org-foresight-profile--elsewhere (run)
  "Add to RUN the time that was not in any of the timed phases."
  (let ((counted (apply #'+ (mapcar (lambda (p)
                                      (car (org-foresight-profile--get run (car p))))
                                    org-foresight-profile--phases))))
    (cons (list 'elsewhere
                (max 0.0 (- (car (org-foresight-profile--get run 'total)) counted))
                1)
          run)))

;;;###autoload
(defun org-foresight-profile (&optional runs)
  "Time what redrawing this agenda costs, and write a report free of content.

Run it from the agenda that is slow, with the buffer in the state it is
usually in.  It redraws `org-foresight-profile-runs' times -- RUNS with a
prefix argument -- and writes `org-foresight-profile-file', then shows it.

Read the file before sending it anywhere.  It is built to contain nothing but
numbers, and looking is how you find out that it does."
  (interactive "P")
  (unless (derived-mode-p 'org-agenda-mode)
    (user-error "Run this from the agenda whose redraw is slow"))
  (let* ((runs (or (and (numberp runs) runs)
                   (and (consp runs) (car runs))
                   org-foresight-profile-runs))
         (tallies
          (mapcar (lambda (_)
                    (org-foresight-profile--elsewhere
                     (org-foresight-profile--measure
                      (lambda () (org-agenda-redo)))))
                  (number-sequence 1 runs)))
         (report (org-foresight-profile--render tallies)))
    (with-temp-file org-foresight-profile-file (insert report))
    (with-current-buffer (find-file-noselect org-foresight-profile-file t)
      (revert-buffer t t)
      (display-buffer (current-buffer)))
    (message "Wrote %s -- read it before sending it on"
             org-foresight-profile-file)))

(provide 'org-foresight-profile)

;;; org-foresight-profile.el ends here
