;;; org-foresight-observe.el --- ActivityWatch  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yoshzucker

;; Author: yoshzucker
;; URL: https://github.com/yoshzucker/org-foresight

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A read-only client for a local ActivityWatch server, plus the coverage
;; analysis built on it: partitioning the observed day into clocked/unclocked
;; and active/afk, which yields "leak" -- time spent working with no clock
;; running.
;;
;; Leak is what makes surge learning possible: it is the measured size of the
;; work that never made it into the plan.  See `org-foresight-surge' in
;; org-foresight-core.el for how that feeds capacity.
;;
;; This file is separated out because ActivityWatch is an OPTIONAL runtime
;; dependency.  Every entry point here must degrade to nil rather than signal
;; when the server is absent, so the rest of the package keeps working on a
;; machine where ActivityWatch is not installed or not running.

;;; Code:

(require 'org-foresight-core)
(require 'url)
(require 'json)
;; `parse-iso8601-time-string' lives in parse-time, which nothing here pulls in
;; on its own -- it was only ever available because Org happened to have loaded
;; it first.  Require it explicitly so this file stands up alone.
(require 'parse-time)
(require 'seq)
(require 'cl-lib)

;;;; Server access

(defvar org-foresight-observe-url "http://127.0.0.1:5600/api/0"
  "Base URL of the local ActivityWatch REST API.
Use the IPv4 literal, not `localhost': on Windows `localhost' resolves to IPv6
`::1' first while ActivityWatch binds 127.0.0.1 only, so each request pays a
~2s connect-fallback before reaching the server.")
(defvar org-foresight-observe--cache nil
  "Cons (FETCH-TIME . STRING) caching `org-foresight-report-observed'.")
(defvar org-foresight-observe-cache-ttl 120
  "Seconds to reuse `org-foresight-observe--cache' before refetching.")
(defvar org-foresight-observe-clamp-afk t
  "When non-nil, exclude overnight idle from today's afk total.
The day is treated as starting at the first not-afk activity, so `away (afk)'
counts only breaks within the active window, not pre-dawn idle time.")

(defun org-foresight-observe--get-json (path)
  "GET PATH under `org-foresight-observe-url'.
Return the parsed JSON, or nil when the server cannot be reached."
  (condition-case nil
      (let ((buf (url-retrieve-synchronously (concat org-foresight-observe-url path) t t 5)))
        (when buf
          (unwind-protect
              (with-current-buffer buf
                (goto-char (if (bound-and-true-p url-http-end-of-headers)
                               url-http-end-of-headers (point-min)))
                (json-parse-buffer :object-type 'alist :array-type 'list))
            (kill-buffer buf))))
    (error nil)))

(defun org-foresight-observe--find-bucket (buckets prefix)
  "Return the id (string) of the first bucket in BUCKETS whose id starts
with PREFIX.  BUCKETS is the parsed `/buckets/' alist."
  (let ((b (seq-find (lambda (kv) (string-prefix-p prefix (symbol-name (car kv))))
                     buckets)))
    (and b (symbol-name (car b)))))

(defun org-foresight-observe--today-range ()
  "Return (START . END) ISO8601 strings for local midnight..now."
  (let* ((now (current-time))
         (d (decode-time now))
         (mid (encode-time 0 0 0 (nth 3 d) (nth 4 d) (nth 5 d))))
    (cons (format-time-string "%Y-%m-%dT%H:%M:%S%:z" mid)
          (format-time-string "%Y-%m-%dT%H:%M:%S%:z" now))))

(defun org-foresight-observe--events (bucket start end)
  "Fetch BUCKET events between START and END (ISO8601 strings)."
  (org-foresight-observe--get-json
   (format "/buckets/%s/events?start=%s&end=%s&limit=20000"
           bucket (url-hexify-string start) (url-hexify-string end))))

;;;; Events and intervals

(defun org-foresight-observe--parse-ts (s)
  "Parse an ActivityWatch ISO8601 timestamp S into an Emacs time value."
  (parse-iso8601-time-string s))

(defun org-foresight-observe--afk-split (events)
  "Return (ACTIVE-SECONDS . AFK-SECONDS) from afk EVENTS."
  (let ((active 0) (afk 0))
    (dolist (e events)
      (let ((status (alist-get 'status (alist-get 'data e)))
            (dur (or (alist-get 'duration e) 0)))
        (cond ((equal status "not-afk") (setq active (+ active dur)))
              ((equal status "afk") (setq afk (+ afk dur))))))
    (cons active afk)))

(defun org-foresight-observe--afk-after (afk-events day-start)
  "Sum afk-status seconds in AFK-EVENTS occurring at/after DAY-START.
The event straddling DAY-START is counted only for its post-DAY-START part."
  (let ((sum 0.0))
    (dolist (e afk-events sum)
      (when (equal (alist-get 'status (alist-get 'data e)) "afk")
        (let* ((s (org-foresight-observe--parse-ts (alist-get 'timestamp e)))
               (dur (or (alist-get 'duration e) 0))
               (end (time-add s (seconds-to-time dur)))
               (cs (if (time-less-p s day-start) day-start s)))
          (when (time-less-p cs end)
            (setq sum (+ sum (float-time (time-subtract end cs))))))))))

(defun org-foresight-observe--status-intervals (events status)
  "Return sorted (START . END) Emacs-time conses for STATUS periods in EVENTS.
STATUS is \"afk\" or \"not-afk\"."
  (let (ivs)
    (dolist (e events)
      (when (equal (alist-get 'status (alist-get 'data e)) status)
        (let ((s (org-foresight-observe--parse-ts (alist-get 'timestamp e)))
              (dur (or (alist-get 'duration e) 0)))
          (push (cons s (time-add s (seconds-to-time dur))) ivs))))
    (sort ivs (lambda (a b) (time-less-p (car a) (car b))))))

(defun org-foresight-observe--sum-by (events intervals key)
  "Return alist (VALUE . SECONDS) desc: each EVENTS duration clipped to
INTERVALS (not-afk), grouped by KEY (\\='app or \\='project) of its data."
  (let ((h (make-hash-table :test 'equal)) out)
    (dolist (e events)
      (let* ((s (org-foresight-observe--parse-ts (alist-get 'timestamp e)))
             (dur (or (alist-get 'duration e) 0))
             (ov (org-foresight--overlap-seconds
                  s (time-add s (seconds-to-time dur)) intervals)))
        (when (> ov 0)
          (let ((v (or (alist-get key (alist-get 'data e)) "?")))
            (puthash v (+ ov (gethash v h 0)) h)))))
    (maphash (lambda (k v) (push (cons k v) out)) h)
    (seq-sort-by #'cdr #'> out)))

(defun org-foresight-observe--binned (intervals)
  "Return a 48-element vector of active seconds per local half-hour bin."
  (let ((v (make-vector 48 0.0)))
    (dolist (iv intervals v)
      (let ((a (float-time (car iv)))
            (b (float-time (cdr iv))))
        (while (< a b)
          (let* ((dt (decode-time (seconds-to-time a)))
                 (min (nth 1 dt))
                 (bin (+ (* 2 (nth 2 dt)) (if (>= min 30) 1 0)))
                 ;; seconds elapsed into the current half-hour bin
                 (into (+ (* 60 (mod min 30)) (nth 0 dt)))
                 (bin-end (+ a (- 1800 into)))
                 (seg-end (min b bin-end)))
            (aset v bin (+ (aref v bin) (- seg-end a)))
            ;; guard against a zero-length step at an exact boundary
            (setq a (if (> seg-end a) seg-end (+ a 1800)))))))))

;;;; Today

(defun org-foresight-observe--switch-count (window-events)
  "Count app transitions across WINDOW-EVENTS (order-independent)."
  (let ((prev nil) (n 0))
    (dolist (e window-events n)
      (let ((app (alist-get 'app (alist-get 'data e))))
        (when (and prev (not (equal app prev))) (setq n (1+ n)))
        (setq prev app)))))

(defun org-foresight-observe-today ()
  "Return today's parsed ActivityWatch data as a plist, or nil on failure.
Keys: :active :afk SECONDS ; :active-apps :emacs-projects ALIST (NAME . SEC)
of window/emacs time intersected with not-afk ; :binned 48-vector of active
sec per half-hour ; :first :last not-afk boundary times ; :switches count ;
:window-events :afk-events the raw AW events (reused by the coverage view).
Cached for `org-foresight-observe-cache-ttl' seconds, so the coverage metric
\(which uses :active) and the Observed table share one fetch."
  (if (and org-foresight-observe--cache
           (< (float-time (time-subtract (current-time) (car org-foresight-observe--cache)))
              org-foresight-observe-cache-ttl))
      (cdr org-foresight-observe--cache)
    (let ((data
           (condition-case nil
               (let* ((buckets (org-foresight-observe--get-json "/buckets/"))
                      (wb (org-foresight-observe--find-bucket buckets "aw-watcher-window")))
                 (when wb
                   (let* ((ab (org-foresight-observe--find-bucket buckets "aw-watcher-afk"))
                          (eb (org-foresight-observe--find-bucket buckets "aw-watcher-emacs"))
                          (rng (org-foresight-observe--today-range))
                          (win (org-foresight-observe--events wb (car rng) (cdr rng)))
                          (afk-ev (and ab (org-foresight-observe--events ab (car rng) (cdr rng))))
                          (em (and eb (org-foresight-observe--events eb (car rng) (cdr rng))))
                          (split (org-foresight-observe--afk-split afk-ev))
                          (ivs (org-foresight-observe--status-intervals afk-ev "not-afk"))
                          (day-start (and ivs (car (car ivs))))
                          (afk-sec (if (and org-foresight-observe-clamp-afk day-start)
                                       (org-foresight-observe--afk-after afk-ev day-start)
                                     (cdr split))))
                     (list :active (car split) :afk afk-sec
                           :active-apps (org-foresight-observe--sum-by win ivs 'app)
                           :emacs-projects (and em (org-foresight-observe--sum-by
                                                    em ivs 'project))
                           :binned (org-foresight-observe--binned ivs)
                           :first (and ivs (car (car ivs)))
                           :last (and ivs (cdr (car (last ivs))))
                           :switches (org-foresight-observe--switch-count win)
                           ;; Raw events retained so the coverage/leak view
                           ;; reuses this single cached fetch (no extra HTTP).
                           :window-events win :afk-events afk-ev))))
             (error nil))))
      (setq org-foresight-observe--cache (cons (current-time) data))
      data)))

;; --- Coverage (clocked vs leak) -----------------------------------------
;; Partition the active window into 4 disjoint classes by `clocked?' (inside
;; an org CLOCK segment) x `active/afk' (ActivityWatch).  Answers "when I was
;; NOT clocked in but active, which apps ate the time?" -- the leak that is
;; either forgotten-clock work (work-category apps) or distraction.  Folded
;; into `org-foresight-report-observed' below: one sparkline colored by clocked-status
;; instead of a second 4-row timeline, and a "Leak" column on the same
;; per-app table instead of a second leak-only table.

;;;; Coverage and leak

(defun org-foresight-observe-coverage (clock)
  "Return today's clocked/leak coverage as a plist, or nil when AW is down.
Reuses `org-foresight-observe-today' (a single cached fetch) and the
:today-intervals of CLOCK, the plist from `org-foresight-clock-scan'.
Keys:
:active-sec :clocked-sec :leak-sec  totals (seconds)
:ca :cf :ua :uf  48-vectors (active sec per half-hour) for
   clocked-active / clocked-afk / unclocked-active / unclocked-afk
:leak-apps  ALIST (APP . SEC) desc over the unclocked-active window."
  (let ((data (org-foresight-observe-today)))
    (when data
      (let ((first (plist-get data :first))
            (last (plist-get data :last)))
        (when (and first last)
          (let* ((win (plist-get data :window-events))
                 (afk-ev (plist-get data :afk-events))
                 (window (list (cons first last)))
                 (active (org-foresight--intervals-intersect
                          (org-foresight-observe--status-intervals afk-ev "not-afk") window))
                 (afk (org-foresight--intervals-intersect
                       (org-foresight-observe--status-intervals afk-ev "afk") window))
                 (clocked (org-foresight--intervals-intersect
                           (plist-get clock :today-intervals) window))
                 (ca (org-foresight--intervals-intersect clocked active))
                 (cf (org-foresight--intervals-intersect clocked afk))
                 (ua (org-foresight--intervals-subtract active clocked))
                 (uf (org-foresight--intervals-subtract afk clocked))
                 (leak-apps (org-foresight-observe--sum-by win ua 'app))
                 (off-pc (org-foresight--intervals-seconds uf)))
            (list :active-sec (org-foresight--intervals-seconds active)
                  :clocked-sec (org-foresight--intervals-seconds clocked)
                  :leak-sec (+ (org-foresight--intervals-seconds ua) off-pc)
                  :ca (org-foresight-observe--binned ca)
                  :cf (org-foresight-observe--binned cf)
                  :ua (org-foresight-observe--binned ua)
                  :uf (org-foresight-observe--binned uf)
                  :leak-apps leak-apps)))))))

;;;; Learning the surge reserve
;; Interruptions cannot be scheduled, but their volume can be measured.  Time
;; spent active at the machine with no clock running is work that happened and
;; was never planned; taking the median of that over recent working days gives
;; a reserve to hold back, so a day that gets interrupted still ends on time.

(defcustom org-foresight-surge-window 20
  "How many days back `org-foresight-learn-surge' looks.
Only working days within the window contribute a sample."
  :type 'integer
  :group 'org-foresight)

(defun org-foresight-observe--day-range (offset)
  "Return (START . END) ISO8601 strings covering the day OFFSET days back."
  (let* ((start (org-foresight--day-start offset))
         (end (time-add start (days-to-time 1))))
    (cons (format-time-string "%Y-%m-%dT%H:%M:%S%:z" start)
          (format-time-string "%Y-%m-%dT%H:%M:%S%:z" end))))

(defun org-foresight-observe-day-leak (offset clocked)
  "Return minutes of unplanned work on the day OFFSET days back, or nil.

Unplanned work is time ActivityWatch saw as active while CLOCKED (that day's
org clock intervals) was not running.  Only `work' and `comms' applications
count: time lost to `distraction' is not capacity a plan could have reclaimed,
so counting it would inflate the reserve and shrink every future day for no
reason.

Returns nil when ActivityWatch has nothing for that day, which a caller must
treat as \"no sample\" rather than as zero -- a day the server was simply not
running is not evidence of a quiet day."
  (let* ((rng (org-foresight-observe--day-range offset))
         (buckets (org-foresight-observe--get-json "/buckets/"))
         (wb (org-foresight-observe--find-bucket buckets "aw-watcher-window"))
         (ab (org-foresight-observe--find-bucket buckets "aw-watcher-afk")))
    (when (and wb ab)
      (let* ((win (org-foresight-observe--events wb (car rng) (cdr rng)))
             (afk-ev (org-foresight-observe--events ab (car rng) (cdr rng)))
             (active (org-foresight-observe--status-intervals afk-ev "not-afk")))
        (when active
          (let* ((unclocked (org-foresight--intervals-subtract active clocked))
                 (by-app (org-foresight-observe--sum-by win unclocked 'app))
                 (sec 0.0))
            (dolist (kv by-app)
              (unless (equal (org-foresight--app-category (car kv)) "distraction")
                (setq sec (+ sec (cdr kv)))))
            (/ sec 60.0)))))))

;;;###autoload
(defun org-foresight-learn-surge (&optional days)
  "Learn the surge reserve from the last DAYS days and cache the result.

Deliberately a command rather than something the agenda does on its own: it
makes one HTTP request per day examined, which is far too slow to sit on
`org-agenda-finalize-hook'.  The agenda only ever reads the cache this writes.

The reserve conflates genuine interruptions with forgotten clock-ins.  Both
consume the day, so both belong in a figure meant to answer \"how much can I
still promise\" -- but it does mean better clocking discipline will shrink the
reserve over time, which is the correct direction."
  (interactive)
  (let* ((days (or days org-foresight-surge-window))
         (clock (org-foresight-clock-scan days))
         (ivs (plist-get clock :intervals-byday))
         samples)
    (dotimes (i days)
      ;; index 0 of the clock scan is the oldest day in the window
      (let* ((offset (- days 1 i))
             (day (org-foresight--day-start offset)))
        (when (memq (nth 6 (decode-time day)) org-foresight-workdays)
          (when-let ((leak (org-foresight-observe-day-leak offset (aref ivs i))))
            (push leak samples)))))
    (if (null samples)
        (user-error "No ActivityWatch history to learn from (is it running?)")
      (let ((median (org-foresight--median samples)))
        (with-temp-file org-foresight-surge-cache-file
          (prin1 (list :minutes median
                       :samples (length samples)
                       :updated (format-time-string "%Y-%m-%d"))
                 (current-buffer)))
        (message "Surge reserve: %s from %d working day(s)"
                 (org-duration-from-minutes median) (length samples))
        median))))

(provide 'org-foresight-observe)

;;; org-foresight-observe.el ends here
