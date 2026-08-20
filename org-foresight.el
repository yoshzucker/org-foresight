;;; org-foresight.el --- Plan ahead in Org  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yoshzucker

;; Author: yoshzucker
;; URL: https://github.com/yoshzucker/org-foresight
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; Keywords: outlines, calendar, convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Org records the past well and the future badly.  Clock reports, logbooks and
;; agenda logs all answer "what happened"; almost nothing answers "what can
;; still be promised".  org-foresight adds that missing axis.
;;
;; It answers three questions:
;;
;;   How much can I still take on today?
;;     available time - what is already committed - a reserve for work that has
;;     not arrived yet
;;
;;   When will I be done?
;;     the remaining commitment poured into the gaps between meetings, which
;;     gives an actual finish time rather than a hope
;;
;;   When *could* I take this on?
;;     the same calculation over the next fortnight, which is what turns "I'm
;;     busy" into a date
;;
;; The reserve is the interesting part.  Interruptions are not controllable, but
;; their volume is remarkably stable, and it is measurable: time spent active at
;; the machine with no clock running is work that was done but never planned.
;; org-foresight learns the size of that from history and subtracts it up front,
;; so a day that gets interrupted still ends where it was supposed to.
;;
;; Layout:
;;
;;   org-foresight-core.el     the model -- intervals, clock, capacity
;;   org-foresight-observe.el  ActivityWatch (optional at runtime)
;;   org-foresight-report.el   rendering into agenda buffers
;;   org-foresight-agenda.el   derived rows inside Org's own agenda
;;   org-foresight-plan.el     signals and placement (the only writer)
;;
;; This file is the aggregator: requiring `org-foresight' gets the whole
;; package, and the user-facing commands are gathered here.

;;; Code:

(require 'org-foresight-core)
(require 'org-foresight-observe)
(require 'org-foresight-report)
(require 'org-foresight-agenda)
(require 'org-foresight-plan)
(require 'org-foresight-profile)

;;;; Entry points

(provide 'org-foresight)

;;; org-foresight.el ends here
