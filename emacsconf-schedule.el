;;; emacsconf-schedule.el --- Scheduling support     -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Sacha Chua

;; Author: Sacha Chua <sacha@sachachua.com>
;; Keywords: calendar

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(defvar emacsconf-schedule-strategies nil "List of scheduling functions.
Each function should take the info and manipulate it as needed, returning the new info.")

(defvar emacsconf-schedule-max-time 30)
(defun emacsconf-schedule-allocate-at-most (info)
  "Allocate at most `emacsconf-schedule-max-time' to the talks."
  (mapcar (lambda (o)
            (when (plist-get o :max-time)
              (plist-put o :time
                         (number-to-string
                          (min
                           (string-to-number (plist-get o :max-time))
                           emacsconf-max-time))))
            o)
          info))


(defvar emacsconf-schedule-break-time 10 "Number of minutes for break.")
(defvar emacsconf-schedule-lunch-time 45 "Number of minutes for lunch.")

(defun emacsconf-schedule-override-breaks (info)
  (mapcar (lambda (o)
            (when (string-match "BREAK" (plist-get o :title))
              (plist-put o :time (number-to-string emacsconf-schedule-break-time)))
            (when (string-match "LUNCH" (plist-get o :title))
              (plist-put o :time (number-to-string emacsconf-schedule-lunch-time)))
            o)
          info))

(defun emacsconf-schedule-prepare (&optional info)
  (emacsconf-schedule-based-on-info
   (seq-reduce (lambda (prev val) (funcall val prev))
               emacsconf-schedule-strategies
               (or info (emacsconf-get-talk-info)))))

(defun emacsconf-schedule-summarize-breaks (&optional list)
  (setq list (or list (emacsconf-schedule-summarize)))
  (let ((list (or list (emacsconf-schedule-summarize)))
        (title-field 5))
    (append
     (cdr (seq-filter (lambda (o) (string-match "BREAK" (elt o title-field)))
                      list))
     '(hline)
     (cdr (seq-filter (lambda (o) (string-match "LUNCH" (elt o title-field)))
                 list)))))

(defun emacsconf-schedule-strategy-pack-everything-in-just-as-confirmed (&optional info)
  (let* ((emacsconf-schedule-break-time 10)
         (emacsconf-schedule-lunch-time 30)
         (emacsconf-schedule-max-time 30)
         (emacsconf-schedule-default-buffer-minutes 5)
         (emacsconf-schedule-default-buffer-minutes-for-live-q-and-a 10)
         (emacsconf-schedule-tweaked-allocations '(("indieweb" . 20)
                                                   ("maint" . 20)
                                                   ("workflows" . 20)))
         (emacsconf-schedule-strategies '(emacsconf-schedule-allocate-buffer-time
                                          emacsconf-schedule-override-breaks
                                          emacsconf-schedule-allocate-buffer-time-at-most-max-time
                                          emacsconf-schedule-allocate-max-time
                                          emacsconf-schedule-allocate-at-most
                                          emacsconf-schedule-tweak-allocations)))
    (emacsconf-schedule-prepare info)))

(defun emacsconf-schedule-allocate-buffer-time-at-most-max-time (info)
  (mapcar (lambda (o)
            (when (plist-get o :slug)
              (plist-put o :buffer
                         (number-to-string
                          (if (string-match "live" (plist-get o :q-and-a))
                              (min (string-to-number (plist-get o :max-time))
                                   emacsconf-schedule-default-buffer-minutes-for-live-q-and-a)
                            emacsconf-schedule-default-buffer-minutes))))
            o)
          info))

(defun emacsconf-schedule-dump-sexp (info &optional include-time)
  (mapcar (lambda (o)
            (cond
             ((plist-get o :slug) (if include-time (cons (plist-get o :slug) (plist-get o :time)) (intern (plist-get o :slug))))
             ((plist-get o :fixed-time) (cons (plist-get o :title) (format-time-string "%Y-%m-%d %H:%M" (plist-get o :start-time))))
             (t (if include-time
                    (cons (plist-get o :title) (or (plist-get o :time) (plist-get o :max-time)))
                  (plist-get o :title)))))
          info))

(defun emacsconf-schedule-validate-and-summarize (schedule)
  (let ((validation-results (emacsconf-schedule-validate-time-constraints schedule))
        (sched (emacsconf-schedule-summarize schedule)))
    (append
     (list (list (format "%d talks" (length (emacsconf-filter-talks arranged)))))
     (mapcar (lambda (o) (list nil nil nil nil nil o)) validation-results)
     (if show-breaks
         (append
          (emacsconf-summarize-schedule
           (seq-filter (lambda (o) (string-match "BREAK\\|LUNCH" (plist-get o :title)))
                       schedule))
          '(hline)))
     sched nil)))
(defun emacsconf-schedule-inflate-sexp (sequence &optional info include-time)
  "Pairs with `emacsconf-schedule-dump-sexp'."
  (setq info (or info (emacsconf-get-talk-info)))
  (let ((by-assoc (mapcar (lambda (o) (cons (intern (plist-get o :slug)) o)) (emacsconf-filter-talks info)))
        date)
    (mapcar
     (lambda (seq)
       (unless (listp seq) (setq seq (list seq)))
       
       (if include-time
           (error "Not yet implemented")
         (let ((start-prop (or (plist-get (cdr seq) :start)
                               (and (stringp (cdr seq)) (cdr seq))))
               (time-prop (or (plist-get (cdr seq) :time) ; this is duration in minutes
                              (and (numberp (cdr seq)) (cdr seq))))
               (track-prop (plist-get (cdr seq) :track)))
           (append
            ;; overriding 
            (when start-prop
              (if (string-match "-" start-prop)
                  (setq date (format-time-string "%Y-%m-%d" (date-to-time start-prop)))
                (setq start-prop  (concat date " " start-prop)))
              (list
               :scheduled (format-time-string (cdr org-time-stamp-formats) (date-to-time start-prop))
               :start-time (date-to-time start-prop)
               :fixed-time t))
            (when track-prop
              (list :track track-prop))
            (when time-prop
              (list :time (if (numberp time-prop) (number-to-string time-prop) time-prop)))
            ;; base entity
            (cond
             ((eq (car seq) 'lunch)
              (list :title "LUNCH" :time (number-to-string emacsconf-schedule-lunch-time)))
             ((eq (car seq) 'break)
              (list :title "BREAK" :time (number-to-string emacsconf-schedule-break-time)))
             ((symbolp (car seq))
              (assoc-default (car seq) by-assoc))
             ((stringp (car seq))
              (or (seq-find (lambda (o) (string= (plist-get o :title) (car seq))) info)
                  (list :title (car seq))))
             (t (error "Unknown %s" (prin1-to-string seq))))))))
     sequence)))

(defun emacsconf-format-schedule-summary-row (o)
  (pcase emacsconf-focus
    ('status
      (list
       (plist-get o :status)
       (if (plist-get o :slug)
           (org-link-make-string (plist-get o :url)
                                 (plist-get o :slug))
         "")
       (if (plist-get o :scheduled)
           (emacsconf-format-short-time (plist-get o :scheduled))
         "")
       (or (plist-get o :time) "")
       (or (plist-get o :buffer) "")
       (org-link-make-string (org-link-heading-search-string (plist-get o :title))
                             (plist-get o :title))
       (or (plist-get o :speakers) "")
       (or (plist-get o :q-and-a) "")
       (or (plist-get o :availability) "")))
    ('time
      (list
       (if (plist-get o :slug)
           (org-link-make-string (plist-get o :url) (plist-get o :slug))
         "------")
       (if (and (plist-get o :scheduled)
                (not (plist-get o :fixed-time)))
           (emacsconf-format-short-time (plist-get o :scheduled) t)
         "")
       (or (plist-get o :time) "")
       (or (plist-get o :buffer) "")
       (if (< (string-to-number (or (plist-get o :time) ""))
              (string-to-number (or (plist-get o :max-time) "")))
           (plist-get o :max-time)
         "")
       (org-link-make-string (org-link-heading-search-string (plist-get o :title))
                             (plist-get o :title))
       (if (plist-get o :scheduled) ; time is here twice so we can easily check it against availability
           (emacsconf-format-short-time (plist-get o :scheduled))
         "")
       (or (plist-get o :availability) "")))))

(defun emacsconf-schedule-summarize (&optional info)
  (cons
   (if (eq emacsconf-focus 'time)
       (list "Slug" "Schedule" "Time" "Buffer" "Max" "Title" "Time" "Availability")
     (list "Status" "Slug" "Schedule" "Time" "Buffer" "Title" "Name" "Q&A" "Availability"))
   (mapcar #'emacsconf-schedule-format-summary-row (or info (emacsconf-get-talk-info)))))

(defun emacsconf-schedule-update-from-info (info)
  (save-window-excursion
    (save-excursion
      (mapc (lambda (talk)
              (emacsconf-go-to-talk (plist-get talk :slug))
              (org-entry-put (point) "SCHEDULED" (plist-get talk :scheduled))
              (org-entry-put (point) "TIME" (plist-get talk :time)))
            (emacsconf-filter-talks info)))))


(defun emacsconf-schedule-svg-track (svg base-x base-y width height start-time end-time info &optional modify-func)
  (let ((scale (/ width (float-time (time-subtract end-time start-time)))))
    (mapc
     (lambda (o)
       (let* ((offset (floor (* scale (float-time (time-subtract (plist-get o :start-time) start-time)))))
              (size (floor (* scale (float-time (time-subtract (plist-get o :end-time) (plist-get o :start-time))))))
              (x (+ base-x offset))
              (y base-y))
         (dom-append-child
          svg
          (dom-node
           'a
           `((href . ,(concat "/" (plist-get o :url)))
             (title . ,(plist-get o :title)))
           (dom-node
            'title
            nil
            (plist-get o :title))
           (let ((node (dom-node
                        'rect
                        `((x . ,x)
                          (y . ,y)
                          (opacity . "0.8")
                          (width . ,size)
                          (height . ,(1- height))
                          (stroke . "black")
                          (stroke-dasharray . 
                                            ,(if (string-match "live" (or (plist-get o :q-and-a) "live"))
                                                 ""
                                               "5,5,5"
                                               ))
                          (fill . ,(cond
                                    ((string-match "BREAK\\|LUNCH" (plist-get o :title)) "white")
                                    ((plist-get o :invalid) "red")
                                    ((string-match "EST"
                                                   (or (plist-get o :availability) ""))
                                     "lightgray")
                                    (t "lightgreen")))))))
             (if modify-func
                 (funcall modify-func o node)
               node))
           (dom-node
            'g
            `((transform . ,(format "translate(%d,%d)"
                                    (+ x size -2) (+ y height -2))))
            (dom-node
             'text
             '((fill . "black")
               (x . 0)
               (y . 0)
               (font-size . 10)
               (transform . "rotate(-90)"))
             (svg--encode-text (or (plist-get o :slug) (plist-get o :title)))))))))
     info)))

(defun emacsconf-schedule-svg-day (elem label width height start end tracks &optional modify-func)
  (let* ((label-margin 15)
         (track-height (/ (- height (* 2 label-margin)) (length tracks)))
         (x 0) (y label-margin)
         (scale (/ width (float-time (time-subtract end start))))
         (time start))
    (svg-rectangle elem 0 0 width height :fill "white")
    (svg-text elem label :x 3 :y (- label-margin 3) :fill "black" :font-size "10")
    (mapc (lambda (track)
            (emacsconf-schedule-svg-track
             elem x y width track-height
             start end track modify-func)
            (setq y (+ y track-height)))
          tracks)
    ;; draw grid
    (while (time-less-p time end)
      (let ((x (* (float-time (time-subtract time start)) scale)))
        (dom-append-child
         elem
         (dom-node
          'g
          `((transform . ,(format "translate(%d,%d)" x label-margin)))
          (dom-node
           'line
           `((stroke . "lightgray")
             (x1 . 0)
             (y1 . 0)
             (x2 . 0)
             (y2 . ,(- height label-margin label-margin))))
          (dom-node
           'text
           `((fill . "darkgray")
             (x . 0)
             (y . ,(- height 2 label-margin))
             (font-size . 10)
             (text-anchor . "middle"))
           (svg--encode-text (format-time-string "%-l" time emacsconf-timezone)))))
        (setq time (time-add time (seconds-to-time 3600)))))
    elem))

(defun emacsconf-schedule-svg (width height days)
  (let ((svg (svg-create width height :background "white"))
        (day-height (/ height (length days)))
        (y 0))
    (mapc
     (lambda (day)
       (let ((group (dom-node 'g `((transform . ,(format "translate(0,%d)" y))))))
         (dom-append-child svg group)
         (emacsconf-schedule-svg-day group
                                     (plist-get day :label)
                                     width day-height
                                     (date-to-time (plist-get day :start))
                                     (date-to-time (plist-get day :end))
                                     (plist-get day :tracks)))
       (setq y (+ y day-height)))
     days)
    svg))

(defun emacsconf-schedule-get-subsequence (info start &optional end)
  "START and END are regexps to match against the title in INFO."
  (let ((start-position
         (and start
              (seq-position info start
                            (lambda (o match) (string-match match (plist-get o :title)))))))
    (seq-subseq info
                (or start-position 0)
                (if end
                    (+ (or start-position 0)
                       (seq-position (seq-subseq info (or start-position 0)) 
                                     end
                                     (lambda (o match) (string-match match (plist-get o :title)))))
                  (length info)))))

;;; Schedule summary

(defun emacsconf-schedule-round-start-to-five (o)
  (let* ((start-time (plist-get o :start))
         (decoded-time (decode-time start-time "America/Toronto"))
         (duration (* (string-to-number (plist-get o :time)) 60))
         (minutes (elt decoded-time 1))
         offset end-time)
    (unless (= (mod minutes 5) 0)
      (setq offset (seconds-to-time (* 60 (- 5 (mod minutes 5))))
            end-time (time-add start-time (time-to-seconds duration)))
      (plist-put o :scheduled (format "%s-%s" (format-time-string "%Y-%m-%d %H:%M" (time-add start-time offset))
                                      (format-time-string "%H:%M" (time-add end-time offset))))
      (plist-put o :start-time (time-add start-time offset))
      (plist-put o :end-time (time-add end-time offset)))))

;(emacsconf-update-schedules #'emacsconf-round-start-to-five)
(defvar emacsconf-schedule-default-buffer-minutes-for-live-q-and-a 15)
(defvar emacsconf-schedule-default-buffer-minutes 5)
(defun emacsconf-schedule-allocate-buffer-time (info)
  (mapcar (lambda (o)
            (when (plist-get o :slug)
              (plist-put o :buffer
                         (number-to-string 
                          (if (string-match "live" (plist-get o :q-and-a))
                              emacsconf-schedule-default-buffer-minutes-for-live-q-and-a
                            emacsconf-schedule-default-buffer-minutes))))
            o)
          info))

(defun emacsconf-schedule-allocate-max-time (info)
  (mapcar (lambda (o)
            (when (plist-get o :max-time)
              (plist-put o :time (plist-get o :max-time)))
            o)
          info))

(defvar emacsconf-schedule-tweaked-allocations nil "Alist of slug . time")
(defun emacsconf-schedule-tweak-allocations (info)
  (mapcar (lambda (o)
            (let ((talk-times emacsconf-schedule-tweaked-allocations))
              (when (assoc (plist-get o :slug) emacsconf-schedule-tweaked-allocations)
                (plist-put o :time
                           (number-to-string
                            (assoc-default (plist-get o :slug) emacsconf-schedule-tweaked-allocations)))))
            o)
          info))

(defun emacsconf-schedule-based-on-info (info)
  (let (current-time end-time duration) 
    (mapcar
     (lambda (talk)
       (when (plist-get talk :fixed-time)
         (setq current-time (plist-get talk :start-time)))
       (when (and (plist-get talk :time) 
                  (not (string= (plist-get talk :status) "CANCELLED")))
         (setq duration (* (string-to-number (plist-get talk :time)) 60)
               end-time (time-add current-time (seconds-to-time duration)))
         (plist-put talk :scheduled
                    (format "<%s-%s>" (format-time-string "%Y-%m-%d %a %H:%M" current-time)
                            (format-time-string "%H:%M" end-time)))
         (plist-put talk :start-time current-time)
         (plist-put talk :end-time end-time)
         (setq current-time (time-add end-time (* (string-to-number (or (plist-get talk :buffer) "0")) 60))))
       talk)
     info)))

(defun emacsconf-schedule-update (&optional modify-func)
  "Schedule the talks based on TIME and BUFFER.
Talks with a FIXED_TIME property are not moved."
  (interactive)
  (save-excursion
    (org-with-wide-buffer
     (let (current-time end-time duration) 
       (org-map-entries
        (lambda ()
          (when (or (org-entry-get (point) "TIME") (org-entry-get (point) "FIXED_TIME"))
            (let ((talk (emacsconf-get-talk-info-for-subtree)))
              (when (org-entry-get (point) "FIXED_TIME")
                (setq current-time (plist-get talk :start-time)))
              (when (and (plist-get talk :time) 
                         (not (string= (plist-get talk :status) "CANCELLED")))
                (setq duration (* (string-to-number (plist-get talk :time)) 60)
                      end-time (time-add current-time (seconds-to-time duration)))
                (org-set-property "SCHEDULED" (format "%s-%s" (format-time-string "%Y-%m-%d %H:%M" current-time)
                                                      (format-time-string "%H:%M" end-time)))
                (when (functionp modify-func)
                  (funcall modify-func))
                (setq end-time (time-add (org-get-scheduled-time (point)) (seconds-to-time duration)))
                (setq current-time (time-add end-time (* (string-to-number (or (plist-get talk :buffer) "0")) 60))))))))))))

(defun emacsconf-schedule-validate-time-constraints (&optional info)
  (interactive)
  (let* ((info (or info (emacsconf-get-talk-info)))
         (results (delq nil
                        (append
                         (mapcar
                          (lambda (o)
                            (apply #'emacsconf-schedule-check-time
                                   (car o)
                                   (emacsconf-search-talk-info (car o) info)
                                   (cdr o)))
                          emacsconf-time-constraints)
                         (mapcar
                          (lambda (o)
                            (let (result
                                  (constraint (emacsconf-schedule-get-time-constraint o)))
                              (when constraint
                                (setq result (apply #'emacsconf-schedule-check-time
                                                      (plist-get o :slug)
                                                      o
                                                      constraint))
                                (when result (plist-put o :invalid result))
                                result)))
                          info)))))
    (if (called-interactively-p 'any)
        (message "%s" (string-join results "\n"))
      results)))

(defun emacsconf-schedule-check-time (label o &optional from-time to-time)
  "FROM-TIME and TO-TIME should be strings like HH:MM in EST.
Both start and end time are tested."
  (let* ((start-time (format-time-string "%H:%M" (plist-get o :start-time)))
         (end-time (format-time-string "%H:%M" (plist-get o :end-time)))
         result)
    (setq result
          (or
           (and (null o) (format "%s: Not found" label))
           (and from-time (string< start-time from-time)
                (format "%s: Starts at %s before %s" label start-time from-time))
           (and to-time (string< to-time end-time)
                (format "%s: Ends at %s after %s" label end-time to-time))))
    (when result (plist-put o :invalid result))
    result))

(defun emacsconf-schedule-get-time-constraint (o)
  (unless (string-match "after the event" (or (plist-get o :q-and-a) ""))
    (let ((avail (or (plist-get o :availability) ""))
          hours)
      (when (string-match "\\([<>]\\)=? *\\([0-9]+:[0-9]+\\) *EST" avail)
        (if (string= (match-string 1 avail) ">")
            (list (match-string 2 avail) nil)
          (list nil (match-string 2 avail)))))))

(defun emacsconf-schedule-validate (sched)
  (let* ((sched-slugs (mapcar (lambda (o) (plist-get o :slug))
                              (emacsconf-filter-talks sched)))
         (diff (delq nil
                     (seq-difference
                      (mapcar (lambda (o) (plist-get o :slug))
                              (seq-remove (lambda (o)
                                            (string= (plist-get o :status) "CANCELLED"))
                                          (let ((emacsconf-talk-info-functions '(emacsconf-get-talk-info-from-properties)))
                                            (emacsconf-get-talk-info))))
                      sched-slugs)))
         (dupes (seq-filter (lambda (o) (> (length (cdr o)) 1))
                            (seq-group-by #'identity sched-slugs))))
    (append
     (emacsconf-schedule-validate-time-constraints sched)
     (emacsconf-schedule-validate-live-q-and-a-sessions-are-staggered sched)
     (when diff
       (list (concat "Missing talks: " (string-join diff ", "))))
     (when dupes
       (list (concat "Duplicate talks: " (mapconcat 'car dupes ", ")))))))

(defun emacsconf-schedule-inflate-tracks (tracks schedule)
  (mapcar
   (lambda (day)
     (plist-put day :tracks
                (mapcar
                 (lambda (track)
                   (if (stringp track)
                       ;; track property
                       (seq-filter (lambda (o) (string= (or (plist-get o :track) (car (plist-get day :tracks)))
                                                        track))
                                   schedule)
                     ;; start and end regexp
                     (apply #'emacsconf-schedule-get-subsequence schedule track)))
                 (plist-get day :tracks)))
     day)
   tracks))

(defmacro emacsconf-schedule-test (filename &rest varlist)
  `(let* (,@varlist)
     (let* ((schedule (emacsconf-schedule-prepare arranged))
            (validation (or (emacsconf-schedule-validate schedule) "")))
       (with-temp-file ,filename
         (svg-print (emacsconf-schedule-svg 800 200 (emacsconf-schedule-inflate-tracks tracks schedule))))
       (mapconcat (lambda (o) (format "- %s\n" o)) (append validation (list (format "[[file:%s]]" filename)))))))

(defun emacsconf-schedule-format-summary-row (o)
  (pcase emacsconf-focus
    ('status
      (list
       (plist-get o :status)
       (if (plist-get o :slug)
           (org-link-make-string (concat "https://emacsconf.org/" emacsconf-year "/talks/"
                                         (plist-get o :slug))
                                 (plist-get o :slug))
         "")
       (if (plist-get o :scheduled)
           (emacsconf-format-short-time (plist-get o :scheduled))
         "")
       (or (plist-get o :time) "")
       (or (plist-get o :buffer) "")
       (org-link-make-string (org-link-heading-search-string (plist-get o :title))
                             (plist-get o :title))
       (or (plist-get o :speakers) "")
       (or (plist-get o :q-and-a) "")
       (or (plist-get o :availability) "")))
    ('time
      (list
       (if (plist-get o :slug)
           (org-link-make-string (concat "https://emacsconf.org/" emacsconf-year "/talks/"
                                         (plist-get o :slug))
                                 (plist-get o :slug))
         "------")
       (if (plist-get o :scheduled)
           (format-time-string "%l:%M%#p" (plist-get o :start-time))
         "")
       (or (plist-get o :time) "")
       (or (plist-get o :buffer) "")
       (if (< (string-to-number (or (plist-get o :time) ""))
              (string-to-number (or (plist-get o :max-time) "")))
           (plist-get o :max-time)
         "")
       (org-link-make-string (org-link-heading-search-string (plist-get o :title))
                             (plist-get o :title))
       (if (plist-get o :scheduled) ; time is here twice so we can easily check it against availability
           (emacsconf-format-short-time (plist-get o :scheduled))
         "")
       (or (plist-get o :availability) "")))))

(defvar emacsconf-schedule-validate-live-q-and-a-sessions-buffer 5 "Number of minutes' allowance for a streamer to adjust audio and get set up.
Try to avoid overlapping the start of live Q&A sessions.")
(defun emacsconf-schedule-validate-live-q-and-a-sessions-are-staggered (schedule)
  "Return nil if there are no errors.
Try to avoid overlapping the start of live Q&A sessions."
  (when emacsconf-schedule-validate-live-q-and-a-sessions-buffer
    (let (last-end)
      (delq nil
            (mapcar (lambda (o)
                      (prog1
                          (when (and last-end
                                     (time-less-p
                                      (plist-get o :end-time)
                                      (time-add last-end (seconds-to-time (* emacsconf-schedule-validate-live-q-and-a-sessions-buffer 60)))))
                            (plist-put o :invalid (format "%s live Q&A starts at %s within %d minutes of previous live Q&A at %s"
                                                          (plist-get o :slug)
                                                          (format-time-string "%m-%d %-l:%M"
                                                                              (plist-get o :end-time))
                                                          emacsconf-validate-live-q-and-a-sessions-buffer
                                                          (format-time-string "%m-%d %-l:%M"
                                                                              last-end)))
                            (plist-get o :invalid))
                        (setq last-end (plist-get o :end-time))))
                    (sort 
                     (seq-filter (lambda (o) (string-match "live" (or (plist-get o :q-and-a) "")))
                                 schedule)
                     (lambda (a b)
                       (time-less-p (plist-get a :end-time) (plist-get b :end-time)))
                     ))))))
(defvar emacsconf-schedule-plan nil "Sequence of talks.")
(provide 'emacsconf-schedule)
;;; emacsconf-schedule.el ends here
