;;; emacsconf.el --- Core functions and variables for EmacsConf  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Sacha Chua

;; Author: Sacha Chua <sacha@sachachua.com>
;; Keywords: multimedia

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

(defgroup emacsconf nil "EmacsConf" :group 'multimedia)

(defcustom emacsconf-name "EmacsConf"
  "Name of conference"
  :group 'emacsconf
  :type 'string)
(defcustom emacsconf-year "2022"
  "Conference year. String for easy inclusion."
  :group 'emacsconf
  :type 'string)
(defcustom emacsconf-directory "~/vendor/emacsconf-wiki"
  "Directory where the wiki files are."
  :group 'emacsconf
  :type 'directory)
(defcustom emacsconf-ansible-directory nil
  "Directory where the Ansible repository is."
  :group 'emacsconf
  :type 'directory)

(defcustom emacsconf-timezone "US/Eastern" "Main timezone."
  :group 'emacsconf
  :type 'string)

(defcustom emacsconf-timezones '("US/Eastern" "US/Central" "US/Mountain" "US/Pacific" "UTC" "Europe/Paris" "Europe/Athens" "Asia/Kolkata" "Asia/Singapore" "Asia/Tokyo") "List of timezones."
  :group 'emacsconf
  :type '(repeat string))

(defcustom emacsconf-base-url "https://emacsconf.org/" "Includes trailing slash"
  :group 'emacsconf
  :type 'string)
(defcustom emacsconf-publishing-phase 'program
  "Controls what information to include.
'program - don't include times
'schedule - include times; use this leading up to the emacsconference
'resources - after EmacsConf, don't need status"
  :group 'emacsconf
  :type '(choice
          (const :tag "Program: Don't include times" program)
          (const :tag "Schedule: Include detailed times" schedule)
          (const :tag "Resources: Don't include status" resources)))

(defcustom emacsconf-org-file nil
  "Path to the Org file with emacsconference information."
  :type 'file
  :group 'emacsconf)

(defcustom emacsconf-upcoming-file nil
  "Path to the Org file with upcoming talks."
  :type 'file
  :group 'emacsconf)

(defvar emacsconf-stream-base "https://live0.emacsconf.org/")
(defvar emacsconf-chat-base "https://chat.emacsconf.org/")

(defcustom emacsconf-download-directory "~/Downloads"
  "Directory to check for downloaded files."
  :type 'directory
  :group 'emacsconf)

(defun emacsconf-latest-file (path &optional filter)
  "Return the newest file in PATH. Optionally filter by FILTER."
  (car (sort (seq-remove #'file-directory-p (directory-files path 'full filter t)) #'file-newer-than-file-p)))

(defun emacsconf-find-captions-from-slug (search)
  (interactive (list (emacsconf-complete-talk)))
  (emacsconf-with-talk-heading search (emacsconf-find-captions)))

(defun emacsconf-edit-wiki-page (search)
  (interactive (list (emacsconf-complete-talk)))
  (setq search (emacsconf-get-slug-from-string search))
  (find-file (expand-file-name (concat search ".md")
                               (expand-file-name "talks" (expand-file-name emacsconf-year emacsconf-directory)))))

(defun emacsconf-find-caption-directives-from-slug (search)
  (interactive (list (emacsconf-complete-talk)))
  (setq search (emacsconf-get-slug-from-string search))
  (find-file (expand-file-name (concat search ".md")
                               (expand-file-name "captions" (expand-file-name emacsconf-year emacsconf-directory)))))


(defun emacsconf-browse-wiki-page (search)
  (interactive (list (emacsconf-complete-talk)))
  (setq search (emacsconf-get-slug-from-string search))
  (browse-url (concat emacsconf-base-url emacsconf-year "/talks/" search "/")))

(defun emacsconf-set-property-from-slug (search prop value)
  (interactive (list (emacsconf-complete-talk) nil nil))
  (save-window-excursion
    (emacsconf-with-talk-heading search
      (setq prop (or prop (org-read-property-name)))
      (setq value (or value (org-read-property-value prop)))
      (org-entry-put (point) prop value))))


(defun emacsconf-complete-slug ()
  (emacsconf-get-slug-from-string (emacsconf-complete-talk)))

(defun emacsconf-export-slug (link description format _)
  (let ((path (format "https://emacsconf.org/%s/talks/%s" emacsconf-year link))
        (desc (or description link)))
    (pcase format
      (`html
       (format "<a href=\"#%s\">%s</a>" link desc))
      (`ascii (format "%s (%s)" desc path))
      (`markdown
       (format "[[%s|%s/talks/%s]]" desc emacsconf-year link))
      (_ path))))

(with-eval-after-load 'org
  (org-link-set-parameters
   "emacsconf"
   :follow #'emacsconf-go-to-talk
   :complete (lambda () (concat "emacsconf:" (emacsconf-complete-slug)))
   :export #'emacsconf-export-slug))


(defun emacsconf-complete-talk (&optional info)
  (let ((choices
         (mapcar (lambda (o)
                   (string-join
                    (delq nil
                          (mapcar (lambda (f) (plist-get o f)) '(:slug :title :speakers :irc)))
                    " - "))
                 (or info (emacsconf-get-talk-info)))))
    (completing-read
     "Talk: " 
     (lambda (string predicate action)
       (if (eq action 'metadata)
           '(metadata (category . emacsconf))
         (complete-with-action action choices string predicate))))))

(defun emacsconf-complete-talk-info (&optional info)
  (emacsconf-search-talk-info (emacsconf-complete-talk info) info))

(defun emacsconf-get-slug-from-string (search)
  (if (listp search) (setq search (car search)))
  (if (and search (string-match "\\(.*?\\) - " search))
      (match-string 1 search)
    search))

(defun emacsconf-go-to-talk (search)
  (interactive (list (emacsconf-complete-talk)))
  (pop-to-buffer (find-file-noselect emacsconf-org-file))
  (widen)
  (cond
   ((plist-get search :slug)
    (goto-char (org-find-property "SLUG" (plist-get search :slug))))
   ((emacsconf-get-slug-from-string search)
    (goto-char (org-find-property "SLUG" (emacsconf-get-slug-from-string search))))
   (t
    (goto-char
     (catch 'found
       (org-map-entries
        (lambda ()
          (when (string-match search
                              (cons
                               (concat (org-entry-get (point) "SLUG") " - "
                                       (org-entry-get (point) "ITEM") " - "
                                       (org-entry-get (point) "NAME") " - "
                                       (org-entry-get (point) "EMAIL"))
                               (point)))
            (throw 'found (point))))
        "SLUG={.}")))))
  (org-reveal))

(defmacro emacsconf-for-each-talk (&rest body)
  (declare (indent 0) (debug t))
  `(org-map-entries (lambda () ,@body) "SLUG={.}"))

(defmacro emacsconf-with-talk-heading (search &rest body)
  (declare (indent 1) (debug t))
  `(progn
     (emacsconf-go-to-talk ,search)
     ,@body))

(defvar emacsconf-status-types
  '(("WAITING_FOR_PREREC" . "Waiting for video from speaker")
    ("TO_PROCESS" . "Processing uploaded video")
    ("TO_AUTOCAP" . "Processing uploaded video")
    ("TO_ASSIGN" . "Waiting for a caption volunteer")
    ("TO_CAPTION" . "Processing uploaded video")
    ("TO_STREAM" . "Talk captioned")
    ("PLAYING" . "Now playing on the conference livestream")
    ("CLOSED_Q" . "Q&A starting (not yet open for joining)")
    ("OPEN_Q" . "Q&A open for participation")
    ("UNSTREAMED_Q" . "Q&A continues off the stream")
    ("TO_ARCHIVE" . "Q&A finished, IRC and pad will be archived on this page")
    ("TO_EXTRACT" . "Q&A to be extracted from the room recordings")
    ("DONE" . "All done")
    ("CANCELLED" . "Talk cancelled")))

(defun emacsconf-get-talk-categories (o)
  (org-narrow-to-subtree)
  (let (list)
    (while (re-search-forward "Category[^ \t\n]+" nil t)
      (setq list (cons (match-string-no-properties 0) list)))
    (plist-put o :categories (reverse list))))

(defun emacsconf-get-talk-info-from-properties (o)
  (let ((heading (org-heading-components))
        (field-props '(                 
                       ;; Initial creation
                       (:title "ITEM")
                       (:track "TRACK")
                       (:slug "SLUG")
                       (:speakers "NAME")                       
                       (:speakers-short "NAME_SHORT")
                       (:email "EMAIL")
                       (:public-email "PUBLIC_EMAIL")
                       (:emergency "EMERGENCY")
                       (:buffer "BUFFER")
                       (:duration "TIME")
                       (:min-time "MIN_TIME")
                       (:max-time "MAX_TIME")
                       (:availability "AVAILABILITY")
                       (:q-and-a "Q_AND_A")
                       (:timezone "TIMEZONE")
                       (:irc "IRC")
                       (:pronunciation "PRONUNCIATION")                       
                       (:pronouns "PRONOUNS") 
                       ;; Scheduling
                       (:scheduled "SCHEDULED")
                       (:time "TIME")
                       (:fixed-time "FIXED_TIME")
                       ;; Coordination
                       (:prerec-info "PREREC_INFO")
                       ;; Prep
                       (:bbb-room "ROOM")                       
                       ;; Processing
                       (:video-slug "VIDEO_SLUG")
                       (:video-file "VIDEO_FILE")                       
                       (:video-file-size "VIDEO_FILE_SIZE")                       
                       (:video-duration "VIDEO_DURATION")
                       (:youtube-url "YOUTUBE_URL")                       
                       (:toobnix-url "TOOBNIX_URL")
                       ;; Captioning
                       (:captioner "CAPTIONER")
                       (:caption-note "CAPTION_NOTE")
                       (:intro-note "INTRO_NOTE")
                       ;; Conference
                       (:check-in "CHECK_IN")
                       (:public "PUBLIC")
                       ;; Extraction
                       (:qa-youtube "QA_YOUTUBE")
                       (:qa-toobnix "QA_TOOBNIX")
                       ;; Old
                       (:alternate-apac "ALTERNATE_APAC")                       
                       (:extra-live-time "EXTRA_LIVE_TIME")
                       (:present "PRESENT")
                       (:talk-id "TALK_ID") ; use slug instead
                       (:qa-public "QA_PUBLIC") ; now tracked by the OPEN_Q and UNSTREAMED_Q status
                       (:uuid "UUID")           ; Pentabarf export
                       )))
    (apply
     'append
     o
     (list
      :point (point)
      :year emacsconf-year
      :conf-year emacsconf-year
      :type (if (org-entry-get (point) "SLUG") 'talk 'headline)
      :status (elt heading 2)
      :level (car heading)
      :url (concat emacsconf-year "/talks/" (org-entry-get (point) "SLUG"))
      :schedule-group (org-entry-get-with-inheritance "SCHEDULE_GROUP")
      :wiki-file-path
      (expand-file-name 
       (concat (org-entry-get (point) "SLUG") ".md")
       (expand-file-name "captions" (expand-file-name emacsconf-year emacsconf-directory)))
      :start-time
      (when (org-entry-get (point) "SCHEDULED")
		    (date-to-time
		     (concat
		      (format-time-string "%Y-%m-%dT%H:%M:%S"
					                    (org-timestamp-to-time
					                     (org-timestamp-split-range
					                      (org-timestamp-from-string
					                       (org-entry-get (point) "SCHEDULED")))))
		      emacsconf-timezone-offset)))
      :end-time (when (org-entry-get (point) "SCHEDULED")
		              (date-to-time
		               (concat
		                (format-time-string "%Y-%m-%dT%H:%M:%S"
					                              (org-timestamp-to-time
					                               (org-timestamp-split-range
					                                (org-timestamp-from-string
					                                 (org-entry-get (point) "SCHEDULED"))
					                                t)))
		                emacsconf-timezone-offset))))
     (let* ((entry-props (org-entry-properties)))
       (mapcar 
        (lambda (o) (list (car o) (assoc-default (cadr o) entry-props)))
        field-props)))))

(defvar emacsconf-abstract-heading-regexp "abstract" "Regexp matching heading for talk abstract.")

(defun emacsconf-get-subtree-entry (heading-regexp)
  (car
   (delq nil
         (org-map-entries
          (lambda ()
            (when (string-match heading-regexp (org-entry-get (point) "ITEM"))
              (org-get-entry)))
          nil 'tree))))

(defun emacsconf-get-talk-abstract-from-subtree (o)
  "Add the abstract from a subheading with a title matching Abstract."
  (plist-put o :abstract (substring-no-properties (or (emacsconf-get-subtree-entry "abstract") ""))))


(defun emacsconf-get-talk-comments-from-subtree (o)
  (setq o (plist-put o :comments
                     (apply 'append
                            (org-map-entries
                             (lambda ()
                               (org-end-of-meta-data)
                               (mapcar (lambda (item)
                                         (string-trim
                                          (replace-regexp-in-string
                                           " *\n *"
                                           " "
                                           (buffer-substring-no-properties (+ (car item) (length (elt item 2)))
                                                                           (min (point-max) (elt item 6))))))
                                       (org-element-property  :structure (org-element-at-point)))
                               )
                             "ITEM={comments}" 'tree))))
  (plist-put o :acceptance-comment
             (car (delq nil (mapcar
                             (lambda (o)
                               (when (string-match "For the [^ ]+ speakers?: " o)
                                 (replace-match "" t t o)))
                             (plist-get o :comments))))))

(defun emacsconf-convert-talk-abstract-to-markdown (o)
  (plist-put o :abstract-md (org-export-string-as (or (plist-get o :abstract) "") 'md t)))

(defun emacsconf-summarize-times (time timezones)
  (let (prev-day)
    (mapconcat
     (lambda (tz)
       (let ((cur-day (format-time-string "%a %b %-e" time tz))
             (cur-time (format-time-string "%H%MH %Z" time tz)))
         (if (equal prev-day cur-day)
             cur-time
           (setq prev-day cur-day)
           (concat cur-day " " cur-time))))
     timezones
     " / ")))

(defun emacsconf-add-timezone-conversions (o)
  (plist-put o :scheduled-tzs
             (concat (org-timestamp-format (plist-get o :start-time) "%a %b %e %l:%M%p Toronto time (")
                     (emacsconf-summarize-times (plist-get o :start-time) emacsconf-timezones)
                     ")")))

(defun emacsconf-get-abstract-from-wiki (o)
  (plist-put o :markdown (emacsconf-talk-markdown-from-wiki (plist-get o :slug))))


(defun emacsconf-add-talk-status (o)
  (plist-put o :status-label
             (or (assoc-default (plist-get o :status) 
                                emacsconf-status-types 'string= "")
                 (plist-get o :status)))
  (if (member (plist-get o :status)
              (split-string "PLAYING CLOSED_Q OPEN_Q UNSTREAMED_Q TO_ARCHIVE TO_EXTRACT TO_FOLLOW_UP"))
      (plist-put o :public t))
  o)

(defun emacsconf-test-public-states ()
  (let ((states (split-string (replace-regexp-in-string "(.*?)" "" "TODO(t) TO_REVIEW TO_ACCEPT TO_CONFIRM WAITING_FOR_PREREC(w) TO_PROCESS(p) TO_AUTOCAP(y) TO_ASSIGN(a) TO_CAPTION(c) TO_STREAM(s) PLAYING(m) CLOSED_Q(q) OPEN_Q(o) UNSTREAMED_Q(u) TO_ARCHIVE TO_EXTRACT TO_FOLLOW_UP"))))
    (mapc (lambda (state) (assert (null (plist-get (emacsconf-add-talk-status
                                                    (list :status state))
                                                   :public))))
          (subseq states
                  0
                  (seq-position
                   states "PLAYING")))
    (mapc (lambda (state) (assert (plist-get (emacsconf-add-talk-status (list
                                                                         :status
                                                                         state))
                                             :public)))
          (subseq states (seq-position
                          states "PLAYING")))))

(defvar emacsconf-talk-info-functions
  '(emacsconf-get-talk-info-from-properties
    emacsconf-get-talk-categories
    emacsconf-get-talk-abstract-from-subtree
    emacsconf-add-talk-status
    emacsconf-add-timezone-conversions
    emacsconf-add-live-info))

(defun emacsconf-add-live-info (o)
  (let ((track (emacsconf-get-track (plist-get o :track))))
    (when track
      (plist-put o :watch-url (concat emacsconf-base-url emacsconf-year "/watch/" (plist-get track :id))))
    (plist-put o :channel (plist-get track :channel))
    (cond
     ((string-match "live" (or (plist-get o :q-and-a) ""))
      (plist-put o :bbb-redirect (format "https://emacsconf.org/current/%s/room/" (plist-get o :slug)))
      (plist-put o :qa-info (plist-get o :bbb-redirect)))
     ((string-match "IRC" (or (plist-get o :q-and-a) ""))
      (plist-put o :qa-info (concat (emacsconf-surround "nick: " (plist-get o :irc) ", " "")
                                    (plist-get o :channel))))
     (t (plist-put o :qa-info "none")))
    (plist-put o :pad-url (format "https://pad.emacsconf.org/%s-%s" emacsconf-year (plist-get o :slug)))
    o))

(defun emacsconf-search-talk-info (search &optional info)
  (setq info (or info (emacsconf-get-talk-info)))
  (or
   (seq-find (lambda (o) (string= (plist-get o :slug)
                                  (emacsconf-get-slug-from-string search)))
             info)
   (seq-find (lambda (o)
               (string-match
                search
                (format "%s - %s - %s - %s"
                        (plist-get o :slug)
                        (plist-get o :title)
                        (plist-get o :speakers)
                        (plist-get o :email))))
             info)))

(defun emacsconf-get-talk-info-for-subtree ()
  (seq-reduce (lambda (prev val) (save-excursion (save-restriction (funcall val prev))))
              emacsconf-talk-info-functions
              nil))

(defun emacsconf-sort-by-scheduled (a b)
  (let ((time-a (plist-get a :start-time))
        (time-b (plist-get b :start-time)))
    (cond
     ((null time-b) t)
     ((null time-a) nil)
     ((time-less-p time-a time-b) t)
     ((time-less-p time-b time-a) nil)
     (t (< (or (plist-get a :point) 0) (or (plist-get b :point) 0))))))

(defun emacsconf-get-talk-info ()
  (with-current-buffer (find-file-noselect emacsconf-org-file)
    (save-excursion
      (save-restriction
        (widen)
        (let (results)
          (org-map-entries
           (lambda ()
             (when (or (org-entry-get (point) "TIME")
                       (org-entry-get (point) "SLUG")
                       (org-entry-get (point) "INCLUDE_IN_INFO"))
               (setq results
                     (cons (emacsconf-get-talk-info-for-subtree)
                           results)))))
          (nreverse results))))))

(defun emacsconf-filter-talks (list)
  "Return only talk info in LIST."
  (seq-filter
   (lambda (talk) (eq (plist-get talk :type) 'talk))
   list))

(defun emacsconf-collect-field-for-status (status field &optional info)
  (seq-map
   (lambda (o)
     (plist-get o field))
   (seq-filter
    (lambda (o)
      (if (listp status)
          (member (plist-get o :status) status)
        (string= status (plist-get o :status))))
    (emacsconf-filter-talks (or info (emacsconf-get-talk-info))))))


(defun emacsconf-get-talk-info-from-file (&optional filename)
  (with-temp-buffer
    (insert-file-contents (or filename "conf.org"))
    (org-mode)
    (org-show-all)
    (goto-char (point-min))
    (goto-char (org-find-property "ID" "talks"))
    (emacsconf-get-talk-info 'wiki)))

(defun emacsconf-include-next-talks (info number)
  (let* ((info (sort (emacsconf-active-talks (emacsconf-filter-talks (or info (emacsconf-get-talk-info))))
                     #'emacsconf-sort-by-scheduled))
         (cur-list info))
    ;; add links to the next talks
    (while cur-list
      (plist-put (pop cur-list) :next-talks (seq-take cur-list number)))
    info))

(defun emacsconf-find-talk-info (filter &optional info)
  (setq info (or info (emacsconf-filter-talks (emacsconf-get-talk-info))))
  (when (stringp filter) (setq filter (list filter)))
  (or (seq-find (lambda (o) (string= (plist-get o :slug) (car filter))) info)
      (seq-find (lambda (o)
                  (let ((case-fold-search t)
                        (all (mapconcat (lambda (f) (plist-get o f)) '(:title :speakers :slug) " ")))
                    (null (seq-contains-p
                           (mapcar (lambda (condition) (string-match condition all)) filter)
                           nil))))
                info)))

(defun emacsconf-combine-plist (list-of-talks separator)
  (let (result entry)
    (while list-of-talks
      (setq entry (car list-of-talks))
      (while entry
        (unless (equal (plist-get result (car entry))
                         (cadr entry))
          (setq result
                (plist-put result
                           (car entry)
                           (cons (cadr entry)
                                 (or (plist-get result (car entry)))))))
        (setq entry (cddr entry)))
      (setq list-of-talks (cdr list-of-talks)))
    result))

(defun emacsconf-goto-talk-id (id)
  (goto-char (org-find-property "TALK_ID" id)))

(defun emacsconf-goto-slug (slug)
  (goto-char (org-find-property "SLUG" id)))

(defun emacsconf-talk-markdown-from-wiki (slug)
  "Return the markdown from SLUG."
  (when (file-exists-p (expand-file-name (format "%s/talks/%s.md" emacsconf-year slug) emacsconf-directory))
    (with-temp-buffer
      (insert-file-contents (expand-file-name (format "%s/talks/%s.md" emacsconf-year slug) emacsconf-directory))
      (goto-char (point-min))
      (while (re-search-forward "<!--" nil t)
        (let ((start (match-beginning 0)))
          (when (re-search-forward "-->" nil t)
            (delete-region start (match-end 0)))))
      (goto-char (point-min))
      (while (re-search-forward "\\[\\[![^]]+\\]\\]" nil t)
        (replace-match ""))
      (string-trim (buffer-string)))))

(defun emacsconf-replace-plist-in-string (attrs string)
  "Replace ${keyword} from ATTRS in STRING."
  (let ((a attrs) name val)
    (while a
      (setq name (pop a) val (pop a))
      (when (stringp val)
        (setq string
              (replace-regexp-in-string (regexp-quote (concat "${" (substring (symbol-name name) 1) "}"))
                                        (or val "")
                                        string t t))))
    string))

(defun emacsconf-public-talks (info)
  (seq-filter (lambda (f) (plist-get f :public)) info))

(defun emacsconf-format-short-time (string &optional omit-end-time)
  (if (stringp string) (setq string (org-timestamp-from-string string)))
  (downcase
   (concat (format-time-string "~%l:%M%p"
                               (org-timestamp-to-time
                                (org-timestamp-split-range
                                 string)))
           (if omit-end-time ""
             (format-time-string "-%l:%M%p"
                                 (org-timestamp-to-time
                                  (org-timestamp-split-range
                                   string t)))))))

(defvar emacsconf-focus 'time "'time or 'status")

;;; Embark
(defun emacsconf-embark-finder ()
  (when (and (derived-mode-p 'org-mode)
             (org-entry-get-with-inheritance "SLUG"))
    (cons 'emacsconf (org-entry-get-with-inheritance "SLUG"))))

(defun emacsconf-insert-talk-title (search)
  (interactive (list (emacsconf-complete-talk)))
  (insert (plist-get (emacsconf-search-talk-info search) :title)))

(with-eval-after-load 'embark
  (add-to-list 'embark-target-finders 'emacsconf-embark-finder)
  (embark-define-keymap embark-emacsconf-actions
    "Keymap for emacsconference-related things"
    ("a" emacsconf-announce)
    ("c" emacsconf-find-captions-from-slug)
    ("d" emacsconf-find-caption-directives-from-slug)
    ("p" emacsconf-set-property-from-slug)
    ("w" emacsconf-edit-wiki-page)
    ("s" emacsconf-set-start-time-for-slug)
    ("W" emacsconf-browse-wiki-page)
    ("u" emacsconf-update-talk)
    ("it" emacsconf-insert-talk-title)
    ("m" emacsconf-mail-speaker-from-slug)
    ("n" emacsconf-notmuch-search-mail-from-entry)
    ("f" org-forward-heading-same-level)
    ("b" org-backward-heading-same-level)
    ("RET" emacsconf-go-to-talk))
  (add-to-list 'embark-keymap-alist '(emacsconf . embark-emacsconf-actions)))

;;; Status updates

(defun emacsconf-status-update ()
  (interactive)
  (let ((emacsconf-info (emacsconf-get-talk-info)))
    (kill-new
     (format "%d captioned (%d minutes), %d received and waiting to be captioned (%d minutes)"
             (length (emacsconf-collect-field-for-status "CAPTIONED" :title))
             (apply '+ (seq-map 'string-to-number (conf-collect-field-for-status "CAPTIONED" :duration)))
             (length (emacsconf-collect-field-for-status "PREREC_RECEIVED" :title))
             (apply '+ (seq-map 'string-to-number (conf-collect-field-for-status "PREREC_RECEIVED" :duration)))))))

;; Timezones
(defvar emacsconf-date "2022-12-03" "Starting date of EmacsConf.")
(defvar emacsconf-timezone-offset
  (format-time-string "%z" (date-to-time emacsconf-date) emacsconf-timezone)
  "Timezone offset for `emacsconf-timezone' on `emacsconf-date'.")

(defun emacsconf-timezone-string (o tz)
  (let* ((timestamp (org-timestamp-from-string (plist-get o :scheduled)))
         (start (org-timestamp-to-time (org-timestamp-split-range timestamp)))
         (end (org-timestamp-to-time (org-timestamp-split-range timestamp t))))
    (if (string= tz "UTC")
        (format "%s - %s "
                (format-time-string "%A, %b %-e %Y, ~%-l:%M %p"
                                    start tz)
                (format-time-string "%-l:%M %p %Z"
                                    end tz))
      (format "%s - %s (%s)"
              (format-time-string "%A, %b %-e %Y, ~%-l:%M %p"
                                  start tz)
              (format-time-string "%-l:%M %p %Z"
                                  end tz)
              tz))))
(defun emacsconf-timezone-strings (o &optional timezones)
  (mapcar (lambda (tz) (emacsconf-timezone-string o tz)) (or timezones emacsconf-timezones)))

(defun emacsconf-convert-from-timezone (timezone time)
  (interactive (list (completing-read "From zone: " tzc-time-zones)
                     (read-string "Time: ")))
  (let* ((from-offset (format-time-string "%z" (date-to-time emacsconf-date) timezone))
         (time
          (date-to-time
           (concat emacsconf-date "T" (string-pad time 5 ?0 t)  ":00.000"
                   from-offset))))
    (message "%s = %s"
             (format-time-string
              "%b %d %H:%M %z"
              time
              timezone)
             (format-time-string
              "%b %d %H:%M %z"
              time
              emacsconf-timezone))))

;;; Etherpad
(defvar emacsconf-review-comments-heading "Comments")
(defun emacsconf-import-comments-from-etherpad-text (filename)
  (interactive "FEtherpad text export: ")
  (with-temp-buffer
    (insert-file-contents filename)
    (goto-char (point-min))
    (while (re-search-forward "^[\t ]+Comments for \\([^:]+\\)" nil t)
      (let ((slug (match-string 1))
            comments)
        (forward-line 1)
        (setq comments
              (split-string
                (replace-regexp-in-string
                "\t\t\\*[ \t]*"
                ""
                (buffer-substring-no-properties
                 (point)
                 (if (re-search-forward "^[^\t]" nil t)
                     (match-beginning 0)
                   (point-max))))
               "\n"))
        (save-window-excursion
          (emacsconf-with-talk-heading slug
            ;; Do we already have a heading for comments?
            (if (re-search-forward (concat "^\\(\\*+\\) +" emacsconf-review-comments-heading)
                                   (save-excursion (org-end-of-subtree)) t)
                (org-end-of-meta-data)
              (org-end-of-subtree)
              (org-insert-heading-after-current)
              (insert emacsconf-review-comments-heading "\n"))
            ;; Are these comments already included?
            (save-restriction
              (org-narrow-to-subtree)
              (mapc (lambda (o)
                      (goto-char (point-min))
                      (unless (re-search-forward (regexp-quote o) nil t)
                        (goto-char (point-max))
                        (unless (bolp) (insert "\n"))
                        (insert "- " o "\n")))
                    comments))))))))

;;; Validation

(defun emacsconf-validate-all-talks-have-comments-for-speakers ()
  (interactive)
  (emacsconf-for-each-talk
    (unless (re-search-forward "^\\(- \\)?For \\(the \\)?[^ ]+ speaker" (save-excursion (org-end-of-subtree) (point)) t)
      (error "Could not find comment for %s" (org-entry-get (point) "SLUG"))))
  nil)

(defun emacsconf-validate-all-talks-have-field (field)
  (emacsconf-for-each-talk
    (when (string= (or (org-entry-get (point) field) "") "")
      (error "%s is missing %s" (org-entry-get (point) "SLUG") field)))
  nil)



(defvar emacsconf-time-constraints
  '(("saturday morning break" "10:00" "11:30")
    ("saturday lunch" "11:30" "13:30")
    ("saturday closing remarks" "16:30" "17:30")
    ("sunday morning break" "10:00" "11:30")
    ("sunday lunch" "11:30" "13:30")
    ("sunday closing remarks" "16:30" "17:30")))

(defun emacsconf-validate-no-overlaps (&optional info)
  (let (results))
  (while (cdr info)
    (when (and (plist-get (car info) :slug)
               (time-less-p (plist-get (cadr info) :start-time) (plist-get (car info) :end-time)))
      (setq results (cons "%s overlaps with %s (ends %s, starts %s)"
                          (or (plist-get (car info) :slug)
                              (plist-get (car info) :title))
                          (or (plist-get (cadr info) :slug)
                              (plist-get (cadr info) :title))
                          (format-time-string "%H:%M" (plist-get (car info) :end-time))
                          (format-time-string "%H:%M" (plist-get (cadr info) :start-time)))))
    (setq info (cdr info))))

(defun emacsconf-active-talks (list)
  "Remove CANCELLED talks from the list."
  (seq-remove (lambda (o) (string= (plist-get o :status) "CANCELLED")) list))

(defun emacsconf-validate-talk-subtree ()
  "Report an error if required properties are missing."
  (interactive)
  (let* ((props (org-entry-properties))
         (missing (seq-remove
                  (lambda (o) (assoc-default (symbol-name o) props))
                  '(CUSTOM_ID SLUG NAME NAME_SHORT EMAIL AVAILABILITY Q_AND_A TRACK MAX_TIME))))
    (when missing
      (if (called-interactively-p 'any)
          (message "Missing %s"  (mapconcat #'symbol-name missing ", "))
        (format "Missing %s"  (mapconcat #'symbol-name missing ", "))))))

;;; Ansible support
(defun emacsconf-ansible-export-talks ()
  (interactive)
  (when emacsconf-ansible-directory
    (with-temp-file (expand-file-name "talks.json" emacsconf-ansible-directory)
      (insert (json-encode (list :talks
                                 (mapcar (lambda (o)
                                           (apply 'list
                                                  (cons :start-time (format-time-string "%FT%T%z" (plist-get o :start-time) t))
                                                  (cons :end-time (format-time-string "%FT%T%z" (plist-get o :end-time) t))
                                                  (mapcar (lambda (field)
                                                            (cons field (plist-get o field)))
                                                          '(:slug :title :speakers :pronouns :pronunciation :url :track)))
                                           )
                                         (emacsconf-filter-talks (emacsconf-get-talk-info)))))))))

(defun emacsconf-ansible-load-vars (file)
  (interactive (list (read-file-name "File: " emacsconf-ansible-directory)))
  (with-temp-buffer
    (insert-file-contents file)
    (let ((vars
           (yaml-parse-string (buffer-string))))
      (mapc (lambda (var)
              (set (car var) (gethash (cdr var) vars))
              )
            '((emacsconf-pad-api-key . etherpad_api_key)
              (emacsconf-pad-base . etherpad_url))))))
;; (emacsconf-ansible-load-vars (expand-file-name "prod-vars.yml" emacsconf-ansible-directory))
;;; Tracks
(defvar emacsconf-tracks '((:name "General" :color "peachpuff" :id "gen" :channel "emacsconf-gen")
                           (:name "Development" :color "skyblue" :id "dev" :channel "emacsconf-dev")))

(defun emacsconf-get-track (name)
  (seq-find (lambda (track) (or (string= name (plist-get track :name))
                                (string= name (plist-get track :id))))
            emacsconf-tracks))

(defun emacsconf-by-track (info)
  (mapcar (lambda (track)
            (seq-filter
             (lambda (talk)
               (string= (plist-get talk :track) (plist-get track :name)))
             info))
          emacsconf-tracks))

(defun emacsconf-by-day (info)
  (seq-group-by (lambda (o)
                  (format-time-string "%Y-%m-%d" (plist-get o :start-time) emacsconf-timezone))
                (sort (seq-filter (lambda (o)
                                    (or (plist-get o :slug)
                                        (plist-get o :include-in-info)))
                                  info)
                      #'emacsconf-sort-by-scheduled)))

(defun emacsconf-filter-talks-by-track (track info)
  (when (stringp track) (setq track (emacsconf-get-track track)))
  (seq-filter (lambda (o) (string= (plist-get o :track) (plist-get track :name))) info))

(defvar emacsconf-shifts
  `((:id "sat-am"
         :label "Sat Dec 3 morning"
         :start ,(concat "2022-12-03T09:00:00" emacsconf-timezone-offset)
         :end ,(concat "2022-12-03T12:00:00" emacsconf-timezone-offset))
    (:id "sat-pm"
         :label "Sat Dec 3 afternoon"
         :start ,(concat "2022-12-03T13:00:00" emacsconf-timezone-offset)
         :end ,(concat "2022-12-03T17:30:00" emacsconf-timezone-offset))
    (:id "sun-am"
         :label "Sun Dec 4 morning"
         :start ,(concat "2022-12-04T09:00:00" emacsconf-timezone-offset)
         :end ,(concat "2022-12-04T12:00:00" emacsconf-timezone-offset))
    (:id "sun-pm"
         :label "Sun Dec 4 afternoon"
         :start ,(concat "2022-12-04T13:00:00" emacsconf-timezone-offset)
         :end ,(concat "2022-12-04T17:30:00" emacsconf-timezone-offset))))

(defun emacsconf-filter-talks-by-time (start-time end-time info)
  "Return talks that are between START-TIME and END-TIME (inclusive) in INFO."
  (when (stringp start-time) (setq start-time (date-to-time start-time)))
  (when (stringp end-time) (setq end-time (date-to-time end-time)))
  (seq-filter (lambda (o)
                (and (not (time-less-p (plist-get o :start-time) end-time)) 
                     (not (time-less-p start-time (plist-get o :end-time)))))
              info))

(defun emacsconf-get-shift (time)
  "Return the shift that TIME is in."
  (unless (stringp time)
    (setq time (format-time-string "%Y-%m-%dT%H:%M:%S%z" time emacsconf-timezone)))
  (seq-find (lambda (shift)
              (and (not (string> time (plist-get shift :end)))
                   (not (string> (plist-get shift :start) time))))
            emacsconf-shifts))

(defun emacsconf-filter-talks-by-shift (time track info)
  "Return a list of talks that are in the shift specified by TIME.
Filter by TRACK if given.  Use INFO as the list of talks."
  (let* ((shift (emacsconf-get-shift time))
         (list (emacsconf-filter-talks-by-time (plist-get shift :start) (plist-get shift :end) info)))
    (if track
        (emacsconf-filter-talks-by-track track info)
      list)))

(defun emacsconf-talk-all-done-p (talk)
  (member (plist-get talk :status) (split-string "TO_ARCHIVE TO_EXTRACT TO_FOLLOW_UP DONE")))

(defun emacsconf-bbb-status (talk)
  (let ((states
         '((open . "OPEN_Q UNSTREAMED_Q")
           (before . "TODO TO_REVIEW TO_ACCEPT WAITING_FOR_PREREC TO_PROCESS TO_AUTOCAP TO_ASSIGN TO_CAPTION TO_STREAM PLAYING CLOSED_Q")
           (after . "TO_ARCHIVE TO_EXTRACT TO_FOLLOW_UP DONE"))))
    (if (string-match "live" (or (plist-get talk :q-and-a) ""))
        (or (car (seq-find (lambda (state)
                             (member (plist-get talk :status) (split-string (cdr state))))
                           states))
            (throw 'error "Unknown talk BBB state"))
      'irc)))

(defvar emacsconf-bbb-base-url "https://bbb.emacsverse.org/" "Include trailing slash.")
(defun emacsconf-bbb-room-title-list (&optional info)
  (delq nil
        (mapcar
         (lambda (o)
           (when (car o)
             (concat "ec" (substring emacsconf-year 2)
                     "-" (plist-get (emacsconf-get-shift (plist-get (cadr o) :start-time)) :id)
                     "-" (plist-get (emacsconf-get-track (plist-get (cadr o) :track)) :id)
                     " " (car o)
                     " ("
                     (mapconcat (lambda (talk) (plist-get talk :slug)) (cdr o) ", ")
                     ")")))
         (seq-group-by (lambda (o) (plist-get o :speakers))
                       (or info (emacsconf-active-talks (emacsconf-filter-talks (emacsconf-get-talk-info))))))))

(defun emacsconf-surround (before text after alternative)
  "Concat BEFORE, TEXT, and AFTER if TEXT is specified, or return ALTERNATIVE."
  (if (and text (not (string= text "")))
      (concat (or before "") text (or after ""))
    alternative))
;;

(provide 'emacsconf)
;;; emacsconf.el ends here
