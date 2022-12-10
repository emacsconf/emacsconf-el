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

(defcustom emacsconf-id "emacsconf"
  "ID of conference"
  :group 'emacsconf
  :type 'string)
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

(defcustom emacsconf-emergency-contact nil
  "Emergency contact information."
  :type 'string
  :group 'emacsconf)

(defvar emacsconf-stream-base "https://live0.emacsconf.org/")
(defvar emacsconf-chat-base "https://chat.emacsconf.org/")
(defvar emacsconf-backstage-dir "/ssh:orga@media.emacsconf.org:/var/www/media.emacsconf.org/2022/backstage")
(defvar emacsconf-upload-dir "/ssh:orga@media.emacsconf.org:/srv/upload")
(defvar emacsconf-res-dir (format "/ssh:orga@res.emacsconf.org:/data/emacsconf/%s" emacsconf-year))
(defvar emacsconf-media-extensions '("webm" "mkv" "mp4" "webm" "avi" "ts" "ogv" "wav" "ogg" "mp3"))
(defvar emacsconf-ftp-upload-dir "/ssh:orga@media.emacsconf.org:/srv/ftp/anon/upload-here")
(defvar emacsconf-backstage-user "emacsconf")
(defvar emacsconf-backstage-password nil "Password for backstage area")
(defvar emacsconf-notebook
  (expand-file-name
   "index.org"
   (expand-file-name "organizers-notebook"
                     (expand-file-name emacsconf-year emacsconf-directory))))

(defun emacsconf-prep-agenda ()
  (interactive)
  (let* ((org-agenda-custom-commands
         `(("a" "Agenda"
            ((tags-todo "-PRIORITY=\"C\"-SCHEDULED={.}-nextyear"
                        ((org-agenda-files (list ,emacsconf-notebook))
												 (org-agenda-sorting-strategy '(priority-down effort-up))))
             (agenda ""
                     ((org-agenda-files (list ,emacsconf-notebook))
                      (org-agenda-span 7)))
             )))))
    (org-agenda nil "a")))

(defun emacsconf-talk-agenda ())

(defun emacsconf-notebook-goto-custom-id (id)
	(interactive "MID: ")
	(find-file-other-window emacsconf-notebook)
	(goto-char (org-find-property "CUSTOM_ID" id)))

(defun emacsconf-ftp-upload-dired ()
  (interactive)
  (dired emacsconf-ftp-upload-dir "-tl"))
(defun emacsconf-upload-dired ()
  (interactive)
  (dired emacsconf-upload-dir "-tl"))
(defun emacsconf-backstage-dired ()
  (interactive)
  (dired emacsconf-backstage-dir "-tl"))
(defun emacsconf-res-dired () (interactive) (dired emacsconf-res-dir "-tl"))
(defun emacsconf-media-dired () (interactive) (dired emacsconf-public-media-directory "-tl"))
(defun emacsconf-cache-dired ()
  (interactive)
  (dired emacsconf-cache-dir "-tl"))
(defun emacsconf-slugify (s)
  (replace-regexp-in-string " +" "-" (replace-regexp-in-string "[^a-z0-9 ]" "" (downcase s))))

(defun emacsconf-video-slug (talk)
  (concat "emacsconf-" emacsconf-year "-" (plist-get talk :slug) "--"
          (emacsconf-slugify (plist-get talk :title))
          (if (plist-get talk :speakers)
              (concat"--"
                     (emacsconf-slugify (plist-get talk :speakers)))
            "")))



(defun emacsconf-set-video-slug-if-needed (o)
  (interactive (list (emacsconf-complete-talk-info)))
  (unless (plist-get o :video-slug)
    (let ((video-slug
           (read-string "Set video slug: " (emacsconf-video-slug o))))
      (save-window-excursion
        (emacsconf-with-talk-heading (plist-get o :slug)
          (org-entry-put (point) "VIDEO_SLUG" video-slug)))
      (plist-put o :video-slug video-slug)))
  (plist-get o :video-slug))

(defun emacsconf-set-video-slugs ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-entry-put (point) "VIDEO_SLUG" (emacsconf-video-slug (emacsconf-get-talk-info-for-subtree))))
   "SLUG={.}-VIDEO_SLUG={.}"))

(defun emacsconf-upload-to-backstage ()
  (interactive)
  (mapc (lambda (file)
          (copy-file file (expand-file-name (file-name-nondirectory file)
                                            emacsconf-backstage-dir)
                     t)
          (when (and emacsconf-cache-dir
                     (not (string= (expand-file-name (file-name-nondirectory file) default-directory)
                                   (expand-file-name (file-name-nondirectory file) emacsconf-cache-dir))))
            (copy-file file (expand-file-name (file-name-nondirectory file)
                                              emacsconf-cache-dir)
                       t)))
        (or (dired-get-marked-files)
            (list (buffer-file-name)))))

(defun emacsconf-get-srv2-and-upload-to-backstage (talk)
  (interactive (list (emacsconf-complete-talk-info (seq-filter (lambda (o) (plist-get o :youtube-url)) (emacsconf-get-talk-info)))))
  (let ((filename (make-temp-file nil nil "srv2"))
        (buf (get-buffer-create "*test*")))
    (when (plist-get talk :youtube-url)
      (call-process "yt-dlp" nil buf t "--write-sub" "--write-auto-sub" "--no-warnings" "--sub-lang" "en" "--skip-download" "--sub-format" "srv2" "-o" filename
                    (plist-get talk :youtube-url))
      (with-current-buffer (find-file-noselect (concat filename ".en.srv2"))
        (emacsconf-upload-to-backstage-and-rename talk "main--srt")))))

(defun emacsconf-upload-to-backstage-and-rename (talk &optional filename)
  (interactive (list (emacsconf-complete-talk-info)))
  (mapc (lambda (file)
          (let ((new-file (or filename (read-string (format "Filename (%s): " (file-name-base file))))))
            (copy-file file
                       (expand-file-name (concat (plist-get talk :video-slug)
                                                 (if (string= new-file "")
                                                     ""
                                                   (concat "--" new-file))
                                                 "."
                                                 (file-name-extension file))
                                         emacsconf-backstage-dir)
                       t)))
        (or (dired-get-marked-files) (list (buffer-file-name)))))

(defun emacsconf-upload-copy-from-json (talk key filename)
  (interactive (let-alist (json-parse-string (buffer-string) :object-type 'alist)
                 (list (emacsconf-complete-talk-info)
                       .metadata.key
                       (read-string (format "Filename: ")))))
  (let ((new-filename (concat (plist-get talk :video-slug)
                              (if (string= filename "")
                                  filename
                                (concat "--" filename))
                              "."
                              (let-alist (json-parse-string (buffer-string) :object-type 'alist)
                                (file-name-extension .metadata.name)))))
    (copy-file key
               (expand-file-name new-filename
                                 emacsconf-backstage-dir) t)
    ;; (copy-file key (expand-file-name new-filename emacsconf-cache-dir))
    ;; (unless (file-directory-p (expand-file-name (plist-get talk :slug) emacsconf-res-dir))
    ;;   (make-directory (expand-file-name (plist-get talk :slug) emacsconf-res-dir)))
    ;; (copy-file (expand-file-name new-filename emacsconf-cache-dir)
    ;;            (expand-file-name new-filename (expand-file-name (plist-get talk :slug) emacsconf-res-dir)))
    ))

(defcustom emacsconf-download-directory "~/Downloads"
  "Directory to check for downloaded files."
  :type 'directory
  :group 'emacsconf)

(defun emacsconf-latest-file (path &optional filter)
  "Return the newest file in PATH. Optionally filter by FILTER."
  (car (sort (seq-remove #'file-directory-p (directory-files path 'full filter t)) #'file-newer-than-file-p)))

(defun emacsconf-find-captions-from-slug (search)
  (interactive (list (emacsconf-complete-talk)))
  (emacsconf-with-talk-heading search (emacsconf-subed-find-captions)))

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

(defvar emacsconf-complete-talk-cache nil)
;; (setq emacsconf-complete-talk-cache (mapcar (lambda (o) (string-join (delq nil (mapcar (lambda (f) (plist-get o f)) '(:slug :title :speakers :irc))) " - ")) (emacsconf-get-talk-info)))

(defun emacsconf-complete-talk (&optional info)
  (let ((choices
				 (if (and (null info) emacsconf-complete-talk-cache)
						 emacsconf-complete-talk-cache
					 (mapcar (lambda (o)
										 (string-join
											(delq nil
														(mapcar (lambda (f) (plist-get o f)) '(:slug :title :speakers :irc)))
											" - "))
									 (or info (emacsconf-get-talk-info))))))
    (completing-read
     "Talk: " 
     (lambda (string predicate action)
       (if (eq action 'metadata)
           '(metadata (category . emacsconf))
         (complete-with-action action choices string predicate))))))

(defun emacsconf-complete-talk-info (&optional info)
  (emacsconf-search-talk-info (emacsconf-complete-talk info) info))

(defun emacsconf-get-slug-from-string (search)
  (when (listp search) (setq search (car search)))
  (cond
	 ((and search (stringp search) (string-match "\\(.*?\\) - " search))
		(match-string 1 search))
	 ((and (stringp search) (string-match (concat "^" emacsconf-id "-" emacsconf-year "-\\(.+?\\)--") search))
		(match-string 1 search))
   (t search)))

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
    ("PROCESSING" . "Processing uploaded video")
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
    ("CANCELLED" . "Sorry, this talk has been cancelled")))

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
                       (:video-time "VIDEO_TIME")                       
                       (:video-file-size "VIDEO_FILE_SIZE")                       
                       (:video-duration "VIDEO_DURATION")
                       (:stream-files "STREAM_FILES")
                       (:youtube-url "YOUTUBE_URL")                       
                       (:toobnix-url "TOOBNIX_URL")
											 (:intro-time "INTRO_TIME")
                       ;; Captioning
                       (:captioner "CAPTIONER")
                       (:caption-note "CAPTION_NOTE")
                       (:captions-edited "CAPTIONS_EDITED")
                       ;; Conference
                       (:check-in "CHECK_IN")
                       (:public "PUBLIC")
                       (:intro-note "INTRO_NOTE")
                       (:hyperlist-note "HYPERLIST_NOTE")
                       ;; Extraction
                       (:qa-youtube "QA_YOUTUBE")
                       (:qa-toobnix "QA_TOOBNIX")
											 (:bbb-playback "BBB_PLAYBACK")
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
  (if (or
			 (member (plist-get o :status)
							 (split-string "PLAYING CLOSED_Q OPEN_Q UNSTREAMED_Q TO_ARCHIVE TO_EXTRACT TO_FOLLOW_UP DONE"))
			 (time-less-p (plist-get o :start-time)
										(current-time)))
      (plist-put o :public t))
  o)

(defun emacsconf-talk-live-p (talk)
  "Return non-nil if TALK is ready to be published."
  (plist-get talk :public))

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
    emacsconf-add-checkin-time
    emacsconf-add-timezone-conversions
    emacsconf-add-speakers-with-pronouns
    emacsconf-add-live-info))
(defun emacsconf-add-speakers-with-pronouns (o)
  (plist-put o :speakers-with-pronouns
             (cond
              ((null (plist-get o :pronouns)) (plist-get o :speakers))
              ((string= (plist-get o :pronouns) "nil") (plist-get o :speakers))
              ((string-match "(" (plist-get o :pronouns)) (plist-get o :pronouns))
              (t (format "%s (%s)" (plist-get o :speakers) (plist-get o :pronouns)))))
  o)

(defun emacsconf-add-checkin-time (o)
  (unless (or (null (plist-get o :status))
              (null (plist-get o :email))
              (string= (plist-get o :status) "CANCELLED")
              (string-match "after" (plist-get o :q-and-a)))
    (if (null (plist-get o :video-file))
				(progn
					(plist-put o :live-time (plist-get o :start-time))
					(plist-put o :qa-time (plist-get o :live-time))
					(plist-put
					 o :checkin-label
					 "1 hour before the scheduled start of your talk, since you don't have a pre-recorded video")
					(plist-put
					 o :checkin-time
					 (seconds-to-time (time-subtract (plist-get o :start-time) (seconds-to-time 3600)))))
			(plist-put o :live-time
                 (seconds-to-time
									(+
									 (time-to-seconds (plist-get o :start-time))
									 (* 60 (string-to-number (or (plist-get o :video-time) "0")))
									 (* 60 (string-to-number (or (plist-get o :intro-time) "0")))
									 )))
			(unless (string-match "none\\|after" (or (plist-get o :q-and-a) "none"))
				(plist-put o :qa-time
									 (plist-get o :live-time)))      
      (plist-put o :checkin-label
                 "30 minutes before the scheduled start of your Q&A, since you have a pre-recorded video")
      (when (plist-get o :video-time)
        (plist-put o :checkin-time
                   (seconds-to-time
										(time-subtract (time-add (plist-get o :start-time)
																						 (seconds-to-time (* 60 (string-to-number (plist-get o :video-time)))))
																	 (seconds-to-time (/ 3600 2))))))))
  o)

(require 'emacsconf-pad)
(defun emacsconf-add-live-info (o)
  (plist-put o :absolute-url (concat emacsconf-base-url (plist-get o :url)))
  (plist-put o :in-between-url (format "%s%s/in-between/%s.png"
																			 emacsconf-media-base-url
																			 emacsconf-year
																			 (plist-get o :slug)))
  (plist-put o :qa-slide-url (format "%s%s/in-between/%s.png"
                                     emacsconf-media-base-url
                                     emacsconf-year
                                     (plist-get o :slug)))
	(plist-put o :intro-expanded (emacsconf-pad-expand-intro o))
  (let ((track (emacsconf-get-track (plist-get o :track))))
    (when track
      (plist-put o :watch-url (concat emacsconf-base-url emacsconf-year "/watch/" (plist-get track :id)))
      (plist-put o :webchat-url (concat emacsconf-chat-base "?join=emacsconf,"
                                        (replace-regexp-in-string "#" ""
                                                                  (plist-get track :channel))))
			(plist-put o :track-id (plist-get track :id))) 
		(plist-put o :channel (plist-get track :channel))
    (plist-put o :bbb-backstage (concat emacsconf-media-base-url emacsconf-year "/backstage/current/room/" (plist-get o :slug)))
    (cond
     ((string= (or (plist-get o :q-and-a) "") "")
      (plist-put o :qa-info "none")
      (plist-put o :qa-link "none"))
     ((string-match "live" (plist-get o :q-and-a))
      (plist-put o :bbb-redirect (format "https://emacsconf.org/current/%s/room/" (plist-get o :slug)))
      (plist-put o :qa-info (plist-get o :bbb-redirect))
      (plist-put o :qa-link (format "<a href=\"%s\">BBB</a>" (plist-get o :bbb-redirect))))
     ((string-match "IRC" (plist-get o :q-and-a))
      (plist-put o :qa-info (concat "#" (plist-get o :channel) 
                                    (emacsconf-surround ", speaker nick: " (plist-get o :irc) "")))
      (plist-put o :qa-link (format "<a href=\"%s\">%s</a>" (plist-get o :webchat-url) (plist-get o :qa-info))))
     ((string-match "Mumble" (plist-get o :q-and-a))
      (plist-put o :qa-info "Moderated via Mumble, ask questions via pad or IRC")
      (plist-put o :qa-link (format "<a href=\"%s\">%s</a>" (plist-get o :webchat-url) (plist-get o :qa-info))))
     ((string-match "pad" (plist-get o :q-and-a))
      (plist-put o :qa-info "Etherpad")
      (plist-put o :qa-link (format "<a href=\"%s\">%s</a>"
                                    (plist-get o :pad-url)
                                    (plist-get o :qa-info))))
     (t (plist-put o :qa-info "none")
        (plist-put o :qa-link "none")))
    (plist-put o :pad-url (format "https://pad.emacsconf.org/%s-%s" emacsconf-year (plist-get o :slug)))
    (plist-put o :recorded-intro
               (let ((filename
                      (expand-file-name (concat (plist-get o :slug) ".webm")
                                        (expand-file-name "intros" emacsconf-stream-asset-dir))))
                 (and (file-exists-p filename) filename)))
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
  (let* ((info (emacsconf-prepare-for-display info))
         (cur-list info))
    ;; add links to the next talks
    (while cur-list
      (plist-put (pop cur-list) :next-talks (seq-take cur-list number)))
    info))

(defun emacsconf-previous-talk (talk &optional info)
	(setq info (emacsconf-prepare-for-display (or info (emacsconf-get-talk-info))))
	(let* ((pos (seq-position info talk))
				 (prev (and pos
										(> pos 0)
										(elt info (1- pos)))))
		(and prev
				 (string= (format-time-string "%Y-%m-%d" (plist-get prev :start-time) emacsconf-timezone)
									(format-time-string "%Y-%m-%d" (plist-get talk :start-time) emacsconf-timezone))
				 prev)))
;; (emacsconf-previous-talk (emacsconf-resolve-talk "lspbridge"))

(defun emacsconf-resolve-talk (talk)
  "Return the plist for TALK."
  (if (stringp talk) (emacsconf-find-talk-info talk) talk))

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
                                                          '(:slug :title :speakers :pronouns :pronunciation :url :track :video-slug)))
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
              (emacsconf-pad-base . etherpad_url)
              (emacsconf-backstage-password . emacsconf_backstage_password))))))
;; (emacsconf-ansible-load-vars (expand-file-name "prod-vars.yml" emacsconf-ansible-directory))
;;; Tracks
(defvar emacsconf-tracks
  `((:name "General" :color "peachpuff" :id "gen" :channel "emacsconf-gen"
           :watch "https://live.emacsconf.org/2022/watch/gen/"
				   :tramp "/ssh:emacsconf-gen@res.emacsconf.org#46668:"
           :webchat-url "https://chat.emacsconf.org/?join=emacsconf,emacsconf-org,emacsconf-accessible,emacsconf-dev,emacsconf-gen"
           :stream ,(concat emacsconf-stream-base "gen.webm")
           :480p ,(concat emacsconf-stream-base "gen-480p.webm")
					 :youtube-url "https://www.youtube.com/watch?v=UEJ88c7MJq0"
					 :youtube-studio-url "https://studio.youtube.com/video/UEJ88c7MJq0/livestreaming"
					 :toobnix-url "https://toobnix.org/w/7t9X8eXuSby8YpyEKTb4aj"
           :start "09:00" :end "17:00"
           :vnc-display ":5"
           :vnc-port "5905"
           :status "offline")
   (:name "Development" :color "skyblue" :id "dev" :channel "emacsconf-dev"
          :watch "https://live.emacsconf.org/2022/watch/dev/"
				  :webchat-url "https://chat.emacsconf.org/?join=emacsconf,emacsconf-org,emacsconf-accessible,emacsconf-gen,emacsconf-dev"
          :tramp "/ssh:emacsconf-dev@res.emacsconf.org#46668:"
					:toobnix-url "https://toobnix.org/w/w6K77y3bNMo8xsNuqQeCcD"
					:youtube-url "https://www.youtube.com/watch?v=PMaoF-xa1b4"
					:youtube-studio-url "https://studio.youtube.com/video/PMaoF-xa1b4/livestreaming"
					:stream ,(concat emacsconf-stream-base "dev.webm")
          :480p ,(concat emacsconf-stream-base "dev-480p.webm")
          :start "10:00" :end "17:00"
          :vnc-display ":6"
          :vnc-port "5906"
          :status "offline")))

(defun emacsconf-get-track (name)
  (when (and (listp name) (plist-get name :track))
		(setq name (plist-get name :track)))
	(if (stringp name)
			(or
			 (seq-find (lambda (track) (or (string= name (plist-get track :name))
																		 (string= name (plist-get track :id))))
								 emacsconf-tracks)
			 (let ((talk (emacsconf-resolve-talk name)))
				 (seq-find (lambda (track) (or (string= (plist-get talk :track) (plist-get track :name))
																		 (string= (plist-get talk :track) (plist-get track :id))))
									 emacsconf-tracks))
			 name)
		name))

(defun emacsconf-by-track (info)
  (mapcar (lambda (track)
            (seq-filter
             (lambda (talk)
               (string= (plist-get talk :track) (plist-get track :name)))
             info))
          emacsconf-tracks))

(defun emacsconf-complete-track (&optional prompt tracks)
  (emacsconf-get-track
	 (completing-read
		(or prompt "Track: ")
		(mapcar (lambda (o) (plist-get o :name)) (or tracks emacsconf-tracks)))))

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
	(list (list :id "sat-am-gen" :track "General" :start "2022-12-03T08:00:00-0500" :end "2022-12-03T12:00:00-0500" :host "zaeph" :streamer "sachac" :checkin "corwin" :irc "dto" :pad "publicvoit" :coord "sachac") (list :id "sat-pm-gen" :track "General" :start "2022-12-03T13:00:00-0500" :end "2022-12-03T18:00:00-0500" :host "zaeph" :streamer "sachac" :checkin "FlowyCoder" :irc "bandali" :pad "publicvoit" :coord "sachac") (list :id "sat-am-dev" :track "Development" :start "2022-12-03T08:00:00-0500" :end "2022-12-03T12:00:00-0500" :host "bandali" :streamer "sachac" :checkin "corwin" :irc "dto" :coord "sachac") (list :id "sat-pm-dev" :track "Development" :start "2022-12-03T13:00:00-0500" :end "2022-12-03T18:00:00-0500" :host "bandali" :streamer "sachac" :checkin "FlowyCoder" :irc "bandali" :coord "sachac") (list :id "sun-am-gen" :track "General" :start "2022-12-04T08:00:00-0500" :end "2022-12-04T12:00:00-0500" :host "zaeph" :streamer "sachac" :checkin "corwin" :irc "dto" :pad "publicvoit" :coord "sachac") (list :id "sun-pm-gen" :track "General" :start "2022-12-04T13:00:00-0500" :end "2022-12-04T18:00:00-0500" :host "zaeph" :streamer "jman" :checkin "FlowyCoder" :irc "bandali" :pad "publicvoit" :coord "sachac") (list :id "sun-am-dev" :track "Development" :start "2022-12-04T08:00:00-0500" :end "2022-12-04T12:00:00-0500" :host "bandali" :streamer "sachac" :checkin "corwin" :irc "dto" :coord "sachac") (list :id "sun-pm-dev" :track "Development" :start "2022-12-04T13:00:00-0500" :end "2022-12-04T18:00:00-0500" :host "bandali" :streamer "sachac" :checkin "FlowyCoder" :irc "bandali" :coord "sachac")))

(defun emacsconf-filter-talks-by-time (start-time end-time info)
  "Return talks that are between START-TIME and END-TIME (inclusive) in INFO."
  (when (stringp start-time) (setq start-time (date-to-time start-time)))
  (when (stringp end-time) (setq end-time (date-to-time end-time)))
  (seq-filter (lambda (o)
                (and (plist-get o :start-time)
                     (time-less-p (plist-get o :start-time) end-time) 
                     (time-less-p start-time (plist-get o :end-time))))
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
           (before . "TODO TO_REVIEW TO_ACCEPT WAITING_FOR_PREREC TO_PROCESS PROCESSING TO_AUTOCAP TO_ASSIGN TO_CAPTION TO_STREAM PLAYING CLOSED_Q")
           (after . "TO_ARCHIVE TO_EXTRACT TO_FOLLOW_UP DONE")
           (cancelled . "CANCELLED"))))
    (if (string-match "live" (or (plist-get talk :q-and-a) ""))
        (or (car (seq-find (lambda (state)
                             (member (plist-get talk :status) (split-string (cdr state))))
                           states))
            (throw 'error "Unknown talk BBB state"))
      'irc)))

(defun emacsconf-captions-edited-p (filename)
  "Return non-nil if FILENAME has been edited and is okay for inclusion."
  (and (file-exists-p filename)
       (with-temp-buffer
         (insert-file-contents filename)
         (goto-char (point-min))
         (re-search-forward "captioned by" (line-end-position) t))))

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

(defun emacsconf-surround (before text after &optional alternative)
  "Concat BEFORE, TEXT, and AFTER if TEXT is specified, or return ALTERNATIVE."
  (if (and text (not (string= text "")))
      (concat (or before "") text (or after ""))
    alternative))

;;; Volunteer management
(defun emacsconf-get-volunteer-info ()
  (with-current-buffer (find-file-noselect emacsconf-org-file)
    (org-map-entries (lambda () (org-entry-properties))
                     "volunteer+EMAIL={.}")))

(defun emacsconf-volunteer-emails-for-completion (&optional info)
  (mapcar (lambda (o)
            (emacsconf-surround
             (if (assoc-default "ITEM" o)
                 (concat (assoc-default "ITEM" o) " <")
               "<")
             (assoc-default "EMAIL" o)
             ">" ""))
          (or info (emacsconf-get-volunteer-info))))

(defun emacsconf-complete-volunteer (&optional info)
  (setq info (or info (emacsconf-get-volunteer-info)))
  (let* ((choices
          (emacsconf-volunteer-emails-for-completion))
         (choice (completing-read
                  "Volunteer: " 
                  (lambda (string predicate action)
                    (if (eq action 'metadata)
                        '(metadata (category . volunteer))
                      (complete-with-action action choices string predicate))))))
    (elt info (seq-position choices choice))))

;;; Reflowing
(defun emacsconf-reflow ()
  "Help reflow text files."
  (interactive)
  (let (input last-input (case-fold-search t))
    (while (not (string= "" (setq input (read-string "Word: "))))
      (when (string= input "!")
        (delete-backward-char 1)
        (insert " ")
        (end-of-line)
        (re-search-forward (regexp-quote last-input) nil t)
        (setq input last-input))
      (if (string= input "'")
          (progn
            (end-of-line)          
            (unless (looking-back " ")
              (insert " "))          
            (delete-char 1))
        (forward-word)
        (cond
         ((string= input ",")
          (re-search-forward ", " nil t)
          (goto-char (match-end 0)))
         ((string= input ".")
          (re-search-forward "\\. " nil t)
          (goto-char (match-end 0)))
         (t
          (re-search-forward (concat "\\<" (regexp-quote input)) nil t)
          (goto-char (match-beginning 0))))
        (insert "\n")
        (when (< (- (line-end-position) (point)) fill-column)
          (save-excursion
            (goto-char (line-end-position))
            (insert " ")
            (delete-char 1)))
        (setq last-input input)
        (recenter)
        (undo-boundary)))))

(defmacro emacsconf-with-todo-hooks (&rest body)
  "Run BODY with the Emacsconf todo hooks."
  `(with-current-buffer (find-file-noselect emacsconf-org-file)
     (let ((org-after-todo-state-change-hook '(emacsconf-org-after-todo-state-change)))
       ,@body)))

(defun emacsconf-add-org-after-todo-state-change-hook ()
  "Add FUNC to `org-after-todo-stage-change-hook'."
  (interactive)
  (with-current-buffer (find-buffer-visiting emacsconf-org-file)
    (add-hook 'org-after-todo-state-change-hook #'emacsconf-org-after-todo-state-change nil t)))

(defun emacsconf-remove-org-after-todo-state-change-hook ()
  "Remove FUNC from `org-after-todo-stage-change-hook'."
  (interactive)
  (with-current-buffer (find-buffer-visiting emacsconf-org-file)
    (remove-hook 'org-after-todo-state-change-hook
                 #'emacsconf-org-after-todo-state-change  t)))

(defvar emacsconf-todo-hooks
  '((emacsconf-stream-play-talk-on-change "gen" "dev")
    (emacsconf-stream-open-qa-windows-on-change "gen" "dev")
    emacsconf-erc-announce-on-change ;; announce via ERC
    emacsconf-publish-bbb-redirect
    emacsconf-stream-update-talk-info-on-change
    emacsconf-publish-media-files-on-change
    ;; emacsconf-publish-update-talk  ;; skipping this for now, I'll do this locally
    emacsconf-publish-backstage-org-on-state-change ;; update the backstage index
    ;; write to the talk text
    )
  "Functions to run when the todo state changes.
They will be called with TALK.")

(defun emacsconf-org-after-todo-state-change ()
  "Run all the hooks in `emacsconf-todo-hooks'.
If an `emacsconf-todo-hooks' entry is a list, run it only for the
tracks with the ID in the cdr of that list."
  (let* ((talk (emacsconf-get-talk-info-for-subtree))
         (track (emacsconf-get-track (plist-get talk :track))))
    (mapc
     (lambda (hook-entry)
       (cond
        ((symbolp hook-entry) (funcall hook-entry talk))
        ((member (plist-get track :id) (cdr hook-entry))
         (funcall (car hook-entry) talk))))
     emacsconf-todo-hooks)))

(defun emacsconf-broadcast (message)
  (interactive "MMessage: ")
  (when (not (string= (or message "") ""))
    (erc-cmd-BROADCAST message))
  (emacsconf-stream-broadcast message))

(defun emacsconf-agenda ()
  (interactive)
  (let ((org-agenda-files (list emacsconf-org-file)))
    (org-agenda-list nil emacsconf-date 2)))

(defun emacsconf-track-agenda (track)
  (interactive (list (emacsconf-complete-track)))
  (when (stringp track) (setq track (emacsconf-get-track track)))
  (let ((org-agenda-files (list emacsconf-org-file))
        (org-agenda-category-filter-preset (list (concat "+" (plist-get track :id)))))
    (org-agenda-list nil emacsconf-date 2)))

(defun emacsconf-update-talk-status-with-hooks (slug from-states to-state)
	(interactive (list (emacsconf-complete-talk) "." (completing-read "To: " (mapcar 'car emacsconf-status-types))))
	(emacsconf-with-todo-hooks
	 (emacsconf-update-talk-status slug from-states to-state)))

(defun emacsconf-update-talk-status (slug from-states to-state)
  (interactive (list (emacsconf-complete-talk) "." (completing-read "To: " (mapcar 'car emacsconf-status-types))))
  (emacsconf-with-talk-heading slug
    (when (string-match from-states (org-entry-get (point) "TODO"))
      (org-todo to-state)
      (save-buffer))))

;; copied from org-ascii--indent-string
(defun emacsconf-indent-string (s width)
  "Indent S by WIDTH spaces."
  (replace-regexp-in-string "\\(^\\)[ \t]*\\S-" (make-string width ?\s) s nil nil 1))

(defun emacsconf-add-to-logbook (note)
  "Add NOTE as a logbook entry for the current subtree."
  (move-marker org-log-note-return-to (point))
  (move-marker org-log-note-marker (point))
  (with-temp-buffer
    (insert note)
    (let ((org-log-note-purpose 'note))
      (org-store-log-note))))

(defun emacsconf-add-to-talk-logbook (talk note)
  "Add NOTE as a logbook entry for TALK."
  (interactive (list (emacsconf-complete-talk) (read-string "Note: ")))
  (save-excursion
    (emacsconf-with-talk-heading talk
      (emacsconf-add-to-logbook note))))

(defun emacsconf-reload ()
  "Reload the emacsconf-el modules."
  (interactive)
  (mapc #'load-library '("emacsconf" "emacsconf-erc" "emacsconf-publish" "emacsconf-stream" "emacsconf-pad")))

(defun emacsconf-find-talk-file-in-cache (talk filename)
  (interactive (let ((talk (emacsconf-complete-talk-info)))
                 (list
                  talk
                  (completing-read "File: " (directory-files emacsconf-cache-dir t (plist-get talk :video-slug))))))
  (find-file filename))

(defun emacsconf-cache-find-file (filename)
  (interactive (list (read-file-name "File: " (expand-file-name "./" emacsconf-cache-dir) nil t)))
  (find-file (expand-file-name filename emacsconf-cache-dir)))

(defun emacsconf-format-seconds (seconds)
	(concat (format-seconds "%.2m:%.2s" (floor seconds))
					"." (format "%03d" (% (floor (* 1000 seconds)) 1000))))

(defun emacsconf-insert-time-for-speaker (talk)
	(interactive (list (emacsconf-complete-talk-info)))
	(insert
	 (format-time-string "%-I:%M %P %Z" (plist-get talk :start-time) emacsconf-timezone)
	 " (" emacsconf-timezone ")"
	 (if (string= (format-time-string "%z" (plist-get talk :start-time) (plist-get talk :timezone))
								emacsconf-timezone-offset)
			 ""
		 (concat
			" which should be the same as "
			(format-time-string "%-I:%M %P %Z" (plist-get talk :start-time) (plist-get talk :timezone))
			" in "
			(plist-get talk :timezone)))))
(provide 'emacsconf)
;;; emacsconf.el ends here
