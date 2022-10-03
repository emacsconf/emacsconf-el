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


(defcustom emacsconf-timezone "America/Toronto" "Main timezone."
  :group 'emacsconf
  :type 'string)

(defcustom emacsconf-timezones '("America/Toronto" "America/Los_Angeles" "UTC" "Europe/Paris" "Europe/Athens" "Asia/Kolkata" "Asia/Singapore" "Asia/Tokyo") "List of timezones."
  :group 'emacsconf
  :type '(repeat string))

(defcustom emacsconf-base-url "https://emacsconf.org/" "Includes trailing slash"
  :group 'emacsconf
  :type 'string)
(defcustom emacsconf-publishing-phase 'program
  "Controls what information to include.
'program - don't include times
'schedule - include times; use this leading up to the emacsconference
'resources - after the emacsconference, don't need status"
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
  (browse-url (concat emacsconf-base-url "/" emacsconf-year "/talks/" search "/")))

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
      (t path))))

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

(defun emacsconf-get-slug-from-string (search)
  (if (listp search) (setq search (car search)))
  (if (and search (string-match "\\(.*?\\) - " search))
      (match-string 1 search)
    search))

(defun emacsconf-go-to-talk (search)
  (interactive (list (emacsconf-complete-talk)))
  (pop-to-buffer (find-file-noselect emacsconf-org-file))
  (if (emacsconf-get-slug-from-string search)
      (goto-char (org-find-property "SLUG" (emacsconf-get-slug-from-string search)))
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
           (throw 'found)))
       "SLUG={.}")))
  (org-reveal))

(defmacro emacsconf-for-each-talk (&rest body)
  (declare (indent 0) (debug t))
  `(org-map-entries (lambda () ,@body) "SLUG={.}"))

(defmacro emacsconf-with-talk-heading (search &rest body)
  (declare (indent 1) (debug t))
  `(progn
     (emacsconf-go-to-talk ,search)
     ,@body))

(defun emacsconf-status-types ()
  ;; TODO
  )

(defun emacsconf-get-talk-categories (o)
  (org-narrow-to-subtree)
  (let (list)
    (while (re-search-forward "Category[^ \t\n]+" nil t)
      (setq list (cons (match-string-no-properties 0) list)))
    (plist-put o :categories (reverse list))))

(defun emacsconf-get-talk-info-from-properties (o)
  (let ((heading (org-heading-components))
        (field-props '((:title "ITEM")
                       (:talk-id "TALK_ID")
                       (:slug "SLUG")
                       (:video-slug "VIDEO_SLUG")
                       (:public "PUBLIC")
                       (:qa-public "QA_PUBLIC")
                       (:scheduled "SCHEDULED")
                       (:uuid "UUID")
                       (:email "EMAIL")
                       (:caption-note "CAPTION_NOTE")
                       (:availability "AVAILABILITY")
                       (:q-and-a "Q_AND_A")
                       (:bbb-room "ROOM")
                       (:irc "IRC")
                       (:intro-note "INTRO_NOTE")
                       (:check-in "CHECK_IN")
                       (:contact "CONTACT")
                       (:captioner "CAPTIONER")
                       (:youtube-url "YOUTUBE_URL")
                       (:toobnix-url "TOOBNIX_URL")
                       (:qa-youtube "QA_YOUTUBE")
                       (:qa-toobnix "QA_TOOBNIX")
                       (:pronunciation "PRONUNCIATION")
                       (:pronouns "PRONOUNS")
                       (:public-email "PUBLIC_EMAIL")
                       (:buffer "BUFFER")
                       (:duration "TIME")
                       (:time "TIME")
                       (:min-time "MIN_TIME")
                       (:max-time "MAX_TIME")
                       (:fixed-time "FIXED_TIME")
                       (:present "PRESENT")
                       (:speakers "NAME")
                       (:speakers-short "NAME_SHORT")
                       (:video-file "VIDEO_FILE")
                       (:video-file-size "VIDEO_FILE_SIZE")
                       (:video-duration "VIDEO_DURATION")
                       (:alternate-apac "ALTERNATE_APAC")
                       (:extra-live-time "EXTRA_LIVE_TIME"))))
    (apply
     'append
     o
     (list
      :point (point)
      :year emacsconf-year
      :type (if (org-entry-get (point) "SLUG") 'talk 'headline)
      :status (elt heading 2)
      :level (car heading)
      :url (concat emacsconf-base-url emacsconf-year "/talks/" (org-entry-get (point) "SLUG"))
      :schedule-group 
      (org-entry-get-with-inheritance "SCHEDULE_GROUP")
      :wiki-file-path (expand-file-name 
                       (concat (org-entry-get (point) "SLUG") ".md")
                       (expand-file-name "captions" (expand-file-name emacsconf-year emacsconf-directory)))
      :conf-year emacsconf-year
      :start-time (when (org-entry-get (point) "SCHEDULED")
                    (org-timestamp-to-time
                     (org-timestamp-split-range
                      (org-timestamp-from-string
                       (org-entry-get (point) "SCHEDULED")))))
      :end-time (when (org-entry-get (point) "SCHEDULED")
                  (org-timestamp-to-time
                   (org-timestamp-split-range
                    (org-timestamp-from-string
                     (org-entry-get (point) "SCHEDULED"))
                    t))))
     (mapcar 
      (lambda (o) (list (car o) (org-entry-get (point) (cadr o))))
      field-props))))

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
             (assoc-default (plist-get o :status) 
                            (emacsconf-status-types) 'string= "")))

(defvar emacsconf-talk-info-functions
  '(emacsconf-get-talk-info-from-properties
    emacsconf-get-talk-categories
    emacsconf-get-talk-abstract-from-subtree
    emacsconf-add-talk-status
    emacsconf-add-timezone-conversions))

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
     ((time-less-p time-a time-b) t)
     ((time-less-p time-b time-a) nil)
     (t (< (plist-get a :point) (plist-get b :point))))))

(defun emacsconf-get-talk-info ()
  (with-current-buffer (find-file-noselect emacsconf-org-file)
    (save-excursion
      (let (results)
        (org-map-entries
         (lambda ()
           (when (or (org-entry-get (point) "TIME")
                     (org-entry-get (point) "SLUG")
                     (org-entry-get (point) "INCLUDE_IN_INFO"))
             (setq results
                   (cons (emacsconf-get-talk-info-for-subtree)
                         results)))))
        (nreverse results)))))

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

;;; Mail merge

(defun emacsconf-mail-speaker (&optional subject body)
  "Compose a message to the speaker of the current talk."
  (interactive)
  (compose-mail (format "%s <%s>" (org-entry-get (point) "NAME") (org-entry-get (point) "EMAIL")) subject)
  (when body (message-goto-body) (insert body)))

(defun emacsconf-mail-speaker-schedule (&optional subject body)
  (interactive (list (read-string "Subject: ") nil))
  (let ((info (emacsconf-get-talk-info-for-subtree)))
    (emacsconf-mail-speaker subject body)
    (when body (message-goto-body) (insert body))
    (goto-char (point-max))
    (insert (string-join (emacsconf-timezone-strings info) "\n"))))

(defvar emacsconf-submit-email "emacsconf-submit@gnu.org" "E-mail address for submissions.")

(defun emacsconf-mail-speaker-cc-submit (&optional subject body)
  "Compose a message to the speaker of the current talk."
  (interactive)
  (compose-mail (format "%s <%s>" (org-entry-get (point) "NAME") (org-entry-get (point) "EMAIL"))
                subject '(("Reply-To" . emacsconf-submit-email) ("Cc" . emacsconf-submit-email)))
  (message-goto-body)
  (when body (insert body))
  (save-excursion (insert "Please keep " emacsconf-submit-email " in the To: or Cc: when replying. Thank you!")))

(defun emacsconf-show-talk-info-for-mail ()
  (interactive)
  (let ((email (or (mail-fetch-field "reply-to") (mail-fetch-field "from"))))
    (when (string-match "<\\(\\(\\sw\\|\\s_\\|\\s.\\)+@\\(\\sw\\|\\s_\\|\\s.\\)+\\)>" email)
      (setq email (match-string 1 email)))
    (pop-to-buffer (find-file-noselect emacsconf-org-file))
    (goto-char (point-min))
    (goto-char
     (or (org-find-property "EMAIL" email)
         (org-find-property "NAME"
                            (completing-read "Name: " (delq nil (org-map-entries (lambda () (org-entry-get "NAME"))))))))))

(defun emacsconf-mail-merge-wrap ()
  (interactive)
  (with-undo-amalgamate 
    (save-excursion
      (while (re-search-forward " *${wrap}" nil t)
        (replace-match "")
        (fill-paragraph)))))

(defun emacsconf-mail-merge-get-template (id)
  "Return the information for the e-mail template with EMAIL_ID set to ID."
  (save-excursion
    (goto-char (org-find-property "EMAIL_ID" id))
    (list :subject (org-entry-get-with-inheritance "SUBJECT")
          :cc (org-entry-get-with-inheritance "CC")
          :reply-to (or (org-entry-get-with-inheritance "REPLY_TO") (org-entry-get-with-inheritance "REPLY-TO"))
          :mail-followup-to (or (org-entry-get-with-inheritance "MAIL_FOLLOWUP_TO")
                                (org-entry-get-with-inheritance "MAIL-FOLLOWUP-TO"))
          :body (replace-regexp-in-string "\n *," "\n" (buffer-substring-no-properties
                                                        (progn (org-end-of-meta-data) (point))
                                                        (org-end-of-subtree))))))

(defun emacsconf-mail-merge-fill (string)
  "Fill in the values for STRING using the properties at point.
Include some other things, too, such as emacsconf-year, title, name, email, url, and duration."
  (let (start (values `(("year" . ,emacsconf-year)
                  ("title" . ,(org-entry-get (point) "ITEM"))
                  ("name" . ,(org-entry-get (point) "NAME"))
                  ("email" . ,(org-entry-get (point) "EMAIL"))
                  ("url" . ,(format "%s%s/talks/%s" emacsconf-base-url emacsconf-year (org-entry-get (point) "SLUG")))
                  ("duration" . ,(org-entry-get (point) "TIME")))))
    (while (string-match "\\${\\([-a-zA-Z_]+?\\)}" string start)
      (if (assoc-default (match-string 1 string) values)
          (setq string (replace-match (assoc-default (match-string 1 string) values) t t string))
        (setq string (replace-match (save-match-data (org-entry-get (point) (match-string 1 string))) t t string)))
      (setq start (1+ (match-beginning 0))))
    string))

(defun emacsconf-mail-merge-format-email-address-for-subtree ()
  (if (string-match  "," (org-entry-get (point) "EMAIL"))
      (org-entry-get (point) "EMAIL")
    (format "%s <%s>" (org-entry-get (point) "NAME") (org-entry-get (point) "EMAIL"))))

(defun emacsconf-mail-merge-for-subtree (id note-field)
  (let* ((template (emacsconf-mail-merge-get-template id))
         (body (emacsconf-mail-merge-fill (plist-get template :body)))
         (subject (emacsconf-mail-merge-fill (plist-get template :subject)))
         (note (org-entry-get (point) note-field)))
    (compose-mail (emacsconf-mail-merge-format-email-address-for-subtree)
                  subject
                  `(("Reply-To" . ,(plist-get template :reply-to))
                    ("Mail-Followup-To" . ,(plist-get template :mail-followup-to))
                    ("Cc" . ,(plist-get template :cc))))
    (message-goto-body)
    (save-excursion 
      (when note (insert "#+NOTE: " note "\n======== Delete above before sending =============\n\n"))
      (insert body))))

(defun emacsconf-cancel-mail-merge ()
  (interactive)
  (mapc (lambda (buffer)
          (when (string-match "unsent" (buffer-name buffer))
            (let ((kill-buffer-query-functions nil)
                  (buffer-modified-p nil))
              (kill-buffer buffer))))
        (buffer-list)))

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



(provide 'emacsconf)
;;; emacsconf.el ends here
