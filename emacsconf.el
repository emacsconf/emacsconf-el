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
(defcustom emacsconf-year "2021"
  "Conference year. String for easy inclusion."
  :group 'emacsconf
  :type 'string)
(defcustom emacsconf-directory "~/vendor/emacsconf-wiki"
  "Directory where the wiki files are."
  :group 'emacsconf
  :type 'directory)
(defcustom emacsconf-timezones '("America/Toronto" "America/Los_Angeles" "UTC" "Europe/Paris" "Europe/Athens" "Asia/Kolkata" "Asia/Singapore" "Asia/Tokyo") "List of timezones."
  :group 'emacsconf
  :type '(repeat string))

(defcustom emacsconf-base-url "https://emacsconf.org/" "Includes trailing slash"
  :group 'emacsconf
  :type 'string)
(defcustom emacsconf-publishing-phase 'resources
  "Controls what information to include.
'program - don't include times
'schedule - include times; use this leading up to the conference
'resources - after the emacsconference, don't need status"
  :group 'emacsconf
  :type '(choice
          (const :tag "Program: Don't include times" program)
          (const :tag "Schedule: Include detailed times" schedule)
          (const :tag "Resources: Don't include status" resources)))

(defcustom emacsconf-org-file nil
  "Path to the Org file with conference information."
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


(defun emacsconf-get-slug-from-string (search)
  (if (listp search) (setq search (car search)))
  (if (and search (string-match "\\(.*?\\) - " search))
      (match-string 1 search)
    search))
(defun emacsconf-go-to-talk (&optional search)
  (interactive (list (emacsconf-complete-talk)))
  (when search
    (setq search (emacsconf-get-slug-from-string search))
    (pop-to-buffer (find-file-noselect emacsconf-org-file))
    (let ((choices
           (save-excursion
             (delq nil
                   (org-map-entries
                    (lambda ()
                      (when (org-entry-get (point) "SLUG")
                        (cons
                         (concat (org-entry-get (point) "SLUG") " - "
                                 (org-entry-get (point) "ITEM") " - "
                                 (org-entry-get (point) "NAME") " - "
                                 (org-entry-get (point) "EMAIL"))
                         (point)))))))))
      (goto-char
       (if search
           (or (org-find-property "SLUG" search)
               (cdr (seq-find (lambda (s) (string-match search (car s))) choices)))
         (assoc-default (completing-read "Find: " choices)
                        choices)))
      (org-reveal))))

(defmacro emacsconf-with-talk-heading (search &rest body)
  (declare (indent 1) (debug t))
  `(progn
     (emacsconf-go-to-talk ,search)
     ,@body))

(defun emacsconf-status-types ()
  ;; TODO
  )

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
                       (:duration "TIME")
                       (:q-and-a "Q_AND_A")
                       (:bbb-room "ROOM")
                       (:irc "IRC")
                       (:intro-note "INTRO_NOTE")
                       (:check-in "CHECK_IN")
                       (:contact "CONTACT")
                       (:captioner "CAPTIONER")
                       (:youtube-url "YOUTUBE_URL")
                       (:toobnix-url "TOOBNIX_URL")
                       (:pronunciation "PRONUNCIATION")
                       (:pronouns "PRONOUNS")
                       (:buffer "BUFFER")
                       (:time "MIN_TIME")
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

(defun emacsconf-get-abstract-from-wiki (o)
  (plist-put o :markdown (emacsconf-talk-markdown-from-wiki (plist-get o :slug))))

(defun emacsconf-add-talk-status (o)
  (plist-put o :status-label
             (assoc-default (plist-get o :status) 
                            (emacsconf-status-types) 'string= "")))

(defvar emacsconf-talk-info-functions '(emacsconf-get-talk-info-from-properties emacsconf-add-talk-status))

(defun emacsconf-get-talk-info-for-subtree ()
  (seq-reduce (lambda (prev val) (funcall val prev))
              emacsconf-talk-info-functions
              nil))

(defun emacsconf-get-talk-info (&optional description-source)
  (with-current-buffer (find-file-noselect emacsconf-org-file)
    (save-excursion
      (let (talk results (status-types (emacsconf-status-types)))
        (org-map-entries
         (lambda ()
           (when (or (org-entry-get (point) "SLUG")
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

(defun emacsconf-goto-talk-id (id)
  (goto-char (org-find-property "TALK_ID" id)))

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
  (let ((a attrs))
    (while a
      (setq string
            (replace-regexp-in-string (regexp-quote (concat "${" (substring (symbol-name (pop a)) 1) "}"))
                                      (pop a)
                                      string t t)))
    string))

(provide 'emacsconf)
;;; emacsconf.el ends here
