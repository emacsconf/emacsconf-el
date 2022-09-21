;;; emacsconf-ical.el ---  ical export  -*- lexical-binding: t; -*-

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

(defun emacsconf-ical-send-via-email ()
  (interactive)
  (let ((ical-file (expand-file-name
                    (concat (org-entry-get (point) "SLUG") ".ics")
                    (expand-file-name "ics" (file-name-directory emacsconf-org-file))))
        (ical-entry (emacsconf-ical-convert-entry-to-string (format-time-string "%Y%m%dT%H%M%SZ" nil t))))
    (with-temp-file ical-file
        (insert ical-entry))
    (emacsconf-mail-speaker "Calendar entry")
    (mml-attach-file ical-file)))

(defun emacsconf-ical-convert-entry-to-string (&optional updated)
  (string-join 
   (list
    "BEGIN:VCALENDAR"
    "VERSION:2.0"
    "PRODID:EmacsConf"
    (concat "X-WR-CALNAME:EmacsConf " emacsconf-year)
    "X-WR-CALNAME:EmacsConf"
    "CALSCALE:GREGORIAN"
    "METHOD:PUBLISH"
    (emacsconf-ical-format-talk (emacsconf-get-talk-info-for-subtree) updated)
    "END:VCALENDAR")
   "\r\n"))

(defun emacsconf-ical-format-talk (o &optional updated)
  (string-join
   (delq
    nil
    (list
     "BEGIN:VEVENT"
     (string-trim (org-icalendar-fold-string
                   (org-icalendar-cleanup-string
                    (concat "SUMMARY:" (plist-get o :title)
                            (if (plist-get o :speakers)
                                (concat " - " (plist-get o :speakers))
                              "")))))
     "ORGANIZER:EmacsConf"
     (concat "LOCATION:" "https://emacsconf.org/")
     ;; (concat "UID:emacsconf-" emacsconf-year "-" (plist-get o :slug))
     (concat "UID:" (plist-get o :uuid))
     (concat "URL:" "https://emacsconf.org/" emacsconf-year "/talks/" (plist-get o :slug))
     (concat "DTSTART:" (format-time-string "%Y%m%dT%H%M%SZ" (plist-get o :start-time) t))
     (concat "DTEND:" (format-time-string "%Y%m%dT%H%M%SZ" (plist-get o :end-time) t))
     (if updated (concat "DTSTAMP:" updated))
     (if (plist-get o :speakers)
         (mapconcat
          (lambda (s)
            (format "ATTENDEE;ROLE=REQ-PARTICIPANT;CUTYPE=INDIVIDUAL;CN=\"%s\":invalid:nomail" s))
          (split-string (plist-get o :speakers) ", +")
          "\r\n")
       nil)
     (string-trim
      (org-icalendar-fold-string
       (org-icalendar-cleanup-string
        (concat "DESCRIPTION: Times are approximate and will probably change.\n"
                "https://emacsconf.org/" emacsconf-year "/talks/" (plist-get o :slug) "\n"
                (plist-get o :markdown)))))
     "END:VEVENT"))
   "\r\n"))

(defun emacsconf-format-as-ical (emacsconf-info)
  (require 'ox-icalendar)
  (let ((updated (format-time-string "%Y%m%dT%H%M%SZ" nil t)))
    (string-join 
     (list
      "BEGIN:VCALENDAR"
      "VERSION:2.0"
      "PRODID:EmacsConf"
      (concat "X-WR-CALNAME:EmacsConf " emacsconf-year)
      "X-WR-CALNAME:EmacsConf"
      "CALSCALE:GREGORIAN"
      "METHOD:PUBLISH"
      (mapconcat (lambda (o) (emacsconf-ical-format-talk o updated))
                 (seq-remove (lambda (o) (string= (plist-get o :status) "CANCELLED"))
                             (emacsconf-filter-talks emacsconf-info))
                 "\r\n")
      "END:VCALENDAR")
     "\r\n")))

(defun emacsconf-generate-ical ()
  (unless (file-directory-p emacsconf-directory) (error "Please specify the wiki directory in the emacsconf-directory variable."))
  (with-temp-file (expand-file-name "emacsconf.ics" (expand-file-name emacsconf-year emacsconf-directory))
    (insert (emacsconf-format-as-ical (emacsconf-get-talk-info)))))
