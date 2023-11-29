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

(defvar emacsconf-ical-public-directory (concat "/ssh:orga@media.emacsconf.org:/var/www/media.emacsconf.org/" emacsconf-year)
  "Directory to post ics files to.")

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

(defun emacsconf-ical-fold (s)
	(replace-regexp-in-string
	 "\r\n$" ""
	 (replace-regexp-in-string
		"\n" "\r\n"
		(string-trim
		 (org-icalendar-fold-string
			(org-icalendar-cleanup-string
			 s))))))

(defun emacsconf-ical-format-talk (o &optional updated)
  (string-join
   (delq
    nil
    (list
     "BEGIN:VEVENT"
		 (emacsconf-ical-fold
			(concat "SUMMARY:" (plist-get o :title)
							(emacsconf-surround " - " (plist-get o :speakers) "" "")))
     "ORGANIZER:EmacsConf"
     (concat "LOCATION:" (plist-get (emacsconf-get-track (plist-get o :track)) :watch))
		 (if (plist-get o :uuid)
				 (concat "UID:" (plist-get o :uuid))
			 (concat "UID:" emacsconf-id "-" emacsconf-year "-" (plist-get o :slug)))
     (concat "URL:" (plist-get o :absolute-url))
     (concat "DTSTART:" (format-time-string "%Y%m%dT%H%M%SZ" (plist-get o :start-time) t))
     (concat "DTEND:" (format-time-string "%Y%m%dT%H%M%SZ" (plist-get o :end-time) t))
     (if updated (concat "DTSTAMP:" updated))
			(emacsconf-ical-fold
				(emacsconf-replace-plist-in-string
				 (append o
								 (list
									:year emacsconf-year
									:watch-url (plist-get (emacsconf-get-track (plist-get o :track)) :watch)
									:stream (plist-get (emacsconf-get-track (plist-get o :track)) :stream)
									:replay
									(if (plist-get o :video-file) "There is a pre-recorded video for this talk, so it should be available from ${url} shortly after the talk starts."
										(format "This talk is live. We'll upload it to %s%s by January or so. You can subscribe to emacsconf-discuss for updates: https://lists.gnu.org/mailman/listinfo/emacsconf-discuss"
														emacsconf-media-base-url
														emacsconf-year))
									:speaker-text (emacsconf-surround
																 (if (length= (plist-get o :speakers) 1) "\nSpeaker: " "\nSpeakers: ")
																 (plist-get o :speakers)
																 "" "")))
				 "DESCRIPTION:${url}${speaker-text}

Times are approximate and may change. Please check the talk webpage for details and other resources.

Watch live using a streaming media player. Some examples:
mpv ${stream}
vlc ${stream}
ffplay ${stream}

or watch using the web-based player: ${watch-track}

Q&A: ${qa-info}

${replay}

Description:
${markdown}
"))
     "END:VEVENT"))
   "\r\n"))

(defun emacsconf-format-as-ical (emacsconf-info &optional title)
  (require 'ox-icalendar)
  (let ((updated (format-time-string "%Y%m%dT%H%M%SZ" nil t)))
    (string-join 
     (list
      "BEGIN:VCALENDAR"
      "VERSION:2.0"
      (concat "PRODID:" emacsconf-name "-" emacsconf-year (emacsconf-surround "-" title "" ""))
			(concat "X-WR-CALNAME:" emacsconf-name " " emacsconf-year (emacsconf-surround " " title "" ""))
			"CALSCALE:GREGORIAN"
      "METHOD:PUBLISH"
      (mapconcat
			 (lambda (o) (emacsconf-ical-format-talk o updated))
			 (emacsconf-publish-prepare-for-display (emacsconf-filter-talks emacsconf-info))
       "\r\n")
      "END:VCALENDAR")
     "\r\n")))

;;;###autoload
(defun emacsconf-ical-generate-all ()
  (interactive)
  (let* ((emacsconf-talk-info-functions (append emacsconf-talk-info-functions (list 'emacsconf-get-abstract-from-wiki)))
         (info (emacsconf-get-talk-info)))
    (emacsconf-ical-generate-main info)
    (emacsconf-ical-generate-tracks info)))

(defun emacsconf-ical-generate-main (&optional info)
  (with-temp-file (expand-file-name "emacsconf.ics"
                                    (or emacsconf-ical-public-directory
                                        (expand-file-name emacsconf-year emacsconf-directory)))
    (insert (emacsconf-format-as-ical (emacsconf-publish-prepare-for-display (or info (emacsconf-get-talk-info)))))))

(defun emacsconf-ical-generate-tracks (&optional info)
  (interactive)
  (mapc (lambda (entry)
          (let ((track (emacsconf-get-track (car entry))))
            (when track
              (with-temp-file (expand-file-name (format "emacsconf-%s.ics" (plist-get track :id))
                                                (or emacsconf-ical-public-directory
                                                    (expand-file-name emacsconf-year emacsconf-directory)))
                (insert (emacsconf-format-as-ical (cdr entry)
																									(plist-get track :name)))))))
        (seq-group-by (lambda (o) (plist-get o :track)) (emacsconf-publish-prepare-for-display (or info (emacsconf-get-talk-info))))))

(provide 'emacsconf-ical)
