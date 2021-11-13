;;; emacsconf-update.el --- Update an Org file with the current schedule  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Sacha Chua

;; Author: Sacha Chua <sacha@sachachua.com>
;; Keywords: convenience, calendar
;; Version: 0.1
;; URL: https://github.com/emacsconf/emacsconf-el
;; 
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

;; The following code should retrieve the conference schedule and update
;; a file that people use for notes. It should keep the notes people
;; already have, and just update the scheduled times or create new notes
;; as needed. If the calendar file has UID properties, it is used to
;; match headings. If not, headings are used.

;; Make a file for your notes with Org, and then call
;; =M-x emacsconf-update-org-from-ical-url= . It should load the talks and
;; tentative schedules from the wiki. You can call it again and it should
;; update the titles and schedules while still keeping whatever notes
;; you've added.
;;
;; You may also like =M-x emacsconf-update-view-org-agenda=.
;; 
;;; Code:

(defgroup emacsconf-update nil
  "Convenience functions for downloading the schedule and updating an Org file."
  :group 'communication)

(defcustom emacsconf-update-ical-url "https://emacsconf.org/current/emacsconf.ics"
  "URL to get schedule updates from."
  :type 'string
  :group 'emacsconf-update)

(defcustom emacsconf-update-create-entries t
  "Non-nil means create missing entries."
  :type '(choice (const t "Create missing entries")
                 (const nil "Don't create missing entries"))
  :group 'emacsconf-update)

(require 'icalendar)
(require 'ox-icalendar)

(defun emacsconf-update-org-from-ical-url ()
  "Update current notes from iCalendar file at `emacsconf-update-ical-url'."
  (interactive)
  (emacsconf-update-org-from-ical-string
   (with-temp-buffer
     (url-insert-file-contents emacsconf-update-ical-url)
     (buffer-string))))

(defun emacsconf-update-org-from-ical-file (filename)
  "Prompt for FILENAME and update current notes from schedule."
  (interactive "FFilename: ")
  (emacsconf-update-org-from-ical-string (with-temp-buffer (insert-file-contents filename) (buffer-string))))

(defun emacsconf-ical-get-calendar-from-string (string)
  "Read the calendar data from STRING."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (with-current-buffer (icalendar--get-unfolded-buffer (current-buffer))
      (goto-char (point-min))
      (when (re-search-forward "^BEGIN:VCALENDAR\\s-*$" nil t)
        (beginning-of-line)
        (icalendar--read-element nil nil)))))

(defun emacsconf-update--get-time-from-event (event property zone-map)
  "Return time of EVENT in PROPERTY with possible conversion from ZONE-MAP."
  (let* ((dt (icalendar--get-event-property event property))
         (tz (icalendar--find-time-zone (icalendar--get-event-property-attributes event property) zone-map)))
    (apply 'encode-time (icalendar--decode-isodatetime dt nil tz))))

(defun emacsconf-update--format-new-entry (event zone-map)
  "Return string for new entry for EVENT."
  (let* ((dtstart-time (emacsconf-update--get-time-from-event event 'DTSTART zone-map))
         (dtend-time (emacsconf-update--get-time-from-event event 'DTEND zone-map)))
    (format "\n* %s\n:PROPERTIES:\n:UUID: %s\n:URL: %s\n:END:\nTentative schedule: %s--%s\n\n"
            (icalendar--convert-string-for-import
             (icalendar--get-event-property event 'SUMMARY))
            (icalendar--get-event-property event 'UID)
            (icalendar--get-event-property event 'URL)
            (org-format-time-string (cdr org-time-stamp-formats) dtstart-time)                    
            (org-format-time-string (cdr org-time-stamp-formats) dtend-time))))

(defun emacsconf-update--update-entry (event zone-map)
  "Update the current Org heading to match EVENT.
Use ZONE-MAP for timestamp decoding."
  (let* ((dtstart-time (emacsconf-update--get-time-from-event e 'DTSTART zone-map))
         (dtend-time (emacsconf-update--get-time-from-event e 'DTEND zone-map)))
    (org-back-to-heading)
    (when (looking-at org-complex-heading-regexp)
      (replace-match (icalendar--convert-string-for-import
                      (icalendar--get-event-property e 'SUMMARY))
                     t t nil 4))
    (org-end-of-meta-data)
    (when (re-search-forward (concat "Tentative schedule: " org-ts-regexp "--" org-ts-regexp " *\n")
                             nil
                             (save-excursion (org-end-of-subtree)))
      (replace-match ""))
    (insert "Tentative schedule: "
            (org-format-time-string (cdr org-time-stamp-formats)
                                    dtstart-time)
            "--"
            (org-format-time-string (cdr org-time-stamp-formats)
                                    dtend-time)
            "\n")))

(defun emacsconf-update-org-from-ical-string (string)
  "Update Org headings in current file by using STRING."
  (save-excursion
    (let* ((ical (emacsconf-ical-get-calendar-from-string string))
           (zone-map (icalendar--convert-all-timezones ical)))
      (mapc (lambda (e)
              (let ((pos (org-find-property "UUID" (icalendar--get-event-property e 'UID))))
                (cond
                 (pos 
                  (goto-char pos)
                  (emacsconf-update--update-entry e zone-map))
                 (emacsconf-update-create-entries
                  (goto-char (point-max))
                  (insert (emacsconf-update--format-new-entry e zone-map))))))
            (icalendar--all-events ical)))))

;;;###autoload
(defun emacsconf-update-view-org-agenda ()
  "Display the agenda."
  (interactive)
  (let (times
        min-time
        max-time
        (org-agenda-files (list (buffer-file-name))))
    ;; Collect all the times
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-ts-regexp nil t)
        (setq times (cons (time-to-days (encode-time (org-parse-time-string (match-string 0)))) times)))
      (setq min-time (apply 'min times) max-time (apply 'max times)))
    (org-agenda-list nil
                     min-time
                     (1+  (- max-time min-time)))))

(provide 'emacsconf-update)
;;; emacsconf-update.el ends here
