;;; emacsconf-spookfox.el --- Spookfox browser automation  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Sacha Chua

;; Author: Sacha Chua <sacha@sachachua.com>
;; Keywords: convenience

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

;;; Code:

(defun emacsconf-spookfox-create-bbb (group)
	"Create a BBB room for this group of talks.
GROUP is (email . (talk talk talk)).
Needs a Spookfox connection."
	(let* ((bbb-name
					(format "%s (%s) - %s%s"
									(mapconcat (lambda (o) (plist-get o :slug)) (cdr group) ", ")
									(plist-get (cadr group) :speakers)
									emacsconf-id
									emacsconf-year))
				 path
				 (retrieve-command
					(format
					 "window.location.origin + [...document.querySelectorAll('h4.room-name-text')].find((o) => o.textContent.trim() == '%s').closest('tr').querySelector('.delete-room').getAttribute('data-path')"
					 bbb-name))
				 (create-command (format "document.querySelector('#create-room-block').click();
document.querySelector('#create-room-name').value = \"%s\";
document.querySelector('#room_mute_on_join').click();
document.querySelector('.create-room-button').click();"
																 bbb-name)))
		(setq path (spookfox-js-injection-eval-in-active-tab retrieve-command t))
		(unless path
			(dolist (cmd (split-string create-command ";"))
				(spookfox-js-injection-eval-in-active-tab cmd t)
				(sleep-for 2))
			(sleep-for 2)
			(setq path (spookfox-js-injection-eval-in-active-tab retrieve-command t)))
		(when path
			(dolist (talk (cdr group))
				(save-window-excursion
					(emacsconf-with-talk-heading talk
						(org-entry-put (point) "ROOM" path))))
			(cons bbb-name path))))

(defun emacsconf-spookfox-create-bbb-for-live-talks ()
	"Create BBB rooms for talks that don't have them yet."
	(let* ((talks (seq-filter
								 (lambda (o)
									 (and (string-match "live" (or (plist-get o :q-and-a) ""))
												(not (string= (plist-get o :status) "CANCELLED"))
												(not (plist-get o :bbb-room))))
								 (emacsconf-publish-prepare-for-display (emacsconf-get-talk-info))))
				 (groups (and talks (emacsconf-mail-groups talks))))
		(dolist (group groups)
			(emacsconf-spookfox-create-bbb group))))

(provide 'emacsconf-spookfox)
;;; emacsconf-spookfox.el ends here
