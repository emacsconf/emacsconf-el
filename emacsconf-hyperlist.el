;;; emacsconf-hyperlist.el --- step-by-step checklists  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Sacha Chua

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

(defun emacsconf-hyperlist-format-talk-streamer (talk)
	(setq talk (emacsconf-resolve-talk talk))
	(emacsconf-replace-plist-in-string
   (append
		(list :hhmm (format-time-string "%H:%M" (plist-get talk :start-time) emacsconf-timezone)
					:intro-type (if (plist-get talk :recorded-intro) "recorded" "live")
					:talk-type (if (plist-get talk :video-file) "recorded" "live")
					:qa-type (or (plist-get talk :q-and-a) "none")
					)
		talk) 
   (concat
		"- ${hhmm} ${track-id} ${slug} (intro: ${intro-type}, talk: ${talk-type}, Q&A: ${qa-type}) [[${absolute-url}][talk page]]\n"
		(emacsconf-surround "  - " (plist-get talk :hyperlist-note) "\n" "")
		"  - [ ] [[elisp:(emacsconf-update-talk-status-with-hooks \"${slug}\" \".\" \"PLAYING\")][set talk playing]]\n"
		;; Intro
		(cond
		 ((plist-get talk :recorded-intro)
			"    - [[elisp:(emacsconf-stream-play-intro \"${slug}\")][backup: play intro]]\n")
		 ((plist-get talk :video-file) ;; recorded talk, so intro comes from Mumble
			"    - [[elisp:(emacsconf-play-intro \"${slug}\")][backup: open in-between slide]]\n")
		 (t ;; live talk and intro, join BBB
			"    - [[elisp:(emacsconf-stream-bbb \"${slug}\")][backup: join BBB for live intro and talk]]
  - [ ] adjust audio as needed\n"))
		;; Talk
		(cond
		 ;; video should already have played
		 ((and (plist-get talk :recorded-intro) (plist-get talk :video-file))
			"    - [[elisp:(emacsconf-stream-play-video \"${slug}\")][backup: play video]]\n")
		 ;; play video manually if intro was live
		 ((plist-get talk :video-file)
			"  - [ ] [[elisp:(emacsconf-stream-play-video \"${slug}\")][play video]] after the host finishes introducing it\n")
		 ;; recorded intro, live talk
		 ((plist-get talk :recorded-intro)
			"  - [ ] [[elisp:(emacsconf-stream-bbb \"${slug}\")][join BBB for live talk]]
  - [ ] adjust audio as needed\n"
			))
		(if (plist-get talk :video-file)
				""
			;; live talks will continue with Q&A
			"  - [ ] [[elisp:(emacsconf-update-talk-status-with-hooks \"${slug}\" \".\" \"CLOSED_Q\")][set talk closed q]] when the Q&A seems to be starting
    - [[elisp:(emacsconf-stream-open-pad \"${slug}\")][backup: open pad]]
    - [[elisp:(emacsconf-stream-join-chat \"${slug}\")][backup: join chat]]
  - [ ] [[elisp:(emacsconf-update-talk-status-with-hooks \"${slug}\" \".\" \"OPEN_Q\")][set talk open q]] when the host gives the go-ahead
  - [ ] [[elisp:(emacsconf-update-talk-status-with-hooks \"${slug}\" \".\" \"TO_ARCHIVE\")][set talk to archive]] when done
"
			)
		)))

;; assumes the talk is not live
(defun emacsconf-hyperlist-format-qa-streamer (talk)
	(setq talk (emacsconf-resolve-talk talk))
	(emacsconf-replace-plist-in-string
	 (append
		(list :hhmm (format-time-string "%H:%M" (plist-get talk :qa-time) emacsconf-timezone)
					:intro-type (if (plist-get talk :recorded-intro) "recorded" "live")
					:talk-type (if (plist-get talk :video-file) "recorded" "live")
					:qa-type (or (plist-get talk :q-and-a) "none")
					)
		talk)
	 (concat
		"- ${hhmm} ${track-id} ${slug} Q&A: ${q-and-a} [[${absolute-url}][talk page]]
  - [ ] [[elisp:(emacsconf-update-talk-status-with-hooks \"${slug}\" \".\" \"CLOSED_Q\")][set talk closed q]]
" 
		(pcase (or (plist-get talk :q-and-a) "")
			((rx "live")
			 "    - [[elisp:(emacsconf-stream-bbb \"${slug}\")][backup: join BBB]]
    - [[elisp:(emacsconf-stream-open-pad \"${slug}\")][backup: open pad]]
  - [ ] Check that streaming has started
  - [ ] Give the host the go-ahead via Mumble or #emacsconf-org
  - [ ] [[elisp:(emacsconf-update-talk-status-with-hooks \"${slug}\" \".\" \"OPEN_Q\")][set talk open q]]
  - [ ] Confirm BBB redirect at ${bbb-redirect} goes to BBB room, let host know
")
			((rx "irc")
			 "    - [[elisp:(emacsconf-stream-join-chat \"${slug}\")][backup: join chat]]
    - [[elisp:(emacsconf-stream-open-pad \"${slug}\")][backup: open pad]]
  - [ ] [[elisp:(emacsconf-update-talk-status-with-hooks \"${slug}\" \".\" \"OPEN_Q\")][set talk open q]]
")
			((rx "Mumble")
			 "
  - [ ] Bring the speaker's Mumble login over to the ${channel} channel in Mumble. Confirm that Mumble is audible and adjust audio as needed: ssh emacsconf-${track-id}@res.emacsconf.org -p 46668 \"mum-vol 85%%\" (or mum-louder, mum-quieter)
  - [ ] [[elisp:(emacsconf-update-talk-status-with-hooks \"${slug}\" \".\" \"OPEN_Q\")][set talk open q]]
")
			((rx "after")
			 "    - [[elisp:(emacsconf-stream-join-chat \"${slug}\")][backup: join chat]]
     - [[elisp:(emacsconf-stream-open-pad \"${slug}\")][backup: open pad]]
  - [ ] [[elisp:(emacsconf-update-talk-status-with-hooks \"${slug}\" \".\" \"OPEN_Q\")][set talk open q]]
"                      
			 ))
		"  - [ ] [[elisp:(emacsconf-update-talk-status-with-hooks \"${slug}\" \".\" \"TO_ARCHIVE\")][set talk to archive]]
"
		)))

(defun emacsconf-hyperlist-day-events (day &optional track info)
	(let* ((talks
					(emacsconf-prepare-for-display
					 (emacsconf-filter-talks-by-time
						(concat day "T00:00:00" emacsconf-timezone-offset)
						(concat day "T23:59:59" emacsconf-timezone-offset)
						(if track
								(emacsconf-filter-talks-by-track (or info (emacsconf-get-talk-info)))
							(or info (emacsconf-get-talk-info)))))))
		(sort
		 (apply #'append
						(mapcar
						 (lambda (talk)
							 (if (plist-get talk :video-file)
									 (list
										(cons (plist-get talk :start-time)
													(emacsconf-hyperlist-format-talk-streamer talk))
										(cons (plist-get talk :qa-time)
													(emacsconf-hyperlist-format-qa-streamer talk)))
								 (list
									(cons (plist-get talk :start-time)
												(emacsconf-hyperlist-format-talk-streamer talk)))))
						 talks))
		 (lambda (a b)
			 (time-less-p (car a) (car b))))))

(defun emacsconf-hyperlist-format-day (day &optional track info)
	(setq info (emacsconf-prepare-for-display
							(if info (mapcar #'emacsconf-resolve-talk info)
								(emacsconf-get-talk-info))))
	(when track (setq info (emacsconf-filter-talks-by-track track info)))
	(when day (setq info (emacsconf-filter-talks-by-time
												(concat day "T00:00:00" emacsconf-timezone-offset)
												(concat day "T23:59:59" emacsconf-timezone-offset)
												info)))
	(let* ((events
					(sort
					 (apply #'append
									(mapcar
									 (lambda (talk)
										 (if (plist-get talk :video-file)
												 (list
													(cons (plist-get talk :start-time)
																(emacsconf-hyperlist-format-talk-streamer talk))
													(cons (plist-get talk :qa-time)
																(emacsconf-hyperlist-format-qa-streamer talk)))
											 (list
												(cons (plist-get talk :start-time)
															(emacsconf-hyperlist-format-talk-streamer talk)))))
									 info))
					 (lambda (a b)
						 (time-less-p (car a) (car b))))))
		(mapconcat #'cdr events "")))

(defun emacsconf-hyperlist-show-streamer-day (date &optional track info)
	"Display the streamer hyperlist for DATE."
	(interactive (list (org-read-date "Date: ")))
	(pop-to-buffer (get-buffer-create "*hyperlist*"))
	(erase-buffer)
	(insert (emacsconf-hyperlist-format-day date track info))
	(goto-char (point-min))
	(org-mode))

;;; Code:
(provide 'emacsconf-hyperlist)
;;; emacsconf.el ends here
