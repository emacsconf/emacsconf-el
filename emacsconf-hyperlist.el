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

(defun emacsconf-hyperlist-audio (source)
	(format
	 "%s: [[elisp:(emacsconf-stream-audio-mixer \"${track}\")][mixer (F3 output)]] [[elisp:(emacsconf-stream-audio-quieter \"${track}\" \"%s\")][quieter]] [[elisp:(emacsconf-stream-audio-louder \"${track}\" \"%s\")][louder]] %s"
	 source
	 source source
	 (mapconcat (lambda (val)
								(format "[[elisp:(emacsconf-stream-audio-set \"${track}\" \"%s\" \"%d%%\")][%d]]"
												source
												val val))
							'(70 80 90 100 110 120)
							" ")))

(defun emacsconf-hyperlist-format-talk-streamer (talk)
	(setq talk (emacsconf-resolve-talk talk))
	(emacsconf-replace-plist-in-string
   (append
		(list :hhmm (format-time-string "%H:%M" (plist-get talk :start-time) emacsconf-timezone)
					:intro-type (if (plist-get talk :recorded-intro) "recorded" "live")
					:talk-type (if (plist-get talk :video-file) "recorded" "live")
					:qa-type (or (plist-get talk :q-and-a) "none")
					:qa-adjust (emacsconf-hyperlist-audio "qa")
					:mumble-adjust (emacsconf-hyperlist-audio "mumble"))
		talk) 
   (concat
		"- ${hhmm} ${track-id} ${slug} (intro: ${intro-type}, talk: ${talk-type}, Q&A: ${qa-type}) [[${absolute-url}][talk page]]\n"
		(emacsconf-surround "  - " (plist-get talk :hyperlist-note) "\n" "")
		"  - [ ] [[elisp:(emacsconf-update-talk-status-with-hooks \"${slug}\" \".\" \"PLAYING\")][set talk playing]]\n"
		;; Intro
		(cond
		 ((plist-get talk :recorded-intro)
			"    - [[elisp:(emacsconf-stream-play-intro \"${slug}\")][backup: play intro]]
      - if that still doesn't work, [[elisp:(emacsconf-stream-open-in-between-slide \"${slug}\"][open in-between slide]] and ask the host to intro it over Mumble\n"
			)
		 ((plist-get talk :video-file) ;; recorded talk, so intro comes from Mumble
			"    - [[elisp:(emacsconf-play-intro \"${slug}\")][backup: open in-between slide]]
  - [ ] adjust audio as needed ${mumble-adjust}\n")
		 (t ;; live talk and intro, join BBB
			"    - [[elisp:(emacsconf-stream-bbb \"${slug}\")][backup: join BBB for live intro and talk]]
    - [[elisp:(emacsconf-stream-xdotool-set-up-bbb \"${slug}\"][backup: xdotool BBB]]
  - [ ] adjust audio as needed ${qa-adjust}\n"))
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
  - [ ] [[elisp:(emacsconf-stream-xdotool-set-up-bbb \"${slug}\"][xdotool BBB]]
  - [ ] adjust audio as needed ${qa-adjust}\n"
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
					:qa-adjust (emacsconf-hyperlist-audio "qa")
					:mumble-adjust (emacsconf-hyperlist-audio "mumble")
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
  - [ ] [[elisp:(emacsconf-stream-xdotool-set-up-bbb \"${slug}\"][xdotool BBB]]
  - [ ] Check that streaming has started AND RECORDING HAS STARTED
  - [ ] Give the host the go-ahead via Mumble or #emacsconf-org
  - [ ] ${qa-adjust}
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
  - [ ] Bring the speaker's Mumble login over to the ${channel} channel in Mumble.
  - [ ] Confirm that Mumble is audible and adjust audio as needed: ${mumble-adjust}\n
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

(defun emacsconf-hyperlist-format-streamer-day (day &optional track info)
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
	(insert
   "- Setup:
  - [ ] ssh live screen-fallbacks\n"
	 (mapconcat (lambda (track)
								(emacsconf-replace-plist-in-string
								 track
								 "  - ${name}
    - [ ] Connect via VNC
    - [ ] [[elisp:(emacsconf-stream-track-ssh \"${name}\" \"nohup\" \"start-background-music\" \"&\")][start background music]]
    - [ ] Start recording with OBS (not streaming)
    - [ ] Check main stream with MPV ${stream}
    - [ ] Check 480p ${480p}
    - [ ] [[shell:ssh -t orga@live0.emacsconf.org 'screen -S restream-${id}-youtube /home/orga/restream-${id}-youtube.sh'][Start Youtube restream]] and then confirm ${youtube-studio-url} and ${youtube-url}
    - [ ] [[shell:ssh -t orga@live0.emacsconf.org 'screen -S restream-${id}-toobnix /home/orga/restream-${id}-toobnix.sh'][Start Toobnix restream]] and then confirm ${toobnix-url}
    - [ ] [[elisp:(emacsconf-stream-update-track-status \"${name}\")][Update emacsconf-tracks :status and update status page]]
    - [ ] Start Emacs and use emacsconf-stream-display-clock-and-countdown\n"))
							emacsconf-tracks
							"")
	 (emacsconf-hyperlist-format-streamer-day date track info)
	 "- Teardown
  - [ ] Stop recording
"
	 (mapconcat (lambda (track)
								(emacsconf-replace-plist-in-string
								 track
								 "  - ${name}
    - [ ] [[shell:ssh orga@live0.emacsconf.org screen -S restream-${id}-youtube -X quit][stop youtube restream]]
    - [ ] [[shell:ssh orga@live0.emacsconf.org screen -S restream-${id}-toobnix -X quit][stop toobnix restream]]
    - [ ] [[elisp:(emacsconf-stream-update-track-status \"${name}\")][Update emacsconf-tracks :status and update status page]]
    - [ ] Kill the fallback screens on live0
"
								 ))
							emacsconf-tracks ""))
	(goto-char (point-min))
	(org-mode))

;;; Code:
(provide 'emacsconf-hyperlist)
;;; emacsconf.el ends here
