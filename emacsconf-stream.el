;;; emacsconf-stream.el --- Play files and update streaming information  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Sacha Chua

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

(defvar emacsconf-stream-dir "/data/emacsconf/stream/"
  "Directory where the stream versions are.
Files should be in YEAR/file-prefix--main.webm and file-prefix--main.vtt.")
(defvar emacsconf-stream-host "res.emacsconf.org")

(defun emacsconf-stream-track-login (track)
  "Return user@host for the track."
  (when (plist-get track :track)
    (setq track (emacsconf-get-track (plist-get track :track))))
  (or (plist-get track :tramp)
      (concat "/ssh:" emacsconf-id "-" (plist-get track :id) "@" emacsconf-stream-host)))

(defvar emacsconf-stream-bottom-limit 80
  "Number of characters for bottom text.")

(defun emacsconf-stream-set-news (track message)
  (interactive (list (emacsconf-complete-track) (read-string "Message: ")))
  (let* ((home (concat (emacsconf-stream-track-login track) "~"))
         (filename (expand-file-name "news.txt" home)))
    (with-temp-file filename
      (insert message))))

(defun emacsconf-stream-broadcast (message)
  (interactive (list (read-string "Message: ")))
  (mapc (lambda (track) (emacsconf-stream-set-news track message))
        emacsconf-tracks))

(defun emacsconf-stream-clear-talk-info (track)
  (interactive (list (emacsconf-complete-track)))
  (emacsconf-stream-set-talk-info-from-strings track "" ""))

(defun emacsconf-stream-clear-track (track)
  (interactive (list (emacsconf-complete-track)))
  (emacsconf-stream-set-talk-info-from-strings track "" "")
  (emacsconf-stream-set-news track ""))

(defun emacsconf-stream-clear-all ()
  (interactive)
  (mapc #'emacsconf-stream-clear-track emacsconf-tracks))

(defun emacsconf-stream-svg-set-text (dom id text)
  "Update DOM to set the tspan in the element with ID to TEXT.
If the element doesn't have a tspan child, use the element itself."
  (if (or (null text) (string= text ""))
      (let ((node (dom-by-id dom id)))
        (when node
          (dom-set-attribute node 'style "visibility: hidden")
          (dom-set-attribute (dom-child-by-tag node 'tspan) 'style "fill: none; stroke: none")))
    ;; (setq text (svg--encode-text text))
    (let ((node (or (dom-child-by-tag
                     (car (dom-by-id dom id))
                     'tspan)
                    (dom-by-id dom id))))
      (cond
       ((null node)
        (error "Could not find node %s" id))                      ; skip
       ((= (length node) 2)
        (nconc node (list text)))
       (t (setf (elt node 2) text))))))

(defun emacsconf-stream-add-talk-props (talk)
  "Create an overlay for TALK.
VIDEO-FILENAME will be displayed while the video is playing,
while OTHER-FILENAME will be displayed at other times."
  (plist-put
   talk
   :overlay-url
   (concat (replace-regexp-in-string "^.*//" "" emacsconf-base-url)
           (plist-get talk :url)
           (cond
            ((null (plist-get talk :q-and-a)) "")
            ((string-match "live" (plist-get talk :q-and-a))
             " - Q&A: live (see talk page for URL)")
            ((and (string-match "irc" (plist-get talk :q-and-a))
                  (plist-get talk :irc))
             (format " - Q&A: IRC (#%s) - speaker nick: %s"
                     (plist-get (emacsconf-get-track (plist-get talk :track)) :channel)
                     (plist-get talk :irc)))
            ((string-match "irc" (plist-get talk :q-and-a))
             (format " - Q&A: IRC (#%s)"
                     (plist-get (emacsconf-get-track (plist-get talk :track)) :channel)))
            (t ""))))
  (plist-put talk :overlay-bottom (or (plist-get talk :speakers-with-pronouns) "EmacsConf"))
  talk)

(defun emacsconf-stream-set-talk-info-from-strings (track url bottom)
  (interactive (list (emacsconf-complete-track) (read-string "URL: ") (read-string "Bottom: ")))
  (let* ((home (concat (emacsconf-stream-track-login track) "~"))
         (default-directory home)
         (filename
          (if (file-exists-p (expand-file-name "roles/obs/overlay.svg" emacsconf-ansible-directory))
              (expand-file-name "roles/obs/overlay.svg" emacsconf-ansible-directory)
            (expand-file-name "other.svg" home)))
         (dom (xml-parse-file filename)))
    (emacsconf-stream-svg-set-text dom "bottom" bottom)
    (emacsconf-stream-svg-set-text dom "url" url)
    (with-temp-file filename (dom-print dom))
    (with-temp-file (expand-file-name "video.svg" home)
      (let ((node (dom-by-id dom "bottom")))
        (when node
          (dom-set-attribute node 'style "visibility: hidden")
          (dom-set-attribute (dom-child-by-tag node 'tspan) 'style "fill: none; stroke: none")))
      (dom-print dom))
    ;; OBS doesn't kern SVG text as prettily as Inkscape does, so we use Inkscape for the conversion
    (shell-command "inkscape --export-type=png --export-dpi=96 --export-background-opacity=0 video.svg")
    (shell-command "inkscape --export-type=png --export-dpi=96 --export-background-opacity=0 other.svg")))

(defun emacsconf-stream-set-talk-info (talk)
  (interactive (list (emacsconf-complete-talk-info)))
  (setq talk (emacsconf-stream-add-talk-props talk))
  (let ((home (concat (emacsconf-stream-track-login (emacsconf-get-track talk)) "~")))
    (if (file-exists-p
         (expand-file-name (concat (plist-get talk :slug) "-video.png") emacsconf-stream-overlay-dir))
        (progn
          (copy-file
           (expand-file-name (concat (plist-get talk :slug) "-video.png") emacsconf-stream-overlay-dir)
           (expand-file-name "video.png" home)
           t)
          (copy-file
           (expand-file-name (concat (plist-get talk :slug) "-other.png") emacsconf-stream-overlay-dir)
           (expand-file-name "other.png" home)
           t))
      (emacsconf-stream-set-talk-info-from-strings
       (emacsconf-get-track talk)
       (plist-get talk :overlay-url)
       (plist-get talk :overlay-bottom)))))

(defun emacsconf-stream-open-in-between-slide (talk)
  (interactive (list (emacsconf-complete-talk-info)))
  (let ((default-directory (emacsconf-stream-track-login talk)))
    (shell-command (concat "firefox " (plist-get talk :in-between-url) "&"))))

(defun emacsconf-stream-open-qa-slide (talk)
  (interactive (list (emacsconf-complete-talk-info)))
  (let ((default-directory (emacsconf-stream-track-login talk)))
    (shell-command (concat "firefox " (plist-get talk :qa-slide-url) "&"))))

(defun emacsconf-stream-open-qa-windows-on-change (talk)
	"Open the QA window(s) for TALK if the track's autopilot is set to 'todo."
  (interactive (list (emacsconf-complete-talk-info)))
	(let ((track (emacsconf-get-track (plist-get talk :track))))
		(when (eq (plist-get track :autopilot) 'todo)
			(when (or (not (boundp 'org-state)) (string= org-state "CLOSED_Q"))
				(save-window-excursion
					(if (or (null (plist-get talk :q-and-a))
									(string-match "Mumble" (plist-get talk :q-and-a)))
							(emacsconf-stream-open-in-between-slide talk)
						(emacsconf-stream-join-qa talk)
						(shell-command "i3-msg 'layout splith'")))))))

(defun emacsconf-stream-update-talk-info-on-change (talk)
  "Update TALK info if the track's autopilot is set to 'todo."
  (when (string= org-state "PLAYING")
		(let* ((talk (emacsconf-resolve-talk talk))
					 (track (emacsconf-get-track (plist-get talk :track))))
			(when (eq (plist-get track :autopilot) 'todo)
				(save-window-excursion
					(emacsconf-stream-set-talk-info talk))))))

(defun emacsconf-stream-track-ssh (track &rest commands)
  "SSH to the account for TRACK and run COMMANDS.
This might be more reliable than using TRAMP to call file processes,
especially when two things need to happen close together."
	(setq track
				(if (stringp track)
						(or (emacsconf-get-track track)
								(emacsconf-get-track (emacsconf-resolve-talk track)))
					(emacsconf-get-track track)))
  (let ((info (tramp-dissect-file-name (emacsconf-stream-track-login track))))
    (apply
     #'start-process
     (delq nil
					 (append
						(list
						 (concat "ssh-" (plist-get track :id))
						 (concat "*" (plist-get track :name) "*") "ssh"
						 (concat (tramp-file-name-user info)
										 "@" (tramp-file-name-host info))
						 "-p" (tramp-file-name-port info)
						 (format "DISPLAY=%s" (plist-get track :vnc-display)))
						(if (listp (car commands)) (car commands) commands))))))

(defun emacsconf-stream-play-intro (talk)
  "Play the recorded intro or display the in-between slide for TALK."
  (interactive (list (emacsconf-complete-talk-info)))
  (setq talk (emacsconf-resolve-talk talk))
  (emacsconf-stream-track-ssh talk "nohup" "intro" (plist-get talk :slug)))

(defun emacsconf-stream-play-talk-on-change (talk)
  "Play the talk if the track's autopilot is set to 'todo."
  (interactive (list (emacsconf-complete-talk-info)))
  (setq talk (emacsconf-resolve-talk talk))
	(let ((track (emacsconf-get-track (plist-get talk :track))))
		(when (eq (plist-get track :autopilot) 'todo)
			(when (or (not (boundp 'org-state)) (string= org-state "PLAYING"))
				(if (plist-get talk :stream-files)
						(progn
							(emacsconf-stream-track-ssh
							 talk
							 "overlay"
							 (plist-get talk :slug))
							(emacsconf-stream-track-ssh
							 talk
							 (append
								(list
								 "nohup"
								 "mpv")
								(split-string-and-unquote (plist-get talk :stream-files))
								(list "&"))))
					(emacsconf-stream-track-ssh
					 talk
					 (cons
						"nohup"
						(cond
						 ((and
							 (plist-get talk :recorded-intro)
							 (plist-get talk :video-file)) ;; recorded intro and recorded talk
							(message "should automatically play intro and recording")
							(list "play-with-intro" (plist-get talk :slug))) ;; todo deal with stream files
						 ((and
							 (plist-get talk :recorded-intro)
							 (null (plist-get talk :video-file))) ;; recorded intro and live talk; play the intro and join BBB
							(message "should automatically play intro; join %s" (plist-get talk :bbb-backstage))
							(list "intro" (plist-get talk :slug)))
						 ((and
							 (null (plist-get talk :recorded-intro))
							 (plist-get talk :video-file)) ;; live intro and recorded talk, show slide and use Mumble; manually play talk
							(message "should show intro slide; play %s afterwards" (plist-get talk :slug))
							(list "intro" (plist-get talk :slug)))
						 ((and
							 (null (plist-get talk :recorded-intro))
							 (null (plist-get talk :video-file))) ;; live intro and live talk, join the BBB
							(message "join %s for live intro and talk" (plist-get talk :bbb-backstage))
							(list "bbb" (plist-get talk :slug)))))))))))

(defun emacsconf-stream-get-filename (talk)
  "Return the local filename for the video file for TALK.
Final files should be stored in /data/emacsconf/stream/YEAR/file-prefix--main.webm."
  (expand-file-name
   (concat (plist-get talk :file-prefix) "--main.webm")
   (expand-file-name emacsconf-year
                     emacsconf-stream-dir)))

(defun emacsconf-stream-play-video (talk)
	"Play just the video for TALK."
  (interactive (list (emacsconf-complete-talk-info)))
  (setq talk (emacsconf-resolve-talk talk))
  (emacsconf-stream-track-ssh
	 talk "nohup" "play" (plist-get talk :slug)))

(defun emacsconf-stream-play-video-file (talk filename)
  (interactive (list (emacsconf-complete-talk-info)))
  (setq talk (emacsconf-resolve-talk talk))
  (apply
   #'emacsconf-stream-track-ssh
   talk
	 "nohup"
   "overlay"
   (plist-get talk :slug))
  (apply
   #'emacsconf-stream-track-ssh
   talk
	 "nohup"
   "mpv"
   filename))

(defun emacsconf-stream-open-pad (talk)
	"Open the Etherpad collaborative document for TALK."
  (interactive (list (emacsconf-complete-talk-info)))
	(setq talk (emacsconf-resolve-talk talk))
	(emacsconf-stream-track-ssh
	 talk
	 "nohup"
	 "firefox"
	 (plist-get talk :pad-url)))

(defun emacsconf-stream-bbb (talk)
	"Open the BBB room for TALK."
	(interactive (list (emacsconf-complete-talk-info)))
	(setq talk (emacsconf-resolve-talk talk))
	(emacsconf-stream-track-ssh talk "nohup" "bbb" (plist-get talk :slug)))

(defun emacsconf-stream-join-qa (talk)
  "Join the Q&A for TALK.
This uses the BBB room if available, or the IRC channel if not."
  (interactive (list (emacsconf-complete-talk-info)))
  (if (and (null (plist-get talk :video-file))
					 (string-match "live" (plist-get talk :q-and-a)))
			(emacsconf-stream-track-ssh
			 talk
			 "nohup"
			 "firefox"
			 "-new-window"
			 (plist-get talk :pad-url))
		(emacsconf-stream-track-ssh
		 talk
		 "nohup"
		 "firefox"
		 "-new-window"
		 (pcase (plist-get talk :q-and-a)
			 ((or 'nil "" (rx "Mumble"))
				(plist-get talk :qa-slide-url))
			 ((rx "live")
				(plist-get talk :bbb-backstage))
			 ((rx "IRC")
				(plist-get talk :webchat-url))
			 ((rx "pad")
				(plist-get talk :pad-url))
			 (_ (plist-get talk :qa-slide-url))))))

(defun emacsconf-stream-join-chat (talk)
  "Join the IRC chat for TALK."
  (interactive (list (emacsconf-complete-talk-info)))
	(setq talk (emacsconf-resolve-talk talk))
	(emacsconf-stream-track-ssh
	 talk
	 "nohup"
	 "firefox"
	 (plist-get talk :webchat-url)))

(defun emacsconf-stream-write-talk-overlay-svgs (talk video-filename)
  (setq talk (emacsconf-stream-add-talk-props talk))
  (let ((dom (xml-parse-file (expand-file-name "roles/obs/overlay.svg" emacsconf-ansible-directory)))
        (default-directory (file-name-directory video-filename)))
    (emacsconf-stream-svg-set-text dom "url" (plist-get talk :overlay-url))
    (with-temp-file video-filename (dom-print dom))
		(shell-command
     (concat "inkscape --export-type=png -w 1280 -h 720 --export-background-opacity=0 "
             (shell-quote-argument (file-name-nondirectory video-filename))))))

(defvar emacsconf-stream-asset-dir "/data/emacsconf/assets/")
(defvar emacsconf-stream-overlay-dir "/data/emacsconf/assets/overlays/")

(defun emacsconf-stream-set-overlay (talk)
	"Reset the overlay for TALK, just in case.
With a prefix argument (\\[universal-argument]), clear the overlay."
	(interactive (list
								(if current-prefix-arg
										(emacsconf-complete-track)
									(emacsconf-complete-talk-info))))
	(emacsconf-stream-track-ssh
	 (emacsconf-get-track talk)
	 "overlay"
	 (if current-prefix-arg
			 "blank"
		 (plist-get talk :slug))))

(defun emacsconf-stream-generate-overlays (&optional info force)
	"Make overlays for INFO."
  (interactive (list (emacsconf-get-talk-info) current-prefix-arg))
  (setq info (emacsconf-publish-prepare-for-display (or info (emacsconf-get-talk-info))))
  (unless (file-directory-p emacsconf-stream-overlay-dir)
    (make-directory emacsconf-stream-overlay-dir t))
  (mapc (lambda (talk)
          (when (or force (null (file-exists-p (expand-file-name (concat (plist-get talk :slug) "-video.png") emacsconf-stream-overlay-dir))))
            (emacsconf-stream-write-talk-overlay-svgs
             talk
             (expand-file-name (concat (plist-get talk :slug) "-video.svg") emacsconf-stream-overlay-dir))))
        info)
  (emacsconf-stream-write-talk-overlay-svgs
   nil
   (expand-file-name "blank-video.svg" emacsconf-stream-overlay-dir)))

(defun emacsconf-stream-generate-test-videos (&optional info)
	"Generate 1-minute test videos for INFO."
	(interactive)
  (setq info (or info (emacsconf-publish-prepare-for-display (emacsconf-get-talk-info))))
	(let* ((dir (expand-file-name "test" emacsconf-stream-asset-dir))
				 (default-directory dir)
				 (subed-default-subtitle-length 1000)
				 (test-length 60))
		(unless (file-directory-p dir)
			(make-directory dir t))
		(shell-command
		 (format "ffmpeg -y -f lavfi -i testsrc=duration=%d:size=1280x720:rate=10 -i background-music.opus -shortest %s "
						 test-length (expand-file-name "template.webm" dir)))
		(dolist (talk info)
			(with-temp-file (expand-file-name (concat (plist-get talk :file-prefix) "--main.vtt") dir)
				(subed-vtt-mode)
				(subed-auto-insert)
				(dotimes (i test-length)
					(subed-append-subtitle
					 nil
					 (* i 1000)
					 (1- (* i 1000))
					 (format "%s %02d %s"
									 (plist-get talk :slug)
									 i
									 (substring "123456789 123456789 123456789 123456789 123456789 123456789 "
															(1+ (length (format "%s %02d" (plist-get talk :slug) i))))))))
			(copy-file
			 (expand-file-name "template.webm" dir)
			 (expand-file-name (concat (plist-get talk :file-prefix) "--main.webm") dir)
			 t))))

(defun emacsconf-stream-display-talk-info (talk)
  (interactive (list (emacsconf-complete-talk-info)))
  (let ((buf (get-buffer-create "*EmacsConf*"))
        (title-mult 1.5)
        (title (plist-get talk :title)))
    (modus-themes-load-operandi)
    (switch-to-buffer buf)
    (erase-buffer)
    (face-remap-add-relative 'default '(:height 150))
    (insert
     (replace-regexp-in-string
      "https://" ""
      (concat
       "\n\n\n\n\n\n"
       (propertize
        (string-join
         (apply
          #'append
          (mapcar
           (lambda (s)
             (org-wrap s 40))
           (if (string-match "\\(.*:\\) \\(.*\\)" title)
               (list (match-string 1 title)
                     (match-string 2 title))
             (list title))))
         "\n")
        'face `(:height ,(floor (* 200 title-mult)) :weight bold)) "\n\n"
       (emacsconf-surround "" (plist-get talk :speakers-with-pronouns) "\n" "")
       (emacsconf-surround "Info: "
                           (if (plist-get talk :url)
                               (concat emacsconf-base-url (plist-get talk :url)))
                           "\n" "")
       (emacsconf-surround "Pad: " (plist-get talk :pad-url) "\n" "")
       (emacsconf-surround "Q&A: " (plist-get talk :qa-info) "\n" "")
       (emacsconf-surround "IRC: #" (plist-get talk :channel) "\n" ""))))
    (display-time-mode -1)
    (when (functionp 'hl-line-mode)
      (global-hl-line-mode -1))
    (set-window-margins nil 10 10)))

(defun emacsconf-stream-generate-title-page-in-emacs (talk)
  (interactive (list (emacsconf-complete-talk-info)))
  (emacsconf-stream-display-talk-info talk)
  (message nil)
  (with-temp-file (expand-file-name (concat (plist-get talk :slug) "-title.svg")
                                    (expand-file-name "titles" emacsconf-stream-asset-dir))
    (insert (x-export-frames nil 'svg)))
  (shell-command
     (concat "inkscape --export-type=png --export-dpi=96 --export-background-opacity=0 "
             (shell-quote-argument (expand-file-name (concat (plist-get talk :slug) "-title.svg")
                                    (expand-file-name "titles" emacsconf-stream-asset-dir))))))

(defun emacsconf-stream-generate-title-pages-in-emacs (&optional info)
  (interactive)
  (setq info (emacsconf-publish-prepare-for-display (or info (emacsconf-get-talk-info))))
  (let ((title-dir (expand-file-name "titles" emacsconf-stream-asset-dir)))
    (unless (file-directory-p title-dir) (make-directory title-dir t))
    (set-frame-size nil 1280 720 t)
    (mapc #'emacsconf-stream-generate-title-page info)))

(defun emacsconf-stream-generate-in-between-page (talk &optional prev info force)
	(interactive (list (emacsconf-complete-talk-info)))
	(let* ((prev (or prev (emacsconf-previous-talk talk info)))
				 (dir (expand-file-name "in-between" emacsconf-stream-asset-dir))
         (template (expand-file-name "template.svg" dir))
				 (dom (xml-parse-file template)))
		(mapc (lambda (entry)
            (let ((prefix (car entry)))
              (emacsconf-stream-svg-set-text dom (concat prefix "title")
                                             (plist-get (cdr entry) :title))
              (emacsconf-stream-svg-set-text dom (concat prefix "speakers")
                                             (plist-get (cdr entry) :speakers-with-pronouns))
              (emacsconf-stream-svg-set-text dom (concat prefix "url")
                                             (and (cdr entry) (concat emacsconf-base-url (plist-get (cdr entry) :url))))
              (emacsconf-stream-svg-set-text
               dom
               (concat prefix "qa")
               (pcase (plist-get (cdr entry) :q-and-a)
                 ("live" "Live Q&A after talk")
                 ("IRC" "IRC Q&A after talk")
                 (_ "")))))
          (list (cons "previous-" prev)
                (cons "current-" talk)))
    (with-temp-file (expand-file-name (concat (plist-get talk :slug) ".svg") dir)
      (dom-print dom))
    (when (or force (null (file-exists-p (expand-file-name (concat (plist-get talk :slug) ".png")
																													 dir))))
      (shell-command
       (concat "inkscape --export-type=png -w 1280 -h 720 --export-background-opacity=0 "
               (shell-quote-argument (expand-file-name (concat (plist-get talk :slug) ".svg")
                                                       dir))))
			(shell-command
       (concat "mogrify -alpha off "
               (shell-quote-argument (expand-file-name (concat (plist-get talk :slug) ".png")
                                                       dir))))
			(expand-file-name (concat (plist-get talk :slug) ".png")
                        dir))))
;; (emacsconf-stream-generate-in-between-page (emacsconf-resolve-talk "science") nil nil t)
;; (emacsconf-stream-generate-in-between-page (emacsconf-resolve-talk "health") nil nil t)
;; (emacsconf-stream-generate-in-between-page (emacsconf-resolve-talk "eev") nil nil t)

(defun emacsconf-stream-generate-in-between-pdf (&optional info)
	(interactive)
  (setq info (or emacsconf-schedule-draft (emacsconf-publish-prepare-for-display (emacsconf-filter-talks (or info (emacsconf-get-talk-info))))))
  (let* ((by-track (seq-group-by (lambda (o) (plist-get o :track)) info))
         (dir (expand-file-name "in-between" emacsconf-stream-asset-dir))
				 (default-directory dir))
		(dolist (track by-track)
			(shell-command
			 (concat
				"convert "
				(mapconcat (lambda (talk) (shell-quote-argument
																	 (concat (plist-get talk :slug) ".svg.png")))
									 (emacsconf-filter-talks (cdr track))
									 " ")
				" "
				(plist-get (emacsconf-get-track (car track)) :id)
				"-in-between.pdf"
				)))))

(defun emacsconf-stream-generate-in-between-pages (&optional info)
	"Generate the title images."
  (interactive)
  (setq info (or emacsconf-schedule-draft (emacsconf-publish-prepare-for-display (emacsconf-filter-talks (or info (emacsconf-get-talk-info))))))
  (let* ((by-track (seq-group-by (lambda (o) (plist-get o :track)) info))
         (dir (expand-file-name "in-between" emacsconf-stream-asset-dir))
         (template (expand-file-name "template.svg" dir)))
		(unless (file-directory-p dir)
			(make-directory dir t))
    (mapc (lambda (track)
            (let (prev)
              (mapc (lambda (talk)
                      (let ((dom (xml-parse-file template)))
                        (mapc (lambda (entry)
                                (let ((prefix (car entry)))
                                  (emacsconf-stream-svg-set-text dom (concat prefix "title")
                                                 (plist-get (cdr entry) :title))
                                  (emacsconf-stream-svg-set-text dom (concat prefix "speakers")
                                                 (plist-get (cdr entry) :speakers))
                                  (emacsconf-stream-svg-set-text dom (concat prefix "url")
                                                 (and (cdr entry) (concat emacsconf-base-url (plist-get (cdr entry) :url))))
                                  (emacsconf-stream-svg-set-text
                                   dom
                                   (concat prefix "qa")
                                   (pcase (plist-get (cdr entry) :q-and-a)
                                     ((rx "live") "Live Q&A after talk")
                                     ((rx "pad") "Etherpad")
																		 ((rx "IRC") "IRC Q&A after talk")
                                     (_ "")))))
                              (list (cons "previous-" prev)
                                    (cons "current-" talk)))
                        (with-temp-file (expand-file-name (concat (plist-get talk :slug) ".svg") dir)
                          (dom-print dom))
												(shell-command
                         (concat "inkscape --export-type=png -w 1280 -h 720 --export-background-opacity=0 "
                                 (shell-quote-argument (expand-file-name (concat (plist-get talk :slug) ".svg")
                                                                         dir)))))
                      (setq prev talk))
                    (emacsconf-filter-talks (cdr track)))))
          by-track)))

(defun emacsconf-stream-generate-titles (&optional info)
  (interactive)
  (setq info (or emacsconf-schedule-draft (emacsconf-publish-prepare-for-display (emacsconf-filter-talks (or info (emacsconf-get-talk-info))))))
  (let* ((by-track (seq-group-by (lambda (o) (plist-get o :track)) info))
         (dir (expand-file-name "titles" emacsconf-stream-asset-dir))
         (template (expand-file-name "template.svg" (expand-file-name "in-between"
																																			emacsconf-stream-asset-dir))))
		(unless (file-directory-p dir)
			(make-directory dir t))
    (mapc (lambda (track)
            (let (prev)
              (mapc (lambda (talk)
                      (let ((dom (xml-parse-file template)))
												;; hide all the previous stuff
												(svg-remove dom "g-prev")
												(svg-remove dom "divider")
												(svg-remove dom "current-qa")
												(emacsconf-stream-svg-set-text dom "current-title" (plist-get talk :title))
                        (emacsconf-stream-svg-set-text dom "current-speakers" (plist-get talk :speakers))
                        (emacsconf-stream-svg-set-text dom "current-url" (plist-get talk :absolute-url))
                        (with-temp-file (expand-file-name (concat (plist-get talk :slug) ".svg") dir)
                          (dom-print dom))
												(shell-command
                         (concat "inkscape --export-type=png -w 1280 -h 720 --export-background-opacity=0 "
                                 (shell-quote-argument (expand-file-name (concat (plist-get talk :slug) ".svg")
                                                                         dir)))))
                      (setq prev talk))
                     (emacsconf-filter-talks (cdr track)))))
          by-track)))


(defun emacsconf-stream-generate-test-subtitles (&optional info)
  (interactive)
  (setq info (emacsconf-filter-talks (or info (emacsconf-get-talk-info))))
  (let ((dir (expand-file-name "test" emacsconf-stream-asset-dir))
        (subtitle-fill (substring "0123456789012345678901234567890123456789012345678901234567890123456789" 0 55))
        (subtitle-len 1))
    (unless (file-directory-p dir) (make-directory dir t))
    (mapc (lambda (talk)
            (with-temp-file (expand-file-name (concat (plist-get talk :file-prefix) "--main.vtt")
                                              dir)
              (insert "WEBVTT\n\n"
                      (cl-loop
                       for i from 0 to (/ 60 subtitle-len)
                       concat
                       (let ((sub-prefix (format "%s %d "
                                                 (plist-get talk :slug)
                                                 i)))
                         (format "%s --> %s\n%s%s\n\n"
                                 (format-seconds "00:%.2m:%.2s.000"
                                                 (* subtitle-len i))
                                 (format-seconds "00:%.2m:%.2s.900"
                                                 (1- (* subtitle-len (1+ i))))
                                 sub-prefix
                                 (substring subtitle-fill (length sub-prefix))))))))
          info)))

(defun emacsconf-stream-generate-test-videos (&optional info)
  (interactive)
  (setq info (emacsconf-filter-talks (or info (emacsconf-get-talk-info))))
  (let ((dir (expand-file-name "test" emacsconf-stream-asset-dir)))
    (unless (file-directory-p dir) (make-directory dir t))
    (mapc (lambda (talk)
            (add-name-to-file (expand-file-name "template.webm" dir)
                              (expand-file-name (concat (plist-get talk :file-prefix) "--main.webm") dir)
                              t))
          info)))

(defun emacsconf-stream-handle-talk-timer (talk)
  (interactive (list (save-match-data (emacsconf-complete-talk-info))))
  (save-window-excursion
    (save-match-data
      (undo-boundary)
      (message "Start %s" talk)
      (with-local-quit
	(ignore-error 'remote-file-error
	  (emacsconf-with-talk-heading (plist-get talk :slug)
	    (org-todo "PLAYING"))))
      (message "Done %s" talk)
      (undo-boundary))))

(defun emacsconf-stream-generate-assets-for-talk (talk)
  (interactive (list (emacsconf-complete-talk-info)))
  (let ((info (list talk)))
    (emacsconf-stream-generate-test-videos info)
    (emacsconf-stream-generate-test-subtitles info)
    (emacsconf-stream-generate-title-pages info)
    (emacsconf-stream-generate-overlays info)))
;; (emacsconf-stream-display-talk-info
;;  '(:title "The ship that builds itself: How we used Emacs to develop a workshop for communities"
;;           :speakers-with-pronouns "Noorah Alhasan (she/her), Joseph Corneli (he/him), Leo Vivier (he/him)"
;;           :url "2022/talks/community"
;;           :pad-url "https://pad.emacsconf.org/2022-community"
;;           :channel "emacsconf-gen"
;;           :qa-info "https://emacsconf.org/current/community/room/")
;;  )

;; Updates live.emacsconf.org
(defvar emacsconf-stream-live-index "/ssh:orga@front0.emacsconf.org:/var/www/live.emacsconf.org/index.html")

(defun emacsconf-stream-shift-days ()
  "Return a label for the conference days."
  (let ((start
         (date-to-time (car (sort (mapcar (lambda (o) (plist-get o :start))
                                          emacsconf-shifts)
                                  'string<))))
        (end
         (date-to-time (car (last (sort (mapcar (lambda (o) (plist-get o :end))
                                                emacsconf-shifts)
                                        'string<))))))
    (if (string= (format-time-string "%Y-%m-%d" start emacsconf-timezone)
                 (format-time-string "%Y-%m-%d" end emacsconf-timezone))
        (format-time-string "%A, %b %-d, %Y"
                            start emacsconf-timezone)
      (concat
       (format-time-string "%b %-d (%a)"
                           start emacsconf-timezone)
       " to "
       (format-time-string "%b %-d (%a), %Y"
                           end emacsconf-timezone)))))

(defvar emacsconf-status-timezones '("US/Eastern" "UTC" "Europe/Berlin"))
(defun emacsconf-stream-update-status-page ()
  (interactive)
  (with-temp-file emacsconf-stream-live-index
    (insert
     (emacsconf-replace-plist-in-string
      (list :name emacsconf-name
            :year emacsconf-year
            :base-url emacsconf-base-url
            :days (emacsconf-stream-shift-days)
            :timezone-info
            (mapconcat (lambda (zone) (concat "<th>" zone "</th>"))
                       emacsconf-status-timezones "")
            :stream-info
            (mapconcat
             (lambda (track)
               (emacsconf-replace-plist-in-string
                (append (list :stream-base emacsconf-stream-base)
                        track)
                "<tr><td>${id}</td><td><a href=\"${stream-base}${id}.webm\">${stream-base}${id}.webm</a></td></tr>
<tr><td>${id}-480p</td><td><a href=\"${stream-base}${id}-480p.webm\">${stream-base}${id}-480p.webm</a></td></tr>"))
             emacsconf-tracks "\n")
            :watch-info
            (mapconcat
             (lambda (track)
               (let ((start-time (date-to-time (concat emacsconf-date "T" (plist-get track :start) emacsconf-timezone-offset))))
                 (emacsconf-replace-plist-in-string
                  (append
                   (list
                    :time-info
                    (mapconcat
                     (lambda (zone)
                       (format-time-string "<td>%l:%M %p %Z</td>" start-time zone))
                     emacsconf-status-timezones
                     ""))
                   track)
                  "<tr>
            <td><a href=\"${watch}\"><strong>${name}</strong></a></td><td class=\"status-${status}\">${status}</td>${time-info}
</tr>")))
             emacsconf-tracks "\n")
            )
      "<!doctype html>
<html lang=\"en\">
  <head>
    <meta charset=\"utf-8\">
    <meta name=viewport content=\"width=device-width, initial-scale=1\">
    <meta name=\"description\" content=\"${name} Livestreams\"/>
    <title>${name} ${year} ~ Live</title>
    <link rel=\"stylesheet\" href=\"/style.css\" />
  </head>
  <body>
    <header>
      <h1><a href=\"${base-url}${year}/\">${name} ${year}</a> Livestreams</h1>
      <h3>December 2 (Saturday) and 3 (Sunday), 2023</h3>
    </header>
    <main>
      <table>
        <thead>
          <tr>
            <th>track</th>
            <th>status</th>
${timezone-info}
          </tr>
        </thead>
        <tbody>
${watch-info}
        </tbody>
      </table>
      <p>
        Click on the track names above to watch the stream in your browser.
      </p>
      <h4>Watching the streams directly</h4>
      <p>
        You can also watch the streams in any media player that supports streaming.
      </p>
      <table>
        <thead>
          <tr>
            <th>track</th>
            <th>stream address</th>
          </tr>
        </thead>
        <tbody>
${stream-info}
        </tbody>
      </table>
      <p>
        Depending on which media player you use, you may enter the stream address
        in a graphical user interface or provide it as an argument to the program
        when launching it from the terminal.
      </p>
      <p>
        Examples:
      </p>
<pre>
mpv URL
vlc URL
ffplay URL
</pre>
    </main>
    <footer>
      <p>
        If you experience any disruptions, please check our status page at
        <a href=\"//status.emacsconf.org\">status.emacsconf.org</a> for updates
        on the status of various parts of our infrastructure, and instructions
        on how to get in touch with us about disruptions.
      </p>
    </footer>
  </body>
</html>
"))))

;;; Clock and display

(defvar emacsconf-stream-track "General")
(defvar emacsconf-stream-clock-buffer "*emacsconf*")
(defvar emacsconf-stream-clock-timer nil)

(require 'diary-lib)
(require 'text-property-search)
(defun emacsconf-stream-display-clock-and-countdown (&optional time message)
  "TIME is HH:MM."
  (interactive "MTime: \nMMessage: ")
  (switch-to-buffer (get-buffer-create emacsconf-stream-clock-buffer))
  (erase-buffer)
  (when (string= time "") (setq time nil))
  (when (string= message "") (setq message nil))
  (let* ((hhmm (and time (diary-entry-time time)))
         (target (and time
                      (time-to-seconds (date-to-time
                                        (concat (format-time-string "%Y-%m-%d" nil emacsconf-timezone)
                                                "T"
                                                (format "%02d:%02d:00"
                                                        (/ hhmm 100)
                                                        (% hhmm 100))
                                                emacsconf-timezone-offset))))))
    (face-remap-add-relative 'default :height 300)
    (insert
     (propertize
      "CURRENT"
      'face '(:weight bold :height 600)
      'emacsconf-time (lambda () (format-time-string "%H:%M:%S %Z" nil emacsconf-timezone)))
     " (" emacsconf-timezone ")\n"
     (propertize (concat "Track: " emacsconf-stream-track "\n")
                 'face '(:height 400))
     "IRC: #" (plist-get (emacsconf-get-track emacsconf-stream-track) :channel) "\n"
     "\n"
     (if time (propertize
               "TO-GO"
               'emacsconf-time
               (lambda ()
                 (let ((seconds-to-go (- target
                                         (time-to-seconds (current-time)))))
                   (if (> (or seconds-to-go 0) 0)
                       (concat
                        (format-seconds "%.2h:%z%.2m:%.2s"
                                        seconds-to-go)
                        " to go" (if message ": " ""))
                     ""))))
       "")
     (or message ""))
    (when (timerp emacsconf-stream-clock-timer) (cancel-timer emacsconf-stream-clock-timer))
    (emacsconf-stream-update-time)
    (setq emacsconf-stream-clock-timer (run-at-time t 1 #'emacsconf-stream-update-time))))

(defun emacsconf-stream-update-time ()
	"Update the displayed time."
  (if (get-buffer emacsconf-stream-clock-buffer)
      (when (get-buffer-window emacsconf-stream-clock-buffer)
        (with-current-buffer emacsconf-stream-clock-buffer
          (save-excursion
            (goto-char (point-min))
            (let (match)
              (while (setq match (text-property-search-forward 'emacsconf-time))
                (goto-char (prop-match-beginning match))
                (add-text-properties
                 (prop-match-beginning match)
                 (prop-match-end match)
                 (list 'display
                       (funcall (get-text-property
                                 (prop-match-beginning match)
                                 'emacsconf-time))))
                (goto-char (prop-match-end match)))))))
    (when (timerp emacsconf-stream-clock-timer)
      (cancel-timer emacsconf-stream-clock-timer))))

;;; Timers

(defvar emacsconf-stream-timers nil "List of timers for easy reference")

(defun emacsconf-stream-cancel-timer (id)
	"Cancel a timer by ID."
	(interactive (list
								(completing-read
								 "ID: "
								 (lambda (string pred action)
									  (if (eq action 'metadata)
												`(metadata (display-sort-function . ,#'identity))
											(complete-with-action action
																						(sort
																						 (seq-filter (lambda (o)
																													 (and (timerp (cdr o))
																																(not (timer--triggered (cdr o)))))
																												 emacsconf-stream-timers)
																						 (lambda (a b) (string< (car a) (car b))))
																						string pred))))))
	(when (timerp (assoc-default id emacsconf-stream-timers))
		(cancel-timer (assoc-default id emacsconf-stream-timers))
		(setq emacsconf-stream-timers
					(delq (assoc id emacsconf-stream-timers)
								(seq-filter (lambda (o)
															(and (timerp (cdr o))
																	 (not (timer--triggered (cdr o)))))
														emacsconf-stream-timers)))))

(defun emacsconf-stream-schedule-talk-status-change (talk time new-status &optional notification)
	"Schedule a one-off timer for TALK at TIME to set it to NEW-STATUS."
	(interactive (list (emacsconf-complete-talk-info)
										 (read-string "Time: ")
										 (completing-read "Status: " (mapcar 'car emacsconf-status-types))))
	(require 'diary-lib)
	(setq talk (emacsconf-resolve-talk talk))
	(let* ((converted
					(cond
					 ((listp time) time)
					 ((timer-duration time) (timer-relative-time nil (timer-duration time)))
					 (t														; HH:MM
						(date-to-time (concat (format-time-string "%Y-%m-%d" nil emacsconf-timezone)
																	"T"
																	(string-pad time 5 ?0 t)
																	emacsconf-timezone-offset)))))
				 (timer-id (concat (format-time-string "%m-%dT%H:%M" converted)
													 "-"
													 (plist-get talk :slug)
													 "-"
													 new-status)))
		(emacsconf-stream-cancel-timer timer-id)
		(add-to-list 'emacsconf-stream-timers
									(cons
									 timer-id
									 (run-at-time time converted #'emacsconf-stream-update-talk-status-from-timer
																talk new-status
																notification)))))

(defun emacsconf-stream-update-talk-status-from-timer (talk new-status &optional notification)
	;; (when notification
	;; 	(apply #'notifications-notify notification))
	(emacsconf-update-talk-status-with-hooks (plist-get talk :slug) "." new-status))

(defun emacsconf-stream-schedule-timers (&optional info)
	"Schedule PLAYING for the rest of talks and CLOSED_Q for recorded talks."
  (interactive)
	(emacsconf-stream-cancel-all-timers)
  (setq info (emacsconf-publish-prepare-for-display (emacsconf-filter-talks (or info (emacsconf-get-talk-info)))))
  (let ((now (current-time)))
    (mapc (lambda (talk)
            (when (and (time-less-p now (plist-get talk :start-time)))
							(emacsconf-stream-schedule-talk-status-change talk (plist-get talk :start-time) "PLAYING"
																														`(:title (concat "Starting " (plist-get talk :slug)))))
						(when (and
									 (plist-get talk :video-file)
									 (plist-get talk :qa-time)
									 (not (string-match "none" (or (plist-get talk :q-and-a) "none")))
									 (null (plist-get talk :stream-files)) ;; can't tell when this is
									 (time-less-p now (plist-get talk :qa-time)))
							(emacsconf-stream-schedule-talk-status-change talk (plist-get talk :qa-time) "CLOSED_Q"
																														`(:title (concat "Q&A for " (plist-get talk :slug) " (" (plist-get talk :q-and-a) ")"))))
						)
          info)))

;; (notifications-notify :title "Hello")
(defun emacsconf-stream-cancel-all-timers ()
	(interactive)
	(cancel-function-timers #'emacsconf-update-talk-status-with-hooks)
	(cancel-function-timers #'emacsconf-stream-update-talk-status-from-timer)
	(setq emacsconf-stream-timers nil))

(defun emacsconf-stream-send-to-mpv (talk-or-track command &optional parse)
	(interactive (list (emacsconf-complete-track)))
	(setq talk-or-track (emacsconf-get-track talk-or-track))
	(let* ((default-directory (emacsconf-stream-track-login talk-or-track))
				 (response (shell-command-to-string
										(format
										 "echo %s | socat - ~/mpv-socket"
										 (shell-quote-argument (if (listp command) (json-encode command) command))))))
		(if parse (json-parse-string response) response)))

(defun emacsconf-stream-show-playback-info (talk-or-track)
	(interactive (list (emacsconf-complete-track)))
	(let* ((default-directory (emacsconf-stream-track-login talk-or-track))
				 (playback-position
					(gethash "data"
									 (emacsconf-stream-send-to-mpv
										talk-or-track
										'(:command ("get_property" "playback-time"))
										t)))
				 (duration (gethash "data" (emacsconf-stream-send-to-mpv
																		talk-or-track
																		'(:command ("get_property" "duration"))
																		t))))
		(message "%s of %s (%s remaining, ending at %s)"
						 (emacsconf-format-seconds playback-position)
						 (emacsconf-format-seconds duration)
						 (emacsconf-format-seconds (- duration playback-position))
						 (format-time-string
							"%H:%M %p"
							(time-add (current-time) (seconds-to-time (- duration playback-position)))
							emacsconf-timezone))))

(defun emacsconf-stream-rebroadcast (source-track dest-track)
	(interactive
	 (let* ((source (emacsconf-complete-track "Source: "))
					(others (remove source emacsconf-tracks)))
		 (list
			source
			(if (= (length others) 1)
					(car others)
				(emacsconf-complete-track "Destination: " others)))))
	(setq source-track (emacsconf-get-track source-track))
	(setq dest-track (emacsconf-get-track dest-track))
	(emacsconf-stream-track-ssh dest-track "nohup" "mpv" (plist-get source-track :stream) "--profile=full" "&"))
;; (emacsconf-stream-rebroadcast "Development" "General")
;; (emacsconf-stream-send-to-mpv "General" '(:command ("video-zoom" "0")))
;; (emacsconf-stream-send-to-mpv "General" "set video-zoom 0")
;;; xdotool

(defun emacsconf-stream-xdotool (track command)
	(setq track (emacsconf-get-track track))
	(let ((default-directory (plist-get track :tramp)))
		(shell-command-to-string (concat "DISPLAY=" (plist-get track :vnc-display) " xdotool " command))))

(defun emacsconf-stream-xdotool-set-up-bbb (track)
	(interactive (list (emacsconf-complete-track)))
	(when (stringp track)
		(setq track (or (emacsconf-get-track track)
										(emacsconf-get-track (emacsconf-resolve-talk track)))))
	(emacsconf-stream-xdotool track "mousemove 806 385 click 1 sleep 2")
	;; type emacsconf
	(emacsconf-stream-xdotool track "mousemove 791 512 click 1 sleep 1 key Ctrl+a type emacsconf")
	(emacsconf-stream-xdotool track "mousemove 1043 521 click 1 sleep 4")
	;; listen only
	(emacsconf-stream-xdotool track "mousemove 720 461 click 1 sleep 2")
	;; hide the chat
	(emacsconf-stream-xdotool track "mousemove 553 157 click 1")
	;; go full-screen
	(emacsconf-stream-xdotool track "key F11"))

;;; audio levels

(defun emacsconf-stream-audio-louder (track source)
	(interactive (list (emacsconf-complete-track) (completing-read "Source: " '("qa" "mumble"))))
	(emacsconf-stream-audio-set track source "+5%"))

(defun emacsconf-stream-audio-quieter (track source)
	(interactive (list (emacsconf-complete-track) (completing-read "Source: " '("qa" "mumble"))))
	(emacsconf-stream-audio-set track source "-5%"))

(defun emacsconf-stream-audio-set (track source volume)
	(interactive (list (emacsconf-complete-track) (completing-read "Source: " '("qa" "mumble"))
										 (read-string "Volume: ")))
	(setq track
				(if (stringp track)
						(or (emacsconf-get-track track)
								(emacsconf-get-track (emacsconf-resolve-talk track)))
					(emacsconf-get-track track)))
	(let ((default-directory (plist-get track :tramp)))
		(message "%s volume %s"
						 source
						 (string-trim
							(shell-command-to-string
							 (format
								"pactl set-sink-volume %s %s; pactl list sinks | perl -000ne 'if(/%s/){/Volume:.*?([0-9]+%%)/;print \"$1\n\"}'"
								source volume source))))))

(defun emacsconf-stream-audio-get-volume (track source)
	(interactive (list (emacsconf-complete-track) (completing-read "Source: " '("qa" "mumble"))))
	(setq track
				(if (stringp track)
						(or (emacsconf-get-track track)
								(emacsconf-get-track (emacsconf-resolve-talk track)))
					(emacsconf-get-track track)))
	(let ((default-directory (plist-get track :tramp)))
		(string-trim
		 (shell-command-to-string
			(format
			 "pactl list sinks | perl -000ne 'if(/%s/){/Volume:.*?([0-9]+%%)/;print \"$1\n\"}'"
			 source)))))

(defvar emacsconf-stream-console '("konsole" "-e"))
(defvar emacsconf-stream-mixer "pamix")

(defun emacsconf-stream-audio-mixer (track)
	(interactive (list (emacsconf-complete-track)))
	(setq track (emacsconf-get-track track))
	(let ((info (tramp-dissect-file-name (emacsconf-stream-track-login track))))
    (apply
     #'start-process
     (delq nil
					 (append
						(list
						 (concat "mixer-" (plist-get track :id))
						 (concat "*" (plist-get track :name) "*"))
						emacsconf-stream-console
						(list
						 "ssh"
						 "-t"
						 (concat (tramp-file-name-user info)
										 "@" (tramp-file-name-host info))
						 "-p" (tramp-file-name-port info)
						 (format "DISPLAY=%s" (plist-get track :vnc-display))
						 emacsconf-stream-mixer))))))

;; (emacsconf-stream-audio-get-volume "General" "qa")
;; (emacsconf-stream-audio-louder "General" "qa")
;; (emacsconf-stream-audio-quieter "General" "qa")

;;; Background music

(defun emacsconf-stream-start-music (track)
	(interactive (list (emacsconf-complete-track)))
	(emacsconf-stream-track-ssh track "nohup" "start-background-music" "&"))
(defun emacsconf-stream-stop-music (track)
	(interactive (list (emacsconf-complete-track)))
	(emacsconf-stream-track-ssh track "screen" "-S" "background" "-X" "quit"))

;;; Live

(defun emacsconf-stream-update-track-status (track &optional status)
	(interactive (list (emacsconf-complete-track)))
	(plist-put (emacsconf-get-track track)
						 :status (or status (completing-read "Status: " '("online" "offline"))))
	(emacsconf-stream-update-status-page))

(defun emacsconf-stream-edit-status-page ()
	(interactive)
	(find-file "/ssh:orga@front0.emacsconf.org:/var/www/status.emacsconf.org/index.html"))

;;; Autopilot with crontab

(defun emacsconf-stream-format-crontab (track talks &optional test-mode)
	"Return crontab entries for TALKS.
Use the display specified in TRACK.
If TEST-MODE is non-nil, load the videos from the test directory."
	(concat
	 (format
		"PATH=/usr/local/bin:/usr/bin
MAILTO=\"\"
XDG_RUNTIME_DIR=\"/run/user/%d\"
" (plist-get track :uid))
	 (mapconcat
		(lambda (talk)
			(format "%s /usr/bin/screen -dmS play-%s bash -c \"DISPLAY=%s TEST_MODE=%s /usr/local/bin/handle-session %s | tee -a ~/track.log\"\n"
							;; cron times are UTC
							(format-time-string "%-M %-H %-d %m *" (plist-get talk :start-time))
							(plist-get talk :slug)
							(plist-get track :vnc-display)
							(if test-mode "1" "")
							(plist-get talk :slug)))
		(emacsconf-filter-talks talks))))

(defun emacsconf-stream-crontabs (&optional test-mode info)
	"Write the streaming users' crontab files.
If TEST-MODE is non-nil, use the videos in the test directory.
If INFO is non-nil, use that as the schedule instead."
	(interactive)
	(let ((emacsconf-publishing-phase 'conference))
		(setq info (or info (emacsconf-publish-prepare-for-display (emacsconf-get-talk-info))))
		(dolist (track emacsconf-tracks)
			(let ((talks (seq-filter (lambda (talk)
																 (string= (plist-get talk :track)
																					(plist-get track :name)))
															 info))
						(crontab (expand-file-name (concat (plist-get track :id) ".crontab")
																			 (concat (plist-get track :tramp) "~"))))
				(with-temp-file crontab
					(when (plist-get track :autopilot)
						(insert (emacsconf-stream-format-crontab track talks test-mode))))
				(emacsconf-stream-track-ssh track (concat "crontab ~/" (plist-get track :id) ".crontab"))))))

(defun emacsconf-stream-cancel-crontab (track)
	"Remove crontab for TRACK."
	(interactive (list (emacsconf-complete-track)))
	(plist-put track :autopilot nil)
	(emacsconf-stream-track-ssh track "crontab -r"))

(defun emacsconf-stream-cancel-all-crontabs ()
	"Remove crontabs."
	(interactive)
	(dolist (track emacsconf-tracks)
		(plist-put track :autopilot nil)
		(emacsconf-stream-track-ssh track "crontab -r")))

(provide 'emacsconf-stream)
;;; emacsconf-stream.el ends here
