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
Files should be in YEAR/video-slug--main.webm and video-slug--main.vtt.")
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
    (setq text (svg--encode-text text))
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

(defun emacsconf-stream-open-qa-windows-on-change (talk)
  (interactive (list (emacsconf-complete-talk-info)))
  (when (or (not (boundp 'org-state)) (string= org-state "CLOSED_Q"))
    (let ((default-directory (emacsconf-stream-track-login talk)))
      (save-window-excursion
        (emacsconf-stream-open-pad talk)
        (emacsconf-stream-join-qa talk)
        (shell-command "i3-msg 'layout splith'")))))

(defun emacsconf-stream-update-talk-info-on-change (talk)
  "Update talk info."
  (when (string= org-state "PLAYING")
    (save-window-excursion
      (emacsconf-stream-set-talk-info talk))))

(defun emacsconf-stream-play-talk-on-change (talk)
  "Play the talk."
  (when (string= org-state "PLAYING")
    (save-window-excursion
      (emacsconf-stream-play-video talk))))

(defun emacsconf-stream-get-filename (talk)
  "Return the local filename for the video file for TALK.
Final files should be stored in /data/emacsconf/stream/YEAR/video-slug--main.webm."
  (expand-file-name
   (concat (plist-get talk :video-slug) "--main.webm")
   (expand-file-name emacsconf-year
                     emacsconf-stream-dir)))

(defun emacsconf-stream-play-video (talk)
  (interactive (list (emacsconf-complete-talk-info)))
  (let ((info (tramp-dissect-file-name (emacsconf-stream-track-login talk))))
    (apply
     #'call-process
     (append
      (list
       "ssh" nil nil t
		   (concat (tramp-file-name-user info)
			         "@" (tramp-file-name-host info))
		   "-p" (tramp-file-name-port info)
		   "nohup" "~/bin/track-mpv")
      (or (and (plist-get talk :stream-files)
               (split-string-and-unquote (plist-get talk :stream-files)))
          (list (emacsconf-stream-get-filename talk)))
      (list ">" "/dev/null" "2>&1" "&")))))

(defun emacsconf-stream-open-pad (talk)
  (interactive (list (emacsconf-complete-talk-info)))
  (let ((default-directory (emacsconf-stream-track-login talk))
        (async-shell-command-buffer 'new-buffer))
    (shell-command
     (concat "nohup firefox -new-window "
	           (shell-quote-argument (plist-get talk :pad-url))
	           " > /dev/null 2>&1 & "))))

(defun emacsconf-stream-join-qa (talk)
  "Join the Q&A for TALK.
This uses the BBB room if available, or the IRC channel if not."
  (interactive (list (emacsconf-complete-talk-info)))
  (let ((default-directory (emacsconf-stream-track-login talk))
        (async-shell-command-buffer 'new-buffer))
    (save-window-excursion
      (shell-command
       (concat "nohup firefox -new-window "
	             (shell-quote-argument
	              (if (string-match (plist-get talk :q-and-a) "IRC")
                    (plist-get talk :webchat-url)
                  (plist-get talk :bbb-room)))
	             " > /dev/null 2>&1 & ")))))

(defun emacsconf-stream-join-chat (talk)
  "Join the IRC chat for TALK."
  (interactive (list (emacsconf-complete-talk-info)))
  (let ((default-directory (emacsconf-stream-track-login talk))
        (async-shell-command-buffer 'new-buffer))
    (shell-command
     (concat "nohup firefox -new-window "
	     (shell-quote-argument
	      (plist-get talk :webchat-url))
	     " > /dev/null 2>&1 & "))))

(defun emacsconf-stream-write-talk-overlay-svgs (talk video-filename other-filename)
  (setq talk (emacsconf-stream-add-talk-props talk))
  (let ((dom (xml-parse-file (expand-file-name "roles/obs/overlay.svg" emacsconf-ansible-directory)))
        (default-directory (file-name-directory video-filename)))
    (emacsconf-stream-svg-set-text dom "bottom" (plist-get talk :overlay-bottom))
    (emacsconf-stream-svg-set-text dom "url" (plist-get talk :overlay-url))
    (with-temp-file other-filename (dom-print dom))
    (with-temp-file video-filename
      (let ((node (dom-by-id dom "bottom")))
        (when node
          (dom-set-attribute node 'style "visibility: hidden")
          (dom-set-attribute (dom-child-by-tag node 'tspan) 'style "fill: none; stroke: none")))
      (dom-print dom))
    (shell-command
     (concat "inkscape --export-type=png --export-dpi=96 --export-background-opacity=0 "
             (shell-quote-argument (file-name-nondirectory video-filename))))
    (shell-command
     (concat "inkscape --export-type=png --export-dpi=96 --export-background-opacity=0 "
             (shell-quote-argument (file-name-nondirectory other-filename))))))

(defvar emacsconf-stream-asset-dir "/data/emacsconf/assets/")
(defvar emacsconf-stream-overlay-dir "/data/emacsconf/assets/overlays/")

(defun emacsconf-stream-generate-overlays (&optional info)
  (interactive)
  (setq info (emacsconf-filter-talks (or info (emacsconf-get-talk-info))))
  (unless (file-directory-p emacsconf-stream-overlay-dir)
    (make-directory emacsconf-stream-overlay-dir))
  (mapc (lambda (talk)
          (unless (file-exists-p (expand-file-name (concat (plist-get talk :slug) "-video.png") emacsconf-stream-overlay-dir))
            (emacsconf-stream-write-talk-overlay-svgs
             talk
             (expand-file-name (concat (plist-get talk :slug) "-video.svg") emacsconf-stream-overlay-dir)
             (expand-file-name (concat (plist-get talk :slug) "-other.svg") emacsconf-stream-overlay-dir))))
        info)
  (emacsconf-stream-write-talk-overlay-svgs
   nil
   (expand-file-name "blank-video.svg" emacsconf-stream-overlay-dir)
   (expand-file-name "blank-other.svg" emacsconf-stream-overlay-dir)))

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

(defun emacsconf-stream-generate-title-page (talk)
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

(defun emacsconf-stream-generate-title-pages (&optional info)
  (interactive)
  (setq info (emacsconf-filter-talks (or info (emacsconf-get-talk-info))))
  (let ((title-dir (expand-file-name "titles" emacsconf-stream-asset-dir)))
    (unless (file-directory-p title-dir) (make-directory title-dir t))
    (set-frame-size nil 1280 720 t)
    (mapc #'emacsconf-stream-generate-title-page info)))

(defun emacsconf-stream-generate-in-between-pages (&optional info)
  (interactive)
  (setq info (emacsconf-prepare-for-display (emacsconf-filter-talks (or info (emacsconf-get-talk-info)))))
  (let* ((by-track (seq-group-by (lambda (o) (plist-get o :track)) info))
         (dir (expand-file-name "in-between" emacsconf-stream-asset-dir))
         (template (expand-file-name "template.svg" dir)))
    (mapc (lambda (track)
            (let (prev)
              (mapc (lambda (talk)
                      (let ((dom (xml-parse-file template)))
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
                        (unless (file-exists-p (expand-file-name (concat (plist-get talk :slug) ".png")
                                                                 dir))
                          (shell-command
                           (concat "inkscape --export-type=png --export-dpi=300 --export-background-opacity=0 "
                                   (shell-quote-argument (expand-file-name (concat (plist-get talk :slug) ".svg")
                                                                           dir))))))
                      (setq prev talk))
                    (cdr track)))
            (let ((default-directory dir))
              (shell-command
               (concat
                "convert "
                (mapconcat (lambda (talk) (shell-quote-argument
                                           (concat (plist-get talk :slug) ".png")))
                           (cdr track)
                           " ")
                " "
                (plist-get (emacsconf-get-track (car track)) :id)
                "-in-between.pdf"
                ))))
          by-track)))

(defun emacsconf-stream-generate-test-subtitles (&optional info)
  (interactive)
  (setq info (emacsconf-filter-talks (or info (emacsconf-get-talk-info))))
  (let ((dir (expand-file-name "test" emacsconf-stream-asset-dir))
        (subtitle-fill (substring "0123456789012345678901234567890123456789012345678901234567890123456789" 0 55))
        (subtitle-len 1))
    (unless (file-directory-p dir) (make-directory dir t))
    (mapc (lambda (talk)
            (with-temp-file (expand-file-name (concat (plist-get talk :video-slug) "--main.vtt")
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
                              (expand-file-name (concat (plist-get talk :video-slug) "--main.webm") dir)
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


(defun emacsconf-stream-schedule-timers (&optional info)
  (interactive)
  (setq info (emacsconf-filter-talks (or info (emacsconf-get-talk-info))))
  (cancel-function-timers #'emacsconf-stream-handle-talk-timer)
  (let ((now (current-time)))
    (mapc (lambda (talk)
            (when (and (time-less-p now (plist-get talk :start-time)))
              (run-at-time
               (time-to-seconds (time-subtract (plist-get talk :start-time) now))
               nil
               #'emacsconf-stream-handle-talk-timer
               talk)))
          info)))

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
(provide 'emacsconf-stream)
;;; emacsconf-stream.el ends here
