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
  (concat emacsconf-id "-" (plist-get track :id) "@" emacsconf-stream-host))

(defvar emacsconf-stream-bottom-limit 80
  "Number of characters for bottom text.")

(defun emacsconf-stream-write-news (track message)
  (interactive (list (emacsconf-complete-track) (read-string "Message: ")))
  (with-temp-file (expand-file-name "news.txt" (concat "/ssh:" (emacsconf-stream-track-login track) ":~"))
    (insert message)))

(defun emacsconf-stream-broadcast (message)
  (interactive (list (read-string "Message: ")))
  (mapc (lambda (track) (emacsconf-stream-write-news track message)) emacsconf-tracks))

(defun emacsconf-stream-clear-talk-info (track)
  (interactive (list (emacsconf-complete-track)))
  (emacsconf-stream-set-talk-info-from-strings track "" ""))

(defun emacsconf-stream-clear-track (track)
  (interactive (list (emacsconf-complete-track)))
  (emacsconf-stream-set-talk-info-from-strings track "" "")
  (emacsconf-stream-write-news track ""))

(defun emacsconf-stream-clear-all ()
  (interactive)
  (mapc #'emacsconf-stream-clear-track emacsconf-tracks))

(defun emacsconf-stream-set-talk-info-from-strings (track url bottom)
  (interactive (list (emacsconf-complete-track) (read-string "URL: ") (read-string "Bottom: ")))
  (let* ((home (concat "/ssh:" (emacsconf-stream-track-login track) ":~")))
    (with-temp-file (expand-file-name "url.txt" home) (insert url))
    (with-temp-file (expand-file-name "bottom.txt" home) (insert bottom))))

(defun emacsconf-stream-set-talk-info (talk)
  (interactive (list (emacsconf-complete-talk-info)))
  (emacsconf-stream-set-talk-info
   (track (emacsconf-get-track talk))
   (concat (replace-regexp-in-string "^.*//" "" emacsconf-base-url)
           (plist-get talk :url))
   (concat (cond
        ((or (null (plist-get talk :pronouns)) (string= (plist-get talk :pronouns) "nil"))
         (plist-get talk :speakers))
        ((string-match ", " (plist-get talk :pronouns))
         (plist-get talk :pronouns))
        (t (format "%s (%s)"
                   (plist-get talk :speakers)
                   (plist-get talk :pronouns))))    
       "\n"    
       (cond
        ((string-match "live" (plist-get talk :q-and-a))
         "Q&A: live - see talk page for URL")
        ((string-match "irc" (plist-get talk :q-and-a))
         (format "Q&A: IRC (#%s) - speaker nick: %s"
                 (plist-get track :channel)
                 (plist-get talk :irc)))
        (t "")))))

(defun emacsconf-stream-update-talk-info-org-after-todo-state-change ()
  "Update talk info."
  (when (string= org-state "PLAYING")
    (emacsconf-stream-write-talk-info (emacsconf-get-talk-info-for-subtree))))

(defun emacsconf-stream-play-talk-org-after-todo-state-change ()
  "Play the talk."
  (when (string= org-state "PLAYING")
    (emacsconf-stream-play-video (emacsconf-get-talk-info-for-subtree))))

(defun emacsconf-stream-get-filename (talk)
  "Return the local filename for the video file for TALK.
Final files should be stored in /data/emacsconf/stream/YEAR/video-slug--main.webm."
  (expand-file-name
   (concat (plist-get talk :video-slug) "--main.webm")
   (expand-file-name emacsconf-year
                     emacsconf-stream-dir)))

(defun emacsconf-stream-play-video (talk)
  (interactive (list (emacsconf-complete-talk-info)))
  (start-process (concat "mpv-" (plist-get talk :slug))
                 "test"
                 "ssh" (emacsconf-stream-track-login talk) "nohup" "~/bin/track-mpv" (emacsconf-stream-get-filename talk)  "&"))

(provide 'emacsconf-stream)
;;; emacsconf-stream.el ends here
