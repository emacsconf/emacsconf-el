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
  (setq text (svg--encode-text text))
  (let ((node (or (dom-child-by-tag
                   (car (dom-by-id dom id))
                   'tspan)
                  (dom-by-id dom id))))
    (cond
     ((null node))                      ; skip
     ((= (length node) 2)
      (nconc node (list text)))
     (t (setf (elt node 2) text)))))

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

(defun emacsconf-stream-update-talk-info-org-after-todo-state-change ()
  "Update talk info."
  (when (string= org-state "PLAYING")
    (emacsconf-stream-set-talk-info (emacsconf-get-talk-info-for-subtree))))

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
  (let ((default-directory (emacsconf-stream-track-login talk)))
    (shell-command
     (concat "~/bin/track-mpv "
			       (shell-quote-argument (emacsconf-stream-get-filename talk))))))

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

(defvar emacsconf-stream-overlay-dir "/data/emacsconf/overlays/")

(defun emacsconf-stream-generate-overlays (&optional info)
  (interactive)
  (setq info (emacsconf-filter-talks (or info (emacsconf-get-talk-info))))
  (unless (file-directory-p emacsconf-stream-overlay-dir)
    (make-directory emacsconf-stream-overlay-dir))
  (mapc (lambda (talk)
          (emacsconf-stream-write-talk-overlay-svgs
           talk
           (expand-file-name (concat (plist-get talk :slug) "-video.svg") emacsconf-stream-overlay-dir)
           (expand-file-name (concat (plist-get talk :slug) "-other.svg") emacsconf-stream-overlay-dir)))
        info))

(provide 'emacsconf-stream)
;;; emacsconf-stream.el ends here
