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
         (default-directory home)
         (filename (expand-file-name "other.svg" home))
         (dom (xml-parse-file filename)))
    (emacsconf-stream-svg-set-text dom "news" message)
    (with-temp-file filename (dom-print dom t))
    (with-temp-file (expand-file-name "video.svg" home)
      (emacsconf-stream-svg-set-text dom "bottom" "")
      (dom-print dom t))
    ;; (shell-command "inkscape --export-type=png --export-dpi=96 --export-background-opacity=0 video.svg")
    ;; (shell-command "inkscape --export-type=png --export-dpi=96 --export-background-opacity=0 other.svg")
    ))

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
     ((and (string= text "")
           (= (length node) 2)))        ; already nothing, skip
     ((string= text "")
      (setf (elt node 2) ""))
     ((= (length node) 2)
      (nconc node (list text)))
     (t (setf (elt node 2) text)))))

(defun emacsconf-stream-set-talk-info-from-strings (track url bottom)
  (interactive (list (emacsconf-complete-track) (read-string "URL: ") (read-string "Bottom: ")))
  (let* ((home (concat (emacsconf-stream-track-login track) "~"))
         (default-directory home)
         (filename (expand-file-name "other.svg" home))
         (dom (xml-parse-file filename)))
    (emacsconf-stream-svg-set-text dom "bottom" bottom)
    (emacsconf-stream-svg-set-text dom "url" url)
    (with-temp-file filename (dom-print dom t))
    (with-temp-file (expand-file-name "video.svg" home)
      (emacsconf-stream-svg-set-text dom "bottom" "")
      (dom-print dom t))
    ;; (shell-command "inkscape --export-type=png --export-dpi=96 --export-background-opacity=0 video.svg")
    ;; (shell-command "inkscape --export-type=png --export-dpi=96 --export-background-opacity=0 other.svg")
    ))

(defun emacsconf-stream-set-talk-info (talk)
  (interactive (list (emacsconf-complete-talk-info)))
  (emacsconf-stream-set-talk-info-from-strings
   (emacsconf-get-track talk)
   (concat (replace-regexp-in-string "^.*//" "" emacsconf-base-url)
           (plist-get talk :url)
           (cond
            ((string-match "live" (plist-get talk :q-and-a))
             " - Q&A: live (see talk page for URL)")
            ((string-match "irc" (plist-get talk :q-and-a))
             (format " - Q&A: IRC (#%s) - speaker nick: %s"
                     (plist-get track :channel)
                     (plist-get talk :irc)))
            (t "")))
   (cond
    ((or (null (plist-get talk :pronouns)) (string= (plist-get talk :pronouns) "nil"))
     (plist-get talk :speakers))
    ((string-match ", " (plist-get talk :pronouns))
     (plist-get talk :pronouns))
    (t (format "%s (%s)"
               (plist-get talk :speakers)
               (plist-get talk :pronouns))))))

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

(provide 'emacsconf-stream)
;;; emacsconf-stream.el ends here
