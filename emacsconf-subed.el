;;; emacsconf-subed.el --- Utilities for working with subtitle files  -*- lexical-binding: t; -*-

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

;; You will need the subed package, which is available from NonGNU
;; ELPA. For creating new caption files with
;; emacsconf-subed-find-captions, you will also need compile-media.el
;; from https://github.com/sachac/compile-media .

;;; Code:

(require 'subed)

(defcustom emacsconf-subed-subtitle-max-length 50
  "Target number of characters."
  :group 'emacsconf
  :type 'integer)

(defcustom emacsconf-subed-subtitle-minimum-duration-ms 600
  "Minimum length in milliseconds."
  :group 'emacsconf
  :type 'integer)


(defun emacsconf-subed-fix-timestamps ()
  "Change overlapping timestamps to the start of the next subtitle."
  (interactive)
  (goto-char (point-max))
  (let ((timestamp (subed-subtitle-msecs-start)))
    (while (subed-backward-subtitle-time-start)
      (when (> (subed-subtitle-msecs-stop) timestamp)
        (subed-set-subtitle-time-stop timestamp))
      (setq timestamp (subed-subtitle-msecs-start)))))

(defun emacsconf-subed-mark-chapter (chapter-name)
  (interactive "MChapter: ")
  (let ((start (subed-subtitle-msecs-start)))
    (save-excursion
      (when (and (subed-backward-subtitle-time-stop)
                 (= (subed-subtitle-msecs-stop) start))
        (subed-set-subtitle-time-stop (1- start))))
    (with-current-buffer (find-file-noselect
                          (if (string-match "--main" (buffer-file-name))
                              (replace-match "--chapters" nil t (buffer-file-name))
                            (concat (file-name-sans-extension (buffer-file-name)) "--chapters.vtt")))
      (goto-char (point-max))
      (if (bobp)
          (insert "WEBVTT\n\n"
                  (subed-make-subtitle nil start nil chapter-name))
        (insert "\n" (subed-make-subtitle nil start nil chapter-name)))
      (when (subed-backward-subtitle-time-stop)
        (subed-set-subtitle-time-stop (1- start)))
      (save-buffer))))

(defun emacsconf-subed-convert-transcript-to-directives (id &optional chapters)
  (interactive (list (read-string "ID: " "mainVideo")))
  (goto-char (point-min))
  (let* ((chapter-starts (mapcar 'car chapters))
         (result (concat
                  "<a name=\"transcript\"></a>\n# Transcript\n\n"
                  (cl-loop while (subed-forward-subtitle-text)
                           concat (format
                                   "[[!template %stext=\"%s\" start=\"%s\" video=\"%s\" id=subtitle]]\n"
                                   (if (member (subed-subtitle-msecs-start) chapter-starts)
                                       "new=\"1\" "
                                     "")
                                   (replace-regexp-in-string
                                    "\n" " " 
                                    (replace-regexp-in-string
                                     "\"" "&quot ;"
     (replace-regexp-in-string "[][]" "" (subed-subtitle-text))))
                                   (subed-msecs-to-timestamp (subed-subtitle-msecs-start))
                                   id)))))
    (when (called-interactively-p 'any)
      (kill-new result))
    result))

(defun emacsconf-subed-prepare-transcript-directives ()
  (interactive)
  (let* ((info (emacsconf-get-talk-info-for-subtree))
         (wiki-file (plist-get info :wiki-file-path))
         (caption-file (expand-file-name (concat (plist-get info :video-slug) "--main.vtt")
                                         emacsconf-captions-directory))
         (chapters (with-current-buffer (find-file-noselect caption-file)
                     (subed-subtitle-list))))
    (with-temp-file wiki-file
      (insert
       (with-current-buffer (find-file-noselect caption-file)
         (emacsconf-subed-convert-transcript-to-directives "mainVideo" chapters))))
    (find-file wiki-file)))

(defun emacsconf-subed-download-captions (&optional youtube-url video-slug)
  (interactive (list (org-entry-get (point) "YOUTUBE_URL") (org-entry-get (point) "VIDEO_SLUG")))
  (shell-command
   (mapconcat
    (lambda (f)
      (format "youtube-dl --write-sub --write-auto-sub --no-warnings --sub-lang en --skip-download --sub-format %s %s -o %s"
              f
              youtube-url
              (expand-file-name video-slug emacsconf-captions-directory)))
    '("vtt" "srv2")
    ";")))

(defun emacsconf-subed--copy-downloaded-captions-base (video-slug url type)
  (let ((new-file (expand-file-name (concat video-slug "--" type ".ass") emacsconf-captions-directory)))
    (call-process "ffmpeg" nil nil nil "-y" "-i" (emacsconf-latest-file emacsconf-download-directory "srt$")
                  new-file)
    (emacsconf-subed-download-captions url new-file)
    (with-current-buffer (find-file new-file)
      (goto-char (point-min))
      (emacsconf-subed-fix-timestamps)
      (save-buffer))))

(defun emacsconf-subed-copy-downloaded-captions ()
  "Copy the most recently downloaded captions for this entry's main talk."
  (interactive)
  (emacsconf-subed--copy-downloaded-captions-base
   (org-entry-get (point) "VIDEO_SLUG")
   (org-entry-get (point) "YOUTUBE_URL")
   "main"))

(defun emacsconf-subed-copy-downloaded-qa-captions ()
  "Copy the most recently downloaded captions for this entry's Q&A"
  (interactive)
  (emacsconf-subed--copy-downloaded-captions-base
   (org-entry-get (point) "VIDEO_SLUG")
   (org-entry-get (point) "QA_YOUTUBE")
   "answers"))

(defun emacsconf-subed-find-captions ()
  "Open the caption file for this talk.
Create it if necessary."
  (interactive)
  (require 'compile-media)
  (let ((video-slug (org-entry-get (point) "VIDEO_SLUG")))
    (find-file
     (or (car (directory-files emacsconf-captions-directory
                               t
                               (concat (regexp-quote video-slug)
                                       "--main\\.\\(srt\\|vtt\\)")))
         (expand-file-name (concat video-slug "--main.vtt") "captions")))
    (when (eobp)
      (insert "WEBVTT\n\n0:00:00.000 --> "
              (compile-media-msecs-to-timestamp
               (compile-media-get-file-duration-ms (subed-guess-video-file)))
              "\n"))))

(defun emacsconf-subed-check-subtitles ()
  "Do some simple validation of subtitles."
  (interactive)
  (while (not (eobp))
    (if (> (length (subed-subtitle-text)) emacsconf-subed-subtitle-max-length)
        (error "Length %d exceeds maximum length" (length (subed-subtitle-text))))
    (if (< (- (subed-subtitle-msecs-stop) (subed-subtitle-msecs-start)) emacsconf-subed-subtitle-minimum-duration-ms)
        (error "Duration %d is less than minimum" (- (subed-subtitle-msecs-stop) (subed-subtitle-msecs-start))))
    (or (subed-forward-subtitle-text) (goto-char (point-max)))))

(provide 'emacsconf-subed)
;;; emacsconf-subed.el ends here
