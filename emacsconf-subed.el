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

(defcustom emacsconf-subed-subtitle-max-length nil
  "Target number of characters. Default to `fill-column'."
  :group 'emacsconf
  :type 'integer)

(defcustom emacsconf-subed-subtitle-minimum-duration-ms 600
  "Minimum length in milliseconds."
  :group 'emacsconf
  :type 'integer)

(defun emacsconf-combine-close-subtitles (subtitles &optional auto-space-msecs)
  "Combine closely-spaced SUBTITLES for analysis.
If a subtitle ends within AUTO-SPACE-MSECS of the previous one, concatenate them."
  (setq auto-space-msecs (or auto-space-msecs 1000))
  (let ((current (seq-copy (car subtitles)))
        results)
    (setq subtitles (cdr subtitles))
    ;; if the subtitle ends within auto-space-msecs of the current one, combine it
    (while subtitles
      (if (< (elt (car subtitles) 1) (+ (elt current 2) auto-space-msecs))
          (progn
            (setf (elt current 2) (elt (car subtitles) 2))
            (setf (elt current 3) (concat (elt current 3) " "
                                          (propertize (elt (car subtitles) 3)
                                                      :start (elt (car subtitles) 1)))))
        (setq results (cons current results))
        (setq current (seq-copy (car subtitles))))
      (setq subtitles (cdr subtitles)))
    (nreverse (cons current results))))

;; (emacsconf-combine-close-subtitles (subed-parse-file "/home/sacha/proj/emacsconf/cache/emacsconf-2022-health--health-data-journaling-and-visualization-with-org-mode-and-gnuplot--david-otoole--main.vtt") 700)

(defun emacsconf-last-string-match-before (regexp string before &optional after type)
  "Find the last match of REGEXP in STRING before BEFORE position (exclusive).
If AFTER is specified, start the search from there.
TYPE can be 'end if you want the match end instead of the beginning."
  (let (pos (found (string-match regexp string after)) result)
    (while (and found (< found before))
      (setq result (if (eq type 'end) (match-end 0) (match-beginning 0)))
      (if (and (< found (length string)) (string-match regexp string (1+ found)))
          (setq found (match-end 0))
        (setq found nil)))
    result))

;; (assert (= (emacsconf-last-string-match-before " " "012345 789 12345 7890" 12) 10))

(defun emacsconf-split-text-based-on-heuristics (text limit)
  "Split subtitle TEXT based on simple heuristics so that it fits in LIMIT."
  (let ((position 0)
        (current "")
        (reversed (string-reverse text))
        (case-fold-search t)
        result new-position (next-limit limit))
    ;; Treat sub as a bag of words
    (while (< position (length text))
      (let ((heuristic (delq
                        nil
                        (list
                         (emacsconf-last-string-match-before "[,\\.\\?;]+ "
                                                             text next-limit (1+ position) 'end)
                         (emacsconf-last-string-match-before
                          (concat "\\<" (regexp-opt (list "or how"
                                                          "or that"
                                                          "now that"
                                                          "then"
                                                          "but that"
                                                          "now"
                                                          "which"
                                                          "when"
                                                          "using"
                                                          "of"
                                                          "by"
                                                          "and" "that" "so" "so in" "because" "but"
                                                          "is" "how" "in" "or" "--" "than" "with" "to"
                                                          "I"))
                                  "\\>")
                          text next-limit (1+ position))))))
        (setq
         new-position
         (or
          (and (< (length text) next-limit) (length text))
          (and heuristic (apply #'max heuristic))
          (emacsconf-last-string-match-before
           " " text next-limit (1+ position) 'end)
          (length text))))
      (setq result (cons (string-trim (substring text position new-position))
                         result))
      (setq position new-position)
      (setq next-limit (+ new-position limit)))
    (setq result (cons (string-trim current) result))
    (nreverse result)))

(defun emacsconf-reflow-automatically ()
  (interactive)
  (let* ((subtitle-text-limit (or emacsconf-subed-subtitle-max-length fill-column))
         (auto-space-msecs 700)
         (subtitles
          (mapconcat
           (lambda (sub)
             (string-join 
              (emacsconf-split-text-based-on-heuristics (elt sub 3) subtitle-text-limit)
              "\n"))
           (emacsconf-combine-close-subtitles (subed-subtitle-list))
           "\n")))
    (find-file (concat (file-name-sans-extension (buffer-file-name)) ".txt"))
    (erase-buffer)
    (insert subtitles)
    (goto-char (point-min))
    (set-fill-column subtitle-text-limit)
    (display-fill-column-indicator-mode 1)))

;;;###autoload
(defun emacsconf-subed-make-chapter-file-based-on-comments ()
  "Create a chapter file based on NOTE comments."
  (interactive)
  (let ((new-filename (concat (file-name-sans-extension (buffer-file-name)) "--chapters.vtt"))
        (subtitles (subed-subtitle-list))
        (subed-auto-play-media nil))
    (when (or (not (file-exists-p new-filename))
              (yes-or-no-p (format "%s exists. Overwrite? " new-filename)))
      (subed-create-file
       new-filename 
       (emacsconf-subed-list-chapter-markers-based-on-comments
        subtitles)
       t))))

(defun emacsconf-subed-list-chapter-markers-based-on-comments (subtitles)
  "Make a list of subtitles based on which SUBTITLES have comments."
  (let (result current)
    (mapc
     (lambda (o)
       (if (not (string= (or (elt o 4) "") ""))
           (progn
             (when current (setq result (cons current result)))
             (setq current
                   (list nil (elt o 1) (elt o 2)
                         (string-trim (replace-regexp-in-string "^NOTE[ \n]+\\|[ \n]+" " " (elt o 4))))))
         ;; update the end time to include the current subtitle
         (when current (setf (elt current 2) (elt o 2)))))
     subtitles)
    (nreverse (cons current result))))

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
         (caption-file (expand-file-name (concat (plist-get info :file-prefix) "--main.vtt")
                                         emacsconf-captions-directory))
         (chapters (with-current-buffer (find-file-noselect caption-file)
                     (subed-subtitle-list))))
    (with-temp-file wiki-file
      (insert
       (with-current-buffer (find-file-noselect caption-file)
         (emacsconf-subed-convert-transcript-to-directives "mainVideo" chapters))))
    (find-file wiki-file)))

(defun emacsconf-subed-download-captions (&optional youtube-url file-prefix)
  (interactive (list (org-entry-get (point) "YOUTUBE_URL") (org-entry-get (point) "FILE_PREFIX")))
  (shell-command
   (mapconcat
    (lambda (f)
      (format "youtube-dl --write-sub --write-auto-sub --no-warnings --sub-lang en --skip-download --sub-format %s %s -o %s"
              f
              youtube-url
              (expand-file-name file-prefix emacsconf-captions-directory)))
    '("vtt" "srv2")
    ";")))

(defun emacsconf-subed--copy-downloaded-captions-base (file-prefix url type)
  (let ((new-file (expand-file-name (concat file-prefix "--" type ".ass") emacsconf-captions-directory)))
    (call-process "ffmpeg" nil nil nil "-y" "-i" (emacsconf-latest-file emacsconf-download-directory "srt$")
                  new-file)
    (emacsconf-subed-download-captions url new-file)
    (with-current-buffer (find-file new-file)
      (goto-char (point-min))
      (subed-trim-overlaps)
      (save-buffer))))

(defun emacsconf-subed-copy-downloaded-captions ()
  "Copy the most recently downloaded captions for this entry's main talk."
  (interactive)
  (emacsconf-subed--copy-downloaded-captions-base
   (org-entry-get (point) "FILE_PREFIX")
   (org-entry-get (point) "YOUTUBE_URL")
   "main"))

(defun emacsconf-subed-copy-downloaded-qa-captions ()
  "Copy the most recently downloaded captions for this entry's Q&A"
  (interactive)
  (emacsconf-subed--copy-downloaded-captions-base
   (org-entry-get (point) "FILE_PREFIX")
   (org-entry-get (point) "QA_YOUTUBE")
   "answers"))

(defun emacsconf-subed-find-captions ()
  "Open the caption file for this talk.
Create it if necessary."
  (interactive)
  (let ((file-prefix (org-entry-get (point) "FILE_PREFIX")))
    (find-file
     (or (car (directory-files emacsconf-cache-dir
                               t
                               (concat (regexp-quote file-prefix)
                                       "--main\\.\\(srt\\|vtt\\)")))
         (expand-file-name (concat file-prefix "--main.vtt") "captions")))
    (when (eobp)
      (require 'compile-media)
      (insert "WEBVTT\n\n0:00:00.000 --> "
              (compile-media-msecs-to-timestamp
               (compile-media-get-file-duration-ms (subed-guess-video-file)))
              "\n"))))

(defun emacsconf-subed-check-subtitles ()
  "Do some simple validation of subtitles."
  (interactive)
  (while (not (eobp))
    (if (> (length (subed-subtitle-text)) (or emacsconf-subed-subtitle-max-length fill-column))
        (error "Length %d exceeds maximum length" (length (subed-subtitle-text))))
    (if (< (- (subed-subtitle-msecs-stop) (subed-subtitle-msecs-start)) emacsconf-subed-subtitle-minimum-duration-ms)
        (error "Duration %d is less than minimum" (- (subed-subtitle-msecs-stop) (subed-subtitle-msecs-start))))
    (or (subed-forward-subtitle-text) (goto-char (point-max)))))

(defun emacsconf-subed-split-at-mouse (event)
	"Split at the word clicked on."
	(interactive "e")
	(goto-char (posn-point (event-start event)))
	(skip-syntax-backward "w")
	(subed-split-subtitle)
	(recenter))

(defun emacsconf-subed-merge-and-unfill ()
	"Merge this subtitle with the next one."
	(interactive)
	(subed-merge-with-next)
	(emacsconf-subed-unfill-paragraph))

(defun emacsconf-subed-unfill-paragraph ()
	"Sometimes the regular fill doesn't work."
	(interactive)
	(subed-set-subtitle-text
	 (replace-regexp-in-string
		"\n+" " "
		(subed-subtitle-text))))

(defvar emacsconf-subed-split-map
	(let ((map (make-sparse-keymap)))
		(define-key map [down-mouse-1] #'emacsconf-subed-split-at-mouse)
		(define-key map [mouse-1] #'ignore)
		(define-key map [up-1] #'ignore)
		(define-key map [drag-mouse-1] #'ignore)
		(define-key map [mouse-movement] #'ignore)
		(define-key map (kbd "<up>") #'scroll-down)
		(define-key map (kbd "<down>") #'scroll-up)
		(define-key map (kbd "M-q") #'emacsconf-subed-unfill-paragraph)
		(define-key map (kbd "M-.") #'emacsconf-subed-merge-and-unfill)
		(define-key map "q" #'emacsconf-subed-unfill-paragraph)
		(define-key map "p" #'subed-backward-subtitle-text)
		(define-key map "n" #'subed-forward-subtitle-text)
		(define-key map "k" #'subed-kill-subtitle)
		(define-key map "." #'emacsconf-subed-merge-and-unfill)
		(define-key map "u" #'undo)
		(define-key map (kbd "SPC") #'scroll-up)
		map)
	"Map for splitting.")

;;;###autoload
(defun emacsconf-subed-split ()
	"Use transient map for splitting subtitles."
	(interactive)
	(set-fill-column 60)
	(set-transient-map emacsconf-subed-split-map t))

(defun emacsconf-subed-intro-subtitles ()
	"Create the introduction as subtitles."
	(interactive)
	(subed-auto-insert)
	(let ((emacsconf-publishing-phase 'conference))
		(mapc
		 (lambda (sub) (apply #'subed-append-subtitle nil (cdr sub)))
		 (seq-map-indexed
			(lambda (talk i)
				(list
				 nil
				 (* i 5000)
				 (1- (* i 5000))
				 (emacsconf-pad-expand-intro talk)
				 (format "#+OUTPUT: %s.webm\n[[file:%s]]"
								 (plist-get talk :slug)
								 (expand-file-name
									(concat (plist-get talk :slug) ".png")
									(expand-file-name "in-between" emacsconf-stream-asset-dir)))))
			(emacsconf-publish-prepare-for-display (emacsconf-get-talk-info))))))

(defvar-local emacsconf-subed-subtitle-source nil "Buffer with the intended subtitles.")

(defun emacsconf-subed-prepare-string-for-matching (s)
	(downcase
	 (replace-regexp-in-string "[^A-Za-z0-9]" ""
		(replace-regexp-in-string "^\\(\\[\\[.*?\]\\]\\|#\\+.*?\\)\\(\n\\|$\\)" "" s))))

(defun emacsconf-subed-similar-p (source current)
	(let* ((current-stripped (emacsconf-subed-prepare-string-for-matching current))
				 (distance (string-distance current-stripped
																		(emacsconf-subed-prepare-string-for-matching source)))
				 (ratio (/ (* 100.0 distance) (length current-stripped)))
				 (threshold 10))
		(< ratio threshold)))

(defun emacsconf-subed-source-backward ()
	(interactive)
	(with-current-buffer (get-buffer emacsconf-subed-subtitle-source)
		(subed-backward-subtitle-end)
		(set-mark (point))
		(subed-jump-to-subtitle-text)
		(when (get-buffer-window (current-buffer))
			(set-window-point
			 (get-buffer-window (current-buffer)) (point))
			(with-selected-window (get-buffer-window (current-buffer))
				(recenter)))))

(defun emacsconf-subed-source-forward ()
	(interactive)
	(with-current-buffer (get-buffer emacsconf-subed-subtitle-source)
		(subed-forward-subtitle-end)
		(set-mark (point))
		(subed-jump-to-subtitle-text)
		(when (get-buffer-window (current-buffer))
			(set-window-point
			 (get-buffer-window (current-buffer)) (point))
			(with-selected-window (get-buffer-window (current-buffer))
				(recenter)))))

(defun emacsconf-subed-match-up-subtitles ()
	(interactive)
	(let* ((source
					(with-current-buffer (get-buffer emacsconf-subed-subtitle-source)
						(or (subed-subtitle-text) (subed-forward-subtitle-text) (subed-subtitle-text)))))
		(subed-set-subtitle-text source)
		(subed-forward-subtitle-text)
		(emacsconf-subed-source-forward)))

(defun emacsconf-subed-match-multiple (prefix)
	"Set up for matching multiple subtitles."
	(interactive "p")
	(when (or (>= prefix 4) (null emacsconf-subed-subtitle-source))
		(setq-local emacsconf-subed-subtitle-source (read-buffer "Source subtitles: " nil t)))
	(set-transient-map
	 (let ((map (make-sparse-keymap)))
		 (define-key map (kbd "RET") #'emacsconf-subed-match-up-subtitles)
		 (define-key map (kbd "SPC") #'emacsconf-subed-match-up-subtitles)
		 (define-key map (kbd "<up>") #'subed-backward-subtitle-text)
		 (define-key map (kbd "<down>") #'subed-forward-subtitle-text)
		 (define-key map (kbd "<S-up>") #'emacsconf-subed-source-backward)
		 (define-key map (kbd "<S-down>") #'emacsconf-subed-source-forward)
		 (define-key map "k" #'subed-kill-subtitle)
		 (define-key map "m" #'emacsconf-subed-merge-and-fill)
		 map)
	 t))

(provide 'emacsconf-subed)
;;; emacsconf-subed.el ends here
