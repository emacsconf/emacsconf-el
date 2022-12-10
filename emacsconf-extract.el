;;; emacsconf-extract.el --- BigBlueButton               -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Sacha Chua

;; Author: Sacha Chua <sacha@sachachua.com>


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

(defun emacsconf-extract-extract-chat (filename)
	(when (file-exists-p filename)
		(message "%s" filename)
		(mapcar
		 (lambda (node)
			 (when (string= (dom-attr node 'target) "chat")
				 (let ((message
								(replace-regexp-in-string
								 "\\[<u>\\([^<]+\\)?</u>\\](\"\\([^<]+\\)\")"
								 "<\\2>"
								 (condition-case nil
										 (html-to-markdown-string (dom-attr node 'message))
									 (error
										(replace-regexp-in-string
										 "<a href=\"\\(.+?\\)\" rel=\"nofollow\"><u>\\(.+?\\)</u></a>"
										 "<\\2>"
										 (dom-attr node 'message)))))))
					 (list (string-to-number (dom-attr node 'in)) (dom-attr node 'name) message))))
		 (dom-by-tag (xml-parse-file filename) 'chattimeline))))
;; (emacsconf-extract-extract-chat (expand-file-name "bbb-playbacks/haskell/slides_new.xml" emacsconf-cache-dir)) 

(defvar emacsconf-extract-bbb-chat-use-wall-clock-time nil
	"Non-nil means use wall clock time for logs.")
(defun emacsconf-extract-chats ()
	(interactive)
	(mapc (lambda (o)
					(let* ((playback-dir (expand-file-name (plist-get o :slug)
																								 (expand-file-name "bbb-playbacks" emacsconf-cache-dir)))
								 (chat
									(emacsconf-extract-extract-chat 
									 (expand-file-name
										"slides_new.xml"
										playback-dir)))
								 metadata)
						(when chat
							(setq metadata (xml-parse-file (expand-file-name "metadata.xml"
																															 playback-dir)))
							(let ((recording-start (/ (string-to-number (dom-text
																													 (dom-by-tag metadata 'start_time)))
																				1000)))
								(with-temp-file (expand-file-name (concat (plist-get o :video-slug) "--extract.txt")
																									emacsconf-cache-dir)
									(insert
									 (mapconcat
										(lambda (line)
											(format "`%s` _%s_ %s  \n"
															(if emacsconf-extract-bbb-chat-use-wall-clock-time
																	(format-time-string "%H:%M:%S"
																											(seconds-to-time
																											 (+ recording-start
																													(elt line 0))))
																(format-seconds "%h:%.2m:%.2s"
																								(elt line 0)))
															(elt line 1)
															(elt line 2)))
										chat
										"")))))))
				(emacsconf-prepare-for-display (emacsconf-get-talk-info))))

(defun emacsconf-extract-bbb-copy-files ()
	(interactive)
	(mapc
	 (lambda (o)
		 (let ((playback-dir (expand-file-name (plist-get o :slug)
																					 (expand-file-name "bbb-playbacks" emacsconf-cache-dir))))
			 (mapc (lambda (file)
							 (when (and (file-exists-p (expand-file-name file playback-dir))
													(not (file-exists-p (expand-file-name (concat (plist-get o :video-slug) "--bbb-" file) emacsconf-cache-dir))))
								 (copy-file (expand-file-name file playback-dir)
														(expand-file-name (concat (plist-get o :video-slug) "--bbb-" file) emacsconf-cache-dir)
														t)))
						 '("webcams.webm" "metadata.xml" "deskshare.webm" "deskshare.xml" "slides_new.xml" "webcams.opus"))))
	 (emacsconf-prepare-for-display (emacsconf-get-talk-info))))

(defvar emacsconf-extract-dump-dir "/ssh:orga@res.emacsconf.org#46668:~/current/live0-streams/")
(defun emacsconf-extract-dump-time-from-filename (f)
  (when (string-match "\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)_\\([0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)" f)
    (encode-time
     (append
      (mapcar (lambda (i) (string-to-number (match-string i f))) (number-sequence 6 1 -1))
      (list nil nil "UTC")))))

(defun emacsconf-extract-dump-filename (directory input-prefix start-time)
  (seq-find
   (lambda (f) (time-less-p (emacsconf-extract-dump-time-from-filename f) start-time))
   (nreverse (sort (directory-files (or directory emacsconf-extract-dump-dir) 
                                    nil (concat emacsconf-id "-" emacsconf-year "-"
																								input-prefix ".*\\.webm$"))
                   'string<))))

;; (emacsconf-extract-dump-filename emacsconf-extract-dump-dir "dev" (emacsconf-extract-time-or-offset-to-time "2022-12-04 11:30"))
;; emacsconf-2021-main_2021-11-20_15-31-16.webm hmm, this might be GMT
(defun emacsconf-extract-dump-ffmpeg-command (input-file start-time end-time output-file &optional compress-command)
	(when (stringp start-time) (setq start-time (emacsconf-extract-time-or-offset-to-time start-time)))
	(when (stringp end-time) (setq end-time (emacsconf-extract-time-or-offset-to-time end-time)))
  (let* ((target-file-start (emacsconf-extract-dump-time-from-filename input-file))
				 (dump-args (emacsconf-extract-dump-ffmpeg-seek-and-filename
										 input-file
										 (- (time-to-seconds start-time)
												(time-to-seconds target-file-start))
										 (- (time-to-seconds end-time)
												(time-to-seconds target-file-start)))))
		(if compress-command 
				(format "ffmpeg -y %s -c copy %s; %s %s &"
								dump-args
								output-file
								compress-command
								output-file)
			(format "ffmpeg -y %s -c copy %s"
							dump-args
							output-file))))
;; (emacsconf-extract-dump-ffmpeg-command (emacsconf-extract-dump-filename emacsconf-extract-dump-dir "dev" (emacsconf-extract-time-or-offset-to-time "2022-12-04T11:30:00")) "2022-12-04T11:30:00" "2022-12-04T13:00:00" "rms.webm")

;; output-prefix            
;; (format-time-string "%Y-%m-%d_%H-%M-%S" start-time t)
(defun emacsconf-extract-dump-get-command (input-prefix start-time end-time filename)
  (interactive)
  (setq start-time (emacsconf-extract-time-or-offset-to-time start-time))
  (setq end-time (emacsconf-extract-time-or-offset-to-time end-time))
  (format "ssh conf -- %s; scp conf:~/emacsconf-2021-stream-dumps/%s %s"
          (shell-quote-argument
           (format "cd %s; sudo %s"
                   "~/emacsconf-2021-stream-dumps/"
                   (emacsconf-extract-dump-ffmpeg-command
                    (emacsconf-extract-dump-filename emacsconf-extract-dump-dir input-prefix start-time)
                    start-time end-time
                    (concat "output/" filename))))
          (concat "output/" filename)
          filename))

;; todo timezones
(defun emacsconf-extract-time-or-offset-to-time (input)
  (cond ((numberp input)
         (seconds-to-time (+ (time-to-seconds (current-time))
                             (* input 60))))
        ((listp input) input)
				((stringp input) (date-to-time (if (string-match "[-Z+]" input) input (concat input emacsconf-timezone-offset))))
        ((string-match " " input)
         (org-read-date t t input))
        (t (seconds-to-time (+ (time-to-seconds (current-time))
                               (* (string-to-number input) 60))))))

(defun emacsconf-extract-dump-ffmpeg-seek-and-filename (filename start-seconds to-seconds)
  "Return seek and input file argument."
  (if (> start-seconds 30)
      (format "-ss %f -i %s -ss %f -to %f" (- start-seconds 30) filename 30 (- to-seconds start-seconds -30))
    (format "-i %s -ss %f -to %f" filename start-seconds to-seconds)))

(defun emacsconf-extract-dump-get (track start-time end-time output-prefix)
  (interactive (list (emacsconf-complete-track)
                     (read-string "Start: ")
                     (read-string "End: ")
                     (read-string "Output prefix: ")))
  (let ((result
         (emacsconf-extract-dump-get-command
          (concat emacsconf-id "-" emacsconf-year "-" (plist-get track :id))
          (emacsconf-extract-time-or-offset-to-time start-time)
          (emacsconf-extract-time-or-offset-to-time end-time)
          (concat output-prefix (format-time-string "%Y-%m-%d_%H-%M-%S" (emacsconf-extract-time-or-offset-to-time start-time) t) ".webm"))))
    (when (called-interactively-p 'any)
      (kill-new result))
    result))

(defun emacsconf-extract-dump-refine (filename starting-ts ending-ts)
  (interactive
   (list (read-file-name "Input: " nil (conf-latest-file ".") t)
         (read-string "Start timestamp: ")
         (read-string "End timestamp: ")))
  (let ((result
         (format "ffmpeg -y %s -c copy %s"
                 ()
                 starting-ts filename 
                 starting-ts ending-ts (expand-file-name (concat "trimmed-" (file-name-nondirectory filename))
                                                         (file-name-directory filename)))))
    (when (called-interactively-p 'any)
      (kill-new result))
    result))

(defvar emacsconf-extract-qa-caption-length 50)

(defun emacsconf-extract-qa-from-assemblyai-sentences (file)
	(let ((data
				 (with-temp-buffer
					 (insert-file-contents file)
					 (json-parse-buffer)))
				last-speaker)
		(subed-create-file
		 (concat (file-name-sans-extension file) ".vtt")
		 (mapcar
			(lambda (sent)
				(let* ((words (mapconcat
											 (lambda (w)
												 (propertize (gethash "text" w)
																		 'start (gethash "start" w)
																		 'end (gethash "end" w)
																		 'confidence (gethash "confidence" w)))
											 (get-hash "words" sent)
											 " "))
							 (reflowed (emacsconf-split-text-based-on-heuristics
													words
													emacsconf-extract-qa-caption-length)))
					(seq-map-indexed
					 (lambda (line index)
						 (list
							
							)
						 )
					 )
					(list
					 nil
					 (gethash "start" sent)
					 (gethash "end" sent)
					 (if (string= (or last-speaker "") (gethash "speaker" sent))
							 words
						 (format "[Speaker %s]: %s" (gethash "speaker" sent) (gethash "text" sent)))
					 (if (string= (or last-speaker "") (gethash "speaker" sent))
							 nil
						 (setq last-speaker (gethash "speaker" sent))
						 (emacsconf-surround "NOTE Speaker " (gethash "speaker" sent) "\n\n" nil)))))
			(gethash "sentences" data)))))
;; (emacsconf-extract-qa-from-assemblyai-sentences "~/proj/emacsconf/rms/sentences")

(defun emacsconf-extract-copy-pad ()
	(interactive)
	(let ((slug (emacsconf-get-slug-from-string (file-name-base (buffer-file-name))))
				(delimiter "\\\\-\\\\-\\\\-\\\\-\\\\-")
				notes
				questions)
		(goto-char (point-min))
		(when (re-search-forward "Notes, discussions, links, feedback:" nil t)
			(forward-line 1)
			(setq notes (string-trim (buffer-substring (point) (if (re-search-forward delimiter nil t) (match-beginning 0) (point-max))))))
		(when (re-search-forward "Questions and answers go here:" nil t)
			(forward-line 1)
			(setq questions (string-trim (buffer-substring (point) (if (re-search-forward delimiter nil t) (match-beginning 0) (point-max))))))
		(find-file (expand-file-name (concat slug ".md") (expand-file-name "talks" (expand-file-name emacsconf-year emacsconf-directory))))
		(goto-char (point-min))
		(if (re-search-forward "^# Discussion\n\n" nil t)
				nil
			(re-search-forward "-after)" nil t)
			(forward-line -1)
			(insert "# Discussion\n\n"))
		(save-excursion
			(unless (string= (or notes "") "")
				(insert "## Notes\n\n" notes "\n\n"))
			(unless (string= (or questions "") "")
				(insert "## Questions and answers\n\n" questions "\n\n"))
			)))

(defun emacsconf-extract-question-headings (slug)
	(with-temp-buffer
		(insert-file-contents
		 (expand-file-name
			(concat slug ".md")
			(expand-file-name "talks"
												(expand-file-name emacsconf-year emacsconf-directory))))
		(goto-char (point-min))
		(let (results)
			(while (re-search-forward "^-[ \t]+Q:[  ]*" nil t)
				(setq results
							(cons
							 (string-trim
								(replace-regexp-in-string
								 "[\n \t ]+" " "
								 (replace-regexp-in-string
									"\\\\"
									""
									(buffer-substring
									 (point)
									 (and (re-search-forward "^[ \t]+-\\|^[ \t]+*$" nil t)
												(match-beginning 0))))))
							 results)))
			(nreverse results))))
;; (emacsconf-extract-question-headings "asmblox")

(defun emacsconf-extract-insert-note-with-question-heading (question)
	(interactive
	 (list
		(completing-read
		 "Question: "
		 (emacsconf-extract-question-headings
			(emacsconf-get-slug-from-string (file-name-base (buffer-file-name)))))))
	(insert "NOTE " question "\n\n"))
(provide 'emacsconf-extract)
;;; emacsconf-extract.el ends here
