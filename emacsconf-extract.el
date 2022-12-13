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

(defun emacsconf-extract-bbb-copy-files (&optional info)
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
	 (or info (emacsconf-prepare-for-display (emacsconf-get-talk-info)))))

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

(defun emacsconf-extract-wget-bbb (o)
	(when (plist-get o :bbb-playback)
		(let ((meeting-id (when (string-match "meetingId=\\(.+\\)"
																					(plist-get o :bbb-playback))
												(match-string 1 (plist-get o :bbb-playback)))))
			(concat "mkdir " (plist-get o :slug) "\n"
							"cd " (plist-get o :slug) "\n"
							(mapconcat
							 (lambda (file)
								 (concat
									"wget https://bbb.emacsverse.org/presentation/"
									meeting-id "/" file "\n"))
							 '("video/webcams.webm" "metadata.xml" "deskshare/deskshare.webm" "panzooms.xml" "cursor.xml" "deskshare.xml" "captions.json" "presentation_text.json" "slides_new.xml")
							 "")
							"cd ..\n"))))

(defun emacsconf-extract-bbb-events-xml (o)
	"Copy the events.xml from the raw BBB directory copied from bbb@bbb.emacsverse.org."
	(if (plist-get o :bbb-playback)
			(let ((meeting-id (when (string-match "meetingId=\\(.+\\)"
																						(plist-get o :bbb-playback))
													(match-string 1 (plist-get o :bbb-playback)))))
				(format "scp ~/current/bbb-raw/%s/events.xml orga@media.emacsconf.org:~/backstage/%s--bbb-events.xml\n"
								meeting-id
								(plist-get o :video-slug)))
		""))

(defun emacsconf-extract-bbb-voice-events (file)
	"Return a list of voice events.
(:name participant :start-clock start-time :start-ms ... :stop-clock stop-time :stop-ms)."
	(let ((dom (xml-parse-file file))
				start-recording
				stop-recording
				start-ms stop-ms
				participants results)
		(setq start-recording
					(date-to-time
					 (dom-text
						(dom-by-tag
						 (dom-elements dom 'eventname "StartRecordingEvent")
						 'date))))
		(setq stop-recording
					(date-to-time
					 (dom-text
						(dom-by-tag
						 (dom-elements dom 'eventname "StopRecordingEvent")
						 'date))))
		(setq start-ms (* 1000 (time-to-seconds start-recording))
					stop-ms (* 1000 (time-to-seconds stop-recording)))
		;; get the participant names and put them in an alist
		(setq participants
					(mapcar (lambda (o) (list
															 (dom-text (dom-by-tag o 'userId))
															 :name (dom-text (dom-by-tag o 'name))))
									(seq-filter
									 (lambda (node) (string= (dom-attr node 'eventname)
																					 "ParticipantJoinEvent"))
									 (dom-by-tag dom 'event))))
		;; get the voice events
		(mapc (lambda (o)
						(let ((participant (assoc-default
																(dom-text (dom-by-tag o 'participant))
																participants))
									(time (date-to-time (dom-text (dom-by-tag o 'date))))
									o-start o-stop)
							(if (string= (dom-text (dom-by-tag o 'talking))
													 "true")
									;; start talking
									(plist-put participant
														 ;; although maybe timestampUTC will be useful somehow
														 :start time)
								;;  clamp it to start-recording and stop-recording
								(when (and (time-less-p (plist-get participant :start)
																				stop-recording)
													 (time-less-p start-recording time))
									(setq o-start
												(- (max (* 1000 (time-to-seconds (plist-get participant :start)))
																start-ms)
													 start-ms)
												o-stop
												(- (min (* 1000 (time-to-seconds time))
																stop-ms)
													 start-ms))
									(setq results
												(cons (list
															 :name
															 (plist-get participant :name)
															 :start-ms
															 o-start
															 :stop-ms
															 o-stop
															 :start-clock
															 (plist-get participant :start)
															 :stop-clock
															 time
															 :duration-ms
															 (- o-stop o-start))
															results))))))
					(seq-filter
					 (lambda (node) (string= (dom-attr node 'eventname)
																	 "ParticipantTalkingEvent"))
					 (dom-by-tag dom 'event)))
		(nreverse results)))
;; (emacsconf-extract-bbb-voice-events "~/proj/emacsconf/cache/emacsconf-2022-sqlite--using-sqlite-as-a-data-source-a-framework-and-an-example--andrew-hyatt--bbb-events.xml")
;; Okay, now that we have voice events, what can we do with them?
;; We can insert notes into the VTT for now to try to guess the speaker, when the speaker changes
;; The audio is not split up by speaker, so the transcript is also not very split up
;; Some speech-to-text systems can do speaker diarization, which also tries to identify speakers
;; huh, is the StartRecordingEvent timestamp reliable? Am I misreading it?

(defvar emacsconf-extract-irc-speaker-nick nil)
(defun emacsconf-extract-irc-copy-line-to-other-window-as-list-item ()
	(interactive)
	(goto-char (line-beginning-position))
	(when (looking-at "\\[[0-9:]+\\] <\\(.*?\\)> \\([^ ]+?:\\)?\\(.+\\)$")
		(let ((line (string-trim (match-string 3))))
			(setq line
						(if (string= (or emacsconf-extract-irc-speaker-nick "")
												 (match-string 1))
								(concat "  - A: " line "\n")
							(concat "- " line "\n")))
			(other-window 1)
			(insert line)
			(other-window 1)
			(forward-line 1))))

(defvar emacsconf-extract-irc-map (make-sparse-keymap))
(defalias 'emacsconf-extract-irc-other-window #'other-window)
(defalias 'emacsconf-extract-irc-next-line #'next-line)
(defalias 'emacsconf-extract-irc-previous-line #'previous-line)
(defun emacsconf-extract-irc-open-talk-in-other-window (talk)
	(interactive (list (emacsconf-complete-talk-info)))
	(other-window 1)
	(emacsconf-edit-wiki-page talk))

(define-key emacsconf-extract-irc-map "c" #'emacsconf-extract-irc-copy-line-to-other-window-as-list-item)
(define-key emacsconf-extract-irc-map "o" #'emacsconf-extract-irc-other-window)
(define-key emacsconf-extract-irc-map "t" #'emacsconf-extract-irc-open-talk-in-other-window)
(define-key emacsconf-extract-irc-map "n" #'emacsconf-extract-irc-next-line)
(define-key emacsconf-extract-irc-map "p" #'emacsconf-extract-irc-previous-line)
(mapc (lambda (sym)
				(put sym 'repeat-map 'emacsconf-extract-irc-map))
			'(emacsconf-extract-irc-copy-line-to-other-window-as-list-item
				emacsconf-extract-irc-other-window
				emacsconf-extract-irc-next-line
				emacsconf-extract-irc-previous-line))

;; (local-set-key (kbd "C-c C-c") emacsconf-extract-irc-map)

(defun emacsconf-extract-publish-qa (talk &optional note)
	(interactive (list (emacsconf-complete-talk-info)))
	(setq talk (emacsconf-resolve-talk talk))
	(let ((large-file-warning-threshold nil))
		;; Copy the files
		(unless (file-exists-p (expand-file-name (concat (plist-get talk :video-slug) "--answers.webm" emacsconf-cache-dir)))
			(if (file-exists-p (expand-file-name (concat (plist-get talk :video-slug) "--bbb-deskshare.webm") emacsconf-cache-dir))
					;; use the screenshare if available
					(call-process "ffmpeg" nil (get-buffer-create "*ffmpeg*") t
												"-y"
												"-i" (expand-file-name (concat (plist-get talk :video-slug) "--bbb-deskshare.webm") emacsconf-cache-dir)
												"-i" (expand-file-name (concat (plist-get talk :video-slug) "--bbb-webcams.opus") emacsconf-cache-dir)
												"-c" "copy"
												(expand-file-name (concat (plist-get talk :video-slug) "--answers.webm") emacsconf-cache-dir))
				(copy-file
				 (expand-file-name (concat (plist-get talk :video-slug) "--bbb-webcams.webm") emacsconf-cache-dir)
				 (expand-file-name (concat (plist-get talk :video-slug) "--answers.webm") emacsconf-cache-dir)
				 t)))
		(unless (file-exists-p (expand-file-name (concat (plist-get talk :video-slug) "--answers.opus") emacsconf-cache-dir))
			(copy-file
			 (expand-file-name (concat (plist-get talk :video-slug) "--bbb-webcams.opus") emacsconf-cache-dir)
			 (expand-file-name (concat (plist-get talk :video-slug) "--answers.opus") emacsconf-cache-dir)
			 t))
		(dolist (suffix '("--answers.webm" "--answers.opus"))
			(unless (file-exists-p (expand-file-name (concat (plist-get talk :video-slug) suffix) emacsconf-backstage-dir))
				(copy-file
				 (expand-file-name (concat (plist-get talk :video-slug) suffix) emacsconf-cache-dir)
				 (expand-file-name (concat (plist-get talk :video-slug) suffix) emacsconf-backstage-dir)
				 t))
			(unless (file-exists-p (expand-file-name (concat (plist-get talk :video-slug) suffix) emacsconf-public-media-directory))
				(copy-file
				 (expand-file-name (concat (plist-get talk :video-slug) suffix) emacsconf-backstage-dir)
				 (expand-file-name (concat (plist-get talk :video-slug) suffix) emacsconf-public-media-directory)
				 t)))
		;; Update the org entry
		(save-window-excursion
			(emacsconf-go-to-talk talk)
			(org-entry-put (point) "QA_PUBLIC" "t")
			(org-entry-put (point) "QA_NOTE" (or note (concat "Q&A posted publicly." (emacsconf-surround " " (plist-get talk :qa-note) "" "")))))))
;; (kill-new (mapconcat #'emacsconf-extract-bbb-events-xml (emacsconf-get-talk-info) ""))
;; (dolist (slug '("haskell" "hyperorg" "health" "jupyter" "workflows" "wayland" "mail" "meetups" "orgsuperlinks" "rde" "science"))
;; 	(emacsconf-extract-publish-qa slug))
(provide 'emacsconf-extract)
;;; emacsconf-extract.el ends here
