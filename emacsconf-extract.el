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

;; - set BBB_REC to the published presentation URL. If keeping the presentation recording private, don't set BBB_REC; set BBB_MEETING_ID to just the meeting ID.
;; - Use the command from emacsconf-extract-raw-recordings-download-command to download raw data.

;;; Code:

(defun emacsconf-extract-chat (filename)
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
								(with-temp-file (expand-file-name (concat (plist-get o :file-prefix) "--extract.txt")
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
													(not (file-exists-p (expand-file-name (concat (plist-get o :file-prefix) "--bbb-" file) emacsconf-cache-dir))))
								 (copy-file (expand-file-name file playback-dir)
														(expand-file-name (concat (plist-get o :file-prefix) "--bbb-" file) emacsconf-cache-dir)
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

;;;###autoload
(defun emacsconf-extract-copy-pad-to-wiki ()
	"Copy the notes and questions from the current file to the wiki page for this talk."
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
			(unless (string= (or questions "") "")
				(insert "## Questions and answers\n\n" questions "\n\n"))
			(unless (string= (or notes "") "")
				(insert "## Notes\n\n" notes "\n\n")))))

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
								(plist-get o :file-prefix)))
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

(defvar emacsconf-extract-irc-speaker-nick nil "*Nick for the speaker.")

(defun emacsconf-extract-selected-irc ()
	"Copy the lines that start with -."
	(interactive)
	(let ((results ""))
		(save-excursion
			(goto-char (point-min))
			(while (re-search-forward "^\\( *- \\([QA]: \\)?\\)\\[[0-9:]+\\] <.*?> \\(.*\n\\)" nil t)
				(setq results (concat results (match-string 1) (match-string 3)))
				(replace-match "" nil t nil 1))
			(kill-new results))))

(defun emacsconf-extract-irc-backward-by-nick ()
	(interactive)
	(goto-char (line-beginning-position))
	(when (looking-at "\\[[0-9:]+\\] <\\(.*?\\)> \\([^ :]+?\\)?[ :]\\(.+\\)$")
		(save-excursion
			(let ((nick (match-string 2)))
				(while (and (re-search-backward (concat "\\[[0-9:]+\\] <" (regexp-quote nick) ">") nil t)
										(y-or-n-p "Continue? "))
					;; keep going backwards
					)))))

(defun emacsconf-extract-irc-copy-line-to-other-window-as-list-item (&optional prefix indent)
	(interactive)
	(goto-char (line-beginning-position))
	(when (looking-at " *\\[[0-9:]+\\] <\\(.*?\\)> \\([^ ]+?:\\)?\\(.+\\)$")
		(let ((line (string-trim (match-string 3)))
					(prefix (or
									 prefix
									 (and (string= (or emacsconf-extract-irc-speaker-nick "")
																 (match-string 1))
												"A: ")
									 "")))
			(setq line
						(concat
						 (if (or (string= prefix "A: ") indent) "  " "")
						 "- "
						 prefix
						 line "\n"))
			(other-window 1)
			(insert line)
			(other-window 1)
			(forward-line 1))))

(defun emacsconf-extract-irc-copy-line-to-other-window-as-question ()
	(interactive)
	(emacsconf-extract-irc-copy-line-to-other-window-as-list-item "Q: "))

(defvar emacsconf-extract-irc-map (make-sparse-keymap))
(defalias 'emacsconf-extract-irc-other-window #'other-window)
(defalias 'emacsconf-extract-irc-next-line #'next-line)
(defalias 'emacsconf-extract-irc-previous-line #'previous-line)
(defun emacsconf-extract-irc-open-talk-in-other-window (talk)
	(interactive (list (emacsconf-complete-talk-info)))
	(other-window 1)
	(emacsconf-edit-wiki-page talk))

(require 'hydra)
(defhydra emacsconf-extract-irc ()
  "Make it easy to extract lines from IRC"
  ("c" emacsconf-extract-irc-copy-line-to-other-window-as-list-item "copy")
	("q" (emacsconf-extract-irc-copy-line-to-other-window-as-list-item "Q: ") "question")
	("o" other-window "other")
	("t" emacsconf-extract-irc-open-talk-in-other-window "talk")
	("n" next-line "next")
	("p" previous-line "previous")
	("N" move-line-down "move down")
	("P" move-line-up "move up")
	("<right>" (progn (goto-char (line-beginning-position)) (insert "  ")) "indent")
	("<left>" (progn (goto-char (line-beginning-position)) (delete-char 2)) "dedent")
	("<prior>" scroll-down-command)
	("<next>" scroll-up-command)
	("a" (emacsconf-extract-irc-copy-line-to-other-window-as-list-item "A: ") "answer")
	("l" (save-window-excursion (other-window 1) (consult-line)) "check line")
	("r" (when (string-match )) (re-search-backward nil t))
	(" " pop-to-mark-command)
  )

(defun emacsconf-extract-irc-anonymize-log (beg end speakers)
	(interactive "r\nMNick(s): ")
	(when (stringp speakers) (setq speakers (split-string speakers)))
	(let ((text (buffer-substring beg end))
				nicks)
		(with-temp-buffer
			(insert text)
			(goto-char (point-min))
			;; make a list of nicks
			(while (re-search-forward "^\\[[0-9:]+\\] <\\(.*?\\)>" nil t)
				(unless (member (match-string 1) speakers)
					(add-to-list 'nicks (match-string 1))))
			(goto-char (point-min))
			(while (re-search-forward "^\\[[0-9:]+\\] <\\(.*?\\)> \\(.+\\)" nil t)
				(replace-match
				 (if (member (match-string 1) speakers)
						 (concat "  - A: " (match-string 2))
					 (format "- {{%d}} %s"
									 (seq-position nicks (match-string 1))
									 (propertize (match-string 2)
															 'nick (match-string 1))))))
			(goto-char (point-min))
			(perform-replace (regexp-opt nicks) (lambda ()))
			(setq text (buffer-string))
			(other-window 1)
			(insert text))))

(defun emacsconf-private-qa (&optional info)
	(seq-remove (lambda (o)
								(or (null (emacsconf-talk-file o "--bbb-webcams.webm"))
										(plist-get o :qa-public)))
							(or info (emacsconf-get-talk-info))))
;; sqlite detached localizing
(defun emacsconf-extract-review-qa (talk)
	(interactive (list (emacsconf-complete-talk-info (emacsconf-private-qa))))
	(find-file (emacsconf-talk-file talk "--bbb-webcams.vtt")))

(defun emacsconf-extract-publish-qa (talk &optional time)
	(interactive (list (emacsconf-complete-talk-info (unless current-prefix-arg (emacsconf-private-qa)))
										 (when current-prefix-arg
											 (read-string "Time: "))))
	(when (stringp talk) (setq talk (emacsconf-resolve-talk talk)))
	(let ((buff (get-buffer-create "*ffmpeg*"))
				(large-file-warning-threshold nil))
		(cond
		 ((emacsconf-talk-file talk "--bbb-deskshare.webm")
			(apply 'call-process "ffmpeg" nil buff nil
						 (append
							(list
							 "-y"
							 "-i"
							 (expand-file-name
								(concat
								 (plist-get talk :file-prefix)
								 "--bbb-deskshare.webm")
								emacsconf-cache-dir))
							(when time (list "-to" time))
							(list
							 "-i"
							 (expand-file-name
								(concat
								 (plist-get talk :file-prefix)
								 "--bbb-webcams.opus")
								emacsconf-cache-dir))
							(when time (list "-to" time))
							(list
							 "-c"
							 "copy"
							 (expand-file-name
								(concat
								 (plist-get talk :file-prefix)
								 "--answers.webm")
								emacsconf-cache-dir)))))
		 (time
			(apply 'call-process "ffmpeg" nil buff nil
						 (append
							(list
							 "-y"
							 "-i"
							 (expand-file-name
								(concat
								 (plist-get talk :file-prefix)
								 "--bbb-webcams.webm")
								emacsconf-cache-dir))
							(when time (list "-to" time))
							(list
							 "-c"
							 "copy"
							 (expand-file-name
								(concat
								 (plist-get talk :file-prefix)
								 "--answers.webm")
								emacsconf-cache-dir)))))
		 (t
			(copy-file
			 (expand-file-name
				(concat
				 (plist-get talk :file-prefix)
				 "--bbb-webcams.webm")
				emacsconf-cache-dir)
			 (expand-file-name
				(concat
				 (plist-get talk :file-prefix)
				 "--answers.webm")
				emacsconf-cache-dir)
			 t)))
		(call-process "ffmpeg" nil buff nil "-y" "-i"
									(emacsconf-talk-file talk "--answers.webm")
									"-c" "copy"
									(emacsconf-talk-file talk "--answers.opus" t))
		(dolist (suffix '("opus" "webm"))
			(copy-file
			 (expand-file-name
				(concat
				 (plist-get talk :file-prefix)
				 "--answers." suffix)
				emacsconf-cache-dir)
			 (expand-file-name
				(concat
				 (plist-get talk :file-prefix)
				 "--answers." suffix)
				emacsconf-backstage-dir)
			 t)
			(copy-file
			 (expand-file-name
				(concat
				 (plist-get talk :file-prefix)
				 "--answers." suffix)
				emacsconf-backstage-dir)
			 (expand-file-name
				(concat
				 (plist-get talk :file-prefix)
				 "--answers." suffix)
				emacsconf-public-media-directory)
			 t))
		(save-window-excursion
			(emacsconf-go-to-talk talk)
			(org-entry-put
			 (point)
			 "QA_PUBLIC" "t")
			(unless (string-match "Q&A posted publicly." (or (org-entry-get (point) "QA_NOTE") ""))
				(org-entry-put
				 (point)
				 "QA_NOTE"
				 (concat "Q&A posted publicly."
								 (emacsconf-surround " "
																		 (org-entry-get (point) "QA_NOTE")
																		 "" "")))))))
;; (emacsconf-extract-publish-qa "workflows" "13:56.000") (emacsconf-extract-publish-qa "journalism") (emacsconf-extract-publish-qa "handwritten" "28:36.240")
   ;; (kill-new (mapconcat #'emacsconf-extract-bbb-events-xml (emacsconf-get-talk-info) ""))
;; (dolist (slug '("haskell" "hyperorg" "health" "jupyter" "workflows" "wayland" "mail" "meetups" "orgsuperlinks" "rde" "science"))
;; 	(emacsconf-extract-publish-qa slug))

(defun emacsconf-extract-add-help-index-qa (talk)
  (interactive (list (emacsconf-complete-talk-info)))
	(if (stringp talk) (setq talk (emacsconf-resolve-talk talk)))
	(when (and (emacsconf-talk-file talk "--answers.vtt")
						 (not (emacsconf-talk-file talk "--answers--chapters.vtt")))
		(with-current-buffer (find-file-noselect (expand-file-name (concat (plist-get talk :slug) ".md") (expand-file-name "talks" (expand-file-name emacsconf-year emacsconf-directory))))
			(goto-char (point-min))
			(unless (re-search-forward "help_with_chapter_markers" nil t)
				(when (re-search-forward (concat (plist-get talk :slug) "-before)") nil t)
					(forward-line 1)
					(insert (format "[[!template id=\"help\"
volunteer=\"\"
summary=\"Q&A could be indexed with chapter markers\"
tags=\"help_with_chapter_markers\"
message=\"\"\"The Q&A session for this talk does not have chapter markers yet.
Would you like to help? See [[help_with_chapter_markers]] for more details. You can use the vidid=\"%s-qanda\" if adding the markers to this wiki page, or e-mail your chapter notes to <emacsconf-submit@gnu.org>.\"\"\"]]

" (plist-get talk :slug)))
					(save-buffer))))))

;; (mapc #'emacsconf-extract-add-help-index-qa (emacsconf-prepare-for-display (emacsconf-get-talk-info)))

;; (emacsconf-extract-download-published-recordings "bbb:/var/bigbluebutton/published/presentation/" "" ~/proj/emacsconf/2023/bbb-published/"")
(defvar emacsconf-extract-bbb-raw-dir "~/proj/emacsconf/2023/bbb/" "End with a \"/\".")
(defvar emacsconf-extract-bbb-published-dir "~/proj/emacsconf/2023/bbb-published/" "End with a \"/\".")
(defvar emacsconf-extract-conference-username "emacsconf" "Name of the streaming user.")
(defvar emacsconf-extract-bbb-path "/ssh:bbb@bbb.emacsverse.org:/var/bigbluebutton/")

(defun emacsconf-extract-raw-recordings-download-command ()
	"Copy the command for downloading raw recordings."
	(interactive)
	(let ((s (mapconcat (lambda (o) (if (plist-get o :bbb-meeting-id)
																			(format "rsync -avzue ssh %s %s\n"
																							(expand-file-name (plist-get o :bbb-meeting-id) emacsconf-extract-bbb-path)
																							emacsconf-extract-bbb-raw-dir)
																		""))
											(emacsconf-get-talk-info))))
		(when (called-interactively-p 'any) (kill-new s))
		s))

(defun emacsconf-extract-download-published-recordings (source dest)
	"Copy the command for downloading published recordings from SOURCE to DEST."
	(kill-new
	 (mapconcat (lambda (o) (if (plist-get o :bbb-meeting-id)
															(format "rsync -avzue ssh %s%s %s\n"
																			source
																			(match-string 1 (plist-get o :bbb-rec))
																			dest)
														""))
							(emacsconf-get-talk-info))))

(defun emacsconf-extract-bbb-parse-events-dir (&optional dir)
	(mapcar (lambda (file)
						(emacsconf-extract-bbb-parse-events file))
					(directory-files-recursively (or dir emacsconf-extract-bbb-raw-dir) "events.xml")))

(defun emacsconf-extract-bbb-raw-events-file-name (talk)
	(setq talk (emacsconf-resolve-talk talk))
	(expand-file-name "events.xml" (expand-file-name (plist-get talk :bbb-meeting-id) emacsconf-extract-bbb-raw-dir)))

(defun emacsconf-extract-bbb-parse-events (talk)
	"Parse events TALK from raw recordings.
This works with the events.xml from /var/bigbluebutton/raw.
Files should be downloaded to `emacsconf-extract-bbb-raw-dir'."
	(setq talk (emacsconf-resolve-talk talk))
	(let* ((xml-file (emacsconf-extract-bbb-raw-events-file-name talk))
				 (dom (xml-parse-file xml-file))
				 (meeting-name (dom-attr (dom-by-tag dom 'metadata) 'meetingName))
				 (meeting-id (dom-attr dom 'meeting_id))
				 (conf-joined (dom-search dom (lambda (o) (and (string= (dom-tag o) "name") (string= (dom-text o) emacsconf-extract-conference-username)))))
				 (conf-joined-time
					(and conf-joined
							 (string-to-number (dom-text (dom-by-tag (dom-parent dom conf-joined) 'timestampUTC)))))
				 recording-start
				 recording-stop
				 recording-spans
				 chat
				 talking
				 participants
				 talking-starts
				 deskshare-info
				 (meeting-date (dom-text (dom-by-tag (dom-parent dom (car conf-joined)) 'date))))
		(dolist (event (dom-by-tag dom 'event))
			(let ((timestamp (string-to-number (dom-text (dom-by-tag event 'timestampUTC)))))
				(pcase (dom-attr event 'eventname)
					("ParticipantJoinEvent"
					 (push (cons (dom-text (dom-by-tag event 'userId))
											 (dom-text (dom-by-tag event 'name)))
								 participants))
					("StartRecordingEvent"
					 (setq recording-start timestamp
								 recording-stop nil
								 recording-file
								 (file-name-nondirectory (dom-text (dom-by-tag event 'filename)))))
					("StopRecordingEvent"
					 (setq recording-stop timestamp)
					 (push (cons recording-start recording-stop) recording-spans))
					("PublicChatEvent"
					 ;; only include events in the public recording
					 (when (and recording-start
											(null recording-stop)
											(>= timestamp recording-start))
						 (push (list
										timestamp
										(dom-text (dom-by-tag event 'sender))
										(with-temp-buffer
											(insert
											 (replace-regexp-in-string
												"&#39;" "'"
												(replace-regexp-in-string "<.*?>" ""
																									(dom-text (dom-by-tag event 'message)))))
											(mm-url-decode-entities)
											(buffer-string)))
									 chat)))
					("ParticipantTalkingEvent"
					 (let* ((speaker (assoc-default
														(dom-text (dom-by-tag event 'participant))
														participants)))
						 (if (string= (dom-text (dom-by-tag event 'talking)) "true")
								 ;; started talking
								 (if (assoc-default speaker talking-starts)
										 (setcdr (assoc speaker talking-starts)
														 timestamp)
									 (push (cons speaker timestamp) talking-starts))
							 (when (and recording-start
													(>= timestamp recording-start)
													(assoc-default speaker talking-starts)
													(or (null recording-stop)
															(<= (assoc-default speaker talking-starts)
																	recording-stop)))
								 (push (list speaker
														 (- (max (assoc-default speaker talking-starts) recording-start)
																recording-start)
														 (- (if recording-stop (min recording-stop timestamp) timestamp)
																recording-start)
														 recording-file)
											 talking)))))
					("StartWebRTCDesktopShareEvent"
					 (setq deskshare-info (cons (dom-text (dom-by-tag event 'filename))
																			timestamp)))

					)))
		`((name . ,meeting-name)
			(id . ,meeting-id)
			(conf-joined . ,conf-joined-time)
			(recording-start . ,recording-start)
			(meeting-date . ,meeting-date)
			(participants . ,participants)
			(talking . ,(nreverse talking))
			(chat . ,(nreverse chat)))))

(defun emacsconf-extract-bbb-format-chat ()
	(mapconcat
		 (lambda (events)
			 (format "- %s (%s)\n%s"
							 (assoc-default 'name events)
							 (assoc-default 'id events)
							 (mapconcat
								(lambda (message)
									(format "  - %s: %s\n"
													(elt message 1)
													(with-temp-buffer
														(insert
														 (replace-regexp-in-string
															"&#39;" "'"
															(replace-regexp-in-string "<.*?>" ""
																												(elt message 2))))
														(mm-url-decode-entities)
														(buffer-string))))
								(assoc-default 'chat events)
								"")))
		 (emacsconf-extract-bbb-parse-events-dir)
		 ""))

(defun emacsconf-extract-spookfox-update-bbb-rec ()
	(interactive)
	(let* ((data
					(spookfox-js-injection-eval-in-active-tab
					 "row = [...document.querySelectorAll('.dropdown-toggle')].find((o) => o.textContent.match('Unlisted')).closest('tr'); [row.querySelector('#recording-text').getAttribute('title'), row.querySelector('a.btn-primary').getAttribute('href')]" t)
					)
				 (slug
					(when (and data (string-match "^\\([^(]+\\) (" (elt data 0)))
						(split-string
						 (match-string 1 (elt data 0))
						 ", "))))
		(when data
			(if (> (length slug) 1)
					(setq slug (completing-read "Talk: " slug))
				(setq slug (car slug)))
			(emacsconf-with-talk-heading slug
				(if (org-entry-get (point) "BBB_REC")
						(progn
							(kill-new (elt data 1))
							(error "%s already has BBB_REC?" slug))
					(org-entry-put (point) "BBB_REC" (elt data 1))))
			(message "Updated BBB_REC for %s to %s" slug (elt data 1))
			(spookfox-js-injection-eval-in-active-tab
			 "row = [...document.querySelectorAll('.dropdown-toggle')].find((o) => o.textContent.match('Unlisted')).closest('tr'); row.querySelector('.button_to .dropdown-item .fa-globe').closest('button').click();" t))))


(defun emacsconf-extract-chat (slug speaker)
  (interactive (list
                (emacsconf-complete-talk)
                (completing-read "Speaker: "
                                 (seq-uniq
                                  (mapcar (lambda (node) (dom-attr node 'name))
                                          (dom-by-tag (xml-parse-region (point-min) (point-max)) 'chattimeline)))
                                 )))
  (let ((text
         (mapconcat (lambda (node)
                      (when (string= (dom-attr node 'target) "chat")
                        (let ((message
                               (replace-regexp-in-string
                                "\\(^[^ +]?\\): " ""
                                (replace-regexp-in-string "<a href=\"\\(.+?\\)\" rel=\"nofollow\"><u>\\(.+?\\)</u></a>"
                                                          "<\\1>" (dom-attr node 'message)))))
                          (if (string-match speaker (dom-attr node 'name))
                              (format "- %s: %s\n" speaker message)
                            (format "- %s\n" message)))))
                    (dom-by-tag (xml-parse-region (point-min) (point-max)) 'chattimeline)
                    "")))
    (emacsconf-edit-wiki-page slug)
    (if (re-search-forward "# Discussion" nil t)
        (progn
          (goto-char (match-end 0))
          (insert "\n\n"))
      (goto-char (point-max)))
    (kill-new text)))
;; TODO: Combine lines from same nick, or identify speakers with anon1/2/etc.
(defun emacsconf-extract-chat-from-dired ()
  (interactive)
  (find-file (expand-file-name "slides_new.xml" (dired-get-file-for-visit)))
  (call-interactively 'emacsconf-extract-chat))

(defun emacsconf-make-webcams-deskshare-spans (talk &optional start-ms stop-ms strategy)
  (let* ((start-ms (or start-ms 0))
				 (source-dir (expand-file-name (plist-get talk :bbb-meeting-id) emacsconf-extract-bbb-published-dir))
				 (secs (/ start-ms 1000.0))
				 (deskshare (xml-parse-file (expand-file-name "deskshare.xml" source-dir)))
         (webcam-video (expand-file-name "video/webcams.webm" source-dir))
         (deskshare-video (expand-file-name "deskshare/deskshare.webm" source-dir))
				 (stop-ms (or stop-ms (emacsconf-get-file-duration-ms deskshare-video)))
         spans)
    (mapc (lambda (o)
            (unless (or (= secs (string-to-number (dom-attr o 'start_timestamp)))
                        (= (string-to-number (dom-attr o 'start_timestamp)) 0)
                        (> secs (/ stop-ms 1000.0)))
              (setq spans (cons (list :source webcam-video
                                      :start-ms (* secs 1000)
                                      :stop-ms
                                      (* 1000
                                         (if (eq strategy 'test)
                                             (+ secs 3)
                                           (max secs (string-to-number (dom-attr o 'start_timestamp))))))
                                spans)))
            (when (and (<= (string-to-number (dom-attr o 'start_timestamp))
                           (/ stop-ms 1000.0))
                       (>= (string-to-number (dom-attr o 'stop_timestamp))
                           (/ start-ms 1000.0)))
              (setq spans (cons (list :source deskshare-video
                                      :start-ms (max (* 1000 (string-to-number (dom-attr o 'start_timestamp)))
                                                     start-ms)
                                      :stop-ms
                                      (if (eq strategy 'test)
                                          (* 1000 (+ (string-to-number (dom-attr o 'start_timestamp)) 3))
                                        (min (* 1000 (string-to-number (dom-attr o 'stop_timestamp)))
                                             stop-ms)))
                                spans))
              (setq secs (string-to-number (dom-attr o 'stop_timestamp)))))
          (dom-by-tag deskshare 'event))
    (unless (>= (floor (* secs 1000)) stop-ms)
      (setq spans (cons (list :source webcam-video
                              :start-ms (* 1000 secs)
                              :stop-ms (if (eq strategy 'test)
                                           (* 1000 (+ secs 3))
                                         stop-ms))
                        spans)))
    (if (eq strategy 'test)
        `((video ,@(nreverse spans))
          (audio ,@(mapcar (lambda (o)
                             (list :source webcam-video
                                   :start-ms (plist-get o :start-ms)
                                   :stop-ms (plist-get o :stop-ms)))
                           (reverse spans))))
      `((video ,@(nreverse spans))
        (audio (:source ,webcam-video :start-ms ,start-ms :stop-ms ,stop-ms))))))

(defun emacsconf-get-ffmpeg-to-splice-webcam-and-recording (talk &optional start-ms stop-ms info strategy)
  "Return FFMPEG command for slicing.
Strategies:
- 'fast-cut-start-keyframe - find the keyframe before the start ms and cut from there, doing a fast copy.
- 'start-keyframe-and-reencode - find the keyframe before the start ms and cut from there, reencoding.
- 'cut-and-concat - seek to the keyframe before, slowly find the start-ms, reencode the snippet, and then do a fast copy of the remaining. May have encoding errors.
- default: copy from start-ms to stop-ms, reencoding.
"
	(interactive (list (emacsconf-complete-talk-info)))
	(setq talk (emacsconf-resolve-talk talk))
  (let* ((slug (plist-get talk :slug))
				 (start-ms (or start-ms 0))
				 (source-dir (expand-file-name (plist-get talk :bbb-meeting-id) emacsconf-extract-bbb-published-dir))
         (video-slug (plist-get (seq-find (lambda (o) (string= (plist-get o :slug) slug)) info) :video-slug))
         (output (expand-file-name (concat (plist-get talk :file-prefix) "--answers.webm") emacsconf-cache-dir))
         (webcam-video (expand-file-name "video/webcams.webm" source-dir))
         (deskshare-video (expand-file-name "deskshare/deskshare.webm" source-dir))
				 (stop-ms (or stop-ms (emacsconf-get-file-duration-ms webcam-video)))
				 (command
					(if (file-exists-p deskshare-video)
							;; Has deskshare
							(let* ((deskshare (xml-parse-file (expand-file-name "deskshare.xml" source-dir)))
										 (final-size (compile-media-max-dimensions
																	deskshare-video
																	webcam-video))
										 (duration (compile-media-get-file-duration-ms webcam-video))
										 (spans (emacsconf-make-webcams-deskshare-spans talk start-ms stop-ms strategy))
										 (compile-media-output-video-width (car final-size))
										 (compile-media-output-video-height (cdr final-size)))
								(compile-media-get-command spans output))
						;; Just webcams
						(compile-media-get-command
						 (compile-media-split-tracks
							(list (list :source webcam-video :start-ms start-ms :stop-ms stop-ms)))
						 output))))
		(when (called-interactively-p 'any)
			(kill-new command))
		command))

;; (kill-new
;;  (emacsconf-extract-replace-strings
;;  `((,(expand-file-name emacsconf-extract-bbb-published-dir) . "bbb-published/")
;; 	 (,(expand-file-name emacsconf-cache-dir) . "~/current/cache"))
;;  (emacsconf-get-ffmpeg-to-splice-webcam-and-recording "emacsconf")))

(defun emacsconf-extract-replace-strings (replacements s)
	(with-temp-buffer
		(insert s)
		(dolist (pair replacements (buffer-string))
			(goto-char (point-min))
			(while (re-search-forward (car pair) nil t)
				(replace-match (cdr pair))))
		(buffer-string)))

;; Works with a table of the form
;; | Start | End | Slug | Notes | URL | Timestamp |
;; |-------+-----+------+-------+-----+-----------|

(defun emacsconf-process-qa-recordings (qa dir)
	;; (setq conf-qa-recordings qa)
	;; (memoize 'conf-ffmpeg-get-closest-keyframe-in-msecs)
	;; (memoize 'conf-ffmpeg-get-keyframes-between)
	;; (memoize 'conf-video-dimensions)
	;; (memoize 'compile-media-get-file-duration-ms)
	;; (memoize-restore 'conf-ffmpeg-get-keyframes-around)

	(let ((info (emacsconf-get-talk-info)))
		(replace-regexp-in-string
		 "captions/" "answers-slow/"
		 (replace-regexp-in-string
			dir ""
			(string-join
			 (nreverse
				(sort
				 (delq nil
							 (mapcar
								(lambda (o)
									(when (> (length (car o)) 0)
										(emacsconf-get-ffmpeg-to-splice-webcam-and-recording
										 (elt o 2)
										 (compile-media-timestamp-to-msecs (elt o 0))
										 (compile-media-timestamp-to-msecs (elt o 1))
										 info)))
																				;       (seq-take qa 2)
								qa
								))
				 (lambda (a b) (string-match "trim" a))))
			 "\n")))))

;;; YouTube

;; When the token needs refreshing, delete the associated lines from ~/.authinfo

(defvar emacsconf-extract-google-client-identifier nil)
(defvar emacsconf-extract-youtube-api-channels nil)
(defvar emacsconf-extract-youtube-api-categories nil)

(defun emacsconf-extract-oauth-browse-and-prompt (url)
	"Open URL and wait for the redirected code URL."
	(browse-url url)
	(read-from-minibuffer "Paste the redirected code URL: "))

(defun emacsconf-extract-youtube-api-setup ()
	(interactive)
	(require 'plz)
	(require 'url-http-oauth)
	(when (getenv "GOOGLE_APPLICATION_CREDENTIALS")
		(let-alist (json-read-file (getenv "GOOGLE_APPLICATION_CREDENTIALS"))
			(setq emacsconf-extract-google-client-identifier .web.client_id)))
	(unless (url-http-oauth-interposed-p "https://youtube.googleapis.com/youtube/v3/")
		(url-http-oauth-interpose
		 `(("client-identifier" . ,emacsconf-extract-google-client-identifier)
			 ("resource-url" . "https://youtube.googleapis.com/youtube/v3/")
			 ("authorization-code-function" . emacsconf-extract-oauth-browse-and-prompt)
			 ("authorization-endpoint" . "https://accounts.google.com/o/oauth2/v2/auth")
			 ("authorization-extra-arguments" .
				(("redirect_uri" . "http://localhost:8080")))
			 ("access-token-endpoint" . "https://oauth2.googleapis.com/token")
			 ("scope" . "https://www.googleapis.com/auth/youtube")
			 ("client-secret-method" . prompt))))
	(setq emacsconf-extract-youtube-api-channels
				(plz 'get "https://youtube.googleapis.com/youtube/v3/channels?part=contentDetails&mine=true"
					:headers `(("Authorization" . ,(url-oauth-auth "https://youtube.googleapis.com/youtube/v3/")))
					:as #'json-read))
	(setq emacsconf-extract-youtube-api-categories
				(plz 'get "https://youtube.googleapis.com/youtube/v3/videoCategories?part=snippet&regionCode=CA"
					:headers `(("Authorization" . ,(url-oauth-auth "https://youtube.googleapis.com/youtube/v3/")))
					:as #'json-read))
	(setq emacsconf-extract-youtube-api-videos
				(plz 'get (concat "https://youtube.googleapis.com/youtube/v3/playlistItems?part=snippet,contentDetails,status&forMine=true&order=date&maxResults=50&playlistId="
													(url-hexify-string
													 (let-alist (elt (assoc-default 'items emacsconf-extract-youtube-api-channels) 0)
														 .contentDetails.relatedPlaylists.uploads)
													 ))
					:headers `(("Authorization" . ,(url-oauth-auth "https://youtube.googleapis.com/youtube/v3/")))
					:as #'json-read)))

(defvar emacsconf-extract-youtube-tags '("emacs" "emacsconf"))
(defun emacsconf-extract-youtube-object (video-id talk &optional privacy-status)
	"Format the video object for VIDEO-ID using TALK details."
	(setq privacy-status (or privacy-status "unlisted"))
	(let ((properties (emacsconf-publish-talk-video-properties talk 'youtube)))
		`((id . ,video-id)
			(kind . "youtube#video")
			(snippet
			 (categoryId . "28")
			 (title . ,(plist-get properties :title))
			 (tags . ,emacsconf-extract-youtube-tags)
			 (description . ,(plist-get properties :description))
			 ;; Even though I set recordingDetails and status, it doesn't seem to stick.
			 ;; I'll leave this in here in case someone else can figure it out.
			 (recordingDetails (recordingDate . ,(format-time-string "%Y-%m-%dT%TZ" (plist-get talk :start-time) t))))
			(status (privacyStatus . "unlisted")
							(license . "creativeCommon")))))

(defun emacsconf-extract-youtube-api-update-video (video-object)
	"Update VIDEO-OBJECT."
	(let-alist video-object
		(let* ((slug (cond
									;; not yet renamed
									((string-match (rx (literal emacsconf-id) " " (literal emacsconf-year) " "
																		 (group (1+ (or (syntax word) "-")))
																		 "  ")
																 .snippet.title)
									 (match-string 1 .snippet.title))
									;; renamed, match the description instead
									((string-match (rx (literal emacsconf-base-url) (literal emacsconf-year) "/talks/"
																		 (group (1+ (or (syntax word) "-"))))
																 .snippet.description)
									 (match-string 1 .snippet.description))
									;; can't find, prompt
									(t
									 (when (string-match (rx (literal emacsconf-id) " " (literal emacsconf-year))
																			 .snippet.title)
										 (completing-read (format "Slug for %s: "
																							.snippet.title)
																			(seq-map (lambda (o) (plist-get o :slug))
																							 (emacsconf-publish-prepare-for-display (emacsconf-get-talk-info))))))))
					 (video-id .snippet.resourceId.videoId)
					 (id .id)
					 result)
			(when slug
				;; set the YOUTUBE_URL property
				(emacsconf-with-talk-heading slug
					(org-entry-put (point) "YOUTUBE_URL" (concat "https://www.youtube.com/watch?v=" video-id))
					(org-entry-put (point) "YOUTUBE_ID" id))
				(plz 'put "https://www.googleapis.com/youtube/v3/videos?part=snippet,recordingDetails,status"
					:headers `(("Authorization" . ,(url-oauth-auth "https://youtube.googleapis.com/youtube/v3/"))
										 ("Accept" . "application/json")
										 ("Content-Type" . "application/json"))
					:body (json-encode (emacsconf-extract-youtube-object video-id (emacsconf-resolve-talk slug))))))))

(defun emacsconf-extract-youtube-rename-videos (&optional videos)
	"Rename videos and set the YOUTUBE_URL property in the Org heading."
	(let ((info (emacsconf-get-talk-info)))
		(mapc
		 (lambda (video)
			 (when (string-match (rx (literal emacsconf-id) " " (literal emacsconf-year)))
				 (emacsconf-extract-youtube-api-update-video video)))
		 (assoc-default 'items (or videos emacsconf-extract-youtube-api-videos)))))

(provide 'emacsconf-extract)
;;; emacsconf-extract.el ends here
