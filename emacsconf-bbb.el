;;; emacsconf-bbb.el --- BigBlueButton               -*- lexical-binding: t; -*-

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

;;;###autoload
(defun emacsconf-bbb-status (talk)
  (let ((states
         '((open . "OPEN_Q UNSTREAMED_Q")
           (before . "TODO TO_REVIEW TO_ACCEPT WAITING_FOR_PREREC TO_PROCESS PROCESSING TO_AUTOCAP TO_ASSIGN TO_CAPTION TO_CHECK TO_STREAM PLAYING CLOSED_Q")
           (after . "TO_ARCHIVE TO_EXTRACT TO_REVIEW_QA TO_INDEX_QA TO_CAPTION_QA TO_FOLLOW_UP DONE")
           (cancelled . "CANCELLED"))))
    (if (string-match "live" (or (plist-get talk :q-and-a) ""))
        (or (car (seq-find (lambda (state)
                             (member (plist-get talk :status) (split-string (cdr state))))
                           states))
            (throw 'error "Unknown talk BBB state"))
      'irc)))

(defvar emacsconf-bbb-base-url "https://bbb.emacsverse.org/" "Include trailing slash.")
(defun emacsconf-bbb-room-title-list (&optional info)
  (delq nil
        (mapcar
         (lambda (o)
           (when (car o)
             (concat "ec" (substring emacsconf-year 2)
                     "-" (plist-get (emacsconf-get-shift (plist-get (cadr o) :start-time)) :id)
                     "-" (plist-get (emacsconf-get-track (plist-get (cadr o) :track)) :id)
                     " " (car o)
                     " ("
                     (mapconcat (lambda (talk) (plist-get talk :slug)) (cdr o) ", ")
                     ")")))
         (seq-group-by (lambda (o) (plist-get o :speakers))
                       (or info (emacsconf-active-talks (emacsconf-filter-talks (emacsconf-get-talk-info))))))))

(defun emacsconf-bbb-create-rooms ()
  "Copy the commands needed to create the rooms.
docker exec -it greenlight-v3 /bin/bash -c \"bundle exec rails console\"
user_id = User.find_by_email(\"emacsconf@sachachua.com\").id"
  (interactive)
  (kill-new
   (mapconcat (lambda (group)
					      (format
					       "Room.create(user_id: user_id, name: \"%s - %s\")\n"
					       (plist-get (cadr group) :speakers)
					       (string-join (mapcar (lambda (talk) (plist-get talk :slug))
																		  (cdr group))
                              ", ")))
				      (emacsconf-mail-groups (emacsconf-active-talks (emacsconf-get-talk-info)))
				      ""))
  (message "Copied. Run it inside the greenlight-v3 rails console."))

(defun emacsconf-bbb-load-rooms (string)
	"Split STRING and load them as ROOM properties.
STRING should be a list of rooms, one room per line, like this:
friendly-id speaker - slugs
friendly-id speaker - slugs

Print out room IDs with:
Room.all.each { |x| puts x.friendly_id + " " + x.name }; nil
"
  (interactive "MInput: ")
	(let ((rooms
				 (mapcar (lambda (row) (when (string-match "^\\(.+?\\) \\(.+\\)" row)
																 (list (match-string 1 row) (match-string 2 row))))
								 (split-string string "\n"))))
		(mapc (lambda (talk)
						(emacsconf-go-to-talk talk)
						(when (plist-get talk :speakers)
							(org-entry-put
							 (point)
							 "ROOM"
							 (concat
								emacsconf-bbb-base-url
								"rooms/"
								(car
								 (seq-find
									(lambda (o)
										(string-match
										 (concat
											"^"
											(regexp-quote
											 (plist-get talk :speakers))
											" - ")
										 (cadr o)))
									rooms))
								"/join"))))
					(emacsconf-active-talks (emacsconf-get-talk-info)))))



(defun emacsconf-bbb-spookfox-set-moderator-codes ()
  (interactive)
  (dolist (talk (seq-filter (lambda (o)
														  (and (plist-get o :bbb-room)
																   (not (plist-get o :bbb-mod-code))))
													  (emacsconf-publish-prepare-for-display (emacsconf-get-talk-info))))
	  (spookfox-js-injection-eval-in-active-tab
	   (format "window.location.href = \"%s\""
					   (replace-regexp-in-string "/join" "" (plist-get talk :bbb-room)))
	   t)
	  (sleep-for 3)
	  (spookfox-js-injection-eval-in-active-tab
	   "document.querySelector('button[data-rr-ui-event-key=\"settings\"]').click()" t)
	  (spookfox-js-injection-eval-in-active-tab
	   "document.querySelector('input#glAnyoneCanStart').checked = true")
	  (spookfox-js-injection-eval-in-active-tab
	   "document.querySelector('input#muteOnStart').checked = true")
	  (spookfox-js-injection-eval-in-active-tab
	   "document.querySelectorAll('.border-end button')[2].click()" t)
	  (let ((code (spookfox-js-injection-eval-in-active-tab
							   "document.querySelector('.access-code-input input').value" t)))
		  (message "Setting %s to %s" (plist-get talk :slug) code)
		  (emacsconf-set-property-from-slug
		   talk "BBB_MOD_CODE"
		   code)
		  (sit-for 2))))

(defun emacsconf-bbb-spookfox-confirm-settings ()
  (interactive)
  (dolist (talk (seq-filter (lambda (o)
														(plist-get o :bbb-room))
													(emacsconf-publish-prepare-for-display (emacsconf-get-talk-info))))
	(spookfox-js-injection-eval-in-active-tab
	 (format "window.location.href = \"%s\""
					 (replace-regexp-in-string "/join" "" (plist-get talk :bbb-room)))
	 t)
	(sleep-for 3)
	(spookfox-js-injection-eval-in-active-tab
	 "document.querySelector('button[data-rr-ui-event-key=\"settings\"]').click()" t)
	(sleep-for 3)))

(defvar emacsconf-extract-bbb-raw-dir (format "~/proj/emacsconf/%s/bbb/" emacsconf-year) "End with a \"/\".")
(defvar emacsconf-extract-bbb-published-dir (format "~/proj/emacsconf/%s/bbb/published/" emacsconf-year) "End with a \"/\".")
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

(defun emacsconf-extract-download-published-recordings-command ()
	"Copy the command for downloading published recordings from SOURCE to DEST."
	(interactive)
	(kill-new
	 (mapconcat (lambda (o) (if (plist-get o :bbb-meeting-id)
															(replace-regexp-in-string
															 "/ssh:" ""
															 (format "rsync -avzue ssh %s %s # %s\n"
																			 (expand-file-name
																				(plist-get o :bbb-meeting-id)
																				(expand-file-name "published/presentation"
																													emacsconf-extract-bbb-path
																													))
																			 emacsconf-extract-bbb-published-dir
																			 (plist-get o :slug)))
														""))
							(emacsconf-get-talk-info))))

(defun emacsconf-extract-bbb-parse-events-dir (&optional dir)
	(mapcar (lambda (file)
						(emacsconf-extract-bbb-parse-events file))
					(directory-files-recursively (or dir emacsconf-extract-bbb-raw-dir) "events.xml")))

(defun emacsconf-extract-bbb-raw-events-file-name (talk)
	(setq talk (emacsconf-resolve-talk talk))
	(expand-file-name "events.xml" (expand-file-name (plist-get talk :bbb-meeting-id) emacsconf-extract-bbb-raw-dir)))

(defun emacsconf-extract-bbb-report (&optional event-xml-files)
	(let* ((max 0)
				 (participant-count 0)
				 (meeting-count 0)
				 (max-meetings 0)
				 (max-participants 0)
				 meeting-participants
				 (meeting-events
					(sort
					 (seq-mapcat
						(lambda (file)
							(let ((dom (xml-parse-file file))
										participants talking meeting-events)
								(mapc (lambda (o)
												(pcase (dom-attr o 'eventname)
													("ParticipantJoinEvent"
													 (cl-pushnew (cons (dom-text (dom-by-tag o 'userId))
																						 (dom-text (dom-by-tag o 'name)))
																			 participants)
													 (push (cons (string-to-number (dom-text (dom-by-tag o 'timestampUTC)))
																			 (dom-attr o 'eventname))
																 meeting-events))
													("ParticipantLeftEvent"
													 (when (string= (dom-attr o 'module) "PARTICIPANT")
														 (push (cons (string-to-number (dom-text (dom-by-tag o 'timestampUTC)))
																				 (dom-attr o 'eventname))
																	 meeting-events)))
													("ParticipantTalkingEvent"
													 (cl-pushnew (assoc-default (dom-text (dom-by-tag o 'participant)) participants) talking))
													((or
														"CreatePresentationPodEvent"
														"EndAndKickAllEvent")
													 (push (cons (string-to-number (dom-text (dom-by-tag o 'timestampUTC)))
																			 (dom-attr o 'eventname))
																 meeting-events))))
											(dom-search dom (lambda (o) (dom-attr o 'eventname))))
								(cl-pushnew (list ;; :slug (plist-get talk :slug)
														 :participants participants
														 :talking talking)
														meeting-participants)
								meeting-events))
						(or event-xml-files
								(mapcar #'emacsconf-extract-bbb-raw-events-file-name
												(seq-filter (lambda (talk) (plist-get talk :bbb-meeting-id))
																		(emacsconf-get-talk-info)))))
					 (lambda (a b) (< (car a) (car b))))))
		(dolist (event meeting-events)
			(pcase (cdr event)
				("CreatePresentationPodEvent" (cl-incf meeting-count) (when (> meeting-count max-meetings) (setq max-meetings meeting-count)))
				("ParticipantJoinEvent" (cl-incf participant-count) (when (> participant-count max-participants) (setq max-participants participant-count)))
				("ParticipantLeftEvent" (cl-decf participant-count))
				("EndAndKickAllEvent" (cl-decf meeting-count))))
		`((,(length meeting-participants) "Number of meetings analyzed")
			(,max-participants "Max number of simultaneous users")
			(,max-meetings "Max number of simultaneous meetings")
			(,(apply 'max (mapcar (lambda (o) (length (plist-get o :participants))) meeting-participants)) "Max number of people in one meeting")
			(,(length (seq-uniq (seq-mapcat (lambda (o) (mapcar #'cdr (plist-get o :participants))) meeting-participants))) "Total unique users")
			(,(length (seq-uniq (seq-mapcat (lambda (o) (plist-get o :talking)) meeting-participants))) "Total unique talking"))))

(defun emacsconf-extract-bbb-dired-raw (talk)
	(interactive (list (emacsconf-complete-talk-info)))
	(setq talk (emacsconf-resolve-talk talk))
	(dired (expand-file-name (plist-get talk :bbb-meeting-id) emacsconf-extract-bbb-raw-dir)))

(defun emacsconf-extract-bbb-dired-published (talk)
	(interactive (list (emacsconf-complete-talk-info)))
	(setq talk (emacsconf-resolve-talk talk))
	(dired (expand-file-name (plist-get talk :bbb-meeting-id) emacsconf-extract-bbb-published-dir)))


(defun emacsconf-extract-bbb-parse-events (xml-file)
	(let* ((dom (xml-parse-file xml-file))
				 (meeting-name (dom-attr (dom-by-tag dom 'metadata) 'meetingName))
				 (meeting-id (dom-attr dom 'meeting_id))
				 (conf-joined (dom-search dom (lambda (o) (and (string= (dom-tag o) "name") (string= (dom-text o) emacsconf-extract-conference-username)))))
				 (conf-joined-time
					(and conf-joined
							 (string-to-number (dom-text (dom-by-tag (dom-parent dom conf-joined) 'timestampUTC)))))
				 recording-start
				 recording-stop
				 recording-spans
				 stream-start
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

(defun emacsconf-extract-bbb-talking-report (meeting-xml)
	(let ((data (emacsconf-extract-bbb-parse-events meeting-xml)))
		(unless (string= "" (alist-get 'meeting-date data))
			(format "- %s %s: %s\n"
							(alist-get 'name data)
							(format-time-string "%a %I:%M %p"
																	(date-to-time
																	 (alist-get 'meeting-date data)))
							(mapconcat
							 (lambda (person)
								 (format "%s (%s)"
												 (car person)
												 (/ (cdr person) 60000)))
							 (sort
								(mapcar
								 (lambda (group)
									 (cons
										(car group)
										(apply '+ (mapcar (lambda (o) (- (elt o 2) (elt o 1))) (cdr group)))))
								 (seq-group-by 'car (alist-get 'talking data)))
								:key 'cdr
								:reverse t)
							 ", ")))))



(defun emacsconf-extract-bbb-parse-events-for-talk (talk)
	"Parse events TALK from raw recordings.
This works with the events.xml from /var/bigbluebutton/raw.
Files should be downloaded to `emacsconf-extract-bbb-raw-dir'."
	(setq talk (emacsconf-resolve-talk talk))
	(emacsconf-extract-bbb-parse-events (emacsconf-extract-bbb-raw-events-file-name talk)))

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
				 (webcam-duration (emacsconf-get-file-duration-ms webcam-video))
				 (stop-ms (or stop-ms webcam-duration))
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
						(if (and (= start-ms 0)
										 (= stop-ms webcam-duration))
								(format "cp %s %s"
												webcam-video
												output)
							(compile-media-get-command
							 (compile-media-split-tracks
								(list (list :source webcam-video :start-ms start-ms :stop-ms stop-ms)))
							 output)))))
		(when (called-interactively-p 'any)
			(kill-new command))
		command))

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
						 (or (dom-elements dom 'eventname "StopRecordingEvent")
								 (dom-elements dom 'eventname "EndAndKickAllEvent"))
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

(defun emacsconf-extract-waveform-published-webcam-video (talk)
	(interactive (list (emacsconf-complete-talk-info)))
	(setq talk (emacsconf-resolve-talk talk))
	(waveform-show (expand-file-name "video/webcams.webm"
																	 (expand-file-name (plist-get talk :bbb-meeting-id) emacsconf-extract-bbb-published-dir))))

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

(defvar emacsconf-extract-conference-username "emacsconf" "Name of the streaming user.")

(provide 'emacsconf-bbb)
;;; emacsconf-bbb.el ends here
