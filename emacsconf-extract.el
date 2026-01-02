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


;; (emacsconf-extract-extract-chat (expand-file-name "bbb-playbacks/haskell/slides_new.xml" emacsconf-cache-dir)) 

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

(defun emacsconf-extract-unescape (s)
	(replace-regexp-in-string
   "\\\\\\(['\"]\\)"
   "\\1" s))

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
			(insert "# Discussion\n\n")
			(save-excursion
				(unless (string= (or questions "") "")
					(insert "## Questions and answers\n\n" (emacsconf-extract-unescape questions) "\n\n"))
				(unless (string= (or notes "") "")
					(insert "## Notes\n\n" (emacsconf-extract-unescape notes) "\n\n"))))))

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
	(subed-set-subtitle-comment (concat "Q: " question)))


(defvar emacsconf-extract-irc-speaker-nick nil "*Nick for the speaker.")

(defun emacsconf-extract-pad-clean-up ()
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "\\\\" nil t)
    (replace-match ""))
  (goto-char (point-min))
  (while (re-search-forward "{rel=\"noreferrer noopener\"}" nil t)
    (replace-match "")))

(defun emacsconf-extract-selected-irc ()
	"Copy the lines that start with -."
	(interactive)
	(let ((results ""))
		(save-excursion
			(goto-char (point-min))
			(while (re-search-forward "^\\([qna] *\\| *- +\\([QA]: \\)?\\)\\[[0-9:]+\\] <.*?> \\(.*\n\\)" nil t)
				(setq results (concat results
															(save-match-data
																(pcase (match-string 1)
																	((rx "q") "- Q: ")
																	((rx "a") "- A: ")
																	((rx "n") "- ")
																	(_ "- ")))
															(match-string 3)))
				(replace-match "" nil t nil 1))
			(kill-new results))))

(defvar-keymap emacsconf-extract-irc-log-map
	"<down>" #'forward-line
	"<up>" #'previous-line
	"<right>" (lambda () (interactive) (insert "-") (forward-line))
	"<left>" #'forward-line
  "q" #'emacsconf-extract-irc-copy-line-to-other-window-as-question)

(defun emacsconf-extract-irc-log ()
	(interactive)
	(set-transient-map emacsconf-extract-irc-log-map t))

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
      (when (string= (match-string 2) "https")
        (setq line (concat (match-string 2) line)))
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

(defvar-keymap emacsconf-extract-irc-keymap
  :doc "Make it easy to extract lines from IRC"
  "c" #'emacsconf-extract-irc-copy-line-to-other-window-as-list-item
	"q" (lambda () (interactive) (emacsconf-extract-irc-copy-line-to-other-window-as-list-item "Q: "))
  "o" #'other-window
  "t" #'emacsconf-extract-irc-open-talk-in-other-window
  "n" #'next-line
	"p" #'previous-line
	"N" #'move-line-down
	"P" #'move-line-up
  "<up>" #'previous-line
  "<down>" #'next-line
  "<right>" (lambda () (interactive) (goto-char (line-beginning-position)) (insert "  "))
  "<left>" (lambda () (interactive) (goto-char (line-beginning-position)) (delete-char 2))
  "<prior>" #'scroll-down-command
	"<next>" #'scroll-up-command
  "a" (lambda () (interactive) (emacsconf-extract-irc-copy-line-to-other-window-as-list-item "A: "))
  "l" (lambda () (interactive) (save-window-excursion (other-window 1) (consult-line)))
	"<spc>" #'pop-to-mark-command)

(defun emacsconf-extract-irc ()
  (interactive)
  (set-transient-map emacsconf-extract-irc-keymap t))

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
  ("<up>" previous-line)
  ("<down>" next-line)
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
	(save-excursion
		(goto-char beg)
		(while (re-search-forward "^\\[[0-9:]+\\] <\\(.*?\\)> \\(.+\\)" end t)
			(if (member (match-string 1) speakers)
					(replace-match (concat "- " (match-string 1) ": " (match-string 2)) t t)
				(replace-match (concat "- " (match-string 2)) t t)))))


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

;; When the token needs refreshing, delete the associated lines from
;; ~/.authinfo This code just sets the title and description. Still
;; need to figure out how to properly set the license, visibility,
;; recording date, and captions.
;;
;; To avoid being prompted for the client secret, it's helpful to have a line in ~/.authinfo or ~/.authinfo.gpg with
;; machine https://oauth2.googleapis.com/token username CLIENT_ID password CLIENT_SECRET

;; reset:
;; (setq url-http-oauth--interposed nil url-http-oauth--interposed-regexp nil)
;; and remove the token from ~/.authinfo

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
				(("redirect_uri" . "http://localhost:8080")
				 ("access_type" . "offline")
				 ("prompt" . "consent")))
			 ("access-token-endpoint" . "https://oauth2.googleapis.com/token")
			 ("scope" . "https://www.googleapis.com/auth/youtube https://www.googleapis.com/auth/youtube.force-ssl https://www.googleapis.com/auth/youtube.upload")
			 ("client-secret-method" . prompt))))
	(setq emacsconf-extract-youtube-api-channels
				(plz 'get "https://youtube.googleapis.com/youtube/v3/channels?part=contentDetails&mine=true"
					:headers `(("Authorization" . ,(url-oauth-auth "https://youtube.googleapis.com/youtube/v3/")))
					:as #'json-read))
	(setq emacsconf-extract-youtube-api-playlists
				(plz 'get "https://youtube.googleapis.com/youtube/v3/playlists?part=snippet,contentDetails&mine=true"
					:headers `(("Authorization" . ,(url-oauth-auth "https://youtube.googleapis.com/youtube/v3/")))
					:as #'json-read))
	(setq emacsconf-extract-youtube-api-categories
				(plz 'get "https://youtube.googleapis.com/youtube/v3/videoCategories?part=snippet&regionCode=CA"
					:headers `(("Authorization" . ,(url-oauth-auth "https://youtube.googleapis.com/youtube/v3/")))
					:as #'json-read))
	(setq emacsconf-extract-youtube-api-videos
				(emacsconf-extract-youtube-api-paginated-request
				 (concat "https://youtube.googleapis.com/youtube/v3/playlistItems?part=snippet,contentDetails,status&forMine=true&order=date&maxResults=50&playlistId="
								 (url-hexify-string
									(let-alist (elt (assoc-default 'items emacsconf-extract-youtube-api-channels) 0)
										.contentDetails.relatedPlaylists.uploads)
									))
				 nil
				 (lambda (item)
					 (string-match (regexp-quote emacsconf-year)
												 (let-alist item .snippet.title))))))

(defun emacsconf-extract-youtube-comment-list ()
 (seq-mapcat
		(lambda (item)
			(append
			 (if (alist-get 'topLevelComment (alist-get 'snippet item))
					 (list (alist-get 'topLevelComment (alist-get 'snippet item))))
			 (alist-get 'comments (alist-get 'replies item))))
		(alist-get
		 'items
		 (or emacsconf-extract-youtube-comments (emacsconf-extract-youtube-get-channel-comments)))))

(defun emacsconf-extract-youtube-get-talk-for-video-id (video-id)
	(seq-find (lambda (o)
							(or (string-match (regexp-quote video-id) (or (plist-get o :youtube-url) ""))
									(string-match (regexp-quote video-id) (or (plist-get o :qa-youtube-url) ""))))
						(emacsconf-get-talk-info)))

(defun emacsconf-extract-youtube-comments-after (date)
	(interactive (list (org-read-date nil t nil "On or after date: ")))
	(when (stringp date)
		(setq date (org-read-date nil t date)))
	(seq-filter
	 (lambda (entry)
		 (time-less-p
			date
			(date-to-time
			 (alist-get
				'publishedAt
				(alist-get 'snippet entry)))))
	 (emacsconf-extract-youtube-comment-list)))

(defun emacsconf-extract-youtube-format-talk-comments (videos)
	(mapconcat
	 (lambda (video)
		 (format
			"- https://youtu.be/%s\n%s\n"
			(car video)
			(mapconcat
			 (lambda (comment)
				 (let-alist comment
					 (format
						"  - %s: %s\n"
						.snippet.authorDisplayName
						(replace-regexp-in-string "\n" "\n    " .snippet.textOriginal))))
			 (cdr video)
			 "")))
	 videos
	 ""))

(defun emacsconf-extract-youtube-comments-by-talk (&optional comments)
	(interactive (list
								(if current-prefix-arg (emacsconf-extract-youtube-comments-after (org-read-date nil nil nil "Date: ")))))
	(setq comments (or comments (emacsconf-extract-youtube-comment-list)))
	(let ((by-talk
				 (seq-group-by
					(lambda (group)
						(plist-get (emacsconf-extract-youtube-get-talk-for-video-id (car group)) :slug))
					(seq-group-by (lambda (o)
													(alist-get 'videoId (alist-get 'snippet o)))
												comments))))
		(when (called-interactively-p 'any)
			(with-current-buffer (get-buffer-create "*comments*")
				(erase-buffer)
				(org-mode)
				(dolist (group by-talk)
					(when (car group)
						(insert (format
										 "* %s\n\n%s\n\n"
										 (org-link-make-string
											(concat "file:"
															(expand-file-name
															 (concat
																(car group) ".md")
															 (expand-file-name
																"talks"
																(expand-file-name
																 emacsconf-year
																 emacsconf-directory))))
											(car group))
										 (emacsconf-extract-youtube-format-talk-comments (cdr group))))))
				(display-buffer (current-buffer))))
		by-talk))


;; (emacsconf-extract-youtube-comment-list)

;; (emacsconf-extract-youtube-comments-after "-2mon")

(defvar emacsconf-extract-youtube-comments nil)
(defun emacsconf-extract-youtube-get-channel-comments (&optional no-cache)
	(setq
	 emacsconf-extract-youtube-comments
	 (or (and emacsconf-extract-youtube-comments (not no-cache))
			 (plz 'get
				 (format
					"https://youtube.googleapis.com/youtube/v3/commentThreads?part=snippet,replies&allThreadsRelatedToChannelId=%s&maxResults=100"
					(alist-get 'id (car (alist-get 'items emacsconf-extract-youtube-api-channels))))
				 :headers `(("Authorization" . ,(url-oauth-auth "https://youtube.googleapis.com/youtube/v3/")))
				 :as #'json-read))))

(defvar emacsconf-extract-youtube-tags '("emacs" "emacsconf"))
(defun emacsconf-extract-youtube-object (video-id talk &optional privacy-status qa)
	"Format the video object for VIDEO-ID using TALK details.
If QA is non-nil, treat it as a Q&A video."
	(setq privacy-status (or privacy-status "public"))
	(let ((properties
				 (funcall (if qa
											#'emacsconf-publish-answers-video-properties
										#'emacsconf-publish-talk-video-properties)
									talk 'youtube)))
		`((id . ,video-id)
			(kind . "youtube#video")
			(snippet
			 (categoryId . "28")
			 (title . ,(plist-get properties :title))
			 (tags . ,emacsconf-extract-youtube-tags)
			 (description . ,(plist-get properties :description))
			 (recordingDetails (recordingDate . ,(format-time-string "%Y-%m-%dT%TZ" (plist-get talk :start-time) t))))
			;; oooh, publishing seems to work now
			(status (privacyStatus . ,privacy-status)
							(license . "creativeCommon")))))

(defun emacsconf-extract-youtube-get-slug-for-video (video-object)
	(let-alist video-object
		(cond
		 ;; not yet renamed
		 ((and .snippet.title (string-match (rx (literal emacsconf-id) " " (literal emacsconf-year) " "
																						(group (1+ (or (syntax word) "-")))
																						"  ")
																				.snippet.title))
			(match-string 1 .snippet.title))
		 ;; renamed, match the description instead
		 ((and .snippet.description
					 (string-match (rx (literal emacsconf-base-url) (literal emacsconf-year) "/talks/"
														 (group (1+ (or (syntax word) "-"))))
												 .snippet.description))
			(match-string 1 .snippet.description))
		 (t
			(plist-get
			 (seq-find (lambda (o)
									 (or
										(string-match (regexp-quote (or .snippet.videoId
																										.snippet.resourceId.videoId))
																	(or (plist-get o :youtube-url) ""))
										(string-match (regexp-quote (or .snippet.videoId
																										.snippet.resourceId.videoId))
																	(or (plist-get o :qa-youtube-url) ""))))
								 (emacsconf-get-talk-info))
			 :slug)))))

(defun emacsconf-extract-youtube-get-talk-for-video (video-object)
	(when-let ((slug (emacsconf-extract-youtube-get-slug-for-video video-object)))
		(emacsconf-resolve-talk slug)))

(defun emacsconf-extract-youtube-api-videos ()
	(plz 'get (concat "https://youtube.googleapis.com/youtube/v3/playlistItems?part=snippet,contentDetails,status&forMine=true&order=date&maxResults=50&playlistId="
										(url-hexify-string
										 (let-alist (elt (assoc-default 'items emacsconf-extract-youtube-api-channels) 0)
											 .contentDetails.relatedPlaylists.uploads)
										 ))
		:headers `(("Authorization" . ,(url-oauth-auth "https://youtube.googleapis.com/youtube/v3/")))
		:as #'json-read))

(defun emacsconf-extract-youtube-api-paginated-request (url &optional num-pages condition)
	(let (result current-page (base-url url) current-page-items)
		(while url
			(setq current-page
						(plz 'get url
							:headers
							`(("Authorization" . ,(url-oauth-auth "https://youtube.googleapis.com/youtube/v3/")))
							:as #'json-read))
			(setq current-page-items
						(if condition
								(seq-filter condition (assoc-default 'items current-page))
							(assoc-default 'items current-page)))
			(setq result (append result current-page-items nil))
			(let-alist current-page
				(setq url
							(if .nextPageToken
									(concat base-url
													(if (string-match "\\?" base-url) "&" "?")
													"pageToken=" .nextPageToken)
								nil)))
			(when (= (length current-page-items) 0) (setq url nil))
			(when num-pages
				(setq num-pages (1- num-pages))
				(if (<= num-pages 0) (setq url null))))
			result))


(defun emacsconf-extract-youtube-api-update-video (video-object &optional qa)
	"Update VIDEO-OBJECT.
If QA is non-nil, treat it as a Q&A video."
	(let-alist video-object
		(let* ((slug (or
									(emacsconf-extract-youtube-get-slug-for-video video-object)
									(when (string-match (rx (literal emacsconf-id) " " (literal emacsconf-year))
																			.snippet.title)
										(completing-read (format "Slug for %s: "
																						 .snippet.title)
																		 (seq-map (lambda (o) (plist-get o :slug))
																							(emacsconf-publish-prepare-for-display (emacsconf-get-talk-info)))))))
					 (talk (and slug (emacsconf-resolve-talk slug)))
					 (video-id .snippet.resourceId.videoId)
					 (id .id)
					 result)
			(when slug
				;; set the YOUTUBE_URL property
				(emacsconf-with-talk-heading slug
					(org-entry-put (point) (if qa "QA_YOUTUBE_URL" "YOUTUBE_URL") (concat "https://www.youtube.com/watch?v=" video-id))
					(org-entry-put (point) (if qa "QA_YOUTUBE_ID" "YOUTUBE_ID") id))
				(plz 'put "https://www.googleapis.com/youtube/v3/videos?part=snippet,recordingDetails,status"
					:headers `(("Authorization" . ,(url-oauth-auth "https://youtube.googleapis.com/youtube/v3/"))
										 ("Accept" . "application/json")
										 ("Content-Type" . "application/json"))
					:body (json-encode (emacsconf-extract-youtube-object video-id talk nil qa)))))))

(defun emacsconf-extract-youtube-rename-videos (&optional videos)
	"Rename videos and set the YOUTUBE_URL property in the Org heading."
	(interactive)
	(let ((info (emacsconf-get-talk-info)))
		(mapc
		 (lambda (video)
			 (let-alist video
				 (when (string-match (rx (literal emacsconf-id) " " (literal emacsconf-year) " ")
														 .snippet.title)
					 (emacsconf-extract-youtube-api-update-video video))))
		 (assoc-default
			'items
			(or videos emacsconf-extract-youtube-api-videos
					(emacsconf-extract-youtube-api-videos))))))

(defun emacsconf-extract-youtube-rename-draft-videos-as-qa (&optional videos)
	"Rename videos and set the QA_YOUTUBE_URL property in the Org heading."
	(interactive)
	(let ((info (emacsconf-get-talk-info)))
		(mapc
		 (lambda (video)
			 (let-alist video
				 (when (and
								(string= .status.privacyStatus "private")
								(string-match (rx (literal emacsconf-id) " " (literal emacsconf-year) " ")
															.snippet.title))
					 (emacsconf-extract-youtube-api-update-video video t))))
		 (assoc-default
			'items
			(or videos emacsconf-extract-youtube-api-videos
					(emacsconf-extract-youtube-api-videos))))))

;; This still needed some tweaking, so maybe next time we'll try just inserting the items into the playlist
(defvar emacsconf-extract-youtube-api-playlist nil)
(defvar emacsconf-extract-youtube-api-playlist-items nil)
(defun emacsconf-extract-youtube-api-sort-playlist (&optional dry-run-only)
	"Try to roughly sort the playlist."
	(interactive)
	(setq emacsconf-extract-youtube-api-playlist (seq-find (lambda (o) (let-alist o (string= .snippet.title (concat emacsconf-name " " emacsconf-year))))
																				(assoc-default 'items emacsconf-extract-youtube-api-playlists)))
	(setq emacsconf-extract-youtube-api-playlist-items
				(emacsconf-extract-youtube-api-paginated-request (concat "https://youtube.googleapis.com/youtube/v3/playlistItems?part=snippet,contentDetails,status&forMine=true&order=date&maxResults=100&playlistId="
																								(url-hexify-string (assoc-default 'id emacsconf-extract-youtube-api-playlist)))))
	(let* ((playlist-info emacsconf-extract-youtube-api-playlists)
				 (playlist-items emacsconf-extract-youtube-api-playlist-items)
				 (info (emacsconf-publish-prepare-for-display (emacsconf-get-talk-info)))
				 (slugs (seq-map (lambda (o) (plist-get o :slug)) info))
				 (position (1- (length playlist-items)))
				 result)
		;; sort items
		(mapc (lambda (talk)
						(when (plist-get talk :qa-youtube-id)
							;; move the q & a
							(let ((video-object (emacsconf-extract-youtube-find-url-video-in-list
																	 (plist-get talk :qa-youtube-url)
																	 playlist-items)))
								(let-alist video-object
									(cond
									 ((null video-object)
										(message "Could not find video for %s" (plist-get talk :slug)))
									 ;; not in the right position, try to move it
									 ((< .snippet.position position)
										(let ((video-id .id)
													(playlist-id .snippet.playlistId)
													(resource-id .snippet.resourceId))
											(message "Trying to move %s Q&A to %d from %d" (plist-get talk :slug) position .snippet.position)
											(add-to-list 'result (list (plist-get talk :slug) "answers" .snippet.position position))
											(unless dry-run-only
												(plz 'put "https://www.googleapis.com/youtube/v3/playlistItems?part=snippet"
													:headers `(("Authorization" . ,(url-oauth-auth "https://youtube.googleapis.com/youtube/v3/"))
																		 ("Accept" . "application/json")
																		 ("Content-Type" . "application/json"))
													:body (json-encode
																 `((id . ,video-id)
																	 (snippet
																		(playlistId . ,playlist-id)
																		(resourceId . ,resource-id)
																		(position . ,position))))))))))
								(setq position (1- position))))
						;; move the talk if needed
						(let ((video-object
									 (emacsconf-extract-youtube-find-url-video-in-list
										(plist-get talk :youtube-url)
										playlist-items)))
							(let-alist video-object
								(cond
								 ((null video-object)
									(message "Could not find video for %s" (plist-get talk :slug)))
								 ;; not in the right position, try to move it
								 ((< .snippet.position position)
									(let ((video-id .id)
												(playlist-id .snippet.playlistId)
												(resource-id .snippet.resourceId))
										(message "Trying to move %s to %d from %d" (plist-get talk :slug) position .snippet.position)
										(add-to-list 'result (list (plist-get talk :slug) "main" .snippet.position position))
										(unless dry-run-only
											(plz 'put "https://www.googleapis.com/youtube/v3/playlistItems?part=snippet"
												:headers `(("Authorization" . ,(url-oauth-auth "https://youtube.googleapis.com/youtube/v3/"))
																	 ("Accept" . "application/json")
																	 ("Content-Type" . "application/json"))
												:body (json-encode
															 `((id . ,video-id)
																 (snippet
																	(playlistId . ,playlist-id)
																	(resourceId . ,resource-id)
																	(position . ,position))))))
										))))
							(setq position (1- position))))
					(nreverse info))
		result))

(defun emacsconf-extract-youtube-get-video-details (&optional videos)
	(let (result url)
		(dolist (partition (seq-partition (or videos emacsconf-extract-youtube-api-videos) 50) result)
			(setq url
 						(format "https://www.googleapis.com/youtube/v3/videos?id=%s&part=snippet,contentDetails,statistics"
										(mapconcat (lambda (item) (url-hexify-string (let-alist item .contentDetails.videoId))) partition ",")))
			(message "%s" url)
			(setq result
						(append result
										(assoc-default
										 'items

										 (plz 'get
											 url
											 :headers `(("Authorization" . ,(url-oauth-auth "https://youtube.googleapis.com/youtube/v3/"))
																	("Accept" . "application/json")
																	("Content-Type" . "application/json"))
											 :as #'json-read))
										nil)))))



(defun emacsconf-extract-youtube-duration-msecs (video)
	(let-alist video
		(when-let ((duration .contentDetails.duration))
			(when (string-match "PT\\(?:\\([0-9]+\\)H\\)?\\([0-9]+\\)M\\([0-9]+\\)" duration)
				(+ (* (string-to-number (or (match-string 1 duration) "0")) 60 60 1000)
					 (* (string-to-number (or (match-string 2 duration) "0")) 60 1000)
					 (* (string-to-number (or (match-string 3 duration) "0")) 1000))))))

(defun emacsconf-extract-youtube-format-duration (s)
	"Converts a string of the form PT1H9M22S to 1:09:22."
	(when (and s (string-match "PT\\(?:\\([0-9]+\\)H\\)?\\([0-9]+\\)M\\([0-9]+\\)" s))
		(concat
		 (emacsconf-surround "" (match-string 1 s) ":")
		 (if (match-string 1 s)
				 (format "%02d:" (string-to-number (match-string 2 s)))
			 (concat (match-string 2 s) ":"))
		 (format "%02d" (string-to-number (match-string 3 s))))))

(defun emacsconf-extract-youtube-find-url-video-in-list (url &optional videos)
	(seq-find
	 (lambda (o)
		 (let-alist o
			 (or (string-match .id url)
					 (and .contentDetails.videoId (string-match .contentDetails.videoId url)))))
	 (or videos emacsconf-extract-youtube-api-videos)))

(ert-deftest emacsconf-extract-youtube-format-duration ()
	(expect (emacsconf-extract-youtube-format-duration "PT1H9M22S") :to-equal "1:09:22")
	(expect (emacsconf-extract-youtube-format-duration "PT9M22S") :to-equal "9:22"))

(defun emacsconf-extract-youtube-publish-video-drafts-with-spookfox ()
	"Look for drafts and publish them."
	(while (not (eq (spookfox-js-injection-eval-in-active-tab
									 "document.querySelector('.edit-draft-button div') != null" t) :false))
		(progn
			(spookfox-js-injection-eval-in-active-tab
			 "document.querySelector('.edit-draft-button div').click()" t)
			(sleep-for 2)
			(spookfox-js-injection-eval-in-active-tab
			 "document.querySelector('#step-title-3').click()" t)
			(when (spookfox-js-injection-eval-in-active-tab
						 "document.querySelector('tp-yt-paper-radio-button[name=\"PUBLIC\"] #radioLabel').click()" t)
				(spookfox-js-injection-eval-in-active-tab
				 "document.querySelector('#done-button').click()" t)
				(while (not (eq  (spookfox-js-injection-eval-in-active-tab
													"document.querySelector('#close-button .label') == null" t)
												 :false))
					(sleep-for 1))

				(spookfox-js-injection-eval-in-active-tab
				 "document.querySelector('#close-button .label').click()" t)
				(sleep-for 1)))))

(defun emacsconf-extract-youtube-store-url (&optional prefix)
	(interactive "p")
	(let* ((desc (spookfox-js-injection-eval-in-active-tab
							 "document.querySelector('#description').innerHTML" t))
				 (slug (if (and desc
												(string-match (rx (literal emacsconf-base-url) (literal emacsconf-year) "/talks/"
																		(group (1+ (not (or " " "/" "\"")))))
																desc))
									(match-string 1 desc)
								(emacsconf-complete-slug)))
				(url (spookfox-js-injection-eval-in-active-tab "window.location.href" t))
				(qa (or (> (or prefix 1)
									 1)
								(string-match "Q&A" (or desc ""))))
				(field (if qa
									 "QA_YOUTUBE_URL"
								 "YOUTUBE_URL")))
		(save-window-excursion
					(emacsconf-with-talk-heading slug
						(org-entry-put (point)
													 field
													 url)
						(message "Updating %s %s %s"
										 slug
										 field
										 url)))))

;; (setq emacsconf-extract-youtube-api-video-details (emacsconf-extract-youtube-get-video-details emacsconf-extract-youtube-api-playlist-items))

;;; PeerTube

(defun emacsconf-extract-youtube-spookfox-add-playlist-numbers ()
	"Number the playlist for easier checking.
Related: `emacsconf-extract-check-playlists'."
	(interactive)
	(spookfox-js-injection-eval-in-active-tab "[...document.querySelectorAll('ytd-playlist-video-renderer')].forEach((o, i) => { o.querySelector('.number')?.remove(); let div = document.createElement('div'); div.classList.add('number'); div.textContent = i; o.prepend(div) }))" t))

(defun emacsconf-extract-check-playlists ()
	"Return a table for checking playlist order."
	(let ((pos 0))
		(seq-mapcat (lambda (o)
									(delq
									 nil
									 (list
										(when (emacsconf-talk-file o "--main.webm")
											(cl-incf pos)
											(list pos
														(plist-get o :title)
														(org-link-make-string
														 (plist-get o :youtube-url)
														 "YouTube")
														(org-link-make-string
														 (plist-get o :toobnix-url)
														 "Toobnix")))
										(when (emacsconf-talk-file o "--answers.webm")
											(cl-incf pos)
											(list pos (concat "Q&A: " (plist-get o :title))
														(org-link-make-string
														 (plist-get o :qa-youtube-url)
														 "YouTube")
														(org-link-make-string
														 (plist-get o :qa-toobnix-url)
														 "Toobnix"))))))
								(emacsconf-publish-prepare-for-display (emacsconf-get-talk-info)))))

(defun emacsconf-extract-update-task-status-after-copying-logs ()
	"Mark non-BBB sessions as all done."
	(interactive)
	(mapc
	 (lambda (o)
		 (when (and (member (plist-get o :status) '("TO_ARCHIVE" "TO_EXTRACT"))
								(emacsconf-talk-file o "--main.vtt")
								(emacsconf-captions-edited-p (emacsconf-talk-file o "--main.vtt"))
								(null (plist-get o :bbb-rec)))
			 (emacsconf-with-talk-heading (plist-get o :slug)
				 (org-todo "DONE"))))
	 (emacsconf-publish-prepare-for-display (emacsconf-get-talk-info))))

(defun emacsconf-extract-store-url (&optional qa)
	"Store the URL for the currently-displayed field.
Call with a prefix arg to store the URL as Q&A."
	(interactive (list current-prefix-arg))
	(let* ((url (spookfox-js-injection-eval-in-active-tab "window.location.href" t))
				 (platform (if (string-match "toobnix" url)
											 'toobnix
										 'youtube))
				 (desc (spookfox-js-injection-eval-in-active-tab
								(format "document.querySelector('%s').innerHTML"
												(if (eq platform 'toobnix)
														".video-info-description"
													"#description"))
								t))
				 (slug (if (and desc
												(string-match (rx (literal emacsconf-base-url) (literal emacsconf-year) "/talks/"
																					(group (1+ (not (or " " "/" "\"")))))
																			desc))
									 (match-string 1 desc)
								 (emacsconf-complete-slug)))
				 (qa (or qa (string-match "Q&A" (or desc ""))))
				 (field
					(concat
					 (if qa "QA_" "")
					 (if (eq platform 'toobnix) "TOOBNIX" "YOUTUBE")
					 "_URL"
					 )))
		(save-window-excursion
			(emacsconf-with-talk-heading slug
				(org-entry-put (point)
											 field
											 url)
				(message "Updating %s %s %s"
								 slug
								 field
								 url)))))


(provide 'emacsconf-extract)
;;; emacsconf-extract.el ends here
