;;; emacsconf-publish.el --- Publishing  -*- lexical-binding: t; -*-

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

;;

;;; Code:

(require 'emacsconf-schedule)

(defcustom emacsconf-media-base-url "https://media.emacsconf.org/" "Base URL for published media files."
  :type 'string
  :group 'emacsconf)

(defcustom emacsconf-main-extensions '("--main.webm" "--main.opus" "--main.org" ".org" ".odp" ".pdf" ".pptx" ".el" "--compressed56.webm" "--main.vtt" "--main_fr.vtt" "--main_ja.vtt" "--main_es.vtt" "--main--chapters.vtt" "--script.fountain" "--main.pdf" "--slides.pdf")
  "Extensions to list on public pages."
  :type '(repeat string)
  :group 'emacsconf)

(defcustom emacsconf-publish-backstage-extensions '(".en.srv2" ".srt" "--original.mp4")
  "Extensions to list in the staging area."
  :group 'emacsconf)
(defcustom emacsconf-public-media-directory (concat "/ssh:orga@media.emacsconf.org:/var/www/media.emacsconf.org/" emacsconf-year "/")
  "Can be over TRAMP" :type 'string :group 'emacsconf)

(defun emacsconf-publish-info-pages-for-talk (talk)
"Publish the before and after pages for this talk."
  (interactive (list (emacsconf-complete-talk-info)))
  (let ((info (emacsconf-get-talk-info)))
    (emacsconf-publish-before-page talk info)
    (emacsconf-publish-after-page talk info)
		(unless (emacsconf-publish-talk-p talk)
			(emacsconf-publish-cancelled-nav-page talk))))

(defun emacsconf-publish-update-talk (talk)
  "Publish the schedule page and the page for this talk."
  (interactive (list (emacsconf-complete-talk-info)))
	(when (stringp talk) (setq talk (emacsconf-resolve-talk talk)))
  (when (functionp 'emacsconf-upcoming-insert-or-update)
    (emacsconf-upcoming-insert-or-update))
	(emacsconf-publish-with-wiki-change
		(let ((info (emacsconf-publish-prepare-for-display (emacsconf-get-talk-info))))
			(emacsconf-publish-before-page talk info)
			(emacsconf-publish-after-page talk info)
			(emacsconf-publish-schedule info))))

(defun emacsconf-publish-add-talk ()
  "Add the current talk to the wiki."
  (interactive)
  (emacsconf-publish-talk-page (emacsconf-get-talk-info-for-subtree))
  (emacsconf-publish-info-pages)
	(emacsconf-publish-schedule)
  (magit-status-setup-buffer emacsconf-directory))

(defun emacsconf-publish-update-conf-html ()
  "Update the schedules and export the page so I can easily review it."
  (interactive)
  (cl-letf* ((new-org (>= (string-to-number (org-version)) 9.5))
             ;; Fix bug probably introduced by org 9.5, but not investigated
             ;; thoroughly.
             ((symbol-function 'org-src-mode--maybe-disable-indent-tabs-mode)
              (eval `(lambda ()
                       (when (or ,(when new-org
                                    '(not org-src--tab-width))
                                 (= org-src--tab-width 0))
                         (setq indent-tabs-mode nil))))))
    (let ((org-confirm-babel-evaluate (or (null emacsconf-allow-dangerous-stuff)
                                          org-confirm-babel-evaluate)))
      (org-update-all-dblocks)
      (org-babel-execute-buffer)
      (org-html-export-to-html))))

(defun emacsconf-publish-regenerate-wiki (&optional force)
  (interactive)
  (when
    (let ((info (emacsconf-get-talk-info))
          (force (or force (yes-or-no-p "Overwrite existing talk pages? "))))
      (emacsconf-publish-info-pages info)
      (emacsconf-publish-schedule info)
      (magit-status emacsconf-directory))))

(declare-function 'emacsconf-ical-generate-all "emacsconf-ical")
(defun emacsconf-update-schedule ()
  "Change times for talks."
  (interactive)
  (emacsconf-publish-with-wiki-change
    (emacsconf-publish-info-pages)
    (emacsconf-publish-schedule)
    (emacsconf-ical-generate-all)
    (emacsconf-publish-schedule-org-files)
    (emacsconf-publish-watch-pages)
		(emacsconf-pad-prepopulate-all-talks)
		(emacsconf-pad-prepopulate-hyperlists)
    (when (functionp 'emacsconf-pentabarf-generate)
      (emacsconf-pentabarf-generate))))

(defun emacsconf-update-and-publish ()
  (interactive)
  (with-current-buffer (find-file-noselect emacsconf-org-file)
    (emacsconf-update-schedules)
    (emacsconf-upcoming-update-file)
    (emacsconf-update-schedules-in-wiki)
    (emacsconf-publish-update-conf-html)
    (setq emacsconf-info (emacsconf-get-talk-info))))

(defun emacsconf-publish-update-media ()
	"Update public media.emacsconf.org directory."
  (interactive)
  (emacsconf-publish-public-index-on-wiki)
  (when emacsconf-public-media-directory
    (emacsconf-publish-public-index (expand-file-name "index.html" emacsconf-public-media-directory))
		(emacsconf-publish-playlist (expand-file-name "index.m3u" emacsconf-public-media-directory)
               (concat emacsconf-name emacsconf-year)
							 (emacsconf-publish-prepare-for-display (emacsconf-get-talk-info))
               (format "https://media.emacsconf.org/%s/" emacsconf-year)))
  (when emacsconf-backstage-dir
    (emacsconf-publish-backstage-index (expand-file-name "index.html" emacsconf-backstage-dir)))
  (emacsconf-publish-playlist (expand-file-name "index.m3u" emacsconf-backstage-dir)
             (concat emacsconf-name emacsconf-year)
						 (emacsconf-publish-prepare-for-display (emacsconf-get-talk-info))
             (format "https://media.emacsconf.org/%s/backstage/" emacsconf-year)))

(defun emacsconf-publish-index-card (talk)
  "Format an HTML card for TALK."
  (let* ((file-prefix (plist-get talk :file-prefix))
         (video-file (plist-get talk :video-file))
         (video (and file-prefix
                     (emacsconf-publish-index-card-video
                      (or (plist-get talk :video-id)
                          (concat (plist-get talk :slug) "-mainVideo"))
                      video-file talk))))
    ;; Add extra information to the talk
    (setq talk
          (append
           talk
           (list
						:time-info (emacsconf-surround "Duration: " (plist-get talk :video-duration) " minutes" "")
            :video-html (or (plist-get video :video) "")
            :audio-html (or (plist-get video :audio) "")
						:chapter-list (or (plist-get video :chapter-list) "")
            :resources (or (plist-get video :resources) "")
            :extra (or (plist-get talk :extra) "")
            :speaker-info (or (plist-get talk :speakers-with-pronouns) ""))))
    (emacsconf-replace-plist-in-string
     talk
     "<div class=\"vid\">${video-html}${audio-html}<div>${extra}</div>${time-info}${resources}${chapter-list}</div>")))

;; (emacsconf-publish-format-track-as-org (car emacsconf-tracks) "US/Eastern")
;; (emacsconf-get-talk-info)
(defun emacsconf-publish-format-track-as-org (track tz &optional info)
	(let ((emacsconf-talk-info-functions (append emacsconf-talk-info-functions (list 'emacsconf-get-abstract-from-wiki))))
		(setq info (or info (emacsconf-publish-prepare-for-display (emacsconf-get-talk-info)))))
  (concat
   "** " (plist-get track :name) "  :" (plist-get track :id) ":\n:PROPERTIES:\n:CATEGORY: " (plist-get track :id) "\n:END:\n"
   (mapconcat
    (lambda (talk)
      (concat
       "*** " (plist-get talk :title) "\n"
       "<"
			 (format-time-string
        (cdr org-time-stamp-formats)
        (plist-get talk :start-time)
        tz)
			 ">--<"
			 (format-time-string
        (cdr org-time-stamp-formats)
        (plist-get talk :end-time)
        tz)
			 ">\n"
       (emacsconf-surround "- " (plist-get talk :speakers-with-pronouns) "\n" "")
       (emacsconf-surround "- " (plist-get talk :absolute-url) "\n" "")
			 "- Watch live: "
			 (mapconcat (lambda (player)
										(org-link-make-string
										 (concat "shell:" player " " (plist-get track :stream) " &")
										 player))
									'("mpv" "vlc" "ffplay")
									" or ")
			 " or "
			 (org-link-make-string
				(plist-get track :watch)
				"web-based player")
			 "\n"
       (emacsconf-surround "- Etherpad: " (plist-get talk :pad-url) "\n" "")
       (emacsconf-surround "- Chat: "
													 (org-link-make-string
														(plist-get talk :webchat-url)
														(concat "#" (plist-get talk :channel)))
													 "\n" "")
       (emacsconf-surround "- Q&A: "
													 (if (plist-get talk :qa-url)
															 (org-make-link-string
																(plist-get talk :qa-url)
																(plist-get talk :qa-info))
														 (plist-get talk :qa-info))
													 "\n" "")
       (emacsconf-surround "\n" (plist-get talk :intro-note) "\n" "")
			 (emacsconf-surround "\nDescription:\n\n"
													 (when (plist-get talk :org-description)
														 (with-temp-buffer
															 (org-paste-subtree 3 (plist-get talk :org-description))
															 (buffer-string)))
													 "\n" "")))
    (emacsconf-filter-talks-by-track track info)
    "\n")))

(defun emacsconf-publish-schedule-org-for-timezone (timezone &optional info)
  (interactive (list (completing-read "Time zone: " emacsconf-timezones)))
  (let ((new-filename (expand-file-name
                       (concat "schedule-"
                               (replace-regexp-in-string
                                "[^-+a-z0-9]+" "-"
																(downcase
																 (emacsconf-schedule-rename-etc-timezone timezone)))
                               ".org")
                       (expand-file-name "schedules"
                                         emacsconf-public-media-directory))))
    (unless (file-directory-p (file-name-directory new-filename))
      (make-directory (file-name-directory new-filename)))
    (with-temp-file new-filename
      (insert
       "* " emacsconf-name " " emacsconf-year "\n\nTimes are in "
			 (emacsconf-schedule-rename-etc-timezone timezone) " time zone. You can find this file and other calendars at "
       emacsconf-media-base-url emacsconf-year "/schedules/ .\n\n"
       (mapconcat (lambda (track)
                    (emacsconf-publish-format-track-as-org track timezone info))
                  emacsconf-tracks
                  "\n")))))

(defun emacsconf-publish-schedule-org-files (&optional info)
  (interactive)
	(let ((emacsconf-talk-info-functions (append emacsconf-talk-info-functions (list 'emacsconf-get-abstract-from-wiki))))
		(setq info (or info (emacsconf-publish-prepare-for-display (emacsconf-get-talk-info)))))
  (mapc (lambda (tz) (emacsconf-publish-schedule-org-for-timezone tz info))
				(append
				 emacsconf-timezones
				 (cl-loop for offset from -12 upto 14
									collect
									(format "Etc/GMT%s%d" (if (< offset 0) "+" "-") (abs offset))))))

(defun emacsconf-publish-format-res-talks (info)
	"Format lines for the backstage index."
  (mapconcat
   (lambda (o)
     (concat
      (format "<tr id=\"%s\">" (plist-get o :slug))
      "<td>" (format-time-string "%-l:%M" (plist-get o :start-time) emacsconf-timezone) "</td>"
      "<td><strong>"
			(cond
			 ((not (emacsconf-talk-recorded-p o))
				(format-time-string "%-l:%M" (plist-get o :start-time) emacsconf-timezone))
			 ((string-match "live" (plist-get o :qa-type))
				(format-time-string "%-l:%M" (plist-get o :qa-time) emacsconf-timezone))
			 (t ""))
			"</strong></td>"
      "<td>" (format "<a href=\"%s%s/talks/%s\" target=\"_blank\" rel=\"noreferrer\">%s</a>"
										 emacsconf-base-url
										 emacsconf-year
										 (plist-get o :slug)
										 (plist-get o :slug))
			"</td>"
      "<td>" (if (emacsconf-talk-recorded-p o) (plist-get o :qa-type) "(live talk)") "</td>"
			(if (plist-get o :bbb-room)
					(format "<td><button class=\"copy\" data-copy=\"%s\" data-label=\"Copy mod code\">Copy mod code</button></td>"
									(plist-get o :bbb-mod-code))
        "<td></td>")
			(concat "<td>"
							(if (plist-get o :bbb-room)
									(format "<a href=\"%s\" target=\"_blank\" rel=\"noreferrer\">Join Q&A</a>" (plist-get o :bbb-room)
													""))
							"</td>")
      "<td>" (if (plist-get o :pad-url)
                 (format "<a href=\"%s\" target=\"_blank\" rel=\"noreferrer\">Pad</a>" (plist-get o :pad-url))
               "")
      "</td>"
      "<td>" (format "<a href=\"%s\" target=\"_blank\" rel=\"noreferrer\">Chat</a>" (plist-get o :webchat-url))
      ""
      "</td>"

      "<td>" (or (plist-get o :title) "") "</td>"
      "<td>" (or (plist-get o :speakers) "") (emacsconf-surround " (" (plist-get o :irc) ")" "") "</td>"
      "</tr>"))
   info
   "\n"))

(defun emacsconf-publish-backstage-talk-index ()
  "Publish BBB room URLs and pad links for volunteer convenience."
  (interactive)
  (let* ((emacsconf-publishing-phase 'conference)
         (info (mapcar (lambda (o)
                         (append (list
																	:url (concat "#" (plist-get o :slug)))
																 o))
                       (emacsconf-publish-prepare-for-display (emacsconf-get-talk-info)))))
    (mapc
     (lambda (track)
       (let*
           ((track-talks (seq-filter (lambda (o) (string= (plist-get o :track)
                                                          (plist-get track :name)))
                                     info))
            (result
             (concat
              "<html><head><meta charset=\"utf-8\" /><link rel=\"stylesheet\" href=\"style.css\"></head><body>
<div>"
							(let ((emacsconf-use-absolute-url t)
										(emacsconf-schedule-svg-modify-functions '(emacsconf-schedule-svg-color-by-status))
										(emacsconf-base-url ""))
								(with-temp-buffer
									(svg-print (emacsconf-schedule-svg 800 300 info))
									(buffer-string)))
              "</div><h1>"
              (plist-get track :name)
              "</h1><table>"
							(mapconcat
							 (lambda (day)
								 (format
									"<tr><th colspan=\"7\" style=\"text-align: left\">%s</th></tr>
<tr><th>Talk start</th><th>BBB start</th><th>Talk ID</th><th>Q&A type</th><th>Mod code</th><th>BBB</th><th>Pad</th><th>Chat</th><th>Title</th><th>Speakers</th></tr>
%s"
									(car day) (emacsconf-publish-format-res-talks (cdr day))))
							 (seq-group-by
								(lambda (o) (format-time-string "%A, %b %-e" (plist-get o :start-time)))
								track-talks)
							 "\n")
              "</table></body>"
							(with-temp-buffer
								(insert-file-contents (expand-file-name "include-in-index-footer.html" emacsconf-cache-dir))
								(buffer-string))
							"</html>")))
         (with-temp-file (expand-file-name (format "index-%s.html" (plist-get track :id)) emacsconf-backstage-dir)
           (insert result))))
     emacsconf-tracks)))

(defun emacsconf-publish-index-card-video (video-id video-file talk)
  (let* ((video-base (and (stringp video-file) (replace-regexp-in-string "reencoded\\|original" "main" (file-name-base video-file))))
         (chapter-info (and (stringp video-file)
                            (emacsconf-make-chapter-strings
														 (or (plist-get talk :chapter-file)
																 (expand-file-name
																	(concat (file-name-sans-extension video-base) "--chapters.vtt")
																	emacsconf-cache-dir))
                             (plist-get talk :track-base-url)
                             video-id)))
         (info
          (append
           (list
            :source-src
            (when (stringp video-file)
              (if (plist-get talk :public)
                  (format "%s%s/%s" emacsconf-media-base-url (plist-get talk :conf-year)
                          (file-name-nondirectory video-file))
                (file-name-nondirectory video-file)))
            :captions
						(or
						 (and (stringp video-file)
									(or (plist-get talk :captions-edited)
											(and
											 (plist-get talk :caption-file)
											 ;; Let's try always including the captions even if they're not edited
											 ;; (emacsconf-captions-edited-p
											 ;; 	(expand-file-name (plist-get talk :caption-file) emacsconf-cache-dir))
											 ))
									(let ((tracks
                         (emacsconf-video-subtitle-tracks
													(or (plist-get talk :caption-file)
															(emacsconf-talk-file talk "--main.vtt")
															(emacsconf-talk-file talk "--reencoded.vtt"))
													(or (plist-get talk :track-base-url)
															(plist-get talk :base-url))
													(plist-get talk :files))))
										(cond
                     ((zerop (length tracks)) "")
                     ((eq (plist-get talk :format) 'wiki) (format "captions=\"\"\"%s\"\"\"" tracks))
                     (t tracks))))
						 "")
            :chapter-track (or (plist-get chapter-info :track) "")
            :chapter-list
            (if chapter-info
                (if (eq (plist-get talk :format) 'wiki)
                    (format "[[!template id=\"chapters\" vidid=\"%s\" data=\"\"\"\n%s\n\"\"\"]]"
                            video-id
                            (plist-get chapter-info :md))
                  (plist-get chapter-info :html))
              "")
            :video-id video-id
            :video-file-size (if (and (stringp video-file) (file-exists-p video-file))
                                 (file-size-human-readable (file-attribute-size (file-attributes video-file))))
						:links
						(concat
						 (emacsconf-surround "<li><a href=\""
																 (unless (eq emacsconf-publishing-phase 'resources)
																	 (if (plist-get talk :backstage)
																			 (emacsconf-backstage-url (plist-get talk :pad-url))
																		 (plist-get talk :pad-url)))
																 "\">Open Etherpad</a></li>" "")
						 (emacsconf-surround "<li><a href=\""
																 (and (plist-get talk :backstage)
																			(plist-get talk :bbb-backstage))
																 "\">Open backstage BigBlueButton</a></li>" "")
						 (emacsconf-surround "<li><a href=\""
																 (and (member emacsconf-publishing-phase '(schedule conference))
																			(plist-get talk :qa-url))
																 "\">Open public Q&A</a></li>" "")
						 (emacsconf-surround "<li><a href=\""
																 (and (not (eq emacsconf-publishing-phase 'resources)) (plist-get talk :bbb-rec))
																 "\">Play recording from BigBlueButton</a></li>" ""))
            :other-files
            (mapconcat
             (lambda (s)
               (concat "<li>" s "</li>"))
             (emacsconf-publish-link-file-formats-as-list talk)
             "")
            :toobnix-info (if (plist-get talk :toobnix-url)
                              (format
                               "<li><a href=\"%s\">View on Toobnix</a></li>"
                               (plist-get talk :toobnix-url))
                            "")
            :transcript-link
            (if (plist-get talk :public)
                (format "[View transcript](%s#%s-transcript)  \n" (plist-get talk :absolute-url) video-id)
              ""))
           talk)))
    (list
     :video
     (emacsconf-replace-plist-in-string
      info
			(if (and (stringp video-file) (string-match "webm$" video-file))
					"<video controls preload=\"none\" id=\"${video-id}\"><source src=\"${source-src}\" />${captions}${chapter-track}<p><em>Your browser does not support the video tag. Please download the video instead.</em></p></video>${chapter-list}"
				(or (plist-get talk :video-note) "")))
		 :audio
		 (if (and (plist-get talk :audio-file) (plist-get talk :public))
				 (format "<div>Listen to just the audio:<br /><audio controls preload=\"none\" id=\"%s-audio\" src=\"%s\"></audio></div>"
								 video-id
								 (if (plist-get talk :public)
										 (format "%s%s/%s" emacsconf-media-base-url (plist-get talk :conf-year)
														 (file-name-nondirectory (plist-get talk :audio-file)))
									 (file-name-nondirectory (plist-get talk :audio-file))))
			 "")
     :resources
     (emacsconf-replace-plist-in-string
      info
      "<div class=\"files resources\"><ul>${links}${other-files}${toobnix-info}</ul></div>"))))

(defun emacsconf-publish-format-public-email (o &optional email)
  (format "[%s](mailto:%s?subject=%s)"
          (or email (plist-get o :public-email))
          (or email (plist-get o :public-email))
          (url-hexify-string (format "Comment for EmacsConf 2023 %s: %s" (plist-get o :slug) (plist-get o :title)))))

(defun emacsconf-publish-format-speaker-info (o)
  (let ((extra-info (mapconcat #'identity
                               (delq nil (list
                                          (unless (string= (plist-get o :pronunciation) "nil")
																						(emacsconf-surround "Pronunciation: " (plist-get o :pronunciation) ""))
                                          (when (plist-get o :irc) (format "IRC: %s" (plist-get o :irc)))
																					(plist-get o :public-contact)
                                          (when (plist-get o :public-email) (format "<mailto:%s>" (plist-get o :public-email)))))
                               ", ")))
    (concat (plist-get o :speakers-with-pronouns)
            (if (> (length extra-info) 0)
                (concat " - " extra-info)
              ""))))

(defun emacsconf-publish-talk-page (o &optional force)
  "Draft the talk page for O unless the page already exists or FORCE is non-nil."
  (interactive (list (emacsconf-get-talk-info-for-subtree)
                     (> (prefix-numeric-value current-prefix-arg) 1)))
  (let ((filename (expand-file-name (format "%s.md" (plist-get o :slug))
                                    (expand-file-name "talks" (expand-file-name emacsconf-year emacsconf-directory)))))
    (unless (file-directory-p (expand-file-name "talks" (expand-file-name emacsconf-year emacsconf-directory)))
      (mkdir (expand-file-name "talks" (expand-file-name emacsconf-year emacsconf-directory))))
    (when (or force (null (file-exists-p filename)))
      (with-temp-file filename
        (insert
         (emacsconf-replace-plist-in-string
          (emacsconf-convert-talk-abstract-to-markdown
           (append o (list
                      :speaker-info (emacsconf-publish-format-speaker-info o)
                      :meta "!meta"
                      :categories (if (plist-get o :categories)
                                      (mapconcat (lambda (o) (format "[[!taglink %s]]" o))
                                                 (plist-get o :categories)
                                                 " ")
                                    ""))))
          "[[${meta} title=\"${title}\"]]
[[${meta} copyright=\"Copyright &copy; ${year} ${speakers}\"]]
[[!inline pages=\"internal(${year}/info/${slug}-nav)\" raw=\"yes\"]]

<!-- Initially generated with emacsconf-publish-talk-page and then left alone for manual editing -->
<!-- You can manually edit this file to update the abstract, add links, etc. --->\n

# ${title}
${speaker-info}

[[!inline pages=\"internal(${year}/info/${slug}-before)\" raw=\"yes\"]]

${abstract-md}

[[!inline pages=\"internal(${year}/info/${slug}-after)\" raw=\"yes\"]]

[[!inline pages=\"internal(${year}/info/${slug}-nav)\" raw=\"yes\"]]

${categories}
"))))))

(defun emacsconf-publish-talk-p (talk)
	"Return non-nil if the talk is ready to be published.
												Talks that are pending review will not be published yet."
	(pcase (plist-get talk :status)
		('nil nil)
		("TODO" nil)
		("TO_REVIEW" nil)
		;; ("TO_ACCEPT" nil)
		("CANCELLED" nil)
		(_ t)))

(defun emacsconf-publish-talk-pages (emacsconf-info &optional force)
  "Populate year/talks/*.md files.
												These should include the nav and schedule files, which will be
												rewritten as needed.  After they are generated, they should be all
												right to manually edit to include things like additional
												resources."
	(interactive (list (emacsconf-get-talk-info) (> (prefix-numeric-value current-prefix-arg) 1)))
  (mapc (lambda (o) (emacsconf-publish-talk-page o force))
				(emacsconf-filter-talks emacsconf-info)))

(defun emacsconf-wiki-talk-resources (o)
  (setq o (append (list :format 'wiki
                        :base-url
                        (concat emacsconf-media-base-url (plist-get o :conf-year) "/")
                        :track-base-url
                        (format "/%s/captions/" (plist-get o :conf-year))
												:chapter-file (emacsconf-talk-file o "--main--chapters.vtt"))
                  o))
  (concat
   (if (plist-get o :qa-public) "# Talk\n\n" "")
   (emacsconf-publish-index-card
		(append o
						(list
						 :caption-file (emacsconf-talk-file o "--main.vtt")
						 :files (seq-remove (lambda (f) (string-match "--answers" f))
																(emacsconf-publish-filter-public-files o)))))
   (if (plist-get o :qa-public)
       (concat "\n\n# Q&A\n\n"
               (emacsconf-publish-index-card (append
                             (list
                              :public 1
                              :video-id (concat (plist-get o :slug) "-qanda")
                              :toobnix-url nil
															:captions-edited (plist-get o :qa-captions-edited)
															:caption-file (emacsconf-talk-file o "--answers.vtt")
                              :video-file (emacsconf-talk-file o "--answers.webm")
															:video-duration (plist-get o :qa-video-duration)
															:audio-file (emacsconf-talk-file o "--answers.opus")
															:chapter-file (emacsconf-talk-file o "--answers--chapters.vtt")
															:files (emacsconf-publish-filter-public-files o "answers"))
                             o)))
     "")))

(defun emacsconf-publish-webchat-link (o)
  (let ((track (emacsconf-get-track (plist-get o :track))))
    (format "<a href=\"%s\">#%s</a>"
						(plist-get track :webchat-url)
						(plist-get track :channel))))

(defvar emacsconf-publish-include-pads nil "When non-nil, include Etherpad info.")

(defun emacsconf-publish-format-talk-schedule-info (o)
	"Format schedule information for O."
  (let ((friendly (concat "/" emacsconf-year "/talks/" (plist-get o :slug) ))
        (timestamp (org-timestamp-from-string (plist-get o :scheduled)))
				(talk-p (emacsconf-publish-talk-p o)))
    (emacsconf-replace-plist-in-string
     (append o
             (list
							:format
							(if talk-p
								(concat (or (plist-get o :video-time)
														(plist-get o :time))
												"-min talk			; Q&A: "
												(pcase (plist-get o :qa-type)
													("none" "ask questions via Etherpad/IRC; we'll e-mail the speaker and post answers on this wiki page after the conference")
													("live" "BigBlueButton conference room")
													("pad" "Etherpad")
													("irc" "IRC")
													(_ (plist-get o :qa-type)))
												(emacsconf-surround " <" (and (member emacsconf-publishing-phase '(schedule conference))
																											(plist-get o :qa-url)) ">" ""))
							 (concat (or (plist-get o :video-time)
													 (plist-get o :time)) "-min talk cancelled"))
							:pad-info
							(if (and talk-p emacsconf-publish-include-pads (not (and (member emacsconf-publishing-phase '(schedule conference))
																																			 (string= (plist-get o :qa-type) "etherpad"))))
									(format "Etherpad: <https://pad.emacsconf.org/%s-%s>  \n" emacsconf-year (plist-get o :slug))
								"")
							:irc-info
							(if (member emacsconf-publishing-phase '(schedule conference))
									(format "Discuss on IRC: [#%s](%s)  \n" (plist-get o :channel)
													(plist-get o :webchat-url))
								"")
							:status-info
							(format "Status: %s  \n" (plist-get o :status-label))
							:alternate-apac-info
							(if (plist-get o :alternate-apac)
									(format "[[!inline pages=\"internal(%s/inline-alternate)\" raw=\"yes\"]]  \n" emacsconf-year)
								"")
							:schedule-info
							(if (and (member emacsconf-publishing-phase '(schedule conference))
											 (not (emacsconf-talk-all-done-p o))
											 (not (string= (plist-get o :status) "CANCELLED")))
									(let* ((end
													(org-timestamp-to-time
													 (org-timestamp-split-range
														(org-timestamp-from-string (plist-get o :scheduled)) t)))
												 (start (org-timestamp-to-time
																 (org-timestamp-split-range
																	(org-timestamp-from-string (plist-get o :scheduled))))))
                    (format
                     "<div>Times in different time zones:</div><div class=\"times\" start=\"%s\" end=\"%s\"><div class=\"conf-time\">%s</div><div class=\"others\"><div>which is the same as:</div>%s</div></div><div><strong><a href=\"/%s/watch/%s/\">Find out how to watch and participate</a></strong></div>"
                     (format-time-string "%Y-%m-%dT%H:%M:%SZ" start t)
                     (format-time-string "%Y-%m-%dT%H:%M:%SZ" end t)
                     (emacsconf-timezone-string o emacsconf-timezone)
                     (string-join (emacsconf-timezone-strings
                                   o
                                   (seq-remove (lambda (zone) (string= emacsconf-timezone zone))
                                               emacsconf-timezones)) "<br />")
                     emacsconf-year
                     (plist-get (emacsconf-get-track (plist-get o :track)) :id)))
                "")))
     "[[!toc  ]]
Format: ${format}  \n${pad-info}${irc-info}${status-info}${schedule-info}\n
${alternate-apac-info}\n")))

(defun emacsconf-publish-format-email-questions-and-comments (talk)
	"Invite people to e-mail either the public contact for TALK or the private list."
	(format "Questions or comments? Please e-mail %s"
					(emacsconf-publish-format-public-email talk
																(or
																 (and (string= (plist-get talk :public-email) "t")
																			(plist-get talk :email))
																 (plist-get talk :public-email)
																 "emacsconf-org-private@gnu.org"))))

(defun emacsconf-publish-captions-in-wiki (talk)
  "Copy the captions file."
  (interactive (list (emacsconf-complete-talk-info)))
  (unless (file-directory-p (expand-file-name "captions" (expand-file-name emacsconf-year emacsconf-directory)))
    (make-directory (expand-file-name "captions" (expand-file-name emacsconf-year emacsconf-directory))))
	(let ((default-directory emacsconf-directory))
		(mapc
		 (lambda (ext)
			 (let ((filename (expand-file-name (concat (plist-get talk :file-prefix) ext)
																				 (expand-file-name "captions" (expand-file-name emacsconf-year emacsconf-directory))))
						 (cached-file (expand-file-name (concat (plist-get talk :file-prefix) ext) emacsconf-cache-dir)))
				 (when (and (file-exists-p cached-file)
										(or
										 (not (file-exists-p filename))
										 (file-newer-than-file-p cached-file filename)))
					 (copy-file cached-file filename t)
					 (shell-command (concat "git add " (shell-quote-argument filename))))))
		 (seq-filter (lambda (o) (string-match "vtt$" o))
								 emacsconf-main-extensions))))

(defun emacsconf-publish-format-talk-schedule-image (talk info)
	"Add the schedule image for TALK based on INFO."
	(concat
	 "\nThe following image shows where the talk is in the schedule for "
	 (format-time-string "%a %Y-%m-%d" (plist-get talk :start-time) emacsconf-timezone) ". Solid lines show talks with Q&A via BigBlueButton. Dashed lines show talks with Q&A via IRC or Etherpad."
	 (format "<div class=\"schedule-in-context schedule-svg-container\" data-slug=\"%s\">\n" (plist-get talk :slug))
	 (let* ((width 700) (height 150)
					(talk-date (format-time-string "%Y-%m-%d" (plist-get talk :start-time) emacsconf-timezone))
					(start (date-to-time (concat talk-date "T" emacsconf-schedule-start-time emacsconf-timezone-offset)))
					(end (date-to-time (concat talk-date "T" emacsconf-schedule-end-time emacsconf-timezone-offset)))
					svg)
		 (with-temp-buffer
			 (setq svg (emacsconf-schedule-svg-day
									(svg-create width height)
									(format-time-string "%A" (plist-get talk :start-time) emacsconf-timezone)
									width height
									start end
									(emacsconf-by-track
									 (seq-filter (lambda (o) (string= (format-time-string "%Y-%m-%d" (plist-get o :start-time) emacsconf-timezone)
																										talk-date))
															 info))))
			 (mapc (lambda (node)
							 (let ((rect (car (dom-by-tag node 'rect))))
								 (if (string= (dom-attr node 'data-slug) (plist-get talk :slug))
										 (progn
											 (dom-set-attribute rect 'opacity "0.8")
											 (dom-set-attribute rect 'stroke-width "3")
											 (dom-set-attribute (car (dom-by-tag node 'text)) 'font-weight "bold"))
									 (dom-set-attribute rect 'opacity "0.5"))))
						 (dom-by-tag svg 'a))
			 (svg-print svg)
			 (buffer-string)))
	 "\n</div>\n\n"))

(defun emacsconf-publish-before-page (talk &optional info)
  "Generate the page that has the info included before the abstract.
This includes the intro note, the schedule, and talk resources."
  (interactive (list (emacsconf-complete-talk-info)))
  (setq info (or info (emacsconf-publish-prepare-for-display (emacsconf-get-talk-info))))
  (with-temp-file (expand-file-name (format "%s-before.md" (plist-get talk :slug))
                                    (expand-file-name "info" (expand-file-name emacsconf-year emacsconf-directory)))

    (insert "<!-- Automatically generated by emacsconf-publish-before-page -->\n")
    (insert (emacsconf-surround "" (plist-get talk :intro-note) "\n\n" ""))
    (let ((is-live (emacsconf-talk-live-p talk)))
      (when is-live (emacsconf-publish-captions-in-wiki talk))
			(when (emacsconf-publish-talk-p talk)
				(when (member emacsconf-publishing-phase '(schedule conference))
					(insert (emacsconf-publish-format-talk-schedule-image talk info))))
			(insert (emacsconf-publish-format-talk-schedule-info talk) "\n\n")
			(insert
			 (if (plist-get talk :public) (emacsconf-wiki-talk-resources talk) "")
			 "\n# Description\n"))
    (insert "<!-- End of emacsconf-publish-before-page -->")))


(defun emacsconf-format-transcript-from-list (subtitles video-id &optional lang)
  "Return subtitle directives for SUBTITLES."
  (when (stringp subtitles) (setq subtitles (subed-parse-file subtitles)))
  (mapconcat
   (lambda (sub)
     (let ((msecs (elt sub 1)))
			 (concat
				(if (and (elt sub 4) (not (string= (elt sub 4) "")))
						(format "\n[[!template new=\"1\" text=\"\"\"%s\"\"\" start=\"%s\" video=\"%s\" id=\"subtitle\"%s]]\n\n"
										(string-trim (replace-regexp-in-string "^NOTE[ \n]" "" (elt sub 4)))
										(concat (format-seconds "%02h:%02m:%02s" (/ (floor msecs) 1000))
														"." (format "%03d" (mod (floor msecs) 1000)))
										video-id
										(emacsconf-surround " lang=\"" lang "\"" ""))
					"")
				(format
				 "[[!template text=\"\"\"%s\"\"\" start=\"%s\" video=\"%s\" id=\"subtitle\"%s]]"
				 (replace-regexp-in-string "^#" "\\\\#"
																	 (replace-regexp-in-string "\"" "&quot;" (elt sub 3)))
				 (concat (format-seconds "%02h:%02m:%02s" (/ (floor msecs) 1000))
								 "." (format "%03d" (mod (floor msecs) 1000)))
				 video-id
				 (emacsconf-surround " lang=\"" lang "\"" "")))))
   subtitles "\n"))

(defun emacsconf-publish-format-transcript (talk &optional video-id lang title)
  "Format the transcript for TALK, adding paragraph markers when possible."
	(require 'subed)
  (let* ((subtitles
          (subed-parse-file (if lang
																(format "%s_%s.vtt"
																				(file-name-sans-extension
																				 (plist-get talk :caption-file))
																				lang)
															(plist-get talk :caption-file)))))
    (if subtitles
        (format "<a name=\"%s-%s-transcript%s\"></a>
# %s%s

%s

"
								(plist-get talk :slug)
                (or video-id "mainVideo")
                (emacsconf-surround "-" lang "" "")
								(if lang (assoc-default lang emacsconf-publish-subtitle-languages) (or title "Transcript"))
								(if (emacsconf-captions-edited-p (plist-get talk :caption-file))
										""
									" (unedited)")
								(emacsconf-format-transcript-from-list
                 subtitles (concat video-id "-" (plist-get talk :slug))))
      "")))

(defun emacsconf-publish-format-captions (talk)
	(let ((transcripts
				 (mapconcat
					(lambda (lang)
						(let ((filename
									 (emacsconf-talk-file
										talk
										(if lang
												(format "--main_%s.vtt" lang)
											"--main.vtt"))))
							(if filename
									;; (and filename (emacsconf-captions-edited-p filename))
																				; todo: cache this somewhere
									(emacsconf-publish-format-transcript
									 (append
										(list :chapter-file (emacsconf-talk-file talk "--main--chapters.vtt")
													:caption-file (emacsconf-talk-file talk "--main.vtt"))
										talk)
									 "mainVideo" lang)
								"")))
					(cons nil (mapcar 'car emacsconf-publish-subtitle-languages))
					"")))
		(if (> (length transcripts) 0)
				(concat transcripts
								(emacsconf-surround
								 (if (string-match "[,;]" (or (plist-get talk :captioner) ""))
										 "\n\nCaptioners: "
									 "\n\nCaptioner: ")
								 (plist-get talk :captioner)
								 "\n\n"))
			"")))

(defun emacsconf-publish-after-page (talk &optional info)
  "Generate the page with info included after the abstract.
This includes captions, contact, and an invitation to participate."
  (interactive (list (emacsconf-complete-talk-info)))
  ;; Contact information
  (with-temp-file (expand-file-name (format "%s-after.md" (plist-get talk :slug))
                                    (expand-file-name "info" (expand-file-name emacsconf-year emacsconf-directory)))
    (insert
     "<!-- Automatically generated by emacsconf-publish-after-page -->\n"
     "\n\n"
		 ;; main transcript
     (if (plist-get talk :public) (emacsconf-publish-format-captions talk) "")
		 (if (emacsconf-talk-file talk "--answers.vtt")
				 (emacsconf-publish-format-transcript
					(append
					 (list :chapter-file (emacsconf-talk-file talk "--answers--chapters.vtt")
								 :caption-file (emacsconf-talk-file talk "--answers.vtt"))
					 talk)
					"qanda" nil "Q&A transcript")
			 "")
     (emacsconf-publish-format-email-questions-and-comments talk) "\n"
		 (if (eq emacsconf-publishing-phase 'cfp)
				 (format "\n----\nGot an idea for an EmacsConf talk or session? We'd love to hear from you! Check out the [[Call for Participation|/%s/cfp]] for details.\n" emacsconf-year)
			 "")
     "\n\n<!-- End of emacsconf-publish-after-page -->\n")))

(defun emacsconf-sort-by-track-then-schedule (a b)
  ;; Gen,Dev; then by time
  (cond
	 ((and (plist-get a :speakers)
				 (not (plist-get b :speakers))) t)
	 ((and (plist-get b :speakers)
				 (not (plist-get a :speakers))) nil)
   ((string< (plist-get a :track)
             (plist-get b :track)) nil)
   ((string< (plist-get b :track)
             (plist-get a :track)) t)
   ((time-less-p (plist-get a :start-time)
                 (plist-get b :start-time)) t)
   (t nil)))

(defun emacsconf-publish-cancelled-nav-page (talk)
	(with-temp-file (expand-file-name (format "%s-nav.md" (plist-get talk :slug))
                                    (expand-file-name "info" (expand-file-name emacsconf-year emacsconf-directory)))
          (insert "\n<div class=\"talk-nav\">
Back to the [[talks]]  \n</div>")))

(defun emacsconf-publish-nav-pages (&optional talks)
	"Generate links to the next and previous talks.
During the schedule and conference phase, the talks are sorted by time.
Otherwise, they're sorted by track and then schedule."
  (interactive (list (emacsconf-publish-prepare-for-display (or emacsconf-schedule-draft (emacsconf-get-talk-info)))))
  (let* ((next-talks (cdr talks))
         (prev-talks (cons nil talks))
         (label (if (member emacsconf-publishing-phase '(schedule conference))
                    "time"
                  "track")))
    (unless (file-directory-p (expand-file-name "info" (expand-file-name emacsconf-year emacsconf-directory)))
      (mkdir (expand-file-name "info" (expand-file-name emacsconf-year emacsconf-directory))))
    (while talks
      (let* ((o (pop talks))
             (next-talk (emacsconf-format-talk-link (pop next-talks)))
             (prev-talk (emacsconf-format-talk-link (pop prev-talks))))
        (with-temp-file (expand-file-name (format "%s-nav.md" (plist-get o :slug))
                                          (expand-file-name "info" (expand-file-name emacsconf-year emacsconf-directory)))
          (insert (concat "\n<div class=\"talk-nav\">
Back to the [[talks]]  \n"
                          (if prev-talk (format "Previous by %s: %s  \n" label prev-talk) "")
                          (if next-talk (format "Next by %s: %s  \n" label next-talk) "")
                          (if (plist-get o :track) ; tagging doesn't work here because ikiwiki will list the nav page
															(if (member emacsconf-publishing-phase '(schedule conference))
																	(format "Track: <span class=\"sched-track %s\">%s</span> - <strong><a href=\"%s\">Watch</a></strong>  \n"
																					(plist-get o :track)
																					(plist-get o :track)
																					(plist-get o :watch-url))
																(format "Track: <span class=\"sched-track %s\">%s</span>  \n"
																				(plist-get o :track)
																				(plist-get o :track)))
                            "")
                          "</div>
")))))))

(defun emacsconf-publish-prepare-for-display (&optional talks)
	"Return the sequence of TALKS to publish."
  (seq-filter 'emacsconf-publish-talk-p
              (sort (copy-sequence (or talks emacsconf-schedule-draft (emacsconf-filter-talks (emacsconf-get-talk-info))))
										(if (member emacsconf-publishing-phase '(schedule conference))
												#'emacsconf-sort-by-scheduled
											#'emacsconf-sort-by-track-then-schedule))))

(defun emacsconf-publish-info-pages (&optional info)
  "Populate year/info/*-nav, -before, and -after files."
  (interactive (list nil))
  (setq info (or info (emacsconf-publish-prepare-for-display info)))
  (emacsconf-publish-with-wiki-change
    (emacsconf-publish-nav-pages info)
		(emacsconf-publish-schedule info)
    (mapc (lambda (o)
            (emacsconf-publish-before-page o info)
            (emacsconf-publish-after-page o info))
          info)))

(defun emacsconf-publish-before-pages (&optional info)
  "Populate -before files."
  (interactive)
  (setq info (or info (emacsconf-get-talk-info)))
  (setq info (seq-remove (lambda (o) (string= (plist-get o :status) "CANCELLED"))
                         (sort (emacsconf-filter-talks info) #'emacsconf-sort-by-scheduled)))
  (emacsconf-publish-with-wiki-change
    (mapc (lambda (o)
            (emacsconf-publish-before-page o info))
          info)
		(mapc (lambda (o) (emacsconf-publish-before-page o info))
					(seq-filter (lambda (o) (string= (plist-get o :status) "CANCELLED"))
											(emacsconf-filter-talks info)))))

(defun emacsconf-generate-main-schedule-with-tracks (&optional info)
  (interactive (list nil))
  (setq info (or info (emacsconf-publish-prepare-for-display info)))
  (with-temp-file (expand-file-name "schedule-details.md"
                                    (expand-file-name emacsconf-year emacsconf-directory))
		(when (member emacsconf-publishing-phase '(schedule conference))
			(insert
			 (emacsconf-replace-plist-in-string
				(list
				 :timezone emacsconf-timezone
				 :gmt-offset emacsconf-timezone-offset
				 :alternative-timezones
				 (string-join (emacsconf-timezone-strings range nil "~%-l:%M %p")
											" / ")
				 :icals
				 (concat
					(format "<a href=\"%s%s%s.ics\">%s.ics</a> - "
									emacsconf-media-base-url
									emacsconf-year
									emacsconf-id)
					(mapconcat (lambda (track)
											 (format "<a href=\"%s%s%s-%s.ics\">%s-%s.ics</a>"
															 emacsconf-media-base-url
															 emacsconf-year
															 emacsconf-id
															 (plist-get track :id)))
										 emacsconf-tracks " - "))
				 :schedule-directory
				 (concat emacsconf-media-base-url emacsconf-year "/schedules/"))
				"The conference is from ${alternative-timezones}.

Times below are in %{timezone} (GMT${gmt-offset}). If you have Javascript enabled, clicking on talk pages should include times in your computer's local time setting.

You can also get this schedule as iCalendar files: ${icals}. Importing that into your calendar should translate things into your local time zone. Alternatively, you can use these time-zone-translated Org files: <${schedule-directory}>")))
		;; By track
    (let* ((by-day (mapcar (lambda (o))
													 (seq-group-by (lambda (o)
																					 (format-time-string "%Y-%m-%d" nil emacsconf-timezone))
																				 info)))
					 (links
						(mapconcat (lambda (track)
												 (concat (cadr track) ": "
																 (mapconcat (lambda (sec)
																							(format "<a href=\"#%s-%s\">%s</a>"
																											(car track) (car sec) (cadr sec)))
																						(mapcar (lambda (day)
																											(list (downcase (format-time-string "%a" (date-to-time (car day))))
																														(format-time-string "%A" (date-to-time (car day)))))
																										by-day)
																						" - ")))
											 (mapcar (lambda (o) (list (plist-get o :id)
																								 (plist-get o :name)))
															 emacsconf-tracks)
											 " | ")))
      (insert
			 (mapconcat
        (lambda (track)
          (let* ((id (elt track 0))
                 (label (elt track 1))
                 (start (elt track 2))
                 (end (elt track 3)))
            (format "\n\n<a name=\"%s\"></a>\n
<div class=\"%s\">
## %s\n\n%s\n</div>\n"
                    id
                    label
                    label
                    (mapconcat
                     (lambda (section)
                       (let ((section-id (elt section 0))
                             (section-label (elt section 1))
                             (section-seq (caddr section)))
                         (format "%s\n\n<a name=\"%s-%s\"></a>\n### %s track - %s\n%s\n"
                                 links
                                 id section-id
                                 label
                                 section-label
                                 (emacsconf-publish-format-main-schedule
                                  section-seq))))
                     `(("sat" "Saturday, Dec 3" ,sat)
                       ("sun" "Sunday, Dec 4" ,sun))
                     "\n\n")
                    ))
					()
					)
        '(("gen" "General" "^GEN Sat" "^DEV Sat")
          ("dev" "Development" "^DEV Sat"))
        "\n")))
    (let ((by-day (sort (seq-remove (lambda (s) (string-match "^\\(GEN\\|DEV\\)" (plist-get s :title)))
                                    info)
                        #'emacsconf-sort-by-scheduled)))
      (insert
       "\n<a name=\"by-day\"></a>\n\n# By day\n\n## Saturday\n"
       ;; Everything all together
       (emacsconf-publish-format-main-schedule
        (emacsconf-schedule-get-subsequence
         by-day
         "Saturday opening remarks"
         "Saturday closing remarks"))
       "\n\n## Sunday\n"
       ;; Everything all together
       (emacsconf-publish-format-main-schedule
        (emacsconf-schedule-get-subsequence
         by-day
         "Sunday opening remarks"
         "Sunday closing remarks"))
       ))))

(defun emacsconf-publish-format-interleaved-schedule (&optional info)
  "Return a list with the schedule for INFO.
Entries are sorted chronologically, with different tracks interleaved."
  (setq info (or info (emacsconf-get-talk-info)))
  (let* ((by-day (emacsconf-by-day (seq-remove (lambda (o)
																								 (or
																									(not (plist-get o :scheduled))
																									(member (plist-get o :status) '("TODO" "TO_REVIEW" "TO_ACCEPT"))))
																							 (emacsconf-publish-prepare-for-display info))))
         (cancelled (seq-filter (lambda (o) (string= (plist-get o :status) "CANCELLED")) (emacsconf-get-talk-info)))
         (dates (seq-map (lambda (o) (plist-get (cadr o) :start-time)) by-day))
         (links (mapcar (lambda (o)
                          (format "<a href=\"#date-%s\">%s</a>"
                                  (format-time-string "%Y-%m-%d" o emacsconf-timezone)
                                  (format-time-string "%a %b %-e" o emacsconf-timezone)))
                        dates))
         (height 150)
         (width 600))
    (concat
     (mapconcat (lambda (day)
                  (let ((day-start (date-to-time
					                          (concat (format-time-string "%Y-%m-%dT" (plist-get (cadr day) :start-time) emacsconf-timezone)
                                            emacsconf-schedule-start-time
						                                emacsconf-timezone-offset)))
			                  (day-end (date-to-time (concat (format-time-string "%Y-%m-%dT" (plist-get (cadr day) :start-time) emacsconf-timezone)
                                                       emacsconf-schedule-end-time
							                                         emacsconf-timezone-offset))))
                    (concat
                     (if (> (length links) 1) (concat "Jump to: " (string-join links " - ")) "")
                     (format "<a name=\"date-%s\"></a>\n"
                             (format-time-string "%Y-%m-%d"
                                                 (plist-get (cadr day) :start-time)
                                                 emacsconf-timezone))
                     (format-time-string "# %A %b %-e, %Y\n" (plist-get (cadr day) :start-time) emacsconf-timezone)
                     (format "[[!inline pages=\"internal(%s/schedule-%s)\" raw=\"yes\"]]" emacsconf-year (car day))
                     "\n\n"
                     (format "<div class=\"schedule\" data-start=\"%s\" data-end=\"%s\" data-tracks=\"General,Development\">\n"
                             (format-time-string "%FT%T%z" day-start t)
                             (format-time-string "%FT%T%z" day-end t))
                     (emacsconf-publish-format-main-schedule (cdr day))
                     "</div>")))
                by-day
                "\n\n")
     (if (> (length cancelled) 0)
         (format "<div class=\"cancelled\">Cancelled:<ul>%s</ul></div>"
                 (mapconcat (lambda (talk) (format "<li>%s - %s</li>"
                                                   (plist-get talk :title)
                                                   (plist-get talk :speakers)))
                            cancelled "\n"))
       "")
     )))

(defun emacsconf-publish-schedule-with-times (&optional info)
	(insert
	 (emacsconf-replace-plist-in-string
		(list
		 :timezone emacsconf-timezone
		 :year emacsconf-year
		 :gmt-offset emacsconf-timezone-offset
		 :alternative-timezones
		 (string-join (emacsconf-timezone-strings
									 (format "<%s %s-%s>"
													 emacsconf-date
													 (plist-get (car emacsconf-tracks) :start)
													 (plist-get (car emacsconf-tracks) :end))
									 nil "~%-l:%M %p")
									" / ")
		 :icals
		 (concat
			(format "<a href=\"%s%s/%s.ics\">%s.ics</a> - "
							emacsconf-media-base-url
							emacsconf-year
							emacsconf-id
							emacsconf-id)
			(mapconcat (lambda (track)
									 (format "<a href=\"%s%s/%s-%s.ics\">%s-%s.ics</a>"
													 emacsconf-media-base-url
													 emacsconf-year
													 emacsconf-id
													 (plist-get track :id)
													 emacsconf-id
													 (plist-get track :id)))
								 emacsconf-tracks " - "))
		 :schedule-directory
		 (concat emacsconf-media-base-url emacsconf-year "/schedules/"))
		"Times below are in ${timezone} (GMT${gmt-offset}). If you have Javascript enabled, clicking on talk pages should include times in your computer's local time setting.

[[!inline pages=\"internal(${year}/schedule-image)\" raw=\"yes\"]]

The conference is from ${alternative-timezones}.

You can also get this schedule as iCalendar files: ${icals}. Importing that into your calendar should translate things into your local time zone. Alternatively, you can use these time-zone-translated Org files: <${schedule-directory}>

")
	 (emacsconf-publish-format-interleaved-schedule info)))

(defun emacsconf-publish-program-without-times (&optional info)
	(insert
	 (let ((sorted (emacsconf-publish-prepare-for-display (or info (emacsconf-get-talk-info)))))
		 (mapconcat
			(lambda (track)
				(concat
				 "Jump to: "
				 ;; links to other tracks
				 (string-join (seq-keep (lambda (track-link)
																				(unless (string= (plist-get track-link :id)
																												 (plist-get track :id))
																					(format "<a href=\"#%s\">%s</a>"
																									(plist-get track-link :id)
																									(plist-get track-link :name))))
																			emacsconf-tracks)
														" | ")
				 "\n\n"
				 (let ((track-talks (seq-filter (lambda (o) (string= (plist-get o :track)
																																	 (plist-get track :name)))
																				sorted)))
					 (format
						"<h1 id=\"%s\" class=\"sched-track %s\">%s (%d talk%s)</h1>\n%s"
						(plist-get track :id)
						(plist-get track :name)
						(plist-get track :name)
						(length track-talks)
						(if (= (length track-talks) 1) "" "s")
						(emacsconf-publish-format-main-schedule track-talks)))))
			emacsconf-tracks "\n\n"))))

(defun emacsconf-publish-schedule (&optional info)
	"Generate the schedule or program."
  (interactive)
  (unless (eq emacsconf-publishing-phase 'cfp) (emacsconf-publish-schedule-svg-snippets))
	(setq info (or info (emacsconf-publish-prepare-for-display info)))
  (pcase emacsconf-publishing-phase
		((or 'schedule 'conference)
		 (with-temp-file (expand-file-name "schedule-details.md"
																			 (expand-file-name emacsconf-year emacsconf-directory))
			 (emacsconf-publish-schedule-with-times info)))
		((or 'cfp 'program)
		 (with-temp-file (expand-file-name "schedule-details.md"
																			 (expand-file-name emacsconf-year emacsconf-directory))
			 (emacsconf-publish-program-without-times info))
		 (with-temp-file (expand-file-name
										 "draft-schedule.md"
										 (expand-file-name emacsconf-year emacsconf-directory))
      (insert
			 "[[!sidebar content=\"\"]]\n\n"
       "This is a *DRAFT* schedule.\n"
       (let ((emacsconf-publishing-phase 'schedule))
         (emacsconf-publish-format-interleaved-schedule info)))))))

(defun emacsconf-format-talk-link (talk)
  (and talk (if (plist-get talk :slug)
                (format "<a href=\"/%s\">%s</a>"
                        (plist-get talk :url)
                        (plist-get talk :title))
              (plist-get talk :title))))

(defun emacsconf-summarize-caption-status (info)
  (let* ((talks (seq-filter
                 (lambda (o)
                   (and (not (string= (plist-get o :status) "CANCELLED"))
                        (plist-get o :speakers)))
                 (emacsconf-filter-talks info)))
         (captioned (seq-filter (lambda (o) (plist-get o :captioner)) talks))
         (received (seq-remove (lambda (o)
                                 (plist-get o :captioner))
                               talks)))
    (format "<div>%d talks total: %d captioned (%d min), %d waiting for captions (%d min)</div>"
                    (length talks)
                    (length captioned)
                    (apply '+ (mapcar (lambda (info) (string-to-number (plist-get info :time))) captioned))
                    (length received)
                    (apply '+ (mapcar (lambda (info) (string-to-number (plist-get info :time)))
                                      received)))))

(defun emacsconf-publish-sched-directive (o)
	"Format the schedule directive with info for O."
  (format "[[!template id=sched%s]]"
          (let ((result "")
                (attrs (append
                        (pcase emacsconf-publishing-phase
													('program
													 (list
														:time (plist-get o :time)
														:note (plist-get o :sched-note)))
													((or 'schedule 'conference)
													 (list
														:status (pcase (plist-get o :status)
																			("CAPTIONED" "captioned")
																			("PREREC_RECEIVED" "received")
																			("DONE" "done")
																			("STARTED" "now playing")
																			(_ nil))
														:time (plist-get o :time)
														:q-and-a (plist-get o :qa-link)
														:note (plist-get o :sched-note)
														:pad (and emacsconf-publish-include-pads (plist-get o :pad-url))
														:startutc (format-time-string "%FT%T%z" (plist-get o :start-time) t)
														:endutc (format-time-string "%FT%T%z" (plist-get o :end-time) t)
														:start (format-time-string "%-l:%M" (plist-get o :start-time) emacsconf-timezone)
														:end (format-time-string "%-l:%M" (plist-get o :end-time) emacsconf-timezone)))
													((or 'harvest 'resources)
													 (list
														:pad nil
														:channel nil
														:resources
														(concat
														 (emacsconf-surround "<li><a href=\""
																								 (and (not (eq emacsconf-publishing-phase 'resources))
																											(plist-get o :bbb-rec))
																								 "\">Play recording from BigBlueButton</a></li>" "")
														 (mapconcat
															(lambda (s) (concat "<li>" s "</li>"))
															(emacsconf-publish-link-file-formats-as-list
															 (append o
																			 (list :base-url (format "%s%s/" emacsconf-media-base-url emacsconf-year))))
															""))
)))
												(list
                         :title (plist-get o :title)
                         :url (concat "/" (plist-get o :url))
                         :speakers (plist-get o :speakers)
												 :track (if (member emacsconf-publishing-phase '(schedule conference)) (plist-get o :track))
                         :watch (plist-get o :watch-url)
												 :slug (plist-get o :slug)
                         :note
                         (string-join
                          (delq nil
                                (list
                                 (when (plist-get o :captions-edited)
                                   "captioned")
                                 (when (and (plist-get o :public)
                                            (or (plist-get o :toobnix-url)
                                                (plist-get o :video-file)))
                                   "video posted")
																 (emacsconf-surround "video: " (plist-get o :video-duration) "" nil)
																 (emacsconf-surround "answers: " (and (plist-get o :qa-public)
																																			(plist-get o :qa-video-duration))
																										 "" nil))
																)
                          ", ")
												 ))))
            (while attrs
              (let ((field (pop attrs))
                    (val (pop attrs)))
                (when val
                  (setq result (concat result " " (substring (symbol-name field) 1) "=\"\"\"" val "\"\"\"")))))
            result)))

(defun emacsconf-publish-format-main-schedule (info)
	"Include the schedule information for INFO."
  (mapconcat #'emacsconf-publish-sched-directive info "\n"))

(defun emacsconf-publish-talk-files (talk &optional files)
	"Return a list of files for TALK.
Use FILES as the file list, or look in the cache directory."
  (seq-filter (lambda (o)
								(string-match (concat "^" (regexp-quote (plist-get talk :file-prefix))) o))
							(or files
									(directory-files emacsconf-cache-dir))))

(defun emacsconf-sum (field talks)
  (apply '+ (seq-map (lambda (talk)
											 (string-to-number
												(or
												 (if (listp field)
														 (car
															(seq-keep
															 (lambda (f)
																 (plist-get talk f))
															 field))
													 (plist-get talk field))
												 "0")))
										 talks)))

(defun emacsconf-publish-backstage-org-on-state-change (talk)
  (save-window-excursion
    (emacsconf-with-talk-heading talk
      (when (and (member org-state '("PROCESSING" "TO_ASSIGN"))
                 (not (plist-get talk :video-time)))
        (emacsconf-publish-cache-video-data talk))
      (when (member org-state '("TO_CAPTION"))
        (unless (or noninteractive (org-entry-get (point) "CAPTIONER"))
          (org-entry-put (point) "CAPTIONER"
                         (assoc-default "CUSTOM_ID" (emacsconf-complete-volunteer)))))
      (when (member org-state '("WAITING_FOR_PREREC" "TO_ASSIGN" "TO_CAPTION" "TO_STREAM"))
        (emacsconf-publish-backstage-index)))))

(defun emacsconf-publish-backstage-to-assign (by-status files)
	(emacsconf-publish-backstage-list
	 (assoc-default "TO_ASSIGN" by-status)
	 files
	 "to be captioned, waiting for volunteers"
	 "<p>You can e-mail <a href=\"mailto:sacha@sachachua.com\">sacha@sachachua.com</a> to call dibs on editing the captions for one of these talks. We use OpenAI Whisper to provide auto-generated VTT that you can use as a starting point, but you can also write the captions from scratch if you like. The VTT file has timing information and the TXT file has the plain text; you can work with either. If you're writing the captions from scratch, you can choose to include timing information, or we can figure them out afterwards with a forced alignment tool. More info: <a href=\"https://emacsconf.org/captioning/\">captioning tips</a></p>"
	 (lambda (f)
		 (append
			f
			(list :extra
						(if (plist-get f :caption-note) (concat "<div class=\"caption-note\">" (plist-get f :caption-note) "</div>") "")
						:files
						(emacsconf-publish-talk-files f files))))))

(defun emacsconf-publish-backstage-to-caption (by-status files)
	(emacsconf-publish-backstage-list
	 (assoc-default "TO_CAPTION" by-status)
	 files
	 "being captioned"
	 "People are working on these ones, yay! <a href=\"https://emacsconf.org/captioning\">Captioning process/tips</a>"
	 (lambda (f)
		 (append
			f
			(list :extra
						(concat "<div class=\"caption-note\">"
										(emacsconf-surround "Being captioned by " (plist-get f :captioner) " " "")
										(emacsconf-surround "Note: " (plist-get f :caption-note) "" "")
										"</div>")
						:files
						(emacsconf-publish-talk-files f files))))))

(defun emacsconf-publish-backstage-list (talks files header-description &optional description modify-func)
	"Display a list of TALKS.
Use the files listed in FILES.
Start the header with HEADER-DESCRIPTION.
If MODIFY-FUNC is specified, use it to modify the talk."
	(format
   "<h1>%d talk(s) %s (%d minutes)</h1>%s<ol class=\"videos\">%s</ol>"
   (length talks)
	 header-description
	 (emacsconf-sum '(:video-time :time) talks)
   (or description "")
	 (mapconcat (lambda (f)
                (format  "<li><a name=\"%s\"></a><strong><a href=\"%s%s\">%s</a></strong><br />%s (id:%s)<br />%s</li>"
                         (plist-get f :slug)
                         emacsconf-base-url
                         (plist-get f :url)
                         (plist-get f :title)
                         (plist-get f :speakers-with-pronouns)
                         (plist-get f :slug)
                         (emacsconf-publish-index-card
													(if (functionp modify-func)
															(funcall modify-func f)
														f))))
							talks "\n")))

(defun emacsconf-publish-backstage-index (&optional filename)
	"Render the backstage index to FILENAME."
  (interactive)
  (setq filename (or filename (expand-file-name "index.html" emacsconf-backstage-dir)))
  (let ((info (or emacsconf-schedule-draft (emacsconf-publish-prepare-for-display (emacsconf-get-talk-info)))))
    (with-temp-file filename
      (let* ((talks
              (mapcar
               (lambda (o) (append
														(list :captions-edited t
																	:backstage t) o))
							  (emacsconf-filter-talks info)))
             (by-status (seq-group-by (lambda (o) (plist-get o :status)) talks))
             (files (directory-files emacsconf-backstage-dir)))
        (insert
         "<html><head><meta charset=\"UTF-8\"><link rel=\"stylesheet\" href=\"/style.css\" /></head><body>"
				 (if (file-exists-p (expand-file-name "include-in-index.html" emacsconf-cache-dir))
             (with-temp-buffer (insert-file-contents (expand-file-name "include-in-index.html" emacsconf-cache-dir)) (buffer-string))
           "")
				 "<p>Schedule by status: (gray: waiting, light yellow: processing, yellow: to assign, light blue: captioning, light green: to check, green: captioned and ready)<br />Updated by conf.org and the wiki repository</br />"
				 (let* ((emacsconf-schedule-svg-modify-functions '(emacsconf-schedule-svg-color-by-status))
								(img (emacsconf-schedule-svg 800 200 info)))
					 (with-temp-buffer
						 (mapc (lambda (node)
										 (dom-set-attribute
											node 'href
											(concat "#" (dom-attr node 'data-slug)))

										 (dom-append-child
											(dom-by-tag node 'title)
											(plist-get (emacsconf-resolve-talk (dom-attr node 'data-slug) talks) :status)))
									 (dom-by-tag img 'a))
						 (svg-print img)
						 (buffer-string)))
				 "</p>"
				 (if (member emacsconf-publishing-phase '(program schedule conference))
						 (format "<p>Waiting for %d talks (~%d minutes) out of %d total</p>"
										 (length (assoc-default "WAITING_FOR_PREREC" by-status))
										 (emacsconf-sum :time (assoc-default "WAITING_FOR_PREREC" by-status))
										 (length talks))
					 "")
         "<ul>"
         (mapconcat
          (lambda (status)
            (format "<li>%s - %d talk(s), %d minutes: %s</li>"
										(if (string= status "TO_ASSIGN")
												"<strong>TO_ASSIGN (waiting for captioning volunteers)</strong>"
											status)
										(length (assoc-default status by-status))
										(emacsconf-sum '(:video-time :time) (assoc-default status by-status))
                    (mapconcat (lambda (o) (format "<a href=\"#%s\">%s</a>%s"
                                                   (plist-get o :slug)
                                                   (plist-get o :slug)
																									 (emacsconf-surround " (" (plist-get o :video-duration) ")" "")))
                               (assoc-default status by-status)
                               ", ")))
					(pcase emacsconf-publishing-phase
						((or 'program 'schedule 'conference)
						 '("TO_CONFIRM" "WAITING_FOR_PREREC" "PROCESSING" "TO_ASSIGN" "TO_CAPTION" "TO_CHECK" "TO_STREAM"))
						((or 'harvest 'resources)
						 '("TO_ARCHIVE" "TO_REVIEW_QA" "TO_INDEX_QA" "TO_CAPTION_QA" "DONE")))
					"")
         "</ul>"
				 ;; alphabetical index
				 "<div>Alphabetical index: "
				 (mapconcat (lambda (o)
											(format "<a href=\"#%s\">%s</a>"
                              (plist-get o :slug)
                              (plist-get o :slug)))
										(sort talks (lambda (a b) (string< (plist-get a :slug) (plist-get b :slug))))
										", ")
				 "</div>"
				 (pcase emacsconf-publishing-phase
					 ((or 'program 'schedule 'conference 'cfp)
						(concat
						 (emacsconf-publish-backstage-list
							(append
							 (assoc-default "TO_PROCESS" by-status)
							 (assoc-default "PROCESSING" by-status)
							 (assoc-default "TO_AUTOCAP" by-status))
							files
							"being processed"
							"Not ready for captioning yet, but they will be eventually")
						 (emacsconf-publish-backstage-to-assign by-status files)
						 (emacsconf-publish-backstage-to-caption by-status files)
						 (emacsconf-publish-backstage-list
							(assoc-default "TO_CHECK" by-status) files
							"to be checked"
							"These can be checked to see if the subtitle timings are correct, audio/video is fine, etc.")
						 (emacsconf-publish-backstage-list
							(assoc-default "TO_STREAM" by-status)
							files
							"ready to be streamed")
						 (emacsconf-publish-backstage-list
							(seq-sort
							 #'emacsconf-sort-by-scheduled
							 (append (assoc-default "WAITING_FOR_PREREC" by-status)
											 (assoc-default "TO_CONFIRM" by-status)))
							files
							"we're waiting for"
							"Speakers might submit these, do them live, or cancel the talks.")))
					 ((or 'harvest 'resources)
						(let ((stages
									 '(("TO_REVIEW_QA" .
											"Please review the --bbb-webcams.webm file and/or the --bbb-webcams.vtt and tell us (emacsconf-submit@gnu.org) if a Q&amp;A session can be published or if it needs to be trimmed (lots of silence at the end of the recording, accidentally included sensitive information, etc.).")
										 ("TO_INDEX_QA" .
											"Please review the --answers.webm and --answers.vtt files to make chapter markers so that people can jump to specific parts of the Q&amp;A session. The <a href=\"https://emacsconf.org/harvesting/\">harvesting page on the wiki</a> has some notes on the process. That way, it's easier for people to see the questions and
answers without needing to listen to everything again. You can see <a href=\"https://emacsconf.org/2022/talks/asmblox\">asmblox</a> for an example of the Q&amp;A chapter markers.")
										 ("TO_CAPTION_QA" .
											"Please edit the --answers.vtt for the Q&amp;A talk you're interested in, correcting misrecognized words and cleaning it up so that it's nice to use as closed captions. All talks should now have large-model VTTs to make it easier to edit."))))
							(mapconcat
							 (lambda (stage)
								 (let ((status (car stage)))
									 (format
										"<h1>%s: %d talk(s) (%d minutes)</h1>%s<ol class=\"videos\">%s</ol>"
										status
										(length (assoc-default status by-status))
										(emacsconf-sum :video-time (assoc-default status by-status))
										(cdr stage)
										(mapconcat
										 (lambda (f)
											 (format  "<li><a name=\"%s\"></a><strong><a href=\"%s%s\">%s</a></strong><br />%s (id:%s)<br />%s</li>"
																(plist-get f :slug)
																emacsconf-base-url
																(plist-get f :url)
																(plist-get f :title)
																(plist-get f :speakers-with-pronouns)
																(plist-get f :slug)
																(emacsconf-publish-index-card
																 (append (list
																					:video-note
																					(unless (file-exists-p (expand-file-name (concat (plist-get f :file-prefix) "--bbb-webcams.webm") emacsconf-cache-dir))
																						"<div>No Q&A video for this talk</div>")
																					:video-file
																					(cond
																					 ((file-exists-p (expand-file-name (concat (plist-get f :file-prefix) "--answers.webm") emacsconf-cache-dir))
																						(concat (plist-get f :file-prefix) "--answers.webm"))
																					 ((file-exists-p (expand-file-name (concat (plist-get f :file-prefix) "--bbb-webcams.webm") emacsconf-cache-dir))
																						(concat (plist-get f :file-prefix) "--bbb-webcams.webm"))
																					 (t t)) ;; omit video
																					:video-id "-qanda"
																					:extra
																					(concat
																					 (emacsconf-surround "QA note: " (plist-get f :qa-note) "<br />")
																					 (format "Q&A archiving: <a href=\"%s-%s.txt\">IRC: %s-%s</a>"
																									 (format-time-string "%Y-%m-%d" (plist-get f :start-time))
																									 (plist-get (emacsconf-get-track f) :channel)
																									 (format-time-string "%Y-%m-%d" (plist-get f :start-time))
																									 (plist-get (emacsconf-get-track f) :channel))
																					 (emacsconf-surround ", <a href=\""
																															 (if (file-exists-p (expand-file-name (concat (plist-get f :file-prefix) "--pad.txt")
																																																		emacsconf-cache-dir))
																																	 (concat (plist-get f :file-prefix) "--pad.txt"))
																															 "\">Etherpad (Markdown)</a>" "")
																					 (emacsconf-surround ", <a href=\"" (plist-get f :bbb-playback) "\">BBB playback</a>" "")
																					 (emacsconf-surround ", <a href=\""
																															 (if (file-exists-p (expand-file-name (concat (plist-get f :file-prefix) "--bbb.txt")
																																																		emacsconf-cache-dir))
																																	 (concat (plist-get f :file-prefix) "--bbb.txt"))
																															 "\">BBB text chat</a>" "")
																					 (emacsconf-surround ", <a href=\""
																															 (if (file-exists-p (expand-file-name (concat (plist-get f :file-prefix) "--bbb-webcams.opus")
																																																		emacsconf-cache-dir))
																																	 (concat (plist-get f :file-prefix) "--bbb-webcams.opus"))
																															 "\">BBB audio only</a>" ""))
																					:files (emacsconf-publish-talk-files f files))
																				 f))))
										 (assoc-default status by-status) "\n"))))
							 stages
							 "\n"))))
				 (if (file-exists-p (expand-file-name "include-in-index-footer.html" emacsconf-cache-dir))
             (with-temp-buffer (insert-file-contents (expand-file-name "include-in-index-footer.html" emacsconf-cache-dir)) (buffer-string))
           "")
         "</body></html>")))))

(defun emacsconf-publish-filter-public-files (talk &optional selector files)
	"Return files that are okay to post publicly for TALK."
	(setq files (or files (emacsconf-publish-talk-files talk)))
  (and (plist-get talk :file-prefix)
			 (seq-filter
				(lambda (f)
					(when (string-match (concat (regexp-quote (plist-get talk :file-prefix))
																			(if selector (concat "--" selector) ""))
															f)
						;; further tests
						(pcase f
							((rx (seq "--"
												(or "reencoded" "normalized" "final" "old" "bbb" "backstage")))
							 nil)
							((rx ".diff") nil)
							((rx "--original")

							 ;; include original only if --main or does not exist
							 (and
								(not (string-match "--answers" f))
								(not (member (concat (plist-get talk :file-prefix)
																		 "--main.webm")
														 files))))
							((rx (seq "vtt" string-end))
							 t
							 ;; Let's try always posting captions even if they're not edited
							 ;; (or (plist-get talk :captions-edited)
							 ;; 		 (emacsconf-captions-edited-p (expand-file-name f emacsconf-cache-dir)))
							 )


							((rx (or "--main.txt" "--after-zaeph")) nil)
							(_ t))))
				files)))

(defun emacsconf-publish-public-index-for-talk (o files)
	(format "<li><div class=\"title\"><a name=\"%s\" href=\"%s\">%s</a></div></div><div class=\"speakers\">%s</div>%s</li>%s"
					(plist-get o :slug)
          (plist-get o :absolute-url)
          (plist-get o :title)
          (plist-get o :speakers)
          (emacsconf-publish-index-card
           (append (list :files
                         (seq-remove (lambda (f) (string-match "--answers" f))
																		 (emacsconf-publish-filter-public-files o))
												 :audio-file
												 (emacsconf-talk-file o "--main.opus")
												 :links
												 (concat
													(emacsconf-surround "<li><a href=\""
																							(unless (eq emacsconf-publishing-phase 'resources)
																								(if (plist-get o :backstage)
																										(emacsconf-backstage-url (plist-get o :pad-url))
																									(plist-get o :pad-url)))
																							"\">Open Etherpad</a></li>" "")
													(emacsconf-surround "<li><a href=\""
																							(and (member emacsconf-publishing-phase '(schedule conference))
																									 (plist-get o :qa-url))
																							"\">Open public Q&A</a></li>" "")
													(unless (eq emacsconf-publishing-phase 'resources)
														(emacsconf-surround "<li><a href=\""
																								(plist-get o :bbb-rec)
																								"\">Play recording from BigBlueButton</a></li>" ""))))
									 o))
          (if (or (emacsconf-talk-file o "--answers.webm")
									(emacsconf-talk-file o "--answers.opus"))
              (format "<li><div class=\"title\"><a href=\"%s\">Q&amp;A for %s</a></div>%s</li>"
											(plist-get o :absolute-url)
                      (plist-get o :title)
                      (emacsconf-publish-index-card
                       (append
                        (list
                         :public 1
                         :video-id (concat "qanda-" (plist-get o :slug))
                         :toobnix-url nil
												 :video-duration (plist-get o :qa-video-duration)
												 :video-file-size (plist-get o :qa-video-file-size)
                         :video-file (plist-get o :qa-video-file)
												 :audio-file (emacsconf-talk-file o "--answers.opus")
                         :files (emacsconf-publish-filter-public-files
                                 o
                                 "--answers"
																 files))
                        o)))
            "")))
;; (emacsconf-publish-public-index-for-talk (emacsconf-resolve-talk "rms") (directory-files emacsconf-cache-dir))

(defun emacsconf-publish-public-index (&optional filename)
  (interactive (list (expand-file-name "index.html" emacsconf-public-media-directory)))
  (setq filename (or filename (expand-file-name "index.html" emacsconf-public-media-directory)))
  (let ((files (directory-files emacsconf-public-media-directory))
				(info (emacsconf-public-talks (emacsconf-publish-prepare-for-display (emacsconf-get-talk-info)))))
		(emacsconf-publish-playlist (expand-file-name "index.m3u" emacsconf-public-media-directory)
                                (concat emacsconf-name emacsconf-year)
                                info
                                (format "https://media.emacsconf.org/%s/" emacsconf-year))
    (with-temp-file filename
      (insert
			 (emacsconf-replace-plist-in-string
				(list
				 :conf-name emacsconf-name
				 :year emacsconf-year
				 :quick-links (mapconcat (lambda (o) (format "<a href=\"#%s\">%s</a>"
																										 (plist-get o :slug)
																										 (plist-get o :slug)))
																 info ", ")
				 :videos (mapconcat (lambda (o) (emacsconf-publish-public-index-for-talk o files)) info "\n")
				 :include (if (file-exists-p (expand-file-name "include-in-public-index.html" emacsconf-cache-dir))
											(with-temp-buffer (insert-file-contents (expand-file-name "include-in-public-index.html" emacsconf-cache-dir)) (buffer-string))
										""))
				"<html><head><meta charset=\"utf-8\" /></head><link rel=\"stylesheet\" href=\"/style.css\" /><body>
<h1>${conf-name} ${year}</h1>
<div class=\"m3u\"><a href=\"index.m3u\">M3U playlist for playing in MPV and other players</a></div>
<div>Quick links: ${quick-links}</div>
<ol class=\"videos\">${videos}</ol>
${include}
</body></html>")))))

(defun emacsconf-publish-public-index-on-wiki ()
  (interactive)
  (let ((info (emacsconf-publish-prepare-for-display (emacsconf-filter-talks (emacsconf-get-talk-info))))
        (files (directory-files emacsconf-public-media-directory)))
    (with-temp-file (expand-file-name "all-include.md" (expand-file-name emacsconf-year emacsconf-directory))
      (insert
       "<ol class=\"videos\">"
       (mapconcat
        (lambda (f)
          (format "<li><div class=\"title\"><a name=\"%s\" href=\"%s\">%s</a></div><div class=\"speakers\">%s</div>%s%s</li>"
									(plist-get f :slug)
                  (plist-get f :absolute-url)
                  (plist-get f :title)
                  (or (plist-get f :speakers) "")
                  (if (plist-get f :public)
                      (emacsconf-publish-index-card
                       (append (list :base-url
                                     (concat emacsconf-media-base-url (plist-get f :conf-year) "/")
                                     :track-base-url
                                     (format "/%s/captions/" (plist-get f :conf-year))
																		 :links
																		 (unless (eq emacsconf-publishing-phase 'resources)
																			 (emacsconf-surround "<li><a href=\""
																													 (plist-get o :bbb-rec)
																													 "\">Play recording from BigBlueButton</a></li>" ""))

																		 :files
																		 (seq-remove (lambda (f) (string-match "--answers" f))
																								 (emacsconf-publish-filter-public-files f files)))
																		 f))
                    "")
                  (if (plist-get f :qa-public)
                      (emacsconf-publish-index-card
                       (append
                        (list
                         :public 1
                         :base-url (concat emacsconf-media-base-url (plist-get f :conf-year) "/")
                         :video-id (concat "qanda-" (plist-get f :slug))
                         :track-base-url
                         (format "/%s/captions/" (plist-get f :conf-year))
                         :video-file
												 (emacsconf-talk-file f "--answers.webm")
												 :files
												 (emacsconf-publish-filter-public-files f files "--answers"))
                        f))
                    "")))
        info "\n"))
      "</ol>")))

(defun emacsconf-make-chapter-strings (filename track-base-url &optional target)
  (let ((chapters (and filename (file-exists-p filename) (subed-parse-file filename))))
    (when chapters
      (list
       :track (format "<track kind=\"chapters\" label=\"Chapters\" src=\"%s\" />"
                      (concat (or track-base-url "") (file-name-nondirectory filename)))
       :md (mapconcat (lambda (chapter)
                        (concat
												 (if (= (elt chapter 1) 0)
														 "00:00"
													 (format-seconds "%.2h:%z%.2m:%.2s" (floor (/ (elt chapter 1) 1000))))
                         (format ".%03d" (mod (elt chapter 1) 1000))
                         " "
                         (elt chapter 3)
                         "\n"))
                      chapters
                      "")
       :html (format "<pre data-target=\"%s\" class=\"chapters\">\n%s\n</pre>"
                     (or target "")
                     (mapconcat
                      (lambda (chapter)
												(format "%s.%03d %s"
																(if (= (elt chapter 1) 0)
																		"00:00"
																	(format-seconds "%.2h:%z%.2m:%.2s" (floor (/ (elt chapter 1) 1000))))
                                (mod (elt chapter 1) 1000)
                                (elt chapter 3)))
                      chapters
                      "\n"))))))

(defvar emacsconf-publish-subtitle-languages '(("fr" . "French") ("ja" . "Japanese") ("es" . "Spanish")))

(defun emacsconf-video-subtitle-tracks (filename track-base-url &optional files)
  (setq files (or files (directory-files emacsconf-cache-dir)))
	(if filename
			(concat
			 (if (member (file-name-nondirectory filename) files)
					 (format "<track label=\"English\" kind=\"captions\" srclang=\"en\" src=\"%s\" default />"
									 (concat (or track-base-url "") (file-name-nondirectory filename)))
				 "")
			 (mapconcat
				(lambda (lang)
					(let ((lang-file (concat (file-name-sans-extension filename) "_" (car lang) "." (file-name-extension filename))))
						(if (member lang-file files)
								(format "<track label=\"%s\" kind=\"captions\" srclang=\"%s\" src=\"%s\" />"
												(cdr lang)
												(car lang)
												(concat (or track-base-url "") (file-name-nondirectory lang-file)))
							"")))
				emacsconf-publish-subtitle-languages
				""))
		""))

(defun emacsconf-publish-link-file-formats (file-prefix)
  (string-join (emacsconf-publish-link-file-formats-as-list file-prefix) " "))

(defun emacsconf-publish-link-file-formats-as-list (talk)
	(seq-map
	 (lambda (file)
		 (let ((cache-file (expand-file-name (file-name-nondirectory file) emacsconf-cache-dir)))
			 (format "<a href=\"%s%s\">Download %s%s</a>%s"
							 (or (plist-get talk :base-url) "")
							 (file-name-nondirectory file)
							 (replace-regexp-in-string (concat "^" (regexp-quote (plist-get talk :file-prefix))) "" (file-name-nondirectory file))
							 (if (and (file-exists-p cache-file)
												(> (file-attribute-size (file-attributes cache-file)) 1000000))
									 (format " (%sB)" (file-size-human-readable (file-attribute-size (file-attributes cache-file))))
								 "")
							 (if (and (string-match "--\\(main\\|answers\\)\\.vtt" file)
												(not (emacsconf-captions-edited-p (expand-file-name file emacsconf-cache-dir))))
									 " (unedited)"
								 ""))))
	 (or (plist-get talk :files)
			 (if (plist-get talk :backstage)
					 (emacsconf-publish-talk-files talk)
				 (emacsconf-publish-filter-public-files talk)))))

(defun emacsconf-publish-talks-json ()
	"Return JSON format with a subset of talk information."
	(let ((emacsconf-publishing-phase 'conference))
		(json-encode
		 (list
			:talks
			(mapcar
			 (lambda (o)
				 (apply
					'list
					(cons :start-time (format-time-string "%FT%T%z" (plist-get o :start-time) t))
					(cons :end-time (format-time-string "%FT%T%z" (plist-get o :end-time) t))
					(mapcar
					 (lambda (field)
						 (cons field (plist-get o field)))
					 '(:slug :title :speakers :pronouns :pronunciation :url :track :file-prefix
									 :qa-url
									 :qa-type
									 :live
									 :qa-backstage-url))))
			 (emacsconf-filter-talks (emacsconf-get-talk-info)))
			:tracks
			(mapcar
			 (lambda (o)
				 (mapcar
					(lambda (field)
						(cons field (plist-get o field)))
					'(:name :color :id :channel :webchat-url :stream :480p :start :end)))
			 emacsconf-tracks)))))

(defun emacsconf-publish-talks-json-to-files ()
	"Export talk information as JSON so that we can use it in shell scripts."
	(interactive)
	(mapc (lambda (dir)
					(when (and dir (file-directory-p dir))
						(with-temp-file (expand-file-name "talks.json" dir)
							(insert (emacsconf-publish-talks-json)))))
				(list emacsconf-res-dir emacsconf-ansible-directory emacsconf-public-media-directory)))

(defun emacsconf-talks-csv ()
  "Make a CSV of the talks.
            Columns are: slug,title,speakers,talk page url,video url,duration,sha."
  (interactive)
  (require 'org-table)
  (require 'compile-media)
  (with-temp-file (expand-file-name "talks.csv" emacsconf-public-media-directory)
    (let ((results (delq nil
                         (seq-mapcat
                          (lambda (o)
                            (let ((date (format-time-string "%Y-%m-%d"
                                                            (plist-get o :start-time)
                                                            emacsconf-timezone))
                                  (main-video (expand-file-name
                                               (concat (plist-get o :file-prefix) "--main.webm")
                                               emacsconf-cache-dir))
                                  (qa-video (expand-file-name
                                             (concat (plist-get o :file-prefix) "--answers.webm")
                                             emacsconf-cache-dir))
                                  (talk-page-url (plist-get o :url))
                                  (speakers (or (plist-get o :speakers) "")))
                              (delq
                               nil
                               (list
                                (list
                                 (concat emacsconf-name " " emacsconf-year)
                                 (plist-get o :slug)
                                 (plist-get o :title)
                                 speakers
                                 talk-page-url
                                 (format "%s%s/%s--main.webm"
                                         emacsconf-media-base-url
                                         (plist-get o :conf-year)
                                         (plist-get o :file-prefix))
                                 date
                                 (format-seconds "%02h:%z%02m:%02s" (/ (compile-media-get-file-duration-ms main-video) 1000))
                                 (if (file-exists-p main-video)
                                     (string-trim (shell-command-to-string (concat "sha1sum -b " (shell-quote-argument main-video) " | cut -d ' ' -f 1")))
                                   "")
                                 (or (plist-get o :youtube-url) "")
                                 (or (plist-get o :toobnix-url) ""))
                                (if (plist-get o :qa-public)
                                    (list
                                     (concat emacsconf-name " " emacsconf-year)
                                     (format "%s-qa" (plist-get o :slug))
                                     (format "Q&A for %s" (plist-get o :title))
                                     speakers
                                     talk-page-url
                                     (format "%s%s/%s--answers.webm"
                                             emacsconf-media-base-url
                                             (plist-get o :conf-year)
                                             (plist-get o :file-prefix))
                                     date
                                     (format-seconds "%02h:%z%02m:%02s" (/ (compile-media-get-file-duration-ms qa-video) 1000))
                                     (if (file-exists-p qa-video)
                                         (string-trim (shell-command-to-string (concat "sha1sum -b " (shell-quote-argument qa-video) " | cut -d ' ' -f 1")))
                                       "")
                                     (or (plist-get o :qa-youtube) "")
                                     (or (plist-get o :qa-toobnix) "")))))))
                          (emacsconf-public-talks (emacsconf-get-talk-info))))))
      (insert (orgtbl-to-csv
               (cons '("Conference" "Slug" "Title" "Speakers" "Talk page URL" "Video URL" "Date" "Duration" "SHA" "Youtube URL" "Toobnix URL")
                     results)
               nil)))))

(defvar emacsconf-conduct-url (concat emacsconf-base-url "conduct/") "URL for guidelines for conduct.")

(defun emacsconf-generate-pad-template (emacsconf-info)
  "Generate a template for copying and pasting into the pad.
            Writes it to pad-template.html."
  (interactive (list (emacsconf-get-talk-info)))
  (let* ((talks (emacsconf-filter-talks emacsconf-info))
         (text (concat
								(emacsconf-replace-plist-in-string
								 (list :conf-info (concat emacsconf-base-url emacsconf-year)
											 :conduct emacsconf-conduct-url
											 :base-url emacsconf-base-url
											 :conf-name emacsconf-name))
								"<p>Conference info, how to watch/participate: ${conf-info}<br />
            Guidelines for conduct: https://emacsconf.org/conduct/</p>

            <p>Except where otherwise noted, the material on the ${conf-name} pad are dual-licensed under the terms of the Creative Commons Attribution-ShareAlike 4.0 International Public License ; and the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) an later version.
            Copies of these two licenses are included in the ${conf-name} wiki repository, in the COPYING.GPL and COPYING.CC-BY-SA files (${base-url}COPYING/).</p>

            <p>By contributing to this pad, you agree to make your contributions available under the above licenses. You are also promising that you are the author of your changes, or that you copied them from a work in the public domain or a work released under a free license that is compatible with the above two licenses. DO NOT SUBMIT COPYRIGHTED WORK WITHOUT PERMISSION.</p>

            <p>
            This pad is here to be curated by everybody and its rough structure is like this:
            <ol><li>General info and license
            <li>A section for each talk -> please do add questions and notes
            <li>A general feedback section
            </ol>
            </p>
            "
								(mapconcat
								 (lambda (o)
									 (let ((url (plist-get o :absolute-url)))
										 (format "-------------------------------------------------------------------------------------------------<br/><strong>Talk%s: %s</strong><br />
            Speaker(s): %s<br />
            Talk page: <a href=\"%s\">%s</a><br />
            Actual start of talk EST: &nbsp ;&nbsp;&nbsp;  Start of Q&A: &nbsp;&nbsp;  End of Q&A: &nbsp;&nbsp;<br />
            <strong>Questions:</strong>
            Speakers may answer in any order or skip questions. As much as possible, put your questions at the top level instead of under another question. If adding an answer, please indicate [speaker] or your nick accordingly. Volunteers, please add new slots as ones get filled.<br />
            <ul>
            <li>Q1:&nbsp              ;<ul><li>A:&nbsp;</li></ul></li>
            <li>Q2:&nbsp              ;<ul><li>A:&nbsp;</li></ul></li>
            <li>Q3:&nbsp              ;<ul><li>A:&nbsp;</li></ul></li>
            <li>Q4:&nbsp              ;<ul><li>A:&nbsp;</li></ul></li>
            </ul>

            <strong>Links and other notes:</strong>
            <ul>
            <li>sample text</li>
            <li>sample text</li>
            <li>sample text</li>
            <li>sample text</li>
            </ul>
            " (plist-get o :slug) (plist-get o :title) (plist-get o :speakers) url url))) talks "<br/><br/>\n")
								"<br/><br/>-------------------------------------------------------------------------------------------------<br/>
            <strong>General Feedback: What went well?</strong><br/><br/>
            <ul>
            <li>sample text</li>
            <li>sample text</li>
            <li>sample text</li>
            <li>sample text</li>
            </ul>
            <br /><br />
            -------------------------------------------------------------------------------------------------<br/>
            <strong>General Feedback: What to improve?</strong><br/><br/>
            <ul>
            <li>sample text</li>
            <li>sample text</li>
            <li>sample text</li>
            <li>sample text</li>
            </ul>
            <br/><br/>
            -------------------------------------------------------------------------------------------------<br/>
            <strong>Colophon:</strong>
            <ul></ul>")))
    (with-current-buffer (find-file "pad-template.html")
      (erase-buffer)
      (insert text)
      (save-buffer)))
  (browse-url-of-file "pad-template.html"))

(defun emacsconf-publish-format-files-as-playlist (playlist-name files)
	(format "#EXTM3U\n#PLAYLIST: %s\n#EXTALB: %s\n#EXTGENRE: Speech\n%s"
          playlist-name playlist-name
          (mapconcat
           (lambda (file)
						 (format "#EXTINF:-1,%s\n%s\n"
										 file
										 file))
					 files
           "")))

(defun emacsconf-publish-playlist (filename playlist-name talks &optional base-url)
  (with-temp-file filename
    (insert (format "#EXTM3U\n#PLAYLIST: %s\n#EXTALB: %s\n#EXTGENRE: Speech\n%s"
                    playlist-name playlist-name
                    (mapconcat
                     (lambda (talk)
											 (concat
												(if (emacsconf-talk-file talk "--main.webm")
														(format "#EXTINF:-1,%s - %s\n%s%s\n"
                                    (plist-get talk :title)
                                    (plist-get talk :speakers)
                                    base-url
																		(file-name-nondirectory (emacsconf-talk-file talk "--main.webm")))
													"")
												(if (emacsconf-talk-file talk "--answers.webm")
														(format "#EXTINF:-1,Q&A for %s - %s\n%s%s\n"
                                    (plist-get talk :title)
                                    (plist-get talk :speakers)
                                    base-url
																		(file-name-nondirectory (emacsconf-talk-file talk "--answers.webm")))
													"")
												))
                     talks
                     "")))))

(defun emacsconf-get-preferred-video (file-prefix &optional files)
	(when (listp file-prefix) (setq file-prefix (plist-get file-prefix :file-prefix)))
  (or
   (car
    (mapcar
     (lambda (suffix)
       (seq-find (lambda (s) (string-match
                              (concat (regexp-quote
                                       (if suffix (concat file-prefix "--" suffix)
                                         file-prefix))
                                      "\\." (regexp-opt emacsconf-media-extensions)) s)) files))
     '("main" "captioned" "normalized" "reencoded" "compressed" "original" nil)))
   (seq-find
    'file-exists-p
    (seq-map (lambda (suffix)
               (expand-file-name (concat file-prefix "--" suffix ".webm")
                                 emacsconf-cache-dir))
             '("main" "captioned" "normalized" "reencoded" "compressed" "original")))
   (car
		(seq-remove (lambda (o) (string-match "--intro"  o))
								(directory-files emacsconf-cache-dir
																 nil
																 (concat (regexp-quote file-prefix)
																				 ".*\\."
																				 (regexp-opt emacsconf-media-extensions)))))))

(defun emacsconf-check-video-formats ()
  (interactive)
  (with-current-buffer (get-buffer-create "*Video check*")
    (erase-buffer)
    (mapc
     (lambda (filename)
       (insert "* " (if (string-match "--\\(main\\|compressed\\|normalized\\|captioned\\)\\.webm$" filename)
                        (match-string 1 filename)
                      "")
               ": "
               filename "\n"
               (shell-command-to-string (format "ffprobe %s 2>&1 | grep -E '(Duration|Stream)'"
                                                (shell-quote-argument filename)))
               "\n"))
     (delq nil
           (mapcar (lambda (talk) (emacsconf-get-preferred-video (plist-get talk :file-prefix)))
                   emacsconf-info)))
    (switch-to-buffer (current-buffer))))

;;; Video services

(autoload 'subed-parse-file "subed-common")
(defun emacsconf-publish-video-description (talk &optional copy skip-title)
  (interactive (list (emacsconf-complete-talk-info) t))
	(when (stringp talk) (setq talk (emacsconf-resolve-talk talk)))
  (let ((chapters (subed-parse-file
                   (expand-file-name
                    (concat
                     (file-name-base (plist-get talk :file-prefix)) "--main--chapters.vtt")
                    emacsconf-cache-dir)))
        result)
    (setq result
          (emacsconf-replace-plist-in-string
           (append
            (list
             :conf-name emacsconf-name :year emacsconf-year
             :chapters
             (if chapters
                 (concat
                  (mapconcat
                   (lambda (chapter)
                     (format "%s.%03d %s"
                             (format-seconds "%.2h:%z%.2m:%.2s" (floor (/ (elt chapter 1) 1000)))
                             (mod (elt chapter 1) 1000)
                             (elt chapter 3)))
                   chapters "\n")
                  "\n\n")
               ""))
            talk)
					 (concat
						(if skip-title "" "${conf-name} ${year}: ${title} - ${speakers-with-pronouns}
")
						"${absolute-url}

${chapters}You can view this and other resources using free/libre software at ${absolute-url} . During the conference, you can ask questions via the Etherpad or through IRC (${webchat-url} , or ${channel} on irc.libera.chat). Afterwards, check the talk page at ${absolute-url} for notes and contact information.

This video is available under the terms of the Creative Commons Attribution-ShareAlike 4.0 International (CC BY-SA 4.0) license.")))
    (if copy (kill-new result))
    result))

(defun emacsconf-publish-youtube-step-through-publishing ()
	(interactive)
	(catch 'done
		(while t
			(let ((talk (seq-find (lambda (o)
															(and (member (plist-get o :status) '("TO_STREAM" "TO_CHECK"))
																		(not (plist-get o :youtube))
																		(emacsconf-talk-file o "--main.webm")))
														 (emacsconf-publish-prepare-for-display (emacsconf-get-talk-info)))))
				(unless talk
					(message "All done so far.")
					(throw 'done t))
				(kill-new (emacsconf-talk-file talk "--main.webm"))
				(message "Video: %s - press any key" (emacsconf-talk-file talk "--main.webm"))
				(when (eq (read-char) ?q) (throw 'done t))
				(emacsconf-publish-video-description talk t)
				(message "Copied description - press any key")
				(when (eq (read-char) ?q) (throw 'done t))
				(when (emacsconf-talk-file talk "--main.vtt")
					(kill-new (emacsconf-talk-file talk "--main.vtt"))
					(message "Captions: %s - press any key" (emacsconf-talk-file talk "--main.vtt"))
					(when (eq (read-char) ?q) (throw 'done t)))
				(emacsconf-set-property-from-slug
				 (plist-get talk :slug)
				 "YOUTUBE"
				 (read-string (format "%s - YouTube URL: " (plist-get talk :scheduled))))))))

(defun emacsconf-publish-toobnix-step-through-publishing ()
	(interactive)
	(catch 'done
		(while t
			(let ((talk (seq-find (lambda (o)
															(and (member (plist-get o :status) '("TO_STREAM" "TO_CHECK"))
																		(not (plist-get o :toobnix-url))
																		(emacsconf-talk-file o "--main.webm")))
														 (emacsconf-publish-prepare-for-display (emacsconf-get-talk-info)))))
				(unless talk
					(message "All done so far.")
					(throw 'done t))
				(kill-new (emacsconf-talk-file talk "--main.webm"))
				(message "Video: %s - press any key" (emacsconf-talk-file talk "--main.webm"))
				(when (eq (read-char) ?q) (throw 'done t))
				(emacsconf-publish-video-description talk t)
				(message "Copied description - press any key")
				(when (eq (read-char) ?q) (throw 'done t))
				(when (emacsconf-talk-file talk "--main.vtt")
					(kill-new (emacsconf-talk-file talk "--main.vtt"))
					(message "Captions: %s - press any key" (emacsconf-talk-file talk "--main.vtt"))
					(when (eq (read-char) ?q) (throw 'done t)))
				(emacsconf-set-property-from-slug
				 (plist-get talk :slug)
				 "YOUTUBE"
				 (read-string (format "%s - Toobnix URL: " (plist-get talk :scheduled))))))))


;; (emacsconf-publish-video-description (emacsconf-find-talk-info "async") t)

(defun emacsconf-cache-all-video-data ()
	(interactive)
	(mapc
	 (lambda (talk)
		 (when (plist-get talk :file-prefix)
			 (emacsconf-publish-cache-video-data talk)))
	 (emacsconf-get-talk-info)))
;; (emacsconf-cache-all-video-data t)
(defvar emacsconf-cache-dir (expand-file-name "cache" (file-name-directory emacsconf-org-file)))

(defun emacsconf-publish-cache-video-data (talk)
  (interactive (list (emacsconf-complete-talk-info)))
  (let ((main (expand-file-name (concat (plist-get talk :file-prefix) "--main.webm")
                                emacsconf-cache-dir)))
    (emacsconf-with-talk-heading talk
      (let* ((video-file-name (emacsconf-get-preferred-video (plist-get talk :file-prefix)))
             (video-file (and video-file-name (expand-file-name video-file-name emacsconf-cache-dir)))
						 (qa-file (emacsconf-talk-file talk "--answers.webm"))
						 (intro-file (expand-file-name (concat (plist-get talk :slug) ".webm")
																					 (expand-file-name "intros" emacsconf-stream-asset-dir)))
             duration)
        (unless (file-exists-p main)
          (setq main video-file-name))
        (if video-file
						(progn
							(org-entry-put (point) "VIDEO_FILE" (file-name-nondirectory video-file))
							(org-entry-put (point) "VIDEO_FILE_SIZE" (file-size-human-readable (file-attribute-size (file-attributes video-file))))
							(unless (plist-get talk :captions-edited)
								(let ((caption-file (expand-file-name
																		 (concat (plist-get talk :file-prefix)
																						 "--main.vtt")
																		 emacsconf-cache-dir)))
									(if (emacsconf-captions-edited-p caption-file)
											(org-entry-put (point) "CAPTIONS_EDITED" "1")
										(org-entry-delete (point) "CAPTIONS_EDITED"))))
							(setq duration (/ (compile-media-get-file-duration-ms video-file) 1000))
							(org-entry-put (point) "VIDEO_DURATION" (format-seconds "%h:%z%.2m:%.2s" duration))
							(org-entry-put (point) "VIDEO_TIME" (number-to-string (ceiling (/ duration 60)))))
					(org-entry-delete (point) "VIDEO_FILE")
					(org-entry-delete (point) "VIDEO_FILE_SIZE")
					(org-entry-delete (point) "VIDEO_DURATION")
					(org-entry-delete (point) "VIDEO_TIME")
					(org-entry-delete (point) "CAPTIONS_EDITED"))
				(if qa-file
						(progn
							(org-entry-put (point) "QA_VIDEO_FILE" (file-name-nondirectory qa-file))
							(org-entry-put (point) "QA_VIDEO_FILE_SIZE" (file-size-human-readable (file-attribute-size (file-attributes qa-file))))
							(unless (plist-get talk :qa-captions-edited)
								(let ((caption-file (emacsconf-talk-file talk "--answers.vtt")))
									(if (emacsconf-captions-edited-p caption-file)
											(org-entry-put (point) "QA_CAPTIONS_EDITED" "1")
										(org-entry-delete (point) "QA_CAPTIONS_EDITED"))))
							(setq duration (/ (compile-media-get-file-duration-ms qa-file) 1000))
							(org-entry-put (point) "QA_VIDEO_DURATION" (format-seconds "%h:%z%.2m:%.2s" duration))
							(org-entry-put (point) "QA_VIDEO_TIME" (number-to-string (ceiling (/ duration 60)))) )
					(org-entry-delete (point) "QA_VIDEO_FILE")
					(org-entry-delete (point) "QA_VIDEO_FILE_SIZE")
					(org-entry-delete (point) "QA_VIDEO_DURATION")
					(org-entry-delete (point) "QA_VIDEO_TIME")
					(org-entry-delete (point) "QA_CAPTIONS_EDITED"))
				(when (file-exists-p intro-file)
					(org-entry-put
					 (point) "INTRO_TIME"
					 (number-to-string (ceiling (/ (compile-media-get-file-duration-ms intro-file) 60000)))))))))

(defvar emacsconf-youtube-channel-id "UCwuyodzTl_KdEKNuJmeo99A")
(defun emacsconf-youtube-edit ()
  (interactive)
  (let ((url (org-entry-get (point) "YOUTUBE_URL")))
    (if url
        (when (or (string-match "youtu\\.be/\\([-A-Za-z0-9_]+\\)" url)
                  (string-match "watch\\?v=\\([-A-Za-z0-9_]+\\)" url))
          (browse-url (format "https://studio.youtube.com/video/%s/edit" (match-string 1 url))))
      (browse-url (concat "https://studio.youtube.com/channel/" emacsconf-youtube-channel-id)))))

(defun emacsconf-toobnix-edit ()
  (interactive)
  (let ((url (org-entry-get (point) "TOOBNIX_URL")))
    (if url
        (when (string-match "/w/\\([A-Za-z0-9]+\\)" url)
          (browse-url (format "https://toobnix.org/videos/update/%s" (match-string 1 url))))
      (when (> (length (org-entry-get (point) "FILE_PREFIX")) 80)
        (copy-file (expand-file-name (concat (org-entry-get (point) "FILE_PREFIX") "--main.webm") emacsconf-cache-dir)
                   (expand-file-name (concat "emacsconf-" emacsconf-year "-" (org-entry-get (point) "SLUG") ".webm") emacsconf-cache-dir) t))
      (browse-url "https://toobnix.org/videos/upload#upload"))))

(defun emacsconf-publish-files ()
  (interactive)
  (let* ((slug (org-entry-get (point) "FILE_PREFIX"))
         (video-file (emacsconf-get-preferred-video slug))
         (wiki-captions-directory (expand-file-name "captions" (expand-file-name emacsconf-year emacsconf-directory))))
    (org-entry-put (point) "PUBLIC" "1")
    (when (file-exists-p video-file)
      (emacsconf-youtube-edit)
      (emacsconf-toobnix-edit)
      (emacsconf-publish-update-talk (emacsconf-get-talk-info-for-subtree)))))

(defvar emacsconf-publish-autocommit-wiki nil)
(defun emacsconf-publish-commit-and-push-wiki-maybe (&optional do-it message)
	(interactive (list t))
	(let ((default-directory emacsconf-directory))
    (shell-command "git add -u")
    (when (or noninteractive emacsconf-publish-autocommit-wiki do-it)
			(call-process "git" nil nil nil "commit" "-a" "-m" (or message "Automated commit"))
			(start-process "git" nil "git" "push"))))

(defmacro emacsconf-publish-with-wiki-change (&rest body)
  (declare (indent 0) (debug t))
  `(progn
     ,@body
		 (emacsconf-publish-commit-and-push-wiki-maybe
			,emacsconf-publish-autocommit-wiki
			(and (stringp ,(car body)) ,(car body)))))

(defun emacsconf-publish-schedule-svg-snippets ()
  (interactive)
  (let* ((info (emacsconf-publish-prepare-for-display (emacsconf-get-talk-info)))
         (by-day (emacsconf-by-day info))
         (year-dir (expand-file-name emacsconf-year emacsconf-directory))
         (width 800)
         (height 300))
    (emacsconf-publish-with-wiki-change
      (with-temp-file (expand-file-name "schedule-image.md" year-dir)
        (insert "<div class=\"schedule-svg-container\">")
        (svg-print (emacsconf-schedule-svg 800 300 info))
        (insert "</div>"))
      (mapc (lambda (day)
              (let ((start (date-to-time (concat (car day) "T" emacsconf-schedule-start-time emacsconf-timezone-offset)))
                    (end (date-to-time (concat (car day) "T" emacsconf-schedule-end-time emacsconf-timezone-offset))))
                (with-temp-file (expand-file-name (concat "schedule-" (car day) ".md") year-dir)
                  (insert "<div class=\"schedule-svg-container\">")
                  (svg-print (emacsconf-schedule-svg-day
                              (svg-create width (/ height (length by-day)))
                              (format-time-string "%A" (plist-get (cadr day) :start-time) emacsconf-timezone)
                              width (/ height (length by-day))
                              start end
                              (emacsconf-by-track (cdr day))))
                  (insert "</div>"))))
            by-day))))

(defvar emacsconf-publish-watch-directory "/ssh:orga@front0.emacsconf.org:/var/www/live.emacsconf.org/")

(defun emacsconf-publish-format-watch-index (info)
  (concat
   "<!-- Automatically generated by emacsconf-publish-watch-pages -->\n
<h2>Tracks</h2>
We recommend using a streaming player like mpv to watch the livestreams. Example: <pre>
"
	 (mapconcat (lambda (track)
								(concat "mpv " (plist-get track :stream) "\n"))
							emacsconf-tracks
							"")
   "</pre><table width=\"100%\"><tr><th>Watch page</th><th>Watch page (low-res)</th><th>IRC channel (libera.chat)</th><th>URL for streaming player (ex: mpv, vlc, ffplay)</th><th>Low res</th></tr>\n"
   (mapconcat (lambda (track)
                (emacsconf-replace-plist-in-string
                 (append (list :year emacsconf-year
															 :watch-base emacsconf-live-base-url
															 ) track)
                 "<tr><td><div class=\"sched-track ${name}\"><a href=\"${watch-base}${year}/watch/${id}/\">${name}</a></div></td><td><a href=\"${watch-base}${year}/watch/${id}-480p/\">${name} (low-res)</a></td><td><a href=\"${webchat-url}\">${channel}</a></td><td><a href=\"${stream}\">${stream}</a></td><td><a href=\"${480p}\">${id}-480p.webm</a></tr>"))
              emacsconf-tracks
              "\n")
   "</table>\n\n"
   (with-temp-buffer
     (svg-print (emacsconf-schedule-svg 800 300 info))
     (buffer-string))))

(defun emacsconf-publish-schedule-line (talk)
  (setq talk (append talk (list
                           :startutc (format-time-string "%FT%T%z" (plist-get talk :start-time) t)
                           :endutc (format-time-string "%FT%T%z" (plist-get talk :end-time) t)
                           :start (format-time-string "%-l:%M" (plist-get talk :start-time) emacsconf-timezone)
                           :end (format-time-string "%-l:%M" (plist-get talk :end-time) emacsconf-timezone)
                           :base-url emacsconf-base-url)))
  (emacsconf-replace-plist-in-string
   (append talk (list
                 :start-info (emacsconf-surround "<span class=\"sched-start\">" (plist-get talk :start) "</span>" "")
                 :end-info (emacsconf-surround " - <span class=\"sched-end\">" (plist-get talk :end) "</span>" "")
                 :track-info (emacsconf-surround (format " <span class=\"sched-track %s\">" (or (plist-get talk :track) "")) (plist-get talk :track) "</span>" "")
                 :q-info  (emacsconf-surround " <span class=\"sched-q-and-a\">Q&amp;A: " (plist-get talk :qa-link) "</span>; " "")
                 :pad-info (emacsconf-surround " <span class=\"sched-pad\"> <a href=\""
                                               (plist-get talk :pad-url) "\">Etherpad</a></span>; " "")
                 :slug-info (emacsconf-surround " <span class=\"sched-slug\">id:" (plist-get talk :slug) "</span>" "")
                 :speaker-info (emacsconf-surround " <div class=\"sched-speakers\">" (plist-get talk :speakers-with-pronouns) "</div>" "")
                 :resources-info (emacsconf-surround "<ul class=\"resources\">" (plist-get talk :resources) "</ul>" "")))
   "<div data-start=\"${startutc}\" data-end=\"${endutc}\" class=\"sched-entry track-${track}\">
<div class=\"sched-meta\"><span class=\"sched-time\">${start-info}${end-info}</span>${track-info}${pad-info}${q-info}${slug-info}</div>
<div class=\"sched-title\"><a href=\"${base-url}${url}\">${title}</a></div>
${speaker-info}
${resources-info}
</div>
"))

(defun emacsconf-publish-schedule-short (info)
  (mapconcat (lambda (o)
               (emacsconf-replace-plist-in-string
                (append o (list
													 :qa-link-include (emacsconf-surround ", " (plist-get o :qa-link) "" "")
													 :base-url emacsconf-base-url))
                "<span><a href=\"${base-url}${url}\">${slug}</a> (<a class=\"pad-link\" href=\"${pad-url}\">pad</a>${qa-link-include})</span>"))
             info " - "))

(defun emacsconf-publish-page-nav (nav &optional current sep)
  (concat (if current (format "<a name=\"%s\"></a>" current) "")
          (mapconcat (lambda (n)
                       (if (string= current (car n))
                           (concat "<strong>" (cdr n) "</strong>")
                         (concat "<a href=\"#" (car n) "\">" (cdr n) "</a>")))
                     nav (or sep " - "))))

(defun emacsconf-publish-format-watch-track (track info)
  (let ((nav '(("watch" . "Watch")
               ("links" . "Pad and Q&amp;A links")
               ("chat" . "Chat")
               ("sched" . "Schedule")))
        (track-talks (seq-filter (lambda (o) (string= (plist-get o :track)
                                                      (plist-get track :name)))
                                 info))
        )
    (emacsconf-replace-plist-in-string
     (append
      (list :links (concat
                    "<a href=\"#watch\">Watch</a> - <a href=\"#links\">Pad and Q&amp;A links</a> - <a href=\"#chat\">Chat</a> - <a href=\"#sched\">View schedule</a> - \nStreams: "
                    )
            :sched (with-temp-buffer
                     (svg-print (emacsconf-schedule-svg 800 300 info))
                     (buffer-string))
            :title-info (emacsconf-surround "<h1>" (plist-get track :title) "</h1>" "")
            :year emacsconf-year
						:base-url emacsconf-base-url
            :brief (emacsconf-publish-schedule-short
                    track-talks)
            :stream-nav (concat "Tracks: " (mapconcat (lambda (tr)
                                                        (if (string= (plist-get tr :name) (plist-get track :name))
                                                            (format "<strong>%s</strong>" (plist-get track :name))
                                                          (format "<a href=\"/%s/watch/%s/\">%s</a>"
                                                                  emacsconf-year
                                                                  (plist-get tr :id)
                                                                  (plist-get tr :name))))
                                                      emacsconf-tracks
                                                      " - "))
            :talks
            (mapconcat (lambda (entry) (format "<h1>%s</h1>\n%s\n" (car entry)
                                               (mapconcat #'emacsconf-publish-schedule-line (cdr entry) "\n")))
                       (seq-group-by (lambda (o)
                                       (format-time-string "%A, %b %-e, %Y" (plist-get o :start-time) emacsconf-timezone))
                                     track-talks)
                       ""))
      track)
     (concat
      "<!-- Automatically generated by emacsconf-publish-watch-pages -->
<!--
[[!inline pages=\"internal(${year}/watch/announce)\" raw=\"yes\"]]
[[!meta title=\"${name} stream\"]]
[[!sidebar content=\"\"]] -->
${title-info}
<hr size=\"1\">
<div>"
      (emacsconf-publish-page-nav nav "watch")
      " | ${stream-nav} | <a href=\"${base-url}${year}/watch/\">Tips for watching/participating</a></div>

For better performance, we recommend watching <a href=\"${stream-hires}\">${stream-hires}</a> using a streaming media player. Examples:

<ul>
<li>mpv ${stream-hires}</li>
<li>vlc ${stream-hires}</li>
<li>ffplay ${stream-hires}</li>
</ul>

If you have limited bandwidth, you can watch the low-res stream <a href=\"${480p}\">${480p}</a>.

If you don't have a streaming media player, you might be able to watch using the player below. (Google Chrome seems to be having issues; Mozilla Firefox might work better. If watching from a phone, Google Chrome seems to work there, or download VLC from your phone's app store and use the URLs like ${stream-hires} .)

<video controls class=\"reload\"><source src=\"${stream}\" type=\"video/webm\" /></video>

<hr size=\"1\"><div>" (emacsconf-publish-page-nav nav "links") " | ${stream-nav}</div>"
      "<div>${brief}</div>
<div class=\"pad-output\"></div>
<hr size=\"1\"><div>" (emacsconf-publish-page-nav nav "chat") " | ${stream-nav}</div>"
      "<div>Chat: <a href=\"${webchat-url}\">${channel}</a> on libera.chat</div>

<div class=\"chat-iframe\" data-track=\"${id}\"></div>
<iframe src=\"${webchat-url}\" height=\"600\" width=\"100%\"></iframe>
<hr size=\"1\"><div>" (emacsconf-publish-page-nav nav "sched") " | ${stream-nav}</div>"
      "
<ul>Legend:
<li>Solid lines: Q&A will be through a BigBlueButton room (you can ask questions there or through IRC/Etherpad)</li>
<li>Dashed lines: Q&A will be over IRC or the Etherpad, or the speaker will follow up afterwards</li></ul>
<div>Times are in Eastern Standard Time (America/Toronto, GMT-5). If you have Javascript enabled, clicking on talk pages should include times in your computer's local time setting.</div>
<div>${sched}</div>
<div>${talks}</div>
"))))

(defun emacsconf-publish-watch-pages ()
  "Update /year/watch pages."
  (interactive)
  (let* ((tracks
          (mapcar (lambda (track)
                    (append (list
                             :year emacsconf-year
                             :stream (concat emacsconf-stream-base (plist-get track :id) ".webm")
                             :stream-hires (concat emacsconf-stream-base (plist-get track :id) ".webm")
                             :480p (concat emacsconf-stream-base (plist-get track :id) "-480p.webm"))
                            track))
                  emacsconf-tracks))
         (reload-js "
<script>
 // @license magnet:?xt=urn:btih:90dc5c0be029de84e523b9b3922520e79e0e6f08&dn=cc0.txt txt CC0-1.0
 // Copyright (c) 2022 Sacha Chua - CC0 Public Domain
var video = document.querySelector('video.reload');
if (video) {
  var myVar = setInterval(reloadAsNeeded, 1000);
  var oldTime = '';
  function reloadAsNeeded() {
    if ((video.paused != true && (video.currentTime - oldTime) == 0 && video.currentTime != 0)) {
      var source = video.querySelector('source');
      var oldVideo = source.src;
      source.src = '';
      source.src = oldVideo;
      video.load();
      video.play();
    }
    oldTime = video.currentTime;
  };
}
// @license-end
</script>
")
         (info (emacsconf-publish-prepare-for-display (emacsconf-get-talk-info)))
         (emacsconf-publishing-phase 'conference)
         (emacsconf-use-absolute-url t))
    (when emacsconf-directory
      (emacsconf-publish-with-wiki-change
        (make-directory (expand-file-name "watch" (expand-file-name emacsconf-year emacsconf-directory)) t)
        (with-temp-file (expand-file-name "watch/info.md" (expand-file-name emacsconf-year emacsconf-directory))
          (insert "[[!sidebar content=\"\"]]" (emacsconf-publish-format-watch-index info)))
        (mapc (lambda (track)
                (with-temp-file (expand-file-name (format "%s/watch/%s.md" emacsconf-year (plist-get track :id))
                                                  emacsconf-directory)
                  (insert (emacsconf-publish-format-watch-track (append track (list :title "")) info))))
              tracks)))
    ;; Update live.emacsconf.org
    (when emacsconf-publish-watch-directory
      (make-directory (expand-file-name "watch" (expand-file-name emacsconf-year emacsconf-publish-watch-directory)) t)
      (with-temp-file (expand-file-name "watch/index.html" (expand-file-name emacsconf-year emacsconf-publish-watch-directory))
        (insert
				 (emacsconf-replace-plist-in-string
					(list
					 :conf-name emacsconf-name
					 :year emacsconf-year
					 :watch-index (emacsconf-publish-format-watch-index info)
					 )
					"<html><head><title>Watch ${conf-name} ${year}</title><link rel=\"stylesheet\" type=\"text/css\" href=\"/style.css\"></link></head><body>
<h1>${conf-name} ${year}</h1>
${watch-index}
<p>
Times are in Eastern Standard Time (America/Toronto, GMT-5). If you have Javascript enabled, clicking on talk pages should include times in your computer's local time setting.</p>

        <p>Depending on which media player you use, you may enter the stream address
        in a graphical user interface or provide it as an argument to the program
        when launching it from the terminal.
      </p>
      <p>
        Examples:
      </p>
<pre>mpv https://live0.emacsconf.org/gen.webm
vlc https://live0.emacsconf.org/gen.webm
ffplay https://live0.emacsconf.org/gen.webm
</pre>

<p>If you experience any disruptions, try reloading the page you're using
to watch the video. If that still doesn't work, please check our
status page at <a href=\"https://status.emacsconf.org\">https://status.emacsconf.org</a> for updates on the
status of various parts of our infrastructure, and instructions on how
to get in touch with us about disruptions.</p>

Pre-recorded videos and replays will also be available on Toobnix in
the <a href=\"https://toobnix.org/c/emacsconf\">EmacsConf channel</a>.</p>

<p>To participate in the Q&A, please check the talk page for the Q&A
details, including the Etherpad link, IRC channel, and optionally
a BigBlueButton room (BBB) for Q&A. If you plan to participate in
Q&A in the BigBlueButton room, please use headphones or earphones
in order to minimize audio feedback. The link on the talk page
will take you to a waiting room that will automatically refresh
when the host has opened the Q&A.</p>
</body></html>")))
      (mapc (lambda (track)
              (make-directory (expand-file-name (format "%s/watch/%s" emacsconf-year (plist-get track :id)) emacsconf-publish-watch-directory) t)
              (make-directory (expand-file-name (format "%s/watch/%s-480p" emacsconf-year (plist-get track :id)) emacsconf-publish-watch-directory) t)
              (with-temp-file (expand-file-name (format "%s/watch/%s/index.html" emacsconf-year (plist-get track :id))
                                                emacsconf-publish-watch-directory)
                (insert
                 (emacsconf-replace-plist-in-string
                  track
                  "<html><head><title>Watch EmacsConf ${name} track</title><link rel=\"stylesheet\" type=\"text/css\" href=\"/style.css\"></link></head><body>")
                 (emacsconf-publish-format-watch-track (append track
                                                               (list :title
                                                                     (emacsconf-replace-plist-in-string
                                                                      track
                                                                      "EmacsConf ${year}: ${name} track")))
                                                       info)
                 reload-js
                 "</body></html>"))
              (with-temp-file (expand-file-name (format "%s/watch/%s-480p/index.html" emacsconf-year (plist-get track :id))
                                                emacsconf-publish-watch-directory)
                (insert
                 (emacsconf-replace-plist-in-string
                  track
                  "<html><head><title>Watch EmacsConf ${name} track (low-res)</title><link rel=\"stylesheet\" type=\"text/css\" href=\"/style.css\"></link></head><body>")
                 (emacsconf-publish-format-watch-track
                  (append (list :stream (plist-get track :480p)
                                :title (emacsconf-replace-plist-in-string track "EmacsConf ${year}: ${name} track (low-res)"))
                          track)
                  info)
                 reload-js
                 "</body></html>")))
            tracks))))


(defvar emacsconf-publish-current-dir
	(expand-file-name
	 "current"
	 (expand-file-name
		emacsconf-year
		"/ssh:orga@media.emacsconf.org:/var/www/media.emacsconf.org"))
  "Directory to publish BBB redirects and current information to.")

;; (assert (eq (emacsconf-get-bbb-state '(:status "OPEN_Q")) 'open))
;; (assert (eq (emacsconf-get-bbb-state '(:status "TO_ARCHIVE")) 'after))

(defun emacsconf-publish-make-directories ()
	"Set up all the directories needed."
	(interactive)
	(dolist (dir
					 (list
						emacsconf-stream-asset-dir
						emacsconf-stream-overlay-dir
						emacsconf-cache-dir))
		(unless (file-directory-p dir)
			(make-directory dir t))))

(defun emacsconf-publish-bbb-static-redirects ()
  "Create emergency redirects that can be copied over the right location."
  (interactive)
  (mapc (lambda (state)
          (let ((emacsconf-publish-current-dir
                 (expand-file-name
                  state
                  (expand-file-name
									 "redirects"
									 (expand-file-name "assets" emacsconf-backstage-dir)))))
            (unless (file-directory-p emacsconf-publish-current-dir)
              (make-directory emacsconf-publish-current-dir t))
            (mapc
             (lambda (talk)
               (emacsconf-publish-bbb-redirect talk (intern state)))
             (emacsconf-publish-prepare-for-display (emacsconf-get-talk-info)))))
        '("before" "open" "after")))

(defun emacsconf-publish-bbb-redirect (talk &optional status)
	"Update the publicly-available redirect for TALK."
  (interactive (list (emacsconf-complete-talk-info)))
  (let ((bbb-filename (expand-file-name (format "bbb-%s.html" (plist-get talk :slug))
                                        emacsconf-publish-current-dir))
        (bbb-redirect-url (concat "https://media.emacsconf.org/" emacsconf-year "/current/bbb-" (plist-get talk :slug) ".html"))
        (status (or status (emacsconf-bbb-status (if (boundp 'org-state) (append (list :status org-state) talk) talk)))))
    (with-temp-file bbb-filename
      (insert
       (emacsconf-replace-plist-in-string
        (append talk (list :base-url emacsconf-base-url :bbb-redirect-url bbb-redirect-url))
        (pcase status
          ('before
           "<html><head><meta http-equiv=\"refresh\" content=\"5; URL=${bbb-redirect-url}\"></head><body>
The Q&A room for ${title} is not yet open. This page will refresh every 5 seconds until the BBB room is marked as open, or you can refresh it manually.</body></html>")
          ('open
           "<html><head><meta http-equiv=\"refresh\" content=\"0; URL=${bbb-room}\"></head><body>
The live Q&A room for ${title} is now open. You should be redirected to <a href=\"${bbb-room}\">${bbb-room}</a> automatically, but if not, please visit the URL manually to join the Q&A.</body></html>")
          ('after
           "<html><head><body>
The Q&A room for ${title} has finished. You can find more information about the talk at <a href=\"${base-url}${url}\">${base-url}${url}</a>.</body></html>")
          (_
           "<html><head><body>
There is no live Q&A room for ${title}. You can find more information about the talk at <a href=\"${base-url}${url}\">${base-url}${url}</a>.</body></html>"
           )))))))

(defun emacsconf-publish-media-files-on-change (talk)
  "Publish the files and update the index."
  (interactive (list (emacsconf-complete-talk-info)))
  (let ((org-state (if (boundp 'org-state) org-state (plist-get talk :status))))
    (if (plist-get talk :public)
        ;; Copy main files from backstage to public
        (let ((public-files (emacsconf-publish-filter-public-files talk)))
          (mapc (lambda (file)
                  (when (not (file-exists-p (expand-file-name file emacsconf-public-media-directory)))
                    (copy-file (expand-file-name file emacsconf-backstage-dir)
                               (expand-file-name file emacsconf-public-media-directory) t)))
                public-files))
      ;; Remove files from public
      (let ((files (directory-files emacsconf-public-media-directory nil
                                    (concat "^"
                                            (regexp-quote (plist-get talk :file-prefix)
                                                          )))))
        (mapc (lambda (file)
                (delete-file (expand-file-name file emacsconf-public-media-directory)))
              files)))
    (emacsconf-publish-public-index)
		(emacsconf-publish-playlist
		 (expand-file-name "index.m3u" emacsconf-public-media-directory)
     (concat emacsconf-name " " emacsconf-year)
     (emacsconf-public-talks (emacsconf-get-talk-info)))))

(defun emacsconf-publish-bbb-redirect-all ()
  (interactive)
  (unless (file-directory-p emacsconf-publish-current-dir)
    (make-directory emacsconf-publish-current-dir))
  (mapc #'emacsconf-publish-bbb-redirect (emacsconf-publish-prepare-for-display (emacsconf-get-talk-info))))
;; (emacsconf-publish-bbb-redirect '(:slug "test" :status "TO_STREAM" :bbb-room "https://bbb.emacsverse.org/b/sac-fwh-pnz-ogz" :title "Test room" :q-and-a "live" :url "2022/talks/test"))
;; (emacsconf-publish-bbb-redirect '(:slug "test" :status "CLOSED_Q" :bbb-room "https://bbb.emacsverse.org/b/sac-fwh-pnz-ogz" :title "Test room" :q-and-a "live" :url "2022/talks/test"))
;; (emacsconf-publish-bbb-redirect '(:slug "test" :status "OPEN_Q" :bbb-room "https://bbb.emacsverse.org/b/sac-fwh-pnz-ogz" :title "Test room" :q-and-a "live" :url "2022/talks/test"))
;; (emacsconf-publish-bbb-redirect '(:slug "test" :status "UNSTREAMED_Q" :bbb-room "https://bbb.emacsverse.org/b/sac-fwh-pnz-ogz" :title "Test room" :q-and-a "live" :url "2022/talks/test"))
;; (emacsconf-publish-bbb-redirect '(:slug "test" :status "TO_ARCHIVE" :bbb-room "https://bbb.emacsverse.org/b/sac-fwh-pnz-ogz" :title "Test room" :q-and-a "live" :url "2022/talks/test"))

;;; Toobnix

(defun emacsconf-publish-video-description (talk &optional include-title)
	(interactive (list (emacsconf-complete-talk-info) t))
	(let ((result
				 (emacsconf-replace-plist-in-string
					(append (list :conf-name emacsconf-name :year emacsconf-year
												:chapters
												(let ((chapters (subed-parse-file (expand-file-name (concat (plist-get talk :file-prefix) "--main--chapters.vtt") emacsconf-cache-dir))))
													(if chapters
															(concat
															 (mapconcat (lambda (chapter)
																						(concat (format-seconds "%.2h:%z%.2m:%.2s" (floor (/ (elt chapter 1) 1000)))
																										" " (elt chapter 3) "\n"))
																					chapters
																					"")
															 "\n")
														"")))
									talk)
					(concat
					 (if include-title
							 "${conf-name} ${year}: ${title} - ${speakers-with-pronouns}\n"
						 "")
					 "${absolute-url}

${chapters}You can view this and other resources using free/libre software at ${absolute-url} .
This video is available under the terms of the Creative Commons Attribution-ShareAlike 4.0 International (CC BY-SA 4.0) license."))))
		(when (called-interactively-p 'any)
			(kill-new result)
			(emacsconf-with-talk-heading talk))
		result))

(defvar emacsconf-publish-toobnix-upload-command "peertube-cli")
(defvar emacsconf-publish-toobnix-channel "EmacsConf")
;; (defun emacsconf-publish-get-toobnix-token ()
;; 	(let ((secrets (plz 'get "https://toobnix.org/api/v1/oauth-clients/local" :as #'json-read)))

;; 		)

;; 	)
(defun emacsconf-publish-upload-to-toobnix (properties)
	"Uses peertube-cli: https://github.com/Chocobozzz/PeerTube/blob/develop/support/doc/tools.md"
	(with-temp-buffer
		(let ((arguments
					 (append
						(list "upload" "-f" (plist-get properties :file))
						(when (plist-get properties :title)
							(list "-n" (plist-get properties :title)))
						(when (plist-get properties :description)
							(list "-d" (plist-get properties :description)))
						(list "-L" "en"
									"-C" emacsconf-publish-toobnix-channel
									"-l" "2"
									"-c" "15"
									"-P" (if (string= (plist-get properties :privacy) "public") "1" "2") "-t"
									(cond
									 ((stringp (plist-get properties :tags))
										(plist-get properties :tags))
									 ((listp (plist-get properties :tags))
										(string-join (plist-get properties :tags) ","))
									 (t "emacs"))))))
			(kill-new (mapconcat
								 #'shell-quote-argument
								 (append (list emacsconf-publish-toobnix-upload-command) arguments)
								 " "))
			(apply #'call-process
						 emacsconf-publish-toobnix-upload-command
						 nil t t arguments)
			(buffer-string))))
;; YouTube

(defun emacsconf-publish-spookfox-update-youtube-video ()
	(interactive)
	(require 'spookfox)
	;; Figure out which video this is
	(let* ((filename (spookfox-js-injection-eval-in-active-tab "document.querySelector('#original-filename').textContent.trim()" t))
				 (slug (emacsconf-get-slug-from-string filename))
				 (talk (emacsconf-resolve-talk slug))
				 (properties (emacsconf-publish-talk-video-properties talk 'youtube)))
		(kill-new (plist-get properties :title))
		(shell-command "xdotool search --name \"Channel content\" windowactivate sleep 1 key Ctrl+Shift+v sleep 2")
		)

	)
(defvar emacsconf-publish-youtube-upload-command '("python3" "/home/sacha/vendor/youtube-upload/bin/youtube-upload"))

(defun emacsconf-publish-upload-to-youtube (properties)
	"Use youtube-upload to upload the talk based on PROPERTIES.
Tends to be quota-limited, though."
	(let ((arguments (append
										(cdr emacsconf-publish-youtube-upload-command)
										(when (plist-get properties :title)
											(list "--title" (plist-get properties :title)))
										(when (plist-get properties :description)
											(list "--description" (plist-get properties :description)))
										(delq
										 nil
										 (list
											(concat "--tags="
															(cond
															 ((stringp (plist-get properties :tags))
																(plist-get properties :tags))
															 ((listp (plist-get properties :tags))
																(string-join (plist-get properties :tags) ","))
															 (t "emacs")))
											(concat "--category=" (or (plist-get properties :category) "Science & Technology"))
											(concat "--privacy=" (or (plist-get properties :privacy) "unlisted"))
											(concat "--license=" (or (plist-get properties :license) "creativeCommon"))
											(when (plist-get properties :date)
												(concat "--recording-date="
																(format-time-string "%Y-%m-%dT%H:%M:%SZ"
																										(plist-get properties :date) t)))
											(concat "--default-language="
															(or (plist-get properties :lang) "en"))
											(concat "--default-audio-language="
															(or (plist-get properties :lang) "en"))
											(when (plist-get properties :playlist)
												(concat "--playlist=" (plist-get properties :playlist)))
											"--embeddable=true"))
										(list (plist-get properties :file)))))
		(kill-new (mapconcat 'shell-quote-argument (append (seq-take emacsconf-publish-youtube-upload-command 1)
																											 arguments) " "))
		(with-current-buffer (get-buffer-create "*YouTube*")
			(erase-buffer)
			(apply #'call-process
						 (car emacsconf-publish-youtube-upload-command)
						 nil t t
						 arguments)
			(display-buffer (current-buffer))
			(buffer-string))))

(defun emacsconf-publish-answers-title (talk &optional len)
	(let ((title (concat emacsconf-name " " emacsconf-year " Q&A: " (plist-get talk :title))))
		(if (or (null len) (< (length title) len))
				title
			(concat (substring title 0 (- len 3)) "..."))))

(defun emacsconf-publish-talk-description (talk platform)
	(let ((title (concat emacsconf-name " " emacsconf-year ": " (plist-get talk :title))))
		(concat
		 (if (< (length title) 100) "" (concat title "\n"))
		 (plist-get talk :speakers-with-pronouns) "\n\n"
		 (plist-get talk :absolute-url) "\n\n"
		 (if (emacsconf-talk-file talk "--main--chapters.vtt")
				 (let ((chapters (subed-parse-file (emacsconf-talk-file talk "--main--chapters.vtt"))))
					 (concat
						(mapconcat
						 (lambda (chapter)
							 (concat
								(if (= (elt chapter 1) 0)
										"00:00"
									(format-seconds "%.2h:%z%.2m:%.2s" (floor (/ (elt chapter 1) 1000))))
								" " (elt chapter 3) "\n"))
						 chapters
						 "")
						"\n"))
			 "")
		 "You can view this and other resources using free/libre software at " (plist-get talk :absolute-url) " .\n"
		 "This video is available under the terms of the Creative Commons Attribution-ShareAlike 4.0 International (CC BY-SA 4.0) license.\n")))

(defun emacsconf-publish-answers-description (talk platform)
	(let ((title (concat emacsconf-name " " emacsconf-year " Q&A: " (plist-get talk :title))))
		(concat
		 (if (< (length title) 100) "" (concat title "\n"))
		 (plist-get talk :speakers-with-pronouns) "\n\n"
		 "This is the Q&A for the talk at "
		 (plist-get talk
								(if (eq platform 'toobnix) :toobnix-url :youtube-url)) " .\n\n"
		 (if (emacsconf-talk-file talk "--answers--chapters.vtt")
				 (let ((chapters (subed-parse-file (emacsconf-talk-file talk "--answers--chapters.vtt"))))
					 (concat
						(mapconcat
						 (lambda (chapter)
							 (concat
								(if (= (elt chapter 1) 0)
										"00:00"
									(format-seconds "%.2h:%z%.2m:%.2s" (floor (/ (elt chapter 1) 1000))))
								" " (elt chapter 3) "\n"))
						 chapters
						 "")
						"\n"))
			 "")
		 "You can view this and other resources using free/libre software at " (plist-get talk :absolute-url) " .\n"
		 "This video is available under the terms of the Creative Commons Attribution-ShareAlike 4.0 International (CC BY-SA 4.0) license.\n")))
;; (emacsconf-publish-answers-description (emacsconf-resolve-talk "async") 'toobnix)

(defvar emacsconf-publish-talk-video-tags (format "emacs,%s,%s%s" emacsconf-id emacsconf-id emacsconf-year)
	"Comma-separated tags to add to the talk videos.")

(defun emacsconf-publish-talk-video-properties (talk platform)
	(let ((title (concat emacsconf-name " " emacsconf-year ": "
											 (plist-get talk :title) " - " (plist-get talk :speakers))))
		(list
		 :file (emacsconf-talk-file talk "--main.webm")
		 :tags emacsconf-publish-talk-video-tags
		 :playlist (concat emacsconf-name " " emacsconf-year)
		 :date (plist-get talk :start-time)
		 :privacy (if (plist-get talk :public) "public" "unlisted")
		 :title (if (< (length title) 100) title (concat (substring title 0 97) "..."))
		 :description (emacsconf-publish-talk-description talk platform))))

(defun emacsconf-publish-answers-video-properties (talk platform)
	(let ((title (concat emacsconf-name " " emacsconf-year " Q&A: "
											 (plist-get talk :title) " - " (plist-get talk :speakers))))
		(list
		 :file (emacsconf-talk-file talk "--answers.webm")
		 :tags emacsconf-publish-talk-video-tags
		 :playlist (concat emacsconf-name " " emacsconf-year)
		 :date (plist-get talk :start-time)
		 :privacy (if (plist-get talk :public) "public" "unlisted")
		 :title (if (< (length title) 100) title (concat (substring title 0 97) "..."))
		 :description (emacsconf-publish-answers-description talk platform))))

(defun emacsconf-publish-upload-talk (talk platform)
	(interactive
	 (list (emacsconf-complete-talk-info)
				 (intern (completing-read "Platform: " '("youtube" "toobnix")))))
	(let ((file (emacsconf-talk-file talk "--main.webm"))
				output)
		(when (and file (not (plist-get talk (if (eq platform 'toobnix) :toobnix-url :youtube-url))))
			(setq output
						(funcall
						 (if (eq platform 'toobnix)
								 #'emacsconf-publish-upload-to-toobnix
							 #'emacsconf-publish-upload-to-youtube)
						 (emacsconf-publish-talk-video-properties talk platform)))
			(when (and (string-match "Video URL: \\(.*+\\)" output) (eq platform 'youtube))
				(setq output (match-string 1 output))
				(save-window-excursion
					(emacsconf-go-to-talk talk)
					(org-entry-put (point) "YOUTUBE_URL" output)))
			output)))

(defun emacsconf-publish-upload-answers (talk platform)
	(interactive (list (emacsconf-complete-talk-info)
										 (intern (completing-read "Platform: " '("youtube" "toobnix")))))
	(let ((file (emacsconf-talk-file talk "--answers.webm"))
				(title (concat emacsconf-name " " emacsconf-year " Q&A: " (plist-get talk :title)))
				output)
		(when (and file (not (plist-get talk (if (eq platform 'toobnix) :qa-toobnix :qa-youtube))))
			(setq output
						(funcall
						 (if (eq platform 'toobnix)
								 #'emacsconf-publish-upload-to-toobnix
							 #'emacsconf-publish-upload-to-youtube)
						 (list
							:file file
							:tags "emacs,emacsconf,answers"
							:playlist (concat emacsconf-name " " emacsconf-year)
							:date (plist-get talk :start-time)
							:title (if (< (length title) 100) title (concat (substring title 0 97) "..."))
							:description (emacsconf-publish-answers-description talk platform))))
			(when (string-match "Video URL: \\(.*+\\)" output)
				(setq output (match-string 1 output))
				(save-window-excursion
					(emacsconf-go-to-talk talk)
					(org-entry-put (point) "QA_YOUTUBE" output)))
			output)))

(defun emacsconf-publish-upload-answers-to-youtube (talk)
	(let ((file (emacsconf-talk-file talk "--answers.webm"))
				(title (concat emacsconf-name " " emacsconf-year " Q&A: " (plist-get talk :title)))
				output)
		(when (and file (not (plist-get talk :qa-youtube)))
			(setq output
						(emacsconf-publish-upload-to-youtube
						 (list
							:file file
							:tags "emacs,emacsconf,answers"
							:playlist (concat emacsconf-name " " emacsconf-year)
							:date (plist-get talk :start-time)
							:title (if (< (length title) 100) title (concat (substring title 0 97) "..."))
							:description (emacsconf-publish-answers-description talk 'youtube)
							)))
			(when (string-match "Video URL: \\(.*+\\)" output)
				(setq output (match-string 1 output))
				(save-window-excursion
					(emacsconf-go-to-talk talk)
					(org-entry-put (point) "QA_YOUTUBE" output)))
			output)))

(defun emacsconf-publish-talk-file ()
	(interactive)
	(emacsconf-upload-to-backstage)
	(mapc (lambda (file)
          (copy-file file (expand-file-name (file-name-nondirectory file)
                                            emacsconf-public-media-directory)
                     t))
        (or (dired-get-marked-files)
            (list (buffer-file-name))))
	(mapc #'emacsconf-publish-update-talk
				(seq-uniq (seq-map #'emacsconf-get-slug-from-string (or (dired-get-marked-files)
																																(list (buffer-file-name))))))
	(emacsconf-publish-public-index)
	(emacsconf-publish-backstage-index))

(defun emacsconf-publish-process-answers-chapters (file)
	(interactive (list
								(if (string-match "chapters" (buffer-file-name))
										(buffer-file-name)
									(read-file-name "Chapters file: "))))
	(let ((slug (emacsconf-get-slug-from-string (file-name-nondirectory file))))
		(emacsconf-with-talk-heading slug
			(when (string= (org-get-todo-state) "TO_INDEX_QA")
				(org-todo "TO_CAPTION_QA")))
		(unless (string= (expand-file-name file)
										 (expand-file-name (file-name-nondirectory file)
																			 emacsconf-cache-dir))
			(copy-file file (expand-file-name (file-name-nondirectory file)
																				emacsconf-cache-dir)
								 t))
		(chmod file #o644)
		(copy-file file (expand-file-name (file-name-nondirectory file)
																			emacsconf-public-media-directory)
							 t)
		(copy-file file (expand-file-name (file-name-nondirectory file)
																			emacsconf-backstage-dir)
							 t)
		(emacsconf-publish-update-talk slug)))

(defun emacsconf-publish-intros-to-backstage ()
	(interactive)
	(dolist (talk (emacsconf-get-talk-info))
		(when (file-exists-p (expand-file-name (concat (plist-get talk :slug) ".webm")
																					 (expand-file-name "intros" emacsconf-stream-asset-dir)))
			(copy-file (expand-file-name (concat (plist-get talk :slug) ".webm")
																	 (expand-file-name "intros" emacsconf-stream-asset-dir))
								 (expand-file-name (concat (plist-get talk :file-prefix) "--intro.webm")
																	 emacsconf-backstage-dir)
								 t))))
;;
(provide 'emacsconf-publish)
