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

(defcustom emacsconf-main-extensions '("--main.webm" "--main.opus" "--main.org" ".org" ".odp" ".pdf" ".el" "--compressed56.webm" "--main.vtt" "--main_fr.vtt" "--main_ja.vtt" "--main_es.vtt" "--main--chapters.vtt" "--script.fountain" "--main.pdf" "--slides.pdf")
  "Extensions to list on public pages."
  :type '(repeat string)
  :group 'emacsconf)

(defcustom emacsconf-backstage-extensions '(".en.srv2" ".srt")
  "Extensions to list in the staging area."
  :group 'emacsconf)
(defcustom emacsconf-public-media-directory (concat "/ssh:orga@media.emacsconf.org:/var/www/media.emacsconf.org/" emacsconf-year "/")
  "Can be over TRAMP" :type 'string :group 'emacsconf)

(defun emacsconf-publish-info-pages-for-talk (talk)
"Publish the before and after pages for this talk."
  (interactive (list (emacsconf-complete-talk-info)))
  (let ((info (emacsconf-get-talk-info)))
    (emacsconf-publish-before-page talk info)
    (emacsconf-publish-after-page talk info)))

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
  (magit-status-setup-buffer emacsconf-directory))

(defun emacsconf-update-conf-html ()
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

  
(defun emacsconf-regenerate-wiki (&optional force)
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
    (when (functionp 'emacsconf-pentabarf-generate)
      (emacsconf-pentabarf-generate))))

(defun emacsconf-update-and-publish ()
  (interactive)
  (with-current-buffer (find-file-noselect emacsconf-org-file)
    (emacsconf-update-schedules)
    (emacsconf-upcoming-update-file)
    (emacsconf-update-schedules-in-wiki)
    (emacsconf-update-conf-html)
    (setq emacsconf-info (emacsconf-get-talk-info))))

(defun emacsconf-update-media ()
  (interactive)
  (emacsconf-publish-public-index-on-wiki)
  (when emacsconf-public-media-directory
    (emacsconf-publish-public-index (expand-file-name "index.html" emacsconf-public-media-directory))
		(emacsconf-publish-playlist (expand-file-name "index.m3u" emacsconf-public-media-directory)
                                 (concat emacsconf-name emacsconf-year)
                                 (emacsconf-public-talks emacsconf-info)
                                 (format "https://media.emacsconf.org/%s/" emacsconf-year)))
  (when emacsconf-backstage-dir
    (emacsconf-publish-backstage-index (expand-file-name "index.html" emacsconf-backstage-dir)))
  (emacsconf-publish-playlist (expand-file-name "index.m3u" emacsconf-backstage-dir)
                               (concat emacsconf-name emacsconf-year)
                               (emacsconf-filter-talks emacsconf-info)
                               (format "https://media.emacsconf.org/%s/backstage/" emacsconf-year)))

(defun emacsconf-index-card (talk &optional extensions)
  "Format an HTML card for TALK, linking the files in EXTENSIONS."
  (let* ((video-slug (plist-get talk :video-slug))
         (video-file (plist-get talk :video-file))
         (video (and video-slug
                     (emacsconf-index-card-video
                      (or (plist-get talk :video-id)
                          (concat (plist-get talk :slug) "-mainVideo"))
                      video-file talk extensions))))
    ;; Add extra information to the talk
    (setq talk
          (append
           talk
           (list
            :video-html (or (plist-get video :video) "")
            :audio-html (or (plist-get video :audio) "")
            :chapter-list (or (plist-get video :chapter-list) "")
            :resources (or (plist-get video :resources) "")
            :extra (or (plist-get talk :extra) "") 
            :speaker-info (or (plist-get talk :speakers-with-pronouns) ""))))
    (emacsconf-replace-plist-in-string
     talk
     "<div class=\"vid\">${video-html}${audio-html}<div>${extra}</div>${resources}${chapter-list}</div>")))

;; (emacsconf-publish-format-track-as-org (car emacsconf-tracks) "US/Eastern")
;; (emacsconf-get-talk-info)
(defun emacsconf-publish-format-track-as-org (track tz &optional info)
  (setq info (or info (emacsconf-publish-prepare-for-display (emacsconf-get-talk-info))))
  (concat
   "** " (plist-get track :name) "  :" (plist-get track :id) ":\n:PROPERTIES:\n:CATEGORY: " (plist-get track :id) "\n:END:\n"
   (mapconcat
    (lambda (talk)
      (concat
       "*** " (plist-get talk :title) "\n"
       "<" (format-time-string
                       (cdr org-time-stamp-formats)
                       (plist-get talk :start-time)
                       tz)
       ">\n"
       (emacsconf-surround "- " (plist-get talk :speakers-with-pronouns) "\n" "")
       (emacsconf-surround "- " (plist-get talk :absolute-url) "\n" "")
       (emacsconf-surround "- Etherpad: " (plist-get talk :pad-url) "\n" "")
       (emacsconf-surround "- Q&A: " (plist-get talk :qa-info) "\n" "")
       (emacsconf-surround "\n" (plist-get talk :intro-note) "\n" "")))
    (emacsconf-filter-talks-by-track track info)
    "\n")))

(defun emacsconf-publish-schedule-org-for-timezone (timezone &optional info)
  (interactive (list (completing-read "Timezone: " emacsconf-timezones)))
  (let ((new-filename (expand-file-name
                       (concat "schedule-"
                               (replace-regexp-in-string
                                "[^a-z]+" "-"
                                (downcase timezone))
                               ".org")
                       (expand-file-name "schedules"
                                         emacsconf-public-media-directory))))
    (unless (file-directory-p (file-name-directory new-filename))
      (make-directory (file-name-directory new-filename)))
    (with-temp-file new-filename
      (insert
       "* " emacsconf-name " " emacsconf-year "\n\nTimes are in " timezone " timezone. You can find this file and other calendars at "
       emacsconf-media-base-url emacsconf-year "/schedules/ .\n\n"
       (mapconcat (lambda (track)
                    (emacsconf-publish-format-track-as-org track timezone info))
                  emacsconf-tracks
                  "\n")))))

(defun emacsconf-publish-schedule-org-files (&optional info)
  (interactive)
  (setq info (or info (emacsconf-publish-prepare-for-display (emacsconf-get-talk-info))))
  (mapc (lambda (tz) (emacsconf-publish-schedule-org-for-timezone tz info))
        emacsconf-timezones))

(defun emacsconf-publish-format-res-talks (info)
  (mapconcat
   (lambda (o)
     (concat
      "<tr>"
      (format
       "<td><a name=\"%s\"></a>"
       (plist-get o :slug))
      (plist-get o :qa-link)
      "</td>"
      "<td>" (if (plist-get o :pad-url)
                 (format "<a href=\"%s\" target=\"_blank\" rel=\"noreferrer\">Pad</a>" (plist-get o :pad-url))
               "")
      "</td>"
      "<td>" (format "<a href=\"%s\" target=\"_blank\" rel=\"noreferrer\">Chat</a>" (plist-get o :webchat-url))
      ""
      "</td>"
      "<td>" (format-time-string "%-l:%M" (plist-get o :start-time) emacsconf-timezone) "</td>"
      "<td>" (or (plist-get o :slug) "") "</td>"
      "<td>" (or (plist-get o :title) "") "</td>"
      "</tr>"))
   info
   "\n"))
(defun emacsconf-publish-res-index ()
  "Publish BBB room URLs and pad links for volunteer convenience."
  (interactive)
  (let ((emacsconf-use-absolute-url t)
        (emacsconf-base-url "")
        (info (mapcar (lambda (o)
                        (append (list
                                 :url (concat "#" (plist-get o :slug)))
                                (if (and (string-match "live" (or (plist-get o :q-and-a) ""))
                                         (plist-get o :bbb-room))
                                    (append (list
                                             :qa-link
                                             (format "<a href=\"%s\" target=\"_blank\" rel=\"noreferrer\">Join Q&A</a>" (plist-get o :bbb-room)))
                                            o)
                                  o)))
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
              (with-temp-buffer
                (svg-print (emacsconf-schedule-svg 800 300 info))
                (buffer-string))
              "</div><h1>"
              (plist-get track :name)
              "</h1><table>"
              "<tr><th colspan=\"6\" style=\"text-align: left\">Saturday</th></tr>"
              (emacsconf-publish-format-res-talks
               (emacsconf-publish-prepare-for-display
                (emacsconf-filter-talks-by-time
                 (format "2022-12-03T08:00:00%s" emacsconf-timezone-offset)
                 (format "2022-12-03T18:00:00%s" emacsconf-timezone-offset)
                 track-talks)))
              "<tr><th colspan=\"6\" style=\"text-align: left\">Sunday</th></tr>"
              (emacsconf-publish-format-res-talks
               (emacsconf-publish-prepare-for-display
                (emacsconf-filter-talks-by-time
                 (format "2022-12-04T08:00:00%s" emacsconf-timezone-offset)
                 (format "2022-12-04T18:00:00%s" emacsconf-timezone-offset)
                 track-talks)))
              "</table></body></html>")))
         (with-temp-file (expand-file-name (format "index-%s.html" (plist-get track :id)) emacsconf-res-dir)
           (insert result))
         (with-temp-file (expand-file-name (format "index-%s.html" (plist-get track :id)) emacsconf-backstage-dir)
           (insert result))))
     emacsconf-tracks)))

(defun emacsconf-index-card-video (video-id video-file talk extensions)
  (let* ((video-base (and (stringp video-file) (replace-regexp-in-string "reencoded\\|original" "main" (file-name-base video-file))))
         (chapter-info (and (stringp video-file)
                            (emacsconf-make-chapter-strings
														 (plist-get talk :chapter-file)
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
            (and (stringp video-file)
                 (plist-get talk :captions-edited)
                 (let ((tracks
                        (emacsconf-video-subtitle-tracks
												 (or (plist-get talk :caption-file)
														 (concat (replace-regexp-in-string "reencoded\\|original" "main"
																															 video-base)
																		 ".vtt"))
                         (or (plist-get talk :track-base-url)
                             (plist-get talk :base-url))
                         (plist-get talk :files))))
                   (cond
                    ((zerop (length tracks)) "")
                    ((eq (plist-get talk :format) 'wiki) (format "captions=\"\"\"%s\"\"\"" tracks))
                    (t tracks))))
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
            :other-files
            (mapconcat
             (lambda (s)
               (concat "<li>" s "</li>"))
             (emacsconf-link-file-formats-as-list talk (or extensions emacsconf-main-extensions))
             "")
            :toobnix-info (if (plist-get talk :toobnix-url)
                              (format
                               "<li><a href=\"%s\">View on Toobnix</a></li>"
                               (plist-get talk :toobnix-url))
                            "")
            :transcript-link
            (if (and
                 (plist-get talk :public)
                 (plist-get talk :captions-edited))
                (format "[View transcript](%s#%s-transcript)  \n" (plist-get talk :absolute-url) video-id)
              ""))
           talk)))
    (list
     :video
     (emacsconf-replace-plist-in-string
      info
			(if (stringp video-file)
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
      "<div class=\"files resources\"><ul>${other-files}${toobnix-info}</ul></div>"))))

(defun emacsconf-format-public-email (o &optional email)
  (format "[%s](mailto:%s?subject=%s)"
          (or email (plist-get o :public-email))
          (or email (plist-get o :public-email))
          (url-hexify-string (format "Comment for EmacsConf 2022 %s: %s" (plist-get o :slug) (plist-get o :title)))))

(defun emacsconf-format-speaker-info (o)
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
                      :speaker-info (emacsconf-format-speaker-info o)
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
		("TO_ACCEPT" nil)
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
   (emacsconf-index-card o
                         (if (plist-get o :captions-edited)
                             emacsconf-main-extensions
                           (remove "--main.vtt" emacsconf-main-extensions)))
   (if (plist-get o :qa-public)
       (concat "\n\n# Q&A\n\n"
               (emacsconf-index-card (append
                                      (list
                                       :public 1
                                       :video-id (concat (plist-get o :slug) "-qanda")
                                       :toobnix-url nil
																			 :captions-edited (plist-get o :qa-captions-edited)
                                       :video-file (emacsconf-talk-file o "--answers.webm")
																			 :audio-file (emacsconf-talk-file o "--answers.opus")
																			 :chapter-file (emacsconf-talk-file o "--answers--chapters.vtt"))
																			
                                      o)
                                     (list "--answers.webm" "--answers.vtt" "--answers--chapters.vtt" "--answers.opus")))
     "")))

(defun emacsconf-publish-webchat-link (o)
  (let ((track (emacsconf-get-track (plist-get o :track))))
    (format "<a href=\"%s\">#%s</a>"
						(plist-get track :webchat-url)
						(plist-get track :channel))))

(defvar emacsconf-publish-include-pads nil "When non-nil, include Etherpad info.")

(defun emacsconf-format-talk-schedule-info (o)
  (let ((friendly (concat "/" emacsconf-year "/talks/" (plist-get o :slug) ))
        (timestamp (org-timestamp-from-string (plist-get o :scheduled))))
    (emacsconf-replace-plist-in-string
     (append o
             (list
							:format
              (concat (or (plist-get o :video-time)
                          (plist-get o :time))
                      "-min talk"
                      (if (plist-get o :q-and-a)
                          (format " followed by %s Q&A%s"
                                  (plist-get o :q-and-a)
																	(if (eq emacsconf-publishing-phase 'conference)
																			(format " (%s)"
																							(if (string-match "live" (plist-get o :q-and-a))
																									(if (eq 'after (emacsconf-bbb-status o))
																											"done"
																										(format "<https://emacsconf.org/current/%s/room>" (plist-get o :slug)))
																								(emacsconf-publish-webchat-link o)))
																		""))
                        ""))
              :pad-info
              (if emacsconf-publish-include-pads
                  (format "Etherpad: <https://pad.emacsconf.org/%s-%s>  \n" emacsconf-year (plist-get o :slug))
                "")
              :irc-info
              (format "Discuss on IRC: [#%s](%s)  \n" (plist-get o :channel)
                      (plist-get o :webchat-url))
              :status-info
              (if (member emacsconf-publishing-phase '(cfp program schedule conference)) (format "Status: %s  \n" (plist-get o :status-label)) "")
              :schedule-info
              (if (and (member emacsconf-publishing-phase '(schedule conference))
                       (not (emacsconf-talk-all-done-p o))
                       (not (string= (plist-get o :status) "CANCELLED")))
                  (let ((start (org-timestamp-to-time (org-timestamp-split-range timestamp)))
                        (end (org-timestamp-to-time (org-timestamp-split-range timestamp t))))
                    (format
                     "<div>Times in different timezones:</div><div class=\"times\" start=\"%s\" end=\"%s\"><div class=\"conf-time\">%s</div><div class=\"others\"><div>which is the same as:</div>%s</div></div><div><a href=\"/%s/watch/%s/\">Find out how to watch and participate</a></div>"
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
     (concat
      "[[!toc  ]]
Format: ${format}  
${pad-info}${irc-info}${status-info}${schedule-info}\n" 
      (if (plist-get o :alternate-apac)
          (format "[[!inline pages=\"internal(%s/inline-alternate)\" raw=\"yes\"]]  \n" emacsconf-year)
        "")
      "\n"))))

(defun emacsconf-format-email-questions-and-comments (talk)
  (format "Questions or comments? Please e-mail %s"
          (emacsconf-format-public-email talk
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
			 (let ((filename (expand-file-name (concat (plist-get talk :video-slug) ext)
																				 (expand-file-name "captions" (expand-file-name emacsconf-year emacsconf-directory))))
						 (cached-file (expand-file-name (concat (plist-get talk :video-slug) ext) emacsconf-cache-dir)))
				 (when (and (file-exists-p cached-file)
										(or
										 (not (file-exists-p filename))
										 (file-newer-than-file-p cached-file filename)))
					 (copy-file cached-file filename t)
					 (shell-command (concat "git add " (shell-quote-argument filename))))))
		 (seq-filter (lambda (o) (string-match "vtt$" o)) emacsconf-main-extensions))))

(defun emacsconf-publish-format-talk-page-schedule (talk info)
	"Add the schedule image for TALK based on INFO."
	(concat
	 (if (member emacsconf-publishing-phase '(schedule conference))
			 (concat
				"\nThe following image shows where the talk is in the schedule for "
				(format-time-string "%a %Y-%m-%d" (plist-get talk :start-time) emacsconf-timezone) ". Solid lines show talks with Q&A via BigBlueButton. Dashed lines show talks with Q&A via IRC or Etherpad."
				(format "<div class=\"schedule-in-context schedule-svg-container\" data-slug=\"%s\">\n" (plist-get talk :slug))           
				(let* ((width 800) (height 150)
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
				"\n</div>\n\n")
		 "")
   (emacsconf-format-talk-schedule-info talk) "\n\n"))

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
			(insert (emacsconf-publish-format-talk-page-schedule talk info))
			(insert
			 (if (plist-get talk :public) (emacsconf-wiki-talk-resources talk) "")
			 "\n# Description\n"))
    (insert "<!-- End of emacsconf-publish-before-page -->")))


(defun emacsconf-format-transcript-from-list (subtitles paragraphs video-id &optional lang)
  "Return subtitle directives for SUBTITLES split by PARAGRAPHS."
  (when (stringp subtitles) (setq subtitles (subed-parse-file subtitles)))
  (when (stringp paragraphs) (setq paragraphs (subed-parse-file paragraphs)))
  (mapconcat
   (lambda (sub)
     (let ((msecs (elt sub 1)))
       (format "[[!template %stext=\"\"\"%s\"\"\" start=\"%s\" video=\"%s\" id=\"subtitle\"%s]]"
               (if (and paragraphs (>= msecs (elt (car paragraphs) 1)))
                   (progn
                     (while (and paragraphs (>= (elt sub 1) (elt (car paragraphs) 1)))
                       (setq paragraphs (cdr paragraphs)))
                     "new=\"1\" ")
                 "")
							 (replace-regexp-in-string "^#" "\\\\#"
															 (replace-regexp-in-string "\"" "&quot;" (elt sub 3)))
               (concat (format-seconds "%02h:%02m:%02s" (/ (floor msecs) 1000))
                       "." (format "%03d" (mod (floor msecs) 1000)))
               video-id
               (emacsconf-surround " lang=\"" lang "\"" ""))))
   subtitles "\n"))

(defun emacsconf-publish-format-transcript (talk &optional video-id lang)
  "Format the transcript for TALK, adding paragraph markers when possible."
	(require 'subed)  
  (let* ((chapters (plist-get talk :chapter-file))
         (subtitles
          (subed-parse-file (if lang
																(format "%s_%s.vtt"
																				(file-name-sans-extension
																				 (plist-get talk :caption-file))
																				lang)
															(plist-get talk :caption-file))))
         (pars (subed-parse-file chapters)))
    (if subtitles
        (format "<a name=\"%s-%s-transcript%s\"></a>
# %s

%s

"
								(plist-get talk :slug)
                (or video-id "mainVideo")
                (emacsconf-surround "-" lang "" "")
								(if lang (assoc-default lang emacsconf-publish-subtitle-languages) "Transcript")
								(emacsconf-format-transcript-from-list
                 subtitles pars (concat video-id "-" (plist-get talk :slug))))
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
     (if (plist-get talk :public)
				 (let ((transcripts
								(mapconcat
								 (lambda (lang)
									 (let ((filename
													(emacsconf-talk-file
													 talk
													 (if lang
															 (format "--main_%s.vtt" lang)
														 "--main.vtt"))))
										 (if (emacsconf-captions-edited-p filename) ; todo: cache this somewhere
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
						 ""))
			 "")
     (emacsconf-format-email-questions-and-comments talk) "\n"
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
                              (format "Track: <span class=\"sched-track %s\">%s</span>  \n" (plist-get o :track) (plist-get o :track))
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
          info)))

(defun emacsconf-publish-talks-page (&optional emacsconf-info)
	"Generate a list of talks."
  (interactive)
  (let ((info (or emacsconf-info
									(sort
									 (seq-filter #'emacsconf-publish-talk-p
															 (or emacsconf-schedule-draft (emacsconf-get-talk-info)))
									 #'emacsconf-sort-by-track-then-schedule))))
    (with-temp-file (expand-file-name "talk-details.md" (expand-file-name emacsconf-year emacsconf-directory))
      (insert (format "<table><thead><th>Duration</th><th>Title</th><th>Speakers</th></thead><tbody>%s</tbody></table>"
                      (mapconcat
                       (lambda (o)
                         (if (null (plist-get o :slug))
                             (format "<tr><td colspan=\"3\">%s</td></tr>" (emacsconf-format-talk-link o))
                           (format "<tr><td>%s</td><td>%s</td><td>%s</td><tr>" 
                                   (plist-get o :time)
                                   (emacsconf-format-talk-link o)
                                   (plist-get o :speakers))))
                       info "\n"))))))

(defun emacsconf-generate-main-schedule-with-tracks (&optional info)
  (interactive (list nil))
  (setq info (or info (emacsconf-publish-prepare-for-display info)))
  (with-temp-file (expand-file-name "schedule-details.md"
                                    (expand-file-name emacsconf-year emacsconf-directory))
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
  (let* ((by-day (emacsconf-by-day (emacsconf-publish-prepare-for-display info)))
         (cancelled (seq-filter (lambda (o) (string= (plist-get o :status) "CANCELLED")) info))
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

(defun emacsconf-publish-schedule (&optional info)
	"Generate the schedule or program."
  (interactive)
  (emacsconf-publish-schedule-svg-snippets)
	(setq info (or info (emacsconf-publish-prepare-for-display info)))
  (with-temp-file (expand-file-name "schedule-details.md" (expand-file-name emacsconf-year emacsconf-directory))
    (insert
     (if (member emacsconf-publishing-phase '(cfp program))
         (let ((sorted (emacsconf-publish-prepare-for-display (or info (emacsconf-get-talk-info)))))
					 (mapconcat (lambda (track)
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
														"<h1 id=\"%s\" class=\"sched-track %s\">%s (%d talks)</h1>\n%s"
														(plist-get track :id)
														(plist-get track :name)
														(plist-get track :name)
														(length track-talks)
														(emacsconf-publish-format-main-schedule track-talks)))))
											emacsconf-tracks "\n\n"))
       (emacsconf-publish-format-interleaved-schedule info))))
  (when (member emacsconf-publishing-phase '(cfp program))
    (with-temp-file (expand-file-name "draft-schedule.md" (expand-file-name emacsconf-year emacsconf-directory))
      (insert
       "This is a *DRAFT* schedule.\n"
       (let ((emacsconf-publishing-phase 'schedule))
         (emacsconf-publish-format-interleaved-schedule info))))))

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
														:time (plist-get o :time)))
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
														:pad (plist-get o :pad-url)
														:startutc (format-time-string "%FT%T%z" (plist-get o :start-time) t)
														:endutc (format-time-string "%FT%T%z" (plist-get o :end-time) t)
														:start (format-time-string "%-l:%M" (plist-get o :start-time) emacsconf-timezone)
														:end (format-time-string "%-l:%M" (plist-get o :end-time) emacsconf-timezone)))
													('resources
													 (list
														:pad nil
														:channel nil
														:resources (mapconcat (lambda (s) (concat "<li>" s "</li>"))
																									(emacsconf-link-file-formats-as-list
																									 (append o
																													 (list :base-url (format "%s%s/" emacsconf-media-base-url emacsconf-year)))
																									 (append emacsconf-main-extensions (list "--answers.webm" "--answers.opus" "--answers.vtt")))
																									""))))
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
                                   "video posted")))
                          ", ")
												 )
												)))
            (while attrs
              (let ((field (pop attrs))
                    (val (pop attrs)))
                (when val
                  (setq result (concat result " " (substring (symbol-name field) 1) "=\"\"\"" val "\"\"\"")))))
            result)))

(defun emacsconf-publish-format-main-schedule (info)
	"Include the schedule information for INFO."
  (mapconcat #'emacsconf-publish-sched-directive info "\n"))

(defun emacsconf-publish-talk-files (talk files)
  (seq-filter (lambda (o)
                (and (string-match (concat "^" (regexp-quote (plist-get talk :video-slug))) o)
                     (not (string= (plist-get talk :video-file) o))))
              files))

(defun emacsconf-sum (field talks)
  (apply '+ (seq-map (lambda (talk) (string-to-number (or (plist-get talk field) "0"))) talks)))

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

(defun emacsconf-publish-backstage-processing (by-status files)
	(let ((list (append
							 (assoc-default "TO_PROCESS" by-status)
							 (assoc-default "PROCESSING" by-status)
							 (assoc-default "TO_AUTOCAP" by-status))))
		(format "<h1>%s talk(s) being processed (%d minutes)</h1>Not ready for captioning yet, but they will be eventually<ul class=\"videos\">%s</ul>"
						(length list)
						(emacsconf-sum :video-time list)
						(mapconcat
						 (lambda (f)
							 (setq f (append
												f
												(list :extra
															(if (plist-get f :caption-note) (concat "<div class=\"caption-note\">" (plist-get f :caption-note) "</div>") "")
															:files
															(emacsconf-publish-talk-files f files))))
							 (format "<li><a name=\"%s\"></a><strong><a href=\"%s\">%s</a></strong><br />%s (id:%s)<br />%s</li>"
											 (plist-get f :slug)
											 (plist-get f :absolute-url)
											 (plist-get f :title)
											 (plist-get f :speakers)
											 (plist-get f :slug)
											 (emacsconf-index-card f)))
						 list
						 "\n"))))

(defun emacsconf-publish-backstage-to-assign (by-status files)
	(let ((list (assoc-default "TO_ASSIGN" by-status)))
    (format "<h1>%s talk(s) to be captioned (%d minutes)</h1><p>You can e-mail <a href=\"mailto:sacha@sachachua.com\">sacha@sachachua.com</a> to call dibs on editing the captions for one of these talks. This year, we're experimenting with using OpenAI Whisper to provide auto-generated VTT that you can use as a starting point. If you're writing them from scratch, you can choose to include timing information, or we can probably figure them out afterwards with a forced alignment tool. More info: <a href=\"https://media.emacsconf.org/2022/backstage/editing-captions.html\">Editing captions</a>, <a href=\"https://emacsconf.org/captioning/\">captioning tips</a></p><ul class=\"videos\">%s</ul>"
            (length list)
            (emacsconf-sum :video-time list)
            (mapconcat
             (lambda (f)
               (setq f (append
                        f
                        (list :extra
                              (if (plist-get f :caption-note) (concat "<div class=\"caption-note\">" (plist-get f :caption-note) "</div>") "")
                              :files
                              (emacsconf-publish-talk-files f files))))
               (format  "<li><a name=\"%s\"></a><strong><a href=\"%s\">%s</a></strong><br />%s (id:%s)<br />%s</li>"
                        (plist-get f :slug)
                        (plist-get f :absolute-url)
                        (plist-get f :title)
                        (plist-get f :speakers)
                        (plist-get f :slug)
                        (emacsconf-index-card f)))
             list
             "\n"))))

(defun emacsconf-publish-backstage-to-caption (by-status files)
	(format
   "<h1>%d talk(s) being captioned (%s minutes)</h1>People are working on these ones, yay!<ul>%s</ul>"
   (length (assoc-default "TO_CAPTION" by-status))
   (emacsconf-sum :video-time (assoc-default "TO_CAPTION" by-status))
   (mapconcat
    (lambda (f)
      (setq f (append
               f
               (list :extra
                            
                     (concat "<div class=\"caption-note\">"
                             (emacsconf-surround "Being captioned by " (plist-get f :captioner) " " "")
                             (emacsconf-surround "Note: " (plist-get f :caption-note) "" "")
                             "</div>")
                     :files
                     (emacsconf-publish-talk-files f files))))
      (format  "<li><a name=\"%s\"></a><strong><a href=\"%s%s\">%s</a></strong><br />%s (id:%s)<br />%s</li>"
               (plist-get f :slug)
               emacsconf-base-url
               (plist-get f :url)
               (plist-get f :title)
               (plist-get f :speakers-with-pronouns)
               (plist-get f :slug)
               (emacsconf-index-card f)))
    (assoc-default "TO_CAPTION" by-status)
    "\n")))

(defun emacsconf-publish-backstage-to-stream (by-status files)
	(format
          "<h1>%d captioned talk(s) ready for enjoyment (%d minutes)</h1><ol class=\"videos\">%s</ol>"
          (length (assoc-default "TO_STREAM" by-status))
          (emacsconf-sum :video-time (assoc-default "TO_STREAM" by-status))
          (mapconcat (lambda (f)
                       (format  "<li><a name=\"%s\"></a><strong><a href=\"%s%s\">%s</a></strong><br />%s (id:%s)<br />%s</li>"
                                (plist-get f :slug)
                                emacsconf-base-url
                                (plist-get f :url)
                                (plist-get f :title)
                                (plist-get f :speakers-with-pronouns)
                                (plist-get f :slug)
                                (emacsconf-index-card (append f (list :extra (concat "Captioned by " (plist-get f :captioner))
                                                                      :files (emacsconf-publish-talk-files f files)))
                                                      emacsconf-main-extensions)))
                     (assoc-default "TO_STREAM" by-status) "\n")))

(defvar emacsconf-backstage-phase 'harvest) ; prerec


(defun emacsconf-publish-backstage-index (&optional filename)
  (interactive)
  (setq filename (or filename (expand-file-name "index.html" emacsconf-backstage-dir)))
  (let ((info (emacsconf-publish-prepare-for-display (emacsconf-get-talk-info))))
    (let ((emacsconf-schedule-svg-modify-functions '(emacsconf-schedule-svg-color-by-status)))
      (with-temp-file (expand-file-name "schedule.svg" emacsconf-backstage-dir)
        (svg-print (emacsconf-schedule-svg 800 200 info))))
    (with-temp-file filename
      (let* ((talks
              (mapcar
               (lambda (o) (append
														(list :captions-edited t) o))
               (seq-filter (lambda (o) (plist-get o :speakers))
			                     (emacsconf-active-talks (emacsconf-filter-talks info)))))
             (by-status (seq-group-by (lambda (o) (plist-get o :status)) talks))
             (files (directory-files emacsconf-backstage-dir)))
        (insert
         "<html><head><meta charset=\"UTF-8\"><link rel=\"stylesheet\" href=\"/style.css\" /></head><body>"
				 (if (file-exists-p (expand-file-name "include-in-index.html" emacsconf-cache-dir))
             (with-temp-buffer (insert-file-contents (expand-file-name "include-in-index.html" emacsconf-cache-dir)) (buffer-string))
           "")
				 "<p>Schedule by status: (gray: waiting, light yellow: processing, yellow: to assign, light green: captioning, green: captioned and ready)<br />Updated by conf.org and the wiki repository</br />"
         "<img src=\"schedule.svg\" /></p>"
				 (if (eq emacsconf-backstage-phase 'prerec)
						 (format "<p>Waiting for %d talks (~%d minutes) out of %d total</p>"
										 (length (assoc-default "WAITING_FOR_PREREC" by-status))
										 (emacsconf-sum :time (assoc-default "WAITING_FOR_PREREC" by-status))
										 (length talks))
					 "")
         "<ul>"
         (mapconcat
          (lambda (status)
            (concat "<li>" status ": " 
                    (mapconcat (lambda (o) (format "<a href=\"#%s\">%s</a>"
                                                   (plist-get o :slug)
                                                   (plist-get o :slug)))
                               (assoc-default status by-status)
                               ", ")
                    "</li>"))
					(pcase emacsconf-backstage-phase
						('prerec '("TO_PROCESS" "PROCESSING" "TO_ASSIGN" "TO_CAPTION" "TO_STREAM"))
						('harvest '("TO_ARCHIVE" "TO_REVIEW_QA" "TO_INDEX_QA" "TO_CAPTION_QA")))
					"")
         "</ul>"
				 (pcase emacsconf-backstage-phase
					 ('prerec
						(concat
						 (emacsconf-publish-backstage-processing by-status files)
						 (emacsconf-publish-backstage-to-assign by-status files) 
						 (emacsconf-publish-backstage-to-caption by-status files)
						 (emacsconf-publish-backstage-to-stream by-status files)))
					 ('harvest
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
																(emacsconf-index-card
																 (append (list
																					:video-note
																					(unless (file-exists-p (expand-file-name (concat (plist-get f :video-slug) "--bbb-webcams.webm") emacsconf-cache-dir))
																						"<div>No Q&A video for this talk</div>")
																					:video-file
																					(cond
																					 ((file-exists-p (expand-file-name (concat (plist-get f :video-slug) "--answers.webm") emacsconf-cache-dir))
																						(concat (plist-get f :video-slug) "--answers.webm"))
																					 ((file-exists-p (expand-file-name (concat (plist-get f :video-slug) "--bbb-webcams.webm") emacsconf-cache-dir))
																						(concat (plist-get f :video-slug) "--bbb-webcams.webm"))
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
																															 (if (file-exists-p (expand-file-name (concat (plist-get f :video-slug) "--pad.txt")
																																																		emacsconf-cache-dir))
																																	 (concat (plist-get f :video-slug) "--pad.txt"))
																															 "\">Etherpad (Markdown)</a>" "")
																					 (emacsconf-surround ", <a href=\"" (plist-get f :bbb-playback) "\">BBB playback</a>" "")
																					 (emacsconf-surround ", <a href=\""
																															 (if (file-exists-p (expand-file-name (concat (plist-get f :video-slug) "--bbb.txt")
																																																		emacsconf-cache-dir))
																																	 (concat (plist-get f :video-slug) "--bbb.txt"))
																															 "\">BBB text chat</a>" "")
																					 (emacsconf-surround ", <a href=\""
																															 (if (file-exists-p (expand-file-name (concat (plist-get f :video-slug) "--bbb-webcams.opus")
																																																		emacsconf-cache-dir))
																																	 (concat (plist-get f :video-slug) "--bbb-webcams.opus"))
																															 "\">BBB audio only</a>" ""))
																					:files (emacsconf-publish-talk-files f files))
																				 f)
																 emacsconf-main-extensions)))
										 (assoc-default status by-status) "\n"))))
							 stages
							 "\n"))))
				 (if (file-exists-p (expand-file-name "include-in-index-footer.html" emacsconf-cache-dir))
             (with-temp-buffer (insert-file-contents (expand-file-name "include-in-index-footer.html" emacsconf-cache-dir)) (buffer-string))
           "")
         "</body></html>")))))

(defun emacsconf-publish-filter-files (talk files extensions &optional selector)
  (when (plist-get talk :video-slug)
      (seq-filter
       (lambda (f)
         (string-match (concat (regexp-quote (plist-get talk :video-slug))
                               (if selector (concat "--" selector))
                               ".*"
                               (regexp-opt extensions)
															 "$")
                       f))
       files)))

(defun emacsconf-publish-public-index-for-talk (o files)
	(format "<li><div class=\"title\"><a name=\"%s\" href=\"%s\">%s</a></div></div><div class=\"speakers\">%s</div>%s</li>%s"
					(plist-get o :slug)
          (plist-get o :absolute-url)
          (plist-get o :title)
          (plist-get o :speakers)
          (emacsconf-index-card
           (append (list :files
                         (emacsconf-publish-filter-files o files emacsconf-main-extensions)
												 :audio-file
												 (emacsconf-talk-file o "--main.opus"))
                   o)
           '(".org" ".pdf" "--main.vtt" "--compressed56.webm"))
          (if (or (emacsconf-talk-file o "--answers.webm")
									(emacsconf-talk-file o "--answers.opus"))
              (format "<li><div class=\"title\"><a href=\"%s\">Q&amp;A for %s</a></div>%s</li>"
											(plist-get o :absolute-url)
                      (plist-get o :title)
                      (emacsconf-index-card
                       (append
                        (list
                         :public 1
                         :video-id (concat "qanda-" (plist-get o :slug))
                         :toobnix-url nil
												 :video-duration (plist-get o :qa-video-duration)
												 :video-file-size (plist-get o :qa-video-file-size)
                         :video-file (plist-get o :qa-video-file)
												 :audio-file (emacsconf-talk-file o "--answers.opus")
                         :files (emacsconf-publish-filter-files
                                 o
                                 files emacsconf-main-extensions
                                 "answers"))
                        o)
                       (list "--answers.webm" "--answers.opus" "--answers.vtt" "--answers--chapters.vtt")))
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
       "<html><head><meta charset=\"utf-8\" /></head><body>"
       "<h1>" emacsconf-name " " emacsconf-year "</h1>"
       "<div class=\"m3u\"><a href=\"index.m3u\">M3U playlist for playing in MPV and other players</a></div>"
			 "<div>Quick links: " (mapconcat (lambda (o) (format "<a href=\"#%s\">%s</a>"
																													 (plist-get o :slug)
																													 (plist-get o :slug)))
																			 info ", ")
			 "</div>"
       "<ol class=\"videos\">"
       (mapconcat (lambda (o) (emacsconf-publish-public-index-for-talk o files)) info "\n")
       "</ol>"
       (if (file-exists-p (expand-file-name "include-in-public-index.html" emacsconf-cache-dir))
           (with-temp-buffer (insert-file-contents (expand-file-name "include-in-public-index.html" emacsconf-cache-dir)) (buffer-string))
         "")
       "</body></html>"))))

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
                      (emacsconf-index-card
                       (append (list :base-url
                                     (concat emacsconf-media-base-url (plist-get f :conf-year) "/")
                                     :track-base-url
                                     (format "/%s/captions/" (plist-get f :conf-year))
                                     :files
                                     (emacsconf-publish-filter-files f files emacsconf-main-extensions))
                               f)
                       emacsconf-main-extensions)
                    "")
                  (if (plist-get f :qa-public)
                      (emacsconf-index-card
                       (append
                        (list
                         :public 1
                         :base-url (concat emacsconf-media-base-url (plist-get f :conf-year) "/")
                         :video-id (concat "qanda-" (plist-get f :slug))
                         :track-base-url
                         (format "/%s/captions/" (plist-get f :conf-year))
                         :video-file
												 (emacsconf-talk-file f "--answers.webm"))
                        f)
                       (list "--answers.vtt" "--answers--chapters.vtt" "--answers.webm"))
                    "")))
        info "\n"))
      "</ol>")))

(defun emacsconf-make-chapter-strings (filename track-base-url &optional target)
  (let ((chapters (and filename (subed-parse-file filename))))
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
  (concat
   (if (member filename files)
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
    "")))

(defun emacsconf-link-file-formats (video-slug extensions)
  (string-join (emacsconf-link-file-formats-as-list video-slug extensions) " "))

(defun emacsconf-link-file-formats-as-list (talk extensions)
  (if (plist-get talk :files)
      (seq-map
			 (lambda (file)
         (format "<a href=\"%s%s\">Download %s</a>"
                 (or (plist-get talk :base-url) "")
                 file
                 (replace-regexp-in-string (concat "^" (regexp-quote (plist-get talk :video-slug))) "" file)))
       (plist-get talk :files))
    (let ((video-slug (plist-get talk :video-slug)))
      (delq nil (seq-map (lambda (ext)
                           (let ((file (expand-file-name
                                        (concat video-slug ext)
                                        emacsconf-cache-dir))
                                 size)
                             (when (file-exists-p file)
                               (setq size
                                     (if (> (file-attribute-size (file-attributes file)) 1000000)
                                         (format " (%sB)" (file-size-human-readable (file-attribute-size (file-attributes file))))
                                       ""))
                               (format "<a href=\"%s%s\">Download %s%s</a>"
                                       (or (plist-get talk :base-url) "")
                                       (concat video-slug ext)
                                       ext
                                       size))))
                         extensions)))))

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
                                               (concat (plist-get o :video-slug) "--main.webm")
                                               emacsconf-cache-dir))
                                  (qa-video (expand-file-name
                                             (concat (plist-get o :video-slug) "--answers.webm")
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
                                         (plist-get o :video-slug))
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
                                             (plist-get o :video-slug))
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

(defun emacsconf-generate-pad-template (emacsconf-info)
    "Generate a template for copying and pasting into the pad.
            Writes it to pad-template.html."
    (interactive (list (emacsconf-get-talk-info)))
    (let* ((talks (emacsconf-filter-talks emacsconf-info))
           (text (concat
                "<p>Conference info, how to watch/participate: https://emacsconf.org/2021/<br />
            Guidelines for conduct: https://emacsconf.org/conduct/</p>

            <p>Except where otherwise noted, the material on the EmacsConf pad are dual-licensed under the terms of the Creative Commons Attribution-ShareAlike 4.0 International Public License ; and the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) an later version.
            Copies of these two licenses are included in the EmacsConf wiki repository, in the COPYING.GPL and COPYING.CC-BY-SA files (https://emacsconf.org/COPYING/).</p>

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
            (let ((url (format "https://emacsconf.org/%s/talks/%s" emacsconf-year (plist-get o :slug))))
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

(defun emacsconf-get-preferred-video (video-slug &optional files)
  (or
   (car
    (mapcar
     (lambda (suffix)
       (seq-find (lambda (s) (string-match
                              (concat (regexp-quote
                                       (if suffix (concat video-slug "--" suffix)
                                         video-slug))
                                      "\\." (regexp-opt emacsconf-media-extensions)) s)) files))
     '("main" "captioned" "normalized" "reencoded" "compressed" "original" nil)))
   (seq-find
    'file-exists-p
    (seq-map (lambda (suffix)
               (expand-file-name (concat video-slug "--" suffix ".webm")
                                 emacsconf-cache-dir))
             '("main" "captioned" "normalized" "reencoded" "compressed" "original")))
   (car (directory-files emacsconf-cache-dir
                         nil
                         (concat (regexp-quote video-slug)
                                 ".*\\."
                                 (regexp-opt emacsconf-media-extensions))))))

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
           (mapcar (lambda (talk) (emacsconf-get-preferred-video (plist-get talk :video-slug)))
                   emacsconf-info)))
    (switch-to-buffer (current-buffer))))

;;; Video services

(autoload 'subed-parse-file "subed-common")
(defun emacsconf-publish-video-description (talk &optional copy skip-title)
  (interactive (list (emacsconf-complete-talk-info) t))
  (let ((chapters (subed-parse-file
                   (expand-file-name
                    (concat
                     (file-name-base (plist-get talk :video-slug)) "--main--chapters.vtt")
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
;; (emacsconf-publish-video-description (emacsconf-find-talk-info "async") t)

(defun emacsconf-cache-all-video-data (&optional force)
  (interactive (list current-prefix-arg))
  (mapc
   (lambda (talk)
     (when (and (plist-get talk :video-slug)
                (or force (null (plist-get talk :video-file-size))))
       (emacsconf-publish-cache-video-data talk)))
   (emacsconf-get-talk-info)))
;; (emacsconf-cache-all-video-data t)
(defvar emacsconf-cache-dir (expand-file-name "cache" (file-name-directory emacsconf-org-file)))

(defun emacsconf-publish-cache-video-data (talk)
  (interactive (list (emacsconf-complete-talk-info)))
  (let ((main (expand-file-name (concat (plist-get talk :video-slug) "--main.webm")
                                emacsconf-cache-dir)))
    (emacsconf-with-talk-heading talk
      (let* ((video-file-name (emacsconf-get-preferred-video (plist-get talk :video-slug)))
             (video-file (and video-file-name (expand-file-name video-file-name emacsconf-cache-dir)))
						 (qa-file (emacsconf-talk-file talk "--answers.webm"))
						 (intro-file (expand-file-name (concat (plist-get talk :slug) ".webm")
																					 (expand-file-name "intros" emacsconf-stream-asset-dir)))
             duration)
        (unless (file-exists-p main)
          (setq main video-file-name))
        (when video-file
          (org-entry-put (point) "VIDEO_FILE" (file-name-nondirectory video-file))
          (org-entry-put (point) "VIDEO_FILE_SIZE" (file-size-human-readable (file-attribute-size (file-attributes video-file))))
          (unless (plist-get talk :captions-edited)
            (let ((caption-file (expand-file-name
                                 (concat (plist-get talk :video-slug)
                                         "--main.vtt")
                                 emacsconf-cache-dir)))
              (when (emacsconf-captions-edited-p caption-file)
                (org-entry-put (point) "CAPTIONS_EDITED" "1"))))
          (unless (plist-get talk :video-duration)
            (setq duration (/ (compile-media-get-file-duration-ms video-file) 1000))
            (org-entry-put (point) "VIDEO_DURATION" (format-seconds "%h:%z%.2m:%.2s" duration))
            (org-entry-put (point) "VIDEO_TIME" (number-to-string (ceiling (/ duration 60))))))
				(when qa-file
          (org-entry-put (point) "QA_VIDEO_FILE" (file-name-nondirectory qa-file))
          (org-entry-put (point) "QA_VIDEO_FILE_SIZE" (file-size-human-readable (file-attribute-size (file-attributes qa-file))))
          (unless (plist-get talk :qa-captions-edited)
            (let ((caption-file (emacsconf-talk-file talk "--answers.vtt")))
              (when (emacsconf-captions-edited-p caption-file)
                (org-entry-put (point) "QA_CAPTIONS_EDITED" "1"))))
          (unless (plist-get talk :qa-video-duration)
            (setq duration (/ (compile-media-get-file-duration-ms qa-file) 1000))
            (org-entry-put (point) "QA_VIDEO_DURATION" (format-seconds "%h:%z%.2m:%.2s" duration))
            (org-entry-put (point) "QA_VIDEO_TIME" (number-to-string (ceiling (/ duration 60))))))
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
      (when (> (length (org-entry-get (point) "VIDEO_SLUG")) 80)
        (copy-file (expand-file-name (concat (org-entry-get (point) "VIDEO_SLUG") "--main.webm") emacsconf-cache-dir)
                   (expand-file-name (concat "emacsconf-" emacsconf-year "-" (org-entry-get (point) "SLUG") ".webm") emacsconf-cache-dir) t))
      (browse-url "https://toobnix.org/videos/upload#upload"))))

(defun emacsconf-publish-files ()
  (interactive)
  (let* ((slug (org-entry-get (point) "VIDEO_SLUG"))
         (video-file (emacsconf-get-preferred-video slug))
         (wiki-captions-directory (expand-file-name "captions" (expand-file-name emacsconf-year emacsconf-directory))))
    (org-entry-put (point) "PUBLIC" "1")
    (when (file-exists-p video-file)
      (emacsconf-youtube-edit)      
      (emacsconf-toobnix-edit) 
      (emacsconf-publish-update-talk (emacsconf-get-talk-info-for-subtree)))
    ;; (copy-file (emacsconf-get-preferred-video slug) emacsconf-public-media-directory t)
    ;; (mapc (lambda (ext)
    ;;         (when (file-exists-p (expand-file-name (concat slug ext) emacsconf-cache-dir))
    ;;           (copy-file (expand-file-name (concat slug ext) emacsconf-cache-dir)
    ;;                      emacsconf-public-media-directory
    ;;                      t)))
    ;;       emacsconf-published-extensions)
    ))


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
<h1>Tracks</h1>"
   "<table width=\"100%\"><tr><th>Watch page</th><th>IRC channel (libera.chat)</th><th>Alternative for streaming player</th><th>Low res</th></tr>\n"
   (mapconcat (lambda (track)
                (emacsconf-replace-plist-in-string
                 (append (list :year emacsconf-year) track)
                 "<tr><td><div class=\"sched-track ${name}\"><a href=\"/${year}/watch/${id}/\">${name}</a></div></td><td><a href=\"${webchat-url}\">${channel}</a></td><td><a href=\"${stream}\">${stream}</a></td><td><a href=\"${480p}\">${id}-480p.webm</a></tr>"))
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
                (append o (list :base-url emacsconf-base-url))
                "<span><a href=\"${base-url}${url}\">${slug}</a> (<a class=\"pad-link\" href=\"${pad-url}\">pad</a>, ${qa-link})</span>"))
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
      " | ${stream-nav} | <a href=\"https://emacsconf.org/2022/watch/\">Tips for watching/participating</a></div>

<video controls class=\"reload\"><source src=\"${stream}\" type=\"video/webm\" /></video>
<div>Alternatively, load <a href=\"${stream-hires}\">${stream-hires}</a> or <a href=\"${480p}\">${480p}</a> (low-res) in a streaming media player such as MPV.</div>
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
        (insert "<html><head><title>Watch EmacsConf</title><link rel=\"stylesheet\" type=\"text/css\" href=\"/style.css\"></link></head><body>" (emacsconf-publish-format-watch-index info)

                " <p>
        Depending on which media player you use, you may enter the stream address
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

<p>If you prefer, you can watch the livestream via Toobnix:
<a href=\"https://toobnix.org/w/7t9X8eXuSby8YpyEKTb4aj\">General track</a>,
<a href=\"https://toobnix.org/w/w6K77y3bNMo8xsNuqQeCcD\">Development track</a>.
Pre-recorded videos and replays will also be available on Toobnix in
the <a href=\"https://toobnix.org/c/emacsconf\">EmacsConf channel</a>.</p>

<p>To participate in the Q&A, please check the talk page for the Q&A
details, including the Etherpad link, IRC channel, and optionally
a BigBlueButton room (BBB) for Q&A. If you plan to participate in
Q&A in the BigBlueButton room, please use headphones or earphones
in order to minimize audio feedback. The link on the talk page
will take you to a waiting room that will automatically refresh
when the host has opened the Q&A.</p>

"

                "</body></html>"))
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


(defvar emacsconf-publish-current-dir "/ssh:orga@media.emacsconf.org:/var/www/media.emacsconf.org/2022/current"
  "Directory to publish BBB redirects and current information to.")


;; (assert (eq (emacsconf-get-bbb-state '(:status "OPEN_Q")) 'open))
;; (assert (eq (emacsconf-get-bbb-state '(:status "TO_ARCHIVE")) 'after))


(defun emacsconf-publish-bbb-static-redirects ()
  "Create emergency redirects that can be copied over the right location."
  (interactive)
  (mapc (lambda (state)
          (let ((emacsconf-publish-current-dir
                 (expand-file-name
                  state
                  (expand-file-name "redirects" emacsconf-stream-asset-dir))))
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
          ('open
           "<html><head><meta http-equiv=\"refresh\" content=\"0; URL=${bbb-room}\"></head><body>
The live Q&A room for ${title} is now open. You should be redirected to <a href=\"${bbb-room}\">${bbb-room}</a> automatically, but if not, please visit the URL manually to join the Q&A.</body></html>")
          ('before
           "<html><head><meta http-equiv=\"refresh\" content=\"5; URL=${bbb-redirect-url}\"></head><body>
The Q&A room for ${title} is not yet open. This page will refresh every 5 seconds until the BBB room is marked as open, or you can refresh it manually.</body></html>")
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
    (when (or (string= org-state "PLAYING")
              (string= org-state "TO_STREAM"))
      (if (plist-get talk :public)
          ;; Copy main extension files from backstage to public
          (let ((files (directory-files emacsconf-backstage-dir nil
                                        (concat "^"
                                                (regexp-quote (plist-get talk :video-slug))
                                                (regexp-opt emacsconf-main-extensions)))))
            (mapc (lambda (file)
                    (when (or (not (string-match "--main.vtt$" file))
                              (plist-get talk :captions-edited))
                      (copy-file (expand-file-name file emacsconf-backstage-dir)
                                 (expand-file-name file emacsconf-public-media-directory) t)))
                  files))
        ;; Remove files from public
        (let ((files (directory-files emacsconf-public-media-directory nil
                                      (concat "^"
                                              (regexp-quote (plist-get talk :video-slug)
                                                            )))))
          (mapc (lambda (file)
                  (delete-file (expand-file-name file emacsconf-public-media-directory)))
                files)))
      (emacsconf-publish-public-index)
			(emacsconf-generate-playlist
			 (expand-file-name "index.m3u" emacsconf-public-media-directory)
       (concat emacsconf-name " " emacsconf-year)
       (emacsconf-public-talks (emacsconf-get-talk-info))))))

(defun emacsconf-publish-bbb-redirect-all ()
  (interactive)
  (unless (file-directory-p emacsconf-publish-current-dir)
    (make-directory emacsconf-publish-current-dir))
  (mapc #'emacsconf-publish-bbb-redirect (emacsconf-filter-talks (emacsconf-get-talk-info))))
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
												(let ((chapters (subed-parse-file (expand-file-name (concat (plist-get talk :video-slug) "--main--chapters.vtt") emacsconf-cache-dir))))
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

(defvar emacsconf-publish-toobnix-upload-command '("node" "/home/sacha/vendor/PeerTube/dist/server/tools/peertube.js"))
(defun emacsconf-publish-upload-to-toobnix (properties)
	(with-temp-buffer
		(apply #'call-process
					 (car emacsconf-publish-toobnix-upload-command)
					 nil t t
					 (append
						(cdr emacsconf-publish-toobnix-upload-command)
						(list "upload" "-f" (plist-get properties :file))
						(when (plist-get properties :title)
							(list "-n" (plist-get properties :title)))
						(when (plist-get properties :description)
							(list "-d" (plist-get properties :description)))
						(list "-l" "2" "-c" "15" "-P" (if (string= (plist-get properties :privacy) "unlisted") "2" "1") "-t"
									(cond
									 ((stringp (plist-get properties :tags))
										(plist-get properties :tags))
									 ((listp (plist-get properties :tags))
										(string-join (plist-get properties :tags) ","))
									 (t "emacs")))))
		(buffer-string)))
;; YouTube

(defvar emacsconf-publish-youtube-upload-command '("python3" "/home/sacha/vendor/youtube-upload/bin/youtube-upload"))


(defun emacsconf-publish-upload-to-youtube (properties)
	(with-temp-buffer
		(apply #'call-process
					 (car emacsconf-publish-youtube-upload-command)
					 nil t t
					 (append
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
						(list (plist-get properties :file))))
		(buffer-string)))

(defun emacsconf-publish-answers-title (talk &optional len)
	(let ((title (concat emacsconf-name " " emacsconf-year " Q&A: " (plist-get talk :title))))
		(if (or (null len) (< (length title) len))
				title
			(concat (substring title 0 (- len 3)) "..."))))

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
								(format ".%03d" (% (elt chapter 1) 1000))
								" " (elt chapter 3) "\n"))
						 chapters
						 "")
						"\n"))
			 "")
		 "You can view this and other resources using free/libre software at " (plist-get talk :absolute-url) " .\n"
		 "This video is available under the terms of the Creative Commons Attribution-ShareAlike 4.0 International (CC BY-SA 4.0) license.\n")))
;; (emacsconf-publish-answers-description (emacsconf-resolve-talk "async") 'toobnix)



(defun emacsconf-publish-upload-answers (talk platform)
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
							:playlist "EmacsConf 2022"
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
							:playlist "EmacsConf 2022"
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
;; 
(provide 'emacsconf-publish)
