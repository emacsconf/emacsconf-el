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

(defcustom emacsconf-main-extensions '(".webm" "--main.webm" "--main.org" ".org" ".odp" ".pdf" ".el" "--compressed56.webm" "--main.vtt" "--main_fr.vtt" "--main_ja.vtt" "--chapters.vtt" "--main--chapters.vtt" "--script.fountain")
  "Extensions to list on public pages."
  :type '(repeat string)
  :group 'emacsconf)

(defcustom emacsconf-backstage-extensions '(".en.srv2" ".srt")
  "Extensions to list in the staging area."
  :group 'emacsconf)
(defcustom emacsconf-public-media-directory (concat "/ssh:orga@media.emacsconf.org:/var/www/media.emacsconf.org/" emacsconf-year "/")
  "Can be over TRAMP" :type 'string :group 'emacsconf)

(defun emacsconf-update-talk (talk)
  "Publish the schedule page and the page for this talk."
  (interactive (list (emacsconf-complete-talk-info)))
  (when (functionp 'emacsconf-upcoming-insert-or-update)
    (emacsconf-upcoming-insert-or-update))
  (let ((info (emacsconf-get-talk-info)))
    (emacsconf-generate-before-page talk info)
    (emacsconf-publish-after-page talk info)
    (emacsconf-generate-main-schedule info)))

(defun emacsconf-publish-add-talk ()
  "Add the current talk to the wiki."
  (interactive)
  (emacsconf-update-talk)
  (emacsconf-publish-info-pages)
  (emacsconf-generate-main-schedule)
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
      (emacsconf-generate-main-schedule info)
      (emacsconf-generate-talk-pages info force)
      (magit-status emacsconf-directory))))

(defun emacsconf-update-schedule ()
  (interactive)
  (require 'emacsconf-ical)
  (emacsconf-publish-with-wiki-change
    (emacsconf-publish-info-pages)
    (emacsconf-generate-main-schedule)
    (emacsconf-ical-generate-all)
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
    (emacsconf-generate-playlist (expand-file-name "index.m3u" emacsconf-public-media-directory)
                                 (concat emacsconf-name emacsconf-year)
                                 (emacsconf-public-talks emacsconf-info)
                                 (format "https://media.emacsconf.org/%s/" emacsconf-year)))
  (when emacsconf-backstage-dir
    (emacsconf-publish-backstage-index (expand-file-name "index.html" emacsconf-backstage-dir)))
  (emacsconf-generate-playlist (expand-file-name "index.m3u" emacsconf-backstage-dir)
                               (concat emacsconf-name emacsconf-year)
                               (emacsconf-filter-talks emacsconf-info)
                               (format "https://media.emacsconf.org/%s/backstage/" emacsconf-year)))

(defun emacsconf-index-card (talk &optional extensions)
  "Format an HTML card for TALK, linking the files in EXTENSIONS."
  (let* ((video-slug (plist-get talk :video-slug))
         (video-file
          (or (plist-get talk :video-file)
              (and video-slug
                   (emacsconf-get-preferred-video (plist-get talk :video-slug)
                                                  (plist-get talk :files)))))
         (video (and video-slug
                     (emacsconf-index-card-video (or (plist-get talk :video-id) (concat "mainVideo-" (plist-get talk :slug)))
                                                 video-file talk extensions))))
    ;; Add extra information to the talk
    (setq talk
          (append
           talk
           (list
            :video-html (or (plist-get video :video) "")
            :chapter-list (or (plist-get video :chapter-list) "")
            :resources (or (plist-get video :resources) "")
            :extra (or (plist-get talk :extra) "") 
            :speaker-info (or (plist-get talk :speakers-with-pronouns) ""))))
    (if (eq (plist-get talk :format) 'wiki)
        (plist-get talk :video-html)
      (emacsconf-replace-plist-in-string
       talk
       "<div class=\"vid\">${video-html}<div>${extra}</div>${resources}${chapter-list}</div>"))))

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
                 (format "<a href=\"%s\" target=\"_blank\" rel=\"noreferrer\">Open pad</a>" (plist-get o :pad-url))
               "")
      "</td>"
      "<td>" (format "<a href=\"%s\" target=\"_blank\" rel=\"noreferrer\">Open chat</a>" (plist-get o :webchat-url))
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
                      (emacsconf-prepare-for-display (emacsconf-get-talk-info)))))
    (mapc
     (lambda (track)
       (let*
           ((track-talks (seq-filter (lambda (o) (string= (plist-get o :track)
                                                          (plist-get track :name)))
                                     info))
            (result
             (concat
              "<html><head><link rel=\"stylesheet\" href=\"style.css\"></head><body>
<div>"
              (with-temp-buffer
                (svg-print (emacsconf-schedule-svg 800 300 info))
                (buffer-string))
              "</div><h1>"
              (plist-get track :name)
              "</h1><table>"
              "<tr><th colspan=\"6\" style=\"text-align: left\">Saturday</th></tr>"
              (emacsconf-publish-format-res-talks
               (emacsconf-prepare-for-display
                (emacsconf-filter-talks-by-time
                 (format "2022-12-03T08:00:00%s" emacsconf-timezone-offset)
                 (format "2022-12-03T18:00:00%s" emacsconf-timezone-offset)
                 track-talks)))
              "<tr><th colspan=\"6\" style=\"text-align: left\">Sunday</th></tr>"
              (emacsconf-publish-format-res-talks
               (emacsconf-prepare-for-display
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

(defun emacsconf-index-card-video (video-id video-file talk extensions &optional backstage)
  (let* ((video-base (and video-file (file-name-base video-file)))
         (chapter-info (and video-file
                            (emacsconf-make-chapter-strings
                             (expand-file-name
                              (concat video-base "--chapters.vtt")
                              emacsconf-cache-dir)
                             (plist-get talk :track-base-url)
                             video-id)))
         (info
          (append
           (list
            :source-src
            (when video-file
              (if (plist-get talk :public)
                  (format "%s%s/%s" emacsconf-media-base-url (plist-get talk :conf-year)
                          (file-name-nondirectory video-file))
                (file-name-nondirectory video-file)))
            :captions
            (and video-file
                 (let ((tracks
                        (emacsconf-video-subtitle-tracks
                         (concat (replace-regexp-in-string "reencoded\\|original" "main" video-base)
				 ".vtt")
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
            :video-file-size (if (and video-file (file-exists-p video-file))
                                 (file-size-human-readable (file-attribute-size (file-attributes video-file))))
            :other-files
            (mapconcat
             (lambda (s)
               (if (eq (plist-get talk :format) 'wiki)
                   (concat s "  \n")
                 (concat "<li>" s "</li>")))
             (emacsconf-link-file-formats-as-list talk (or extensions emacsconf-main-extensions))
             "")
            :poster (and video-file (format "https://media.emacsconf.org/%s/%s.png" (plist-get talk :conf-year) (file-name-base video-file)))
            :toobnix-info (if (plist-get talk :toobnix-url)
                              (format
                               (if (eq (plist-get talk :format) 'wiki)
                                   "[View on Toobnix](%s)  \n"
                                 "<li><a href=\"%s\">View on Toobnix</a></li>")
                               (plist-get talk :toobnix-url))
                            ""))
           
           talk)))
    (list
     :video
     (emacsconf-replace-plist-in-string
      info
      (if video-file
          (if (eq (plist-get talk :format) 'wiki)
              "[[!template id=\"vid\" vidid=\"${video-id}\" src=\"${source-src}\" poster=\"${poster}\" ${captions}
size=\"${video-file-size}\" duration=\"${video-duration}\" other_resources=\"\"\"${other-files}${toobnix-info}\"\"\"]]
${chapter-list}
"
            "<video controls preload=\"metadata\" poster=\"${poster}\" id=\"${video-id}\"><source src=\"${source-src}\" />${captions}${chapter-track}</video>${chapter-list}")
        "The video for \"${title}\" will be posted here when available. You can also subscribe to the <a href=\"https://lists.gnu.org/mailman/listinfo/emacsconf-discuss\">emacsconf-discuss mailing list</a> for updates."))
     :resources
     (emacsconf-replace-plist-in-string
      (append info
              (list :video-download
                    (if video-file
                        (emacsconf-replace-plist-in-string
                         info
                         "<li><a href=\"${source-src}\">Download video (${video-duration}, ${video-file-size}B)</a></li>")
                      "")))
      "<div class=\"files resources\"><ul>${video-download}${other-files}${toobnix-info}</ul></div>"))))

(defun emacsconf-format-public-email (o &optional email)
  (format "[%s](mailto:%s?subject=%s)"
          (or email (plist-get o :public-email))
          (or email (plist-get o :public-email))
          (url-hexify-string (format "Comment for EmacsConf 2022 %s: %s" (plist-get o :slug) (plist-get o :title)))))

(defun emacsconf-format-speaker-info (o)
  (let ((extra-info (mapconcat #'identity
                               (delq nil (list
                                          (unless (string= (plist-get o :pronunciation) "nil") (plist-get o :pronunciation))
                                          (when (plist-get o :irc) (format "IRC: %s" (plist-get o :irc)))
                                          (when (plist-get o :public-email) (format "<mailto:%s>" (plist-get o :public-email)))))
                               ", ")))
    (concat (plist-get o :speakers-with-pronouns)
            (if (> (length extra-info) 0)
                (concat " - " extra-info)
              ""))))

(defun emacsconf-generate-talk-page (o &optional force)
  "Draft the talk page for O unless the page already exists or FORCE is non-nil."
  (interactive (list (emacsconf-get-talk-info-for-subtree) (> (prefix-numeric-value current-prefix-arg) 1)))
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

<!-- Initially generated with emacsconf-generate-talk-page and then left alone for manual editing -->
<!-- You can manually edit this file to update the abstract, add links, etc. --->\n

# ${title}
${speaker-info}

[[!inline pages=\"internal(${year}/info/${slug}-before)\" raw=\"yes\"]]

${abstract-md}

[[!inline pages=\"internal(${year}/info/${slug}-after)\" raw=\"yes\"]]

[[!inline pages=\"internal(${year}/info/${slug}-nav)\" raw=\"yes\"]]

${categories}
"))))))

(defun emacsconf-generate-talk-pages (emacsconf-info force)
  (interactive (list (emacsconf-get-talk-info) (> (prefix-numeric-value current-prefix-arg) 1)))
  "Populate year/talks/*.md files.
These should include the nav and schedule files, which will be
rewritten as needed.  After they are generated, they should be all
right to manually edit to include things like additional
resources."
  (mapc (lambda (o) (emacsconf-generate-talk-page o force)) (emacsconf-filter-talks emacsconf-info)))

(defun emacsconf-wiki-talk-resources (o)
  (setq o (append (list :format 'wiki
                        :base-url
                        (concat emacsconf-media-base-url (plist-get o :conf-year) "/")
                        :track-base-url
                        (format "/%s/captions/" (plist-get o :conf-year)))
                  o))
  (concat
   (if (plist-get o :qa-public) "# Talk\n\n" "")
   (emacsconf-index-card o emacsconf-main-extensions)
   (if (plist-get o :qa-public)
       (concat "\n\n# Q&A\n\n"
               (emacsconf-index-card (append
                                      (list
                                       :public 1
                                       :video-id (concat "qanda-" (plist-get o :slug))
                                       :toobnix-url nil
                                       :video-file (expand-file-name
                                                    (concat (file-name-sans-extension (plist-get o :video-slug)) "--answers.webm")
                                                    emacsconf-cache-dir))
                                      o)
                                     (list "--answers.vtt" "--answers--chapters.vtt" "--answers--compressed32.webm")))
     "")))

(defun emacsconf-publish-webchat-link (o)
  (let ((track (emacsconf-get-track (plist-get o :track))))
    (format "<a href=\"%s?join=emacsconf,emacsconf-%s\">#emacsconf-%s</a>"
            emacsconf-chat-base
            (plist-get track :id)
            (plist-get track :id))))

(defvar emacsconf-publish-include-pads nil "When non-nil, include Etherpad info.")

(defun emacsconf-format-talk-schedule-info (o)
  (let ((friendly (concat "/" emacsconf-year "/talks/" (plist-get o :slug) ))
        (timestamp (org-timestamp-from-string (plist-get o :scheduled))))
    (emacsconf-replace-plist-in-string
     (append o
             (list :format
                   (concat (or (plist-get o :video-duration)
                               (concat (plist-get o :duration) "-min talk"))
                           (if (plist-get o :q-and-a)
                               (format " followed by %s Q&A (%s)  "
                                       (plist-get o :q-and-a)
                                       (if (string-match "live" (plist-get o :q-and-a))
                                           (if (eq 'after (emacsconf-bbb-status o))
                                               "done"
                                             (format "<https://emacsconf.org/current/%s/room>" (plist-get o :slug)))
                                         (emacsconf-publish-webchat-link o)))
                             ""))
                   :pad-info
                   (if emacsconf-publish-include-pads
                       (format "Pad: <https://pad.emacsconf.org/%s-%s>  \n" emacsconf-year (plist-get o :slug))
                     "")
                   :status-info
                   (if (member emacsconf-publishing-phase '(program schedule)) (format "Status: %s  \n" (plist-get o :status-label)) "")     
                   :schedule-info
                   (if (and (member emacsconf-publishing-phase '(program schedule))
                            (not (emacsconf-talk-all-done-p o)))
                       (let ((start (org-timestamp-to-time (org-timestamp-split-range timestamp)))
                             (end (org-timestamp-to-time (org-timestamp-split-range timestamp t))))
                         (format
                          "<div>Times in different timezones:</div><div class=\"times\" start=\"%s\" end=\"%s\"><div class=\"conf-time\">%s</div><div class=\"others\">%s</div></div><div><a href=\"/%s/watch/%s/\">Find out how to watch and participate</a></div>"
                          (format-time-string "%Y-%m-%dT%H:%M:%SZ" start t)
                          (format-time-string "%Y-%m-%dT%H:%M:%SZ" end t)
                          (emacsconf-timezone-string o emacsconf-timezone)
                          (string-join (emacsconf-timezone-strings
                                        o
                                        (seq-filter (lambda (zone) (string= emacsconf-timezone zone))
                                                    emacsconf-timezones)) "<br />")
                          emacsconf-year
                          (plist-get (emacsconf-get-track (plist-get o :track)) :id)))
                     "")))
     (concat
      "[[!toc  ]]
Format: ${format}
${pad-info}${status-info}${schedule-info}\n" 
      (if (plist-get o :alternate-apac)
          (format "[[!inline pages=\"internal(%s/inline-alternate)\" raw=\"yes\"]]  \n" emacsconf-year)
        "")
      "\n"
      (if (plist-get o :public) (emacsconf-wiki-talk-resources o) "")
      "\n# Description\n\n"))))

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
  (let ((filename (expand-file-name (concat (plist-get talk :video-slug) "--main.vtt")
                                    (expand-file-name "captions" (expand-file-name emacsconf-year emacsconf-directory))))
        (cached-file (expand-file-name (concat (plist-get talk :video-slug) "--main.vtt") emacsconf-cache-dir)))
    (when (and (file-exists-p cached-file)
               (or
                (not (file-exists-p filename))
                (file-newer-than-file-p cached-file filename)))
      (copy-file cached-file filename t))))

(defun emacsconf-publish-before-page (talk &optional info)
  "Info included before the abstract."
  (interactive (list (emacsconf-complete-talk-info)))
  (setq info (or info (emacsconf-get-talk-info)))
  (when (plist-get talk :public) (emacsconf-publish-captions-in-wiki talk))
  (with-temp-file (expand-file-name (format "%s-before.md" (plist-get talk :slug))
                                    (expand-file-name "info" (expand-file-name emacsconf-year emacsconf-directory)))
    (insert "<!-- Automatically generated by emacsconf-publish-before-page -->\n")
    (when (eq emacsconf-publishing-phase 'schedule)
      (insert "\n"
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
              "\n</div>\n"
              "\n"
              (emacsconf-format-talk-schedule-info talk) "\n"))
    ;; Contact information
    ;; (insert "\n\n" (emacsconf-format-email-questions-and-comments talk) "\n")
    (insert "<!-- End of emacsconf-publish-before-page -->")))

(defun emacsconf-format-transcript (talk)
  "Format the transcript for TALK, adding paragraph markers when possible."
  (require 'subed)
  (let* ((paragraphs (expand-file-name
                      (concat (plist-get talk :video-slug) "--main--paragraphs.vtt")
                      emacsconf-cache-dir))
         (chapters (expand-file-name
                    (concat (plist-get talk :video-slug) "--main--chapters.vtt")
                    emacsconf-cache-dir))
         (subtitles
          (subed-parse-file (expand-file-name
                             (concat (plist-get talk :video-slug) "--main.vtt")
                             emacsconf-cache-dir)))
         (pars (or
                (subed-parse-file paragraphs)
                (subed-parse-file chapters))))
    (if subtitles
        (concat "<a name=\"transcript\"></a>
# Transcript

"
                (mapconcat (lambda (sub)
                             (let ((msecs (elt sub 1)))
                               (format "[[!template %stext=\"%s\" start=\"%s\" video=\"mainVideo-%s\" id=\"subtitle\"]]"
                                       (if (and pars (>= msecs (elt (car pars) 1)))
                                           (progn 
                                             (while (and pars (>= (elt sub 1) (elt (car pars) 1)))
                                               (setq pars (cdr pars)))
                                             "new=\"1\" ")
                                         "")
                                       (replace-regexp-in-string "\"" "&quot;" (elt sub 3))
                                       (concat (format-seconds "%02h:%02m:%02s" (/ (floor msecs) 1000))
                                               "." (format "%03d" (mod (floor msecs) 1000)))
                                       (plist-get talk :slug))))
                           subtitles "\n")
                "

")
      "")))

(defun emacsconf-publish-after-page (talk &optional info)
  "Info included before the abstract."
  (interactive (list (emacsconf-complete-talk-info)))
  ;; Contact information
  (with-temp-file (expand-file-name (format "%s-after.md" (plist-get talk :slug))
                                    (expand-file-name "info" (expand-file-name emacsconf-year emacsconf-directory)))
    (insert
     "<!-- Automatically generated by emacsconf-publish-after-page -->\n"
     "\n\n"
     (if (plist-get talk :public) (emacsconf-format-transcript talk) "")
     (emacsconf-format-email-questions-and-comments talk) "\n"
     "\n\n<!-- End of emacsconf-publish-after-page -->\n")))

(defun emacsconf-sort-by-track-then-schedule (a b)
  ;; Gen,Dev; then by time
  (cond
   ((string< (plist-get a :track)
             (plist-get b :track)) nil)
   ((string< (plist-get a :track)
             (plist-get b :track)) t)
   ((time-less-p (plist-get a :start-time)
                 (plist-get b :start-time)) t)
   (t nil)))

(defun emacsconf-generate-nav-pages (&optional talks)
  (interactive (list
                (emacsconf-active-talks
                 (sort (emacsconf-filter-talks (emacsconf-get-talk-info))
                       (if (eq emacsconf-publishing-phase 'schedule)
                           #'emacsconf-sort-by-scheduled
                         #'emacsconf-sort-by-track-then-schedule)))))
  (let* ((next-talks (cdr talks))
         (prev-talks (cons nil talks))
         (label (if (eq emacsconf-publishing-phase 'schedule)
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

(defun emacsconf-prepare-for-display (info)
  "Sort by scheduled and remove cancelled talks."
  (seq-remove (lambda (o) (string= (plist-get o :status) "CANCELLED"))
              (sort (emacsconf-filter-talks info) #'emacsconf-sort-by-scheduled)))
(defun emacsconf-publish-info-pages (&optional info)
  "Populate year/info/*-nav, -before, and -after files."
  (interactive)
  (setq info (or info (emacsconf-get-talk-info)))
  (emacsconf-publish-with-wiki-change
    (let* ((talks (seq-remove (lambda (o) (string= (plist-get o :status) "CANCELLED"))
                              (sort (emacsconf-filter-talks info) #'emacsconf-sort-by-scheduled))))
      (emacsconf-generate-nav-pages talks)
      (mapc (lambda (o)
              (emacsconf-publish-before-page o talks)
              (emacsconf-publish-after-page o talks))
            talks))))

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

(defun emacsconf-generate-talks-page (emacsconf-info)
  (interactive "p")
  (let ((info emacsconf-info))
    (with-temp-buffer
      (find-file "talk-details.md")
      (erase-buffer)
      (insert (format "<table><thead><th>Duration</th><th>Title</th><th>Speakers</th></thead><tbody>%s</tbody></table>"
                      (mapconcat
                       (lambda (o)
                         (let* ((title (plist-get o :title))
                                (speakers (plist-get o :speakers)))
                           (if (null (plist-get o :slug))
                               (format "<tr><td colspan=\"3\">%s</td></tr>" (emacsconf-format-talk-link o))
                             (format "<tr><td>%s</td><td>%s</td><td>%s</td><tr>" 
                                     (plist-get o :duration)
                                     (emacsconf-format-talk-link o)
                                     (plist-get o :speakers)))))
                       info "\n")))
      (save-buffer))))


(defun emacsconf-generate-main-schedule-with-tracks (&optional info)
  (interactive)
  (setq info (or info (emacsconf-schedule-inflate-sexp emacsconf-schedule-plan
                                                       (emacsconf-get-talk-info))))
  (with-temp-file (expand-file-name "schedule-details.md"
                                    (expand-file-name emacsconf-year emacsconf-directory))
    ;; By track
    (let ((links
           (mapconcat (lambda (track)
                        (concat (cadr track) ": "
                                (mapconcat (lambda (sec)
                                             (format "<a href=\"#%s-%s\">%s</a>"
                                                     (car track) (car sec) (cadr sec)))
                                           '(("sat" "Saturday")
                                             ("sun" "Sunday"))
                                           " - ")))
                      '(("gen" "General")
                        ("dev" "Development"))
                      " | ")))
      (insert (mapconcat
               (lambda (track)
                 (let* ((id (elt track 0))
                        (label (elt track 1))
                        (start (elt track 2))
                        (end (elt track 3))
                        (sequence (emacsconf-schedule-get-subsequence info start end))
                        (sat (cdr (emacsconf-schedule-get-subsequence sequence "Saturday" "Sunday")))
                        (sun (cdr (emacsconf-schedule-get-subsequence sequence "Sunday"))))
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
                                        (emacsconf-format-main-schedule
                                         section-seq))))
                            `(("sat" "Saturday, Dec 3" ,sat)
                              ("sun" "Sunday, Dec 4" ,sun))
                            "\n\n")
                           )))
               '(("gen" "General" "^GEN Sat" "^DEV Sat")
                 ("dev" "Development" "^DEV Sat"))
               "\n")))
    (let ((by-day (sort (seq-remove (lambda (s) (string-match "^\\(GEN\\|DEV\\)" (plist-get s :title)))
                                    info)
                        #'emacsconf-sort-by-scheduled)))
      (insert
       "\n<a name=\"by-day\"></a>\n\n# By day\n\n## Saturday\n"
       ;; Everything all together
       (emacsconf-format-main-schedule
        (emacsconf-schedule-get-subsequence
         by-day
         "Saturday opening remarks"
         "Saturday closing remarks"))
       "\n\n## Sunday\n"
       ;; Everything all together
       (emacsconf-format-main-schedule
        (emacsconf-schedule-get-subsequence
         by-day
         "Sunday opening remarks"
         "Sunday closing remarks"))
       ))))

(defun emacsconf-publish-format-interleaved-schedule (&optional info)
  "Return a list with the schedule for INFO.
Entries are sorted chronologically, with different tracks interleaved."
  (setq info (or info (emacsconf-get-talk-info)))
  (let* ((by-day (emacsconf-by-day (emacsconf-prepare-for-display info)))
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
                     (emacsconf-format-main-schedule (cdr day))
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
  (interactive)
  (emacsconf-publish-schedule-svg-snippets)
  (with-temp-file (expand-file-name "schedule-details.md" (expand-file-name emacsconf-year emacsconf-directory))
    (insert
     (if (eq emacsconf-publishing-phase 'program)
         (let ((sorted (emacsconf-prepare-for-display (or info (emacsconf-get-talk-info)))))
           (concat
            "<a href=\"#development\">Jump to development talks</a>\n<a name=\"general\"></a>\n<h1><span class=\"sched-track General\">General talks</span></h1>\n"
            (emacsconf-format-main-schedule
             (seq-filter (lambda (o) (string= (plist-get o :track) "General")) sorted))
            "\n<a name=\"development\"></a>\n
<h1><span class=\"sched-track Development\">Development talks</h1>
\n"
            (emacsconf-format-main-schedule
             (seq-filter (lambda (o) (string= (plist-get o :track) "Development")) sorted))))
       (emacsconf-publish-format-interleaved-schedule info))))
  (when (eq emacsconf-publishing-phase 'program)
    (with-temp-file (expand-file-name "draft-schedule.md" (expand-file-name emacsconf-year emacsconf-directory))
      (insert
       "This is a *DRAFT* schedule.\n"
       (let ((emacsconf-publishing-phase 'schedule))
         (emacsconf-publish-format-interleaved-schedule info)))))
  (emacsconf-publish-watch-pages))

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
                    (apply '+ (mapcar (lambda (info) (string-to-number (plist-get info :duration))) captioned))
                    (length received)
                    (apply '+ (mapcar (lambda (info) (string-to-number (plist-get info :duration)))
                                      received)))))

(defun emacsconf-publish-sched-directive (o)
  (format "[[!template id=sched%s%s]]"
          (let ((result "")
                (attrs (append
                        (list
                         :title (plist-get o :title)
                         :url (concat "/" (plist-get o :url))
                         :speakers (plist-get o :speakers)
                         :q-and-a (plist-get o :q-and-a))
                        (unless (eq emacsconf-publishing-phase 'program)
                          (list
                           :track (plist-get o :track)
                           :slug (plist-get o :slug)
                           :status (pcase (plist-get o :status)
                                     ("CAPTIONED" "captioned")
                                     ("PREREC_RECEIVED" "received")
                                     ("DONE" "done")
                                     ("STARTED" "now playing")
                                     (_ nil))
                           :time (plist-get o :time)
                           :startutc (format-time-string "%FT%T%z" (plist-get o :start-time) t)
                           :endutc (format-time-string "%FT%T%z" (plist-get o :end-time) t)
                           :start (format-time-string "%-l:%M" (plist-get o :start-time) emacsconf-timezone)
                           :end (format-time-string "%-l:%M" (plist-get o :end-time) emacsconf-timezone))))))
            (while attrs
              (let ((field (pop attrs))
                    (val (pop attrs)))
                (when val
                  (setq result (concat result " " (substring (symbol-name field) 1) "=\"" val "\"")))))
            result)
          (if (eq emacsconf-publishing-phase 'resources)
              (format" resources=\"\"\"\n%s\n\"\"\""
                     (mapconcat (lambda (s) (concat "<li>" s "</li>"))
                                (emacsconf-link-file-formats-as-list
                                 (append o
                                         (list :base-url (format "%s%s/" emacsconf-media-base-url emacsconf-year)))
                                 (append emacsconf-main-extensions '("--main.webm")))
                                ""))
            "")))

(defun emacsconf-format-main-schedule (info)
  (mapconcat #'emacsconf-publish-sched-directive (emacsconf-active-talks info) "\n"))

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
      (when (member org-state '("PROCESSING" "TO_ASSIGN"))
        (emacsconf-cache-video-data talk))
      (when (member org-state '("TO_CAPTION"))
        (unless (or noninteractive (org-entry-get (point) "CAPTIONER"))
          (org-entry-put (point) "CAPTIONER"
                         (assoc-default "CUSTOM_ID" (emacsconf-complete-volunteer)))))
      (when (member org-state '("WAITING_FOR_PREREC" "TO_ASSIGN" "TO_CAPTION" "TO_STREAM"))
        (emacsconf-publish-backstage-index)))))

(defun emacsconf-publish-backstage-index (&optional filename)
  (interactive)
  (setq filename (or filename (expand-file-name "index.html" emacsconf-backstage-dir)))
  (setq emacsconf-info (emacsconf-get-talk-info))
  (with-temp-file filename
    (let* ((talks (seq-filter (lambda (o) (plist-get o :speakers))
			      (emacsconf-active-talks (emacsconf-filter-talks emacsconf-info))))
           (by-status (seq-group-by (lambda (o) (plist-get o :status)) talks))
           (files (directory-files emacsconf-backstage-dir)))
      (insert
       "<html><head><meta charset=\"UTF-8\"><link rel=\"stylesheet\" href=\"/style.css\" /></head><body>"
       "<p>Schedule by status: (gray: waiting, light yellow: processing, yellow: to assign, light green: captioning, green: captioned and ready)<br />Updated by conf.org and the wiki repository</br />"
       "<img src=\"schedule.svg\" /></p>"
       (format "<p>Waiting for %d talks (~%d minutes) out of %d total</p>"
               (length (assoc-default "WAITING_FOR_PREREC" by-status))
               (emacsconf-sum :time (assoc-default "WAITING_FOR_PREREC" by-status))
               (length talks))
       (let ((list (append
		                (assoc-default "TO_ASSIGN" by-status)
		                (assoc-default "TO_PROCESS" by-status)
                    (assoc-default "PROCESSING" by-status)
                    (assoc-default "TO_AUTOCAP" by-status))))
         (format "<h1>%s talk(s) to be captioned (%d minutes)</h1><p>You can e-mail <a href=\"mailto:sacha@sachachua.com\">sacha@sachachua.com</a> to call dibs on editing the captions for one of these talks. This year, we're experimenting with using OpenAI Whisper to provide auto-generated VTT that you can use as a starting point. If you're writing them from scratch, you can choose to include timing information, or we can probably figure them out afterwards with a forced alignment tool. Also, if you feel like making chapter markers, that's cool too. More info: <a href=\"https://media.emacsconf.org/2022/backstage/editing-captions.html\">Editing captions</a>, <a href=\"https://emacsconf.org/captioning/\">captioning tips</a></p><ul class=\"videos\">%s</ul>"
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
                    (format  "<li><strong><a href=\"%s%s\">%s</a></strong><br />%s (id:%s)<br />%s</li>"
                             emacsconf-base-url
                             (plist-get f :url)
                             (plist-get f :title)
                             (plist-get f :speakers)
                             (plist-get f :slug)
                             (emacsconf-index-card f)))
                  list
                  "\n")))
       (format
        "<h1>%d talk(s) being captioned (%s minutes)</h1><ul>%s</ul>"
        (length (assoc-default "TO_CAPTION" by-status))
        (emacsconf-sum :video-time (assoc-default "TO_CAPTION" by-status))
        (mapconcat
         (lambda (f)
           (setq f (append
                    f
                    (list :extra
                          (if (plist-get f :captioner) (concat "<div class=\"caption-note\">Being captioned by " (plist-get f :captioner) "</div>") "")
                          :files
                          (emacsconf-publish-talk-files f files))))
           (format  "<li><strong><a href=\"%s%s\">%s</a></strong><br />%s (id:%s)<br />%s</li>"
                    emacsconf-base-url
                    (plist-get f :url)
                    (plist-get f :title)
                    (plist-get f :speakers-with-pronouns)
                    (plist-get f :slug)
                    (emacsconf-index-card f)))
         (assoc-default "TO_CAPTION" by-status)
         "\n"))
       (format
        "<h1>%d captioned talk(s) ready for enjoyment (%d minutes)</h1><ol class=\"videos\">%s</ol>"
        (length (assoc-default "TO_STREAM" by-status))
        (emacsconf-sum :video-time (assoc-default "TO_STREAM" by-status))
        (mapconcat (lambda (f)
                     (format  "<li><strong><a href=\"%s%s\">%s</a></strong><br />%s (id:%s)<br />%s</li>"
                              emacsconf-base-url
                              (plist-get f :url)
                              (plist-get f :title)
                              (plist-get f :speakers-with-pronouns)
                              (plist-get f :slug)
                              (emacsconf-index-card (append f (list :extra (concat "Captioned by " (plist-get f :captioner))
                                                                    :files (emacsconf-publish-talk-files f files)))
                                                    emacsconf-main-extensions)))
                   (assoc-default "TO_STREAM" by-status) "\n"))
       (if (file-exists-p (expand-file-name "include-in-index.html" emacsconf-cache-dir))
           (with-temp-buffer (insert-file-contents (expand-file-name "include-in-index.html" emacsconf-cache-dir)) (buffer-string))
         "")
       "</body></html>"))))

(defun emacsconf-publish-filter-files (talk files extensions &optional selector)
  (when (plist-get talk :video-slug)
      (seq-filter
       (lambda (f)
         (string-match (concat (regexp-quote (plist-get talk :video-slug))
                               (if selector (concat "--" selector))
                               ".*"
                               (regexp-opt extensions))
                       f))
       files)))

(defun emacsconf-publish-public-index (&optional filename)
  (interactive (list (expand-file-name "index.html" emacsconf-public-media-directory)))
  (setq filename (or filename (expand-file-name "index.html" emacsconf-public-media-directory)))
  (let ((files (directory-files emacsconf-public-media-directory)))
    (with-temp-file filename
      (insert
       "<html><body>"
       "<h1>" emacsconf-name " " emacsconf-year "</h1>"
       "<div class=\"m3u\"><a href=\"index.m3u\">M3U playlist for playing in MPV and other players</a></div>"
       "<ol class=\"videos\">"
       (mapconcat (lambda (o)
                    (format "<li><div class=\"title\"><a href=\"%s\">%s</a></div></div><div class=\"speakers\">%s</div>%s</li>%s"
                            (plist-get o :url)
                            (plist-get o :title)
                            (plist-get o :speakers)
                            (emacsconf-index-card
                             (append (list :files
                                           (emacsconf-publish-filter-files o files emacsconf-main-extensions))
                                     o)
                             '(".org" ".pdf" "--main.vtt" "--compressed56.webm"))
                            (if (member (concat (plist-get o :video-slug)
                                                "--answers.webm")
                                        files)
                                (format "<li><div class=\"title\">Q&A for %s</div>%s</li>"
                                        (plist-get o :title)
                                        (emacsconf-index-card
                                         (append
                                          (list
                                           :public 1
                                           :video-id (concat "qanda-" (plist-get o :slug))
                                           :toobnix-url nil
                                           :video-file
                                           (expand-file-name
                                            (concat (file-name-sans-extension (plist-get o :video-slug))
                                                    "--answers.webm")
                                            emacsconf-public-media-directory)
                                           :files (emacsconf-publish-filter-files
                                                   o
                                                   files emacsconf-main-extensions
                                                   "answers"))
                                          o)
                                         (list "--answers.webm" "--answers.vtt" "--answers--chapters.vtt")))
                              "")))
                  (emacsconf-public-talks (emacsconf-get-talk-info))
                  "\n")
       "</ol>"
       (if (file-exists-p (expand-file-name "include-in-index.html" emacsconf-cache-dir))
           (with-temp-buffer (insert-file-contents (expand-file-name "include-in-index.html" emacsconf-cache-dir)) (buffer-string))
         "")
       "</body></html>"))))

(defun emacsconf-publish-public-index-on-wiki ()
  (interactive)
  (let ((info (seq-filter (lambda (o)
                            (not (string= (plist-get o :status) "CANCELLED")))
                          (emacsconf-filter-talks (emacsconf-get-talk-info))))
        (files (directory-files emacsconf-public-media-directory)))
    (with-temp-file (expand-file-name "all-include.md" (expand-file-name emacsconf-year emacsconf-directory))
      (insert
       "<ol class=\"videos\">"
       (mapconcat
        (lambda (f)
          (format "<li><div class=\"title\"><a href=\"%s\">%s</a></div><div class=\"speakers\">%s</div>%s%s</li>"
                  (plist-get f :url)
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
                         :video-file (expand-file-name
                                      (concat (file-name-sans-extension (plist-get f :video-slug)) "--answers.webm")
                                      emacsconf-cache-dir))
                        f)
                       (list "--answers.vtt" "--answers--chapters.vtt"))
                    "")))
        info "\n"))
      "</ol>")))

(defun emacsconf-make-chapter-strings (filename track-base-url &optional target)
  (let ((chapters (subed-parse-file filename)))
    (when chapters
      (list
       :track (format "<track kind=\"chapters\" label=\"Chapters\" src=\"%s\"\" />"
                      (concat (or track-base-url "") (file-name-nondirectory filename)))
       :md (mapconcat (lambda (chapter)
                        (concat
                         (format-seconds "%.2h:%z%.2m:%.2s" (floor (/ (elt chapter 1) 1000)))
                         " "
                         (elt chapter 3)
                         "\n"))
                      chapters
                      "")
       :html (format "<pre data-target=\"%s\" class=\"chapters\">\n%s\n</pre>"
                     (or target "")
                     (mapconcat
                      (lambda (chapter)
                        (format "%s %s"
                                (format-seconds "%.2h:%z%.2m:%.2s" (floor (/ (elt chapter 1) 1000)))
                                (elt chapter 3)))
                      chapters
                      "\n"))))))

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
                    (concat (or track-base-url "") (file-name-nondirectory lang-file))))
        ""))
    '(("fr" . "French") ("ja" . "Japanese"))
    "")))

(defun emacsconf-link-file-formats (video-slug extensions)
  (string-join (emacsconf-link-file-formats-as-list video-slug extensions) " "))

(defun emacsconf-link-file-formats-as-list (talk extensions)
  (if (plist-get talk :files)
      (seq-map (lambda (file)
                 (if (eq (plist-get talk :format) 'wiki)
                     (format "[Download %s](%s%s)"
                             (replace-regexp-in-string (concat "^" (regexp-quote (plist-get talk :video-slug))) "" file) 
                             (or (plist-get talk :base-url) "")
                             file)
                   (format "<a href=\"%s%s\">Download %s</a>"
                           (or (plist-get talk :base-url) "")
                           file
                           (replace-regexp-in-string (concat "^" (regexp-quote (plist-get talk :video-slug))) "" file))))
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
                               (if (eq (plist-get talk :format) 'wiki)
                                   (format "[Download %s%s](%s%s)"
                                           ext
                                           size
                                           (or (plist-get talk :base-url) "")
                                           (concat video-slug ext))
                                 (format "<a href=\"%s%s\">Download %s%s</a>"
                                         (or (plist-get talk :base-url) "")
                                         (concat video-slug ext)
                                         ext
                                         size)))))
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


(defun emacsconf-generate-playlist (filename playlist-name talks &optional base-url)
  (with-temp-file filename
    (insert (format "#EXTM3U\n#PLAYLIST: %s\n#EXTALB: %s\n#EXTGENRE: Speech\n%s"
                    playlist-name playlist-name
                    (mapconcat
                     (lambda (talk)
                       (let* ((slug (plist-get talk :video-slug))
                              (filename (concat (plist-get talk :video-slug) "--main.webm")))
                         (if (and slug (file-exists-p (expand-file-name filename emacsconf-cache-dir))) 
                             (format "#EXTINF:-1,%s - %s\n%s%s\n"
                                     (plist-get talk :title)
                                     (plist-get talk :speakers)
                                     base-url
                                     filename)
                           "")))
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

(defun emacsconf-cache-all-video-data ()
  (interactive)
  (org-map-entries (lambda () (when (and (org-entry-get (point) "VIDEO_SLUG") (null (org-entry-get (point) "VIDEO_FILE_SIZE"))) (emacsconf-cache-video-data-for-entry)))))
(defvar emacsconf-cache-dir (expand-file-name "cache" (file-name-directory emacsconf-org-file)))
(defun emacsconf-cache-video-data (talk)
  (interactive (list (emacsconf-complete-talk-info)))
  (let ((main (expand-file-name (concat (plist-get talk :video-slug) "--main.webm") emacsconf-backstage-dir)))
    (emacsconf-with-talk-heading talk
      (let* ((video-file-name (emacsconf-get-preferred-video (org-entry-get (point) "VIDEO_SLUG")))
             (video-file (expand-file-name video-file-name emacsconf-cache-dir))
             duration)
        (unless (file-exists-p main)
          (setq main video-file-name))
        (org-entry-put (point) "VIDEO_FILE" (file-name-nondirectory video-file))
        (org-entry-put (point) "VIDEO_FILE_SIZE" (file-size-human-readable (file-attribute-size (file-attributes video-file))))
        (unless (plist-get talk :video-time)
          (setq duration (/ (compile-media-get-file-duration-ms video-file) 1000))
          (org-entry-put (point) "VIDEO_DURATION" (format-seconds "%m:%.2s" duration))
          (org-entry-put (point) "VIDEO_TIME" (number-to-string (ceiling (/ duration 60)))))))))

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
         (wiki-captions-directory (expand-file-name "captions" (expand-file-name emacsconf-year emacsconf-directory)))
         (new-captions-file (expand-file-name (concat slug "--main.vtt") wiki-captions-directory)))
    (org-entry-put (point) "PUBLIC" "1")
    (when (file-exists-p video-file)
      (emacsconf-youtube-edit)      
      (emacsconf-toobnix-edit)
      (emacsconf-cache-video-data-for-entry)
      (emacsconf-update-talk)
      (when (file-exists-p (expand-file-name (concat slug ".md")  wiki-captions-directory))
        (with-current-buffer (find-file-noselect (file-exists (expand-file-name (concat slug ".md")  wiki-captions-directory)))
          (magit-stage-file (buffer-file-name))))
      (mapc
       (lambda (suffix)
         (when (file-exists-p (expand-file-name (concat slug suffix) emacsconf-cache-dir))
           (copy-file (expand-file-name (concat slug suffix) emacsconf-cache-dir)
                      (expand-file-name (concat slug suffix) wiki-captions-directory)t)
           (with-current-buffer (find-file-noselect (expand-file-name (concat slug suffix) wiki-captions-directory))
             (magit-stage-file (buffer-file-name)))))
       '("--main.vtt" "--chapters.vtt" "--main_ja.vtt" "--main_fr.vtt"))
      (magit-status-setup-buffer emacsconf-directory)
      (when (and emacsconf-public-media-directory slug (> (length (string-trim slug)) 0)
                 ;; TODO: make this customizable
                 (shell-command
                  (format "ssh media.emacsconf.org -- 'rm /var/www/media.emacsconf.org/%s/%s* ; cp -n -l /var/www/media.emacsconf.org/%s/backstage/%s* /var/www/media.emacsconf.org/%s/; chmod ugo+r /var/www/media.emacsconf.org/%s/ -R'"
            emacsconf-year slug
            emacsconf-year slug
            emacsconf-year
            emacsconf-year)))
        (when emacsconf-public-media-directory
          (emacsconf-publish-public-index (expand-file-name "index.html" emacsconf-public-media-directory))
          (emacsconf-generate-playlist (expand-file-name "index.m3u" emacsconf-public-media-directory)
                                  "EmacsConf 2021"
                                  (emacsconf-public-talks (emacsconf-get-talk-info))))))
    ;; (copy-file (emacsconf-get-preferred-video slug) emacsconf-public-media-directory t)
    ;; (mapc (lambda (ext)
    ;;         (when (file-exists-p (expand-file-name (concat slug ext) emacsconf-cache-dir))
    ;;           (copy-file (expand-file-name (concat slug ext) emacsconf-cache-dir)
    ;;                      emacsconf-public-media-directory
    ;;                      t)))
    ;;       emacsconf-published-extensions)
    ))


(defmacro emacsconf-publish-with-wiki-change (&rest body)
  (declare (indent 0) (debug t))
  `(progn
     ,@body
     (let ((default-directory emacsconf-directory))
       (if (featurep 'magit)
           (progn
             (magit-with-toplevel
               (magit-stage-1 "-u" magit-buffer-diff-files))
             (magit-status-setup-buffer))
         (shell-command "git add -u"))
       ;; (when noninteractive
       ;;   (call-process "git" nil nil nil "commit" "-m" (if (stringp (car body))
       ;;                                                       (car body)
       ;;                                                     "Automated commit"))
       ;;   (call-process "git" nil nil nil "commit" "-m" (if (stringp (car body))
       ;;                                                     (car body)
       ;;                                                   "Automated commit")))
       )))

(defun emacsconf-publish-schedule-svg-snippets ()
  (interactive)
  (let* ((info (emacsconf-prepare-for-display (emacsconf-get-talk-info)))
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
                 track
                 "<tr><td><a href=\"/${year}/watch/${id}\">${name}</a></td><td><a href=\"${webchat}\">${channel}</a></td><td><a href=\"${stream}\">${stream}</a></td><td><a href=\"${480p}\">${id}-480p.webm</a></tr>"))
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
                 :end-info(emacsconf-surround " - <span class=\"sched-end\">" (plist-get talk :end) "</span>" "")
                 :track-info (emacsconf-surround (format " <span class=\"sched-track %s\">" (or (plist-get talk :track) "")) (plist-get talk :track) "</span>" "")
                 :q-info  (emacsconf-surround " <span class=\"sched-q-and-a\">Q&amp;A: " (plist-get talk :q-and-a) "</span>; " "")
                 :slug-info (emacsconf-surround " <span class=\"sched-slug\">id:" (plist-get talk :slug) "</span>" "")
                 :speaker-info (emacsconf-surround " <div class=\"sched-speakers\">" (plist-get talk :speakers-with-pronouns) "</div>" "")
                 :resources-info (emacsconf-surround "<ul class=\"resources\">" (plist-get talk :resources) "</ul>" ""))) 
   "<div data-start=\"${startutc}\" data-end=\"${endutc}\" class=\"sched-entry track-${track}\">
<div class=\"sched-meta\"><span class=\"sched-time\">${start-info}${end-info}</span>${track-info}${q-info}${slug-info}</div>
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
      " | ${stream-nav}</div>

<video controls><source src=\"${stream}\" type=\"video/webm\" /></video>
<div>Alternatively, load <a href=\"${stream-hires}\">${stream-hires}</a> or <a href=\"${480p}\">${480p}</a> (low-res) in a streaming media player such as MPV.</div>
<hr size=\"1\"><div>" (emacsconf-publish-page-nav nav "links") " | ${stream-nav}</div>"
      "<div>${brief}</div>
<div class=\"pad-output\"></div>
<hr size=\"1\"><div>" (emacsconf-publish-page-nav nav "chat") " | ${stream-nav}</div>"
"<div>Chat: <a href=\"${webchat}\">${channel}</a> on libera.chat</div>

<div class=\"chat-iframe\" data-track=\"${id}\"></div>
<iframe src=\"${webchat}\" height=\"600\" width=\"100%\"></iframe>
<hr size=\"1\"><div>" (emacsconf-publish-page-nav nav "sched") " | ${stream-nav}</div>"
"
<div>${sched}</div>
<div>${talks}</div>
"))))

(defun emacsconf-publish-watch-pages ()
  "Update /year/watch pages."
  (interactive)
  (mapc (lambda (track)
          (plist-put track :year emacsconf-year)
          (plist-put track :stream (concat emacsconf-stream-base (plist-get track :id) ".webm"))
          (plist-put track :stream-hires (concat emacsconf-stream-base (plist-get track :id) ".webm"))
          (plist-put track :480p (concat emacsconf-stream-base (plist-get track :id) "-480p.webm"))
          (plist-put track :webchat-channels (concat "emacsconf,emacsconf-" (plist-get track :id)))
          (plist-put track :webchat (concat emacsconf-chat-base "?join=" (plist-get track :webchat-channels)))
          (plist-put track :channel (concat "#emacsconf-" (plist-get track :id))))
        emacsconf-tracks)
  (let* ((info (sort (emacsconf-get-talk-info) #'emacsconf-sort-by-scheduled))
         (emacsconf-publishing-phase 'schedule)
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
              emacsconf-tracks)))
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
<pre>mpv https://live0.emacsconf.org:9001/emacsconf/gen.webm
vlc https://live0.emacsconf.org:9001/emacsconf/gen.webm
ffplay https://live0.emacsconf.org:9001/emacsconf/gen.webm
</pre>"
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
                 "</body></html>")))
            emacsconf-tracks))))


(defvar emacsconf-publish-current-dir "/ssh:orga@media.emacsconf.org:/var/www/media.emacsconf.org/2022/current"
  "Directory to publish BBB redirects and current information to.")


;; (assert (eq (emacsconf-get-bbb-state '(:status "OPEN_Q")) 'open))
;; (assert (eq (emacsconf-get-bbb-state '(:status "TO_ARCHIVE")) 'after))

(defun emacsconf-publish-bbb-redirect (talk)
  (interactive (list (emacsconf-complete-talk-info)))
  (let ((bbb-filename (expand-file-name (format "bbb-%s.html" (plist-get talk :slug))
                                        emacsconf-publish-current-dir))
        (bbb-redirect-url (concat "https://media.emacsconf.org/" emacsconf-year "/current/bbb-" (plist-get talk :slug) ".html"))
        (status (emacsconf-bbb-status (if (boundp 'org-state) (append (list :status org-state) talk) talk))))
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
           )
          ))))))

(defun emacsconf-publish-media-files-on-change (talk)
  "Publish the files and update the index."
  (interactive (list (emacsconf-complete-talk-info)))
  (when (or (not (boundp 'org-state))
            (string= org-state "PLAYING")
            (string= org-state "TO_STREAM"))
    (if (plist-get talk :public)
        ;; Copy main extension files from backstage to public
        (let ((files (directory-files emacsconf-backstage-dir nil
                                      (concat "^"
                                              (regexp-quote (plist-get talk :video-slug))
                                              (regexp-opt emacsconf-main-extensions)))))
          (mapc (lambda (file)
                  (copy-file (expand-file-name file emacsconf-backstage-dir)
                             (expand-file-name file emacsconf-public-media-directory) t))
                files))
      ;; Remove files from public
      (let ((files (directory-files emacsconf-public-media-directory nil
                                    (concat "^"
                                            (regexp-quote (plist-get talk :video-slug)
                                                          )))))
        (mapc (lambda (file)
                (delete-file (expand-file-name file emacsconf-public-media-directory)))
              files)))
    (emacsconf-publish-public-index)))

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

(provide 'emacsconf-publish)
