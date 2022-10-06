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


(defcustom emacsconf-media-base-url "https://media.emacsconf.org/" "Base URL for published media files."
  :type 'string
  :group 'emacsconf)

(defcustom emacsconf-main-extensions '(".org" ".odp" ".pdf" ".el" "--compressed56.webm" "--main.vtt" "--main_fr.vtt" "--main_ja.vtt" "--chapters.vtt" "--main--chapters.vtt")
  "Extensions to list on public pages."
  :type '(repeat string)
  :group 'emacsconf)

(defcustom emacsconf-protected-extensions '(".en.srv2" ".srt")
  "Extensions to list in the staging area."
  :group 'emacsconf)
(defcustom emacsconf-public-media-directory nil "Can be over TRAMP" :type 'string :group 'emacsconf)
(defcustom emacsconf-protected-media-directory nil "Can be over TRAMP" :type 'string :group 'emacsconf)

(defun emacsconf-update-talk ()
  "Publish the schedule page and the page for this talk."
  (interactive)
  (when (functionp 'emacsconf-upcoming-insert-or-update)
    (emacsconf-upcoming-insert-or-update))
  (let ((info (emacsconf-get-talk-info-for-subtree)))
    (emacsconf-generate-before-page info)
    (emacsconf-generate-after-page info))
  (emacsconf-generate-main-schedule))

(defun emacsconf-publish-add-talk ()
  "Add the current talk to the wiki."
  (interactive)
  (emacsconf-update-talk)
  (emacsconf-generate-info-pages)
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
      (emacsconf-generate-info-pages info)
      (emacsconf-generate-main-schedule info)
      (emacsconf-generate-talk-pages info force)
      (magit-status emacsconf-directory))))

(defun emacsconf-update-schedules-in-wiki ()
  (emacsconf-generate-info-pages)
  (emacsconf-generate-main-schedule)
  (emacsconf-generate-ical)
  (emacsconf-pentabarf-generate))

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
  (emacsconf-make-public-index-on-wiki)
  (when emacsconf-public-media-directory
    (emacsconf-make-public-index (expand-file-name "index.html" emacsconf-public-media-directory))
    (emacsconf-generate-playlist (expand-file-name "index.m3u" emacsconf-public-media-directory)
                                 "EmacsConf2021"
                                 (emacsconf-public-talks emacsconf-info)
                                 (format "https://media.emacsconf.org/%s/" emacsconf-year)))
  (when emacsconf-protected-media-directory
    (emacsconf-make-protected-index (expand-file-name "index.html" emacsconf-protected-media-directory)))
  (emacsconf-generate-playlist (expand-file-name "index.m3u" emacsconf-protected-media-directory)
                               "EmacsConf2021" (emacsconf-filter-talks emacsconf-info)
                               (format "https://media.emacsconf.org/%s/protected/" emacsconf-year)))

(defun emacsconf-index-card (talk &optional extensions)
  "Format an HTML card for TALK, linking the files  in EXTENSIONS."
  (let* ((video-slug (plist-get talk :video-slug))
         (video-file (and (plist-get talk :video-file) (expand-file-name (plist-get talk :video-file) emacsconf-captions-directory)))
         (video (emacsconf-index-card-video (or (plist-get talk :video-id) "mainVideo") video-file talk extensions)))
    ;; Add extra information to the talk
    (setq talk
          (append
           talk
           (list
            :video-html (plist-get video :video)
            :chapter-list (or (plist-get video :chapter-list) "")
            :resources (plist-get video :resources)
            :extra (or (plist-get talk :extra) "") 
            :speaker-info (or (plist-get talk :speakers) ""))))
    (if (eq (plist-get talk :format) 'wiki)
        (plist-get talk :video-html)
      (emacsconf-replace-plist-in-string
       talk
       "<div class=\"vid\">${video-html}${resources}${extra}${chapter-list}</div>"))))

(defun emacsconf-index-card-video (video-id video-file talk extensions)
  (let* ((wiki-caption-dir (expand-file-name
                            "captions"
                            (expand-file-name 
                             (plist-get talk :conf-year)
                             emacsconf-directory)))
         (chapter-info (and video-file
                            (emacsconf-make-chapter-strings
                             (expand-file-name
                              (concat (file-name-base video-file) "--chapters.vtt")
                              wiki-caption-dir)
                             (plist-get talk :track-base-url))))
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
                         (expand-file-name (concat (file-name-base video-file) ".vtt") wiki-caption-dir)
                         (or (plist-get talk :track-base-url)
                             (plist-get talk :base-url)))))
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
            :video-duration (if (and video-file (file-exists-p video-file))
                                (format-seconds "%m:%.2s" (/ (compile-media-get-file-duration-ms video-file) 1000)))
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
      (if (and video-file (file-exists-p video-file))
          (if (eq (plist-get talk :format) 'wiki)
              "[[!template id=\"vid\" vidid=\"${video-id}\" src=\"${source-src}\" poster=\"${poster}\" ${captions}
size=\"${video-file-size}\" duration=\"${video-duration}\" other_resources=\"\"\"${other-files}${toobnix-info}\"\"\"]]
${chapter-list}
"
            "<video controls preload=\"metadata\" poster=\"${poster}\" id=\"${video-id}\"><source src=\"${source-src}\" />${captions}${chapter-track}</video>")
        "The video for \"${title}\" will be posted here when available. You can also subscribe to the <a href=\"https://lists.gnu.org/mailman/listinfo/emacsconf-discuss\">emacsconf-discuss mailing list</a> for updates."))
     :resources
     (emacsconf-replace-plist-in-string
      (append info
              (list :video-download
                    (if video-file
                        (emacsconf-replace-plist-in-string
                         info
                         "<li><a href=\"${source-src}\">Download .webm video (${video-duration}, ${video-file-size}B)</a></li>")
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
                                          (unless (string= (plist-get o :pronouns) "nil") (plist-get o :pronouns))
                                          (when (plist-get o :irc) (format "IRC: %s" (plist-get o :irc)))
                                          (when (plist-get o :public-email) (format "<mailto:%s>" (plist-get o :public-email)))))
                               ", ")))
    (concat (plist-get o :speakers)
            (if (> (length extra-info) 0)
                (concat " (" extra-info ")")
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
                                       :video-id "qanda"
                                       :toobnix-url nil
                                       :video-file (expand-file-name
                                                    (concat (file-name-sans-extension (plist-get o :video-slug)) "--answers.webm")
                                                    emacsconf-captions-directory))
                                      o)
                                     (list "--answers.vtt" "--answers--chapters.vtt" "--answers--compressed32.webm")))
     "")))



(defun emacsconf-format-talk-schedule-info (o)
  (let ((friendly (concat "/" emacsconf-year "/talks/" (plist-get o :slug) ))
        (timestamp (org-timestamp-from-string (plist-get o :scheduled))))
    (concat
     "[[!toc  ]]\n"
     (if (plist-get o :q-and-a) (format "Q&A: %s  \n" (plist-get o :q-and-a)) "")
     (if (member emacsconf-publishing-phase '(program schedule)) (concat "Status: " (plist-get o :status-label) "  \n") "")
     "Duration: " (or (plist-get o :video-duration)
                      (concat (plist-get o :duration) " minutes"))
     "  \n"
     (if (and (member emacsconf-publishing-phase '(program schedule))
              (not (member (plist-get o :status) '("DONE" "CANCELLED" "STARTED"))))
         (let ((start (org-timestamp-to-time (org-timestamp-split-range timestamp)))
               (end (org-timestamp-to-time (org-timestamp-split-range timestamp t))))
           (format
            "<div class=\"times\" start=\"%s\" end=\"%s\">%s<br /><a href=\"/2021/\">Find out how to watch and participate</a></div>"
            (format-time-string "%Y-%m-%dT%H:%M:%SZ" start t)
            (format-time-string "%Y-%m-%dT%H:%M:%SZ" end t)
            (string-join (emacsconf-timezone-strings o) "<br />")))
       "") 
     "\n"
     (if (plist-get o :alternate-apac)
         (format "[[!inline pages=\"internal(%s/inline-alternate)\" raw=\"yes\"]]  \n" emacsconf-year)
       "")
     "\n"
     "If you have questions and the speaker has not indicated public contact information on this page, please feel free to e-mail us at <emacsconf-submit@gnu.org> and we'll forward your question to the speaker.\n\n"
     (if (plist-get o :public) (emacsconf-wiki-talk-resources o) "")
     "\n# Description\n\n")))

(defun emacsconf-format-email-questions-and-comments (talk)
  (format "Questions or comments? Please e-mail %s"
          (emacsconf-format-public-email talk
                                         (or
                                          (and (string= (plist-get talk :public-email) "t")
                                               (plist-get talk :email))
                                          (plist-get talk :public-email)
                                          "emacsconf-org-private@gnu.org"))))

(defun emacsconf-generate-before-page (talk)
  "Info included before the abstract."
  (interactive (list (emacsconf-get-talk-info-for-subtree)))
  (with-temp-file (expand-file-name (format "%s-before.md" (plist-get talk :slug))
                                    (expand-file-name "info" (expand-file-name emacsconf-year emacsconf-directory)))
    (insert "<!-- Automatically generated by emacsconf-generate-before-page -->\n")
    (when (eq emacsconf-publishing-phase 'schedule)
      (insert "\n"
              (emacsconf-format-talk-schedule-info talk) "\n"))
    ;; Contact information
    ;; (insert "\n\n" (emacsconf-format-email-questions-and-comments talk) "\n")
    (insert "<!-- End of emacsconf-generate-before-page -->")))

(defun emacsconf-generate-after-page (talk &optional info)
  "Info included before the abstract."
  (interactive (list (emacsconf-get-talk-info-for-subtree)))
  ;; Contact information
  (with-temp-file (expand-file-name (format "%s-after.md" (plist-get talk :slug))
                                    (expand-file-name "info" (expand-file-name emacsconf-year emacsconf-directory)))
    (insert "<!-- Automatically generated by emacsconf-generate-after-page -->\n")
    (insert "\n\n"
            (emacsconf-format-email-questions-and-comments talk) "\n")
    (insert "<!-- End of emacsconf-generate-after-page -->\n")))

(defun emacsconf-generate-nav-pages (&optional talks)
  (interactive (list
                (seq-remove (lambda (o) (string= (plist-get o :status) "CANCELLED"))
                            (sort (emacsconf-filter-talks (emacsconf-get-talk-info)) #'emacsconf-sort-by-scheduled))))
  (let* ((next-talks (cdr talks))
         (prev-talks (cons nil talks)))
    (unless (file-directory-p (expand-file-name "info" (expand-file-name emacsconf-year emacsconf-directory)))
      (mkdir (expand-file-name "info" (expand-file-name emacsconf-year emacsconf-directory))))
    (while talks
      (let* ((o (pop talks))
             (next-talk (emacsconf-format-talk-link (pop next-talks)))
             (prev-talk (emacsconf-format-talk-link (pop prev-talks))))
        (with-temp-file (expand-file-name (format "%s-nav.md" (plist-get o :slug))
                                          (expand-file-name "info" (expand-file-name emacsconf-year emacsconf-directory)))
          (insert (concat "Back to the [[talks]]  \n"
                          (if prev-talk (format "Previous: %s  \n" prev-talk) "")
                          (if next-talk (format "Next: %s  \n" next-talk) "")
                          (if (plist-get o :track) ; tagging doesn't work here because ikiwiki will list the nav page
                              (format "Track: %s  \n" (plist-get o :track))
                            ""))))))))

(defun emacsconf-generate-info-pages (&optional info)
  (interactive)
  "Populate year/info/*-nav, -before, and -after files."
  (let* ((talks (seq-remove (lambda (o) (string= (plist-get o :status) "CANCELLED"))
                            (sort (emacsconf-filter-talks (or info (emacsconf-get-talk-info))) #'emacsconf-sort-by-scheduled))))
    (emacsconf-generate-nav-pages talks)
    (mapc #'emacsconf-generate-before-page talks)
    (mapc #'emacsconf-generate-after-page talks)))

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
                   (format "\n\n<a name=\"%s\"></a>\n## %s\n\n%s\n\n"
                           id
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

(defun emacsconf-publish-schedule ()
  (interactive)
  (emacsconf-generate-main-schedule-with-tracks)
  (let ((default-directory emacsconf-directory))
    (magit-status emacsconf-directory)
    (magit-stage-modified)
    (magit-commit-create (list "-m" (read-string "Commit message: ")))
    (call-interactively #'magit-push-current-to-pushremote)))

(defun emacsconf-generate-main-schedule (&optional info)
  (interactive)
  (with-temp-file (expand-file-name "schedule-details.md" (expand-file-name emacsconf-year emacsconf-directory))
    (insert
     (if (eq emacsconf-publishing-phase 'program)
         (let ((sorted (sort (emacsconf-active-talks
                              (emacsconf-filter-talks
                               (or info (emacsconf-get-talk-info))))
                             #'emacsconf-sort-by-scheduled)))
           (concat
            "<a href=\"#development\">Jump to development talks</a>\n<a name=\"general\"></a>\n# General talks\n"
            (emacsconf-format-main-schedule
             (seq-filter (lambda (o) (string= (plist-get o :track) "General")) sorted))
            "\n<a name=\"development\"></a>\n# Development talks\n"
            (emacsconf-format-main-schedule
             (seq-filter (lambda (o) (string= (plist-get o :track) "Development")) sorted))))
       (let* ((by-day (seq-group-by (lambda (o)
                                      (format-time-string "%Y-%m-%d" (plist-get o :start-time) emacsconf-timezone))
                                    (sort (seq-filter (lambda (o)
                                                        (or (plist-get o :slug)
                                                            (plist-get o :include-in-info)))
                                                      (emacsconf-get-talk-info))
                                          #'emacsconf-sort-by-scheduled)))
              (dates (seq-map (lambda (o) (plist-get (cadr o) :start-time))
                              by-day))
              (links (mapcar (lambda (o)
                               (format "<a href=\"#date-%s\">%s</a>"
                                       (format-time-string "%Y-%m-%d" o emacsconf-timezone)
                                       (format-time-string "%a %b %-e" o emacsconf-timezone)))
                             dates)))
         (mapconcat (lambda (day)
                      (with-temp-file (expand-file-name (concat emacsconf-year "/talks/"
                                                                (format-time-string "schedule-%Y-%m-%d.svg" 
                                                                                    (plist-get (cadr day) :start-time)
                                                                                    emacsconf-timezone))
                                                        emacsconf-directory)
                        (let ((height 200) (width 800))
                          (svg-print
                           (emacsconf-schedule-svg-day
                            (svg-create width height)
                            (format-time-string "%a" (plist-get (cadr day) :start-time) emacsconf-timezone)
                            width height
                            (date-to-time (format-time-string "%Y-%m-%d 9:00" (plist-get (cadr day) :start-time)))
                            (date-to-time (format-time-string "%Y-%m-%d 17:00" (plist-get (cadr day) :start-time)))
                            (list (seq-filter (lambda (o) (string= (plist-get o :track) "General")) (cdr day))
                                  (seq-filter (lambda (o) (string= (plist-get o :track) "Development")) (cdr day)))
                            ))))
                      (concat
                       (if (> (length links) 1) (concat "Jump to: " (string-join links " - ")) "")
                       (format "<a name=\"date-%s\"></a>\n"
                               (format-time-string "%Y-%m-%d"
                                                   (plist-get (cadr day) :start-time)
                                                   emacsconf-timezone))
                       (format-time-string "# %A %b %-e, %Y\n" (plist-get (cadr day) :start-time) emacsconf-timezone)
                       (format "<img src=\"/%s/talks/schedule-%s.svg\" alt=\"Schedule\"/>  \n"
                               emacsconf-year
                               (format-time-string "%Y-%m-%d"
                                                   (plist-get (cadr day) :start-time)
                                                   emacsconf-timezone))
                       (emacsconf-format-main-schedule (cdr day))))
                    by-day
                    "\n\n")))))
  (magit-status-setup-buffer emacsconf-directory))

(defun emacsconf-format-talk-link (talk)
  (and talk (if (plist-get talk :slug)
                (format "<a href=\"/%s/talks/%s\">%s</a>"
                        emacsconf-year
                        (plist-get talk :slug)
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


(defun emacsconf-format-main-schedule (info)
  (let* ((cancelled (seq-filter (lambda (o) (string= (plist-get o :status) "CANCELLED")) info)))
    (concat
     (format
      "<div class=\"schedule\" data-start=\"%s\">\n"
      (format-time-string "%FT%T%z" (plist-get (car info) :start-time) t))
     (mapconcat
      (lambda (o)
        (let* ((status (pcase (plist-get o :status)
                         ("CAPTIONED" "captioned")
                         ("PREREC_RECEIVED" "received")
                         ("DONE" "done")
                         ("STARTED" "now playing")
                         (_ nil))))
          (format "[[!template id=sched%s%s]]"
                  (let ((result "")
                        (attrs (list
                                :title (plist-get o :title)
                                :url (plist-get o :url)
                                :speakers (plist-get o :speakers)
                                :track (plist-get o :track)
                                :slug (plist-get o :slug)
                                :status (if (eq emacsconf-publishing-phase 'program)
                                            nil
                                          status)
                                :time (if (eq emacsconf-publishing-phase 'program)
                                          nil
                                        (plist-get o :time))
                                :startutc
                                (if (eq emacsconf-publishing-phase 'program)
                                    nil
                                  (format-time-string "%FT%T%z" (plist-get o :start-time) t))
                                :start                                
                                (if (eq emacsconf-publishing-phase 'program)
                                    nil
                                  (format-time-string "%-l:%M" (plist-get o :start-time)))
                                :end
                                (if (eq emacsconf-publishing-phase 'program)
                                    nil
                                  (format-time-string "%-l:%M" (plist-get o :end-time)))
                                :q-and-a
                                (plist-get o :q-and-a)
                                )))
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
                    ""))))
      (emacsconf-active-talks info)
      "\n")
     "\n</div>\n"
     (if (> (length cancelled) 0)
         (format "<div class=\"cancelled\">Cancelled:<ul>%s</ul></div>"
                 (mapconcat (lambda (talk) (format "<li><a href=\"/%s/talks/%s\">%s</a> - %s</li>"
                                                   emacsconf-year
                                                   (plist-get talk :slug)
                                                   (plist-get talk :title)
                                                   (plist-get talk :speakers)))
                            cancelled "\n"))
       ""))))

(defun emacsconf-timezone-strings (o)
  (let* ((timestamp (org-timestamp-from-string (plist-get o :scheduled)))
         (start (org-timestamp-to-time (org-timestamp-split-range timestamp)))
         (end (org-timestamp-to-time (org-timestamp-split-range timestamp t))))
    (mapcar
     (lambda (tz)
       (format "%s - %s"
               (format-time-string "%A, %b %e %Y, ~%l:%M %p"
                                   start tz)
               (format-time-string "%l:%M %p %Z"
                                   end tz)))
     emacsconf-timezones)))

(defun emacsconf-make-protected-index (filename)
  (interactive (list (expand-file-name "index.html" emacsconf-protected-media-directory))) 
  (setq emacsconf-info (emacsconf-get-talk-info))
  (with-temp-file filename
    (let* ((talks (seq-filter (lambda (o) (plist-get o :video-file)) (emacsconf-filter-talks emacsconf-info)))
           (received (seq-remove (lambda (o) (plist-get o :captioner)) talks))
           (captioned (seq-filter (lambda (o) (plist-get o :captioner)) talks)))
      (insert
       "<html><meta charset=\"UTF-8\"><body>"
       (format "<h1>Talks to be captioned (%d minutes)</h1><ul class=\"videos\">"
               (apply '+ (seq-map (lambda (talk) (string-to-number (plist-get talk :duration)))
                                  received)))
       (mapconcat
        (lambda (f)
          (format  "<li><strong>%s</strong><br />%s<br />%s</li>"
                   (plist-get f :title)
                   (plist-get f :speakers)
                   (emacsconf-index-card
                    (append
                     f
                     (list :extra
                           (if (plist-get f :caption-note) (concat "<div class=\"caption-note\">" (plist-get f :caption-note) "</div>") "")))
                    (append emacsconf-main-extensions emacsconf-protected-extensions))))
        received
        "\n")
       (format
        "</ul><h1>%d captioned talks ready for enjoyment (%d minutes)</h1>"
        (length captioned)
        (apply '+ (seq-map (lambda (talk) (string-to-number (plist-get talk :duration))) captioned)))
       "<ol class=\"videos\">"
       (mapconcat (lambda (f) (format "<li><strong>%s</strong><br />%s<br />%s</li>"
                                      (plist-get f :title)
                                      (plist-get f :speakers)
                                      (emacsconf-index-card f emacsconf-main-extensions)))
                  captioned "\n")
       "</ol>"
       (if (file-exists-p (expand-file-name "include-in-index.html" emacsconf-captions-directory))
           (with-temp-buffer (insert-file-contents (expand-file-name "include-in-index.html" emacsconf-captions-directory)) (buffer-string))
         "")
       "</body></html>"))))

(defun emacsconf-make-public-index (filename)
  (interactive (list (expand-file-name "index.html" emacsconf-public-media-directory))) 
  (setq emacsconf-info (emacsconf-get-talk-info))
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
                          (emacsconf-index-card o '(".org" ".pdf" "--main.vtt" "--compressed56.webm"))
                          (if (plist-get o :qa-public)
                              (format "<li><div class=\"title\">Q&A for %s</div>%s</li>"
                                      (plist-get o :title)
                                      (emacsconf-index-card (append
                                                             (list
                                                              :public 1
                                                              :video-id "qanda"
                                                              :toobnix-url nil
                                                              :video-file (expand-file-name
                                                                           (concat (file-name-sans-extension (plist-get o :video-slug))
                                                                                   "--answers.webm")
                                                                           emacsconf-captions-directory))
                                                             o)
                                                            (list "--answers.vtt" "--answers--chapters.vtt")))
                            "")))
                (emacsconf-public-talks emacsconf-info)
                "\n")
     "</ol>"
     (if (file-exists-p (expand-file-name "include-in-index.html" emacsconf-captions-directory))
         (with-temp-buffer (insert-file-contents (expand-file-name "include-in-index.html" emacsconf-captions-directory)) (buffer-string))
       "")
     "</body></html>")))

(defun emacsconf-make-public-index-on-wiki ()
  (interactive)
  (let ((info (seq-filter (lambda (o)
                            (not (string= (plist-get o :status) "CANCELLED")))
                          (emacsconf-filter-talks (emacsconf-get-talk-info)))))
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
                                     (format "/%s/captions/" (plist-get f :conf-year)))
                               f)
                       emacsconf-main-extensions)
                    "")
                  (if (plist-get f :qa-public)
                      (emacsconf-index-card
                       (append
                        (list
                         :public 1
                         :base-url (concat emacsconf-media-base-url (plist-get f :conf-year) "/")
                         :video-id "qanda"
                         :track-base-url
                         (format "/%s/captions/" (plist-get f :conf-year))
                         :video-file (expand-file-name
                                      (concat (file-name-sans-extension (plist-get f :video-slug)) "--answers.webm")
                                      emacsconf-captions-directory))
                        f)
                       (list "--answers.vtt" "--answers--chapters.vtt"))
                    "")))
        info "\n"))
      "</ol>")))



(defun emacsconf-make-chapter-strings (filename track-base-url)
  (when (file-exists-p filename)
    (let ((chapters (with-temp-buffer
                      (insert-file-contents filename)
                      (subed--init "vtt")
                      (subed-subtitle-list))))
      (list
       :track (format "<track kind=\"chapters\" label=\"Chapters\" src=\"%s\"\" />"
                      (concat (or track-base-url "") (file-name-nondirectory filename)))
       :md (subed-convert--chapters chapters)
       :html (format "<ol class=\"chapters\">\n%s\n</ol>"
                     (mapconcat
                      (lambda (chapter)
                        (format "<li data-start=\"%.3f\" data-stop=\"%.3f\">%s %s</li>"
                                (/ (elt chapter 1) 1000.0)
                                (/ (elt chapter 2) 1000.0)
                                (format-seconds "%.2h:%z%.2m:%.2s" (floor (/ (elt chapter 1) 1000)))
                                (elt chapter 3)))
                      chapters
                      "\n"))))))

(defun emacsconf-video-subtitle-tracks (filename track-base-url)
  (concat
   (if (file-exists-p filename)
       (format "<track label=\"English\" kind=\"captions\" srclang=\"en\" src=\"%s\" default />"
               (concat (or track-base-url "") (file-name-nondirectory filename)))
     "")
   (mapconcat
    (lambda (lang)
      (let ((lang-file (concat (file-name-sans-extension filename) "_" (car lang) "." (file-name-extension filename))))
        (if (file-exists-p lang-file)
            (format "<track label=\"%s\" kind=\"captions\" srclang=\"%s\" src=\"%s\" />"
                    (cdr lang)
                    (car lang)
                    (concat (or track-base-url "") (file-name-nondirectory lang-file)))
          "")))
    '(("fr" . "French") ("ja" . "Japanese"))
    "")))

(defun emacsconf-link-file-formats (video-slug extensions)
  (string-join (emacsconf-link-file-formats-as-list video-slug extensions) " "))

(defun emacsconf-link-file-formats-as-list (talk extensions)
  (let ((video-slug (plist-get talk :video-slug))
        (wiki-captions-dir (expand-file-name "captions" (expand-file-name (plist-get talk :conf-year) emacsconf-directory))))
    (delq nil (seq-map (lambda (ext)
                         (let ((file (expand-file-name
                                      (concat video-slug ext)
                                      (if (string-match "\\.vtt$" ext)
                                          wiki-captions-dir
                                        emacsconf-captions-directory)))
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
                       extensions))))
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
                                               emacsconf-captions-directory))
                                  (qa-video (expand-file-name
                                             (concat (plist-get o :video-slug) "--answers.webm")
                                             emacsconf-captions-directory))
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
                         (if (and slug (file-exists-p (expand-file-name filename emacsconf-captions-directory))) 
                             (format "#EXTINF:-1,%s - %s\n%s%s\n"
                                     (plist-get talk :title)
                                     (plist-get talk :speakers)
                                     base-url
                                     filename)
                           "")))
                     talks
                     "")))))

(defun emacsconf-get-preferred-video (video-slug)
  (or
   (seq-find
    'file-exists-p
    (seq-map (lambda (suffix)
               (expand-file-name (concat video-slug "--" suffix ".webm")
                                 emacsconf-captions-directory))
             '("main" "captioned" "normalized" "compressed")))
   (car (directory-files emacsconf-captions-directory
                         nil
                         (concat (regexp-quote video-slug)
                                 "\\."
                                 (regexp-opt subed-video-extensions))))))

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
(defun emacsconf-cache-video-data-for-entry ()
  (interactive)
  (let* ((video-file (emacsconf-get-preferred-video (org-entry-get (point) "VIDEO_SLUG")))
         (duration (/ (compile-media-get-file-duration-ms video-file) 1000)))
    (org-entry-put (point) "VIDEO_FILE" (file-name-nondirectory video-file))
    (org-entry-put (point) "VIDEO_FILE_SIZE" (file-size-human-readable (file-attribute-size (file-attributes video-file))))
    (org-entry-put (point) "VIDEO_DURATION" (format-seconds "%m:%.2s" duration))
    (org-entry-put (point) "TIME" (number-to-string (ceiling (/ duration 60))))))

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
        (copy-file (expand-file-name (concat (org-entry-get (point) "VIDEO_SLUG") "--main.webm") emacsconf-captions-directory)
                   (expand-file-name (concat "emacsconf-" emacsconf-year "-" (org-entry-get (point) "SLUG") ".webm") emacsconf-captions-directory) t))
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
         (when (file-exists-p (expand-file-name (concat slug suffix) emacsconf-captions-directory))
           (copy-file (expand-file-name (concat slug suffix) emacsconf-captions-directory)
                      (expand-file-name (concat slug suffix) wiki-captions-directory)t)
           (with-current-buffer (find-file-noselect (expand-file-name (concat slug suffix) wiki-captions-directory))
             (magit-stage-file (buffer-file-name)))))
       '("--main.vtt" "--chapters.vtt" "--main_ja.vtt" "--main_fr.vtt"))
      (magit-status-setup-buffer emacsconf-directory)
      (when (and emacsconf-public-media-directory slug (> (length (string-trim slug)) 0)
                 ;; TODO: make this customizable
                 (shell-command
                  (format "ssh front -- 'rm /var/www/media.emacsconf.org/%s/%s* ; cp -n -l /var/www/media.emacsconf.org/%s/protected/%s* /var/www/media.emacsconf.org/%s/; chmod ugo+r /var/www/media.emacsconf.org/%s/ -R'"
            emacsconf-year slug
            emacsconf-year slug
            emacsconf-year
            emacsconf-year)))
        (when emacsconf-public-media-directory
          (emacsconf-make-public-index (expand-file-name "index.html" emacsconf-public-media-directory))
          (emacsconf-generate-playlist (expand-file-name "index.m3u" emacsconf-public-media-directory)
                                  "EmacsConf 2021"
                                  (emacsconf-public-talks (emacsconf-get-talk-info))))))
    ;; (copy-file (emacsconf-get-preferred-video slug) emacsconf-public-media-directory t)
    ;; (mapc (lambda (ext)
    ;;         (when (file-exists-p (expand-file-name (concat slug ext) emacsconf-captions-directory))
    ;;           (copy-file (expand-file-name (concat slug ext) emacsconf-captions-directory)
    ;;                      emacsconf-public-media-directory
    ;;                      t)))
    ;;       emacsconf-published-extensions)
    ))



(provide 'emacsconf-publish)
