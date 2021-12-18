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
  (emacsconf-upcoming-insert-or-update)
  (emacsconf-generate-schedule-page (emacsconf-get-talk-info-for-subtree))
  (emacsconf-generate-main-schedule))

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

(when (featurep 'memoize)
  (memoize #'compile-media-get-file-duration-ms))

(defun emacsconf-create-talk-pages (emacsconf-info)
  (interactive (list (emacsconf-get-talk-info)))
  "Populate year/talks/*.md files.
These should include the nav and schedule files, which will be
rewritten as needed.  After they are generated, they should be all
right to manually edit to include things like additional
resources."
  ;; TODO: No longer necessary?
  (require 's)
  (mapc (lambda (o)
          (when (plist-get o :talk-id)
            (let ((filename (expand-file-name (format "%s.md" (plist-get o :slug))
                                              (expand-file-name "talks" (expand-file-name emacsconf-year emacsconf-directory)))))
              (unless (file-exists-p filename)
                (with-temp-file filename
                  (let ((meta "!meta")
                        (title (or (plist-get o :title) ""))
                        (speakers (or (plist-get o :speakers) ""))
                        (talk-id (or (plist-get o :talk-id) ""))
                        (slug (or (plist-get o :slug) ""))
                        (info (or (plist-get o :info) "")))
                    (insert
                     (s-lex-format "[[${meta} title=\"${title}\"]]
[[${meta} copyright=\"Copyright &copy; ${emacsconf-year} ${speakers}\"]]
[[!inline pages=\"internal(${emacsconf-year}/info/${slug}-nav)\" raw=\"yes\"]]

<!-- You can manually edit this file to update the abstract, add links, etc. --->\n

# ${title}
${speakers}

${info}

[[!inline pages=\"internal(${emacsconf-year}/info/${slug}-schedule)\" raw=\"yes\"]]

[[!inline pages=\"internal(${emacsconf-year}/info/${slug}-nav)\" raw=\"yes\"]]
"))))))))
        emacsconf-info))

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

(defun emacsconf-generate-schedule-page (talk)
  (interactive (list (emacsconf-get-talk-info-for-subtree)))
  (with-temp-file (expand-file-name (format "%s-schedule.md" (plist-get talk :slug))
                                    (expand-file-name "info" (expand-file-name emacsconf-year emacsconf-directory)))
    (insert
     "<!-- Automatically generated by conf-create-info-pages -->\n\n"
     (emacsconf-format-talk-schedule-info talk) "\n")))

(defun emacsconf-generate-info-pages ()
  (interactive)
  "Populate year/info/*-nav and *-schedule.md files."
  ;; TODO: No longer necessary?
  (require 's)
  (let* ((talks (seq-remove (lambda (o) (string= (plist-get o :status) "CANCELLED"))
                            (emacsconf-filter-talks (emacsconf-get-talk-info))))
         (next-talks (cdr talks))
         (prev-talks (cons nil talks)))
    (while talks
      (let* ((o (pop talks))
             (next-talk (emacsconf-format-talk-link (pop next-talks)))
             (prev-talk (emacsconf-format-talk-link (pop prev-talks)))
             (friendly (concat "/" emacsconf-year "/talks/" (plist-get o :slug) ))
             (nav-links (format "Back to the [[schedule]]  \n%s%s"
                                (if prev-talk (format "Previous: %s  \n" prev-talk) "")
                                (if next-talk (format "Next: %s  \n" next-talk) ""))))
        (with-temp-file (expand-file-name (format "%s-nav.md" (plist-get o :slug))
                                          (expand-file-name "info" (expand-file-name emacsconf-year emacsconf-directory)))
          (insert nav-links))
        (emacsconf-generate-schedule-page o)))))

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
                           (if (null (plist-get o :talk-id))
                               (format "<tr><td colspan=\"3\">%s</td></tr>" (emacsconf-format-talk-link o))
                             (format "<tr><td>%s</td><td>%s</td><td>%s</td><tr>" 
                                     (plist-get o :duration)
                                     (emacsconf-format-talk-link o)
                                     (plist-get o :speakers)))))
                       info "\n")))
      (save-buffer))))

(defun emacsconf-generate-main-schedule (&optional filename)
  (interactive)
  (with-temp-file (expand-file-name "schedule-details.md" (expand-file-name emacsconf-year emacsconf-directory))
    (insert (emacsconf-format-talk-info-as-schedule (emacsconf-get-talk-info)))))

(defun emacsconf-format-talk-link (talk)
  (and talk (if (plist-get talk :talk-id)
                (format "<a href=\"/%s/talks/%s\">%s</a>"
                        emacsconf-year
                        (plist-get talk :slug)
                        (plist-get talk :title))
              (plist-get talk :title))))


(defun emacsconf-format-talk-info-as-schedule (info)
  (let* ((talks (seq-filter
                 (lambda (o)
                   (and (not (string= (plist-get o :status) "CANCELLED"))
                        (plist-get o :speakers)))
                 (emacsconf-filter-talks info)))
         (captioned (seq-filter (lambda (o) (plist-get o :captioner)) talks))
         (cancelled (seq-filter (lambda (o) (string= (plist-get o :status) "CANCELLED")) info))
         (received (seq-remove (lambda (o) 
                                 (plist-get o :captioner)) 
                               talks)))
    (format "<div>%d talks total: %d captioned (%d min), %d waiting for captions (%d min)</div>
<table width=\"100%%\">%s%s</table>%s"
            (length talks)
            (length captioned)
            (apply '+ (mapcar (lambda (info) (string-to-number (plist-get info :duration))) captioned))
            (length received)
            (apply '+ (mapcar (lambda (info) (string-to-number (plist-get info :duration)))
                              received))
            (pcase emacsconf-publishing-phase
              ('program "<tr><th>Status</th><th>Title<th><th>Speaker(s)</th></tr>")
              ('schedule "<tr><th>Status</th><th>Start</th><th>Title</th><th>Speaker(s)</th></tr>")
              ('resources "<tr><th>Title</th><th>Speaker(s)</th><th>Resources</th></tr>"))
            (mapconcat
             (lambda (o)
               (let* ((time-fmt "%l:%M %p")
                      (timestamp (org-timestamp-from-string (plist-get o :scheduled)))
                      (start (if timestamp (format-time-string time-fmt (org-timestamp-to-time (org-timestamp-split-range timestamp))) ""))
                      (end (if timestamp (format-time-string time-fmt (org-timestamp-to-time (org-timestamp-split-range timestamp t))) ""))
                      (title (plist-get o :title))
                      (status (pcase (plist-get o :status)
                                ("CAPTIONED" "captioned")
                                ("PREREC_RECEIVED" "received")
                                ("DONE" "done")
                                ("STARTED" "now playing")
                                (_ "")))
                      (speakers (or (plist-get o :speakers) "")))
                 (pcase emacsconf-publishing-phase
                   ('program
                    (if (eq (plist-get o :type) 'headline)
                        (format "<tr><td colspan=\"3\"><strong>%s<strong></td></tr>"
                                (if (plist-get o :slug)
                                    (emacsconf-format-talk-link o)
                                  title))
                      (format "<tr><td>%s</td><td>%s</td><td>%s</td></tr>"
                              status
                              (emacsconf-format-talk-link o) speakers)))
                   ('schedule
                    (if (eq (plist-get o :type) 'headline)
                        (format "<tr><td colspan=\"4\"><strong>%s<strong></td></tr>"
                                (if (plist-get o :slug)
                                    (emacsconf-format-talk-link o)
                                  title))
                      (format "<tr><td>%s</td><td width=100>~%s</td><td>%s</td><td>%s</td></tr>"
                              status
                              start (emacsconf-format-talk-link o) speakers)))
                   ('resources
                    (if (eq (plist-get o :type) 'headline)
                        (format "<tr><td colspan=\"3\"><strong>%s<strong></td></tr>"
                                (if (plist-get o :slug)
                                    (emacsconf-format-talk-link o)
                                  title))
                      (format "<tr><td>%s</td><td>%s</td><td><ul>%s</ul></td></tr>"
                              (emacsconf-format-talk-link o) speakers
                              (mapconcat (lambda (s) (concat "<li>" s "</li>"))
                                         (emacsconf-link-file-formats-as-list
                                          (append o
                                                  (list :base-url (format "%s%s/" emacsconf-media-base-url emacsconf-year)))
                                          (append emacsconf-main-extensions '("--main.webm")))
                                         "")))))))
             (seq-remove (lambda (o) (string= (plist-get o :status) "CANCELLED"))
                         (cdr info))
             "\n")
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
                                 (string-trim (shell-command-to-string (concat "sha1sum -b " (shell-quote-argument main-video) " | cut -d ' ' -f 1"))))
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
                                     (string-trim (shell-command-to-string (concat "sha1sum -b " (shell-quote-argument qa-video) " | cut -d ' ' -f 1")))
                                     ))))))
                          (emacsconf-public-talks (emacsconf-get-talk-info))))))
      (insert (orgtbl-to-csv
               (cons '("Conference" "Slug" "Title" "Speakers" "Talk page URL" "Video URL" "Date" "Duration" "SHA")
                     results)
               nil)))))

(provide 'emacsconf-publish)
