
(defcustom conf-media-base-url "https://media.emacsconf.org/" "Base URL for published media files."
  :type 'string
  :group 'emacsconf)

(defcustom conf-main-extensions '(".org" ".odp" ".pdf" ".el" "--main.vtt" "--main_fr.vtt" "--main_ja.vtt" "--chapters.vtt" "--main--chapters.vtt")
  "Extensions to list on public pages."
  :type '(repeat string)
  :group 'emacsconf)

(defcustom conf-protected-extensions '(".en.srv2" ".srt")
  "Extensions to list in the staging area."
  :group 'emacsconf)
(defcustom conf-public-media-directory nil "Can be over TRAMP" :type 'string :group 'emacsconf)
(defcustom conf-protected-media-directory nil "Can be over TRAMP" :type 'string :group 'emacsconf)

(defun conf-index-card (talk &optional extensions)
  "Format an HTML card for TALK, linking the files  in EXTENSIONS."
    (let* ((video-slug (plist-get talk :video-slug))
           (video-file (and (plist-get talk :video-file) (expand-file-name (plist-get talk :video-file) conf-captions-directory)))
           (video (conf-index-card-video (or (plist-get talk :video-id) "mainVideo") video-file talk extensions)))
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
      (conf-replace-plist-in-string
       talk
       "<div class=\"vid\">${video-html}${resources}${extra}${chapter-list}</div>")))

(defun conf-index-card-video (video-id video-file talk extensions)
  (let* ((wiki-caption-dir (expand-file-name
                            "captions"
                            (expand-file-name 
                             (plist-get talk :conf-year)
                             conf-directory)))
         (chapter-info (and video-file
                            (conf-make-chapter-strings
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
                  (format "%s%s/%s" conf-media-base-url (plist-get talk :conf-year)
                          (file-name-nondirectory video-file))
                (file-name-nondirectory video-file)))
            :captions
            (and video-file
                 (conf-video-subtitle-tracks
                  (expand-file-name (concat (file-name-base video-file) ".vtt") wiki-caption-dir)
                  (or (plist-get talk :track-base-url)
                      (plist-get talk :base-url))))
            :chapter-track (or (plist-get chapter-info :track) "")
            :chapter-list (or (plist-get chapter-info :html) "")
            :video-id video-id
            :video-duration (if (and video-file (file-exists-p video-file))
                                (format-seconds "%m:%.2s" (/ (conf-get-file-duration-ms video-file) 1000)))
            :video-file-size (if (and video-file (file-exists-p video-file))
                                 (file-size-human-readable (file-attribute-size (file-attributes video-file))))
            :other-files (mapconcat (lambda (s) (concat "<li>" s "</li>"))
                                    (conf-link-file-formats-as-list talk (or extensions conf-published-extensions))
                                    "")
            :poster (and video-file (format "https://media.emacsconf.org/%s/%s.png" (plist-get talk :conf-year) (file-name-base video-file)))
            :toobnix-info (if (plist-get talk :toobnix-url)
                              (format
                               "<li><a href=\"%s\">View on Toobnix</a></li>"
                               (plist-get talk :toobnix-url))
                            ""))
           
           talk)))
    (list
     
     :video
     (conf-replace-plist-in-string
      info
      (if (and video-file (file-exists-p video-file))
          "<video controls preload=\"metadata\" poster=\"${poster}\" id=\"${video-id}\"><source src=\"${source-src}\" />${captions}${chapter-track}</video>"
        "The video for \"${title}\" will be posted here when available. You can also subscribe to the <a href=\"https://lists.gnu.org/mailman/listinfo/emacsconf-discuss\">emacsconf-discuss mailing list</a> for updates."))
     :chapter-list (plist-get chapter-info :html)
     :resources
     (conf-replace-plist-in-string
      (append info
              (list :video-download
                    (if video-file
                        (conf-replace-plist-in-string
                         info
                         "<li><a href=\"${source-src}\">Download .webm video (${video-duration}, ${video-file-size}B)</a></li>")
                      "")))
      "<div class=\"files resources\"><ul>${video-download}${other-files}${toobnix-info}</ul></div>"))))

(when (featurep 'memoize)
  (memoize #'conf-get-file-duration-ms))

(defun conf-create-talk-pages (conf-info)
  (interactive (list (conf-get-talk-info)))
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
                                              (expand-file-name "talks" (expand-file-name conf-year conf-directory)))))
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
[[${meta} copyright=\"Copyright &copy; ${conf-year} ${speakers}\"]]
[[!inline pages=\"internal(${conf-year}/info/${slug}-nav)\" raw=\"yes\"]]

<!-- You can manually edit this file to update the abstract, add links, etc. --->\n

# ${title}
${speakers}

${info}

[[!inline pages=\"internal(${conf-year}/info/${slug}-schedule)\" raw=\"yes\"]]

[[!inline pages=\"internal(${conf-year}/info/${slug}-nav)\" raw=\"yes\"]]
"))))))))
        conf-info))

(defun conf-wiki-talk-resources (o)
  (setq o (append (list :base-url
                        (concat conf-media-base-url (plist-get o :conf-year) "/")
                        :track-base-url
                        (format "/%s/captions/" (plist-get o :conf-year)))
                  o))
  (concat
   (if (plist-get o :qa-public) "# Talk\n\n" "")
   (conf-index-card o conf-main-extensions)
   (if (plist-get o :qa-public)
       (concat "\n\n# Q&A\n\n"
               (conf-index-card (append
                                 (list
                                  :public 1
                                  :video-id "qanda"
                                  :toobnix-url nil
                                  :video-file (expand-file-name
                                               (concat (file-name-sans-extension (plist-get o :video-slug)) "--answers.webm")
                                               conf-captions-directory))
                                 o)
                                (list "--answers.vtt" "--answers--chapters.vtt")))
     "")))



(defun conf-format-talk-schedule-info (o)
  (let ((friendly (concat "/" conf-year "/talks/" (plist-get o :slug) ))
        (timestamp (org-timestamp-from-string (plist-get o :scheduled))))
    (concat
     "[[!toc  ]]\n"

     (if (plist-get o :q-and-a) (format "Q&A: %s  \n" (plist-get o :q-and-a)) "")
     (if (member conf-publishing-phase '(program schedule)) (concat "Status: " (plist-get o :status-label) "  \n") "")
     "Duration: " (or (plist-get o :video-duration)
                      (concat (plist-get o :duration) " minutes"))
     "  \n"
     (if (and (member conf-publishing-phase '(program schedule))
              (not (member (plist-get o :status) '("DONE" "CANCELLED" "STARTED"))))
         (let ((start (org-timestamp-to-time (org-timestamp-split-range timestamp)))
               (end (org-timestamp-to-time (org-timestamp-split-range timestamp t))))
           (format
            "<div class=\"times\" start=\"%s\" end=\"%s\">%s<br /><a href=\"/2021/\">Find out how to watch and participate</a></div>"
            (format-time-string "%Y-%m-%dT%H:%M:%SZ" start t)
            (format-time-string "%Y-%m-%dT%H:%M:%SZ" end t)
            (string-join (conf-timezone-strings o) "<br />")))
       "") 
     "\n"
     (if (plist-get o :alternate-apac)
         (format "[[!inline pages=\"internal(%s/inline-alternate)\" raw=\"yes\"]]  \n" conf-year)
       "")
     "\n"
     "If you have questions and the speaker has not indicated public contact information on this page, please feel free to e-mail us at <emacsconf-submit@gnu.org> and we'll forward your question to the speaker.\n\n"
     (if (plist-get o :public) (conf-wiki-talk-resources o) "")
     "\n# Description\n\n")))

(defun conf-generate-schedule-page (talk)
  (interactive (list (conf-get-talk-info-for-subtree)))
  (with-temp-file (expand-file-name (format "%s-schedule.md" (plist-get talk :slug))
                                    (expand-file-name "info" (expand-file-name conf-year conf-directory)))
    (insert
     "<!-- Automatically generated by conf-create-info-pages -->\n\n"
     (conf-format-talk-schedule-info talk) "\n")))

(defun conf-generate-info-pages ()
  (interactive)
  "Populate year/info/*-nav and *-schedule.md files."
  ;; TODO: No longer necessary?
  (require 's)
  (let* ((talks (seq-remove (lambda (o) (string= (plist-get o :status) "CANCELLED"))
                            (conf-filter-talks (conf-get-talk-info))))
         (next-talks (cdr talks))
         (prev-talks (cons nil talks)))
    (while talks
      (let* ((o (pop talks))
             (next-talk (conf-format-talk-link (pop next-talks)))
             (prev-talk (conf-format-talk-link (pop prev-talks)))
             (friendly (concat "/" conf-year "/talks/" (plist-get o :slug) ))
             (nav-links (format "Back to the [[schedule]]  \n%s%s"
                                (if prev-talk (format "Previous: %s  \n" prev-talk) "")
                                (if next-talk (format "Next: %s  \n" next-talk) ""))))
        (with-temp-file (expand-file-name (format "%s-nav.md" (plist-get o :slug))
                                          (expand-file-name "info" (expand-file-name conf-year conf-directory)))
          (insert nav-links))
        (conf-generate-schedule-page o)))))

(defun conf-generate-talks-page (conf-info)
    (interactive "p")
    (let ((info conf-info))
      (with-temp-buffer
        (find-file "talk-details.md")
        (erase-buffer)
        (insert (format "<table><thead><th>Duration</th><th>Title</th><th>Speakers</th></thead><tbody>%s</tbody></table>"
                        (mapconcat
                         (lambda (o)
                           (let* ((title (plist-get o :title))
                                  (speakers (plist-get o :speakers)))
                             (if (null (plist-get o :talk-id))
                                 (format "<tr><td colspan=\"3\">%s</td></tr>" (conf-format-talk-link o))
                               (format "<tr><td>%s</td><td>%s</td><td>%s</td><tr>" 
                                       (plist-get o :duration)
                                       (conf-format-talk-link o)
                                       (plist-get o :speakers)))))
                         info "\n")))
        (save-buffer))))

(defun conf-generate-main-schedule (&optional filename)
  (interactive)
  (with-temp-file (expand-file-name "schedule-details.md" (expand-file-name conf-year conf-directory))
    (insert (conf-format-talk-info-as-schedule (conf-get-talk-info)))))

(defun conf-format-talk-link (talk)
  (and talk (if (plist-get talk :talk-id)
                (format "<a href=\"/%s/talks/%s\">%s</a>"
                        conf-year
                        (plist-get talk :slug)
                        (plist-get talk :title))
              (plist-get talk :title))))


(defun conf-format-talk-info-as-schedule (info)
  (let* ((talks (seq-filter
                 (lambda (o)
                   (and (not (string= (plist-get o :status) "CANCELLED"))
                        (plist-get o :speakers)))
                 (conf-filter-talks info)))
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
            (pcase conf-publishing-phase
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
                 (pcase conf-publishing-phase
                   ('program
                    (if (eq (plist-get o :type) 'headline)
                        (format "<tr><td colspan=\"3\"><strong>%s<strong></td></tr>"
                                (if (plist-get o :slug)
                                    (conf-format-talk-link o)
                                  title))
                      (format "<tr><td>%s</td><td>%s</td><td>%s</td></tr>"
                              status
                              (conf-format-talk-link o) speakers)))
                   ('schedule
                    (if (eq (plist-get o :type) 'headline)
                        (format "<tr><td colspan=\"4\"><strong>%s<strong></td></tr>"
                                (if (plist-get o :slug)
                                    (conf-format-talk-link o)
                                  title))
                      (format "<tr><td>%s</td><td width=100>~%s</td><td>%s</td><td>%s</td></tr>"
                              status
                              start (conf-format-talk-link o) speakers)))
                   ('resources
                    (if (eq (plist-get o :type) 'headline)
                        (format "<tr><td colspan=\"3\"><strong>%s<strong></td></tr>"
                                (if (plist-get o :slug)
                                    (conf-format-talk-link o)
                                  title))
                      (format "<tr><td>%s</td><td>%s</td><td><ul>%s</ul></td></tr>"
                              (conf-format-talk-link o) speakers
                              (mapconcat (lambda (s) (concat "<li>" s "</li>"))
                                         (conf-link-file-formats-as-list
                                          (append o
                                                  (list :base-url (format "%s%s/" conf-media-base-url conf-year)))
                                          (append conf-published-extensions '("--main.webm")))
                                         "")))))))
             (seq-remove (lambda (o) (string= (plist-get o :status) "CANCELLED"))
                         (cdr info))
             "\n")
            (if (> (length cancelled) 0)
                (format "<div class=\"cancelled\">Cancelled:<ul>%s</ul></div>"
                        (mapconcat (lambda (talk) (format "<li><a href=\"/%s/talks/%s\">%s</a> - %s</li>"
                                                          conf-year
                                                          (plist-get talk :slug)
                                                          (plist-get talk :title)
                                                          (plist-get talk :speakers)))
                                   cancelled "\n"))
              ""))))

(defun conf-timezone-strings (o)
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
     conf-timezones)))

(defun conf-make-protected-index (filename)
  (interactive (list (expand-file-name "index.html" conf-protected-media-directory))) 
  (setq conf-info (conf-get-talk-info))
  (with-temp-file filename
    (let* ((talks (seq-filter (lambda (o) (plist-get o :video-file)) (conf-filter-talks conf-info)))
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
                  (conf-index-card
                   (append
                    f
                    (list :extra
                          (if (plist-get f :caption-note) (concat "<div class=\"caption-note\">" (plist-get f :caption-note) "</div>") "")))
                   (append conf-main-extensions conf-protected-extensions))))
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
                                      (conf-index-card f conf-main-extensions)))
                  captioned "\n")
       "</ol>"
       (if (file-exists-p (expand-file-name "include-in-index.html" conf-captions-directory))
           (with-temp-buffer (insert-file-contents (expand-file-name "include-in-index.html" conf-captions-directory)) (buffer-string))
         "")
       "</body></html>"))))

(defun conf-make-public-index (filename)
  (interactive (list (expand-file-name "index.html" conf-public-media-directory))) 
  (setq conf-info (conf-get-talk-info))
  (with-temp-file filename
    (insert
     "<html><body>"
     "<h1>" conf-name " " conf-year "</h1>"
     "<div class=\"m3u\"><a href=\"index.m3u\">M3U playlist for playing in MPV and other players</a></div>"
     "<ol class=\"videos\">"
     (mapconcat (lambda (f) (format "<li>%s</li>" (conf-index-card f '(".org" ".pdf" "--main.vtt"))))
                (conf-public-talks conf-info)
                "\n")
     "</ol>"
     (if (file-exists-p (expand-file-name "include-in-index.html" conf-captions-directory))
         (with-temp-buffer (insert-file-contents (expand-file-name "include-in-index.html" conf-captions-directory)) (buffer-string))
       "")
     "</body></html>")))

(defun conf-make-public-index-on-wiki ()
  (interactive)
  (let ((info (seq-filter (lambda (o)
                            (not (string= (plist-get o :status) "CANCELLED")))
                          (conf-filter-talks (conf-get-talk-info)))))
    (with-temp-file (expand-file-name "all-include.md" (expand-file-name conf-year conf-directory))
      (insert
       "<ol class=\"videos\">"
       (mapconcat
        (lambda (f)
          (format "<li><div class=\"title\"><a href=\"%s\">%s</a></div><div class=\"speakers\">%s</div>%s%s</li>"
                  (plist-get f :url)
                  (plist-get f :title)
                  (or (plist-get f :speakers) "")
                  (if (plist-get f :public)
                      (conf-index-card
                       (append (list :base-url
                                     (concat conf-media-base-url (plist-get f :conf-year) "/")
                                     :track-base-url
                                     (format "/%s/captions/" (plist-get f :conf-year)))
                               f)
                       conf-published-extensions)
                    "")
                  (if (plist-get f :qa-public)
                      (conf-index-card
                       (append
                        (list
                         :public 1
                         :base-url (concat conf-media-base-url (plist-get f :conf-year) "/")
                         :video-id "qanda"
                         :track-base-url
                         (format "/%s/captions/" (plist-get f :conf-year))
                         :video-file (expand-file-name
                                      (concat (file-name-sans-extension (plist-get f :video-slug)) "--answers.webm")
                                      conf-captions-directory))
                        f)
                       (list "--answers.vtt" "--answers--chapters.vtt"))
                    "")))
        info "\n"))
       "</ol>")))



(defun conf-make-chapter-strings (filename track-base-url)
  (when (file-exists-p filename)
    (let ((chapters (with-temp-buffer
                      (insert-file-contents filename)
                      (subed-vtt--init)
                      (conf-chapters-buffer-as-list))))
      (list
       :track (format "<track kind=\"chapters\" label=\"Chapters\" src=\"%s\"\" />"
                      (concat (or track-base-url "") (file-name-nondirectory filename)))
       :html (format "<ol class=\"chapters\">\n%s\n</ol>"
                     (mapconcat
                      (lambda (chapter)
                        (format "<li data-start=\"%.3f\" data-stop=\"%.3f\">%s %s</li>"
                                (/ (plist-get chapter :start-ms) 1000.0)
                                (/ (plist-get chapter :stop-ms) 1000.0)
                                (format-seconds "%.2h:%z%.2m:%.2s" (floor (/ (plist-get chapter :start-ms) 1000)))
                                (plist-get chapter :text)))
                      chapters
                      "\n"))))))

(defun conf-video-subtitle-tracks (filename track-base-url)
  (concat
   (if (file-exists-p filename)
       (format "<track label=\"English\" kind=\"captions\" srclang=\"en\" src=\"%s\" default />"
               (concat (or track-base-url "") (file-name-nondirectory filename)))
     "")
   (mapconcat
    (lambda (lang)
      (let ((lang-file (concat (file-name-sans-extension filename) "_" (car lang) (file-name-extension filename))))
        (if (file-exists-p lang-file)
            (format "<track label=\"%s\" kind=\"captions\" srclang=\"%s\" src=\"%s--main_%s.vtt\" />"
                    (cdr lang)
                    (car lang)
                    (concat (or track-base-url "") (file-name-nondirectory lang-file))
                    (car lang))
          "")))
    '(("fr" . "French") ("ja" . "Japanese"))
    "")))

(defun conf-link-file-formats (video-slug extensions)
  (string-join (conf-link-file-formats-as-list video-slug extensions) " "))

(defun conf-link-file-formats-as-list (talk extensions)
  (let ((video-slug (plist-get talk :video-slug))
        (wiki-captions-dir (expand-file-name "captions" (expand-file-name (plist-get talk :conf-year) conf-directory))))
    (delq nil (seq-map (lambda (ext)
                         (if (file-exists-p
                              (expand-file-name
                               (concat video-slug ext)
                               (if (string-match "\\.vtt$" ext)
                                   wiki-captions-dir
                                 conf-captions-directory)))
                             (format "<a href=\"%s%s\">Download %s</a>"
                                     (or (plist-get talk :base-url) "")
                                     (concat video-slug ext)
                                     ext)))
                       extensions))))
