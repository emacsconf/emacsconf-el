;;; emacsconf-toobnix.el --- Use the Toobnix REST API  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Sacha Chua

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

;; https://docs.joinpeertube.org/api/rest-getting-started
;; https://docs.joinpeertube.org/api-rest-reference.html
;;
;; Get the access token with M-x `emacsconf-toobnix-api-setup'.

;;; Code:

(defvar emacsconf-toobnix-upload-command "peertube-cli")
(defvar emacsconf-toobnix-channel "EmacsConf")

(defun emacsconf-toobnix-update-video-description (talk &optional type)
  "Update the description for TALK.
TYPE is 'talk or 'answers."
  (interactive
   (let ((talk (emacsconf-complete-talk-info)))
     (list
      talk
      (if (plist-get talk :qa-toobnix-url)
          (intern (completing-read "Type: " '("talk" "answers")))
        'talk))))
  (setq type (or type 'talk))
  (let* ((properties
          (pcase type
            ('answers (emacsconf-publish-answers-video-properties talk 'toobnix))
            (_ (emacsconf-publish-talk-video-properties talk 'toobnix))))
         (id
          (emacsconf-toobnix-id-from-url
           (plist-get talk (pcase type
                             ('answers :qa-toobnix-url)
                             (_ :toobnix-url)))))
         (boundary (format "%s%d" (make-string 20 ?-)
                           (time-to-seconds)))
         (url-request-method "PUT")
         (url-request-extra-headers
          (cons (cons "Content-Type"
                      (concat "multipart/form-data; boundary=" boundary))
                (emacsconf-toobnix-api-header)))
         (url-request-data
          (mm-url-encode-multipart-form-data
                     `(("description" .
                        ,(replace-regexp-in-string
                          "\n" "\r\n"
                          (plist-get properties :description))))
                     boundary))
         (url (concat "https://toobnix.org/api/v1/videos/" id)))
    (with-current-buffer (url-retrieve-synchronously url)
      (prog1 (buffer-string)
        (kill-buffer (current-buffer))))))

(defun emacsconf-toobnix-upload-all ()
  (interactive)
  (dolist (talk (emacsconf-publish-prepare-for-display (emacsconf-get-talk-info)))
    (unless (or (plist-get talk :toobnix-url)
                (null (plist-get talk :video-file)))
      (message "Uploading %s" (plist-get talk :slug))
      (emacsconf-publish-upload-talk talk 'toobnix))
    (unless (or (plist-get talk :qa-toobnix)
                (null (plist-get talk :qa-video-file)))
      (message "Uploading %s answers" (plist-get talk :slug))
      (emacsconf-publish-upload-answers talk 'toobnix))))

(defun emacsconf-toobnix-upload (properties)
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
									"-C" emacsconf-toobnix-channel
									"-l" "2"
                  "--verbose" "debug"
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
								 (append (list emacsconf-toobnix-upload-command) arguments)
								 " "))
			(apply #'call-process
						 emacsconf-toobnix-upload-command
						 nil t t arguments)
			(buffer-string))))

(defun emacsconf-toobnix-step-through-publishing-talk (talk)
  (interactive (list (emacsconf-complete-talk-info
                      (seq-remove
                       (lambda (talk)
                         (or (not (plist-get talk :video-file))
                             (plist-get talk :toobnix-url)))
                       (emacsconf-get-talk-info)))))
  (kill-new (plist-get talk :video-file))
  (y-or-n-p
   (format "Video: %s - create video and upload this filename. Done?"
           (plist-get talk :video-file)))
  (kill-new (emacsconf-publish-video-description talk t))
  (y-or-n-p "Copied description. Paste into description, move first line to title, add to playlist. Done?")
  (when (emacsconf-talk-file talk "--main.vtt")
    (kill-new (emacsconf-talk-file talk "--main.vtt"))
    (y-or-n-p (format "Captions: %s. Add to video elements. Done?"
                      (emacsconf-talk-file talk "--main.vtt"))))
  (emacsconf-set-property-from-slug
   (plist-get talk :slug)
   "TOOBNIX_URL"
   (read-string (format "%s - Toobnix URL: " (plist-get talk :scheduled)))))

(defun emacsconf-toobnix-step-through-publishing-all ()
	(interactive)
	(catch 'done
		(while t
			(let ((talk (seq-find (lambda (o)
															(and (not (plist-get o :toobnix-url))
																	 (emacsconf-talk-file o "--main.webm")))
														(emacsconf-publish-prepare-for-display (emacsconf-get-talk-info)))))
				(unless talk
					(message "All done so far.")
					(throw 'done t))
        (emacsconf-toobnix-step-through-publishing-talk talk)))))

(defun emacsconf-toobnix-id-from-url (url)
  (when (string-match "https://toobnix.org/\\(?:w\\|videos/watch\\)/\\(.+\\)" url)
    (match-string 1 url)))

(defun emacsconf-toobnix-add-captions (talk type)
  (interactive (list (emacsconf-complete-talk-info)
                     (intern (completing-read "Type of talk: " '("talk" "answers")))))
  (let* ((url (format "https://toobnix.org/api/v1/videos/%s/captions/en"
						          (emacsconf-toobnix-id-from-url (plist-get talk (pcase type
                                                              ('talk :toobnix-url)
                                                              ('answers :qa-toobnix-url))))))
         (filename (expand-file-name
                    (concat (plist-get talk :file-prefix)
                            (pcase type
                              ('talk "--main.vtt")
                              ('answers "--answers.vtt")))
                    emacsconf-cache-dir))
         (file-arg (concat "captionfile=@" filename))
         (auth-header (emacsconf-toobnix-api-header)))
    (with-temp-buffer
      (call-process "curl" nil t nil
                    "-s"
                    "-i"
                    "--request" "PUT"
                    "--header" (concat (caar auth-header) ": " (cdar auth-header))
                    "--form" file-arg
                    url)
      (buffer-string))))

(defun emacsconf-toobnix-upload-all-captions ()
  (interactive)
  (dolist (talk (emacsconf-publish-prepare-for-display (emacsconf-get-talk-info)))
    (when (plist-get talk :toobnix-url)
      (message "Uploading captions for %s" (plist-get talk :slug))
      (emacsconf-toobnix-add-captions talk 'talk))
    (when (plist-get talk :qa-toobnix-url)
      (message "Uploading captions for %s Q&A" (plist-get talk :slug))
      (emacsconf-toobnix-add-captions talk 'answers))))

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


(defvar emacsconf-toobnix-api-client nil)
(defvar emacsconf-toobnix-api-bearer-token nil)
(defvar emacsconf-toobnix-api-username "bandali")
(defvar emacsconf-toobnix-api-channel-handle "emacsconf")

(defun emacsconf-toobnix-api-header ()
	`(("Authorization" . ,(concat "Bearer "
																(if (stringp emacsconf-toobnix-api-bearer-token)
																		emacsconf-toobnix-api-bearer-token
																	(assoc-default 'access_token emacsconf-toobnix-api-bearer-token))))))

(defun emacsconf-toobnix-api-setup ()
	(interactive)
	(require 'plz)
	(require 'url-http-oauth)
	(setq emacsconf-toobnix-api-client
				(plz 'get "https://toobnix.org/api/v1/oauth-clients/local" :as #'json-read))
	(setq emacsconf-toobnix-api-bearer-token
				(assoc-default
				 'access_token
				 (plz 'post "https://toobnix.org/api/v1/users/token"
					 :body (mm-url-encode-www-form-urlencoded
									`(("client_id" . ,(assoc-default 'client_id emacsconf-toobnix-api-client))
										("client_secret" . ,(assoc-default 'client_secret emacsconf-toobnix-api-client))
										("grant_type" . "password")
										("username" . ,emacsconf-toobnix-api-username)
										("password" . ,(auth-info-password (car (auth-source-search :host "https://toobnix.org"))))))
					 :as #'json-read)
				 )
)
	(setq emacsconf-toobnix-api-channels
				(plz 'get (format "https://toobnix.org/api/v1/accounts/%s/video-channels"
													emacsconf-toobnix-api-username)
					:headers (emacsconf-toobnix-api-header)
					:as #'json-read))
	(setq emacsconf-toobnix-api-videos
				(plz 'get
					(format "https://toobnix.org/api/v1/accounts/%s/videos?count=100&sort=-createdAt"
									emacsconf-toobnix-api-username)
					:headers (emacsconf-toobnix-api-header)
					:as #'json-read))
	(setq emacsconf-toobnix-api-playlists
				(append
				 (assoc-default 'data
												(plz 'get
													(format "https://toobnix.org/api/v1/video-channels/%s/video-playlists?sort=-createdAt"
																	emacsconf-toobnix-api-channel-handle)
													:headers (emacsconf-toobnix-api-header)
													:as #'json-read))
				 nil)))

(defun emacsconf-toobnix-latest-video-url (&optional props)
  (if props
      (alist-get 'url
                 (seq-find (lambda (o)
                             (string= (alist-get 'name o)
                                      (plist-get props :title)))
                  (alist-get 'data
                             (plz 'get
		                           (format "https://toobnix.org/api/v1/accounts/%s/videos?count=100&sort=-createdAt"
						                           emacsconf-toobnix-api-username)
		                           :headers (emacsconf-toobnix-api-header)
		                           :as #'json-read)
                             )))
    (alist-get 'url
               (car (alist-get 'data
                               (plz 'get
                                 (format "https://toobnix.org/api/v1/accounts/%s/videos?count=1&sort=-createdAt"
                                         emacsconf-toobnix-api-username)
                                 :headers (emacsconf-toobnix-api-header)
                                 :as #'json-read))))))



(defun emacsconf-toobnix-video-captions (url)
  (when (string-match "https://toobnix.org/\\(?:w\\|videos/watch\\)/\\(.+\\)" url)
    (let ((id (match-string 1 url)))
      (alist-get 'data (plz 'get
		                     (format "https://toobnix.org/api/v1/videos/%s/captions"
						                     id)
		                     :headers (emacsconf-toobnix-api-header)
		                     :as #'json-read)))))

(defun emacsconf-toobnix-publish-video-from-edit-page ()
	"Messy hack to set a video to public and store the URL."
	(interactive)
	(spookfox-js-injection-eval-in-active-tab "document.querySelector('label[for=privacy]').scrollIntoView(); document.querySelector('label[for=privacy]').closest('.form-group').querySelector('input').dispatchEvent(new Event('input'));" t)
	(sit-for 1)
	(spookfox-js-injection-eval-in-active-tab "document.querySelector('span[title=\"Anyone can see this video\"]').click()" t)
	(sit-for 1)
	(spookfox-js-injection-eval-in-active-tab "document.querySelector('button.orange-button').click()" t)(sit-for 3)
	(emacsconf-extract-store-url)
	(shell-command "xdotool key Alt+Tab sleep 1 key Ctrl+w Alt+Tab"))

(defun emacsconf-toobnix-set-up-playlist ()
	(interactive)
	(mapcar
	 (lambda (o)
		 (when (plist-get o :toobnix-url)
			 (browse-url (plist-get o :toobnix-url))
			 (read-key "press a key when page is loaded")
			 (spookfox-js-injection-eval-in-active-tab "document.querySelector('.action-button-save').click()" t)
			 (spookfox-js-injection-eval-in-active-tab "document.querySelector('my-peertube-checkbox').click()" t)
			 (read-key "press a key when saved to playlist"))
		 (when (plist-get o :qa-toobnix-url)
			 (browse-url (plist-get o :qa-toobnix-url))
			 (read-key "press a key when page is loaded")
			 (spookfox-js-injection-eval-in-active-tab "document.querySelector('.action-button-save').click()" t)
			 (spookfox-js-injection-eval-in-active-tab "document.querySelector('my-peertube-checkbox').click()" t)
			 (read-key "press a key when saved to playlist")))
	 (emacsconf-publish-prepare-for-display (emacsconf-get-talk-info))))

(defun emacsconf-toobnix-view (talk)
	(interactive (list (emacsconf-complete-talk-info)))
	(browse-url (plist-get (emacsconf-resolve-talk talk) :toobnix-url)))

(defun emacsconf-toobnix-view-qa (talk)
	(interactive (list (emacsconf-complete-talk-info)))
	(browse-url (plist-get (emacsconf-resolve-talk talk) :qa-toobnix-url)))


(provide 'emacsconf-toobnix)
;;; emacsconf-toobnix.el ends here
