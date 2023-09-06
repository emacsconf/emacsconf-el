;;; emacsconf-mail.el --- Mail merge functions       -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Sacha Chua

;; Author: Sacha Chua <sacha@sachachua.com>
;; Keywords: mail

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

(defun emacsconf-mail-groups (&optional info)
	(setq info (emacsconf-filter-talks (or info (emacsconf-get-talk-info))))
	(seq-group-by (lambda (o) (plist-get o :email)) info))

(defun emacsconf-mail-complete-email-group (&optional info)
  "Return (email . (talk talk))."
  (setq info (emacsconf-filter-talks (or info (emacsconf-get-talk-info))))
	(save-window-excursion
    (let* ((grouped (emacsconf-mail-groups info))
           (slug (emacsconf-get-slug-from-string (emacsconf-complete-talk info)))
           (email (plist-get (seq-find (lambda (o) (string= (plist-get o :slug) slug)) info) :email)))
      (assoc email grouped))))

(defun emacsconf-mail-prepare (template email attrs)
  (compose-mail
   email
   (emacsconf-replace-plist-in-string attrs (or (plist-get template :subject) ""))
   (delq nil
         (list
          (if (plist-get template :reply-to) (cons "Reply-To" (emacsconf-replace-plist-in-string attrs (plist-get template :reply-to))))
          (if (plist-get template :mail-followup-to)
              (cons "Mail-Followup-To" (emacsconf-replace-plist-in-string attrs (plist-get template :mail-followup-to))))
          (if (plist-get template :cc)
              (cons "Cc" (emacsconf-replace-plist-in-string attrs (plist-get template :cc)))))))
  (message-sort-headers)
  (message-goto-body)
  (save-excursion (insert (string-trim (emacsconf-replace-plist-in-string attrs (plist-get template :body))))
                  (goto-char (point-min))
                  (emacsconf-mail-merge-wrap)))

(defun emacsconf-mail-template-to-me ()
  "Might be useful for testing."
  (interactive)
  (let* ((template (if (org-entry-get (point) "EMAIL_ID")
                       (emacsconf-mail-merge-get-template-from-subtree)
                     (emacsconf-mail-merge-get-template
                      (completing-read "Template: " (org-property-values "EMAIL_ID")))))
         (mail-func (plist-get template :function)))
    (funcall mail-func user-mail-address template)))

(defun emacsconf-mail-template-to-volunteer (volunteer)
  "Prompt for a volunteer and e-mail current template to them."
  (interactive (list (with-current-buffer (find-file-noselect emacsconf-org-file))))
  (let ((template (if (org-entry-get (point) "EMAIL_ID")
                      (emacsconf-mail-merge-get-template-from-subtree)
                    (emacsconf-mail-merge-get-template
                     (completing-read "Template: " (org-property-values "EMAIL_ID")))))
        (volunteers (emacsconf-get-volunteer-info))
        (mail-func (plist-get template :function)))
    (funcall mail-func (emacsconf-complete-volunteer) template)))

(defun emacsconf-mail-template-to-first-group ()
  "Draft the current template for the first group on the list."
  (interactive)
  (let* ((template (if (org-entry-get (point) "EMAIL_ID")
                       (emacsconf-mail-merge-get-template-from-subtree)
                     (emacsconf-mail-merge-get-template
                      (completing-read "Template: " (org-property-values "EMAIL_ID")))))
         (mail-func (plist-get template :function))
				 (filtered-talks (emacsconf-mail-filter-talks-by-template template))
				 (group (car (emacsconf-mail-groups (emacsconf-mail-filter-talks-by-template template)))))
		(if filtered-talks
				(progn
					(funcall mail-func group template)
					(when (plist-get template :log-note)
						(mapc (lambda (talk)
										(emacsconf-mail-log-message-when-sent talk (plist-get template :log-note)))
									(cdr group))))
			(message "All done!"))))

(defun emacsconf-mail-template-to-group ()
  "Prompt for a speaker and e-mail current template to them."
  (interactive)
  (let* ((template (if (org-entry-get (point) "EMAIL_ID")
                       (emacsconf-mail-merge-get-template-from-subtree)
                     (emacsconf-mail-merge-get-template
                      (completing-read "Template: " (org-property-values "EMAIL_ID")))))
         (mail-func (plist-get template :function))
				 (filtered-talks (emacsconf-mail-filter-talks-by-template template))
				 (group (emacsconf-mail-complete-email-group
								 filtered-talks)))
		(if filtered-talks
				(progn
					(funcall mail-func group template)
					(when (plist-get template :log-note)
						(mapc (lambda (talk)
										(emacsconf-mail-log-message-when-sent talk (plist-get template :log-note)))
									(cdr group))))
			(message "All done!"))))

(defun emacsconf-mail-filter-talks-by-template (template)
	(let ((list (emacsconf-prepare-for-display (emacsconf-filter-talks (emacsconf-get-talk-info)))))
		(when list
			(setq list (emacsconf-filter-talks-by-slugs (plist-get template :slugs) list)))
		(when list
			(setq list (emacsconf-filter-talks-by-logbook (plist-get template :log-note) list)))
		(when list
			(setq list
						(seq-filter
						 (lambda (o) (plist-get o :email))
						 list)))
		list))

(defun emacsconf-mail-template-to-all-groups ()
  "Uses the current template to draft messages to all the speakers.
Group by e-mail."
  (interactive)
  (let* ((template (if (org-entry-get (point) "EMAIL_ID")
                       (emacsconf-mail-merge-get-template-from-subtree)
                     (emacsconf-mail-merge-get-template
                      (completing-read "Template: " (org-property-values "EMAIL_ID")))))
         (info (emacsconf-mail-filter-talks-by-template template))
         (grouped (emacsconf-mail-group-by-email info))
         (mail-func (plist-get template :function)))
    (mapc (lambda (group)
            (funcall mail-func group template)
						(when (plist-get template :log-note)
							(mapc (lambda (talk)
											(emacsconf-mail-log-message-when-sent talk (plist-get template :log-note)))
										(cdr group))))
          grouped)))

(defun emacsconf-mail-log-message-when-sent (o message)
  (add-hook 'message-sent-hook
            `(lambda ()
               (save-window-excursion
                 (emacsconf-add-to-talk-logbook ,(plist-get o :slug) ,message)))
            nil t))

(defun emacsconf-mail-group-by-email (info)
  (seq-group-by (lambda (o) (plist-get o :email)) info))

(defun emacsconf-mail-speaker (&optional subject body talk)
  "Compose a message to the speaker of the current talk."
  (interactive (list nil nil (emacsconf-complete-talk-info)))
  (compose-mail (plist-get talk :email) subject)
  (when body (message-goto-body) (insert body)))

(defun emacsconf-mail-speaker-schedule (&optional subject body)
  (interactive (list (read-string "Subject: ") nil))
  (let ((info (emacsconf-get-talk-info-for-subtree)))
    (emacsconf-mail-speaker subject body)
    (when body (message-goto-body) (insert body))
    (goto-char (point-max))
    (insert (string-join (emacsconf-timezone-strings info) "\n"))))

(defvar emacsconf-submit-email "emacsconf-submit@gnu.org" "E-mail address for submissions.")

(defun emacsconf-mail-speaker-cc-submit (&optional subject body)
  "Compose a message to the speaker of the current talk."
  (interactive)
  (compose-mail (format "%s <%s>" (org-entry-get (point) "NAME") (org-entry-get (point) "EMAIL"))
                subject '(("Reply-To" . emacsconf-submit-email) ("Cc" . emacsconf-submit-email)))
  (message-goto-body)
  (when body (insert body))
  (save-excursion (insert "Please keep " emacsconf-submit-email " in the To: or Cc: when replying. Thank you!")))

(defun emacsconf-mail-show-talk-info ()
  (interactive)
  (let ((email (or (mail-fetch-field "reply-to") (mail-fetch-field "from"))))
    (when (string-match "<\\(\\(\\sw\\|\\s_\\|\\s.\\)+@\\(\\sw\\|\\s_\\|\\s.\\)+\\)>" email)
      (setq email (match-string 1 email)))
    (pop-to-buffer (find-file-noselect emacsconf-org-file))
    (goto-char (point-min))
    (goto-char
     (or (org-find-property "EMAIL" email)
         (org-find-property "NAME"
                            (completing-read "Name: " (delq nil (org-map-entries (lambda () (org-entry-get "NAME"))))))))))





(defun emacsconf-mail-merge-wrap ()
  (interactive)
  (with-undo-amalgamate 
    (save-excursion
      (while (re-search-forward " *${wrap}" nil t)
        (replace-match "")
        (fill-paragraph)))))

(defun emacsconf-mail-merge-get-template-from-subtree ()
  (list :subject (org-entry-get-with-inheritance "SUBJECT")
        :cc (org-entry-get-with-inheritance "CC")
        :slugs (org-entry-get-with-inheritance "SLUGS")
        :reply-to (or (org-entry-get-with-inheritance "REPLY_TO") (org-entry-get-with-inheritance "REPLY-TO"))
        :mail-followup-to (or (org-entry-get-with-inheritance "MAIL_FOLLOWUP_TO")
                              (org-entry-get-with-inheritance "MAIL-FOLLOWUP-TO"))
        :body (replace-regexp-in-string "\n *," "\n"
                                        (buffer-substring-no-properties
                                         (progn
                                           (org-back-to-heading)
                                           (org-end-of-meta-data) (point))
                                         (org-end-of-subtree)))
        :function (when (org-entry-get-with-inheritance "FUNCTION")
                    (intern (org-entry-get-with-inheritance "FUNCTION")))
				:log-note (org-entry-get-with-inheritance "LOG_NOTE")))

(defun emacsconf-mail-merge-get-template (id)
  "Return the information for the e-mail template with EMAIL_ID set to ID."
  (save-excursion
    (let ((char (org-find-property "EMAIL_ID" id)))
      (if char
          (progn (goto-char char) (emacsconf-mail-merge-get-template-from-subtree))
        (with-current-buffer
            (find-file-noselect (expand-file-name "organizers-notebook/index.org" (expand-file-name emacsconf-year emacsconf-directory)))
          (setq char (org-find-property "EMAIL_ID" id))
          (if char
              (progn
                (goto-char char)
                (emacsconf-mail-merge-get-template-from-subtree))
            ;; Try the conf.org file
            (with-current-buffer (find-file-noselect emacsconf-org-file)
              (setq char (org-find-property "EMAIL_ID" id))
              (if char
                  (progn
                    (goto-char char)
                    (emacsconf-mail-merge-get-template-from-subtree))
                (error "Could not find template %s" id)))))))))

(defun emacsconf-mail-parse-submission (body)
	"Extract data from EmacsConf 2023 submissions in BODY."
	(let (data
				(fields '((:title "^Talk title")
									(:description "^Talk description")
									(:format "^Format")
									(:intro "^Introduction for you and your talk")
									(:name "^Speaker name")
									(:availability "^Speaker availability")
									(:q-and-a "^Preferred Q&A approach")
									(:public "^Public contact information")
									(:private "^Private emergency contact information")
									(:release "^Please include this speaker release"))))
		(with-temp-buffer
			(insert body)
			(goto-char (point-min))
			;; Try to parse it
			(while fields
				;; skip the field title
				(when (and (or (looking-at (cadar fields))
											 (re-search-forward (cadar fields) nil t))
									 (re-search-forward "\\(:[ \t\n]+\\|\n\n\\)" nil t))
					;; get the text between this and the next field
					(setq data (plist-put data (caar fields)
																(buffer-substring (point)
																									(or
																									 (when (and (cdr fields)
																															(re-search-forward (cadr (cadr fields)) nil t))
																										 (goto-char (match-beginning 0))
																										 (point))
																									 (point-max))))))
				(setq fields (cdr fields)))
			(if (string-match "[0-9]+" (or (plist-get data :format) ""))
					(plist-put data :time (match-string 0 (or (plist-get data :format) ""))))
			data)))

;;;###autoload
(defun emacsconf-mail-review ()
	(interactive)
	(let ((notification-date (format-time-string
														"%Y-%m-%d"
														(time-add
														 (days-to-time emacsconf-review-days)
														 (date-to-time (plist-get (plist-get (notmuch-show-get-message-properties) :headers) :Date))))))
		(notmuch-show-reply)
		(message-goto-body)
		(save-excursion
			(insert (format
							 "Thanks for submitting your proposal! (TODO: feedback) We're experimenting
with early acceptance this year, so we'll wait a week (~ %s) in case the
other volunteers want to chime in regarding your talk. =)

"
							 notification-date)))))

;;;###autoload
(defun emacsconf-mail-add-submission (slug)
	"Add the submission from the current e-mail."
	(interactive "MTalk ID: ")
	(let* ((props (notmuch-show-get-message-properties))
				 (from (plist-get (plist-get props :headers) :From))
				 (body (plist-get
								(car
								 (plist-get props :body))
								:content))
				 (date (format-time-string "%Y-%m-%d"
																	 (date-to-time (plist-get (plist-get props :headers) :Date))))
				 (to-notify (format-time-string
										 "%Y-%m-%d"
										 (time-add
											(days-to-time emacsconf-review-days)
											(date-to-time (plist-get (plist-get props :headers) :Date)))))
				 (data (emacsconf-mail-parse-submission body)))
		(when (string-match "<\\(.*\\)>" from)
			(setq from (match-string 1 from)))
		(with-current-buffer
				(find-file emacsconf-org-file)
			;;  go to the submissions entry
			(goto-char (org-find-property "CUSTOM_ID" "submissions"))
			(when (org-find-property "CUSTOM_ID" slug)
				(error "Duplicate talk ID")))
		(find-file emacsconf-org-file)
		(delete-other-windows)
		(outline-next-heading)
		(org-insert-heading)
		(insert " " (or (plist-get data :title) "") "\n")
		(org-todo "TO_REVIEW")
		(org-entry-put (point) "CUSTOM_ID" slug)
		(org-entry-put (point) "SLUG" slug)
		(org-entry-put (point) "TRACK" "General")
		(org-entry-put (point) "EMAIL" from)
		(org-entry-put (point) "DATE_SUBMITTED" date)
		(org-entry-put (point) "DATE_TO_NOTIFY" to-notify)
		(when (plist-get data :time)
			(org-entry-put (point) "TIME" (plist-get data :time)))
		(when (plist-get data :availability)
			(org-entry-put (point) "AVAILABILITY"
										 (replace-regexp-in-string "\n+" " "
																							 (plist-get data :availability))))
		(when (plist-get data :public)
			(org-entry-put (point) "PUBLIC_CONTACT"
										 (replace-regexp-in-string "\n+" " "
																							 (plist-get data :public))))
		(when (plist-get data :private)
			(org-entry-put (point) "EMERGENCY"
										 (replace-regexp-in-string "\n+" " "
																							 (plist-get data :private))))
		(when (plist-get data :q-and-a)
			(org-entry-put (point) "Q_AND_A"
										 (replace-regexp-in-string "\n+" " "
																							 (plist-get data :q-and-a))))
		(save-excursion
			(insert body))
		(re-search-backward org-drawer-regexp)
		(org-fold-hide-drawer-toggle 'off)
		(org-end-of-meta-data)
		(split-window-below)))

(defun emacsconf-mail-merge-fill (string)
  "Fill in the values for STRING using the properties at point.
Include some other things, too, such as emacsconf-year, title, name, email, url, and duration."
  (let (start (values `(("year" . ,emacsconf-year)
												("title" . ,(org-entry-get (point) "ITEM"))
												("name" . ,(org-entry-get (point) "NAME"))
												("email" . ,(org-entry-get (point) "EMAIL"))
												("url" . ,(format "%s%s/talks/%s" emacsconf-base-url emacsconf-year (org-entry-get (point) "SLUG")))
												("duration" . ,(org-entry-get (point) "TIME")))))
    (while (string-match "\\${\\([-a-zA-Z_]+?\\)}" string start)
      (if (assoc-default (match-string 1 string) values)
          (setq string (replace-match (assoc-default (match-string 1 string) values) t t string))
        (setq string (replace-match (save-match-data (org-entry-get (point) (match-string 1 string))) t t string)))
      (setq start (1+ (match-beginning 0))))
    string))

(defun emacsconf-mail-merge-format-email-address-for-subtree ()
  (if (string-match  "," (org-entry-get (point) "EMAIL"))
      (org-entry-get (point) "EMAIL")
    (format "%s <%s>" (org-entry-get (point) "NAME") (org-entry-get (point) "EMAIL"))))

(defun emacsconf-mail-merge-for-subtree (id note-field)
  (let* ((template (emacsconf-mail-merge-get-template id))
         (body (emacsconf-mail-merge-fill (plist-get template :body)))
         (subject (emacsconf-mail-merge-fill (plist-get template :subject)))
         (note (org-entry-get (point) note-field)))
    (compose-mail (emacsconf-mail-merge-format-email-address-for-subtree)
                  subject
                  `(("Reply-To" . ,(plist-get template :reply-to))
                    ("Mail-Followup-To" . ,(plist-get template :mail-followup-to))
                    ("Cc" . ,(plist-get template :cc))))
    (message-goto-body)
    (save-excursion 
      (when note (insert "#+NOTE: " note "\n======== Delete above before sending =============\n\n"))
      (insert body))))

(defun emacsconf-mail-merge-cancel ()
  (interactive)
  (mapc (lambda (buffer)
          (when (string-match "unsent" (buffer-name buffer))
            (let ((kill-buffer-query-functions nil)
                  (buffer-modified-p nil))
              (set-buffer-modified-p nil)
              (kill-buffer buffer))))
        (buffer-list)))

;;; Specific mail merges

(defun emacsconf-mail-captions-for-approval (talk)
  (interactive (list (emacsconf-complete-talk-info)))
  (let ((captions (expand-file-name (concat (plist-get talk :video-slug) "--main.vtt")
                                    emacsconf-cache-dir))
        (captioner-info
         (with-current-buffer (find-file-noselect emacsconf-org-file)
           (org-entry-properties (org-find-property "CUSTOM_ID" (plist-get talk :captioner))))))
    (emacsconf-mail-prepare
     (emacsconf-mail-merge-get-template "captions")
     (plist-get talk :email)
     (list
      :speakers-short (plist-get talk :speakers-short)
      :year emacsconf-year
      :email (plist-get talk :email)
      :title (plist-get talk :title)
      :captioner (assoc-default "NAME_SHORT" captioner-info)
      :url
      (format "https://%s:%s@media.emacsconf.org/%s/backstage/#%s"
              emacsconf-backstage-user
              emacsconf-backstage-password
              emacsconf-year
              (plist-get talk :slug))
      :password emacsconf-backstage-password
      :captioner-email (assoc-default "EMAIL" captioner-info)
      :captioner-volunteered
      (if (string= (plist-get talk :captioner) "sachac")
          ""
        (format "%s volunteered to edit the captions for your video. " (assoc-default "NAME_SHORT" captioner-info)))
      :chapters-note
      (if (file-exists-p
           (expand-file-name (concat (plist-get talk :video-slug) "--main--chapters.vtt")
                             emacsconf-cache-dir))
          "I've come up with some potential chapter headings which you can see as NOTE in the transcript or in the backstage entry for your video. Let me know if you want to tweak those.\n\n"
        "")
      :intro-note
      (emacsconf-surround
       "${wrap}Also, I drafted a quick intro for the host to read. Let me know if you want to tweak this: " (plist-get talk :intro-note) "\n\n"
       "")
      :captioner-thanks
      (if (string= (plist-get talk :captioner) "sachac")
          ""
        (format "%s: Thank you for editing the captions!\n\n" (assoc-default "NAME_SHORT" captioner-info)))
      :captions (mapconcat (lambda (sub) (concat (emacsconf-surround "\n" (elt sub 4) "" "") (elt sub 3))) (subed-parse-file captions) "\n")))
    (mml-attach-file captions "text/vtt" "Subtitles" "attachment")))

;;; Notmuch

(defun emacsconf-mail-notmuch-search-for-talk (talk)
  "Search for e-mail related to TALK."
  (interactive (list (emacsconf-complete-talk-info))) 
  (notmuch-search
   (concat
    (mapconcat
     (lambda (o)
       (format "from:%s or to:%s" o o))
     (split-string (plist-get talk :email) " *, *")
     " or ")
    " or (" emacsconf-id " and " (plist-get talk :slug) ")")))

;;; Volunteers

(defun emacsconf-mail-volunteers (volunteers)
  (interactive
   (list
    (completing-read-multiple
     "Volunteers: " (emacsconf-volunteer-emails-for-completion))))
  (compose-mail (string-join volunteers ", ")))

(defun emacsconf-mail-notmuch-search-for-volunteer (volunteer)
  (interactive
   (list
    (completing-read
     "Volunteer: " (emacsconf-volunteer-emails-for-completion))))
  (let ((email (if (string-match "<\\(.*?\\)>" volunteer) (match-string 1) volunteer)))
    (notmuch-search (format "from:%s or to:%s" email email))))

(provide 'emacsconf-mail)
;;; emacsconf-mail.el ends here
