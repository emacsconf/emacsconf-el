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
	"Group all the talks by e-mail address."
	(setq info (seq-filter (lambda (o) (plist-get o :email))
												 (emacsconf-filter-talks (or info (emacsconf-get-talk-info)))))
	(seq-group-by (lambda (o) (plist-get o :email)) info))

(defun emacsconf-mail-complete-email-group (&optional info)
  "Return (email . (talk talk))."
  (setq info (emacsconf-filter-talks (or info (emacsconf-get-talk-info))))
	(save-window-excursion
    (let* ((grouped (emacsconf-mail-groups info))
           (talk (emacsconf-resolve-talk (emacsconf-complete-talk-info info)))
           (email (plist-get talk :email)))
      (assoc email grouped))))

(defvar emacsconf-mail-prepare-behavior nil "*Modify behavior for preparing messages.
String: insert into buffer with that name
t: insert into current buffer
'new-message: always create new message.")

(defun emacsconf-mail-update-reply-headers (template _ attrs fields)
	"Update the current reply with TEMPLATE, ATTRS, and FIELDS."
	(when (plist-get template :subject)
		(message-replace-header
		 "Subject"
		 (format "%s (was %s)"
						 (emacsconf-replace-plist-in-string attrs (or (plist-get template :subject) ""))
						 (message-field-value "Subject"))))
	(mapc (lambda (field)
					(when (plist-get template (car field))
						(message-replace-header
						 (cadr field)
						 (concat (emacsconf-replace-plist-in-string attrs (plist-get template (car field)))
										 (if (message-field-value (cadr field))
												 (format
													(if (string= (cadr field) "Subject")
															" (was %s)"
														", %s")
													(message-field-value (cadr field)))
											 "")))))
				fields))

(defun emacsconf-mail-prepare-for-batch-test (template email attrs fields)
	"Put the e-mail in the `emacsconf-mail-batch-test' buffer .
Compose it using TEMPLATE, EMAIL, and ATTRS."
	(with-current-buffer (cond ((eq emacsconf-mail-prepare-behavior t) (current-buffer))
														 ((stringp emacsconf-mail-prepare-behavior) (get-buffer-create emacsconf-mail-prepare-behavior)))
		(save-excursion
			(goto-char (point-max))
			(insert
			 "* " email "\n"
			 (string-join
				(seq-keep (lambda (field)
										(and (plist-get template (car field))
												 (concat (cadr field) ": "
																 (emacsconf-replace-plist-in-string attrs (plist-get template (car field)))
																 "\n")))
									(append '((:subject "Subject")) fields))
				"")
			 (string-trim (emacsconf-replace-plist-in-string attrs (plist-get template :body)))
			 "\n\n"
			 (make-string 50 ?-)
			 "\n"))
		(emacsconf-mail-merge-wrap (point) (point-max))
		(goto-char (point-max))
		(display-buffer (current-buffer))))

(defun emacsconf-mail-uniquify-headers ()
	"Make sure the mail headers contain unique values."
	(mapc (lambda (header)
					(when (message-field-value header)
						(message-replace-header
						 header
						 (string-join (seq-uniq (seq-map #'mail-strip-quoted-names
																							 (message-tokenize-header (message-field-value header)))
																		'string=) ", "))))
				'("To" "Cc" "Bcc" "Mail-Followup-To" "Mail-Reply-To" "Reply-To"))
	;; Set Cc to only e-mail addresses not in To:
	(when (and (message-field-value "To") (message-field-value "Cc"))
		(message-replace-header
		 "Cc"
		 (string-join (seq-difference (message-tokenize-header (message-field-value "Cc"))
																	(message-tokenize-header (message-field-value "To"))
																	'string=)
									", "))))

(defun emacsconf-mail-prepare (template email attrs)
	"Prepare the e-mail following TEMPLATE. Send it to EMAIL.
Use ATTRS to fill in the template.
Behavior is modified by `emacsconf-mail-prepare-behavior'."
	(let ((fields '((:reply-to "Reply-To")
									(:mail-followup-to "Mail-Followup-To")
									(:cc "Cc")
									(:bcc "Bcc"))))
		(unless (plist-get template :bcc) (setq template (append template (list :bcc emacsconf-mail-bcc-email))))
		(if (or (eq emacsconf-mail-prepare-behavior t) (stringp emacsconf-mail-prepare-behavior))
				(emacsconf-mail-prepare-for-batch-test template email attrs fields)
			;; prepare to send the mail
			(if (and (derived-mode-p 'message-mode)
							 (string-match "unsent mail" (buffer-name))
							 (not (eq emacsconf-mail-prepare-behavior 'new-message))
							 ;; is the current message a reply to this e-mail?
							 (string-match (regexp-quote email) (message-field-value "To")))
					;; add to headers
					(emacsconf-mail-update-reply-headers template email attrs fields) 
				;; compose a new message
				(compose-mail
				 email
				 (emacsconf-replace-plist-in-string attrs (or (plist-get template :subject) ""))
				 (seq-keep (lambda (field)
										 (when (plist-get template (car field))
											 (cons
												(cadr field)
												(emacsconf-replace-plist-in-string
												 attrs
												 (plist-get template (car field))))))
									 fields)))
			(message-sort-headers)
			(emacsconf-mail-uniquify-headers)
			(message-goto-body)
			(save-excursion
				(insert (string-trim (emacsconf-replace-plist-in-string attrs (plist-get template :body)))
								"\n\n")
				(goto-char (point-min))
				(emacsconf-mail-merge-wrap))
			(when (plist-get template :log-note)
				(mapc (lambda (talk)
								(emacsconf-mail-log-message-when-sent talk (plist-get template :log-note)))
							(emacsconf-mail-talks email))))))

(defun emacsconf-mail-complete-template-function ()
	"Get a mail template function."
	(intern (completing-read "Function: "
													 (or (when (derived-mode-p 'org-mode)
																 (seq-map #'intern (org-property-values "FUNCTION")))
															 #'help--symbol-completion-table)
													 (lambda (f)
														 (and (commandp f)
																	(string-match "emacsconf-mail" (symbol-name f))))
													 nil nil nil
													 (and (derived-mode-p 'org-mode) (org-entry-get (point) "FUNCTION")))))

(defun emacsconf-mail-template-to-me (template-function)
  "Set up the current template for a talk, but e-mail it only to me."
  (interactive (list (emacsconf-mail-complete-template-function)))
	(call-interactively template-function)
	(message-replace-header "To" user-mail-address))

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

;; TODO: Figure out how to rewrite this for templates that specify slugs
;; (defun emacsconf-mail-template-to-first-group ()
;;   "Draft the current template for the first group on the list."
;;   (interactive)
;;   (let* ((mail-func (emacsconf-mail-complete-template-function))
;; 				 (filtered-talks (emacsconf-mail-filter-talks-by-template template))
;; 				 (group (car (emacsconf-mail-groups (emacsconf-mail-filter-talks-by-template template)))))
;; 		(if filtered-talks
;; 				(progn
;; 					(funcall mail-func group template)
;; 					(when (plist-get template :log-note)
;; 						(mapc (lambda (talk)
;; 										(emacsconf-mail-log-message-when-sent talk (plist-get template :log-note)))
;; 									(cdr group))))
;; 			(message "All done!"))))

;; (defun emacsconf-mail-template-to-group ()
;;   "Prompt for a speaker and e-mail current template to them."
;;   (interactive)
;;   (let* ((template (if (org-entry-get (point) "EMAIL_ID")
;;                        (emacsconf-mail-merge-get-template-from-subtree)
;;                      (emacsconf-mail-merge-get-template
;;                       (completing-read "Template: " (org-property-values "EMAIL_ID")))))
;;          (mail-func (plist-get template :function))
;; 				 (filtered-talks (emacsconf-mail-filter-talks-by-template template))
;; 				 (group (emacsconf-mail-complete-email-group
;; 								 filtered-talks)))
;; 		(if filtered-talks
;; 				(progn
;; 					(funcall mail-func group template)
;; 					(when (plist-get template :log-note)
;; 						(mapc (lambda (talk)
;; 										(emacsconf-mail-log-message-when-sent talk (plist-get template :log-note)))
;; 									(cdr group))))
;; 			(message "All done!"))))

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

(defun emacsconf-mail-template-to-all-groups (&optional arg)
  "Uses the current template to draft messages to all the speakers.
Group by e-mail.  With prefix argument (e.g. \\[universal-argument]),
insert into the current buffer instead of drafting e-mails."
  (interactive "P")
  (let* ((mail-func (emacsconf-mail-complete-template-function))
         (grouped (emacsconf-mail-group-by-email))
				 (emacsconf-mail-prepare-behavior (if arg t 'new-message)))
    (mapc (lambda (group)
            (funcall mail-func group))
          grouped)))

(defun emacsconf-mail-log-message-when-sent (o message)
  (add-hook 'message-sent-hook
            `(lambda ()
               (save-window-excursion
                 (emacsconf-add-to-talk-logbook ,(plist-get o :slug) ,message)))
            nil t))

(defun emacsconf-mail-group-by-email (&optional info)
  (seq-group-by (lambda (o) (plist-get o :email))
								(or info (seq-filter (lambda (o) (and (plist-get o :email)
																											(not (string= (plist-get o :status) "CANCELLED"))))
																		 (emacsconf-get-talk-info)))))

;;;###autoload
(defun emacsconf-mail-speaker-from-slug (talk)
	"E-mail the speaker for TALK."
	(interactive (list (emacsconf-complete-talk-info)))
  (compose-mail (plist-get talk :email)))

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





(defun emacsconf-mail-merge-wrap (&optional beg end)
  (interactive "r")
	(unless beg (setq beg (point-min)))
	(unless end (setq end (point-max)))
  (with-undo-amalgamate 
    (save-excursion
			(goto-char beg)
      (while (re-search-forward " *${\\(wrap\\|fill\\)}" end t)
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
	"Extract data from EmacsConf submissions in BODY."
	(when (listp body) (setq body (plist-get (car body) :content)))
	(let* ((data (list :body body))
				 (fields '((:title "^[* ]*Talk title")
									 (:description "^[* ]*Talk description")
									 (:format "^[* ]*Format")
									 (:intro "^[* ]*Introduction for you and your talk")
									 (:name "^[* ]*Speaker name")
									 (:availability "^[* ]*Speaker availability")
									 (:q-and-a "^[* ]*Preferred Q&A approach")
									 (:public "^[* ]*Public contact information")
									 (:private "^[* ]*Private emergency contact information")
									 (:release "^[* ]*Please include this speaker release")))
				 field
				 (field-regexp (mapconcat
												(lambda (o)
													(concat "\\(?:" (cadr o) "\\)"))
												fields "\\|")))
		(with-temp-buffer
			(insert body)
			(goto-char (point-min))
			;; Try to parse it
			(catch 'done
				(while (not (eobp))
					;; skip the field title
					(unless (looking-at field-regexp)
						(unless (re-search-forward field-regexp nil t)
							(throw 'done nil)))
					(goto-char (match-beginning 0))
					(setq field (seq-find (lambda (o)
																	(looking-at (cadr o)))
																fields))
					(when field
						;; get the text between this and the next field
						(re-search-forward "\\(:[ \t\n]+\\|\n\n\\)" nil t)
						(setq data
									(plist-put
									 data
									 (car field)
									 (buffer-substring
										(point)
										(or (and
												 (re-search-forward field-regexp nil t)
												 (goto-char (match-beginning 0))
												 (point))
												(point-max))))))))
			(if (string-match "[0-9]+" (or (plist-get data :format) ""))
					(plist-put data :time (match-string 0 (or (plist-get data :format) ""))))
			data)))

;; Documented in https://sachachua.com/blog/2023/09/emacsconf-capturing-submissions-from-e-mails/
;;;###autoload
(defun emacsconf-mail-add-submission (slug)
	"Add the submission from the current e-mail."
	(interactive "MTalk ID: ")
	(let* ((props (notmuch-show-get-message-properties))
				 (from (or (plist-get (plist-get props :headers) :Reply-To)
									 (plist-get (plist-get props :headers) :From)))
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
			(insert (plist-get data :body)))
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
                    ("Cc" . ,(plist-get template :cc))
										("Bcc" . ,(plist-get template :bcc))))
    (message-goto-body)
    (save-excursion 
      (when note (insert "#+NOTE: " note "\n======== Delete above before sending =============\n\n"))
      (insert body))))

(defun emacsconf-mail-merge-check-drafts ()
	"Put all the drafts in one buffer to check."
	(interactive)
	(let (result)
		(mapc (lambda (buffer)
						(when (string-match "unsent" (buffer-name buffer))
							(with-current-buffer buffer
								(add-to-list 'result (buffer-string)))))
					(buffer-list))
		(with-current-buffer (get-buffer-create "*Drafts*")
			(erase-buffer)
			(insert (string-join result "\n-------------------------------------------------------\n"))
			(goto-char (point-min))
			(switch-to-buffer (current-buffer)))))

(defun emacsconf-mail-merge-cancel ()
	"Cancel all the unsent messages."
  (interactive)
  (mapc (lambda (buffer)
          (when (string-match "unsent" (buffer-name buffer))
						(with-current-buffer buffer
							(let ((kill-buffer-query-functions nil)
										(undo-tree-auto-save-history nil))
								(set-buffer-modified-p nil)
								(kill-buffer buffer)))))
        (buffer-list)))

;;; Notmuch

;;;###autoload
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
    ;; " or (" emacsconf-id " and " (plist-get talk :slug) ")"
		)))

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

;;;###autoload
(defun emacsconf-mail-check-for-zzz-before-sending ()
	"Throw an error if the ZZZ todo marker is still in the message.
Good for adding to `message-send-hook'."
	(save-excursion
		(goto-char (point-min))
		(when (re-search-forward "ZZZ\\|\\${" nil t)
			(unless (yes-or-no-p "ZZZ marker found. Send anyway? ")
				(error "ZZZ marker found.")))))

(defun emacsconf-mail-insert-info (talk)
	"Insert mail info for TALK for easy reference.
This includes NAME_SHORT and EMAIL_NOTES."
	(interactive (list (emacsconf-complete-talk-info)))
	(setq talk (emacsconf-resolve-talk talk))
	(let ((info (concat (emacsconf-surround "Hi, " (plist-get talk :speakers-short) "!\n\n" "")
											(emacsconf-surround "--- ZZZ ---\n" (plist-get talk :email-notes) "\n------\n\n" ""))))
		(unless (string= info "")
			(save-excursion (insert info)))))

;;; Templates

;;;###autoload
(defun emacsconf-mail-review (talk)
	"Let the speaker know we've received their proposal."
	(interactive (list (emacsconf-complete-talk-info)))
	(emacsconf-mail-prepare '(:subject "${conf-name} ${year} review: ${title}"
											 :cc "emacsconf-submit@gnu.org"
											 :reply-to "emacsconf-submit@gnu.org, ${email}, ${user-email}"
											 :mail-followup-to "emacsconf-submit@gnu.org, ${email}, ${user-email}"
											 :body "
Hi, ${speakers-short}!

Thanks for submitting your proposal! (ZZZ TODO: feedback)

We'll wait a week (~ ${notification-date}) in case the other volunteers want to chime in regarding your talk. =)

${signature}
")
						(plist-get talk :email)
						(list
						 :user-email user-mail-address
						 :signature user-full-name
						 :title (plist-get talk :title)
						 :email (plist-get talk :email)
						 :conf-name emacsconf-name
						 :speakers-short (plist-get talk :speakers-short)
						 :year emacsconf-year
						 :notification-date (plist-get talk :date-to-notify))))

(defun emacsconf-mail-accept-talk (talk)
	"Send acceptance letter."
  (interactive (list (emacsconf-complete-talk-info)))
  (emacsconf-mail-prepare '(:subject "${conf-name} ${year} acceptance: ${title}"
											 :cc "emacsconf-submit@gnu.org"
											 :slugs nil
											 :reply-to "emacsconf-submit@gnu.org, ${email}, ${user-email}"
											 :mail-followup-to "emacsconf-submit@gnu.org, ${email}, ${user-email}"
											 :body
											 "
Hi, ${speakers-short}!

Looks like all systems are a go for your talk. Thanks for
proposing it! Your talk page is now at ${url} . Please feel free
to update it or e-mail us if you'd like help with any changes.${fill}

If you want to get started on your talk early, we have some
instructions at ${base}${year}/prepare/ that might help.
We strongly encourage speakers to prepare a talk video by
${video-target-date} in order to reduce technical risks and make
things flow more smoothly. Plus, we might be able to get it captioned
by volunteers, just like the talks last year. We'll save ${time} minutes
for your talk, not including time for Q&A. Don't sweat it if
you're a few minutes over or under. If it looks like a much shorter or
longer talk once you start getting into it, let us know and we might
be able to adjust.${wrap}

I'll follow up with the specific schedule for your talk once things
settle down. In the meantime, please let us know if you have any
questions or if there's anything we can do to help out!

${signature}"
											 :function emacsconf-mail-accept-talk
											 :log-note "accepted talk")
   (plist-get talk :email)
   (list
		:base emacsconf-base-url
		:user-email user-mail-address
		:year emacsconf-year
		:signature user-full-name
		:conf-name emacsconf-name
		:title (plist-get talk :title)
		:email (plist-get talk :email)
		:time (plist-get talk :time)
		:speakers-short (plist-get talk :speakers-short)
		:url (concat emacsconf-base-url (plist-get talk :url))
		:video-target-date emacsconf-video-target-date)))

(defvar emacsconf-mail-bcc-email "*Extra e-mail address to Bcc for delivery confirmation.")

(defun emacsconf-mail-format-talk-schedule (o)
	"Format the schedule for O for inclusion in mail messages etc."
	(interactive (list (emacsconf-complete-talk)))
	(when (stringp o)
		(setq o
					(emacsconf-resolve-talk
					 (emacsconf-get-slug-from-string o)
					 (or emacsconf-schedule-draft (emacsconf-get-talk-info)))))
	(let ((result
				 (concat
					(plist-get o :title) "\n"
					(emacsconf-timezone-strings-combined
					 (plist-get o :start-time)
					 (plist-get o :timezone)
					 "%b %-e %a %-I:%M %#p %Z"))))
		(when (called-interactively-p 'any)
			(insert result))
		result))

(defun emacsconf-mail-draft-schedule (group &optional arg)
	"Send draft schedule to speakers.
GROUP is (email . (talk talk talk)).
If called with ARG, insert into current buffer instead of composing or updating a message."
  (interactive (list (emacsconf-mail-complete-email-group) current-prefix-arg))
	(let ((emacsconf-mail-prepare-behavior (if arg t emacsconf-mail-prepare-behavior)))
		(emacsconf-mail-prepare
		 (list
			:subject
			"${conf-name} ${year} draft schedule FYI: ${slugs}"
			:reply-to "emacsconf-submit@gnu.org, ${email}, ${user-email}"
			:mail-followup-to "emacsconf-submit@gnu.org, ${email}, ${user-email}"
			:filter (lambda (talk)
								(and (plist-get talk :email)
										 (not (string= (plist-get talk :status) "CANCELLED"))))
			:body
			(if (emacsconf-schedule-q-and-a-p (cadr group))
					;; live
					"${email-notes}Hi, ${speakers-short}!

There are so many wonderful talks at ${conf-name} this year! I
think we're going to have a two-track conference again, so I
wanted to run the draft schedule by you in case you had any
comments or requests.

You can see the draft schedule at
${base}${year}/organizers-notebook/?highlight=${slugs}#draft-schedule
.  If you use a Javascript-enabled browser, your talk${plural}
will be highlighted with a black border in the schedule, and your
talk ID${plural} (${slugs}) will be highlighted with a yellow
background in the notes that follow.${wrap}

As of the time I write this e-mail, your tentative schedule is:

${schedule}
${availability-note}${timezone-note}I might also be able to move things around if you want to
attend any conflicting Q&A sessions or if your availability
changes.${wrap}

Things may have changed a little depending on what other speakers
have asked for, and we'll also update the schedule as we get
closer to the conference. I'll let you know if the schedule for
your talk${plural} changes by more than 2 hours, and we'll send
the updated schedule along with check-in instructions before the
conference.${wrap}

We plan to announce the schedule to the general public on
${schedule-announcement-date}, so we'd love to incorporate any
schedule feedback before then.${wrap}

In the meantime, good luck working on your presentation. ${todos}
Looking forward to ${conf-name} with you!

${signature}
"
				;; after the event
				"${email-notes}Hi, ${speakers-short}!

There are so many wonderful talks at ${conf-name} this year! I
think we're going to have a two-track conference again. You've
indicated that you'd like to take questions after the conference,
so that's totally all right. You don't have to make it to the
time your talk is scheduled; this e-mail is just to keep you up
to date. =)

You can see the draft schedule at
${base}${year}/organizers-notebook/?highlight=${slugs}#draft-schedule
.  If you use a Javascript-enabled browser, your talk${plural}
will have a black border in the schedule and a yellow background
in the notes that follow.

We'll also update the schedule as we get closer to the
conference, but I'll let you know if things change a lot. Anyway,
that's how things are shaping up.

In the meantime, good luck working on your presentation. ${todos}
Looking forward to ${conf-name} with you!

${signature}
"))
		 (plist-get (cadr group) :email)
		 (list
			:schedule-announcement-date emacsconf-schedule-announcement-date
			:slugs (mapconcat (lambda (o) (plist-get o :slug)) (cdr group) ",")
			:email (plist-get (cadr group) :email)
			:base emacsconf-base-url
			:user-email user-mail-address
			:year emacsconf-year
			:signature user-full-name
			:conf-name emacsconf-name
			:speakers-short (plist-get (cadr group) :speakers-short)
			:plural (if (= 1 (length (cdr group))) "" "s")
			:todos
			(concat
			 (if (= 1 (length (cdr group))) "Here's a handy TODO you can use if you want:" "Here are handy TODOs you can use if you want:")
			 "\n\n"
			 (mapconcat
				(lambda (o)
					(emacsconf-replace-plist-in-string
					 (list :title (plist-get o :title)
								 :conf-name emacsconf-name
								 :year emacsconf-year
								 :video-target-date (format-time-string "%Y-%m-%d %a" (date-to-time emacsconf-video-target-date))
								 :submit-email emacsconf-submit-email
								 :base emacsconf-base-url
								 :q-and-a (if (emacsconf-schedule-q-and-a-p o) " (+ time afterwards for Q&A)" "")
								 :time (plist-get o :time)
								 :url (plist-get o :url))
					 "** TODO Prepare \"${title}\" for ${conf-name} ${year}
   DEADLINE: <${video-target-date}>
   (feel free to send it in earlier; let us know at ${submit-email} you're running late)
   Reserved time: ${time} minutes${q-and-a}
   Instructions: ${base}${year}/prepare/
   Talk page: ${base}${url}
"))
				(cdr group) "\n\n"))
			:email-notes (emacsconf-surround "ZZZ: " (plist-get (cadr group) :email-notes) "\n\n" "")
			:schedule
			(emacsconf-indent-string (mapconcat #'emacsconf-mail-format-talk-schedule (cdr group) "\n") 2)
			:availability-note
			(if (delq nil (emacsconf-schedule-get-time-constraint (cadr group)))
					(emacsconf-replace-plist-in-string
					 (list :constraint (emacsconf-schedule-format-time-constraint (cadr group) t (plist-get (cadr group) :timezone)))
					 "I'm using \"${constraint}\" as the availability constraint for you when planning the talks, but since I sometimes mess up encoding these things, could you please doublecheck that this works for you? ")
				"I think you've indicated being available during the conference hours. Thanks for your flexibility. ")
			:timezone-note
			(if (plist-get (cadr group) :timezone)
					(emacsconf-replace-plist-in-string
					 (append
						(list :renamed-timezone (emacsconf-schedule-rename-etc-timezone (plist-get (cadr group) :timezone)))
						(cadr group))
					 "Just let me know if you want us to use a different timezone for translating times in future e-mails. ")
				"I don't think I have a timezone noted for you yet. If you want, I can translate times into your local timezone for you in future e-mails. Just let me know what you would like. ")))))

(defun emacsconf-mail-acknowledge-upload (talk)
	"Acknowledge uploaded files for TALK."
  (interactive (list (emacsconf-complete-talk-info)))
	(emacsconf-mail-prepare
	 '(:subject "${conf-name} ${year}: received uploaded file${plural} for ${title}"
							:cc "emacsconf-submit@gnu.org"
							:reply-to "emacsconf-submit@gnu.org, ${email}, ${user-email}"
							:mail-followup-to "emacsconf-submit@gnu.org, ${email}, ${user-email}"
							:body
							"Hi, ${speakers-short}!

Just a quick note to let you know that I've downloaded your file${plural} for \"${title}\".
Now we have the following file${plural} starting with ${file-prefix}:

${file-list}

I've added the video to the processing queue. You can see how
things are going backstage at ${backstage-url-with-auth} . We'll
be working on captioning your talk over the next few weeks. We'll
e-mail again a little closer to the conference with schedule
updates and other useful information. If you want to upload a new
version, you can upload it the same way you did the previous
one.${fill}

Please feel free to e-mail us at ${submit-email} if you need help updating the talk wiki page at ${base}${url} or if you have other questions.

Thank you so much for all the work you put into preparing a talk for ${conf-name} ${year}, and thank you for submitting the prerecorded video before the conference!

${signature}")
	 (plist-get talk :email)
	 (let ((default-directory emacsconf-cache-dir)
				 (files
					(seq-remove
					 (lambda (o)
						 (string-match "--intro\\.\\(webm\\|vtt\\)"
													 o))
					 (directory-files emacsconf-cache-dir
														t (regexp-quote (plist-get talk :file-prefix))))))
		 (list
			:title (plist-get talk :title)
			:conf-name emacsconf-name
			:year emacsconf-year
			:base emacsconf-base-url
			:url (plist-get talk :url)
			:email (plist-get talk :email)
			:submit-email emacsconf-submit-email
			:plural (if (= 1 (length files)) "" "s")
			:file-prefix (plist-get talk :file-prefix)
			:signature user-full-name
			:user-email user-mail-address
			:speakers-short (plist-get talk :speakers-short)
			:backstage-url-with-auth
			(concat
			 (replace-regexp-in-string
				"https://"
				(format "https://%s:%s@"
								emacsconf-backstage-user
								emacsconf-backstage-password)
				emacsconf-media-base-url)
			 emacsconf-year
			 "/backstage/#"
			 (plist-get talk :slug))
			:file-list
			(mapconcat
			 (lambda (file)
				 (concat
					(format "- %s\n  File size: %s, MD5 sum: %s\n"
									(replace-regexp-in-string (plist-get talk :file-prefix)
																						"" (file-name-nondirectory file))
									(file-size-human-readable (file-attribute-size (file-attributes file)))
									(string-trim
									 (shell-command-to-string
										(concat "md5sum " (shell-quote-argument file) " | cut -f 1 -d ' '"))))
					(if (member (file-name-extension file) emacsconf-media-extensions)
							(format "  (around %d minutes long)\n"
											(ceiling
											 (/ (emacsconf-get-file-duration-ms file)
													60000.0)))
						"")))
			 files
			 "\n")))))

(defun emacsconf-mail-captions-for-review (talk)
	"E-mail captions for TALK so that the speakers can review them."
  (interactive (list (emacsconf-complete-talk-info
											(seq-filter
											 (lambda (talk)
												 (and (emacsconf-talk-file talk "--main.vtt")
															(file-exists-p (emacsconf-talk-file talk "--main.vtt"))))
											 (emacsconf-get-talk-info)))))
  (let ((captions (expand-file-name (concat (plist-get talk :file-prefix) "--main.vtt")
                                    emacsconf-cache-dir))
        (captioner-info
         (with-current-buffer (find-file-noselect emacsconf-org-file)
           (org-entry-properties (org-find-property "CUSTOM_ID" (plist-get talk :captioner))))))
    (emacsconf-mail-prepare
		 (list
			:subject "${conf-name} ${year}: Captions for ${title}"
			:to "${email}"
			:cc "${captioner-email}"
			:body "${email-notes}Hi ${speakers-short}!

Because you sent in your video before the conference, we were able to
caption it so that more people can find and enjoy your talk.
${captioner-volunteered} I've attached the caption text file in case
you want to review it, suggest any corrections, or use the text in a
blog post or elsewhere. You can look at the attached file or watch
your video with closed captions at ${url} . I've also included the
captions at the end of this e-mail for your convenience.${wrap}

${chapters-note}Thanks again for your contribution!

${captioner-thanks}${signature}

${captions}
")
     (plist-get talk :email)
     (list
			:email-notes (emacsconf-surround "ZZZ: " (plist-get talk :email-notes) "\n\n" "")
			:conf-name emacsconf-name
      :speakers-short (plist-get talk :speakers-short)
      :year emacsconf-year
      :email (plist-get talk :email)
      :title (plist-get talk :title)
      :captioner (assoc-default "NAME_SHORT" captioner-info)
			:signature user-full-name
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
           (expand-file-name (concat (plist-get talk :file-prefix) "--main--chapters.vtt")
                             emacsconf-cache-dir))
          "We have some potential chapter headings which you can see as NOTE in the transcript or in the backstage entry for your video. Let me know if you want to tweak those.\n\n"
        "")
      :captioner-thanks
      (if (string= (plist-get talk :captioner) "sachac")
          ""
        (format "%s: Thank you for editing the captions!\n\n" (assoc-default "NAME_SHORT" captioner-info)))
      :captions (mapconcat (lambda (sub) (concat (emacsconf-surround "\n" (elt sub 4) "" "") (elt sub 3))) (subed-parse-file captions) "\n")))
    (mml-attach-file captions "text/vtt" "Subtitles" "attachment")))

(defun emacsconf-mail-upload-and-backstage-info (group)
	"E-mail upload and backstage access information to GROUP."
  (interactive (list (emacsconf-mail-complete-email-group)))
  (emacsconf-mail-prepare
	 (list
		:subject "${conf-name} ${year}: Upload instructions, backstage info"
		:reply-to "emacsconf-submit@gnu.org, ${email}, ${user-email}"
		:mail-followup-to "emacsconf-submit@gnu.org, ${email}, ${user-email}"
		:log-note "sent backstage and upload information"
		:body
		"${email-notes}Hi ${name}!

Hope things are going well for you! I got the upload service up
and running so you can upload your video${plural} and other talk
resources (ex: speaker notes, Org files, slides, etc.).  You can
access it at ${upload-url} with the password
\"${upload-password}\". Please let me know if you run into technical issues.${fill}

If you can get your file(s) uploaded by ${video-target-date},
that would give us plenty of time to reencode it, edit captions,
and so on. If life has gotten a bit busier than expected, that's
okay. You can upload it when you can or we can plan to do your
presentation live.
(Late submissions and live presentations are a bit more stressful, but
it'll probably work out.)${fill}

We've also set up ${backstage} as the backstage area where you
can view the videos and resources uploaded so far. You can access
it with the username \"${backstage-user}\" and the password
\"${backstage-password}\".  Please keep the backstage password
and other speakers' talk resources secret. This is a manual
process, so your talk won't immediately show up there once you've
upload it. Once we've processed your talk, you can use the
backstage area to check if your talk has been correctly reencoded
and see the progress on captions. You can also check out other
people's talks. Enjoy!${fill}

Thank you so much for contributing to ${conf-name} ${year}!

${signature}")
   (car group)
   (list
		:email-notes (emacsconf-surround "ZZZ: " (plist-get (cadr group) :email-notes) "\n\n" "")
    :backstage (emacsconf-replace-plist-in-string
								(list :year emacsconf-year)
								"https://media.emacsconf.org/${year}/backstage/")
		:plural (if (= 1 (length (cdr group))) "" "s")
    :backstage-user emacsconf-backstage-user
    :backstage-password emacsconf-backstage-password
		:upload-url
		(concat "https://ftp-upload.emacsconf.org/?sid="
						emacsconf-upload-password
						"-"
						(mapconcat (lambda (o) (plist-get o :slug)) (cdr group) "-"))
		:upload-password emacsconf-upload-password
		:video-target-date emacsconf-video-target-date
		:name (plist-get (cadr group) :speakers-short)
		:email (car group)
		:user-email user-mail-address
    :signature user-full-name
		:conf-name emacsconf-name
    :year emacsconf-year)))

(defun emacsconf-mail-upload-and-backstage-info-to-waiting-for-prerecs ()
	"Mail upload and backstage information to all speakers who will submit files."
	(interactive)
	(let ((groups (emacsconf-mail-groups (seq-filter (lambda (o) (string= (plist-get o :status) "WAITING_FOR_PREREC"))
																		 (emacsconf-get-talk-info)))))
		(dolist (group groups)
			(emacsconf-mail-upload-and-backstage-info group))))

(defun emacsconf-mail-backstage-info (group)
	"E-mail backstage access information to GROUP."
  (interactive (list (emacsconf-mail-complete-email-group)))
  (emacsconf-mail-prepare
	 (list
		:subject "EmacsConf backstage area with videos and other resources"
		:reply-to "emacsconf-submit@gnu.org, ${email}, ${user-email}"
		:mail-followup-to "emacsconf-submit@gnu.org, ${email}, ${user-email}"
		:log-note "sent backstage information"
		:body
		"Hi ${name}!

You're getting this e-mail because you are a ${role} for ${conf-name} ${year}. (Thanks!)
As a perk, you get early access as the talks come in.${fill}

We've set up ${backstage} as the backstage area where you can view the
videos and resources uploaded so far. You can access it with the
username \"${backstage-user}\" and the password \"${backstage-password}\".
Please keep the backstage password and other speakers' talk resources
secret. ${backstage-use}${fill}

Thank you!

${signature}")
   (car group)
   (list
    :backstage (emacsconf-replace-plist-in-string
								(list :year emacsconf-year)
								"https://media.emacsconf.org/${year}/backstage/")
		:role (or (plist-get (cadr group) :role) "speaker")
		:plural (if (= 1 (length (cdr group))) "" "s")
		:backstage-use
		(cond
		 ((string= (plist-get (cadr group) :role) "volunteer")
			"If you see a talk that you'd like to caption, please let us know and we can reserve it for you. I'll also let people periodically know when more talks come in.")
		 ((string= (plist-get (cadr group) :status) "WAITING_FOR_PREREC")
			"As we add more talks, you can skim through any relevant ones to
see if there are any points you'd like to build on in your talk.
Also, you can get a sense of what we do behind the scenes to try
to get as many talks captioned for broadcast, and what you can do
to make it easier. (A script or a text file with names and
technical terms can be helpful. No need to type out a manual
transcript if you don't start from a script.) After you upload
your talk and we process the files, you can use the backstage
area to check the quality of the reencoded video.")
		 (t
			"You can confirm the quality of your talk's reencoding to make sure everything plays as well as you'd like (and
check the captions after they're edited), and you can watch other
people's talks too."))
    :backstage-user emacsconf-backstage-user
    :backstage-password emacsconf-backstage-password
		:name (plist-get (cadr group) :speakers-short)
		:email (car group)
		:user-email user-mail-address
    :signature user-full-name
		:conf-name emacsconf-name
    :year emacsconf-year)))

(defun emacsconf-mail-backstage-info-to-volunteer (volunteer)
	"E-mail backstage info to captioning volunteers."
	(interactive (list (emacsconf-complete-volunteer)))
	(emacsconf-mail-backstage-info
	 (list
		(assoc-default "EMAIL" volunteer)
		(list :role "volunteer"
					:speakers-short
					(or (assoc-default "NAME" volunteer)
							(assoc-default "NAME_SHORT" volunteer))))))

(defun emacsconf-mail-backstage-info-to-captioning-volunteers ()
	"E-mail backstage info to captioning volunteers."
	(interactive)
	(dolist (volunteer (seq-filter
											(lambda (o)
												(string-match ":captions:" (assoc-default "ALLTAGS" o 'string= "")))
											(emacsconf-get-volunteer-info)))
		(emacsconf-mail-backstage-info
		 (list
			(assoc-default "EMAIL" volunteer)
			(list :role "volunteer"
						:speakers-short
						(or (assoc-default "NAME" volunteer)
								(assoc-default "NAME_SHORT" volunteer)))))))

(defun emacsconf-mail-backstage-info-to-speakers-and-captioners ()
  (interactive)
  (let ((template (emacsconf-mail-merge-get-template "backstage"))
        (speaker-groups
         (seq-uniq
          (mapcar
           (lambda (talk)
             (list
              :name (plist-get talk :speakers-short)
              :email (plist-get talk :email)
              :role "speaker"
              :backstage-use
              "As we add more talks, you can skim through any relevant ones to
see if there are any points you'd like to build on in your talk.
Also, you can get a sense of what we do behind the scenes to try
to get as many talks captioned for broadcast, and what you can do
to make it easier. (A text file with names and technical terms
can be helpful. No need to type out a manual transcript if you
don't start from a script.) After you upload your talk and we
process the files, you can use the backstage area to check the
quality of the reencoded video."))
           (seq-filter (lambda (o) (string= (plist-get o :status) "WAITING_FOR_PREREC"))
                       (emacsconf-filter-talks (emacsconf-get-talk-info))))))
        (volunteer-groups
         (with-current-buffer (find-file-noselect emacsconf-org-file)
           (org-map-entries (lambda ()
                              (list :name (org-entry-get (point) "NAME_SHORT")
                                    :email (org-entry-get (point) "EMAIL")
                                    :role "captioning volunteer"
                                    :backstage-use "If you see a talk that you'd like to caption, you can e-mail me at sacha@sachachua.com and I can reserve it for you."))
                            "captions"))))
    (mapcar (lambda (g) (emacsconf-mail-backstage-info g template))
            (append
             speaker-groups
             (seq-remove (lambda (v) (seq-find (lambda (s) (string= (plist-get s :email)
                                                                    (plist-get v :email)))
                                               speaker-groups))
                         volunteer-groups)))))

(defun emacsconf-mail-bbb-tips-and-intro-to-all (&optional types)
	"Draft BBB information for all speakers."
	(interactive)
	(let* ((log-note "sent bbb-tips-and-intro")
				 (talk-groups (seq-group-by
											 (lambda (talk)
												 (cond
													((string= (plist-get talk :status) "WAITING_FOR_PREREC")
													 'waiting-for-prerec)
													((string= (plist-get talk :qa-type) "live")
													 'live)
													((or (string= (plist-get talk :qa-type) "irc")
															 (string= (plist-get talk :qa-type) "pad"))
													 'pad-or-irc)
													((string= (plist-get talk :qa-type) "none")
													 'after)
													(t (error "Exception for %s" (plist-get talk :slug)))))
											 (seq-filter (lambda (o) (plist-get o :email))
																	 (emacsconf-filter-talks-by-logbook
																		log-note
																		(emacsconf-active-talks (emacsconf-get-talk-info))))))
				 (base (list :reply-to "emacsconf-submit@gnu.org, ${email}, ${user-email}"
										 :log-note log-note
										 :mail-followup-to "emacsconf-submit@gnu.org, ${email}, ${user-email}")))
		(setq types (or types (mapcar 'car talk-groups)))
		(dolist (type types)
			(dolist (group (emacsconf-mail-groups (cdr (assoc-default type talk-groups))))
				(emacsconf-mail-prepare
				 (append
					base
					(pcase type
						('waiting-for-prerec
						 (list
							:subject "${conf-name} ${year} action needed: Options, BigBlueButton info, intro"
							:body
							"${email-notes}Hi, ${name}!

${conf-name} is in two weeks. Aaah! I don't think we have your
presentation yet, but I'm not panicking (much) because we've got plans
and backup plans.

Option A: You can upload your presentation before the conference

There's still a little time to squeeze in processing presentations and
possibly even captioning them. If you're getting stuck because you
want your presentation to be totally awesome, it's okay, it doesn't
have to be perfect. If it's too long or too short, that's cool too; we
can manage the time around that. We'll figure things out together. =)

You can upload your file(s) to ${upload-url} (password
${upload-password}) and we'll get things going.${fill}

If you're thinking of doing the Q&A in a live web conference,
please also test the BigBlueButton URL described under Option
B. Thanks!

Option B: You can do it live

Sometimes it's easier to do a presentation live, or sometimes you're
making last-minute tweaks and want to play the latest copy of your
video from your own computer. We can do the presentation live over
BigBlueButton. Your room URL is ${bbb-url} , and I've put together
some tips at ${bbb-tips} . Please
help reduce the technical risks by trying out the BigBlueButton setup
before November 28 so that there's time to help troubleshoot.${fill}

Option C: It's okay, you can cancel

Sometimes an interesting talk idea turns out to be more challenging
than you'd like. Sometimes life happens. If you're stressing out and
you don't think you can make it, no worries, no need to feel
embarrassed or guilty or anything like that. Let me know and I can
update the schedule so that other speakers have extra time for Q&A. We
hope you'll consider proposing a talk for another EmacsConf!

----------------
If you're planning to go through with the talk (yay!), could you
please take a minute to check if I pronounce your name correctly in
the intro I recorded? The recording is at ${intro-url} , and you can
also find it under \"--intro.webm\" in your talk's section at
${backstage-url-with-credentials} . If it needs tweaking, you can
upload a recording to ${upload-url} (password ${upload-password}) or
e-mail me the corrections.${fill}

Best regards,

${signature}"))
						('live
						 (list
							:subject "${conf-name} ${year} action needed: BigBlueButton info, intro"
							:body
							"${email-notes}Hi, ${name}!

Thanks again for uploading your presentation early!

Since you're planning to do a live Q&A session, I've set up a
BigBlueButton web conference room for you at ${bbb-url} . I've
also put together some tips at ${bbb-tips} . Please help reduce
the technical risks by trying out the BigBlueButton setup before
November 28 so that there's time to help troubleshoot. Sharing
system audio or multi-monitor setups can sometimes be tricky, so
please let us know if you need help figuring things out. ${fill}

Also, could you please take a minute to check if I pronounce your name
correctly in the intro I recorded? The recording is at ${intro-url} ,
and you can also find it under \"--intro.webm\" in your talk's section
at ${backstage-url-with-credentials} . If it needs tweaking, you can
upload a recording to ${upload-url} (password ${upload-password}) or
e-mail me the corrections.

Best regards,

${signature}"))
						('pad-or-irc
						 (list
							:subject "${conf-name} ${year} action needed: check intro pronunciation"
							:body
							"${email-notes}Hi, ${name}!

Thanks again for uploading your presentation early!

Could you please take a minute to check if I pronounce your name
correctly in the intro I recorded? The recording is at ${intro-url} ,
and you can also find it under \"--intro.webm\" in your talk's section
at ${backstage-url-with-credentials} . If it needs tweaking, you can
upload a recording to ${upload-url} (password ${upload-password}) or
e-mail me the corrections.

We'll send you check-in instructions a few days before the conference
so that you'll be all set.

Best regards,

${signature}"))
						('after
						 (list
							:subject "${conf-name} ${year} action needed: check intro pronunciation"
							:body
							"${email-notes}Hi, ${name}!

Thanks again for uploading your presentation early!

Could you please take a minute to check if I pronounce your name
correctly in the intro I recorded? The recording is at ${intro-url} ,
and you can also find it under \"--intro.webm\" in your talk's section
at ${backstage-url-with-credentials} . If it needs tweaking, you can
upload a recording to ${upload-url} (password ${upload-password}) or
e-mail me the corrections.

You've indicated that you won't be able to attend the conference
itself. That's cool, we'll collect questions during the conference and
e-mail you afterwards. If I got your preferences wrong, please let me
know if you'd like to handle questions during the conference via
Etherpad, IRC, or a live web conference. We'd love to have you join
us!

Best regards,

${signature}"))
						(_ (error "Unknown type %s" (symbol-name type)))))
				 (car group)
				 (list
					:email-notes (or (plist-get (car group) :email-notes) "")
					:conf-name emacsconf-name
					:year emacsconf-year
					:name (plist-get (cadr group) :speakers-short)
					:email (car group)
					:user-email user-mail-address
					:signature user-full-name
					:backstage-url-with-credentials
					(mapconcat (lambda (talk)
											 (format "https://%s:%s@media.emacsconf.org/%s/backstage/#%s"
															 emacsconf-backstage-user
															 emacsconf-backstage-password
															 emacsconf-year
															 (plist-get talk :slug)))
										 (cdr group)
										 " , ")
					:upload-url (concat "https://ftp-upload.emacsconf.org/?sid="
															emacsconf-upload-password
															"-"
															(mapconcat (lambda (o) (plist-get o :slug)) (cdr group) "-"))
					:upload-password emacsconf-upload-password
					:bbb-url
					(cond
					 ((null (file-exists-p
									 (expand-file-name
										(format "assets/redirects/open/bbb-%s.html" (plist-get (cadr group) :slug))
										emacsconf-backstage-dir)))
						(error "Backstage redirect for %s does not exist" (plist-get (cadr group) :slug)))
					 ((null (= (length (seq-uniq (mapcar (lambda (o) (plist-get o :bbb-room)) (cdr group)))) 1))
						(error "Number of rooms for %s speaker: %d"
									 (plist-get (cadr group) :slug)
									 (length (seq-uniq (mapcar (lambda (o) (plist-get o :bbb-room)) (cdr group))))))
					 (t (emacsconf-backstage-url (plist-get (car (cdr group)) :bbb-backstage))))
					:bbb-tips (concat emacsconf-base-url emacsconf-year "/bbb-for-speakers/")
					:intro-url (mapconcat
											(lambda (talk)
												(if (file-exists-p (expand-file-name
																						(concat (plist-get talk :file-prefix) "--intro.webm")
																						emacsconf-backstage-dir))
														(format "https://%s:%s@media.emacsconf.org/%s/backstage/%s--intro.webm"
																		emacsconf-backstage-user
																		emacsconf-backstage-password
																		emacsconf-year
																		(plist-get talk :file-prefix))
													(error "No intro file for %s" (plist-get talk :slug))))
											(cdr group)
											" , ")))))))

(defun emacsconf-mail-checkin-instructions-to-all ()
	"Draft check-in instructions for all speakers."
	(interactive)
	(let* ((talks
					(emacsconf-filter-talks-by-logbook
					 "sent check-in information"
					 (seq-filter (lambda (o) (and (plist-get o :email)
																				(plist-get o :q-and-a)))
											 (emacsconf-publish-prepare-for-display (emacsconf-get-talk-info)))))
				 (by-attendance (seq-group-by (lambda (o) (null (string-match "after" (plist-get o :q-and-a))))
																			talks)))
		(dolist (group (emacsconf-mail-groups (assoc-default nil by-attendance)))
			(emacsconf-mail-checkin-instructions-for-nonattending-speakers group))
		(dolist (group (emacsconf-mail-groups (assoc-default t by-attendance)))
			(emacsconf-mail-checkin-instructions-for-attending-speakers group))))

(defun emacsconf-mail-schedule-update ()
	"E-mail day-of schedule updates"
	(interactive)
	(let ((groups
				 (emacsconf-mail-groups
					(seq-remove (lambda (talk)
												(string= (replace-regexp-in-string "[<>]" "" (plist-get talk :scheduled))
																 (plist-get talk :checkin-schedule-sent)))
											(emacsconf-publish-prepare-for-display (emacsconf-get-talk-info))))))
		(dolist (group groups)
			(emacsconf-mail-prepare
			 (list
				:subject "${conf-name} ${year}: Schedule update - new check-in time ${checkin-time}"
				:reply-to "emacsconf-submit@gnu.org, ${email}, ${user-email}"
				:mail-followup-to "emacsconf-submit@gnu.org, ${email}, ${user-email}"
				:log-note "sent updated schedule"
				:body
				"${email-notes}Hello, ${speakers-short}!

We tweaked the schedule a bit! Your new check-in information is:

${checkin-info}

Please check in early so that we can deal with scheduling changes
or technical issues, and so that we don't worry too much about
missing speakers (aaah!). You can find the check-in
process at ${base-url}${year}/speakers/ . ${wrap}

If something comes up, please let us know as soon as you can. Here's
my emergency contact information: ${emergency}${wrap}

Thank you for sharing your time and energy with the ${conf-name}
community!

${signature}

p.s. If you need to cancel, that's okay too, life happens. Let me know
as soon as you can and I'll try to shuffle things around. Thank you!")
			 (car group)
			 (list
				:year emacsconf-year
				:base-url emacsconf-base-url
				:conf-name emacsconf-name
				:user-email user-mail-address
				:email (car group)
				:checkin-time (mapconcat (lambda (o)
																	 (emacsconf-timezone-strings-combined
																		(plist-get o :checkin-time)
																		(plist-get o :timezone)))
																 (cdr group)
																 "; ")
				:emergency emacsconf-emergency-contact
				:plural (if (> (length (cdr group)) 1) "s" "")
				:speakers-short (plist-get (cadr group) :speakers-short)
				:url (mapconcat (lambda (o) (concat emacsconf-base-url (plist-get o :url)))
												(cdr group) " , ")
				:email-notes (emacsconf-surround "ZZZ: " (plist-get (cadr group) :email-notes) "\n\n" "")
				:signature user-full-name
				:checkin-info
				(mapconcat
				 (lambda (o)
					 (emacsconf-replace-plist-in-string
						(append (list :base-url emacsconf-base-url
													:check-in
													(concat
													 "Before "
													 (emacsconf-timezone-strings-combined
														(plist-get o :checkin-time)
														(plist-get o :timezone))
													 "\n  (this is " (plist-get o :checkin-label) ")"
													 "\n")
													:qa-info-speakers
													(cond
													 ((or (plist-get o :live) (null (plist-get o :video-file))) ;; intentionally a live talk
														(concat "Talk & Q&A BigBlueButton room: "
																		(emacsconf-backstage-url (plist-get o :bbb-backstage))))
													 ((string= (plist-get o :qa-type) "none")
														"Q&A: After the event; we'll collect the questions and e-mail them to you")
													 ((string= (plist-get o :qa-type) "live")
														(concat "Q&A BigBlueButton room: "
																		(emacsconf-backstage-url (plist-get o :bbb-backstage))))
													 ((string= (plist-get o :qa-type) "irc")
														(concat "Q&A: On IRC: #" (plist-get o :channel) " ( " (plist-get o :webchat-url) " )"))
													 ((string= (plist-get o :qa-type) "pad")
														(concat "Q&A: On Etherpad"))
													 (t "Q&A: After the event; we'll collect the questions and e-mail them to you"))
													)
										o)
						"- ${title}
  Info and sched: ${base-url}${url}
  Check-in: ${check-in}
  Pad: ${pad-url}
  ${qa-info-speakers}"))
				 (cdr group) "\n\n"))))))

(defun emacsconf-mail-checkin-instructions-for-attending-speakers (group)
  "Send checkin instructions to speakers who will be there.
GROUP is (email . (talk talk))"
  (interactive (list (emacsconf-mail-complete-email-group
                      (seq-remove
                       (lambda (o)
                         (or
                          (string= (plist-get o :status) "CANCELLED")
                          (null (plist-get o :email))
                          (string-match "after" (or (plist-get o :q-and-a) ""))))
                       (emacsconf-get-talk-info)))))
	(emacsconf-mail-prepare
	 (list
		:subject "${conf-name} ${year}: Check-in instructions"
		:reply-to "emacsconf-submit@gnu.org, ${email}, ${user-email}"
		:mail-followup-to "emacsconf-submit@gnu.org, ${email}, ${user-email}"
		:log-note "sent check-in information for people who will be there"
		:body
		"${email-notes}Hello, ${speakers-short}!

We're looking forward to having you join us at ${conf-name}!
Here's your talk page URL and checkin information:

${checkin-info}

Please check in early so that we can deal with scheduling changes
or technical issues, and so that we don't worry too much about
missing speakers (aaah!). You can find the check-in
process at ${base-url}${year}/speakers/ . ${wrap}

${waiting}If something comes up, please let us know as soon as you can. Here's
my emergency contact information: ${emergency}${wrap}

Thank you for sharing your time and energy with the ${conf-name}
community!

${signature}

p.s. If you need to cancel, that's okay too, life happens. Let me know
as soon as you can and I'll try to shuffle things around. Thank you!")
   (car group)
   (list
		:year emacsconf-year
		:base-url emacsconf-base-url
		:conf-name emacsconf-name
		:user-email user-mail-address
		:email (car group)
		:emergency emacsconf-emergency-contact
		:plural (if (> (length (cdr group)) 1) "s" "")
		:speakers-short (plist-get (cadr group) :speakers-short)
		:url (mapconcat (lambda (o) (concat emacsconf-base-url (plist-get o :url)))
										(cdr group) " , ")
		:email-notes (emacsconf-surround "ZZZ: " (plist-get (cadr group) :email-notes) "\n\n" "")
		:waiting (let ((waiting (seq-remove (lambda (o) (plist-get o :video-file)) (cdr group))))
							 (cond
								((= (length waiting) 0) "")
								((= (length waiting) 1) "If you happen to be able to get a pre-recorded video together in the next few days, I think we might be able to still manage that.${fill}\n\n")
								(t "If you happen to be able to get your pre-recorded videos together in the next few days, I think we might be able to still manage them.${fill}\n\n")))
		:signature user-full-name
		:checkin-info
		(mapconcat
     (lambda (o)
       (emacsconf-replace-plist-in-string
				(append (list :base-url emacsconf-base-url
											:check-in
											(concat
                       "Before "
											 (emacsconf-timezone-strings-combined
												(plist-get o :checkin-time)
												(plist-get o :timezone))
                       "\n  (this is " (plist-get o :checkin-label) ")")
											:qa-info-speakers
											(cond
											 ((or (plist-get o :live) (null (plist-get o :video-file)))	;; intentionally a live talk
												(concat "Talk & Q&A BigBlueButton room: "
																(emacsconf-backstage-url (plist-get o :bbb-backstage))))
                       ((string= (plist-get o :qa-type) "none")
												"Q&A: After the event; we'll collect the questions and e-mail them to you")
											 ((string= (plist-get o :qa-type) "live")
												(concat "Q&A BigBlueButton room: "
																(emacsconf-backstage-url (plist-get o :bbb-backstage))))
											 ((string= (plist-get o :qa-type) "irc")
												(concat "Q&A: On IRC: #" (plist-get o :channel) " ( " (plist-get o :webchat-url) " )"))
											 ((string= (plist-get o :qa-type) "pad")
												(concat "Q&A: On Etherpad"))
											 (t "Q&A: After the event; we'll collect the questions and e-mail them to you")))
								o)
				"- ${title}
  Info and sched: ${base-url}${url}
  Check-in: ${check-in}
  Pad: ${pad-url}
  ${qa-info-speakers}"))
		 (cdr group) "\n\n"))))

(defun emacsconf-mail-checkin-instructions-for-nonattending-speakers (group)
  "Send checkin note to speakers who will not be there.
GROUP is (email . (talk talk))"
  (interactive (list (emacsconf-mail-complete-email-group
                      (seq-remove
                       (lambda (o)
                         (or
                          (string= (plist-get o :status) "CANCELLED")
                          (null (plist-get o :email))
                          (null (string-match "after" (or (plist-get o :q-and-a) "")))))
                       (emacsconf-get-talk-info)))))
	 (emacsconf-mail-prepare
		(list
		 :subject "${conf-name} ${year}: Check-in instructions in case you happen to want to join us"
		 :reply-to "emacsconf-submit@gnu.org, ${email}, ${user-email}"
		 :mail-followup-to "emacsconf-submit@gnu.org, ${email}, ${user-email}"
		 :log-note "sent check-in information for people who will not be there"
		 :body
		 "${email-notes}Hello, ${speakers-short}!

Thank you so much for contributing to ${conf-name} ${year}! We're
looking forward to collecting questions and forwarding them to
you by e-mail after the conference. We'll also post the
prerecording at the time that it gets streamed, so people will be
able to access it at ${url} once it has gone live.${wrap}

If it turns out that you can make it to the conference after all, feel
free to drop us a line at #emacsconf-org and we'll let people know
you're around. You can find the check-in process at
${base-url}${year}/speakers/ .

Thank you again for being part of ${conf-name} ${year}!

Sacha")
    (car group)
    (list
     :year emacsconf-year
     :base-url emacsconf-base-url
     :conf-name emacsconf-name
     :email (car group)
		:user-email user-mail-address
     :speakers-short (plist-get (cadr group) :speakers-short)
     :url (mapconcat (lambda (o) (concat emacsconf-base-url (plist-get o :url)))
                     (cdr group) " , ")
		 :email-notes (emacsconf-surround "ZZZ: " (plist-get (cadr group) :email-notes) "\n\n" ""))))


;;; Other mail functions

(defun emacsconf-mail-verify-delivery (groups subject)
	"Verify that the email addresses in GROUPS have all received an email with SUBJECT."
	(interactive (list (emacsconf-mail-groups (seq-filter (lambda (o) (not (string= (plist-get o :status) "CANCELLED")))
																					(emacsconf-get-talk-info)))
										 (read-string "Subject: ")))
	(let ((missing
				 (seq-keep (lambda (group)
										 (and (string= "0"
																	 (string-trim
																		(shell-command-to-string
																		 (format "notmuch count to:%s and to:%s and subject:%s"
																						 (shell-quote-argument (car group))
																						 (shell-quote-argument emacsconf-mail-bcc-email)
																						 subject))))
													(car group)))
									 groups)))
		(if missing
				(prin1 missing)
			(message "All good."))))

(defun emacsconf-mail-get-all-email-addresses (talk)
	"Return all the possible e-mail addresses for TALK."
	(split-string
	 (downcase
		(string-join
		 (seq-uniq
			(seq-keep
			 (lambda (field) (plist-get talk field))
			 '(:email :public-email :email-alias)))
		 ","))
	 " *, *"))

(defvar emacsconf-mail-notmuch-tag "emacsconf" "Tag to use when searching the Notmuch database for mail.")

(defun emacsconf-mail-notmuch-last-message-for-talk (talk &optional subject)
	"Return the most recent message from the speakers for TALK.
Limit to SUBJECT if specified."
	(let ((message (json-parse-string
									(shell-command-to-string
									 (format "notmuch search --limit=1 --format=json \"%s%s\""
													 (mapconcat
														(lambda (email) (concat "from:" (shell-quote-argument email)))
														(emacsconf-mail-get-all-email-addresses talk)
														" or ")
													 (emacsconf-surround
														" and "
														(and emacsconf-mail-notmuch-tag (shell-quote-argument emacsconf-mail-notmuch-tag))
														"" "")
													 (emacsconf-surround
														" and subject:"
														(and subject (shell-quote-argument subject)) "" "")))
									:object-type 'alist)))
		(cons `(email . ,(plist-get talk :email))
					(when (> (length message) 0)
						(elt message 0)))))

(defun emacsconf-mail-notmuch-visit-thread-from-summary ()
	"Display the thread from the summary."
	(interactive)
	(let (message-buffer)
		(save-window-excursion
			(setq message-buffer (notmuch-show (tabulated-list-get-id))))
		(display-buffer message-buffer t)))

(defun emacsconf-mail-notmuch-show-latest-messages-from-speakers (groups &optional subject)
	"Verify that the email addresses in GROUPS have e-mailed recently.
When called interactively, pop up a report buffer showing the e-mails
and messages by date, with oldest messages on top.
This minimizes the risk of mail delivery issues and radio silence."
	(interactive (list (emacsconf-mail-groups (seq-filter
															 (lambda (o) (not (string= (plist-get o :status) "CANCELLED")))
															 (emacsconf-get-talk-info)))))
	(let ((results
				 (sort (mapcar
								(lambda (group)
									(emacsconf-mail-notmuch-last-message-for-talk (cadr group) subject))
								groups)
							 (lambda (a b)
								 (< (or (alist-get 'timestamp a) -1)
										(or (alist-get 'timestamp b) -1))))))
		(when (called-interactively-p 'any)
			(with-current-buffer (get-buffer-create "*Mail report*")
				(let ((inhibit-read-only t))
					(erase-buffer))
				(tabulated-list-mode)
				(setq
				 tabulated-list-entries
				 (mapcar
					(lambda (row)
						(list
						 (alist-get 'thread row)
						 (vector
							(alist-get 'email row)
							(or (alist-get 'date_relative row) "")
							(or (alist-get 'subject row) ""))))
					results))
				(setq tabulated-list-format [("Email" 30 t)
																		 ("Date" 10 nil)
																		 ("Subject" 30 t)])
				(local-set-key (kbd "RET") #'emacsconf-mail-notmuch-visit-thread-from-summary)
				(tabulated-list-print)
				(tabulated-list-init-header)
				(pop-to-buffer (current-buffer))))
		results))

(defun emacsconf-mail-talks (email)
	"Return a list of talks matching EMAIL."
	(setq email (downcase (mail-strip-quoted-names email)))
	(seq-filter
	 (lambda (o) (member email (emacsconf-mail-get-all-email-addresses o)))
	 (emacsconf-get-talk-info)))

(defun emacsconf-mail-add-to-logbook (email note)
	"Add to logbook for all matching talks from this speaker."
	(interactive
	 (let* ((email (mail-strip-quoted-names
									(plist-get (plist-get (notmuch-show-get-message-properties) :headers)
														 :From)))
					(talks (emacsconf-mail-talks email)))
		 (list
			email
			(read-string (format "Note for %s: "
													 (mapconcat (lambda (o) (plist-get o :slug))
																			talks", "))))))
	(save-window-excursion
		(mapc
		 (lambda (talk)
			 (emacsconf-add-to-talk-logbook talk note))
		 (emacsconf-mail-talks email))))

(defun emacsconf-mail-notmuch-save-attachments-to-cache (talk)
	"Save the attached files to the cache and backstage dir for TALK."
	(interactive (list (emacsconf-complete-talk-info)))
	(with-current-notmuch-show-message
	 (notmuch-foreach-mime-part
		(lambda (part)
			(when (string= (car (mm-handle-disposition part)) "attachment")
				(prin1 part)
				(let* ((filename (or (mail-content-type-get
															(mm-handle-disposition part) 'filename)
														 (mail-content-type-get
															(mm-handle-type part) 'name)))
							 (extra (read-string (concat filename ": ")))
							 (new-filename (concat (plist-get talk :file-prefix)
																		 extra "." (file-name-extension filename))))
					(mm-save-part-to-file
					 part (expand-file-name new-filename emacsconf-cache-dir))
					(mm-save-part-to-file
					 part (expand-file-name new-filename (expand-file-name "cache" emacsconf-res-dir)))
					(mm-save-part-to-file
					 part (expand-file-name new-filename emacsconf-backstage-dir)))))
		(mm-dissect-buffer))))

(provide 'emacsconf-mail)
;;; emacsconf-mail.el ends here
