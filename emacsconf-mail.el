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

(defun emacsconf-mail-complete-email-group (&optional info)
  "Return (email . (talk talk))."
  (setq info (emacsconf-filter-talks (or info (emacsconf-get-talk-info))))
  (save-window-excursion
    (let* ((grouped (seq-group-by (lambda (o) (plist-get o :email)) info))
           (slug (emacsconf-get-slug-from-string (emacsconf-complete-talk)))
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
  (save-excursion (insert (emacsconf-replace-plist-in-string attrs (plist-get template :body)))
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

(defun emacsconf-mail-template-to-group ()
  "Prompt for a speaker and e-mail current template to them."
  (interactive)
  (let* ((template (if (org-entry-get (point) "EMAIL_ID")
                       (emacsconf-mail-merge-get-template-from-subtree)
                     (emacsconf-mail-merge-get-template
                      (completing-read "Template: " (org-property-values "EMAIL_ID")))))
         (mail-func (plist-get template :function)))
    (funcall mail-func (emacsconf-mail-complete-email-group) template)))

(defun emacsconf-mail-template-to-all-groups ()
  "Uses the current template to draft messages to all the speakers.
Group by e-mail."
  (interactive)
  (let* ((template (if (org-entry-get (point) "EMAIL_ID")
                       (emacsconf-mail-merge-get-template-from-subtree)
                     (emacsconf-mail-merge-get-template
                      (completing-read "Template: " (org-property-values "EMAIL_ID")))))
         (info (seq-filter (lambda (o)
                             (if (plist-get template :slugs)
                                 (member (plist-get o :slug)
                                         (split-string (plist-get template :slugs) " "))
                               t))
                           (emacsconf-prepare-for-display (emacsconf-filter-talks (emacsconf-get-talk-info)))))
         (grouped (emacsconf-mail-group-by-email info))
         (mail-func (plist-get template :function)))
    (mapc (lambda (group)
            (funcall mail-func group template))
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
                    (intern (org-entry-get-with-inheritance "FUNCTION")))))

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
