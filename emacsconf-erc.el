;;; emacsconf-erc.el --- Integrate with Emacs Relay Chat  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Sacha Chua

;; Author: Sacha Chua <sacha@sachachua.com>
;; Keywords: 

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

;; - /announce slug
;; - /broadcast message
;; - /conftopic message
;; - /checkin ROOM nick
;; - /ready ROOM slug

;;; Code:

(defcustom emacsconf-collaborative-pad
  (concat "https://etherpad.wikimedia.org/p/emacsconf-" emacsconf-year)
  "URL for the pad that will have questions."
  :group 'emacsconf
  :type 'string)

(defcustom emacsconf-streaming-nick nil "Set to the host's IRC nick in order to notify them."
  :group 'emacsconf
  :type 'string)

(defcustom emacsconf-topic-templates
  '(("#emacsconf" "EmacsConf 2022 | Subscribe to https://lists.gnu.org/mailman/listinfo/emacsconf-discuss for updates")
    ("#emacsconf-accessible" "EmacsConf 2022 accessibility - help by describing what's happening | Subscribe to https://lists.gnu.org/mailman/listinfo/emacsconf-discuss for updates")
    ("#emacsconf-org" "EmacsConf 2022 | Dedicated channel for EmacsConf organizers and speakers | this is intended as an internal, low-traffic channel; for main discussion around EmacsConf, please join #emacsconf | Subscribe to https://lists.gnu.org/mailman/listinfo/emacsconf-discuss for updates")
    ("#emacsconf-questions" "EmacsConf 2022 | Low-traffic channel for questions if speakers prefer IRC and need help focusing; for main discussion around EmacsConf, please join #emacsconf | Subscribe to https://lists.gnu.org/mailman/listinfo/emacsconf-discuss for updates"))
  "List of (channel topic-template) entries for mass-setting channel topics."
  :group 'emacsconf
  :type '(repeat (list (string :tag "Channel")
                       (string :tag "Topic suffix"))))

;; For testing: (setq emacsconf-topic-templates '(("#emacsconf-test" "EmacsConf 2021 | Dedicated channel for EmacsConf organizers and speakers | this is intended as an internal, low-traffic channel; for main discussion around EmacsConf, please join #emacsconf | Subscribe to https://lists.gnu.org/mailman/listinfo/emacsconf-discuss for updates")))

(defcustom emacsconf-rooms
  '(("A" "http://example.org?room=a")
    ("B" "http://example.org?room=b")
    ("C" "http://example.org?room=c"))
  "Room IDs and URLs."
  :group 'emacsconf
  :type '(repeat (list (string :tag "ID")
                       (string :tag "URL"))))

    (defmacro emacsconf-erc-with-channels (channel-list &rest forms)
      (declare (indent 1) (debug (form form body)))
      `(mapcar (lambda (channel)
                 (with-current-buffer (erc-get-buffer channel)
                   ,@forms))
               ,channel-list))

    (defun emacsconf-get-room (room)
      (cadr (assoc (upcase room) emacsconf-rooms)))

    (defun erc-cmd-CONFTOPIC (&rest message)
      "Set the topic to MESSAGE | template in the emacsconference channels.
    If MESSAGE is not specified, reset the topic to the template."
      (mapc (lambda (template) 
              (with-current-buffer (erc-get-buffer (car template))
                (erc-cmd-TOPIC (if message (concat (if (stringp message) message (s-join " " message)) " | " (cadr template))
                                 (cadr template)))))
            emacsconf-topic-templates))

(defun erc-cmd-CHECKIN (nick &optional q-and-a)
  (let ((talk (emacsconf-complete-talk))
        info)
    (save-excursion
      (emacsconf-with-talk-heading talk
        (org-entry-put (point) "IRC" nick)
        (org-entry-put (point) "CHECK_IN" (format-time-string "%H:%M"))
        (setq info (emacsconf-get-talk-info-for-subtree))))
    (setq q-and-a (or q-and-a (plist-get info :q-and-a) ""))
    (cond
     ((string-match "live" q-and-a)
      (erc-send-message (format "%s: Thanks for checking in! I'll send you some private messages with instructions, so please check there. Let me know if you don't get them." nick))
      (erc-cmd-BBB (completing-read "Room: " emacsconf-rooms) nick talk))
     ((string-match "pad" q-and-a)
      (erc-send-message (format "%s: Thanks for checking in! The collaborative pad we'll be using for questions is at %s . We'll collect questions from #emacsconf and put them there. If you'd like to jump to your part of the document, you might be able to keep an eye on questions. Please let us know if you need help, or if you want to switch to live Q&A." nick emacsconf-collaborative-pad)))
     ((string-match "IRC" q-and-a)
      (erc-send-message (format "%s: Thanks for checking in! Feel free to keep an eye on #emacsconf for questions and discussion, and we'll copy things from the pad to there. If the volume gets overwhelming, let us know and we can forward questions to #emacsconf-questions for you. If you'd like to try Q&A over live video or the collaborative pad instead, or if you need help, please let us know." nick emacsconf-collaborative-pad)))
     (t (erc-send-message (format "%s: Thanks for checking in! How would you like to handle Q&A today - live video, the collaborative Etherpad at %s , or IRC (like this)?" nick emacsconf-collaborative-pad))))
    (emacsconf-with-talk-heading talk
      (emacsconf-upcoming-insert-or-update nil t))))

(defun erc-cmd-BBB (room nick &optional talk)
  "Send instructions for ROOM and `conf-collaborative-pad' to NICK."
  (let ((room-url (emacsconf-get-room room)))
    (unless room-url (error "Please specify nick and room name"))
    (erc-message "PRIVMSG" (format "%s You can use this BBB room: %s . I'll join you there shortly to set up the room and do the last-minute tech check." nick room-url))
    (erc-message "PRIVMSG" (format "%s The collaborative pad we'll be using for questions is at %s . We'll collect questions from #emacsconf and put them there. If you'd like to jump to your part of the document, you might be able to keep an eye on questions. Alternatively, we can read questions to you." nick emacsconf-collaborative-pad))
    (erc-message "PRIVMSG" (format "%s The host will join and give you the go-ahead when it's time to present. See you in the BBB room!" nick))
    (setq talk (or talk (emacsconf-complete-talk)))
    (emacsconf-with-talk-heading talk
      (let ((talk-info (emacsconf-get-talk-info-for-subtree)))
        (org-entry-put (point) "BBB_ROOM" room-url)
        (when emacsconf-tasks-file
          (with-current-buffer (find-file-noselect emacsconf-tasks-file)
            (goto-char (point-min))
            (insert (emacsconf-replace-plist-in-string
                     (append info (list :current-time (format-time-string "[%Y-%m-%d %a %H:%M]")))
                     "* WAITING [#A] Check in ${nick} (${speakers}) for %s at %s
:PROPERTIES:
:CREATED: ${current-time}
:END:
[[file:~/vendor/emacsconf-wiki/playbook.org::#check-in][file:~/vendor/emacsconf-wiki/playbook.org::#check-in]]

"))))))))

(defun erc-cmd-READY (code &rest filter)
  "Notify #emacsconf-org and `conf-streaming-nick' that CODE is ready for the talk specified by FILTER.
FILTER can be the talk ID or strings to match against the title or speaker names."
  (let ((room-url (emacsconf-get-room code))
        (talk (emacsconf-find-talk-info filter))
        pronouns pronunciation)
    (unless room-url (error "Could not find room"))
    (unless talk (error "Could not find talk"))
    (emacsconf-with-talk-heading (plist-get talk :slug)
      (setq pronouns (org-entry-get (point) "PRONOUNS")
            pronunciation (org-entry-get (point) "PRONUNCIATION")))
    (with-current-buffer (erc-get-buffer "#emacsconf-org")
      (erc-send-message (format "Ready in Room %s: %s (%s)"
                                (upcase code)
                                (plist-get talk :title)
                                (plist-get talk :speakers))))
    (mapc (lambda (nick)
            (erc-message "PRIVMSG" 
                         (format "%s Ready in Room %s ( %s ): %s (%s) %s %s"
                                 emacsconf-nick
                                 (upcase code)
                                 room-url
                                 (plist-get talk :title)
                                 (plist-get talk :speakers)
                                 (or pronouns "")                                    
                                 (or pronunciation ""))))
          (if (listp emacsconf-streaming-nick) emacsconf-streaming-nick (list emacsconf-streaming-nick)))))


(defun erc-cmd-ANNOUNCE (filter)
  "Set the channel topics to announce the talk specified by FILTER.
    FILTER can be the talk ID or strings to match against the title or speaker names."
  (let ((info
         (if (listp filter)
             (car filter)
           (emacsconf-find-talk-info filter))))
    (unless info (error "Could not find talk."))
    (erc-cmd-CONFTOPIC (format "%s: %s (%s)"
                               (plist-get info :slug)
                               (plist-get info :title)
                               (plist-get info :speakers)))
    (erc-cmd-BROADCAST (format "---- %s (%s, https://emacsconf.org/%s/talks/%s) ----"
                               (plist-get info :title)
                               (plist-get info :speakers)
                               emacsconf-year
                               (plist-get info :slug)))))

(defun erc-cmd-OPALL ()
  (emacsconf-erc-with-channels (mapcar 'car emacsconf-topic-templates)
    (erc-cmd-OPME)))

(defun erc-cmd-BROADCAST (&rest message)
  "Say MESSAGE in all the emacsconference channels."
  (emacsconf-erc-with-channels (mapcar 'car emacsconf-topic-templates)
    (erc-send-message (s-join " " message))))

(defun erc-cmd-JUMP (talk)
  (emacsconf-go-to-talk talk))

(defun erc-cmd-SETPROP (property talk value)
  (save-window-excursion
    (emacsconf-with-talk-heading talk
      (org-entry-put (point) (upcase property) value))))

(defun erc-cmd-GETPROP (property talk)
  (save-window-excursion
    (emacsconf-with-talk-heading talk
      (message "%s" (org-entry-get (point) (upcase property))))))

(defun emacsconf-set-start-time-for-slug (slug time)
  (interactive (list (emacsconf-complete-talk) (read-string "Start: ")))
  (emacsconf-with-talk-heading slug
    (emacsconf-org-set-start-time (emacsconf-time-or-offset-to-time time))))

(defun emacsconf-org-set-qa-end (slug time)
  (interactive (list (emacsconf-complete-talk) (read-string "Q&A end: ")))
  (emacsconf-with-talk-heading slug
    (org-entry-put (point) "Q_AND_A_END"
                   (org-format-time-string (cdr org-time-stamp-formats) (emacsconf-time-or-offset-to-time time)))
    (let ((info (list :title (org-entry-get (point) "ITEM"))))
      (when emacsconf-tasks-file
        (with-current-buffer (find-file-noselect emacsconf-tasks-file)
          (goto-char (point-max))
          (insert (emacsconf-replace-plist-in-string info "\n* TODO [#C] Process Q&A for ${title}

")))))))

(defun emacsconf-end-current-talk (time)
  (interactive (read-string "Time: "))
  (with-current-buffer (find-file-noselect emacsconf-org-file)
    (org-map-entries
     (lambda ()
       (org-todo "DONE")
       (when (and (string-match "live" (org-entry-get (point) "Q_AND_A"))
                  (null (org-entry-get (point) "Q_AND_A_END")))
         (save-excursion
           (emacsconf-org-set-qa-end
            (org-entry-get (point) "SLUG")
            time)))
       (emacsconf-update-talk))
     "SLUG=.+TODO=\"STARTED\"")))

(defun emacsconf-announce (slug time)
  "Announce that the talk SLUG has started. Optionally, set the start time to TIME."
  (interactive (list (emacsconf-complete-talk) (read-string "Start: ")))
  ;; mark the previous talk done
  (emacsconf-end-current-talk time)
  ;; mark the current talk started
  (emacsconf-with-talk-heading slug
    (erc-cmd-ANNOUNCE (org-entry-get (point) "SLUG"))
    (emacsconf-org-set-start-time (emacsconf-time-or-offset-to-time time))))

(defun emacsconf-org-set-start-time (time)
  "Set the start time of the current entry to TIME.
TIME can be hh:mm or an offset such as -2 (two minutes ago) based on current time."
  (interactive "MStart: ")
  (let ((sched (org-timestamp-from-string (org-entry-get (point) "SCHEDULED")))
        info)
    (org-todo "STARTED")
    (org-set-property "SCHEDULED"
                      (org-format-time-string "%Y-%m-%d %H:%M" (emacsconf-time-or-offset-to-time time)))
    (org-entry-put (point) "FIXED_TIME" "1")
    (org-entry-put (point) "PUBLIC" "1")
    (setq info (emacsconf-get-talk-info-for-subtree))
    (setq info (append info
                       (list :video-description (emacsconf-video-description info)
                             :conf-year emacsconf-year
                             :conf-directory emacsconf-directory)))
    (emacsconf-update-talk)
    (when emacsconf-tasks-file
      (with-current-buffer (find-file-noselect emacsconf-tasks-file)
        (goto-char (point-max))
        (unless (re-search-backward "TODO.*Update schedule on wiki" nil t)
          (insert "\n* TODO [#B] Update schedule on wiki\n"))
        (goto-char (point-max))
        (insert (emacsconf-replace-plist-in-string info 
"\n* TODO [#B] Check ${title} on https://media.emacsconf.org/${conf-year}

,* TODO [#B] Commit the page update for ${title} and check https://emacsconf.org/${conf-year}/talks/${slug}/

[[elisp:(magit-status-setup-buffer \"${conf-directory}\"]]

,* TODO [#C] Publish ${title} on YouTube and ToobNix

${video-description}

"))))
    (emacsconf-publish-files)
    (emacsconf-update-media)))


(defun erc-cmd-START (talk &optional time)
  "Adjusts the start time and sets the FIXED_TIME property.
/FIXSTART news 9:03  - set it to start at that time
/FIXSTART news -2    - two minutes ago"
  (save-window-excursion
    (emacsconf-with-talk-heading talk
      (emacsconf-org-set-start-time time))))

(defun emacsconf-org-log-note (note)
  "Add NOTE to the current entry's logbook."
  (interactive "MNote: ")
  (setq org-log-note-window-configuration (current-window-configuration))
  (move-marker org-log-note-return-to (point))
  (move-marker org-log-note-marker (point))
  (setq org-log-note-purpose 'note)
  (with-temp-buffer
    (insert note)
    (org-store-log-note)))

(defun erc-cmd-CONFLOG (talk &rest notes)
  "Go to TALK and store NOTES in the :LOGBOOK:.
Usage: /conflog keyword notes go here"
  (save-window-excursion
    (emacsconf-with-talk-heading talk (emacsconf-org-log-note (s-join " " notes)))))

(defun erc-cmd-GIT (&optional location)
  (if (string= location "conf")
      (magit-status (file-name-directory emacsconf-org-file))
    (magit-status emacsconf-directory)))

(defun erc-cmd-CONF (&rest args)
  (cond
   ((string= (car args) "checkin")
    (apply 'erc-cmd-CHECKIN (cdr args)))
   ((string= (car args) "start")
    (apply 'erc-cmd-START (cdr args)))
   ((string= (car args) "git")
    (apply 'erc-cmd-GIT (cdr args)))
   ((string= (car args) "log")
    (apply 'erc-cmd-CONFLOG (cdr args)))
   (t (message "checkin start git log"))))

(provide 'emacsconf-erc)
;;; emacsconf-erc.el ends here
