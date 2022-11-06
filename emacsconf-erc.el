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
;;
;; Goals:
;; - Announce talks and Q&A sessions
;; - Change talk status and trigger hooks
;;
;; Commands:
;;
;; announcements only
;; - /nowplaying slug
;; - /nowclosedq slug
;; - /nowopenq slug
;; - /nowunstreamedq slug
;; - /nowdone slug

;; use M-x emacsconf-erc-add-to-todo-hook in conf.org to have the announcements triggered by todo state changes
;; 
;; updating task status
;; - /markplaying slug
;; - /markclosedq slug
;; - /markopenq slug
;; - /markunstreamedq slug
;; - /markdone slug
;;
;; general
;; - /broadcast message
;; - /conftopic message
;; - /checkin nick
;; 
;;; Code:

(defcustom emacsconf-collaborative-pad
  (concat "https://etherpad.wikimedia.org/p/emacsconf-" emacsconf-year)
  "URL for the pad that will have questions."
  :group 'emacsconf
  :type 'string)

(defcustom emacsconf-streaming-nick nil "Set to the host's IRC nick in order to notify them."
  :group 'emacsconf
  :type 'string)

(defcustom emacsconf-erc-hallway "#emacsconf" "Channel for hallway conversations")
(defcustom emacsconf-erc-org "#emacsconf-org" "Channel for organizers")

(defcustom emacsconf-topic-templates
  '(("#emacsconf" "${emacsconf-name} ${year} | Subscribe to https://lists.gnu.org/mailman/listinfo/emacsconf-discuss for updates")
    ("#emacsconf-gen" "General track | https://emacsconf.org/2022/watch/gen/ | Subscribe to https://lists.gnu.org/mailman/listinfo/emacsconf-discuss for updates")
    ("#emacsconf-dev" "Development track | https://emacsconf.org/2022/watch/dev/ | Subscribe to https://lists.gnu.org/mailman/listinfo/emacsconf-discuss for updates")
    ("#emacsconf-accessible" "EmacsConf 2022 accessibility - help by describing what's happening | Subscribe to https://lists.gnu.org/mailman/listinfo/emacsconf-discuss for updates")
    ("#emacsconf-org" "EmacsConf 2022 | Dedicated channel for EmacsConf organizers and speakers | this is intended as an internal, low-traffic channel; for main discussion around EmacsConf, please join #emacsconf | Subscribe to https://lists.gnu.org/mailman/listinfo/emacsconf-discuss for updates")
    ("#emacsconf-questions" "EmacsConf 2022 | Low-traffic channel for questions if speakers prefer IRC and need help focusing; for main discussion around EmacsConf, please join #emacsconf | Subscribe to https://lists.gnu.org/mailman/listinfo/emacsconf-discuss for updates"))
  "List of (channel topic-template) entries for mass-setting channel topics."
  :group 'emacsconf
  :type '(repeat (list (string :tag "Channel")
                       (string :tag "Topic suffix"))))

;; For testing: (setq emacsconf-topic-templates '(("#emacsconf-test" "EmacsConf 2022 | Dedicated channel for EmacsConf organizers and speakers | this is intended as an internal, low-traffic channel; for main discussion around EmacsConf, please join #emacsconf | Subscribe to https://lists.gnu.org/mailman/listinfo/emacsconf-discuss for updates")))

(defmacro emacsconf-erc-with-channels (channel-list &rest forms)
  (declare (indent 1) (debug (form form body)))
  `(mapcar (lambda (channel)
             (unless (erc-get-buffer channel)
               (erc-join-channel channel))
             (with-current-buffer (erc-get-buffer channel)
               ,@forms))
           ,channel-list))

(defun erc-cmd-CONFTOPIC (&rest message)
  "Set the topic to MESSAGE | template in the EmacsConf channels.
If MESSAGE is not specified, reset the topic to the template."
  (mapc (lambda (template) 
          (with-current-buffer (erc-get-buffer (car template))
            (erc-cmd-TOPIC (if message (concat (if (stringp message) message (s-join " " message)) " | " (cadr template))
                             (cadr template)))))
        emacsconf-topic-templates))

(defun erc-cmd-CHECKIN (nick)
  (let* ((talk (emacsconf-complete-talk-info))
         (q-and-a (plist-get talk :q-and-a) ""))
    (save-excursion
      (emacsconf-with-talk-heading (plist-get talk :slug)
        (org-entry-put (point) "IRC" nick)
        (org-entry-put (point) "CHECK_IN" (format-time-string "%H:%M"))))
    (cond
     ((string-match "live" q-and-a)
      (erc-send-message (format "%s: Thanks for checking in! I'll send you some private messages with instructions, so please check there. Let me know if you don't get them." nick))
      (erc-cmd-ROOM nick talk))
     ((string-match "pad" q-and-a)
      (erc-send-message (format "%s: Thanks for checking in! The collaborative pad we'll be using for questions is at %s . We'll collect questions and put them there. If you'd like to open it, you can keep  an eye on questions. Please let us know if you need help, or if you want to switch to live Q&A." nick (plist-get talk :pad-url))))
     ((string-match "IRC" q-and-a)
      (erc-send-message (format "#%s: Thanks for checking in! Feel free to keep an eye on %s for questions and discussion, and we'll copy things from the pad to there. If the volume gets overwhelming, let us know and we can /msg you questions or add them to the pad. If you'd like to try Q&A over live video or the collaborative pad instead, or if you need help, please let us know." nick
                                (plist-get (emacsconf-get-track (plist-get talk :track)) :channel))))
     (t (erc-send-message (format "%s: Thanks for checking in! How would you like to handle Q&A today - live video, the collaborative Etherpad at %s , or IRC (like this)?" nick (plist-get) emacsconf-collaborative-pad))))
    (when (functionp 'emacsconf-upcoming-insert-or-update)
      (emacsconf-with-talk-heading (plist-get talk :slug)
        (emacsconf-upcoming-insert-or-update nil t)))))

(defun erc-cmd-BBB (nick &optional talk)
  "Send live Q&A instructions to NICK."
  (setq talk (or talk (emacsconf-complete-talk-info)))
  (erc-message "PRIVMSG" (format "%s You can use this BBB room: %s . We'll join you there shortly to set up the room and do the last-minute tech check." nick (plist-get talk :bbb-room)))
  (erc-message "PRIVMSG" (format "%s The collaborative pad we'll be using for questions is at %s . We'll collect questions from #emacsconf and put them there. If you'd like to jump to your part of the document, you might be able to keep an eye on questions. Alternatively, we can read questions to you." nick (plist-get talk :pad-url)))
  (erc-message "PRIVMSG" (format "%s The host will join and give you the go-ahead when you go on air. See you in the BBB room!" nick)))

(defun erc-cmd-READY (&rest filter)
  "Notify #emacsconf-org and `emacsconf-streaming-nick' that the speaker is ready."
  (let ((talk (emacsconf-find-talk-info filter))
        pronunciation)
    (unless talk (error "Could not find talk"))
    (setq pronunciation (plist-get talk :pronunciation))
    (emacsconf-erc-with-channels (list emacsconf-erc-org)
      (erc-send-message (format "Ready: %s (%s)" (plist-get talk :title) (plist-get talk :speakers))))
    (mapc (lambda (nick)
            (erc-message "PRIVMSG" 
                         (format "%s ready in %s: %s (%s) %s %s"
                                 emacsconf-nick
                                 (plist-get talk :bbb-room)
                                 (plist-get talk :title)
                                 (plist-get talk :speakers-with-pronouns)
                                 (or pronunciation ""))))
          (if (listp emacsconf-streaming-nick) emacsconf-streaming-nick (list emacsconf-streaming-nick)))))

;;; Announcements

(defun erc-cmd-NOWPLAYING (talk)
  "Set the channel topics to announce TALK."
  (interactive (list (emacsconf-complete-talk-info)))
  (when (stringp talk) (setq talk (or (emacsconf-find-talk-info talk) (error "Could not find talk %s" talk))))
  ;; Announce it in the track's channel
  (when (plist-get talk :track)
    (emacsconf-erc-with-channels (list (concat "#" (plist-get talk :channel)))
      (erc-cmd-TOPIC (format "%s: %s (%s) pad: %s Q&A: %s | %s"
                             (plist-get talk :slug)
                             (plist-get talk :title)                               
                             (plist-get talk :speakers)
                             (plist-get talk :pad-url)
                             (plist-get talk :qa-info)
                             (car (assoc-default (concat "#" (plist-get talk :channel)) emacsconf-topic-templates))))
      (erc-send-message (format "---- %s: %s - %s ----"
                                (plist-get talk :slug)
                                (plist-get talk :title)
                                (plist-get talk :speakers-with-pronouns)))
      (erc-send-message (concat "Add your notes/questions to the pad: " (plist-get talk :pad-url)))
      (cond
       ((string-match "live" (or (plist-get talk :q-and-a) ""))
        (erc-send-message (concat "Live Q&A: " (plist-get talk :bbb-redirect))))
       ((plist-get talk :irc)
        (erc-send-message "or discuss the talk on IRC (nick: %s)")))))
  ;; Short announcement in #emacsconf
  (emacsconf-erc-with-channels (list emacsconf-erc-hallway emacsconf-erc-org)
    (erc-send-message (format "-- %s track: %s: %s (watch: %s, pad: %s, channel: #%s)"
                              (plist-get talk :track)
                              (plist-get talk :slug)
                              (plist-get talk :title)                               
                              (plist-get talk :watch-url)
                              (plist-get talk :pad-url)
                              (plist-get talk :channel)))))

(defun erc-cmd-NOWCLOSEDQ (talk)
  "Announce TALK has started Q&A, but the host has not yet opened it up."
  (interactive (list (emacsconf-complete-talk-info)))
  (when (stringp talk) (setq talk (or (emacsconf-find-talk-info talk) (error "Could not find talk %s" talk))))
  (emacsconf-erc-with-channels (list (concat "#" (plist-get talk :channel)))
    (erc-send-message (format "-- Q&A beginning for \"%s\" (%s) Watch: %s Add notes/questions: %s"
                              (plist-get talk :title)
                              (plist-get talk :qa-info)
                              (plist-get talk :watch-url)
                              (plist-get talk :pad-url))))  
  (emacsconf-erc-with-channels (list emacsconf-erc-hallway emacsconf-erc-org)
    (erc-send-message (format "-- Q&A beginning for \"%s\" in the %s track (%s) Watch: %s Add notes/questions: %s . Chat: #%s"
                         (plist-get talk :title)
                         (plist-get talk :track)
                         (plist-get talk :qa-info)
                         (plist-get talk :watch-url)
                         (plist-get talk :pad-url)
                         (plist-get talk :channel)))))

(defun erc-cmd-NOWOPENQ (talk)
  (interactive (list (emacsconf-complete-talk-info)))
  (when (stringp talk) (setq talk (or (emacsconf-find-talk-info talk) (error "Could not find talk %s" talk))))
  (emacsconf-erc-with-channels (list (concat "#" (plist-get talk :channel)))
    (erc-send-message (format "-- Q&A now open for \"%s\" (%s). Watch: %s Add notes/questions: %s ."
                         (plist-get talk :title)
                         (plist-get talk :qa-info)
                         (plist-get talk :watch-url)
                         (plist-get talk :pad-url))))  
  (emacsconf-erc-with-channels (list emacsconf-erc-hallway emacsconf-erc-org)
    (erc-send-message (format "-- Q&A now open for \"%s\" in the %s track (%s). Watch: %s Add notes/questions: %s IRC: #%s"
                         (plist-get talk :title)
                         (plist-get talk :track)
                         (plist-get talk :qa-info)
                         (plist-get talk :watch-url)
                         (plist-get talk :pad-url)
                         (plist-get talk :channel)))))

(defun erc-cmd-NOWUNSTREAMEDQ (talk)
  (interactive (list (emacsconf-complete-talk-info)))
  (when (stringp talk) (setq talk (or (emacsconf-find-talk-info talk) (error "Could not find talk %s" talk))))
  (emacsconf-erc-with-channels (list (concat "#" (plist-get talk :channel)))
    (erc-send-message (format "-- Q&A continues off-stream for \"%s\" (%s) Add notes/questions: %s ."
                              (plist-get talk :title)
                              (plist-get talk :qa-info)
                              (plist-get talk :pad-url))))  
  (emacsconf-erc-with-channels (list emacsconf-erc-hallway emacsconf-erc-org)
    (erc-send-message (format "-- Q&A continues off-stream for \"%s\" in the %s track (%s) Add notes/questions: %s IRC: #%s"
                              (plist-get talk :title)
                              (plist-get talk :track)
                              (plist-get talk :qa-info)
                              (plist-get talk :pad-url)
                              (concat "#" (plist-get talk :channel))))))

(defun erc-cmd-NOWDONE (talk)
  (interactive (list (emacsconf-complete-talk-info)))
  (when (stringp talk) (setq talk (or (emacsconf-find-talk-info talk) (error "Could not find talk %s" talk))))
  (emacsconf-erc-with-channels (list (concat "#" (plist-get talk :channel)))
    (erc-send-message (format "-- Q&A finished for \"%s\". Add notes/questions: %s %s"
                              (plist-get talk :title)
                              (plist-get talk :pad-url)
                              (emacsconf-surround " Speaker IRC nick: " (plist-get talk :irc) "" ""))))  
  (emacsconf-erc-with-channels (list emacsconf-erc-hallway emacsconf-erc-org)
    (erc-send-message (format "-- Q&A finished for \"%s\" in the %s track. Add notes/questions: %s %s"
                              (plist-get talk :title)
                              (plist-get talk :track)
                              (plist-get talk :pad-url)
                              (emacsconf-surround " Speaker IRC nick: " (plist-get talk :irc) "" "")))))

;;; For todo hooks

(defun emacsconf-erc-announce-on-change (talk)
  "Announce talk."
  (let ((func
         (pcase org-state
           ("PLAYING" #'erc-cmd-NOWPLAYING)
           ("CLOSED_Q" #'erc-cmd-NOWCLOSEDQ)
           ("OPEN_Q" #'erc-cmd-NOWOPENQ)
           ("UNSTREAMED_Q" #'erc-cmd-NOWUNSTREAMEDQ)
           ("TO_ARCHIVE" #'erc-cmd-NOWDONE))))
    (when func
      (funcall func talk))))

;;; Change TODO states
(defun erc-cmd-MARKPLAYING (talk)
  "Mark TALK as starting streaming."
  (interactive (list (emacsconf-complete-talk-info)))
  (when (stringp talk) (setq talk (emacsconf-find-talk-info talk)))
  (save-window-excursion
    (emacsconf-with-talk-heading talk (org-todo "PLAYING"))))

(defun erc-cmd-MARKCLOSEDQ (talk)
  "Mark TALK as starting closed Q&A."
  (interactive (list (emacsconf-complete-talk-info)))
  (when (stringp talk) (setq talk (emacsconf-find-talk-info talk)))
  (save-window-excursion
    (emacsconf-with-talk-heading talk (org-todo "CLOSED_Q"))))

(defun erc-cmd-MARKOPENQ (talk)
  "Mark TALK as starting open Q&A."
  (interactive (list (emacsconf-complete-talk-info)))
  (when (stringp talk) (setq talk (emacsconf-find-talk-info talk)))
  (save-window-excursion
    (emacsconf-with-talk-heading talk (org-todo "OPEN_Q"))))

(defun erc-cmd-MARKDONE (talk)
  "Mark TALK as done with the streaming and Q&A"
  (interactive (list (emacsconf-complete-talk-info)))
  (when (stringp talk) (setq talk (emacsconf-find-talk-info talk)))
  (save-window-excursion
    (emacsconf-with-talk-heading talk (org-todo "TO_ARCHIVE"))))

;;; Other commands

(defun erc-cmd-OPME ()
  "Request chanserv to op me."
  (erc-message "PRIVMSG"
	       (format "chanserv op %s %s"
		       (erc-default-target)
		       (erc-current-nick)) nil))

(defun erc-cmd-DEOPME ()
  "Deop myself from current channel."
  (erc-cmd-DEOP (format "%s" (erc-current-nick))))

(defun erc-cmd-OPALL (&optional nick)
  (emacsconf-erc-with-channels (mapcar 'car emacsconf-topic-templates)
    (if nick
	(erc-cmd-OP nick)
      (erc-cmd-OPME))))

(defun erc-cmd-BROADCAST (&rest message)
  "Say MESSAGE in all the emacsconference channels."
  (emacsconf-erc-with-channels (mapcar 'car emacsconf-topic-templates)
    (erc-send-message (string-join message " "))))

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
    (emacsconf-with-talk-heading talk (emacsconf-org-log-note (string-join notes " ")))))

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
