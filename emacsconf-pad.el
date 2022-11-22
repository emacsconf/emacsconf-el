;;; emacsconf-pad.el --- Working with Etherpad       -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Sacha Chua

;; Author: Sacha Chua <sacha@sachachua.com>
;; Keywords: convenience

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

;; Prepopulate the Etherpad

(defcustom emacsconf-pad-base "https://pad.emacsconf.org/"
  "Base URL for the Etherpad. Include trailing slash.
Use \"wikimedia\" to use etherpad.wikimedia.org instead."
  :group 'emacsconf
  :type 'string)

(defcustom emacsconf-pad-api-key nil
  "API key for authenticating against the Etherpad.
You can find it in $ETHERPAD_PATH/APIKEY.txt"
  :group 'emacsconf
  :type 'string)

(defcustom emacsconf-pad-slug-base (concat emacsconf-year)
  "Base for the pad IDs"
  :group 'emacsconf
  :type 'string)

(defcustom emacsconf-pad-directory ""
  "Set to /p/ if you don't have friendly URLs set up in the proxy."
  :group 'emacsconf
  :type 'string)

;;; Code:
(defun emacsconf-pad-delete-pad (pad-id)
  (interactive "MPad ID: ")
  (switch-to-buffer (url-retrieve-synchronously (format "%sapi/1/deletePad?apikey=%s&padID=%s" emacsconf-pad-base emacsconf-pad-api-key pad-id))))

(defun emacsconf-pad-json-request (url &optional display-result)
  (with-current-buffer (url-retrieve-synchronously url)
    (goto-char (point-min))
    (re-search-forward "^$")
    (let ((result (json-read)))
      (when display-result (prin1 result))
      (kill-buffer)
      result)))

(defun emacsconf-pad-create-pad (pad-id)
  (interactive "MPad ID: ")
  (emacsconf-pad-json-request (format "%sapi/1/createPad?apikey=%s&padID=%s" emacsconf-pad-base
                                      (url-hexify-string emacsconf-pad-api-key)
                                      (url-hexify-string pad-id))
                              (called-interactively-p 'any)))

(defun emacsconf-pad-delete-pad (pad-id)
  (interactive "MPad ID: ")
  (emacsconf-pad-json-request (format "%sapi/1/deletePad?apikey=%s&padID=%s" emacsconf-pad-base
                                      (url-hexify-string emacsconf-pad-api-key)
                                      (url-hexify-string pad-id))
                              (called-interactively-p 'any)))

(defun emacsconf-pad-get-text (pad-id)
  (interactive "MPad ID: ")
  (emacsconf-pad-json-request (format "%sapi/1/getText?apikey=%s&padID=%s" emacsconf-pad-base
                                      (url-hexify-string emacsconf-pad-api-key)
                                      (url-hexify-string pad-id))
                              (called-interactively-p 'any)))

(defun emacsconf-pad-set-text (pad-id text)
  (interactive "MPad ID: \nMText: ")
  (emacsconf-pad-json-request (format "%sapi/1/setText?apikey=%s&padID=%s&text=%s"
                                      emacsconf-pad-base
                                      (url-hexify-string emacsconf-pad-api-key)
                                      (url-hexify-string pad-id)
                                      (url-hexify-string text))
                              (called-interactively-p 'any)))

(defun emacsconf-pad-append-text (pad-id text)
  (interactive "MPad ID: \nMText: ")
  (emacsconf-pad-json-request (format "%sapi/1/appendText?apikey=%s&padID=%s&text=%s"
                                      emacsconf-pad-base
                                      (url-hexify-string emacsconf-pad-api-key)
                                      (url-hexify-string pad-id)
                                      (url-hexify-string text))
                              (called-interactively-p 'any)))

(defun emacsconf-pad-get-html (pad-id)
  (interactive "MPad ID: ")
  (emacsconf-pad-json-request (format "%sapi/1/getHTML?apikey=%s&padID=%s"
                                      emacsconf-pad-base
                                      (url-hexify-string emacsconf-pad-api-key)
                                      (url-hexify-string pad-id))
                              (called-interactively-p 'any)))

(defun emacsconf-pad-set-html (pad-id html)
  (interactive "MPad ID: \nMHTML: ")
  (let ((url-request-data (concat "html=" (url-hexify-string html)))
        (url-request-method "POST")
        (url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded"))))
    (emacsconf-pad-json-request (format "%sapi/1/setHTML?apikey=%s&padID=%s"
                                        emacsconf-pad-base
                                        (url-hexify-string emacsconf-pad-api-key)
                                        (url-hexify-string pad-id))
                                (called-interactively-p 'any))))
;; (emacsconf-pad-append-text "emacsconf-2022-journalism" "Hello again")
;; (emacsconf-pad-get-html "emacsconf-2022-journalism")
;; (emacsconf-pad-set-html "emacsconf-2022-journalism" "<div><strong>Hello</strong> world</div>")

(defun emacsconf-pad-get-last-edited (pad-id)
  (interactive "MPad ID: ")
  (emacsconf-pad-json-request (format "%sapi/1/getLastEdited?apikey=%s&padID=%s"
                                      emacsconf-pad-base
                                      (url-hexify-string emacsconf-pad-api-key)
                                      (url-hexify-string pad-id))
                              (called-interactively-p 'any)))

(defun emacsconf-pad-id (o)
  (concat emacsconf-pad-slug-base "-" (plist-get o :slug)))

(defun emacsconf-pad-url (o)
  (if (string= emacsconf-pad-base "wikimedia")
      (format "https://etherpad.wikimedia.org/p/emacsconf-%s-%s"
              emacsconf-year
              (plist-get o :slug))
    (concat emacsconf-pad-base emacsconf-pad-directory (emacsconf-pad-id o))))

(defvar emacsconf-pad-number-of-next-talks 3 "Integer limiting the number of next talks to link to from the pad.")

(defun emacsconf-pad-initial-content (o)
  (emacsconf-replace-plist-in-string
   (append (list :base-url emacsconf-base-url
                 :channel (concat "emacsconf-" (plist-get (emacsconf-get-track (plist-get o :track)) :id))
                 :bbb-info
                 (cond
                  ((null (plist-get o :q-and-a))
                   "<div>Q&amp;A: none</div>")
                  ((string-match "live" (plist-get o :q-and-a))
                   (format "<div>Q&amp;A room: %s</div>" (plist-get o :bbb-redirect)))
                  (t "<div>Q&amp;A: IRC</div>"))
                 :next-talk-list
                 (if (plist-get o :next-talks)
                     (concat "<div>Next talks:\n<ul>"
                             (mapconcat
                              (lambda (o)
                                (format "<li>%s: %s %s</li>"
                                        (plist-get o :track)
                                        (plist-get o :title)
                                        (emacsconf-pad-url o)))
                              (plist-get o :next-talks)
                              "\n")
                             "</ul></div>")
                   "")
                 :track-id
                 (plist-get (emacsconf-get-track (plist-get o :track)) :id)
                 :watch
                 (concat emacsconf-base-url emacsconf-year "/watch/" (plist-get (emacsconf-get-track (plist-get o :track)) :id) "/")
                 :talks
                 (concat emacsconf-base-url emacsconf-year "/talks/")
                 :notes
                 (string-join (make-list 6 "<li></li>"))
                 :questions
                 (string-join (make-list 6 "<li>Q: <ul><li>A: </li></ul></li>"))
                 :conf-pad-url
                 (concat "https://pad.emacsconf.org/" emacsconf-year)
                 :irc-nick-details
                 (if (plist-get o :irc)
                     (concat "Speaker nick: " (plist-get o :irc) " - ")
                   "")
                 :irc-url (concat "" ))
           o)
   "<div>
<div>All talks: ${talks}</div>
<div><strong>${title}</strong></div>
<div>${base-url}${url} - ${speakers} - Track: ${track}</div>
<div>Watch/participate: ${watch}</div>
${bbb-info}
<div>IRC: ${irc-nick-details} https://chat.emacsconf.org/#/connect?join=emacsconf,emacsconf-${track-id} or #emacsconf-${track-id} on libera.chat network</div>
<div>Guidelines for conduct: ${base-url}conduct</div>
<div>See end of file for license (CC Attribution-ShareAlike 4.0 + GPLv3 or later)</div>
<div>----------------------------------------------------------------</div>
<div>Notes, discussions, links, feedback:</div>
<ul>${notes}</ul>
<div>----------------------------------------------------------------</div>
<div>Questions and answers go here:</div>
<ul>${questions}</ul>
<div>----------------------------------------------------------------</div>
${next-talk-list}
<div>Questions/comments related to EmacsConf ${year} as a whole? ${conf-pad-url}
<div>----------------------------------------------------------------</div>
<div>This pad will be archived at ${base-url}${url} after the conference.</div>
<div>Except where otherwise noted, the material on the EmacsConf pad are dual-licensed under the terms of the Creative Commons Attribution-ShareAlike 4.0 International Public License; and the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) an later version. Copies of these two licenses are included in the EmacsConf wiki repository, in the COPYING.GPL and COPYING.CC-BY-SA files (https://emacsconf.org/COPYING/)</div>

<div>By contributing to this pad, you agree to make your contributions available under the above licenses. You are also promising that you are the author of your changes, or that you copied them from a work in the public domain or a work released under a free license that is compatible with the above two licenses. DO NOT SUBMIT COPYRIGHTED WORK WITHOUT PERMISSION.</div></div>"))

(defun emacsconf-pad-prepopulate-main-pad ()
  (interactive)
  (let ((pad-id emacsconf-year))
    (emacsconf-pad-create-pad pad-id)
    (emacsconf-pad-set-html
     pad-id
     (emacsconf-replace-plist-in-string
      (list
       :base-url emacsconf-base-url
       :notes (string-join (make-list 6 "<li></li>"))
       :questions
       (string-join (make-list 6 "<li>Q: <ul><li>A: </li></ul></li>"))
       :year emacsconf-year)
      "<div><strong>EmacsConf ${year}</strong> - this pad is for general conference-related questions and feedback</div>
<div>All talks: ${base-url}${year}/talks - see specific talk pages for links to their Etherpads</div>
<div>Conference hallway IRC channel: https://chat.emacsconf.org/#/connect?join=emacsconf or #emacsconf on libera.chat network</div>
<div>Organizers channel: https://chat.emacsconf.org/#/connect?join=emacsconf,emacsconf-org or #emacsconf-org on libera.chat network</div>
<div>Guidelines for conduct: ${base-url}conduct</div>
<div>See end of file for license (CC Attribution-ShareAlike 4.0 + GPLv3 or later)</div>
<div>----------------------------------------------------------------</div>
<div>Notes, discussions, links, feedback: </div>
<ul>${notes}</ul>
<div>----------------------------------------------------------------</div>
<div>Questions and answers go here:</div>
<ul>${questions}</ul>
<div>----------------------------------------------------------------</div>
<div>This pad will be archived at ${base-url}${url} after the conference.</div>
<div>Except where otherwise noted, the material on the EmacsConf pad are dual-licensed under the terms of the Creative Commons Attribution-ShareAlike 4.0 International Public License; and the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) an later version. Copies of these two licenses are included in the EmacsConf wiki repository, in the COPYING.GPL and COPYING.CC-BY-SA files (https://emacsconf.org/COPYING/)</div>

<div>By contributing to this pad, you agree to make your contributions available under the above licenses. You are also promising that you are the author of your changes, or that you copied them from a work in the public domain or a work released under a free license that is compatible with the above two licenses. DO NOT SUBMIT COPYRIGHTED WORK WITHOUT PERMISSION.</div></div>"))))

(defvar emacsconf-pad-force-all nil "Set to t to clear everything.")
(defun emacsconf-pad-prepopulate-talk-pad (o)
  (interactive (list (let ((info (emacsconf-include-next-talks (emacsconf-get-talk-info) emacsconf-pad-number-of-next-talks)))
                       (emacsconf-complete-talk-info info))))
  (let ((pad-id (emacsconf-pad-id o)))
    (emacsconf-pad-create-pad pad-id)
    (when (or emacsconf-pad-force-all
              (not (emacsconf-pad-modified-p pad-id))
              (progn
                (browse-url (emacsconf-pad-url o))
                (y-or-n-p (format "%s might have been modified. Reset? " (plist-get o :slug)))))
      (emacsconf-pad-set-html
       pad-id
       (emacsconf-pad-initial-content o))
      (save-window-excursion
        (emacsconf-with-talk-heading (plist-get o :slug)
          (let-alist (emacsconf-pad-get-last-edited pad-id)
            (org-entry-put (point) "PAD_RESET" (number-to-string .data.lastEdited))))))))

(defun emacsconf-pad-prepopulate-all-talks (&optional info)
  (interactive)
  (mapc #'emacsconf-pad-prepopulate-talk-pad
          (emacsconf-include-next-talks (or info (emacsconf-get-talk-info)) emacsconf-pad-number-of-next-talks)))

(defun emacsconf-pad-export-initial-content (o file)
  (interactive
   (list (let ((info (emacsconf-include-next-talks (emacsconf-get-talk-info) emacsconf-pad-number-of-next-talks)))
           (emacsconf-complete-talk-info info))
         (read-file-name "Output file: ")))
  (when (file-directory-p file) (setq file (expand-file-name (concat (plist-get o :slug) ".html") file)))
  (with-temp-file file
    (insert (emacsconf-pad-initial-content o))))

(defun emacsconf-pad-export-initial-content-for-all-talks (dir &optional info)
  (interactive (list (read-file-name "Output directory: " nil nil nil nil 'file-directory-p)))
  (setq info (emacsconf-include-next-talks (or info (emacsconf-get-talk-info))
                                           emacsconf-pad-number-of-next-talks))
  (mapcar (lambda (o)
            (emacsconf-pad-export-initial-content o dir))
          (emacsconf-active-talks (emacsconf-filter-talks info))))

(defmacro emacsconf-pad-with-heading (pad-id &rest body)
  (declare (indent 1) (debug t))
  `(progn
     (with-current-buffer (find-file emacsconf-org-file)
       (goto-char (org-find-property "CUSTOM_ID"
                                     (if (string-match "^[0-9]+-\\(.*\\)$" ,pad-id)
                                         (match-string 1 ,pad-id)
                                       "meta")))
       ,@body)))

(defun emacsconf-pad-modified-p (pad-id)
  (save-window-excursion
    (save-excursion
      (let ((cached-last-modified (emacsconf-pad-with-heading pad-id (org-entry-get (point) "PAD_RESET")))
            (result (emacsconf-pad-get-last-edited pad-id)))
        (let-alist result
          (not (string= cached-last-modified
                        (number-to-string .data.lastEdited))))))))

(defun emacsconf-pad-export-initial-content-for-hyperlists (dir &optional info)
  (interactive (list (read-file-name "Output directory: " nil nil nil nil 'file-directory-p)))
  (setq info (emacsconf-prepare-for-display (emacsconf-get-talk-info)))
  (unless (file-directory-p dir)
    (make-directory dir))
  (mapc
   (lambda (shift)
     (with-temp-file (expand-file-name (concat (plist-get shift :id) ".html") dir)
       (insert (emacsconf-pad-format-shift-hyperlist
                (append
                 (list
                  :year
                  emacsconf-year
                  :track-id
                  (plist-get
                   (emacsconf-get-track (plist-get shift :track))
                   :id)))
                shift info))))
   emacsconf-shifts))
;; (emacsconf-pad-export-initial-content-for-hyperlists "/ssh:media:~/backstage/hyperlists")

(defun emacsconf-pad-format-shift-hyperlist (shift info)
  (let* ((prefixed (list
                   :start (plist-get shift :start)
                   :end (plist-get shift :end)
                   :host (emacsconf-surround "HOST-" (plist-get shift :host) "" "HOST")
                   :stream (emacsconf-surround "STREAM-" (plist-get shift :streamer) "" "STREAM")
                   :irc-volunteer (emacsconf-surround "IRC-" (plist-get shift :irc) "" "IRC")
                   :checkin (emacsconf-surround "CHECKIN-" (plist-get shift :checkin) "" "CHECKIN")
                   :pad (emacsconf-surround "PAD-" (plist-get shift :pad) "" "PAD")
                   :coord (emacsconf-surround "COORD-" (plist-get shift :coord) "" "COORD")))
        (shift-talks
         (mapcar (lambda (o) (append prefixed o))
                 (seq-filter
                  (lambda (talk) (string= (plist-get talk :track) (plist-get shift :track)))
                  (emacsconf-filter-talks-by-time (plist-get shift :start) (plist-get shift :end) info)))))
    (concat
     (emacsconf-replace-plist-in-string
      shift
      (concat
       "<h1>" (plist-get shift :id) "</h1>"
       "<p>Host: ${host}, Streamer: ${streamer}, IRC: ${irc}, Pad: ${pad}, Check-in: ${checkin}, Coord: ${coord}</p>"))
     (emacsconf-replace-plist-in-string
      prefixed
      (concat
       "
<strong>Setup</strong>
<ul><li>[ ] ${checkin}: Open the index: https://media.emacsconf.org/${year}/backstage/index-${track-id}.html</li>
<li>[ ] ${host}: Open the intro pad and the index: https://media.emacsconf.org/${year}/backstage/index-${track-id}.html</li>
<li>[ ] ${irc}: Watch the #emacsconf-${track-id} channel</li>
<li>[ ] ${pad}: Open the index: https://media.emacsconf.org/${year}/backstage/index-${track-id}.html</li>
<li>[ ] ${streamer}: Start streaming with OBS
<ul><li>[ ] Set up the local environment
<ul><li>[? gen] export TRACK=gen; export TRACK_PORT=5905; export SSH_PORT=46668</li>
<li>[? dev] export TRACK=dev; export TRACK_PORT=5906; export SSH_PORT=46668</li></ul></li>
<li>[ ] Copy the password file: scp emacsconf-$TRACK@res.emacsconf.org:~/.vnc/passwd vnc-passwd-$TRACK -p $SSH_PORT</li>
<li>[ ] Forward your local ports: ssh emacsconf-$TRACK@res.emacsconf.org -N -L $TRACK_PORT:127.0.0.1:$TRACK_PORT -p $SSH_PORT &</li>
<li>[ ] Connect via VNC: xvncviewer 127.0.0.1:$TRACK_PORT -shared -geometry 1280x720 -passwd vnc-passwd-$TRACK &
<ul>
<li>[? Can't connect to VNC]: ssh emacsconf-$TRACK@res.emacsconf.org -p $SSH_PORT /home/emacsconf-$TRACK/bin/track-vnc</li>
<li>[? Can't find OBS]: track-obs</li></ul></li>
<li>[ ] Start recording (not streaming). (Alt-2, switch to workspace 2; Alt-Shift-2, move something to workspace 2).</li>
<li>[ ] Watch the stream with MPV on your local system: mpv https://live0.emacsconf.org/emacsconf/$TRACK.webm &</li>
<li>[ ] Check 480p: mpv https://live0.emacsconf.org/emacsconf/$TRACK-480p.webm &</li>
<li>[ ] Test with a sample video or Q&A session: ssh emacsconf-$TRACK@res.emacsconf.org -p 46668 \"~/bin/track-mpv meetups &\"</li>
</ul></li>
<li>[ ] ${coord}: ssh -t orga@live0.emacsconf.org 'screen -S restream-${track-id}-youtube /home/orga/restream-${track-id}-youtube.sh' and then confirm</li>
<li>[ ] ${coord}: ssh -t orga@live0.emacsconf.org 'screen -S restream-${track-id}-toobnix /home/orga/restream-${track-id}-toobnix.sh' and then confirm</li>
<li>[ ] ${coord}: update the status page on live.emacsconf.org</li>
</ul>
"
       "<ul>"
       (mapconcat #'cdr
                  (sort
                   (append
                    (delq nil
                          (mapcar (lambda (talk)
                                    (when (plist-get talk :checkin-time)
                                      (emacsconf-replace-plist-in-string
                                       (append (list :checkin-time-info
                                                     (format-time-string "%-l:%M" (plist-get talk :checkin-time) emacsconf-timezone))
                                               talk)
                                       "<li>${checkin}: Double-check that the speaker for ${title} has checked in; let #emacsconf-org know if not</li>")))
                                  shift-talks))
                    (mapcar
                     ;; talks
                     (lambda (talk)
                       (cons (plist-get talk :start-time)
                             (emacsconf-pad-talk-hyperlist
                              (append prefixed talk))))
                     shift-talks))
                   (lambda (a b) (time-less-p (car a) (car b))))
                  "\n")
       "</ul>"
       "Teardown
<ul>
<li>[ ] ${coord}: stop the restream-${track-id}-youtube screen on live0</li>
<li>[ ] ${coord}: stop the restream-${track-id}-toobnix screen on live0</li>
<li>[ ] ${coord}: update the status page on live.emacsconf.org</li>
</ul>"))
     )))

(defun emacsconf-pad-prepopulate-hyperlists ()
  (interactive)
  (let ((info (emacsconf-prepare-for-display (emacsconf-get-talk-info))))
    (mapc (lambda (shift)
            (let ((pad-id (format "private_%s_%s"
                                  emacsconf-private-pad-prefix
                                  (plist-get shift :id))))
              (emacsconf-pad-create-pad pad-id)
              (emacsconf-pad-set-html
               pad-id
               (emacsconf-pad-format-shift-hyperlist shift info))))
          emacsconf-shifts)))

;; Related: emacsconf-talk-hyperlist
(defun emacsconf-pad-talk-hyperlist (talk &optional do-insert)
  (interactive (list (emacsconf-complete-talk-info) t))
  (let* ((track-id (plist-get (emacsconf-get-track talk) :id))
         (modified-talk
          (apply
           #'append
           (list
            :track-id track-id
            :ssh  "ssh orga@res.emacsconf.org -p 46668 "
            :ssh-audio (format "ex: ssh emacsconf-%s@res.emacsconf.org -p 46668 \"%s-vol 85%%\" (or %s-louder, %s-quieter)" track-id track-id track-id track-id))
           talk
           (mapcar (lambda (status)
                     (list (intern (concat ":ssh-" (replace-regexp-in-string "_" "" (downcase status))))
                           (format "ssh orga@res.emacsconf.org -p 46668 \"~/scripts/update-task-status.sh %s . %s\""
                                   (plist-get talk :slug)
                                   status)))
                   '("PLAYING" "OPEN_Q" "CLOSED_Q"))))
         (result
          (emacsconf-replace-plist-in-string
           modified-talk
           (format "<li><strong>%s %s %s %s <a href=\"%s%s\">%s%s</a></strong>\n%s</li>"
                   (format-time-string "%H:%M" (plist-get talk :start-time) emacsconf-timezone)
                   (plist-get talk :track)
                   (plist-get talk :slug)
                   (plist-get talk :title)
                   emacsconf-base-url
                   (plist-get talk :url)
                   emacsconf-base-url
                   (plist-get talk :url)
                   (pcase (or (plist-get talk :q-and-a) "")
                     ((rx "live")
                      "<ul>
<li>[ ] ${checkin}: Check ${speakers-with-pronouns} (${irc}) into ${bbb-room} sometime beforehand and make them a moderator</li>
<li>[ ] ${stream}: Display the in-between slide <a href=\"https://media.emacsconf.org/${year}/in-between/${slug}.png\">https://media.emacsconf.org/${year}/in-between/${slug}.png</a></li>
<li>[ ] ${host}: Connect to the ${track} channel in Mumble and introduce the talk</li>
<li>[ ] ${stream}: Start playing the talk: ${ssh-playing}</li>
<li>[ ] ${host}: Join the Q&A room at <a href=\"${bbb-room}\">${bbb-room}</a> and open the pad at <a href=\"${pad-url}\">${pad-url}</a>; optionally open IRC for ${channel} (<a href=\"${webchat-url}\">${webchat-url}</a>)</li>
<li>[ ] [? speaker missing?] ${host}: Let #emacsconf-org know so that we can text or call the speaker</li>
<li>[ ] ${stream}: After the talk, open the Q&A window and the pad: ${ssh-closedq} </li>
<li>[ ] ${stream}: Give the host the go-ahead via Mumble or #emacsconf-org</li>
<li>[ ] ${host}: Start recording and read questions</li>
<li>[ ] ${stream}: Adjust the audio levels as needed: ${ssh-audio}</li>
<li>[ ] ${host}: Decide when to open the Q&A and let the streamer know</li>
<li>[ ] ${stream}: Mark the Q&A as open: ${ssh-openq}</li>
<li>[ ] ${host}: Announce that people can join using the URL on the talk page</li>
<li>[? Open Q&A is still going on and it's about five minutes before the next talk]
  <ul><li>[ ] ${host}: Let the speaker know about the time and that the Q&A can continue off-stream if people want to join</li></ul></li>
<li>[? Open Q&A is still going on and it's about two minutes before the next talk]
  <ul><li>[ ] ${host}: Announce that the Q&A will continue if people want to join the BBB room from the talk page, and the stream will now move to the next talk</li></ul></li>
<li>[? Q&A is done early]
  <ul>
  <li>[ ] ${stream}: Mark the talk as archived: ${ssh} \"~/current/scripts/update-task-status.sh ${slug} . TO_ARCHIVE\"</li>
</ul>
<li>[ ] ${stream}: Close the Q&A windows and move on to the next talk</li></ul>
")
                     ((rx "irc")
                      "
<ul><li>[ ] ${stream}: Display the in-between slide <a href=\"https://media.emacsconf.org/${year}/in-between/${slug}.png\">https://media.emacsconf.org/${year}/in-between/${slug}.png</a></li>
<li>[ ] ${host}: Connect to the ${track} channel in Mumble and introduce the talk</li>
<li>[ ] ${stream}: ${ssh-playing}</li>
<li>[ ] ${stream}: Open the IRC channel (${channel}) and the pad, and arrange the windows: ${ssh-closedq}</li>
<li>[ ] ${stream}: When it's time for the next talk, close the Q&A windows and move on to the next talk</li></ul>
")
                     (_
                      "<ul><li>[ ] ${stream}: Display the in-between slide <a href=\"https://media.emacsconf.org/${year}/in-between/${slug}.png\">https://media.emacsconf.org/${year}/in-between/${slug}.png</a></li>
<li>[ ] ${host}: Connect to the ${track} channel in Mumble and introduce the talk</li>
<li>[ ] ${stream}: Start the talk: ${ssh-playing}</li>
<li>[ ] ${stream}: Open the IRC channel (${channel}) and the pad, and arrange the windows: ${ssh-closedq}</li>
<li>[ ] ${stream}: When it's time for the next talk, close the Q&A windows and move on to the next talk</li>
</ul>
"))
                   nil nil 1))))
    (if do-insert (insert result))
    result))

(defun emacsconf-pad-prepopulate-intros ()
  (interactive)
  (emacsconf-pad-create-pad "intros")
  (emacsconf-pad-set-html
   "intros"
   (concat "<p>https://media.emacsconf.org/2022/backstage/</p><ul>"
           (mapconcat
            (lambda (o)
              (emacsconf-replace-plist-in-string
               (append (list :full-url (concat emacsconf-base-url (plist-get o :url))) o)
               "<li>${slug} - ${track}: ${title} (${speakers-with-pronouns}, Q&amp;A: ${q-and-a})<ul><li>${full-url}</li><li>Intro: ${intro-note}</li></ul></li>"))
            (emacsconf-prepare-for-display (emacsconf-get-talk-info)))
           "</ul>")))
(provide 'emacsconf-pad)
;;; emacsconf-pad.el ends here
