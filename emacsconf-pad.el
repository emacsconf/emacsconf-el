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

;; Prepopulate the Etherpad: emacsconf-pad-prepopulate-all-talks
;;
;; Prepopulate shift hyperlists: emacsconf-pad-prepopulate-hyperlists
;; Prepopulate checkins: emacsconf-pad-prepopulate-checkins

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
	(assoc-default
	 'html
	 (assoc-default 'data
									(emacsconf-pad-json-request (format "%sapi/1/getHTML?apikey=%s&padID=%s"
																											emacsconf-pad-base
																											(url-hexify-string emacsconf-pad-api-key)
																											(url-hexify-string pad-id))
																							(called-interactively-p 'any)))))

(defun emacsconf-pad-set-html (pad-id html)
	"Set PAD-ID contents to the given HTML."
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
                 (pcase (plist-get o :q-and-a)
									 ('nil "<div>Q&amp;A: none</div>")
                   ((rx "live")
										(format "<div>Q&amp;A room: %s</div>" (plist-get o :bbb-redirect)))
									 ((rx "pad")
										(format "<div>Q&amp;A: Etherpad</div>"))
									 ((rx "irc")
										(format "<div>Q&amp;A: IRC (%s)</div>" (plist-get o :webchat-url)))
									 ((rx "after")
										(format "<div>Q&amp;A: after the event</div>"))
									 (_ "<div>Q&amp;A: none</div>"))
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
	"Fill in the pad for O."
  (interactive
	 (list
		(let ((info (emacsconf-include-next-talks (emacsconf-get-talk-info) emacsconf-pad-number-of-next-talks)))
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
       (goto-char (org-find-property "SLUG"
                                     (if (string-match "^[0-9]+-\\(.*\\)$" ,pad-id)
                                         (match-string 1 ,pad-id)
                                       "meta")))
       ,@body)))

(defun emacsconf-pad-modified-p (pad-id)
  (save-window-excursion
    (save-excursion
      (let ((cached-last-modified (emacsconf-pad-with-heading pad-id
																		(org-entry-get (point) "PAD_RESET")))
            (result (emacsconf-pad-get-last-edited pad-id)))
        (let-alist result
					(and cached-last-modified
							 (not (string= cached-last-modified
														 (number-to-string .data.lastEdited)))))))))

;;; Hyperlists

(defun emacsconf-pad-export-initial-content-for-hyperlists (dir &optional info)
  (interactive (list (read-file-name "Output directory: " nil nil nil nil 'file-directory-p)))
  (setq info (emacsconf-publish-prepare-for-display (emacsconf-get-talk-info)))
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
  (let* ((track (emacsconf-get-track (plist-get shift :track)))
         (prefixed (list
                    :start (plist-get shift :start)
                    :end (plist-get shift :end)
                    :base-url emacsconf-base-url
                    :year emacsconf-year
                    :ssh-port "46668"
                    :vnc-port (plist-get track :vnc-port)
                    :host (format "<em>%s</em>"
                                  (emacsconf-surround "HOST-" (plist-get shift :host) "" "HOST"))
                    :stream (format "<em>%s</em>"
                                    (emacsconf-surround "STREAM-" (plist-get shift :streamer) "" "STREAM"))
                    :irc-volunteer (format "<em>%s</em>" (emacsconf-surround "IRC-" (plist-get shift :irc) "" "IRC"))
                    :track-id (plist-get track :id)
                    :conf-id emacsconf-id
                    :checkin (format "<em>%s</em>" (emacsconf-surround "CHECKIN-" (plist-get shift :checkin) "" "CHECKIN"))
                    :pad (format "<em>%s</em>" (emacsconf-surround "PAD-" (plist-get shift :pad) "" "PAD"))
                    :coord (format "<em>%s</em>" (emacsconf-surround "COORD-" (plist-get shift :coord) "" "COORD"))
                    :checkin-pad (concat emacsconf-pad-base "checkin-" (downcase (format-time-string "%a" (date-to-time (plist-get shift :start)))))))
         (shift-talks
          (mapcar (lambda (o) (append prefixed o))
                  (seq-filter
                   (lambda (talk) (string= (plist-get talk :track) (plist-get shift :track)))
                   (emacsconf-filter-talks-by-time (plist-get shift :start) (plist-get shift :end) info)))))
    (concat
     (emacsconf-replace-plist-in-string
      (append (list :index (concat emacsconf-pad-base
                                   "private-"
                                   emacsconf-private-pad-prefix
                                   "-"
																	 emacsconf-year)
										:year emacsconf-year)
              shift)
      (concat
       "Back to ${index}<br />In case of ...: https://emacsconf.org/${year}/organizers-notebook/#exceptions<br />"
       "<h1><strong>" (plist-get shift :id) "</strong></h1>"
       "<p>Host: ${host}, Streamer: ${streamer}, IRC: ${irc}, Pad: ${pad}, Check-in: ${checkin}, Coord: ${coord}</p>"))
     (emacsconf-replace-plist-in-string
      prefixed
      (concat
       "
<p>Ctrl-5 is the shortcut for striking through on Etherpad.</p>
<p>Don't use this for notes since it gets overwritten.</p>

<strong>Setup</strong>
<ul>
<li>[ ] ${checkin}: Open ${checkin-pad}</li>
<li>[ ] ${irc-volunteer}: Watch the #emacsconf-${track-id} channel and open ${base-url}${year}/talks for links to the pads</li>
<li>[ ] ${pad}: Open ${base-url}${year}/talks for links to the pads</li>
<li>[ ] ${coord}: ssh orga@live0.emacsconf.org and run screen-fallbacks; confirm that the streams are showing fallbacks</li>
<li>[ ] ${stream}: Start recording with OBS
<li>[ ] Copy the password file if you don't already have it: <strong>scp emacsconf-${track-id}@res.emacsconf.org:~/.vnc/passwd vnc-passwd-${track-id} -p ${ssh-port}</strong></li>
<li>[ ] Forward your local ports: <strong>ssh emacsconf-${track-id}@res.emacsconf.org -N -L ${vnc-port}:127.0.0.1:${vnc-port} -p ${ssh-port} &</strong></li>
<li>[ ] Connect via VNC: <strong>xvncviewer 127.0.0.1:${vnc-port} -shared -geometry 1280x720 -passwd vnc-passwd-${track-id} &</strong>
<ul>
<li>[? Can't connect to VNC]: ssh emacsconf-${track-id}@res.emacsconf.org -p ${ssh-port} /home/${conf-id}-${track-id}/bin/track-vnc</li>
<li>[? Can't find OBS]: track-obs</li></ul></li>
<li>[ ] Start background music via SSH or VNC: <em>music</em>
<ul><li>[? No audio device]:
<ul><li><em>pulseaudio -k; pulseaudio --start</em></li>
<li>quit OBS</li>
<li><em>track-obs</em></li></ul></li>
<li>[ ] Start recording (not streaming). (Alt-2, switch to workspace 2; Alt-Shift-2, move something to workspace 2).</li>
<li>[ ] Watch the stream with MPV on your local system: <strong>mpv https://live0.emacsconf.org/emacsconf/${track-id}.webm &</strong></li>
<li>[ ] Check 480p by viewing it : <strong>mpv https://live0.emacsconf.org/emacsconf/${track-id}-480p.webm &</strong></li>
<li>[ ] Confirm that the streaming user has connected to Mumble, is in the ${channel} channel, and can hear what we say on Mumble.</li>
<li>[ ] Test with a sample video or Q&A session. You can run this command on your local system if you want to do things off-screen: <strong>ssh emacsconf-${track-id}@res.emacsconf.org -p 46668 \"~/bin/track-mpv emacsconf &\"</strong></li>
<li>[ ] ${stream}: Restart the background music via SSH or VNC: <em>music</em>  . The background music should automatically get killed when the talks start, but if it doesn't, you can stop it with: <em>screen -S background -X quit</em></li>
</ul></li>"
			 (if emacsconf-restream-youtube
					 "<li>[ ] ${coord}: ssh -t orga@live0.emacsconf.org 'screen -S restream-${track-id}-youtube /home/orga/restream-${track-id}-youtube.sh' and then confirm at ${youtube-url}</li>
" "")
			 (if emacsconf-restream-toobnix
					 "<li>[ ] ${coord}: ssh -t orga@live0.emacsconf.org 'screen -S restream-${track-id}-toobnix /home/orga/restream-${track-id}-toobnix.sh' and then confirm at ${toobnix-url}</li>"
				 "")
			 "
<li>[ ] ${coord}: update the status page on live.emacsconf.org by changing emacsconf-tracks and calling emacsconf-stream-update-status-page</li>
<li>${stream}: OR: <ul>
     <li>start emacs and use M-x emacsconf-stream-display-clock-and-countdown. time and message are optional</li>
     <li>display the in-between slide for the next talk</li></ul></li>
</ul>"
       "<strong>Talks</strong>
<ul>"
       (mapconcat
        (lambda (talk)
          (emacsconf-pad-talk-hyperlist
           (append prefixed talk)))
        (emacsconf-include-next-talks shift-talks 1)
        "\n")
       "</ul>"
       "Teardown
<ul>
<li>[ ] ${stream}: stop recording</li>
"
			 (if emacsconf-restream-youtube
					 "
<li>[ ] ${coord}: stop the restream-${track-id}-youtube screen on live0: <strong>screen -S restream-${track-id}-youtube -X quit</strong></li>
"
				 "")
			 (if emacsconf-restream-toobnix
					 "
<li>[ ] ${coord}: stop the restream-${track-id}-toobnix screen on live0: <strong>screen -S restream-${track-id}-toobnix -X quit</strong></li>
"
				 "")
"
<li>[ ] ${coord}: update the status page on live.emacsconf.org by changing emacsconf-tracks and calling emacsconf-stream-update-status-page</li>
</ul>"))
     )))

(defun emacsconf-pad-format-checkin-hyperlist (talk)
  (emacsconf-replace-plist-in-string
   (append (list
						:speakers (concat
											 "<strong>"
											 (plist-get talk :speakers)
											 (emacsconf-surround " (" (plist-get talk :irc) ")" "")
											 "</strong>")
            :time
            (format-time-string "%H:%M" (plist-get talk :checkin-time) emacsconf-timezone)
            :live
            (format-time-string "%H:%M" (plist-get talk :live-time) emacsconf-timezone)
            :start
            (format-time-string "%H:%M" (plist-get talk :start-time) emacsconf-timezone)
            :bbb-checklist
						(emacsconf-replace-plist-in-string
						 (list :backstage-url-with-password (emacsconf-backstage-url (plist-get talk :bbb-backstage))
									 :backstage-user emacsconf-backstage-user
									 :pronunciation
									 (concat (emacsconf-surround "" (plist-get talk :pronunciation) " or listen to " "Refer to ")
													 (format "%s%s/backstage/%s--intro.webm" emacsconf-media-base-url emacsconf-year (plist-get talk :file-prefix)))
									 :backstage-password emacsconf-backstage-password
									 :backstage-url (plist-get talk :bbb-backstage))
						 "<ul>
<li>Message for the speaker: Thanks for checking in! Your BigBlueButton web conference room is at ${backstage-url}. If you don't have the backstage username and password saved, let me know and I can send you a direct message with the info. Please join me there so that I can set you as a moderator and go through the preflight checklist with you.
<li>Direct message for the speaker if needed: Your BigBlueButton web conference room is at ${backstage-url-with-password}, or username \"${backstage-user}\" and password \"${backstage-password}\".</li>
<li>Pronunciation: ${pronunciation}</li>
<li>Checklist<ul>
<li>[ ] Speaker is set as a moderator</li>
<li>[ ] Speaker can be heard</li>
<li>[ ] Speaker can hear others</li>
<li>[ ] No audio feedback issues (may need headphones or earphones)</li>
<li>[ ] Screen sharing: (optional)
<ul><li>[ ] Window or screen can be shared
<li>[ ] Text is readable</li></ul>
<li>[ ] Webcam sharing (optional)</li></ul></li>
<li>OK to do other things until the prerec ends</li>
<li>People will add questions to the pad or IRC channel; host can read them to you, or you can read them</li>
<li>You can answer questions in any order, and you can skip questions if you want. Feel free to take your time to think about answers or to save some for following up later</li>
<li>Host and streamer will join after prerec ends and give you the signal when you're good to go</li>
<li>Please close any tabs or apps watching the stream when your Q&A starts (otherwise the audio is confusing)</li>
<li>It can take some time for people to think of questions, or you might have a quiet Q&A. Please feel free to demo other things you couldn't fit in your talk, or start with questions people might be thinking about.</li>
<li>We'll let you know when the stream is going to move on to the next talk. Even after the streamer switches over to the next talk, you can still stay and chat here for as long as you like. When you're done, you can end the meeting and leave.</li></ul>"))
           talk)
   (if (plist-get talk :video-file)
       (pcase (or (plist-get talk :q-and-a) "")
         ((rx "live")
          "<li>[ ] <strong>${time}</strong> Q&A: BBB: ${speakers} should be checked into ${bbb-backstage} and set as moderator(s) to go live at ${live} ${absolute-url}
${bbb-checklist}</li>")
         ((or (rx "IRC") (rx "pad"))
          "<li>[ ] <strong>${time}</strong> Q&A: IRC/pad: ${speakers} should be in #${channel} to go live at ${live} ${absolute-url}</li>")
         ((rx "Mumble")
          "<li>[ ] <strong>${time}</strong> Q&A: Mumble: ${speakers} should be in Mumble to go live at ${live} ${absolute-url}</li>")
         (_ ""))
     "<li>[ ] <strong>${time}</strong> LIVE: ${speakers} should be checked into ${bbb-backstage} and set as moderator(s) for ${absolute-url} to go live at ${start}${bbb-checklist}</li>")))

(defun emacsconf-pad-prepopulate-checkins (&optional info)
  (interactive)
  (setq info (or info (seq-filter (lambda (o) (plist-get o :email))
																	(emacsconf-publish-prepare-for-display (emacsconf-get-talk-info)))))
  (mapc
   (lambda (day)
     (let ((pad-id (concat "checkin-" (downcase (format-time-string "%a" (plist-get (cadr day) :checkin-time))))))
       (emacsconf-pad-create-pad pad-id)
       (emacsconf-pad-set-html
        pad-id
        (replace-regexp-in-string
         "<em>${checkin}:</em> " ""
         (concat
          (car day)
					"<p>If anyone's still missing by the specified time, please let us know in #emacsconf-org so we can call them.</p>"
					"<ul>"
          (mapconcat
           (lambda (talk)
             (emacsconf-pad-format-checkin-hyperlist talk))
           (seq-sort
            (lambda (a b)
              (time-less-p (plist-get a :checkin-time)
                           (plist-get b :checkin-time)))
            (seq-filter (lambda (talk) (plist-get talk :checkin-time)) day))
           "\n")
          "</ul>")))))
   (seq-group-by (lambda (talk)
                   (format-time-string "%A, %b %-e, %Y" (plist-get talk :checkin-time)))
                 info)))

(defun emacsconf-pad-prepopulate-index ()
	(interactive)
	(let ((id (format "private-%s-%s"
                    emacsconf-private-pad-prefix
										emacsconf-year)))
		(emacsconf-pad-create-pad id)
		(emacsconf-pad-set-html
		 id
		 (emacsconf-replace-plist-in-string
			(list
			 :base-url emacsconf-base-url
			 :year emacsconf-year
			 :checkin-list (mapconcat
											(lambda (day) (concat "<li>" emacsconf-pad-base "checkin-"
																						(downcase (format-time-string "%a" (plist-get (cadr day) :checkin-time)))
																						"</li>"))
											(seq-group-by (lambda (talk)
																			(format-time-string "%A, %b %-e, %Y" (plist-get talk :checkin-time)))
																		(seq-filter (lambda (o) (plist-get o :checkin-time))
																								(emacsconf-publish-prepare-for-display (emacsconf-get-talk-info))))
											"")
			 :shift-list (mapconcat
										(lambda (shift)
											(format "<li>%sprivate-%s-%s-%s</li>"
															emacsconf-pad-base
															emacsconf-private-pad-prefix
															emacsconf-year
															(plist-get shift :id)))
										emacsconf-shifts
										"")
			 :host-list
			 (mapconcat
				(lambda (shift)
					(format "<li>%shost-%s</li>"
									emacsconf-pad-base
									(plist-get shift :id)))
				emacsconf-shifts
				"")

			 )
			"<p>Things to do in case of... ${base-url}${year}/organizers-notebook/#exceptions</p>

<div>Checkin:
<ul>${checkin-list}</ul></div>
</div>

<div>Host-focused hyperlists:
<ul>${host-list}</ul></div>

<div>Combined shift info:
<ul>${shift-list}</ul></div>
"))))

(defun emacsconf-pad-prepopulate-shift-hyperlist (shift &optional info)
  (interactive (list (completing-read "Shift: "
                                      (mapcar (lambda (o) (plist-get o :id)) emacsconf-shifts))))
  (when (stringp shift)
    (setq shift (seq-find (lambda (o) (string= (plist-get o :id) shift)) emacsconf-shifts)))
  (unless info (setq info (emacsconf-publish-prepare-for-display (emacsconf-get-talk-info))))
  (let ((info (emacsconf-publish-prepare-for-display (emacsconf-get-talk-info)))
				(pad-id (format "private-%s-%s-%s"
                        emacsconf-private-pad-prefix
												emacsconf-year
                        (plist-get shift :id))))
    (emacsconf-pad-create-pad pad-id)
    (emacsconf-pad-set-html
     pad-id
     (emacsconf-pad-format-shift-hyperlist shift info))))

(defun emacsconf-pad-prepopulate-host-hyperlists ()
	(interactive)
	(mapc #'emacsconf-pad-prepopulate-shift-hyperlist-host emacsconf-shifts))

(defun emacsconf-pad-prepopulate-shift-hyperlist-host (shift &optional info)
  (interactive (list (completing-read "Shift: "
                                      (mapcar (lambda (o) (plist-get o :id)) emacsconf-shifts))))
  (when (stringp shift)
    (setq shift (seq-find (lambda (o) (string= (plist-get o :id) shift)) emacsconf-shifts)))
  (unless info (setq info (emacsconf-publish-prepare-for-display (emacsconf-get-talk-info))))
  (let ((info (emacsconf-publish-prepare-for-display (emacsconf-get-talk-info))))
    (let* ((pad-id (format "host-%s"
                           (plist-get shift :id)))
					 (shift-talks
						(seq-filter
						 (lambda (talk) (string= (plist-get talk :track) (plist-get shift :track)))
						 (emacsconf-filter-talks-by-time (plist-get shift :start) (plist-get shift :end) info))))
			(emacsconf-pad-create-pad pad-id)
			(emacsconf-pad-set-html
			 pad-id
			 (concat
				"
			 <p>Ctrl-5 is the shortcut for striking through on Etherpad.</p>
			 <p>Don't use this for notes since it gets overwritten.</p>

			 <strong>Setup:</strong>
			 <ul>
			 <li>Join Mumble</li>
			 <li>Join the IRC channel for your track (optional)</li>
			 </ul>
			 "
				"<strong>Talks</strong>
			 <ul>"
				(mapconcat
				 (lambda (talk)
					 (let ((next-talk (car (plist-get talk :next-talks))))
						 (emacsconf-replace-plist-in-string
							(append
							 (list :start-hhmm (format-time-string "%H:%M" (plist-get talk :start-time) emacsconf-timezone)
										 :expanded-intro (emacsconf-pad-expand-intro talk)
										 :intro-url (format "%s%s/backstage/%s--intro.webm" emacsconf-media-base-url emacsconf-year (plist-get talk :file-prefix))
										 :mumble (concat emacsconf-id "-" (plist-get (emacsconf-get-track talk) :id))
										 :open-qa (if emacsconf-qa-start-open ""
																"<li>Decide when to open the Q&A BBB up to everyone. Let ${coord} know.</li>")
										 :end-of-qa (if next-talk (format-time-string "%H:%M" (plist-get next-talk :start-time) emacsconf-timezone)
																	"end of shift")
										 :pronunciation
										 (concat (emacsconf-surround "" (plist-get talk :pronunciation) " or listen to " "Refer to ")
														 (format "%s%s/backstage/%s--intro.webm" emacsconf-media-base-url emacsconf-year (plist-get talk :file-prefix)))
										 :qa-hhmm (format-time-string "%H:%M" (plist-get talk :qa-time) emacsconf-timezone)
										 :hyperlist-note-info
										 (emacsconf-surround
											(format "<li><strong>%s NOTE for ${slug}:</strong> "
															(format-time-string "%H:%M" (plist-get talk :start-time) emacsconf-timezone))
											(plist-get talk :hyperlist-note) "</li>" "")
										 :next-talk-in-5 (if next-talk (format-time-string "%H:%M" (time-subtract (plist-get next-talk :start-time) (seconds-to-time 300)) emacsconf-timezone) "")
										 :next-talk-in-1 (if next-talk (format-time-string "%H:%M" (time-subtract (plist-get next-talk :start-time) (seconds-to-time 60)) emacsconf-timezone) ""))
							 talk)
							(concat
							 "${hyperlist-note-info}"
							 (cond
								(;; live talk, join BBB
								 (null (plist-get talk :video-file))
								 "<li><strong>${start-hhmm} ${slug} live talk</strong>: it should play a prerecorded intro, but if it doesn't, join ${bbb-backstage} and introduce talk, then turn it over to speaker for <strong>live talk</strong>: ${expanded-intro} (pronunciation: ${pronunciation})</li>")
								(t ;; prerecorded talk
								 "<li>Backup: ${start-hhmm} ${slug}: it should play a prerecorded intro and talk, but if it doesn't, join ${mumble} in Mumble and introduce talk: ${expanded-intro} (pronunciation: ${pronunciation})</li>"))
							 ;; Q&A
							 (if (and (null (plist-get talk :video-file)) (not (string= (or (plist-get talk :q-and-a) "none") "none")))
									 "<li>Continue in the BBB room for live Q&A because the talk was live</li>"
								 (pcase (plist-get talk :q-and-a)
									 ((or 'nil "" "none" (rx "after"))
										(if (plist-get talk :video-file)
												"<li>[ ] ${qa-hhmm} ${slug} Q&A after: Join ${mumble} in Mumble and say that the speaker will follow up with answers on the talk page afterwards. Read questions. ${pad-url}</li>"
											""))
									 ((rx "IRC")
										"<li>[ ] ${qa-hhmm} ${slug} Q&A IRC: Join ${mumble} in Mumble. Invite people to put their questions in the ${channel} IRC channel and read questions and answers from there. ${webchat-url} ${pad-url}</li>")
									 ((rx "pad")
										"<li>[ ] <strong>${qa-hhmm}</strong> ${slug} Q&A pad: Join ${mumble} in Mumble. Invite people to put their questions in the Etherpad and read questions and answers from there. ${pad-url}</li>")
									 ((rx "Mumble")
										"<li>[ ] <strong>${qa-hhmm}</strong> ${slug} Q&A mumble: Join ${mumble} in Mumble. Bring the speaker into the right channel if needed. Invite people to put their questions in the Etherpad and read questions and answers from there. ${pad-url} Paste questions into Mumble chat or read them out loud.</li>")
									 ((rx "live")
										(concat
										 "<li>[ ] <strong>${qa-hhmm} ${slug} Q&A live</strong> (on stream until ${end-of-qa}): Join ${bbb-backstage}. START RECORDING. Invite people to put their questions in the Etherpad, and read questions from there. ${pad-url}</li>
			 ${open-qa}
"
										(if next-talk
												"
<li><strong>${next-talk-in-5}</strong> [? Open Q&A is still going on and it's about five minutes before the next talk]
  <ul><li>[ ] Let the speaker know about the time and that the Q&A can continue off-stream if people want to join</li></ul></li>
<li><strong>${next-talk-in-1}</strong> [? Open Q&A is still going on and it's about a minute before the next talk]
  <ul><li>[ ] Announce that the Q&A will continue if people want to join the BBB room from the talk page, and the stream will now move to the next talk</li></ul></li>
			 "
											""))
)))))))
				 (emacsconf-include-next-talks shift-talks 1)
				 "\n")
				"</ul>")))))

(defun emacsconf-pad-prepopulate-shift-hyperlists ()
  (interactive)
  (let ((info (emacsconf-publish-prepare-for-display (emacsconf-get-talk-info))))
    (mapc (lambda (shift)
            (emacsconf-pad-prepopulate-shift-hyperlist shift info))
          emacsconf-shifts)))

(defun emacsconf-pad-prepopulate-hyperlists ()
	(interactive)
	(emacsconf-pad-prepopulate-shift-hyperlists)
	(emacsconf-pad-prepopulate-checkins)
	(emacsconf-pad-prepopulate-host-hyperlists))

(defun emacsconf-pad-expand-intro (talk)
	"Make an intro for TALK."
  (cond
   ((null (plist-get talk :speakers))
    (format "Next, we have \"%s\"." (plist-get talk :title)))
   ((plist-get talk :intro-note)
    (plist-get talk :intro-note))
   (t
    (let ((pronoun (pcase (plist-get talk :pronouns)
                     ((rx "she") "She")
										 ((rx "\"ou\"" "Ou"))
                     ((or 'nil "nil" (rx string-start "he") (rx "him")) "He")
                     ((rx "they") "They")
                     (_ (or (plist-get talk :pronouns) "")))))
      (format "Next, we have \"%s\", by %s%s.%s"
							(plist-get talk :title)
              (replace-regexp-in-string ", \\([^,]+\\)$"
                                        ", and \\1"
                                        (plist-get talk :speakers))
							(emacsconf-surround " (" (plist-get talk :pronunciation) ")" "")
              (pcase (plist-get talk :q-and-a)
                ((or 'nil "") "")
                ((rx "after") " You can ask questions via Etherpad and IRC. We'll send them to the speaker, and we'll post the answers on the talk page afterwards.")
                ((rx "live")
                 (format " %s will answer questions via BigBlueButton. You can join using the URL from the talk page or ask questions through Etherpad or IRC."
                         pronoun
                         ))
								((rx "pad")
                 (format " %s will answer questions via Etherpad."
                         pronoun
                         ))
								((rx "IRC")
                 (format " %s will answer questions via IRC in the #%s channel."
                         pronoun
                         (plist-get talk :channel)))))))))

;; Related: emacsconf-talk-hyperlist
(defun emacsconf-pad-talk-hyperlist (talk &optional do-insert)
  (interactive (list (emacsconf-complete-talk-info) t))
  (let* ((track-id (plist-get (emacsconf-get-track talk) :id))
         (next-talk (car (plist-get talk :next-talks)))
         (modified-talk
          (apply
           #'append
           (list
            :year emacsconf-year
            :track-id track-id
            :intro-note
            (emacsconf-pad-expand-intro talk)
            :media-base emacsconf-media-base-url
            :mumble (concat emacsconf-id "-" track-id)
            :next-talk-in-5 (if next-talk (format-time-string "%H:%M" (time-subtract (plist-get next-talk :start-time) (seconds-to-time 300)) emacsconf-timezone) "")
            :next-talk-in-1 (if next-talk (format-time-string "%H:%M" (time-subtract (plist-get next-talk :start-time) (seconds-to-time 60)) emacsconf-timezone) "")
						:qa-start (format-time-string "%H:%M" (plist-get talk :qa-time) emacsconf-timezone)
						:qa-end (if next-talk (format-time-string "%H:%M" (plist-get next-talk :start-time))
											"end of shift")
            :ssh  "ssh orga@res.emacsconf.org -p 46668 "
            :ssh-track (format "ssh %s-%s@res.emacsconf.org -p 46668 " emacsconf-id track-id)
            :ssh-audio (format "ex: ssh emacsconf-%s@res.emacsconf.org -p 46668 \"qa-vol 85%%\" (or qa-louder, qa-quieter, mum-vol, mum-louder, mum-quieter)" track-id))
           talk
           (mapcar (lambda (status)
                     (list (intern (concat ":ssh-" (replace-regexp-in-string "_" "" (downcase status))))
                           (format "<strong>ssh orga@res.emacsconf.org -p 46668 \"talk %s %s\"</strong>"
                                   (plist-get talk :slug)
                                   status)))
                   '("PLAYING" "OPEN_Q" "CLOSED_Q"))))
         result)
		(unless (plist-get talk :recorded-intro)
			(error "Unplanned scenario for %s - no recorded intro?"
						 (plist-get talk :slug)))
    (setq result
          (emacsconf-replace-plist-in-string
           modified-talk
           (format "<li><strong>%s %s (intro: %s, talk: %s, Q&A: %s) %s <a href=\"%s\">%s</a></strong><ul>%s</ul>\n</li>"
                   (format-time-string "%H:%M" (plist-get talk :start-time) emacsconf-timezone)
                   (plist-get talk :slug)
                   (if (plist-get talk :recorded-intro) "recorded" "live")
                   (if (plist-get talk :video-file) "recorded" "live")
                   (or (plist-get talk :q-and-a) "none")
                   (plist-get talk :title)
                   (plist-get talk :absolute-url)
                   (plist-get talk :absolute-url)

                   (concat
                    (emacsconf-surround "<li><strong>" (plist-get talk :hyperlist-note) "</strong></li>" "")
										"<li>Recorded intro: <a href=\"${media-base}${year}/backstage/${file-prefix}--intro.webm\">${media-base}${year}/backstage/${file-prefix}--intro.webm</a>"
										(if (plist-get talk :video-file)
												"<li>[ ] [? stream didn't auto-play] ${stream}: <em>handle-session ${slug}</em>; if that doesn't work, <em>play ${slug}</em>; if that still doesn't work, <em>track-mpv ~/current/cache/${conf-id}-${year}-${slug}*--intro.webm</em> and <em>track-mpv ~/current/cache/${conf-id}-${year}-${slug}*--main.webm</em></li>"
											(concat
                       "<li>Live talk:<ul>"
                       "<li>[ ] [? stream didn't auto-join] ${stream}: <a href=\"${bbb-backstage}\">${bbb-backstage}</a></li>"
                       "<li>[ ] ${host}: Join <a href=\"${bbb-backstage}\">${bbb-backstage}</a> and turn over to speaker.</li></ul></li>"))
                    (pcase (or (plist-get talk :q-and-a) "")
                      ((rx "live")
                       (concat
                        "<li>Live Q&A start ${qa-start}, on stream until ${qa-end}<ul>
<li>[ ] ${host}: Join the Q&A room at <a href=\"${bbb-backstage}\">${bbb-backstage}</a> and open the pad at <a href=\"${pad-url}\">${pad-url}</a>; optionally open IRC for ${channel} (<a href=\"${webchat-url}\">${webchat-url}</a>)</li>
<li>[ ] [? speaker missing?] ${host}: Let #emacsconf-org know so that we can text or call the speaker</li>
<li>[ ] [? stream didn't auto-join?] ${stream}: <em>bbb ${slug}</em>
<ul>
<li>Backup URL for BBB: <a href=\"${bbb-backstage}\">${bbb-backstage}</a></li>
<li>Backup URL for pad: <a href=\"${pad-url}\">${pad-url}</a></li>
</ul>
</li>
<li>[ ] ${stream}: Give the host the go-ahead via Mumble or #emacsconf-org</li>
<li>[ ] ${host}: Start recording and read questions</li>
<li>[ ] ${stream}: Adjust the audio levels as needed: ${ssh-audio}</li>
"
												(if emacsconf-qa-start-open
														""
													"<li>[ ] ${host}: Decide when to open the Q&A and let ${stream} know</li>
<li>[ ] ${stream}: Update the task status (no visible changes): ${ssh-openq}</li>")
												"
<li>[ ] ${stream}: Confirm BBB redirect at <a href=\"${bbb-redirect}\">${bbb-redirect}</a> goes to BBB room, let host know</li>
<li>[ ] ${host}: Announce that people can join using the URL on the talk page or ask questions on the pad or IRC channel</li>
<li>${next-talk-in-5} [? Open Q&A is still going on and it's about five minutes before the next talk]
  <ul><li>[ ] ${host}: Let the speaker know about the time and that the Q&A can continue off-stream if people want to join</li></ul></li>
<li>${next-talk-in-1} [? Open Q&A is still going on and it's about a minute before the next talk]
  <ul><li>[ ] ${host}: Announce that the Q&A will continue if people want to join the BBB room from the talk page, and the stream will now move to the next talk</li></ul></li>
<li>[? Q&A is done early]
  <ul>
  <li>${stream}: OR: <ul>
     <li>start emacs and use M-x emacsconf-stream-display-clock-and-countdown. time and message are optional</li>
     <li>display the in-between slide for the next talk</li></ul></li>
</ul></li></ul></li>"))
                      ((rx "irc")
                       "
<li>[ ] ${stream}: Update the task status, which should open the pad and IRC; arrange windows: ${ssh-closedq}
<ul><li>Backup link to pad: <a href=\"${pad-url}\">${pad-url}</a></li>
<li>Backup link to #${channel}: <a href=\"${webchat-url}\">${webchat-url}</a></li></ul></li>
<li>[ ] ${stream}: Update the task status (no visible changes): ${ssh-openq}</li>
<li>[ ] ${host}: Announce that people can ask questions in the ${channel} IRC channel.</li>
")
                      ((rx "Mumble")
                       "
<li>[ ] ${stream}: Bring the speaker's Mumble login over to the ${channel} channel in Mumble. Confirm that Mumble is audible and adjust audio as needed: ssh emacsconf-${track-id}@res.emacsconf.org -p 46668 \"mum-vol 85%%\" (or mum-louder, mum-quieter)</li>
<li>[ ] ${stream}: Mark the Q&A as closed: ${ssh-closedq} . This should display the QA slide (backup: ${ssh-track} and run <em>firefox ${qa-slide-url} &</em>)</li>
<li>[ ] ${stream}: Update the task status (no visible changes): ${ssh-openq}</li>

<li>[ ] ${host}: Announce that people can ask questions in the pad or on the ${channel} IRC channel.</li>
")
                      ((rx "after")
                       "
<li>[ ] ${stream}: Update the task status: ${ssh-closedq} # this should open the pad and IRC; arrange the windows
<ul><li>Backup link to pad: ${pad-url}</li>
<li>Backup link to #${channel}: ${webchat-url}</li></ul></li>
<li>[ ] ${host}: Announce that people can ask questions in the pad or on the ${channel} IRC channel, and that the speaker will follow up later.</li>
<li>[ ] ${stream}: Update the task status: ${ssh-openq} # this should not make any visible changes, just update the task status</li>"
                       )
											((rx "pad")
											 "<li>[ ] [? pad didn't auto-open] ${stream}: ${pad-url}</li>")
                      (_
                       "<li>[ ] ${stream}: Open the IRC channel (${channel}) and the pad, and arrange the windows: ${ssh-closedq}</li>
"))))))
    (if do-insert (insert result))
    result))

(defun emacsconf-pad-prepopulate-intros ()
  (interactive)
  (emacsconf-pad-create-pad "intros")
  (emacsconf-pad-set-html
   "intros"
   (concat "<p>https://media.emacsconf.org/" emacsconf-year "/backstage/</p>
This page is for easy reference and recording. Please make sure any changes here are reflected in the INTRO_NOTE property of the talk.
<ul>"
           (mapconcat
            (lambda (o)
              (emacsconf-replace-plist-in-string
               (append (list :intro-note (emacsconf-pad-expand-intro o)) o)
               "<li>${slug} - ${track}: ${title} (${speakers-with-pronouns}, Q&amp;A: ${q-and-a})<ul><li>${absolute-url}</li><li>Intro: ${intro-note}</li></ul></li>"))
            (emacsconf-publish-prepare-for-display (emacsconf-get-talk-info)))
           "</ul>")))

(defun emacsconf-pad-backup-talk (talk)
	(interactive (list (emacsconf-complete-talk-info)))
	(with-temp-file (expand-file-name (concat (plist-get talk :file-prefix) "--pad.html")
																		emacsconf-cache-dir)
		(insert
		 (emacsconf-pad-get-html (emacsconf-pad-id talk))))
	(call-process "pandoc" nil nil nil "-o"
								(expand-file-name (concat (plist-get talk :file-prefix) "--pad.md")
																	emacsconf-cache-dir)
								(expand-file-name (concat (plist-get talk :file-prefix) "--pad.html")
																	emacsconf-cache-dir)))

(defun emacsconf-pad-backup-talks ()
	(interactive)
	(mapc
	 #'emacsconf-pad-backup-talk
	 (emacsconf-publish-prepare-for-display (emacsconf-get-talk-info))))
(provide 'emacsconf-pad)
;;; emacsconf-pad.el ends here
