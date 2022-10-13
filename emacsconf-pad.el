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

(defcustom emacsconf-pad-base "http://pad.emacsconf.org/"
  "Base URL for the Etherpad. Include trailing slash."
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

(defun emacsconf-pad-id (o)
  (concat emacsconf-pad-slug-base "-" (plist-get o :slug)))

(defun emacsconf-pad-url (o)
  (concat emacsconf-pad-base emacsconf-pad-directory (emacsconf-pad-id o)))

(defvar emacsconf-pad-number-of-next-talks 3 "Integer limiting the number of next talks to link to from the pad.")

(defun emacsconf-pad-initial-content (o)
  (emacsconf-replace-plist-in-string
      (append (list :base-url emacsconf-base-url
                    :channel (concat "emacsconf-" (downcase (plist-get o :track)))
                    :bbb-info
                    (if (plist-get o :bbb-room)
                        (concat "<div>Q&amp;A room: ${bbb-room}</div>")
                      "")
                    :next-talk-list
                    (if (plist-get o :next-talks)
                        (concat "<div>Next talks:\n<ul>"
                                (mapconcat
                                 (lambda (o)
                                   (format "<li>%s: %s %s</li>"
                                           (plist-get o :track)
                                           (emacsconf-pad-url o)
                                           (plist-get o :title)))
                                 (plist-get o :next-talks)
                                 "\n")
                                "</ul></div>")
                      "")
                    :irc-nick-details
                    (if (plist-get o :irc)
                        (concat "Speaker nick: " (plist-get o :irc) " - ")
                      "")
                    :irc-url (concat "https://chat.emacsconf.org/#/connect?join=emacsconf-" (downcase (plist-get o :track))))
              o)
      "<div>
<div><strong>${title}</strong></div>
<div>${base-url}${url} - ${speakers} - Track: ${track}</div>
${bbb-info}
<div>IRC: ${irc-nick-details}<a href=\"${irc-url}\">${irc-url}</a> or #${channel} on libera.chat network</div>
<div>How to watch/participate: ${base-url}${year}</div>
<div>Guidelines for conduct: ${base-url}conduct</div>
<div>See end of file for license (CC Attribution-ShareAlike 4.0 + GPLv3 or later)</div>
<div>----------------------------------------------------------------</div>
<div>Questions and discussion go here:</div>
<ul><li>Q1. </li><li>Q2. </li><li>Q3. </li><li>Q4. </li><li>Q5. </li><li>Q6. </li><li></li><li></li><li></li></ul>
<div>----------------------------------------------------------------</div>
${next-talk-list}
<div>This pad will be archived at ${base-url}${url} after the conference.</div>
<div>Except where otherwise noted, the material on the EmacsConf pad are dual-licensed under the terms of the Creative Commons Attribution-ShareAlike 4.0 International Public License; and the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) an later version. Copies of these two licenses are included in the EmacsConf wiki repository, in the COPYING.GPL and COPYING.CC-BY-SA files (https://emacsconf.org/COPYING/)</div>

<div>By contributing to this pad, you agree to make your contributions available under the above licenses. You are also promising that you are the author of your changes, or that you copied them from a work in the public domain or a work released under a free license that is compatible with the above two licenses. DO NOT SUBMIT COPYRIGHTED WORK WITHOUT PERMISSION.</div></div>"))

(defun emacsconf-pad-prepopulate-talk-pad (o)
  (interactive (list (let ((info (emacsconf-include-next-talks (emacsconf-get-talk-info) emacsconf-pad-number-of-next-talks)))
                       (emacsconf-complete-talk-info info))))
  (let ((pad-id (emacsconf-pad-id o)))
    (emacsconf-pad-create-pad pad-id)
    (emacsconf-pad-set-html
     pad-id
     (emacsconf-pad-initial-content o))))

(defun emacsconf-pad-prepopulate-all-talks (&optional info)
  (interactive)
  (mapc #'emacsconf-pad-prepopulate-talk-pad (emacsconf-include-next-talks (or info (emacsconf-get-talk-info)) emacsconf-pad-number-of-next-talks)))

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

(provide 'emacsconf-pad)
;;; emacsconf-pad.el ends here
