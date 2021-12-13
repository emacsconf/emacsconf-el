;;; emacsconf-upcoming.el --- Update upcoming.org with information about the next talks  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Sacha Chua

;; Author: Sacha Chua <sacha@sachachua.com>
;; Keywords: data

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

(defun emacsconf-upcoming-update-file ()
  (interactive)
  (with-current-buffer (find-file-noselect emacsconf-upcoming-file)
    (save-excursion
      (org-map-entries #'emacsconf-upcoming-insert-or-update "SLUG={.}")
)))

(defun emacsconf-upcoming-add-subtree ()
  (interactive)
  (org-map-entries #'emacsconf-upcoming-insert-or-update "SLUG={.}" 'tree)
  (save-excursion
    (with-current-buffer (find-file-noselect emacsconf-upcoming-file)
      (emacsconf-upcoming-sort))))

(defun emacsconf-upcoming-sort ()
  (interactive)
  ;; Sort
  (goto-char (point-min))
  (when (looking-at "\\*") (save-excursion (insert "\n"))) ;; Need to be before the first heading.
  (org-sort-entries
   nil ?f
   (lambda () (cons (org-entry-is-done-p) (org-entry-get (point) "SCHEDULED")))
   (lambda (a b)
     (cond
      ((and (car a) (car b)) (string< (cdr a) (cdr b)))
      ((car b) t)
      ((car a) nil)                   ; move done things to the bottom
      (t (string< (cdr a) (cdr b)))))))

(defun emacsconf-upcoming-insert-or-update-from-slug (slug)
  (interactive (list (emacsconf-complete-talk)))
  (save-excursion
    (emacsconf-with-talk-heading slug
      (emacsconf-upcoming-insert-or-update nil nil))))

(defun emacsconf-upcoming-insert-or-update (&optional info sort)
  (interactive (list nil nil))
  (let ((info
         (or info
             (when (buffer-file-name)
               (if (string= (expand-file-name (buffer-file-name)) emacsconf-org-file)
                   (emacsconf-get-talk-info-for-subtree)
                 (let ((slug (org-entry-get (point) "SLUG")))
                   (emacsconf-with-talk-heading slug
                     (emacsconf-get-talk-info-for-subtree)))))))
        pos)
    (when (and (plist-get info :slug) emacsconf-upcoming-file)
      (with-current-buffer (find-file-noselect emacsconf-upcoming-file)
        (setq pos (org-find-property "SLUG" (plist-get info :slug)))
        (if pos
            (goto-char pos)
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (insert (format "* TODO %s: %s - %s\n"
                          (plist-get info :slug)
                          (plist-get info :title)
                          (plist-get info :speakers))))
        (org-todo (plist-get info :status))
        (org-entry-put (point) "SLUG" (plist-get info :slug))
        (org-entry-put (point) "URL" (concat emacsconf-base-url emacsconf-year "/talks/" (plist-get info :slug)))
        (org-set-property "SCHEDULED" (plist-get info :scheduled))
        (unless (org-entry-get (point) "INTRO_NOTE")
          (org-entry-put (point) "INTRO_NOTE" (plist-get info :intro-note)))
        (when (or (plist-get info :pronouns) (plist-get info :pronunciation))
          (unless (org-entry-get (point) "OTHER_INFO")
            (org-entry-put (point) "OTHER_INFO"
                           (string-join (delq nil (list (plist-get info :pronouns) (plist-get info :pronunciation))) " "))))
        (org-entry-put (point) "PRESENT" (or (plist-get info :present) "?"))
        (org-entry-put (point) "DURATION"
                       (or (plist-get info :video-duration)
                           (concat "~ " (plist-get info :duration))))
        (org-entry-put (point) "BUFFER"
                       (format "%s (ending ~ %s)"
                               (plist-get info :buffer)
                               (format-time-string
                                "%l:%M%p"
                                (seconds-to-time
                                 (+
                                  (time-to-seconds
                                   (org-timestamp-to-time (org-timestamp-split-range (org-timestamp-from-string (plist-get info :scheduled)))))
                                  (* 60 (string-to-number (plist-get info :duration)))
                                  (* 60 (string-to-number (plist-get info :buffer)))))
                                emacsconf-timezone)))
        (org-entry-put (point) "Q_AND_A" 
                       (or (plist-get info :bbb-room)
                           (plist-get info :q-and-a)
                           "?"))
        ;(org-entry-put (point) "IRC"
        ;               (or (plist-get info :irc) "?"))
        (org-entry-put (point) "CHECK_IN"
                       (or (plist-get info :check-in) "?"))
        (org-entry-put (point) "EMAIL" (plist-get info :email))
        (if (plist-get info :contact)
            (org-entry-put (point) "CONTACT" (plist-get info :check-in)))
        (if sort (emacsconf-upcoming-sort))))))






(provide 'emacsconf-upcoming)
;;; emacsconf-upcoming.el ends here
