;;; emacsconf-pentabarf.el ---  pentabarf XML export  -*- lexical-binding: t; -*-

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

;;; Code:
(defun emacsconf-assign-talk-uuids ()
  (interactive)
  (require 'uuid)
  (org-map-entries (lambda ()
                     (when (and (org-entry-get (point) "SLUG")
                                (not (org-entry-get (point) "UUID")))
                       (org-entry-put (point) "UUID" (uuid-string))))))

(defun emacsconf-talk-info-as-pentabarf-xml (o)
  (require 'svg)
  (dom-node
   'event `((id . ,(plist-get o :slug))
            (guid . ,(plist-get o :uuid)))
   (dom-node 'date nil (emacsconf-pentabarf-format-datetime (plist-get o :start-time)))
   (dom-node 'start nil (format-time-string "%H:%M" (plist-get o :start-time)))
   (dom-node 'language nil "en")
   (dom-node 'room nil "Main")
   (dom-node 'subtitle)
   (dom-node 'type nil "Talk")
   (dom-node 'track nil "Main")
   (dom-node 'slug nil (concat "emacsconf-" emacsconf-year "-talk-" (plist-get o :slug)))
   (dom-node 'duration nil (format-seconds "%h:%.2m"
                                           (time-to-seconds
                                            (time-subtract
                                             (plist-get o :end-time)
                                             (plist-get o :start-time)))))
   ;; (dom-node 'slug nil (plist-get o :slug))
   (dom-node 'title nil (plist-get o :title))
   (dom-node 'abstract)
   (dom-node 'description nil (svg--encode-text (concat "Times are approximate and will probably change.\n\n" (plist-get o :markdown))))
   (dom-node 'url nil (concat "https://emacsconf.org/" emacsconf-year "/talks/" (plist-get o :slug)))
   (apply 'dom-node 'persons
          nil
          (seq-map (lambda (p)
                     (dom-node 'person nil p))
                   (split-string (or (plist-get o :speakers) "EmacsConf") ", ")))))

(defun emacsconf-pentabarf-format-datetime (time)
  (format-time-string "%Y-%m-%dT%H:%M:%SZ" time t))
(defun emacsconf-pentabarf-days (talks)
  (seq-map-indexed
   (lambda (day i)
     (dom-node
      'day
      `((date . ,(car day)) 
        (start . ,(emacsconf-pentabarf-format-datetime (plist-get (cadr day) :start-time)))
        (end . ,(emacsconf-pentabarf-format-datetime (plist-get (car (last day)) :end-time)))
        (index . ,(1+ i)))
      (apply
       'dom-node 'room
       '((name . "Main"))
       (seq-map #'conf-talk-info-as-pentabarf-xml
                (cdr day)))))
   (seq-group-by
    (lambda (o) (format-time-string "%Y-%m-%d" (plist-get o :start-time)))
    talks)))
;; (emacsconf-pentabarf-days (seq-take (emacsconf-filter-talks emacsconf-info) 3))
;; based on svg-print, but without spaces
(defun emacsconf-pentabarf-print (dom)
  "Convert DOM into a string containing the xml representation."
  (if (stringp dom)
      (insert dom)
    (insert (format "<%s" (car dom)))
    (dolist (attr (nth 1 dom))
      ;; Ignore attributes that start with a colon.
      (unless (= (aref (format "%s" (car attr)) 0) ?:)
        (insert (format " %s=\"%s\"" (car attr) (cdr attr)))))
    (insert ">")
    (dolist (elem (nthcdr 2 dom))
      (emacsconf-pentabarf-print elem))
    (insert (format "</%s>" (car dom)))))

(defun emacsconf-pentabarf  (emacsconf-info)
  (require 'svg)
  (apply
   'dom-node
   'schedule
   nil
   (append
    (list
     (dom-node 'generator '((name . "EmacsConf") (version . "0.1")))
     (dom-node 'version nil (format-time-string "%Y%m%d%H%M%S"))
     (dom-node 'conference nil
               (dom-node 'acronym nil "emacsconf" emacsconf-year)
               (dom-node 'title nil (concat "EmacsConf " emacsconf-year))
               (dom-node 'start nil "2021-11-27")
               (dom-node 'end nil "2021-11-28")
               (dom-node 'time_zone_name nil "America/Toronto")
               (dom-node 'base_url nil (concat "https://emacsconf.org/" emacsconf-year))))
    (emacsconf-pentabarf-days (emacsconf-filter-talks emacsconf-info)))))

;; (emacsconf-pentabarf-days (seq-take (emacsconf-filter-talks emacsconf-info) 3))
;; 

(defun emacsconf-pentabarf-generate ()
  (unless (file-directory-p emacsconf-directory) (error "Please specify the wiki directory in the emacsconf-directory variable."))
  (with-temp-file (expand-file-name "emacsconf-pentabarf.xml" (expand-file-name emacsconf-year emacsconf-directory))
    (emacsconf-pentabarf-print (emacsconf-pentabarf (emacsconf-filter-talks (emacsconf-get-talk-info 'wiki))))))

;;; emacsconf-pentabarf.el ends here
