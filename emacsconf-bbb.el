;;;###autoload
(defun emacsconf-bbb-status (talk)
  (let ((states
         '((open . "OPEN_Q UNSTREAMED_Q")
           (before . "TODO TO_REVIEW TO_ACCEPT WAITING_FOR_PREREC TO_PROCESS PROCESSING TO_AUTOCAP TO_ASSIGN TO_CAPTION TO_CHECK TO_STREAM PLAYING CLOSED_Q")
           (after . "TO_ARCHIVE TO_EXTRACT TO_REVIEW_QA TO_INDEX_QA TO_CAPTION_QA TO_FOLLOW_UP DONE")
           (cancelled . "CANCELLED"))))
    (if (string-match "live" (or (plist-get talk :q-and-a) ""))
        (or (car (seq-find (lambda (state)
                             (member (plist-get talk :status) (split-string (cdr state))))
                           states))
            (throw 'error "Unknown talk BBB state"))
      'irc)))

(defvar emacsconf-bbb-base-url "https://bbb.emacsverse.org/" "Include trailing slash.")
(defun emacsconf-bbb-room-title-list (&optional info)
  (delq nil
        (mapcar
         (lambda (o)
           (when (car o)
             (concat "ec" (substring emacsconf-year 2)
                     "-" (plist-get (emacsconf-get-shift (plist-get (cadr o) :start-time)) :id)
                     "-" (plist-get (emacsconf-get-track (plist-get (cadr o) :track)) :id)
                     " " (car o)
                     " ("
                     (mapconcat (lambda (talk) (plist-get talk :slug)) (cdr o) ", ")
                     ")")))
         (seq-group-by (lambda (o) (plist-get o :speakers))
                       (or info (emacsconf-active-talks (emacsconf-filter-talks (emacsconf-get-talk-info))))))))

(defun emacsconf-bbb-create-rooms ()
  "Copy the commands needed to create the rooms.
docker exec -it greenlight-v3 /bin/bash -c \"bundle exec rails console\"
user_id = User.find_by_email(\"emacsconf@sachachua.com\").id"
  (interactive)
  (kill-new
   (mapconcat (lambda (group)
					      (format
					       "Room.create(user_id: user_id, name: \"%s - %s\")\n"
					       (plist-get (cadr group) :speakers)
					       (string-join (mapcar (lambda (talk) (plist-get talk :slug))
																		  (cdr group))
                              ", ")))
				      (emacsconf-mail-groups (emacsconf-active-talks (emacsconf-get-talk-info)))
				      ""))
  (message "Copied. Run it inside the greenlight-v3 rails console."))

(defun emacsconf-bbb-load-rooms (string)
	"Split STRING and load them as ROOM properties.
STRING should be a list of rooms, one room per line, like this:
friendly-id speaker - slugs
friendly-id speaker - slugs

Print out room IDs with:
Room.all.each { |x| puts x.friendly_id + " " + x.name }; nil
"
  (interactive "MInput: ")
	(let ((rooms
				 (mapcar (lambda (row) (when (string-match "^\\(.+?\\) \\(.+\\)" row)
																 (list (match-string 1 row) (match-string 2 row))))
								 (split-string string "\n"))))
		(mapc (lambda (talk)
						(emacsconf-go-to-talk talk)
						(when (plist-get talk :speakers)
							(org-entry-put
							 (point)
							 "ROOM"
							 (concat
								emacsconf-bbb-base-url
								"rooms/"
								(car
								 (seq-find
									(lambda (o)
										(string-match
										 (concat
											"^"
											(regexp-quote
											 (plist-get talk :speakers))
											" - ")
										 (cadr o)))
									rooms))
								"/join"))))
					(emacsconf-active-talks (emacsconf-get-talk-info)))))



(defun emacsconf-bbb-spookfox-set-moderator-codes ()
  (interactive)
  (dolist (talk (seq-filter (lambda (o)
														  (and (plist-get o :bbb-room)
																   (not (plist-get o :bbb-mod-code))))
													  (emacsconf-publish-prepare-for-display (emacsconf-get-talk-info))))
	  (spookfox-js-injection-eval-in-active-tab
	   (format "window.location.href = \"%s\""
					   (replace-regexp-in-string "/join" "" (plist-get talk :bbb-room)))
	   t)
	  (sleep-for 3)
	  (spookfox-js-injection-eval-in-active-tab
	   "document.querySelector('button[data-rr-ui-event-key=\"settings\"]').click()" t)
	  (spookfox-js-injection-eval-in-active-tab
	   "document.querySelector('input#glAnyoneCanStart').checked = true")
	  (spookfox-js-injection-eval-in-active-tab
	   "document.querySelector('input#muteOnStart').checked = true")
	  (spookfox-js-injection-eval-in-active-tab
	   "document.querySelectorAll('.border-end button')[2].click()" t)
	  (let ((code (spookfox-js-injection-eval-in-active-tab
							   "document.querySelector('.access-code-input input').value" t)))
		  (message "Setting %s to %s" (plist-get talk :slug) code)
		  (emacsconf-set-property-from-slug
		   talk "BBB_MOD_CODE"
		   code)
		  (sit-for 2))))

(defun emacsconf-bbb-spookfox-confirm-settings ()
  (interactive)
  (dolist (talk (seq-filter (lambda (o)
														(plist-get o :bbb-room))
													(emacsconf-publish-prepare-for-display (emacsconf-get-talk-info))))
	(spookfox-js-injection-eval-in-active-tab
	 (format "window.location.href = \"%s\""
					 (replace-regexp-in-string "/join" "" (plist-get talk :bbb-room)))
	 t)
	(sleep-for 3)
	(spookfox-js-injection-eval-in-active-tab
	 "document.querySelector('button[data-rr-ui-event-key=\"settings\"]').click()" t)
	(sleep-for 3)))
