(defun icalendar--convert-org-diary-class-to-ical (nonmarker entry-main)
  "Convert `org-diary-class' diary entry to icalendar format.
NONMARKER is a regular expression matching the start of non-marking
entries.  ENTRY-MAIN is the first line of the diary entry."
  (if (string-match (concat nonmarker
			    "%%(org-diary-class "
			    "\\([0-9]+ [0-9]+ [0-9]+\\) " ;date start
			    "\\([0-9]+ [0-9]+ [0-9]+\\) " ;date start
			    "\\([0-9]+\\)"  ;DAYNAME
			    "\\([ 0-9]*\\)) "
			    "\\("
			    "\\([0-9][0-9]?:[0-9][0-9]\\)\\([ap]m\\)?"
			    "\\(-\\([0-9][0-9]?:[0-9][0-9]\\)\\([ap]m\\)?\\)?"
			    "\\)?"
			    "\\s-*\\(.*\\) ?$")
		    entry-main)
      (let* ((start (match-string 1 entry-main))
	     (end   (match-string 2 entry-main))
	     (dayname (read (match-string 3 entry-main)))
	     (skip-week (match-string 4 entry-main))
	     (remain-week ())
	     (startisostring (icalendar--datestring-to-isodate start))
	     (endisostring (icalendar--datestring-to-isodate end))
	     (starttimestring (icalendar--diarytime-to-isotime
			       (when (match-beginning 6)
				 (match-string 6 entry-main))
			       (when (match-beginning 7)
				 (match-string 7 entry-main))))
	     (endtimestring (icalendar--diarytime-to-isotime
			     (when (match-beginning 9)
			       (match-string 9 entry-main))
			     (when (match-beginning 10)
			       (match-string 10 entry-main))))
	     (summary (icalendar--convert-string-for-export
		       (match-string 11 entry-main))))
	(icalendar--dmsg "org-diary-class %s" entry-main)

	(setq skip-week (read (concat "(" skip-week ")")))
	(when skip-week
	  (setq remain-week (number-sequence 1 52))
	  (mapc (lambda (el) (setq remain-week (delete el remain-week)))
		skip-week))

	(when starttimestring
	  (unless endtimestring
	    (let ((time
		   (read (icalendar--rris "^T0?" ""
					  starttimestring))))
	      (setq endtimestring (format "T%06d"
					  (+ 10000 time))))))
	(list (concat "\nDTSTART;TZID=Europe/Paris;"
		      (if starttimestring "VALUE=DATE-TIME:"
			  "VALUE=DATE:")
		      startisostring
		      (or starttimestring "")
		      "\nDTEND;TZID=Europe/Paris;"
		      (if endtimestring "VALUE=DATE-TIME:"
			  "VALUE=DATE:")
		      startisostring
		      (or endtimestring "")
		      "\nRRULE:FREQ=YEARLY"
		      ";BYDAY="
		      (nth dayname '("SU" "MO" "TU" "WE" "TH" "FR" "SA"))
		      ";UNTIL="
		      endisostring
		      (if remain-week
			  (concat ";BYWEEKNO="
				  (mapconcat 'int-to-string remain-week ","))
			  ""))
	      summary))
      ;; no match
      nil))

(defadvice icalendar--convert-to-ical (around ical-for-org (nonmarker entry-main))
  (let ((res (icalendar--convert-org-diary-class-to-ical nonmarker entry-main)))
    (if res
	(setq ad-return-value res)
	ad-do-it)))

(ad-activate 'icalendar--convert-to-ical)

(provide 'ical-for-org)