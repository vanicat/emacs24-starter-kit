;;; @(#) guess-lang.el --- Automagically guess what is the language of a buffer

;;; @(#) $Id: guess-lang.el,v 1.1.1.1 2002/07/01 17:04:37 benj Exp $

;; This file is *NOT* part of GNU Emacs.

;; Copyright (C) 2001 by Benjamin Drieu
;; Author:	 Benjamin Drieu <bdrieu@april.org>
;; Maintainer:	 Benjamin Drieu <bdrieu@april.org>
;; Created:	 2001-04-23
;; Keywords: languages, i18n

;; LCD Archive Entry:
;; guess-lang|Benjamin Drieu|bdrieu@april.org|
;; Automagically guess what is the language of a buffer|
;; 22-Apr-2001|$Revision: 1.1.1.1 $|~/misc/guess-lang.el|

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Automagically guess what is the language of a buffer.

;; This works basically by counting occurrences of common words in
;; every language that is known and comparing respective numbers.
;; Languages currently supported are english, french, german, danish,
;; polish, spanish and somewhat russian (based on loosely collected
;; words from various .po files ... does anybody want to help ?).

;; guess-lang.el is based on an idea from Pascal Courtois
;; <Pascal.Courtois@nouvo.com> but evolved a lot.

;; Because ispell.el doesn't use hooks, guess-lang use is a little bit
;; tricky and ask you to add things like the two following examples in
;; your emacs initialization file.

;; (add-hook 'latex-mode-hook
;; 	  (lambda ()
;; 	    (ispell-change-dictionary (guess-lang-buffer-cached))))

;; (add-hook 'message-send-hook
;; 	     (lambda ()
;; 	       (ispell-change-dictionary (guess-lang-message))
;; 	       (ispell-message)))

;; You may try to use the following with "experimental" versions of
;; ispell (add-hook 'ispell-hook 'guess-lang-set-ispell-dictionnary)

;; To do:
;; - add support for aliases again
;; - notify when no dictionnary can be loaded
;; - explain how to make new frequency tables of keywords.
;; - speed up execution
;; - get rid of functional thingies like <html> or \begin{section}
;; - in case of huge buffers, use a better algorithm to check only
;;   relevant parts

;; History:

;; $Log: guess-lang.el,v $
;; Revision 1.1.1.1  2002/07/01 17:04:37  benj
;; - version initiale
;;
;; Revision 1.10  2001/10/17 17:26:54  benj
;; - in guess-lang-set-ispell-dictionnary, set the dictionnary only if a
;;   language can be found
;;
;; Revision 1.9  2001/06/13 20:02:09  benj
;; - bugfixes
;;
;; Revision 1.8  2001/06/13 19:38:13  benj
;; - added support for separate dictionnaries loaded on demand
;; - these are stored in one of th directories specified by the new
;;   configuration variable `guess-lang-dictionnaries-path'
;;
;; Revision 1.7  2001/06/06 12:20:06  drieu
;; - added support for swedish thanks to Knut Wannheden
;;
;; Revision 1.6  2001/06/05 23:59:54  benj
;; - added narrowing to save computation time in case of huge buffers
;;
;; Revision 1.5  2001/06/04 22:38:48  benj
;; - added hooks for guess-lang functions
;; - updated examples in header
;;
;; Revision 1.4  2001/06/04 22:34:01  benj
;; - added caching capabilities
;; - added experimental support for experimental ``ispell hooks''
;; - did some cleanup in frequency lists
;; - fixed some docstrings
;; - added support for Polish thanks to Michel Luczak
;;
;; Revision 1.3  2001/05/18 22:40:19  benj
;; - added support for various charsets
;; - changed a bit dictionnaries structure
;; - added _experimental_ support for russian
;;
;; Revision 1.2  2001/05/13 17:28:27  drieu
;; - added support for Danish thanks to Ole Laursen
;; - added better German frequencies thanks to Jesper Harder
;; - replaced all ``ratio'' occurences by ``frequencies''
;;
;; Revision 1.1  2001/04/24 20:18:11  drieu
;; Initial revision
;;

;;; Variables:

(defvar guess-lang-version
  (progn
    (let ((revision "$Revision: 1.1.1.1 $"))
      (string-match "\\([0-9\\.]*\\) *\\$$" revision)
      (match-string 1 revision)))
  "Version number.")

(defgroup guess-lang nil
  "Language automatic detection."
  :group 'i18n
  :group 'convenience)

(defcustom guess-lang-languages-to-guess
  '("francais" "american" "deutsch" "spanish" "italiano" "danish" "polish" "swedish")
  "Languages to guess.  Their names should be the same as Ispell dictionnaries you have."
  :type 'list
  :group 'guess-lang)

(defcustom guess-lang-min-words-to-cache
  50
  "Minimum amount of words to have in a buffer to cache its content."
  :type 'number
  :group 'guess-lang)

(defcustom guess-lang-max-buffer-size
  2000
  "Maximum size in bytes of text to check within a buffer.
Truncate the buffer if necessary.  Set this variable to the lowest possible
value to save computation time.  2000 seems to be a good choice for
state-of-the-art Intels at this time."
  :type 'number
  :group 'guess-lang)

(defcustom guess-lang-dictionnaries-path 
  "/usr/share/guess-lang/dict"
  "List of directories where guess-lang-dictionnaries are installed.
After looking at this list, `guess-lang' examines $HOME/.guess-lang/.
You can optionally use a string to specify directories a la $PATH."
  :group 'guess-lang)

(defvar guess-lang-cached-buffers nil
  "Hold the list of cached buffer where language is already known.")

;;; Code:

(defun guess-lang-how-many (regexp)
  "Return number of occurences for REGEXP following point."
  (let ((count 0) opoint)
    (save-excursion
     (while (and (not (eobp))
		 (progn (setq opoint (point))
			(re-search-forward regexp nil t)))
       (if (= opoint (point))
	   (forward-char 1)
	 (setq count (1+ count))))
     count)))


(defun guess-lang-frequency (lang)
  "Compute frequency of occurences of LANG keywords in buffer."
  (let ((numwords (guess-lang-how-many (guess-lang-language-word-regexp lang))) ; TODO: should it be better to compute this _once_ ?
	(keywords-alist (guess-lang-keyword-list lang)))
    (/ (guess-lang-frequency-1 keywords-alist numwords)
       (float (length keywords-alist)))))


(defun guess-lang-language-word-regexp (&optional lang)
  "Return a regexp matching a word.
Optional argument LANG provides the name of the language which you are looking for the regexp of a word."
  (if (or (null lang)
	  (null (car (guess-lang-property-list lang))))
      "\\<[a-zA-Z]+\\>"
    (concat "\\<[" (car (guess-lang-property-list lang)) "]+\\>")))


(defun guess-lang-frequency-1 (alist numwords)
  "Recursor for `guess-lang-frequency'.
Argument ALIST is a list of pairs which describe probability of keywords.
Argument NUMWORDS is the number of words in the buffer, passed as an argument to save time."
  (cond
   ((null alist) 0)
   ((let ((occurences (guess-lang-how-many (concat "\\<" (caar alist) "\\>"))))
      (+
       (if (> (/ occurences (float numwords)) (cdar alist))
	   1 0)
       (guess-lang-frequency-1 (cdr alist) numwords))))))
	  

(defun guess-lang-keyword-list (lang)
  "Return keyword list for LANG from a dictionnary.
There may be aliases, see dictionnaries installed on your system for examples."
  (cdr (guess-lang-property-list lang)))


(defun guess-lang-property-list (lang)
  "Return property list for LANG from `guess-lang-words'.
There may be aliases, see `guess-lang-words' for examples."
  (let ((dictname (read (concat "guess-lang-" lang "-dictionnary"))))
    (if (boundp dictname)
	(eval dictname)
      (guess-lang-load-dictionnary lang))))


(defun guess-lang-load-dictionnary (lang)
  "Load the LANG dictionnary if found."
  (let ((filename (guess-lang-find-dictionnary lang (guess-lang-make-path-list))))
    (if filename 
	(progn (load filename nil t t)
	       (eval (read (concat "guess-lang-" lang "-dictionnary")))))))


(defun guess-lang-find-dictionnary (lang path)
  "Find the guess-lang dictionnary for LANG in PATH list of directories."
  (if path
      (let ((filename-elc (concat (car path) "/" lang ".elc"))
	    (filename (concat (car path) "/" lang))
	    (filename-el (concat (car path) "/" lang ".el")))
	(cond 
	 ((file-exists-p filename-elc) filename-elc)
	 ((file-exists-p filename-el) filename-el)
	 ((file-exists-p filename) filename)
	 ((guess-lang-find-dictionnary lang (cdr path)))))))
   

(defun guess-lang-make-path-list ()
  "Return a list of directories where to find Guess-lang dictionnaries.

Guess-lang will look at dictionnaries first in
`guess-lang-dictionnaries-path', then in $HOME/.guess-lang if not
already in `guess-lang-dictionnaries-path' and then in the current
directory."
  (let ((home-dir-path (concat (getenv "HOME") "/.guess-lang"))
	(current-dir (chop default-directory))
	(guess-lang-default-directories 
	  (cond 
	   ((listp guess-lang-dictionnaries-path) guess-lang-dictionnaries-path)
	   ((stringp guess-lang-dictionnaries-path) 
	    (split-string guess-lang-dictionnaries-path ":")))))
    (append guess-lang-default-directories
	    (if (not (member home-dir-path guess-lang-default-directories)) 
		(list home-dir-path))
	    (if (not (member current-dir guess-lang-default-directories)) 
		(list current-dir)))))


(defun chop (string)
  "Remove the last character for STRING.
Pure perlism. ;-)"
  (substring string 0 -1))
	  

(defun guess-lang-buffer (&optional lang cached)
  "Guess language for current buffer.
Optional argument LANG is a list of language to check.
Defaulted to `guess-lang-languages-to-guess'.
Optional argument CACHED specifies whether we should process the buffer or 
look at the cache."
  (run-hooks 'guess-lang-buffer-hook)
  (or (and cached
	   (cdr (assoc (current-buffer) guess-lang-cached-buffers)))
      (save-excursion
	(if (< (point-max) (+ (point) guess-lang-max-buffer-size))
	    (beginning-of-buffer))
	(guess-lang lang))))


(defun guess-lang-buffer-cached (&optional lang)
  "Guess language for current buffer from the cache.
Optional argument LANG Guess language for current buffer from the cache."
  (guess-lang-buffer lang t))


(defun guess-lang-other-buffer (buffer &optional lang cached)
  "Guess language for another BUFFER.
Optional argument LANG  is a list of languages to check for and is
defaulted to `guess-lang-languages-to-guess'.

Optional argument CACHED specifies wheter the search is cached."
  (run-hooks 'guess-lang-other-buffer-hook)
  (if (buffer-live-p (get-buffer buffer))
      (or (and cached
	       (cdr (assoc (get-buffer buffer) guess-lang-cached-buffers)))
	  (save-excursion
	    (set-buffer buffer)
	    (beginning-of-buffer)
	    (guess-lang (or lang guess-lang-languages-to-guess))))))


(defun guess-lang-other-buffer-cached (buffer &optional lang)
  "Guess language for another BUFFER from the cache.

Optional argument LANG  is a list of languages to check for and is
defaulted to `guess-lang-languages-to-guess'."
  (guess-lang-other-buffer buffer lang t))


(defun guess-lang-message (&optional lang cached)
  "Guess language for current buffer, which is a message.
Optional argument LANG is a list of language to check.
Defaulted to `guess-lang-languages-to-guess'.
Optional argument CACHED specifies whether the search performed is cached."
  ;; TODO: do not check signature as well as quoted and attached parts
  (run-hooks 'guess-lang-message-hook)
  (or (and cached
	   (cdr (assoc (current-buffer) guess-lang-cached-buffers)))
      (save-excursion
	(eval-and-compile
	  (autoload 'message-goto-body "message"))
	(message-goto-body)
	(guess-lang lang))))


(defun guess-lang-add-buffer-to-list (&optional buffer lang)
  "Add BUFFER to the cache and specify its language is LANG.
BUFFER is defaulted to current buffer."
  (run-hooks 'guess-lang-add-buffer-to-list-hook)
  (add-hook 'kill-buffer-hook 'guess-lang-remove-buffer-from-list nil t)
  (setq guess-lang-cached-buffers (cons (cons (or buffer (current-buffer))
					      (or lang (guess-lang-buffer))) guess-lang-cached-buffers)))


(defun guess-lang-remove-buffer-from-list (&optional buffer)
  "Remove BUFFER from the cache."
  (setq guess-lang-cached-buffers
	(guess-lang-remove-buffer-from-list-1
	 (or buffer (current-buffer)) guess-lang-cached-buffers)))


(defun guess-lang-remove-buffer-from-list-1 (buffer list)
  "Recursor for `guess-lang-remove-buffer-from-list'.
Remove BUFFER from the cache, which is LIST for this recursion."
  (cond
   ((null list) nil)
   ((equal (caar list) buffer) (cdr list))
   (t (cons (car list) (guess-lang-remove-buffer-from-list-1 buffer (cdr list))))))


(defun guess-lang-flush-cache ()
  "Flush the cache from all buffers."
  (run-hooks 'guess-lang-flush-cache-hook)
  (setq guess-lang-cached-buffers nil))


(defun guess-lang (&optional langs)
  "Guess language in current buffer, but only among LANGS."
  (run-hooks 'guess-lang-hook)
  (message (concat "Guessing language of buffer " (buffer-name (current-buffer)) " ..."))
  (save-excursion
    (save-restriction
      (if (> (buffer-size) guess-lang-max-buffer-size)
	  (narrow-to-region (point) (min (point-max) (+ (point) guess-lang-max-buffer-size))))
      (let ((lang (guess-lang-1 (or langs guess-lang-languages-to-guess))))
	(let ((numwords (guess-lang-how-many (guess-lang-language-word-regexp lang))))
	  (when (and (> numwords guess-lang-min-words-to-cache) lang)
	    (guess-lang-add-buffer-to-list (current-buffer) lang))
	  (message nil)
	  lang)))))


;; This is going to be accelerated since this is not very optimal to
;; compute this several times.  In the future, one pass will be made
;; to compute the number of words and then we will count occurences
;; of words from every language.
(defun guess-lang-1 (langs &optional threshold)
  "Compute frequencies for every languages in LANGS and return the best one.
THRESHOLD ensure that of no language has better frequency than
THRESHOLD, it won't be returned."
  (cond
   ((null langs) nil)
   ((atom langs) (> (guess-lang-frequency langs) 0))
   ((let ((thres (guess-lang-frequency (car langs))))
      (if (and (> thres (or threshold 0))
	       (not (guess-lang-1 (cdr langs) thres)))
	  (car langs)
	(guess-lang-1 (cdr langs) threshold))))))


(defun guess-lang-set-ispell-dictionnary (&optional dictionnary)
  "Utility function provided for use with Ispell mode.
Your version of Ispell may not support it (in fact, this is not _yet_
included in Ispell distribution).

Set Ispell dictionnary either to DICTIONNARY or to guessed one."
  (let ((language (guess-lang-buffer-cached)))
    (message language)
    (if language 
	(ispell-change-dictionary language))))


(provide 'guess-lang)
(run-hooks 'guess-lang-load-hook)

;;; guess-lang.el ends here
