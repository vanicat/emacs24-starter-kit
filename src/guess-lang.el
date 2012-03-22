;;; @(#) guess-lang.el --- Automagically guess what is the language of a buffer

;;; @(#) $Id: guess-lang.el,v 1.1 2001/10/17 17:27:38 benj Exp $

;; This file is *NOT* part of GNU Emacs.

;; Copyright (C) 2001 by Benjamin Drieu
;; Author:	 Benjamin Drieu <bdrieu@april.org>
;; Maintainer:	 Benjamin Drieu <bdrieu@april.org>
;; Created:	 2001-04-23
;; Keywords: languages, i18n

;; LCD Archive Entry:
;; guess-lang|Benjamin Drieu|bdrieu@april.org|
;; Automagically guess what is the language of a buffer|
;; 22-Apr-2001|$Revision: 1.1 $|~/misc/guess-lang.el|

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
;; polish, spanish, esperanto and somewhat russian (based on loosely
;; collected words from various .po files ... does anybody want to
;; help ?).

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
;; ispell (add-hook 'ispell-generic-hook 'guess-lang-set-ispell-dictionnary)

;; To do:
;; - notify when no dictionnary can be loaded
;; - explain how to make new frequency tables of keywords.
;; - speed up execution
;; - get rid of functional thingies like <html> or \begin{section}
;; - in case of huge buffers, use a better algorithm to check only
;;   relevant parts

;;; Variables:

(defvar guess-lang-version
  (progn
    (let ((revision "$Revision: 1.1 $"))
      (string-match "\\([0-9\\.]*\\) *\\$$" revision)
      (match-string 1 revision)))
  "Version number.")

(defgroup guess-lang nil
  "Language automatic detection."
  :group 'i18n
  :group 'convenience)

(defcustom guess-lang-languages-to-guess
  '("french" "english")
  "Languages to guess.  Their names should be the same as Ispell
dictionnaries you have."
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

(defvar guess-lang-cached-buffers nil
  "Hold the list of cached buffer where language is already known.")

(defvar guess-lang-french-dictionnary
  '(;; This is iso8859-1 representation for a word
    "a-zA-Z\270\300-\326\300-\366\370-\377"
    
    ("de" . 0.038982874)
    ("la" . 0.026053034)
    ("et" . 0.015805825)
    ("le" . 0.015539979)
    ("les" . 0.015394974)
    ("des" . 0.013244028)
    ("a" . 0.012881505)
    ("en" . 0.011238087)
    ("un" . 0.008821295)
    ("est" . 0.008821295)
    ("il" . 0.008047921)
    ("que" . 0.007757904)
    ("du" . 0.007709569)
    ("une" . 0.006936195)
    ("dans" . 0.006356168)
    ("qui" . 0.005921146)
    ("pour" . 0.005510288)
    ("au" . 0.005147772)
    ("pas" . 0.004857755)
    ("s" . 0.004567738)
    ("sur" . 0.004471068)
    ("ne" . 0.004422733)
    ("par" . 0.004205222)
    ("ce" . 0.003987711)
    ("plus" . 0.003939376)
    ("se" . 0.003818535)
    ("ont" . 0.003141831)
    ("on" . 0.002948491)
    ("ou" . 0.002900149)
    ("sont" . 0.002851814)
    ("nous" . 0.002755144)
    ("mais" . 0.002634303)
    ("aux" . 0.002489298)
    ("e" . 0.002416792)
    ("me" . 0.002392628)
    ("ils" . 0.002344286)
    ("avec" . 0.002295951)
    ("son" . 0.002150946)
    ("je" . 0.002102611)
    ("y" . 0.00207844)
    ("cette" . 0.002005941)))

(defvar guess-lang-english-dictionnary
  '(;; This is iso8859-1 representation for a word
    "a-zA-Z\270\300-\326\300-\366\370-\377"

    ("the" . 0.049920353)
    ("of" . 0.022687168)
    ("to" . 0.019041785)
    ("in" . 0.01741208)
    ("and" . 0.015953924)
    ("a" . 0.015053304)
    ("is" . 0.010635954)
    ("that" . 0.00896336)
    ("s" . 0.006733244)
    ("for" . 0.006261486)
    ("it" . 0.006089937)
    ("not" . 0.005661068)
    ("be" . 0.005618186)
    ("on" . 0.005017768)
    ("this" . 0.004631788)
    ("as" . 0.004588899)
    ("was" . 0.004503128)
    ("are" . 0.00441735)
    ("have" . 0.003945592)
    ("i" . 0.003859821)
    ("by" . 0.003816932)
    ("with" . 0.003816932)
    ("will" . 0.00377405)
    ("has" . 0.003473841)
    ("you" . 0.003388063)
    ("they" . 0.003302292)
    ("but" . 0.003216521)
    ("at" . 0.003173632)
    ("he" . 0.003044972)
    ("would" . 0.003002083)
    ("an" . 0.003002083)
    ("all" . 0.002959194)
    ("people" . 0.002873423)
    ("there" . 0.002873423)
    ("from" . 0.002701874)
    ("or" . 0.002616103)
    ("which" . 0.002573214)
    ("his" . 0.002487443)
    ("we" . 0.002487443)
    ("what" . 0.002358783)
    ("one" . 0.002273005)
    ("who" . 0.002230116)
    ("q" . 0.002015685)))

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
  (let ((numwords
         (guess-lang-how-many (guess-lang-language-word-regexp lang)))
	(keywords-alist (guess-lang-keyword-list lang)))
    (/ (guess-lang-frequency-1 keywords-alist numwords)
       (float (length keywords-alist)))))

(defun guess-lang-language-word-regexp (&optional lang)
  "Return a regexp matching a word.
Optional argument LANG provides the name of the language which
you are looking for the regexp of a word."
  (if (or (null lang)
	  (null (car (guess-lang-property-list lang))))
      "\\<[a-zA-Z]+\\>"
    (concat "\\<[" (car (guess-lang-property-list lang)) "]+\\>")))

(defun guess-lang-frequency-1 (alist numwords)
  "Recursor for `guess-lang-frequency'.
Argument ALIST is a list of pairs which describe probability of
keywords.  Argument NUMWORDS is the number of words in the
buffer, passed as an argument to save time."
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
  (if (string= lang "english")
      guess-lang-english-dictionnary
    guess-lang-french-dictionnary))

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

;;;###autoload
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
  (setq guess-lang-cached-buffers
        (cons (cons (or buffer (current-buffer))
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
   (t (cons (car list)
            (guess-lang-remove-buffer-from-list-1 buffer (cdr list))))))

(defun guess-lang-flush-cache ()
  "Flush the cache from all buffers."
  (run-hooks 'guess-lang-flush-cache-hook)
  (setq guess-lang-cached-buffers nil))


(defun guess-lang (&optional langs)
  "Guess language in current buffer, but only among LANGS."
  (run-hooks 'guess-lang-hook)
  (save-excursion
    (save-restriction
      (princ (concat "Guessing language of buffer "
                     (buffer-name (current-buffer)) " ... "))
      (if (> (buffer-size) guess-lang-max-buffer-size)
	  (narrow-to-region
           (point) (min (point-max) (+ (point) guess-lang-max-buffer-size))))
      (let ((lang (guess-lang-1 (or langs guess-lang-languages-to-guess))))
	(let ((numwords
               (guess-lang-how-many (guess-lang-language-word-regexp lang))))
	  (when (> numwords guess-lang-min-words-to-cache)
	    (guess-lang-add-buffer-to-list (current-buffer) lang))
	  (princ (concat "(" lang ")"))
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
  (ispell-change-dictionary (guess-lang-buffer-cached)))

(provide 'guess-lang)
(run-hooks 'guess-lang-load-hook)

;;; guess-lang.el ends here
