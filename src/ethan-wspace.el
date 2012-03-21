;;; ethan-wspace.el
;;; Whitespace customizations.
;;
;; Editing whitespace. I don't like to forcibly clean[1] whitespace
;; because when doing diffs/checkins, it pollutes changesets. However,
;; I prefer clean whitespace -- often I've deleted whitespace by
;; accident and been unable to "put it back" for purposes of diffing.
;;
;; [1] "Clean" whitespace, to me, means always using spaces instead of
;; tabs and no trailing whitespace at end of line. Additionally I
;; prefer to end each file with a newline, for many of the same
;; reasons; although this can be considered "clean" whitespace, it
;; probably deservesto be treated separately.
;;
;; Therefore, my approach is hybrid -- maintain clean whitespace where
;; possible, and avoid disturbing messy whitespce when I come across
;; it. I add a find-file-hook to track whether the whitespace of a
;; given file was clean to begin with, and add a before-save-hook that
;; uses this information to forcibly clean whitespace only if it was
;; clean at start.
;;
;; Viewing whitespace. Originally I had a font-lock-mode hook that
;; always called show-trailing-whitespace and show-tabs, but this
;; turns out to be annoying for a lot of reasons. Emacs internal
;; buffers like *Completions* and *Shell* would get highlit. So I
;; decided that a more sophisticated approach was called for.
;;
;; 1) If a buffer does not correspond to a file, I almost certainly do
;; not care whether the whitespace is clean. I'm not going to be
;; saving it, after all.
;;
;; 2) If a buffer corresponds to a file that was originally clean, I
;; still don't really care about the whitespace, since my hook (above)
;; would ensure that it was clean going forward.
;;
;; 3) If a buffer corresponds to a file that was not originally clean,
;; I do care about seeing whitespace, because I do not want to edit it
;; by accident.
;;
;; There are a few exceptions -- if I'm looking at a patch, or hacking
;; a Makefile, whitespace is different. More about those later.
;;
;; This file adds hooks to make this stuff happen -- check whether the
;; whitespace is clean when a file is first found, and preserve that
;; cleanliness if so, and highlight that dirtiness if not. It does
;; this by setting show-trailing-whitespace when necessary.
;;
;; NOTE: take out any customizations like this:
;; '(show-trailing-whitespace t)
;; show-trailing-whitespace will be turned on by ethan-wspace.
;;
;; I would re-implement show-trailing-whitespace, but it's handled
;; specially by the emacs core. Probably just as well..
;;
;; FIXME: treat separately the trailing-whitespace, tabs, and
;; end-with-newline thing? But basically all three are the same --
;; check on find-file whether it's fine, and fix it if it is, and
;; highlight if it's not?
;
; FIXME: These are all essentially minor modes, should be able to turn
; them off?

(defvar buffer-whitespace-was-clean nil
  "Keep track, per-buffer, whether the whitespace was clean initially.

Used by clean-whitespace-tentative and show-ws.")
(make-variable-buffer-local 'buffer-whitespace-was-clean)

(defun clean-whitespace ()
  "Clean the whitespace in a buffer -- strip trailing whitespace and untabify."
  (interactive)
  ; Clean active region if any, otherwise whole buffer.
; This could use nuke-trailing-whitespace and friends, but that wouldn't
; untabify
  (save-excursion
    (delete-trailing-whitespace))
  nil)

(defun clean-whitespace-tentative ()
  "If the whitespace for this buffer started clean, preserve cleanliness.

Used as a save-file-hook."
  (if buffer-whitespace-was-clean
      (clean-whitespace)
    nil))

(defun clean-whitespace-check ()
  "Sets buffer-local variable buffer-whitespace-was-clean if there's nothing weird in the whitespace.

Used as a find-file-hook. (Seems to run after font-lock-mode hooks.)"
  ; FIXME: weird buffers, like if you open a binary file?
  ; FIXME: if interactive, report current status of ws
  (interactive)
  (setq buffer-whitespace-was-clean (buffer-whitespace-clean-p)))

(defun buffer-whitespace-clean-p ()
  (save-excursion
    (goto-char (point-min))
    (if (not (or
         (re-search-forward "^ +\t" nil t)
         (re-search-forward "[ \t]+$" nil t)))
        t
      nil)))

(add-hook 'find-file-hook 'clean-whitespace-check)
; FIXME: gosh, write-files-functions, before-save-hook, write-contents-functions
(add-hook 'before-save-hook 'clean-whitespace-tentative)

;;; Showing whitespace
;;
;; We want to show whitespace if it's a real file and the original ws
;; was broken. If it was clean originally, we assume it'll be correct
;; (we'll clean it every save anyhow).
;;
;; We also turn it off for real files with dont-show-ws-this-buffer set to t.
;; Thus for .patch or .diff files, we can use special diff-mode magic.
(defvar dont-show-ws-this-buffer nil)
(make-variable-buffer-local 'dont-show-ws-this-buffer)

(require 'show-wspace)
(add-hook 'font-lock-mode-hook
          (lambda ()
            (if (and (buffer-file-name)
                     (not dont-show-ws-this-buffer)
                     (not (buffer-whitespace-clean-p)))
                (progn
                  (show-ws-highlight-tabs)
                  (setq show-trailing-whitespace t)))))

(defun dont-show-ws ()
;  (setq show-trailing-whitespace nil)  ; I have this variable customized
  (setq dont-show-ws-this-buffer t))

(add-hook 'diff-mode-hook 'dont-show-ws)

;; Diff mode.
;;
;; Every line in a diff starts with a plus, a minus, or a space -- so
;; empty lines in common with both files will show up as lines with
;; just a space in the diff. As a result, we only highlight trailing
;; whitespace for non-empty lines.
;;
;; The regex matches whitespace that only comes at
;; the end of a line with non-space in it.
;;
; FIXME: This also makes the diff-mode font lock break a little --
; changes text color on lines that match.
;
; FIXME: "bzr diff" format contains tabs?
(add-hook 'diff-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\S-\\([\240\040\t]+\\)$"
                                       (1 'show-ws-trailing-whitespace t))))))


; FIXME: compute this color based on the current color-theme
(setq space-color "#763636")
(set-face-background 'show-ws-trailing-whitespace space-color)
(set-face-background 'trailing-whitespace space-color)

(provide 'ethan-wspace)
