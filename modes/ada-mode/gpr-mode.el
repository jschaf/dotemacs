;;; gpr-mode --- major-mode for editing GNAT project files

;; Copyright (C) 2007, 2008  Stephen Leake
;; Copyright (C) 2004  Rolf Ebert

;; Author: Rolf Ebert      <rolf.ebert_nosp...@gmx.net>
;; Keywords: languages ada

;; This file is not part of GNU Emacs.

;; gpr-mode is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; ada-gpr is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;; Commentary:
;;
;;; History:
;;
;;; Code:

;; We use the key bindings from ada-mode

(require 'ada-mode)

;; ---------------------------------------------------
;;    support for font-lock.el
;; ----------------------------------------------------

(defvar gpr-font-lock-keywords
  (progn ;; eval-when-compile
    (list
     ;;
     ;; keyword plus name.
     (list (concat
	    "\\<\\("
	    "package\\|"
	    "project\\|"
	    "for"
	    "\\)\\>[ \t]*"
	    "\\(\\sw+\\(\\.\\sw*\\)*\\)?")
	   '(1 font-lock-keyword-face) '(2 font-lock-function-name-face nil t))
     ;;
     ;; Main keywords
     (list (concat "\\<"
		   (regexp-opt
		    '("abstract" "case" "external" "is" "library" "others" "renames" "type"
		      "use" "when" "with") t)
		   "\\>")
	   '(1 font-lock-keyword-face))
     ;;
     ;; Anything following end and not already fontified is a body name.
     '("\\<\\(end\\)\\>\\([ \t]+\\)?\\(\\(\\sw\\|[_.]\\)+\\)?"
       (1 font-lock-keyword-face) (3 font-lock-function-name-face nil t))
     ;;
     ))
  "Default expressions to highlight in GNAT project file (gpr) mode.")

;;;
(defun gpr-mode ()
  "Ada gpr mode is the major mode for editing GNAT project files."

  (interactive)
  (kill-all-local-variables)

  (set (make-local-variable 'require-final-newline) t)

  ;;  Set the paragraph delimiters so that one can select a whole
  ;;  block simply with M-h
  (set (make-local-variable 'paragraph-start) "[ \t\n\f]*$")
  (set (make-local-variable 'paragraph-separate) "[ \t\n\f]*$")

  ;; comment end must be set because it may hold a wrong value if
  ;; this buffer had been in another mode before. RE
  (set (make-local-variable 'comment-end) "")

  ;; used by autofill and indent-new-comment-line
  (set (make-local-variable 'comment-start-skip) "---*[ \t]*")

  ;; used by autofill to break a comment line and continue it on
  ;; another line. The reason we need this one is that the default
  ;; behavior does not work correctly with the definition of
  ;; paragraph-start above when the comment is right after a
  ;; multi-line subprogram declaration (the comments are aligned under
  ;; the latest parameter, not under the declaration start).
  (set (make-local-variable 'comment-line-break-function)
       (lambda (&optional soft) (let ((fill-prefix nil))
				  (indent-new-comment-line soft))))

  ;; use indenting from Ada, just add "project"
  (set (make-local-variable 'ada-subprog-start-re)
       (concat "\\<" (regexp-opt '("project" "package") t) "\\>"))

  (set (make-local-variable 'indent-line-function)
       'ada-indent-current-function)

  (set (make-local-variable 'comment-column) 40)

  (set 'case-fold-search t)

  (set (make-local-variable 'fill-paragraph-function)
       'ada-fill-comment-paragraph)

  ;;  font-lock support :
  ;;  We need to set some properties for XEmacs, and define some variables
  ;;  for Emacs

  (if (featurep 'xemacs)
      ;;  XEmacs
      (put 'ada-mode 'font-lock-defaults
	   '(gpr-font-lock-keywords
	     nil t ((?\_ . "w") (?# . ".")) beginning-of-line))
    ;;  Emacs
    (set (make-local-variable 'font-lock-defaults)
	 '(gpr-font-lock-keywords
	   nil t
	   ((?\_ . "w") (?# . "."))
	   beginning-of-line
	   (font-lock-syntactic-keywords . ada-font-lock-syntactic-keywords)))
    )

  ;;  Support for ispell : Check only comments
  (set (make-local-variable 'ispell-check-comments) 'exclusive)

  ;;  Support for indent-new-comment-line (Especially for XEmacs)
  (setq comment-multi-line nil)

  (setq major-mode 'gpr-mode
	mode-name "GNAT Project")

  ;;  use the keybindings from Ada mode
  (use-local-map ada-mode-map)

  ;;  use Ada syntax table
  (set-syntax-table ada-mode-syntax-table)

  (if ada-clean-buffer-before-saving
      (progn
	;; remove all spaces at the end of lines in the whole buffer.
	(add-hook 'local-write-file-hooks 'delete-trailing-whitespace)
	;; convert all tabs to the correct number of spaces.
	(add-hook 'local-write-file-hooks
		  (lambda () (untabify (point-min) (point-max))))))

  (run-hooks 'gpr-mode-hook)

  ;;  To be run after the hook, in case the user modified
  ;;  ada-fill-comment-prefix
  (make-local-variable 'comment-start)
  (if ada-fill-comment-prefix
      (set 'comment-start ada-fill-comment-prefix)
    (set 'comment-start "-- "))

  ;;  Run this after the hook to give the users a chance to activate
  ;;  font-lock-mode

  (unless (featurep 'xemacs)
    (progn
      (ada-initialize-syntax-table-properties)
      (add-hook 'font-lock-mode-hook 'ada-handle-syntax-table-properties nil t)))

  (if ada-auto-case
      (ada-activate-keys-for-case)))

(add-to-list 'auto-mode-alist '("\\.gpr\\'" . gpr-mode))  ; GNAT project files

(provide 'gpr-mode)

;;; gpr-mode.el ends here
