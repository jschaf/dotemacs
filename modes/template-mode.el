;; Major mode for editing Else definition files.
;;
;; Copyright (C) 1999, 2000, 2001 Stephen Leake
;;
;; Author   : Stephen Leake <stephen_leake@acm.org>
;; Web Site : http://users.erols.com/leakstan/Stephe/index.html
;;
;; Keywords: else, lse
;;
;; template-mode requires GNU Emacs 20.3.1 or newer.
;;
;; This file is NOT part of GNU Emacs (yet), but is distributed under
;; the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later
;; version.

;; This code is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; USAGE
;;; =====
;;;
;;; The main starting point is template-mode; it should be added to
;;; auto-mode-alist for file type .lse.
;;;
;;; DESIGN
;;; ======
;;;
;;; else uses the mode-name to get the language. Since Peter Milliken
;;; chose "TEMPLATE" as the name of the language, that's what we use
;;; for the mode name (this is case sensitive, since assoc is used to
;;; compare language names). He also uses ".lse" as the file
;;; extension, so we associate .lse files with template-mode.

(defconst template-mode-version "0.6")


;;;--------------------
;;;    USER OPTIONS
;;;--------------------

(defgroup template nil
  "Major mode for editing Else definition files."
  :group 'languages)

(defcustom template-mode-hook nil
  "*List of functions to call when lse mode is invoked."
  :type 'hook
  :group 'lse)

(defvar template-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [next]  'scroll-up)
    (define-key map [prior] 'scroll-down)
    map)
"Local keymap for lse mode.")

;;; ---- end of user configurable variables


(defun template-mode-create-syntax-table ()
  "Create a basic syntax table"
  (interactive)
  (setq template-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\; ". 12" template-mode-syntax-table)
  (modify-syntax-entry ?\f ">   " template-mode-syntax-table)
  (modify-syntax-entry ?\n ">   " template-mode-syntax-table))

(template-mode-create-syntax-table)

;; main entry point
;;;###autoload
(defun template-mode ()
  "lse mode is a major mode for editing Else definition files.
Keybindings:
\\{template-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'template-mode)
  (setq mode-name "Template")
  (use-local-map template-mode-map)
  (add-hook 'local-write-file-hooks '(lambda () (untabify (point-min) (point-max))))
  (set-syntax-table template-mode-syntax-table)

  (set (make-local-variable 'font-lock-defaults)
        '(template-mode-font-lock-keywords
          nil t
          ((?\_ . "w") (?# . "."))
          beginning-of-line))
  (else-mode)
  (run-hooks 'template-mode-hook)
 )





(defvar template-mode-keywords-regexp
  (regexp-opt
   '("/DESCRIPTION" 
     "/LANGUAGE" 
     "/NOAUTO_SUBSTITUTE" 
     "/DUPLICATION"
     "/SEPARATOR"
     "/PLACEHOLDER"
     "/INITIAL_STRING"
     "/PUNCTUATION_CHARACTERS"
     "/SELF_INSERT_CHARACTERS"
     "/VALID_IDENTIFIER_CHARACTERS"
     "/INDENT_SIZE"
     "/VERSION"
     "/TYPE"
     "/AUTO_SUBSTITUTE"
     "/SUBSTITUTE_COUNT")))

(defvar template-mode-functions-regexp
  (regexp-opt
   '("DELETE" 
     "DEFINE" 
     "TOKEN" 
     "END"
     "PLACEHOLDER"
     "LANGUAGE")
   'words))

(defvar template-mode-types-regexp
  (regexp-opt
   '("CONTEXT_DEPENDENT"
     "NONTERMINAL"
     "TERMINAL"
     "MENU")
   'words))


(setq template-mode-font-lock-keywords
      `(
        (,template-mode-keywords-regexp . font-lock-keyword-face)
        (,template-mode-types-regexp . font-lock-type-face)
        (,template-mode-functions-regexp . font-lock-function-name-face)))

(provide 'template-mode)

;;; end of file
