;; @(#) gnat-fix-error.el --- utilities for automatically fixing
;; errors reported by the GNAT Ada compiler.

;; Copyright (C) 1999-2007 Stephen Leake.

;; Author     : Stephen Leake      <Stephen_Leake@stephe-leake.org>
;; Maintainer : Stephen Leake      <Stephen_Leake@stephe-leake.org>
;; Web site   : http://www.stephe-leake.org/
;; CVS version:   $Revision: 1.69 $
;; Keywords   : languages ada error

;; This file is not part of GNU Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Notes

;; You must use -gnatf (extended error messages) for this to work
;; well.

(require 'ada-mode)

(defgroup gnat-fix nil
  "Customization of gnat-fix"
  :version "21.3"
  :group 'ada)

(defcustom gnat-fix-sort-context-clause t
  "*If non-nil, sort context clause when inserting 'with'"
  :type 'boolean
  :group 'gnat-fix)

;;; General functions

(defconst gnat-identifier-regexp-base
  "[a-zA-Z][a-zA-Z0-9_]*[a-zA-Z0-9]*"
  "Base regexp for an identifier")

(defconst gnat-identifier-regexp
  (concat "\\(" gnat-identifier-regexp-base "\\)")
  "Regexp for extracting an identifier")

(defconst gnat-name-regexp
  (concat "\\(" gnat-identifier-regexp-base "\\(." gnat-identifier-regexp-base "\\)*" "\\)")
  "Regexp for extracting a full name")

(defconst gnat-fix-compilation-unit-start-regexp
  "^separate\\|^package\\|^private package\\|^procedure\\|^function\\|^generic"
  "regexp matching start of compilation unit, after context clauses.
Assumes keyword is at left margin.")

(defconst gnat-fix-context-clause-start-regexp
  "^with"
  "regexp matching start of context clauses. Assumes keyword is at
left margin.")

(defun gnat-fix-add-with-clause (package-name)
  "Add a with_clause for PACKAGE_NAME, at the start of the compilation
unit in the current buffer. If gnat-fix-sort-context-clause, sort the context clauses
using sort-lines."
  (let (context-clause-start compilation-unit-start)
    (goto-char (point-min))
    (ada-search-ignore-string-comment gnat-fix-context-clause-start-regexp nil)
    (beginning-of-line)
    (setq context-clause-start (point))
    ;; If there is no context clause, context-clause-start will be at end of buffer.
    (goto-char (point-min))
    (ada-search-ignore-string-comment gnat-fix-compilation-unit-start-regexp nil)
    (beginning-of-line)
    (insert "with ")
    (gnat-fix-insert-unit-name package-name)
    (insert ";\n")
    (setq compilation-unit-start (point))
    (if (and (> compilation-unit-start context-clause-start)
             gnat-fix-sort-context-clause)
        (sort-lines nil context-clause-start compilation-unit-start))))

(defun gnat-fix-extend-with-clause (child-name)
  "Assuming point is in a full name, just before CHILD-NAME, add or
extend a with_clause to include CHILD-NAME."
  (let ((parent-name-end (point)))
    ;; Find the full parent name; skip back to whitespace, then match
    ;; the name forward.
    (search-backward-regexp "\\s-")
    (forward-char 1) ; skip forward over found whitespace
    (search-forward-regexp ada-name-regexp parent-name-end)
    (let ((parent-name (match-string 0)))
      (goto-char (point-min))
      (ada-search-ignore-string-comment (concat "with " parent-name ";") nil)
      (if (eobp)
          ;; oops, no with_clause found (we are in a package body, with_clause for parent is in spec).
          ;; insert a new one
          (gnat-fix-add-with-clause (concat parent-name "." child-name))
        (progn
          (forward-char -1) ; skip back over semicolon
          (insert "." child-name))))))

(defun gnat-fix-insert-unit-name (unit-name)
  "Insert (at point) and capitalize unit-name, normally gotten from
file-name, and thus all lower-case."
  (let ((start-point (point))
        search-bound)
    (insert unit-name)
    (setq search-bound (point))
    (insert " ") ; separate from following words, if any, for ada-adjust-case-identifier
    (goto-char start-point)
    (while (search-forward "." search-bound t)
      (forward-char -1)
      (ada-adjust-case-identifier)
      (forward-char 1))
    (goto-char search-bound)
    (ada-adjust-case-identifier)
    (delete-char 1)))

(defun gnat-fix-subprogram-decl-end ()
  "If at or in a subprogram declaration, move to the end of the declaration."
  ; ada-procedure-start-regexp is anchored at a line beginning
  (beginning-of-line)
  (if (not (looking-at ada-procedure-start-regexp))
      (search-backward-regexp ada-procedure-start-regexp))

  (let ((func-found (equal "function" (match-string 2)))
        (procname (match-string 3)))

    ;; goto end of procname
    (goto-char (nth 1 (match-data)))

    ;; skip over parameterlist, if present
    (unless (looking-at "[ \t\n]*\\(;\\|return\\|is\\)")
      (forward-sexp))

    ;; if function, skip over 'return' and result type.
    (if func-found
        (progn
          (forward-word 1) ; return
          (re-search-forward ada-name-regexp))
      ;; else warn if 'return' with procedure
      (save-excursion
        (forward-word 1)
        (forward-word -1)
        (if (looking-at "return")
            (error "Invalid 'return' with procedure")
          )))))

;;; ada project file stuff

(defun gnat-fix-prj-show-source (file-name line)
  "Show source file at line (string), using ff-get-file-name, with
search path from current buffer's ada project file."
  (ada-require-prj-file)
  (find-file (ff-get-file-name ada-prj-src-dir file))
  (goto-line (string-to-number line)))

;;; gnat specific stuff

(defconst gnat-file-line-regexp
  "\\([a-z0-9-_./:]+\\):\\([0-9]+\\)"
  "match gnat-style file and line, possibly including full path due to preprocessor")

(defconst gnat-quoted-name-regexp
  "\"\\([a-zA-Z0-9_.']+\\)\""
  "regexp to extract the quoted names in error messages")

(defconst gnat-quoted-punctuation-regexp
  "\"\\([,:;=()|]+\\)\""
  "regexp to extract quoted punctuation in error messages")

(defconst gnat-quoted-operator-regexp
  "\\(\"[+*/-]+\"\\)"
  "regexp to extract quoted operator in error messages")

(defconst gnat-predefined-package-alist
  '(("a-textio" . "Ada.Text_IO")
    ("a-chahan" . "Ada.Characters.Handling")
    ("a-comlin" . "Ada.Command_Line")
    ("a-except" . "Ada.Exceptions")
    ("a-numeri" . "Ada.Numerics")
    ("a-string" . "Ada.Strings")
    ("g-socket" . "GNAT.Sockets")
    ("interfac" . "Interfaces")
    ("i-c" . "Interfaces.C")
    ("i-cstrin" . "Interfaces.C.Strings")
    ("s-stoele" . "System.Storage_Elements")
    ("unchconv" . "Unchecked_Conversion") ; Ada 83 name
    )
  "Alist (filename . package name) of GNAT file names for predefined Ada packages.")

(defun gnat-unit-name-from-file-name (file-name)
  "Return the Ada unit name corresponding to FILENAME, using gnat
default naming convention. Special case the predefined packages, since
gnat truncates them to 8 characters."
  (let* ((nondirname (file-name-nondirectory file-name))
         (unit-name
          (if (equal (file-name-extension file-name t) ".gp")
              (file-name-sans-extension (file-name-sans-extension nondirname))
            (file-name-sans-extension nondirname)) )
         (predefined (cdr (assoc unit-name gnat-predefined-package-alist))))

    (if predefined
        predefined
      (while (string-match "-" unit-name)
        (setq unit-name (replace-match "." nil t unit-name)))
      unit-name)))

(defun gnat-fix-compiler-error ()
  "Attempt to fix the current compiler error. Assumes point is at
error line and column (ie, positioned by 'next-error). Leave point at
fix."
  (interactive)

  (let ((source-buffer (current-buffer))
        (source-window (selected-window))
        compilation-buffer-start)

    ;; Goto the error message. Sometimes (the very first time the
    ;; compilation buffer is used?) point is not at the right place in
    ;; the compilation buffer. This sequence seems to fix it.
    (pop-to-buffer compilation-last-buffer)
    (pop-to-buffer source-buffer)
    (set-buffer compilation-last-buffer)
    (if (not (eq source-window (selected-window)))
        (error "two windows open on source buffer; please close one"))

    ;; Save starting point in compilation buffer, in case we need to
    ;; repeat operation. We don't use save-excursion, because we want
    ;; point to change in source-buffer
    (setq compilation-buffer-start (point))
    (if (not (or (search-forward "error: " (line-end-position) t) (search-forward " " (line-end-position) t)))
        (error "gnat-fix-error: space not found from point %d" (point)))

    ;; recognize it, handle it
    (unwind-protect
        (cond
         ;; This list will get long, so let's impose some order.
         ;;
         ;; First expressions that start with a named regexp, alphabetical by variable name.
         ;;
         ;; Then expressions that start with a string, alphabetical by string.
         ;;
         ;; Then style errors.
         ;;
         ;; Then auto_text_io errors

         ((looking-at (concat gnat-quoted-name-regexp " is not visible"))
          (let ((ident (match-string 1)))
            (next-line 1)
            (let ((unit-file
                   (cond
                    ((looking-at (concat ".*at " gnat-file-line-regexp ".*at " gnat-file-line-regexp))
                      (match-string 3))
                    ((looking-at (concat "non-visible \\((private) \\)?declaration at " gnat-file-line-regexp))
                      (match-string 2)))))
              (pop-to-buffer source-buffer)
              ;; We either need to add a with_clause for a package, or
              ;; prepend the package name here (or add a use clause, but I
              ;; don't want to do that automatically). unit-name may be
              ;; only the prefix of the real package name, but in that
              ;; case we'll be back after the next compile; no way to get
              ;; the full package name (without the function/type name) now.
              (let ((unit-name (gnat-unit-name-from-file-name unit-file)))
                (cond
                 ((looking-at (concat unit-name "\\."))
                  (gnat-fix-add-with-clause unit-name))
                 (t
                  (gnat-fix-insert-unit-name unit-name)
                  (insert ".")))))))

         ((looking-at (concat gnat-quoted-name-regexp " is undefined"))
          ;; We either need to add a with_clause for a package, or
          ;; something is spelled wrong. Check next line for spelling error.
          (let ((unit-name (match-string 1))
                correct-spelling)
            (save-excursion
              (next-line 1)
              (if (looking-at (concat "possible misspelling of " gnat-quoted-name-regexp))
                  ;; correctable misspelling
                  (progn
                    (setq correct-spelling (match-string 1))
                    (pop-to-buffer source-buffer)
                    (search-forward unit-name)
                    (replace-match correct-spelling)
                    );; progn

                ;; assume missing with
                (pop-to-buffer source-buffer)
                (gnat-fix-add-with-clause unit-name)))))

         ((looking-at (concat gnat-quoted-name-regexp " is not a component of \\(type\\|the aggregate subtype\\)"))
          ;; Check next line for spelling error.
          (let ((unit-name (match-string 1))
                correct-spelling)
            (save-excursion
              (next-line 1)
              (if (looking-at (concat "possible misspelling of " gnat-quoted-name-regexp))
                  ;; correctable misspelling
                  (progn
                    (setq correct-spelling (match-string 1))
                    (pop-to-buffer source-buffer)
                    (search-forward unit-name)
                    (replace-match correct-spelling)
                    );; progn

                ;; else can't deal with it
                (error "error not recognized"))
                )))

         ((looking-at (concat gnat-quoted-name-regexp " not declared in " gnat-quoted-name-regexp))
          (let ((child-name (match-string 1))
                correct-spelling)
            ;; First check for "possible misspelling" message
            (save-excursion
              (next-line 1)
              (if (looking-at (concat "possible misspelling of " gnat-quoted-name-regexp))
                  ;; correctable misspelling
                  (progn
                    (setq correct-spelling (match-string 1))
                    (pop-to-buffer source-buffer)
                    (search-forward child-name)
                    (replace-match correct-spelling)
                    );; progn
                ;; else guess that "child" is a child package, and extend the with_clause
                (pop-to-buffer source-buffer)
                (gnat-fix-extend-with-clause child-name)))))

         ((or
           (looking-at (concat gnat-quoted-punctuation-regexp " should be " gnat-quoted-punctuation-regexp))
           (looking-at (concat gnat-quoted-punctuation-regexp " illegal here, replaced by "
                               gnat-quoted-punctuation-regexp))
           )
          (let ((bad (match-string-no-properties 1))
                (good (match-string-no-properties 2)))
            (pop-to-buffer source-buffer)
            (looking-at bad)
            (delete-region (match-beginning 0) (match-end 0))
            (insert good)))

         ;; Now expressions that start with a string, alphabetical by string ("warning" at end).

         ((looking-at "\"aliased\" should be before \"constant\"")
          (pop-to-buffer source-buffer)
          (delete-char 8)
          (backward-word 1)
          (insert "aliased "))

         ((looking-at (concat "\"end " gnat-name-regexp ";\" expected"))
          (let ((expected-name (match-string 1)))
            (pop-to-buffer source-buffer)
            (if (looking-at (concat "end " gnat-name-regexp ";"))
                (progn
                  (goto-char (match-end 1))   ; just before ';'
                  (delete-region (match-beginning 1) (match-end 1)))
              ;; else we have just 'end;'
              (forward-word 1)
              (insert " "))
            (insert expected-name)))

         ((looking-at "extra \".\" ignored")
            (set-buffer source-buffer)
            (delete-char 1))

         ((looking-at "extra right paren")
          (pop-to-buffer source-buffer)
          (delete-char 1))

         ((looking-at (concat "expected \\(private \\)?type " gnat-quoted-name-regexp))
          (let ((type (match-string 2)))
            (next-line 1)
            (if (or (looking-at "found type access")
                    (looking-at "found type .*_Access_Type"))
                ;; assume just need '.all'
                (progn
                  (pop-to-buffer source-buffer)
                  (forward-word 1)
                  (insert ".all"))
              ;; Not "found type access"
              (pop-to-buffer source-buffer)
              (if (looking-at "\\.")
                  (progn
                    (message "move to beginning of expression and repeat")
                    (ding))
                (progn
                  (insert type " (")
                  (message "move to end of expression and insert matching paren")
                  (ding))))))

         ((looking-at "if qualified expression was meant, use apostrophe")
            (progn
              (set-buffer source-buffer)
              (delete-backward-char 1)
              (insert "'")))

         ((looking-at (concat "missing " gnat-quoted-punctuation-regexp))
          (let ((stuff (match-string-no-properties 1)))
              (set-buffer source-buffer)
              (insert stuff)))

         ((looking-at (concat "missing #"))
            (set-buffer source-buffer)
            (insert "#"))

         ((looking-at (concat "missing body for " gnat-quoted-name-regexp " declared at " gnat-file-line-regexp))
          (let ((file (match-string 2))
                (line (match-string 3)))
            (set-buffer source-buffer)  ; for ada project file
            (gnat-fix-prj-show-source file line)))

         ((looking-at "missing semicolon after #end if")
          (progn
            (set-buffer source-buffer)
            (insert ";")))

         ((looking-at (concat "missing string quote"))
          (progn
            (set-buffer source-buffer)
            (insert "\"")))

         ((looking-at (concat "missing with_clause for child unit " gnat-quoted-name-regexp))
          (let ((child-name (match-string-no-properties 1)))
            (pop-to-buffer source-buffer)
            (gnat-fix-extend-with-clause child-name)))

         ((or (looking-at (concat "missing with for " gnat-quoted-name-regexp)) ; gnat 5.04
              (looking-at (concat "missing \"with \\([a-zA-Z0-9_.']+\\);\"")))  ; gnat 5.05
          (let ((package-name (match-string-no-properties 1)))
            (pop-to-buffer source-buffer)
            (gnat-fix-add-with-clause package-name)))

         ((looking-at (concat "misspelling of " gnat-quoted-name-regexp))
          ;; correctable misspelling
          (progn
            (setq correct-spelling (match-string 1))
            (pop-to-buffer source-buffer)
            (kill-word 1)
            (insert correct-spelling)
            ))

         ((looking-at (concat "no selector " gnat-quoted-name-regexp))
          ;; Check next line for spelling error.
          (let ((unit-name (match-string 1))
                correct-spelling)
            (save-excursion
              (next-line 1)
              (if (looking-at (concat "possible misspelling of " gnat-quoted-name-regexp))
                  ;; correctable misspelling
                  (progn
                    (setq correct-spelling (match-string 1))
                    (pop-to-buffer source-buffer)
                    (search-forward unit-name)
                    (replace-match correct-spelling)
                    );; progn

                ;; else can't deal with it
                (error "error not recognized"))
                )))

         ((looking-at "no space allowed here")
          (progn
            (set-buffer source-buffer)
            (delete-char 1)))

         ((or
           (looking-at (concat "not fully conformant with declaration at " gnat-file-line-regexp))
           (looking-at (concat "not type conformant with declaration at " gnat-file-line-regexp)))
          (let ((file (match-string 1))
                (line (match-string 2)))
            (pop-to-buffer source-buffer) ; for ada project file
            (gnat-fix-prj-show-source file line)))

         ((looking-at "numeric literal cannot start with point")
          (progn
            (pop-to-buffer source-buffer)
            (insert "0")))

         ;; types in pre-defined packages don't give file.
         ((looking-at (concat "operator for \\(private \\)?type " gnat-quoted-name-regexp
                              "\\( defined at " gnat-file-line-regexp "\\)?"))
          (let ((file (match-string 4))
                (type (match-string 2)))
            (pop-to-buffer source-buffer)
            ;; search back to either start of current declarative region,
            ;; or end of current subprogram's declarative region
            (ada-search-ignore-string-comment "\\<begin\\>\\|\\<is\\>" t)
            (cond
             ((looking-at "is")
              ;; might be a case statement
              (while (progn
                       (ada-search-ignore-string-comment (concat "\\<case\\>\\|" ada-subprog-start-re) t)
                       (if (looking-at "case")
                           t
                         (ada-search-ignore-string-comment "is")
                         nil)))
              (end-of-line))

             ((looking-at "begin")
              (forward-line -1)
              (end-of-line))
             )
            (newline-and-indent)
            (insert "use type ")
            (if file
                (progn
                  (gnat-fix-insert-unit-name (gnat-unit-name-from-file-name file))
                  (insert ".")))
            (insert type ";")))

         ((looking-at "prefix of dereference must be an access type")
          (pop-to-buffer source-buffer)
          ;; point is after '.' in '.all'
          (delete-region (- (point) 1) (+ (point) 3)))

         ((looking-at "\"procedure\" should be \"function\"")
          (pop-to-buffer source-buffer)
          (delete-region (point) (+ (point) 9))
          (insert "function"))

         ((looking-at (concat "undefined selector for type " gnat-quoted-name-regexp
                              " defined at " gnat-file-line-regexp))
          (let ((file (match-string 2))
                (line (match-string 3)))
            (pop-to-buffer source-buffer) ; for ada project file
            (gnat-fix-prj-show-source file line)))

         ((looking-at "unexpected right parenthesis")
            (set-buffer source-buffer)
            (delete-char 1))

         ((looking-at "unexpected semicolon ignored")
            (set-buffer source-buffer)
            (delete-char 1))

         ((looking-at (concat "warning: " gnat-quoted-name-regexp " is not modified, could be declared constant"))
          (pop-to-buffer source-buffer)
          (ada-search-ignore-string-comment ": ")
          ;; "aliased" must be before "constant", so check for it
          (if (looking-at "aliased")
              (progn
                (forward-word 1)
                (forward-char 1)))
          (insert "constant "))

         ((or
           (looking-at (concat "warning: constant " gnat-quoted-name-regexp " is not referenced"))
           (looking-at (concat "warning: variable " gnat-quoted-name-regexp " is assigned but never read"))
           (looking-at (concat "warning: variable " gnat-quoted-name-regexp " is not referenced")))
          (let ((param (match-string 1)))
            (pop-to-buffer source-buffer)
            (ada-search-ignore-string-comment ";")
            (newline-and-indent)
            (insert "pragma Unreferenced ("
                    param
                    ");")))

         ((looking-at (concat "warning: formal parameter "
                              gnat-quoted-name-regexp
                              " is not referenced"))
          (let ((param (match-string 1)))
            (pop-to-buffer source-buffer)
            (ada-search-ignore-string-comment "is")
            (newline-and-indent)
            (insert "pragma Unreferenced ("
                    param
                    ");")))

         ((looking-at (concat "warning: \\(instantiation of\\|call to\\) "
                              gnat-quoted-name-regexp
                              " may raise Program_Error"))
          (next-line 1)
          (if (looking-at (concat "warning: missing pragma Elaborate_All for " gnat-quoted-name-regexp))
              (let ((unit (match-string 1)))
                (pop-to-buffer source-buffer)
                (goto-char (point-min))
                (ada-search-ignore-string-comment unit nil)
                (forward-line)
                (insert "pragma Elaborate_All (")
                (gnat-fix-insert-unit-name unit)
                (insert ");\n"))
            (message "error not recognized")))

         ((or
           (looking-at (concat "warning: no entities of "
                               gnat-quoted-name-regexp
                               " are referenced$"))
           (looking-at (concat "warning: unit "
                               gnat-quoted-name-regexp
                               " is never instantiated$"))
           (looking-at (concat "warning: unit "
                               gnat-quoted-name-regexp
                               " is not referenced$"))
           (looking-at "warning: redundant with clause"))
          ;; just delete the 'with'; assume it's on a line by itself.
          (pop-to-buffer source-buffer)
          (beginning-of-line)
          (delete-region (point) (progn (forward-line 1) (point))))

         ((looking-at "warning: redundant parentheses")
          ;; delete them
          (pop-to-buffer source-buffer)
          (save-excursion
            (forward-sexp)
            (delete-char -1))
          (delete-char 1))

         ;; Now style errors.
         ((looking-at "(style) bad capitalization, mixed case required")
          (progn
            (set-buffer source-buffer)
            (ada-capitalize-word)))

         ((or (looking-at "(style) bad indentation")
              (looking-at "(style) bad column")
              (looking-at "(style) incorrect layout"))
          (progn
            (set-buffer source-buffer)
            (ada-indent-current)))

         ((or (looking-at (concat "(style) bad identifier casing, should be " gnat-quoted-name-regexp)) ; gnat 3.12
              (looking-at (concat "(style) bad casing of " gnat-quoted-name-regexp))) ; gnat 3.13
          (let ((correct (match-string-no-properties 1))
                end)
            ;; gnat leaves point on first bad character, but we need to replace the whole word
            (set-buffer source-buffer)
            (skip-syntax-backward "w")
            (setq end (point))
            (skip-syntax-forward "w")
            (delete-region (point) end)
            (insert correct)))

         ((looking-at "(style) bad casing for entity in Standard")
          ;; gnat leaves point on first bad character, so just capitalize it.
          (set-buffer source-buffer)
          (ada-adjust-case-current-identifier))

         ((or
           (looking-at (concat "(style) \"end " gnat-quoted-operator-regexp))
           (looking-at "(style) \"end \\([a-zA-Z0-9_.]+\\)\" required"))
          (let ((correct (match-string-no-properties 1)))
            (set-buffer source-buffer)
            (skip-syntax-forward "w")
            (insert (concat " " correct))))

         ((looking-at "(style) \"end\" in wrong column")
          (progn
            (set-buffer source-buffer)
            (ada-indent-current)))

         ((looking-at "(style) \"exit \\([a-zA-Z0-9_.]+\\)\" required")
          (let ((correct (match-string-no-properties 1)))
            (set-buffer source-buffer)
            (skip-syntax-forward "w")
            (insert (concat " " correct))))

         ((looking-at "(style) horizontal tab not allowed")
          (progn
            (set-buffer source-buffer)
            (delete-char 1)
            (ada-indent-current)))

         ((looking-at "(style) misplaced \"then\"")
          (let (then-pos)
            (set-buffer source-buffer)
            (setq then-pos (point))
            (previous-line 1)
            (if (looking-at "if\\|elsif")
                (progn
                  ;; delete new-line and whitespace before "then"
                  (end-of-line)
                  (delete-region (point) then-pos)
                  (insert " "))
              (error "can't fix \"then\""))))

         ((looking-at "(style) reserved words must be all lower case")
          (progn
            (set-buffer source-buffer)
            ;; error places point on first char in word
            (downcase-word 1)))

         ((or (looking-at "(style) space not allowed")
              (looking-at "(style) trailing spaces not permitted")
              (looking-at "(style) form feed not allowed"))
          (progn
            (set-buffer source-buffer)
            ;; Error places point on space. More than one trailing
            ;; space should be fixed by ada-remove-trailing-spaces,
            ;; once the file is modified. The problem is the file is
            ;; not in ada-mode for some reason, so the filter doesn't
            ;; run. So fix that.
            (ada-mode)
            (delete-char 1)))

         ((looking-at "(style) space required")
          (progn
            (set-buffer source-buffer)
            (insert " ")))

         ((looking-at "(style): subprogram body has no previous spec")
          (progn
            (set-buffer source-buffer)
            (let ((start (point))
                  (end (progn (gnat-fix-subprogram-decl-end) (point))))
              (goto-char start)
              (insert-string (buffer-substring start end))
              (insert ";")
              (newline)
              (newline-and-indent) )))

         ((looking-at "(style) two spaces required") ; GNAT 5.05
          (progn
            (set-buffer source-buffer)
            (insert "  ")))

         ;;; other tools

         ;; auto_text_io
         ((looking-at "auto_text_io: not supported")
          (progn
            (set-buffer source-buffer)
            (goto-char (line-beginning-position))
            (newline-and-indent)
            (previous-line 1)
            (indent-according-to-mode)
            (insert "--  auto_text_io: ignore")
            ))

         (t
          (error "error not recognized"))

         )    ;; end of (cond
      ;; restore compilation buffer point
      (set-buffer compilation-last-buffer)
      (goto-char compilation-buffer-start)
      )))

(provide 'gnat-fix-error)
;; end of file
