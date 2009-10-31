;; Ada mode keys, extensions to gnu ada-mode

;; loaded after ada-mode; load rest of ada-mode utils, then fix
;; various things

;; additional ada-mode stuff
(require 'ada-xref)
(require 'ada-prj)
(require 'gnat-fix-error)
(load "ada-mode-patches")

;; ;;; auto-loads and associated fixes
;; (autoload 'ada-customize "ada-prj" "customize ada project files" t)
;; (eval-after-load 'ada-prj
;;   (remove-hook 'ada-mode-hook 'ada-prj-add-ada-menu))

;;;; supplemental packages

;;; Align
(require 'align) ; so ada-mode will set the appropriate stuff


; ignore gnat library files
(add-to-list 'completion-ignored-extensions ".ali")
(add-to-list 'completion-ignored-extensions ".o")

(setq ada-other-file-alist
      ;; First element is regexp, matches are not.
      (list
       ;; my and gnade gnatprep file extensions.
       (list (regexp-quote ".ads")
             (list ".adb" ".adb.gp" ".gpb"))
       (list (regexp-quote ".adb")
             (list ".ads" ".ads.gp"))
       (list (regexp-quote ".gpb")
             (list ".ads"))
       ))

;;; hooks

;; ada-xref adds this, but we don't want all of it
;;(remove-hook 'ada-mode-hook 'ada-xref-initialize)


;;(add-hook 'ada-mode-hook 'ada-find-file-setup)

; menu stuff added to ada-mode-hook below

;; ada-mode variables

; prompt for project file when first required
(setq ada-always-ask-project t)

;; remove trailing spaces, untabify
;;(setq ada-clean-buffer-before-saving t)

;; overwrite tree file if it exists, no file header
(setq ada-gnatstub-opts "-q -hs -t -I${src_dir}")

;; ada-xref variables

(setq ada-xref-confirm-compile nil) ; don't want chance to change make command

(setq ada-xref-create-ali nil) ; don't try to compile if xref doesn't find a current ali file.

(setq ada-xref-other-buffer nil); let me manage my windows!

;;; adjust settings for various projects
(defun sal-ada-mode-setup ()
  ;; indentation; use ada-mode defaults, so AdaCore doesn't complain
  ;; when I submit code. Well, except for:
  (make-variable-buffer-local 'ada-when-indent)  ;; 'when' in a case statement
  (make-variable-buffer-local 'ada-label-indent) ;; label indent

  (let ((case-fold-search t))
    (cond

     ((string-match "gnavi" default-directory)
      ;; We are editing GNAVI code, so use strict AdaCore style
      (setq ada-language-version 'ada95)
      )

     (t
      ;; my projects
      (setq ada-when-indent 0)
      (setq ada-label-indent 0)
      (setq ada-language-version 'ada05)
      )
     ))

  )

;(add-hook 'ada-mode-hook 'sal-ada-mode-setup)
;;; custom functions

(defun ada-adjust-case-current-identifier ()
  "Auto-case the identifier the point is on."
  (interactive)
  (let ((start (point)))
    ;; apparently nesting save-excursion doesn't work, so do it ourselves!
    (if (= (char-syntax (char-after)) ?w)
        ;; in a word; move to end first
        (progn
          (forward-word 1)
          (ada-adjust-case))
      ;; else move to end of previous word
      (progn
        (forward-word -1)
        (forward-word 1)
        (ada-adjust-case)) )
    (goto-char start)))

;;; custom formatting stuff
(defun ada-make-package-spec ()
  "Create an Ada package spec in the current buffer, based on the buffer's filename"
  (interactive)
  (let (start-point
        package-name)
    (insert "{compilation_unit_header}")
    (newline 1)
    (insert "package ")
    (setq start-point (point))
    (gnat-fix-insert-unit-name (gnat-unit-name-from-file-name (buffer-file-name)))
    (setq package-name (buffer-substring start-point (point)))
    (insert " is")
    (newline 1)
    (insert "   [elaboration_pragma]")
    (newline 2)
    (insert "   [basic_declarative_item]...")
    (newline 2)
    (insert "end " package-name ";")
    (newline)
    (goto-char (point-min))
    (forward-char 1)
    ))

;; expand bodies in place, not in other file
(defun ada-make-subprogram-body ()
  "make one dummy subprogram body from spec surrounding point"
  (interactive)
  (let ((found (re-search-backward ada-procedure-start-regexp nil t)))
    (if found
        (ada-gen-treat-proc)
      (error "Not in subprogram spec"))))

;; Make subprogram bodies the way I want them
;; Only used for single bodies; gnatstub is used for whole package bodies.
(defun ada-gen-treat-proc ()
  "Make dummy body of a procedure/function specification. match-data
must contain results from a search for ada-procedure-start-regexp."
  (let ((func-found (equal "function" (match-string 4)))
        (procname (match-string 5)))

    ;; goto end of procname before we delete stuff; match data is not
    ;; adjusted by deletions.
    (goto-char (match-end 5))

    ;; delete '[not] overriding' if present
    (if (match-string 2)
        (delete-region (match-beginning 2) (match-beginning 4)))

    ;; skip over parameterlist, if present
    (unless (looking-at "[ \t\n]*\\(;\\|return\\)")
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
          )))

    ;; look for next non WS
    (cond
     ((looking-at "[ \t]*;")
      ;; we are processing a copied spec; change to a body
      (delete-region (match-beginning 0) (match-end 0)) ;; delete the ';'
      (ada-indent-newline-indent)
      (insert " is begin ")
      (ada-indent-newline-indent)
      (ada-indent-newline-indent)
      (insert "end ")
      (insert procname)
      (insert ";")
      (ada-indent-newline-indent)
      (forward-line -2)
      (ada-indent-current)
      )

     ((looking-at "[ \t\n]*is")
      ;; already a body; do nothing
      )

     ((looking-at "[ \t\n]*rename")
      ;; a copied rename; if copied from spec, should delete. But it
      ;; might have been added to rename a private subprogram, so do
      ;; nothing.
      )

     (t
      (message "unknown subprogram syntax"))
     ) ;; cond
    ))

;; movement
;; things for next/previous page
(defcustom ada-thing-regexp
  (list
   ;; 0: second level indented stuff
   "^      \\(begin\\|function\\|generic\\|overriding\\|package\\|procedure\\|task\\|type\\)\\>"

   ;; 1: first level stuff
   "^   \\(begin\\|function\\|generic\\|overriding\\|package\\|procedure\\|task\\|type\\)\\>"

   ;; 2: exactly 10 dashes; avoid box headers
   "\\(^   ----------$\\)"

   ;; 3: top level packages, library procedures
   "^begin\\|^end\\|^generic\\|^package\\|^private\\|^procedure")
  "*List of regexp for movement with various levels. First level moves
to anything in entire list. Second level skips things in first
element. Third level skips things in second element, etc. Cursor is
placed at beginning of first match."
  :type '(repeat (choice :tag "regexp"))
  :group 'ada)

;;; printing
(autoload 'ps-print-preprint "ps-print" "" t)

(defun ada-print-buffer (&optional filename)
  "Print current buffer the way I want it. If optional FILENAME, save PostScript to that file."
  ;; Assume all ps options are at default, change those we need to. We
  ;; have to print a buffer, not a file, to let font-lock set the
  ;; colors. In batch mode, we have to explicitly enable colors and
  ;; call font-lock.
  (interactive
   (list (ps-print-preprint current-prefix-arg)))
  (let ((ps-landscape-mode t)
        (ps-number-of-columns 1)
        (ps-n-up-on nil)
        (ps-print-color-p t)
        (ps-font-size 10.0)
        (ps-line-number t)
        (ps-print-header-frame nil) )
    (add-to-list 'ps-lpr-switches "-color")
    (font-lock-fontify-buffer)
    (ps-print-buffer-with-faces filename)))

(defun ada-ps-buffer (&optional buffer)
  "Save PostScript rendition of BUFFER (default current) to buffer-file-name.ps"
  (interactive)
  (if (not buffer)
      (setq buffer (current-buffer)))

  (ada-print-buffer (concat (buffer-file-name) ".ps")))

;;; keys

;; format
(define-key ada-mode-map "\C-c;"    'ada-comment-box-header)
(define-key ada-mode-map "\M-["     'ada-align)
(define-key ada-mode-map "\C-\M-\\" 'ada-indent-region); indent-region
(define-key ada-mode-map [(control c) (b)] 'ada-make-subprogram-body)
;; (define-key ada-mode-map "\M-c"     'ada-calc-record-rep)
;; (define-key ada-mode-map "\C-c\C-d" 'ada-goto-declaration-at-point)
;; (define-key ada-mode-map "\C-c\M-d" 'ada-xref-goto-previous-reference)
;; (define-key ada-mode-map "\C-c\C-s" 'ada-make-package-spec)
;; (define-key ada-mode-map "\M-\C-w"  'ada-capitalize-word)

;; movement (M-S-* same as M-* for these keys on Windows keyboard)
(define-key ada-mode-map [prior]
  '(lambda () (interactive) (sal-prev-meta-thing 0 ada-thing-regexp)))
(define-key ada-mode-map [M-prior]
  '(lambda () (interactive) (sal-prev-meta-thing 1 ada-thing-regexp)))
(define-key ada-mode-map [C-prior]
  '(lambda () (interactive) (sal-prev-meta-thing 2 ada-thing-regexp)))
(define-key ada-mode-map [M-C-prior]
  '(lambda () (interactive) (sal-prev-meta-thing 3 ada-thing-regexp)))
(define-key ada-mode-map [next]
  '(lambda () (interactive) (sal-next-meta-thing 0 ada-thing-regexp)))
(define-key ada-mode-map [M-next]
  '(lambda () (interactive) (sal-next-meta-thing 1 ada-thing-regexp)))
(define-key ada-mode-map [C-next]
  '(lambda () (interactive) (sal-next-meta-thing 2 ada-thing-regexp)))
(define-key ada-mode-map [M-C-next]
  '(lambda () (interactive) (sal-next-meta-thing 3 ada-thing-regexp)))

(define-key ada-mode-map [f5] 'misc-compile)
(define-key ada-mode-map [M-f5] 'ada-compile-current)
(define-key ada-mode-map [C-f5] 'ada-compile-application)
(define-key ada-mode-map [(meta g) (meta s)] 'gnat-fix-compiler-error)
(define-key ada-mode-map [f7] 'ada-find-references)
;; f8 free
(define-key ada-mode-map [f9] nil); ada-mode: 'ada-compile-application
(define-key ada-mode-map [f10] nil)     ; ada-mode: 'next-error)

(setq ada-mode-extra-prefix "\C-c")

;;; something in here is killing this
(setq yas/trigger-key [(j)])


;; Add to default ada-mode menu. This must be done in ada-mode-hook,
;; since ada-mode recreates the menu for each buffer.
(defun ada-prj-reload-project-file ()
  "Reload current project file."
  (interactive)
  (ada-reread-prj-file ada-prj-default-project-file) )

;; (defun ada-menu-setup ()
;;   "customize ada-menu"
;;   (define-key-after (lookup-key ada-mode-map [menu-bar Ada Project]) [reload]
;;     '("Reload" . ada-prj-reload-project-file) 'Load)

;;   (define-key-after (lookup-key ada-mode-map [menu-bar Ada]) [print]
;;     '("Print" . ada-print-buffer) 'Print))

;; (add-hook 'ada-mode-hook 'ada-menu-setup)

; end of file
