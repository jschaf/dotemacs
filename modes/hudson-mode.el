;;; hudson-mode.el --- A major mode for editing Hudson programs

;; Author: Joe Schafer
;; Created: Apr 2009
;; Keywords: hudson languages functional oop

(defconst hudson-version "0.30"
  "`hudson-mode' version number.")

;;; Commentary:

;; This is a major mode for editing Hudson programs.  Most of the
;; functionality is ported/stolen from python.el.  There is support
;; for imenu, compilation, and hideshow.

;;; Installation:

;; To install, just drop this file into a directory on your load-path.
;; There is no need to modify your `auto-mode-alist' because it is
;; autoloaded in this file.  You must add the location of hudson.jar
;; in order for compilation to succeed.

;;; Bug Reporting:

;; Look at the source ;)


;;; History:
;; 

;;; Todo:

;; Fix end-of-defun bug when there are trailing comments
;; Get `which-func-mode' working.
;; Get imenu to group functions, procedures and classes.
;; Have hideshow show class sub function and procedures.

;;; Code:

(require 'comint)
(require 'custom)

(defgroup hudson nil
  "Major mode for editing Hudson programs in Emacs."
  :group 'languages
  :prefix "hudson-")

(defcustom hudson-jar-file ""
  "Location of the hudson.jar file used to run Hudson files."
  :type 'string
  :group 'hudson)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.hud\\'" . hudson-mode))


;;;; Font Lock

(defvar hudson-font-lock-keywords
  ;; Keywords
  `(,(rx symbol-start
         (or
          "and" "assert" "class" "constant" "do" "else" "false" "fun"
          "function" "if" "inherit" "is" "not" "null" "or" "procedure"
          "ref" "return" "then" "true" "variable" "while")
         symbol-end)
    ;; Function and procedure names must begin with a lowercase letter
    (,(rx symbol-start (or "function" "procedure") (1+ space)
          (group (? ".") lower (0+ (or alnum "_"))))
     (1 font-lock-function-name-face))
    ;; Classes
    (,(rx line-start (0+ blank) "class" (1+ space)
          (group upper (0+ (or ?_ alnum))))
     (1 font-lock-type-face))
    ;; Subtypes (e.g "< Foldable")
    (,(rx "<" (1+ (0+ space) (? ",") (0+ space) (group upper (0+ (or ?_ alnum))))))
    ;; Top-level variable assignments
    (,(rx line-start (or "variable" "constant") (1+ space)
          (group lower (0+ (or alnum "_"))))
     (1 font-lock-variable-name-face))
    ;; Optional type annotations for variables and constants
    (,(rx symbol-start (or "variable" "constant")
          (1+ space) lower (0+ (or alnum "_"))
          (0+ space) ":" (0+ space)
          (group upper (0+ (or alnum "_"))))
     (1 font-lock-type-face))
    ;; Type checking using the ? operator
    (,(rx "?" (0+ space) (group upper (0+ (or alnum "_"))))
     (1 font-lock-type-face))
    ;; Unnamed numerical constants
    (,(rx not-wordchar (group (? "-") (1+ num)) word-end)
     (1 font-lock-constant-face))
    ;; include directives
    (,(rx line-start (group "#include" (1+ space) (1+ (or alnum "-" "_" "."))))
     (1 font-lock-preprocessor-face))))

(defvar hudson-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\177" 'hudson-backspace)
    (define-key map "\C-c<" 'hudson-shift-left)
    (define-key map "\C-c>" 'hudson-shift-right)
    (define-key map "\C-c\C-k" 'hudson-mark-block)
    (define-key map "\C-c\C-d" 'hudson-pdbtrack-toggle-stack-tracking)
    (define-key map "\C-c\C-n" 'hudson-next-statement)
    (define-key map "\C-c\C-p" 'hudson-previous-statement)
    (define-key map "\C-c\C-u" 'hudson-beginning-of-block)
    (define-key map "\C-c\C-c" 'hudson-compile-and-run)
    (define-key map "\C-j" 'hudson-newline-and-indent)
    (define-key map "\C-cj" 'hudson-delete-indentation)
    
    (easy-menu-define hudson-menu map "Hudson Mode menu"
      `("Hudson"
	:help "Hudson-specific Features"
	["Shift region left" hudson-shift-left :active mark-active
	 :help "Shift by a single indentation step"]
	["Shift region right" hudson-shift-right :active mark-active
	 :help "Shift by a single indentation step"]
	"-"
	["Mark block" hudson-mark-block
	 :help "Mark innermost block around point"]
	["Mark def/class" mark-defun
	 :help "Mark innermost definition around point"]
	"-"
	["Start of block" hudson-beginning-of-block
	 :help "Go to start of innermost definition around point"]
	["End of block" hudson-end-of-block
	 :help "Go to end of innermost definition around point"]
	["Start of def/class" beginning-of-defun
	 :help "Go to start of innermost definition around point"]
	["End of def/class" end-of-defun
	 :help "Go to end of innermost definition around point"]
	"-"
	("Templates..."
	 :help "Expand templates for compound statements"
	 :filter (lambda (&rest junk)
                   (abbrev-table-menu hudson-mode-abbrev-table)))))
    map))

(defvar hudson-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Give punctuation syntax to ASCII that normally has symbol
    ;; syntax or has word syntax and isn't a letter.
    (let ((symbol (string-to-syntax "_"))
	  (sst (standard-syntax-table)))
      (dotimes (i 128)
	(unless (= i ?_)
	  (if (equal symbol (aref sst i))
	      (modify-syntax-entry i "." table)))))
    (modify-syntax-entry ?$ "." table)
    (modify-syntax-entry ?% "." table)
    (modify-syntax-entry ?? "." table)
    ;; exceptions
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    table))

;;;; Utility methods

(defsubst hudson-in-string/comment ()
  "Return non-nil if point is in a Hudson literal (a comment or string)."
  ;; We don't need to save the match data.
  (nth 8 (syntax-ppss)))

(defun hudson-skip-comments/blanks (&optional backward)
  "Skip comments and blank lines.
BACKWARD non-nil means go backwards, otherwise go forwards.
Backslash is treated as whitespace so that continued blank lines
are skipped.  Doesn't move out of comments -- should be outside
or at end of line."
  (let ((arg (if backward
		 ;; If we're in a comment (including on the trailing
		 ;; newline), forward-comment doesn't move backwards out
		 ;; of it.  Don't set the syntax table round this bit!
		 (let ((syntax (syntax-ppss)))
		   (if (nth 4 syntax)
		       (goto-char (nth 8 syntax)))
		   (- (point-max)))
	       (point-max))))
    (forward-comment arg)))

(defun hudson-explicit-continuation-line-p ()
  "Return t if preceding line is an explicit continuation line.
An explicit continuation line ends with a double pound sign (##) that
is not in a comment."
  (save-excursion
    (beginning-of-line)
    (and
     (forward-line -1)               ;always true
     (looking-at hudson-continued-re))))

(defun hudson-continuation-line-p ()
  "Return t iff current line is a continuation line.
The only way to continue lines is with the explict double pound signs, so we simply return that result."
  (save-excursion
    (beginning-of-line)
    (hudson-explicit-continuation-line-p)))

(defun hudson-comment-line-p ()
  "Return non-nil if and only if current line has only a comment."
  (save-excursion
    (end-of-line)
    (when (eq 'comment (syntax-ppss-context (syntax-ppss)))
      (back-to-indentation)
      (looking-at (rx (or (syntax comment-start) line-end))))))

(defun hudson-blank-line-p ()
  "Return non-nil if and only if current line is blank."
  (save-excursion
    (beginning-of-line)
    (looking-at "\\s-*$")))

(defun hudson-open-block-statement-p (&optional bos)
  "Return non-nil if statement at point opens a block.
BOS non-nil means point is known to be at beginning of statement."
  (save-excursion
    (unless bos (hudson-beginning-of-statement))
    (looking-at (rx (and (or "if" "else" "while" "function" "procedure"
			     "class")
			 symbol-end)))))

(defun hudson-close-block-statement-p (&optional bos)
  "Return non-nil if current line is a statement closing a block.
BOS non-nil means point is at beginning of statement.
The criteria are that the line isn't a comment or in string and
 starts with keyword `return', `null' or `error'."
  (save-excursion
    (unless bos (hudson-beginning-of-statement))
    (back-to-indentation)
    (looking-at (rx (or "null" "return" "error")
		    symbol-end))))

(defun hudson-outdent-p ()
  "Return non-nil if current line should outdent a level."
  (save-excursion
    (back-to-indentation)
    (and (looking-at (rx (and (or "else")
			      symbol-end)))
	 (not (hudson-in-string/comment))
	 ;; Ensure there's a previous statement and move to it.
	 (zerop (hudson-previous-statement))
	 (not (hudson-close-block-statement-p t))
	 (not (hudson-open-block-statement-p)))))


;;;; Indentation

;; Alist of possible indentations and start of statement they would
;; close.  Used in indentation cycling (below).

(defcustom hudson-indent 4
  "*Amout of offset per level of indentation."
  :type 'integer
  :group 'hudson)
(put 'hudson-indent 'safe-local-variable 'integerp)

(defcustom hudson-guess-indent t
  "Non-nil means Hudson mode guesses `hudson-indent' for the buffer."
  :type 'boolean
  :group 'hudson)

(defcustom hudson-honor-comment-indentation nil
  "Non-nil means indent relative to preceding comment line.
Only do this for comments where the leading comment character is
followed by space.  This doesn't apply to comment lines, which
are always indented in lines with preceding comments."
  :type 'boolean
  :group 'hudson)

(defcustom hudson-continuation-offset 3
  "*Additional amount of offset to for continuation lines.
Continuation lines are those that immediately follow a double pound
continued line."
  :type 'integer
  :group 'hudson)

(defun hudson-guess-indent ()
  "Guess step for indentation of current buffer.
Set `hudson-indent' locally to the value guessed."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let (done indent)
	(while (and (not done) (not (eobp)))
	  (when (and (re-search-forward (rx (or "do" "is" "else" "then")
                                            (0+ space)
					    (or (syntax comment-start)
						line-end))
					nil 'move)
		     (hudson-open-block-statement-p))
	    (save-excursion
	      (hudson-beginning-of-statement)
	      (let ((initial (current-indentation)))
		(if (zerop (hudson-next-statement))
                    (setq indent (- (current-indentation) initial)))
		(if (and indent (>= indent 2) (<= indent 8)) ; sanity check
		    (setq done t))))))
	(when done
	  (when (/= indent (default-value 'hudson-indent))
	    (set (make-local-variable 'hudson-indent) indent)
	    (unless (= tab-width hudson-indent)
	      (setq indent-tabs-mode nil)))
	  indent)))))

;; Alist of possible indentations and start of statement they would
;; close.  Used in indentation cycling (below).
(defvar hudson-indent-list nil
  "Internal use.")
;; Length of the above
(defvar hudson-indent-list-length nil
  "Internal use.")
;; Current index into the alist.
(defvar hudson-indent-index nil
  "Internal use.")

(defun hudson-calculate-indentation ()
  "Calculate Hudson indentation for line at point."
  (setq hudson-indent-list nil
	hudson-indent-list-length 1)
  (save-excursion
    (beginning-of-line)
    (let ((syntax (syntax-ppss))
	  start)
      (cond
       ((hudson-continuation-line-p)   ; after backslash, or bracketed
        (let ((start-point (point))
	      (open-start (cadr syntax))
	      (continued (hudson-explicit-continuation-line-p))
	      (block-starter (save-excursion
                               (forward-line -1)
                               (end-of-line)
                               (hudson-skip-comments/blanks t)
                               (looking-back " is\\|do\\|else\\|then"
                                             (- (point) 5)))))
	  (if open-start ;; Inside bracketed expression.
	      (progn
		(goto-char (1+ open-start))
		;; Look for first item in list (preceding point) and
		;; align with it, if found.
		(if (let ((parse-sexp-ignore-comments t))
                      (condition-case ()
                          (progn (forward-sexp)
                                 (backward-sexp)
                                 (< (point) start-point))
                        (error nil)))
                    (current-column)))
	    ;; Otherwise explicitly continued with double pound signs.
	    (if (hudson-continuation-line-p)
              ;; We're past first continuation line.  Align with
              ;; previous line.
                (progn (forward-line -1)
                       (+ hudson-continuation-offset (current-indentation)))
	      ;; First continuation line.  Indent one step, with an
	      ;; extra one if statement opens a block.
              (hudson-beginning-of-statement)
	      (+ (current-indentation) hudson-continuation-offset
		 (if (hudson-open-block-statement-p t)
		     hudson-indent
		   0))
              ))))
       ((bobp) 0)
       ;; Fixme: Like python-mode.el; not convinced by this.
       ((looking-at (rx (0+ space) (syntax comment-start)
			(not (any " \t\n")))) ; non-indentable comment
	(current-indentation))
       ((and hudson-honor-comment-indentation
	     ;; Back over whitespace, newlines, non-indentable comments.
             (catch 'done
	       (while (cond ((bobp) nil)
			    ((not (forward-comment -1))
			     nil)	; not at comment start
			    ;; Now at start of comment -- trailing one?
			    ((/= (current-column) (current-indentation))
			     nil)
			    ;; Indentable comment, like python-mode.el?
			    ((and (looking-at (rx (syntax comment-start)
						  (or space line-end)))
				  (/= 0 (current-column)))
			     (throw 'done (current-column)))
			    ;; Else skip it (loop).
			    (t))))))
       (t
	(hudson-indentation-levels)
	;; Prefer to indent comments with an immediately-following
	;; statement, e.g.
	;;       ...
	;;   # ...
	;;   def ...
	(when (and (> hudson-indent-list-length 1)
		   (hudson-comment-line-p))
	  (forward-line)
	  (unless (hudson-comment-line-p)
	    (let ((elt (assq (current-indentation) hudson-indent-list)))
	      (setq hudson-indent-list
		    (nconc (delete elt hudson-indent-list)
			   (list elt))))))
	(caar (last hudson-indent-list)))))))

;;;; Cycling through the possible indentations with successive TABs.

;; These don't need to be buffer-local since they're only relevant
;; during a cycle.

(defun hudson-initial-text ()
  "Text of line following indentation and ignoring any trailing comment."
  (save-excursion
    (buffer-substring (progn
			(back-to-indentation)
			(point))
		      (progn
			(end-of-line)
			(forward-comment -1)
			(point)))))

(defconst hudson-block-pairs
  '(("else" "if" "else"))
  "Alist of keyword matches.
The car of an element is a keyword introducing a statement which
can close a block opened by a keyword in the cdr.")

(defun hudson-first-word ()
  "Return first word (actually symbol) on the line."
  (save-excursion
    (back-to-indentation)
    (current-word t)))

(defun hudson-indentation-levels ()
  "Return a list of possible indentations for this line.
It is assumed not to be a continuation line or in a multi-line string.
Includes the default indentation and those which would close all
enclosing blocks.  Elements of the list are actually pairs:
\(INDENTATION . TEXT), where TEXT is the initial text of the
corresponding block opening (or nil)."
  (save-excursion
    (let ((initial "")
	  levels indent)
      ;; Only one possibility immediately following a block open
      ;; statement, assuming it doesn't have a `suite' on the same line.
      (cond
       ((save-excursion (and (hudson-previous-statement)
			     (hudson-open-block-statement-p t)
			     (setq indent (current-indentation))
			     ;; Check we don't have something like:
			     ;;   if ...: ...
			     (if (progn (hudson-end-of-statement)
					(hudson-skip-comments/blanks t)
					(looking-back "is\\|do\\|then\\|else"
                                                      (- 5 (point))))
				 (progn
                                   (setq indent (+ hudson-indent indent)))))
                        
                        )
	(push (cons indent initial) levels))
       ;; Only one possibility for comment line immediately following
       ;; another.
       ((save-excursion
	  (when (hudson-comment-line-p)
	    (forward-line -1)
	    (if (hudson-comment-line-p)
		(push (cons (current-indentation) initial) levels)))))
       (t
	(let ((start (car (assoc (hudson-first-word) hudson-block-pairs))))
	  (hudson-previous-statement)
	  ;; Is this a valid indentation for the line of interest?
	  (unless (or (if start		; potentially only outdentable
			  ;; Check for things like:
			  ;;   if ...: ...
			  ;;   else ...:
			  ;; where the second line need not be outdented.
			  (not (member (hudson-first-word)
				       (cdr (assoc start
						   hudson-block-pairs)))))
		      ;; Not sensible to indent to the same level as
		      ;; previous `return' &c.
		      (hudson-close-block-statement-p))
	    (push (cons (current-indentation) (hudson-initial-text))
		  levels))
	  (while (hudson-beginning-of-block)
	    (when (or (not start)
		      (member (hudson-first-word)
			      (cdr (assoc start hudson-block-pairs))))
	      (push (cons (current-indentation) (hudson-initial-text))
		    levels))))
        ))
      (prog1 (or levels (setq levels '((0 . ""))))
	(setq hudson-indent-list        levels
	      hudson-indent-list-length (length hudson-indent-list))))))

;; This is basically what `hudson-indent-line' would be if we didn't
;; do the cycling.
(defun hudson-indent-line-1 (&optional leave)
  "Subroutine of `hudson-indent-line' for non-repeated indentation.
LEAVE non-nil means leave indentation if it is valid and set
`hudson-indent-index' to the current indentation.  A valid indentation
is one of the positions returned by `hudson-calculate-indentation'."
  (let ((target (hudson-calculate-indentation))
	(pos (- (point-max) (point)))
        (ci (current-indentation)))
    (if (or (= target ci)
	    ;; Maybe keep a valid indentation.
	    (and leave hudson-indent-list
		 (assq ci hudson-indent-list)))
	(progn
          (if (< (current-column) ci)
              (back-to-indentation)))
      (beginning-of-line)
      (delete-horizontal-space)
      (indent-to target)
      (if (> (- (point-max) pos) (point))
	  (goto-char (- (point-max) pos))))))

(defun hudson-indent-line ()
  "Indent current line as Hudson code.
When invoked via `indent-for-tab-command', cycle through possible
indentations for current line.  Indents at increasing positions
and wraps after the last valid position.  The cycle is broken by
a command different from `indent-for-tab-command',
i.e. successive TABs do the cycling."
  (interactive)
  (if (and (eq this-command 'indent-for-tab-command)
	   (eq last-command this-command))
      (progn
        (if (= 1 hudson-indent-list-length)
            (message "Sole indentation")
          (progn
            (setq hudson-indent-index
                  (% (1+ hudson-indent-index) hudson-indent-list-length))
            (beginning-of-line)
            (delete-horizontal-space)
            (indent-to (car (nth hudson-indent-index hudson-indent-list)))
            (if (hudson-block-end-p)
                (let ((text (cdr (nth hudson-indent-index
                                      hudson-indent-list))))
                  (if text
                      (message "Closes: %s" text)
                    ))))))
    (hudson-indent-line-1)
    (setq hudson-indent-index (1- hudson-indent-list-length))))

(defun hudson-indent-region (start end)
  "`indent-region-function' for Hudson.
Leaves validly-indented lines alone, i.e. doesn't indent to
another valid position.
Argument START the start point.
Argument END the end point."
  (save-excursion
    (goto-char end)
    (setq end (point-marker))
    (goto-char start)
    (or (bolp) (forward-line 1))
    (while (< (point) end)
      (or (and (bolp) (eolp))
	  (hudson-indent-line-1 t))
      (forward-line 1))
    (move-marker end nil)))

(defun hudson-block-end-p ()
  "Return t if this line ends a block.
Non-nil if this is a line in a statement closing a block,
or a blank line indented to where it would close a block."
  (and (not (hudson-comment-line-p))
       (or (hudson-close-block-statement-p t)
	   (< (current-indentation)
	      (save-excursion
		(hudson-previous-statement)
		(current-indentation))))))


;;;; Movement

(defun hudson-beginning-of-defun ()
  "`beginning-of-defun-function' for Hudson.
Finds beginning of innermost nested class or method definition.
Returns the name of the definition found at the end, or nil if
reached start of buffer."
  (let ((ci (current-indentation))
	(def-re (rx line-start (0+ space) (or "function" "procedure" "class")
                    (1+ space) (group (1+ (or word (syntax symbol))))))
	found lep) ;; def-line
    (if (hudson-comment-line-p)
	(setq ci most-positive-fixnum))
    (while (and (not (bobp)) (not found))
      ;; Treat bol at beginning of function as outside function so
      ;; that successive C-M-a makes progress backwards.
      ;;(setq def-line (looking-at def-re))
      (unless (bolp) (end-of-line))
      (setq lep (line-end-position))
      (if (and (re-search-backward def-re nil 'move)
	       ;; Must be less indented or matching top level, or
	       ;; equally indented if we started on a definition line.
	       (let ((in (current-indentation)))
		 (or (and (zerop ci) (zerop in))
		     (= lep (line-end-position)) ; on initial line
		     ;; Not sure why it was like this -- fails in case of
		     ;; last internal function followed by first
		     ;; non-def statement of the main body.
                     ;; 		     (and def-line (= in ci))
		     (= in ci)
		     (< in ci)))
	       (not (hudson-in-string/comment)))
	  (setq found t)))
    found))

(defun hudson-end-of-defun ()
  "`end-of-defun-function' for Hudson.
Finds end of innermost nested class or method definition."
  (let ((orig (point))
	(pattern (rx line-start (0+ space)
                     (or "function" "procedure" "class") space)))
    ;; Go to start of current block and check whether it's at top
    ;; level.  If it is, and not a block start, look forward for
    ;; definition statement.
    (when (hudson-comment-line-p)
      (end-of-line)
      (forward-comment most-positive-fixnum))
    (if (not (hudson-open-block-statement-p))
	(hudson-beginning-of-block))
    (if (zerop (current-indentation))
	(unless (hudson-open-block-statement-p)
	  (while (and (re-search-forward pattern nil 'move)
		      (hudson-in-string/comment))) ; just loop
	  (unless (eobp)
	    (beginning-of-line)))
      ;; Don't move before top-level statement that would end defun.
      (end-of-line)
      (hudson-beginning-of-defun))
    ;; If we got to the start of buffer, look forward for
    ;; definition statement.
    (if (and (bobp) (not (looking-at "function\\|procedure\\|class")))
	(while (and (not (eobp))
		    (re-search-forward pattern nil 'move)
		    (hudson-in-string/comment)))) ; just loop
    ;; We're at a definition statement (or end-of-buffer).
    (unless (eobp)
      (hudson-end-of-block)
      ;; Count trailing space in defun (but not trailing comments).
      (skip-syntax-forward " >")
      (unless (eobp)			; e.g. missing final newline
	(beginning-of-line)))
    ;; Catch pathological cases like this, where the beginning-of-defun
    ;; skips to a definition we're not in:
    ;; if ... then
    ;;     ...
    ;; else
    ;;     ...  # point here
    ;;     ...
    ;;     function ...
    (if (< (point) orig)
	(goto-char (point-max)))))

(defun hudson-beginning-of-string ()
  "Go to beginning of string around point.
Do nothing if not in string."
  (let ((state (syntax-ppss)))
    (when (eq 'string (syntax-ppss-context state))
      (goto-char (nth 8 state)))))

(defun hudson-beginning-of-statement ()
  "Go to start of current statement.
Accounts for continuation lines, multi-line strings, and
multi-line bracketed expressions."
  (beginning-of-line)
  (hudson-beginning-of-string)
  (let (start-point)
    (while (and (hudson-continuation-line-p)
		(if start-point
		    (< (point) start-point)
		  t))
      (beginning-of-line)
      (if (hudson-explicit-continuation-line-p)
	  (progn
	    (forward-line -1)
	    (while (hudson-explicit-continuation-line-p)
	      (forward-line -1)))
	(hudson-beginning-of-string)
	(hudson-skip-out))
      (setq start-point (point))))
  (back-to-indentation))

(defun hudson-skip-out (&optional forward syntax)
  "Skip out of any nested brackets.
Skip forward if FORWARD is non-nil, else backward.
If SYNTAX is non-nil it is the state returned by `syntax-ppss' at point.
Return non-nil if and only if skipping was done."
  (let ((depth (syntax-ppss-depth (or syntax (syntax-ppss))))
	(forward (if forward -1 1)))
    (unless (zerop depth)
      (if (> depth 0)
	  ;; Skip forward out of nested brackets.
	  (condition-case ()		; beware invalid syntax
	      (progn (backward-up-list (* forward depth)) t)
	    (error nil))
	;; Invalid syntax (too many closed brackets).
	;; Skip out of as many as possible.
	(let (done)
	  (while (condition-case ()
		     (progn (backward-up-list forward)
			    (setq done t))
		   (error nil)))
	  done)))))

(defun hudson-end-of-statement ()
  "Go to the end of the current statement and return point.
Usually this is the start of the next line, but if this is a
multi-line statement we need to skip over the continuation lines.
On a comment line, go to end of line."
  (end-of-line)
  (while (let (comment)
	   ;; Move past any enclosing strings and sexps, or stop if
	   ;; we're in a comment.
	   (while (let ((s (syntax-ppss)))
		    (cond ((eq 'comment (syntax-ppss-context s))
                           (setq comment t)
			   nil)
			  ((eq 'string (syntax-ppss-context s))
			   ;; Go to start of string and skip it.
                           (let ((pos (point)))
                             (goto-char (nth 8 s))
                             (condition-case () ; beware invalid syntax
                                 (progn (forward-sexp) t)
                               ;; If there's a mismatched string, make sure
                               ;; we still overall move *forward*.
                               (error (goto-char pos) (end-of-line)))))
			  ((hudson-skip-out t s))))
	     (end-of-line))
	   (when comment                ; line continuation comment?
             (hudson-skip-comments/blanks t)
             (looking-at (rx (0+ space) "##"))))
    (end-of-line 2))			; Try next line.
  (point))

(defun hudson-previous-statement (&optional count)
  "Go to start of previous statement.
With argument COUNT, do it COUNT times.  Stop at beginning of buffer.
Return count of statements left to move."
  (interactive "p")
  (unless count (setq count 1))
  (if (< count 0)
      (hudson-next-statement (- count))
    (hudson-beginning-of-statement)
    (while (and (> count 0) (not (bobp)))
      (hudson-skip-comments/blanks t)
      (hudson-beginning-of-statement)
      (unless (bobp) (setq count (1- count))))
    count))

(defun hudson-next-statement (&optional count)
  "Go to start of next statement.
With argument COUNT, do it COUNT times.  Stop at end of buffer.
Return count of statements left to move."
  (interactive "p")
  (unless count (setq count 1))
  (if (< count 0)
      (hudson-previous-statement (- count))
    (beginning-of-line)
    (let (bogus)
      (while (and (> count 0) (not (eobp)) (not bogus))
	(hudson-end-of-statement)
	(hudson-skip-comments/blanks)
	(if (eq 'string (syntax-ppss-context (syntax-ppss)))
	    (setq bogus t)
	  (unless (eobp)
	    (setq count (1- count))))))
    count))

(defun hudson-beginning-of-block (&optional arg)
  "Go to start of current block.
With numeric arg, do it that many times.  If ARG is negative, call
`hudson-end-of-block' instead.
If point is on the first line of a block, use its outer block.
If current statement is in column zero, don't move and return nil.
Otherwise return non-nil."
  (interactive "p")
  (unless arg (setq arg 1))
  (cond
   ((zerop arg))
   ((< arg 0) (hudson-end-of-block (- arg)))
   (t
    (let ((point (point)))
      (if (or (hudson-comment-line-p)
	      (hudson-blank-line-p))
	  (hudson-skip-comments/blanks t))
      (hudson-beginning-of-statement)
      (let ((ci (current-indentation)))
	(if (zerop ci)
	    (not (goto-char point))	; return nil
	  ;; Look upwards for less indented statement.
	  (if (catch 'done
;;; This is slower than the below.
;;; 	  (while (zerop (hudson-previous-statement))
;;; 	    (when (and (< (current-indentation) ci)
;;; 		       (hudson-open-block-statement-p t))
;;; 	      (beginning-of-line)
;;; 	      (throw 'done t)))
		(while (and (zerop (forward-line -1)))
		  (when (and (< (current-indentation) ci)
			     (not (hudson-comment-line-p))
			     ;; Move to beginning to save effort in case
			     ;; this is in string.
			     (progn (hudson-beginning-of-statement) t)
			     (hudson-open-block-statement-p t))
		    (beginning-of-line)
		    (throw 'done t)))
		(not (goto-char point))) ; Failed -- return nil
	      (hudson-beginning-of-block (1- arg)))))))))

(defun hudson-end-of-block (&optional arg)
  "Go to end of current block.
With numeric arg, do it that many times.  If ARG is negative,
call `hudson-beginning-of-block' instead.
If current statement is in column zero and doesn't open a block,
don't move and return nil.  Otherwise return t."
  (interactive "p")
  (unless arg (setq arg 1))
  (if (< arg 0)
      (hudson-beginning-of-block (- arg))
    (while (and (> arg 0)
		(let* ((point (point))
		       (_ (if (hudson-comment-line-p)
			      (hudson-skip-comments/blanks t)))
		       (ci (current-indentation))
		       (open (hudson-open-block-statement-p)))
		  (if (and (zerop ci) (not open))
		      (not (goto-char point))
		    (catch 'done
		      (while (zerop (hudson-next-statement))
			(when (or (and open (<= (current-indentation) ci))
				  (< (current-indentation) ci))
			  (hudson-skip-comments/blanks t)
			  (beginning-of-line 2)
			  (throw 'done t)))))))
      (setq arg (1- arg)))
    (zerop arg)))


;;;; Imenu.

(defvar hudson-recursing)
(defun hudson-imenu-create-index ()
  "`imenu-create-index-function' for Hudson.
Makes nested Imenu menus from nested `class', `function' and
`procedure' statements.  The nested menus are headed by an item
referencing the outer definition; it has a space prepended to the
name so that it sorts first with `imenu--sort-by-name' (though,
unfortunately, sub-menus precede it)."
  (unless (boundp 'hudson-recursing)	; dynamically bound below
    ;; Normal call from Imenu.
    (goto-char (point-min))
    ;; Without this, we can get an infloop if the buffer isn't all
    ;; fontified.  I guess this is really a bug in syntax.el.  OTOH,
    ;; _with_ this, imenu doesn't immediately work; I can't figure out
    ;; what's going on, but it must be something to do with timers in
    ;; font-lock.
    ;; This can't be right, especially not when jit-lock is not used.  --Stef
    ;; (unless (get-text-property (1- (point-max)) 'fontified)
    ;;   (font-lock-fontify-region (point-min) (point-max)))
    )
  (let (index-alist)			; accumulated value to return
    (while (re-search-forward
	    (rx line-start (0+ space)	; leading space
		(or (group "function") (group "procedure") (group "class"))
		(1+ space) (group (1+ (or word ?_))))	   ; name
	    nil t)
      (unless (hudson-in-string/comment)
	(let ((pos (match-beginning 0))
	      (name (match-string-no-properties 4)))
	  (if (match-beginning 3)	; def or class?
	      (setq name (concat "class " name)))
	  (save-restriction
	    (narrow-to-defun)
	    (let* ((hudson-recursing t)
		   (sublist (hudson-imenu-create-index)))
	      (if sublist
		  (progn (push (cons (concat " " name) pos) sublist)
			 (push (cons name sublist) index-alist))
		(push (cons name pos) index-alist)))))))
    (unless (boundp 'hudson-recursing)
      ;; Look for module variables.
      (let (vars)
	(goto-char (point-min))
	(while (re-search-forward
		(rx line-start (group (1+ (or word ?_))) (0+ space) ":=")
		nil t)
	  (unless (hudson-in-string/comment)
	    (push (cons (match-string 1) (match-beginning 1))
		  vars)))
	(setq index-alist (nreverse index-alist))
	(if vars
	    (push (cons "Module variables"
			(nreverse vars))
		  index-alist))))
    index-alist))


;;;; `Electric' commands.
(defun hudson-backspace (arg)
  "Maybe delete a level of indentation on the current line.
Do so if point is at the end of the line's indentation outside
strings and comments.
Otherwise just call `backward-delete-char-untabify'.
Repeat ARG times."
  (interactive "*p")
  (if (or (/= (current-indentation) (current-column))
	  (bolp)
	  (hudson-continuation-line-p)
	  (hudson-in-string/comment))
      (backward-delete-char-untabify arg)
    ;; Look for the largest valid indentation which is smaller than
    ;; the current indentation.
    (let ((indent 0)
	  (ci (current-indentation))
	  (indents (hudson-indentation-levels))
	  initial)
      (dolist (x indents)
	(if (< (car x) ci)
	    (setq indent (max indent (car x)))))
      (setq initial (cdr (assq indent indents)))
      (if (> (length initial) 0)
	  (message "Closes %s" initial))
      (delete-horizontal-space)
      (indent-to indent))))
(put 'hudson-backspace 'delete-selection 'supersede)

(define-abbrev-table 'hudson-mode-abbrev-table ()
  "Abbrev table for Hudson mode."
  :case-fixed t
  ;; Only expand in code.
  :enable-function (lambda () (not (hudson-in-string/comment))))


;; Functions for moving the point


;; Helper functions

(defconst hudson-continued-re
  "\\([^#\n]\\)*##.*$"
  "Regular expression matching a Hudson double pound sign continuation line.")

;;;###autoload
(define-derived-mode hudson-mode fundamental-mode "Hudson"
  "Major mode for editing Hudson programs.
Blank lines separate paragraphs, comments start with `# '.

The indentation width is controlled by `hudson-indent', which
defaults to 4.  If `hudson-guess-indent' is non-nil, then try to
match the indentation of the file.

To enable compilation, `hudson-jar-file' must be set to the
location of `hudson.jar'.

Modules can hook in via `hudson-mode-hook'.

Use `hudson-version' to find out what version this is.

\\{hudson-mode-map} "
  :group 'hudson
  (set (make-local-variable 'font-lock-defaults)
       '(hudson-font-lock-keywords))
  (interactive)
  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'indent-line-function) #'hudson-indent-line)
  (set (make-local-variable 'indent-region-function) #'hudson-indent-region)
  (set (make-local-variable 'paragraph-start) "\\s-*$")
  (set (make-local-variable 'fill-paragraph-function) 'hudson-fill-paragraph)
  (set (make-local-variable 'require-final-newline) mode-require-final-newline)
  ;; FIXME
  (set (make-local-variable 'which-func-functions) '(hudson-which-function))
  (set (make-local-variable 'add-log-current-defun-function)
       'hudson-current-defun)

  (set (make-local-variable 'outline-regexp)
       (rx (* space) (or "class" "function" "else" "if" "procedure" "while")
	   symbol-end))
  (set (make-local-variable 'outline-heading-end-regexp)
       (rx symbol-start (or "do" "else" "is" "then")))
  (set (make-local-variable 'outline-level) #'hudson-outline-level)
  (set (make-local-variable 'open-paren-in-column-0-is-defun-start) nil)
  
  (add-to-list 'hs-special-modes-alist
               '(hudson-mode "\\(?:procedure\\|function\\|class\\)" nil
                             "#" hudson-end-of-block nil))
;;;   (set (make-local-variable 'hs-hide-all-non-comment-function)
;;;        'hudson-hs-hide-level-1)
  (set (make-local-variable 'beginning-of-defun-function)
       'hudson-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function) 'hudson-end-of-defun)
  (add-hook 'which-func-functions 'hudson-which-func nil t)
  (setq imenu-create-index-function #'hudson-imenu-create-index)
  (set (make-local-variable 'ispell-check-comments) 'exclusive)
;;;   (set (make-local-variable 'eldoc-documentation-function)
;;;        #'python-eldoc-function)
  (unless font-lock-mode (font-lock-mode 1))
  (when hudson-guess-indent (hudson-guess-indent))
  )

;; Not done automatically in Emacs 21 or 22.
(defcustom hudson-mode-hook nil
  "Hook run when entering Hudson mode."
  :group 'hudson
  :type 'hook)
(custom-add-option 'hudson-mode-hook 'imenu-add-menubar-index)
(custom-add-option 'hudson-mode-hook 'abbrev-mode)


;;;; Compilation and Execution

(defvar hudson-compilation-error-regexp
  `(,(rx line-start "[" (group (1+ alnum) ".hud") ": line" (? "s")
         space (group (1+ digit)) (group (opt "-" (1+ digit))) "]")
    1 2))

(defun hudson-get-includes ()
  "Return all files that should be included in compilation."
  (interactive)
  (let (includes '())
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              (rx line-start  "#include" (1+ space)
                  (group (1+ (or alnum "-" "_" "."))))
              nil t)
        (push (buffer-substring-no-properties (match-beginning 1)
                                              (match-end 1))
              includes)))
    includes))

(defun hudson-compile-and-run (&optional arg)
  "Compile and run the Hudson file.
If ARG is non-nil, ask for user confirmation"
  (interactive)
  (require 'compile)
  
  (when (equal hudson-jar-file "")
    (error "hudson-jar-file is not set."))

  (let* ((filename (file-name-sans-extension (buffer-name)))
         (includes (reverse (cons filename (hudson-get-includes))))
         (includes-str (mapconcat 'identity includes " "))
         (command (concat "java -classpath " hudson-jar-file
                          " hudson.Interp " includes-str))
         (compilation-error-regexp-alist
          (cons hudson-compilation-error-regexp
                compilation-error-regexp-alist)))
    (compilation-start command))


;;;; Miscellany.
  (defun hudson-fill-paragraph (&optional justify)
    "`fill-paragraph-function' handling multi-line strings and possibly comments.
If any of the current line is in or at the end of a multi-line string,
fill the string or the paragraph of it that point is in, preserving
the string's indentation."
    (interactive "P")
    (or (fill-comment-paragraph justify)
        (save-excursion
          (end-of-line)
          (let* ((syntax (syntax-ppss))
                 (orig (point))
                 start end)
            (cond ((nth 4 syntax) ; comment.   fixme: loses with trailing one
                   (let (fill-paragraph-function)
                     (fill-paragraph justify)))
                  ;; The `paragraph-start' and `paragraph-separate'
                  ;; variables don't allow us to delimit the last
                  ;; paragraph in a multi-line string properly, so narrow
                  ;; to the string and then fill around (the end of) the
                  ;; current line.
                  ((eq t (nth 3 syntax))    ; in fenced string
                   (goto-char (nth 8 syntax)) ; string start
                   (setq start (line-beginning-position))
                   (setq end (condition-case () ; for unbalanced quotes
                                 (progn (forward-sexp)
                                        (- (point) 3))
                               (error (point-max)))))
                  ((re-search-backward "\\s|\\s-*\\=" nil t) ; end of fenced string
                   (forward-char)
                   (setq end (point))
                   (condition-case ()
                       (progn (backward-sexp)
                              (setq start (line-beginning-position)))
                     (error nil))))
            (when end
              (save-restriction
                (narrow-to-region start end)
                (goto-char orig)
                ;; Avoid losing leading and trailing newlines in doc
                ;; strings written like:
                ;;   """
                ;;   ...
                ;;   """
                (let ((paragraph-separate
                       ;; Note that the string could be part of an
                       ;; expression, so it can have preceding and
                       ;; trailing non-whitespace.
                       (concat
                        (rx (or
                             ;; Opening triple quote without following text.
                             (and (* nonl)
                                  (group (syntax string-delimiter))
                                  (repeat 2 (backref 1))
                                  ;; Fixme:  Not sure about including
                                  ;; trailing whitespace.
                                  (* (any " \t"))
                                  eol)
                             ;; Closing trailing quote without preceding text.
                             (and (group (any ?\" ?')) (backref 2)
                                  (syntax string-delimiter))))
                        "\\(?:" paragraph-separate "\\)"))
                      fill-paragraph-function)
                  (fill-paragraph justify))))))) t)


  (defun hudson-mode-version ()
    "Return `hudson-mode' version."
    (interactive)
    (if (interactive-p)
        (message hudson-version)
      hudson-version))

  (defun hudson-hs-hide-level-1 ()
    (hs-hide-level 1)
    (hudson-next-statement)))

(defun hudson-shift-left (start end &optional count)
  "Shift lines in region COUNT (the prefix arg) columns to the left.
The region shifted includes the lines in which START and END
lie.  If region isn't active, just shift current line.  COUNT
defaults to `hudson-indent'.  It is an error if any lines in the
region are indented less than COUNT columns."
  (interactive (if mark-active
		   (list (region-beginning) (region-end) current-prefix-arg)
		 (list (point) (point) current-prefix-arg)))
  (if count
      (setq count (prefix-numeric-value count))
    (setq count hudson-indent))
  (when (> count 0)
    (save-excursion
      (goto-char start)
      (while (< (point) end)
	(if (and (< (current-indentation) count)
		 (not (looking-at "[ \t]*$")))
	    (error "Can't shift all lines enough"))
	(forward-line))
      (indent-rigidly start end (- count)))))

(add-to-list 'debug-ignored-errors "^Can't shift all lines enough")

(defun hudson-newline-and-indent ()
  "Possibly continue this line and then `newline-and-indent'.
If there is an enclosing parenthesis, add a line continuation if
there isn't one already there.  Always insert a newline and
indent."
  (interactive)
  (when (nth 1 (syntax-ppss))
    (if (save-excursion (forward-line 1)
                        (hudson-continuation-line-p))
        (end-of-line)
      (unless (eq (char-before) ?\s)
        (insert " "))
      (insert "##")))
  (newline-and-indent))

(defun hudson-delete-indentation (&optional arg)
  "Delete comments and join lines."
  (interactive "*P")
  (save-excursion
    (forward-line -1)
    (if arg (forward-line 1))
    (beginning-of-line)
    (comment-kill 1))
  (delete-indentation arg))

(defun hudson-shift-right (start end &optional count)
  "Shift lines in region COUNT (the prefix arg) columns to the right.
COUNT defaults to `hudson-indent'.  If region isn't active, just shift
current line.  The region shifted includes the lines in which START and
END lie."
  (interactive (if mark-active
		   (list (region-beginning) (region-end) current-prefix-arg)
		 (list (point) (point) current-prefix-arg)))
  (if count
      (setq count (prefix-numeric-value count))
    (setq count hudson-indent))
  (indent-rigidly start end count))

(defun hudson-outline-level ()
  "Variable `outline-level' function for Hudson mode.
The level is the number of `hudson-indent' steps of indentation
of current line."
  (1+ (/ (current-indentation) hudson-indent)))

(defun hudson-mark-block ()
  "Mark the block around point.
Uses `hudson-beginning-of-block', `hudson-end-of-block'."
  (interactive)
  (push-mark)
  (hudson-beginning-of-block)
  (push-mark (point) nil t)
  (hudson-end-of-block)
  (exchange-point-and-mark))

(defun hudson-current-defun (&optional length-limit)
  "`add-log-current-defun-function' for Hudson.
Optional argument LENGTH-LIMIT FIXME."
  (save-excursion
    ;; Move up the tree of nested `class', `function' and `procedure' blocks until we
    ;; get to zero indentation, accumulating the defined names.
    (let ((accum)
	  (length -1))
      (catch 'done
	(while (or (null length-limit)
		   (null (cdr accum))
		   (< length length-limit))
	  (let ((started-from (point)))
	    (hudson-beginning-of-block)
	    (end-of-line)
	    (hudson-beginning-of-defun)
	    (when (= (point) started-from)
	      (throw 'done nil)))
	  (when (looking-at (rx (0+ space) (or "function" "procedure" "class")
                                (1+ space)
				(group (1+ (or word (syntax symbol))))))
	    (push (match-string 1) accum)
	    (setq length (+ length 1 (length (car accum)))))
	  (when (= (current-indentation) 0)
	    (throw 'done nil))))
      (when accum
	(when (and length-limit (> length length-limit))
	  (setcar accum ".."))
	(mapconcat 'identity accum ".")))))


(defvar hudson-which-func-length-limit 40
  "Non-strict length limit for `hudson-which-func' output.")

(defun hudson-which-func ()
  "Return fontified function name for `which-func-mode'."
  (let ((function-name (hudson-current-defun hudson-which-func-length-limit)))
    (set-text-properties 0 (length function-name) nil function-name)
    function-name))

(provide 'hudson-mode)

;;; hudson-mode.el ends here
