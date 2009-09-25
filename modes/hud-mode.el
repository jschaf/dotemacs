;;; hud-mode.el --- A Hudson editing mode
;;; Author: Chris Okasaki 
;;; Version: 1.2 [07-AUG-2006]

;; User options

(defvar hud-indent-width 3
  "*Number of spaces per indentation level in a Hudson program.")

;; Define the editing commands provided by the major mode

(defun hud-indent-region (start end)
  "Add 'hud-indent-width' spaces to the beginning of each line 
in the current region."
  (interactive "r")
  (indent-rigidly start end hud-indent-width))

(defun hud-outdent-region (start end)
  "Remove 'hud-indent-width' spaces from the beginning of each line 
in the current region.  If a line begins fewer than 'hud-indent-width'
spaces, then simply remove what spaces there are."
  (interactive "r")
  (indent-rigidly start end (- hud-indent-width)))

(defun spaces-to-next-tab (offset)
  (let ((mod (1+ (% offset hud-indent-width))))
    (1+ (% (- hud-indent-width mod) hud-indent-width))))

(defun spaces-to-prev-tab (offset)
  (1+ (% (+ offset (1- hud-indent-width)) hud-indent-width)))

(defun hud-indent-line ()
  "Indent the current line to the next tab stop."
  (interactive)
  (back-to-indentation)
  (let* ((current-spaces (- (point) (line-beginning-position)))
         (spaces-needed (spaces-to-next-tab current-spaces)))
    (insert-char 32 spaces-needed)))

(defun hud-outdent-line ()
  "Outdent the current line to the previous tab stop."
  (interactive)
  (back-to-indentation)
  (let* ((current-spaces (- (point) (line-beginning-position)))
         (spaces-needed (spaces-to-prev-tab current-spaces)))
    (unless (bolp)
        (delete-backward-char spaces-needed))))

(defun inside-indentation ()
  "Does point occur before the first non-space character in the current line?"
  (save-excursion
    (let ((pt (point)))
      (back-to-indentation)
      (<= pt (point)))))

(defun is-current-line-blank()
  "Is the current line blank?"
  (save-excursion
    (back-to-indentation)
    (eolp)))

(defun hud-smart-space ()
  "Increase the indentation level of the current line if appropriate.
Otherwise, insert a single space."
  (interactive)
  (if (inside-indentation) (hud-indent-line) (hud-simple-space)))

(defun hud-simple-space ()
  "Insert a single space."
  (interactive)
  (insert-char 32 1))

(defun hud-smart-backspace ()
  "Decrease the indentation level of the current line if appropriate.
Otherwise, delete a single character."
  (interactive)
  (if (and (inside-indentation) (not (bolp))) (hud-outdent-line) (hud-simple-backspace)))

(defun hud-simple-backspace ()
  "Delete a single character."
  (interactive)
  (delete-backward-char 1))

(defun hud-indent-relative ()
  "Indent the current line by the same amount as the previous non-blank line."
  (interactive)
  (let ((prev-indent 0))
    (save-excursion
      (beginning-of-line)
      (when (re-search-backward "[^ \t\n]" nil t)
        (back-to-indentation)
        (setq prev-indent (current-column))))
    (indent-line-to prev-indent)))

(defun hud-newline-and-indent ()
  "Insert a newline and attempt to indent intelligently."
  (interactive)
  (cond
    ((not (inside-indentation)) (newline-and-indent))
    ((not (is-current-line-blank))
      (beginning-of-line)
      (newline)
      (forward-line -1)
      (hud-indent-relative)
      (forward-line 1)
      (back-to-indentation))
    (t (newline-and-indent))))

(defun hud-toggle-comment-and-advance ()
  "Add a comment character to the beginning of the current line,
unless one is there already, in which case it is deleted.
Then advance to the beginning of the following line."
  (interactive)
  (beginning-of-line)
  (if (= (following-char) ?#)
      (delete-char 1)
      (insert-char ?# 1))
  (forward-line))

(defun hud-scroll-up ()
  "Scroll but handle the end of buffer gracefully."
  (interactive)
  (unless (scroll-up) (end-of-buffer)))

(defun shell-other-window ()
  "Open a shell in another window"
  (interactive)
  (split-window-vertically)
  (other-window 1)
  (shell))

;; Keymap

(defvar hud-mode-map (make-sparse-keymap)
  "Keymap for Hudson major mode")

(define-key hud-mode-map "\C-m" 'hud-newline-and-indent)
(define-key hud-mode-map "\C-j" 'newline)
(define-key hud-mode-map "\t" 'hud-indent-line)
(define-key hud-mode-map [(control tab)] 'hud-outdent-line)
(define-key hud-mode-map [(shift tab)] 'hud-outdent-line)
(define-key hud-mode-map "\M->" 'hud-indent-region)
(define-key hud-mode-map "\M-<" 'hud-outdent-region)
(define-key hud-mode-map [(?\ )] 'hud-smart-space)
(define-key hud-mode-map [(shift ?\ )] 'hud-simple-space)
(define-key hud-mode-map [(?\d)] 'hud-smart-backspace)
(define-key hud-mode-map [(shift backspace)] 'hud-simple-backspace)
(define-key hud-mode-map [(control ?\;)] 'hud-toggle-comment-and-advance)
(define-key hud-mode-map [(home)] 'beginning-of-buffer)
(define-key hud-mode-map [(end)] 'end-of-buffer)
(define-key hud-mode-map [(f3)] 'shell-other-window)

;; Hudson keywords

(defconst hud-font-lock-keywords
  (list
   '("\\<\\(and\\|c\\(lass\\|onstant\\)\\|do\\|else\\|fun\\(ction\\)?\\|i\\(f\\|n\\(herit\\|out\\)?\\|s\\)\\|n\\(ot\\|ull\\)\\|o\\(r\\|ut\\)\\|procedure\\|re\\(f\\|turn\\)\\|th\\(en\\|is\\)\\|variable\\|while\\)\\>" . font-lock-keyword-face)
   '("\\<\\(true\\|false\\)\\>" . font-lock-constant-face))
  "Keywords for highlighting in Hudson major mode")

;; Syntax table

(defvar hud-mode-syntax-table
  (let ((hud-mode-syntax-table (make-syntax-table)))
	(modify-syntax-entry ?# "<" hud-mode-syntax-table)
	(modify-syntax-entry ?\n ">" hud-mode-syntax-table)
	hud-mode-syntax-table)
  "Syntax table for Hudson major mode")

;; Define the major mode

(defun hud-mode ()
  "Major mode for editing Hudson programs.

Supports the following features:

  - F3 opens a shell in another window
  - RETURN automatically indents to the same level as the previous non-blank line
  - SPACE automatically indents the current line when appropriate (if it 
      indents when you don't want it to, use SHIFT-SPACE instead)
  - BACKSPACE automatically un-indents the current line when appropriate (if it
      un-indents when you don't want it to, use SHIFT-BACKSPACE instead)
  - TAB indents the current line
  - SHIFT-TAB un-indents the current line
  - ESC > indents the current region
  - ESC < un-indents the current region
  - CONTROL-; comments the current line and advances to the next line.  If
      the current line is already commented, then uncomments it instead.  Hit
      CONTROL-; repeatedly to quickly comment or uncomment a group of lines.

The 'hud-indent-width' variable controls the number of spaces per
indentation level.  The default is 3.  If you want to change this to,
say, 4, then type
    ESC x set-variable <RETURN> hud-indent-width <RETURN> 4 <RETURN>"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'hud-mode)
  (setq mode-name "Hudson")
  (use-local-map hud-mode-map)
  ;; Indentation
  (setq indent-tabs-mode nil) ;; make sure to use spaces, not tabs
  (set (make-local-variable 'indent-line-function) 'hud-indent-relative)
  ;; Set up font-lock
  (require 'font-lock)
  (set-syntax-table hud-mode-syntax-table)
  (set (make-local-variable 'font-lock-defaults) '(hud-font-lock-keywords))
  (turn-on-font-lock)
  ;; Display column number in the mode line
  (column-number-mode t))

(provide 'hud-mode)
