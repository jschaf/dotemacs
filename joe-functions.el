(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying 'Active processes exist' query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

(defun goto-snippets ()
  "Go to my YASnippets directory."
  (interactive)
  (find-file "~/.emacs.d/extras/yasnippet/snippets/text-mode/"))

(defun latex-compile-pdf ()
  "Compile a LaTeX file into a PDF document."
  (interactive)
  (save-excursion
    (let ((cb (current-buffer)))
      (set-buffer (get-buffer-create "*TEMP*"))
      (call-process "xelatex" nil t nil (buffer-file-name cb)))))

(defun my-smart-kill ()
  "`kill-region' if the mark is active, else `backward-kill-word'."
  (interactive)
  (if mark-active
      (call-interactively 'kill-region)
    (call-interactively 'backward-kill-word)))

(defun my-copy-line (arg)
  "Copy ARG lines into the kill ring"
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

(defadvice yank (after indent-region activate)
  "Indent region after it is yanked."
  (if (member major-mode '(emacs-lisp-mode scheme-mode lisp-mode
					   c-mode c++-mode objc-mode ada-mode
					   latex-mode plain-tex-mode))
      (indent-region (region-beginning) (region-end) nil)))

(defadvice yank-pop (after indent-region activate)
  (if (member major-mode '(emacs-lisp-mode scheme-mode lisp-mode
					   c-mode c++-mode objc-mode ada-mode
					   latex-mode plain-tex-mode))
      (indent-region (region-beginning) (region-end) nil)))

(defun open-line-and-indent (&optional arg)
  "Same as `open-line' but it indents the line that is pushed down"
  (interactive "p")
  (save-excursion
    (while (> arg 0)
      (newline-and-indent)
      (setq arg (1- arg))
      ))
  (indent-according-to-mode))

(defun joe/ada-incr-variable (&optional arg)
  "Increment or decrement the variable before the point by ARG.
  If ARG is positive then increment the variable, else decrement
  the variable."
  (interactive "p")
  (save-excursion (re-search-backward "[ \t]+\\([a-z_0-9]+\\)"
                                      (line-beginning-position) 'noerror))
  (just-one-space)
  (insert (format ":= %s %s %d;"
                  (match-string 1)
                  (if (<= 0 arg) "+" "-")
                  (abs arg))))

;;; breaks paredit-forward-slurp-sexp

;; ;; Always re-indent the top-level sexp
;; (defadvice indent-sexp (around indent-defun (&optional endpos))
;;   "Indent the enclosing defun (or top-level sexp)."
;;   (interactive)
;;   (save-excursion
;;     (beginning-of-defun)
;;     ad-do-it))

;; (ad-activate 'indent-sexp)

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME." 
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn (rename-file name new-name 1) (rename-buffer new-name) (set-visited-file-name new-name) (set-buffer-modified-p nil))))))

;;; This is to open local files as root using tramp mode
(defvar find-file-root-prefix (if (featurep 'xemacs) "/[sudo/root@localhost]" "/sudo:root@localhost:" )
  "*The filename prefix used to open a file with `find-file-root'.")

(defvar find-file-root-history nil
  "History list for files found using `find-file-root'.")

(defvar find-file-root-hook nil
  "Normal hook for functions to run after finding a \"root\" file.")

(defun find-file-root ()
  "*Open a file as the root user.
   Prepends `find-file-root-prefix' to the selected file name so that it
   maybe accessed via the corresponding tramp method."

  (interactive)
  (require 'tramp)
  (let* ( ;; We bind the variable `file-name-history' locally so we can
   	 ;; use a separate history list for "root" files.
   	 (file-name-history find-file-root-history)
   	 (name (or buffer-file-name default-directory))
   	 (tramp (and (tramp-tramp-file-p name)
   		     (tramp-dissect-file-name name)))
   	 path dir file)

    ;; If called from a "root" file, we need to fix up the path.
    (when tramp
      (setq path (tramp-file-name-path tramp)
   	    dir (file-name-directory path)))

    (when (setq file (read-file-name "Find file (UID = 0): " dir path))
      (find-file (concat find-file-root-prefix file))
      ;; If this all succeeded save our new history list.
      (setq find-file-root-history file-name-history)
      ;; allow some user customization
      (run-hooks 'find-file-root-hook))))

(global-set-key [(control x) (control r)] 'find-file-root)

(defface find-file-root-header-face
  '((t (:foreground "white" :background "red3")))
  "*Face use to display header-lines for files opened as root.")

(defun find-file-root-header-warning ()
  "*Display a warning in header line of the current buffer.
   This function is suitable to add to `find-file-root-hook'."
  (let* ((warning "WARNING: EDITING FILE AS ROOT!")
   	 (space (+ 6 (- (window-width) (length warning))))
   	 (bracket (make-string (/ space 2) ?-))
   	 (warning (concat bracket warning bracket)))
    (setq header-line-format
   	  (propertize  warning 'face 'find-file-root-header-face))))

(defun find-file-hook-root-header-warning ()
  (when (and buffer-file-name (string-match "root@localhost" buffer-file-name))
    (find-file-root-header-warning)))

(add-hook 'find-file-root-hook 'find-file-hook-root-header-warning)

(defun joe/comment-or-uncomment-line ()
  "Either comments the current line or uncomments it."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position)
                               (line-end-position))
  (indent-according-to-mode))

(defun hardWrap-to-softWrap (beg end)
  "Covert hard wrapped lines into soft wrapped."
  (interactive "r")
  (shell-command-on-region beg end "fmt -w2000" nil t))

(defun get-or-create-term-buffer ()
  "Switch to the *terminal* buffer if there is one, else create one."
  (interactive)
  (switch-to-buffer
   (or (get-buffer "*terminal*")
       (make-term "terminal" (getenv "SHELL"))))
  (term-char-mode))

(defun unfill-paragraph (&optional region)
  "Replace newline characters with a single space."
  (interactive)
  (let ((fill-column 90002000))
    (if mark-active
        (fill-region (region-beginning) (region-end))
      (fill-paragraph))))


(defun move-to-char (arg char)
  "Move to ARG'th occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current
buffer. Goes backward if ARG is negative; error if CHAR not
found. Trivial modification of zap-to-char from GNU Emacs
22.2.1."
  (interactive "p\ncMove to char: ")
  (when (char-table-p translation-table-for-input)
    (setq char (or (aref translation-table-for-input char) char)))
  (search-forward (char-to-string char) nil nil arg)
  (goto-char (1+ (match-beginning 0))))

(defun toggle-line-spacing ()
  "Toggle line spacing between 1 and 5 pixels."
  (interactive)
  (if (not (eq line-spacing nil))
      (setq-default line-spacing nil)
    (setq-default line-spacing 7))
  )


(global-set-key "\C-x\;" 'joe/comment-or-uncomment-line)
(global-set-key "\C-w" 'my-smart-kill)
(global-set-key "\C-cv" 'my-copy-line)
(global-set-key "\C-cs" 'just-one-space)
(global-set-key "\C-o" 'open-line-and-indent)
(global-set-key "\C-ct" 'get-or-create-term-buffer)
(global-set-key "\M-Q" 'unfill-paragraph)