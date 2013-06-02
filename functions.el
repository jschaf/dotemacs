(eval-when-compile
 (require 'cl-lib))

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying 'Active processes exist' query when you quit Emacs."
  (cl-flet ((process-list ())) ad-do-it))

(defun kill-region-or-backward-kill-word ()
  "`kill-region' if the mark is active, else `backward-kill-word'."
  (interactive)
  (if mark-active
      (call-interactively 'kill-region)
    (call-interactively 'backward-kill-word)))

(defun copy-line (arg)
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

(defun comment-or-uncomment-line ()
  "Either comments the current line or uncomments it."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position)
                               (line-end-position)))

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

(defun toggle-line-spacing ()
  "Toggle line spacing between 1 and 5 pixels."
  (interactive)
  (if (not (eq line-spacing nil))
      (setq-default line-spacing nil)
    (setq-default line-spacing 7)))

(defun ido-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
   ((not symbol-list)
    (let ((ido-mode ido-mode)
          name-and-pos symbol-names position)
      (unless ido-mode
        (ido-mode 1)
        (setq ido-enable-flex-matching t))
      (while (progn
               (imenu--cleanup)
               (setq imenu--index-alist nil)
               (ido-goto-symbol (imenu--make-index-alist))
	       (setq initial-entry (car (member (thing-at-point 'symbol) symbol-names)))
               (setq selected-symbol
                     (ido-completing-read "Symbol? " symbol-names nil nil initial-entry))
               (string= (car imenu--rescan-item) selected-symbol)))
      (setq position (cdr (assoc selected-symbol name-and-pos)))
      (cond
       ((overlayp position)
        (goto-char (overlay-start position)))
       (t
        (goto-char position)))
      (when (listp symbol-list)
        (dolist (symbol symbol-list)
          (let (name position)
            (cond
             ((and (listp symbol) (imenu--subalist-p symbol))
              (ido-goto-symbol symbol))
             ((listp symbol)
              (setq name (car symbol))
              (setq position (cdr symbol)))
             ((stringp symbol)
              (setq name symbol)
              (setq position
                    (get-text-property 1 'org-imenu-marker symbol))))
            (unless (or (null position) (null name)
                        (string= (car imenu--rescan-item) name))
              (add-to-list 'symbol-names name)
              (add-to-list 'name-and-pos (cons name position))))))))))

(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let* ((file-assoc-list
	  (mapcar (lambda (x)
		    (cons (file-name-nondirectory x)
			  x))
		  recentf-list))
	 (filename-list
	  (cl-remove-duplicates (mapcar #'car file-assoc-list)
                                :test #'string=))
	 (filename (ido-completing-read "Choose recent file: "
					filename-list
					nil
					t)))
    (when filename
      (find-file (cdr (assoc filename
			     file-assoc-list))))))

(setq my:old-theme 'solarized-dark
      my:new-theme 'solarized-light)

(defun toggle-color-theme ()
  "Switch between the light and dark versions of Solarized."
  (interactive)
  (let ((temp my:old-theme))
    (setq my:old-theme my:new-theme)
    (setq my:new-theme temp)
    (disable-theme my:old-theme)
    (load-theme my:new-theme t)))

(defun my:indent-defun-around-point ()
  "Indent the sexp that we're currently in."
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (indent-sexp)))

(defun my:yank-sexp ()
  "Yank the sexp in front of the point."
  (interactive)
  (save-excursion
    (mark-sexp)
    (kill-ring-save (point) (mark))))

(defun my:find-function-at-point-this-window ()
  "Find function at point."
  (interactive)
  (let ((symb (function-called-at-point)))
    (when symb
      (find-function symb))))

(defun info-mode ()
  "A simple function to open standalone info files correctly."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (kill-buffer (current-buffer))
    (info file-name)))
(add-to-list 'auto-mode-alist '("\\.info\\'" . info-mode))

(global-set-key "\C-cr" 'recentf-ido-find-file)
(global-set-key "\C-ci" 'ido-goto-symbol)
(global-set-key "\C-x\;" 'comment-or-uncomment-line)
(global-set-key "\C-w" 'kill-region-or-backward-kill-word)
(global-set-key "\C-cv" 'copy-line)
(global-set-key "\C-o" 'open-line-and-indent)
(global-set-key "\C-ct" 'get-or-create-term-buffer)
(global-set-key "\M-Q" 'unfill-paragraph)
