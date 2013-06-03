(eval-when-compile
 (require 'cl-lib))

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying 'Active processes exist' query when you quit Emacs."
  (cl-letf ((process-list ())) ad-do-it))

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
      (setq arg (1- arg))))
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

(defun unfill-paragraph ()
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

(defvar my:dark-theme 'solarized-dark)
(defvar my:light-theme 'solarized-light)
(defvar my:current-theme my:light-theme)
(load-theme my:current-theme t)

(defun toggle-color-theme ()
  "Switch between the `my:dark-theme' and `my:light-theme'."
  (interactive)
  (disable-theme my:current-theme)
  (if (eq my:current-theme my:dark-theme)
      (setq my:current-theme my:light-theme)
    (setq my:current-theme my:dark-theme))
  (load-theme my:current-theme t))

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


(defun my:back-to-indentation-or-beginning ()
  "Go back to indentation, or beginning of line on second press."
  (interactive)
  (if (eq last-command 'my:back-to-indentation-or-beginning)
      (evil-beginning-of-line)
    (evil-first-non-blank)))

(defun my:last-non-blank-or-end-of-line ()
  "Go to last non blank, or end of line on second press."
  (interactive)
  (if (eq last-command 'my:last-non-blank-or-end-of-line)
      (evil-end-of-line)
    (evil-last-non-blank)))

(defun my:maybe-byte-compile ()
  "Byte compile current file if .elc file exists."
  (interactive)
  (when (file-exists-p (byte-compile-dest-file buffer-file-name))
    (byte-compile-file buffer-file-name)))

(defun my:maybe-byte-compile-after-save ()
  "Add `my:maybe-byte-compile' to the local `after-save-hook'."
  (add-hook (make-local-variable 'after-save-hook) 'my:maybe-byte-compile))

(defun my:delete-trailing-whitespace-before-save ()
  "Add `delete-trailing-whitespace' to the local `after-save-hook'."
  (add-hook (make-local-variable 'before-save-hook) 'delete-trailing-whitespace))

(defun my:highlight-long-lines ()
  "Highlight long lines."
  (interactive)
  (require 'whitespace)
  (set (make-local-variable 'whitespace-style) '(face lines-tail))
  (set (make-local-variable 'whitespace-line-column) 80)
  (whitespace-mode 1))

(defun my:evil-define-keys (states keymaps key def &rest bindings)
  "Run `evil-define-key' over all STATES and KEYMAPS."
  (loop for state in states
        for keymap in keymaps
        do
        (evil-define-key state keymap key def)
        (let (k d (b bindings))
             (while bindings
               (setq k (nth 0 bindings)
                     d (nth 1 bindings)
                     bindings (cddr bindings))
               (evil-define-key state keymap k d)))))

(global-set-key "\C-cr" 'recentf-ido-find-file)
(global-set-key "\C-ci" 'ido-goto-symbol)
(global-set-key "\C-x\;" 'comment-or-uncomment-line)
(global-set-key "\C-w" 'kill-region-or-backward-kill-word)
(global-set-key "\C-cv" 'copy-line)
(global-set-key "\C-o" 'open-line-and-indent)
(global-set-key "\M-Q" 'unfill-paragraph)

;; Enable lexical binding.
;;
;; Local Variables:
;; lexical-binding: t
;; End:
