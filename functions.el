;;; functions.el --- Personal functions for Emacs.

;;; Commentary:
;;

;;; Code:

(eval-when-compile
 (require 'cl-lib))

(defun kill-region-or-backward-kill-word ()
  "`kill-region' if the mark is active, else `backward-kill-word'."
  (interactive)
  (if mark-active
      (call-interactively 'kill-region)
    (call-interactively 'backward-kill-word)))

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

(defun my:save-buffer (&optional args)
  "If trying to save the *scratch* buffer, do nothing."
  (interactive)
  (unless (eq (current-buffer) (get-buffer "*scratch*"))
    (save-buffer args)))

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn (rename-file name new-name 1) (rename-buffer new-name)
               (set-visited-file-name new-name) (set-buffer-modified-p nil))))))

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
  (set (make-local-variable 'whitespace-line-column) (max 80 fill-column))
  (whitespace-mode 1))

(defun my:show-column-80 ()
  "Enable a rule at column 80."
  (interactive)
  (require 'fill-column-indicator)
  (setq fci-rule-column 79
        fci-rule-width 1
        fci-always-use-textual-rule t
        fci-rule-color "#E8E2D0"
        fci-rule-character ?│)
  (fci-mode 1))

(defun my:local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(defun my:pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(defun my:add-hooks (mode functions)
  "Call `add-hook' for each function in FUNCTIONS into MODE."
  (loop for hook in functions
        do
        (add-hook mode hook)))

(defun my:add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
          1 font-lock-warning-face t))))

(defun my:enable-auto-complete-mode ()
  "Enable auto-complete mode."
  (require 'auto-complete-config)
  (auto-complete-mode 1))

(defun my:evil-define-keys (states keymaps key def &rest bindings)
  "Run `evil-define-key' over all STATES and KEYMAPS."
  (let* ( ;; Keep bindings unmodified to use for all iterations
         (bindings (append (list key def) bindings))
         ;; Destructively modify k, d and bs.
         bs k d)
    (dolist (state states)
      (dolist (keymap keymaps)
        (setq bs bindings)
        (while bs
          (setq k (pop bs)
                d (pop bs))
          (evil-define-key state keymap k d)
          (message "defined state %s in %s of %s:%s" state keymap k d))))))

(global-set-key "\C-w" 'kill-region-or-backward-kill-word)

;; Local Variables:
;; lexical-binding: t
;; End:

(provide 'functions)

;;; functions.el ends here
