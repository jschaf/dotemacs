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
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil))))))

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
  (add-hook (make-local-variable 'before-save-hook)
            'delete-trailing-whitespace))

(defun my:highlight-long-lines ()
  "Highlight long lines."
  (interactive)
  (require 'whitespace)
  (set (make-local-variable 'whitespace-style) '(face lines-tail))
  (set (make-local-variable 'whitespace-line-column) (max 80 fill-column))
  (whitespace-mode 1))

(defun my:evil-setup ()
  "The initial customization for evil mode.

This is separated into a function so I can edit it without
figuring out how to reload the package."
  (interactive)
  (evil-mode 1)

  ;; We need to add text before we can edit it.
  (add-to-list 'evil-insert-state-modes 'git-commit-mode)

  (setq evil-highlight-closing-paren-at-point-states nil)
  ;; Use different colors for fonts to easily determine what mode we're in.
  (setq evil-default-cursor "#0971B2")
  ;; (setq evil-default-cursor "#5EA0AD")
  (setq evil-normal-state-cursor evil-default-cursor)
  (setq evil-insert-state-cursor "#AD5E5E")
  (setq evil-visual-state-cursor evil-default-cursor)
  (setq evil-replace-state-cursor evil-default-cursor)
  (setq evil-operator-state-cursor nil)
  (setq evil-motion-state-cursor evil-default-cursor)
  (setq evil-emacs-state-cursor "#00FF48")

  (setq evil-want-visual-char-semi-exclusive t)
  (setq evil-move-cursor-back nil)

  (defmacro my:make-evil-line-move-motion (name multiplier)
    `(evil-define-motion ,name (count)
       ,(format "Move the cursor (COUNT * %s) lines down." multiplier)
       :type line
       (let (line-move-visual)
         (evil-line-move (* ,multiplier (or count 1))))))

  (my:make-evil-line-move-motion my:evil-next-line-5 5)
  (my:make-evil-line-move-motion my:evil-previous-line-5 -5)
  (my:make-evil-line-move-motion my:evil-next-line-3 3)
  (my:make-evil-line-move-motion my:evil-previous-line-3 -3)

  ;; Make movement keys work on visual lines instead of acutal lines.
  ;; This imitates Emacs behavior rather than Vim behavior.
  (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>")
    'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>")
    'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>")
    'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>")
    'evil-previous-visual-line)

  ;; Make horizontal movement cross lines
  (setq-default evil-cross-lines t)

  ;; Commands for both the normal, motion and visual state
  (loop for (key . func) in
        `(("zk" . beginning-of-defun)
          ("zj" . end-of-defun)
          ("zl" . forward-sexp)
          ("zh" . backward-sexp)
          ("zu" . paredit-backward-up)
          ("J" . my:evil-next-line-5)
          ("K" . my:evil-previous-line-5)
          ("\M-j" . my:evil-next-line-3)
          ("\M-k" . my:evil-previous-line-3)
          ("gj" . evil-join)
          ("H" . my:back-to-indentation-or-beginning)
          ("L" . evil-end-of-line)
          ("zdy" . my:yank-sexp)
          ("\C-j" . scroll-up-command)
          ("\C-k" . scroll-down-command))
        do
        (define-key evil-normal-state-map key func)
        (define-key evil-visual-state-map key func)
        (define-key evil-motion-state-map key func))

  (define-key evil-insert-state-map (kbd "C-<return>") 'evil-open-below)

  ;; Commands for only the normal state map
  (loop for (key . func) in
        `((,(kbd "<tab>")  . indent-for-tab-command)
          ("z," . comment-dwim)
          ("zn" . evil-toggle-fold)
          ("z;" . comment-or-uncomment-line))
        do
        (define-key evil-normal-state-map key func))

  ;; Paredit Mode
  (add-hook 'paredit-mode-hook
            (lambda () (loop for (key . func) in
                        '(("zsh" . paredit-backward)
                          ("zsl" . paredit-forward)
                          ("zsj" . paredit-forward-down)
                          ("zsk" . paredit-backward-up)
                          ("zdl" . paredit-forward-barf-sexp)
                          ("zdh" . paredit-backward-barf-sexp)
                          ("zfh" . paredit-backward-slurp-sexp)
                          ("zfl" . paredit-forward-slurp-sexp)
                          ("zfc" . paredit-convolute-sexp)
                          ("z9" . paredit-wrap-round)
                          ("zdk" . kill-sexp)
                          ("zss" . paredit-splice-sexp)
                          ("zsd" . paredit-join-sexps)
                          ("z'" . paredit-meta-doublequote))
                        do (define-key evil-normal-state-map key func))))

  (add-hook 'Info-mode-hook
            (lambda () (loop for (key . func) in
                        '(("H" . Info-history-back)
                          ("L" . Info-history-forward))
                        do (define-key Info-mode-map key func)))))

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
  (cl-loop for hook in functions
        do
        (add-hook mode hook)))

(defun not-in-minibuffer (fn &rest args)
  "Execute FN normally, but in the minibuffer, do nothing.
Apply ARGS normally."
  `(lambda ()
     (interactive)
     (unless (window-minibuffer-p)
       (apply (quote ,fn) ,args))))

(defun my:add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
          1 font-lock-warning-face t))))

(defun my:esc ()
  "Functionality for escaping generally."
  (interactive)
  (cond
   ;; If we're in one of the Evil states return to the normal-state
   ((or (evil-insert-state-p) (evil-normal-state-p) (evil-replace-state-p)
        (evil-visual-state-p))
    (evil-normal-state))

   ((window-minibuffer-p)
    (abort-recursive-edit))

   ((string-prefix-p "*magit-key" (buffer-name))
    (magit-key-mode-command nil))

   (t (keyboard-quit))))

;; Exit isearch by pressing jk, see
;; http://stackoverflow.com/questions/20926215
(defun my:isearch-exit-chord-worker (&optional arg)
  (interactive "p")
  (execute-kbd-macro (kbd "<backspace> <return>")))

(defun my:isearch-exit-chord (arg)
  (interactive "p")
  (isearch-printing-char)
  (unless (fboundp 'smartrep-read-event-loop)
    (require 'smartrep))
  (run-at-time 0.3 nil 'keyboard-quit)
  (condition-case e
    (smartrep-read-event-loop
      '(("j" . my:isearch-exit-chord-worker)
         ("k" . my:isearch-exit-chord-worker)))
    (quit nil)))
(define-key isearch-mode-map "j" 'my:isearch-exit-chord)
(define-key isearch-mode-map "k" 'my:isearch-exit-chord)

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


(defun my:toggle-identifier-naming-style ()
  "Toggles the symbol at point between C-style naming,
e.g. `hello_world_string', and camel case,
e.g. `HelloWorldString'."
  (interactive)
  (let* ((symbol-pos (bounds-of-thing-at-point 'symbol))
         case-fold-search symbol-at-point cstyle regexp func)
    (unless symbol-pos
      (error "No symbol at point"))
    (save-excursion
      (narrow-to-region (car symbol-pos) (cdr symbol-pos))
      (setq cstyle (string-match-p "_" (buffer-string))
            regexp (if cstyle "\\(?:\\_<\\|_\\)\\(\\w\\)" "\\([A-Z]\\)")
            func (if cstyle
                     'capitalize
                   (lambda (s)
                     (concat (if (= (match-beginning 1)
                                    (car symbol-pos))
                                 ""
                               "_")
                             (downcase s)))))
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (replace-match (funcall func (match-string 1))
                       t nil))
      (widen))))


(global-set-key "\C-w" 'kill-region-or-backward-kill-word)

;; Local Variables:
;; lexical-binding: t
;; End:

(provide 'functions)

;;; functions.el ends here
