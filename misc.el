;;; misc.el --- Personal customizations for Emacs.

;;; Commentary:

(require 'functions)

;;; Code:

;; Unnecessary.
(setq inhibit-startup-screen t
      initial-scratch-message "")

;; Recent file mode.
(require 'recentf)
(recentf-mode 1)
(setq-default recentf-save-file "~/.emacs.d/private/.recentf")

;; Saveplace - go to the last point when opening a previously closed
;; file.
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/private/places")

;; Highlight when mark is active
(setq transient-mark-mode t)

;; Ignore all safe local variables.  The endless prompting is
;; annoying.
(setq-default enable-local-variables :safe)

;; Kill and yank use the clipboard.
(setq x-select-enable-clipboard t)

;; Disable tooltip help.  The mouse help is annoying and blocks the
;; minibuffer.
(setq show-help-function nil)

;; TODO: why doesn't minibuffer-avoid-prompt work?
(setq minibuffer-prompt-properties
      (quote (point-entered minibuffer-avoid-prompt
              read-only t
              face minibuffer-prompt)))

;; Follow symlinks to source controlled files without prompting.
(setq vc-follow-symlinks t)

;; Replace yes with y.
(fset 'yes-or-no-p 'y-or-n-p)

;; Disable cursor blinking
(blink-cursor-mode 0)

(setq-default bookmark-default-file "~/.emacs.d/private/bookmarks")

;; Useful for regexps.
(put 'narrow-to-region 'disabled nil)

;; Helpful for csv files.
(put 'scroll-left 'disabled nil)

;; Scrolling goes back to the same place.  It's less jarring to figure
;; out where you are.
(setq scroll-preserve-screen-position 'keep)

;; Buffer cycling
(keydef "C-M-j" bs-cycle-next)
(keydef "C-M-k" bs-cycle-previous)

;; Store our the position we last visited.
(require 'saveplace)
(setq save-place t
      save-place-file "~/.emacs.d/private/save-place")

;; Ido
(require 'ido)
(ido-mode t)
(ido-ubiquitous-mode 1)
(setq ido-use-faces nil)
(flx-ido-mode 1)
(ido-vertical-mode)

(setq ido-ignore-files
      '("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "\\`b~")
      ido-use-filename-at-point nil)
(setq ido-save-directory-list-file "~/.emacs.d/private/.ido.last")

;; Turn off the bell
(setq ring-bell-function 'ignore)

;; Tramp
(setq tramp-default-method "ssh"
      tramp-temp-name-prefix "~/.emacs.d/private/tramp.")

;; Emacs backups
(setq-default auto-save-list-file-prefix
              "~/.emacs.d/private/auto-save-list/.saves-"
              backup-by-copying t
              backup-directory-alist
              '(("." . "~/.emacs.d/private/.emacs-backups"))
              kept-new-versions 3
              delete-old-versions t
              version-control t)

;; Completion options
(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

(nconc completion-ignored-extensions
       '(".pyc" ".toc" ".aux" ".blg" ".ln" ".a" ".lnk" ".pif" ".ico" ".map"
         ".obj" ".bak" ".bin" "~" ".o"))

;; Never allow tabs
(setq-default indent-tabs-mode nil)


;;; Usability tweaks

;; Toggle column number display in the mode line.
(column-number-mode 1)

;; Registers
(set-register ?i '(file . "~/.emacs.d/init.el"))
(set-register ?a '(file . "~/.emacs.d/autoloads.el"))
(set-register ?f '(file . "~/.emacs.d/functions.el"))
(set-register ?c '(file . "~/.emacs.d/misc.el"))
(set-register ?e '(file . "~/.emacs.d/el-get/esup/esup.el"))

;; Help
(keydef (help "k") (scroll-down 1))
(keydef (help "L") help-go-back)
(keydef (help "H") help-go-back)
(keydef (help "<tab>") forward-button)
(keydef (help "<shift>-<tab>") backward-button)

(add-hook 'dired-mode-hook
          (lambda ()
            (local-set-key "\C-\M-k" 'dired-kill-subdir)
            ;;(dired-omit-mode 1)
            ))

(keydef "<RET>" newline-and-indent)

(run-with-idle-timer 1 nil
                     (lambda ()
                       (require 'yasnippet)
                       (setq yas-verbosity 0)
                       (yas-global-mode 1)))

;; All programming modes
(my:add-hooks 'prog-mode-hook
  '(my:add-watchwords
    git-gutter-mode
    my:show-column-80
    my:enable-auto-complete-mode
    my:local-comment-auto-fill))

;; Emacs Lisp
(my:add-hooks 'emacs-lisp-mode-hook
              '(enable-paredit-mode
                ;; flycheck-mode
                hs-minor-mode
                my:delete-trailing-whitespace-before-save
                my:maybe-byte-compile-after-save
                my:pretty-lambdas
                rainbow-delimiters-mode
                subword-mode
                turn-on-eldoc-mode
                turn-on-page-break-lines-mode
                elisp-slime-nav-mode))

(loop for (key . func) in
      '(("g." . elisp-slime-nav-find-elisp-thing-at-point)
        ("g," . pop-tag-mark)
        ("gh" . elisp-slime-nav-describe-elisp-thing-at-point))
      do
      (evil-define-key 'normal emacs-lisp-mode-map key func)
      (evil-define-key 'normal lisp-interaction-mode-map key func)
      (evil-define-key 'motion emacs-lisp-mode-map key func)
      (evil-define-key 'motion lisp-interaction-mode-map key func))

(my:add-hooks 'compilation-mode-hook
  '(page-break-lines-mode))

(my:add-hooks 'org-mode-hook
              '(auto-fill-mode))

(my:add-hooks 'rst-mode-hook
              '(auto-fill-mode))
;; Ada mode
(setq ada-case-attribute 'ada-loose-case-word
      ada-case-exception-file '("~/.emacs.d/.ada_case_exceptions")
      ada-case-identifier 'ada-loose-case-word
      ada-clean-buffer-before-saving nil
      ada-label-indent 0
      ada-language-version 'ada2005
      ada-move-to-declaration t
      ada-search-directories  '("." "$ADA_INCLUDE_PATH")
      ada-xref-create-ali nil
      ada-xref-other-buffer t)

;; Python
(my:add-hooks 'python-mode-hook
  '(hs-minor-mode
    my:pretty-lambdas
    virtualenv-minor-mode
    jedi:setup))

(eval-after-load 'python
  '(progn
     (setq python-shell-interpreter "ipython3"
           python-shell-interpreter-args ""
           python-shell-prompt-regexp "In \\[[0-9]+\\]: "
           python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
           python-shell-completion-setup-code
           "from IPython.core.completerlib import module_completion"
           python-shell-completion-module-string-code
           "';'.join(module_completion('''%s'''))\n"
           python-shell-completion-string-code
           "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")))

(eval-after-load 'jedi
  '(progn
     (setq jedi:complete-on-dot t)
     (loop for (key . func) in
           '(("g." . jedi:goto-definition)
             ("g," . jedi:goto-definition-pop-marker)
             ("gh" . jedi:show-doc))
           do
           (evil-define-key 'normal python-mode-map key func)
           (evil-define-key 'motion python-mode-map key func))))

;; SGML and HTML
(add-hook 'sgml-mode-hook 'emmet-mode)

;; CSS
(add-hook 'css-mode-hook 'emmet-mode)

;; Info customizations
(setenv "INFOPATH" (concat (expand-file-name "~/.emacs.d/info:")
                           (getenv "INFOPATH")))
(add-to-list 'Info-default-directory-list "~/.emacs.d/info")
(add-hook 'Info-mode-hook
          (lambda ()
            ;; Hide ^M, the carriage return
            (setq buffer-display-table (make-display-table))
            (aset buffer-display-table ?\^M [])
            (add-to-list 'Info-directory-list "~/.emacs.d/info")
    	    (setq Info-additional-directory-list Info-default-directory-list)))

;; Haskell
(my:add-hooks 'haskell-mode-hook
  '(turn-on-haskell-indentation))

;; LaTeX
(add-hook 'latex-mode-hook
          (lambda ()
            (turn-on-reftex)
            (set (make-local-variable sentence-end)
                 "[.?!][]\"')}]*\\($\\|     \\|  \\)[
]*")))

(defvar my:rust-compiled-buffer nil)

(defun my:rust-save-compile (&optional arg)
  (interactive "p")
  (save-buffer)
  (compile (concat "rustc " (buffer-file-name)))
  (setq my:rust-compiled-buffer (current-buffer)))

(defun my:run-in-eshell (buffer msg)
  (when (string-match "^finished" msg)
    (unless (get-buffer "*eshell*")
      (eshell))
    (with-current-buffer "*eshell*"
      (goto-char (point-max))
      (insert (file-name-sans-extension
               (buffer-file-name my:rust-compiled-buffer)))
      (eshell-send-input))
    (switch-to-buffer-other-window "*eshell*")))

(add-to-list 'compilation-finish-functions
             'my:run-in-eshell)

(keydef (rust "C-c C-c") my:rust-save-compile)

;; (add-hook 'rust-mode-hook
;;           )
;; Automatically reload files when changed.
(global-auto-revert-mode 1)

;; Eshell
(setq-default eshell-directory-name "~/.emacs.d/private/eshell")

;; We need to add text before we can edit it.
(add-to-list 'evil-insert-state-modes 'git-commit-mode)

(eval-after-load 'magit
  '(progn
     (defadvice magit-key-mode-popup-committing (after toggle-verbose-commits)
       "Enable the verbose option for commiting."
       (magit-key-mode-toggle-option 'committing "--verbose"))
     (ad-activate 'magit-key-mode-popup-committing)
     (add-hook 'magit-mode-hook
               '(lambda ()
                  (local-set-key "j" #'evil-next-line)
                  (local-set-key "k" #'evil-previous-line)))))
;; Misc
(global-set-key "\C-ha" 'apropos)
(global-set-key "\C-h\C-w" 'where-is)
(global-set-key (kbd "<f1>") 'menu-bar-mode)

;; Anzu mode - show the number of matches when searching
(global-anzu-mode 1)

;; Global project management mode.
(projectile-global-mode 1)

(eval-after-load 'git-gutter
  '(progn
     ;; Turn off annoying "here is not git repository" message
     (setq git-gutter:verbosity 0)))

(require 'smartparens)
(require 'smartparens-config)
(smartparens-global-mode 1)
(show-paren-mode 1)

(eval-after-load 'smartparens
  '(progn
     ;; (sp-local-pair 'jinja2-mode "{" nil :actions :rem)
     ;; (sp-local-pair 'jinja2-mode "{%" "%}")
     )
  )
(autoload 'toggle-uniquify-buffer-names "uniquify" nil t)
(toggle-uniquify-buffer-names)

(setq eval-expression-print-level nil
      eval-expression-print-length nil)

;; Local Variables:
;; lexical-binding: t
;; End:

(provide 'misc)
;;; misc.el ends here
