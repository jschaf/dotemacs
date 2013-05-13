;;; Personal customizations for Emacs

;; Unnecessary.
(setq inhibit-startup-screen t
      initial-scratch-message "")

;; Recent file mode.
(setq recentf-mode t
      recentf-save-file "~/.emacs.d/private/.recentf")

;; Highlight when mark is active
(setq transient-mark-mode t)

;; Kill and yank use the clipboard.
(setq x-select-enable-clipboard t)

;; Disable tooltip help.  The mouse help is annoying and blocks the
;; minibuffer.
(setq show-help-function nil)

;; Follow symlinks to source controlled files without prompting.
(setq vc-follow-symlinks t)

;; Replace yes with y.
(fset 'yes-or-no-p 'y-or-n-p)

(setq bookmark-default-file "~/.emacs.d/private/bookmarks")

;; Useful for regexps.
(put 'narrow-to-region 'disabled nil)

;; Helpful for csv files.
(put 'scroll-left 'disabled nil)

;; Ido
(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point nil)
(setq ido-auto-merge-work-directories-length -1)
(add-hook 'ido-setup-hook
	  (lambda ()
	    (setq ido-enable-flex-matching t)))
(setq ido-ignore-files '("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "\\`b~")
      ido-use-filename-at-point nil)
(setq ido-save-directory-list-file "~/.emacs.d/private/.ido.last")

;; Turn off the bell
(setq ring-bell-function 'ignore)

;; Tramp
(setq tramp-default-method "ssh"
      tramp-temp-name-prefix "~/.emacs.d/private/tramp.")

;; Emacs backups
(setq-default auto-save-list-file-prefix "~/.emacs.d/private/auto-save-list/.saves-"
              backup-by-copying t
              backup-directory-alist  '(("." . "~/.emacs.d/private/.emacs-backups"))
              kept-new-versions 3
              delete-old-versions t
              version-control t)

;; Completion options
(setq completion-ignored-extensions
      '(".o" "~" ".bin" ".bak" ".obj" ".map" ".ico" ".pif" ".lnk" ".a" ".ln"
        ".blg" ".aux" ".dvi" ".toc" ".out" ".snm" ".pyc"))

(setq completion-pcm-word-delimiters "-_. ")

;; Never allow tabs
(setq-default indent-tabs-mode nil)

;; Change default highlight level for headers so it's easier to see
;; against a dark background.
;; (setq rst-level-face-base-light 60)

;;; Usability tweaks

;; Toggle visualization of matching parens.
(show-paren-mode 1)

;; Toggle column number display in the mode line.
(column-number-mode 1)


;; Registers
(set-register ?i '(file . "~/.emacs.d/init.el"))
(set-register ?a '(file . "~/.emacs.d/autoloads.el"))
(set-register ?l '(file . "~/.emacs.d/colors.el"))
(set-register ?f '(file . "~/.emacs.d/functions.el"))
(set-register ?c '(file . "~/.emacs.d/custom.el"))

;; Aliases
(defalias 'dtw 'delete-trailing-whitespace)
(defalias 'ar  'align-regexp)
(defalias 'rr  'replace-regexp)
(defalias 'qrr 'query-replace-regexp)
(defalias 'ap 'apropos)
(defalias 'cv 'customize-variable)
(defalias 'cg 'customize-group)
(defalias 'ttl 'toggle-truncate-lines)

;; Hide-show
(add-hook 'hs-minor-mode-hook
          (lambda ()
            (local-set-key "\M-=" 'hs-toggle-hiding)
            (local-set-key "\M-_" 'hs-hide-all)
            (local-set-key "\M-+" 'hs-show-all)))

(add-hook 'compilation-mode-hook
          (lambda ()
            (local-set-key "\M-n" 'cycle-buffer)
            (local-set-key "\M-p" 'cycle-buffer-backward)
            ))

;; Help
(add-hook 'help-mode-hook
          (lambda ()
            (local-set-key "j" (lambda () (interactive) (scroll-up 1)))
            (local-set-key "k" (lambda () (interactive) (scroll-down 1)))
            (local-set-key "l" 'help-go-back)
            (local-set-key "h" 'help-go-forward)))

(add-hook 'dired-mode-hook
          (lambda ()
            (local-set-key "\C-\M-k" 'dired-kill-subdir)
            ;;(dired-omit-mode 1)
            ))


;; Emacs lisp
(add-hook 'emacs-lisp-mode-hook
	  '(lambda ()
             (subword-mode 1)
             (hs-minor-mode 1)
             (turn-on-eldoc-mode)
             (enable-paredit-mode)))


(add-hook 'lisp-interaction-mode-hook
          (lambda ()
            (local-set-key "\C-\M-j" 'eval-print-last-sexp)))

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

(add-hook 'ada-mode-hook
          (lambda ()
            (outline-minor-mode 1)
            (local-set-key "\M-_" 'hide-leaves)
            (local-set-key "\M-=" 'show-entry)
            (local-set-key "\C-cm" 'ada-method-header)
            (local-set-key "\C-xnd" 'ada-narrow-to-defun)
            ;; (local-set-key "\C-ci" 'joe/ada-incr-variable)
            (setq ada-fill-comment-postfix "-- ")
            (else-mode 1)
            (local-set-key [(meta \')] 'else-kill-placeholder)))

;; ELSE
(add-hook 'else-mode-hook
          (lambda ()
            (local-set-key "\M-n" 'else-next-placeholder)
            (local-set-key "\M-p" 'else-previous-placeholder)
            (local-set-key "\M-i" 'else-insert-placeholder)
            (local-set-key "\M-o" 'else-expand-placeholder)
            (local-set-key "\M-k" 'else-kill-placeholder)
            (local-set-key "\M-'" 'else-kill-proceed-to-next-placeholder)))

;; Python
(add-hook 'python-mode-hook
          (lambda ()
            (hs-minor-mode)
            ;; Set Paredit to not insert a space when inserting
            ;; parens, but only in python-mode
            (set (make-local-variable 'paredit-space-for-delimiter-predicates)
                 (list (function (lambda (endp delimiter) nil))))
            ))

;; Add Info for Python 2.7
(add-to-list 'Info-default-directory-list "~/.emacs.d/info")

;; LaTeX
(add-hook 'latex-mode-hook
          (lambda ()
            (turn-on-reftex)
            (set (make-local-variable sentence-end) "[.?!][]\"')}]*\\($\\|     \\|  \\)[
]*")
            (set-face-attribute 'font-latex-sedate-face nil
                                :foreground "red")))

;; Automatically reload files when changed.
(global-auto-revert-mode 1)

;; Eshell
(setq-default eshell-directory-name "~/.emacs.d/private/eshell")

;; Emacs server
(setq-default server-auth-dir "~/.emacs.d/private/server")

;; Misc
(global-set-key "\C-ha" 'apropos)
(global-set-key (kbd "<f1>") 'menu-bar-mode)
(global-set-key (kbd "C-M-/") (lambda () (interactive) (kill-buffer nil)))
(global-set-key "\C-cs" (lambda () (interactive) (switch-to-buffer "*scratch*")))

;; Evil
(setq evil-default-cursor "#5EA0AD")
(setq evil-motion-state-cursor evil-default-cursor)
(setq evil-normal-state-cursor evil-default-cursor)
(setq evil-insert-state-cursor "#AD5E5E")
(setq evil-operator-state-cursor nil)
(setq evil-want-visual-char-semi-exclusive t)
(setq evil-move-cursor-back nil)
(define-key evil-normal-state-map "\C-i" 'evil-indent-line)


(define-key evil-normal-state-map "\C-j" 'scroll-up-command)
(define-key evil-motion-state-map "\C-j" 'scroll-up-command)
(define-key evil-normal-state-map "\C-k" 'scroll-down-command)
(define-key evil-motion-state-map "\C-k" 'scroll-down-command)
;; Undefine , to use it as the leader key
(define-key evil-normal-state-map "," nil)
(define-key evil-motion-state-map "," nil)
(let ((leader-map (make-sparse-keymap)))
  (define-key evil-normal-state-map "," leader-map)
  (define-key evil-motion-state-map "," leader-map)
  (define-key leader-map "xg" 'magit-status)
  (define-key leader-map "ht" 'describe-text-properties)
  (define-key leader-map "k" '(lambda () (interactive) (kill-buffer nil)))
  (define-key leader-map "cs" '(lambda () (interactive) (switch-to-buffer "*scratch*")))
  (define-key leader-map "o" 'delete-blank-lines)
  (define-key leader-map "dt" 'delete-trailing-whitespace)
  (define-key leader-map "xh" 'mark-whole-buffer)
  (define-key leader-map "xd" 'ido-dired)
  (define-key leader-map "ei" 'el-get-install)
  (define-key leader-map "r" 'jump-to-register))
