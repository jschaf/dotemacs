;;; misc.el --- Personal customizations for Emacs.

;;; Commentary:

;;; Code:

;; Unnecessary.
(setq inhibit-startup-screen t
      initial-scratch-message "")

;; Recent file mode.
(require 'recentf)
(recentf-mode 1)
(setq-default recentf-save-file "~/.emacs.d/private/.recentf")

;; Highlight when mark is active
(setq transient-mark-mode t)

;; Ignore all local variables.  The endless prompting is annoying.
(setq-default enable-local-variables :safe)

;; Kill and yank use the clipboard.
(setq x-select-enable-clipboard t)

;; Disable tooltip help.  The mouse help is annoying and blocks the
;; minibuffer.
(setq show-help-function nil)

;; Follow symlinks to source controlled files without prompting.
(setq vc-follow-symlinks t)

;; Replace yes with y.
(fset 'yes-or-no-p 'y-or-n-p)

(setq-default bookmark-default-file "~/.emacs.d/private/bookmarks")

;; Useful for regexps.
(put 'narrow-to-region 'disabled nil)

;; Helpful for csv files.
(put 'scroll-left 'disabled nil)

;; Scrolling goes back to the same place.  It's less jarring to figure
;; out where you are.
(setq scroll-preserve-screen-position 'keep)

;; Store our the position we last visited.
(require 'saveplace)
(setq save-place t
      save-place-file "~/.emacs.d/private/save-place")

;; Ido
(ido-mode t)
(ido-ubiquitous-mode 1)
(setq ido-use-faces nil)

;; (flx-ido-mode 1)
(ido-vertical-mode)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point nil)
;; (setq ido-auto-merge-work-directories-length -1)
(add-hook 'ido-setup-hook
	  (lambda ()
	    (setq ido-enable-flex-matching t)))
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
(setq-default auto-save-list-file-prefix "~/.emacs.d/private/auto-save-list/.saves-"
              backup-by-copying t
              backup-directory-alist  '(("." . "~/.emacs.d/private/.emacs-backups"))
              kept-new-versions 3
              delete-old-versions t
              version-control t)

;; Completion options
(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

(setq completion-ignored-extensions
      '(".o" "~" ".bin" ".bak" ".obj" ".map" ".ico" ".pif" ".lnk" ".a" ".ln"
        ".blg" ".aux" ".dvi" ".toc" ".out" ".snm" ".pyc"))

(setq completion-pcm-word-delimiters "-_. ")

;; Never allow tabs
(setq-default indent-tabs-mode nil)


;;; Usability tweaks

;; Toggle visualization of matching parens.
(show-paren-mode 1)

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

(add-hook 'dired-mode-hook
          (lambda ()
            (local-set-key "\C-\M-k" 'dired-kill-subdir)
            ;;(dired-omit-mode 1)
            ))

(keydef "<RET>" newline-and-indent)

;; All programming modes
(defvar my:prog-mode-hooks
  '(my:add-watchwords
    my:local-comment-auto-fill))

(loop for hook in my:prog-mode-hooks
      do
      (add-hook 'prog-mode-hook hook))


;; Emacs Lisp
(defvar my:elisp-hooks
  '(enable-paredit-mode
    flycheck-mode
    hs-minor-mode
    my:delete-trailing-whitespace-before-save
    my:enable-auto-complete-mode
    my:maybe-byte-compile-after-save
    my:pretty-lambdas
    ;; my:show-column-80
    rainbow-delimiters-mode
    subword-mode
    turn-on-eldoc-mode
    turn-on-page-break-lines-mode
    elisp-slime-nav-mode))

(loop for hook in my:elisp-hooks
      do
      (add-hook 'emacs-lisp-mode-hook hook))

(loop for (key . func) in
      '(("g." . elisp-slime-nav-find-elisp-thing-at-point)
        ("g," . pop-tag-mark)
        ("gh" . elisp-slime-nav-describe-elisp-thing-at-point))
      do
      (evil-define-key 'normal emacs-lisp-mode-map key func)
      (evil-define-key 'normal lisp-interaction-mode-map key func)
      (evil-define-key 'motion emacs-lisp-mode-map key func)
      (evil-define-key 'motion lisp-interaction-mode-map key func))

(defvar my:compilation-hooks
  '(page-break-lines-mode))

(loop for hook in my:compilation-hooks
      do
      (add-hook 'compilation-mode-hook hook))


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
(add-hook 'python-mode-hook 'hs-minor-mode)

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

;; LaTeX
(add-hook 'latex-mode-hook
          (lambda ()
            (turn-on-reftex)
            (set (make-local-variable sentence-end)
                 "[.?!][]\"')}]*\\($\\|     \\|  \\)[
]*")))

;; Automatically reload files when changed.
(global-auto-revert-mode 1)

;; Eshell
(setq-default eshell-directory-name "~/.emacs.d/private/eshell")

;; Emacs server
(setq-default server-auth-dir "~/.emacs.d/private/server")

(eval-after-load 'magit
  '(progn
     (add-hook 'magit-mode-hook
               '(lambda ()
                  (local-set-key "j" #'evil-next-line)
                  (local-set-key "k" #'evil-previous-line)))))
;; Misc
(global-set-key "\C-ha" 'apropos)
(global-set-key (kbd "<f1>") 'menu-bar-mode)

(autoload 'toggle-uniquify-buffer-names "uniquify" nil t)
(toggle-uniquify-buffer-names)

;; Local Variables:
;; lexical-binding: t
;; End:

(provide 'misc)
;;; misc.el ends here
