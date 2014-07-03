;;; misc.el --- Personal customizations for Emacs.

;;; Commentary:

(eval-when-compile
 (require 'cl-lib))

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
(setq-default enable-local-variables t)

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

;; Set the path
;; (when (string-equal system-type "windows-nt")
;;   (let ((mypaths '("C:/msys64/bin")))
;;     (setenv "PATH" (concat (mapconcat 'identity mypaths ";")
;;                            ";"
;;                            (getenv "PATH")))
;;     (setq exec-path (append mypaths (list "." exec-directory)))))


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
(require 'ido)
(ido-mode t)
(setq ido-use-faces nil)

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

(add-hook 'dired-mode-hook
          (lambda ()
            (local-set-key "\C-\M-k" 'dired-kill-subdir)
            ;;(dired-omit-mode 1)
            ))

(global-set-key (kbd "RET") 'newline-and-indent)

;; All programming modes
(my:add-hooks 'prog-mode-hook
  '(my:add-watchwords
    my:delete-trailing-whitespace-before-save
    my:local-comment-auto-fill))

(my:add-hooks 'LaTeX-mode-hook
  '(my:add-watchwords
    my:delete-trailing-whitespace-before-save
    my:local-comment-auto-fill))

;; Emacs Lisp
(my:add-hooks 'emacs-lisp-mode-hook
              '(;; flycheck-mode
                hs-minor-mode
                my:maybe-byte-compile-after-save
                my:pretty-lambdas
                subword-mode
                turn-on-eldoc-mode))

;; Use C-x C-e to pretty print expressions
(global-set-key [remap eval-last-sexp] 'pp-eval-last-sexp)

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
    my:pretty-lambdas))

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

(eval-after-load 'erc
  '(progn
     (setq erc-hide-list '("JOIN" "PART" "QUIT"))))

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

;; Automatically reload files when changed.
(global-auto-revert-mode 1)

;; Eshell
(setq-default eshell-directory-name "~/.emacs.d/private/eshell")

;; Misc
(global-set-key "\C-ha" 'apropos)
(global-set-key "\C-h\C-w" 'where-is)
(global-set-key (kbd "<f1>") 'menu-bar-mode)

(autoload 'toggle-uniquify-buffer-names "uniquify" nil t)
(toggle-uniquify-buffer-names)

(setq eval-expression-print-level nil
      eval-expression-print-length nil)

;; TODO: make comment-out work for regions
;; TODO: make jk exit isearch and evil movement commands (e.g. after
;; pressing d)
;; TODO: don't highlight parens in comments
;; TODO: use normal quotes in comments in auctex
(when (eq system-type 'darwin)
  ;; Don't use option to input special chars, use it as alt like it
  ;; was meant to be.
  (setq mac-option-modifier 'control
        mac-command-modifier 'meta))

;; Local Variables:
;; lexical-binding: t
;; End:

(provide 'misc)
;;; misc.el ends here
