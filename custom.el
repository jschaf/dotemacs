;;; Personal customizations for emacs separate from customize.

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

(setq bookmark-default-file "~/.emacs.d/.emacs.bmk")

;; Useful for regexps.
(put 'narrow-to-region 'disabled nil)

;; Helpful for csv files.
(put 'scroll-left 'disabled nil)

;; Ido options
(setq ido-ignore-files '("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "\\`b~")
      ido-use-filename-at-point nil)

;; Tramp
(setq tramp-default-method "ssh"
      tramp-temp-name-prefix "~/.emacs.d/private/tramp.")

;; Emacs backups
(setq auto-save-list-file-prefix "~/.emacs.d/private/auto-save-list/.saves-"
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

;; Save window configs
(desktop-save-mode 1)
(setq desktop-path '("~/.emacs.d/private" "." "~"))

;; Restructured Text
(setq rst-level-face-base-light 60)

;;; Usability tweaks
(show-paren-mode 1)
(column-number-mode 1)
(winner-mode 1)

;; Aliases
(defalias 'dtw 'delete-trailing-whitespace)
(defalias 'ar  'align-regexp)
(defalias 'rr  'replace-regexp)
(defalias 'qrr 'query-replace-regexp)
(defalias 'ap 'apropos)
(defalias 'cv 'customize-variable)
(defalias 'cg 'customize-group)
(defalias 'ttl 'toggle-truncate-lines)

;; Hudson
(add-hook 'hudson-mode-hook
          (lambda ()
            (local-set-key "\C-m" 'hudson-newline-and-indent)
            (else-mode 1)
            (hs-minor-mode)
            (subword-mode 1)))

(setq hudson-jar-file (expand-file-name "~/prog/hudson/hudson.jar"))

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

;; Redefine the 8 primary terminal colors to look good against black
(setq ansi-term-color-vector
      [unspecified "#000000" "#963F3C" "#5FFB65" "#FFFD65"
                   "#0082FF" "#FF2180" "#57DCDB" "#FFFFFF"])

;; Help
(add-hook 'help-mode-hook
          (lambda ()
            (local-set-key "j" (lambda () (interactive) (scroll-up 1)))
            (local-set-key "k" (lambda () (interactive) (scroll-down 1)))
            (local-set-key "l" 'help-go-back)
            (local-set-key "h" 'help-go-forward)))

;; Dired
(setq dired-listing-switches "-alh")

(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")
            ;; Don't show dot files in dired
            (setq dired-omit-files
                  (concat dired-omit-files "\\|^\\..+$")
                  )))

(add-hook 'dired-mode-hook
          (lambda ()
            (dired-omit-mode 1)))

;; Shime

(add-hook 'shime-mode-hook
          '(lambda ()
             (global-set-key [f6] '(lambda ()
                                     (interactive)
                                     (switch-to-buffer "*shime*")))
             (global-set-key [f7] '(lambda ()
                                     (interactive)
                                     (shime-kill-session-by-name "shime")))))

;; Emacs lisp
(add-hook 'emacs-lisp-mode-hook
	  '(lambda ()
             (subword-mode 1)
             (hs-minor-mode 1)
             (turn-on-eldoc-mode)
             (enable-paredit-mode)))

(defun shime-elisp-hooks ()
 (font-lock-add-keywords nil '(("(\\(if-let\\|when-let\\|shime-with-[^ ]+\\)\\>"
                                1 font-lock-keyword-face))))

(add-hook 'emacs-lisp-mode-hook 'shime-elisp-hooks)


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
            (key-chord-define ada-mode-map "ja"
                              (lambda () (interactive) (insert ":= ")))
            (key-chord-define ada-mode-map "jd"
                              (lambda () (interactive) (insert "_")))
            (local-set-key [(meta \')] 'else-kill-placeholder)))

(defun ada-incr-variable (&optional arg)
  "Increment or decrement the variable before the point by ARG.
  If ARG is positive then increment the variable, else decrement
  the variable."
  (interactive "p")
  (save-excursion (re-search-backward "[ \t]+\\([a-z_0-9]+\\)"
                                      (line-beginning-position)
                                      'noerror))
  (just-one-space)
  (insert (format ":= %s %s %d;"
                  (match-string 1)
                  (if (<= 0 arg) "+" "-")
                  (abs arg))))

;; ELSE
(add-hook 'else-mode-hook
          (lambda ()
            (local-set-key "\M-n" 'else-next-placeholder)
            (local-set-key "\M-p" 'else-previous-placeholder)
            (local-set-key "\M-i" 'else-insert-placeholder)
            (local-set-key "\M-o" 'else-expand-placeholder)
            (local-set-key "\M-k" 'else-kill-placeholder)
            (local-set-key "\M-'" 'else-kill-proceed-to-next-placeholder)))

;; Haskell
(add-hook 'haskell-mode-hook
          (lambda ()
            (key-chord-define haskell-mode-map "ja"
                              (lambda () (interactive) (insert "-> ")))
            (require 'hs-lint)
	    (local-set-key "\C-c\C-c" 'shime-cabal-build)
	    (local-set-key "\C-cc" 'shime-cabal-ido)
            ;; (scion-flycheck-on-save 1)
            ;; (scion-mode 1)
	    (local-set-key [f5] 'shime-load-file)
            (local-set-key "\C-cl" 'hs-lint)))

;; LaTeX
(add-hook 'latex-mode-hook
          (lambda ()
            (turn-on-reftex)
            (set (make-local-variable sentence-end) "[.?!][]\"')}]*\\($\\|     \\|  \\)[
]*")
            (set-face-attribute 'font-latex-sedate-face nil
                                :foreground "red")))

;; Automatically reload pdf/dvi files when changed.
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

;; Misc
(global-set-key "\C-ha" 'apropos)
(global-set-key (kbd "<f1>") 'menu-bar-mode)
(global-set-key (kbd "C-M-/") (lambda () (interactive) (kill-buffer nil)))
(global-set-key "\C-cs" (lambda () (interactive) (switch-to-buffer "*scratch*")))
