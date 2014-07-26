;;; autoloads.el --- bootstrap packages to improve the Emacs experience.

;;; Commentary:

;; Use el-get to bootstrap packages.  el-get has the advantage that it
;; will automatically byte-compile, add auto-loads and install info
;; for our packages.

;;; Code:

;; Installing from Emacswiki is slow and insecure
(defvar el-get-install-skip-emacswiki-recipes t)

;; Require el-get and bootstrap if it doesn't exist.
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let ((el-get-master-branch t))
      (when el-get-master-branch
        (message "Bootstrapping el-get with master branch."))
      (goto-char (point-max))
      (eval-print-last-sexp))))

(setq el-get-sources
      '(
        (:name ace-jump-mode
               :after
               (eval-after-load 'evil
                 '(progn
                    (define-key evil-normal-state-map (kbd "SPC")
                      'ace-jump-mode))))
        (:name ag)
        ;; Anzu mode - show the number of matches when searching
        (:name anzu
               :after (global-anzu-mode 1))

        (:name auctex
               :after (progn
                        (setq TeX-auto-save t)
                        (setq TeX-parse-self t)
                        (eval-after-load 'latex
                          '(progn
                             (require 'smartparens-latex)
                             ;; Remove the :trigger for a regular
                             ;; double quote to insert LaTeX double
                             ;; quotes.  Now smartparens will default
                             ;; to normal double quotes.
                             (sp-local-pair 'latex-mode "``" "''"
                                            :trigger "\""
                                            :actions :rem)))))

        (:name auto-complete
               :submodule nil

               :after (progn (require 'auto-complete-config)
                             (global-auto-complete-mode 1)
                             (eval-after-load 'auto-complete-config
                               '(progn
                                  (defun set-auto-complete-as-completion-at-point-function ()
                                    (add-to-list 'completion-at-point-functions 'auto-complete-mode-maybe))
                                  (add-hook 'auto-complete-mode-hook
                                            'set-auto-complete-as-completion-at-point-function)
                                  (setq-default ac-comphist-file "~/.emacs.d/private/ac-comphist.dat")
                                  (setq-default ac-sources
                                                '(ac-source-yasnippet
                                                  ac-source-imenu
                                                  ac-source-dictionary
                                                  ac-source-words-in-buffer
                                                  ac-source-words-in-same-mode-buffers
                                                  ac-source-words-in-all-buffer))))))

        (:name esup
               :website "https://github.com/jschaf/esup"
               :description "Emacs Start Up Profiler"
               :type "github"
               :branch "master"
               :pkgname "jschaf/esup")

        (:name elisp-slime-nav
               :after
               (progn
                 (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)
                 (eval-after-load 'evil
                   '(progn
                      (cl-loop for (key . func) in
                               '(("g." . elisp-slime-nav-find-elisp-thing-at-point)
                                 ("g," . pop-tag-mark)
                                 ("gh" . elisp-slime-nav-describe-elisp-thing-at-point))
                               do
                               (evil-define-key 'normal emacs-lisp-mode-map key func)
                               (evil-define-key 'normal lisp-interaction-mode-map key func)
                               (evil-define-key 'motion emacs-lisp-mode-map key func)
                               (evil-define-key 'motion lisp-interaction-mode-map key func))))))

        (:name emmet-mode
               :after (progn
                        (add-hook 'sgml-mode-hook 'emmet-mode)
                        (add-hook 'css-mode-hook 'emmet-mode)))

        (:name evil
               ;; remove info from make target
	       :build (("make" "all"))
               ;; I can't use the git protocol at work, which is the
               ;; only protocol gitorious supports.
	       :url "https://github.com/emacsmirror/evil.git"
               ;; makeinfo fails on Windows
               :info nil
               :after (my:evil-setup))

        (:name evil-leader
               :after
               (progn
                 (global-evil-leader-mode)
                 (evil-leader/set-leader ",")
                 (evil-leader/set-key
                   "ci" 'ido-goto-symbol
                   "cp" 'check-parens
                   "cs" '(lambda () (interactive) (switch-to-buffer "*scratch*"))
                   "cB" 'my:new-blah-buffer
                   "cb" 'my:switch-to-blah-buffer
                   "de" 'toggle-debug-on-error
                   "dc" 'describe-char
                   "dtw" 'delete-trailing-whitespace
                   "eb" 'eval-buffer
                   "ed" 'eval-defun
                   "ee" 'edebug-eval-top-level-form
                   "ei" 'el-get-install
                   "ff" 'find-function
                   "fp" 'my:find-function-at-point-this-window
                   "fP" 'find-function-at-point
                   "gc" 'goto-char
                   "gj" 'next-error
                   "gk" 'previous-error
                   "gh" '(lambda () (interactive) (find-file "~/"))
                   "ht" 'describe-text-properties
                   "is" 'my:indent-defun-around-point
                   "js" 'just-one-space
                   "k" '(lambda () (interactive) (kill-buffer nil))
                   "ms" 'mark-sexp
                   "o" 'delete-blank-lines
                   "r"  'jump-to-register
                   "sp" 'eval-print-last-sexp
                   "ts" 'toggle-color-theme
                   "tc" 'my:toggle-identifier-naming-style
                   "xd" 'ido-dired
                   "xg" 'magit-status
                   "xh" 'mark-whole-buffer
                   "xc" 'copy-to-register
                   "xr " 'point-to-register)))

        (:name evil-numbers
               :after
               (progn
                 (define-key evil-normal-state-map (kbd "M-<up>") 'evil-numbers/inc-at-pt)
                 (define-key evil-normal-state-map (kbd "M-<down>") 'evil-numbers/dec-at-pt)))

        (:name evil-surround
               :after (global-evil-surround-mode 1))

        (:name fill-column-indicator
               :after
               (progn
                 (defun my:show-column-80 ()
                   "Enable a rule at column 80."
                   (interactive)
                   (require 'fill-column-indicator)
                   (setq fci-rule-column 79
                         fci-rule-width 1
                         ;; fci-always-use-textual-rule t
                         fci-rule-color "#E8E2D0"
                         ;; fci-rule-character ?│
                         )

                   (fci-mode 1))
                 (add-hook 'prog-mode-hook 'my:show-column-80)

                 ;; Disable `fci-mode' when we have less than 80
                 ;; chars.  This prevents those ugly line continuation
                 ;; markers down the entire buffer.
                 (defun my:auto-fci-mode (&optional unused)
                   (when (derived-mode-p 'prog-mode)
                     (if (> (window-width) fci-rule-column)
                         (fci-mode 1)
                       (fci-mode 0))))

                 (add-hook 'after-change-major-mode-hook
                           'my:auto-fci-mode)
                 (add-hook 'window-configuration-change-hook
                           'my:auto-fci-mode)))

        (:name flx
               :after (flx-ido-mode 1))

        (:name fuzzy
               ;; fuzzy-el ert from github, which is empty because
               ;; it's included in Emacs trunk
               :submodule nil)

        (:name git-gutter
               :after
               (progn (add-hook 'prog-mode-hook 'git-gutter-mode)
                      (eval-after-load 'git-gutter
                        '(progn
                           ;; Turn off annoying "here is not git
                           ;; repository" message
                           (setq git-gutter:verbosity 0)))))

        (:name helm
               :after (progn
                        (helm-mode 1)

                        (loop for ext in '("\\.swf$" "\\.elc$" "\\.pyc$")
                              do (add-to-list 'helm-boring-file-regexp-list ext))
                        (setq
                         ;; use curl for async, instead of emacs url synchronus
                         helm-google-suggest-use-curl-p t
                         helm-scroll-amount 4 ; scroll 4 lines other window using M-<next>/M-<prior>
                         helm-quick-update t ; do not display invisible candidates
                         helm-idle-delay 0.01 ; be idle for this many seconds, before updating in delayed sources.
                         helm-input-idle-delay 0.01 ; be idle for this many seconds, before updating candidate buffer
                         helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.

                         ;; you can customize helm-do-grep to execute ack-grep
                         ;; helm-grep-default-command "ack-grep -Hn --smart-case --no-group --no-color %e %p %f"
                         ;; helm-grep-default-recurse-command "ack-grep -H --smart-case --no-group --no-color %e %p %f"
                         helm-split-window-default-side 'other ;; open helm buffer in another window
                         helm-split-window-in-side-p t ;; open helm buffer inside current window, not occupy whole other window
                         helm-buffers-favorite-modes (append helm-buffers-favorite-modes
                                                             '(picture-mode artist-mode))
                         helm-candidate-number-limit 200 ; limit the number of displayed canidates
                         helm-M-x-requires-pattern 0 ; show all candidates when set to 0
                         helm-ff-file-name-history-use-recentf t
                         ;; move to end or beginning of source when
                         ;; reaching top or bottom of source.
                         helm-move-to-line-cycle-in-source t
                         ido-use-virtual-buffers t ; Needed in helm-buffers-list
                         ;; fuzzy matching buffer names when non--nil
                         ;; useful in helm-mini that lists buffers
                         helm-buffers-fuzzy-matching t)

                        ;; rebind tab to do persistent action
                        (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
                        (define-key helm-find-files-map (kbd "<tab>") 'helm-execute-persistent-action)
                        ;; make TAB works in terminal
                        (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
                        (define-key helm-find-files-map (kbd "C-i") 'helm-execute-persistent-action)
                        ;; list actions using C-z
                        (define-key helm-map (kbd "C-z") 'helm-select-action)
                        (define-key helm-find-files-map (kbd "C-z") 'helm-select-action)

                        ;; mimic Ido
                        (define-key helm-map (kbd "C-s") 'helm-next-line)
                        (define-key helm-find-files-map (kbd "C-s") 'helm-next-line)
                        (define-key helm-find-files-map (kbd "C-r") 'helm-previous-line)))

        (:name highlight-symbol
               :after (progn
                        (setq highlight-symbol-idle-delay 0.8)
                        (add-hook 'prog-mode-hook 'highlight-symbol-mode)))

        (:name haskell-mode)

        (:name ido-ubiquitous
               :description "Use ido (nearly) everywhere"
               :type github
               :pkgname "technomancy/ido-ubiquitous"
               :after (ido-ubiquitous-mode 1))

        (:name ido-vertical-mode
               :after (ido-vertical-mode 1))

        (:name jedi
               :after
               (progn
                 (eval-after-load 'jedi
                   '(progn
                      (add-hook 'python-mode-hook
                                'jedi:setup)
                      (setq jedi:complete-on-dot t)
                      (loop for (key . func) in
                            '(("g." . jedi:goto-definition)
                              ("g," . jedi:goto-definition-pop-marker)
                              ("gh" . jedi:show-doc))
                            do
                            (evil-define-key 'normal python-mode-map key func)
                            (evil-define-key 'motion python-mode-map key func))))))

        (:name jinja2-mode
               :after
               (eval-after-load 'jinja2-mode
                 '(progn
                    (defun my-jinja2-block (id action context)
                      (insert " ")
                      (save-excursion
                        (insert " ")))

                    ;; Remove curly brace binding because it prevents
                    ;; a binding for Jinja constructs.
                    (sp-local-pair 'jinja2-mode "{" "}" :actions nil)
                    (sp-local-pair 'jinja2-mode "{%" "%}"
                                   :post-handlers '(:add my-jinja2-block)
                                   :trigger "jjb")
                    (sp-local-pair 'jinja2-mode "{{" "}}"
                                   :post-handlers '(:add my-jinja2-block)
                                   :trigger "jji"))))

        (:name key-chord
               :after
               (progn
                 (key-chord-mode 1)
                 (setq key-chord-two-keys-delay 0.08)
                 (loop for (key . func) in
                       `(("fh" . windmove-left)
                         ("fj" . windmove-down)
                         ("fk" . windmove-up)
                         ("fl" . ,(not-in-minibuffer 'windmove-right))
                         ("vh" . buf-move-left)
                         ("vj" . buf-move-down)
                         ("vk" . buf-move-up)
                         ("vl" . buf-move-right)
                         ("jr" . delete-window)
                         ("jq" . delete-other-windows)
                         ("jw" . split-window-vertically)
                         ;; je is a substring of projectile.
                         ("je" . ,(not-in-minibuffer 'split-window-horizontally))
                         ("jx" . smex)
                         ("jt" . dabbrev-expand)
                         ("xb" . ido-switch-buffer)
                         ("/f" . helm-find-files)
                         ("/d" . (lambda () (interactive) (dired default-directory)))
                         ("/s" . my:save-buffer)
                         ("nb" . bookmark-jump)
                         ("nm" . bookmark-set)
                         ("nl" . bookmark-bmenu-list)
                         ("jk" . my:esc)
                         ("/x" . helm-mini)
                         ("/c" . goto-last-change))
                       do (key-chord-define-global key func))))

        (:name keydef
               :description "A simpler way to define keys in Emacs."
               :type github
               :pkgname "jschaf/keydef.el"
               :after (progn (keydef "C-M-j" bs-cycle-next)
                             (keydef "C-M-k" bs-cycle-previous)

                             ;; Help
                             (keydef (help "k") (scroll-down 1))
                             (keydef (help "L") help-go-back)
                             (keydef (help "H") help-go-back)
                             (keydef (help "<tab>") forward-button)
                             (keydef (help "<shift>-<tab>") backward-button)))

        (:name lua-mode)
        (:name magit
               :after
               (eval-after-load 'magit
                 '(progn
                    (defadvice magit-key-mode-popup-committing (after toggle-verbose-commits)
                      "Enable the verbose option for commiting."
                      (magit-key-mode-toggle-option 'committing "--verbose"))
                    (ad-activate 'magit-key-mode-popup-committing)
                    (add-hook 'magit-mode-hook
                              '(lambda ()
                                 (local-set-key "j" #'evil-next-line)
                                 (local-set-key "k" #'evil-previous-line))))))

        (:name markdown-mode)

        (:name page-break-lines
               :after (progn
                        (add-hook 'emacs-lisp-mode-hook
                                  'turn-on-page-break-lines-mode)

                        (add-hook 'compilation-mode-hook
                                  'page-break-lines-mode)))

        (:name popup
               :submodule nil)

        (:name projectile
               :after (projectile-global-mode 1))

        (:name rainbow-delimiters
               :after (add-hook 'emacs-lisp-mode-hook
                                'rainbow-delimiters-mode))

        (:name rst-mode
               :url "http://svn.code.sf.net/p/docutils/code/trunk/docutils/tools/editors/emacs/rst.el"
               :after (progn
                        (add-hook 'rst-mode-hook 'zotelo-minor-mode)
                        (add-hook 'rst-mode-hook 'reftex-mode)))

        (:name rust-mode

               :after
               (progn
                 (keydef (rust "C-c C-c") my:rust-save-compile)

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

                 ;; (add-to-list 'compilation-finish-functions 'my:run-in-eshell)
                 ))

        (:name smartparens
               :after (progn
                        (require 'smartparens)
                        (require 'smartparens-config)
                        (smartparens-global-mode 1)
                        (show-paren-mode 1)))

        (:name yasnippet
               :after
               (progn
                 (defun my:load-yasnippet ()
                   (require 'yasnippet)
                   (setq yas-verbosity 0)
                   (yas-global-mode 1)
                   ;; Trying use to tab for everything is confusing
                   ;; and fragile
                   (define-key yas-minor-mode-map  [(tab)] nil)
                   (define-key yas-minor-mode-map (kbd "TAB") nil)
                   (define-key yas-minor-mode-map (kbd "\C-o") 'yas-expand)
                   (define-key evil-insert-state-map (kbd "\C-o") nil)

                   (define-key yas-keymap [(tab)] nil)
                   (define-key yas-keymap (kbd "TAB") nil)
                   (define-key yas-keymap (kbd "\C-o") 'yas-next-field-or-maybe-expand))
                 ;; Run after emacs is done loading because yasnippet
                 ;; adds about 1 second to load time.
                 (run-with-idle-timer 1 nil 'my:load-yasnippet)))))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(defvar el-get-packages
      (append
       '(
         solarized-theme
         buffer-move
         dash
         el-get
         flycheck
;;         gist
         git-modes
         goto-chg
;;         jedi
;;         jinja2-mode
         magit
         moz-repl
         projectile
;;         pymacs
;;         rust-mode
         reftex
         s
         smex
         smartparens
         smartrep
         ;;yasnippet
         zencoding-mode
         zotelo)
       (mapcar 'el-get-source-name el-get-sources)))

;; We can get whole repositories later if we want to hack on them.
(defvar el-get-git-shallow-clone t)

;; Delete locally installed packages that aren't listed in
;; `el-get-sources'
(el-get-cleanup el-get-packages)

;; nil, the second parameter means install all the packages
;; asynchronusly. Waaaay faster.
(el-get nil el-get-packages)

;; Local Variables:
;; lexical-binding: t
;; End:

(provide 'autoloads)
;;; autoloads.el ends here
