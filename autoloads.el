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

        ;; Anzu mode - show the number of matches when searching
        (:name anzu
               :after (global-anzu-mode 1))
        (:name auctex
               :after (progn
                        (setq TeX-auto-save t)
                        (setq TeX-parse-self t)
                        (setq-default TeX-master nil)))

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

        (:name evil-numbers
               :after
               (progn
                 (define-key evil-normal-state-map (kbd "M-<up>") 'evil-numbers/inc-at-pt)
                 (define-key evil-normal-state-map (kbd "M-<down>") 'evil-numbers/dec-at-pt)))

        (:name evil-surround
               :after (global-surround-mode 1))

        (:name fill-column-indicator
               :after
               (progn
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
                 (add-hook 'prog-mode-hook 'my:show-column-80)))

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
                         ("/f" . ido-find-file)
                         ("/s" . my:save-buffer)
                         ("nb" . bookmark-jump)
                         ("nm" . bookmark-set)
                         ("nl" . bookmark-bmenu-list)
                         ("jk" . evil-normal-state)
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


        (:name page-break-lines
               :after (progn
                        (add-hook 'emacs-lisp-mode-hook
                                  'turn-on-page-break-lines-mode)

                        (add-hook 'compilation-mode-hook
                                  'page-break-lines-mode)))

        (:name paredit
               :after (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode))

        (:name popup
               :submodule nil)

        (:name projectile
               :after (projectile-global-mode 1))

        (:name rainbow-delimiters
               :after (add-hook 'emacs-lisp-mode-hook
                                'rainbow-delimiters-mode))

        (:name rst-mode
               :url "http://svn.code.sf.net/p/docutils/code/trunk/docutils/tools/editors/emacs/rst.el")

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

                 (add-to-list 'compilation-finish-functions 'my:run-in-eshell)))

        (:name smartparens
               :after (progn
                        (require 'smartparens)
                        (require 'smartparens-config)
                        (smartparens-global-mode 1)
                        (show-paren-mode 1)))

        ;; (:name yasnippet
        ;;        :after
        ;;        (run-with-idle-timer 1 nil
        ;;                             (lambda ()
        ;;                               (require 'yasnippet)
        ;;                               (setq yas-verbosity 0)
        ;;                               (yas-global-mode 1))))
        ))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(defvar el-get-packages
      (append
       '(
         solarized-theme
         ace-jump-mode
         buffer-move
         dash
         el-get
         flycheck
;;         gist
         ;;git-gutter
         git-modes
;;         jedi
;;         jinja2-mode
         magit
         ;;markdown-mode
         projectile
;;         pymacs
;;         rust-mode
         s
         smex
         smartparens
         ;;yasnippet
         zencoding-mode)
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
