;;; autoloads.el --- bootstrap packages.


;;; Commentary:

;; Use el-get to bootstrap packages.  el-get has the advantage that it
;; will automatically byte-compile, add auto-loads and install info
;; for our packages.

;;; Code:

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
      '((:name ace-jump-mode
               :after
               (after 'evil
                 (define-key evil-normal-state-map (kbd "SPC")
                   'ace-jump-mode)))
        (:name ag)
        ;; Anzu mode - show the number of matches when searching
        (:name anzu
               :after (global-anzu-mode 1))

        (:name auctex
               :after (progn
                        (setq TeX-auto-save t)
                        (setq TeX-parse-self t)
                        (after 'latex
                          ;; Remove the :trigger for a regular
                          ;; double quote to insert LaTeX double
                          ;; quotes.  Now smartparens will default
                          ;; to normal double quotes.
                          (sp-local-pair 'latex-mode "``" "''"
                                         :trigger "\""
                                         :actions :rem))))

        (:name auto-complete
               :submodule nil

               :after (progn (require 'auto-complete-config)
                             (global-auto-complete-mode 1)
                             (after 'auto-complete-config
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
                                               ac-source-words-in-all-buffer)))))

        (:name diminish
               :after (progn

                        (loop for (mode-to-diminish . file-name) in
                              '((eldoc-mode . "eldoc")
                                (magit-auto-revert-mode . "magit")
                                (hs-minor-mode . "hideshow")
                                (yas-minor-mode . "yasnippet")
                                (auto-complete-mode . "auto-complete")
                                (anzu-mode . "anzu")
                                (highlight-symbol-mode . "highlight-symbol")
                                (page-break-lines-mode . "page-break-lines")
                                (helm-mode . "helm-mode")
                                (elisp-slime-nav-mode . "elisp-slime-nav")
                                (auto-fill-function . "simple")
                                (smartparens-mode . "smartparens")
                                (undo-tree-mode . "undo-tree")
                                (git-gutter-mode . "git-gutter"))
                              do
                              (eval-after-load file-name
                                `(diminish ',mode-to-diminish)))))

        (:name ein)
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
                 ;; TODO: this breaks with lexical-binding enabled.
                 (after 'evil
                   (cl-loop for (key . func) in
                            '(("g." . elisp-slime-nav-find-elisp-thing-at-point)
                              ("g," . pop-tag-mark)
                              ("gh" . elisp-slime-nav-describe-elisp-thing-at-point))
                            do
                            (evil-define-key 'normal emacs-lisp-mode-map key func)
                            (evil-define-key 'normal lisp-interaction-mode-map key func)
                            (evil-define-key 'motion emacs-lisp-mode-map key func)
                            (evil-define-key 'motion lisp-interaction-mode-map key func)))))

        (:name emmet-mode
               :after (progn
                        (add-hook 'sgml-mode-hook 'emmet-mode)
                        (add-hook 'css-mode-hook 'emmet-mode)))

        (:name evil
               :after (my:evil-setup))

        (:name evil-leader
               :after
               (progn
                 (global-evil-leader-mode)
                 (evil-leader/set-leader ",")
                 (evil-leader/set-key
                   "ci" 'ido-goto-symbol
                   "cs" '(lambda () (interactive) (switch-to-buffer "*scratch*"))
                   "cB" 'my:new-blah-buffer
                   "cb" 'my:switch-to-blah-buffer
                   "cp" 'check-parens
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
                   "hs" 'highlight-symbol-at-point
                   "hn" 'highlight-symbol-next
                   "hp" 'highlight-symbol-prev
                   "ht" 'describe-text-properties
                   "idd" 'my:insert-date
                   "idc" 'my:insert-date-civilian
                   "ida" 'my:insert-date-american
                   "idi" 'my:insert-date-iso
                   "idms" 'my:insert-date-military-short
                   "idml" 'my:insert-date-military-long
                   "idtc" 'my:insert-date-time-civilian
                   "idta" 'my:insert-date-time-american
                   "idti" 'my:insert-date-time-iso
                   "idtm" 'my:insert-date-time-military
                   "ip" 'ispell
                   "is" 'my:indent-defun-around-point
                   "js" 'just-one-space
                   "k" '(lambda () (interactive) (kill-buffer nil))
                   "ms" 'mark-sexp
                   "md" 'my:rst-demote-section
                   "mp" 'my:rst-promote-section
                   "o" 'delete-blank-lines
                   "pc" 'my:pandoc-reftex-cite
                   "r"  'jump-to-register
                   "sp" 'eval-print-last-sexp
                   "ts" 'toggle-color-theme
                   "tc" 'my:toggle-identifier-naming-style
                   "xd" 'ido-dired
                   "xg" 'magit-status
                   "xh" 'mark-whole-buffer
                   "xc" 'copy-to-register
                   "xr " 'point-to-register)
                 (after 'lorem-ipsum
                   (evil-leader/set-key
                     "lis" 'Lorem-ipsum-insert-sentences
                     "lip" 'Lorem-ipsum-insert-paragraphs
                     "lil" 'Lorem-ipsum-insert-list))))

        (:name evil-numbers
               :after
               (progn
                 (define-key evil-normal-state-map (kbd "M-<up>") 'evil-numbers/inc-at-pt)
                 (define-key evil-normal-state-map (kbd "M-<down>") 'evil-numbers/dec-at-pt)))

        (:name evil-surround
               :after (global-evil-surround-mode 1))

        (:name fontawesome
               :type github
               :pkgname "syohex/emacs-fontawesome")

        (:name fill-column-indicator
               :after
               (progn
                 ;; Not entirely sure why I need to require it.
                 ;; Adding autoloads for the fci-* functions I used
                 ;; below didn't work
                 (require 'fill-column-indicator)
                 (defun my:create-subtle-fci-rule ()
                   (interactive)
                   (setq fci-rule-color (my:differentiate-color (face-background 'default) 9))
                   ;; This is `fci-redraw-frame'.  Included here
                   ;; because we need to call
                   ;; `fci-make-overlay-strings' for `fci-rule-color'
                   ;; to take effect.  But we can only call
                   ;; `fci-make-overlay-strings' in buffers that have
                   ;; `fci-mode'
                   (let* ((wins (window-list (selected-frame) 'no-minibuf))
                          (bufs (delete-dups (mapcar #'window-buffer wins))))
                     (dolist (buf bufs)
                       (with-current-buffer buf
                         (when fci-mode
                           (fci-make-overlay-strings)
                           (fci-delete-unneeded)
                           (fci-update-all-windows))))))

                 (add-hook 'my:load-theme-hook 'my:create-subtle-fci-rule)

                 (defun my:show-column-80 ()
                   "Enable a rule at column 80."
                   (interactive)
                   (require 'fill-column-indicator)
                   (setq fci-rule-column 79
                         fci-rule-width 1
                         ;; fci-always-use-textual-rule t
                         ;; fci-rule-character ?│
                         )
                   (fci-mode 1))

                 (add-hook 'prog-mode-hook 'my:show-column-80)

                 ;; TODO: add to upstream
                 (defadvice fci-redraw-region (after fci-dont-redraw-if-narrow activate)
                   "Don't draw fci-lines if the window isn't wide enough.
Otherwise, we get the line continuation characters down the whole
screen."
                   (when (<= (window-width) (1+ fci-rule-column))
                     (fci-delete-overlays-region start end)))))

        (:name flx
               :after (flx-ido-mode 1))

        (:name fuzzy
               ;; fuzzy-el ert from github, which is empty because
               ;; it's included in Emacs trunk
               :submodule nil)

        (:name git-gutter
               :after
               (progn (add-hook 'prog-mode-hook 'git-gutter-mode)
                      (after 'git-gutter
                        (add-hook 'git-gutter:update-hooks 'magit-revert-buffer-hook)
                        ;; Turn off annoying "here is not git
                        ;; repository" message
                        (setq git-gutter:verbosity 0))))

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
                        (after 'highlight-symbol
                          (defun my:create-subtle-highlight ()
                            (interactive)
                            (set-face-attribute 'highlight-symbol-face nil
                                                :foreground nil
                                                :background (my:differentiate-color (face-background 'default) 7)
                                                :underline t))

                          (add-hook 'my:load-theme-hook 'my:create-subtle-highlight))

                        (setq highlight-symbol-idle-delay 0.4
                              highlight-symbol-colors
                              '("khaki1" "PaleVioletRed" "springgreen1"
                                "MediumPurple1" "SpringGreen1" "orange"
                                "plum2" "skyblue1" "seagreen1"))
                        (add-hook 'prog-mode-hook 'highlight-symbol-mode)))

        (:name hl-sentence
               :website "https://github.com/milkypostman/hl-sentence"
               :description "Highlight sentences in Emacs with a custom face. Very nice."
               :type "github"
               :branch "master"
               :pkgname "milkypostman/hl-sentence"
               :after (progn
                        (add-hook 'markdown-mode-hook 'hl-sentence-mode)
                        (set-face-attribute 'hl-sentence-face nil
                                            :foreground "#444")))

        (:name haskell-mode)

        (:name hungry-delete
               :after (global-hungry-delete-mode 1))

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
                 (defun my:jedi-setup-venv ()
                   "Activates the virtualenv of the current buffer."
                   (let ((project-name (projectile-project-name)))
                     (when project-name (venv-workon project-name))))

                 (add-hook 'python-mode-hook 'my:jedi-setup-venv)
                 (add-hook 'python-mode-hook 'jedi:setup)
                 (setq jedi:complete-on-dot t)

                 (cl-loop for (key . func) in
                          '(("g." . jedi:goto-definition)
                            ("g," . jedi:goto-definition-pop-marker)
                            ("gh" . jedi:show-doc))
                          do
                          (evil-define-key 'normal python-mode-map key func)
                          (evil-define-key 'motion python-mode-map key func))))

        (:name jinja2-mode
               :after
               (after 'jinja2-mode
                 (defun my-jinja2-block (id action context)
                   (insert " ")
                   (save-excursion
                     (insert " ")))

                 (add-to-list 'sp-navigate-consider-stringlike-sexp
                              'jinja2-mode)

                 ;; Remove curly brace binding because it prevents
                 ;; a binding for Jinja constructs.
                 (sp-local-pair 'jinja2-mode "{" "}" :actions nil)
                 (sp-local-pair 'jinja2-mode "{%" "%}"
                                :post-handlers '(:add my-jinja2-block)
                                :trigger "jjb")
                 (sp-local-pair 'jinja2-mode "{{" "}}"
                                :post-handlers '(:add my-jinja2-block)
                                :trigger "jji")))

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

        (:name lorem-ipsum)
        (:name lua-mode)
        (:name magit
               :after
               (after 'magit
                 (defadvice magit-key-mode-popup-committing (after toggle-verbose-commits)
                   "Enable the verbose option for commiting."
                   (magit-key-mode-toggle-option 'committing "--verbose"))
                 (ad-activate 'magit-key-mode-popup-committing)
                 (add-hook 'magit-mode-hook
                           '(lambda ()
                              (local-set-key "j" #'evil-next-line)
                              (local-set-key "k" #'evil-previous-line)))))

        (:name markdown-mode
               :after (progn
                        (my:add-citations-to-sentence-end)))

        (:name page-break-lines
               :after (progn
                        (add-hook 'emacs-lisp-mode-hook
                                  'turn-on-page-break-lines-mode)

                        (add-hook 'compilation-mode-hook
                                  'page-break-lines-mode)))

        (:name perspective
               :after (progn
                        (persp-mode)
                        (require 'persp-projectile)))

        (:name popup
               :submodule nil)

        (:name projectile
               :after (projectile-global-mode 1))

        (:name python-environment
               :after (progn
                        (setq-default python-environment-directory
                                      (concat my:emacs-private-dir
                                              ".python-environments"))))
        (:name rainbow-delimiters
               :after (add-hook 'emacs-lisp-mode-hook
                                'rainbow-delimiters-mode))

        (:name rst-mode
               :url "http://svn.code.sf.net/p/docutils/code/trunk/docutils/tools/editors/emacs/rst.el"
               :after (progn
                        (add-hook 'rst-mode-hook 'zotelo-minor-mode)
                        (add-hook 'rst-mode-hook 'reftex-mode)
                        (defun my:rst-promote-section (&optional arg)
                          (interactive "P")
                          (rst-adjust-adornment-work nil nil))
                        (defun my:rst-demote-section (&optional arg)
                          (interactive "P")
                          (rst-adjust-adornment-work nil 'reverse))))

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

        (:name scss-mode
               :after (progn
                        (setq scss-compile-at-save nil)))

        (:name smartparens
               :after (progn
                        (require 'smartparens)
                        (require 'smartparens-config)
                        (smartparens-global-mode 1)
                        ))

        (:name virtualenvwrapper
               :after ())

        (:name yasnippet
               :submodule nil
               :build nil
               :after
               (progn
                 (defun my:load-yasnippet ()
                   (require 'yasnippet)
                   (setq yas-verbosity 0)
                   (yas-global-mode 1)
                   ;; Trying use to tab for everything is confusing
                   ;; and fragile.  So, let `auto-complete-mode' have
                   ;; tab, and use \C-o for yasnippet.
                   (define-key yas-minor-mode-map  [(tab)] nil)
                   (define-key yas-minor-mode-map (kbd "TAB") nil)
                   (define-key yas-minor-mode-map (kbd "\C-o") 'yas-expand)
                   (define-key evil-insert-state-map (kbd "\C-o") nil)

                   (define-key yas-keymap [(tab)] nil)
                   (define-key yas-keymap (kbd "TAB") nil)
                   (define-key yas-keymap (kbd "\C-o") 'yas-next-field-or-maybe-expand)

                   (defun my:yas-skip-and-clear-or-backspace-char (&optional field)
                     "Clears unmodified field if at field start, skips to next tab.

Otherwise deletes a character normally by calling
`my:hungry-delete-backward'."
                     (interactive)
                     (let ((field (or field
                                      (and yas--active-field-overlay
                                           (overlay-buffer yas--active-field-overlay)
                                           (overlay-get yas--active-field-overlay 'yas--field)))))
                       (cond ((and field
                                   (not (yas--field-modified-p field))
                                   (eq (point) (marker-position (yas--field-start field))))
                              (yas--skip-and-clear field)
                              (yas-next-field 1))
                             (t
                              (call-interactively 'my:hungry-delete-backward)))))

                   (define-key yas-keymap [(backspace)] 'my:yas-skip-and-clear-or-backspace-char)

                   ;; Clear message buffer
                   (message nil))
                 ;; Run after emacs is done loading because yasnippet
                 ;; adds about 1 second to load time.
                 (run-with-idle-timer 0.01 nil 'my:load-yasnippet)))

        (:name yasnippet-snippets
               :website "https://github.com/jschaf/yasnippet-snippets"
               :description "A collection of yasnippet snippets for many languages."
               :type "github"
               :branch "master"
               :pkgname "jschaf/yasnippet-snippets"
               :depends (yasnippet)
               :post-init (after 'yasnippet
                            (add-to-list 'yas-snippet-dirs "~/.emacs.d/el-get/yasnippet-snippets")))))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(defvar el-get-packages
      (append
       '(
         solarized-emacs
         buffer-move
         dash
         el-get
         flycheck
;;         gist
         git-modes
         goto-chg
         help-fns+
         magit
         moz-repl
         projectile
;;         pymacs
         reftex
         s
         smex
         smartparens
         smartrep
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

(provide 'autoloads)
;;; autoloads.el ends here
