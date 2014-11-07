;;; autoloads.el --- bootstrap packages.

;; Run :after code when the library is required.  We only do this for
;; autoloads.el because we need the packages in base.el to load right
;; away.
;; (setq el-get-is-lazy t)

(setq my:primary-packages
      '(
        (:name ag)
        ;; Anzu mode - show the number of matches when searching
        (:name anzu
               :after (global-anzu-mode 1))

        ;; ;; This is a pain to install on Windows
        ;; (:name auctex
        ;;        :after (progn
        ;;                 (setq TeX-auto-save t)
        ;;                 (setq TeX-parse-self t)
        ;;                 (after 'latex
        ;;                   ;; Remove the :trigger for a regular
        ;;                   ;; double quote to insert LaTeX double
        ;;                   ;; quotes.  Now smartparens will default
        ;;                   ;; to normal double quotes.
        ;;                   (sp-local-pair 'latex-mode "``" "''"
        ;;                                  :trigger "\""
        ;;                                  :actions :rem))))

        (:name esup
               :website "https://github.com/jschaf/esup"
               :description "Emacs Start Up Profiler"
               :type "github"
               :branch "master"
               :pkgname "jschaf/esup")

        (:name emmet-mode
               :after (progn
                        (add-hook 'sgml-mode-hook 'emmet-mode)
                        (add-hook 'css-mode-hook 'emmet-mode)))

        (:name evil-numbers
               :after
               (progn
                 (define-key evil-normal-state-map (kbd "M-<up>") 'evil-numbers/inc-at-pt)
                 (define-key evil-normal-state-map (kbd "M-<down>") 'evil-numbers/dec-at-pt)))

        (:name evil-surround
               :after (global-evil-surround-mode 1))

        (:name exec-path-from-shell
               :lazy nil
               :after (progn
                        (defun my:initialize-exec-path-from-shell ()
                          (when (memq window-system '(mac ns))
                            (exec-path-from-shell-initialize)))
                        (run-with-idle-timer
                         0.01
                         nil
                         'my:initialize-exec-path-from-shell)))

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
                           (fci-update-all-windows t))))))

                 (my:create-subtle-fci-rule)
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

        (:name flycheck
               ;; flycheck only 'builds' info.
               :build/windows-nt nil
               :build nil
               :after
               (progn
                 (add-hook 'emacs-lisp-mode-hook 'flycheck-mode)))

        (:name flycheck-cask
               :after (progn
                        (after 'flycheck
                          (add-hook 'flycheck-mode-hook #'flycheck-cask-setup))))

        (:name highlight-symbol
               :after (progn
                        (require 'highlight-symbol)
                        (defun my:create-subtle-highlight ()
                          (interactive)
                          (set-face-attribute 'highlight-symbol-face nil
                                              :foreground nil
                                              :background (my:differentiate-color (face-background 'default) 5)
                                              :underline nil))

                        ;; Not sure why this didn't work in `after-init-hook'
                        (run-with-idle-timer 0.01 nil 'my:create-subtle-highlight)
                        (add-hook 'my:load-theme-hook 'my:create-subtle-highlight)

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
                        (defun my:create-subtle-sentence-highlight ()
                          (interactive)
                          (set-face-attribute 'hl-sentence-face nil
                                              :foreground nil
                                              :background (my:differentiate-color (face-background 'default) 4)))
                        (add-hook 'markdown-mode-hook 'hl-sentence-mode)
                        (add-hook 'rst-mode-hook 'hl-sentence-mode)

                        (run-with-idle-timer 0.01 nil 'my:create-subtle-sentence-highlight)
                        (add-hook 'my:load-theme-hook 'my:create-subtle-sentence-highlight)))

        ;; Doesn't compile on Windows because sed is missing.  El-get
        ;; says haskell-mode is missing on melpa.
        ;; (:name haskell-mode)

        (:name iedit
               :after
               (progn
                 (defun my:create-subtle-iedit-face ()
                   (interactive)
                   (set-face-attribute iedit-occurrence-face nil
                                       :foreground nil
                                       ;; negative arg for :box draws inside the char
                                       ;; box so the line-height is preserved
                                       :box -1
                                       :background (my:differentiate-color (face-background 'default) 9)))
                 (run-with-idle-timer 0.01 nil 'my:create-subtle-iedit-face)
                 (add-hook 'my:load-theme-hook 'my:create-subtle-iedit-face)))

        (:name jedi
               :after
               (progn
                 (add-hook 'python-mode-hook 'jedi:setup)
                 (setq jedi:complete-on-dot t)

                 (evil-define-key 'normal python-mode-map "g." 'jedi:goto-definition)
                 (evil-define-key 'normal python-mode-map "g," 'jedi:goto-definition-pop-marker)
                 (evil-define-key 'normal python-mode-map "gh" 'jedi:show-doc)))

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

        (:name js2-mode
               :after (progn
                       (add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-mode))))
        (:name lorem-ipsum)

        (:name lua-mode)

        (:name markdown-mode
               :after (progn
                        (my:add-citations-to-sentence-end)))

        (:name multiple-cursors
               :after ())

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

        (:name python
               ;; repo.or.cz is blocked on work network
               :url "https://raw.githubusercontent.com/emacs-mirror/emacs/master/lisp/progmodes/python.el"
               :after (progn
                        (defun my:configure-python-venv ()
                          (interactive)
                          (let* ((project-name (projectile-project-name))
                                 (virtualenv-path
                                  (file-truename
                                   (concat "~/.virtualenvs/" project-name))))
                            (when (file-directory-p virtualenv-path)
                              (setq python-shell-virtualenv-path virtualenv-path))))


                        (defun flycheck-python-set-executables ()
                          (let ((exec-path (python-shell-calculate-exec-path)))
                            (setq flycheck-python-pylint-executable (executable-find "pylint")
                                  flycheck-python-flake8-executable (executable-find "flake8")))
                          ;; Force Flycheck mode on
                          (flycheck-mode))

                        (defun flycheck-python-setup ()
                          (add-hook 'hack-local-variables-hook #'flycheck-python-set-executables
                                    nil 'local))
                        (add-hook 'python-mode-hook 'my:configure-python-venv)
                        (add-hook 'python-mode-hook #'flycheck-python-setup)))

        (:name python-environment
               :after (progn
                        (setq-default python-environment-directory
                                      (my:privatize "python-environments"))))

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
                 (defvar my:rust-compiled-buffer nil)

                 (defun my:rust-save-compile (&optional arg)
                   (interactive "p")
                   (save-buffer)
                   (compile (concat "rustc " (buffer-file-name)))
                   (setq my:rust-compiled-buffer (current-buffer)))

                 ;; (define-key rust-mode-map (kbd "C-c C-c") 'my:rust-save-compile)

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

        (:name smart-mode-line
               :after
               (progn
                 (require 'smart-mode-line)
                 (sml/setup)
                 (sml/apply-theme 'respectful)))

        (:name smart-newline
               :type github
               :pkgname "jschaf/smart-newline.el")

        (:name scss-mode
               :after (progn
                        (setq scss-compile-at-save nil)))

        (:name virtualenvwrapper
               :after ())

        (:name yasnippet
               :lazy nil
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

(defvar my:primary-package-names
      (append
       '(
         buffer-move
         dash
         el-get
         flycheck
         git-modes
         goto-chg
         projectile
         reftex
         s
         smex
         smartparens
         zencoding-mode
         zotelo)
       (mapcar 'el-get-source-name my:primary-packages)))

(setq el-get-sources (cl-concatenate 'list el-get-sources my:primary-packages))
(setq my:package-names (append my:primary-package-names my:base-package-names))


;; nil, the second parameter means install all the packages
;; asynchronusly. Waaaay faster.
(el-get nil my:package-names)


;; Delete locally installed packages that aren't listed in
;; `el-get-sources'
;; (el-get-cleanup my:package-names)

(provide 'autoloads)
;;; autoloads.el ends here
