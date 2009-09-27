;; ELSE mode
(autoload 'else-mode "else-mode" "Emacs Language Sensitive Editor" t)

(require 'uniquify)
(toggle-uniquify-buffer-names)

;; Fancy symbol completion with a dropdown list
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)
(setq ac-auto-start nil)
(global-set-key "\M-i" 'ac-start)
(define-key ac-complete-mode-map "\M-n" 'ac-next)
(define-key ac-complete-mode-map "\M-p" 'ac-previous)

;; Key-chord (pressing "jf" and executing a command)
(require 'key-chord)
(key-chord-mode 1)

(key-chord-define-global "5j" 'other-frame)
(key-chord-define-global "js" 'isearch-forward)
(key-chord-define-global "jr" 'isearch-backward)
(key-chord-define-global "jf" 'flymake-mode)
(key-chord-define-global "fh" 'windmove-left)
(key-chord-define-global "fj" 'windmove-down)
(key-chord-define-global "fk" 'windmove-up)
(key-chord-define-global "fl" 'windmove-right)
(key-chord-define-global "j1" 'delete-other-windows)
(key-chord-define-global "j2" 'split-window-vertically)
(key-chord-define-global "j3" 'split-window-horizontally)
(key-chord-define-global "j0" 'delete-window)
(key-chord-define-global "jx" 'smex)
(key-chord-define-global "kx" 'smex-major-mode-commands)
(key-chord-define-global "jg" 'abort-recursive-edit)
(key-chord-define-global "xb" 'ido-switch-buffer)
(key-chord-define-global "/s" 'save-buffer)
(key-chord-define-global "/f" 'ido-find-file)
(key-chord-define-global "/w" 'ido-write-file)
(key-chord-define-global "nb" 'bookmark-jump)
(key-chord-define-global "nm" 'bookmark-set)
(key-chord-define-global "nl" 'bookmark-bmenu-list)
(key-chord-define-global "z," 'beginning-of-buffer)
(key-chord-define-global "z." 'end-of-buffer)
(key-chord-define-global "jk" 'undo)
(key-chord-define-global "/z" 'move-to-char)
(key-chord-define emacs-lisp-mode-map "qh"
                  (lambda () (interactive) (insert "hudson")))
(key-chord-define emacs-lisp-mode-map "wh"
                  (lambda () (interactive) (insert "hudson-mode-")))

;; I <3 pretty colors
(require 'color-theme)
(color-theme-initialize)
;; in .bashrc "export TERM=xterm-256color" from package ncurses-term,
;; so I don't have to worry about not enough colors.
(color-theme-wombat)

;; Another git interface
(require 'magit)                        
(global-set-key [(control c) (g)] 'magit-status)

;; w3m is an emacs interface to the w3 backend
(require 'w3m-load)
(setq browse-url-browser-function 'w3m-browse-url)
(global-set-key [(control c)(control u)] 'browse-url-at-point)

;; Binary movement with chop
(autoload 'chop-move-up "chop.el"
  "Use binary movement up (successively move the point half the
  remaining buffer up)" t)
(autoload 'chop-move-down "chop.el"
  "Use binary movement down (successively move the point half the
  remaining buffer down)" t)
(global-set-key [(super meta u)] 'chop-move-up)
(global-set-key [(super meta i)] 'chop-move-down)


;; Wikipedia Mode
(autoload 'wikipedia-mode "wikipedia-mode.el"
  "Major mode for editing documents in Wikipedia markup." t)
(add-to-list 'auto-mode-alist
             '("\\.wikipedia\\.org.*\\.txt\\'" . wikipedia-mode))
(add-to-list 'auto-mode-alist
             '("\\.wikibooks\\.org.*\\.txt\\'" . wikipedia-mode))

;; Template-mode for editing ELSE templates
(autoload 'template-mode "template-mode" "Template Mode for ELSE templates" t)
(add-to-list 'auto-mode-alist '("\\.lse$" . template-mode))

;; Markdown mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing documents in Markdown markup." t)
(add-to-list 'auto-mode-alist '("\\.reddit\\.com.*\\.txt\\'" . markdown-mode))

;; Paredit
(autoload 'paredit-mode "paredit-beta"
  "Minor mode for pseudo-structurally editing Lisp code."
  t)

;; Hudson mode for cs478
(autoload 'hudson-mode "hudson-mode" "Major Mode for the Hudson Programming language" t)
(add-to-list 'auto-mode-alist '("\\.hud$" . hudson-mode))

;; YAML
(autoload 'yaml-mode "yaml-mode" "Major Mode for 'Yet Another Markup Language'" t)
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;; CSV
(add-to-list 'auto-mode-alist '("\\.csv\\'" . csv-mode))
(autoload 'csv-mode "csv-mode"
  "Major mode for editing comma-separated value files." t)
(add-hook 'csv-mode-hook 'toggle-truncate-lines)

;; GNUPlot
(autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
(autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot mode" t)
(add-to-list 'auto-mode-alist '("\\.gp$" . gnuplot-mode))

;; Graphviz
(autoload 'graphviz-dot-mode "graphviz-dot-mode"
  "Major Mode for working with the dot language file used by
  graphviz"
  t)
(add-to-list 'auto-mode-alist '("\\.dot$" . graphviz-dot-mode))
(add-hook 'graphviz-dot-mode
          (lambda ()
            (local-set-key [(control c)(control c)] 'compile)))

(autoload 'inferior-moz-mode "moz" "Major mode for interacting with Mozilla" t)
(autoload 'inferior-moz-process "moz" "Process to interact with Mozilla" t)
(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
(add-hook 'javascript-mode-hook 'javascript-custom-setup)
(defun javascript-custom-setup ()
  (moz-minor-mode 1))
(defun auto-reload-firefox-on-after-save-hook ()         
  (add-hook 'after-save-hook
            '(lambda ()
               (interactive)
               (comint-send-string (inferior-moz-process)
                                   "setTimeout(BrowserReload(), \"1000\");"))
            'append 'local)) ; buffer-local

;; Example - you may want to add hooks for your own modes.
;; I also add this to python-mode when doing django development.
(add-hook 'html-mode-hook 'auto-reload-firefox-on-after-save-hook)
(add-hook 'css-mode-hook 'auto-reload-firefox-on-after-save-hook)

;; (add-hook 'espresso-mode-hook 'espresso-custom-setup)
;; (defun espresso-custom-setup ()
;;   (moz-minor-mode 1))

;; ido
(require 'ido)
(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point t)
(setq ido-auto-merge-work-directories-length -1)
(add-hook 'ido-setup-hook
	  (lambda ()
	    (setq ido-enable-flex-matching t)))

(autoload 'describe-unbound-keys "unbound"
  "Display a list of unbound keystrokes of complexity no greater than MAX.
Keys are sorted by their complexity; `key-complexity' determines
  it."  t)

;;; easy window navigation using vim esque movement
(global-set-key [(super meta j)] 'windmove-down)
(global-set-key [(super meta k)] 'windmove-up)
(global-set-key [(super meta l)] 'windmove-right)
(global-set-key [(super meta h)] 'windmove-left)

(autoload 'cycle-buffer "cycle-buffer" "Cycle-forward" t)
(autoload 'cycle-buffer-backward "cycle-buffer" "Cycle backward." t)
(autoload 'cycle-buffer-permissive "cycle-buffer"
  "Cycle forward allowing *buffers*." t)
(autoload 'cycle-buffer-backward-permissive "cycle-buffer"
  "Cycle backward allowing *buffers*." t)
(autoload 'cycle-buffer-toggle-interesting "cycle-buffer"
  "Toggle if this buffer will be considered." t)
(global-set-key (kbd "M-s-n") 'cycle-buffer)
(global-set-key (kbd "M-s-p") 'cycle-buffer-backward)
(global-set-key (kbd "M-s-m") 'cycle-buffer-permissive)
(global-set-key (kbd "M-s-o") 'cycle-buffer-backward-permissive)
(global-set-key (kbd "M-s-/") 'kill-current-buffer-no-prompt)

(defun kill-current-buffer-no-prompt ()
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key "\C-xp" 'find-file-at-point)
(global-set-key "\C-x\C-b" 'ido-switch-buffer)
(global-set-key "\C-cj" 'delete-indentation)
(global-set-key "\M-g\M-f" 'next-error)
(global-set-key "\M-g\M-d" 'prev-error)
(global-set-key "\C-h\M-f" 'describe-face)

;; Recognize gnat project files using the gpr-mode.el from ada-mode
(autoload 'gpr-mode "gpr-mode" "Major mode for GNAT Project files" t)
(add-to-list 'auto-mode-alist '("\\.gpr$" . gpr-mode))

(autoload 'goto-last-change "goto-last-change"
  "Set point to the position of the last change." t)
(key-chord-define-global "/c" 'goto-last-change)

;; M-x enhancement for emacs
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'smex-update-and-run)
;; This is the old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(global-set-key "\C-h\M-c" 'describe-char)