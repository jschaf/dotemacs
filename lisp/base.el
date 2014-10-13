
;;; Commentary:

;; Use el-get to bootstrap packages.  el-get has the advantage that it
;; will automatically byte-compile, add auto-loads and install info
;; for our packages.

;;; Code:

;; Require el-get and bootstrap if it doesn't exist.

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(require 'cl)

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let ((el-get-master-branch t))
      (when el-get-master-branch
        (message "Bootstrapping el-get with master branch."))
      (goto-char (point-max))
      (eval-print-last-sexp))))

(defmacro with-timer-callback (callback &rest forms)
  "Evaluate FORMS and call CALLBACK with the elapsed time."
  (declare (indent 1))
  (let ((nowvar (make-symbol "now")))
    `(let ((,nowvar (current-time)))
       (prog1 (progn ,@forms)
         (let ((elapsed (float-time (time-subtract (current-time) ,nowvar))))
           (funcall ,callback elapsed))))))

(defadvice el-get-run-package-support (around time-el-get activate)
  "Time how long el-get takes to run package support."
  (with-timer-callback
      (lambda (time-passed)
        (when (and (equal "after" (ad-get-arg 1))
                   (> time-passed 0.05))
          (message "el-get support for %s took %s seconds"
                   (el-get-package-name (ad-get-arg 2))
                   time-passed)))
    ad-do-it))

(setq my:base-packages
      '((:name ace-jump-mode
               :after
               (after 'evil
                 (define-key evil-normal-state-map (kbd "SPC")
                   'ace-jump-mode)))

        (:name auto-complete
               :submodule nil

               :after (progn (require 'auto-complete-config)
                             (global-auto-complete-mode 1)
                             (after 'auto-complete-config
                               (defun set-auto-complete-as-completion-at-point-function ()
                                 (add-to-list 'completion-at-point-functions 'auto-complete-mode-maybe))
                               (add-hook 'auto-complete-mode-hook
                                         'set-auto-complete-as-completion-at-point-function)
                               (setq-default ac-comphist-file (my:privatize "ac-comphist.dat"))
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
                                (quit-chord-mode . "quit-chord")
                                (git-gutter-mode . "git-gutter"))
                              do
                              (eval-after-load file-name
                                `(diminish ',mode-to-diminish)))
                        (when (eval-when-compile (string< "24.3.1" emacs-version))
                          ;; https://github.com/purcell/emacs.d/issues/138
                          (after 'subword
                            (diminish 'subword-mode)))))

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

        (:name evil
               :after (my:evil-setup))

        (:name evil-leader
               :after
               (progn
                 (global-evil-leader-mode)
                 (evil-leader/set-leader ",")
                 (evil-leader/set-key
                   "bup" 'browse-url-at-point
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
                   "gc" 'my:magit-verbose-commit
                   "gj" 'next-error
                   "gk" 'previous-error
                   "gh" '(lambda () (interactive) (find-file "~/"))
                   "gn" 'git-gutter:next-hunk
                   "gp" 'git-gutter:previous-hunk
                   "gr" 'git-gutter:revert-hunk
                   "gs" 'my:git-gutter:stage-hunk
                   "gv" 'git-gutter:revert-hunk
                   "hs" 'highlight-symbol-at-point
                   "hll" 'helm-locate-library
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
                 (evil-leader/set-key
                   "lis" 'Lorem-ipsum-insert-sentences
                   "lip" 'Lorem-ipsum-insert-paragraphs
                   "lil" 'Lorem-ipsum-insert-list)))

        (:name flx
               :after (flx-ido-mode 1))

        (:name fuzzy
               ;; fuzzy-el ert from github, which is empty because
               ;; it's included in Emacs trunk
               :submodule nil)

        (:name git-gutter
               :after (progn (add-hook 'prog-mode-hook 'git-gutter-mode)
                             ;; (add-hook 'git-gutter:update-hooks 'magit-revert-buffer-hook)
                             ;; Turn off annoying "here is not git
                             ;; repository" message
                             (setq git-gutter:verbosity 0)))

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

        (:name hungry-delete
               :after (global-hungry-delete-mode 1))

        (:name ido-ubiquitous
               :description "Use ido (nearly) everywhere"
               :type github
               :pkgname "technomancy/ido-ubiquitous"
               :after (ido-ubiquitous-mode 1))

        (:name ido-vertical-mode
               :after (ido-vertical-mode 1))

        (:name key-chord
               ;; Use my package until the maintainer says we can make
               ;; this canonical
               :type github
               :pkgname "jschaf/key-chord"
               :after
               (progn
                 (key-chord-mode 1)
                 (setq key-chord-two-keys-delay 0.08)

                 ;; Commands that should work in the minibuffer
                 (loop for (key . func) in
                       `(("jk" . my:esc))
                       do (key-chord-define-global key func))

                 ;; Commands that should NOT work in the minibuffer
                 (loop for (key . func) in
                       `(("fh" . windmove-left)
                         ("fj" . windmove-down)
                         ("fk" . windmove-up)
                         ("fl" . windmove-right)
                         ("vh" . buf-move-left)
                         ("vj" . buf-move-down)
                         ("vk" . buf-move-up)
                         ("vl" . buf-move-right)
                         ("jr" . delete-window)
                         ("jq" . delete-other-windows)
                         ("jw" . split-window-vertically)
                         ("je" . split-window-horizontally)
                         ("jx" . smex)
                         ("jt" . dabbrev-expand)
                         ("xb" . ido-switch-buffer)
                         ("/f" . my:helm-find-files)
                         ("/d" . my:open-dired-here)
                         ("/s" . my:save-buffer)
                         ("nb" . bookmark-jump)
                         ("nm" . bookmark-set)
                         ("nl" . bookmark-bmenu-list)
                         ("/x" . helm-mini)
                         ("/c" . goto-last-change))
                       do (key-chord-define-global key (not-in-minibuffer func key)))))

        (:name magit
               :website "https://github.com/magit/magit#readme"
               :description "It's Magit! An Emacs mode for Git."
               :type elpa
               :pkgname "magit"
               :depends (cl-lib git-modes)
               :after (progn
                        (defadvice magit-key-mode-popup-committing (after toggle-verbose-commits activate)
                          "Enable the verbose option for commiting."
                          (magit-key-mode-toggle-option 'committing "--verbose"))

                        (add-hook 'magit-mode-hook
                                  '(lambda ()
                                     (local-set-key "j" #'evil-next-line)
                                     (local-set-key "k" #'evil-previous-line)))))

        (:name popup
               :submodule nil)

        (:name projectile
               :after (progn
                        (defun my:start-projectile ()
                          (projectile-global-mode 1))
                        (run-with-idle-timer 0.01 nil 'my:start-projectile)))

        (:name quit-chord
               :description "Quit chord mode"
               :type github
               :pkgname "jschaf/quit-chord"
               :depends (smartrep key-chord)
               :after (quit-chord-global-mode 1))

        (:name rainbow-delimiters
               :after (add-hook 'emacs-lisp-mode-hook
                                'rainbow-delimiters-mode))

        (:name smartrep)

        (:name smartparens
               :after (progn
                        (require 'smartparens)
                        (require 'smartparens-config)
                        (smartparens-global-mode 1)
                        ))

        (:name solarized-emacs
               :after (progn
                        (ignore-errors
                          (load-theme 'solarized-light))
                        (defun my:create-subtle-region ()
                          (interactive)
                          (set-face-attribute
                           'region nil
                           :foreground nil
                           :background (my:differentiate-color
                                        (face-background 'default) 12)))
                        (add-hook 'my:load-theme-hook 'my:create-subtle-region)))))

;; We can get whole repositories later if we want to hack on them.
(defvar el-get-git-shallow-clone t)

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

;; TODO: make this not redundant
(setq el-get-sources my:base-packages)
(defvar my:base-package-names (mapcar 'el-get-source-name my:base-packages))

;; nil, the second parameter means install all the packages
;; asynchronusly. Waaaay faster.
(el-get nil my:base-package-names)
