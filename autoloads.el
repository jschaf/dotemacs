;;; autoloads.el --- bootstrap packages to improve the Emacs experience.

;;; Commentary:

;; Use el-get to bootstrap packages.  el-get has the advantage that it
;; will automatically byte-compile, add auto-loads and install info
;; for our packages.

;;; Code:

;; Use my fork of el-get.
(defvar el-get-git-install-url "http://github.com/jschaf/el-get.git")

;; Installing from Emacswiki is slow and insecure
(defvar el-get-install-skip-emacswiki-recipes t)

;; Require el-get and bootstrap if it doesn't exist.
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/jschaf/el-get/master/el-get-install.el")
    (let ((el-get-master-branch t))
      (when el-get-master-branch
        (message "Bootstrapping el-get with master branch."))
      (goto-char (point-max))
      (eval-print-last-sexp))))

(defvar el-get-sources
      '((:name esup
                  :website "https://github.com/jschaf/esup"
                  :description "Emacs Start Up Profiler"
                  :type "github"
                  :branch "master"
                  :pkgname "jschaf/esup")
        (:name evil
               :after (require 'evil)
               :build (("make" "all"))
               ;; makeinfo fails
               :info nil)
        (:name key-chord
               :after (require 'key-chord))
        (:name powershell
               :website "https://github.com/jschaf/powershell.el"
               :type "github"
               :branch "master"
               :pkgname "jschaf/powershell.el")))

(defvar el-get-packages
      (append
       '(ace-jump-mode
         auto-complete
         buffer-move
         dash
         el-get
         elisp-slime-nav
         evil
         fill-column-indicator
         flx
         flycheck
         git-gutter
         git-modes
         haskell-mode
         ido-ubiquitous
         jedi
         key-chord
         magit
         markdown-mode
         page-break-lines
         paredit
         projectile
         pymacs
         rainbow-delimiters
         s
         smex
         smartparens
         solarized-theme
         virtualenv
         zencoding-mode)
       (mapcar 'el-get-source-name el-get-sources)))

;; We can get whole repositories later if we want to hack on them.
(defvar el-get-git-shallow-clone t)

;; Install all the packages asynchronusly. Waaaay faster.
(el-get nil el-get-packages)

(defun my:eval-after-init (form)
  "Add `(lambda () FORM)' to `after-init-hook'.
If Emacs has already finished initialization, also eval FORM
immediately."
  (let ((func (list 'lambda nil form)))
    (add-hook 'after-init-hook func)
    (when after-init-time
      (eval form))))

;; Key Chord
(eval-after-load 'key-chord
  '(progn
     (key-chord-mode 1)
     (setq key-chord-two-keys-delay 0.08)
     (loop for (key . func) in
           '(("fh" . windmove-left)
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
             ("/f" . ido-find-file)
             ("/s" . my:save-buffer)
             ("nb" . bookmark-jump)
             ("nm" . bookmark-set)
             ("nl" . bookmark-bmenu-list)
             ("jk" . evil-normal-state)
             ("/c" . goto-last-change))
           do (key-chord-define-global key func))))

;; Evil
(defun my:evil-setup ()
  "The initial customization for evil mode.

This is separated into a function so I can edit it without
figuring out how to reload the package."
  (interactive)
  (evil-mode 1)

  (setq evil-highlight-closing-paren-at-point-states nil)
  ;; Use different colors for fonts to easily determine what mode we're in.
  (setq evil-default-cursor "#0971B2")
  ;; (setq evil-default-cursor "#5EA0AD")
  (setq evil-normal-state-cursor evil-default-cursor)
  (setq evil-insert-state-cursor "#AD5E5E")
  (setq evil-visual-state-cursor evil-default-cursor)
  (setq evil-replace-state-cursor evil-default-cursor)
  (setq evil-operator-state-cursor nil)
  (setq evil-motion-state-cursor evil-default-cursor)
  (setq evil-emacs-state-cursor "#00FF48")

  (setq evil-want-visual-char-semi-exclusive t)
  (setq evil-move-cursor-back nil)

  ;; Leader key definitions
  (define-key evil-normal-state-map "," nil)
  (define-key evil-motion-state-map "," nil)
  (let ((leader-map (make-sparse-keymap)))
    (define-key evil-normal-state-map "," leader-map)
    (define-key evil-motion-state-map "," leader-map)
    (loop for (key . func) in
          '(("ci" . ido-goto-symbol)
            ("cp" . check-parens)
            ("cs" . (lambda () (interactive) (switch-to-buffer "*scratch*")))
            ("de" . toggle-debug-on-error)
            ("dc" . describe-char)
            ("dtw" . delete-trailing-whitespace)
            ("eb" . eval-buffer)
            ("ed" . eval-defun)
            ("ee" . edebug-eval-top-level-form)
            ("ei" . el-get-install)
            ("ff" . find-function)
            ("fp" . my:find-function-at-point-this-window)
            ("fP" . find-function-at-point)
            ("gc" . goto-char)
            ("gj" . next-error)
            ("gk" . previous-error)
            ("gh" . (lambda () (interactive) (find-file "~/")))
            ("ht" . describe-text-properties)
            ("is" . my:indent-defun-around-point)
            ("js" . just-one-space)
            ("k"  . (lambda () (interactive) (kill-buffer nil)))
            ("ms" . mark-sexp)
            ("o"  . delete-blank-lines)
            ("pes" . profile-emacs-startup)
            ("r"  . jump-to-register)
            ("sp" . eval-print-last-sexp)
            ("ts" . toggle-color-theme)
            ("xd" . ido-dired)
            ("xg" . magit-status)
            ("xh" . mark-whole-buffer)
            ("xrs" . copy-to-register)
            ("xr " . point-to-register))
          do (define-key leader-map key func)))

  ;; Commands for both the normal, motion and visual state
  (loop for (key . func) in
        `(("zk" . beginning-of-defun)
          ("zj" . end-of-defun)
          ("zl" . forward-sexp)
          ("zh" . backward-sexp)
          ("zu" . paredit-backward-up)
          ("J" . (lambda () (interactive) (evil-next-line 5)))
          ("K" . (lambda () (interactive) (evil-previous-line 5)))
          ("H" . my:back-to-indentation-or-beginning)
          ("L" . evil-end-of-line)
          ("zdy" . my:yank-sexp)
          (,(kbd "C-<return>") . newline)
          ("\C-j" . scroll-up-command)
          ("\C-k" . scroll-down-command))
        do
        (define-key evil-normal-state-map key func)
        (define-key evil-visual-state-map key func)
        (define-key evil-motion-state-map key func))

  ;; Commands for only the normal state map
  (loop for (key . func) in
        `((,(kbd "<tab>")  . indent-for-tab-command)
          ("z," . comment-dwim)
          ("zn" . evil-toggle-fold)
          ("z;" . comment-or-uncomment-line))
        do
        (define-key evil-normal-state-map key func))

  ;; Paredit Mode
  (add-hook 'paredit-mode-hook
            (lambda () (loop for (key . func) in
                             '(("zsh" . paredit-backward)
                               ("zsl" . paredit-forward)
                               ("zsj" . paredit-forward-down)
                               ("zsk" . paredit-backward-up)
                               ("zdl" . paredit-forward-barf-sexp)
                               ("zdh" . paredit-backward-barf-sexp)
                               ("zfh" . paredit-backward-slurp-sexp)
                               ("zfl" . paredit-forward-slurp-sexp)
                               ("zfc" . paredit-convolute-sexp)
                               ("z9" . paredit-wrap-round)
                               ("zdk" . kill-sexp)
                               ("zss" . paredit-splice-sexp)
                               ("zsd" . paredit-join-sexps)
                               ("z'" . paredit-meta-doublequote))
                             do (define-key evil-normal-state-map key func))))
  (add-hook 'Info-mode-hook
            (lambda () (loop for (key . func) in
                             '(("H" . Info-history-back)
                               ("L" . Info-history-forward))
                             do (define-key Info-mode-map key func)))))

(eval-after-load 'evil '(progn (my:evil-setup)))

;; AceJump is a nice addition to evil's standard motions.

;; The following definitions are necessary to define evil motions
;; for ace-jump-mode (version 2).

;; ace-jump is actually a series of commands which makes handling
;; by evil difficult (and with some other things as well), using
;; this macro we let it appear as one.
(eval-after-load 'ace-jump-mode
  '(progn
     (defmacro evil-enclose-ace-jump (&rest body)
       `(let ((old-mark (mark))
              (ace-jump-mode-scope 'window))
          (remove-hook 'pre-command-hook #'evil-visual-pre-command t)
          (remove-hook 'post-command-hook #'evil-visual-post-command t)
          (unwind-protect
              (progn
                ,@body
                (recursive-edit))
            (if (evil-visual-state-p)
                (progn
                  (add-hook 'pre-command-hook #'evil-visual-pre-command nil t)
                  (add-hook 'post-command-hook #'evil-visual-post-command
                            nil t)
                  (set-mark old-mark))
              (push-mark old-mark)))))

     (evil-define-motion evil-ace-jump-char-mode (count)
       :type exclusive
       (evil-enclose-ace-jump
        (ace-jump-mode 5)))

     (evil-define-motion evil-ace-jump-line-mode (count)
       :type line
       (evil-enclose-ace-jump
        (ace-jump-mode 9)))

     (evil-define-motion evil-ace-jump-word-mode (count)
       :type exclusive
       (evil-enclose-ace-jump
        (ace-jump-mode 1)))

     (add-hook 'ace-jump-mode-end-hook 'exit-recursive-edit)

     ;; some proposals for binding:

     (define-key evil-motion-state-map (kbd "C-SPC") #'evil-ace-jump-line-mode)
     (define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-word-mode)

     ;; different jumps for different visual modes
     (defadvice evil-visual-line (before spc-for-line-jump activate)
       (define-key evil-motion-state-map (kbd "C-SPC")
         #'evil-ace-jump-line-mode))

     (defadvice evil-visual-char (before spc-for-char-jump activate)
       (define-key evil-motion-state-map (kbd "C-SPC")
         #'evil-ace-jump-char-mode))

     (defadvice evil-visual-block (before spc-for-char-jump activate)
       (define-key evil-motion-state-map (kbd "C-SPC")
         #'evil-ace-jump-char-mode))))


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
                     ac-source-words-in-all-buffer))))

;; Local Variables:
;; lexical-binding: t
;; End:

(provide 'autoloads)
;;; autoloads.el ends here
