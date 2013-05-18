;; Use my fork of el-get
(setq el-get-git-install-url "http://github.com/jschaf/el-get.git")

;; This takes forever and I don't want to install stuff from
;; emacswiki.
(setq el-get-install-skip-emacswiki-recipes t)

;; Require el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/jschaf/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(setq el-get-sources
      '((:name evil
               :after (require 'evil)
               :build (("make" "all"))
               ;; makeinfo fails
               :info nil)
        (:name key-chord
               :after (require 'key-chord))
        (:name solarized-theme
               :after (load-theme 'solarized-light t))))

(setq el-get-packages
      (append
       '(el-get
         evil
         auto-complete
         ace-jump-mode
         paredit
         zencoding-mode
         solarized-theme
         smex
         magit
         key-chord)
       (mapcar 'el-get-source-name el-get-sources)))

;; We can get whole repositories later if we want to hack on them.
(setq el-get-git-shallow-clone t)

;; Install all the packages asynchronusly
(el-get nil el-get-packages)

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
             ("j0" . delete-window)
             ("j1" . delete-other-windows)
             ("j2" . split-window-vertically)
             ("j3" . split-window-horizontally)
             ("jx" . smex)
             ("jg" . keyboard-quit)
             ("xb" . ido-switch-buffer)
             ("/f" . ido-find-file)
             ("/r" . recentf-ido-find-file)
             ("/s" . save-buffer)
             ("nb" . bookmark-jump)
             ("nm" . bookmark-set)
             ("nl" . bookmark-bmenu-list)
             ("jk" . evil-normal-state)
             ("cv" . eval-last-sexp)
             ("/c" . goto-last-change))
           do (key-chord-define-global key func))

     (loop for (key . func) in
           '(("sh" . paredit-backward)
             ("sl" . paredit-forward)
             ("sj" . paredit-forward-down)
             ("sk" . paredit-backward-up)
             ("gh" . paredit-backward-slurp-sexp)
             ("gl" . paredit-forward-slurp-sexp)
             ("dl" . paredit-forward-barf-sexp)
             ("dh" . paredit-backward-barf-sexp))
           do (key-chord-define-global key func))))

;; Evil
(defun my:evil-setup ()
  "The initial customization for evil mode.

This is separated into a function so I can edit it without
figuring out how to reload the package."
  (interactive)
  (evil-mode 1)

  ;; Use different colors for fonts to easily determine what mode we're in.
  (setq evil-default-cursor "#5EA0AD")
  (setq evil-normal-state-cursor evil-default-cursor)
  (setq evil-insert-state-cursor "#AD5E5E")
  (setq evil-visual-state-cursor evil-default-cursor)
  (setq evil-replace-state-cursor evil-default-cursor)
  (setq evil-operator-state-cursor nil)
  (setq evil-motion-state-cursor evil-default-cursor)
  (setq evil-emacs-state-cursor evil-default-cursor)

  (setq evil-want-visual-char-semi-exclusive t)
  (setq evil-move-cursor-back nil)

  (define-key evil-normal-state-map "\C-j" 'scroll-up-command)
  (define-key evil-motion-state-map "\C-j" 'scroll-up-command)
  (define-key evil-normal-state-map "\C-k" 'scroll-down-command)
  (define-key evil-motion-state-map "\C-k" 'scroll-down-command)
  (define-key evil-normal-state-map (kbd "<tab>") 'indent-for-tab-command)

  ;; Undefine the comma to use it as the leader key
  (define-key evil-normal-state-map "," nil)
  (define-key evil-motion-state-map "," nil)
  (let ((leader-map (make-sparse-keymap)))
    (define-key evil-normal-state-map "," leader-map)
    (define-key evil-motion-state-map "," leader-map)
    (loop for (key . func) in
          '(("xg" . magit-status)
            ("ht" . describe-text-properties)
            ("k"  . (lambda () (interactive) (kill-buffer nil)))
            ("cs" . (lambda () (interactive) (switch-to-buffer "*scratch*")))
            ("o"  . delete-blank-lines)
            ("dtw" . delete-trailing-whitespace)
            ("gh" . (lambda () (interactive) (find-file "~/")))
            ("xh" . mark-whole-buffer)
            ("xd" . ido-dired)
            ("cp" . check-parens)
            ("ei" . el-get-install)
            ("de" . toggle-debug-on-error)
            ("ts" . toggle-color-theme)
            ("r"  . jump-to-register))
          do (define-key leader-map key func))))

(eval-after-load 'evil 'my:evil-setup)

(eval-after-load 'ace-jump-mode
  '(progn
     ;;(autoload 'ace-jump-mode "ace-jump-mode" "Emacs quick move minor mode" t)
     ;; AceJump is a nice addition to evil's standard motions.

     ;; The following definitions are necessary to define evil motions for ace-jump-mode (version 2).

     ;; ace-jump is actually a series of commands which makes handling by evil
     ;; difficult (and with some other things as well), using this macro we let it
     ;; appear as one.

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
                  (add-hook 'post-command-hook #'evil-visual-post-command nil t)
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

     (define-key evil-motion-state-map (kbd "C-SPC") #'evil-ace-jump-char-mode)
     (define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-word-mode)
     (define-key evil-operator-state-map (kbd "S-SPC") #'evil-ace-jump-line-mode)

     ;; different jumps for different visual modes
     (defadvice evil-visual-line (before spc-for-line-jump activate)
       (define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-line-mode))

     (defadvice evil-visual-char (before spc-for-char-jump activate)
       (define-key evil-motion-state-map (kbd "C-SPC") #'evil-ace-jump-char-mode))

     (defadvice evil-visual-block (before spc-for-char-jump activate)
       (define-key evil-motion-state-map (kbd "C-SPC") #'evil-ace-jump-char-mode))))

(eval-after-load 'smex
  '(progn
     ;; Update smex command cache after all the loads.
     (smex-update)))
