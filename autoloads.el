;; Require el-get
(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (let (el-get-master-branch)
       (goto-char (point-max))
       (eval-print-last-sexp)))))

(setq el-get-sources
      '((:name el-get
               :website "https://github.com/dimitri/el-get#readme"
               :description "Manage the external elisp bits and pieces you depend upon."
               :type github
               :branch "master"
               :pkgname "dimitri/el-get"
               ;:info    "."
               :load    "el-get.el")
        (:name magit
               :website "https://github.com/magit/magit#readme"
               :description "It's Magit! An Emacs mode for Git."
               :type github
               :pkgname "magit/magit"
               :depends (cl-lib)
               :info "."
               ;; let el-get care about autoloads so that it works with all OSes
               :build `(("make" ,(format "EMACS=%s" el-get-emacs) "docs")))


        (:name evil
               :website "http://gitorious.org/evil/pages/Home"
               :description "Evil is an extensible vi layer for Emacs."
               :features evil
               :depends undo-tree
               :build (("make" "all")))
     ))

(setq el-get-packages
      (append
       '(;; Auto complete framework
         auto-complete
         ;; Magit - git mode to interact with emacs
         magit
         ;; Ace-jump
         ace-jump-mode
         ;; Easy way to write empty HTML
         zencoding-mode
         ;; Solarized color theme
         color-theme-solarized
         ;; A smart M-x
         smex
         ;; Key chords, pressing two keys rapidly to execute a command
         key-chord)
       (mapcar 'el-get-as-symbol
               (mapcar 'el-get-source-name el-get-sources))))

;; install new packages and init already installed packages
(el-get 'sync el-get-packages)

(autoload 'toggle-uniquify-buffer-names "uniquify" nil t)
(toggle-uniquify-buffer-names)

(key-chord-mode 1)

(key-chord-define-global "fh" 'windmove-left)
(key-chord-define-global "fj" 'windmove-down)
(key-chord-define-global "fk" 'windmove-up)
(key-chord-define-global "fl" 'windmove-right)
(key-chord-define-global "j0" 'delete-window)
(key-chord-define-global "j1" 'delete-other-windows)
(key-chord-define-global "j2" 'split-window-vertically)
(key-chord-define-global "j3" 'split-window-horizontally)
(key-chord-define-global "jx" 'smex)
(key-chord-define-global "jg" 'keyboard-quit)
(key-chord-define-global "xb" 'ido-switch-buffer)
(key-chord-define-global "/f" 'ido-find-file)
(key-chord-define-global "/s" 'save-buffer)
(key-chord-define-global "nb" 'bookmark-jump)
(key-chord-define-global "nm" 'bookmark-set)
(key-chord-define-global "nl" 'bookmark-bmenu-list)
(key-chord-define-global "jk" 'evil-normal-state)
(key-chord-define-global "cv" 'eval-last-sexp)
(key-chord-define-global "/c" 'goto-last-change)
(setq key-chord-two-keys-delay 0.08)


(require 'evil)
(evil-mode 1)

(require 'ace-jump-mode)

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
  (define-key evil-motion-state-map (kbd "C-SPC") #'evil-ace-jump-char-mode))

(require 'ein)
