(load "custom-modes")
(load "custom-org")


(fset 'yes-or-no-p 'y-or-n-p)           ;replace y-e-s with y
(setq bookmark-default-file "~/.emacs.d/.emacs.bmk")
(menu-bar-mode -1)

(put 'narrow-to-region 'disabled nil)   ;useful for regexps
(put 'scroll-left 'disabled nil)        ;need this mainly for csv files
(setq tramp-default-method "ssh")
;; Mouse
(global-set-key [down-mouse-3] 'imenu)

(setq echo-keystrokes 0.5)

(global-set-key [?\M-\C-z] #'move-to-char)

(defface line-over-72
  '((t :underline "red"))
  "Used to highlight lines with text past 72 columns."
  :group 'basic-faces)

;; Aliases
(defalias 'dtw 'delete-trailing-whitespace)
(defalias 'ar  'align-regexp)
(defalias 'rr  'replace-regexp)
(defalias 'qrr 'query-replace-regexp)
(defalias 'ap 'apropos)
(defalias 'cv 'customize-variable)
(defalias 'cg 'customize-group)
(defalias 'ttl 'toggle-truncate-lines)

;; make the windows key a super key and don't tell windows we hit it
(if (string= system-type "windows-nt")
    '((setq w32-lwindow-modifier 'super)
      (setq w32-pass-lwindow-to-system nil)
      (setq w32-rwindow-modifier 'super)
      (setq w32-pass-rwindow-to-system nil)))
