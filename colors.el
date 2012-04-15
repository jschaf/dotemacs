;; My color theme so I don't depend on the color-theme library.

(setq ranger-ranger-frame-colors
 '((foreground-color . "#cecece")
   (background-color . "gray12")
   (background-mode . dark)
   (cursor-color . "gray47")
   (border-color . "hotPink")))

(setq ranger-ranger-theme
      '((highlight :background "#386466")
        (fringe  :foreground "gray20" :background "gray5")
        (default  :foreground "#cecece")
        (region  :foreground nil :background "#2e3436")
        (menu :font "Consolas")
        (underline  :underline t)
        (modeline  :foreground "white" :background "gray26" :box (:style released-button))
        (modeline-buffer-id  :foreground nil :background nil)
        (modeline-mousable  :foreground "white" :background "#4d4d4d")
        (modeline-inactive  :foreground "gray54" :background "gray17" :box (:style released-button))
        (modeline-highlight :background "#386466")
        (minibuffer-prompt :foreground "skyblue3")
        (vertical-border :background "gray10")
        (italic :foreground "paleGreen3" :italic t)
        (bold-italic  :foreground "dark red" :bold t :italic t)
        (bold  :bold t)
        (isearch :foreground "green" :background "#3d5450")
        (lazy-highlight :background "gray19")
        (variable-pitch :family "Calibri")
        (link :foreground "skyblue3" :underline "skyblue3")

        ;; Font Lock

        (font-lock-builtin-face  :foreground "cadet blue" :weight normal)
        (font-lock-comment-face  :foreground "lightCyan4" :weight normal)
        (font-lock-constant-face  :foreground "#CC916C" :weight normal)
        (font-lock-doc-face  :foreground "cornsilk4" :weight normal)
        (font-lock-function-name-face  :foreground "#6ccc6c" :weight normal)
        (font-lock-keyword-face  :foreground "skyblue3" :weight normal)
        (font-lock-preprocessor-face  :foreground "salmon3" :weight normal)
        (font-lock-string-face  :foreground "DarkSalmon" :weight normal)
        (font-lock-type-face  :foreground "#9773d9" :weight normal)
        (font-lock-variable-name-face  :foreground "darkSlateGray3" :weight normal)
        (font-lock-warning-face  :foreground "orange red" :underline "orange red" :weight normal)
        (show-paren-match-face  :foreground "black" :background "darkSlateGray2" :bold t)
        (show-paren-mismatch-face  :foreground "White" :background "Red")

        ;; Info Mode

        (info-header-node :foreground "lime green")
        (info-header-xref :foreground "peru")
        (info-menu-header :bold t :foreground "lightGoldenRod3" :background nil)
        (info-node :bold t :foreground "lightGoldenRod3")
        (info-xref :foreground "skyblue3" :weight normal)
        (info-xref-visited :foreground "mediumPurple3" :underline nil)

        ;; Woman
        (woman-bold :foreground "skyBlue3" :bold t)
        (woman-italic-face :foreground "beige")
        (woman-unknown-face :foreground "LightSalmon")

        (apropos-symbol-button :foreground "skyblue3")


        (custom-variable-tag-face :foreground "skyblue3")
        (custom-state :foreground "paleGreen3")
        (custom-group-tag :foreground "darkSlateGray3" :height 1.3 :weight ultra-bold)
        (custom-link :foreground "skyblue3" :weight bold :underline "skyblue3")
        (custom-visibility :underline "cadet blue" :foreground "cadet blue" :height 0.8)
        (custom-button :foreground "peachPuff3" :background "gray18" :height 0.8 :box (:line-width 2 :style released-button))
        (custom-button-mouse :foreground "peachPuff3" :background "#386466" :box (:line-width 2 :style released-button))
        (custom-button-pressed :foreground "peachPuff3" :background "gray18" :height 0.8 :box (:line-width 2 :style pressed-button))
        (widget-field :background "gray23")

        (ecb-tag-header-face :background "deepskyblue4")
        (ecb-tree-guide-line-face :foreground "red")
        (ecb-default-general-face :height 0.7)
        (ecb-default-highlight-face :foreground "magenta")
        (ecb-analyse-face :foreground "gray22")
        (speedbar-highlight-face :foreground "skyblue3" :weight normal :underline "skyblue3")

        ;; Diff
        (diff-added :foreground "paleGreen3")
        (diff-removed :foreground "IndianRed")
        (diff-header :background nil)

        ;; Magit
        (magit-item-highlight :foreground nil :background "#2e3436")
        (magit-diff-file-header :foreground "skyblue3" :background nil :inherit nil)
        (magit-header :foreground "skyblue3" :background nil :weight bold :inherit nil)
        ;; Ido
        (ido-first-match :foreground "deep sky blue" :weight normal)
        (ido-subdir :foreground "paleGreen3")
        (ido-only-match :foreground "ForestGreen")

        ;; RST Mode
        (rst-level-1-face :foreground "seagreen3" :background nil)
        (rst-level-2-face :foreground "aquamarine3" :background nil)
        (rst-level-3-face :foreground "darkSlateGray3" :background nil)
        (rst-level-4-face :foreground "PaleTurquoise3" :background nil)
        (rst-level-5-face :foreground "LightCyan3" :background nil)
        (rst-level-6-face :foreground "azure3" :background nil)))

(defun load-ranger-ranger-theme ()
  (interactive)
  (modify-all-frames-parameters ranger-ranger-frame-colors)
  (dolist (face-args ranger-ranger-theme)
    (let* ((face (make-empty-face (car face-args)))
           ;; Apply to all existing frames by using nil
           (frame nil)
           (attrs (cons frame (cdr face-args))))
      (apply 'set-face-attribute face attrs))))

