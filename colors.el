;; My color theme so I don't depend on the color-theme library.

(setq ranger-ranger-frame-colors
 '((foreground-color . "#cecece")
   (background-color . "gray12")
   (background-mode . dark)
   (cursor-color . "gray47")
   (border-color . "hotPink")))

(setq ranger-ranger-theme
      '((highlight nil :background "#386466")
        (fringe nil  :foreground "gray20" :background "gray5")
        (default nil  :foreground "#cecece")
        (region nil  :foreground nil :background "#2e3436" )
        (underline nil  :underline t )
        (modeline nil  :foreground "white" :background "gray26" :box (:style released-button))
        (modeline-buffer-id nil  :foreground nil :background nil )
        (modeline-mousable nil  :foreground "white" :background "#4d4d4d" )
        (modeline-inactive nil  :foreground "gray54" :background "gray17" :box (:style released-button))
        (modeline-highlight nil :background "#386466")
        (minibuffer-prompt nil :foreground "skyblue3")
        (vertical-border nil :background "gray10")
        (italic nil :foreground "paleGreen3" :italic t )
        (bold-italic nil  :foreground "dark red" :bold t :italic t )
        (bold nil  :bold t)
        (isearch nil :foreground "green" :background "#3d5450")
        (lazy-highlight nil :background "gray19")
        (variable-pitch nil :family "Calibri")

        ;; Font Lock

        (font-lock-builtin-face nil  :foreground "cadet blue" :weight normal )
        (font-lock-comment-face nil  :foreground "lightCyan4" :weight normal)
        (font-lock-constant-face nil  :foreground "#CC916C" :weight normal )
        (font-lock-doc-face nil  :foreground "cornsilk4" :weight normal )
        (font-lock-function-name-face nil  :foreground "#6ccc6c" :weight normal )
        (font-lock-keyword-face nil  :foreground "skyblue3" :weight normal )
        (font-lock-preprocessor-face nil  :foreground "salmon3" :weight normal )
        (font-lock-string-face nil  :foreground "#eb8ab7" :weight normal)
        (font-lock-type-face nil  :foreground "#9773d9" :weight normal )
        (font-lock-variable-name-face nil  :foreground "darkSlateGray3" :weight normal )
        (font-lock-warning-face nil  :foreground "orange red" :underline "orange red" :weight normal )
        (show-paren-match-face nil  :foreground "black" :background "darkSlateGray2" :bold t)
        (show-paren-mismatch-face nil  :foreground "White" :background "Red")

        ;; Info Mode

        (info-header-node nil :foreground "lime green")
        (info-header-xref nil :foreground "peru")
        (info-menu-header nil :bold t :foreground "lightGoldenRod3" :background nil)
        (info-node nil :bold t :foreground "lightGoldenRod3" )
        (info-xref nil :foreground "skyblue3" :weight normal)
        (info-xref-visited nil :foreground "mediumPurple3" :underline nil)

        ;; Woman
        (woman-bold nil :foreground "skyBlue3" :bold t)
        (woman-italic-face nil :foreground "beige")
        (woman-unknown-face nil :foreground "LightSalmon")

        (apropos-symbol-button nil :foreground "skyblue3")


        (custom-variable-tag-face nil :foreground "skyblue3")
        (custom-state nil :foreground "paleGreen3")
        (custom-group-tag nil :foreground "darkSlateGray3" :height 1.3 :weight ultra-bold)
        (custom-link nil :foreground "skyblue3" :weight bold :underline "skyblue3")
        (custom-visibility nil :underline "cadet blue" :foreground "cadet blue" :height 0.8)
        (custom-button nil :foreground "peachPuff3" :background "gray18" :height 0.8 :box (:line-width 2 :style released-button))
        (custom-button-mouse nil :foreground "peachPuff3" :background "#386466" :box (:line-width 2 :style released-button))
        (custom-button-pressed nil :foreground "peachPuff3" :background "gray18" :height 0.8 :box (:line-width 2 :style pressed-button))
        (widget-field nil :background "gray23")

        (ecb-tag-header-face nil :background "deepskyblue4")
        (ecb-tree-guide-line-face nil :foreground "red")
        (ecb-default-general-face nil :height 0.7)
        (ecb-default-highlight-face nil :foreground "magenta")
        (ecb-analyse-face nil :foreground "gray22")
        (speedbar-highlight-face nil :foreground "skyblue3" :weight normal :underline "skyblue3")

        ;; Ido
        (ido-first-match nil :foreground "deep sky blue" :weight normal)
        (ido-subdir nil :foreground "paleGreen3")
        (ido-only-match nil :foreground "ForestGreen")

        ;; RST Mode
        (rst-level-1-face nil :foreground "seagreen3" :background "yellow" )
        (rst-level-2-face nil :foreground "aquamarine3" :background nil )
        (rst-level-3-face nil :foreground "darkSlateGray3" :background nil )
        (rst-level-4-face nil :foreground "PaleTurquoise3" :background nil )
        (rst-level-5-face nil :foreground "LightCyan3" :background nil )
        (rst-level-6-face nil :foreground "azure3" :background nil )))

(defun go-ranger-ranger ()
  (interactive)
  (modify-all-frames-parameters ranger-ranger-frame-colors)
  (dolist (face-args ranger-ranger-theme)
    (let ((face (make-empty-face (car face-args)))
          (attrs (cdr face-args)))
      (apply 'set-face-attribute face attrs))))

(go-ranger-ranger)