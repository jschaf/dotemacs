(defun color-theme-wombat ()
  "A dark theme with a little bit of color."
  (interactive)
  (let ((color-theme-is-cumulative t))
    (color-theme-dark-erc)
    (color-theme-dark-gnus)
    ;;(color-theme-dark-diff)
    ;;(color-theme-dark-eshell)
    (color-theme-dark-info)
    (color-theme-dark-font-lock)
    (color-theme-install
     '(color-theme-wombat
       
       ((foreground-color . "#cecece")  ;honeydew2
	(background-color . "gray12")
	(background-mode . dark)
	(cursor-color . "gray47")
	(border-color . "hotPink"))

       (highlight ((t (:background "#386466"))))
       (fringe ((t ( :foreground "gray20" :background "gray5"))))
       (default ((t ( :foreground "white"))))
       (region ((t ( :foreground nil :background "#2e3436" ))))
       (underline ((t ( :underline t ))))
       (modeline ((t ( :foreground "white" :height 0.7 :background "gray26" :box (:style released-button)))))
       (modeline-buffer-id ((t ( :foreground nil :background nil ))))
       (modeline-mousable ((t ( :foreground "white" :background "4d4d4d" ))))
       (modeline-inactive ((t ( :foreground "gray54" :height 1.0  :background "gray17" :box (:style released-button)))))
       (modeline-highlight ((t (:background "#386466"))))
       (minibuffer-prompt ((t (:foreground "skyblue3"))))
       (vertical-border ((t (:background "gray10"))))
       (italic ((t (:foreground "paleGreen3" :italic t ))))
       (bold-italic ((t ( :foreground "dark red" :bold t :italic t ))))
       (bold ((t ( :bold t))))
       (isearch ((t (:background "#3d5450"))))
       (lazy-highlight ((t (:background "gray27"))))

       ;; Font Lock
       
       (font-lock-builtin-face ((t ( :foreground "cadet blue" :weight normal ))))
       (font-lock-comment-face ((t ( :foreground "lightCyan4" :weight normal))))
       (font-lock-constant-face ((t ( :foreground "#CC916C" :weight normal ))))
       (font-lock-doc-face ((t ( :foreground "cornsilk4" :weight normal ))))
       (font-lock-function-name-face ((t ( :foreground "#6ccc6c" :weight normal ))))
       (font-lock-keyword-face ((t ( :foreground "skyblue3" :weight normal ))))
       (font-lock-preprocessor-face ((t ( :foreground "salmon3" :weight normal ))))
       (font-lock-string-face ((t ( :foreground "#eb8ab7" :weight normal))))
       (font-lock-type-face ((t ( :foreground "#9773d9" :weight normal ))))
       (font-lock-variable-name-face ((t ( :foreground "darkSlateGray3" :weight normal ))))
       (font-lock-warning-face ((t ( :foreground "orange red" :underline "orange red" :weight normal ))))
       (show-paren-match-face ((t ( :foreground "black" :background "darkSlateGray2" :bold t))))
       (show-paren-mismatch-face ((t ( :foreground "White" :background "Red"))))

       ;; Info Mode

       (info-header-node ((t (:foreground "lime green"))))
       (info-header-xref ((t (:foreground "peru"))))
       (info-menu-header ((t (:bold t :foreground "lightGoldenRod3" :background nil))))
       (info-node ((t (:bold t :foreground "lightGoldenRod3" ))))
       (info-xref ((t (:foreground "skyblue3" :weight normal))))
       (info-xref-visited ((t (:foreground "mediumPurple3" :underline nil))))

       ;; W3M
       
       (w3m-tab-background ((t (:foreground "pink" :background "gray18"))))
       (w3m-tab-selected-background ((t (:foreground "pink" :background nil))))
       (w3m-tab-selected ((t (:foreground "white" :background "gray26" :box (:style released-button)))))
       (w3m-tab-selected-retrieving ((t (:foreground "green" :background "black"))))
       (w3m-tab-mouse ((t (:foreground nil :background "#386466"))))
       (w3m-tab-unselected ((t (:foreground "gray54" :background "gray26" :box (:style released-button)))))
       (w3m-tab-unselected-retrieving ((t ())))
       (w3m-header-line-location-title ((t (:foreground "skyblue3" :background "gray10"))))
       (w3m-anchor ((t (:foreground "skyBlue3"))))
       (w3m-form ((t (:underline t :foreground "rosyBrown3"))))
       (w3m-arrived-anchor-face ((t (:bold t :foreground "mediumPurple3"))))
       (w3m-current-anchor ((t (:bold t :underline t :foreground "palegreen3"))))
       (w3m-header-line-location-content ((t (:foreground "#e8e4d5" :background "gray12"))))

       ;; Org mode
       (org-todo ((t (:foreground "firebrick"))))
       
       ;; Woman
       (woman-bold ((t (:foreground "skyBlue3" :bold t))))
       (woman-italic-face ((t (:foreground "beige"))))
       (woman-unknown-face ((t (:foreground "LightSalmon"))))
       
       (apropos-symbol-button ((t (:foreground "skyblue3"))))

       ;; ERC

       ;; EMMS
       (emms-browser-album-face ((t (:foreground "aquamarine3"))))
       (emms-browser-track-face ((t (:foreground "gray30"))))
       (emms-playlist-track-face ((t (:foreground "gray30"))))
       (emms-browser-artist-face ((t (:foreground nil))))
       (emms-playlist-selected-face ((t (:foreground "palegreen3"))))
       (emms-browser-year/genre-face ((t (:foreground "lightsalmon3"))))

       (emms-browser-artist-face ((t (:foreground "skyblue3"))))

       ;; JDE

       (jde-java-font-lock-modifier-face ((t (:foreground "skyblue3"))))
       (jde-java-font-lock-number-face ((t (:foreground "#BFA093"))))
       (jde-java-font-lock-constant-face ((t (:foreground "#BDB168"))))
       (jde-java-font-lock-italic-face ((t (:foreground "cornSilk4" :slant italic))))
       (jde-java-font-lock-bold-face ((t (:foreground "cornSilk4" :weight bold))))
       (jde-java-font-lock-doc-tag-face ((t (:foreground "light coral"))))
       (jde-java-font-lock-link-face ((t (:foreground "skyblue3" :underline t))))
       (jde-java-font-lock-code-face ((t (:foreground "#777"))))
       (which-func ((t (:foreground "cadet blue"))))

       (yas/field-highlight-face ((t (:foreground nil :background "gray20"))))

       (flymake-errline ((t (:background nil :underline "tomato4"))))
       (flymake-warnline ((t (:background nil :underline "tan4"))))


       (hl-line ((t (:background "gray15"))))
              
       (custom-variable-tag-face ((t (:foreground "skyblue3"))))
       (custom-state ((t (:foreground "paleGreen3"))))
       (custom-group-tag ((t (:foreground "darkSlateGray3" :height 1.3 :weight ultra-bold))))
       (custom-link ((t (:foreground "skyblue3" :weight bold :underline "skyblue3"))))
       (custom-visibility ((t (:underline "cadet blue" :foreground "cadet blue" :height 0.8))))
       (custom-button ((t (:foreground "peachPuff3" :background "gray18" :height 0.8 :box (:line-width 2 :style released-button)))))
       (custom-button-mouse ((t (:foreground "peachPuff3" :background "#386466" :box (:line-width 2 :style released-button)))))
       (custom-button-pressed ((t  (:foreground "peachPuff3" :background "gray18" :height 0.8 :box (:line-width 2 :style pressed-button)))))
       (widget-field ((t (:background "gray23"))))
       
       (ecb-tag-header-face ((t (:background "deepskyblue4"))))
       (ecb-tree-guide-line-face ((t (:foreground "red"))))
       (ecb-default-general-face ((t (:height 0.7))))
       (ecb-default-highlight-face ((t (:foreground "magenta"))))
       (ecb-analyse-face ((t (:foreground "gray22"))))
       (speedbar-highlight-face ((t (:foreground "skyblue3" :weight normal :underline "skyblue3"))))

       ;; Ido
       (ido-first-match ((t (:foreground "deep sky blue" :weight normal))))
       (ido-subdir ((t (:foreground "paleGreen3"))))
       (ido-only-match ((t (:foreground "ForestGreen"))))
       
       ;; Auto-complete
       (ac-selection-face ((t ( :foreground "palegreen3" :background "gray28" :underline "gray12" ))))
       (ac-completion-face ((t ( :foreground "honeydew2" :background "gray22"))))
       (ac-candidate-face ((t ( :foreground "skyblue3" :background "gray22" :underline "gray12"))))


       ;; VHDL Mode
       (vhdl-font-lock-function-face ((t (:foreground "paleGreen3"))))       

       ;; RST Mode
       (rst-level-1-face ((t (:foreground "seagreen3" :background "yellow" ))))
       (rst-level-2-face ((t (:foreground "aquamarine3" :background nil ))))
       (rst-level-3-face ((t (:foreground "darkSlateGray3" :background nil ))))
       (rst-level-4-face ((t (:foreground "PaleTurquoise3" :background nil ))))
       (rst-level-5-face ((t (:foreground "LightCyan3" :background nil ))))
       (rst-level-6-face ((t (:foreground "azure3" :background nil ))))
       
       ))))