;; Hudson mode
(add-hook 'hudson-mode-hook
          (lambda ()
            (local-set-key "\C-m" 'hudson-newline-and-indent)
            (else-mode 1)
            (subword-mode 1)))

(setq hudson-jar-file (expand-file-name "~/prog/hudson/hudson.jar"))

;; Hideshow support
(dolist (mode '("emacs-lisp" "hudson" "jde"))
  (let ((hook (intern (concat mode "-mode-hook"))))
    (add-hook hook 'hs-minor-mode)))

(add-hook 'hs-minor-mode-hook
          (lambda ()
            (local-set-key "\M-=" 'hs-toggle-hiding)
            (local-set-key "\M-_" 'hs-hide-all)
            (local-set-key "\M-+" 'hs-show-all)))

(add-hook 'compilation-mode-hook
          (lambda ()
            (local-set-key "\M-n" 'cycle-buffer)
            (local-set-key "\M-p" 'cycle-buffer-backward)
            ))

;; Redefine the 8 primary terminal colors to look good against black
(setq ansi-term-color-vector
      [unspecified "#000000" "#963F3C" "#5FFB65" "#FFFD65" 
                   "#0082FF" "#FF2180" "#57DCDB" "#FFFFFF"])

;; Misc
(global-set-key "\C-ha" 'apropos)
(global-set-key (kbd "<f1>") 'menu-bar-mode)
(global-set-key [(super meta /)] (lambda () (interactive) (kill-buffer nil)))
(global-set-key (kbd "C-M-/") (lambda () (interactive) (kill-buffer nil)))

;; Help-mode
;; (define-key help-mode-map "j" (lambda () (interactive) (scroll-up 1)))
;; (define-key help-mode-map "k" (lambda () (interactive) (scroll-down 1)))
;; (define-key help-mode-map "l" 'help-go-back)
;; (define-key help-mode-map "h" 'help-go-forward)

;; VHDL
(defun vhdl-ghdl-check-syntax (&optional arg)
  "Runs the ghdl check syntax command on the current buffer.  If
  arg is non-nil then prompts for the comand."
  (interactive "P")
  (let ((command (concat "ghdl -s "
                         (buffer-name))))
    (compile command)
    ))

(add-hook 'vhdl-mode-hook
          (lambda ()
            (local-set-key "\C-c\C-v" 'vhdl-ghdl-check-syntax)
            (set (make-local-variable 'paragraph-start) "\f\\|[ 	]*$")
            (set (make-local-variable 'paragraph-separate) "[ 	\f]*$")))

;; dired-x
;; Set dired-x global variables here.  For example:
;; (setq dired-guess-shell-gnutar "gtar")
;; (setq dired-x-hands-off-my-keys nil)
(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")
            ;; Don't show dot files in dired
            (setq dired-omit-files
                  (concat dired-omit-files "\\|^\\..+$")
                  )))
(add-hook 'dired-mode-hook
          (lambda ()
            ;; Set dired-x buffer-local variables here.  For example:
            (dired-omit-mode 1)))

;; Emacs lisp
(add-hook 'emacs-lisp-mode-hook
	  '(lambda ()
             (subword-mode 1)
             (turn-on-eldoc-mode)
             (paredit-mode 1)))

(add-hook 'lisp-interaction-mode-hook
          (lambda ()
            (local-set-key "\C-\M-j" 'eval-print-last-sexp)))


(add-hook 'ada-mode-hook
          (lambda ()
            (outline-minor-mode 1)
            (local-set-key "\M-_" 'hide-leaves)
            (local-set-key "\M-=" 'show-entry)
            (local-set-key "\C-cm" 'ada-method-header)
            (local-set-key "\C-xnd" 'ada-narrow-to-defun)
            ;; (local-set-key "\C-ci" 'joe/ada-incr-variable)
            (setq ada-fill-comment-postfix "-- ")
            (else-mode 1)
            (key-chord-define ada-mode-map "ja"
                              (lambda () (interactive) (insert ":= ")))
            (key-chord-define ada-mode-map "jd"
                              (lambda () (interactive) (insert "_")))
            (local-set-key [(meta \')] 'else-kill-placeholder)))

(add-hook 'else-mode-hook
          (lambda ()
            (local-set-key "\M-n" 'else-next-placeholder)
            (local-set-key "\M-p" 'else-previous-placeholder)
            (local-set-key "\M-i" 'else-insert-placeholder)
            (local-set-key "\M-o" 'else-expand-placeholder)
            (local-set-key "\M-k" 'else-kill-placeholder)
            (local-set-key "\M-'" 'else-kill-proceed-to-next-placeholder)))

;; Haskell
(add-hook 'haskell-mode-hook
          (lambda ()
            (key-chord-define haskell-mode-map "ja"
                              (lambda () (interactive) (insert "-> ")))
            (require 'inf-haskell)
            (require 'hs-lint)
            (local-set-key "\C-cl" 'hs-lint)))

;; LaTeX stuff
(add-hook 'latex-mode-hook
          (lambda ()
            (turn-on-reftex)
            (face-spec-set (default ((t ( :foreground "white")))))
            (set (make-local-variable sentence-end) "[.?!][]\"')}]*\\($\\|     \\|  \\)[
]*")
            (set-face-attribute 'font-latex-sedate-face nil
                                :foreground "red")))

;; when viewing pdf/dvi automatically reload them if they change
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

(defun ido-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
   ((not symbol-list)
    (let ((ido-mode ido-mode)
          (ido-enable-flex-matching
           (if (boundp 'ido-enable-flex-matching)
               ido-enable-flex-matching t))
          name-and-pos symbol-names position)
      (unless ido-mode
        (ido-mode 1)
        (setq ido-enable-flex-matching t))
      (while (progn
               (imenu--cleanup)
               (setq imenu--index-alist nil)
               (ido-goto-symbol (imenu--make-index-alist))
               (setq selected-symbol
                     (ido-completing-read "Symbol? " symbol-names))
               (string= (car imenu--rescan-item) selected-symbol)))
      (setq position (cdr (assoc selected-symbol name-and-pos)))
      (cond
       ((overlayp position)
        (goto-char (overlay-start position)))
       (t
        (goto-char position)))))
   ((listp symbol-list)
    (dolist (symbol symbol-list)
      (let (name position)
        (cond
         ((and (listp symbol) (imenu--subalist-p symbol))
          (ido-goto-symbol symbol))
         ((listp symbol)
          (setq name (car symbol))
          (setq position (cdr symbol)))
         ((stringp symbol)
          (setq name symbol)
          (setq position
                (get-text-property 1 'org-imenu-marker symbol))))
        (unless (or (null position) (null name)
                    (string= (car imenu--rescan-item) name))
          (add-to-list 'symbol-names name)
          (add-to-list 'name-and-pos (cons name position))))))))

(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let* ((file-assoc-list
	  (mapcar (lambda (x)
		    (cons (file-name-nondirectory x)
			  x))
		  recentf-list))
	 (filename-list
	  (remove-duplicates (mapcar #'car file-assoc-list)
			     :test #'string=))
	 (filename (ido-completing-read "Choose recent file: "
					filename-list
					nil
					t)))
    (when filename
      (find-file (cdr (assoc filename
			     file-assoc-list))))))
(global-set-key "\C-cr" 'recentf-ido-find-file)
(global-set-key "\C-ci" 'ido-goto-symbol)

;; ERC
(remove-hook 'erc-echo-notice-always-hook 'erc-echo-notice-in-default-buffer)
