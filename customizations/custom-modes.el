;; hudson mode
(add-hook 'hudson-mode-hook
          (lambda ()
            (local-set-key "\C-m" 'hudson-newline-and-indent)
            (else-mode 1)
            (c-subword-mode 1)))
;; (setq hudson-jar-file "E:\\Users\\joe\\prog\\hudson\\hudson.jar")
(setq hudson-jar-file (expand-file-name "~/prog/hudson/hudson.jar"))

;; Hideshow support
(dolist (mode '("emacs-lisp" "hudson" "jde"))
  (let ((hook (intern (concat mode "-mode-hook"))))
    (add-hook hook 'hs-minor-mode)))

(add-hook 'eshell-mode-hook
          (lambda ()
            (let ((path (getenv "PATH")))
              (setq path (concat path ";.;e:/Program Files (x86)/darcs-2.3.1-win1/")))))

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

;; Template (ELSE) mode
(add-hook 'else-mode-hook
          (lambda ()
            (add-to-list 'hs-special-modes-alist
                         '(else-mode "DELETE PLACEHOLDER" "END DEFINE"
                                         "###" nil nil))
            (hs-minor-mode 1)))

;; Term-mode
(add-hook 'term-mode-hook
          (lambda ()
            (set (make-local-variable 'global-hl-line-mode) nil)))

;; Redefine the 8 primary terminal colors to look good against black
(setq ansi-term-color-vector
      [unspecified "#000000" "#963F3C" "#5FFB65" "#FFFD65" 
                   "#0082FF" "#FF2180" "#57DCDB" "#FFFFFF"])
;; Java
(add-to-list 'auto-mode-alist '("\\.java\\'" . jde-mode))
(eval-after-load 'jde
  '(progn
     (load "custom-java")))

;; Python
(setq ipython-command "c:/python26/scripts/ipython.exe")
(require 'ipython)

(eval-after-load "python-mode"
  '(progn
     ;;(load "custom-python")
     ))

;; Misc
(global-set-key "\C-ha" 'apropos)
(global-set-key (kbd "<f1>") 'menu-bar-mode)
(global-set-key [(super meta /)] (lambda () (interactive) (kill-buffer nil)))
(global-set-key (kbd "C-M-/") (lambda () (interactive) (kill-buffer nil)))

;; Help-mode
(define-key help-mode-map "j" (lambda () (interactive) (scroll-up 1)))
(define-key help-mode-map "k" (lambda () (interactive) (scroll-down 1)))
(define-key help-mode-map "l" 'help-go-back)
(define-key help-mode-map "h" 'help-go-forward)

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
                  ;;(concat dired-omit-files "\\|^\\..+$")
                  )))
(add-hook 'dired-mode-hook
          (lambda ()
            ;; Set dired-x buffer-local variables here.  For example:
            (dired-omit-mode 1)))

;; C type languages
(add-hook 'c-mode-common-hook
	  (lambda ()
            (c-subword-mode 1)))
(setq c-default-style '((java-mode . "k&r")
			(awk-mode . "awk")
			(other . "linux")))
;; Emacs lisp
(add-hook 'emacs-lisp-mode-hook
	  '(lambda ()
             (c-subword-mode 1)
             (turn-on-eldoc-mode)
             (paredit-mode 1)))
(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "C-<home>") 'paredit-forward-slurp-sexp)))

(add-hook 'lisp-interaction-mode-hook
          (lambda ()
            (local-set-key "\C-J" 'eval-print-last-sexp)))
;; Ada
(eval-after-load 'ada-mode
  '(progn
     (load "ada-mode-keys")
     ))

;; (defconst ada-hs-start-regexp
;;   (rx-to-string '(line-start (0+ space) (or "function" "procedure") ))
;;   "Regexp to match start of the expression to hide.")
;; (global-unset-key (kbd "C-9"))
(global-set-key (kbd "C-*") 'paredit-forward-slurp-sexp)

;; Stolen from VHDL mode
(defun ada-hs-minor-mode (&optional arg)
  "Toggle hideshow minor."
  (interactive "P")
  (require 'hideshow)
  ;; check for hideshow version 5.x
  (if (not (boundp 'hs-block-start-mdata-select))
      (vhdl-warning-when-idle "Install included `hideshow.el' patch first (see INSTALL file)")
    ;; initialize hideshow
    (unless (assoc 'ada-mode hs-special-modes-alist)
      (setq hs-special-modes-alist
	    (cons (list 'ada-mode ada-hs-start-regexp nil "--\\( \\|$\\)"
			'vhdl-hs-forward-sexp-func nil)
		  hs-special-modes-alist)))
    (make-local-variable 'hs-minor-mode-hook)
    (if vhdl-hide-all-init
	(add-hook 'hs-minor-mode-hook 'hs-hide-all)
      (remove-hook 'hs-minor-mode-hook 'hs-hide-all))
    (hs-minor-mode arg))
  )

(add-hook 'ada-mode-hook
          (lambda ()
            (outline-minor-mode 1)
            (local-set-key "\M-_" 'hide-leaves)
            (local-set-key "\M-=" 'show-entry)
            (local-set-key "\C-cm" 'ada-method-header)
            (local-set-key "\C-xnd" 'ada-narrow-to-defun)
            (local-set-key "\C-ci" 'joe/ada-incr-variable)
            (else-mode 1)
            (local-set-key "\C-ct" 'ada-atest-compile-and-run)
            (local-set-key "\C-c\M-t"
                           'ada-atest-write-test-results)
            (key-chord-define ada-mode-map "ja"
                              (lambda () (interactive) (insert ":= ")))
            (key-chord-define ada-mode-map "jd"
                              (lambda () (interactive) (insert "_")))
            (setenv "ADA_INCLUDE_PATH" "/usr/local/lib/ada:$ADA_INCLUDE_PATH")
            (local-set-key [(meta \')] 'else-kill-placeholder)))

(defface unit-test-success
  '((t :foreground "green" :weight bold))
  "For passing a unit test"
  :group 'basic-faces)

(defface unit-test-failure
  '((t :foreground "red" :weight bold))
  "For failing a unit test"
  :group 'basic-faces)

(defun substitute-pattern-with-string (pattern string)
  "Add a font lock hook to replace the matched part of PATTERN
     with the string"
  (interactive)
  (font-lock-add-keywords
   nil `((,pattern
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,string
                                    'decompose-region)
                    nil))))))



(defun ada-atest-compile-and-run (&optional arg)
  "Runs atest on a homework file and shows the compilation buffer
with some fontification.  If arg is non-nil no extra syntax
highlighting is done"
  (interactive "P")
  (let ((command (concat "atest "
                         (file-name-sans-extension (buffer-name)))))

    (save-excursion
      (set-buffer (get-buffer-create "*atest*"))
      (setq buffer-read-only nil)
      (erase-buffer)
      (compilation-mode "atest")

      (if (not arg)
          (progn
            (font-lock-add-keywords
             nil
             '(("\\<\\(Test #[0-9]+\\):" 1 font-lock-keyword-face t)
               ("Ok\\." 0 'unit-test-success)
               ("\\.\\.\\. \\(Passed\\)$" 1 'unit-test-success t)
               ("\\*\\*\\* \\(FAILED\\):" 1 'unit-test-failure t)
               ("ERROR" 0 font-lock-warning-face)
               ("\\<FAILURE LIMIT REACHED" 0 font-lock-warning-face)))

            (substitute-pattern-with-string "\\( \\.\\.\\. \\*\\*\\* \\)" " ")
            (substitute-pattern-with-string "\\( \\.\\.\\. \\)" " ")
            (substitute-pattern-with-string "\\( \\*\\*\\*$\\)" " ")
            (substitute-pattern-with-string "\\(\\*\\*\\*\\*\\*\\) " "")))

      (start-process "atest" (current-buffer) shell-file-name
                     "-c" command))
    (display-buffer "*atest*")))

(defun ada-atest-write-test-results ()
  "Writes test results of current homework into hwNresults.txt"
  (interactive)
  (let ((test-results (concat "~/docs/cs385/"
                              (file-name-sans-extension (buffer-name))))
        ((command (concat "cd ~/docs/cs385/"
                          (file-name-sans-extension (buffer-name)) ";atest "
                          (file-name-sans-extension buffer-name)))))

    (write-region
     (with-output-to-string
       (start-process "atest" (current-buffer) shell-file-name
                      "-c" command))
     nil test-results)))


(defun get-cs385-homework (&optional arg)
  "Downloads and opens the adb file of homework arg or if it
  already exists it opens up the corresponding file"
  (interactive "NEnter a homework number: ")
  (let* ((baseurl "http://www-internal.eecs.usma.edu/courses/cs385/hw/")
         (homework (concat "hw" (number-to-string arg)))
         (basepath (concat "~/docs/cs385/" homework "/"))
         (test-name (concat "test" homework ".txt"))
         (adb-name (concat homework ".adb"))
         (adb-file (concat basepath adb-name))
         (test-file (concat basepath test-name))
         (adb-url (concat baseurl adb-name))
         (test-url (concat baseurl test-name)))
    (setq mypath basepath)
    (setq myfile adb-file)
    (setq mytest test-file)
    
    (defun clean-and-visit-cs-file (status)
      "Takes input from `url-retrieve' and writes the file
       starting with the first valid Ada comment"
      (if (re-search-forward "^-- " (point-max) t)
          (progn
            (make-directory mypath)
            (write-region (match-beginning 0) (point-max) myfile)
            (find-file myfile))
        (message "Invalid url or there is not a beginning comment")))

    (defun clean-test-file (status)
      "Write the test file and stip out html headers"
      (if (re-search-forward "^[0-9]+ [0-9]+" (point-max) t)
          (write-region (match-beginning 0) (point-max) mytest)
        (message "Invalid test file url or it doesn't begin with 2 numbers")))
    
    (if (file-exists-p adb-file)
        (find-file adb-file)
      (progn
        (url-retrieve adb-url 'clean-and-visit-cs-file)
        (url-retrieve test-url 'clean-test-file)))))

(add-hook 'else-mode-hook
          (lambda ()
            (local-set-key "\M-n" 'else-next-placeholder)
            (local-set-key "\M-p" 'else-previous-placeholder)
            (local-set-key "\M-i" 'else-insert-placeholder)
            (local-set-key "\M-o" 'else-expand-placeholder)
            (local-set-key "\M-k" 'else-kill-placeholder)
            (local-set-key "\M-'" 'else-kill-proceed-to-next-placeholder)))

;; (add-hook 'rst-mode-hook
;;           (lambda ()
;;             ))

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
          '('turn-on-reftex)
          '(face-spec-set (default ((t ( :foreground "white")))))
          '(set-face-attribute 'font-latex-sedate-face nil
                               :foreground "red"))

;; when viewing pdf/dvi automatically reload them if they change
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

;; W3M keymap

(defun joe/w3m-setup-keymap ()
  "Use my heavily customized map."
  (interactive)
                                        ;(define-key w3m-mode-map "a" 'sacha/delicious-url)
  (define-key w3m-mode-map "A" 'w3m-bookmark-add-current-url)
  (define-key w3m-mode-map "w" 'w3m-download-with-wget)
  (define-key w3m-mode-map "g" 'w3m-search)

  ;; i is a more useful mnemonic for toggling images
  (define-key w3m-mode-map "i" 'w3m-toggle-inline-image)
  ;; Vimperator controls since they're stuck in my fingers
  (define-key w3m-mode-map "d" 'w3m-delete-buffer)
  (define-key w3m-mode-map "H" 'w3m-view-previous-page)
  (define-key w3m-mode-map "L" 'w3m-view-next-page)
  (define-key w3m-mode-map "y" 'w3m-print-this-url)
  (define-key w3m-mode-map "o" 'w3m-goto-url)
  (define-key w3m-mode-map "t" 'w3m-goto-url-new-session)
  ;; (define-key w3m-mode-map "\M-n" 'w3m-next-anchor)
  ;; (define-key w3m-mode-map "\M-p" 'w3m-previous-anchor)
  (define-key w3m-mode-map "t" 'w3m-goto-url-new-session)
  (define-key w3m-mode-map "f" 'w3m-view-this-url-new-session)
  (define-key w3m-mode-map "F" 'w3m-view-this-url)
  (define-key w3m-mode-map "." 'w3m-next-buffer)
  (define-key w3m-mode-map "," 'w3m-previous-buffer))

(eval-after-load "w3m"
  '(progn
     (joe/w3m-setup-keymap)))


;; ERC
(remove-hook 'erc-echo-notice-always-hook 'erc-echo-notice-in-default-buffer)
