;;; functions.el --- Personal functions for Emacs. -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(eval-when-compile
 (require 'cl-lib))

(defun kill-region-or-backward-kill-word ()
  "`kill-region' if the mark is active, else `backward-kill-word'."
  (interactive)
  (if mark-active
      (call-interactively 'kill-region)
    (call-interactively 'backward-kill-word)))

(defadvice yank (after indent-region activate)
  "Indent region after it is yanked."
  (if (member major-mode '(emacs-lisp-mode scheme-mode lisp-mode
					   c-mode c++-mode objc-mode ada-mode
					   latex-mode plain-tex-mode))
      (indent-region (region-beginning) (region-end) nil)))

(defadvice yank-pop (after indent-region activate)
  "Auto indent yanked code."
  (if (member major-mode '(emacs-lisp-mode scheme-mode lisp-mode
					   c-mode c++-mode objc-mode ada-mode
					   latex-mode plain-tex-mode))
      (indent-region (region-beginning) (region-end) nil)))

(defun my:save-buffer (&optional args)
  "If trying to save the *scratch* buffer, do nothing."
  (interactive)
  (unless (eq (current-buffer) (get-buffer "*scratch*"))
    (save-buffer args)))

(defun my:helm-find-files ()
  (interactive)
  (funcall 'helm-find-files nil))

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn (rename-file name new-name 1) (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil))))))

(defun comment-or-uncomment-line ()
  "Either comments the current line or uncomments it."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position)
                               (line-end-position)))

(defun unfill-paragraph ()
  "Replace newline characters with a single space."
  (interactive)
  (let ((fill-column 90002000))
    (if mark-active
        (fill-region (region-beginning) (region-end))
      (fill-paragraph))))

(defun toggle-line-spacing ()
  "Toggle line spacing between 1 and 5 pixels."
  (interactive)
  (if (not (eq line-spacing nil))
      (setq-default line-spacing nil)
    (setq-default line-spacing 7)))

(defvar my:dark-theme 'solarized-dark)
(defvar my:light-theme 'solarized-light)
(defvar my:current-theme my:light-theme)


(defvar my:load-theme-hook nil
  "Hook to run when a new theme is loaded.")

(defadvice load-theme (after load-theme-hook activate)
  "Add hook to `load-theme'."
  (run-hooks 'my:load-theme-hook))

(defun my:create-subtle-show-paren-match ()
  (interactive)
  (set-face-attribute 'show-paren-match nil
                      :foreground nil
                      :weight 'normal
                      :background (my:differentiate-color (face-background 'default) 2)))

(after 'paren
  (add-hook 'my:load-theme-hook 'my:create-subtle-show-paren-match)
  (my:create-subtle-show-paren-match))

(defun toggle-color-theme ()
  "Switch between the `my:dark-theme' and `my:light-theme'."
  (interactive)
  (disable-theme my:current-theme)
  (if (eq my:current-theme my:dark-theme)
      (setq my:current-theme my:light-theme)
    (setq my:current-theme my:dark-theme))
  (load-theme my:current-theme t))

(defun my:indent-defun-around-point ()
  "Indent the sexp that we're currently in."
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (indent-sexp)))

(defun my:yank-sexp ()
  "Yank the sexp in front of the point."
  (interactive)
  (save-excursion
    (mark-sexp)
    (kill-ring-save (point) (mark))))

(defun info-mode ()
  "A simple function to open standalone info files correctly."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (kill-buffer (current-buffer))
    (info file-name)))
(add-to-list 'auto-mode-alist '("\\.info\\'" . info-mode))

(defun my:back-to-indentation-or-beginning ()
  "Go back to indentation, or beginning of line on second press."
  (interactive)
  (if (eq last-command 'my:back-to-indentation-or-beginning)
      (evil-beginning-of-line)
    (evil-first-non-blank)))

(defun my:last-non-blank-or-end-of-line ()
  "Go to last non blank, or end of line on second press."
  (interactive)
  (if (eq last-command 'my:last-non-blank-or-end-of-line)
      (evil-end-of-line)
    (evil-last-non-blank)))

(defun my:hungry-delete-backward (n &optional killflag)
  "Delete non-vertical whitespace backwards on first key press.
Delete all whitespace on a succesive key press."
  (interactive "p\nP")
  (if (eq last-command 'my:hungry-delete-backward)
      (hungry-delete-backward n killflag)
    (let ((hungry-delete-chars-to-skip " \t\f\v"))
      (hungry-delete-backward n killflag))))

(defun my:maybe-byte-compile ()
  "Byte compile current file if .elc file exists."
  (interactive)
  (when (file-exists-p (byte-compile-dest-file buffer-file-name))
    (byte-compile-file buffer-file-name)))

(defun my:maybe-byte-compile-after-save ()
  "Add `my:maybe-byte-compile' to the local `after-save-hook'."
  (add-hook (make-local-variable 'after-save-hook) 'my:maybe-byte-compile))

(defun my:toggle-compile-on-save ()
  "Add `compile' to the local `after-save-hook'."
  (interactive)
  (if (memq 'recompile after-save-hook)
      (progn
        (remove-hook 'after-save-hook 'recompile 'local)
        (message "%s will NOT compile when saved" (buffer-name)))
    (add-hook 'after-save-hook 'recompile nil 'local)
    (message "%s will compile when saved" (buffer-name))))

(defun my:delete-trailing-whitespace-except-current-line ()
  "Do `delete-trailing-whitespace', except for current line."
  (interactive)
  (let ((current-line (buffer-substring (line-beginning-position) (line-end-position)))
        (backward (- (line-end-position) (point))))
    (delete-trailing-whitespace)
    (when (not (string-equal (buffer-substring (line-beginning-position) (line-end-position))
                             current-line))
      (delete-region (line-beginning-position) (line-end-position))
      (insert current-line)
      (backward-char backward))))

(defun my:delete-trailing-whitespace-before-save ()
  "Add `delete-trailing-whitespace' to the local `after-save-hook'."
  (add-hook (make-local-variable 'before-save-hook)
            'my:delete-trailing-whitespace-except-current-line))

(defun my:highlight-long-lines ()
  "Highlight long lines."
  (interactive)
  (eval-when-compile
    (require 'whitespace))
  (set (make-local-variable 'whitespace-style) '(face lines-tail))
  (set (make-local-variable 'whitespace-line-column) (max 80 fill-column))
  (whitespace-mode 1))

(defun my:new-blah-buffer ()
  "Open up a guaranteed new blah (scratch) buffer."
  (interactive)
  (switch-to-buffer (cl-loop for num from 0
                          for name = (format "blah-%03i" num)
                          while (get-buffer name)
                          finally return name)))

(defun my:switch-to-blah-buffer ()
  "Switch to a blah buffer, or create a new one."
  (interactive)
  (cl-loop for buffer in (buffer-list)
      if (string-match "blah-.+" (buffer-name buffer))
         return (switch-to-buffer buffer)
      finally do (my:new-blah-buffer)))

(defun my:evil-setup ()
  "The initial customization for evil mode.

This is separated into a function so I can edit it without
figuring out how to reload the package."
  (interactive)
  (eval-when-compile (require 'evil-vars))
  (evil-mode 1)

  ;; We need to add text before we can edit it.
  (add-to-list 'evil-insert-state-modes 'git-commit-mode)

  (setq-default evil-symbol-word-search t)
  (setq evil-highlight-closing-paren-at-point-states nil)

  (setq evil-emacs-state-cursor '("#00FF48" box))
  (setq evil-normal-state-cursor '("#0971B2" box))
  ;; (setq evil-visual-state-cursor '("orange" box))
  (setq evil-insert-state-cursor '("#AD5E5E" bar))
  (setq evil-replace-state-cursor '("#AD5E5E" hollow))
  (setq evil-operator-state-cursor '(hollow))

  (setq evil-want-visual-char-semi-exclusive t)
  (setq evil-move-cursor-back nil)

  (defmacro my:make-evil-line-move-motion (name multiplier)
    `(evil-define-motion ,name (count)
       ,(format "Move the cursor (COUNT * %s) lines down." multiplier)
       :type line
       (let (line-move-visual)
         (evil-next-visual-line (* ,multiplier (or count 1))))))

  (my:make-evil-line-move-motion my:evil-next-visual-line-5 5)
  (my:make-evil-line-move-motion my:evil-previous-visual-line-5 -5)
  (my:make-evil-line-move-motion my:evil-next-visual-line-3 3)
  (my:make-evil-line-move-motion my:evil-previous-visual-line-3 -3)

  ;; Make movement keys work on visual lines instead of acutal lines.
  ;; This imitates Emacs behavior rather than Vim behavior.
  (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>")
    'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>")
    'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>")
    'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>")
    'evil-previous-visual-line)

  (defun my:evil-stop-insert-one-char ()
    (unless (eq this-command #'my:evil-insert-one-char)
      (remove-hook 'post-command-hook 'my:evil-stop-insert-one-char)
      (evil-normal-state)))

  (evil-define-command my:evil-insert-one-char ()
    "Insert one character and return to normal mode."
    (add-hook 'post-command-hook #'my:evil-stop-insert-one-char t)
    (evil-insert 1))

  (define-key evil-normal-state-map (kbd "C-SPC") 'my:evil-insert-one-char)

  ;; Make horizontal movement cross lines
  (setq-default evil-cross-lines t)

  ;; Commands for both the normal, motion and visual state
  (cl-loop for (key . func) in
        `(("J" . my:evil-next-visual-line-5)
          ("K" . my:evil-previous-visual-line-5)
          ("\M-j" . my:evil-next-visual-line-3)
          ("\M-k" . my:evil-previous-visual-line-3)
          ("gj" . evil-join)
          ("H" . my:back-to-indentation-or-beginning)
          ("L" . evil-end-of-line)
          ("\C-j" . scroll-up-command)
          ("\C-k" . scroll-down-command))
        do
        (define-key evil-normal-state-map key func)
        (define-key evil-visual-state-map key func)
        (define-key evil-motion-state-map key func))

  (define-key evil-insert-state-map (kbd "C-<return>") 'evil-open-below)

  (defun my:evil-stop-insert-one-char ()
    (unless (eq this-command #'my:evil-insert-one-char)
      (remove-hook 'post-command-hook 'my:evil-stop-insert-one-char)
      (evil-normal-state)))

   (evil-define-command my:evil-insert-one-char ()
     "Insert one character in insert mode."
     (add-hook 'post-command-hook #'my:evil-stop-insert-one-char t)
     (evil-insert 1))

   (define-key evil-normal-state-map (kbd "C-SPC") 'my:evil-insert-one-char)

  ;; Commands for only the normal state map
  (cl-loop for (key . func) in
        `((,(kbd "<tab>")  . indent-for-tab-command)
          ([backspace] . my:hungry-delete-backward)
          ("z," . comment-dwim)
          ("zn" . evil-toggle-fold)
          ("z;" . comment-or-uncomment-line))
        do
        (define-key evil-normal-state-map key func))

  ;; Command for `evil-insert-mode-map'
  (cl-loop for (key . func) in
        `(([backspace] . my:hungry-delete-backward))
        do
        (define-key evil-insert-state-map key func))

  ;; Leave python-mode alone because it does useful indentation stuff.
  (evil-define-key 'normal python-mode-map [backspace] 'python-indent-dedent-line-backspace)
  (evil-define-key 'insert python-mode-map [backspace] 'python-indent-dedent-line-backspace)

  (add-hook 'Info-mode-hook
            (lambda () (cl-loop for (key . func) in
                        '(("H" . Info-history-back)
                          ("L" . Info-history-forward))
                        do (define-key Info-mode-map key func)))))

(defun my:local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(defun my:pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(defun my:add-hooks (mode functions)
  "Call `add-hook' for each function in FUNCTIONS into MODE."
  (cl-loop for hook in functions
        do
        (add-hook mode hook)))

(defun not-in-minibuffer (fn else-fn)
  "Execute FN normally, but in the minibuffer, execute ELSE-FN.
If ELSE-FN is a string, insert string."
  (let ((name (intern (concat "my:maybe-ignore-keychord-"
                              (symbol-name fn)))))
    (eval
     `(defun ,name ()
        (interactive)
        (if (or (window-minibuffer-p)
                (and (boundp 'evil-state)
                     (eq evil-state 'insert)))
            (if (stringp ,else-fn)
                (insert ,else-fn)
              (funcall ',else-fn))
          (funcall ',fn))))
    name))

(defun my:add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
          1 font-lock-warning-face t))))

(defun my:open-dired-here ()
  (interactive)
  (dired default-directory))

(after 'evil
  (define-key evil-operator-state-map "j" 'my:evil-operator-state-j)
  (evil-define-command my:evil-operator-state-j ()
    (save-excursion
      (let ((evt (read-event "Press k to exit operator state" nil 0.08)))
        (if (and (integerp evt) (char-equal evt ?k))
            (keyboard-quit)
          ;; assume <down> is bound to the same as j:
          ;; get the keys used to invoke the operator
          (let* ((operator-string (substring (this-command-keys) 0 -1))
                 ;; add " <down>" to the end instead of "j"
                 (new-macro (kbd (concat operator-string " <down>"))))
            (evil-force-normal-state)
            (execute-kbd-macro new-macro)
            (when (not (null evt))
              ;; process any other key pressed within 0.5 seconds
              (push evt unread-command-events))))))))

(defun my:esc ()
  "Functionality for escaping generally."
  (interactive)

  (cond
   ;; If we're in one of the Evil states return to the normal-state
   ((or (evil-insert-state-p) (evil-normal-state-p) (evil-replace-state-p)
        (evil-operator-state-p) (evil-visual-state-p))
    (evil-force-normal-state))

   ((window-minibuffer-p)
    (abort-recursive-edit))

   ((string-prefix-p "*magit-key" (buffer-name))
    (magit-key-mode-command nil))

   (t (keyboard-quit))))

;; Exit isearch by pressing jk, see
;; http://stackoverflow.com/questions/20926215
(defun my:isearch-exit-chord-worker (&optional arg)
  (interactive "p")
  arg ;; make the byte compilation errors go away
  ;; delete the j or k and accept the search
  (execute-kbd-macro (kbd "<backspace> <return>")))

(defun my:isearch-exit-chord (arg)
  (interactive "p")
  (isearch-printing-char)
  arg ;; make the byte compilation errors go away
  (eval-when-compile
    (require 'smartrep))
  ;; Manually signal quit because using `keyboard-quit' displays
  ;; "quit" in the echo-area, hiding the search text if you press 'j'
  ;; and another character besides 'k' in rapid succession.
  (run-at-time 0.3 nil '(lambda () (signal 'quit)))
  (condition-case nil
    (smartrep-read-event-loop
      '(("j" . my:isearch-exit-chord-worker)
        ("k" . my:isearch-exit-chord-worker)))
    (quit nil)))
(define-key isearch-mode-map "j" 'my:isearch-exit-chord)
(define-key isearch-mode-map "k" 'my:isearch-exit-chord)

(defun my:evil-define-keys (states keymaps key def &rest bindings)
  "Run `evil-define-key' over all STATES and KEYMAPS."
  (let* ( ;; Keep bindings unmodified to use for all iterations
         (bindings (append (list key def) bindings))
         ;; Destructively modify k, d and bs.
         bs k d)
    (dolist (state states)
      (dolist (keymap keymaps)
        (setq bs bindings)
        (while bs
          (setq k (pop bs)
                d (pop bs))
          (evil-define-key state keymap k d)
          (message "defined state %s in %s of %s:%s" state keymap k d))))))

(defun my:magit-verbose-commit (&optional amend)
  (interactive "P")
  (require 'magit)
  (let ((magit-custom-options (add-to-list 'magit-custom-options "--verbose")))
    (magit-commit amend)))

(defun my:add-citations-to-sentence-end ()
  "Add support for pandoc citations to `sentence-end'.
e.g. 'This is a sentence. [@euler, 2]' The main point is now
`fill-paragraph' will respect the two spaces after the closing
citation and not collapse it to one space."
  (interactive)
  (setq-local sentence-end-base
            (concat
             ;; don't use the local value because we'll keep adding
             ;; the citation regexp over and over again.
             (default-value 'sentence-end-base)
             ;; citation regexp,
             ;; [@euler, 1] good
             ;; TODO
             ;; [...] bad
             "\\( \\[@.*?\\]\\)*")))

(defun my:toggle-identifier-naming-style ()
  "Toggles the symbol at point between C-style naming,
e.g. `hello_world_string', and camel case,
e.g. `HelloWorldString'."
  (interactive)
  (let* ((symbol-pos (bounds-of-thing-at-point 'symbol))
         case-fold-search cstyle regexp func)
    (unless symbol-pos
      (error "No symbol at point"))
    (save-excursion
      (narrow-to-region (car symbol-pos) (cdr symbol-pos))
      (setq cstyle (string-match-p "_" (buffer-string))
            regexp (if cstyle "\\(?:\\_<\\|_\\)\\(\\w\\)" "\\([A-Z]\\)")
            func (if cstyle
                     'capitalize
                   (lambda (s)
                     (concat (if (= (match-beginning 1)
                                    (car symbol-pos))
                                 ""
                               "_")
                             (downcase s)))))
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (replace-match (funcall func (match-string 1))
                       t nil))
      (widen))))


(global-set-key "\C-w" 'kill-region-or-backward-kill-word)


(defun my:insert-date (&optional date-format)
  (interactive "P")
  (if date-format
      (insert (format-time-string date-format))
    (insert (format-time-string "%-e %B %Y"))))

(defmacro new-date-defun (name date-format)
  "Create a new interactive date function.
NAME is the created defun name.
DATE-FORMAT is the format string for `format-time-string.'"
  `(defun ,name ()
     ,(format "Insert the date formated like %s."
              (format-time-string date-format
                                  (date-to-time "1974-06-06 01:30:00 UTC")))
     (interactive)
     (my:insert-date ,date-format)))

(new-date-defun my:insert-date-civilian "%B %-e, %Y")
(new-date-defun my:insert-date-american "%D")
(new-date-defun my:insert-date-iso "%Y-%m-%d")
(new-date-defun my:insert-date-military-short "%d %^b %y")
(new-date-defun my:insert-date-military-long "%d %B %y")

(new-date-defun my:insert-date-time-civilian "%B %-e, %Y")
(new-date-defun my:insert-date-time-american "%D")
(new-date-defun my:insert-date-time-iso "%Y-%m-%dT%T%z")
(new-date-defun my:insert-date-time-military "%d%H%M%^b%y")

(defun my:pandoc-reftex-cite ()
  (interactive)

  (require 'reftex)
  (let* ((cite-key-raw (format "%s" (reftex-citation 'no-insert)))
         (cite-key (substring cite-key-raw 1 (- (length cite-key-raw) 1))))

    (insert (format "[@%s]" cite-key)))

  (backward-char)
  (insert ", ")
  (when (boundp 'evil-mode)
    (evil-insert-state 1)))

(defun my:differentiate-color (name lighten-percent &optional darken-percent)
  "Differentiate color NAME by PERCENT.
If color is closer to white, darken by PERCENT.  If color is
closer to black, lighten by PERCENT"
  (require 'color)
  (let ((white-distance (color-distance name "white"))
        (black-distance  (color-distance name "black"))
        ;; Percuptually, it takes more darkening to get the same
        ;; effect so add a little extra oompf.
        (darken-percent (or darken-percent (+ 2 lighten-percent))))
    (if (> white-distance black-distance)
        (color-lighten-name name lighten-percent)
      (color-darken-name name darken-percent))))

(defun my:python-add-format-to-string ()
  "Add .format to function to current string.
If not in a string, do nothing."
  (interactive)
  (let ((string-start-point (nth 8 (syntax-ppss))))
    (if string-start-point
        (progn
          (goto-char string-start-point)
          (forward-sexp 1)
          (insert ".format()")
          (backward-char 1))
      (message "Not inside string.  Unable to add '.format()'"))))

(defun my:comment-newline-dwim ()
  "Break line at point and indent, continuing comment if within one.
On the first press: acts as `comment-indent-new-line'.
On the second press: acts like `comment-indent-new-line' but with a space.
On the third press: deletes added comment lines and inserts a newline."
   (interactive)
   ;; Saving this for later, there's too  much  interplay between
   ;; different commands.
   )

(provide 'functions)

;;; functions.el ends here
