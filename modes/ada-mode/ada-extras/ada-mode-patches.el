;;; patches and additions to ada-mode
;;; (I don't use ada-stmt or ada-prj)

;;; first new stuff

;; support for ada-calc-record-rep
(defconst ada-record-rep-regexp
  "\\s-*\\w*\\s-+at\\(\\s-+[0-9]+\\)\\s-+\\(\\* Words\\s-+\\)?range\\s-+\\([0-9]+\\)\\s-+\\.\\.\\s-+\\([0-9]+\\)\\s-*;"
  "Regexp to match a record representation line, and extract the 'at'
and 'range' values. First group is 'at', third group is 'range first',
fourth group is 'range last'.")

(defvar ada-word-size nil
  "*Bits in a word, specifically for record representation clauses.")

(defun ada-calc-record-rep ()
  "If point is in a record representation clause, calculate the 'at'
value for the current line, assuming the values on the previous line
are correct. Compute bit range from SIZE. Uses ada-word-size for
bit ranges. Move to next line."
  (interactive)
  (if (not ada-word-size)
      (setq ada-word-size
            (string-to-int (read-from-minibuffer "ada-word-size : "))))
  (save-excursion
    (beginning-of-line)
    (if (not (looking-at ada-record-rep-regexp))
        (error "not in a record representation line")
      (let ((edit-match-data (match-data)))
        (forward-line -1)
        (if (not (looking-at ada-record-rep-regexp))
            (error "can't calculate first record representation line")
          (let ((at-value (string-to-number (match-string 1)))
                (range-first (string-to-number (match-string 3)))
                (range-last (string-to-number (match-string 4))))
            (store-match-data edit-match-data)
            (goto-char (match-beginning 1))

            (let ((size (+ 1 (- (string-to-number (match-string 4)) (string-to-number (match-string 3))))))
              (setq size (string-to-number (read-from-minibuffer "Bit Size: " (number-to-string size))))

              (delete-region (match-beginning 1) (match-end 4))
              (insert " ")
              (if (= 0 (mod (+ 1 range-last) ada-word-size))
                  ;; starts on word boundary; increment 'at', start new bit range
                  (progn
                    (insert
                     (number-to-string (+ at-value (/ (+ 1 range-last) ada-word-size)))
                     (if (not (= 8 ada-word-size))
                         " * Words"
                       "")
                     " range 0 .. "
                     (number-to-string (- size 1))))
                ;; starts on partial word boundary; use same 'at', continue bit range
                (insert
                 (number-to-string at-value)
                 (if (not (= 8 ada-word-size))
                     " * Words")
                 " range "
                 (number-to-string (+ 1 range-last))
                 " .. "
                 (number-to-string (+ range-last size))) )
              ))))))
  (next-line 1))

(defun ada-comment-box-header ()
  "Create a box comment header for the subprogram name at point."
  (interactive "*")
  (save-excursion
    (let ((subprogram-name (ffap-string-at-point)))
      (beginning-of-line)
      (indent-according-to-mode)
      (insert (make-string (+ 8 (length subprogram-name)) ?-))
      (newline-and-indent)
      (insert (concat "--  " subprogram-name "  --"))
      (newline-and-indent)
      (insert (make-string (+ 8 (length subprogram-name)) ?-))
      (newline)
      (newline-and-indent)
      )))

(defun ada-invert-line-order ()
  "Invert line order in region. Useful for editing record representation clauses."
  (interactive)
  ;; Move lines from end of region to after region
  (if (not mark-active)
      (error "must select lines to invert"))

  (let (line
        (source-begin (copy-marker (region-beginning)))
        (source-end   (copy-marker (region-end)))
        (insert-mark (make-marker)))

    (goto-char source-end)
    (forward-char 1) ; beginning of next line
    (set-marker insert-mark (point))

    (goto-char source-end)
    (while (> source-end source-begin)
      (beginning-of-line)
      (forward-char -1) ; include new-line in moved line
      (setq line (buffer-substring (point) source-end))
      (delete-region (point) source-end)
      (goto-char insert-mark)
      (insert-before-markers line)
      (goto-char source-end))))

(defun ada-align ()
  "If region is active, apply 'align'. If not, attempt to align
current construct."
  (interactive)
  (if (and mark-active ada-align-modes)
      (progn
        (align (region-beginning) (region-end) 1 ada-align-modes)
        (deactivate-mark))
    ;; else see if we are in a construct we know how to align
    (cond
     ((ada-in-paramlist-p)
      ;; we might actually be in a discriminant-list
      (let ((in-discrim (save-excursion
                          (search-backward "(")
                          (backward-word 2)
                          (looking-at "type"))))
        (if in-discrim
            (align-current)
          (ada-format-paramlist))))

     (t
      (align-current))
     )))

(defun ada-goto-declaration-at-point (other-window)
  "Goto declaration of entity at point. If OTHER-WINDOW or with arg,
show in other window"
  (interactive "P")
  (ada-require-project-file)
  (push-mark (point))
  (ada-xref-push-pos (buffer-file-name) (point))
  (let ((ada-xref-other-buffer other-window) ; tell ada-find-in-ali to use other window
        (identlist (ada-read-identifier (point))))
    (ada-find-in-ali identlist nil)))

;;; Additional find-file support

;; new constant
(defconst ada-name-regexp
  "\\([a-zA-Z][a-zA-Z0-9_\\.\\']*[a-zA-Z0-9]\\)"
  "Regexp for extracting a fully qualified name (including attribute)")

;; new constant
(defconst ada-parent-name-regexp
  "\\([a-zA-Z0-9_\\.]+\\)\\.[a-zA-Z0-9_]+"
  "regexp for extracting the parent name from fully-qualified name")

(defun ada-find-file-setup ()
  "Set up find-file stuff for ada-mode. For use in ada-mode-hook."
  (make-local-hook 'ff-pre-find-hooks)
  (add-hook 'ff-pre-find-hooks 'ada-ff-package-name nil t)
  (add-hook 'ff-pre-find-hooks 'ada-require-project-file nil t)

  ;; set ff-function-name, for later use by ff-set-point-accordingly
  ;; set fname and suffixes so we can search for .ads or .ads.gp
  ;; first make it buffer local, and throw away anything anyone else has set
  (set (make-local-variable 'ff-special-constructs) nil)
  (add-to-list 'ff-special-constructs
               ;; with'ed package
               (cons (eval-when-compile (concat "^with[ \t]+" ada-name-regexp))
                     (lambda ()
                       (setq ff-function-name (match-string 1))
                       (setq suffixes (list ".ads" ".ads.gp"))
                       (setq fname (ada-make-filename-from-adaname ff-function-name))
                       )))

  (add-to-list 'ff-special-constructs
               ;; parent of child package.
               (cons (eval-when-compile (concat "^\\(private[ \t]+\\)?package[ \t]+" ada-parent-name-regexp " is"))
                     (lambda ()
                       (setq ff-function-name (match-string 2))
                       (setq suffixes (list ".ads" ".ads.gp"))
                       (setq fname (ada-make-filename-from-adaname ff-function-name))
                       )))

  (add-to-list 'ff-special-constructs
               ;; parent of child subprogram (thanks to John McCabe)
               (cons (eval-when-compile
                       (concat "^\\(procedure\\|function\\)[ \t\n]+"
                               ada-parent-name-regexp
                               "\\(\\([ \t\n]*\\((\\|;\\)\\)\\|[ \t\n]+\\(is\\|return\\)\\)"))
                     (lambda ()
                       (setq ff-function-name (match-string 2))
                       (setq suffixes (list ".ads" ".ads.gp"))
                       (setq fname (ada-make-filename-from-adaname ff-function-name))
                       )))

  (make-local-hook 'ff-file-created-hooks)
  (remove-hook 'ff-file-created-hooks 'ada-make-body) ; from global hook
  (remove-hook 'ff-file-created-hooks 'ada-make-body t) ; from local hook
  (add-hook 'ff-file-created-hooks 'ada-make-body-gnatstub nil t)

  ;; Should check if gnatkr is present first
  (setq ada-make-filename-from-adaname 'ada-make-filename-from-adaname-gnatkr)
  )

(defun ada-ff-package-name ()
  "Set 'fname' to filename for package name in current region (if
active), and 'suffixes' to a list of possible suffixes. Used in
ff-pre-find-hooks."
  (interactive)
  (if mark-active
      (progn
        (setq ff-function-name (buffer-substring-no-properties (point) (mark)))
        ;; don't concat ada-spec-suffix, so we can search for .ads or .ads.gp
        (setq fname (ada-make-filename-from-adaname ff-function-name))
        (setq suffixes (list ".ads" ".ads.gp" ".adb" ".adb.gp")))))

;;; now ada-xref patches

;; from ada-xref.el
;; handle pragma Source_Reference
(defun ada-find-file-number-in-ali (file)
  "Returns the file number for FILE in the associated ali file."
  (set-buffer (ada-get-ali-buffer file))
  (goto-char (point-min))

  (let ((begin (re-search-forward "^D"))
        (filename (file-name-nondirectory file)))
    (beginning-of-line)
    (re-search-forward (concat "^D " filename "\\|1:" filename))
    (count-lines begin (point))))


;; from ada-xref.el
;; Allow env vars, relative paths in src_dir, obj_dir, casing in project file.
;; Expand file names while in the project file, so paths are relative to the project file.
;; Allow comments.
(defun ada-parse-prj-file (prj-file)
  "Reads and parses the PRJ-FILE file if it was found.
The current buffer should be the ada-file buffer."
  (if prj-file
      (let (project src_dir obj_dir make_cmd comp_cmd check_cmd casing
                    run_cmd debug_pre_cmd debug_post_cmd
                    (ada-buffer (current-buffer)))
        (setq prj-file (expand-file-name prj-file))

        ;;  Set the project file as the active one, so
        ;;  ada-xref-get-project-field works (called by ada-xref-get-src-dir-field)
        (setq ada-prj-default-project-file prj-file)

        ;;  Initialize the project with the default values
        (ada-xref-set-default-prj-values 'project (current-buffer))

        ;;  Do not use find-file below, since we don't want to show this
        ;;  buffer. If the file is open through speedbar, we can't use
        ;;  find-file anyway, since the speedbar frame is special and does not
        ;;  allow the selection of a file in it.

        (if (file-exists-p prj-file)
            (progn
              (let* ((buffer (run-hook-with-args-until-success
                              'ada-load-project-hook prj-file)))
                (unless buffer
                  (setq buffer (find-file-noselect prj-file nil)))
                (set-buffer buffer))

              (widen)
              (goto-char (point-min))

              ;;  Now overrides these values with the project file
              ;;  Don't cross line endings; this allows comments.
              (while (not (eobp))
                (if (looking-at "^\\([^=\n]+\\)=\\(.*\\)")
                    (cond
                     ((string= (match-string 1) "src_dir")
                      (add-to-list 'src_dir
                                   (file-name-as-directory
                                    (expand-file-name (substitute-in-file-name (match-string 2))))))
                     ((string= (match-string 1) "obj_dir")
                      (add-to-list 'obj_dir
                                   (file-name-as-directory
                                    (expand-file-name (substitute-in-file-name (match-string 2))))))
                     ((string= (match-string 1) "casing")
                      (set 'casing (cons (expand-file-name (substitute-in-file-name (match-string 2))) casing)))
                     ((string= (match-string 1) "build_dir")
                      (set 'project
                           (plist-put project 'build_dir
                                      (file-name-as-directory (match-string 2)))))
                     ((string= (match-string 1) "make_cmd")
                      (add-to-list 'make_cmd (match-string 2)))
                     ((string= (match-string 1) "comp_cmd")
                      (add-to-list 'comp_cmd (match-string 2)))
                     ((string= (match-string 1) "check_cmd")
                      (add-to-list 'check_cmd (match-string 2)))
                     ((string= (match-string 1) "run_cmd")
                      (add-to-list 'run_cmd (match-string 2)))
                     ((string= (match-string 1) "debug_pre_cmd")
                      (add-to-list 'debug_pre_cmd (match-string 2)))
                     ((string= (match-string 1) "debug_post_cmd")
                      (add-to-list 'debug_post_cmd (match-string 2)))
                     (t
                      (set 'project (plist-put project (intern (match-string 1))
                                               (match-string 2))))))
                (forward-line 1))

              (if src_dir (set 'project (plist-put project 'src_dir
                                                   (reverse src_dir))))
              (if obj_dir (set 'project (plist-put project 'obj_dir
                                                   (reverse obj_dir))))
              (if casing  (set 'project (plist-put project 'casing
                                                   (reverse casing))))
              (if make_cmd (set 'project (plist-put project 'make_cmd
                                                    (reverse make_cmd))))
              (if comp_cmd (set 'project (plist-put project 'comp_cmd
                                                    (reverse comp_cmd))))
              (if check_cmd (set 'project (plist-put project 'check_cmd
                                                     (reverse check_cmd))))
              (if run_cmd (set 'project (plist-put project 'run_cmd
                                                   (reverse run_cmd))))
              (if debug_post_cmd (set 'project (plist-put project 'debug_post_cmd
                                                          (reverse debug_post_cmd))))
              (if debug_pre_cmd (set 'project (plist-put project 'debug_pre_cmd
                                                         (reverse debug_pre_cmd))))

              ;; Kill the project buffer
              (kill-buffer nil)
              (set-buffer ada-buffer)
              )

          ;;  Else the file wasn't readable (probably the default project).
          ;;  We initialize it with the current environment variables.
          ;;  We need to add the startup directory in front so that
          ;;  files locally redefined are properly found. We cannot
          ;;  add ".", which varies too much depending on what the
          ;;  current buffer is.
          (set 'project
               (plist-put project 'src_dir
                          (append
                           (list command-line-default-directory)
                           (split-string (or (getenv "ADA_INCLUDE_PATH") "") ":")
                           (list "." default-directory))))
          (set 'project
               (plist-put project 'obj_dir
                          (append
                           (list command-line-default-directory)
                           (split-string (or (getenv "ADA_OBJECTS_PATH") "") ":")
                           (list "." default-directory))))
          )


        ;;  Delete the default project file from the list, if it is there.
        ;;  Note that in that case, this default project is the only one in
        ;;  the list
        (if (assoc nil ada-xref-project-files)
            (setq ada-xref-project-files nil))

        ;;  Memorize the newly read project file
        (if (assoc prj-file ada-xref-project-files)
            (setcdr (assoc prj-file ada-xref-project-files) project)
          (add-to-list 'ada-xref-project-files (cons prj-file project)))

        ;; Sets up the compilation-search-path so that Emacs is able to
        ;; go to the source of the errors in a compilation buffer
        (setq compilation-search-path (ada-xref-get-src-dir-field))

        ;; Set the casing exceptions file list
        (if casing
            (progn
              (setq ada-case-exception-file (reverse casing))
              (ada-case-read-exceptions)))

        ;; Add the directories to the search path for ff-find-other-file, in the ada buffer.
        ;; Do not add the '/' or '\' at the end
        (setq ada-search-directories-internal
              (append (mapcar 'directory-file-name compilation-search-path)
                      ada-search-directories))

        (ada-xref-update-project-menu)
        )

    ;;  No prj file ? => Setup default values
    ;;  Note that nil means that all compilation modes will first look in the
    ;;  current directory, and only then in the current file's directory. This
    ;;  current file is assumed at this point to be in the common source
    ;;  directory.
    (setq compilation-search-path (list nil default-directory))
    ))

;; Renamed from ada-xref.el ada-make-filename-from-adaname, which
;; redefines ada-mode.el ada-make-filename-from-adaname. But there are
;; other things in ada-xref that are useful even when gnat is not
;; installed! Should move those to ada-mode.el.

(defun ada-make-filename-from-adaname-gnatkr (adaname)
  "Determine the filename in which ADANAME is found.
This is a GNAT specific function that uses gnatkr."
  (let (krunch-buf
        adaname-temp)
    (setq krunch-buf (generate-new-buffer "*gkrunch*"))
    (save-excursion
      (set-buffer krunch-buf)
      ;; for GNAT 5.01 and after, gnatkr preserves the file extension, so we
      ;; add a bogus extension.
      (setq adaname-temp (concat adaname "."))

      (call-process "gnatkr" nil krunch-buf nil adaname-temp ada-krunch-args)
      ;; fetch output of that process
      (setq adaname (buffer-substring
                     (point-min)
                     (progn
                       (goto-char (point-min))
                       (end-of-line)
                       (forward-char -1) ; strip bogus extension
                       (point))))
      (kill-buffer krunch-buf)))
  adaname
  )

;;;;;;;;;;;;; ada-mode.el patches

;; set Else placeholders, gnatprep preprocessor lines to have comment syntax
(defconst ada-font-lock-syntactic-keywords
  ;; Mark single quotes as having string quote syntax in 'c' instances.
  ;; As a special case, ''' will not be highlighted, but if we do not
  ;; set this special case, then the rest of the buffer is highlighted as
  ;; a string
  ;; This sets the properties of the characters, so that ada-in-string-p
  ;; correctly handles '"' too...
  '(("[^a-zA-Z0-9)]\\('\\)[^'\n]\\('\\)" (1 (7 . ?')) (2 (7 . ?')))

    ;; this is for gnatprep preprocessor lines
    ("^[ \t]*\\(#\\(if\\|else\\|elsif\\|end\\)\\)" (1 (11 . ?\n)))

    ;; this is for Else placeholders
    ("\\([{[].*[]}]\\(\\.\\.\\.\\)?\\)" (1 (11 . ?\n)))
    ))

;; from ada-mode.el
;; goto beginning of line containing function
;; null ff-function-name for use by ff-treat-special
(defun ada-set-point-accordingly ()
  "Move to the function declaration or body specified in `ff-function-name',
previously set by `ada-which-function-are-we-in'."
  (if ff-function-name
      (progn
        (goto-char (point-min))
        (unless (ada-search-ignore-string-comment
                 (concat ff-function-name "\\b") nil)
          (goto-char (point-min)))
        (beginning-of-line)
        (setq ff-function-name nil))))

;; from ada-mode.el
;; Remember package name, not just 'package', so ff-set-point-accordingly
;; works better.
;; Also allow 'private'
(setq ada-package-start-regexp
      (concat "^[ \t]*\\(private[ \t]+\\)?\\(package\\)[ \t\n]+\\(body[ \t]*\\)?" ada-name-regexp))

;; from ada-mode.el
;; ff-function-name may have been set by ff-treat-special; don't change it
(defun ada-which-function-are-we-in ()
  "Return the name of the function whose definition/declaration point is in.
Redefines the function `ff-which-function-are-we-in'."

  ;; ff-function-name may have been set by ff-treat-special; don't reset it.
  (if (not ff-function-name)
      (save-excursion
        (end-of-line);;  make sure we get the complete name
        (if (or (re-search-backward ada-procedure-start-regexp nil t)
                (re-search-backward ada-package-start-regexp nil t))
            (set 'ff-function-name (match-string 0)))
        )))

;; renamed from ada-mode.el ada-make-filename-from-adaname
(defun ada-make-filename-from-adaname-generic (adaname)
  "Determine the filename in which ADANAME is found.
This is a generic function, independent from any compiler."
  (while (string-match "\\." adaname)
    (set 'adaname (replace-match "-" t t adaname)))
  (downcase adaname)
  )

(defcustom ada-make-filename-from-adaname 'ada-make-filename-from-adaname-generic
  "*Function to call to make a file name from an Ada name. It may be
`ada-make-filename-from-adaname-generic',
`ada-make-filename-from-adaname-gnatkr', or another compiler-specific
function."
  :type '(choice (const ada-make-filename-from-adaname-generic)
                 (const ada-make-filename-from-adaname-gnatkr))
  :group 'ada)

(defun ada-make-filename-from-adaname (adaname)
  (funcall ada-make-filename-from-adaname adaname))

;;; end of file