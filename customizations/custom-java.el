(custom-set-variables
 '(jde-compiler '(("eclipse java compiler server"
                   "/home/joe/.emacs.d/customizations/ecj.jar")))
 '(jde-enable-abbrev-mode t)
 '(jde-help-docsets '(("JDK API" "/usr/share/doc/sun-java6-jdk/html/api" nil)
                      ("User (javadoc)" "/usr/local/share/doc/java/java3d-1_5-spec")
                      ("User (javadoc)" "/usr/local/share/doc/java/jmf20-apidocs")
                      ("User (javadoc)" "/usr/local/share/doc/java/jogl-1.1.2")
                      ("User (javadoc)" "/usr/local/share/doc/java/jama")))
 '(jde-jdk-doc-url "/usr/share/doc/sun-java6-jdk/html/api/overview-summary.html")
 '(jde-jdk-registry '(("1.6.0.10" . "/usr/lib/jvm/java-6-sun-1.6.0.10")) 1)
 '(jde-jdk '("1.6.0.10"))
 '(jde-complete-function 'jde-complete-ido)
 '(jde-sourcepath '(".")))


(require 'jde-eclipse-compiler-server)
(push '("\\.java\\'" jde-ecj-server-flymake-init jde-ecj-flymake-cleanup)
      flymake-allowed-file-name-masks)

(jde-pi-load-plugins)

;; add underscores as a word boundary in jde
(modify-syntax-entry ?\_ "_" jde-mode-syntax-table)

(add-hook 'jde-mode-hook
          (lambda ()
            (auto-fill-mode 1)))

(defun joe/jde-electric-return-and-indent ()
  "Calls jde-electric return and indents according to mode."
  (interactive)
  (jde-electric-return)
  (indent-according-to-mode))

(define-key jde-mode-map "\C-m" 'joe/jde-electric-return-and-indent)
(define-key jde-mode-map "\C-j" 'joe/jde-electric-return-and-indent)

(defun jde-complete-ido ()
  "Custom method completion for JDE using ido-mode and yasnippet."
  (interactive)
  (let
      ((completion-list '())
       (start-pos (point)))
    (dolist (element (jde-complete-find-completion-for-pair
                      (jde-complete-get-pair
                       (jde-parse-java-variable-at-point)
                       nil)
                      nil))
      (add-to-list 'completion-list
                   (cdr element)))
    (if completion-list
        (let ((choice (ido-completing-read "Pick completion: " completion-list))
              (method))
          ;; Get rid of user text
          (delete-region
           (point)
           (re-search-backward "\\."
                               (line-beginning-position)))
          (insert ".")
          (insert choice)
          (search-backward ")" start-pos t)
          )
      (message "No completions at this point"))))

(define-key jde-mode-map "\M-i" 'jde-complete-ido)
(define-key jde-mode-map "\M-o" 'credmp/flymake-display-err-minibuf)


(defun credmp/flymake-display-err-minibuf () 
  "Displays the error/warning for the current line in the minibuffer"
  (interactive)
  (let* ((line-no (flymake-current-line-no))
         (line-err-info-list
          (nth 0 (flymake-find-err-info flymake-err-info line-no)))
         (count (length line-err-info-list))
         )
    (while (> count 0)
      (when line-err-info-list
        (let* ((file       (flymake-ler-file (nth (1- count) line-err-info-list)))
               (full-file  (flymake-ler-full-file (nth (1- count) line-err-info-list)))
               (text (flymake-ler-text (nth (1- count) line-err-info-list)))
               (line       (flymake-ler-line (nth (1- count) line-err-info-list))))
          (message "[%s] %s" line text)
          )
        )
      (setq count (1- count)))))


(defun jde-open-get-path-prefix-list () 
  "Builds all the path prefixes used to search for the full qualified
 source file. For this method to work `jde-sourcepath' needs to be set."
  (if jde-sourcepath

      (append (jde-expand-wildcards-and-normalize jde-sourcepath
                                                  'jde-sourcepath))
    (error (concat "For this method to work the variable "
                   "jde-sourcepath needs to be set"))))

(defun jde-expand-classpath (classpath &optional symbol)
  "If `jde-expand-classpath-p' is nonnil, replaces paths to 
 directories that match `jde-lib-directory-names' with paths to jar or
 zip files in those directories, excepting those specified by
 `jde-lib-excluded-file-names'. This function assumes that the
 existing paths are already normalized."
  (if (or jde-expand-classpath-p  jde-expand-wildcards-in-paths-p)
      (mapcan (lambda (path)
                (cond 
                 ((and jde-expand-classpath-p (file-exists-p path)
                       (file-directory-p path)
                       (let ((dir-name (file-name-nondirectory path)))
                         (member-if 
                          (lambda (lib-name)
                            (string-match lib-name dir-name))
                          jde-lib-directory-names)))
                  (append 
                   (jde-expand-directory
                    path 
                    "\\.jar$"
                    jde-lib-excluded-file-names
                    symbol)
                   (jde-expand-directory
                    path 
                    "\\.zip$"
                    jde-lib-excluded-file-names
                    symbol)))
                 (jde-expand-wildcards-in-paths-p
                  (let ((exp-paths (file-expand-wildcards path)))
                    (if exp-paths exp-paths (list path))))
                 (t (list path))))
              classpath)
    classpath))

(defcustom jde-expand-wildcards-in-paths-p t
  "Expands entries in the 'jde-global-classpath and 'jde-sourcepath
 which are wildcards patterns into a list of matching files or
 directories which are interpolated into classpath or sourcepath list.
 This expansion is done in the functions 'jde-open-get-path-prefix-list
 and 'jde-search-src-dirs for 'jde-sourcepath and in
 'jde-normalize-paths for 'jde-global-classpath."
  :group 'jde-project
  :type 'boolean)

(defun jde-expand-wildcards-and-normalize (path &optional symbol)
  "Expand any entries with wildcard patterns in path and interpolate
 them into the result"
  (if jde-expand-wildcards-in-paths-p
      (mapcan 
       (lambda (path)
         (let ((exp-paths (file-expand-wildcards path)))
           (if exp-paths exp-paths (list path))))
       (jde-normalize-paths path symbol))
    (jde-normalize-paths path symbol)
    ))



