;; @(#) ada-vms.el
;; Copyright (C) 1998, 1999 Ada Core Technologies
;; Maintainer: Stephen Leake <stephen_leake@stephe-leake.org>

;;  Adaptation of the Emacs ada-mode for VMS (Emacs 19.28)
;;  Some functions are missing in Emacs 19.28, and we need to
;;    provide some alternatives
;;  Some variables need to be adapted to VMS for a correct behavior
;;
;;  To use the ada-mode on VMS, you need to put the following commands
;;  in you .emacs :
;;      (setq auto-mode-alist
;;        (append '(("\\.ads$" . ada-mode)
;;                 ("\\.adb$" . ada-mode)) auto-mode-alist))
;;      (require 'ada-xref)
;;      (setq load-path (cons "~/" load-path))
;;      (require 'ada-vms)
   

(defmacro defgroup (&rest args) nil)
(defmacro defcustom (var value doc &rest args)
  (` (defvar (, var) (, value) (, doc))))


(load "regexp-opt")
(load "find-file")



;;  Substitute functions

(defun add-to-list (name list)
  (setq name (cons list (symbol-value name))))

(defun local-variable-p (var &optional buffer) t)
   
(defun file-name-sans-extension (filename)
  "Return FILENAME sans final \"extension\".
The extension, in a file name, is the part that follows the last `.'."
  (save-match-data
    (let ((file (file-name-sans-versions (file-name-nondirectory filename)))
          directory)
      (if (string-match "\\.[^.]*\\'" file)
          (if (setq directory (file-name-directory filename))
              (expand-file-name (substring file 0 (match-beginning 0))
                                directory)
            (substring file 0 (match-beginning 0)))
        filename))))

(defun buffer-substring-no-properties (begin end)
  (buffer-substring begin end))

(defun replace-match (newtext &optional fixedcase literal string)
  (if string
      (concat (substring string 0 (match-beginning 0))
	      newtext
	      (substring string (match-end 0)))
    (save-excursion
      (delete-region (match-beginning 0) (match-end 0))
      (goto-char (match-beginning 0))
      (insert newtext))))

(defun match-string (num &optional string)
  (if (match-beginning num)
      (if string
	  (substring string (match-beginning num) (match-end num))
	(buffer-substring (match-beginning num) (match-end num)))
    ""))

;;  New definition for variables

(setq compilation-error-regexp-alist
      '(
      ("\n\\([^ \n\t:]+\\):\\([0-9]+\\):\\([0-9]+\\)[: \t]" 1 2 2)
      ("\n\\([^ \n\t:]+\\):\\([^ \n\t:]+\\):\\([0-9]+\\):\\([0-9]+\\)[: \t]" 2 3 3)
      ))

(setq ada-prj-default-comp-cmd
  "gcc \"-c\" \"-g\" \"-gnatq\" \"-I${src_dir}\"")
(setq ada-prj-default-make-cmd
      (concat
       "gnatmake ${main} \"-aI${src_dir}\" \"-aO${obj_dir}\" "
       "\"-g\" \"-gnatq\" \"-cargs\" ${comp_opt} "
       "\"-bargs\" ${bind_opt} \"-largs\" ${link_opt}"))
      
(setq ada-search-directories '("." "/gnu/lib/openvms7_1/2_8_1/declib"
			       "/gnu/lib/openvms7_1/2_8_1/adainclude"))

(setq ada-check-switch " \"-gnats\" ")

(ada-add-extensions ".ADS" ".ADB")
(load "ada-mode")



;;  As to be after (load "ada-mode") to override the definitions

;;  Convert Vms file names to Unix format
;;  Examples are:
;;  DDKA0:[RUPP.NETSCAPE.6222-005]
;;     /dka0/rupp/netscape/6222-005/
;;  [RUPP.NETSCAPE.6222-005]
;;     /sys$disk/rupp/netscape/6222-005/
;;  [.NETSCAPE.6222-005]
;;     netscape/6222-005/
;;  []
;;     ./
;;  [-]
;;     ../
;;  [--]
;;     ../../
;;  [-.-]
;;     ../../

(defun ada-convert-file-name (name)
  "Function to convert from a VMS file name to a Unix filename"
  (let ((result ""))
    (if (string-match "^\\([^[:]+\\)?\\(:?\\[[]\\.-]\\)?" name)
	(let ((disk (match-string 1 name))
	      (dir  (match-string 2 name)))
	  (if disk
	      (setq result (concat "/" disk "/"))
	    (if (not dir)
		(setq result "/sys$disk/"))))
      (setq result "/sys$disk/")
      )
    (if (string-match "\\(\\[[^]]*\\]\\)+" name)
	(let ((match (match-string 0 name)))
	  (while (string-match "\\." match)
	    (setq match (replace-match "/" nil nil match)))
	  (while (string-match "\\[\\]" match)
	    (setq match (replace-match "./" nil nil match)))
	  (while (string-match "\\([[/]\\)-\\([]/]\\)" match)
	    (setq match (replace-match (concat
					(match-string 1 match)
					".."
					(match-string 2 match))
				       nil nil match)))
	  (while (string-match "\\[/?" match)
	    (setq match (replace-match "" nil nil match)))
	  (while (string-match "\\]" match)
	    (setq match (replace-match "/" nil nil match)))
	  (setq result (concat result match))))
    (if (string-match "\\]\\([^[]+\\)$" name)
	(setq result (concat result (match-string 1 name))))
    (downcase result)
    ))


(defun ada-search-ignore-string-comment
  (search-re backward search-func &optional limit paramlists)
  ;; Regexp-Search for SEARCH-RE, ignoring comments, strings and
  ;; parameter lists, if PARAMLISTS is nil. Returns a cons cell of
  ;; begin and end of match data or nil, if not found.
  ;; The search is done using search-func, so that we can choose using
  ;; regular expression search, basic search, ...
  (let (found
	begin
	end
	parse-result
	(previous-syntax-table (syntax-table)))

    ;;
    ;; search until found or end-of-buffer
    ;; We have to test that we do not look further than limit
    ;;
    (set-syntax-table ada-mode-symbol-syntax-table)
    (while (and (not found)
		(or (not limit)
		    (or (and backward (<= limit (point)))
			(>= limit (point))))
                (funcall search-func search-re limit 1))
      (setq begin (match-beginning 0))
      (setq end (match-end 0))

      (setq parse-result (parse-partial-sexp
			  (save-excursion (beginning-of-line) (point))
			  (point)))
      
      (cond
       ;;
       ;; If inside a string, skip it (and the following comments)
       ;;
       ((ada-in-string-p)
	(funcall search-func "\"" nil t)
	(if (not backward) (progn (forward-sexp 1) (beginning-of-line))))
       ;;
       ;; If inside a comment, skip it (and the following comments)
       ;;
       ((ada-in-comment-p parse-result)
	(funcall search-func "--" nil t)
	(if (not backward) (progn (forward-comment 1000) (beginning-of-line))))
       ;;
       ;; directly in front of a comment => skip it, if searching forward
       ;;
       ((and (= (char-after begin) ?-) (= (char-after (1+ begin)) ?-))
        (if (not backward) (progn (forward-char -1) (forward-comment 1000))))

       ;;
       ;; found a parameter-list but should ignore it => skip it
       ;;
       ((and (not paramlists) (ada-in-paramlist-p))
        (if backward
	  (goto-char (scan-lists (point) -1 1))))
       ;;
       ;; found what we were looking for
       ;;
       (t
        (setq found t))))		; end of loop

    (set-syntax-table previous-syntax-table)

    (if found
        (cons begin end)
      nil)))


(if window-system
   (progn
     (setq hilit-mode-enable-list  '(not text-mode)
           hilit-background-mode 'light
	   hilit-inhibit-hooks   nil
	   hilit-inhibit-rebinding nil)
     (require 'hilit19)))

(provide 'ada-vms)


