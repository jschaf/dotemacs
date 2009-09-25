;;; Process error messages from Tartan Ada into a form emacs can handle.
;;;
;;
;;; Authors:
;;; Stephen Leake stephen.leake@gsfc.nasa.gov
;; Maintainer: Stephen Leake <stephen_leake@stephe-leake.org>

;; This file is not part of Gnu Emacs (yet)

; This is what a section of the compiler output looks like:
; ____________________________________________________________________________
;
;   43|
;   44|   subtype Mux_Data_Type is Machine.Unsigned_Word_Array (1 .. 112);
;                                         ^1
; ***  1 Error 2002: This name is not visible (RM83 8.3)
;   45|
; ____________________________________________________________________________
;
;  104|   is begin
;  105|      Item := From_Buffer (Buffer (Last + 1 .. Last + 3));
;                    ^1
; ***  1 Warn 4203: UNCHECKED_CONVERSION: sizes of source and target type are
; ***     different
;  106|      Last := Last + 3;
;
; Finished compilation of /export/home/stephe/NGIMS/Build/Tartan/Tartan_Lib/nptm.ssrc

(defun tartan-find-file (root ext-list)
  "Find a file with name ROOT, extension from EXT-LIST, in a directory
in compilation-search-path. Return nil if not found, full file path if
found."
  (let ((dirs compilation-search-path)
        dir
        found
        file
        ext
        temp-ext-list)
    (while (and (not found) dirs)
      (setq dir (file-name-as-directory (car dirs)))
      (setq dirs (cdr dirs))
      (setq temp-ext-list ext-list)
      (while (and (not found) temp-ext-list)
        (setq ext (car temp-ext-list))
        (setq temp-ext-list (cdr temp-ext-list))
        (setq file (concat dir root ext))
        (if (file-exists-p file)
            (setq found file))
        )) ; end whiles
    found))


(defun tartan-error-translator ()
  (interactive)

  (let (error-start
        errors-end
        error-number
        (file-end (point-max))
        column-number
        line-number
        file-name
        file-ext)

    (goto-char (point-min))
    ;; loop for each file compiled
    (while file-end
      ;; get the next file name
      (setq file-end (re-search-forward "Finished compilation of \\(.*\\)\\(\\..*\\)$" (point-max) t))
      (if file-end
          (progn
            (setq file-end (copy-marker file-end))
            (setq file-name (file-name-nondirectory (match-string-no-properties 1)))
            (setq file-ext (match-string-no-properties 2))

            ;; Find the corresponding file section (if any). We are at
            ;; the file name, after the section delimiters. Delete the
            ;; delimiters so we can distinguish successful
            ;; compilations from unsuccessful ones.
            (beginning-of-line)
            (setq errors-end (copy-marker (point)))
            (setq error-start nil)
            (while (re-search-backward "^______*" (point-min) t)
              (setq error-start (point))
              (replace-match ""))
            (if error-start
                (progn
                  (cond
                   ((or (equal file-ext ".ssrc")
                        (equal file-ext ".src"))

                    ;; This is the library copy of a file; find the real
                    ;; file by searching the compilation search path.
                    ;; Assume the GNAT naming convention, or Stephe's
                    ;; preprocessor naming convention.
                    (if (equal file-ext ".ssrc")
                        (setq file-ext (list ".ads" ".ads.gp"))
                      (setq file-ext (list ".adb" ".adb.gp")))

                    (setq file-name
                          (or (tartan-find-file file-name file-ext)
                              "file_not_found")))

                   (t
                    ;; just use this file name and extension
                    (setq file-name (concat file-name file-ext))
                    ))

                  ;; loop over each error in the file section
                  (while (and error-start errors-end)
                    (goto-char error-start)
                    (setq error-start
                          (search-forward-regexp
                           "^\\*\\*\\*\\s-+\\([1-9]+\\)\\s-+\\(Error\\|Warn\\)"
                           errors-end t))
                    (setq error-number (string-to-number (match-string 1)))
                    ;; This routine can't handle more than one error per line yet.
                    (if (and error-start
                             (equal error-number 1))
                        (progn
                          (setq error-start (copy-marker error-start))
                          ;; Get the column number
                          (beginning-of-line)
                          (forward-line -1)
                          (setq column-number
                                (number-to-string
                                 (- (save-excursion
                                      (search-forward "^")
                                      (point))
                                    (point)
                                    5)))

                          ;; get the line number - search backward to start of wrapped lines
                          (forward-line -1)
                          (while (not (looking-at "\\s-*\\([0-9]+\\)"))
                            (forward-line -1))
                          (setq line-number (match-string 1))

                          ;; Paste in a good line for standard gnu entry in compilation-error-regexp-alist
                          (forward-line -1)
                          (insert file-name ":" line-number ":" column-number ":")
                          (newline)
                          (goto-char error-start) ; don't find this one again!
                          ))) ; end loop on errors
                  )) ;; end if error-start
            (goto-char file-end)
            (forward-line 1) ; don't find this one again!
            ))) ; end loop on files

    ) ; end main let
  (goto-char (point-min)) )

(defun tartan-compilation-parse-errors (limit-search find-at-least)
  "see `compilation-parse-errors-function'."
  (interactive)
  ;; First translate Tartan messages into something the gnu
  ;; compilation regexp will recognize, then run the default
  ;; compilation parser.

  ;; reparse the whole buffer, because compilation-parsing-end seems
  ;; to be at the end of the buffer
  (tartan-error-translator)

  (compilation-parse-errors limit-search find-at-least))

(setq compilation-parse-errors-function 'tartan-compilation-parse-errors)
;;; end of file
