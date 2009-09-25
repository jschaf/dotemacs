;; @(#) ada-aunit.el --- support for AUnit template generation

;; Copyright (C) 2001 Free Software Foundation, Inc.

;; Author: Ed Falis <falis@gnat.com>
;;         Emmanuel Briot <briot@gnat.com>
;; Maintainer: Stephen Leake <stephen_leake@stephe-leake.org>
;; Keywords: languages ada aunit


(defvar ada-aunit-make-test-cmd "aunit_make_test"
  "Command used to ask the user for the name and setup of the test.
This command should return the name of the new test to be created.")

(defvar ada-aunit-make-suite-cmd "aunit_make_suite"
  "Command used to create a new test suite.
This command should return the name of the test suite to be created.")

(defvar ada-aunit-make-harness-cmd "aunit_make_harness"
  "Command used to create a new harness.
This command should return the name of the new harness.")

(defconst ada-aunit-not-found-msg
  (concat "Make sure the Aunit package is correctly installed"
	  " on your system.")
  "Message printed when the executables for Aunit are not found on the path.")

;;---------------------
;;--  ada-aunit-chop --
;;---------------------

(defun ada-aunit-chop (str)
  "Remove any trailing newline character.
If there is no such character, STR is unmodified."
  (while (eq (aref str (1- (length str))) ?\n)
    (setq str (substring str 0 -1)))
  str)

;;----------------------------------
;;--  ada-aunit-process-to-string --
;;----------------------------------

(defun ada-aunit-process-to-string (cmd)
  "Return the result of CMD as a string, including any terminating newline.
If CMD was not found, an error is raised."
  (let (result str)
    (setq str
	  (with-output-to-string
	    (with-current-buffer standard-output
	      (setq result (call-process shell-file-name nil t nil
					 shell-command-switch cmd)))))
    (if (= result 0)
	str
      (error (message ada-aunit-not-found-msg)))))

;;---------------
;;-- ada-aunit --
;;---------------

(defun ada-aunit ()
  "Define the menu and keymaps associated with the support for AUnit."
  
  ;;  Define the menu
  (let ((menu '(["New Test Case"   ada-aunit-make-test t]
		["Add Routine" ada-aunit-add-routine t]
		["New Test Suite" ada-aunit-make-suite t]
		["New Test Harness" ada-aunit-make-harness t])))
    (if (featurep 'xemacs)
	(funcall (symbol-function 'add-submenu)
		 '("Ada") (append (list "Unit testing"
					:included '(string= mode-name "Ada"))
				  menu))
      (define-key-after (lookup-key ada-mode-map [menu-bar Ada]) [aunit]
	(list 'menu-item
	      "Aunit"
	      (easy-menu-create-menu "Unit testing" menu)
	      :visible '(string= mode-name "Ada"))
	t)))

  ;;  Define the key bindings
  (define-key ada-mode-map "\C-c\C-ua" 'ada-aunit-add-routine)
  (define-key ada-mode-map "\C-c\C-ut" 'ada-aunit-make-test)
  (define-key ada-mode-map "\C-c\C-us" 'ada-aunit-make-suite)
  (define-key ada-mode-map "\C-c\C-uh" 'ada-aunit-make-harness)
  )

;;-------------------------
;;-- ada-aunit-make-test --
;;-------------------------
(defun ada-aunit-make-test ()
  "Create the required files for a new test.
The name of the test is read from a separate GUI dialog."
  (interactive)
  (let ((name (ada-aunit-process-to-string ada-aunit-make-test-cmd)))
    (if (not (= (length name) 0))
	(progn
	  (setq name (file-name-sans-extension (ada-aunit-chop name)))
	  (find-file-other-window (concat name ".ads"))
	  (find-file-other-window (concat name ".adb"))))
    ))

;;--------------------------
;;-- ada-aunit-make-suite --
;;--------------------------

(defun ada-aunit-make-suite ()
  "Create the required files for a new test suite.
The name of the test suite is read from a separate GUI dialog."
  (interactive)
  (let ((name (ada-aunit-process-to-string ada-aunit-make-suite-cmd)))
    (if (not (= (length name) 0))
	(progn
	  (find-file-other-window
	   (concat (file-name-sans-extension (ada-aunit-chop name)) ".adb"))))
	))

;;----------------------------
;;-- ada-aunit-make-harness --
;;----------------------------

(defun ada-aunit-make-harness ()
  "Create the required files for a new test suite.
The name of the test suite is read from a separate GUI dialog."
  (interactive)
  (let ((name (ada-aunit-process-to-string ada-aunit-make-harness-cmd)))
    (if (not (= (length name) 0))
	(progn
	  (find-file-other-window
	   (concat (file-name-sans-extension (ada-aunit-chop name)) ".adb"))))
    ))

;;----------------------------
;;-- ada-aunit-strip-quotes --
;;----------------------------

(defun ada-aunit-strip-quotes (str)
  "Remove any leading or trailing quotes and spaces in STR.
Return the new string."
  (cond ((eq (aref str 0) ?\ )  (ada-aunit-strip-quotes (substring str 1)))
	((eq (aref str 0) ?\")  (ada-aunit-strip-quotes (substring str 1)))
	((eq (aref str (1- (length str))) ?\ ) 
	 (ada-aunit-strip-quotes (substring str 0 -1)))
	((eq (aref str (1- (length str))) ?\") 
	 (ada-aunit-strip-quotes (substring str 0 -1)))
	(t str)))

;;---------------------------
;;-- ada-aunit-add-routine --
;;---------------------------

(defun ada-aunit-add-routine ()
  "Add a new routine in the current test."
  (interactive)
  (save-excursion
    (let ((function-name (read-string "Name of the new routine: "))
	  (function-description (read-string "Description: ")))
      
      (goto-char (point-max)) 
      (search-backward "procedure Register_Tests")
      (beginning-of-line)
      (let ((start (point)))
	(insert "procedure "
		function-name
		" (T : in out AUnit.Test_Cases.Test_Case'Class) is\n"
		"begin\n"
		"null;\n"
		"end " function-name ";\n\n")
	(ada-adjust-case-region start (point))
	(ada-indent-region start (point)))

      (search-forward "end Register_Tests")
      (beginning-of-line)
      (let ((start (point)))
	(insert "Register_Routine (T, " function-name "'Access, \""
		(ada-aunit-strip-quotes function-description)
		"\");\n")
	(ada-adjust-case-region start (point))
	(ada-indent-region start (point))))))

(add-hook 'ada-mode-hook 'ada-aunit)

(provide 'ada-aunit)
