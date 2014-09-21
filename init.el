;;; init.el --- Joe Schafer's .emacs -*- lexical-binding: t -*-

;;; Commentary:

;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars.  It is not just
;; bigger and brighter; it simply makes everything else vanish."
;;
;; -Neal Stephenson, "In the Beginning was the Command Line"

;;; Code:

;; Use a decent font.
;; (set-face-attribute 'default nil :font "Consolas 12")

;; Remove distractions.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Add the Emacs directory to the load path.
(setq load-path (add-to-list 'load-path (locate-user-emacs-file "lisp/")))

;; Adjust GC threshold to 20 megabytes.  This dramatically improves
;; startup time and helps with the overall speed of Emacs.
(setq gc-cons-threshold (* 20 1024 1024))

;; Start in a reasonable directory.
(setq default-directory "~/")

(defmacro after (mode &rest body)
  "`eval-after-load' MODE evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,mode
     '(progn ,@body)))

(defun my:write-string-to-file (string file)
  "Write STRING to FILE.  Overwrites FILE."
  (interactive "sEnter the string: \nFile to save to: ")
  (with-temp-buffer
    (insert string)
    (when (file-writable-p file)
      (write-region (point-min) (point-max) file))))

(defvar my:emacs-private-dir (locate-user-emacs-file "private/")
  "Location to store personal customizations.")

(defun my:privatize (path)
  "Return string of PATH in `my:emacs-private-dir'."
  (let* ((private-dir (file-name-as-directory my:emacs-private-dir))
         (default-directory private-dir))
    (concat private-dir path)))

(defun my:create-file (path &optional directory-p)
  "Create PATH in `my:emacs-private-dir' if it doesn't exit.
If DIRECTORY-P is non-nil, make a directory instead of a file."
  (let* ((private-dir (file-name-as-directory my:emacs-private-dir))
         (default-directory private-dir)
         (full-path (file-truename path)))

    (cond
     ((file-exists-p path)
      (message "File exists at %s, skipping creation" full-path))

     (directory-p
      (make-directory path)
      (message "Created private directory at %s" full-path))

     (t
      (if (file-regular-p path)
          (message "File exists at %s, skipping creation" full-path)
        (my:write-string-to-file "" path)
        (message "Created private file at %s"  full-path))))))

;; Ensure private directory and emacs-custom file exist
(setq custom-file (my:privatize "emacs-custom.el"))
(my:create-file (my:privatize "") 'dir)
(my:create-file custom-file)

;;; Code Load
(load "misc")
(load custom-file)
(load "base")
(load "autoloads")
(load "functions")
(load "evil-sp")

(load-theme 'solarized-light)

(require 'server)

(setq server-auth-dir (my:privatize "server"))
(my:create-file server-auth-dir 'dir)

(defun my:maybe-start-server ()
  (unless (server-running-p)
    (server-start)))

(setq my:server-timer (run-with-idle-timer 0 10 'my:maybe-start-server))

;; This would go in misc.el, but Emacs purposefully makes it difficult
;; to disable this message.  `inhibit-startup-echo-area-message' must
;; be set to $USERNAME and must be in the actual init-file.
(setq inhibit-startup-echo-area-message "joe")

(provide 'init)
;;; init.el ends here
