;;; init.el --- Joe Schafer's .emacs -*- lexical-binding: t -*-

;;; Commentary:

;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars.  It is not just
;; bigger and brighter; it simply makes everything else vanish."
;;
;; -Neal Stephenson, "In the Beginning was the Command Line"

;;; Code:

;; Use a decent font.
(defun fontify-frame (frame)
  "Use appropriate font and size on FRAME."
  (interactive)
  (let ((font-family (if (member "Consolas" (font-family-list))
                         "Consolas"
                       (face-attribute 'default :family))))
    (when window-system
      (if (> (display-pixel-width) 2000)
          (set-frame-parameter frame 'font (format "%s 13" font-family))
        (set-frame-parameter frame 'font (format "%s 14" font-family))))))

;; Fontify current frame
(fontify-frame nil)

;; Remove distractions.  Since the menu bar is permanent in Mac OS X,
;; we might as well show it.
(unless (eq system-type 'darwin)
  (menu-bar-mode -1))
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Add the Emacs lisp directory to the load path.
(add-to-list 'load-path (locate-user-emacs-file "lisp/"))

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

(package-initialize)

;;; Code Load
(load "misc")
(load custom-file)
(require 'base)
(require 'autoloads)
(require 'functions)
(require 'evil-sp)

(require 'server)

(setq server-auth-dir (my:privatize "server"))
(my:create-file server-auth-dir 'dir)

(defun my:maybe-start-server ()
  (unless (server-running-p)
    (server-start)))

;; Check for server existence every 10 seconds
(defvar my:server-timer (run-with-idle-timer 10 t 'my:maybe-start-server))

;; This would go in misc.el, but Emacs purposefully makes it difficult
;; to disable this message.  `inhibit-startup-echo-area-message' must
;; be set to $USERNAME and must be in the actual init-file.
(setq inhibit-startup-echo-area-message "joe")

;; Clear the echo area
(add-hook 'after-init-hook '(lambda () (message "Emacs load time: %s"
                                           (emacs-init-time))))

(provide 'init)
;;; init.el ends here
