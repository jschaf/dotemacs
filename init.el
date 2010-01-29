;; Joe Schafer's .emacs

;; use a decent font and ignore errors if the font isn't found.
(modify-all-frames-parameters
 '((font . "Consolas 13")
   (menu-bar-lines . 0)
   (tool-bar-lines . 0)))

;; Add the default-directory and all subdirectories to the load path
(let ((default-directory "~/.emacs.d/"))
  (setq load-path (cons default-directory load-path))
  (normal-top-level-add-subdirs-to-load-path))

;; Start in a reasonable directory
(setq default-directory "~/")

;;; Initial Code Load
(setq custom-file ".emacs-custom.el")
(load custom-file)
(load "joe-autoloads.el")
(load "joe-functions.el")
(load "joe-customizations.el")

;; update M-x command cache
(smex-update)

; Suppress error "directory ~/.emacs.d/server is unsafe" on windows.
(require 'server)
(when (and (= emacs-major-version 23) (equal window-system 'w32))
  (defun server-ensure-safe-dir (dir) "Noop" t)) 
(server-start)
