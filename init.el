;; Joe Schafer's .emacs

;; use a decent font and remove distractions
(modify-all-frames-parameters
 '((font . "Consolas 11")
   (menu-bar-lines . 0)
   (tool-bar-lines . 0)))

;; Add the default-directory and all subdirectories to the load path
(let ((default-directory "~/.emacs.d/"))
  (setq load-path (cons default-directory load-path))
  (normal-top-level-add-subdirs-to-load-path))

;; Start in a reasonable directory
(setq default-directory "~/")

;;; Code Load
(require 'cl)

(setq custom-file "~/.emacs.d/.emacs-custom.el")
(load custom-file)
(load "autoloads.el")
(load "funcs.el")
(load "custom.el")
(load "colors.el")

;; Update smex command cache after all the loads.
(smex-update)

(server-start)
