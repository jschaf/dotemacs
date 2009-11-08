;; Joe Schafer's .emacs

;; Use a decent font and ignore errors if the font isn't found.
(ignore-errors
  (modify-all-frames-parameters
   '((font . "Consolas 14")
     (menu-bar-lines . 0)
     (tool-bar-lines . 0))))

;; Add the default-directory and all subdirectories to the load path
(let ((default-directory "~/.emacs.d/"))
  (setq load-path (cons default-directory load-path))
  (normal-top-level-add-subdirs-to-load-path))

;; Start in a reasonable directory
(setq default-directory "~/")

;;; Initial Code Load
(setq custom-file "~/.emacs.d/.emacs-custom.el")
(load custom-file)
(load "joe-autoloads.el")
(load "joe-functions.el")
(load "joe-customizations.el")

;; update M-x command cache
(smex-update)

(server-start)