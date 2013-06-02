;; Joe Schafer's .emacs

;; Use a decent font. 
(modify-all-frames-parameters
 '((font . "Consolas 12")))

;; Remove distractions.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Add the default-directory to the load path.
(let ((default-directory "~/.emacs.d/"))
  (setq load-path (cons default-directory load-path)))

;; Adjust GC threshold to 20 megabytes.
(setq gc-cons-threshold (* 20 1024 1024))

;; Start in a reasonable directory.
(setq default-directory "~/")

;;; Code Load
(setq custom-file "~/.emacs.d/private/emacs-custom.el")
(load custom-file)
(load "autoloads")
(load "functions")
(load "custom")

(require 'server)
(or (server-running-p)
    (server-start))
