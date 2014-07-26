;;; init.el --- Joe Schafer's .emacs

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
(setq load-path (cons user-emacs-directory load-path))

;; Adjust GC threshold to 20 megabytes.  This dramatically improves
;; startup time and helps with the overall speed of Emacs.
(setq gc-cons-threshold (* 20 1024 1024))

;; Start in a reasonable directory.
(setq default-directory "~/")

;;; Code Load
(load "misc")
(setq custom-file "~/.emacs.d/private/emacs-custom.el")
(load custom-file)
(load "autoloads")
(load "functions")
(load "evil-sp")

(load-theme 'solarized-light)

(require 'server)
(defun my:maybe-start-server ()
  (unless (server-running-p)
    (server-start)))

(setq my:server-timer (run-with-idle-timer 0 10 'my:maybe-start-server))

;; This would go in misc.el, but Emacs purposefully makes it difficult
;; to disable this message.  `inhibit-startup-echo-area-message' must
;; be set to $USERNAME and must be in the actual init-file.
(setq inhibit-startup-echo-area-message "joe")

;; Local Variables:
;; lexical-binding: t
;; End:

(provide 'init)
;;; init.el ends here
