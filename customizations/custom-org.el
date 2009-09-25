;; quick Remember mode from outside emacs
;; http://metajack.im/2008/12/30/gtd-capture-with-emacs-orgmode/
(defadvice remember-finalize (after delete-remember-frame activate)  
  "Advise remember-finalize to close the frame if it is the remember frame"  
  (if (equal "*Remember*" (frame-parameter nil 'name))  
      (delete-frame)))  

(defadvice remember-destroy (after delete-remember-frame activate)  
  "Advise remember-destroy to close the frame if it is the remember frame"  
  (if (equal "*Remember*" (frame-parameter nil 'name))  
      (delete-frame)))  

;; make the frame contain a single window. by default org-remember  
;; splits the window.  
(add-hook 'remember-mode-hook  'delete-other-windows)  

(defun make-remember-frame ()  
  "Create a new frame and run org-remember"
  (interactive)  
  (make-frame '((name . "*Remember*") (width . 80) (height . 10)))  
  (select-frame-by-name "*Remember*")
  (org-remember))


(define-key mode-specific-map [?a] 'org-agenda)

(eval-after-load "org"
  '(progn
     (define-prefix-command 'org-todo-state-map)

     (define-key org-mode-map "\C-cx" 'org-todo-state-map)

     (define-key org-todo-state-map "x"
       #'(lambda nil (interactive) (org-todo "CANCELLED")))
     (define-key org-todo-state-map "d"
       #'(lambda nil (interactive) (org-todo "DONE")))
     (define-key org-todo-state-map "f"
       #'(lambda nil (interactive) (org-todo "DEFERRED")))
     (define-key org-todo-state-map "l"
       #'(lambda nil (interactive) (org-todo "DELEGATED")))
     (define-key org-todo-state-map "s"
       #'(lambda nil (interactive) (org-todo "STARTED")))
     (define-key org-todo-state-map "w"
       #'(lambda nil (interactive) (org-todo "WAITING")))

                                        ;(define-key org-agenda-mode-map "\C-n" 'next-line)
                                        ;(define-key org-agenda-keymap "\C-n" 'next-line)
                                        ;(define-key org-agenda-mode-map "\C-p" 'previous-line)
                                        ;(define-key org-agenda-keymap "\C-p" 'previous-line)
     ))
(require 'remember)

(add-hook 'remember-mode-hook 'org-remember-apply-template)
(define-key global-map [f1] 'menu-bar-mode)
(define-key global-map [(control meta ?r)] 'remember)

(custom-set-variables
 '(org-agenda-files (quote ("~/org/todo.org")))
 '(org-default-notes-file "~/org/notes.org")
 '(org-agenda-ndays 7)
 '(org-log-done 'note)
 '(org-deadline-warning-days 14)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-reverse-note-order t)
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-agenda-custom-commands
   (quote (("d" todo "DELEGATED" nil)
           ("c" todo "DONE|DEFERRED|CANCELLED" nil)
           ("w" todo "WAITING" nil)
           ("W" agenda "" ((org-agenda-ndays 21)))
           ("A" agenda ""
            ((org-agenda-skip-function
              (lambda nil
                (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#A\\]")))
             (org-agenda-ndays 1)
             (org-agenda-overriding-header "Today's Priority #A tasks: ")))
           ("u" alltodo ""
            ((org-agenda-skip-function
              (lambda nil
                (org-agenda-skip-entry-if (quote scheduled) (quote deadline)
                                          (quote regexp) "<[^>\n]+>")))
             (org-agenda-overriding-header "Unscheduled TODO entries: "))))))
 '(org-remember-store-without-prompt t)
 '(org-remember-templates
   (quote ((116 "* TODO %?\n  %u" "~/org/todo.org" "Tasks")
           (110 "* %u %?" "~/org/notes.org" "Notes"))))
 '(remember-annotation-functions (quote (org-remember-annotation)))
 '(remember-handler-functions (quote (org-remember-handler))))
