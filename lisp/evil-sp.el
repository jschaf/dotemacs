(require 'smartparens)
(require 'evil)

(defun my:evilize-name (name)
   (intern (format "evil-%s" name)))

(defmacro my:make-evil-sp-movement (name)
  `(let ((evil-name (my:evilize-name ,name))
         (doc (documentation ,name)))
     (evil-define-motion evil-name (count)
       doc
       :type inclusive
       :jump t
       (,name count))))
(defun my:beginning-of-previous-sexp (&optional arg)
  (interactive "P")
  (setq arg (or arg 1))
  (sp-previous-sexp arg)
  (sp-backward-sexp))

(defun my:smartparens-config ()
  (interactive)
  (let ((sexp-motions '(("zh" . sp-backward-sexp)
                        ("zj" . sp-down-sexp)
                        ("zk" . sp-backward-up-sexp)
                        ("zl" . sp-forward-sexp)

                        ("zp" . my:beginning-of-previous-sexp)
                        ("zJ" . sp-end-of-sexp)
                        ("zK" . sp-beginning-of-sexp)
                        ("zn" . sp-next-sexp)

                        ("zH" . sp-beginning-of-previous-sexp)
                        ("zL" . sp-beginning-of-next-sexp)

                        ("z\C-k" . sp-backward-down-sexp)
                        ("z\C-j" . sp-up-sexp)))


        (sexp-modifications '(("zst" . sp-transpose-sexp)

                              ("zsu" . sp-unwrap-sexp)
                              ("zsb" . sp-backward-unwrap-sexp)

                              ("zfh" . sp-backward-slurp-sexp)
                              ("zfH" . sp-backward-barf-sexp)

                              ("zfl" . sp-forward-slurp-sexp)
                              ("zfL" . sp-forward-barf-sexp)

                              ("zgL" . sp-add-to-previous-sexp)
                              ("zgH" . sp-add-to-next-sexp)

                              ("zsw" . sp-swap-enclosing-sexp)
                              ("zss" . sp-splice-sexp)
                              ("zsdl" . sp-splice-sexp-killing-forward)
                              ("zsdh" . sp-splice-sexp-killing-backward)
                              ("zsda" . sp-splice-sexp-killing-around)

                              ("zsc" . sp-convolute-sexp)

                              ("zsh" . sp-absorb-sexp)
                              ("zsl" . sp-emit-sexp)

                              ("zsH" . sp-extract-before-sexp)
                              ("zsL" . sp-extract-after-sexp)

                              ("zsy" . sp-split-sexp)
                              ("zsY" . sp-join-sexp))))

    (loop for (key . func) in sexp-motions
          do
          ;; Define the motion command
          (my:make-evil-sp-movement func)

          ;; Create key-bindings
          (define-key evil-normal-state-map key func)
          (define-key evil-visual-state-map key func)
          (define-key evil-motion-state-map key func))

    (loop for (key . func) in sexp-modifications
          do
          ;; Create key-bindings
          (define-key evil-normal-state-map key func))))

(my:smartparens-config)
