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

(defun my:smartparens-config ()
  (interactive)
  (let ((sexp-motions '(("zh" . sp-backward-sexp)
                        ("zj" . sp-down-sexp)
                        ("zk" . sp-backward-up-sexp)
                        ("zl" . sp-forward-sexp)

                        ("zp" . sp-previous-sexp)
                        ("zJ" . sp-end-of-sexp)
                        ("zK" . sp-beginning-of-sexp)
                        ("zn" . sp-next-sexp)

                        ("zd" . sp-backward-down-sexp)
                        ("zu" . sp-up-sexp)))


        (sexp-modifications '(("Zt" . sp-transpose-sexp)

                              ("zsu" . sp-unwrap-sexp)
                              ("zsb" . sp-backward-unwrap-sexp)

                              ("zfl" . sp-forward-slurp-sexp)
                              ("zfh" . sp-backward-slurp-sexp)

                              ("zgl" . sp-forward-barf-sexp)
                              ("zgL" . sp-add-to-previous-sexp)
                              ("zgh" . sp-backward-barf-sexp)
                              ("zgH" . sp-add-to-next-sexp)

                              ("zss" . sp-splice-sexp)
                              ("zsdl" . sp-splice-sexp-killing-forward)
                              ("zsdh" . sp-splice-sexp-killing-backward)
                              ("zsda" . sp-splice-sexp-killing-around)

                              ("Zsc" . sp-convolute-sexp)
                              ("Zsa" . sp-absorb-sexp)
                              ("Zse" . sp-emit-sexp)
                              ("zyY" . sp-join-sexp)
                              ("zyy" . sp-split-sexp))))

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
