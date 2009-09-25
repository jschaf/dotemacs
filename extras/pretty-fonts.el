;; (add-hook 'emacs-lisp-mode-hook
;; 	  '(lambda ()
;; 	     (interactive)
;; 	     (require 'eldoc)
;; 	     (turn-on-eldoc-mode)
;; 	     (define-key emacs-lisp-mode-map [(control c) (x)] 'copy-eval-dwim-lisp)
;; 	     ;; Default to auto-indent on Enter
;; 	     (define-key emacs-lisp-mode-map [(return)] 'newline-and-indent)
;; 	     (define-key emacs-lisp-mode-map [(control j)] 'newline)
;; 	     (define-key emacs-lisp-mode-map [(control m)] 'newline-and-indent)))


;; (defun pretty-lambdas ()
;;   (font-lock-add-keywords
;;    nil `(("(\\(lambda\\>\\)"
;; 	  (0 (progn (compose-region (match-beginning 1) (match-end 1)
;; 				   ,(make-char 'greek-iso8859-7 30))
;; 		    nil))))))


;;(font-lock-add-keywords 'emacs-scheme-mode
;;    '(("(\\(lambda\\)\\>" (0 (prog1 ()
;;                               (compose-region (match-beginning 1)
;;                                               (match-end 1)
;;                                               ?))))))

;;(add-hook 'scheme-mode-hook 'pretty-lambdas)
;;(add-hook 'emacs-lisp-mode-hook 'pretty-lambdas)

;;;; Set fonts up on MS Windows
(defvar mswindows-p (string-match "windows" (symbol-name system-type)))

(if mswindows-p
    (progn
      ;; Show as much as we can using fonts bundled with IE5
      (setq w32-standard-fontset-spec
	    "-*-Consolas-normal-r-*-*-*-130-*-*-c-*-fontset-courier,
   ascii:-*-Consolas-normal-r-*-*-*-130-*-*-c-*-iso8859-1,
   latin-iso8859-1:-*-Courier New-normal-r-*-*-*-130-*-*-c-*-iso8859-1,
   latin-iso8859-2:-*-Courier New-normal-r-*-*-*-130-*-*-c-*-iso8859-2,
   latin-iso8859-3:-*-Courier New-normal-r-*-*-*-130-*-*-c-*-iso8859-3,
   latin-iso8859-4:-*-Courier New-normal-r-*-*-*-130-*-*-c-*-iso8859-4,
   latin-iso8859-7:-*-Courier New-normal-r-*-*-*-130-*-*-c-*-iso8859-7,
   latin-iso8859-9:-*-Courier New-normal-r-*-*-*-130-*-*-c-*-iso8859-9,
   cyrillic-iso8859-5:-*-Courier New-normal-r-*-*-*-130-*-*-c-*-iso8859-5,
   greek-iso8859-7:-*-Courier New-normal-r-*-*-*-130-*-*-c-*-iso8859-7,
   hebrew-iso8859-8:-*-Rod-normal-r-*-*-*-130-*-*-c-*-iso8859-8,
   ipa:-*-Lucida Sans Unicode-normal-r-*-*-*-130-*-*-c-*-muleipa*-*,
   thai-tis620:-*-Tahoma-normal-r-*-*-*-130-*-*-c-*-tis620-*,
   latin-jisx0201:-*-MS Gothic-normal-r-*-*-*-130-*-*-c-*-jisx0208-sjis,
   katakana-jisx0201:-*-MS Gothic-normal-r-*-*-*-130-*-*-c-*-jisx0208-sjis,
   japanese-jisx0208:-*-MS Gothic-normal-r-*-*-*-130-*-*-c-*-jisx0208-sjis,
   japanese-jisx0208-1978:-*-MS Gothic-normal-r-*-*-*-130-*-*-c-*-jisx0208-sjis,
   japanese-jisx0212:-*-MS Gothic-normal-r-*-*-*-130-*-*-c-*-jisx0212-sjis,
   korean-ksc5601:-*-Gulim-normal-r-*-*-*-130-*-*-c-*-ksc5601-*,
   chinese-gb2312:-*-MS Song-normal-r-*-*-*-130-*-*-c-*-gb2312-*,
   chinese-big5-1:-*-MingLiU-normal-r-*-*-*-130-*-*-c-*-big5-*,
   chinese-big5-2:-*-MingLiU-normal-r-*-*-*-130-*-*-c-*-big5-*")

(setq w32-enable-italics t) (create-fontset-from-fontset-spec w32-standard-fontset-spec t)

(setq default-frame-alist '((font . "fontset-courier")))

(setq initial-frame-alist default-frame-alist)))

(defun unicode-symbol (name)
   "Translate a symbolic name for a Unicode character -- e.g., LEFT-ARROW                                      
 or GREATER-THAN into an actual Unicode character code. "
   (decode-char 'ucs (case name                                             
		       ('left-arrow #X2190)
		       ;('left-arrow 8592)
                       ('up-arrow 8593)
                       ('right-arrow 8594)
                       ('down-arrow 8595)                                                
		       ('double-vertical-bar #X2551)                  
                       ('equal #X003d)
                       ('not-equal #X2260)
                       ('identical #X2261)
                       ('not-identical #X2262)
                       ('less-than #X003c)
                       ('greater-than #X003e)
		       ('less-than-or-equal-to #X2264)
		       ('greater-than-or-equal-to #X2265)                        
                       ('logical-and #X2227)
                       ('logical-or #X2228)
                       ('logical-neg #X00AC)                                                  
                       ('nil #X2205)
                       ('horizontal-ellipsis #X2026)
                       ('double-exclamation #X203C)
                       ('prime #X2032)
                       ('double-prime #X2033)
                       ('for-all #X2200)
                       ('there-exists #X2203)
                       ('element-of #X2208)              
                       ('square-root #X221A)
                       ('squared #X00B2)
                       ('cubed #X00B3)                                            
                       ('lambda #X03BB)
                       ('alpha #X03B1)
                       ('beta #X03B2)
                       ('gamma #X03B3)
                       ('delta #X03B4))))

(defun substitute-pattern-with-unicode (pattern symbol)
    "Add a font lock hook to replace the matched part of PATTERN with the                                       
     Unicode symbol SYMBOL looked up with UNICODE-SYMBOL."
    (interactive)
    (font-lock-add-keywords
    nil `((,pattern 
           (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                     ,(unicode-symbol symbol)
                                     'decompose-region)
                             nil))))))

(defun substitute-patterns-with-unicode (patterns)
   "Call SUBSTITUTE-PATTERN-WITH-UNICODE repeatedly."
   (mapcar #'(lambda (x)
               (substitute-pattern-with-unicode (car x)
                                                (cdr x)))
           patterns))

(defun haskell-unicode ()
 (interactive)
 (substitute-patterns-with-unicode
  (list (cons "\\(<-\\)" 'left-arrow)
        (cons "\\(->\\)" 'right-arrow)
        (cons "\\(==\\)" 'identical)
        (cons "\\(/=\\)" 'not-identical)
        (cons "\\(()\\)" 'nil)
        (cons "\\<\\(sqrt\\)\\>" 'square-root)
        (cons "\\(&&\\)" 'logical-and)
        (cons "\\(||\\)" 'logical-or)
        (cons "\\<\\(not\\)\\>" 'logical-neg)
        (cons "\\(>\\)\\[^=\\]" 'greater-than)
        (cons "\\(<\\)\\[^=\\]" 'less-than)
        (cons "\\(>=\\)" 'greater-than-or-equal-to)
        (cons "\\(<=\\)" 'less-than-or-equal-to)
        (cons "\\<\\(alpha\\)\\>" 'alpha)
        (cons "\\<\\(beta\\)\\>" 'beta)
        (cons "\\<\\(gamma\\)\\>" 'gamma)
        (cons "\\<\\(delta\\)\\>" 'delta)
        (cons "\\(''\\)" 'double-prime)
        (cons "\\('\\)" 'prime)
        (cons "\\(!!\\)" 'double-exclamation)
        (cons "\\(\\.\\.\\)" 'horizontal-ellipsis))))

(add-hook 'scheme-mode-hook 'haskell-unicode)