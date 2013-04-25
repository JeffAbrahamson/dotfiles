

(line-number-mode 1)
;; Useful, but disappeared
;(resize-minibuffer-mode 1) 
(setq default-major-mode 'text-mode)

(setq auto-mode-alist
      (append auto-mode-alist
	      (list
	       '("\\.make" . makefile-mode)
	       '("\\.sh$" . ksh-mode)
	       '("\\.ksh$" . ksh-mode)
	       '("\\.bashrc" . ksh-mode)
	       '("\\..*profile" . ksh-mode)
	       '("\\..*cshrc" . ksh-mode))))

;; (setq ksh-mode-hook
;;       (function (lambda ()
;; 		  (font-lock-mode 1)             ;; font-lock the buffer
;; 		  (setq ksh-indent 8)
;; 		  (setq ksh-group-offset -8))
;; 		(setq ksh-brace-offset -8)   
;; 		(setq ksh-tab-always-indent t)
;; 		(setq ksh-match-and-tell t)
;; 		(setq ksh-align-to-keyword t)	;; Turn on keyword alignment
;; 		))

(defun linux-c-mode ()
  "C mode with adjusted defaults for use with the Linux kernel."
  (interactive)
  (c-mode)
  (local-set-key [(return)] 'newline-and-indent) 
  (setq c-indent-level 8)
  (setq c-brace-imaginary-offset 0)
  (setq c-brace-offset -8)
  (setq c-argdecl-indent 8)
  (setq c-label-offset -8)
  (setq c-continued-statement-offset 8)
  (setq indent-tabs-mode nil)
  (setq tab-width 8))

(setq auto-mode-alist (cons '("/usr/src/linux.*/.*\\.[ch]$" . linux-c-mode)
                       auto-mode-alist))
(setq auto-mode-alist (cons '("/home/jeff/.*\\.[ch]$" . linux-c-mode)
                       auto-mode-alist))

(add-hook 'c-mode-common-hook
	  (function (lambda()
		      (local-set-key [(return)] 'newline-and-indent) 
		      (setq c-basic-offset 8)
		      (setq c-indent-level 8)
		      (setq c-continued-statement-offset 8)
		      (setq indent-tabs-mode nil)
		      (setq c-label-offset -8))))


;; I should turn on electric-c mode here
;; Check these bindings in electric-c:
;;    C-c C-a, C-M-\, C-x H, C-x V V

;; (setq c-mode-hook
;;       (lambda ()
;; 	(setup-programmer-keys)))
(add-hook 'c-mode-hook
	  (function
	   (lambda()
	     (setup-programmer-keys))))

;; (setq c++-mode-hook
;;       (lambda ()
;; 	(setup-programmer-keys)))
(add-hook 'c++-mode-hook
	  (function
	   (lambda()
	     (setup-programmer-keys))))

;; (setq emacs-lisp-mode-hook
;;       (lambda ()
;; 	(setup-programmer-keys)))
(add-hook 'emacs-lisp-mode-hook
	  (function
	   (lambda()
	     (setup-programmer-keys))))

;; ;; (setq text-mode-hook 'turn-on-auto-fill)
;; (setq text-mode-hook
;;       (lambda ()
;; 	(progn
;; 	  (text-mode-hook-identify)
;; 	  (flyspell-mode))))
(add-hook 'text-mode-hook
	  (function
	   (lambda()
	     (progn
	       (text-mode-hook-identify)
	       (flyspell-mode)))))

(setq after-make-frame-functions
      (lambda (frame)
	(save-excursion
	    (select-frame frame)
	      (set-mouse-color "blue"))))

(setq perl-mode-hook
      (lambda ()
	(progn
	  (setup-programmer-keys))))

(add-hook 'tex-mode-hook
	  (function
	   (lambda ()
	     (local-set-key [f11] 'TeX-next-error)
	     (setq ispell-parser 'tex))))

(add-hook 'post-mode-hook
	  (function
	   (lambda ()
	     (search-forward "\n\n" (point-max) t))))

;(autoload 'python-mode "python-mode" "Python Mode." t)
;(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; (add-hook 'python-mode-hook
;; 	  (lambda ()
;; 	    (set (make-variable-buffer-local 'beginning-of-defun-function)
;; 		 'py-beginning-of-def-or-class)
;; 	    (setq outline-regexp "def\\|class ")))

(add-hook 'python-mode-hook
	  (lambda ()
	    ;;(local-set-key "\"" 'electric-pair)
	    ;;(local-set-key "\'" 'electric-pair)
	    ;;(local-set-key "(" 'electric-pair)
	    ;;(local-set-key "[" 'electric-pair)
	    ;;(local-set-key "{" 'electric-pair)

	    (local-set-key [C-m] 'newline-and-indent)
	    ))

(defun electric-pair ()
  "Insert character pair without sournding spaces"
  (interactive)
  (let (parens-require-spaces)
    (insert-pair)))

