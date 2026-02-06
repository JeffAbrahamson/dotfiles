

(line-number-mode 1)
;; Useful, but disappeared
;(resize-minibuffer-mode 1) 
(setq default-major-mode 'text-mode)

;; I usually want my .h files in C++ mode.
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("mutt-[a-z0-9]+-[0-9]+-[0-9]+-[0-9]+" . post-mode))
;; YAML mode
;; https://raw.githubusercontent.com/yoshiki/yaml-mode/master/yaml-mode.el
; (require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.sls\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))

;; C/C++ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook
	  (function (lambda()
		      (if (not buffer-read-only)
			  (local-set-key [(return)] 'newline-and-indent))
		      ;; experiment
		      (setq c-default-style "k&r")
		      ;; from before, s/4/8/
		      (setq c-basic-offset 4)
		      ;; (setq c-indent-level 8)
		      ;; (setq c-continued-statement-offset 8)
		      ;; (setq indent-tabs-mode nil)
		      ;; (setq c-label-offset -8)
		      (local-set-key (kbd "C-c f") 'ff-find-other-file)
		      (local-set-key (kbd "C-M-q") 'clang-format-region)
		      (google-set-c-style)
		      (google-make-newline-indent)
		      (if use-gtags
			  (progn
			    (gtags-mode 1)
			    (local-set-key [mouse-2] 'mouse-yank-primary)
			    (define-key gtags-mode-map [mouse-2] 'mouse-yank-primary)
			    (define-key gtags-mode-map [control-mouse-2] 'gtags-find-tag-by-event)))
		      )))


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

(add-hook 'c++-mode-hook
	  (function
	   (lambda()
	     (setup-programmer-keys)
	     (local-set-key (kbd "C-M-;") 'jma-dnc) ; A comment not to be committed.
	     (local-set-key (kbd "C-;") 'jma-todo)  ; A TODO comment.
	     (if use-gtags
		 (progn
		   (gtags-mode 1)
		   (local-set-key [mouse-2] 'mouse-yank-primary)
		   (define-key gtags-mode-map [mouse-2] 'mouse-yank-primary)
		   (define-key gtags-mode-map [control-mouse-2] 'gtags-find-tag-by-event)))
	     )))

;; elisp ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq emacs-lisp-mode-hook
;;       (lambda ()
;; 	(setup-programmer-keys)))
(add-hook 'emacs-lisp-mode-hook
	  (function
	   (lambda()
	     (setup-programmer-keys))))

;; SQL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jma-sql-mode-setup ()
  (sqlformat-on-save-mode)
  (define-key (kbd "C-M-q") 'sqlformat-region))

(add-hook 'sql-mode-hook 'jma-sql-mode-setup)
(with-eval-after-load 'sqlformat
  (define-key sql-mode-map (kbd "C-M-q") 'sqlformat-region))


;; python ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(autoload 'python-mode "python-mode" "Python Mode." t)
;(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;(add-to-list 'interpreter-mode-alist '("python" . python-mode))

(require 'python-isort)
(add-hook 'python-mode-hook
	  (lambda ()
	    (blacken-mode)
	    (importmagic-mode)
	    (local-set-key (kbd "RET") 'newline-and-indent)
	    (python-isort-on-save-mode)
	    (local-set-key (kbd "C-c F") 'importmagic-fix-symbol-at-point)
	    ))

;; text ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; other ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun electric-pair ()
  "Insert character pair without sournding spaces."
  (interactive)
  (let (parens-require-spaces)
    (insert-pair)))

(add-hook 'org-mode-hook
	  (function
	   (lambda ()
	     (org-indent-mode))))

(add-hook 'yaml-mode-hook
	  (function
	   (lambda()
	     (setup-programmer-keys))))
