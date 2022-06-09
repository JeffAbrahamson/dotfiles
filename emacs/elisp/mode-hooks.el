

(line-number-mode 1)
;; Useful, but disappeared
;(resize-minibuffer-mode 1) 
(setq default-major-mode 'text-mode)

;; I usually want my .h files in C++ mode.
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("mutt-[a-z0-9]+-[0-9]+-[0-9]+-[0-9]+" . post-mode))
;; YAML mode
;; https://raw.githubusercontent.com/yoshiki/yaml-mode/master/yaml-mode.el
; (require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.sls\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))

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

;; I think this only runs when launching python interpretter, not when
;; init'ing a buffer with python code (in python-mode).  So that's not
;; what I mean.
(add-hook 'python-mode-hook
	  (lambda ()
	    ;;(local-set-key "\"" 'electric-pair)
	    ;;(local-set-key "\'" 'electric-pair)
	    ;;(local-set-key "(" 'electric-pair)
	    ;;(local-set-key "[" 'electric-pair)
	    ;;(local-set-key "{" 'electric-pair)

	    (local-set-key [C-m] 'newline-and-indent)
	    ))

(when (load "flymake" t)
  (defun flymake-flake8-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "flake8" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-flake8-init))
)
(add-hook 'find-file-hook 'flymake-find-file-hook)
(setq python-check-command "/usr/bin/flake8")
;; display flymake messages for cursor line in minibuffer
;(require 'flymake-cursor)

;; python-outline no longer present at emacs 27.1 apparently.
;(setq auto-mode-alist (append '(("\\.py" . python-outline)) auto-mode-alist))

(defun electric-pair ()
  "Insert character pair without sournding spaces"
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
