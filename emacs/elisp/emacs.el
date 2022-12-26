;; Package support. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MELPA package support
;; Add the Melpa archive to the list of available repositories.
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(defvar jma-packages
  '(
    auctex
    blacken
    material-theme
    elpy
    ein	         ;; cf. https://github.com/millejoh/emacs-ipython-notebook
    flycheck	 ;; Notably, instead of flymake.
    magit        ;; git support.
    math-preview ;; For use with ein.  https://gitlab.com/matsievskiysv/math-preview
    )
  )
;(setq package-load-list (append package-load-list jma-packages))

;; The elpy package does some setup that I once got confused.  I was
;; able to trigger redoing the setup with
;; M-x elpy-rpc-reinstall-virtualenv .

(add-hook 'after-init-hook 'jma-after-init-hook)
(defun jma-after-init-hook ()
  (load-theme 'material-light t)
  (elpy-enable)
  ;; Enable Flycheck
  (when (require 'flycheck nil t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode)
    (global-flycheck-mode 1))
  (message "After-init completed.")
  )

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;; Scans the list in jma-packages
;; If the package listed is not already installed, install it
(mapc #'(lambda (package)
          (unless (package-installed-p package)
	    (message (concat "Installing " (symbol-name package)))
            (package-install package)
	    (message (concat "Completed install of " (symbol-name package)))
	    ))
      jma-packages)
(message "Package installation completed.")

;; My initialisation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((jma-elisp-base (concat (getenv "HOME") "/.dotfiles/elisp/")))
  ;; This should be the first block of this file.
  ;; The file site-begin.el should not be included with
  ;; this distribution.  It is for user customization.
  (if (file-readable-p (concat jma-elisp-base "site-begin.el"))
      (load-file (concat jma-elisp-base "site-begin.el")))
  ;; End of first block

  (put 'eval-expression 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (global-font-lock-mode 1)
  (auto-compression-mode 1)
  (tool-bar-mode 0)
  (require 'iso-cvt)

  ;;(set-default-font ("9x15"))
  (setq inhibit-startup-message t)
  (setq use-gtags nil)
  (setq-default show-trailing-whitespace t)
  (setq default-tab-width 4)
  ; (setq tab-width 4)

  (if (file-exists-p jma-elisp-base)
      (progn
	(load-file (concat jma-elisp-base "useful.el"))
	(load-file (concat jma-elisp-base "mode-hooks.el"))
	;; Post mode seems to have been abandonned in 2014 or so.
	;; It was written for emacs 20.
	;; Its use of old-style backticks causes emacs 27 to fail at
	;; startup in 8/2021.  So don't include it, but keep it in git
	;; in case I some day need something like this and don't find
	;; a better alternative.
	;;;; (load-file (concat jma-elisp-base "post.el"))
	;; The mutt-alias package uses cl, which is deprecated.
	;; Since I'm not using mutt-alias, don't load this for now.
	;;;; (load-file (concat jma-elisp-base "mutt-alias.el"))
	(load-file (concat jma-elisp-base "dict.el"))
	(load-file (concat jma-elisp-base "evan.el"))
	(load-file (concat jma-elisp-base "jma.el"))
	(load-file (concat jma-elisp-base "misc.el"))
	;; copied from https://github.com/fxbois/web-mode.git
	(load-file (concat jma-elisp-base "web-mode.el"))
	(load-file (concat jma-elisp-base "pdftools.el"))
	(load-file (concat jma-elisp-base "protobuf-mode.el"))
	(load-file (concat jma-elisp-base "google-c-style.el"))
	(load-file (concat jma-elisp-base "clang-format.el"))
	(load-file (concat jma-elisp-base "highlight-indentation.el"))

	;; cf. https://github.com/spotify/dockerfile-mode
	(load-file (concat jma-elisp-base "dockerfile-mode.el"))
	(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
	))

  ;; Downloaded and installed manually due to strange bug in
  ;; emacs 26.1 install.
  (load "auctex.el" nil t t)
  (load "preview-latex.el" nil t t)

  ;; Set frame title so that ratpoison-gtd can record something about
  ;; what I'm doing.
  (setq frame-title-format '("" invocation-name "@" system-name " - %b : %f"))

  ;; This should be the last block of this file.
  ;; The file site-end.el should not be included with
  ;; this distribution.  It is for user customization.
  (if (file-readable-p (concat jma-elisp-base "site-end.el"))
      (load-file (concat jma-elisp-base "site-end.el")))

  )
