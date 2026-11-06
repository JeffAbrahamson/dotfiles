;; Package support. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MELPA package support
;; Add the Melpa archive to the list of available repositories.

(defmacro jma-with-error-handling (description &rest body)
  "Execute BODY and report any errors using DESCRIPTION.
Return the result of BODY on success, or nil if an error occurs."
  (declare (indent 1) (debug t))
  `(condition-case err
       (progn ,@body)
     (error
      (message "Failed to %s: %s" ,description (error-message-string err))
      nil)))

(defun jma-safe-load-file (file)
  "Load FILE and report any errors.
Return non-nil when the file loads successfully."
  (if (file-readable-p file)
      (jma-with-error-handling (format "load file %s" file)
        (load-file file))
    (message "Skipping load of %s: file is not readable." file)
    nil))

(defun jma-safe-load (file &optional noerror nomessage nosuffix must-suffix)
  "Call `load' for FILE and report any errors.
Return non-nil when the load succeeds."
  (jma-with-error-handling (format "load %s" file)
    (load file noerror nomessage nosuffix must-suffix)))

(defun jma-safe-require (feature &optional filename)
  "Require FEATURE (optionally from FILENAME) while handling errors."
  (jma-with-error-handling (format "require %s" feature)
    (require feature filename)))

(when (jma-safe-require 'package)
  (jma-safe-load-file
   (concat (getenv "HOME") "/.dotfiles/elisp/jma-packages.el")))

;; Packages are installed by `make install' (see install-packages.el)
;; rather than here, so that starting Emacs never waits on the network.
;; Emacs has already activated the installed packages by this point.
(when (and (featurep 'package) (boundp 'jma-packages))
  (let ((missing (seq-remove #'package-installed-p jma-packages)))
    (when missing
      (display-warning
       'jma-packages
       (format "Missing packages %S; run `make install' in dotfiles/emacs."
               missing)))))

(add-hook 'after-init-hook 'jma-after-init-hook)
(defun jma-after-init-hook ()
  (jma-with-error-handling "load material-light theme"
    (load-theme 'material-light t))
  (jma-with-error-handling "enable global flycheck mode"
    (global-flycheck-mode 1))
  (custom-set-variables
   '(blacken-line-length 79))
  (setq sqlformat-command 'pgformatter)
  (setq sqlformat-args '("-s2" "-g"))
  (jma-with-error-handling "configure docker via use-package"
    (use-package docker
      :ensure t
      :bind ("C-c d" . docker)))
  (message "After-init completed.")
  )

;; Elpy's RPC server and importmagic's server run in elpy's own venv,
;; which `make install' builds (see install-python-venv.sh).  Nothing
;; activates that venv globally, so formatters, linters and a venv
;; Emacs was started from keep their place on PATH.
;;
;; If the venv gets into a mess: M-x elpy-rpc-reinstall-virtualenv, or
;; delete ~/.emacs.d/elpy/rpc-venv and run `make install' again.
(setq elpy-rpc-python-command "python3")
(setq importmagic-python-interpreter
      (expand-file-name "elpy/rpc-venv/bin/python" user-emacs-directory))

;; Elpy is only needed for Python, so enable it when python.el first
;; loads.  That happens before the first Python buffer runs
;; python-mode-hook, which elpy-enable adds itself to.
(with-eval-after-load 'python
  (jma-with-error-handling "enable elpy"
    (elpy-enable)))
(with-eval-after-load 'elpy
  ;; Use flycheck rather than flymake.
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; My initialisation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((jma-elisp-base (concat (getenv "HOME") "/.dotfiles/elisp/")))
  ;; This should be the first block of this file.
  ;; The file site-begin.el should not be included with
  ;; this distribution.  It is for user customization.
  (let ((site-begin (concat jma-elisp-base "site-begin.el")))
    (if (file-readable-p site-begin)
        (jma-safe-load-file site-begin)
      (message "Skipping site-begin.el; file is not readable.")))
  ;; End of first block

  (put 'eval-expression 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (global-font-lock-mode 1)
  (auto-compression-mode 1)
  (tool-bar-mode 0)
  (jma-safe-require 'iso-cvt)

  ;;(set-default-font ("9x15"))
  (setq inhibit-startup-message t)
  (setq use-gtags nil)
  (setq-default show-trailing-whitespace t)
  (setq default-tab-width 4)
  ; (setq tab-width 4)

  ; I should use expand-file-name here instead of concat.
  (if (file-exists-p jma-elisp-base)
      (progn
        (jma-safe-load-file (concat jma-elisp-base "useful.el"))
        (jma-safe-load-file (concat jma-elisp-base "mode-hooks.el"))
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
        (jma-safe-load-file (concat jma-elisp-base "dict.el"))
        (jma-safe-load-file (concat jma-elisp-base "evan.el"))
        (jma-safe-load-file (concat jma-elisp-base "jma.el"))
        (jma-safe-load-file (concat jma-elisp-base "misc.el"))
	;; copied from https://github.com/fxbois/web-mode.git
	;; These two are big enough to slow start-up, so load them on
	;; first use.  mode-hooks.el maps .html to web-mode.
	(autoload 'web-mode (concat jma-elisp-base "web-mode")
	  "Major mode for editing web templates." t)
        (jma-safe-load-file (concat jma-elisp-base "pdftools.el"))
	(autoload 'protobuf-mode (concat jma-elisp-base "protobuf-mode")
	  "Major mode for editing Protocol Buffers." t)
	(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))
        (jma-safe-load-file (concat jma-elisp-base "google-c-style.el"))
        (jma-safe-load-file (concat jma-elisp-base "clang-format.el"))
	(setq clang-format-style-option "Google")
        (jma-safe-load-file (concat jma-elisp-base "highlight-indentation.el"))
	;; cf. https://github.com/spotify/dockerfile-mode
	;; (load-file (concat jma-elisp-base "dockerfile-mode.el"))
	(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
	))

  ;; Downloaded and installed manually due to strange bug in
  ;; emacs 26.1 install.
  (jma-safe-load "auctex.el" nil t t)
  (jma-safe-load "preview-latex.el" nil t t)

  ;; Set frame title so that ratpoison-gtd can record something about
  ;; what I'm doing.
  (setq frame-title-format '("" invocation-name "@" system-name " - %b : %f"))

  ;; This should be the last block of this file.
  ;; The file site-end.el should not be included with
  ;; this distribution.  It is for user customization.
  (let ((site-end (concat jma-elisp-base "site-end.el")))
    (if (file-readable-p site-end)
        (jma-safe-load-file site-end)
      (message "Skipping site-end.el; file is not readable.")))

  )
