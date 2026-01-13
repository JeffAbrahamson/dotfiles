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

(let ((package-system-available (jma-safe-require 'package)))
  (if package-system-available
      (add-to-list 'package-archives
                   '("melpa" . "http://melpa.org/packages/") t)
    (message "Skipping package archive configuration; package.el unavailable.")))

;; Reminder: M-x package-list-packages
(defvar jma-packages
  '(
    apheleia     ;; Better than black.  https://github.com/radian-software/apheleia
    auctex
    blacken      ;; Alternative to py-autopep8.
    docker
    docker-compose-mode
    dockerfile-mode
    material-theme
    editorconfig
    elpy
    ein	         ;; cf. https://github.com/millejoh/emacs-ipython-notebook
    importmagic  ;; For use with elpy.
    flycheck	 ;; Notably, instead of flymake.
    markdown-mode ;; Requires an external Markdown renderer such as pandoc (not installed here).
    magit        ;; git support.
    math-preview ;; For use with ein.  https://gitlab.com/matsievskiysv/math-preview
    python-isort ;; For use with elpy.
    yaml-mode
    sqlformat    ;; Requires sqlformat or pgformatter to be installed.
    )
  )
;(setq package-load-list (append package-load-list jma-packages))

;; The elpy package does some setup that I once got confused.  I was
;; able to trigger redoing the setup with
;; M-x elpy-rpc-reinstall-virtualenv .
;;
;; I can refresh packages manually with M-x package-refresh-contents .
;; I can list packages with M-x package-list-packages .

(add-hook 'after-init-hook 'jma-after-init-hook)
(defun jma-after-init-hook ()
  (jma-with-error-handling "load material-light theme"
    (load-theme 'material-light t))
  (jma-with-error-handling "enable elpy"
    (elpy-enable))
  ;; Enable Flycheck
  (when (jma-safe-require 'flycheck)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode)
    (jma-with-error-handling "enable global flycheck mode"
      (global-flycheck-mode 1)))
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

(when (featurep 'package)
  (jma-with-error-handling "initialize package system"
    (package-initialize))
  (when (not package-archive-contents)
    (jma-with-error-handling "refresh package archive contents"
      (package-refresh-contents))))

;; Load MELPA packages.
;; Scans the list in jma-packages
;; If the package listed is not already installed, install it
(if (featurep 'package)
    (progn
      (message "Checking for package installation.")
      (mapc #'(lambda (package)
                (let ((installed (package-installed-p package)))
                  (message "  Checking %s...%s"
                           (symbol-name package)
                           (if installed "installed" "not installed"))
                  (unless installed
                    (message "Installing %s" (symbol-name package))
                    (when (jma-with-error-handling (format "install package %s" package)
                            (package-install package))
                      (message "Completed install of %s" (symbol-name package))))))
            jma-packages)
      (message "Package installation completed."))
  (message "Skipping package installation; package.el unavailable."))

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
        (jma-safe-load-file (concat jma-elisp-base "web-mode.el"))
        (jma-safe-load-file (concat jma-elisp-base "pdftools.el"))
        (jma-safe-load-file (concat jma-elisp-base "protobuf-mode.el"))
        (jma-safe-load-file (concat jma-elisp-base "google-c-style.el"))
        (jma-safe-load-file (concat jma-elisp-base "clang-format.el"))
	(setq clang-format-style-option "Google")
        (jma-safe-load-file (concat jma-elisp-base "highlight-indentation.el"))
	;; (load-file (concat jma-elisp-base "copilot.el"))
	;; (load-file (concat jma-elisp-base "copilot-bindings.el"))

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

  ;; I was having problems with elpy not having correct python
  ;; information because of debian policy of not installing ensurepip
  ;; globally.  This is intended to address that by making sure I
  ;; always use the elpy python venv.
  (jma-with-error-handling "activate elpy python virtualenv"
    (pyvenv-activate "~/.emacs.d/elpy/rpc-venv"))

  ;; This should be the last block of this file.
  ;; The file site-end.el should not be included with
  ;; this distribution.  It is for user customization.
  (let ((site-end (concat jma-elisp-base "site-end.el")))
    (if (file-readable-p site-end)
        (jma-safe-load-file site-end)
      (message "Skipping site-end.el; file is not readable.")))

  )
