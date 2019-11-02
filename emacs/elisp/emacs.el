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

  (if (file-exists-p jma-elisp-base)
      (progn
	(load-file (concat jma-elisp-base "useful.el"))
	(load-file (concat jma-elisp-base "mode-hooks.el"))
	(load-file (concat jma-elisp-base "post.el"))
	(load-file (concat jma-elisp-base "mutt-alias.el"))
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
	(load-file (concat jma-elisp-base "puppet-mode.el"))
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
