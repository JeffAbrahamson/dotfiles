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


  (load-file (concat jma-elisp-base "useful.el"))
  (load-file (concat jma-elisp-base "mode-hooks.el"))
  (load-file (concat jma-elisp-base "post.el"))
  (load-file (concat jma-elisp-base "mutt-alias.el"))
  (load-file (concat jma-elisp-base "dict.el"))
  (load-file (concat jma-elisp-base "evan.el"))
  (load-file (concat jma-elisp-base "jma.el"))
  (load-file (concat jma-elisp-base "misc.el"))

  ;; cf. https://github.com/spotify/dockerfile-mode
  (add-to-list 'load-path jma-elisp-base)
  (require 'dockerfile-mode)
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

  ;; Set frame title so that ratpoison-gtd can record something about
  ;; what I'm doing.
  (setq frame-title-format '("" invocation-name "@" system-name " - %b : %f"))

  ;; This should be the last block of this file.
  ;; The file site-end.el should not be included with
  ;; this distribution.  It is for user customization.
  (if (file-readable-p (concat jma-elisp-base "site-end.el"))
      (load-file (concat jma-elisp-base "site-end.el")))

  )
