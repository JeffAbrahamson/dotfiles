;; This should be the first block of this file.
;; The file site-begin.el should not be included with
;; this distribution.  It is for user customization.
(if (file-readable-p (concat (getenv "HOME") "/elisp/site-begin.el"))
    (load-file (concat (getenv "HOME") "/elisp/site-begin.el")))
;; End of first block

(put 'eval-expression 'disabled nil)
(put 'downcase-region 'disabled nil)
(global-font-lock-mode 1)
(auto-compression-mode 1)
(tool-bar-mode 0)
(require 'iso-cvt)

;;(set-default-font ("9x15"))
(setq inhibit-startup-message t)


(load-file (concat (getenv "HOME") "/elisp/useful.el"))
(load-file (concat (getenv "HOME") "/elisp/mode-hooks.el"))
(load-file (concat (getenv "HOME") "/elisp/post.el"))
(load-file (concat (getenv "HOME") "/elisp/mutt-alias.el"))
(load-file (concat (getenv "HOME") "/elisp/dict.el"))
(load-file (concat (getenv "HOME") "/elisp/evan.el"))
(load-file (concat (getenv "HOME") "/elisp/jma.el"))
(load-file (concat (getenv "HOME") "/elisp/misc.el"))

;; This should be the last block of this file.
;; The file site-end.el should not be included with
;; this distribution.  It is for user customization.
(if (file-readable-p (concat (getenv "HOME") "/elisp/site-end.el"))
    (load-file (concat (getenv "HOME") "/elisp/site-end.el")))
