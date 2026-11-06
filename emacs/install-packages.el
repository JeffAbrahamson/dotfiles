;; Install any missing packages from jma-packages.  Run by `make
;; install' as: emacs -Q --batch --load install-packages.el

(load (expand-file-name "elisp/jma-packages.el"
                        (file-name-directory load-file-name))
      nil t)
(package-initialize)

(let ((missing (seq-remove #'package-installed-p jma-packages))
      (failed nil))
  (if (null missing)
      (message "All Emacs packages are installed.")
    (package-refresh-contents)
    ;; Carry on past a package that fails, so that one package
    ;; disappearing from its archive doesn't stop the others.
    (dolist (package missing)
      (message "Installing Emacs package %s" package)
      (condition-case err
          (package-install package)
        (error
         (message "Failed to install %s: %s"
                  package (error-message-string err))
         (push package failed)))))
  (when failed
    (message "Emacs packages not installed: %S" (nreverse failed))
    (kill-emacs 1)))
