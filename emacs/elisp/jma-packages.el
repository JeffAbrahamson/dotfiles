;; ELPA packages my configuration expects.  Shared by emacs.el, which
;; only warns about missing ones, and ../install-packages.el, which
;; `make install' runs to install them, so that starting Emacs never
;; waits on the network.

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; Reminder: M-x package-list-packages
(defvar jma-packages
  '(
    apheleia     ;; Better than black.  https://github.com/radian-software/apheleia
    auctex
    blacken      ;; Alternative to py-autopep8.
    docker
    dockerfile-mode
    material-theme
    editorconfig
    elpy
    ein	         ;; cf. https://github.com/millejoh/emacs-ipython-notebook
    importmagic  ;; For use with elpy.
    flycheck	 ;; Notably, instead of flymake.
    magit        ;; git support.
    math-preview ;; For use with ein.  https://gitlab.com/matsievskiysv/math-preview
    python-isort ;; For use with elpy.
    yaml-mode
    sqlformat    ;; Requires sqlformat or pgformatter to be installed.
    )
  "ELPA packages that `make install' installs if they are missing.")
;; docker-compose-mode was here, but MELPA no longer has it.  Compose
;; files fall back to yaml-mode.
