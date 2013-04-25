
;; (autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
(load-file "~/elisp/html-helper-mode.el")
(setq auto-mode-alist
      (cons
       '("\\.html$" . html-helper-mode)
	auto-mode-alist))
(setq auto-mode-alist
      (cons
       '("mutt-[a-z0-9]+-[0-9]+-[0-9]+-[0-9]+" . post-mode)
	auto-mode-alist))

(if (equal default-directory
	   "~/Mutt/")
    (server-start))
(if (equal default-directory
	   "/home2/jeff/Mutt/")
    (server-start))


(set-mouse-color "blue")
(set-cursor-color "purple")
(column-number-mode 1)

(display-time)

;; Wrong data, fix
;(setq calendar-latitude 40.1)
;(setq calendar-longitude -88.2)
;(setq calendar-location-name "Urbana, IL")

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(TeX-newline-function (quote newline-and-indent))
 '(mutt-alias-file-list (quote ("~/.mutt/aliases"))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

;(setq file-coding-system-alist (cons
;				(cons "mutt-" (cons 'iso-8859-1 'iso-8859-1))
;				file-coding-system-alist))

;(setq file-coding-system-alist (cdr
;				file-coding-system-alist))

;;;;;;;;;;;;;;;;;;;;
;; set up unicode
(prefer-coding-system       'utf-8)
;(set-default-coding-systems 'utf-8)
;; From Emacs wiki
;(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
;; MS Windows clipboard is UTF-16LE 
; (set-clipboard-coding-system 'utf-16le-dos)


;(setq octave-block-offset 8)

;(setq frame-title-format "%f")
(setq transient-mark-mode nil)
(setq browse-url-browser-function 'browse-url-generic
          browse-url-generic-program "chromium-browser")
