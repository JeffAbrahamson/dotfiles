
;(if (equal default-directory
;	   "~/Mutt/")
;    (server-start))
;(if (equal default-directory
;	   "/home2/jeff/Mutt/")
;    (server-start))

(set-mouse-color "blue")
(set-cursor-color "purple")
(column-number-mode 1)

(display-time)

;; Wrong data, fix
;(setq calendar-latitude 40.1)
;(setq calendar-longitude -88.2)
;(setq calendar-location-name "Urbana, IL")

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
          browse-url-generic-program "firefox")
