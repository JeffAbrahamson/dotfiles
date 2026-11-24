
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
;; Set up Unicode

(prefer-coding-system 'utf-8)

;; Work around a Chromium/Electron Wayland clipboard bug.
;; Chromium requests TEXT even when UTF8_STRING is available,
;; but interprets the returned ISO-8859-1 bytes as UTF-8.
;; Force UTF-8 for TEXT and text/plain selection requests.
;; Confirmed with Signal Desktop, November 2026.

(with-eval-after-load 'select
  (defun my-selection-convert-to-utf8 (selection _type value)
    "Export text selections as UTF-8 regardless of the requested text type."
    (xselect-convert-to-string selection 'UTF8_STRING value))

  (dolist (target '(TEXT text/plain))
    (setf (alist-get target selection-converter-alist)
          #'my-selection-convert-to-utf8)))

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

(defun next-error-uses-highlight-mode()
  "Turn on highlight mode in most recent compilation/grep/etc. buffer."
  (with-current-buffer next-error-last-buffer
    (hl-line-mode 1)))

(add-hook 'next-error-hook 'next-error-uses-highlight-mode)
