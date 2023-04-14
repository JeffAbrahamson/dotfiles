;;; Copilot --- Emacs configuration for Copilot

;;; Commentary:
;; Some of this is inspired by https://robert.kra.hn/posts/2023-02-22-copilot-emacs-setup/

;;; Code:
(defvar rk/copilot-manual-mode nil
  "When t will only show completions when manually triggered, e.g. via M-C-<return>.")

(defun rk/copilot-change-activation ()
  "Switch between three activation modes:
-> automatic: copilot will automatically overlay completions
-> manual: you need to press a key (M-C-<return>) to trigger completions
-> off: copilot is completely disabled."
  (interactive)
  (if (and copilot-mode rk/copilot-manual-mode)
      (progn
        (message "deactivating copilot")
        (global-copilot-mode -1)
        (setq rk/copilot-manual-mode nil))
    (if copilot-mode
        (progn
          (message "activating copilot manual mode")
          (setq rk/copilot-manual-mode t))
      (message "activating copilot mode")
      (global-copilot-mode))))

;; Cycle through the three activation modes.
(define-key global-map (kbd "M-C-<escape>") #'rk/copilot-change-activation)

(defun rk/copilot-complete-or-accept ()
  "Either trigger a completion or accept one if one is available."
  (interactive)
  (if (copilot--overlay-visible)
      (progn
        (copilot-accept-completion)
        (open-line 1)
        (next-line))
    (copilot-complete)))

(define-key copilot-mode-map (kbd "M-C-<next>") #'copilot-next-completion)
(define-key copilot-mode-map (kbd "M-C-<prior>") #'copilot-previous-completion)
(define-key copilot-mode-map (kbd "M-C-<right>") #'copilot-accept-completion-by-word)
(define-key copilot-mode-map (kbd "M-C-<down>") #'copilot-accept-completion-by-line)
(define-key global-map (kbd "M-C-<return>") #'rk/copilot-complete-or-accept)

(defun rk/copilot-tab ()
  "Allow tab to complete with copilot if a completion is available.
Otherwise will do normal tab-indent."
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
      (or (copilot-accept-completion)
	  ;(company-yasnippet-or-completion)
	  (indent-for-tab-command))))

(define-key global-map (kbd "<tab>") #'rk/copilot-tab)

(defun rk/copilot-quit ()
  "Run `copilot-clear-overlay' or `keyboard-quit'.
If copilot is cleared, make sure the overlay doesn't come back
too soon."
  (interactive)
  (condition-case err
      (when copilot--overlay
        (lexical-let ((pre-copilot-disable-predicates copilot-disable-predicates))
          (setq copilot-disable-predicates (list (lambda () t)))
          (copilot-clear-overlay)
          (run-with-idle-timer
           1.0
           nil
           (lambda ()
             (setq copilot-disable-predicates pre-copilot-disable-predicates)))))
    (error handler)))

(advice-add 'keyboard-quit :before #'rk/copilot-quit)


(provide 'copilot)
;;; copilot-bindings.el ends here
