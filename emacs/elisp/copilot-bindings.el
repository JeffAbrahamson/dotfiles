;;; Copilot --- Emacs configuration for Copilot

;;; Commentary:
;; Some of this is inspired by https://robert.kra.hn/posts/2023-02-22-copilot-emacs-setup/

;;; Code:
(defvar jma/copilot-manual-mode nil
  "When t will only show completions when manually triggered, e.g. via M-C-<return>.")

(defun copilot-change-activation ()
  "Switch between three activation modes:
-> automatic: copilot will automatically overlay completions
-> manual: you need to press a key (M-C-<return>) to trigger completions
-> off: copilot is completely disabled."
  (interactive)
  (if (and copilot-mode jma/copilot-manual-mode)
      (progn
        (message "deactivating copilot")
        (global-copilot-mode -1)
        (setq jma/copilot-manual-mode nil))
    (if copilot-mode
        (progn
          (message "activating copilot manual mode")
          (setq jma/copilot-manual-mode t))
      (message "activating copilot mode")
      (global-copilot-mode))))

;; Cycle through the three activation modes.
(define-key global-map (kbd "M-C-<escape>") #'copilot-change-activation)

(defun jma/copilot-complete-or-accept ()
  "Either trigger a completion or accept one if one is available."
  (interactive)
  (if (copilot--overlay-visible)
      (progn
        (copilot-accept-completion)
        ;;(open-line 1)
        ;;(next-line)
	)
    (copilot-complete)))

(defun jma/copilot-tab ()
  "Allow tab to complete with copilot if a completion is available.
Otherwise will do normal tab-indent."
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
      (or (copilot-accept-completion)
	  ;(company-yasnippet-or-completion)
	  (indent-for-tab-command))))

(defun jma/copilot-accept-completion-by-word (&optional arg)
  "Accept the completion by word.

Optional argument is repeat count."
  (interactive)
  (if (copilot--overlay-visible)
      (copilot-accept-completion-by-word (or arg 1))
    (forward-word (or arg 1))))

(defun jma/copilot-accept-completion-by-line (&optional arg)
  "If in copilot overlay, accept ARG lines of the completion (or one if no ARG).
If not in copilot overlay, move forward ARG lines, or one line if no ARG."
  (interactive)
  (if (copilot--overlay-visible)
      (copilot-accept-completion-by-line (or arg 1))
    (progn
      (next-line (or arg 1)))))

;; (define-key copilot-mode-map (kbd "M-<right>") #'copilot-next-completion)
;; (define-key copilot-mode-map (kbd "M-<left>") #'copilot-previous-completion)
(define-key copilot-mode-map (kbd "M-f") #'jma/copilot-accept-completion-by-word)
(define-key copilot-mode-map (kbd "C-n") #'jma/copilot-accept-completion-by-line)
(define-key global-map (kbd "M-<return>") #'jma/copilot-complete-or-accept)
;(define-key copilot-completion-map (kbd "<tab>") #'jma/copilot-complete-or-accept)
;; (define-key global-map (kbd "<tab>") #'jma/copilot-tab)

(defun jma/copilot-quit ()
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

(advice-add 'keyboard-quit :before #'jma/copilot-quit)
(add-hook 'prog-mode-hook 'copilot-mode)

(provide 'copilot)
;;; copilot-bindings.el ends here
