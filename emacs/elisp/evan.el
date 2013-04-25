;; Some code contributed by Evan Sultanik on 8 December 2005.

;; Word count!
(defun word-count (&optional filename)
  "Returns the word count of the current buffer.  If `filename' is not nil,
+returns the word count of that file."
  (interactive)
  (save-some-buffers) ;; Make sure the current buffer is saved
  (let ((tempfile nil))
    (if (null filename)
        (progn
          (let ((buffer-file (buffer-file-name))
                (lcase-file (downcase (buffer-file-name))))
            (if (and (>= (length lcase-file) 4) (string= (substring lcase-file
								    +-4 nil) ".tex"))
                ;; This is a LaTeX document, so DeTeX it!
                (progn
                  (setq filename (make-temp-file "wordcount"))
                  (shell-command-to-string (concat "detex < " buffer-file " > "
						   +filename))
                  (setq tempfile t))
              (setq filename buffer-file)))))
    (let ((result (car (string-split " " (shell-command-to-string (concat "wc -w
+" filename)) 1))))
      (if tempfile
          (delete-file filename))
      (message (concat "Word Count: " result))
      )))


