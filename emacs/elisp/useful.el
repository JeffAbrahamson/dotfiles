(setq makefile-name "GNUmakefile")
(setq makefile-name-alt "Makefile")

(global-set-key [C-f1] '(lambda () (interactive) (insert "Stéphane")))
(global-set-key (kbd "<C-$>") '(lambda () (interactive) (insert "£")))
(global-set-key [f2] 'compile-something)

;(global-set-key [(f3)] 'speedbar-get-focus)
(global-set-key [M-f3] 'nuke-crs)
(global-set-key [f4] 'save-buffer)

;; (global-set-key [f5] '(lambda ()
;; 			(interactive)
;; 			(iso-accents-mode 1)
;; 			(flyspell-mode 0)))
;; (global-set-key [C-f5] '(lambda ()
;; 			  (interactive)
;; 			  (iso-accents-mode -1)
;; 			  (flyspell-mode)
;; 			  (auto-fill-mode 1)))
(global-set-key [f5] '(lambda ()
			(interactive)
			(toggle-input-method)
			(flyspell-mode 1)
			(ispell-change-dictionary "francais")
			(auto-fill-mode 0)))
(global-set-key [C-f5] '(lambda ()
			  (interactive)
			  (toggle-input-method)
			  (flyspell-mode)
			  (auto-fill-mode 1)))
(global-set-key [S-f5] 'text-mode)
(global-set-key [f6] '(lambda () (interactive) (flyspell-mode 0)))

;; What kind of text do I want?  Likely "american", "british", or "francais"
;; (There's also "francais-tex" and "deutsch8".)
(set-variable 'debian-ispell-dictionary "american")
(global-set-key [C-f6] 'ispell-change-dictionary)

(global-set-key (quote [f7]) 'word-count)
(global-set-key (quote [C-f7]) 'dictionary-lookup-word-in-text)
(global-set-key (quote [S-f7]) 'thesaurus-lookup-word-in-text)


(global-set-key [f8] 'print-frame-list)
(global-set-key [f9] 'rewrap-paragraph)
(global-set-key [f10] 'calendar)
(global-set-key [C-f10]
		'(lambda
		   ()
		   (interactive)
		   (switch-to-buffer "Makefile")))
(global-set-key [S-f10]
		'(lambda
		   ()
		   (interactive)
		   (switch-to-buffer "*compilation*")))
(global-set-key [f11] 'next-error)
(global-set-key [C-f11] 'calc)

(setq f12-respect-DISPLAY nil)
(setq f12-last (current-time))
(global-set-key [f12] (lambda ()
			"Finish editing and return to caller."
			(interactive)
			(if (not (buffer-file-name))
			    (message "Buffer is not associated with a file")
			  (let ((buf (current-buffer))
				(f12-prev f12-last)
				(f12-cur (current-time)))
			    (setq f12-last f12-cur)
			    (if (elapsed-time f12-prev f12-cur 2)
				(progn
				  (save-buffer buf)
				  (if (or (getenv "DISPLAY") (not f12-respect-DISPLAY))
				      (progn
					(server-edit)
					(kill-buffer buf))
				    (save-buffers-kill-emacs)))
			      (message "Apparent F12 Key bounce: ignored")
			      )))))

;(global-set-key [C-f12] 'drexel-daily-change-log)
;(global-set-key [S-f12] 'drexel-weekly-change-log)


(defun setup-programmer-keys ()
  "Set up some key bindings that are useful while programming."
  (highlight-indentation-mode)
  (if (not buffer-read-only)
      (local-set-key [(return)] 'newline-and-indent)))

; (local-set-key '[M-delete] 'backward-kill-word)
; (local-set-key '(meta backspace) 'backward-kill-word)
; (local-set-key '(meta right) 'end-of-line)
; (local-set-key '(meta left) 'beginning-of-line))

(global-set-key (kbd "C-c o") 'occur)


(defun switch-to-makefile ()
  "Switch to the buffer called makefile-name if it exists, else the buffer called
mkaefile-name-alt. If neither exists, return nil. Else return t."
  (if (bufferp (get-buffer makefile-name))
      (set-buffer makefile-name)
    (if (bufferp (get-buffer makefile-name-alt))
        (set-buffer makefile-name-alt)
      nil)))

(setq compile-command "make -k")

(defun compile-something ()
  "Run latex or else invoke M-x compile"
  (interactive)
  (if (equal (file-name-extension (buffer-file-name)) "tex")
      ;; Compile TeX or LaTeX source, either via a makefile or not.
      (if (makefile-exists-p)
	  (let ((base-name (file-name-sans-extension
			    (file-name-nondirectory
			     (buffer-file-name)))))
	    (let ((pdf-name (concat base-name ".pdf")))
	      (compile (concat "make -k " pdf-name))))
	(latex-compile))
    (let ((compile-prompt (concat "sCompile command (" compile-command "): "))
	  (compile-command-name (read-string
				 "Compile command: "
				 compile-command)))
      ;; Compile anything else.
      (compile compile-command-name))))


(defun latex-compile ()
  "Transform latex file in current buffer to pdf."
  (interactive)
  (let ((latex-name (file-name-nondirectory (buffer-file-name)))
	(base-name (file-name-sans-extension (buffer-file-name))))
    ;; The next let form needs base-name.  Since let binds everything
    ;; simultaneously, the nested form is necessary.
    (let (;(dvi-name (concat base-name ".dvi"))
	    ;(ps-name (concat base-name ".ps"))
	    (pdf-name (concat base-name ".pdf"))
	    (compile-command nil))
      (save-some-buffers)
      ;; (compile (concat "latex -interaction=nonstopmode " latex-name
      ;; 		              " && dvips -t a4 -f " dvi-name " > " ps-name
      ;; 		              " && dvipdfm -p a4 -o " pdf-name " " dvi-name)))))
      ;;;; Before using xelatex:
      ;;(compile (concat "pdflatex -interaction=nonstopmode " latex-name)))))
      (compile (concat "xelatex -interaction=nonstopmode " latex-name)))))


;; (defun latex-compile ()
;;   "Transform latex file in current buffer to pdf and postscript."
;;   (interactive)
;;   (compile (concat "compile-latex" (buff-file-name))))


(setq makefile-name-list '("Makefile" "GNUmakefile" "makefile"))

(defun makefile-exists-p ()
  "Return true if there's a makefile in the current directory."
  (makefile-exists-p-sub  (car makefile-name-list) (cdr makefile-name-list)))


(defun makefile-exists-p-sub (mf-name mf-list)
  ""
  (if (file-exists-p mf-name)
      t
    (if mf-list
	(makefile-exists-p-sub (car mf-list) (cdr mf-list))
      nil)))


(defun nuke-crs ()
  "Remove ascii(0x13) from current buffer."
  (interactive)
  (save-excursion
    (goto-char 0)
    (while (search-forward "" nil t)
      (replace-match "" nil t))))


(defun line-up-backslashes (target-column)
  "Line up backslashes at column specified by ARG."
  (interactive "*NColumn: ")
  (if (or (not (mark)) (= (point) (mark))) (error "No region specified."))
  (save-excursion
    (if (> (point) (mark))
	(exchange-point-and-mark t))
    (forward-char)			; In case we start at beginning of buffer
    (while (< (point) (mark))
      (end-of-line)
      (backward-char)
      (if (= (char-after) (string-to-char "\\"))
	  (while (< (current-column) target-column)
	    (insert "\t")))
      (forward-line 1))))



;;; This won't really do the right thing. We really want most of this to
;;; be executed when the buffer is created, not when emacs starts up
(if (equal default-directory
	   "~/work/journal/")
    (progn
      (global-set-key [f12] 'save-buffers-kill-emacs)
      (end-of-buffer)
      (make-local-variable 'backup-inhibited)
      (setq backup-inhibited t)
      (auto-fill-mode 1)))

  
;;; To make printing text buffers easier
(defun my-print-buffer ()
  "To make printing text buffers easier"
  (interactive)
  (save-excursion
    (shell-command-on-region
     (point-min)
     (point-max)
     "a2ps --portrait --borders=no --columns=1"
     nil)
    (delete-other-windows)))
(global-set-key [C-print] 'my-print-buffer)


(defun rewrap-paragraph ()
  "Lame first draft: combine lines one at a time to let emacs' native
line wrapping algorithms line wrap with whatever prefix starts the
first two lines of the paragraph. Ideally, it should watch to see
where the paragraph ends, and even prompt for a prefix."
  (interactive)
  (delete-backward-char 1)
  (insert " ")
  (end-of-line)
  (forward-char 1))


(defun print-frame-list ()
  ""
  (interactive)
  (message (print-list (frame-list))))

(defun print-list (the-list)
  ""
  (if (consp the-list)
      (concat (frame-parameter (car the-list) 'display)
	      "\n"
	      (print-list (cdr the-list)))
    the-list))

(defun drexel-daily-change-log ()
  "Find my daily change log and add an entry"
  (interactive)
  (find-file "/home/jeff/mst/work/daily.log")
  (add-change-log-entry nil "/home/jeff/mst/work/daily.log" nil t))

(defun drexel-weekly-change-log ()
  "Find my weekly change log and add an entry"
  (interactive)
  (find-file "/home/jeff/mst/work/weekly.log")
  (goto-char (point-min))
  (insert (format-time-string "%Y-%m-%d") "    " "\n")
  (goto-char (point-min))
  (end-of-line))

(defun economist-format ()
  "Change some unicode stuff to ascii, then fill paragraphs between point in mark"
  (interactive)
  (save-excursion
   (if (< (mark) (point))
       (exchange-point-and-mark))
   (let ((start (point))
	 (end (mark)))
     (while (re-search-forward "\\\\u201[cd]" end t)
       ;(replace-match "\"" nil nil))
       (replace-match "\"" nil nil))
     (goto-char start)
     (while (re-search-forward "\\\\u2014" end t)
       (replace-match "--" nil nil))
     (fill-region start end)
     )))

(defun elapsed-time (prev cur elapse)
  "Return t if prev and cur represent times (lists of three ints) no
more than elapse seconds apart.  Else return nil.  This function is
defective, in that it will err if the high bytes are different but
only a short time has passed between the two times."
  ;; High byte, Low byte, and Microseconds 
  (let ((prev-H (car prev))
	(prev-L (second prev))
	(cur-H (car cur))
	(cur-L (second cur)))
    (if (= cur-H prev-H)
	(if (= cur-L prev-L)
	    (if (= elapse 0)
		t
	      (if (= prev-M cur-M)
		  nil
		t))
	  (if (> (- cur-L prev-L) elapse)
	      t
	    nil))
      t)				;This line is in error near
					;the times when the high order
					;number flips.
    ))

;; Don't ask me if the TAGS file has changed, just reload it.
(setq tags-revert-without-query t)

(defun uniquify-lines (&optional start end)
  (interactive "*r")
  (save-excursion
    (shell-command-on-region start end "sort -u" nil t)))

(defun unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single
    logical line.  This is useful, e.g., for use with
    `visual-line-mode'."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))
