(if (not (eq window-system nil))
    (global-unset-key (kbd "C-z")))

(defun jma-grep (pattern prefix)
  "Search for pattern in the files $HOME/data/notes/*txt.
Search is case insensitive unless  a prefix argument is provided,
in which case the search becomes case-sensitive."
  (interactive "sPattern: \np")
  (let ((flag (if (not (= prefix 1)) "" "-i"))
	(args (split-string pattern)))
    (let ((cmd (concat "grep " flag
		       " -n -e \"" (car args) "\" "
		       (getenv "HOME")
		       "/data/notes/*.txt")))
      (compilation-start
       (jma-grep-sub cmd flag (cdr args))
       'grep-mode))))

(defun jma-grep-sub (cmd flag args)
  "cmd is a grep command, args are more things to grep for in the results.
flag should be valid grep flags (typically '-i') or else the empty string."
  (message cmd)
  (if args
      (jma-grep-sub (concat cmd "| grep " flag " -e " (car args))
		   flag
		   (cdr args))
    cmd))

(global-set-key [f9] 'jma-grep)


(require 'add-log)

(defun jma-doc-note ()
  "Insert date string followed by four blank lines at beginning
  of file, placing mark two lines after."
  (interactive)
  (goto-char (point-min))
  (insert "** [" (funcall add-log-time-format) "]\n\n\n\n\n")
  (backward-char 3))

(global-set-key [C-f12] 'jma-doc-note)

(defun jma-doc-update ()
  "Insert date string followed by four blank lines at beginning
  of file, placing mark two lines after."
  (interactive)
  (beginning-of-line)
  (insert " ** Updated [" (funcall add-log-time-format) "]\n\n\n")
  (backward-char 2))

(global-set-key [C-S-f12] 'jma-doc-update)


(defun jma-daily-log ()
  "Find my daily change log and add an entry about what I did today"
  (interactive)
  (let ((daily-log-file (concat (getenv "HOME") "/data/daily.log")))
    (find-file daily-log-file)
    (add-change-log-entry nil daily-log-file nil t)))

(defun jma-weekly-log ()
  "Find my weekly change log and add an entry"
  (interactive)
  (let ((weekly-log-file (concat (getenv "HOME") "/data/weekly.log")))
    (find-file weekly-log-file)
    (goto-char (point-min))
    (insert (format-time-string "%Y-%m-%d") "    " "\n")
    (goto-char (point-min))
    (end-of-line)))

(global-set-key [M-f12] 'jma-daily-log)
(global-set-key [M-S-f12] 'jma-weekly-log)

(defun c-jellybooks-cc-init ()
  "Insert header for use with Jellybooks C++ code."
  (interactive)
  ;; I think I should be able to do this with a lexical-let on
  ;; gpl-for-me and then calling c-header-init, but I'm not getting it
  ;; to work.  So instead I copy and paste.
  (if (= (point-min) (point-max))
      (progn
	(insert (concat
		 "/*\n"
		 "  Copyright " (format-time-string "%Y") "  Jellybooks"
		"\n*/\n\n"))
	(if (string-match "\\.h$" (buffer-name))
	    (let ((guard-name (upcase
			       (concat "__"
				       (replace-regexp-in-string "\\." "_" (buffer-name))
				       "__"))
			      ))
	      (insert (concat
		       "#ifndef " guard-name "\n"
		       "#define " guard-name " 1\n"
		       "\n\n\n"
		       "namespace analytics {\n"
		       "\n\n\n"
		       "}  // namespace analytics\n"
		       "\n"
		       "#endif  /* " guard-name "*/\n"))
	      (beginning-of-line -4)))
	(if (string-match "\\.cc$" (buffer-name))
	    (let ((include-name
		   (concat (replace-regexp-in-string "\\.cc" ".h" (buffer-name)))))
	      (insert (concat
		       "#include \"" include-name "\"\n"
		       "\n\n\n"
		       "namespace analytics {\n"
		       "\n"
		       "namespace {\n"
		       "\n"
		       "}  // namespace\n"
		       "\n\n\n"
		       "}  // namespace analytics\n"))
	      (beginning-of-line -2))))
    (message "Buffer is not empty.")))


(defun py-jellybooks-py-init ()
  "Insert header for use with Jellybooks python code."
  (interactive)
  ;; I think I should be able to do this with a lexical-let on
  ;; gpl-for-me and then calling c-header-init, but I'm not getting it
  ;; to work.  So instead I copy and paste.
  (if (= (point-min) (point-max))
      (insert (concat
	       "\"\"\"Copyright " (format-time-string "%Y") "  Jellybooks"
	       "\n\n\"\"\"\n\n"
	       "import logging\n\n"
	       "logger = logging.getLogger(\"django\")\n\n"
	       ))
    (message "Buffer is not empty.")
    ))


(defun gpl-for-me (prog-name)
  "Create a GPL string for a module of prog-name."
  (let ((copyright-string (replace-regexp-in-string "%d" (format-time-string "%Y")
						   "Copyright %d  Jeff Abrahamson")))
    (let ((gpl-list (list copyright-string
			  ""
			  "This file is part of %s."
			  ""
			  "%s is free software: you can redistribute it and/or modify"
			  "it under the terms of the GNU General Public License as published by"
			  "the Free Software Foundation, either version 3 of the License, or"
			  "(at your option) any later version."
			  ""
			  "%s is distributed in the hope that it will be useful,"
			  "but WITHOUT ANY WARRANTY; without even the implied warranty of"
			  "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the"
			  "GNU General Public License for more details."
			  ""
			  "You should have received a copy of the GNU General Public License"
			  "along with %s.  If not, see <http://www.gnu.org/licenses/>."
			  )))
    (replace-regexp-in-string "%s" prog-name
			      (mapconcat 'identity gpl-list "\n")))))



(defun c-header-init (prog-name)
  "Insert standard copyright and header protection code"
  (interactive "*sProgram Name:  ")
  (if (= (point-min) (point-max))
      (progn
	(insert (concat
		 "/*\n"
		 (replace-regexp-in-string "^" "  " (gpl-for-me prog-name))
		"\n*/\n\n\n\n"))
	(if (string-match "\\.h$" (buffer-name))
	    (let ((guard-name (upcase
			       (concat "__"
				       (replace-regexp-in-string "\\." "_" (buffer-name))
				       "__"))
			      ))
	      (insert (concat
		       "#ifndef " guard-name "\n"
		       "#define " guard-name " 1\n"
		       "\n\n\n"
		       "#endif  /* " guard-name "*/\n")))))
    (message "Buffer is not empty.")))


(defun py-header-init (prog-name)
  "Insert standard copyright and header protection code"
  (interactive "*sProgram Name:  ")
  (if (= (point-min) (point-max))
      (progn
	(insert (concat
		 "#!/usr/bin/python\n\ngpl=\"\"\"\n"
		 (gpl-for-me prog-name)
		"\n\"\"\"\n\n\n\n")))
    (message "Buffer is not empty.")))


(defun c++-accessor (m_name m_type)
  "Add accessor functions for member"
  (interactive "*sMember Name: \nsType: ")
  (let ((f_name (replace-regexp-in-string "m_" "" m_name)))
    (beginning-of-line)
    (newline-and-indent)
    (insert (concat "const " m_type " &" f_name "() const { return " m_name "; };"))
    (newline-and-indent)
    (insert (concat "void " f_name "(const " m_type " &in) { " m_name " = in; };\n"))))

(defun jma-insert-jma-comment ()
  "Insert a JMA-ish comment that I don't want to miss removing at commit."
  ;; As written, this will do the wrong thing on regions, where this becomes
  ;; comment-region.  The intent is to handle the case when no region is active.
  (interactive)
  (comment-dwim nil)
  (insert "JMA JMA JMA  "))

(defun jma-dnc ()
  "Insert a JMA-ish comment that I don't want to miss removing at commit."
  ;; As written, this will do the wrong thing on regions, where this becomes
  ;; comment-region. The intent is to handle the case when no region is active.
  (interactive)
  (comment-dwim nil)
  (insert "JMA  [DO NOT COMMIT]  "))

(defun jma-todo ()
  "Insert a TODO comment for me."
  ;; As written, this will do the wrong thing on regions, where this becomes
  ;; comment-region. The intent is to handle the case when no region is active.
  (interactive)
  (comment-dwim nil)
  (insert "TODO(jeff@p27.eu): "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org mode and GTD
(defun jma-org-files ()
  "Return a list of my GTD org files."
  (file-expand-wildcards (concat (getenv "HOME") "/data/org/*org") t))

;; The function org-todo-list looks at this list.
(setq org-agenda-files (jma-org-files))
(setq browse-url-generic-program "firefox")
(global-set-key (kbd "C-c g") 'org-todo-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pull from PRIMARY selection.  This is the same as middle mouse
;; click, but gtags overrides click-2 in a way that I haven't quite
;; figured out how to fix.
(if use-gtags
    (defun paste-primary-selection ()
      (interactive)
      (insert
       (x-get-selection 'PRIMARY)))
  (global-set-key (kbd "S-<insert>") 'paste-primary-selection)
  )

(setq calendar-week-start-day 1
      calendar-day-name-array ["Dimanche" "Lundi" "Mardi" "Mercredi"
			       "Jeudi" "Vendredi" "Samedi"]
      calendar-month-name-array ["Janvier" "Février" "Mars" "Avril" "Mai"
				 "Juin" "Juillet" "Août" "Septembre"
				 "Octobre" "Novembre" "Décembre"])

;; Set up some buffers that I like to have handy.
(progn
  (switch-to-buffer "tex.txt")
  (set-input-method "TeX")
  (auto-fill-mode 0))
(progn
  (switch-to-buffer "cn.txt")		; ISO 3166-1
  (set-input-method "chinese-py")
  (flyspell-mode 0)
  (auto-fill-mode 0))
;; Some other options:
;;   * chinese-py-punct
;;   * chinese-py-gb
;; Consider setting up pyim.
;; (use-package pyim
;;   :config
;;   (setq default-input-method "pyim"))

(progn
  (switch-to-buffer "en.txt")
  (flyspell-mode 1)
  (ispell-change-dictionary "en_GB")
  (auto-fill-mode 0))
(progn
  (switch-to-buffer "fr.txt")
  (set-input-method "latin-9-prefix")
  (flyspell-mode 1)
  (ispell-change-dictionary "francais")
  (auto-fill-mode 0))
