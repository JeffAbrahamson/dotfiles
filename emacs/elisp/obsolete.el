
  
(defun switch-to-makefile-and-compile ()
  "Change buffer to makefile, then invoke compile. The change buffer just insures that we are compiling in the correct directory."
  (interactive)
  (if (switch-to-makefile)
      (compile "make -k")))



(defun switch-to-makefile-and-compile-with-prompt ()
  "Change buffer to makefile, then invoke compile with prompting for what command to use for compilation. The set buffer just insures that we are compiling in the correct directory."
;;  (interactive concat "sCompile command (" compile-command "): ")
  (interactive)
  (let ((compile-prompt (concat "sCompile command (" compile-command "): "))
        (compile-command-name (read-string "Compile command: " compile-command)))
    (if (switch-to-makefile)
        (compile compile-command-name))))
