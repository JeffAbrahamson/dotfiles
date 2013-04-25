; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         dict.el
; Description:  Dict access functions
; Author:       Evan Sultanik
; Created:      Sun Aug 14 15:44:57 2005
; Language:     Emacs-Lisp
; Package:      N/A
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; *******************************************************
;; ***** THIS IS AN ALPHA TEST VERSION (Version 0.1) *****
;; *******************************************************
;;
;; dict.el
;; Copyright (C) 2005 Evan Sultanik (http://www.sultanik.com/)
;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar dict-protocol-client "dict"
  "This is the name and/or path to the local copy of the DICT protocol client.")

(defconst dict-scratch-buffer-name "*dict*"
  "This is the name of the buffer in which the dict output is displayed.")

(defun dict-extract-word ()
  "From the current buffer, extract and return the word under the cursor."
  (let (start word)
    (save-excursion
      (forward-char 1)
      (backward-word 1)
      (setq start (point))
      (forward-char 1)
      (if (not (re-search-forward "\\b"))
	  (error "Can't find end of word"))
      (buffer-substring start (point))
      )))

(defun dict-lookup-word (word dict)
  "Look up the word WORD using the client, given by
`dict-protocol-client'.  The results will be displayed in the buffer
given by `dict-scratch-buffer-name'.  If DICT is nil, WORD is looked
up from a thesaurus only."
  (interactive "sWord to lookup? \nP")
  (message "Looking up `%s' in the %s..." word (if (null dict) "thesaurus" "dictionary"))
  (let ((dict-buffer (get-buffer-create dict-scratch-buffer-name)))
    (save-excursion
      ; buffer-flush-undo ==> buffer-disable-undo as of emacs 20 or so
      ;(buffer-flush-undo (set-buffer dict-buffer))
      (set-buffer dict-buffer)
      (setq buffer-read-only nil)
      (setq disable-point-adjustment t)
      (erase-buffer)
      (display-buffer dict-buffer)
      (if (null dict)
	  (call-process dict-protocol-client
			nil ;; no infile
			t   ;; put output in the current buffer
			t   ;; re-display as we get more output
			"-P" "-" "-d" "moby-thes" word)
	  (call-process dict-protocol-client
			nil ;; no infile
			t   ;; put output in the current buffer
			t   ;; re-display as we get more output
			"-P" "-" word)
	  )
      (setq buffer-read-only t)
      (goto-char (point-min))
      )
    )
  (message "")
  )

(defun thesaurus-lookup-word (word)
  (dict-lookup-word word nil))

(defun dictionary-lookup-word (word)
  (dict-lookup-word word t))

(defun thesaurus-lookup-word-in-text (exact)
  "Like `dict-lookup-word', but uses the word under the cursor."
  (interactive "P")
  (thesaurus-lookup-word (dict-extract-word)))

(defun dictionary-lookup-word-in-text (exact)
  "Like `dict-lookup-word', but uses the word under the cursor."
  (interactive "P")
  (dictionary-lookup-word (dict-extract-word)))
