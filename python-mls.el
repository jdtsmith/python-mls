;;; python-mls.el --- Multi-line shell for ()iPython  -*- lexical-binding: t -*-

;; Copyright (C) 2021  J.D. Smith

;; Author: J.D. Smith
;; Homepage: https://github.com/jdtsmith/python-mls
;; Package-Requires: ((emacs "27.1"))
;; Package-Version: 0.0.1
;; Keywords: python

;;; Commentary:

;; Python-MLS provides multi-line shell capatibilities for (i)Python
;; run in an inferior shell, building on the Emacs-bundled python.el's
;; python-mode.
;; 
;; Features:
;; - Works with both python and ipython.
;; - Arbitrary command lengths.
;; - Auto-detects and handles native continuation prompts.
;; - Auto-indents multi-line commands.
;; - Replaces buffer-based fontifications with in-buffer python-mode
;;   fontification for dramatic speedup.
;; - Up/Down arrow history browsing with and without block movement
;;   (try shift arrow).
;; - Saves and restore (multi-line) command history.
;; - Directly kill/yank multi-line code blocks to & from Python
;;   buffers.

;; Python-MLS is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; Python-MLS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:
(require 'python)

(defgroup python-mls-command-setup nil
  "Setup for command parameters of the Python Multi-Line Shell."
  :prefix "Multi-line Shell (MLS)"
  :group 'python)

(defcustom python-mls-save-command-history t
  "Non-nil means preserve command history between sessions.
The file `python-mls-command-history-file' is used to save and restore
the history."
  :group 'python-mls-command-setup
  :type 'boolean)

(defcustom python-mls-command-history-file "pyhist"
  "The file in which the command history is saved.
In order to change the size of the history, see the variable
`comint-input-ring-size'.  
The history is only saved if the variable `python-mls-save-command-history'
is non-nil.  Unless it is an absolute filepath, it saved under 
`user-emacs-directory'."
  :group 'python-mls-command-setup
  :type 'file)

(defcustom python-mls-after-prompt-hook '()
  "Hook run each time a new input prompt is found."
  :type 'hook
  :local t
  :group 'python-mls-command-setup)

(defcustom python-mls-kill-buffer-process-quit nil
  "Whether to kill the python buffer when the process completes."
  :group 'python-mls-command-setup
  :type 'boolean)

(defvar python-mls-continuation-prompt-regexp "^\s*\\.\\.\\.:? ")
(defun python-mls-in-continuation (&optional trim-trailing-ws)
  "Test whether we are in an continued input statement.
We are in a continuation statement if at least one non-empty line
exists after the line containing the prompt. Trailing empty lines
don't count."
  (let ((prompt-end
	 (if (and comint-last-prompt
		  (> (point) (cdr comint-last-prompt)))
	     (cdr comint-last-prompt)
	   (field-beginning))))
    (and (not (eq (field-at-pos (point)) 'output))
	 (> (save-excursion
	      (goto-char (field-end))
	      (skip-chars-backward "[:space:]\n\r")
	      (prog1 (line-number-at-pos)
		(when trim-trailing-ws 
		  (if (eq (field-at-pos (point)) 'output) ;no space in prompt
		      (goto-char (field-end)))
		  (delete-region (point) (field-end)))))
	    (line-number-at-pos prompt-end)))))

(defun python-mls--indent-line ()
  "Indent line, narrowing to region after prompt if in continuation."
  (if-let ((end (cdr-safe comint-last-prompt)))
      (save-restriction
	(narrow-to-region end (point-max))
	(python-indent-line-function))
    (python-indent-line-function))
  (if (= (line-beginning-position) (point-max))
      (python-mls-invisible-newline)))

;; (defun python-mls-complete-or-indent ()
;;   "Handle continuation lines for indent or complete.
;; In continuation lines, Tab narrows region and (re)-indents line.
;; In non-continuation lines, functions the same as
;; python-shell-completion-complete-or-indent.  M-Tab can be used
;; for completion in continuation input."
;;   (interactive)
;;   (if (python-mls-in-continuation)
;;       (indent-for-tab-command)
;;     (python-shell-completion-complete-or-indent)))

(defun python-mls-line-empty-p (&optional line-off pos)
  (save-excursion
    (if line-off (forward-line line-off)
      (if pos (goto-char pos)))
    (beginning-of-line)
    (looking-at-p "[[:space:]]*$")))

(defun python-mls-python-send-multiline-input (_proc string)
  (python-shell-send-string string)) ;sends multi-line via file

(defvar-local python-mls-send-multiline-input 
  #'python-mls-python-send-multiline-input
  "Which method to use to send multi-line statements.
Default is file based load/exec/command for python, or, if interpreter matches ipython, %cpaste.")

(defun python-mls-send-input ()
  "Strip space and newlines from end of input and send."
  (interactive)
  (let ((comint-input-sender
	 (if (python-mls-in-continuation 'trim)
	     python-mls-send-multiline-input
	   #'comint-simple-send)))
    (comint-send-input)))

(defun python-mls-get-old-input ()
  "Get old input.
Omits extra newlines at end, and preserves (some) text properties."
  (let* ((bof (field-beginning))
	 (field-prop (get-char-property bof 'field))
	 (str (if (not field-prop) ; regular input
		  (field-string bof)
		(comint-bol) 
		(buffer-substring
		 (point)
		 (if (eq field-prop 'output)
		     (line-end-position)
		   (field-end))))))
    (remove-text-properties 0 (length str) 
			    '(fontified nil 
					font-lock-face  nil
					help-echo nil mouse-face nil) str)
    (string-trim-right str "[\n\r]+")))

(defun python-mls-continue-or-send-input ()
  "Either continue an ongoing continued command, or send input."
  (interactive)
  (let ((ln (line-number-at-pos))
	(lnmx (line-number-at-pos (point-max))))
    (if (or
	 (< ln (line-number-at-pos (cdr comint-last-prompt))) ; old input
	 (not (python-mls-in-continuation))
	 (and (python-mls-line-empty-p) 
	      (or
	       (eq ln lnmx)
	       (and (eq ln (1- lnmx)) ;trailing blank line OK
		    (python-mls-line-empty-p 1)))))
	(python-mls-send-input)
      (newline) ;(comint-accumulate)
      (funcall indent-line-function)))) ; NO completion please

(defun python-mls--strip-input-history-properties (_str)
  "Removes 'line-prefix properties from the just-added comint history."
  (if (and comint-input-ring
	   (not (ring-empty-p comint-input-ring)))
      (let ((last (ring-ref comint-input-ring 0)))
	(remove-text-properties 
	 0 (length last) 
	 '(line-prefix nil font-lock-face nil fontified nil) last))))

(defun python-mls-delete-or-eof (arg)
  "Delete or send process EOF if at end of buffer (not considering final newline)."
  (interactive "p")
  (let ((proc (get-buffer-process (current-buffer))))
    (if (and proc
	     (= (point) (marker-position (process-mark proc)))
	     (save-excursion (forward-line) (python-mls-line-empty-p)))
	(progn
	  (goto-char (point-max))
	  (comint-kill-input)
	  (process-send-eof))
      (delete-char arg))))

(defun python-mls-interrupt-quietly ()
  "Interrupt the python process and bury any output."
  (let ((comint-preoutput-filter-functions '(python-shell-output-filter))
        (python-shell-output-filter-in-progress t))
    (interrupt-process) ;; or better, comint-interrupt-subjob?
    (while python-shell-output-filter-in-progress
      (accept-process-output))
    (setq python-shell-output-filter-buffer nil)))

(defun python-mls-invisible-newline ()
  "Insert an invisible, cursor-intangible newline without moving point.
Since continuation prompts use 'line-prefix property, and the
line after a final newline is entirely empty, it has not prompt.
To solve this we keep an invisible and intangible newline at the
end of the buffer."
  (save-excursion
    (insert (propertize "\n" 
			'invisible t 
			'cursor-intangible t
			'rear-nonsticky '(invisible)))))

(defvar python-mls-continuation-prompt nil
  "Current computed continuation prompt.")
(defvar python-mls-old-prompt nil
  "Last prompt before recent send.")
(defvar-local python-mls-disable-check-prompt nil)

(defvar-local python-mls-in-pdb nil
  "Whether we are in (i)PDB, according to the prompt.")

(defvar python-mls-mode)
(defun python-mls-check-prompt (&rest _args)
  "Check for continuation prompt and fix up comint to handle.
Multi-line statements we handle directly.  But if a single
command sent to (i)Python is the start of multi-line statment,
the process will return a continuation prompt.  We remove it,
sanitize the history, and then bring the last input forward to
continue.  Added as advice for comint-output-filter."
  (if (bound-and-true-p python-mls-mode)
      (with-current-buffer (or (python-shell-get-buffer)
			       (current-buffer))
	(if (and python-mls-mode comint-last-prompt
		 (not python-mls-disable-check-prompt))
	    (let* ((old-end (cdr-safe python-mls-old-prompt))
		   (beg (car comint-last-prompt))
		   (end (cdr comint-last-prompt))
		   (prompt (buffer-substring-no-properties beg end))
		   (python-mls-disable-check-prompt t)
		   (inhibit-read-only t))
	      (cond
	       ((and old-end
		     (marker-position old-end)
		     (string-match-p python-mls-continuation-prompt-regexp
				     prompt))
					;(message "FOUND PROMPT: %s" prompt)
		(python-mls-interrupt-quietly); re-enters
		(let* ((text (buffer-substring-no-properties old-end beg)))
		  ;;(message "Multiline prompt with %s" comint-last-prompt)
		  (delete-region old-end end) ;out with the old
		  (goto-char end)
		  (insert text)
		  ;;(save-excursion (python-mls-invisible-newline))
		  (funcall indent-line-function)
		  (if (and comint-input-ring
			   (not (ring-empty-p comint-input-ring)))
		      (ring-remove comint-input-ring 0))))
	       
	       ((and
		 (or (not old-end) (not (marker-position old-end)) (> end old-end))
		 (string-match-p python-shell--prompt-calculated-input-regexp
				 prompt))
					;(message "Normal prompt with %s" comint-last-prompt)
		(setq python-mls-old-prompt comint-last-prompt)
		(setq python-mls-in-pdb (string-match-p python-shell-prompt-pdb-regexp prompt))
		(add-text-properties beg (1- end) ; make cursor skip
				     '(cursor-intangible t 
							 rear-nonsticky 
							 (field inhibit-line-move-field-capture 
								read-only font-lock-face)))
		;;(goto-char end)
		;; (unless (looking-at "[\r\n]")
		;;   (save-excursion (python-mls-invisible-newline)))
		(python-mls-compute-continuation-prompt prompt)
		(unwind-protect
		    (run-hooks 'python-mls-after-prompt-hook)
		  (setq python-mls-disable-check-prompt nil)))))))))

(defun python-mls-compute-continuation-prompt (prompt)
  "Compute a prompt to use for continuation."
  (let* ((len (length prompt))
	 (has-colon (string-suffix-p ": " prompt))
	 (spaces (- len 3 (if has-colon 2 1))))
    (setq python-mls-continuation-prompt
	  (if (> len 3)
	      (propertize 
	       (concat (make-string spaces ?\s) "..." (if has-colon ": " " "))
	       'font-lock-face 'comint-highlight-prompt)))))
  
(defun python-mls-move-or-history (up &optional arg nocont-move)
  "Move line or recall command history. 
When in the first or last line of input, do
`comint-previous-input' for next or previous input.  Otherwise
just move the line.  Move down unless UP is non-nil.  Also move
normally inside of continued commands, unless NOCONT-MOVE is
non-nil."
  (interactive)
  (let* ((prompt (cdr comint-last-prompt))
	 (arg (or arg 1))
	 (arg (if up arg (- arg))))
    (if (and 
	 prompt
	 (or nocont-move
	     (if up (= (line-number-at-pos)
		       (line-number-at-pos prompt))
	       (>= (line-number-at-pos)
		   (save-excursion ; Down
		     (goto-char (point-max))
		     (skip-chars-backward "\r\n" (1- (point)))
		     (line-number-at-pos))))))
	(comint-previous-input arg)
      (line-move (- arg)))))

(defun python-mls-up-or-history (&optional arg)
  "When in last line of process buffer, move to previous input.
Otherwise just go up one line."
  (interactive "p")
  (python-mls-move-or-history t arg))

(defun python-mls-down-or-history (&optional arg)
  "When in last line of process buffer, move to next input.
Otherwise just go down one line."
  (interactive "p")
  (python-mls-move-or-history nil arg))

(defun python-mls-noblock-up-or-history (&optional arg)
  (interactive "p")
  (python-mls-move-or-history t arg 'noblock))

(defun python-mls-noblock-down-or-history (&optional arg)
  (interactive "p")
  (python-mls-move-or-history nil arg 'noblock))

(defvar-local python-mls-font-lock-keywords nil)
(defun python-mls--fontify-region-function (beg end &optional verbose)
  (if-let ((process (get-buffer-process (current-buffer)))
	   (pmark (process-mark process)))
      (if (> end pmark)
	  (let ((font-lock-keywords python-mls-font-lock-keywords)
		(font-lock-syntactic-face-function
		 #'python-font-lock-syntactic-face-function)
		(font-lock-dont-widen t)
		(start (max pmark beg)))
	    ;(message "FLE: %s->%s" start end)
	    (put-text-property start end 'line-prefix
			       python-mls-continuation-prompt)
	    (save-restriction
	      (narrow-to-region pmark (point-max))
	      (with-syntax-table python-mode-syntax-table
		(font-lock-default-fontify-region start end verbose)))))))

(defun python-mls--save-input ()
  "Save command input history between sessions."
  (if (and python-mls-save-command-history
	   (stringp python-mls-command-history-file))
      (condition-case nil
	  (let ((comint-input-ring-separator " "))
	    (comint-write-input-ring))
	(error nil))))

(defun python-mls-sentinel (process event)
  "The sentinel function for the mls shell process.
Kill buffer when process completes."
  (let ((buf (process-buffer process)))
    (if (buffer-live-p buf)
      (with-current-buffer buf
	(goto-char (point-max))
	(insert (format "\n\n  Process %s %s" process event))
	(if python-mls-kill-buffer-process-quit
	    (kill-buffer buf))))))

(defvar python-mls-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(meta return)] #'python-mls-send-input)
    (define-key map [(shift return)] #'python-mls-send-input)
    (define-key map [remap previous-line] #'python-mls-up-or-history)
    (define-key map [remap next-line] #'python-mls-down-or-history)
    (define-key map [remap comint-send-input]
      #'python-mls-continue-or-send-input)
    (define-key map [remap python-shell-completion-complete-or-indent]
      #'indent-for-tab-command) ; restore this, will use 'complete
;      #'python-mls-complete-or-indent)
    (define-key map [(meta up)] #'comint-previous-matching-input-from-input)
    (define-key map [(meta down)] #'comint-next-matching-input-from-input)
    (define-key map (kbd "C-d") #'python-mls-delete-or-eof)
    map))

(defun python-mls-setup ()
  "Setup python shell for multiline input."
  ;; command history
  (when python-mls-save-command-history
    (unless (file-name-absolute-p python-mls-command-history-file)
      (setq python-mls-command-history-file
	    (expand-file-name python-mls-command-history-file
			      user-emacs-directory)))
    (when (stringp python-mls-command-history-file)
      (set (make-local-variable 'comint-input-ring-file-name)
	   python-mls-command-history-file)
      (if (file-regular-p python-mls-command-history-file)
	  (let ((comint-input-ring-separator " "))
	    (comint-read-input-ring))))
    (let ((process (get-buffer-process (current-buffer))))
      (set-process-sentinel process #'python-mls-sentinel))
    (add-hook 'kill-buffer-hook #'python-mls--save-input nil t))
  (setq-local comint-history-isearch 'dwim)
  
  ;; font-lock handling
  (python-shell-font-lock-turn-off) ;We'll do in-buffer font-lock ourselves!
  (setq-local 
   font-lock-keywords-only nil
   syntax-propertize-function python-syntax-propertize-function
   comment-start-skip "#+\\s-*"
   parse-sexp-lookup-properties t
   parse-sexp-ignore-comments t
   forward-sexp-function #'python-nav-forward-sexp
   font-lock-fontify-region-function #'python-mls--fontify-region-function)
  (setq python-mls-font-lock-keywords
	(symbol-value
	 (font-lock-choose-keywords
	  python-font-lock-keywords (font-lock-value-in-major-mode
				     font-lock-maximum-decoration))))

  (setq-local comint-get-old-input #'python-mls-get-old-input
	      comint-history-isearch 'dwim)
  (add-hook 'comint-input-filter-functions
	    #'python-mls--strip-input-history-properties nil t)
  (cursor-intangible-mode 1)

  ;; Shift up/C-p: skips blocks
  (dolist (key (where-is-internal 'previous-line))
    (let ((new (vector `(shift ,(aref key 0))))) 
      (define-key python-mls-mode-map new 'python-mls-noblock-up-or-history)))
  (dolist (key (where-is-internal 'next-line)) 
    (let ((new (vector `(shift ,(aref key 0)))))
      (define-key python-mls-mode-map new 'python-mls-noblock-down-or-history)))
  
  ;; indentation
  (electric-indent-local-mode -1) ; We handle [Ret] indentation ourselves
  (setq-local indent-line-function #'python-mls--indent-line))

;  (add-to-list 'python-indent-trigger-commands 'python-mls-complete-or-indent))

(define-minor-mode python-mls-mode
  "Minor mode in inferior (i)python buffers for editing multi-line statements."
  :keymap python-mls-mode-map
  (if python-mls-mode
      (python-mls-setup)))

(defun python-mls--python-setup ()
  "Set python-mode buffers to exclude line-prefix on yank."
  (make-local-variable 'yank-excluded-properties) ; for python-mls
  (cl-pushnew 'line-prefix yank-excluded-properties))

;;;###autoload
(add-hook 'inferior-python-mode-hook 'python-mls-mode)

;; prompt & input
;; We cannot use a normal comint-output-filter-function because
;; comint does not update comint-last-prompt until _after_ running hooks.
;;;###autoload
(advice-add #'comint-output-filter :after #'python-mls-check-prompt)

;;;###autoload
(add-hook 'python-mode-hook 'python-mls--python-setup)

(provide 'python-mls)

