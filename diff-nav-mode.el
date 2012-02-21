     ;; Here is a hypothetical example:

     ;;      (define-derived-mode hypertext-mode
     ;;        text-mode "Hypertext"
     ;;        "Major mode for hypertext.
     ;;      \\{hypertext-mode-map}"
     ;;        (setq case-fold-search nil))

     ;;      (define-key hypertext-mode-map
     ;;        [down-mouse-3] 'do-hyper-link)

(define-derived-mode diff-nav-mode
  diff-mode "Diff-Nav"
  "Major mode for navigating output of diff(1) with Ediff.
\\{diff-nav-mode-map}"
  nil)

(define-key diff-nav-mode-map
  [return] 'diff-nav-do-ediff)

(defun diff-nav-do-ediff ()
  (interactive)
  (message "called diff-nav-do-ediff")
  (let ((files (diff-hunk-file-names)))
    (ediff-buffers (get-file-buffer (cadr files)) (get-file-buffer (car files))))
  )

(defun parse-diff-command-line (line)
  (cond
   ((string-match " \\([^ ]+\\) \\([^ ]+\\)$" line)
    (list (match-string 1 line) (match-string 2 line)))
   (t
    (error "Could not determine left and right directories of diff")))
  )

(defun first-line ()
  (save-excursion
    (goto-char (point-min))
    (buffer-substring (line-beginning-position) (line-end-position))
    )
  )

(defun parse-file-summary-line (line dirs)
  (cond
   ((string-match "^Files \\(.+\\) and \\(.+\\) differ$" line)
    (list (match-string 1 line) (match-string 2 line)))
   ((string-match "^Only in \\(.+\\): \\(.+\\)$" line)
    (let* ((dir (match-string 1 line))
           (file (match-string 2 line))
           (existing-path (concat dir "/" file))
           (left-path (concat (car dirs) "/" file))
           (right-path (concat (cadr dirs) "/" file)))
      ;; Idea: remove the leading characters from existing-path that match
      ;; dir1 and dir2. If the first character is '/' or if the resulting
      ;; string is empty, then the file is only on the side of dirN.
      ;; Special case?: if one side is the current directory. '.'

      ;; D'oh! The summary command line won't exist in all diff -qr output.
      ;; It only exists when Emacs invokes diff via M-x diff. Not when
      ;; invoked from M-!.
      (list left-path right-path)
      )
    )
   )
  )

(defun ediff-find-file (file-var buffer-name &optional last-dir hooks-var)
  "Visit FILE and arrange its buffer to Ediff's liking.
FILE-VAR is actually a variable symbol whose value must contain a true
file name.
BUFFER-NAME is a variable symbol, which will get the buffer object into
which FILE is read.
LAST-DIR is the directory variable symbol where FILE's
directory name should be returned.  HOOKS-VAR is a variable symbol that will
be assigned the hook to be executed after `ediff-startup' is finished.
`ediff-find-file' arranges that the temp files it might create will be
deleted."
  (let* ((file (symbol-value file-var))
	 (file-magic (ediff-filename-magic-p file))
	 (temp-file-name-prefix (file-name-nondirectory file)))
    (cond ((file-directory-p file)
	   (error "File `%s' is a directory" file)))

    ;; some of the commands, below, require full file name
    (setq file (expand-file-name file))

    ;; Record the directory of the file
    (if last-dir
	(set last-dir (expand-file-name (file-name-directory file))))

    ;; Setup the buffer
    (set buffer-name (find-file-noselect file))

    (ediff-with-current-buffer (symbol-value buffer-name)
      (widen) ; Make sure the entire file is seen
      (cond (file-magic  ;   file has a handler, such as jka-compr-handler or
	     		 ;;; ange-ftp-hook-function--arrange for temp file
	     (ediff-verify-file-buffer 'magic)
	     (setq file
		   (ediff-make-temp-file
		    (current-buffer) temp-file-name-prefix))
	     (set hooks-var (cons `(lambda () (delete-file ,file))
				  (symbol-value hooks-var))))
	    ;; file processed via auto-mode-alist, a la uncompress.el
	    ((not (equal (file-truename file)
			 (file-truename (buffer-file-name))))
	     (setq file
		   (ediff-make-temp-file
		    (current-buffer) temp-file-name-prefix))
	     (set hooks-var (cons `(lambda () (delete-file ,file))
				  (symbol-value hooks-var))))
	    (t ;; plain file---just check that the file matches the buffer
	     (ediff-verify-file-buffer))))
    (set file-var file)))
