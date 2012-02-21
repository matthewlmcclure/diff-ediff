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
  (let* ((dirs (parse-diff-command-line
                (first-line)))
         (files (parse-file-summary-line
                 (buffer-substring (line-beginning-position) (line-end-position)) dirs)))
    (ediff-files (car files) (cadr files)))
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
