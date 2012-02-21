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
  (let* ((files (diff-hunk-file-names))
         (file-A (cadr files))
         (file-B (car files))
         (buf-A (or (get-file-buffer file-A) (find-file-noselect file-A)))
         (buf-B (or (get-file-buffer file-B) (find-file-noselect file-B))))
    (ediff-buffers buf-A buf-B))
  )
