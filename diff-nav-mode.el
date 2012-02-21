(define-derived-mode diff-nav-mode
  diff-mode "Diff-Nav"
  "Major mode for navigating output of diff(1) with Ediff.
\\{diff-nav-mode-map}"
  nil)

(define-key diff-nav-mode-map "\C-c\C-e" 'diff-nav-do-ediff)
(define-key diff-nav-mode-map "\C-m" 'diff-nav-do-ediff)

(defun diff-nav-do-ediff ()
  (interactive)
  (let* ((files (diff-hunk-file-names))
         (file-A (cadr files))
         (file-B (car files))
         (buf-A (or (get-file-buffer file-A) (find-file-noselect file-A)))
         (buf-B (or (get-file-buffer file-B) (find-file-noselect file-B))))
    (ediff-buffers buf-A buf-B))
  )

(add-to-list 'auto-mode-alist '("\\.\\(diffs?\\|patch\\|rej\\)\\'" . diff-nav-mode))
(add-to-list 'auto-mode-alist '("\\.\\(dif\\|pat\\)\\'" . diff-nav-mode))

(provide 'diff-nav-mode)
