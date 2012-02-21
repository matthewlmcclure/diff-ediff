;;; diff-nav-mode.el --- navigate diff files with Ediff

;; Copyright (C) 2012 Matt McClure

;; Author: Matt McClure
;; Keywords: unix, tools

;; diff-nav-mode is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; diff-nav-mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with diff-nav-mode.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package helps you explore differences between files, using the
;; Ediff to view the output of diff(1). The author uses it like so:
;;
;; 1. In your Emacs startup file, include:
;;
;;    (load "/path/to/diff-nav-mode.el")
;;
;; 2. In ~/.gitconfig, include:
;;
;;    [difftool "ediff"]
;;      cmd = ediff-git-difftool.sh \"$LOCAL\" \"$REMOTE\"
;;      prompt = false
;;    
;;    [diff]
;;      tool = ediff
;;
;; 3. Put ediff-git-difftool.sh on your PATH.
;;
;; 4. git diffall --copy-back # See http://github.com/thenigan/git-diffall
;;
;; 5. N & P move to the next and previous files in the *Diff* buffer.
;;
;; 6. C-c C-e opens Ediff on the current file pair.

;;; Code:

(define-derived-mode diff-nav-mode
  diff-mode "Diff-Nav"
  "Major mode for navigating output of diff(1) with Ediff.
\\{diff-nav-mode-map}"
  nil)

(define-key diff-nav-mode-map "\C-c\C-e" 'diff-nav-do-ediff)

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
