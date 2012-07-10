;;; diff-ediff.el --- navigate diff files with Ediff

;; Copyright (C) 2012 Matt McClure

;; Author: Matt McClure
;; Keywords: unix, tools

;; diff-ediff-mode is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; diff-ediff-mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with diff-ediff-mode.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package helps you explore the output of diff(1) using Ediff. In
;; particular, it adds a keyboard shortcut to navigate from the differences
;; under the `point' to an Ediff session comparing the corresponding files.
;; The author uses it like so, to replace Ediff's directory comparison:
;;
;; 1. C-u M-x diff RET dir1 RET dir2 RET -urN RET
;;
;; 2. N and P navigate to the next and previous file pairs in the *Diff*
;; buffer.
;;
;; 3. C-c C-e compares the current file pair in Ediff.
;;
;; The author uses it like so, as a Git difftool:
;;
;; 1. In ~/.gitconfig, include:
;;
;;    [difftool "ediff"]
;;      cmd = ediff-git-difftool.sh \"$LOCAL\" \"$REMOTE\"
;;      prompt = false
;;    
;;    [diff]
;;      tool = ediff
;;
;; 2. Put ediff-git-difftool.sh on your PATH.
;;
;; 3. git diffall --copy-back # See http://github.com/thenigan/git-diffall
;;
;; 4. N & P move to the next and previous files in the *Diff* buffer.
;;
;; 5. C-c C-e opens Ediff on the current file pair.
;;
;;
;; The above assumes you've added to your Emacs startup file:
;;
;;    (load "/path/to/diff-ediff.el")

;;; Code:

(define-derived-mode diff-ediff-mode
  diff-mode "Diff-Ediff"
  "Major mode for navigating output of diff(1) with Ediff.
\\{diff-ediff-mode-map}"
  nil)

(define-key diff-ediff-mode-map "\C-c\C-e" 'diff-ediff-do-ediff)

(defun diff-ediff-do-ediff ()
  (interactive)
  (let* ((files (diff-hunk-file-names))
         (file-A (cadr files))
         (file-B (car files))
         (buf-A (or (get-file-buffer file-A) (find-file-noselect file-A)))
         (buf-B (or (get-file-buffer file-B) (find-file-noselect file-B))))
    (ediff-buffers buf-A buf-B))
  )

(add-to-list 'auto-mode-alist '("\\.\\(diffs?\\|patch\\|rej\\)\\'" . diff-ediff-mode))
(add-to-list 'auto-mode-alist '("\\.\\(dif\\|pat\\)\\'" . diff-ediff-mode))

(provide 'diff-ediff)
