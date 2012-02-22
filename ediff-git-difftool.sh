#! /bin/bash -i

LOCAL="$1"
REMOTE="$2"


emacs --eval="(progn (diff \"$LOCAL\" \"$REMOTE\" \"-urN\") (switch-to-buffer \"*Diff*\") (diff-ediff-mode) (delete-other-windows) (beginning-of-buffer) (toggle-read-only t))"
