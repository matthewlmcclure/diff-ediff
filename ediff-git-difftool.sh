#! /bin/bash

LOCAL="$1"
REMOTE="$2"

emacs --eval="(progn (diff \"$LOCAL\" \"$REMOTE\" \"-urN\") (switch-to-buffer \"*Diff*\") (diff-nav-mode) (delete-other-windows) (beginning-of-buffer) (toggle-read-only t))"
