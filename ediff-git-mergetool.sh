#! /bin/bash

# local

LOCAL="$1"
REMOTE="$2"
BASE="$3"
MERGED="$4"

emacs --eval="(ediff-merge-files-with-ancestor \"$LOCAL\" \"$REMOTE\" \"$BASE\" nil \"$MERGED\")"
