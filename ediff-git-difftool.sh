#! /bin/bash

LOCAL="$1"
REMOTE="$2"

emacs --eval="(diff \"$LOCAL\" \"$REMOTE\" \"-urN\")"
