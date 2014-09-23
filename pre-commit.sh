#!/bin/sh
git stash -q --keep-index

emacs --batch -l ~/.emacs.d/init.el
RESULT=$?

git stash pop -q
exit 1
[ $RESULT -ne 0 ] && exit 1
exit 0
