git stash -q --keep-index

emacs --batch -l ~/.emacs.d/init.el
RESULT=$?

git stash pop -q

[ $RESULT -ne 0 ] && exit 1
exit 0
