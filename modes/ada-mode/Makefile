# make emacs documentation

all : ada-mode.info
all : ada-mode.html

VPATH := info

%.info : %.texi
	makeinfo $< -o info/$@

%.html : %.texi
	makeinfo --html --no-split $< -o info/$@


# end of file
