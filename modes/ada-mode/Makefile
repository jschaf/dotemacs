# make emacs documentation, and gpr2adp

# environment variables:
# GNAT_SRC : points to gnat source distribution; $(GNAT_SRC)/src/ada/alloc.ads (and other files listed below) must exist

all : ada-mode.info
all : ada-mode.html
all : gnatpath.exe

install : gnatpath.exe
	strip -o ~/bin/gnatpath.exe gnatpath.exe 

VPATH := Info

%.info : %.texi
	makeinfo $< -o Info/$@ 

%.html : %.texi
	makeinfo --html --no-split $< -o Info/$@


gnatpath.log : gnatpath.exe
	./gnatpath.exe list -v -Pgnatpath.gpr > gnatpath.log

export ADA_PROJECT_PATH := $(GDS_ROOT)/makerules/$(GDS_BRANCH);$(GDS_ROOT)/sal/$(GDS_BRANCH)/Build/release_windows_x86_gnu;$(GDS_ROOT)/common/$(GDS_BRANCH)/opentoken/build/x86_gnu_windows_release
gds.log : gnatpath.exe
	cd /Projects/GDS/common/main/build/x86_gnu_windows_test; /Gnu/Emacs/site-lisp/emacs_stephe_site_lisp/gnatpath.exe list -v -Pgds.gpr

.PHONY : force
gnatpath.exe : obj gnat_src force 
	gnatmake -Pgnatpath.gpr

obj :
	mkdir obj

# if we list $(GNAT_SRC)/src/ada as a source directory for the
# project, too many GNAT files get compiled, and we get errors from
# gnatbind. So we copy only the files we need to a separate directory,
# and compile from there.

GNAT_FILES += alloc.ads
GNAT_FILES += atree.adb
GNAT_FILES += atree.ads
GNAT_FILES += casing.adb
GNAT_FILES += casing.ads
GNAT_FILES += csets.ads
GNAT_FILES += csets.adb
GNAT_FILES += debug.adb
GNAT_FILES += debug.ads
GNAT_FILES += einfo.adb
GNAT_FILES += einfo.ads
GNAT_FILES += elists.adb
GNAT_FILES += elists.ads
GNAT_FILES += err_vars.ads
GNAT_FILES += errout.adb
GNAT_FILES += errout.ads
GNAT_FILES += erroutc.adb
GNAT_FILES += erroutc.ads
GNAT_FILES += errutil.adb
GNAT_FILES += errutil.ads
GNAT_FILES += fmap.adb
GNAT_FILES += fmap.ads
GNAT_FILES += fname.adb
GNAT_FILES += fname.ads
GNAT_FILES += gnatvsn.adb
GNAT_FILES += gnatvsn.ads
GNAT_FILES += hostparm.ads
GNAT_FILES += lib.adb
GNAT_FILES += lib.ads
GNAT_FILES += lib-list.adb
GNAT_FILES += lib-sort.adb
GNAT_FILES += mlib-fil.adb
GNAT_FILES += mlib-fil.ads
GNAT_FILES += mlib-tgt.adb
GNAT_FILES += mlib-tgt.ads
GNAT_FILES += mlib-utl.adb
GNAT_FILES += mlib-utl.ads
GNAT_FILES += mlib.adb
GNAT_FILES += mlib.ads
GNAT_FILES += namet.adb
GNAT_FILES += namet.ads
GNAT_FILES += nlists.adb
GNAT_FILES += nlists.ads
GNAT_FILES += opt.adb
GNAT_FILES += opt.ads
GNAT_FILES += osint.adb
GNAT_FILES += osint.ads
GNAT_FILES += output.adb
GNAT_FILES += output.ads
GNAT_FILES += prj-attr-pm.adb
GNAT_FILES += prj-attr-pm.ads
GNAT_FILES += prj-attr.adb
GNAT_FILES += prj-attr.ads
GNAT_FILES += prj-com.ads
GNAT_FILES += prj-com.ads
GNAT_FILES += prj-dect.adb
GNAT_FILES += prj-dect.ads
GNAT_FILES += prj-env.adb
GNAT_FILES += prj-env.ads
GNAT_FILES += prj-err.adb
GNAT_FILES += prj-err.ads
GNAT_FILES += prj-ext.adb
GNAT_FILES += prj-ext.ads
GNAT_FILES += prj-nmsc.adb
GNAT_FILES += prj-nmsc.ads
GNAT_FILES += prj-part.adb
GNAT_FILES += prj-part.ads
GNAT_FILES += prj-proc.adb
GNAT_FILES += prj-proc.ads
GNAT_FILES += prj-strt.adb
GNAT_FILES += prj-strt.ads
GNAT_FILES += prj-tree.adb
GNAT_FILES += prj-tree.ads
GNAT_FILES += prj-util.adb
GNAT_FILES += prj-util.ads
GNAT_FILES += prj.adb
GNAT_FILES += prj.ads
GNAT_FILES += rident.ads
GNAT_FILES += scans.adb
GNAT_FILES += scans.ads
GNAT_FILES += scng.adb
GNAT_FILES += scng.ads
GNAT_FILES += sdefault.ads
GNAT_FILES += sinfo.adb
GNAT_FILES += sinfo.ads
GNAT_FILES += sinput-c.adb
GNAT_FILES += sinput-c.ads
GNAT_FILES += sinput-p.adb
GNAT_FILES += sinput-p.ads
GNAT_FILES += sinput.adb
GNAT_FILES += sinput.ads
GNAT_FILES += snames.adb
GNAT_FILES += snames.ads
GNAT_FILES += stand.adb
GNAT_FILES += stand.ads
GNAT_FILES += stringt.adb
GNAT_FILES += stringt.ads
GNAT_FILES += style.ads
GNAT_FILES += styleg-c.adb
GNAT_FILES += styleg-c.ads
GNAT_FILES += styleg.adb
GNAT_FILES += styleg.ads
GNAT_FILES += stylesw.adb
GNAT_FILES += stylesw.ads
GNAT_FILES += table.adb
GNAT_FILES += table.ads
GNAT_FILES += targparm.adb
GNAT_FILES += targparm.ads
GNAT_FILES += tempdir.adb
GNAT_FILES += tempdir.ads
GNAT_FILES += tree_io.adb
GNAT_FILES += tree_io.ads
GNAT_FILES += types.adb
GNAT_FILES += types.ads
GNAT_FILES += uintp.adb
GNAT_FILES += uintp.ads
GNAT_FILES += uname.adb
GNAT_FILES += uname.ads
GNAT_FILES += urealp.adb
GNAT_FILES += urealp.ads
GNAT_FILES += widechar.adb
GNAT_FILES += widechar.ads

gnat_src :
	mkdir gnat_src
	for I in $(GNAT_FILES); do cp $(GNAT_SRC)/src/ada/$$I gnat_src; done

clean :
	rm -rf obj
	rm -rf gnat_src


# Local Variables:
# eval: (ada-reread-prj-file "emacs.prj")
# eval: (makefile-mode)
# end:
# end of file
