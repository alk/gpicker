bin_PROGRAMS = gpicker
gpicker_SOURCES = main.c scorer.c filtration.c vector.c xmalloc.c \
 refcounted_str.c refcounted_str.h \
 filtration.h vector.h scorer.h xmalloc.h timing.h inline_qsort.h
gpicker_LDADD = @gtk2_LIBS@ @libglade_LIBS@ @gthread_LIBS@
AM_CFLAGS = -DPKGDATADIR=\"$(pkgdatadir)\" @gtk2_CFLAGS@ @libglade_CFLAGS@ @gthread_CFLAGS@

dist_pkgdata_DATA=gpicker.glade gpicker.el gpicker.vim
dist_bin_SCRIPTS=fasteri

if HAVE_CHECK

TESTS = scorer-tests
check_PROGRAMS = scorer-tests
scorer_tests_SOURCES = scorer-tests.c scorer.c
scorer_tests_LDADD = @check_LIBS@

endif # HAVE_CHECK

check-syntax:
	$(COMPILE) -o /dev/null -S ${CHK_SOURCES}
