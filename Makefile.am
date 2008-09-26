bin_PROGRAMS = filechooser
filechooser_SOURCES = main.c scorer.c filtration.c vector.c xmalloc.c
filechooser_LDADD = @gtk2_LIBS@ @libglade_LIBS@
AM_CFLAGS = -DPKGDATADIR=\"$(pkgdatadir)\" @gtk2_CFLAGS@ @libglade_CFLAGS@

dist_pkgdata_DATA=filechooser.glade filechooser.el

if HAVE_CHECK

TESTS = scorer-tests
check_PROGRAMS = scorer-tests
scorer_tests_SOURCES = scorer-tests.c scorer.c
scorer_tests_LDADD = @check_LIBS@

endif # HAVE_CHECK
