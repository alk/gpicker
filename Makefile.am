bin_PROGRAMS = filechooser
filechooser_SOURCES = main.c scorer.c
filechooser_LDADD = @gtk2_LIBS@ @libglade_LIBS@
AM_CFLAGS = @gtk2_CFLAGS@ @libglade_CFLAGS@

if HAVE_CHECK

TESTS = scorer-tests
check_PROGRAMS = scorer-tests
scorer_tests_SOURCES = scorer-tests.c scorer.c
scorer_tests_LDADD = @check_LIBS@

endif # HAVE_CHECK
