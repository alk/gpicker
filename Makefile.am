bin_PROGRAMS = filechooser
filechooser_SOURCES = main.c scorer.c
filechooser_LDADD = @gtk_LIBS@ @glade_LIBS@
AM_CFLAGS = @gtk_CFLAGS@ @glade_CFLAGS@

TESTS = scorer-tests
check_PROGRAMS = scorer-tests
scorer_tests_SOURCES = scorer-tests.c scorer.c
scorer_tests_LDADD = @CHECK_LIBS@
