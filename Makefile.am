bin_PROGRAMS = gpicker
gpicker_SOURCES = main.c scorer.c filtration.c vector.c xmalloc.c \
 refcounted_str.c refcounted_str.h \
 do_with_main_loop.h do_with_main_loop.c \
 loading.h loading.c \
 filtration.h vector.h scorer.h xmalloc.h timing.h inline_qsort.h
gpicker_LDADD = @gtk2_LIBS@ @gthread_LIBS@
AM_CFLAGS = @gtk2_CFLAGS@ @gthread_CFLAGS@
AM_CPPFLAGS = @EXTRA_CPPFLAGS@

EXTRA_DIST=fasteri gpicker.el gpicker.vim IDEAS CodingStyle README.osx

if HAVE_CHECK

TESTS = scorer-tests
check_PROGRAMS = scorer-tests
scorer_tests_SOURCES = scorer-tests.c scorer.c
scorer_tests_LDADD = @check_LIBS@

endif # HAVE_CHECK

check-syntax:
	$(COMPILE) -o /dev/null -S ${CHK_SOURCES}
