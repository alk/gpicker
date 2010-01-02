bin_PROGRAMS = gpicker gpicker-simple
gpicker_SOURCES = main.c scorer.c filtration.c vector.c xmalloc.c \
 refcounted_str.c refcounted_str.h compiler.h \
 do_with_main_loop.h do_with_main_loop.c \
 loading.h loading.c \
 simple_main.c \
 filtration.h vector.h scorer.h xmalloc.h timing.h inline_qsort.h
gpicker_LDADD = @gtk2_LIBS@ @gthread_LIBS@
AM_CFLAGS = @gtk2_CFLAGS@ @gthread_CFLAGS@
AM_CPPFLAGS = @EXTRA_CPPFLAGS@

man_MANS = gpicker.1 gpicker-simple.1

EXTRA_DIST = gpicker.el gpicker.vim IDEAS CodingStyle README.osx Makefile.simple debian

# _SOURCES is only to fool automake not to search for gpicker-simple.c
# Real build rule is simple 'ln'
gpicker_simple_SOURCES = simple_main.c
gpicker-simple: gpicker
	ln -f gpicker gpicker-simple

if HAVE_CHECK

TESTS = scorer-tests
check_PROGRAMS = scorer-tests
scorer_tests_SOURCES = scorer-tests.c scorer.c
scorer_tests_LDADD = @check_LIBS@

endif # HAVE_CHECK

check-syntax:
	$(COMPILE) -o /dev/null -S ${CHK_SOURCES}
