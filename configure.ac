AC_INIT([gpicker], 2.0.2)
AC_CONFIG_HEADERS(config.h)
AM_INIT_AUTOMAKE
AC_CONFIG_FILES([Makefile])
AC_PROG_CC

AC_ARG_ENABLE(timing, [AS_HELP_STRING(--enable-timing, Enable output of timing info on stderr)],
                      [AC_DEFINE([WITH_TIMING], 1, [Output timing info])])

# check for 'check' unit-testing framework
PKG_CHECK_MODULES([check], [check], [HAVE_CHECK=yes], [
HAVE_CHECK=no
echo "Regression tests will not be built"
])
AM_CONDITIONAL([HAVE_CHECK], [test x$HAVE_CHECK = xyes])

AC_DEFINE([FIND_INVOCATION], ["find . '!' -wholename '*.git/*' -a '!' -wholename '*.hg/*']dnl
[ -a '!' -wholename '*.svn/*' -a '!' -wholename '*.bzr/*']dnl
[ -a '!' -wholename '*CVS/*' -type f -follow -print0"], [Command line to fetch list of files])

uname_res=`uname -s`
if test "x$uname_res" = "xDarwin"; then
  AC_MSG_NOTICE([Darwin detected. Assuming OSX and not cross-compiling.])
  export PKG_CONFIG_PATH=$PKG_CONFIG_PATH:/Library/Frameworks/Glib.framework/Resources/dev/lib/pkgconfig:/Library/Frameworks/Cairo.framework/Resources/dev/lib/pkgconfig:/Library/Frameworks/Gtk.framework/Resources/dev/lib/pkgconfig
  dnl find in older OSX versions do not support -wholename
  AC_DEFINE([FIND_INVOCATION], ["find . -type f -print0"], [Command line to fetch list of files])
fi

PKG_CHECK_MODULES([gtk2], [gtk+-2.0 >= 2.8])
PKG_CHECK_MODULES([glib2], [glib-2.0 >= 2.14])
PKG_CHECK_MODULES([gthread], [gthread-2.0])

AC_DEFINE([WITH_GUI], 1, [GUI build])

AC_SUBST(EXTRA_CPPFLAGS)

if test "x$uname_res" = "xDarwin"; then
  EXTRA_CPPFLAGS=-xobjective-c
  gtk2_LIBS="$gtk2_LIBS -framework Cocoa"
fi

AC_MSG_CHECKING([whether -m32 is required to link gtk])
AC_LANG_CONFTEST(AC_LANG_CALL([],[gtk_main]))
ac_func_search_save_LIBS=$LIBS
ac_func_search_save_CFLAGS=$CFLAGS
LIBS="$LIBS $gtk2_LIBS $glib2_LIBS $gthread_LIBS"
AC_LINK_IFELSE([], [AC_MSG_RESULT([no])], [
  dnl link failed. Try -m32
  CFLAGS="-m32 $CFLAGS"
  AC_LINK_IFELSE([], [gtk2_CFLAGS="-m32 $gtk2_CFLAGS"],
     [AC_MSG_ERROR([linking with gtk2 failed! Something is wrong.])])
  AC_MSG_RESULT([yes])
])
LIBS=$ac_func_search_save_LIBS
CFLAGS=$ac_func_search_save_CFLAGS

AC_SUBST([DATE], [`LANG=C date -R`])

AC_CONFIG_FILES(debian/changelog)
AC_OUTPUT
