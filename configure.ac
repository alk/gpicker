AC_INIT([gpicker],1.0)
AC_CONFIG_HEADERS(config.h)
AM_INIT_AUTOMAKE
AC_CONFIG_FILES([Makefile])
AC_PROG_CC

AC_ARG_ENABLE(timing, [  --enable-timing	Enable output of timing info on stderr],
                      [AC_DEFINE([WITH_TIMING], 1, [Output timing info])])

# check for 'check' unit-testing framework
PKG_CHECK_MODULES([check], [check], [HAVE_CHECK=yes], [
HAVE_CHECK=no
echo "Regression tests will not be built"
])
AM_CONDITIONAL([HAVE_CHECK], [test x$HAVE_CHECK = xyes])

PKG_CHECK_MODULES([gtk2], [gtk+-2.0 >= 2.8])
PKG_CHECK_MODULES([glib2], [glib-2.0 >= 2.14])
PKG_CHECK_MODULES([libglade], [libglade-2.0])

AC_OUTPUT
