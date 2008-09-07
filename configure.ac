AC_INIT([filechooser],0.5)
AM_INIT_AUTOMAKE
AC_CONFIG_FILES([Makefile])
AC_PROG_CC

# check for 'check' unit-testing framework
PKG_CHECK_MODULES([CHECK], [check], [HAVE_CHECK=yes], [
HAVE_CHECK=no
echo "Regression tests will not be built"
])
AM_CONDITIONAL([HAVE_CHECK], [test x$HAVE_CHECK = xyes])

PKG_CHECK_MODULES([gtk], [gtk+-2.0 >= 2.8])
PKG_CHECK_MODULES([glade], [libglade-2.0])

AC_OUTPUT
