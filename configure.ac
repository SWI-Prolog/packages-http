dnl Process this file with autoconf to produce a configure script.

AC_INIT(install-sh)
AC_PREREQ([2.50])
AC_CONFIG_HEADER(config.h)
AC_SUBST(JQUERYDIR)
AC_SUBST(JQUERYFILE)
AC_SUBST(JSFILES)

m4_include([../ac_swi_c.m4])

AC_MSG_CHECKING(jquery)
JQUERY="$(ls web/js/jquery-*.min.js)"
JSFILES=$(basename $JQUERY)
if test "$CROSSCOMPILING" != yes; then
  dnl Try jquery from Debian libjs-jquery package
  if test -r /usr/share/javascript/jquery/jquery.min.js; then
    JQUERY=/usr/share/javascript/jquery/jquery.min.js
    JSFILES=
  fi
fi
AC_MSG_RESULT($JQUERY)
JQUERYDIR="$(dirname "$JQUERY")"
JQUERYFILE="$(basename "$JQUERY")"

AC_OUTPUT(Makefile jquery.pl)
