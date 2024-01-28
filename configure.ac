#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT(
  [guile-yamlpp],
  [0.3],
  [yorgath@gmail.com],
  [guile-yamlpp],
  [https://gitlab.com/yorgath/guile-yamlpp])
AC_CONFIG_MACRO_DIRS([m4])
AC_CONFIG_AUX_DIR([build-aux])

dnl Automake initialization.
AM_INIT_AUTOMAKE
AM_SILENT_RULES([yes])

dnl Libtool initialization.
LT_INIT

dnl Guile specific initialization.
GUILE_PKG([3.0])
GUILE_PROGS

dnl Use this in the Makefile to properly set up cross-compilation with
dnl guild.  Lifted verbatim from guile-hall.
if test "$cross_compiling" != no; then
   GUILE_TARGET="--target=$host_alias"
   AC_SUBST([GUILE_TARGET])
fi

dnl Checks for programs.
AC_PROG_CXX
AC_PROG_AWK
AC_PROG_CC
AC_PROG_CPP
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET

dnl Checks for libraries.
PKG_CHECK_MODULES([YAML], [yaml-cpp >= 0.7.0])
PKG_CHECK_MODULES([GUILE], [guile-$GUILE_EFFECTIVE_VERSION >= 3.0])

dnl Checks for typedefs, structures, and compiler characteristics.
AC_CHECK_HEADER_STDBOOL

dnl Checks for library functions.
AC_CHECK_FUNCS([strerror])

dnl Generate these files.
AC_CONFIG_FILES([
  Makefile
  cxx/Makefile
  scm/Makefile
])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])

AC_OUTPUT
