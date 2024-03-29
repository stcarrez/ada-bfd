dnl Autoconf configure script for BfdAda
dnl Copyright 2001, 2002, 2003, 2005, 2015, 2019, 2021, 2023 Free Software Foundation, Inc.
dnl
dnl This file is part of BfdAda.
dnl 
dnl This program is free software; you can redistribute it and/or modify
dnl it under the terms of the GNU General Public License as published by
dnl the Free Software Foundation; either version 2 of the License, or
dnl (at your option) any later version.
dnl 
dnl This program is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU General Public License for more details.
dnl 
dnl You should have received a copy of the GNU General Public License
dnl along with this program; if not, write to the Free Software
dnl Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

dnl Process this file with autoconf to produce a configure script.

AC_INIT([ada-bfd],[1.3])
AC_CONFIG_SRCDIR([src/bfd.ads])

AC_CANONICAL_TARGET
AC_EXEEXT

# Current release settings
BFDADA_MAJOR_VERSION=1
BFDADA_MINOR_VERSION=3
BFDADA_MICRO_VERSION=0
BFDADA_VERSION=$BFDADA_MAJOR_VERSION.$BFDADA_MINOR_VERSION.$BFDADA_MICRO_VERSION

# checking for local tools
AC_PROG_CC
AC_PROG_MAKE_SET
AC_PROG_INSTALL
AC_PROG_RANLIB

# Set the version number of GtkAda
AC_SUBST(BFDADA_VERSION)
AC_SUBST(BFDADA_MAJOR_VERSION)
AC_SUBST(BFDADA_MINOR_VERSION)
AC_SUBST(BFDADA_MICRO_VERSION)

EXEC_PREFIX="$prefix"
AC_SUBST(EXEC_PREFIX)

AM_GNAT_LIBRARY_PROJECT(bfdada)

AC_CONFIG_HEADERS([src/config.h])

AC_CHECK_HEADERS(bfd.h dis-asm.h demangle.h libiberty/demangle.h)

AC_CHECK_LIB(bfd,bfd_openr,[LIBS="$LIBS -lbfd"])

AC_CHECK_LIB(dl,dlopen,[CFLAGS="$CFLAGS -DHAVE_LIB_DL=1"])
AC_CHECK_LIB(intl,dcgettext,[CFLAGS="$CFLAGS -DHAVE_LIB_INTL=1"])

AC_MSG_CHECKING([for bfd fprintf_styled_ftype])
      AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <bfd.h>
#include <dis-asm.h>
]], [[
fprintf_styled_ftype fprintf_styled_test;
]])],
        [AC_MSG_RESULT([yes])
         AC_DEFINE(HAVE_FPRINTF_STYLED)],
        [AC_MSG_RESULT([no])])

AC_MSG_CHECKING([for symbol_at_address_func return type])
      AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <bfd.h>
#include <dis-asm.h>
]], [[
long foo(struct disassemble_info* s) {
  return s->dynrelcount; /* Note: we should check for symbol_at_address_func... */
}
]])],
        [AC_MSG_RESULT([asymbol])
         AC_DEFINE(HAVE_SYMBOL_USE_ASYMBOL)],
        [AC_MSG_RESULT([int])])

AC_SUBST(CFLAGS)

AC_CONFIG_FILES([Makefile.conf
bfdada.gpr
])
AC_OUTPUT
