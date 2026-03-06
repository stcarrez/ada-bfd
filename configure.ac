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
         AC_DEFINE(HAVE_FPRINTF_STYLED, [1], [Define to 1 if you have the fprintf_styled_type declared in <dis-asm.h> header file.])],
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
         AC_DEFINE(HAVE_SYMBOL_USE_ASYMBOL, [1], [Define to 1 if you have the symbol_at_address_func returns a asymbol * type instead of int.])],
        [AC_MSG_RESULT([int])])

AM_BFD_CHECK_ARCH(bfd_arch_i960, I960)
AM_BFD_CHECK_ARCH(bfd_arch_or32, OR32)
AM_BFD_CHECK_ARCH(bfd_arch_a29k, A29K)
AM_BFD_CHECK_ARCH(bfd_arch_spu, SPU)
AM_BFD_CHECK_ARCH(bfd_arch_l1om, L1OM)
AM_BFD_CHECK_ARCH(bfd_arch_k1om, K1OM)
AM_BFD_CHECK_ARCH(bfd_arch_iamcu, IAMCU)
AM_BFD_CHECK_ARCH(bfd_arch_we32k, WE32K)
AM_BFD_CHECK_ARCH(bfd_arch_alliant, ALLIANT)
AM_BFD_CHECK_ARCH(bfd_arch_m88k, M88K)
AM_BFD_CHECK_ARCH(bfd_arch_plugin, PLUGIN)
AM_BFD_CHECK_ARCH(bfd_arch_m9s12x, M9S12X)
AM_BFD_CHECK_ARCH(bfd_arch_s12z, S12Z)
AM_BFD_CHECK_ARCH(bfd_arch_h8500, H8500)
AM_BFD_CHECK_ARCH(bfd_arch_nds32, NDS32)
AM_BFD_CHECK_ARCH(bfd_arch_w65, W65)
AM_BFD_CHECK_ARCH(bfd_arch_tic6x, TIC6X)
AM_BFD_CHECK_ARCH(bfd_arch_tic80, TIC80)
AM_BFD_CHECK_ARCH(bfd_arch_v850_rh850, V850_RH850)
AM_BFD_CHECK_ARCH(bfd_arch_m32c, M32C)
AM_BFD_CHECK_ARCH(bfd_arch_moxie, MOXIE)
AM_BFD_CHECK_ARCH(bfd_arch_ft32, FT32)
AM_BFD_CHECK_ARCH(bfd_arch_mep, MEP)
AM_BFD_CHECK_ARCH(bfd_arch_metag, METAG)
AM_BFD_CHECK_ARCH(bfd_arch_bpf, BPF)
AM_BFD_CHECK_ARCH(bfd_arch_epiphany, EPIPHANY)
AM_BFD_CHECK_ARCH(bfd_arch_mt, MT)
AM_BFD_CHECK_ARCH(bfd_arch_bfin, BFIN)
AM_BFD_CHECK_ARCH(bfd_arch_cr16, CR16)
AM_BFD_CHECK_ARCH(bfd_arch_cr16c, CR16C)
AM_BFD_CHECK_ARCH(bfd_arch_riscv, RISCV)
AM_BFD_CHECK_ARCH(bfd_arch_rl78, RL78)
AM_BFD_CHECK_ARCH(bfd_arch_rx, RX)
AM_BFD_CHECK_ARCH(bfd_arch_score, SCORE)
AM_BFD_CHECK_ARCH(bfd_arch_openrisc, OPENRISC)
AM_BFD_CHECK_ARCH(bfd_arch_xc16x, XC16X)
AM_BFD_CHECK_ARCH(bfd_arch_xgate, XGATE)
AM_BFD_CHECK_ARCH(bfd_arch_maxq, MAXQ)
AM_BFD_CHECK_ARCH(bfd_arch_z80, Z80)
AM_BFD_CHECK_ARCH(bfd_arch_lm32, LM32)
AM_BFD_CHECK_ARCH(bfd_arch_kvx, KVX)
AM_BFD_CHECK_ARCH(bfd_arch_tilepro, TILEPRO)
AM_BFD_CHECK_ARCH(bfd_arch_aarch64, AARCH64)
AM_BFD_CHECK_ARCH(bfd_arch_nios2, NIOS2)
AM_BFD_CHECK_ARCH(bfd_arch_visium, VISIUM)
AM_BFD_CHECK_ARCH(bfd_arch_wasm32, WASM32)
AM_BFD_CHECK_ARCH(bfd_arch_nfp, NFP)
AM_BFD_CHECK_ARCH(bfd_arch_csky, CSKY)
AM_BFD_CHECK_ARCH(bfd_arch_loongarch, LOONGARCH)
AM_BFD_CHECK_ARCH(bfd_arch_amdgcn, AMDGCN)

AC_SUBST(CFLAGS)

AC_CONFIG_FILES([Makefile.conf
bfdada.gpr
])
AC_OUTPUT
