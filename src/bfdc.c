/* Functions for BfdAda
   Copyright 2001, 2002, 2003, 2012, 2015, 2021 Free Software Foundation, Inc.
   Contributed by Stephane Carrez (Stephane.Carrez@gmail.com)

This file is part of BfdAda.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

#include "config.h"
#include <bfd.h>
#include <stdlib.h>
#include <stdarg.h>

// Old Binutils use a C macro with two arguments
// but newer version are using C inline function with a single argument.
#ifdef bfd_section_name
# undef bfd_section_name
# undef bfd_section_vma
# undef bfd_section_lma
# undef bfd_section_size
# define bfd_section_name(ptr) bfd_get_section_name(0, ptr)
# define bfd_section_vma(ptr) bfd_get_section_vma(0, ptr)
# define bfd_section_lma(ptr) bfd_get_section_lma(0, ptr)
# define bfd_section_size(ptr) bfd_get_section_size(0, ptr)
#endif

void*
_bfd_get_filename (struct bfd *abfd)
{
  return (void*)bfd_get_filename (abfd);
}

void*
bfd_get_sections (struct bfd *abfd)
{
  return abfd->sections;
}

void*
bfd_next_section (struct bfd_section *sec)
{
  return sec->next;
}

const void*
_bfd_get_section_name (struct bfd_section *sec)
{
  return bfd_section_name (sec);
}

unsigned long long
_bfd_get_section_vma (struct bfd_section *sec)
{
  return (unsigned long long) bfd_section_vma (sec);
}

unsigned long long
_bfd_get_section_lma (struct bfd_section *sec)
{
  return (unsigned long long) bfd_section_lma (sec);
}

unsigned long long
_bfd_get_section_size (struct bfd_section *sec)
{
  return (unsigned long long) bfd_section_size (sec);
}

unsigned long
_bfd_get_section_flags (struct bfd_section *sec)
{
  return (unsigned long) sec->flags;
}

void
bfd_read_symbols (bfd *abfd, int *cnt, asymbol **sy)
{
  long size;

  if (!(bfd_get_file_flags (abfd) & HAS_SYMS))
    {
      *cnt = 0;
      return;
    }
  size = bfd_get_symtab_upper_bound (abfd);
  if (size < 0)
    {
      *cnt = -1;
      return;
    }
  *cnt = bfd_canonicalize_symtab (abfd, sy);
}

void
bfd_find_nearest_line_ (bfd *abfd,
                        struct bfd_section *sec,
                        asymbol **syms,
                        unsigned long long addr,
                        const char **name,
                        const char **func,
                        unsigned *line)
{
  if (bfd_find_nearest_line (abfd, sec, syms,
                             (bfd_vma) (addr) - sec->vma, name, func, line))
    return;

  *line = -1;
}

int
ada_bfd_is_abs_section (struct bfd_section* sec)
{
   return bfd_is_abs_section (sec);
}

int
ada_bfd_is_com_section (struct bfd_section* sec)
{
   return bfd_is_com_section (sec);
}

int
ada_bfd_is_und_section (struct bfd_section* sec)
{
   return bfd_is_und_section (sec);
}

struct bfd_section*
ada_bfd_asymbol_section (asymbol* sym)
{
   return sym->section;
}

const char*
ada_bfd_asymbol_name (asymbol* sym)
{
   return bfd_asymbol_name (sym);
}


unsigned long long
ada_bfd_asymbol_value (asymbol* sym)
{
   return bfd_asymbol_value (sym);
}

unsigned long
ada_bfd_asymbol_flags (asymbol* sym)
{
   return sym->flags;
}

/**
 * The elf_internal_sym and elf_symbol_value structures are defined in binutils
 * bfd/elf-bfd.h and include/elf/internal.h headers.  These headers are not
 * installed on the system, so we get a copy of their definition here.
 * These definitions are only used by ada_bfd_asymbol_size to find the symbol size.
 */

typedef struct elf_internal_sym {
  bfd_vma       st_value;               /* Value of the symbol */
  bfd_vma       st_size;                /* Associated symbol size */
  unsigned long st_name;                /* Symbol name, index in string tbl */
  unsigned char st_info;                /* Type and binding attributes */
  unsigned char st_other;               /* Visibilty, and target specific */
  unsigned char st_target_internal;     /* Internal-only information */
  unsigned int  st_shndx;               /* Associated section index */
} Elf_Internal_Sym;

typedef struct elf_symbol_value {
  /* The BFD symbol.  */
  asymbol symbol;
  /* ELF symbol information.  */
  Elf_Internal_Sym internal_elf_sym;
  /* Backend specific information.  */
  union
    {
      unsigned int hppa_arg_reloc;
      void *mips_extr;
      void *any;
    }
  tc_data;

  /* Version information.  This is from an Elf_Internal_Versym
     structure in a SHT_GNU_versym section.  It is zero if there is no
     version information.  */
  unsigned short version;
} elf_symbol_value;

unsigned long long
ada_bfd_asymbol_size (asymbol* sym)
{
  bfd* abfd = bfd_asymbol_bfd (sym);

  if (bfd_get_flavour (abfd) == bfd_target_elf_flavour)
    return ((elf_symbol_value *) sym)->internal_elf_sym.st_size;
  else
    return 0;
}

unsigned long
ada_bfd_get_symtab_upper_bound (bfd* abfd)
{
   return bfd_get_symtab_upper_bound (abfd);
}

int
ada_bfd_get_section_contents (bfd* abfd, asection* sec,
                              void* p, unsigned long long pos,
                              unsigned long long size)
{
   return bfd_get_section_contents (abfd, sec, p,
                                    (file_ptr) pos, (bfd_size_type) size);
}

unsigned long long
ada_bfd_get_start_address (bfd* abfd)
{
   return bfd_get_start_address (abfd);
}

unsigned long
ada_bfd_get_symbol_count (bfd* abfd)
{
   return bfd_get_symcount (abfd);
}

int
ada_bfd_get_file_flags (bfd* abfd)
{
  return bfd_get_file_flags (abfd);
}
