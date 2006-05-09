-----------------------------------------------------------------------
--  BFD -- Thin Ada layer for Bfd disassembler (common Bfd functions)
--  <!-- Copyright (C) 2002, 2003, 2004, 2006 Free Software Foundation, Inc.
--  Written by Stephane Carrez (stcarrez@nerim.fr)
--
--  This file is part of BfdAda.
--
--  This program is free software; you can redistribute it and/or
--  modify it under the terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2,
--  or (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; see the file COPYING.  If not, write to
--  the Free Software Foundation, 51 Franklin Street - Fifth Floor,
--  Boston, MA 02110-1301, USA.  -->
-----------------------------------------------------------------------
--  This package defines the C import to access to the BFD C library.
--
with Bfd.Sections;               use Bfd.Sections;
with Bfd.Symtab;                 use Bfd.Symtab;
with Interfaces.C.Strings;       use Interfaces.C;
with Bfd.Symtab;                 use Bfd.Symtab;
package Bfd.Thin.Disassembler is

   function Disassembler_Init (Data : in Ptr; Bfd : Ptr;
                               Options: in Strings.chars_ptr) return Ptr;
   pragma Import (C, Disassembler_Init, "bfd_ada_disassembler_init");

   function Disassemble (Bfd : in Ptr; Data : in Ptr; Addr : in Vma_Type)
                         return Integer;
   pragma Import (C, Disassemble, "bfd_ada_disassembler_disassemble");

   procedure Set_Buffer (D : Ptr; Buf : Ptr; Len : Integer; Addr : Vma_Type);
   pragma Import (C, Set_Buffer, "bfd_ada_disassembler_set_buffer");

   procedure Set_Symbol_Table (D: Ptr; Sym : Symbol_Array_Access; Count : Integer);
   pragma Import (C, Set_Symbol_Table, "bfd_ada_disassembler_set_symbol_table");

end Bfd.Thin.Disassembler;
