-----------------------------------------------------------------------
--  BFD -- Thin Ada layer for Bfd (common Bfd functions)
--  <!-- Copyright (C) 2002, 2003, 2004 Free Software Foundation, Inc.
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
--  the Free Software Foundation, 59 Temple Place - Suite 330,
--  Boston, MA 02111-1307, USA.  -->
-----------------------------------------------------------------------
--  This package defines the C import to access to the BFD C library.
--
with Bfd.Sections; use Bfd.Sections;
with Bfd.Symtab; use Bfd.Symtab;
package Bfd.Thin.Symtab is

   function Get_Symbol_Name (Sym : in Symbol) return Ptr;
   pragma Import (C, Get_Symbol_Name, "ada_bfd_asymbol_name");

   function Get_Symbol_Section (Sym : in Symbol) return Section_Iterator;
   pragma Import (C, Get_Symbol_Section, "ada_bfd_asymbol_section");

   function Is_Local (P : Ptr; Sym : in Symbol) return Boolean;
   pragma Import (C, Is_Local, "bfd_is_local_label");

   function Get_Symbol_Value (Sym : in Symbol) return Symbol_Value;
   pragma Import (C, Get_Symbol_Value, "ada_bfd_asymbol_value");

   procedure Read_Symbols (File : Ptr;
                           Cnt : System.Address;
                           S : System.Address);
   pragma Import (C, Read_Symbols, "bfd_read_symbols");

   procedure Find_Nearest_Line (File : in Ptr;
                                Sec : in Section_Iterator;
                                Syms : in Ptr;
                                Addr : in Vma_Type;
                                Name : in Ptr;
                                Func : in Ptr;
                                Line : in Ptr);
   pragma Import (C, Find_Nearest_Line, "bfd_find_nearest_line_");

   --  function Is_Local (P : Ptr; Name : in String) return Boolean;
   --  pragma Import (C, Is_Local, "bfd_is_local_label_name");

   function Get_Symtab_Upper_Bound (File : in Ptr) return Integer;
   pragma Import (C, Get_Symtab_Upper_Bound, "ada_bfd_get_symtab_upper_bound");

end Bfd.Thin.Symtab;
