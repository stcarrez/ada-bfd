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
package Bfd.Thin.Sections is

   --  Section operations
   function Get_Section_Vma (Sect : in Section_Iterator) return Vma_Type;
   pragma Import (C, Get_Section_Vma, "_bfd_get_section_vma");

   function Get_Section_Lma (Sect : in Section_Iterator) return Lma_Type;
   pragma Import (C, Get_Section_Lma, "_bfd_get_section_lma");

   function Get_Section_Size (Sect : in Section_Iterator) return Size_Type;
   pragma Import (C, Get_Section_Size, "_bfd_get_section_size");

   function Get_Section_Flags
     (Sect : in Section_Iterator) return Section_Flags;
   pragma Import (C, Get_Section_Flags, "_bfd_get_section_flags");

   function Get_Section_Name (Sect : in Section_Iterator) return Ptr;
   pragma Import (C, Get_Section_Name, "_bfd_get_section_name");

   function Get_Sections (File : in Ptr) return Section_Iterator;
   pragma Import (C, Get_Sections, "bfd_get_sections");

   function Get_Next_Section (File : in Section_Iterator)
                              return Section_Iterator;
   pragma Import (C, Get_Next_Section, "bfd_next_section");


end Bfd.Thin.Sections;
