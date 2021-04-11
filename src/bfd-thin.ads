-----------------------------------------------------------------------
--  BFD -- Thin Ada layer for Bfd (common Bfd functions)
--  Copyright (C) 2002, 2003, 2004, 2012, 2021 Free Software Foundation, Inc.
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
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
--  the Free Software Foundation,51 Franklin Street - Fifth Floor,
--  Boston, MA 02110-1301, USA.
-----------------------------------------------------------------------
--  This package defines the C import to access to the BFD C library.
--
with Interfaces.C;
with Interfaces.C.Strings;
with Bfd;
package Bfd.Thin is

   function Get_Error_Type return Integer
     with Import => True, Convention => C, Link_Name => "bfd_get_error";

   function Get_Error_Message (Code : Error) return Interfaces.C.Strings.chars_ptr
     with Import => True, Convention => C, Link_Name => "bfd_errmsg";

   function Get_Filename (File : in Ptr) return Interfaces.C.Strings.chars_ptr
     with Import => True, Convention => C, Link_Name => "_bfd_get_filename";

   function Get_Error return Error
     with Import => True, Convention => C, Link_Name => "bfd_get_error";

   procedure Set_Error (Code : Error)
     with Import => True, Convention => C, Link_Name => "bfd_set_error";

   procedure Set_Error_Program_Name (P : in Interfaces.C.Strings.chars_ptr)
     with Import => True, Convention => C, Link_Name => "bfd_set_error_program_name";

   function Openr (Name : in Interfaces.C.Strings.chars_ptr; Target : in Ptr) return Ptr
     with Import => True, Convention => C, Link_Name => "bfd_openr";

   procedure Close (File : in Ptr)
     with Import => True, Convention => C, Link_Name => "bfd_close";

   function Check_Format (File : Ptr; What : Integer) return Interfaces.C.int
     with Import => True, Convention => C, Link_Name => "bfd_check_format";

   function Get_Start_Address (Bfd : Ptr) return Vma_Type
     with Import => True, Convention => C, Link_Name => "ada_bfd_get_start_address";

   function Get_Symbol_Count (Bfd : Ptr) return Natural
     with Import => True, Convention => C, Link_Name => "ada_bfd_get_symbol_count";

   function Get_File_Flags (Bfd : Ptr) return File_Flags
     with Import => True, Convention => C, Link_Name => "ada_bfd_get_file_flags";

   function Is_Undefined_Class (C : in Character) return Interfaces.C.int
     with Import => True, Convention => C, Link_Name => "bfd_is_undefined_symclass";

end Bfd.Thin;
