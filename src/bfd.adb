-----------------------------------------------------------------------
--  BFD -- Binary File Descriptor Library (Ada Interface)
--  Copyright (C) 2001, 2002, 2003, 2004, 2012 Free Software Foundation, Inc.
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
--  The Bfd package exports the GNU Bfd library found in Binutils
--  and Gdb.  It is not intended to be as complete as the C library
--  but still provide enough methods to read any object or binary,
--  observe its sections, its symbol table.
--
with Bfd.Thin;
with Interfaces.C.Strings;
package body Bfd is

   Current_Program_Name : Interfaces.C.Strings.chars_ptr;

   --  -----------------------
   --  Return the current error code.
   --  -----------------------
   function Get_Error return Error is
   begin
      return Bfd.Thin.Get_Error;
   end Get_Error;

   --  -----------------------
   --  Set the current error code.
   --  -----------------------
   procedure Set_Error (To : in Error) is
   begin
      Bfd.Thin.Set_Error (To);
   end Set_Error;

   --  -----------------------
   --  Tell the BFD library what is the program name.
   --  -----------------------
   procedure Set_Error_Program_Name (To : in String) is
   begin
      Interfaces.C.Strings.Free (Current_Program_Name);
      Current_Program_Name := Interfaces.C.Strings.New_String (To);
      Bfd.Thin.Set_Error_Program_Name (Current_Program_Name);
   end Set_Error_Program_Name;

   --  -----------------------
   --  Return an error message corresponding to the last error
   --  This is equivalent to the C bfd_errmsg.
   --  -----------------------
   function Get_Error_Message (Code : Error) return String is
   begin
      return Interfaces.C.Strings.Value (Bfd.Thin.Get_Error_Message (Code));
   end Get_Error_Message;

end Bfd;
