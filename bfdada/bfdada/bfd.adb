-----------------------------------------------------------------------
--  BFD -- Binary File Descriptor Library (Ada Interface)
--  Copyright (C) 2001, 2002, 2003, 2004 Free Software Foundation, Inc.
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
--  Boston, MA 02111-1307, USA.
-----------------------------------------------------------------------
--  The Bfd package exports the GNU Bfd library found in Binutils
--  and Gdb.  It is not intended to be as complete as the C library
--  but still provide enough methods to read any object or binary,
--  observe its sections, its symbol table.
--
with System; use System;
with Bfd.Internal; use Bfd.Internal;
with Bfd.Thin;
package body Bfd is

   Current_Program_Name : String_Ptr := null;

   Safe_Mode : Boolean := True;

   procedure Check_Bfd (File : in File_Type);
   pragma Inline_Always (Check_Bfd);

   --  -----------------------
   --  Check if the BFD file is valid
   --  Raise USE_ERROR if not
   --  -----------------------
   procedure Check_Bfd (File : in File_Type) is
   begin
      if Safe_Mode and File.Abfd = System.Null_Address then
         raise USE_ERROR;
      end if;
   end Check_Bfd;

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
      S : String_Ptr := new String (1 .. To'Length + 1);
   begin
      S (1 .. To'Length)   := To (To'Range);
      S (To'Length + 1)    := ASCII.NUL;
      Current_Program_Name := S;
      Bfd.Thin.Set_Error_Program_Name (S.all'Address);
   end Set_Error_Program_Name;

   --  -----------------------
   --  Set a new error handler in BFD library.
   --  -----------------------
   procedure Set_Error_Handler (To  : in Error_Handler;
                                Old : out Error_Handler) is
   begin
      Old := Bfd.Thin.Set_Error_Handler (To);
   end Set_Error_Handler;

   --  -----------------------
   --  Return an error message corresponding to the last error
   --  This is equivalent to the C bfd_errmsg.
   --  -----------------------
   function Get_Error_Message (Code : Error) return String is
   begin
      return To_Ada (Bfd.Thin.Get_Error_Message (Code));
   end Get_Error_Message;

   procedure Open (File : in out File_Type;
                   Name : in String;
                   Target : in String := "") is

   begin
      if Target = "" then
         File.Abfd := Bfd.Thin.Openr (Name & ASCII.NUL, Null_Address);
      else
         File.Abfd := Bfd.Thin.Openr (Name & ASCII.NUL, Null_Address);
      end if;

      if File.Abfd = Null_Address then
         raise OPEN_ERROR;
      end if;
   end Open;

   procedure Close (File : in out File_Type) is

   begin
      if File.Abfd /= System.Null_Address then
         Bfd.Thin.Close (File.Abfd);
         File.Abfd := System.Null_Address;
      end if;
   end Close;

   function Is_Open (File : in File_Type) return Boolean is
   begin
      return File.Abfd /= System.Null_Address;
   end Is_Open;

   function Get_Filename (File : in File_Type) return String is
   begin
      Check_Bfd (File);
      return To_Ada (Bfd.Thin.Get_Filename (File.Abfd));
   end Get_Filename;

   function Check_Format (File : in File_Type;
                          Expect : in Format) return Boolean is

      N : Integer;
   begin
      Check_Bfd (File);
      case Expect is
         when UNKNOWN =>
            N := 0;

         when OBJECT =>
            N := 1;

         when ARCHIVE =>
            N := 2;

         when others =>
            N := 0;
      end case;

      return Bfd.Thin.Check_Format (File.Abfd, N);
   end Check_Format;

   function Get_File_Flags (File : in File_Type) return Flags is
   begin
      return 0;
   end Get_File_Flags;

   --  -----------------------
   --  Get the start address.
   --  -----------------------
   function Get_Start_Address (File : in File_Type) return Vma_Type is
   begin
      Check_Bfd (File);
      return Bfd.Thin.Get_Start_Address (File.Abfd);
   end Get_Start_Address;

   --  -----------------------
   --  Return number of symbols.
   --  -----------------------
   function Get_Symbol_Count (File : in File_Type) return Natural is
   begin
      Check_Bfd (File);
      return Bfd.Thin.Get_Symbol_Count (File.Abfd);
   end Get_Symbol_Count;

end Bfd;
