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
with Interfaces.C;
with Bfd.Internal;
with Bfd.Thin;
package body Bfd is

   use type System.Address;

   Current_Program_Name : Bfd.Internal.String_Ptr := null;

   Safe_Mode : constant Boolean := True;

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
      S : constant Bfd.Internal.String_Ptr := new String (1 .. To'Length + 1);
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
      return Bfd.Internal.To_Ada (Bfd.Thin.Get_Error_Message (Code));
   end Get_Error_Message;

   procedure Open (File : in out File_Type;
                   Name : in String;
                   Target : in String := "") is

   begin
      if File.Abfd /= System.Null_Address then
         Close (File);
      end if;

      File.Name := Interfaces.C.Strings.New_String (Name);
      if Target = "" then
         File.Abfd := Bfd.Thin.Openr (File.Name, System.Null_Address);
      else
         File.Abfd := Bfd.Thin.Openr (File.Name, System.Null_Address);
      end if;

      if File.Abfd = System.Null_Address then
         raise OPEN_ERROR;
      end if;
   end Open;

   procedure Close (File : in out File_Type) is
      use type Interfaces.C.Strings.chars_ptr;
   begin
      if File.Abfd /= System.Null_Address then
         Bfd.Thin.Close (File.Abfd);
         File.Abfd := System.Null_Address;
      end if;
      if File.Name /= Interfaces.C.Strings.Null_Ptr then
         Interfaces.C.Strings.Free (File.Name);
      end if;
   end Close;

   function Is_Open (File : in File_Type) return Boolean is
   begin
      return File.Abfd /= System.Null_Address;
   end Is_Open;

   function Get_Filename (File : in File_Type) return String is
   begin
      Check_Bfd (File);
      return Bfd.Internal.To_Ada (Bfd.Thin.Get_Filename (File.Abfd));
   end Get_Filename;

   function Check_Format (File : in File_Type;
                          Expect : in Format) return Boolean is
      use type Interfaces.C.int;

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

      return Bfd.Thin.Check_Format (File.Abfd, N) /= 0;
   end Check_Format;

   function Get_File_Flags (File : in File_Type) return Flags is
      pragma Unreferenced (File);
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

   overriding
   procedure Finalize (File : in out File_Type) is
   begin
      Close (File);
   end Finalize;

end Bfd;
