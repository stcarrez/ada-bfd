-----------------------------------------------------------------------
--  BFD -- Binary File Descriptor Library (Ada Interface)
--  Copyright (C) 2002, 2003 Free Software Foundation, Inc.
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
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Streams; use Ada.Streams;
with Bfd.Internal; use Bfd.Internal;
with Bfd.Thin.Sections;
package body Bfd.Sections is

   function Get_Sections (File : in File_Type) return Section_Iterator is

      Iter : Section_Iterator;
   begin
      Iter := Bfd.Thin.Sections.Get_Sections (File.Abfd);
      return Iter;
   end Get_Sections;

   procedure Next (Iter : in out Section_Iterator) is

   begin
      Iter := Bfd.Thin.Sections.Get_Next_Section (Iter);
   end Next;

   function Is_Done (Iter : in Section_Iterator) return Boolean is
   begin
      return System.Address (Iter) = Null_Address;
   end Is_Done;

   function Current (Iter : in Section_Iterator) return Section is
      S : Section;
   begin
      S.Vma := Bfd.Thin.Sections.Get_Section_Vma (Iter);
      S.Lma := Bfd.Thin.Sections.Get_Section_Lma (Iter);
      S.Size := Bfd.Thin.Sections.Get_Section_Size (Iter);
      S.Flags := Bfd.Thin.Sections.Get_Section_Flags (Iter);
      S.Opaque := Iter;
      return S;
   end Current;

   function Get_Name (S : in Section) return String is
   begin
      return To_Ada (Bfd.Thin.Sections.Get_Section_Name (S.Opaque));
   end Get_Name;

   --  Return true if this is the UNDEF section.
   function Is_Undefined_Section (S : in Section) return Boolean is

      function Is_Undefined (S : Section_Iterator) return Boolean;
      pragma Import (C, Is_Undefined, "ada_bfd_is_und_section");

   begin
      return Is_Undefined (S.Opaque);
   end Is_Undefined_Section;

   --  Return true if this is the COMMON section.
   function Is_Common_Section (S : in Section) return Boolean is

      function Is_Common (S : Section_Iterator) return Boolean;
      pragma Import (C, Is_Common, "ada_bfd_is_com_section");

   begin
      return Is_Common (S.Opaque);
   end Is_Common_Section;

   --  Return true if this is the ABS section.
   function Is_Absolute_Section (S : in Section) return Boolean is

      function Is_Absolute (S : Section_Iterator) return Boolean;
      pragma Import (C, Is_Absolute, "ada_bfd_is_abs_section");

   begin
      return Is_Absolute (S.Opaque);
   end Is_Absolute_Section;

   --  Get the content of the section starting at the given position.
   --  The result is truncated if the buffer is not large enough
   procedure Get_Section_Contents (File : in File_Type;
                                   S : in Section;
                                   Pos : in Stream_Element_Offset := 0;
                                   Item : out Stream_Element_Array;
                                   Last : out Stream_Element_Offset) is

      function Get_Section_Contents (Bfd : in Ptr;
                                     S : in Section_Iterator;
                                     Buf : in System.Address;
                                     Pos : in Unsigned_64;
                                     Size : in Unsigned_64) return Boolean;
      pragma Import (C, Get_Section_Contents, "ada_bfd_get_section_contents");

      Result : Boolean := Get_Section_Contents (File.Abfd,
                                                S.Opaque,
                                                Item (Item'First)'Address,
                                                Unsigned_64 (Pos),
                                                Item'Length);
   begin
      if Result then
         Last := Item'First + Item'Length - 1;
      else
         Last := Item'First - 1;
      end if;
   end Get_Section_Contents;

   --  Set the content of the section
   procedure Set_Section_Content (File : in File_Type;
                                  S : in out Section;
                                  Item : in Stream_Element_Array) is
   begin
      null;
   end Set_Section_Content;

   function Find_Section (File : in File_Type;
                          Name : in String) return Section is
      Iter : Section_Iterator := Get_Sections (File);
   begin
      while not Is_Done (Iter) loop
         declare
            S : Section := Current (Iter);
         begin
            if Get_Name (S) = Name then
               return S;
            end if;
            Next (Iter);
         end;
      end loop;
      Put_Line ("Section " & Name & " not found");
      raise NOT_FOUND;
   end Find_Section;

   --  Set the size of the section.
   procedure Set_Section_Size (File : in File_Type;
                               S : in out Section;
                               Size : in Size_Type) is
   begin
      null;
   end Set_Section_Size;

   procedure Set_Section_Contents (File : in File_Type;
                                   S : in out Section;
                                   Offset : in Offset_Type;
                                   Size : in Size_Type) is
   begin
      null;
   end Set_Section_Contents;

end Bfd.Sections;
