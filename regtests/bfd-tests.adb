-----------------------------------------------------------------------
--  BFD Tests -- Tests for Binary File Descriptor Library (Ada Interface)
--  Copyright (C) 2002, 2003, 2004, 2012, 2015 Free Software Foundation, Inc.
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

with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Ada.Streams;

with Bfd.Sections;
with Bfd.Symbols;
with Bfd.Disassembler;
package body Bfd.Tests is

   use Ada.Strings.Unbounded;

   procedure Disassemble_Section (File : in Bfd.Files.File_Type;
                                  Check_Size : Ada.Streams.Stream_Element_Offset);

   function Get_Test_File (T : in Test_Case) return String is
   begin
      return To_String (T.File_Name);
   end Get_Test_File;

   overriding
   procedure Set_Up (T : in out Test_Case) is
   begin
      T.File := new Bfd.Files.File_Type;
      Bfd.Files.Open (T.File.all, Get_Test_File (T));

   exception
      when OPEN_ERROR =>
         Ada.Text_IO.Put_Line ("Test file '" & Get_Test_File (T) & "' cannot be opened");
         Ada.Text_IO.Put_Line ("Several tests may fail");
   end Set_Up;

   overriding
   procedure Tear_Down (T : in out Test_Case) is
      procedure Free is
         new Ada.Unchecked_Deallocation (Bfd.Files.File_Type, File_Type_Access);
   begin
      if Bfd.Files.Is_Open (T.File.all) then
         Bfd.Files.Close (T.File.all);
      end if;
      Free (T.File);
   end Tear_Down;

   --  --------------------
   --  Perform the test.
   --  --------------------
   overriding
   procedure Run_Test (T : in out Test_Case) is
   begin
      T.Method (T);
   end Run_Test;

   --  --------------------
   --  Test the Open, Close and Is_Open operations.
   --  --------------------
   procedure Test_Open (T : in out Test_Case) is
      File  : Bfd.Files.File_Type;
   begin
      --  Check that OPEN_ERROR exception is raised.
      begin
         Bfd.Files.Open (File, "src/bfd.adsx");
         T.Assert (False, "Bfd.Open didn't raise OPEN_ERROR exception");
      exception
         when OPEN_ERROR =>
            T.Assert (not Bfd.Files.Is_Open (File),
                      "Bfd.Is_Open returns true after OPEN_ERROR exception");
      end;

      --  Check that we can open a file.
      begin
         Bfd.Files.Open (File, Get_Test_File (T));
         T.Assert (Bfd.Files.Is_Open (File), "Bfd.Is_Open returns false for opened file");
         Bfd.Files.Close (File);
         T.Assert (Bfd.Files.Is_Open (File) = False,
                   "Bfd.Is_Open returns true after Bfd.Close");
      exception
         when OPEN_ERROR =>
            T.Assert (False, "Bfd.Open raised an exception when opening "
                      & Get_Test_File (T));
      end;
   end Test_Open;

   procedure Test_Basic (T : in out Test_Case) is
      Symbols : Bfd.Symbols.Symbol_Table;
      --  pragma Unreferenced (Symbols);
   begin
      --  Check that Get_Filename returns our test file.
      declare
         Name : constant String := Bfd.Files.Get_Filename (T.File.all);
      begin
         T.Assert (Name = Get_Test_File (T),
                   "Bfd.Get_Filename returned an invalid filename");
      end;

      T.Assert (Bfd.Files.Check_Format (T.File.all, Bfd.Files.OBJECT),
                "Bfd.Check_Format returned false");
      T.Assert (not Bfd.Files.Check_Format (T.File.all, Bfd.Files.ARCHIVE),
                "Bfd.Check_Format returned true for ARCHIVE");
      T.Assert (not Bfd.Files.Check_Format (T.File.all, Bfd.Files.CORE),
                "Bfd.Check_Format returned true for CORE");
      T.Assert (not Bfd.Files.Check_Format (T.File.all, Bfd.Files.UNKNOWN),
                "Bfd.Check_Format returned true for UNKNOWN");

      --  We must load the symbol table first.
      Bfd.Symbols.Read_Symbols (T.File.all, Symbols);
      Ada.Text_IO.Put_Line ("Count: " & Natural'Image (Bfd.Files.Get_Symbol_Count (T.File.all)));

      --  Can't check in a portable way, assume some reasonable value.
      T.Assert (Bfd.Files.Get_Symbol_Count (T.File.all) > 0
                and Bfd.Files.Get_Symbol_Count (T.File.all) < 10000,
                "Bfd.Get_Symbol_Count returned 0");

   end Test_Basic;

   --  --------------------
   --  Test the Get_Flags operations.
   --  --------------------
   procedure Test_Get_Flags (T    : in out Test_Case;
                             Name : in String;
                             Flag : in Bfd.File_Flags) is
      File  : Bfd.Files.File_Type;
      Flags : Bfd.File_Flags;
   begin
      Bfd.Files.Open (File, Name);

      T.Assert (Bfd.Files.Check_Format (File, Bfd.Files.OBJECT),
                "Bfd.Check_Format returned false");

      Flags := Bfd.Files.Get_File_Flags (File);
      Ada.Text_IO.Put_Line ("File flags for " & Name & ": " & Bfd.File_Flags'Image (Flags));
      T.Assert ((Flags and Flag) /= 0,
                "The flag " & Bfd.File_Flags'Image (Flag) & " is not set on " & Name);
   end Test_Get_Flags;

   --  --------------------
   --  Test the Get_File_Flags operation on a binary executable.
   --  --------------------
   procedure Test_Get_Binary_Flags (T    : in out Test_Case) is
      File  : Bfd.Files.File_Type;
      Flags : Bfd.File_Flags;
   begin
      Bfd.Files.Open (File, "bin/bfdgen");

      T.Assert (Bfd.Files.Check_Format (File, Bfd.Files.OBJECT),
                "Bfd.Check_Format returned false");

      Flags := Bfd.Files.Get_File_Flags (File);
      T.Assert ((Flags and (Bfd.Files.DYNAMIC or Bfd.Files.EXEC_P)) /= 0,
                "The flag DYNAMIC or EXEC_P is not set");
   end Test_Get_Binary_Flags;

   --  --------------------
   --  Test the Get_File_Flags operation on a object file.
   --  --------------------
   procedure Test_Get_Object_Flags (T    : in out Test_Case) is
   begin
      Test_Get_Flags (T, "obj/bfd-tests.o", Bfd.Files.HAS_SYMS);
      Test_Get_Flags (T, "obj/bfd-tests.o", Bfd.Files.HAS_RELOC);
   end Test_Get_Object_Flags;

   --  --------------------
   --  Test the Get_File_Flags operation on a object file.
   --  --------------------
   procedure Test_Get_Debug_Flags (T    : in out Test_Case) is
   begin
      Test_Get_Flags (T, "bin/bfdgen", Bfd.Files.HAS_SYMS);
   end Test_Get_Debug_Flags;

   --  --------------------
   --  Test re-opening without closing.
   --  --------------------
   procedure Test_Reopen (T : in out Test_Case) is
   begin
      for I in 1 .. 5 loop
         Bfd.Files.Open (T.File.all, Get_Test_File (T));
         T.Assert (Bfd.Files.Is_Open (T.File.all), "Bfd.Is_Open returns false for opened file");
      end loop;
   end Test_Reopen;

   --  --------------------
   --  Test trying to make some operation while the file is not opened.
   --  --------------------
   procedure Test_Use_Error (T : in out Test_Case) is
      File  : Bfd.Files.File_Type;
   begin
      --  Check USE_ERROR exception
      begin
         T.Assert (not Bfd.Files.Is_Open (File), "Is_Open failed");
         T.Assert (not Bfd.Files.Check_Format (File, Bfd.Files.OBJECT),
                   "Check_Format must return false");
         T.Assert ("" /= Bfd.Files.Get_Filename (File),
                   "No exception raised");

      exception
         when USE_ERROR =>
            null;
      end;
   end Test_Use_Error;

   --------------------------------------------------
   --  List the sections of the BFD file
   --------------------------------------------------
   procedure Disassemble_Section (File : in Bfd.Files.File_Type;
                                  Check_Size : Ada.Streams.Stream_Element_Offset) is
      use Ada.Streams;

      type Small_Disassembler is new Bfd.Disassembler.Memory_Disassembler_Info_Type
         with record
            Result : Unbounded_String;
         end record;

      overriding
      procedure Output (Info : in out Small_Disassembler;
                        Item : in String);

      overriding
      procedure Output (Info : in out Small_Disassembler;
                        Item : in String) is
      begin
         Append (Info.Result, Item);
      end Output;

      Text_Section : constant Bfd.Sections.Section := Bfd.Sections.Find_Section (File, ".text");
      Size         : constant Ada.Streams.Stream_Element_Offset
        := Ada.Streams.Stream_Element_Offset (Text_Section.Size);
      Addr         : Bfd.Vma_Type := Text_Section.Vma;
      Section      : Ada.Streams.Stream_Element_Array (1 .. Size);
      Last         : Ada.Streams.Stream_Element_Offset;
      Info         : Small_Disassembler;
      Symbols      : Bfd.Symbols.Symbol_Table;
      Path         : Ada.Strings.Unbounded.Unbounded_String;
      Func         : Ada.Strings.Unbounded.Unbounded_String;
      Line         : Natural;
   begin
      Bfd.Symbols.Read_Symbols (File, Symbols);
      Bfd.Sections.Get_Section_Contents (File, Text_Section, 0, Section, Last);
      if Check_Size < Size then
         Bfd.Disassembler.Initialize (Info, File, "", Text_Section.Vma,
                                      Section (1 .. Check_Size));
      else
         Bfd.Disassembler.Initialize (Info, File, "", Text_Section.Vma,
                                      Section (1 .. Size));
      end if;
      Info.Set_Symbol_Table (Symbols);
      loop
         Bfd.Symbols.Find_Nearest_Line (File, Text_Section, Symbols, Addr, Path, Func, Line);

         Info.Disassemble (Addr, Addr);
         exit when Addr >= Text_Section.Vma + Bfd.Vma_Type (Size);
      end loop;
   end Disassemble_Section;

   --  --------------------
   --  Test disassembler API.
   --  --------------------
   procedure Test_Disassembler (T : in out Test_Case) is
   begin
      T.Assert (Bfd.Files.Check_Format (T.File.all, Bfd.Files.OBJECT), "Invalid format");
      begin
         Disassemble_Section (T.File.all, 10);

      exception
         when Bfd.BFD_ERROR =>
            null;
      end;

      Disassemble_Section (T.File.all, 100000);
   end Test_Disassembler;

   --  --------------------
   --  Identifier of test case:
   --  --------------------
   function Name (T : in Test_Case) return Util.Tests.Message_String is
   begin
      return To_String (T.Test_Name);
   end Name;

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is

      procedure Add_Test (Test_Name : in String;
                          File_Name : in String;
                          Method    : in Test_Method_Access);

      procedure Add_Test (Test_Name : in String;
                          File_Name : in String;
                          Method    : in Test_Method_Access) is
         T : constant Test_Case_Access := new Test_Case;
      begin
         T.File_Name := To_Unbounded_String (File_Name);
         T.Test_Name := To_Unbounded_String (Test_Name);
         T.Method    := Method;
         Suite.Add_Test (T.all'Access);
      end Add_Test;

   begin
      Add_Test ("Test Bfd.Open/Bfd.Close/Bfd.Is_Open on object",
                "obj/bfd-tests.o", Test_Open'Access);
      Add_Test ("Test Bfd.Open/Bfd.Close/Bfd.Is_Open on exec",
                "bin/bfdada_harness", Test_Open'Access);

      Add_Test ("Test Bfd.Get_Filename/Bfd.Get_Symbol_Count on object",
                "obj/bfd-tests.o", Test_Basic'Access);

      Add_Test ("Test Bfd.Files.Get_File_Flags on exec",
                "bin/bfdada_harness", Test_Get_Binary_Flags'Access);

      Add_Test ("Test Bfd.Files.Get_File_Flags on objectc",
                "obj/bfd-tests.o", Test_Get_Object_Flags'Access);

      Add_Test ("Test Bfd.Files.Get_File_Flags on exec with debug",
                "bin/bfdgen", Test_Get_Debug_Flags'Access);

      Add_Test ("Test Bfd.Files.Open multiple times",
                "bin/bfdgen", Test_Reopen'Access);
      Add_Test ("Test Bfd.Files.USE_ERROR",
                "bin/bfdgen", Test_Use_Error'Access);
      Add_Test ("Test Bfd.Disassembler.Disassemble",
                "bin/bfdgen", Test_Disassembler'Access);

      --  Running the symbol count on the binary will fail if it is stripped.
      Add_Test ("Test Bfd.Get_Filename/Bfd.Get_Symbol_Count on exec",
                "bin/bfdgen", Test_Basic'Access);
   end Add_Tests;

end Bfd.Tests;
