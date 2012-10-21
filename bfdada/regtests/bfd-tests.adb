-----------------------------------------------------------------------
--  BFD Tests -- Tests for Binary File Descriptor Library (Ada Interface)
--  Copyright (C) 2002, 2003, 2004, 2012 Free Software Foundation, Inc.
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
with AUnit.Test_Cases.Registration;

with AUnit.Assertions;
with Ada.Text_IO;
with Bfd.Symtab;
package body Bfd.Tests is

   use Ada.Strings.Unbounded;

   type Test_Ptr is access Test_Case;

   procedure Test_Open (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Basic (T : in out AUnit.Test_Cases.Test_Case'Class);

   function Create_Test (Test_Name : in String;
                         File_Name : in String) return Test_Ref;

   function Create_Test (Test_Name : in String;
                         File_Name : in String) return Test_Ref is
      T : constant Test_Ptr := new Test_Case;
   begin
      T.File_Name := To_Unbounded_String (File_Name);
      T.Test_Name := To_Unbounded_String (Test_Name);
      T.File := new File_Type;
      return T.all'Access;
   end Create_Test;

   function Get_Test_File (T : in Test_Case) return String is
   begin
      return To_String (T.File_Name);
   end Get_Test_File;

   procedure Set_Up (T : in out Test_Case) is
   begin
      T.File := new File_Type;
      Open (T.File.all, Get_Test_File (T));
   exception
      when OPEN_ERROR =>
         Ada.Text_IO.Put_Line ("Test file '" & Get_Test_File (T) & "' cannot be opened");
         Ada.Text_IO.Put_Line ("Several tests may fail");
   end Set_Up;

   procedure Tear_Down (T : in out Test_Case) is
   begin
      if Is_Open (T.File.all) then
         Close (T.File.all);
      end if;
   end Tear_Down;


   --  Test Routines:

   procedure Test_Open (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Test : constant Test_Case := Test_Case (T);
      Bfd  : File_Type;
   begin
      --  Check that OPEN_ERROR exception is raised.
      begin
         Open (Bfd, "src/bfd.adsx");
         AUnit.Assertions.Assert (False, "Bfd.Open didn't raise OPEN_ERROR exception");
      exception
         when OPEN_ERROR =>
            AUnit.Assertions.Assert (Is_Open (Bfd) = False,
                                     "Bfd.Is_Open returns true after OPEN_ERROR exception");
      end;

      --  Check that we can open a file.
      begin
         Open (Bfd, Get_Test_File (Test));
         AUnit.Assertions.Assert (Is_Open (Bfd), "Bfd.Is_Open returns false for opened file");
         Close (Bfd);
         AUnit.Assertions.Assert (Is_Open (Bfd) = False,
                                  "Bfd.Is_Open returns true after Bfd.Close");
      exception
         when OPEN_ERROR =>
            AUnit.Assertions.Assert (False, "Bfd.Open raised an exception when opening "
                                     & Get_Test_File (Test));
      end;
   end Test_Open;

   procedure Test_Basic (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Test : constant Test_Case := Test_Case (T);
      Symbols : Bfd.Symtab.Symbol_Table;
   begin
      --  Check that Get_Filename returns our test file.
      declare
         Name : constant String := Get_Filename (Test.File.all);
      begin
         AUnit.Assertions.Assert (Name = Get_Test_File (Test),
                                  "Bfd.Get_Filename returned an invalid filename");
      end;

      AUnit.Assertions.Assert (Check_Format (Test.File.all, OBJECT),
                               "Bfd.Check_Format returned false");

      --  We must load the symbol table first.
      Bfd.Symtab.Open_Symbols (Test.File.all, Symbols);
      Ada.Text_IO.Put_Line ("Count: " & Natural'Image (Get_Symbol_Count (Test.File.all)));

      --  Can't check in a portable way, assume some reasonable value.
      AUnit.Assertions.Assert (Get_Symbol_Count (Test.File.all) > 0
                               and Get_Symbol_Count (Test.File.all) < 10000,
                               "Bfd.Get_Symbol_Count returned 0");

   end Test_Basic;

   --  Register test routines to call:
   procedure Register_Tests (T : in out Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      --  Repeat for each test routine.
      Register_Routine (T, Test_Open'Access,
                        "Test Bfd.Open/Bfd.Close/Bfd.Is_Open");
      Register_Routine (T, Test_Basic'Access,
                        "Test Bfd.Get_Filename/Bfd.Get_Symbol_Count");
   end Register_Tests;

   --  Identifier of test case:
   function Name (T : Test_Case) return String_Access is
   begin
      return new String'(To_String (T.Test_Name));
   end Name;

   procedure Add_Tests (Suite : in AUnit.Test_Suites.Access_Test_Suite) is
      use AUnit.Test_Suites;
   begin
      Add_Test (Suite, Create_Test ("Bfd.Basic on object", "obj/bfd-tests.o"));
      Add_Test (Suite, Create_Test ("Bfd.Basic on exec", "bin/bfdada_harness"));
   end Add_Tests;

end Bfd.Tests;
