-----------------------------------------------------------------------
--  BFD Tests -- Tests for BFD section Ada API
--  Copyright (C) 2002, 2003, 2012 Free Software Foundation, Inc.
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
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Streams;

with AUnit.Options;
with AUnit.Test_Cases.Registration;
with AUnit.Assertions;
with AUnit.Tests;
with AUnit.Lists;
with Bfd.Sections;
package body Bfd.Sections.Tests is

   use Ada.Strings.Unbounded;

   type Test_Ptr is access Test_Case;

   --  --------------------
   --  Create a test case object
   --  --------------------
   function Create_Test (Test_Name : in String;
                         File_Name : in String) return Test_Ptr is
      T : Test_Ptr := new Test_Case;
   begin
      T.File_Name := To_Unbounded_String (File_Name);
      T.Test_Name := To_Unbounded_String (Test_Name);
      return T;
   end Create_Test;

   -- Test Routines:

   --  --------------------
   --  Test basic sections operations
   --  --------------------
   procedure Test_Sections (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Test : Test_Case := Test_Case (T);
      S : Section_Iterator;

      Has_Code : Boolean := False;
   begin
      AUnit.Assertions.Assert (Check_Format (Test.File, OBJECT),
                               "Bfd.Check_Format returned false");

      S := Get_Sections (Test.File);
      AUnit.Assertions.Assert (Is_Done (S) = False,
                               "Bfd.Create_Iterator returned null section iterator");

      while not Is_Done (S) loop
         declare
            Sec : Section := Current (S);
         begin
            AUnit.Assertions.Assert (Get_Name (Sec)'Length > 0,
                                     "Bfd.Sections.Get_Name returned empty name");

            if (Sec.Flags and SEC_CODE) /= 0 then
               AUnit.Assertions.Assert ((Sec.Flags and SEC_LOAD) /= 0,
                                        "SEC_CODE is set but SEC_LOAD is not");
               Has_Code := True;
            end if;
         end;
         Next (S);
      end loop;
      AUnit.Assertions.Assert (Has_Code, "No SEC_CODE section found");
   end Test_Sections;


   --  --------------------
   --  Test find sections operations
   --  --------------------
   procedure Test_Find_Section (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Test : Test_Case := Test_Case (T);
      Sec  : Section;
   begin
      AUnit.Assertions.Assert (Check_Format (Test.File, OBJECT),
                               "Bfd.Check_Format returned false");

      --  Check that Find_Section raises an exception when the section
      --  is not known.
      begin
         Sec := Find_Section (Test.File, "toto section");

         AUnit.Assertions.Assert (False, "Bfd.Find_Section didn't raise an exception");
      exception
         when Bfd.NOT_FOUND =>
            null;
      end;

      --  Verify that Find_Section returns the good sections.
      declare
         It : Section_Iterator := Get_Sections (Test.File);
         S  : Section;
      begin
         AUnit.Assertions.Assert (Is_Done (It) = False,
                                  "Bfd.Create_Iterator returned a null section");

         while not Is_Done (It) loop
            S := Current (It);
            Sec := Find_Section (Test.File, Get_Name (S));
            AUnit.Assertions.Assert (Sec = S, "Bfd.Find_Section returned a different section");
            Next (It);
         end loop;
      end;
   end Test_Find_Section;


   --  --------------------
   --  Test get section content operations
   --  --------------------
   procedure Test_Get_Section_Contents (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Test : Test_Case := Test_Case (T);
      Sec  : Section;
      It   : Section_Iterator;
      Read_Something : Boolean := False;
   begin
      AUnit.Assertions.Assert (Check_Format (Test.File, OBJECT),
                               "Bfd.Check_Format returned false");

      It := Get_Sections (Test.File);
      AUnit.Assertions.Assert (Is_Done (It) = False,
                               "Bfd.Create_Iterator returned a null section");

      --  Scan each section and load its content in memory.
      while not Is_Done (It) loop
         Sec := Current (It);
         if Sec.Size /= 0 and (Sec.Flags and SEC_HAS_CONTENTS) /= 0 then
            declare
               use Ada.Streams;

               Cnt : Stream_Element_Offset := Stream_Element_Offset (Sec.Size);
               Buf  : Stream_Element_Array (1 .. Cnt) := (others => 0);
               Last : Stream_Element_Offset;
               Seems_Filled : Boolean := False;
            begin
               --  Get section content in buffer.
               Get_Section_Contents (Test.File, Sec, 0, Buf, Last);
               AUnit.Assertions.Assert (Last = Cnt,
                                        "Cannot get content of section " & Get_Name (Sec));

               if AUnit.Options.Verbose then
                  Ada.Text_IO.Put_Line ("Read content of " & Get_Name (Sec)
                                        & " " & Stream_Element_Offset'Image (Last)
                                        & " bytes read");
               end if;

               --  Crude test to check we got something in.
               for I in Buf'First .. Last loop
                  if Buf (I) /= 0 then
                     Seems_Filled := True;
                     exit;
                  end if;
               end loop;
               AUnit.Assertions.Assert (Seems_Filled,
                                        "Section " & Get_Name (Sec) & " contains all 0");
               Read_Something := True;
            end;
         end if;
         Next (It);
      end loop;

      --  Be sure we loaded some section.
      AUnit.Assertions.Assert (Read_Something, "No section was loaded");
   end Test_Get_Section_Contents;


   --  --------------------
   --  Register test routines to call:
   --  --------------------
   procedure Register_Tests (T : in out Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      --  Repeat for each test routine.
      Register_Routine (T, Test_Sections'Access,
                        "Test Bfd.Sections.Create_Iterator/Is_Done/Next"
                        & "/Get_Name");
      Register_Routine (T, Test_Find_Section'Access,
                        "Test Bfd.Sections.Find_Section");
      Register_Routine (T, Test_Get_Section_Contents'Access,
                        "Test Bfd.Sections.Get_Section_Contents");
   end Register_Tests;


   --  --------------------
   --  Add the tests in the testsuite
   --  --------------------
   procedure Add_Tests (Suite : in AUnit.Test_Suites.Access_Test_Suite) is
      use AUnit.Test_Suites;
   begin
      Add_Test (Suite, Create_Test ("Bfd.Sections on object", "bfd-tests.o"));
      Add_Test (Suite, Create_Test ("Bfd.Sections on exec", "harness"));
   end Add_Tests;

end Bfd.Sections.Tests;
