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

with Bfd.Sections;
with Bfd.Thin.Constants;
package body Bfd.Symtab.Tests is

   use Ada.Strings.Unbounded;
   use type Bfd.Thin.Constants.Section_Flags;

   --  --------------------
   --  Test loading the symbol table
   --  --------------------
   procedure Test_Open_Symbols (T : in out Test_Case) is
      Symbols : Bfd.Symtab.Symbol_Table;
   begin
      T.Assert (Check_Format (T.File.all, OBJECT),
                "Bfd.Check_Format returned false");

      --  We must load the symbol table first.
      Bfd.Symtab.Open_Symbols (T.File.all, Symbols);

      --  Can't check in a portable way, assume some reasonable value.
      T.Assert (Get_Symbol_Count (T.File.all) > 0
                and Get_Symbol_Count (T.File.all) < 10000,
                "Bfd.Get_Symbol_Count returned 0");

   end Test_Open_Symbols;


   --  --------------------
   --  Test the symbol iterator
   --  --------------------
   procedure Test_Symbol_Iterator (T : in out Test_Case) is
      Symbols : Bfd.Symtab.Symbol_Table;
      Iter    : Symbol_Iterator;
      Count   : Natural;
   begin
      T.Assert (Check_Format (T.File.all, OBJECT),
                "Bfd.Check_Format returned false");

      --  We must load the symbol table first.
      Bfd.Symtab.Open_Symbols (T.File.all, Symbols);

      Iter := Bfd.Symtab.Get_Iterator (Symbols);
      T.Assert (Bfd.Symtab.Has_Element (Iter),
                "The symbol table seems empty.");

      --  Can't check in a portable way, assume some reasonable value.
      T.Assert (Get_Symbol_Count (T.File.all) > 0
                and Get_Symbol_Count (T.File.all) < 10000,
                "Bfd.Get_Symbol_Count returned 0");

      Count := 0;
      while Bfd.Symtab.Has_Element (Iter) loop
         declare
            Sym : constant Symbol := Bfd.Symtab.Element (Iter);
         begin
            Ada.Text_IO.Put_Line (Bfd.Symtab.Get_Name (Sym));
            Bfd.Symtab.Next (Iter);
         end;
         Count := Count + 1;
      end loop;

      Ada.Text_IO.Put_Line ("Iterate count: " & Natural'Image (Count));
      Ada.Text_IO.Put_Line ("Symbol count: " & Natural'Image (Get_Symbol_Count (T.File.all)));

      --  Iterator must match the symbol count.
      T.Assert (Get_Symbol_Count (T.File.all) = Count,
                "Bfd.Get_Symbol_Count returned 0");

   end Test_Symbol_Iterator;

   --  --------------------
   --  Add the tests in the testsuite
   --  --------------------
   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is

      procedure Add_Test (Test_Name : in String;
                          File_Name : in String;
                          Method    : in Bfd.Tests.Test_Method_Access);

      procedure Add_Test (Test_Name : in String;
                          File_Name : in String;
                          Method    : in Bfd.Tests.Test_Method_Access) is
         T : constant Bfd.Tests.Test_Case_Access := new Test_Case;
      begin
         T.File_Name := To_Unbounded_String (File_Name);
         T.Test_Name := To_Unbounded_String (Test_Name);
         T.Method    := Method;
         T.File := new File_Type;
         Suite.Add_Test (T.all'Access);
      end Add_Test;
   begin
      Add_Test ("Test Bfd.Symtab.Open_Symbols",
                "obj/bfd-tests.o", Test_Open_Symbols'Access);
      Add_Test ("Test Bfd.Symtab.Get_Iterator",
                "obj/bfd-tests.o", Test_Symbol_Iterator'Access);
   end Add_Tests;

end Bfd.Symtab.Tests;
