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

with Bfd.Files;
with Bfd.Sections;
with Bfd.Thin.Constants;
package body Bfd.Symtab.Tests is

   use Ada.Strings.Unbounded;
   use type Bfd.Thin.Constants.Section_Flags;
   use type Bfd.Thin.Constants.Symbol_Flags;

   --  --------------------
   --  Test loading the symbol table
   --  --------------------
   procedure Test_Open_Symbols (T : in out Test_Case) is
      Symbols : Bfd.Symtab.Symbol_Table;
   begin
      T.Assert (Bfd.Files.Check_Format (T.File.all, Bfd.Files.OBJECT),
                "Bfd.Check_Format returned false");

      --  We must load the symbol table first.
      Bfd.Symtab.Open_Symbols (T.File.all, Symbols);

      --  Can't check in a portable way, assume some reasonable value.
      T.Assert (Bfd.Files.Get_Symbol_Count (T.File.all) > 0
                and Bfd.Files.Get_Symbol_Count (T.File.all) < 10000,
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
      T.Assert (Bfd.Files.Check_Format (T.File.all, Bfd.Files.OBJECT),
                "Bfd.Check_Format returned false");

      --  We must load the symbol table first.
      Bfd.Symtab.Open_Symbols (T.File.all, Symbols);

      Iter := Bfd.Symtab.Get_Iterator (Symbols);
      T.Assert (Bfd.Symtab.Has_Element (Iter),
                "The symbol table seems empty.");

      --  Can't check in a portable way, assume some reasonable value.
      T.Assert (Bfd.Files.Get_Symbol_Count (T.File.all) > 0
                and Bfd.Files.Get_Symbol_Count (T.File.all) < 10000,
                "Bfd.Get_Symbol_Count returned 0");

      Count := 0;
      while Bfd.Symtab.Has_Element (Iter) loop
         declare
            Sym  : constant Symbol := Bfd.Symtab.Element (Iter);
            Name : constant String := Bfd.Symtab.Get_Name (Sym);
         begin
            T.Assert (Name'Length > 0, "Bfd.Symtab.Get_Name returns an empty name");
            if Util.Tests.Verbose then
               Ada.Text_IO.Put_Line (Name);
            end if;
            Bfd.Symtab.Next (Iter);
         end;
         Count := Count + 1;
      end loop;

      Ada.Text_IO.Put_Line ("Iterate count: " & Natural'Image (Count));
      Ada.Text_IO.Put_Line ("Symbol count: " & Natural'Image (Bfd.Files.Get_Symbol_Count (T.File.all)));

      --  Iterator must match the symbol count.
      T.Assert (Bfd.Files.Get_Symbol_Count (T.File.all) = Count,
                "Bfd.Get_Symbol_Count returned 0");

   end Test_Symbol_Iterator;

   --  --------------------
   --  Test the getting a symbol by name
   --  --------------------
   procedure Test_Symbol (T         : in out Test_Case;
                          Name      : in String;
                          Flag      : in Bfd.Symbol_Flags;
                          Undefined : in Boolean) is
      Symbols : Bfd.Symtab.Symbol_Table;
      Sym     : Bfd.Symtab.Symbol;
   begin
      T.Assert (Bfd.Files.Check_Format (T.File.all, Bfd.Files.OBJECT),
                "Bfd.Check_Format returned false");

      --  We must load the symbol table first.
      Bfd.Symtab.Open_Symbols (T.File.all, Symbols);

      Sym := Bfd.Symtab.Get_Symbol (Symbols, Name);
      T.Assert (Sym /= Null_Symbol, "Symbol '" & Name & "' not found");

      Ada.Text_IO.Put_Line ("Flags: " & Symbol_Flags'Image (Bfd.Symtab.Get_Flags (Sym)));
      Ada.Text_IO.Put_Line ("Value: " & Symbol_Value'Image (Bfd.Symtab.Get_Value (Sym)));
      if Flag /= 0 then
         T.Assert ((Bfd.Symtab.Get_Flags (Sym) and Flag) /= 0,
                   "Symbol flag " & Symbol_Flags'Image (Flag) & " not set on " & Name);
      end if;

      declare
         Sec   : constant Bfd.Sections.Section := Bfd.Symtab.Get_Section (Sym);
      begin
         if Undefined then
            T.Assert (Bfd.Sections.Is_Undefined_Section (Sec),
                      "Symbol " & Name & " not in undefined section");
         else
            T.Assert (not Bfd.Sections.Is_Undefined_Section (Sec),
                      "Symbol " & Name & " is in undefined section");
         end if;
      end;
   end Test_Symbol;

   --  --------------------
   --  Test the getting a symbol by name
   --  --------------------
   procedure Test_Get_Symbol (T : in out Test_Case) is
      Symbols : Bfd.Symtab.Symbol_Table;
      Sym     : Bfd.Symtab.Symbol;
   begin
      T.Assert (Bfd.Files.Check_Format (T.File.all, Bfd.Files.OBJECT),
                "Bfd.Check_Format returned false");

      --  We must load the symbol table first.
      Bfd.Symtab.Open_Symbols (T.File.all, Symbols);

      Sym := Bfd.Symtab.Get_Symbol (Symbols, "bfd__tests__name");
      T.Assert (Sym /= Null_Symbol, "Symbol 'bfd__tests__name' not found");

      Ada.Text_IO.Put_Line ("Flags: " & Symbol_Flags'Image (Bfd.Symtab.Get_Flags (Sym)));
      Ada.Text_IO.Put_Line ("Value: " & Symbol_Value'Image (Bfd.Symtab.Get_Value (Sym)));
      T.Assert ((Bfd.Symtab.Get_Flags (Sym) and Bfd.Symtab.BSF_GLOBAL) /= 0,
                "Symbol must be global");

      Sym := Bfd.Symtab.Get_Symbol (Symbols, "this_symbol_does_not_exist");
      T.Assert (Sym = Null_Symbol, "A symbol was found while a null was expected");
   end Test_Get_Symbol;

   --  --------------------
   --  Test a global symbol
   --  --------------------
   procedure Test_Global_Symbol (T : in out Test_Case) is
   begin
      Test_Symbol (T, "bfd__tests__name", Bfd.Symtab.BSF_GLOBAL, False);
   end Test_Global_Symbol;

   --  --------------------
   --  Test an external/undefined symbol
   --  --------------------
   procedure Test_External_Symbol (T : in out Test_Case) is
   begin
      Test_Symbol (T, "bfd__close", Bfd.Symtab.BSF_NO_FLAGS, True);
   end Test_External_Symbol;

   --  --------------------
   --  Test an external/undefined symbol
   --  --------------------
   procedure Test_Section_Symbol (T : in out Test_Case) is
   begin
      Test_Symbol (T, ".text", Bfd.Symtab.BSF_SECTION_SYM, False);
   end Test_Section_Symbol;

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
         T.File := new Bfd.Files.File_Type;
         Suite.Add_Test (T.all'Access);
      end Add_Test;
   begin
      Add_Test ("Test Bfd.Symtab.Open_Symbols",
                "obj/bfd-tests.o", Test_Open_Symbols'Access);
      Add_Test ("Test Bfd.Symtab.Get_Iterator",
                "obj/bfd-tests.o", Test_Symbol_Iterator'Access);
      Add_Test ("Test Bfd.Symtab.Get_Symbol",
                "obj/bfd-tests.o", Test_Get_Symbol'Access);
      Add_Test ("Test Bfd.Symtab.Get_Symbol (global)",
                "obj/bfd-tests.o", Test_Global_Symbol'Access);
      Add_Test ("Test Bfd.Symtab.Get_Symbol (external)",
                "obj/bfd-tests.o", Test_External_Symbol'Access);
      Add_Test ("Test Bfd.Symtab.Get_Symbol (section symbol)",
                "obj/bfd-tests.o", Test_Section_Symbol'Access);
   end Add_Tests;

end Bfd.Symtab.Tests;
