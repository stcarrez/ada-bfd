-----------------------------------------------------------------------
--  BFD Tests -- Tests for BFD section Ada API
--  Copyright (C) 2002, 2003, 2012, 2015, 2021 Free Software Foundation, Inc.
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

with Bfd.Files;
with Bfd.Sections;
with Bfd.Constants;
package body Bfd.Symbols.Tests is

   --  Test the getting a symbol by name
   procedure Test_Symbol (T         : in out Test_Case;
                          Name      : in String;
                          Flag      : in Bfd.Symbol_Flags;
                          Undefined : in Boolean);

   --  --------------------
   --  Test loading the symbol table
   --  --------------------
   procedure Test_Open_Symbols (T : in out Test_Case) is
      Symbols : Bfd.Symbols.Symbol_Table;
      pragma Unreferenced (Symbols);
   begin
      T.Assert (Bfd.Files.Check_Format (T.File.all, Bfd.Files.OBJECT),
                "Bfd.Check_Format returned false");

      --  We must load the symbol table first.
      Bfd.Symbols.Read_Symbols (T.File.all, Symbols);

      --  Can't check in a portable way, assume some reasonable value.
      T.Assert (Bfd.Files.Get_Symbol_Count (T.File.all) > 0
                and Bfd.Files.Get_Symbol_Count (T.File.all) < 10000,
                "Bfd.Get_Symbol_Count returned 0");

   end Test_Open_Symbols;


   --  --------------------
   --  Test the symbol iterator
   --  --------------------
   procedure Test_Symbol_Iterator (T : in out Test_Case) is
      Symbols : Bfd.Symbols.Symbol_Table;
      Iter    : Symbol_Iterator;
      Count   : Natural;
   begin
      T.Assert (Bfd.Files.Check_Format (T.File.all, Bfd.Files.OBJECT),
                "Bfd.Check_Format returned false");

      --  We must load the symbol table first.
      Bfd.Symbols.Read_Symbols (T.File.all, Symbols);

      Iter := Bfd.Symbols.Get_Iterator (Symbols);
      T.Assert (Bfd.Symbols.Has_Element (Iter),
                "The symbol table seems empty.");

      --  Can't check in a portable way, assume some reasonable value.
      T.Assert (Bfd.Files.Get_Symbol_Count (T.File.all) > 0
                and Bfd.Files.Get_Symbol_Count (T.File.all) < 10000,
                "Bfd.Get_Symbol_Count returned 0");

      Count := 0;
      while Bfd.Symbols.Has_Element (Iter) loop
         declare
            Sym  : constant Symbol := Bfd.Symbols.Element (Iter);
            Name : constant String := Bfd.Symbols.Get_Name (Sym);
         begin
            T.Assert (Name'Length > 0, "Bfd.Symbols.Get_Name returns an empty name");
            if Util.Tests.Verbose then
               Ada.Text_IO.Put_Line (Name);
            end if;
            Bfd.Symbols.Next (Iter);
         end;
         Count := Count + 1;
      end loop;

      Ada.Text_IO.Put_Line ("Iterate count: " & Natural'Image (Count));
      Ada.Text_IO.Put_Line ("Symbol count: "
                            & Natural'Image (Bfd.Files.Get_Symbol_Count (T.File.all)));

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
      Symbols : Bfd.Symbols.Symbol_Table;
      Sym     : Bfd.Symbols.Symbol;
   begin
      T.Assert (Bfd.Files.Check_Format (T.File.all, Bfd.Files.OBJECT),
                "Bfd.Check_Format returned false");

      --  We must load the symbol table first.
      Bfd.Symbols.Read_Symbols (T.File.all, Symbols);

      Sym := Bfd.Symbols.Get_Symbol (Symbols, Name);
      T.Assert (Sym /= Null_Symbol, "Symbol '" & Name & "' not found");

      Ada.Text_IO.Put_Line ("Flags: " & Symbol_Flags'Image (Bfd.Symbols.Get_Flags (Sym)));
      Ada.Text_IO.Put_Line ("Value: " & Symbol_Value'Image (Bfd.Symbols.Get_Value (Sym)));
      if Flag /= 0 then
         T.Assert ((Bfd.Symbols.Get_Flags (Sym) and Flag) /= 0,
                   "Symbol flag " & Symbol_Flags'Image (Flag) & " not set on " & Name);
      end if;

      if Flag = Bfd.Symbols.BSF_LOCAL then
         T.Assert (Bfd.Symbols.Is_Local_Label (T.File.all, Sym));
      end if;

      if Flag = Bfd.Symbols.BSF_GLOBAL then
         T.Assert (not Bfd.Symbols.Is_Local_Label (T.File.all, Sym));
      end if;

      declare
         Sec   : constant Bfd.Sections.Section := Bfd.Symbols.Get_Section (Sym);
      begin
         if Undefined then
            T.Assert (Bfd.Sections.Is_Undefined_Section (Sec),
                      "Symbol " & Name & " not in undefined section");
         else
            T.Assert (not Bfd.Sections.Is_Undefined_Section (Sec),
                      "Symbol " & Name & " is in undefined section");
         end if;

         if Flag = Bfd.Symbols.BSF_OBJECT then
            T.Assert (Bfd.Sections.Is_Common_Section (Sec),
                      "Symbol " & Name & " is not common section");
         else
            T.Assert (not Bfd.Sections.Is_Common_Section (Sec),
                      "Symbol " & Name & " is not common section");
         end if;
      end;
   end Test_Symbol;

   --  --------------------
   --  Test the getting a symbol by name
   --  --------------------
   procedure Test_Get_Symbol (T : in out Test_Case) is
      Symbols : Bfd.Symbols.Symbol_Table;
      Sym     : Bfd.Symbols.Symbol;
   begin
      T.Assert (Bfd.Files.Check_Format (T.File.all, Bfd.Files.OBJECT),
                "Bfd.Check_Format returned false");

      --  We must load the symbol table first.
      Bfd.Symbols.Read_Symbols (T.File.all, Symbols);

      Sym := Bfd.Symbols.Get_Symbol (Symbols, "bfd__tests__name");
      T.Assert (Sym /= Null_Symbol, "Symbol 'bfd__tests__name' not found");

      Ada.Text_IO.Put_Line ("Flags: " & Symbol_Flags'Image (Bfd.Symbols.Get_Flags (Sym)));
      Ada.Text_IO.Put_Line ("Value: " & Symbol_Value'Image (Bfd.Symbols.Get_Value (Sym)));
      Ada.Text_IO.Put_Line ("Size:  " & Symbol_Value'Image (Bfd.Symbols.Get_Symbol_Size (Sym)));
      T.Assert ((Bfd.Symbols.Get_Flags (Sym) and Bfd.Symbols.BSF_GLOBAL) /= 0,
                "Symbol must be global");
      T.Assert (Bfd.Symbols.Get_Value (Sym) /= 0, "Symbol must be have a size > 0");
      T.Assert (Bfd.Symbols.Get_Symbol_Size (Sym) /= 0, "Symbol must be have a size > 0");

      Sym := Bfd.Symbols.Get_Symbol (Symbols, "this_symbol_does_not_exist");
      T.Assert (Sym = Null_Symbol, "A symbol was found while a null was expected");
   end Test_Get_Symbol;

   --  --------------------
   --  Test a global symbol
   --  --------------------
   procedure Test_Global_Symbol (T : in out Test_Case) is
   begin
      Test_Symbol (T, "bfd__tests__name", Bfd.Symbols.BSF_GLOBAL, False);
   end Test_Global_Symbol;

   --  --------------------
   --  Test a local symbol
   --  --------------------
   procedure Test_Local_Symbol (T : in out Test_Case) is
   begin
      Test_Symbol (T, ".LFB2", Bfd.Symbols.BSF_LOCAL, False);
   end Test_Local_Symbol;

   --  --------------------
   --  Test an external/undefined symbol
   --  --------------------
   procedure Test_External_Symbol (T : in out Test_Case) is
   begin
      Test_Symbol (T, "bfd__files__close", Bfd.Symbols.BSF_NO_FLAGS, True);
   end Test_External_Symbol;

   --  --------------------
   --  Test an unknown symbol
   --  --------------------
   procedure Test_Unknown_Symbol (T : in out Test_Case) is
      Symbols : Bfd.Symbols.Symbol_Table;
      Sym     : Bfd.Symbols.Symbol;
   begin
      T.Assert (Bfd.Files.Check_Format (T.File.all, Bfd.Files.OBJECT),
                "Bfd.Check_Format returned false");

      --  We must load the symbol table first.
      Bfd.Symbols.Read_Symbols (T.File.all, Symbols);
      Sym := Bfd.Symbols.Get_Symbol (Symbols, "bfd__some_missing_symbol");
      T.Assert (Sym = Null_Symbol, "Get_Symbol return invalid value");
   end Test_Unknown_Symbol;

   --  --------------------
   --  Test an common symbol
   --  --------------------
   procedure Test_Common_Symbol (T : in out Test_Case) is
   begin
      Test_Symbol (T, "common_sect", Bfd.Symbols.BSF_OBJECT, False);
   end Test_Common_Symbol;

   --  --------------------
   --  Test an external/undefined symbol
   --  --------------------
   procedure Test_Section_Symbol (T : in out Test_Case) is
   begin
      Test_Symbol (T, ".text", Bfd.Symbols.BSF_SECTION_SYM, False);
   end Test_Section_Symbol;

   --  --------------------
   --  Test the demangle of symbol.
   --  --------------------
   procedure Test_Demangle_Symbol (T : in out Test_Case) is

      procedure Check (Name, Expect : in String);

      procedure Check (Name, Expect : in String) is
         Value : constant String := Bfd.Symbols.Demangle (T.File.all, Name, Constants.DMGL_GNAT);
      begin
         Ada.Text_IO.Put_Line ("Demangle " & Name & "=" & Value);
         T.Assert (Value = Expect, "Bfd.Symbols.Demangle " & Name);
      end Check;

   begin
      Check ("bfd__symbols__get_name", "bfd.symbols.get_name");
      Check ("ada__calendar__conversion_operations__to_ada_time",
             "ada.calendar.conversion_operations.to_ada_time");
      Check ("ada__command_line_E", "<ada__command_line_E>");
      Check ("util__streams___elabs", "util.streams'Elab_Spec");
   end Test_Demangle_Symbol;

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
         Suite.Add_Test (T.all'Access);
      end Add_Test;
   begin
      Add_Test ("Test Bfd.Symbols.Open_Symbols",
                "obj/bfd-tests.o", Test_Open_Symbols'Access);
      Add_Test ("Test Bfd.Symbols.Get_Iterator",
                "obj/bfd-tests.o", Test_Symbol_Iterator'Access);
      Add_Test ("Test Bfd.Symbols.Get_Symbol",
                "obj/bfd-tests.o", Test_Get_Symbol'Access);
      Add_Test ("Test Bfd.Symbols.Get_Symbol (global)",
                "obj/bfd-tests.o", Test_Global_Symbol'Access);
      Add_Test ("Test Bfd.Symbols.Get_Symbol (external)",
                "obj/bfd-tests.o", Test_External_Symbol'Access);
      Add_Test ("Test Bfd.Symbols.Get_Symbol (section symbol)",
                "obj/bfd-tests.o", Test_Section_Symbol'Access);
      Add_Test ("Test Bfd.Symbols.Get_Symbol (local)",
                "regtests/files/test.o", Test_Local_Symbol'Access);
      Add_Test ("Test Bfd.Symbols.Get_Symbol (common)",
                "regtests/files/test_common.o", Test_Common_Symbol'Access);
      Add_Test ("Test Bfd.Symbols.Get_Symbol (unkown)",
                "obj/bfd-tests.o", Test_Unknown_Symbol'Access);
      Add_Test ("Test Bfd.Symbols.Demangle (symbol)",
                "obj/bfd-tests.o", Test_Demangle_Symbol'Access);
   end Add_Tests;

end Bfd.Symbols.Tests;
