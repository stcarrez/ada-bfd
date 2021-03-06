-----------------------------------------------------------------------
--  bfdinfo -- Example for Bfd Ada library
--  Copyright (C) 2002, 2003, 2005, 2012, 2021 Free Software Foundation, Inc.
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
with Ada.Command_Line;

with Bfd;
with Bfd.Files;
with Bfd.Sections;
with Bfd.Symbols;
with Utils;

procedure BfdInfo is

   use Ada.Text_IO;

   RC : Ada.Command_Line.Exit_Status := 0;

   procedure List_Section (File : Bfd.Files.File_Type);
   procedure List_Symbols (File : Bfd.Files.File_Type);

   --------------------------------------------------
   --  List the sections of the BFD file
   --------------------------------------------------
   procedure List_Section (File : Bfd.Files.File_Type) is
      use type Bfd.Section_Flags;

      Iter : Bfd.Sections.Section_Iterator := Bfd.Sections.Get_Sections (File);
   begin
      Utils.Print ("Name", 30);
      Utils.Print ("Size", -10);
      Utils.Print ("VMA", -17);
      Utils.Print ("LMA", -17);
      Utils.Print ("Flags", -10);
      New_Line;
      while Bfd.Sections.Has_Element (Iter) loop
         declare
            S   : constant Bfd.Sections.Section := Bfd.Sections.Element (Iter);
            P   : String (1 .. 6) := (others => ' ');
            Pos : Positive := 1;
         begin
            Utils.Print (Bfd.Sections.Get_Name (S), 30);
            Utils.Print (Bfd.Size_Type'Image (S.Size), -10);
            Utils.Print (Utils.HexImage (S.Vma), -17);
            Utils.Print (Utils.HexImage (S.Lma), -17);

            if (S.Flags and Bfd.Sections.SEC_ALLOC) /= 0 then
               P (Pos) := 'A';
               Pos := Pos + 1;
            end if;

            if (S.Flags and Bfd.Sections.SEC_LOAD) /= 0 then
               P (Pos) := 'L';
               Pos := Pos + 1;
            end if;

            if (S.Flags and Bfd.Sections.SEC_READONLY) /= 0 then
               P (Pos) := 'R';
               Pos := Pos + 1;
            end if;

            if (S.Flags and Bfd.Sections.SEC_DATA) /= 0 then
               P (Pos) := 'W';
               Pos := Pos + 1;
            end if;

            if (S.Flags and Bfd.Sections.SEC_CODE) /= 0 then
               P (Pos) := 'X';
               Pos := Pos + 1;
            end if;

            Utils.Print (P, -10);
            New_Line;
         end;
         Bfd.Sections.Next (Iter);
      end loop;
   end List_Section;

   --------------------------------------------------
   --  List the symbols of the BFD file
   --------------------------------------------------
   procedure List_Symbols (File : Bfd.Files.File_Type) is
      use type Bfd.Symbol_Flags;

      Symbols : Bfd.Symbols.Symbol_Table;
      It      : Bfd.Symbols.Symbol_Iterator;
   begin
      Bfd.Symbols.Read_Symbols (File, Symbols);
      It := Bfd.Symbols.Get_Iterator (Symbols);
      while Bfd.Symbols.Has_Element (It) loop
         declare
            Sym   : constant Bfd.Symbols.Symbol       := Bfd.Symbols.Element (It);
            Sec   : constant Bfd.Sections.Section    := Bfd.Symbols.Get_Section (Sym);
            Flags : constant Bfd.Symbol_Flags        := Bfd.Symbols.Get_Flags (Sym);
            C     : Character    := Bfd.Symbols.Get_Symclass (Sym);
         begin
            if (Flags and Bfd.Symbols.BSF_OBJECT) /= 0 then
               C := 'O';
               Put ("          ");
            elsif Bfd.Sections.Is_Undefined_Section (Sec) then
               Put ("          ");
            else
               Utils.Print (Utils.HexImage (Bfd.Symbols.Get_Value (Sym)), 9);
               if (Flags and Bfd.Symbols.BSF_GLOBAL) /= 0 then
                  if C >= 'a' then
                     C := Character'Val (Character'Pos (C) + 32);
                  end if;
               end if;
            end if;
            Put (" " & C & " ");
            Put_Line (Bfd.Symbols.Get_Name (Sym));
         end;
         Bfd.Symbols.Next (It);
      end loop;
   end List_Symbols;

   use type Ada.Command_Line.Exit_Status;

   Count  : constant Natural := Ada.Command_Line.Argument_Count;
begin
   if Count < 2 then
      Ada.Text_IO.Put_Line ("Usage: bfdinfo obj-file ...");
      RC := 2;
   end if;

   Bfd.Set_Error_Program_Name (To => "bfdinfo");

   --  Open each file passed as argument and try dumping its
   --  sections and symbol table.
   for I in 1 .. Count loop
      declare
         Arg  : constant String := Ada.Command_Line.Argument (I);
         File : Bfd.Files.File_Type;
      begin
         exit when Arg = "";

         Bfd.Files.Open (File, Arg, "");
         if Bfd.Files.Check_Format (File, Bfd.Files.OBJECT) then
            List_Section (File);
            List_Symbols (File);
         end if;
         Bfd.Files.Close (File);

      exception
         when Bfd.OPEN_ERROR =>
            Put_Line (Standard_Error, "Cannot open file " & Arg);
            Put_Line (Standard_Error, Bfd.Get_Error_Message (Bfd.Get_Error));
            RC := 1;
      end;
   end loop;

   if RC /= 0 then
      Ada.Command_Line.Set_Exit_Status (RC);
      return;
   end if;

end BfdInfo;
