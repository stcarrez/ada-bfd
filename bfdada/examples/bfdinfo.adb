-----------------------------------------------------------------------
-- bfdinfo -- Example for Bfd Ada library
-- Copyright (C) 2002, 2003 Free Software Foundation, Inc.
-- Written by Stephane Carrez (stcarrez@nerim.fr)
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
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;    use Ada.Command_Line;
with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.IO_Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Command_Line;

with Interfaces; use Interfaces;
with Bfd; use Bfd;
with Bfd.Sections; use Bfd.Sections;
with Bfd.Symtab; use Bfd.Symtab;
with Ada.Text_IO; use Ada.Text_IO;

procedure BfdInfo is


   Release   : constant String := "bfdinfo v1.0";
   Copyright : constant String := "Copyright 2002, 2003, Stephane Carrez";

   Opt_H : Boolean := False;
   Opt_V : Boolean := False;

   RC : Ada.Command_Line.Exit_Status := 0;

   procedure Usage;
   procedure Print (Item : in String; Max_Len : in Integer);
   function HexImage (Addr : in Bfd.Vma_Type) return String;
   procedure List_Section (File : Bfd.File_Type);
   procedure List_Symbols (File : Bfd.File_Type);
   procedure Parse_Arguments;

   --------------------------------------------------
   --  Usage
   --------------------------------------------------
   procedure Usage is
      use Ada.Text_IO;
      use Ada.Command_Line;
   begin
      Put_Line (Release);
      New_Line;
      Put ("Usage: ");
      Put (Command_Name);
      Put_Line (" [-v] dir1 dir2");
      Put_Line ("where:");
      Put_Line ("   -v           Verbose");
      New_Line;
      RC := 2;
   end Usage;

   --------------------------------------------------
   --  Usage
   --------------------------------------------------
   procedure Print (Item : in String; Max_Len : in Integer) is
   begin
      if Max_Len > 0 then
         if Item'Length > Max_Len then
            Put (Item (Item'First .. Item'First + Max_Len));
         else
            Put (Item);
            for I in Item'Length .. Max_Len loop
               Put (' ');
            end loop;
         end if;
      else
         if Item'Length > -Max_Len then
            Put (Item (Item'Last + Max_Len .. Item'Last));
         else
            for I in Item'Length .. -Max_Len loop
               Put (' ');
            end loop;
            Put (Item);
         end if;
      end if;
   end Print;

   --------------------------------------------------
   --  Convert an address to a string in hexadecimal form
   --------------------------------------------------
   function HexImage (Addr : in Bfd.Vma_Type) return String is
      Map : constant String := "0123456789ABCDEF";
      S   : String (1 .. 40);
      Val : Bfd.Vma_Type := Addr;
      C   : Natural;
      Pos : Positive := S'Last;
   begin
      loop
         C := Natural (Val mod 16);
         Val := Val / 16;
         S (Pos) := Map (C + 1);
         exit when Val = 0;
         Pos := Pos - 1;
      end loop;
      return S (Pos .. S'Last);
   end HexImage;

   --------------------------------------------------
   --  List the sections of the BFD file
   --------------------------------------------------
   procedure List_Section (File : Bfd.File_Type) is
      Iter : Section_Iterator := Get_Sections (File);
   begin
      Print ("Name", 30);
      Print ("Size", -10);
      Print ("VMA", -17);
      Print ("LMA", -17);
      Print ("Flags", -10);
      New_Line;
      while not Is_Done (Iter) loop
         declare
            S : Section := Current (Iter);
            P : String (1 .. 6) := (others => ' ');
            Pos : Positive := 1;
         begin
            Print (Get_Name (S), 30);
            Print (Size_Type'Image (S.Size), -10);
            Print (HexImage (S.Vma), -17);
            Print (HexImage (S.Lma), -17);

            if (S.Flags and SEC_ALLOC) /= 0 then
               P (Pos) := 'A';
               Pos := Pos + 1;
            end if;

            if (S.Flags and SEC_LOAD) /= 0 then
               P (Pos) := 'L';
               Pos := Pos + 1;
            end if;

            if (S.Flags and SEC_READONLY) /= 0 then
               P (Pos) := 'R';
               Pos := Pos + 1;
            end if;

            if (S.Flags and SEC_DATA) /= 0 then
               P (Pos) := 'W';
               Pos := Pos + 1;
            end if;

            if (S.Flags and SEC_CODE) /= 0 then
               P (Pos) := 'X';
               Pos := Pos + 1;
            end if;

            Print (P, -10);
            New_Line;
         end;
         Next (Iter);
      end loop;
   end List_Section;

   --------------------------------------------------
   --  List the symbols of the BFD file
   --------------------------------------------------
   procedure List_Symbols (File : Bfd.File_Type) is
      Symbols : Symbol_Table;
      It      : Symbol_Iterator;
      Count   : Natural;
   begin
      Open_Symbols (File, Symbols);
      It := Get_Iterator (Symbols);
      while not Is_Done (It) loop
         declare
            Sym   : Symbol       := Current (It);
            Sec   : Section      := Get_Section (Sym);
            Flags : Symbol_Flags := Get_Flags (Sym);
            C     : Character    := Get_Symclass (Sym);
         begin
            if (Flags and BSF_OBJECT) /= 0 then
               C := 'O';
               put ("          ");
            elsif Is_Undefined_Section (Sec) then
               put ("          ");
            else
               Print (HexImage (Get_Value (Sym)), 9);
               if (Flags and BSF_GLOBAL) /= 0 then
                  if C >= 'a' then
                     C := Character'Val (Character'Pos (C) + 32);
                  end if;
               end if;
            end if;
            Put (" " & C & " ");
            Put_Line (Get_Name (Sym));
         end;
         Next (It);
      end loop;
      Close_Symbols (Symbols);
   end List_Symbols;

   --------------------------------------------------
   --  Parse_Arguments
   --------------------------------------------------
   procedure Parse_Arguments is
      use Ada.Text_IO;
      use Ada.Command_Line;
      use GNAT.Command_Line;

      Optch :  Character;
   begin
      ------------------------------
      --  Process command line options.
      ------------------------------
      Initialize_Option_Scan (Stop_At_First_Non_Switch => True);

      begin
         loop
            Optch := Getopt ("h v ");

            case Optch is
               when Standard.Ascii.NUL =>
                  exit;
               when 'h' =>
                  Opt_H := True;
               when 'v' =>
                  Opt_V := True;
               when others =>
                  raise Program_Error;
            end case;
         end loop;
      exception
         when Invalid_Switch =>
            RC := 1;
            Put_Line (Standard_Error, "Invalid option: -" & Full_Switch);
         when Invalid_Parameter =>
            RC := 1;
            Put_Line (Standard_Error, "Missing argument: -" & Full_Switch);
      end;

      ------------------------------
      --  If -v, then show program release
      ------------------------------
      if Opt_V then
         Put_Line (Release);
         Put_Line (Copyright);
      end if;

      --  Open each file passed as argument and try dumping its
      --  sections and symbol table.
      loop
         declare
            Arg  : String := Get_Argument;
            File : Bfd.File_Type;
         begin
            exit when Arg = "";

            Open (File, Arg, "");
            if Check_Format (File, OBJECT) then
               List_Section (File);
               List_Symbols (File);
            end if;
            Close (File);

         exception
            when OPEN_ERROR =>
               Put_Line (Standard_Error, "Cannot open file " & Arg);
               Put_Line (Standard_Error, Get_Error_Message (Get_Error));
         end;
      end loop;
   end Parse_Arguments;

begin
   Set_Error_Program_Name (To => "bfdinfo");

   Parse_Arguments;
   if RC /= 0 then
      Set_Exit_Status (RC);
      return;
   end if;

end BfdInfo;
