-----------------------------------------------------------------------
--  disassemble -- Simple Disassembler
--  Copyright (C) 2006 Free Software Foundation, Inc.
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
--  the Free Software Foundation, 51 Franklin Street - Fifth Floor,
--  Boston, MA 02110-1301, USA.  -->
-----------------------------------------------------------------------
with Ada.Command_Line;    use Ada.Command_Line;
with Ada.IO_Exceptions;
with Ada.Streams;         use Ada.Streams;
with GNAT.Command_Line;

with Interfaces;      use Interfaces;
with Bfd;             use Bfd;
with Bfd.Sections;    use Bfd.Sections;
with Bfd.Symtab;      use Bfd.Symtab;
with Bfd.Disassembler; use Bfd.Disassembler;
with Ada.Text_IO;     use Ada.Text_IO;

with Utils; use Utils;
procedure Disassemble is

   Opt_H : Boolean := False;
   Opt_V : Boolean := False;

   RC : Ada.Command_Line.Exit_Status := 0;

   procedure Usage;
   procedure Disassemble_Section (File : Bfd.File_Type);
   procedure Parse_Arguments;

   --------------------------------------------------
   --  Usage
   --------------------------------------------------
   procedure Usage is
      use Ada.Text_IO;
      use Ada.Command_Line;
   begin
      New_Line;
      Put ("Usage: ");
      Put (Command_Name);
      Put_Line (" [-h] file ...");
      Put_Line ("Lists the sections contained in a binary file");
      Put_Line ("where:");
      Put_Line ("   -h           Verbose");
      New_Line;
      RC := 2;
   end Usage;

   --------------------------------------------------
   --  List the sections of the BFD file
   --------------------------------------------------
   procedure Disassemble_Section (File : Bfd.File_Type) is
      Text_Section : Section := Find_Section (File, ".text");
      Addr         : Vma_Type := Text_Section.Vma;
      Size         : Stream_Element_Offset
        := Stream_Element_Offset (Text_Section.Size);
      Section      : Ada.Streams.Stream_Element_Array (1 .. Size);
      Last         : Ada.Streams.Stream_Element_Offset;
      Info         : Small_Disassembler;
   begin
      Get_Section_Contents (File, Text_Section, 0, Section, Last);
      Bfd.Disassembler.Initialize (Info, File, "", Text_Section.Vma, Section);
      loop
         Bfd.Disassembler.Disassemble (Memory_Disassembler_Info_Type'Class (Info),
                                       Addr, Addr);
         New_Line;
         exit when Addr >= Text_Section.Vma + Vma_Type (Size);
      end loop;
   end Disassemble_Section;

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
            Optch := Getopt ("h ");

            case Optch is
               when Standard.Ascii.NUL =>
                  exit;
               when 'h' =>
                  Opt_H := True;
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
      if Opt_H then
         Usage;
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
               Disassemble_Section (File);
            end if;
            Close (File);

         exception
            when OPEN_ERROR =>
               Put_Line (Standard_Error, "Cannot open file '" & Arg
                         & "': " & Get_Error_Message (Get_Error));
         end;
      end loop;
   end Parse_Arguments;

begin
   Set_Error_Program_Name (To => "sections");

   Parse_Arguments;
   if RC /= 0 then
      Set_Exit_Status (RC);
      return;
   end if;

end Disassemble;
