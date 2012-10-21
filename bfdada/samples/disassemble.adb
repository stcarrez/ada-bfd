-----------------------------------------------------------------------
--  disassemble -- Simple Disassembler
--  Copyright (C) 2006, 2012 Free Software Foundation, Inc.
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
--  the Free Software Foundation, 51 Franklin Street - Fifth Floor,
--  Boston, MA 02110-1301, USA.  -->
-----------------------------------------------------------------------
with Ada.Command_Line;
with Ada.IO_Exceptions;
with Ada.Streams;
with GNAT.Command_Line;

with Interfaces;
with Bfd;
with Bfd.Sections;
with Bfd.Symtab;
with Bfd.Disassembler;
with Ada.Text_IO;

with Utils;
procedure Disassemble is

   use Ada.Text_IO;

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
      use type Bfd.Vma_Type;

      Text_Section : Bfd.Sections.Section := Bfd.Sections.Find_Section (File, ".text");
      Addr         : Bfd.Vma_Type := Text_Section.Vma;
      Size         : Ada.Streams.Stream_Element_Offset
        := Ada.Streams.Stream_Element_Offset (Text_Section.Size);
      Section      : Ada.Streams.Stream_Element_Array (1 .. Size);
      Last         : Ada.Streams.Stream_Element_Offset;
      Info         : Utils.Small_Disassembler;
   begin
      Bfd.Sections.Get_Section_Contents (File, Text_Section, 0, Section, Last);
      Bfd.Disassembler.Initialize (Info, File, "", Text_Section.Vma, Section);
      loop
         Bfd.Disassembler.Disassemble (Bfd.Disassembler.Memory_Disassembler_Info_Type'Class (Info),
                                       Addr, Addr);
         New_Line;
         exit when Addr >= Text_Section.Vma + Bfd.Vma_Type (Size);
      end loop;
   end Disassemble_Section;

   --------------------------------------------------
   --  Parse_Arguments
   --------------------------------------------------
   procedure Parse_Arguments is
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

            Bfd.Open (File, Arg, "");
            if Bfd.Check_Format (File, Bfd.OBJECT) then
               Disassemble_Section (File);
            end if;
            Bfd.Close (File);

         exception
            when Bfd.OPEN_ERROR =>
               Put_Line (Standard_Error, "Cannot open file '" & Arg
                         & "': " & Bfd.Get_Error_Message (Bfd.Get_Error));
         end;
      end loop;
   end Parse_Arguments;

   use type Ada.Command_Line.Exit_Status;
begin
   Bfd.Set_Error_Program_Name (To => "disassemble");

   Parse_Arguments;
   if RC /= 0 then
      Ada.Command_Line.Set_Exit_Status (RC);
      return;
   end if;

end Disassemble;
