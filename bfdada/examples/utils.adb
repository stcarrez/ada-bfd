-----------------------------------------------------------------------
--  print -- Print Utilities for examples
--  Copyright (C) 2002, 2003, 2004 Free Software Foundation, Inc.
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
--  the Free Software Foundation, 59 Temple Place - Suite 330,
--  Boston, MA 02111-1307, USA.
-----------------------------------------------------------------------
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;    use Ada.Command_Line;
with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.IO_Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Interfaces; use Interfaces;
with Ada.Text_IO; use Ada.Text_IO;
with Bfd; use Bfd;

package body Utils is

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

end Utils;
