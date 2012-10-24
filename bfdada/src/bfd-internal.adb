-----------------------------------------------------------------------
--  BFD -- Binary File Descriptor Library (Ada Interface)
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
--  The Bfd package exports the GNU Bfd library found in Binutils
--  and Gdb.  It is not intended to be as complete as the C library
--  but still provide enough methods to read any object or binary,
--  observe its sections, its symbol table.
--

package body Bfd.Internal is

   function To_Ada (P : in Pointer) return String is
      N : Natural;
   begin
      if P = Null_Address then
         return "";
      end if;
      N := Strlen (P);
      if N > 1000 then
         raise Constraint_Error;
      end if;
      declare
         S : aliased String (1 .. N);
      begin
         Memcpy (S'Address, P, N);
         return S;
      end;
   end To_Ada;

   procedure To_Ada (P : in Pointer; S : out String) is
      N : constant Natural := Strlen (P);
   begin
      if N > S'Length then
         Memcpy (S'Address, P, S'Length);
      else
         Memcpy (S'Address, P, N);
      end if;
   end To_Ada;

end Bfd.Internal;
