-----------------------------------------------------------------------
--  BFD Tests -- Tests for Binary File Descriptor Library (Ada Interface)
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
with AUnit.Tests;
with AUnit.Test_Cases;
with AUnit.Test_Suites;

with Bfd;
package Bfd.Tests is

   type Test_Case is new AUnit.Test_Cases.Test_Case with record
      Test_Name : Ada.Strings.Unbounded.Unbounded_String;
      File_Name : Ada.Strings.Unbounded.Unbounded_String;
      File      : File_Type;
   end record;

   type Test_Ref is access all AUnit.Tests.Test'Class;

   --  Override:

   --  Register routines to be run:
   procedure Register_Tests (T : in out Test_Case);

   --  Provide name identifying the test case:
   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access;

   --  Setup before each routine:
   procedure Set_Up (T : in out Test_Case);

   --  Cleanup performed after each routine:
   procedure Tear_Down (T :  in out Test_Case);

   function Get_Test_File (T : in Test_Case) return String;

   procedure Add_Tests (Suite : in AUnit.Test_Suites.Access_Test_Suite);

end Bfd.Tests;
