-----------------------------------------------------------------------
-- BFD Tests -- Tests for BFD section Ada API
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
with AUnit.Test_Suites;
with Bfd; use Bfd;
with Bfd.Tests; use Bfd.Tests;
package Bfd.Tests.Sections is

   type Test_Case is new Bfd.Tests.Test_Case with record
      A : Natural;
   end record;

   --  Override:

   --  Register routines to be run:
   procedure Register_Tests (T : in out Test_Case);

   procedure Add_Tests (Suite : AUnit.Test_Suites.Access_Test_Suite);

end Bfd.Tests.Sections;
