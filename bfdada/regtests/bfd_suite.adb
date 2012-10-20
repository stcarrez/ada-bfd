-----------------------------------------------------------------------
-- BFD Tests -- Testsuite function creation
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
--  the Free Software Foundation,51 Franklin Street - Fifth Floor,
--  Boston, MA 02110-1301, USA.
-----------------------------------------------------------------------
with AUnit.Test_Suites; use AUnit.Test_Suites;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

-- Unit tests for AUnit:
with Bfd.Tests;
with Bfd.Tests.Sections;
function Bfd_Suite return Access_Test_Suite is
   Result : Access_Test_Suite := new Test_Suite;
begin
   Bfd.Tests.Add_Tests (Result);
   Bfd.Tests.Sections.Add_Tests (Result);
   return Result;
end Bfd_Suite;

