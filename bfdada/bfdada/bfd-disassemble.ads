-----------------------------------------------------------------------
--  Disassemble -- Disassembler
--  <!-- Copyright (C) 2003, 2004 Free Software Foundation, Inc.
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
--  Boston, MA 02111-1307, USA.  -->
-----------------------------------------------------------------------
--  The Bfd package exports the GNU Bfd library found in Binutils
--  and Gdb.  It is not intended to be as complete as the C library
--  but still provide enough methods to read any object or binary,
--  observe its sections, its symbol table.
--
with System;
with Ada.Streams;
with Bfd.Symtab; use Bfd.Symtab;
package Bfd.Disassemble is

   pragma Linker_Options ("-lopcodes");

   type Insn_Type is (NONINSN,          --  Not a valid instruction
                      NONBRANCH,        --  Not a branch instruction
                      BRANCH,           --  Unconditional branch
                      CONDBRANCH,       --  Conditional branch
                      JSR,              --  Jump to subroutine
                      CONDJSR,          --  Conditional jump to subroutine
                      DREF,             --  Data reference instruction
                      DREF2);           --  Two data references in instruction

   type Insn_Info_Type is record
      Insn        : Insn_Type;  --  Type of instruction
      Target      : Vma_Type;   --  Target address of branch/dref if known
      Target2     : Vma_Type;   --  Second target address for dref2
      Delay_Insns : Natural;    --  How many sequential insn's will run
                                --  before a branch takes effect.
      Data_Size   : Natural;    --  Size of data reference in insn (in bytes)
   end record;

   ----------------------
   --  Disassembler root type
   ----------------------
   type Disassembler is abstract tagged private;

   procedure Output (Dis  : in out Disassembler;
                     Item : in String) is abstract;
   --  Print the result of disassembling the current instruction.
   --  This procedure is called to print the instruction and its
   --  operands.

   procedure Output (Dis  : in out Disassembler;
                     Addr : in Vma_Type);
   --  Print the address.
   --  The default just translated the address in hexadecimal and
   --  prints it with Output procedure.  It can be overriden by a
   --  derived type to print a symbolic address.

   function Symbol_At (Dis  : in Disassembler;
                       Addr : in Vma_Type) return Boolean;
   --  Returns true if there is a symbol at the given address.
   --  The default always returns true.

   procedure Read (Dis  : in out Disassembler;
                   Addr : in Vma_Type;
                   Item : out Ada.Streams.Stream_Element_Array;
                   Last : out Ada.Streams.Stream_Element_Offset) is abstract;
   --  Reads Item'Size bytes starting at the given address.
   --  This procedure is called by Disassemble to obtain the memory
   --  to disassemble.  It is called several times depending on the assembler.

   procedure Memory_Error (Dis  : in out Disassembler;
                           Addr : in Vma_Type) is abstract;
   --  Report an error while reading memory.
   --  This is called when the Read procedure returns an error.

   procedure Initialize (Dis      : in out Disassembler'Class;
                         For_File : in File_Type);
   --  Initialize the disassembler according to the BFD file.

   procedure Set_Symbol_Table (Dis    : in out Disassembler'Class;
                               Symtab : in Symbol_Table);
   --  Set the symbol table associated with the disassembler.


   procedure Disassemble (Dis  : in out Disassembler'Class;
                          Addr : in Vma_Type);
   --  Disassemble one instruction at address Addr.
   --  Use the Read procedure to read the memory to disassemble.

   ----------------------
   --  Memory Disassembler type
   ----------------------
   type Memory_Disassembler is abstract tagged private;

   procedure Disassemble (Dis        : in out Memory_Disassembler'Class;
                          Addr       : in Vma_Type;
                          Buffer_Vma : in Vma_Type;
                          Buffer     : in Ada.Streams.Stream_Element_Array);
   --  Disassemble one instruction at address Addr in the buffer area
   --  described by Buffer.  The buffer's starting VMA address is specified
   --  by Buffer_Vma.

   procedure Memory_Error (Dis  : in out Memory_Disassembler;
                           Addr : in Vma_Type);
   --  Report an error while reading memory.
   --  This is called when the Read procedure returns an error.

   procedure Output (Dis  : in out Memory_Disassembler;
                     Item : in String) is abstract;
   --  Print the result of disassembling the current instruction.
   --  This procedure is called to print the instruction and its
   --  operands.

private

   procedure Read (Dis  : in out Memory_Disassembler;
                   Addr : in Vma_Type;
                   Item : out Ada.Streams.Stream_Element_Array;
                   Last : out Ada.Streams.Stream_Element_Offset);
   --  Reads Item'Size bytes starting at the given address.
   --  This procedure uses the buffer passed to Disassemble to obtain
   --  the memory.

   subtype D is System.Address;

   type Disassembler is abstract tagged record
      Dis : D;
   end record;

   type Memory_Disassembler is abstract new Disassembler with null record;

end Bfd.Disassemble;
