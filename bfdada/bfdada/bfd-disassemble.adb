-----------------------------------------------------------------------
--  Disassemble -- Disassembler
--  Copyright (C) 2003 Free Software Foundation, Inc.
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
--  The Bfd package exports the GNU Bfd library found in Binutils
--  and Gdb.  It is not intended to be as complete as the C library
--  but still provide enough methods to read any object or binary,
--  observe its sections, its symbol table.
--
with System;
with Ada.Streams; use Ada.Streams;
package body Bfd.Disassemble is

   EIO : constant Integer := -3;

   type Disassembler_Ref is access all Disassembler'Class;

   ----------------------
   --  Handlers called by C callbacks
   ----------------------
   procedure Memory_Handler (Status : in Integer;
                             Addr   : in Vma_Type;
                             Info   : in Disassembler_Ref);
   pragma Export (C, Memory_Handler, "ada_dis_memory_handler");

   function Symbol_At_Handler (Addr : in Vma_Type;
                               Info : in Disassembler_Ref) return Boolean;
   pragma Export (C, Symbol_At_Handler, "ada_dis_symbol_at_handler");

   function Read_Memory_Handler (Addr        : in Vma_Type;
                                 Buffer_Addr : in System.Address;
                                 Len         : in Integer;
                                 Info : in Disassembler_Ref) return Integer;
   pragma Export (C, Read_Memory_Handler, "ada_dis_read_memory_handler");

   ----------------------
   --  Memory error callback
   ----------------------
   procedure Memory_Handler (Status : in Integer;
                             Addr   : in Vma_Type;
                             Info   : in Disassembler_Ref) is
   begin
      Memory_Error (Info.all, Addr);
   end Memory_Handler;

   ----------------------
   --  Symbol check callback
   ----------------------
   function Symbol_At_Handler (Addr : in Vma_Type;
                               Info : in Disassembler_Ref) return Boolean is
   begin
      return Symbol_At (Info.all, Addr);
   end Symbol_At_Handler;

   ----------------------
   --  Read memory callback
   ----------------------
   function Read_Memory_Handler (Addr        : in Vma_Type;
                                 Buffer_Addr : in System.Address;
                                 Len         : in Integer;
                                 Info : in Disassembler_Ref) return Integer is

      --  Buffer_Addr points to some local buffer allocated by the
      --  opcodes C library (on the stack).  Map that buffer to the
      --  Ada type (no copy).
      Buf : Stream_Element_Array (1 .. Stream_Element_Offset (Len));
      for Buf'Address use Buffer_Addr;

      Last : Stream_Element_Offset;
   begin
      Read (Info.all, Addr, Buf, Last);
      if Last = Buf'Last then
         return 0;
      else
         return EIO;
      end if;
   end Read_Memory_Handler;

   ----------------------
   --  Disassembler methods
   ----------------------

   --  Print the address.
   --  The default just translated the address in hexadecimal and
   --  prints it with Output procedure.  It can be overriden by a
   --  derived type to print a symbolic address.
   procedure Output (Dis  : in out Disassembler;
                     Addr : in Vma_Type) is
   begin
      null;
   end Output;

   --  Returns true if there is a symbol at the given address.
   --  The default always returns true.
   function Symbol_At (Dis  : in Disassembler;
                       Addr : in Vma_Type) return Boolean is
   begin
      return True;
   end Symbol_At;

   --  Initialize the disassembler according to the BFD file.
   procedure Initialize (Dis      : in out Disassembler'Class;
                         For_File : in File_Type) is
   begin
      null;
   end Initialize;

   --  Set the symbol table associated with the disassembler.
   procedure Set_Symbol_Table (Dis    : in out Disassembler'Class;
                               Symtab : in Symbol_Table) is
   begin
      null;
   end Set_Symbol_Table;


   --  Disassemble one instruction at address Addr.
   --  Use the Read procedure to read the memory to disassemble.
   procedure Disassemble (Dis  : in out Disassembler'Class;
                          Addr : in Vma_Type) is
   begin
      null;
   end Disassemble;

   ----------------------
   --  Memory Disassembler type
   ----------------------
   --  Disassemble one instruction at address Addr in the buffer area
   --  described by Buffer.  The buffer's starting VMA address is specified
   --  by Buffer_Vma.
   procedure Disassemble (Dis        : in out Memory_Disassembler'Class;
                          Addr       : in Vma_Type;
                          Buffer_Vma : in Vma_Type;
                          Buffer     : in Ada.Streams.Stream_Element_Array) is
   begin
      Disassemble (Dis, Addr);
   end Disassemble;

   --  Reads Item'Size bytes starting at the given address.
   --  This procedure uses the buffer passed to Disassemble to obtain
   --  the memory.
   procedure Read (Dis  : in out Memory_Disassembler;
                   Addr : in Vma_Type;
                   Item : out Ada.Streams.Stream_Element_Array;
                   Last : out Ada.Streams.Stream_Element_Offset) is
   begin
      raise Program_Error;
   end Read;

   --  Report an error while reading memory.
   --  This is called when the Read procedure returns an error.
   procedure Memory_Error (Dis  : in out Memory_Disassembler;
                           Addr : in Vma_Type) is
   begin
      null;
   end Memory_Error;

end Bfd.Disassemble;
