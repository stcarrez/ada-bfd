-----------------------------------------------------------------------
-- BFD -- Binary File Descriptor Library (Ada Interface)
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
--  This package gives access to the symbol table managed by the
--  BFD library.
--
with System;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Bfd.Sections;
package Bfd.Symtab is

   ----------------------
   -- Symbol_Flags     --
   ----------------------
   type Symbol_Flags is mod 2**32;
   for Symbol_Flags'Size use 32;
   --  Represents the flags associated with a symbol.

   --  Constants below are extracted from the BFD C source file
   --  CODE FRAGMENT:  bfd/syms.c:

   BSF_NO_FLAGS : constant Symbol_Flags := 16#00#;
   --  Attributes of a symbol.

   BSF_LOCAL : constant Symbol_Flags := 16#01#;
   --  The symbol has local scope; <<static>> in <<C>>. The value
   --  is the offset into the section of the data.

   BSF_GLOBAL : constant Symbol_Flags := 16#02#;
   --  The symbol has global scope; initialized data in <<C>>. The
   --  value is the offset into the section of the data.

   BSF_EXPORT : constant Symbol_Flags := BSF_GLOBAL;
   --  The symbol has global scope and is exported. The value is
   --  the offset into the section of the data.

   --  A normal C symbol would be one of:
   --  <<BSF_LOCAL>>, <<BSF_FORT_COMM>>,  <<BSF_UNDEFINED>> or
   --  <<BSF_GLOBAL>>.

   BSF_DEBUGGING : constant Symbol_Flags := 16#08#;
   --  The symbol is a debugging record. The value has an arbitary
   --  meaning, unless BSF_DEBUGGING_RELOC is also set.

   BSF_FUNCTION : constant Symbol_Flags := 16#10#;
   --  The symbol denotes a function entry point.  Used in ELF,
   --  perhaps others someday.

   BSF_KEEP : constant Symbol_Flags := 16#20#;
   BSF_KEEP_G : constant Symbol_Flags := 16#40#;
   --  Used by the linker.

   BSF_WEAK : constant Symbol_Flags := 16#80#;
   --  A weak global symbol, overridable without warnings by
   --  a regular global symbol of the same name.

   BSF_SECTION_SYM : constant Symbol_Flags := 16#100#;
   --  This symbol was created to point to a section, e.g. ELF's
   --  STT_SECTION symbols.

   BSF_OLD_COMMON : constant Symbol_Flags := 16#200#;
   --  The symbol used to be a common symbol, but now it is
   --  allocated.

   BFD_FORT_COMM_DEFAULT_VALUE : constant Symbol_Flags := 16#0#;
   --  The default value for common data.

   BSF_NOT_AT_END : constant Symbol_Flags := 16#400#;
   --  In some files the type of a symbol sometimes alters its
   --  location in an output file - ie in coff a <<ISFCN>> symbol
   --  which is also <<C_EXT>> symbol appears where it was
   --  declared and not at the end of a section.  This bit is set
   --  by the target BFD part to convey this information.

   BSF_CONSTRUCTOR : constant Symbol_Flags := 16#800#;
   --  Signal that the symbol is the label of constructor section.

   BSF_WARNING : constant Symbol_Flags := 16#1000#;
   --  Signal that the symbol is a warning symbol.  The name is a
   --  warning.  The name of the next symbol is the one to warn about;
   --  if a reference is made to a symbol with the same name as the next
   --  symbol, a warning is issued by the linker.

   BSF_INDIRECT : constant Symbol_Flags := 16#2000#;
   --  Signal that the symbol is indirect.  This symbol is an indirect
   --  pointer to the symbol with the same name as the next symbol.

   BSF_FILE : constant Symbol_Flags := 16#4000#;
   --  BSF_FILE marks symbols that contain a file name.  This is used
   --  for ELF STT_FILE symbols.

   BSF_DYNAMIC : constant Symbol_Flags := 16#8000#;
   --  Symbol is from dynamic linking information.

   BSF_OBJECT : constant Symbol_Flags := 16#10000#;
   --  The symbol denotes a data object.  Used in ELF, and perhaps
   --  others someday.

   BSF_DEBUGGING_RELOC : constant Symbol_Flags := 16#20000#;
   --  This symbol is a debugging symbol.  The value is the offset
   --  into the section of the data.  BSF_DEBUGGING should be set
   --  as well.

   --  END FRAGMENT: bfd/syms.c

   ----------------------
   -- Symbol           --
   ----------------------
   type Symbol is private;

   function Get_Flags (Sym : in Symbol) return Symbol_Flags;
   --  Get the flags associated with the symbol.

   function Get_Name (Sym : in Symbol) return String;
   --  Return the symbol name.

   function Get_Section (Sym : in Symbol) return Bfd.Sections.Section;
   --  Return the section where the symbol is defined.

   function Get_Value (Sym : in Symbol) return Symbol_Value;
   --  Return the value

   function Get_Symclass (Sym : in Symbol) return Character;
   --  Return a character corresponding to the symbol class of Sym.

   function Is_Local_Label (File : in File_Type;
                            Sym  : in Symbol) return Boolean;
   --  Returns true if the symbol is local.

   function Is_Local_Label_Name (File : in File_Type;
                                 Name : in String) return Boolean;
   --  Returns true if the label is local.

   function Is_Undefined_Class (C : Character) return Boolean;

   ----------------------
   -- Symbol_Table     --
   ----------------------
   type Symbol_Table is private;
   type Symbol_Iterator is private;

   function Is_Done (It : Symbol_Iterator) return Boolean;
   --  Return true if we are at end of the iterator.

   procedure Next (It : in out Symbol_Iterator);
   --  Move the iterator to the next element.

   function Current (It : in Symbol_Iterator) return Symbol;
   --  Return the current symbol pointed to by the iterator.

   procedure Open_Symbols (File : in File_Type;
                           Symbols : out Symbol_Table);
   --  Open and read all the symbols.
   --  The symbol table must be closed to avoid leaks.

   function Get_Iterator (Symbols : in Symbol_Table) return Symbol_Iterator;
   --  Return an iterator which allows scanning the symbol table.

   procedure Close_Symbols (Symbols : in out Symbol_Table);
   --  Close the symbol table and free any resource allocated for it.

   procedure Set_Symbols (File : in File_Type;
                          Symbols : in out Symbol_Table);
   --  Set the symbol table associated with the BFD file.

   procedure Find_Nearest_Line (File : in File_Type;
                                Sec : in Bfd.Sections.Section;
                                Symbols : in Symbol_Table;
                                Addr : in Vma_Type;
                                Name : out Unbounded_String;
                                Func : out Unbounded_String;
                                Line : out Natural);
   --  Find the nearest source file and line for a given address.
   --  Equivalent to bfd_find_nearest_line ().

   function Get_Symbol (Symbols : in Symbol_Table;
                        Pos : in Positive) return Symbol;

   function Get_Symbol (Symbols : in Symbol_Table;
                        Name : in String) return Symbol;

   function Get_Size (Symbols : in Symbol_Table) return Natural;


private

   type Symbol is new System.Address;
   type Symbol_Array is array (Positive range <>) of Symbol;
   type Symbol_Array_Access is access all Symbol_Array;
   --  To avoid memory copies the 'Symbol' is directly mapped to
   --  the BFD asymbol structure.  The C definition is not imported
   --  to simplify things.  The symbol table in BFD is an array
   --  of asymbol pointers (asymbol**).

   type Symbol_Table is record
      Syms : Symbol_Array_Access;
      Size : Natural := 0;
   end record;

   type Symbol_Iterator is record
      Symtab : Symbol_Table;
      Pos    : Positive := 1;
   end record;
   --  The symbol iterator keeps track of the symbol table
   --  and uses an index within it to mark the current symbol.

   Null_Address : constant Symbol := Symbol(System.Null_Address);

   pragma Import (C, Is_Undefined_Class, "bfd_is_undefined_symclass");
   pragma Import (C, Get_Symclass, "bfd_decode_symclass");
   --  C Functions provided by BFD library.

   pragma Import (C, Get_Value, "ada_bfd_asymbol_value");
   pragma Import (C, Get_Flags, "ada_bfd_asymbol_flags");
   --  C Functions provided by specific wrapper.

end Bfd.Symtab;
