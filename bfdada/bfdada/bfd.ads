-----------------------------------------------------------------------
--  BFD -- Binary File Descriptor Library (Ada Interface)
--  <!-- Copyright (C) 2001, 2002, 2003, 2004 Free Software Foundation, Inc.
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
with Interfaces;
package Bfd is

   pragma Linker_Options ("-lbfd");
   pragma Linker_Options ("-liberty");

   type Format is (UNKNOWN, --  unknown file format
                   OBJECT,  --  linker/assembler/compiler object file
                   ARCHIVE, --  object archive file
                   CORE);   --  core dump
   --  The bfd_format.

   type Flags is new Integer;

   type Unsigned_64 is new Interfaces.Unsigned_64;

   type Integer_64 is new Interfaces.Integer_64;

   subtype Vma_Type is Unsigned_64;
   --  The bfd_vma used to represent an address.

   subtype Lma_Type is Vma_Type;
   --  Likewise for lma.

   subtype Size_Type is Unsigned_64;
   --  Used to represent the size of a section.

   subtype Symbol_Value is Unsigned_64;
   --  Used to represent the value of a symbol.

   type Offset_Type is new Integer_64;

   OPEN_ERROR : exception;
   USE_ERROR  : exception;
   NOT_FOUND  : exception;

   ----------------------
   -- General          --
   ----------------------
   type Error is (NO_ERROR,
                  SYSTEM_CALL,
                  INVALID_TARGET,
                  WRONG_FORMAT,
                  INVALID_OPERATION,
                  NO_MEMORY,
                  NO_SYMBOLS,
                  NO_ARMAP,
                  NO_MORE_ARCHIVED_FILES,
                  MALFORMED_ARCHIVE,
                  FILE_NOT_RECOGNIZED,
                  FILE_AMBIGUOUSLY_RECOGNIZED,
                  NO_CONTENTS,
                  NONREPRESENTABLE_SECTION,
                  NO_DEBUG_SECTION,
                  BAD_VALUE,
                  FILE_TRUNCATED,
                  FILE_TOO_BIG,
                  INVALID_ERROR_CODE);

   type Error_Handler is access procedure (Message : in String);
   --  The error handler is a procedure called when BFD functions
   --  want to report an error message.  In the C version, the handler
   --  has a printf-like signature, thus giving freedom for the
   --  formatting.  Here, the message is formatted and passed in Message.
   --
   --  @param Message the message to report

   function Get_Error return Error;
   --  Return the current error code.

   procedure Set_Error (To : in Error);
   --  Set the current error code.

   function Get_Error_Message (Code : in Error) return String;
   --  Return an error message corresponding to the last error
   --  This is equivalent to the C <tt>bfd_errmsg</tt>.
   --
   --  @param Code the error code
   --  @return the error message corresponding to the error code

   procedure Set_Error_Program_Name (To : in String);
   --  Set the program name in the BFD library.

   procedure Set_Error_Handler (To  : in Error_Handler;
                                Old : out Error_Handler);
   --  Set a new error handler in BFD library.


   ----------------------
   -- BFD file         --
   ----------------------
   --  This part deal with opening and closing the main BFD file.

   type File_Type is private;
   --  The file type representing the opened BFD file.

   procedure Open (File   : in out File_Type;
                   Name   : in String;
                   Target : in String := "");
   --  Open the file and obtain a bfd handler.

   procedure Close (File : in out File_Type);
   --  Close the file, releasing any resource allocated for it.

   function Is_Open (File : in File_Type) return Boolean;
   --  Check if the file is open.
   --
   --  @return true if the file is open

   function Get_Filename (File : in File_Type) return String;
   --  Get the filename that was used to open the file.

   function Check_Format (File   : in File_Type;
                          Expect : in Format) return Boolean;
   --  Check if the file is of the specified format.
   --  @return true if the file is open and of the specified format

   function Get_File_Flags (File : in File_Type) return Flags;

   function Get_Start_Address (File : in File_Type) return Vma_Type;
   --  Get the start address.

   function Get_Symbol_Count (File : in File_Type) return Natural;
   --  Return number of symbols.


   subtype Ptr is System.Address;
private
   subtype Pointer is System.Address;

   type Bfd_Ptr is new System.Address;

   type File_Type is record
      Abfd : Ptr := System.Null_Address;
   end record;

end Bfd;
