
# Ada BFD Library

[![Build Status](https://img.shields.io/endpoint?url=https://porion.vacs.fr/porion/api/v1/projects/ada-bfd/badges/build.json)](https://porion.vacs.fr/porion/projects/view/ada-bfd/summary)
[![Test Status](https://img.shields.io/endpoint?url=https://porion.vacs.fr/porion/api/v1/projects/ada-bfd/badges/tests.json)](https://porion.vacs.fr/porion/projects/view/ada-bfd/xunits)
[![Coverage](https://img.shields.io/endpoint?url=https://porion.vacs.fr/porion/api/v1/projects/ada-bfd/badges/coverage.json)](https://porion.vacs.fr/porion/projects/view/ada-bfd/summary)
[![Download](https://img.shields.io/badge/download-1.3.0-brightgreen.svg)](http://download.vacs.fr/ada-bfd/ada-bfd-1.3.0.tar.gz)
[![License](https://img.shields.io/badge/license-GPL-blue.svg)](GPL)
![Commits](https://img.shields.io/github/commits-since/stcarrez/ada-bfd/1.3.0.svg)

The Ada-BFD is a library which provides Ada API for GNU Binutils BFD
library.  It works on any version of GNU Binutils (starting at 2.15).
The recommended version for GNU Binutils is at least the 2.39.

The Ada-BFD library allows to:

* list and scan the ELF sections of an executable or object file,
* get the content of the ELF sections,
* get access to the symbol table,
* use the BFD disassembler

The Ada BFD library is distributed under the GNU GENERAL PUBLIC LICENSE, Version 2
(see COPYING).

The unit tests used by Ada BFD use the Ada Util and Ada Test Util libraries distributed
under the Apache License Version 2 (see testutil/LICENSE.txt).  They also use the
Ahven Unit Test Library distributed under the ISC license (see testutil/LICENSE.Ahven).
These unit test libraries are not installed.

## Version 1.3.0 - Aug 2023
  - Fix #7: Binutils 2.39 changed init_disassemble_info signature
  - Fix #8: The dynamic minisymbols are not loaded by Bfd.Symbols.Read_Symbols

[List all versions](https://github.com/stcarrez/ada-bfd/blob/master/NEWS.md)

# Installing and compiling Ada BFD

To compile Ada BFD you'll need:

* an Ada compiler (gcc 12.1).
* the binutils 2.39 or higher installed for development.
  You need the bfd.h include as well as libbfd and libiberty.
  On Debian systems (including Ubuntu) install the development packages
  by using:

```
sudo apt-get install binutils-dev libiberty-dev
```

On NetBSD 9.2, you need to install by using:

```
sudo pkg_add binutils
sudo pkg_add gettext-lib
sudo pkg_add libiberty
```


Run the GNU configure command and build the library:

```
./configure
make
make check
```


You can install the library by using:

```
make install
```

# Build with Alire

You can also build the Ada BFD library with Alire by using (but the
dependency to the `binutils-dev` is not handled by the crate):

```
alr build
```

# Compiling the Ada BFD samples

Several samples are provided to show how to use the Ada BFD library.
To build them, use:

```
make samples
```

|Example    | Usage example                 | Description                                         |
|-----------|-------------------------------|-----------------------------------------------------|
|bfdinfo    |./bin/bfdinfo ./bin/bfdgen     | Open BFD file, get flags, list sections and symbols |
|sections   |./bin/sections ./bin/bfdgen    | Display the ELF sections with the `Bfd.Sections`    |
|symbol     |./bin/symbol ./bin/bfdgen main | Read the symbol table with `Bfd.Symbols`            |
|disassemble|./bin/disassemble ./bin/bfdgen | Disassemble the text section with `Bfd.Disassemble` |

# Projects using Ada BFD

* [Muen Separation Kernel](https://muen.codelabs.ch/), An x86/64 Separation Kernel for High Assurance
* [Memory Analysis Tool](https://github.com/stcarrez/mat)

# Documentation

* [BFD Documentation](http://sourceware.org/binutils/docs/bfd/index.html)
* [Reading a program symbol table with Ada BFD](https://blog.vacs.fr/vacs/blogs/post.html?post=2012/11/03/Reading-a-program-symbol-table-with-Ada-Bfd)

