
# Ada BFD Library

[![Build Status](https://img.shields.io/jenkins/s/http/jenkins.vacs.fr/Ada-BFD.svg)](https://jenkins.vacs.fr/job/Ada-BFD/)
[![Test Status](https://img.shields.io/jenkins/t/http/jenkins.vacs.fr/Ada-BFD.svg)](https://jenkins.vacs.fr/job/Ada-BFD/)
[![Download](https://img.shields.io/badge/download-1.1.1-brightgreen.svg)](http://download.vacs.fr/ada-bfd/ada-bfd-1.1.1.tar.gz)
[![License](https://img.shields.io/badge/license-GPL-blue.svg)](GPL)
![Commits](https://img.shields.io/github/commits-since/stcarrez/ada-bfd/1.1.0.svg)

The Ada-BFD is a library which provides Ada API for GNU Binutils BFD
library.  It works on any version of GNU Binutils (starting at 2.15).
The recommended version for GNU Binutils is at least the 2.28.

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

# Installing and compiling Ada BFD

To compile Ada BFD you'll need:

* an Ada compiler (GNAT 2018 or gcc 7.3).
* the binutils 2.28 or higher installed for development.
  You need the bfd.h include as well as libbfd and libiberty.
  On Debian systems (including Ubuntu) install the development packages
  by using:

```
     sudo apt-get install binutils-dev
```

On NetBSD 6.1, you need to install by using:

```
     sudo pkgin install binutils-2.24nb3
     sudo pkgin install gettext-lib-0.19.2
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


# Compiling the Ada BFD samples

Several samples are provided to show how to use the Ada BFD library.
To build them, use:

```
  make samples
```

# Projects using Ada BFD

* [Muen Separation Kernel](https://muen.codelabs.ch/), An x86/64 Separation Kernel for High Assurance
* [Memory Analysis Tool](https://github.com/stcarrez/mat)




