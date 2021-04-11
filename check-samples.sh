#!/bin/sh

T=`bin/sections bin/bfdgen | grep .text | wc -l`
if test $T -ne 1; then
    echo "Test samples/sections failed"
    exit 1
fi

T=`bin/sections bin/bfdgen-oops 2>&1 | grep 'Cannot open file' | wc -l`
if test $T -ne 1; then
    echo "Test samples/sections open error failed"
    exit 1
fi

T=`bin/sections 2>&1 | grep Usage: | wc -l`
if test $T -ne 1; then
    echo "Test samples/sections usage failed"
    exit 1
fi

T=`bin/bfdinfo bin/bfdgen | grep .rodata | wc -l`
if test $T -ne 2; then
    echo "Test samples/bfdinfo failed"
    exit 1
fi

T=`bin/bfdinfo regtests/files/test.o | grep alloc_secondary | wc -l`
if test $T -ne 2; then
    echo "Test samples/bfdinfo on test.o failed"
    exit 1
fi

T=`bin/bfdinfo -k 2>&1 | grep Usage: | wc -l`
if test $T -ne 1; then
    echo "Test samples/bfdinfo usage failed"
    exit 1
fi

T=`bin/bfdinfo bin/bfdgen-oops 2>&1 | grep 'Cannot open file' | wc -l`
if test $T -ne 1; then
    echo "Test samples/bfdinfo open error failed"
    exit 1
fi

T=`bin/symbol bin/bfdgen main | grep global | wc -l`
if test $T -ne 1; then
    echo "Test samples/symbol failed"
    exit 1
fi

T=`bin/symbol regtests/files/test_common.o common_sect | grep 'other symbol' | wc -l`
if test $T -ne 1; then
    echo "Test samples/symbol common symbol failed"
    exit 1
fi

T=`bin/symbol bin/bfdgen-oops main 2>&1 | grep 'Cannot open file' | wc -l`
if test $T -ne 1; then
    echo "Test samples/symbol open error failed"
    exit 1
fi

T=`bin/symbol regtests/files/test.o .LFB2 | grep 'local symbol' | wc -l`
if test $T -ne 1; then
    echo "Test samples/symbol local symbol failed"
    exit 1
fi

T=`bin/symbol regtests/files/test.o toto 2>&1 | grep 'toto: not found' | wc -l`
if test $T -ne 1; then
    echo "Test samples/symbol symbol not found failed"
    exit 1
fi

T=`bin/symbol 2>&1 | grep Usage: | wc -l`
if test $T -ne 1; then
    echo "Test samples/symbol usage failed"
    exit 1
fi

T=`bin/disassemble bin/bfdgen | grep support/bfdgen.c | wc -l`
if test $T -lt 1; then
    echo "Test samples/disassemble failed"
    exit 1
fi

T=`bin/disassemble bin/bfdgen | grep bfdgen.c | wc -l`
if test $T -lt 1; then
    echo "Test samples/disassemble failed"
    exit 1
fi

T=`bin/disassemble bin/bfdgen-oops 2>&1 | grep 'Cannot open file' | wc -l`
if test $T -lt 1; then
    echo "Test samples/disassemble open error failed"
    exit 1
fi

T=`bin/disassemble 2>&1 | grep Usage: | wc -l`
if test $T -lt 1; then
    echo "Test samples/disassemble usage failed"
    exit 1
fi

