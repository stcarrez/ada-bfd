#!/bin/sh

T=`bin/sections bin/bfdgen | grep .text | wc -l`
if test $T -ne 1; then
    echo "Test samples/sections failed"
    exit 1
fi

T=`bin/bfdinfo bin/bfdgen | grep .rodata | wc -l`
if test $T -ne 2; then
    echo "Test samples/bfdinfo failed"
    exit 1
fi

T=`bin/symbol bin/bfdgen main | grep global | wc -l`
if test $T -ne 1; then
    echo "Test samples/symbol failed"
    exit 1
fi

T=`bin/disassemble bin/bfdgen | grep support/bfdgen.c | wc -l`
if test $T -ge 1; then
    echo "Test samples/disassemble failed"
    exit 1
fi

T=`bin/disassemble bin/bfdgen | grep bfdgen.c | wc -l`
if test $T -lt 10; then
    echo "Test samples/disassemble failed"
    exit 1
fi

