#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash
set -o nounset
set -o errexit
msg="colecho -b"

f="flowchart_generator.nim"
bin="$f.bin"


nim c                 \
    --cc:tcc          \
    --verbosity:0     \
    --hints:off       \
    --debugger:native \
    -o:"$f.bin"       \
    $f

str="a = 1; b = 2; call(2);"
./$bin --verbose-parse:"$str"
# ./$bin --debug-parse:"$str"
