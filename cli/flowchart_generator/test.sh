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

./$bin --test-line:"tests/simple.txt"
