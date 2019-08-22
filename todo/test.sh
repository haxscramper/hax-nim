#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash
cd "$(dirname "$0")"
set -o nounset
set -o errexit

file="ast_to_dot.nim"
bin="$file.bin"

nim c                             \
    --cc:tcc                      \
    --verbosity:0                 \
    --hints:off                   \
    -o:$bin                    \
    -d:nimOldCaseObjects          \
    --warning[CaseTransition]:off \
    "$file"

./$bin "input.tmp.nim"
dot test.dot -Tpng > test.png
cp test.png res.png

echo "Done"
