#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash
cd "$(dirname "$0")"
set -o nounset
set -o errexit

nim c                             \
    --cc:tcc                      \
    --verbosity:0                 \
    --hints:off                   \
    -o:test.bin                   \
    -d:nimOldCaseObjects          \
    --warning[CaseTransition]:off \
    test.nim

#nim doc test
if [[ "$?" = "0" ]]; then
    echo "Running"
  ./test.bin
fi
