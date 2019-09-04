#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash
cd "$(dirname "$0")"
set -o nounset
set -o errexit

# This file file will run on each update
echo "Running test.sh"
##== Only edit lines after this comment

find .. -name "*.nim" | xargs cat | wc -l

nim c \
  --verbosity:0 \
  --hints:off \
  --cc:tcc \
  -o:main.nim.bin \
  main.nim

# Run target
./main.nim.bin

clang-format result.cpp.tmp | bat -lc++ -p
