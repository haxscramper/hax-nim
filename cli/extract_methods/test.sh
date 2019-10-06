#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash
set -o nounset
set -o errexit
msg="colecho -b"
bin="extract_methods.nim.bin"
rm -f *.tmp.* tests/*.tmp.*
./$bin --input-file:"tests/test.java" --output-dir:"tests"
