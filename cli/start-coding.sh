#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash
set -o nounset
set -o errexit

file="$1"

msg="colecho -b"

create-script --f-ext:"$file" "$file"

test1="test1.sh"

fsm-build --create:"~||($file||$test1)"
chmod +x "$test1"
chmod +x "$file"
./"$test1" "$file"
