#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash
set -o nounset
set -o errexit
msg="colecho -b"

bin="get_daily_note.nim.bin"

./$bin --mod-file --file-dir:"$PWD" --update-symlink

cat today.org
rm *.org
ls -al
