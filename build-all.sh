#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash
cd "$(dirname "$0")"
set -o nounset

function build {
    file="$1"

    colecho -L "start $file"
    rm -f "$file.bin"

    nim c                             \
        --cc:tcc                      \
        --verbosity:0                 \
        --hints:off                   \
        -o:"$file.bin"                \
        -d:nimOldCaseObjects          \
        --warning[CaseTransition]:off \
        "$file"

    build_code="$?"

    colecho -L "end"
    echo


    if [[ "$build_code" != "0" ]]; then
        colecho -e "Build failed"
        return 1
    else
        colecho -i "Build succeded"
    fi
}

cd cli
build "fsm_build.nim"
build "colecho_cli.nim"
build "create_script.nim"
cd ../utils
build "get_daily_note.nim"
