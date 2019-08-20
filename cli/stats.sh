#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash
cd "$(dirname "$0")"
set -o nounset

function fsm_build_stat {
  clear
  colecho "stat" -g -w:2
  ps -a
}

fsm_build_stat

inotifywait -e close_write,moved_to,create -m . |
    while read -r directory events filename; do
        if [[ "$events" = *"CLOSE_WRITE"* ]]; then
            if [[ "$filename" = "colecho_lib.nim" ]]; then
                fsm_build_stat
            fi
        fi
    done
