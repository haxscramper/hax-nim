#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash
set -o nounset
set -o errexit
msg="colecho -b"

function stash_untracked {
    $msg -i "Found untracked changes. They will be stashed automatically"
    $msg -l "Stashing untracked changes"
    git stash
}

current_branch=$(git branch | grep -F "*" | cut -d ' ' -f2)

if [[ "$current_branch" = "master" ]]; then
    $msg -i "Updating master"
    git diff-index --quiet HEAD -- || stash_untracked
    git pull origin master
    ./build-all.sh
else
    $msg -i "Current branch is not master, no updates will be downloaded"
fi
# git pull origin master
