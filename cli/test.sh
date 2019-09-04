#!/usr/bin/env bash
# -*- coding: utf-8 -*- bash
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
    fi

}
function test_colecho {
    clear
    fd -e nim | xargs wc -l | sort -n | tail -n 1
    echo "Rebuilding ... "
    nim c \
        --cc:tcc \
        --verbosity:0 \
        --hints:off \
        -o:colecho_cli.nim.bin \
        colecho_cli.nim

    c=colecho_cli.nim.bin

    $c --bg-col:red -I:3 -w:0 "Test"
    $c --bg-col:blue -s:u

    styles=( "gtest" )

    for style in "${styles[@]}"
    do
        echo "[$style]"
        $c -l:0 --$style "$style log 0"
        $c -i:0 --$style "$style info 0"
        $c -w:0 --$style "$style warn 0"
        $c -e:0 --$style "$style error 0"
    done



    $c -eg -- $(cat << EOF
newSeq[T] for creating new sequences of type T
@ for converting arrays and strings to sequences
add for adding new elements to strings and sequences
& for string and seq concatenation
in (alias for contains) and notin for checking if an item is in a container
EOF
)

    $c -eg --rtr "Hello"
    $c -egu "Hello"
    $c -eLu -I:4 "Unifom log indent 4"
    $c --help
}

function test_builder {
    clear
    killall fsm-build
    colecho --gtest -e "Starting test"
    fd -e nim | xargs wc -l | sort -n | tail -n 1
    colecho -b "Rebuilding ... "

    build "fsm_build.nim"
    if [[ "$?" != 0 ]]; then
        return 0
    fi

    test_file="test.tmp.pl"
    rm -f "$test_file"
    echo -e '\n' | fsm-build dev "$test_file" &
    sleep 1
    echo "say 'test file mod';" >> "$test_file"

}

function test_create_script {
    build "create_script.nim"
    build "fsm_build.nim"
    if [[ "$?" != 0 ]]; then
        return 0
    fi

    test_file="test.tmp.pl"
    echo -en "0\n" | create-script $test_file
    colecho "Hello"
    stat $test_file
    rm -f $test_file

    return 0

    killall fsm-build
    rm -f "$test_file"
    start-coding.sh "$test_file" &
    coding_pid="$!"

    sleep 1

    echo "say 'another test';" >> "$test_file"
    logm="log1 'Using log1'"
    echo "$logm;" >> "$test_file"
    echo "warn1 'Broken code'" >> "$test_file"


    wait "$coding_pid"
    colecho -g -e "Done testing"

    colecho "start-coding.sh returned"
}

function test_argparse {
    build "argparse2.nim"
    if [[ "$?" != 0 ]]; then
        return 0
    fi

    bin="argparse2.nim.bin"
    ./$bin
}

function test_fsm_build {
    build "fsm_build.nim"
    if [[ "$?" != 0 ]]; then
        colecho -e:2 "Build failed"
        return
    else
        colecho -i:2 "Done build"
    fi
    # colecho -g "ps grep"
    # ps -a | grep build
    # colecho -g "done"
    # bin="fsm_build.nim.bin"
    # cd ../wip

    pushd "../bin" &> /dev/null
    ln -f ../cli/fsm_build.nim.bin fsm-build
    echo -en "y\n0\n" | fsm-build dev test.tmp.java &
    sleep 1
    rm -f test.tmp.java
    sleep 2
    killall fsm-build
}


#test_builder
#test_create_script
#test_argparse
test_fsm_build

inotifywait -e close_write,moved_to,create -m . |
    while read -r directory events f; do
        if [[ "$events" = *"CLOSE_WRITE"* ]]; then
          if [[ "$f" = *.nim ]] ||
                 [[ "$f" != "test1.sh" && "$f" == *".sh" ]]; then
            clear
            #test_builder
            #test_create_script
            #test_argparse
            test_fsm_build
          fi
        fi
    done
