#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash
cd "$(dirname "$0")"
set -o nounset
set -o errexit

echo 'Running run-dev.sh'

{ find .. -name "*.nim" ;
  find . -name "*.toml" ;
    cat << EOF
test.sh
types.toml
EOF
} \
| entr sh -c "clear ; echo 'Running entr ...' ; ./test.sh"
