#!/bin/sh

if [ $# -ne 2 ]; then
    echo "Bad args"
    exit 1
fi

{
    cat <<EOF
const WITH_SHARED_MEMORY=$1;
function print(x) {
    console.log(x);
}
EOF
    cat common.js;
    cat "$2";
} > nodeprog.js

node nodeprog.js
rm nodeprog.js
