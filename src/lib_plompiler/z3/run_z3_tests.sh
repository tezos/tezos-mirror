#!/bin/sh

for f in *.z3
do
    if z3 "$f" | grep -Fxq sat
    then
        echo "Z3 tests error: test $f is satisfiable"
        exit 1
    fi
done
echo "Z3 tests passed!"
