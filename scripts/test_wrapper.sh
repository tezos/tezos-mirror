#!/bin/bash

nametest=${1:?}
name=${2:?}

mkdir -p test_results

echo -n "Running test \"dune build @$nametest/runtest\" ..."

dune build "@$nametest/runtest" > "test_results/$name.log" 2>&1

if [ $? ]; then echo "Ok"; else echo "Error"; fi

