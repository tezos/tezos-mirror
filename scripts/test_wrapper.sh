#!/bin/bash

nametest=${1:?}
name=${2:?}

mkdir -p test_results

echo "Running test \"dune build @$nametest/runtest\" ..."

START=$(date +%s.%N)

dune build "@$nametest/runtest" > "test_results/$name.log" 2>&1
EXITCODE=$?

END=$(date +%s.%N)

dt=$(echo "$END - $START" | bc)
dd=$(echo "$dt/86400" | bc)
dt2=$(echo "$dt-86400*$dd" | bc)
dh=$(echo "$dt2/3600" | bc)
dt3=$(echo "$dt2-3600*$dh" | bc)
dm=$(echo "$dt3/60" | bc)
ds=$(echo "$dt3-60*$dm" | bc)

LC_NUMERIC=C printf "Total runtime: %02d:%02d:%02.4f\n" "$dh" "$dm" "$ds"

if [ $EXITCODE -eq 0 ]; then
  echo "Ok";
else
  echo "Error";
  echo "Exited with exitcode $EXITCODE"
  cat "test_results/$name.log"
fi

exit $EXITCODE
