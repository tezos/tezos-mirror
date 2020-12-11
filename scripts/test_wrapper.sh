#!/bin/bash

help() {
  echo "You must specify $0 <type> <test> [name]"
  echo "Ex: $0 dune src/lib_base/test lib_base"
  echo "Ex: $0 shell 'echo \"this is a test\"' testname"
  echo "In the opam case [name] can be omitted"
  echo "Ex: $0 opam lib_base"
  exit 1
}

if [[ $# -eq 0 ]] ; then
  help
fi

testtype=${1:?"must specify type"}
nametest=${2:?"type dependent script arg"}

mkdir -p test_results

START=$(date +%s.%N)

if [ "${testtype}" == "dune" ]; then
  name=${3:?}
  echo "Running test \"dune build @$nametest/runtest\" ..."
  dune build "@$nametest/runtest" > "test_results/$name.log" 2>&1
elif [ "${testtype}" == "opam" ]; then
  name=$nametest
  echo "Running opam test for package ${nametest}"
  echo "depext, install, reinstall, remove ..."
  # if the script is not run in a terminal, we assume the
  # package pinning was already done
  if [ -t 1 ] ; then
    echo "Running in a terminal"
    ./scripts/opam-pin.sh;
  fi
  (
    opam depext --yes "${nametest}"
    opam install --yes "${nametest}"
    opam reinstall --yes --with-test "${nametest}"
    opam remove -a --yes --with-test "${nametest}"
  ) > "test_results/$name.log" 2>&1
elif [ "${testtype}" == "shell" ]; then
  name=${3:?}
  echo "Running shell test $name..."
  echo "${nametest}"
  ${nametest} > "test_results/$name.log" 2>&1
else
  help
fi

EXITCODE=$?

END=$(date +%s.%N)

dt=$(echo "$END - $START" | bc)
dd=$(echo "$dt/86400" | bc)
dt2=$(echo "$dt-86400*$dd" | bc)
dh=$(echo "$dt2/3600" | bc)
dt3=$(echo "$dt2-3600*$dh" | bc)
dm=$(echo "$dt3/60" | bc)
ds=$(echo "$dt3-60*$dm" | bc)

LC_NUMERIC=C printf "Total runtime: %02dh:%02dmin:%02.4fs\n" "$dh" "$dm" "$ds"

if [ $EXITCODE -eq 0 ]; then
  echo "Ok";
else
  echo "Error";
  echo "Exited with exitcode $EXITCODE"
  cat "test_results/$name.log"
fi

exit $EXITCODE
