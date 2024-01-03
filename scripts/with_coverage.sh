#!/bin/sh

set -e

BISECT_FILE=$(pwd)/_coverage_output/

export BISECT_FILE

export DUNE_INSTRUMENT_WITH=bisect_ppx

if [ -z "$1" ]; then
  echo "Missing command to run, example:"
  echo "${0} dune runtest src/lib_shell"
  exit 1
fi

echo "Running $* with:"
echo "BISECT_FILE=${BISECT_FILE}"
echo "DUNE_INSTRUMENT_WITH=bisect_ppx"
echo "-------------------------------"

"$@"
