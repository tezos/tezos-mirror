#!/bin/sh

output=$(dune exec ./main.exe -- "$@" 2>&1)
exit_code=$?
# scrub timestamps
echo "$output" | sed 's/\[[[:digit:]:\.]\+\] //'
exit $exit_code
