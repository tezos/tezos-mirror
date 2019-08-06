#!/bin/bash

## Testing for dependencies

type ocp-indent > /dev/null 2>&-
if [ $? -ne 0 ]; then
  echo "ocp-indent is required but could not be found. Aborting."
  exit 1
fi
type sed > /dev/null 2>&-
if [ $? -ne 0 ]; then
  echo "sed is required but could not be found. Aborting."
  exit 1
fi
type diff > /dev/null 2>&-
if [ $? -ne 0 ]; then
  echo "diff is required but could not be found. Aborting."
  exit 1
fi
type find > /dev/null 2>&-
if [ $? -ne 0 ]; then
  echo "find is required but could not be found. Aborting."
  exit 1
fi

## Setup: temporary directory, failure witness flag, minimal argument parsing

tmp_dir="$(mktemp -d -t tezos_build.XXXXXXXXXX)"
failed=no
if [ "$1" = "fix" ]; then
    fix=yes
    shift 1
fi

## Main argument parsing

files="$@"
if [ -z "$files" ]; then
files=` find . \( -name _build -or \
                  -name .git -or \
                  -name _opam -or \
                  -wholename ./src/environment/v1.ml -or \
                  -name ocplib-json-typed -or \
                  -name registerer.ml \) -prune -or \
                  \( -name \*.ml -or -name \*.mli \) -print`
fi

## Core functionality

for f in $files ; do
  ff=$(basename $f)
  # copy file to temporary directory
  cp $f $tmp_dir/$ff
  # lint temporary file in place (remove tabs, indent, remove trailing spaces)
  sed -i.prev 's/\t/  /' $tmp_dir/$ff && rm $tmp_dir/$ff.prev
  ocp-indent --config match_clause=4 --inplace $tmp_dir/$ff
  sed -i.prev 's/ \+$//' $tmp_dir/$ff && rm $tmp_dir/$ff.prev
  # compare original and linted file, act if need be
  diff -U 3 $f $tmp_dir/$ff
  if [ $? -ne 0 ]; then
    if [ "$fix" = "yes" ]; then
      echo "Fix indentation $f"
      cp $tmp_dir/$ff $f
    else
      failed=yes
    fi
  fi
  # clean up
  rm -f $tmp_dir/$ff $tmp_dir/$ff.diff
done

rm -rf $tmp_dir

if [ $failed = "yes" ]; then
    exit 2
fi
