#!/bin/sh

# Allow this script to be run by dune from another directory.
if [ -n "$DUNE_SOURCEROOT" ]; then
  cd "$DUNE_SOURCEROOT" || exit 1
fi

var_name=CUSTOM_DUNE_FLAGS
var_value="$CUSTOM_DUNE_FLAGS"
file=script-inputs/custom_dune_flags

help() {
  cat << EOF
Usage: $0 <check|sexp|set <FLAG..>|clear>

Set some flags to pass to the OCaml compiler pervasively.
For instance, -opaque can improve compilation times in some cases,
at the cost of runtime performance.

Custom flags can also be set using environment variable $var_name.
This overrides the configuration set with this script.

$0 check

  Check if some custom flags are active, whether set using this script
  or because of environment variable $var_name.
  If some are, print a message to show this set of flags.
  This is meant to be run by 'make'.

$0 sexp

  Output an S-expression containing the set of custom flags.
  This is meant to be run by 'dune' (see the 'dune' file at the root of the repository).

$0 set <FLAG..>

  Set the set of custom flags.
  Example:

    $0 set -opaque

$0 clear

  Clear the set of custom flags to return to the default configuration.
EOF
}

check() {
  if [ -n "$var_value" ]; then
    cat << EOF
The $var_name environment variable is not empty.
The following flags will be passed to the OCaml compiler:

    $var_value

For more information, run:

    $0

EOF
  elif [ -f "$file" ]; then
    contents="$(cat "$file")"
    if [ -n "$contents" ]; then
      cat << EOF
File $file is not empty.
The following flags will be passed to the OCaml compiler:

    $contents

For more information, run:

    $0

EOF
    else
      cat << EOF
File $file exists but is empty.
For more information, run:

    $0

EOF
    fi
  fi
}

sexp() {
  if [ -n "$var_value" ]; then
    echo "($var_value)"
  elif [ -f "$file" ]; then
    echo "($(cat "$file"))"
  else
    echo '()'
  fi
}

set_() {
  if [ "$#" -eq 0 ]; then
    help
    exit 1
  fi

  echo "$@" > "$file"
  cat << EOF
Set custom dune flags to: $@
Note that environment variable $var_name can overwrite this setting.
EOF
}

clear() {
  rm -f "$file"
  cat << EOF
Cleared custom dune flags.
Note that environment variable $var_name can overwrite this setting.
EOF
}

case "$1" in
"set")
  shift
  set_ "$@"
  ;;
check | sexp | clear)
  $1
  ;;
*)
  help
  ;;
esac
