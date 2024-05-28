#!/bin/sh

cd "$(git rev-parse --show-toplevel)" || exit 1

# ORIGIN can be specified to replace the comparison point.
if [ -z "$ORIGIN" ]; then
  ORIGIN=master
fi

make -C manifest manifest
CHANGES="$(git diff --name-only $(git merge-base HEAD "$ORIGIN"))"

if [ -z "$CHANGES" ]; then
  echo "No changes => no tests to run."
  echo "(But if you have untracked files, 'git add' them and try again.)"
  exit 0
fi

# Call the manifest in Manifezt mode.
# "--" ensures that if a filename starts with a dash, it is not interpreted as an option.
# Do not put quotes around $CHANGES since we need files to be separate arguments.
# shellcheck disable=SC2086
TSL="$(manifest/manifest --manifezt -- $CHANGES)"

if [ "$TSL" = "false" ]; then
  echo "There are changes, but no tests are affected."
  echo "(But if you have untracked files, 'git add' them and try again.)"
  exit 0
fi

if [ "$RECOMPILE_TEZT" = "no" ]; then
  _build/default/tezt/tests/main.exe "$TSL" "$@"
else
  dune exec tezt/tests/main.exe -- "$TSL" "$@"
fi
