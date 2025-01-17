#!/usr/bin/env bash
# Generate a single-file text version of the documentation usable by bots

MYDIR="$(dirname "$0")"
BUILDDIR="$MYDIR"/../_build

grep 'toctree-l\d' "$BUILDDIR"/index.html |
  sed 's|^.*href="||; s|.html">.*$|.txt|' |
  grep -v -f "$MYDIR"/octezdoc-excludes.txt |
  while read -r f; do
    cat _build_txt/"$f"
    # insert blank line
    echo ""
  done
