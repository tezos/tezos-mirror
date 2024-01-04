#!/bin/sh

# Scan the log of `make linkcheck` and try to fix any redirected links.
# BEWARE: Sometimes the fixes must be more intelligent than simply replacing
# a link with its redirection (e.g. missing file redirected to a whole website).
# ALWAYS CHECK THE CHANGES BEFORE COMMITTING THEM!

set -eu

trace_file="_build/output.txt"

if ! [ -f $trace_file ]; then
  echo "Should be run within docs/ under the Tezos root directory"
  echo "The log of \`make linkcheck\`, which is assumed to be executed before,"
  echo "must be in _build/output.txt (under docs/)"
  exit 1
fi

regesc_pattern() {
  printf "%s\n" "$1" | sed -e 's/\([][\/.*|]\)/\\&/g'
}

# developer/snoop_example.rst:12: [redirected permanently] https://github.com/project-everest/hacl-star to https://github.com/hacl-star/hacl-star
# user/snapshots.rst:198: [redirected with Found] https://mainnet.xtz-shots.io/ to https://xtz-shots.io/mainnet/
grep -o '^[^/].*:[0-9][0-9]*: .*redirected.*' "$trace_file" | while read -r redirect; do
  pattern="^\([^:]*\):\([0-9][0-9]*\): .*redirected.*\(http[^ ]*\).*\(http[^ ]*\).*"

  file=$(echo "$redirect" | sed "s@${pattern}@\1@")
  line=$(echo "$redirect" | sed "s@${pattern}@\2@")
  prev=$(echo "$redirect" | sed "s@${pattern}@\3@")
  next=$(echo "$redirect" | sed "s@${pattern}@\4@")

  echo "Replacing '$prev' with '$next' on line '$line' of '$file'"
  sed "s/$(regesc_pattern "$prev")/$(regesc_pattern "$next")/" "$file" > "$file.fixed"

  if diff -q "$file" "$file.fixed" > /dev/null; then
    echo "No replacement was made"
    rm -f "$file.fixed"
  else
    mv "$file.fixed" "$file"
  fi
done
