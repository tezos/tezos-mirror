#!/bin/sh
# Render as HTML static pages all the opeanpis passed as arguments

usage() {
  cat >&2 << !EOF
usage:
  $0 <openapi-file.json> ...
!EOF
}

set -e

if [ $# -lt 1 ]; then
  usage
  exit 1
fi

for f in "$@"; do
  file=$(basename "$f")
  dir=$(dirname "$f")
  yaml=${f%.json}.yaml
  out="$dir/static/${file%.json}.html"

  echo "converting $f to $out..."
  yq -o=yaml "$f" > "$yaml"
  redocly build-docs "$yaml"
  mv redoc-static.html "$out"
  rm "$yaml"
done

echo "Done"
