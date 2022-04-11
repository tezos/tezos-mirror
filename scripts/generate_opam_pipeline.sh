#!/usr/bin/env sh

set -eu

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"

#shellcheck disable=SC2154
opams=$(find "$src_dir/vendors" "$src_dir/src" "$src_dir/tezt" -name \*.opam -print)

cat <<EOF
include: ".gitlab/ci/templates.yml"

stages:
  - packaging

EOF

for opam in $opams; do
  file=$(basename "$opam")
  pkg=${file%.opam}

  cat <<EOF

opam:${pkg}:
  extends: .opam_template
  variables:
    package: ${pkg}
EOF
done
