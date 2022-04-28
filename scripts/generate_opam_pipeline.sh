#!/usr/bin/env sh

set -eu

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"

#shellcheck disable=SC2154
opams=$(find "$src_dir/src" "$src_dir/tezt" "$src_dir/opam" -name \*.opam \
             -not -name tezos-benchmark\*.opam \
             -not -name tezos-shell-benchmark\*.opam \
             -not -name tezos-snoop.opam \
             -not -name tezos-protocol-\*-tests.opam \
             -print)

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
