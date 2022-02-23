#!/usr/bin/env bash

set -eu

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"

#shellcheck disable=SC1090
. "$script_dir"opam-pin.sh

#shellcheck disable=SC2154
PACKAGES=$(echo "$packages" | tr '\n' ' ')

{
cat <<EOF
include: ".gitlab/ci/templates.yml"

stages:
  - packaging

EOF
} > opam-ci.yml

for PKG in $PACKAGES; do
{
  cat <<EOF

opam:$PKG:
  extends: .opam_template
  variables:
    package: ${PKG}
EOF
} >> opam-ci.yml
done
