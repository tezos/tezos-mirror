#! /bin/sh

# See `update_unit_test.sh` for documentation.

set -e

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"

. "$script_dir/opam-pin.sh"

tmp=$(mktemp)

packages=$(echo $packages | sed -e 's/ /\n/' | sort)

sed -z 's/^\(.*##BEGIN_OPAM##\n\).*\(\n##END_OPAM##.*\)$/\1/' "$src_dir/.gitlab-ci.yml" > $tmp

for package in $packages; do
    cat >> $tmp <<EOF
opam:$package:
  <<: *opam_definition
  variables:
    package: $package

EOF
done

sed -z 's/^\(.*##BEGIN_OPAM##\n\).*\(\n##END_OPAM##.*\)$/\2/' "$src_dir/.gitlab-ci.yml" >> $tmp

mv $tmp "$src_dir/.gitlab-ci.yml"

