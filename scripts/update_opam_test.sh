#! /bin/sh

# See `update_unit_test.sh` for documentation.

set -e

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"

. "$script_dir/opam-pin.sh"

tmp=$(mktemp)

packages=$(echo "$packages" | sort)

(
  sed -z 's/^\(.*##BEGIN_OPAM##\n\).*\(\n##END_OPAM##.*\)$/\1/' "$src_dir/.gitlab-ci.yml"

  echo "# this section is updated using the script $(basename $script_dir)/$(basename $0)"
  echo

  for package in $packages; do
      cat <<EOF
opam:$package:
  <<: *opam_definition
  variables:
    package: $package

EOF
  done

  sed -z 's/^\(.*##BEGIN_OPAM##\n\).*\(\n##END_OPAM##.*\)$/\2/' "$src_dir/.gitlab-ci.yml"
) > $tmp

mv $tmp "$src_dir/.gitlab-ci.yml"

