#! /bin/sh

# See `update_unit_test.sh` for documentation.

set -e

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"

. "$script_dir/opam-pin.sh"

tmp=$(mktemp)

packages=$(echo "$packages" | LC_COLLATE=C sort)

(
  csplit --quiet --prefix="$tmp" "$src_dir/.gitlab/ci/opam.yml" /##BEGIN_OPAM##/+1
  cat "$tmp"00
  rm "$tmp"0*

  echo "# this section is updated using the script $(basename $script_dir)/$(basename $0)"
  echo

  for package in $packages; do
      cat <<EOF
opam:$package:
  extends: .opam_template
  variables:
    package: $package

EOF
  done

  csplit --quiet --prefix="$tmp" "$src_dir/.gitlab/ci/opam.yml" %##END_OPAM##%
  cat "$tmp"00
  rm "$tmp"0*
) > $tmp

mv $tmp "$src_dir/.gitlab/ci/opam.yml"

