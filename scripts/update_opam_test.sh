#! /bin/sh

# See `update_integration_test.sh` for documentation.

set -e

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"

#shellcheck disable=SC1090
. "$script_dir/opam-pin.sh"

tmp=$(mktemp)
lib_packages=$(echo "$lib_packages" | LC_COLLATE=C sort)

(
  csplit --quiet --prefix="$tmp" "$src_dir/.gitlab/ci/opam.yml" /##BEGIN_OPAM##/+1
  cat "$tmp"00
  rm "$tmp"0*

  echo "# this section is updated using the script $(basename "$script_dir")/$(basename "$0")"
  echo

  for package in $lib_packages; do
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
) > "$tmp"

mv "$tmp" "$src_dir/.gitlab/ci/opam.yml"

tmp=$(mktemp)
bin_packages=$(echo "$bin_packages" | LC_COLLATE=C sort)

(
  csplit --quiet --prefix="$tmp" "$src_dir/.gitlab/ci/opam.yml" /##BEGIN_OPAM_BIN##/+1
  cat "$tmp"00
  rm "$tmp"0*

  echo "# this section is updated using the script $(basename "$script_dir")/$(basename "$0")"
  echo

  for package in $bin_packages; do
      cat <<EOF
opam-bin:$package:
  extends: .opam_bin_template
  variables:
    package: $package

EOF
  done

  csplit --quiet --prefix="$tmp" "$src_dir/.gitlab/ci/opam.yml" %##END_OPAM_BIN##%
  cat "$tmp"00
  rm "$tmp"0*
) > "$tmp"

mv "$tmp" "$src_dir/.gitlab/ci/opam.yml"
