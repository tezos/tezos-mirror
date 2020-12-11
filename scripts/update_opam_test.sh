#! /bin/bash

# See `update_unit_test.sh` for documentation.

set -e

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"

. "$script_dir/opam-pin.sh"

tmp=$(mktemp)
yamlfile="$src_dir/.gitlab/ci/opam.yml"

# 1: Extract the beginning of the CI configuration file. Everything up to
# the line ##BEGIN_OPAM## is added to the temporary file.
csplit --quiet --prefix="$tmp" "$yamlfile" /##BEGIN_OPAM##/+1
mv "$tmp"00 "$tmp"
rm "$tmp"0*

packages=$(echo "$packages" | LC_COLLATE=C sort)

IFS=" " read -r -a packageArray <<< "$(echo $packages |tr "\n" " ")"

# this is the number opam packages thare are
# going to be tested in the same CI job
g=8

for((i=0; i < ${#packageArray[@]}; i+=g))
do
  part=( "${packageArray[@]:i:g}" )
  echo "Creating opam:tests:group$i for: ${part[*]}"
  echo

  cat >> "$tmp" <<EOF

opam:tests:group$i:
  extends: .opam_template
  needs:
    - check_opam_lint
    - check_opam_deps
  script:
    - ./scripts/opam-pin.sh
EOF

  for package in ${part[*]}; do
    echo "    - scripts/test_wrapper.sh opam $package" >> "$tmp"
  done
done

# 3: Extract the end of the CI configuration file. Everything after the line
# ##END_OPAM## is added to the temporary file.
csplit --quiet --prefix="$tmp" "$yamlfile" %##END_OPAM##%
cat "$tmp"00 >> "$tmp"
rm "$tmp"0*

mv "$tmp" "$yamlfile"
