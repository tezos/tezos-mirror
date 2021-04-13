#! /usr/bin/env bash

# Usage: ./scripts/update_unit_tests.sh

# This script is for automatically updating the tests in `.gitlab/ci/unittest.yml`. This
# script specifically updates the unit tests, the script
# `update_integration_test.sh` is for similarly updating the integration
# tests, and the script `update_opam_test.sh` is for upgrading the packaging
# tests. All three scripts are similar in structure and the documentation in
# this one stands for the documentation of the other. Note that step 2 varies
# from script to script, but the logic and the intent is the same.

# In this particular script, unit tests are broken up in a set of
# jobs: one per protocol containing tests and one containing all
# remaining jobs.

set -e

# Removes any lines in input that does not correspond to a file
# tracked by git
function filter_git_tracked() {
    while read -r file; do
        git ls-files --error-unmatch "$file" > /dev/null 2>&1 && echo "$file"
    done
}

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"

tmp=$(mktemp)

# 1: Extract the beginning of the CI configuration file. Everything up to
# the line ##BEGIN_UNITTEST## is added to the temporary file.
csplit --quiet --prefix="$tmp" "$src_dir/.gitlab/ci/unittest.yml" /##BEGIN_UNITTEST##/+1
mv "$tmp"00 "$tmp"
rm "$tmp"0*

# 2a. Find each proto test add the matching incantation to a temporary
# file.
for proto in $(find src/ -mindepth 1 -maxdepth 1 -iname proto_\* -type d | LC_COLLATE=C sort); do
    name_job=${proto##src/proto_}

    tested_libs=$(find "$proto" -name test -type d 2>/dev/null | filter_git_tracked | LC_COLLATE=C sort)
    if [ -n "$tested_libs" ]; then
        cat >> "$tmp" <<EOF
unit:${name_job}:
  extends: .unit_test_template
  script:
EOF
        for lib in $tested_libs ; do
            nametest=${lib%%/test}
            name=$nametest
            name=${name##src/proto_}
            name=${name//\//_}
            cat >> "$tmp" <<EOF
    - scripts/test_wrapper.sh $nametest $name
EOF
        done
        echo "" >> "$tmp"
  fi
done

# add a job for remaining tests
cat >> "$tmp" <<EOF
unit:non-proto:
  extends: .unit_test_template
  script:
EOF

# 2b: Find each non-proto test folder and add the matching incantation to
# the temporary file.
for lib in $(find src/ vendors/ -path src/proto_\* -prune -o -name test -type d -print | filter_git_tracked | LC_COLLATE=C sort) ;
do
    nametest=${lib%%/test}
    name=$nametest
    name=${name##src/bin_}
    name=${name##src/lib_}
    name=${name##vendors/}
    name=${name//\//_}
    cat >> "$tmp" <<EOF
    - scripts/test_wrapper.sh $nametest $name
EOF
done


# 3: Extract the end of the CI configuration file. Everything after the line
# ##END_UNITTEST## is added to the temporary file.
csplit --quiet --prefix="$tmp" "$src_dir/.gitlab/ci/unittest.yml" %##END_UNITTEST##%
cat "$tmp"00 >> "$tmp"
rm "$tmp"0*

# 4: The temporary file is swapped in place of the CI configuration file.
mv "$tmp" "$src_dir/.gitlab/ci/unittest.yml"

