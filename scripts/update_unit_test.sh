#! /usr/bin/env bash

# This script is for automatically updating the tests in `.gitlab-ci.yml`. This
# script specifically updates the unit tests, the script
# `update_integration_test.sh` is for similarly updating the integration
# tests, and the script `update_opam_test.sh` is for upgrading the packaging
# tests. All three scripts are similar in structure and the documentation in
# this one stands for the documentation of the other. Note that step 2 varies
# from script to script, but the logic and the intent is the same.

set -e

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"

tmp=$(mktemp)

# 1: Extract the beginning of the CI configuration file. Everything up to
# the line ##BEGIN_UNITTEST## is added to the temporary file.
csplit --quiet --prefix="$tmp" "$src_dir/.gitlab/ci/unittest.yml" /##BEGIN_UNITTEST##/+1
mv "$tmp"00 "$tmp"
rm "$tmp"0*

cat >> "$tmp" <<EOF
unit:alltest:
  extends: .test_template
  artifacts:
    name: "alltest-\${CI_COMMIT_SHA}"
    paths:
      - test_results
    expire_in: 1 day
#    when: on_failure
  script:
EOF

# 2: Find each test folder and add the matching incantation to the temporary
# file.
for lib in $(find src/ vendors/ -name test -type d | LC_COLLATE=C sort) ; do
  if git ls-files --error-unmatch "$lib"  > /dev/null 2>&1; then
    nametest=${lib%%/test}
    name=$nametest
    name=${name##src/bin_}
    name=${name##src/lib_}
    name=${name##src/proto_}
    name=${name##vendors/}
    name=${name//\//_}
    cat >> "$tmp" <<EOF
    - scripts/test_wrapper.sh $nametest $name
EOF
  fi
done


# 3: Extract the end of the CI configuration file. Everything after the line
# ##END_UNITTEST## is added to the temporary file.
csplit --quiet --prefix="$tmp" "$src_dir/.gitlab/ci/unittest.yml" %##END_UNITTEST##%
cat "$tmp"00 >> "$tmp"
rm "$tmp"0*

# 4: The temporary file is swapped in place of the CI configuration file.
mv "$tmp" "$src_dir/.gitlab/ci/unittest.yml"

