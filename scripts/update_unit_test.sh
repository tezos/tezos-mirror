#! /bin/sh

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
# the line ##BEGIN_UNITEST## is added to the temporary file.
sed -z 's/^\(.*##BEGIN_UNITEST##\n\).*\(\n##END_UNITEST##.*\)$/\1/' "$src_dir/.gitlab-ci.yml" > $tmp

# 2: Find each test folder and add the matching incantation to the temporary
# file.
for lib in `find src/ -name test -type d` ; do
  if git ls-files --error-unmatch $lib  > /dev/null 2>&1; then
    nametest=${lib%%/test}
    name=${nametest##src/lib_}
    cat >> $tmp <<EOF
unit:$name:
  <<: *test_definition
  script:
    - dune build @$nametest/runtest

EOF
  fi
done

# 2: Do the same for vendor libraries
for lib in `find vendors/ -name test -type d` ; do
  if git ls-files --error-unmatch $lib  > /dev/null 2>&1; then
    nametest=${lib%%/test}
    name=${nametest##vendors/}
    cat >> $tmp <<EOF
unit:$name:
  <<: *test_definition
  script:
    - dune build @$nametest/runtest

EOF
  fi
done


# 3: Extract the end of the CI configuration file. Everything after the line
# ##END_UNITEST## is added to the temporary file.
sed -z 's/^\(.*##BEGIN_UNITEST##\n\).*\(\n##END_UNITEST##.*\)$/\2/' "$src_dir/.gitlab-ci.yml" >> $tmp

# 4: The temporary file is swapped in place of the CI configuration file.
mv $tmp "$src_dir/.gitlab-ci.yml"

