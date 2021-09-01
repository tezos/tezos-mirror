#! /bin/sh

# Usage: ./scripts/update_integration_test.sh

# This script is for automatically updating the tests in
# `.gitlab/ci/integration.yml`. This script specifically updates the integration
# tests; the script `update_opam_test.sh` is for upgrading the packaging tests.
# Both scripts are similar in structure and the documentation in this one stands
# for the documentation of the other. Note that step 2 varies from script to
# script, but the logic and the intent is the same.

set -e

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"

tmp=$(mktemp)

# 1: Extract the beginning of the CI configuration file. Everything up to the
# line ##BEGIN_INTEGRATION_PYTHON## is added to the temporary file.
csplit --quiet --prefix="$tmp" "$src_dir/.gitlab/ci/integration.yml" /##BEGIN_INTEGRATION_PYTHON##/+1
mv "$tmp"00 "$tmp"
rm "$tmp"0*

# 2. Find each test add the matching incantation to a temporary file.
for PROTO_DIR in $(find tests_python/ -maxdepth 1 -mindepth 1 -iname 'tests_*' | LC_COLLATE=C sort); do
    PROTO_DIR_BASE=${PROTO_DIR##tests_python/tests_}

    # Add fast python integration tests grouped in one job
    cat >> "$tmp" <<EOF
integration:${PROTO_DIR_BASE}_batch:
  extends: .integration_python_template
  script:
    - poetry run pytest "${PROTO_DIR##tests_python/}" --exitfirst -m "not slow" -s --log-dir=tmp "--junitxml=reports/${PROTO_DIR_BASE}_batch.xml" 2>&1 | tee "tmp/${PROTO_DIR_BASE}_batch.out" | tail
  stage: test

EOF

    # Add slow python integration tests, one per job
    slow_tests=$(cd "$PROTO_DIR";
                 for i in $(poetry run pytest --exitfirst -m slow --collect-only -qq); do
                     echo "${i%%\:\:*}" ;
                 done | grep ^tests_.*\.py | uniq | LC_COLLATE=C sort)

    for test in $slow_tests; do
        testname=${test##*/test_}
        testname=${testname%%.py}

        cat >> "$tmp" <<EOF
integration:${PROTO_DIR_BASE}_${testname}:
  extends: .integration_python_template
  script:
    - poetry run pytest "${test}" --exitfirst -m "slow" -s --log-dir=tmp "--junitxml=reports/${PROTO_DIR_BASE}_${testname}.xml" 2>&1 | tee "tmp/${PROTO_DIR_BASE}_${testname}.out" | tail
  stage: test

EOF
    done

done

# add a job for the examples
cat >> $tmp <<EOF
integration:examples:
  extends: .integration_python_template
  script:
    - PYTHONPATH=\$PYTHONPATH:./ poetry run python examples/forge_transfer.py
    - PYTHONPATH=\$PYTHONPATH:./ poetry run python examples/example.py
    - PYTHONPATH=./ poetry run pytest --exitfirst examples/test_example.py
  stage: test
EOF

# 3: Extract the end of the CI configuration file. Everything after the line
# ##END_INTEGRATION_PYTHON## is added to the temporary file.
csplit --quiet --prefix="$tmp" "$src_dir/.gitlab/ci/integration.yml" %##END_INTEGRATION_PYTHON##%
cat "$tmp"00 >> "$tmp"
rm "$tmp"0*

# 4: The temporary file is swapped in place of the CI configuration file.
mv $tmp "$src_dir/.gitlab/ci/integration.yml"
