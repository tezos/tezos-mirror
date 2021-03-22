#! /bin/sh

# See `update_unit_test.sh` for documentation.

set -e

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"

tmp=$(mktemp)

csplit --quiet --prefix="$tmp" "$src_dir/.gitlab/ci/integration.yml" /##BEGIN_INTEGRATION_PYTHON##/+1
mv "$tmp"00 "$tmp"
rm "$tmp"0*

for PROTO_DIR in $(find tests_python/ -maxdepth 1 -mindepth 1 -iname 'tests_*' | LC_COLLATE=C sort); do
    PROTO_DIR_BASE=${PROTO_DIR##tests_python/tests_}

    # Add fast python integration tests grouped in one job
    cat >> "$tmp" <<EOF
integration:${PROTO_DIR_BASE}_fast:
  extends: .integration_python_template
  script:
    - poetry run pytest "${PROTO_DIR##tests_python/}" --exitfirst -m "not slow" -s --log-dir=tmp "--junitxml=reports/${PROTO_DIR_BASE}_fast.xml" 2>&1 | tee "tmp/${PROTO_DIR_BASE}_fast.out" | tail
  stage: test

EOF

    # Add slow python integration tests, one per job
    slow_tests=$(cd "$PROTO_DIR";
                 for i in $(poetry run pytest --exitfirst -m slow --collect-only -qq); do
                     echo "${i%%\:\:*}" ;
                 done | grep ^tests_.*\.py | uniq | LC_COLLATE=C sort)

    for test in $slow_tests; do
     case "$test" in
       */multibranch/*)
         # skip multibranch tests for now
         ;;
       *)

        testname=${test##*/test_}
        testname=${testname%%.py}

        cat >> "$tmp" <<EOF
integration:${PROTO_DIR_BASE}_${testname}:
  extends: .integration_python_template
  script:
    - poetry run pytest "${test}" --exitfirst -s --log-dir=tmp "--junitxml=reports/${PROTO_DIR_BASE}_${testname}.xml" 2>&1 | tee "tmp/${PROTO_DIR_BASE}_${testname}.out" | tail
  stage: test

EOF
     esac
    done

done

cat >> $tmp <<EOF
integration:examples:
  extends: .integration_python_template
  script:
    - PYTHONPATH=\$PYTHONPATH:./ poetry run python examples/forge_transfer.py
    - PYTHONPATH=\$PYTHONPATH:./ poetry run python examples/example.py
    - PYTHONPATH=./ poetry run pytest --exitfirst examples/test_example.py
  stage: test
EOF

csplit --quiet --prefix="$tmp" "$src_dir/.gitlab/ci/integration.yml" %##END_INTEGRATION_PYTHON##%
cat "$tmp"00 >> "$tmp"
rm "$tmp"0*

mv $tmp "$src_dir/.gitlab/ci/integration.yml"
