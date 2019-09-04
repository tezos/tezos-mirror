#! /bin/sh

# See `update_unit_test.sh` for documentation.

set -e

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"

tmp=$(mktemp)

sed -z 's/^\(.*##BEGIN_INTEGRATION_PYTHON##\n\).*\(\n##END_INTEGRATION_PYTHON##.*\)$/\1/' "$src_dir/.gitlab-ci.yml" > $tmp

for test in tests_python/tests/test_*.py; do
    testname=${test##tests_python/tests/test_}
    testname=${testname%%.py}
    cat >> $tmp <<EOF
integration:$testname:
  <<: *integration_python_definition
  script:
    - pytest $test -s --log-dir=tmp
  stage: test

EOF
done

cat >> $tmp <<EOF
integration:examples_forge_transfer:
  <<: *integration_definition
  script:
    - PYTHONPATH=tests_python/ python3 tests_python/examples/forge_transfer.py
  stage: test

integration:examples_example:
  <<: *integration_definition
  script:
    - PYTHONPATH=tests_python/ python3 tests_python/examples/example.py
  stage: test

integration:examples_test_example:
  <<: *integration_definition
  script:
    - pytest tests_python/examples/test_example.py
  stage: test
EOF

sed -z 's/^\(.*##BEGIN_INTEGRATION_PYTHON##\n\).*\(\n##END_INTEGRATION_PYTHON##.*\)$/\2/' "$src_dir/.gitlab-ci.yml" >> $tmp

mv $tmp "$src_dir/.gitlab-ci.yml"
