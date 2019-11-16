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
  <<: *integration_definition
  script:
    - pytest $test
  stage: test

EOF
done

sed -z 's/^\(.*##BEGIN_INTEGRATION_PYTHON##\n\).*\(\n##END_INTEGRATION_PYTHON##.*\)$/\2/' "$src_dir/.gitlab-ci.yml" >> $tmp

mv $tmp "$src_dir/.gitlab-ci.yml"
