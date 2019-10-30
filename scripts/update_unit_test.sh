#! /bin/sh

set -e

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"

tmp=$(mktemp)

sed -z 's/^\(.*##BEGIN_UNITEST##\n\).*\(\n##END_UNITEST##.*\)$/\1/' "$src_dir/.gitlab-ci.yml" > $tmp

for lib in src/lib* ; do
  if [ -d "$lib/test" ]; then
    cat >> $tmp <<EOF
unit:${lib##src/lib_}:
  <<: *test_definition
  script:
    - dune build @$lib/runtest

EOF
  fi
done

for lib in vendors/* ; do
  if [ -d "$lib/test" ]; then
    cat >> $tmp <<EOF
unit:${lib##vendors/}:
  <<: *test_definition
  script:
    - dune build @$lib/runtest

EOF
  fi
done


sed -z 's/^\(.*##BEGIN_UNITEST##\n\).*\(\n##END_UNITEST##.*\)$/\2/' "$src_dir/.gitlab-ci.yml" >> $tmp

mv $tmp "$src_dir/.gitlab-ci.yml"

