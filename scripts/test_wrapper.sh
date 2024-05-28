#!/bin/sh

set -eu

if [ -z "$1" ] || [ -z "$2" ]; then
  echo "Usage: $0 <test-name> <test-package> [<test-package> ...]"
  echo
  echo "Runs the tests in <test-packages>, redirecting the output to \`tests_results/<test-name>.log\`."
  echo "In addition, it outputs a rudimentary JUnit test report file to \`tests_results/<test-name>.xml\`."
  echo
  echo "Example: $0 p2p_and_base @src/lib_p2p/runtest @src/lib_base/runtest"
  exit 1
fi

name=${1:?}
shift

mkdir -p test_results

all_targets="$*"

# In some cases and especially to reduce the duration of the tests or when we
# find that one global test job introduce hangs or stuck jobs randomly
# we use the parallel capability of Gitlab CI to launch tests in parallel. In
# such a case, the block below calculates the targets to be executed in
# parallel.
# TODO: https://gitlab.com/tezos/tezos/-/issues/6808
if [ -n "${CI_NODE_INDEX:-}" ] &&
  [ -n "${CI_NODE_TOTAL:-}" ] &&
  [ "${DISTRIBUTE_TESTS_TO_PARALLELS:-}" = "true" ]; then

  targets=0
  for target in $all_targets; do
    node=$((targets % CI_NODE_TOTAL))
    # Disable SC1083 as we truly intend the outermost set of braces on
    # the right-hand side to be literal.
    # shellcheck disable=SC1083
    eval group_target_$node=\"\${group_target_${node}-} "$target"\"
    targets=$((targets + 1))
  done
  if [ "$targets" -lt "$CI_NODE_TOTAL" ]; then
    echo "The number of targets is larger than CI_NODE_TOTAL -- consider decreasing 'parallel:'."
    exit 1
  fi
  eval "group_target=\"\$group_target_$((CI_NODE_INDEX - 1))"\"

fi

echo "Running test \"dune build ${COVERAGE_OPTIONS:-} ${group_target:-$all_targets}\" ..."

START=$(date +%s.%N)

exitcode_file=$(mktemp)
{
  echo "0" > "$exitcode_file"
  # If set, COVERAGE_OPTIONS will typically contain "--instrument-with bisect_ppx".
  # We need this to be word split for the arguments to be properly parsed by dune.
  # The same holds for ${group_target:-$all_targets} which may contain multiple targets
  # and must be word split.
  # shellcheck disable=SC2086
  dune build --error-reporting=twice ${COVERAGE_OPTIONS:-} ${group_target:-$all_targets} 2>&1 ||
    echo "$?" > "$exitcode_file"
} | tee "test_results/$name.log"
EXITCODE=$(cat "$exitcode_file")
rm "$exitcode_file"

END=$(date +%s.%N)

dt=$(echo "$END - $START" | bc)
dd=$(echo "$dt/86400" | bc)
dt2=$(echo "$dt-86400*$dd" | bc)
dh=$(echo "$dt2/3600" | bc)
dt3=$(echo "$dt2-3600*$dh" | bc)
dm=$(echo "$dt3/60" | bc)
ds=$(echo "$dt3-60*$dm" | bc)

LC_NUMERIC=C printf "Total runtime: %02d:%02d:%02.4f\n" "$dh" "$dm" "$ds"

if [ "$EXITCODE" -eq 0 ]; then
  echo "Ok"
  nb_fail=0
else
  echo "Error: exited with exitcode $EXITCODE. See full logs in test_results/$name.log"
  nb_fail=1
fi

timestamp=$(date -Is)
hostname=$(hostname)

cat > "test_results/$name.xml" << EOF
<?xml version="1.0" encoding="utf-8"?>
<testsuites>
  <testsuite name="unittest" errors="0" failures="${nb_fail}" skipped="0" tests="1" time="${dt}" timestamp="${timestamp}" hostname="${hostname}">
    <testcase classname="${name}" name="${name}" time="${dt}">
EOF

if [ ! "$EXITCODE" -eq 0 ]; then
  msg="Exited with exitcode $EXITCODE."

  # Add link to log artifact when running in CI
  if [ -n "${CI_SERVER_URL:-}" ]; then
    url=$CI_SERVER_URL/$CI_PROJECT_NAMESPACE/$CI_PROJECT_NAME/-/jobs/$CI_JOB_ID/artifacts/file/test_results/$name.log
    msg="$msg See logs at $url"
  fi

  cat >> "test_results/$name.xml" << EOF
      <failure message="${msg}"/>
EOF
fi

cat >> "test_results/$name.xml" << EOF
    </testcase>
  </testsuite>
</testsuites>
EOF

exit "$EXITCODE"
