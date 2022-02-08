#!/bin/sh

set -eu

if [ -z "$1" ] || [ -z "$2" ]; then
    echo "Usage: $0 <test-package> <test-name> [<additional dune options>]"
    echo
    echo "Runs the tests in <test-package>, redirecting the output to \`tests_results/<test-name>.log\`."
    echo "In addition, it outputs a rudimentary JUnit test report file to \`tests_results/<test-name>.xml\`."
    echo
    echo "Example: $0 src/lib_p2p p2p"
    exit 1
fi

nametest=${1:?}
shift
name=${1:?}
shift

mkdir -p test_results

echo "Running test \"dune build $* @$nametest/runtest\" ..."

START=$(date +%s.%N)

dune build "$@" "@$nametest/runtest" > "test_results/$name.log" 2>&1

EXITCODE=$?

END=$(date +%s.%N)

dt=$(echo "$END - $START" | bc)
dd=$(echo "$dt/86400" | bc)
dt2=$(echo "$dt-86400*$dd" | bc)
dh=$(echo "$dt2/3600" | bc)
dt3=$(echo "$dt2-3600*$dh" | bc)
dm=$(echo "$dt3/60" | bc)
ds=$(echo "$dt3-60*$dm" | bc)

LC_NUMERIC=C printf "Total runtime: %02d:%02d:%02.4f\n" "$dh" "$dm" "$ds"

if [ $EXITCODE -eq 0 ]; then
  echo "Ok";
  nb_fail=0
else
  echo "Error";
  echo "Exited with exitcode $EXITCODE"
  cat "test_results/$name.log"
  nb_fail=1
fi

timestamp=$(date -Is)
hostname=$(hostname)

cat > "test_results/$name.xml" <<EOF
<?xml version="1.0" encoding="utf-8"?>
<testsuites>
  <testsuite name="unittest" errors="0" failures="${nb_fail}" skipped="0" tests="1" time="${dt}" timestamp="${timestamp}" hostname="${hostname}">
    <testcase classname="${name}" name="${name}" time="${dt}">
EOF

if [ ! $EXITCODE -eq 0 ]; then
    msg="Exited with exitcode $EXITCODE."

    # Add link to log artifact when running in CI
    if [ -n "$CI_SERVER_URL" ]; then
        url=$CI_SERVER_URL/$CI_PROJECT_NAMESPACE/$CI_PROJECT_NAME/-/jobs/$CI_JOB_ID/artifacts/file/test_results/$name.log
        msg="$msg See logs at $url"
    fi

    cat >> "test_results/$name.xml" <<EOF
      <failure message="${msg}"/>
EOF
fi

cat >> "test_results/$name.xml" <<EOF
    </testcase>
  </testsuite>
</testsuites>
EOF

exit $EXITCODE
