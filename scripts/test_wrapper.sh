#!/bin/bash

nametest=${1:?}
name=${2:?}

mkdir -p test_results

echo "Running test \"dune build @$nametest/runtest\" ..."

START=$(date +%s.%N)

for i in $(seq 10)
do
    dune build "@$nametest/runtest" -f --no-buffer |& tee -a "test_results/$name.log"
done

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
