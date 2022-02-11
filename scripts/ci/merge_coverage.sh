#!/bin/sh

COVERAGE_MERGED=$(echo $CI_JOB_NAME | tr --squeeze-repeats '[\/_ @[]+' '-')
# If the merge fails, we upload a corrupted coverage file for the test job
bisect-ppx-report merge --coverage-path "$BISECT_FILE" "$COVERAGE_MERGED".coverage \
      || { COVERAGE_MERGED="$COVERAGE_MERGED".corrupted; echo "" > "$COVERAGE_MERGED".coverage; }
# If there is nothing to remove we still want to continue and move the corrupted file
rm "$BISECT_FILE*".coverage || true
mv "$COVERAGE_MERGED".coverage "$BISECT_FILE"
