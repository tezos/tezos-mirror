#!/bin/sh

SCRIPT_DIR="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
cd "$SCRIPT_DIR" || exit

CI_MERGE_REQUEST_TARGET_BRANCH_NAME="origin/master" CHECK_LICENSES_DIFF_BASE="origin/master" TZ_SCHEDULE_KIND=CACHE_REFRESH ./scheduled.sh
