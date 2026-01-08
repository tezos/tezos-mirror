#!/bin/sh

SCRIPT_DIR="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
cd "$SCRIPT_DIR" || exit

TZ_SCHEDULE_KIND=client-libs.daily ./scheduled.sh
