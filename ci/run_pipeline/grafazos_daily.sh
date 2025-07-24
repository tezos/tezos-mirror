#!/bin/sh

SCRIPT_DIR="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
cd "$SCRIPT_DIR"

TZ_SCHEDULE_KIND=grafazos.daily ./scheduled.sh
