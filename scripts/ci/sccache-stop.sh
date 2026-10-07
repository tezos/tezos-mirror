#!/bin/bash

# Stop sccache and print stats in a pre-collapsed section of the job log.
# Also removes the protected-registry@ key written by sccache-start.sh on
# protected refs. after_script runs in a fresh shell, so the path is
# recomputed here rather than read from SCCACHE_GCS_KEY_PATH.

echo -e "\e[0Ksection_start:$(date +%s):sccache_stop[collapsed=true]\r\e[0KStop sccache"
sccache --stop-server || true
rm -f "${TMPDIR:-/tmp}/sccache_protected_sa.json"
echo -e "\e[0Ksection_end:$(date +%s):sccache_stop\r\e[0K"
