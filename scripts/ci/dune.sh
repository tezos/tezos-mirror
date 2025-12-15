#!/bin/bash

set -e

# This script wraps dune to compute and report cache hit ratio in CI when DUNE_CACHE_INFO=true.
# It analyzes dune's cache debug output to show how many files were restored vs rebuilt.

# Set DUNE_CACHE_INFO to "true" if the CI pipeline is scheduled or triggered via api
if [ "$CI_PIPELINE_SOURCE" = "schedule" ] || [ "$CI_PIPELINE_SOURCE" = "api" ]; then
  DUNE_CACHE_INFO="true"
fi

# Check if DUNE_CACHE_INFO is set to "true"
# If not, just run dune build normally without cache reporting
if [ "${DUNE_CACHE_INFO}" != "true" ]; then
  echo "dune cache stats (via [dune.sh]) disabled"
  dune "$@"
  exit $?
fi

# If DUNE_CACHE_INFO is "true", run with cache hit ratio reporting
CACHE_DEBUG_LOG=dune_with_cache_activity.log

echo "💬"
echo "💬   Starting Dune build with cache hit ratio info..."
echo "💬"

# Build with cache debug output enabled, we "tee stderr" to file AND filter it before
# showing to console, this hides cache verbose cache debug lines while showing build
# progress live.

dune "$@" --debug-cache=shared,workspace-local,fs \
  2> >(tee "${CACHE_DEBUG_LOG}" | grep -vE "Workspace-local cache|Shared cache" >&2)

DUNE_BUILD_EXIT_CODE=${PIPESTATUS[0]}

if [ -f "${CACHE_DEBUG_LOG}" ]; then

  # Count workspace cache misses
  WORKSPACE_MISS=$(grep -c "Workspace-local cache miss:" "${CACHE_DEBUG_LOG}" 2> /dev/null || echo "0")

  # Count shared and fs cache misses (Note: filesystem cache are REAL misses)
  # Patterns: "Shared cache miss" or "not found in cache"
  FS_MISS=$(grep -cE "Shared cache miss|not found in cache" "${CACHE_DEBUG_LOG}" 2> /dev/null || echo "0")

  # Clean values and keep only digits and put 0 if empty
  WORKSPACE_MISS=$(echo "${WORKSPACE_MISS}" | tr -cd '0-9')
  FS_MISS=$(echo "${FS_MISS}" | tr -cd '0-9')

  WORKSPACE_MISS=${WORKSPACE_MISS:-0}
  FS_MISS=${FS_MISS:-0}

  # Remove leading zeros and handle empty string after removal
  WORKSPACE_MISS=${WORKSPACE_MISS##+(0)}
  FS_MISS=${FS_MISS##+(0)}

  WORKSPACE_MISS=${WORKSPACE_MISS:-0}
  FS_MISS=${FS_MISS:-0}

  # Cache hits = workspace misses - fs misses
  # Note: If workspace miss but NO fs miss, it means file was restored from fs cache
  CACHE_HITS=$((WORKSPACE_MISS - FS_MISS))

  TOTAL_OPS=$((CACHE_HITS + FS_MISS))

  if [ ${TOTAL_OPS} -gt 0 ]; then
    HIT_RATIO=$(awk -v h="${CACHE_HITS}" -v t="${TOTAL_OPS}" 'BEGIN { printf "%f", (h / t) }')
  else
    HIT_RATIO="0."
  fi
else
  CACHE_HITS=0
  FS_MISS=0
  TOTAL_OPS=0
  HIT_RATIO="0."
fi

echo
echo "---------------------- DUNE CACHE REPORT ----------------------"
echo "Cache hit ratio analysis:"
printf "  Files restored from cache:    %s\n" "${CACHE_HITS}"
printf "  Files built (cache miss):     %s\n" "${FS_MISS}"
printf "  Total operations:             %s\n" "${TOTAL_OPS}"
printf "  Cache hit ratio:              %s\n" "${HIT_RATIO}"
echo "---------------------------------------------------------------"
echo

# Send dune cache metrics to Datadog
if command -v datadog-ci > /dev/null 2>&1; then
  echo "Sending dune cache metrics to Datadog"

  # Build measures
  MEASURE_HITS="--measures dune_cache_hits:${CACHE_HITS}"
  MEASURE_MISSES="--measures dune_cache_misses:${FS_MISS}"
  MEASURE_TOTAL="--measures dune_cache_total_ops:${TOTAL_OPS}"
  MEASURE_RATIO="--measures dune_cache_hit_ratio:${HIT_RATIO}"

  # Concatenate all measures
  DUNE_CACHE_MEASURES="${MEASURE_HITS} ${MEASURE_MISSES} ${MEASURE_TOTAL} ${MEASURE_RATIO}"

  echo "DUNE_CACHE_MEASURES=$DUNE_CACHE_MEASURES"
  eval "DATADOG_SITE=datadoghq.eu datadog-ci measure --level job ${DUNE_CACHE_MEASURES}"

else
  echo "'datadog-ci' not installed. no dune cache metrics sent to Datadog"
fi

# Exit with the same code as dune build
exit "$DUNE_BUILD_EXIT_CODE"
