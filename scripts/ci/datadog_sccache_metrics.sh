#!/bin/bash
# --------------------------------------------------------------
# Sends sccache hit/miss counts to Datadog as custom metrics.
# Assumes:
#   • `datadog-ci` is installed and in the PATH
#   • `jq` is installed
#   • DATADOG_API_KEY is exported in the environment
# --------------------------------------------------------------

json=$(sccache --show-stats --stats-format json)

printf '%s' "$json" | jq 'if . == null then {} else .stats end | {
    "sccache.cache_hits": ((.cache_hits.counts // {}) | to_entries | map(.value) | add // 0),
    "sccache.cache_misses": ((.cache_misses.counts // {}) | to_entries | map(.value) | add // 0),
    "sccache.compile_requests": (.compile_requests // 0),
    "sccache.requests_not_cacheable": (.requests_not_cacheable // 0),
    "sccache.cache_timeouts": (.cache_timeouts // 0),
    "sccache.cache_read_errors": (.cache_read_errors // 0),
    "sccache.non_cacheable_compilations": (.non_cacheable_compilations // 0),
    "sccache.forced_recaches": (.forced_recaches // 0),
    "sccache.cache_write_errors": (.cache_write_errors // 0),
    "sccache.cache_writes": (.cache_writes // 0),
    "sccache.cache_write_duration_secs": (.cache_write_duration.secs // 0),
    "sccache.cache_read_hit_duration_secs": (.cache_read_hit_duration.secs // 0),
    "sccache.compilations": (.compilations // 0),
    "sccache.compiler_write_duration_secs": (.compiler_write_duration.secs // 0),
    "sccache.compile_fails": (.compile_fails // 0)
}' > sccache_metrics.json

export DATADOG_SITE=datadoghq.eu
if datadog-ci measure --level job --measures-file sccache_metrics.json; then
  echo "Datadog sccache measures sent"
else
  echo "Error while sending sccache measures"
fi
rm sccache_metrics.json
