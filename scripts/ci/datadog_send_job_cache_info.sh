#!/bin/sh

set -e

# Check if the section argument ('before' or 'after') is provided.
# We will use it in the tags sent to Datadog to give infos on the
# cache sizes both at the begining of the job or after its script is
# executed.

if [ $# -eq 0 ]; then
  echo "Usage: $0 <before|after>"
  exit 1
fi

# SECTION will be used as a suffix in the tags sent to Datadog
SECTION="$1"
echo "Section: $SECTION"

# Check if argument is valid
if [ "$1" != "before" ] && [ "$1" != "after" ]; then
  echo "Error: Argument must be 'before' or 'after'"
  echo "Usage: $0 <before|after>"
  exit 1
fi

CARGO_CACHE_DIR=$CI_PROJECT_DIR/.cargo/registry/cache
DUNE_CACHE_ROOT=$CI_PROJECT_DIR/_dune_cache
SCCACHE_DIR=$CI_PROJECT_DIR/_sccache
RUST_DEPS_TARGET_DIR=$CI_PROJECT_DIR/_target/rust_deps
RUSTZCASH_DEPS_TARGET_DIR=$CI_PROJECT_DIR/_target/rustzcash_deps
ETHERLINK_WASM_RUNTIME_TARGET_DIR=$CI_PROJECT_DIR/_target/etherlink_wasm_runtime

CACHE_TAGS=""

# Function to get directory size in bytes and human-readable format
get_cache_size() {
  cache_dir="$1"
  cache_name="$2"

  if [ -d "$cache_dir" ]; then
    # Get size in bytes (more precise for metrics)
    size_bytes=$(du -sb "$cache_dir" 2> /dev/null | cut -f1 || echo "0")
    # Get human-readable size
    size_human=$(du -sh "$cache_dir" 2> /dev/null | cut -f1 || echo "0")

    echo "${cache_name} cache: ${size_human} (${size_bytes} bytes)"
  else
    echo "No $(echo "$cache_name" | tr '[:upper:]' '[:lower:]') cache"
  fi

  # Export environment variables
  export "${cache_name}_SIZE_BYTES=${size_bytes:-0}"
  export "${cache_name}_SIZE_HUMAN=${size_human:-0}"
  # Add tags for datadog
  # shellcheck disable=SC3059
  CACHE_TAGS="$CACHE_TAGS --tags $(echo "$cache_name" | tr '[:upper:]' '[:lower:]')_${SECTION}:${size_bytes:-0}"

}

# Get cache sizes and set environment variables
get_cache_size "$DUNE_CACHE_ROOT" "CACHE_DUNE"
get_cache_size "$CARGO_CACHE_DIR" "CACHE_CARGO"
get_cache_size "$SCCACHE_DIR" "CACHE_SCCACHE"
get_cache_size "$RUST_DEPS_TARGET_DIR" "CACHE_RUST_DEPS"
get_cache_size "$RUSTZCASH_DEPS_TARGET_DIR" "CACHE_RUSTZCASH_DEPS"
get_cache_size "$ETHERLINK_WASM_RUNTIME_TARGET_DIR" "CACHE_ETHERLINK_WASM_RUNTIME"

# Send info to Datadog

if command -v datadog-ci > /dev/null 2>&1; then
  echo "Sending job-level info to Datadog"
  echo "CACHE_TAGS=$CACHE_TAGS"
  # FIXME LATER use "datadog-ci tag --level job --tags-file my_tags.json"
  eval "DATADOG_SITE=datadoghq.eu datadog-ci tag --level job ${CACHE_TAGS}"
else
  echo "'datadog-ci' not installed, no job info sent to Datadog"
fi
