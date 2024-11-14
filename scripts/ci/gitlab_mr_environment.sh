#!/bin/sh

set -eu

if [ -n "${TRACE:-}" ]; then set -x; fi

usage() {
  cat << EOT
Usage: $0

Sets common variables for scripts running in GitLab CI jobs of merge
request pipelines:

TEZOS_CI_MR_HEAD

  In merge request pipelines, the string 'HEAD'.
  In merged results pipelines, the contents of \$CI_MERGE_REQUEST_SOURCE_BRANCH_SHA.

TEZOS_CI_MR_TARGET

  In merge request pipelines, the string \$ORIGIN/\$CI_MERGE_REQUEST_TARGET_BRANCH_NAME,
  where \$ORIGIN defaults to 'origin'.
  In merged results pipelines, the contents of '\$CI_MERGE_REQUEST_TARGET_BRANCH_SHA'.

EOT
  exit 1
}

if [ "${1:-}" = "--help" ]; then
  usage
fi

ORIGIN=${ORIGIN:-origin}

export TEZOS_CI_MR_HEAD
export TEZOS_CI_MR_TARGET

# CI_MERGE_REQUEST_{SOURCE,TARGET}_BRANCH_SHA are only available in
# merged results pipelines. In merge request pipelines, use
# CI_MERGE_REQUEST_TARGET_BRANCH_NAME instead.
if [ -n "${CI_MERGE_REQUEST_SOURCE_BRANCH_SHA:-}" ] &&
  [ -n "${CI_MERGE_REQUEST_TARGET_BRANCH_SHA:-}" ]; then
  # This is a merged results pipeline.
  TEZOS_CI_MR_HEAD=${CI_MERGE_REQUEST_SOURCE_BRANCH_SHA}
  TEZOS_CI_MR_TARGET=${CI_MERGE_REQUEST_TARGET_BRANCH_SHA}
elif [ -n "${CI_MERGE_REQUEST_TARGET_BRANCH_NAME:-}" ]; then
  # This is a merge request pipeline.
  TEZOS_CI_MR_HEAD=HEAD
  TEZOS_CI_MR_TARGET=${ORIGIN}/${CI_MERGE_REQUEST_TARGET_BRANCH_NAME}

  # Make CI_MERGE_REQUEST_TARGET_BRANCH_NAME available locally -- this
  # is a prerequisite for 'git_merge_base.sh' when its operands are
  # not fully spelled-out hex object names (e.g. when they are branch
  # names instead of commit SHAs).
  git fetch --depth 1 "${ORIGIN}" "${CI_MERGE_REQUEST_TARGET_BRANCH_NAME}"
else
  cat << EOT
None of:
 - CI_MERGE_REQUEST_SOURCE_BRANCH_SHA
 - CI_MERGE_REQUEST_TARGET_BRANCH_SHA
 - CI_MERGE_REQUEST_TARGET_BRANCH_NAME
are set. Cannot set TEZOS_CI_MR_HEAD and TEZOS_CI_MR_TARGET.
EOT
  exit 1
fi
