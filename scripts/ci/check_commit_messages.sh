#!/bin/sh

set -eu

if [ -n "${TRACE:-}" ]; then set -x; fi

usage() {
  cat << EOF
Usage: $0

Checks that the current branch's history, up to MERGE BASE does not
contain any commits whose title matches any of the forbidden patterns
in ./forbidden_commit_messages.txt

MERGE BASE is obtained by passing the variables
TEZOS_CI_MR_{TARGET,HEAD} obtained by
scripts/ci/gitlab_mr_environment.sh to scripts/ci/git_merge_base.sh.

This script exits with error code 0 if no forbidden commit subjects
are found. If a forbidden commit subject is found in a merge requests
pipeline that is not part of a merge train, it exits with error code
65. Otherwise, it exits with error code 1.
EOF

  exit 1
}

if [ "${1:-}" = "--help" ]; then
  usage
fi

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$(dirname "$script_dir")")"
pattern_file="${script_dir}forbidden_commit_messages.txt"

# shellcheck source=scripts/ci/gitlab_mr_environment.sh
. "$src_dir"/scripts/ci/gitlab_mr_environment.sh

merge_base=$("$src_dir"/scripts/ci/git_merge_base.sh "$TEZOS_CI_MR_TARGET" "$TEZOS_CI_MR_HEAD" || {
  # Print on stderr to make visible outside command substitution
  echo "Failed to get merge base, cannot check commit messages." >&2
  exit 1
})
if [ -z "$merge_base" ]; then
  echo "Merge base is empty, cannot check commit messages."
  exit 1
fi

check_history() {
  echo "Check git commit messages in the range ${merge_base}..${TEZOS_CI_MR_HEAD}:"
  echo ""
  git log --format=" - '%s'" "${merge_base}".."${TEZOS_CI_MR_HEAD}"
  echo

  # Use short flags for grep for busybox-compatibility.
  if git log --format="%s" "${merge_base}".."${TEZOS_CI_MR_HEAD}" | grep -qE -f "${pattern_file}" > /dev/null; then
    # Match commits / forbidden patterns cross wise for user
    # friendly output.
    for commit in $(git log --format="%H" "${merge_base}".."${TEZOS_CI_MR_HEAD}"); do
      commit_title=$(git show -s --format=%B "${commit}" | head -n1)
      while read -r pattern; do
        if echo "${commit_title}" | grep -qE "$pattern"; then
          echo "/!\ Commit ${commit} with title:"
          echo "> '${commit_title}'"
          echo "matches blacklist pattern '${pattern}', please revise."
        fi
      done < "${pattern_file}"
    done

    return 1
  else
    echo "Commit history contains no forbidden words."
    return 0
  fi
}

# If the message check fails inside a merge request pipeline that is
# not part of a merge train, exit with code 65 which is an allowed
# failure. If this check fails inside a merge train pipeline, exit with code 1,
# failing the pipeline.
source=${CI_PIPELINE_SOURCE:-}
event_type=${CI_MERGE_REQUEST_EVENT_TYPE:-}

# If running locally:
if [ -z "${source}" ]; then
  check_history
# If running inside GitLab CI for a merge request:
elif [ "${source}" = "merge_request_event" ]; then
  # Unless if running in a merge train, invalid commit titles are
  # not fatal.
  check_history ||
    if [ "${event_type}" != "train" ]; then
      # Non-fatal failures
      exit 65
    else
      exit 1
    fi
fi
