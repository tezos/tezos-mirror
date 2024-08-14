#!/bin/sh

set -eu

if [ -n "${TRACE:-}" ]; then set -x; fi

usage() {
  cat << EOF
Usage: $0

Checks that the current branch's history, up to MERGE BASE does not
contain any commits whose title matches any of the forbidden patterns
in ./forbidden_commit_messages.txt

When running locally, MERGE BASE is the best common ancestor between
HEAD and the supposed target branch master (can be set through
CI_MERGE_REQUEST_TARGET_BRANCH_NAME), obtained through
'scripts/ci/git_merge_base.sh'.

When running in GitLab CI merge request pipeline, the commit history
is checked between MERGE BASE and CI_MERGE_REQUEST_SOURCE_BRANCH_SHA.

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

# Use git_merge_base.sh on merged result pipelines, to ensure we only
# check commit messages of the MR in question.
ORIGIN=${ORIGIN:-origin}

# CI_MERGE_REQUEST_{SOURCE,TARGET}_BRANCH_SHA are only available in
# merged result pipelines. In merge request pipelines, use
# CI_MERGE_REQUEST_TARGET_BRANCH_NAME instead.
if [ -n "${CI_MERGE_REQUEST_SOURCE_BRANCH_SHA:-}" ] &&
  [ -n "${CI_MERGE_REQUEST_TARGET_BRANCH_SHA:-}" ]; then
  head=${CI_MERGE_REQUEST_SOURCE_BRANCH_SHA}
  target=${CI_MERGE_REQUEST_TARGET_BRANCH_SHA}
elif [ -n "${CI_MERGE_REQUEST_TARGET_BRANCH_NAME:-}" ]; then
  head=HEAD
  target=${ORIGIN}/${CI_MERGE_REQUEST_TARGET_BRANCH_NAME}

  # Make CI_MERGE_REQUEST_TARGET_BRANCH_NAME available locally -- this
  # is a prerequisite for 'git_merge_base.sh' when it's operands are
  # not fully spelled out hex object names (e.g. when they are branch
  # names instead of commit SHAs).
  git fetch --depth 1 "${ORIGIN}" "${CI_MERGE_REQUEST_TARGET_BRANCH_NAME}"
else
  car << EOT
None of:
 - CI_MERGE_REQUEST_SOURCE_BRANCH_SHA
 - CI_MERGE_REQUEST_TARGET_BRANCH_SHA
 - CI_MERGE_REQUEST_TARGET_BRANCH_NAME
are set. Cannot retrieve the commit history of this MR.
EOT
  exit 1
fi

merge_base=$("$src_dir"/scripts/ci/git_merge_base.sh "$target" "$head" || {
  # Print on stderr to make visible outside command substitution
  echo "Failed to get merge base, cannot check commit messages." >&2
  exit 1
})
if [ -z "$merge_base" ]; then
  echo "Merge base is empty, cannot check commit messages."
  exit 1
fi

check_history() {
  echo "Check git commit messages in the range ${merge_base}..${head}:"
  echo ""
  git log --format=" - '%s'" "${merge_base}".."${head}"
  echo

  # Use short flags for grep for busybox-compatibility.
  if git log --format="%s" "${merge_base}".."${head}" | grep -qE -f "${pattern_file}" > /dev/null; then
    # Match commits / forbidden patterns cross wise for user
    # friendly output.
    for commit in $(git log --format="%H" "${merge_base}".."${head}"); do
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
