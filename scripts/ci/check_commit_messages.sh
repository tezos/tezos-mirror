#!/bin/sh

set -eu

if [ -n "${TRACE:-}" ]; then set -x; fi

usage() {
    cat <<EOF
Usage: $0

Checks that the current branch's history, up to MERGE BASE does not
contain any commits whose title matches any of the forbidden patterns
in ./forbidden_commit_messages.txt

When running locally, MERGE BASE is the best common ancestor between
HEAD and the supposed target branch master (can be set through
CI_MERGE_REQUEST_TARGET_BRANCH_NAME), obtained through 'git
merge-base'.

When running in GitLab CI merge request pipeline, as detected by the
presence of the CI_MERGE_REQUEST_DIFF_BASE_SHA variable, MERGE BASE is
given by the said same variable. Furthermore, to account for shallow
checkouts, we repeatedly deepen the fetch (up to 300 commits) should
CI_MERGE_REQUEST_DIFF_BASE_SHA not be present.

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
pattern_file="${script_dir}forbidden_commit_messages.txt"

target_branch=${CI_MERGE_REQUEST_TARGET_BRANCH_NAME:-master}
if [ -n "${CI_MERGE_REQUEST_DIFF_BASE_SHA:-}" ]; then
    merge_base=${CI_MERGE_REQUEST_DIFF_BASE_SHA}
    fetches=0
    while [ $fetches -lt 3 ] && ! git cat-file -t "${merge_base}" > /dev/null 2>&1; do
        git fetch -q --deepen=100 origin "${CI_MERGE_REQUEST_TARGET_BRANCH_NAME}"
        fetches=$((fetches+1))
    done
    if ! git cat-file -t "${merge_base}" > /dev/null 2>&1; then
        echo "Could not retrieve merge base ${merge_base} after ${fetches} of 100 commits, giving up."
        exit 1
    fi
else
    merge_base=$(git merge-base "${target_branch}" HEAD)
fi

check_history() {
    # Use short flags for grep for busybox-compatibility.
    if git log --format="%s" "${merge_base}"..HEAD | grep -qE -f "${pattern_file}" > /dev/null; then
        # Match commits / forbidden patterns cross wise for user
        # friendly output.
        for commit in $(git log --format="%H" "${merge_base}"..HEAD); do
            commit_title=$(git show -s --format=%B "${commit}" | head -n1)
            while read -r pattern; do
                if echo  "${commit_title}" | grep -qE "$pattern"; then
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
