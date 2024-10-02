#!/bin/sh

set -u

if [ -n "${TRACE:-}" ]; then set -x; fi

usage() {
  cat << EOT
Usage: $0 [ref1 [ref2]]

Find the merge base (the best ancestor) between two git refs.

This script works like 'git merge-base' but in the context of shallow checkouts as
is typically the case in tezos/tezos' CI:

https://docs.gitlab.com/ee/user/project/repository/monorepos/#shallow-cloning

The history of the two git refs are deepened in batches of $N commits
(defaults to 100) from $ORIGIN (defaults to 'origin'), until a common
ancestor is found.

<ref2>, if unspecified, defaults to HEAD. If both <ref1> and <ref2>
are unspecified, they default to respectively 'ORIGIN/master' and 'HEAD'.

As a consequence of running this script, the ancestors of <ref1> and
<ref2> will be available, and if the script is successful, up to the
merge base.

If no common ancestor is found after MAX_FETCHES (defaults to 10)
fetches, the script exits with exit code 1 and an error on standard
error.

'git merge-base' is empty and exit code is 1 if no ancestor is found.

Note: this script should be used instead of the GitLab CI predefined variable
CI_MERGE_REQUEST_DIFF_BASE_SHA, which is unreliable due to
https://gitlab.com/gitlab-org/gitlab/-/issues/442031.
EOT
  exit 1
}

if [ "${1:-}" = "--help" ]; then
  usage
fi

ORIGIN=${ORIGIN:-origin}
N=${N:-100}
MAX_FETCHES=${N:-10}

ref1=${1:-${ORIGIN}/master}
ref2=${2:-HEAD}

# parse the refs to sha's such that they can be passed to both 'git
# fetch ORIGIN ref_sha' and 'git merge-base ref_sha' (for instance,
# 'git fetch origin origin/master' is not accepted, but it is if
# origin/master is first resolved to a sha).
ref1_sha=$(git rev-parse "$ref1")
ref2_sha=$(git rev-parse "$ref2")

fetches=0
while [ $fetches -lt "$MAX_FETCHES" ] && ! git merge-base "$ref1_sha" "$ref2_sha" > /dev/null 2>&1; do
  fetches=$((fetches + 1))
  if ! git fetch --quiet --deepen="$N" "$ORIGIN" "${ref1_sha}" "${ref2_sha}"; then
    echo "Couldn't fetch $N commits from ${ref1} and ${ref2} at the ${fetches}:th fetch." >&2
    # exit with status > 0 but <> 1 to distinguish from 'git merge-base' status codes.
    exit 2
  fi
done

git merge-base "$ref1_sha" "$ref2_sha"
