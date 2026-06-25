#!/bin/bash

set -u

if [ $# -lt 2 ]; then
  cat << EOF
Usage: $0 <DISTRIBUTION> <RELEASE..>


<DISTRIBUTION>: The linux distribution, eg. debian or ubuntu

<RELEASE>: The release of the Linux distribution, e.g. 'jammy', 'focal', 'bookworm'.
This argument can be repeated to build for multiple releases.

This script will fail with exitcode 1 if lintian finds any errors.

EOF
  exit 1
fi

# The linux distribution for which we are creating the apt repository
# E.g. 'ubuntu' or 'debian'
DISTRIBUTION=${1}
shift
# The releases of the linux distribution for which we lint packages.
# E.g. '22_04', 'bookworm'.
RELEASES=$*

# Records whether any release failed lintian. We keep linting the
# remaining releases so the full report is produced before exiting.
failed=0

for release in $RELEASES; do # unstable, 22_04, 24_04 ...
  # 'lintian' exits non-zero when it reports errors and 'parallel'
  # propagates that as a non-zero exit status of the pipeline.
  if ! find "packages/${DISTRIBUTION}/${release}" \( -name '*amd64.deb' -o -name '*_all.deb' \) |
    parallel -j4 '
      echo "Lintian package {}"
      lintian "{}" --tag-display-limit 0 --verbose --allow-root
    '; then
    echo "Lintian reported errors for ${DISTRIBUTION}/${release}"
    failed=1
  fi
done

if [ "$failed" -ne 0 ]; then
  echo "Lintian check failed; see the errors reported above."
  exit 1
fi

echo "All clean"
exit 0
