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

# Array to store failed files
FAILED_FILES=()

# Set trap to catch errors and store the failing file
trap 'FAILED_FILES+=("$file")' ERR

# The linux distribution for which we are creating the apt repository
# E.g. 'ubuntu' or 'debian'
DISTRIBUTION=${1}
shift
# The release of the linux distribution for which
# we are creating the apt repository
# E.g. '22_04', 'bookworm'
RELEASES=$*

for release in $RELEASES; do # unstable, 22_04, 24_04 ...
  find "packages/${DISTRIBUTION}/${release}" -name '*amd64.deb' |
    parallel -j4 '
      echo "Lintian package {}"
      lintian "{}" --tag-display-limit 0 --verbose --allow-root
    '
done

if [ ${#FAILED_FILES[@]} -ne 0 ]; then
  echo "Lintian check failed for the following files:"
  for file in "${FAILED_FILES[@]}"; do
    echo "- $file"
  done
  exit 1
else
  echo "All clean"
  exit 0
fi
