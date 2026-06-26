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

# Debian trixie's lintian (2.122) flags systemd units shipped under
# /lib/systemd/system as 'aliased-location' on usr-merged systems. We lint
# with the trixie image in every pipeline, so this affects all Debian
# packages, not only bookworm: whether a package ships under /lib or
# /usr/lib depends on the debhelper version of the release it was built on
# (we pin no debhelper-compat), so trixie packages can trip it too. The
# proper fix is to ship the units under /usr/lib/systemd/system; until the
# packaging is updated we suppress the tag to keep the lint green.
# Debian only: Ubuntu's older lintian (2.117) does not know the tag and
# errors out on an unknown --suppress-tags argument.
suppress=""
if [ "$DISTRIBUTION" = "debian" ]; then
  suppress="--suppress-tags aliased-location"
fi

for release in $RELEASES; do # unstable, 22_04, 24_04 ...
  # 'lintian' exits non-zero when it reports errors and 'parallel'
  # propagates that as a non-zero exit status of the pipeline.
  if ! find "packages/${DISTRIBUTION}/${release}" \( -name '*amd64.deb' -o -name '*_all.deb' \) |
    parallel -j4 '
      echo "Lintian package {}"
      lintian "{}" --tag-display-limit 0 --verbose --allow-root '"$suppress"'
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
