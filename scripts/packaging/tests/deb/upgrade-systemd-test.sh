#!/bin/bash

set -eu

REPO="https://storage.googleapis.com/$GCP_LINUX_PACKAGES_BUCKET/$CI_COMMIT_REF_NAME"
REPOOLD="https://packages.nomadic-labs.com"
DISTRO=$1
RELEASE=$2

# include apt-get function with retry
. scripts/packaging/tests/tests-common.inc.sh

# For the upgrade script in the CI, we do not want debconf to ask questions
export DEBIAN_FRONTEND=noninteractive

apt-get update
apt-get install -y sudo gpg curl apt-utils debconf-utils procps jq

# [add current repository]
sudo curl "$REPOOLD/$DISTRO/octez.asc" | sudo gpg --dearmor -o /etc/apt/keyrings/octez.gpg
repository="deb [signed-by=/etc/apt/keyrings/octez.gpg] $REPOOLD/$DISTRO $RELEASE main"
echo "$repository" | sudo tee /etc/apt/sources.list.d/octez-current.list
apt-get update

# [install octez]
apt-get install -y octez-baker

# Packages to check
mapfile -t packages < <(dpkg -l 'octez-*' | awk '$1 == "ii" && $2 != "octez-zcash-params" { print $2 }')

# Record current versions
declare -A old_versions
while read -r pkg ver; do
  old_versions["$pkg"]="$ver"
done < <(dpkg -l "${packages[@]}" | awk '$1 == "ii" { print $2, $3 }')

# [add next repository]
sudo curl "$REPO/$DISTRO/octez.asc" | sudo gpg --dearmor -o /etc/apt/keyrings/octez-dev.gpg
repository="deb [signed-by=/etc/apt/keyrings/octez-dev.gpg] $REPO/$DISTRO $RELEASE main"
echo "$repository" | sudo tee /etc/apt/sources.list.d/octez-next.list
apt-get update

mapfile -t before_upgrade < <(
  systemctl list-unit-files --type=service |
    awk '/octez/ && $1 !~ /@\.service$/ {print $1 "|" $2}'
)

echo "Listing services before upgrade"
systemctl list-unit-files --type=service | grep "octez"

# [upgrade octez]
apt-get upgrade -y octez-baker

echo "Listing services after upgrade"
systemctl list-unit-files --type=service | grep "octez"

mapfile -t after_upgrade < <(
  systemctl list-unit-files --type=service |
    awk '/octez/ && $1 !~ /@\.service$/ {print $1 "|" $2}'
)

diff_output=$(diff <(printf "%s\n" "${before_upgrade[@]}") <(printf "%s\n" "${after_upgrade[@]}") || true)

if [[ -n "$diff_output" ]]; then
  echo "❌ Services changed after upgrade:"
  echo "$diff_output"
  exit 1
else
  echo "✅ All services unchanged after upgrade."
fi

# Compare versions after upgrade
failed=0
while read -r pkg ver; do
  old_ver="${old_versions["$pkg"]}"
  if [[ "$ver" == "$old_ver" ]]; then
    echo "❌ Package $pkg did not upgrade (still at $ver)"
    failed=1
  else
    echo "✅ Package $pkg upgraded: $old_ver → $ver"
  fi
done < <(dpkg -l "${packages[@]}" | awk '$1 == "ii" { print $2, $3 }')

exit $failed
