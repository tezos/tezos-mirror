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

# [preseed debconf]
echo "debconf debconf/frontend select Noninteractive" | sudo debconf-set-selections

# [add current repository]
sudo curl "$REPOOLD/$DISTRO/octez.asc" | sudo gpg --dearmor -o /etc/apt/keyrings/octez.gpg
repository="deb [signed-by=/etc/apt/keyrings/octez.gpg] $REPOOLD/$DISTRO $RELEASE main"
echo "$repository" | sudo tee /etc/apt/sources.list.d/octez-current.list
apt-get update

# [ preeseed octez ]
# preseed octez-node for debconf. Notice we set purge_warning to yes,
# to make the `autopurge` pass and remove all the node data at the end of this
# script.
cat << EOF > preseed.cfg
octez-node octez-node/configure boolean true
octez-node octez-node/history-mode string rolling
octez-node octez-node/network string ghostnet
octez-node octez-node/purge_warning boolean true
octez-node octez-node/snapshot-import boolean false
octez-node octez-node/snapshot-no-check boolean true
octez-baker octez-baker/liquidity-vote select on
debconf debconf/frontend select Noninteractive
EOF
# preseed the package
sudo debconf-set-selections preseed.cfg
# check the package configuration
sudo debconf-get-selections | grep octez

# [install octez]
apt-get install -y octez-baker

sudo su tezos -c "octez-node config init --rpc-addr 127.0.0.1:8732"
sudo systemctl start octez-node
sudo systemctl start octez-dal-node
sudo systemctl start octez-baker

sudo systemctl status octez-node
version_before_upgrade=$(get_node_version)

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
sudo rm -f /usr/sbin/policy-rc.d
apt-get upgrade -o Dpkg::Options::="--force-confdef" -o Dpkg::Options::="--force-confold" -y octez-node octez-dal-node octez-baker

sudo systemctl start octez-node
sudo systemctl start octez-dal-node
sudo systemctl start octez-baker

systemctl is-active octez-node
systemctl is-active octez-baker
systemctl is-active octez-dal-node

sudo systemctl status octez-node
version_after_upgrade=$(get_node_version)

if [ "$version_before_upgrade" = "$version_after_upgrade" ]; then
  echo "ERROR: Version mismatch between calls:" >&2
  echo "  Before upgrade:  $version_before_upgrade" >&2
  echo "  After upgrade: $version_after_upgrade" >&2
  exit 1
fi

echo "Version before upgrade: $version_before_upgrade"
echo "Version has correctly upgraded: $version_after_upgrade"

if [ "$(echo "$version_before_upgrade" | jq -r '.version.additional_info')" != "release" ]; then
  echo "Unexpected version before upgrade, expected 'release'".
  exit 1
fi

if [ "$(echo "$version_after_upgrade" | jq -r '.version.additional_info')" != "dev" ]; then
  echo "Unexpected version after upgrade, expected 'release'".
  exit 1
fi

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
