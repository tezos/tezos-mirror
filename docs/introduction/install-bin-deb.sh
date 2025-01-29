#!/bin/sh

distribution=$1
release=$2

# If it's a protected branch the value of $bucket will
# be set accordingly but the CI.
bucket="$GCP_LINUX_PACKAGES_BUCKET"
protocol=$(head -1 script-inputs/active_protocol_versions_without_number)

# This logic must be kept in sync with the script in
# ./scripts/ci/create_debian_repo.sh

# The prefix used for these packages in the repository. E.g. 'next'
if [ -n "$PREFIX" ]; then
  PREFIX=${PREFIX}/
else
  PREFIX=
fi

# Function to run apt-get with retries and exponential backoff for a specific error
apt_get_with_retries() {
  # Maximum retries
  max_retries=5
  # Initial delay in seconds
  delay=1

  # Loop for retries
  for i in $(seq 1 "$max_retries"); do
    echo "Attempt $i of $max_retries..."

    # Run apt-get and capture the output and exit status
    output=$(apt-get "$@" 2>&1)
    status=$?

    # Check if apt-get succeeded
    if [ $status -eq 0 ]; then
      return 0
    fi

    # Check for the specific error message pattern
    echo "$output" | grep -q "File has unexpected size" && echo "$output" | grep -q "Mirror sync in progress"

    #shellcheck disable=SC2181
    if [ $? -eq 0 ]; then
      # If the specific error occurs, retry with exponential backoff
      echo "Error detected: Mirror sync in progress. Retrying in $delay seconds..."
      echo "$output"
      sleep "$delay"
      # Exponential backoff (doubling the delay)
      # 1 + 3 + 9 + 27 + 81 = 31, so we wait 121s maximum
      # in total with max_retries = 5.
      delay=$((delay * 3))
    else
      # If the error is not the one we are looking for, exit with failure
      echo "apt-get failed with an unexpected error. Exiting."
      echo "$output"
      exit 1
    fi
  done

  echo "apt-get failed after $max_retries attempts."
  exit 1
}

# Replace apt-get with the new function
alias apt-get="apt_get_with_retries"

. scripts/ci/octez-packages-version.sh

case "$RELEASETYPE" in
ReleaseCandidate | TestReleaseCandidate)
  distribution="${PREFIX}RC/${distribution}"
  ;;
Release | TestRelease)
  # use $distribution as it is
  : nop
  ;;
Master)
  distribution="${PREFIX}master/${distribution}"
  ;;
SoftRelease)
  distribution="${PREFIX}${CI_COMMIT_TAG}/${distribution}"
  ;;
TestBranch)
  distribution="${PREFIX}${CI_COMMIT_REF_NAME}/${distribution}"
  ;;
*)
  echo "Cannot test packages on this branch"
  exit 1
  ;;
esac

# For the upgrade script in the CI, we do not want debconf to ask questions
export DEBIAN_FRONTEND=noninteractive

set -e
set -x

# these two packages are needed here, but they don't need to appear in the doc
apt-get update
apt-get install -y debconf-utils apt-utils procps

if [ "$RELEASETYPE" = "Master" ]; then
  # [add repository]
  apt-get install -y sudo gpg curl

  curl -s "https://packages.nomadic-labs.com/$distribution/octez.asc" |
    sudo gpg --dearmor -o /etc/apt/keyrings/octez.gpg
  echo "deb [signed-by=/etc/apt/keyrings/octez.gpg] https://packages.nomadic-labs.com/$distribution $release main" |
    sudo tee /etc/apt/sources.list.d/octez.list
  apt-get update
  # [end add repository]
else
  apt-get install -y sudo gpg curl
  curl -s "https://$bucket.storage.googleapis.com/$distribution/octez.asc" |
    sudo gpg --dearmor -o /etc/apt/keyrings/octez.gpg
  REPO="deb [signed-by=/etc/apt/keyrings/octez.gpg] https://$bucket.storage.googleapis.com/$distribution $release main"
  echo "$REPO" | sudo tee /etc/apt/sources.list.d/octez.list
  apt-get update
fi

# [ preeseed octez ]
if [ -n "$PREFIX" ]; then
  # preseed octez-node for debconf. Notice we set purge_warning to yes,
  # to make the `autopurge` pass and remove all the node data at the end of this
  # script.
  cat << EOF > preseed.cfg
octez-node octez-node/configure boolean true
octez-node octez-node/history-mode string full
octez-node octez-node/network string mainnet
octez-node octez-node/purge_warning boolean true
octez-node octez-node/snapshot-import boolean false
octez-node octez-node/snapshot-no-check boolean true
debconf debconf/frontend select Noninteractive
EOF
  # preseed the package
  sudo debconf-set-selections preseed.cfg

  # check the package configuration
  sudo debconf-get-selections | grep octez
fi

# [install tezos]
apt-get install -y octez-client
apt-get install -y octez-node

# If systemd is available we test the service scripts
if [ "$(ps --no-headers -o comm 1)" = "systemd" ]; then
  systemctl enable octez-node
  systemctl start octez-node

  sleep 5
  systemctl status octez-node

  journalctl -xeu octez-node.service

fi

apt-get install -y octez-baker
apt-get install -y octez-dal-node

# [install octez additional packages]
if [ -n "$PREFIX" ]; then
  # [install octez NEXT packages]
  apt-get install -y octez-smart-rollup-node
else
  # [install octez current packages]
  apt-get install -y octez-smartrollup
fi

# [test executables]
octez-client --version
octez-node --version
"octez-baker-$protocol" --version
"octez-accuser-$protocol" --version

# If systemd is available we test the service scripts
if [ "$(ps --no-headers -o comm 1)" = "systemd" ]; then
  systemctl stop octez-node
fi

# [test autopurge]
apt-get autopurge -y octez-node octez-client octez-baker octez-dal-node

# [check autopurge]
set +x

if [ -n "$PREFIX" ]; then
  # check the package configuration
  sudo debconf-get-selections | if grep -q octez; then
    echo "Leftovers in debconf db"
    sudo debconf-get-selections | grep -q octez
    exit 1
  fi

  printf "Check if the user tezos was removed:"
  if id tezos > /dev/null 2>&1; then
    echo "Tezos user not correctly removed"
    id tezos
    exit 1
  else
    echo "Ok."
  fi

  printf "Check if the datadir was correctly removed:"
  if [ -e /var/tezos ]; then
    echo "Datadir /var/tezos not correctly removed"
    ls -la /var/tezos
  else
    echo "Ok."
  fi
fi
