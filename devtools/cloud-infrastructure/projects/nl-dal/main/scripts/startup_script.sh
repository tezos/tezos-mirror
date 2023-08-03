#!/bin/bash

# Force re-apply

# Parameters

# Docker deployment script
##########################

cat > /tmp/start.sh <<EOF
apt update
apt install libgmp-dev curl libev-dev libhidapi-dev python3 openssh-server -y
mkdir -p /root/.ssh
mkdir -p /run/sshd

/usr/sbin/sshd -D -p 30000 -e
EOF

docker run \
  -p 30000-30999:30000-30999 \
  --name tezos debian \
  bash -c "$(cat /tmp/start.sh)"
