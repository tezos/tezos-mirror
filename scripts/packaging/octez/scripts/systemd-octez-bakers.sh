#!/bin/sh
#

# Check if the argument is valid (start or stop)
if [ "$1" != "start" ] && [ "$1" != "stop" ]; then
  echo "Usage: $0 {start|stop}"
  exit 1
fi

# Loop through each file matching the pattern
# NB: we avoid selecting `-alpha` or `-next` bakers
for file in /usr/bin/octez-baker-P*; do
  file=$(basename "$file")
  proto=$(echo "$file" | sed 's/^octez-baker-//')

  # Enable or start/stop the systemd service based on the argument
  if [ "$1" = "start" ]; then
    if grep -q "\-\-dal-node" /etc/default/octez-baker; then
      systemctl enable octez-dal-node
      systemctl start octez-dal-node
    fi
    systemctl enable "octez-baker@$proto"
    systemctl enable "octez-accuser@$proto"
    systemctl start "octez-baker@$proto"
    systemctl start "octez-accuser@$proto"

  elif [ "$1" = "stop" ]; then
    systemctl stop "octez-baker@$proto"
    systemctl stop "octez-accuser@$proto"
    if grep -q "\-\-dal-node" /etc/default/octez-baker; then
      systemctl stop octez-dal-node
    fi
  fi
done
