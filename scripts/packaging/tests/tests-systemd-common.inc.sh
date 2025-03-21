#!/bin/sh

set -eu

systemctl start octez-node.service

# give some time to the node to create the identity
# otherwise the octez-client call below will give an error
/usr/share/octez-baker/wait-for-node-up.sh

su tezos -c "octez-client gen keys alice"
key=$(su tezos -c "octez-client show address alice" | grep Hash: | awk '{ print $2 }')
echo "BAKER_KEY=$key" >> /etc/default/octez-baker

systemctl start octez-baker.service

su tezos -c "octez-node config show"

echo "-----------------------"
cat /etc/default/octez-node

echo "-----------------------"
cat /etc/default/octez-baker

systemctl status octez-node.service
echo "Log: /var/log/tezos/node.log"
echo "-----------------------"
tail /var/log/tezos/node.log

systemctl status octez-baker.service

for logfile in /var/log/tezos/baker-P*.log; do
  proto=$(basename "$logfile" | sed -E 's/baker-(P[^.]+).log/\1/')
  systemctl status "octez-baker@$proto.service"
  echo "Log: $logfile"
  echo "-----------------------"
  tail "$logfile"
done

for logfile in /var/log/tezos/accuser-P*.log; do
  proto=$(basename "$logfile" | sed -E 's/accuser-(P[^.]+).log/\1/')
  systemctl status "octez-accuser@$proto.service"
  echo "Log: $logfile"
  echo "-----------------------"
  tail "$logfile"
done
