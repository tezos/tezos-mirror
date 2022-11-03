#!/usr/bin/env bash

# This script is included in page node-configuration.rst

# [remove config file if exists]
rm -f tmp/config.json
# [initialize config file]
./octez-node config init --config-file=tmp/config.json --network=sandbox
# [update config file]
./octez-node config update --config-file=tmp/config.json --data-dir=tmp \
  --peer="[::]:10732" --peer="192.168.1.3:9733" \
  --private-mode \
  --rpc-addr="localhost:8733" --net-addr="1.2.3.4" \
  --connections=25 \
  --log-output="octez-node.log" \
  --history-mode=full
# [show config file]
./octez-node config show --data-dir=tmp
