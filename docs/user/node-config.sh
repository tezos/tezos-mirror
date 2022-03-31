#!/usr/bin/env bash

# This script is included in page node-configuration.rst

# [remove config file if exists]
rm -f tmp/config.json
# [initialize config file]
../tezos-node config --config-file=tmp/config.json --network=sandbox init
# [update config file]
../tezos-node config --config-file=tmp/config.json --data-dir=tmp \
  --expected-pow=24.5 --peer="[::]:10732" --peer="192.168.1.3:9733" \
  --private-mode --disable-mempool \
  --rpc-addr="localhost:8733" --net-addr="1.2.3.4" \
  --connections=100 --max-download-speed=1024 --max-upload-speed=1024 \
  --cors-origin="*" --cors-header="Content-Type" \
  --rpc-tls="tezos-node.crt,tezos-node.key" \
  --log-output="tezos-node.log" \
  --synchronisation-threshold=5 --sync-latency=120 --history-mode=full \
  update
# [show config file]
../tezos-node config --data-dir=tmp show
