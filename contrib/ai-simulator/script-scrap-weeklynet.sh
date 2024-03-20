#!/bin/bash

echo "const total_supply_storage = [" > src/total_supply_storage.js
echo "const total_frozen_stake_storage = [" > src/total_frozen_stake_storage.js

rm -rf "/tmp/ai-sim-supply/"
rm -rf "/tmp/ai-sim-frozen/"

mkdir -p "/tmp/ai-sim-supply"
mkdir -p "/tmp/ai-sim-frozen"

l=$1

batch_size=10

for ((i = 0; i <= l; i += batch_size)); do

  echo "Launching data request for batch starting with cycle: $i"

  for ((n = i; n < $((i + batch_size)) && n <= l; n++)); do
    block=$((128 * (1 + n)))

    (curl -s https://rpc.weeklynet-2024-03-13.teztnets.com/chains/main/blocks/$block/context/total_supply > "/tmp/ai-sim-supply/$n.txt" &)

    (curl -s https://rpc.weeklynet-2024-03-13.teztnets.com/chains/main/blocks/$block/context/total_frozen_stake > "/tmp/ai-sim-frozen/$n.txt" &)
  done

  for ((n = i; n < $((i + batch_size)) && n <= l; n++)); do
    echo -ne "Looking for data $n\r"

    until [ -s "/tmp/ai-sim-supply/$n.txt" ]; do
      sleep 0.1
    done
    until [ -s "/tmp/ai-sim-frozen/$n.txt" ]; do
      sleep 0.1
    done

    echo -ne "\r"
  done
done

for n in $(seq 0 "$l"); do

  supply=$(cat "/tmp/ai-sim-supply/$n.txt")
  frozen=$(cat "/tmp/ai-sim-frozen/$n.txt")

  echo "$supply," >> src/total_supply_storage.js
  echo "$frozen," >> src/total_frozen_stake_storage.js
done

echo "];" >> src/total_supply_storage.js
echo "export {total_supply_storage};" >> src/total_supply_storage.js

echo "];" >> src/total_frozen_stake_storage.js
echo "export {total_frozen_stake_storage};" >> src/total_frozen_stake_storage.js

echo

rm -r "/tmp/ai-sim-supply/"
rm -r "/tmp/ai-sim-frozen/"

echo "Data fetched and stored successfully."
