#!/bin/bash

echo "const total_supply_storage = [" > src/total_supply_storage.js
echo "const total_frozen_stake_storage = [" > src/total_frozen_stake_storage.js
echo "const total_delegated_storage = [" > src/total_delegated_storage.js

rm -rf "/tmp/ai-sim-supply/"
rm -rf "/tmp/ai-sim-frozen/"
rm -rf "/tmp/ai-sim-delegated/"

mkdir -p "/tmp/ai-sim-supply"
mkdir -p "/tmp/ai-sim-frozen"
mkdir -p "/tmp/ai-sim-delegated"

l=$1

batch_size=10

today=$(date +%Y-%m-%d)

# Get the numeric representation of the current day of the week (0 for Sunday, 6 for Saturday)
day_of_week=$(date +%u)

# Calculate how many days ago was the last Wednesday
# Bash considers Sunday as the start of the week (i.e., day 0)
# We need to get to day 3 (Wednesday) of the week
if [ "$day_of_week" -gt 3 ]; then
  # If today is after Wednesday, subtract the difference from today
  days_to_subtract=$((day_of_week - 3))
else
  # If today is before Wednesday, subtract the difference from the previous week
  days_to_subtract=$((day_of_week + 4))
fi

last_wednesday=$(date -d "$today - $days_to_subtract days" +%Y-%m-%d)

endpoint="https://rpc.weeklynet-${last_wednesday}.teztnets.com"

for ((i = 0; i <= l; i += batch_size)); do

  echo "Launching data request for batch starting with cycle: $i"

  for ((n = i; n < $((i + batch_size)) && n <= l; n++)); do
    block=$((128 * (1 + n)))

    (curl -s "$endpoint"/chains/main/blocks/$block/context/total_supply > "/tmp/ai-sim-supply/$n.txt" &)

    (curl -s "$endpoint"/chains/main/blocks/$block/context/total_frozen_stake > "/tmp/ai-sim-frozen/$n.txt" &)

    (
      public_keys=$(curl -s "$endpoint"/chains/main/blocks/$block/context/delegates)
      public_keys=$(echo "$public_keys" | tr -d '[]"')
      public_keys=$(echo "$public_keys" | tr ',' ' ')

      sum=0

      for pkh in $public_keys; do
        balance1=$(curl -s "$endpoint"/chains/main/blocks/$block/context/delegates/"$pkh"/delegated_balance | tr -d '"')

        balance2=$(curl -s "$endpoint"/chains/main/blocks/$block/context/delegates/"$pkh"/full_balance | tr -d '"')

        balance=$((balance1 + balance2))

        sum=$((sum + balance))
      done

      echo "$sum" >> "/tmp/ai-sim-delegated/$n.txt"
    ) &
  done

  for ((n = i; n < $((i + batch_size)) && n <= l; n++)); do
    echo -ne "Looking for data $n\r"

    until [ -s "/tmp/ai-sim-supply/$n.txt" ]; do
      sleep 0.1
    done
    until [ -s "/tmp/ai-sim-frozen/$n.txt" ]; do
      sleep 0.1
    done
    until [ -s "/tmp/ai-sim-delegated/$n.txt" ]; do
      sleep 0.1
    done

    echo -ne "\r"
  done
done

for n in $(seq 0 "$l"); do

  supply=$(cat "/tmp/ai-sim-supply/$n.txt")
  frozen=$(cat "/tmp/ai-sim-frozen/$n.txt")
  delegated=$(cat "/tmp/ai-sim-delegated/$n.txt")

  echo "$supply," >> src/total_supply_storage.js
  echo "$frozen," >> src/total_frozen_stake_storage.js
  echo "$delegated," >> src/total_delegated_storage.js
done

echo "];" >> src/total_supply_storage.js
echo "export {total_supply_storage};" >> src/total_supply_storage.js

echo "];" >> src/total_frozen_stake_storage.js
echo "export {total_frozen_stake_storage};" >> src/total_frozen_stake_storage.js

echo "];" >> src/total_delegated_storage.js
echo "export {total_delegated_storage};" >> src/total_delegated_storage.js

echo

rm -r "/tmp/ai-sim-supply/"
rm -r "/tmp/ai-sim-frozen/"
rm -r "/tmp/ai-sim-delegated/"

echo "Data fetched and stored successfully."
