#!/bin/bash

#
# SPDX-License-Identifier: MIT
# Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>
#

#  One might run this to purge caches
#echo 3 > /proc/sys/vm/drop_caches

if [ "$#" -ne 2 ]; then
  echo "usage: ./rps [output_folder_name] [number_of_runs]"
  echo "[output_folder_name]: location of the .json output files
        containing the results."
  echo "[number_of_runs]: additional runs to reduce the bench
         noise/variance."
  exit
fi

cassowary="docker run --network=host -w $(pwd) -v $(pwd):$(pwd) rogerw/cassowary:v0.14.1"

timeout=15
concurrency=1
header="Accept: application/json"

for id in $(seq 1 1 "$2"); do

  metrics_output="$1_$id"

  mkdir -p "$metrics_output"

  for concurrency in 1 10; do
    echo "------------------------------------------------------------------------------------"

    echo "Concurrency=$concurrency"

    echo "Health (run $id/$2):"
    $cassowary -t "$timeout" -u http://127.0.0.1:8732/health/ready --json-metrics --json-metrics-file="$metrics_output/health-$concurrency-th.json" -c "$concurrency" -n 100000 -H "$header"

    echo "Header (run $id/$2):"
    $cassowary -t "$timeout" -u http://127.0.0.1:8732/chains/main/blocks/header --json-metrics --json-metrics-file="$metrics_output/header-$concurrency-th.json" -c "$concurrency" -n 100000 -H "$header"

    echo "Baking rights (run $id/$2):"
    $cassowary -t "$timeout" -u http://127.0.0.1:8732/chains/main/blocks/head/helpers/baking_rights --json-metrics --json-metrics-file="$metrics_output/baking-$concurrency-th.json" -c "$concurrency" -n 10000 -H "$header"

    echo "Attestation rights (run $id/$2):"
    $cassowary -t "$timeout" -u http://127.0.0.1:8732/chains/main/blocks/head/helpers/attestation_rights --json-metrics --json-metrics-file="$metrics_output/attestation-$concurrency-th.json" -c "$concurrency" -n 1000 -H "$header"

  done

  echo "-------------"
  echo -n "RPS of run $id: "
  cat "$metrics_output"/*.json | jq .requests_per_second | awk '{sum+=$0;count+=1} END{print sum/count}'

done
