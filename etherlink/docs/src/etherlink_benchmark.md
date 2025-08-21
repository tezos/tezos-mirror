# Etherlink Benchmarking Scripts

This repository contains a set of scripts used to benchmark EVM primitive operations on the Etherlink network. The workflow involves generating blocks, replaying them across multiple cores, and visualizing the tick and gas consumption.

Scripts are located in `etherlink/scripts` and their usage can be displayed by running them with no arguments.

## Overview

### 1. `benchmark_primitives.sh`

Runs a full benchmark pipeline:
1. Launches a TEZT scenario that generates benchmark blocks (see [`bin_benchmark_producer/README.md`](../../bin_benchmark_producer/README.md)).
2. Replays these blocks using `split_replay.sh` across multiple processes.
3. Generates a performance graph using `create_graph.sh`.

### 2. `split_replay.sh`

Splits the replay of a block range across multiple processes. Can be used manually to replay mainnet or testnet data.

**Note:** The output directory will contain a `kernel_logs` subdirectory with the relevant CSV files used for graphing.

### 3. `create_graph.sh`

Generates a graph visualizing the results of the replay (tick and gas consumption). This script expects a directory with CSV files, typically located in `kernel_logs` after a `split_replay.sh` run.

## Workflow Summary

For a one-liner benchmark on EVM primitives run `./etherlink/scripts/benchmark_primitives.sh`.

To run steps manually (with real network data), here is an example benchmark session:

1. `./octez-evm-node init config --network mainnet --dont-track-rollup-node --data-dir mainnet_node1`
2. `./octez-evm-node run observer --history rolling:1 --init-from-snapshot --data-dir mainnet_node1`
3. `./etherlink/scripts/split_replay.sh 8509840 8509940 mainnet_node1 4`
4. `./etherlink/scripts/create_graph.sh mainnet_node1/kernel_logs mainnet_node1_graph.png`
