# RPC benchmarking

This benchmark utility aims to benchmark the Requests per second that
a node is able to handle.

This benchmark relies on the
[cassowary](https://github.com/rogerwelin/cassowary) tool.

In the `rps.sh` script, we rely on `cassowary` through a `docker`
image. Feel free to manually update the script if you have a `cassowary`
binary on your machine.

To run the benchmarks you need to:

- set up a node with populated storage (like a recent `mainnet`)
  - For example: `./octez-node snapshot import my_recent_snapshot.rolling --data-dir ./mainnet --no-check`
- run the node with an RPC server listening on localhost:8732 (default port)
  - For example: `./octez-node run --data-dir ./mainnet --connections 0 --rpc-addr 127.0.0.1:8732`
- run `./rps.sh output_directory number_of_runs`

The current version of the benchmark aims to define a simple set of RPCs to test:

- `/health/ready`
- `/chains/main/blocks/header`
- `/chains/main/blocks/head/helpers/baking_rights`
- `/chains/main/blocks/head/helpers/attestation_rights`
The set of RPCs is run twice, first without concurrency (1 thread),
then with a concurrency of 10.

Note that the benchmark of block injection or mempool operation could
be performed as well. However, as the RPC process will always forward
such requests to the node, the benchmark is not valuable. Only the
latency of the forwarding will impact the result (this latency ranges from
80ms on high-end hardware to 320ms on the GCP `n2-standard-4`
recommended hardware).

After the benchmarks are finished, the results can be found in
[output_directory], as several `.json` files, one for each RPC bench.

Here is a small (dirty) script to aggregate the RPS value and compute
a mean value for each kind:

```bash!
#!/bin/bash

for kind in health header baking attestation; do
	for th in 1-th 10-th; do
		echo -n "${kind}-${th}: "
		for i in $(find $1 -type f | grep $kind | grep $th); do
			jq .requests_per_second <"$i"
		done | awk '{sum+=$0; count+=1} END {print sum/count}'
	done
done

```

# Results

Previous values of this benchmark can be found in `results.json`. This
result comes from manual runs and is aimed to be updated manually. The
benchmarks were executed on the Octez recommanded hardware, which is a
GCP `n2-standard-4` virtual machine.
The json file is made of the following fields:
- kind: the RPC server that is tested (local or external)
- rpc: the RPC that is tested
- concurrency: the number of concurrent RPC requests allowed
- rps: the Request Per Second benchmark value

We should consider having this automatized to get track of performance
regression, see https://gitlab.com/tezos/tezos/-/issues/7286
