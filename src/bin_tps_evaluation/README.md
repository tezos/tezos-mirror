# Tezos TPS evaluation tool

The TPS evaluation tool is a CLI application that allows its users to run
different kinds of TPS benchmarks and estimations. It also has a command for
extraction of historical data that can be used in order to make the
benchmarks more credible by using a realistic stream of transactions.

## Building

The CLI app is not built by default. This is done for the following reasons:

* It depends on PostgreSQL and requires it to be installed at compile time.
  We do not want to impose this dependency on users who just want to build
  the core executables (client, baker, node, etc.) by running `make`.

* It patches the protocol in order to lift some hard-coded limits so that we
  can find out the real technical ceiling for the TPS, not the one imposed
  by protocol constants and parameters. Patching is a temporary solution. We
  chose it because the alternative is introducing new protocol parameters,
  which is a troublesome and slow process. There is also [an existing
  issue][unify-protocol-limits] about unifying protocol limits which will
  likely result in a refactoring. Once that refactoring is carried out
  patching will likely be unnecessary.

In order to build the CLI app one should run

```
$ make build-tps-deps
$ make build-tps
```

from the root of the repository.

## Estimating the average block

The CLI tool can be used to extract information about different kinds of
transactions that constitute the average block by using [the Tezos
indexer](https://gitlab.com/nomadic-labs/tezos-indexer) snapshots. Please
refer to the Tezos indexer's [SQL
schema](https://gitlab.com/nomadic-labs/tezos-indexer/-/tree/master/src/db-schema)
for more information about the stored data.

In order to estimate contents of the average block you will need a running
PostgreSQL instance with a snapshot (either full or partial) from the Tezos
indexer.

[Here](https://z.lamini.ca/v970.mainnet.1729000.tar.bz2) is a snapshot containing blocks 0 to 1.729M. And
[here](https://z.lamini.ca/recent_blocks_mainnet.2021-Oct-5.sql.bz2) is a partial snapshot between
2021-09-03 and 2021-10-05.

Once you have a snapshot, you can load it into the PostgreSQL instance:

```bash
bunzip2 -c v970.mainnet.1729000.tar.bz2 | psql -h <pg-instance> -U <pg-user> -a <your-db-name>
```

This might take a while depending on the hardware you use.

Once the above command is complete you'll be able to use the
`estimate-average-block` sub-command to query the database, e.g.:

```bash
./tezos-tps-evaluation estimate-average-block -c <connection-string> --start 2021-09-01 --end 2021-09-30
{
  "regular": 2989498,
  "origination": 5498,
  "contract": {
    "KT1RJ6PbjHpwc3M5rw5s2Nbmefwbuwbdxton": 1772827,
    "KT1HbQepzV1nVGg8QVznG7z4RcHseD5kwqBn": 1165878
  }
}
```

## Estimating TPS using gas

It is possible to get an estimation of the maximal possible TPS by using the
protocol parameters.

```
./tezos-tps-evaluation gas-tps --average-block=src/bin_tps_evaluation/2021-09-01-to-2021-09-30.json
Using the default average block description
Average transaction cost: 1521
Gas TPS: 114
[21:18:07.394] [SUCCESS] (1/1) tezos_tps_gas
```

This estimation is obtained by dividing the hard gas limit per block by the
average transaction cost.

The `gas-tps` command will also register its result in a database if
`tezt_config.json` exists (see [these instructions][long-tezts-locally]).

## Running the TPS benchmark

The TPS benchmark is run with the `benchmark-tps` command. It spawns a
network comprising a node, a baker, and a client. The network will use the
same constants and parameters as the mainnet. It will wait till level 3 is
reached and after that it will run the stress test client command.

The TPS parameter that is passed to the stress test command (we call this
**target TPS of injection**) depends on the presence of the
`--lift-protocol-limits` flag:

* If `--lift-protocol-limits` is passed, an arbitrary big value is passed,
  so that stress test will inject as fast as possible.
* If no `--lift-protocol-limits` is passed, TPS computed from gas (see the
  previous section for details) will be used as the target TPS of injection.

By default 10 blocks will be produced, but this can be changed by supplying
the `--blocks-total` command line option. The total number of applied
operations will be divided by the total time spent producing the blocks and
the resulting value will be presented as the **empirical TPS**. The
benchmark is also capable of calculating **de facto TPS of injection** which
is useful in judging the results.

It is possible to pass command line arguments to the underlying Tezt
framework. To accomplish that pass arguments after the double dash
delimiter. For example:

```bash
./tezos-tps-evaluation benchmark-tps -- --help
```

will print command line options that the Tezt framework accepts. `-- -v` can
be helpful for obtaining a detailed log.

Consult `./tezos-tps-evaluation benchmark-tps --help` to see all accepted
command line options.

The `benchmark-tps` command will also register its result in a database if
`tezt_config.json` exists (see [these instructions][long-tezts-locally]).

### Making sense of the results

The goal of the TPS benchmark is to give a high-level estimate of the TPS
value that the system (simple pair node/baker) is capable of. It can be used
to catch TPS regressions, but not to find where exactly the bottlenecks are.

The empirical TPS is significantly affected by the hardware on which the
benchmark is run and other factors, such as the amount of logging that is
performed. For example, passing `-- -v` is likely to result in much lower
empirical TPS values.

The empirical TPS should normally be very close to the de facto TPS of
injection. If it isn't, then it means that the system cannot keep up with
the injection rate of the stress test, i.e. the bottleneck is in the system.
Otherwise the bottleneck is in the stress test or, in the case of lifted
limits, possibly in the hardware that is used.

Right now, the empirical TPS value is an intermediate result. We need to
address the following problems before we can consider the result
trustworthy:

* Over 60% of transactions on Mainnet are smart contract calls. We need to
  add support for more smart contract calls to the benchmark.

* We should enable the mempool pre-check to be closer to the behavior of the
  system in the real world.

* There is usually a small gap between the target TPS of injection and the
  de facto TPS of injection (see [issue 2414][issue-2414]). It is worth
  investigating this and trying to close the gap, however only after support
  for the mempool pre-check is implemented, because that work will likely
  result in a significant change in how stress test works and might fix this
  issue for us.

### Example usage

```
./tezos-tps-evaluation benchmark-tps --average-block=src/bin_tps_evaluation/2021-09-01-to-2021-09-30.json
Tezos TPS benchmark
Protocol: Alpha
Total number of accounts to use: 5
Blocks to bake: 10
Spinning up the network...
Reading description of the average block from src/bin_tps_evaluation/2021-09-01-to-2021-09-30.json
Average transaction cost: 1521
Using the parameter file: /run/user/1000/tezt-200367/1/parameters.json

{
  "bootstrap_accounts": [
    [
      "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav",
      "4000000000000"
    ],
    [
      "edpktzNbDAUjUk697W7gYg2CRuBQjyPxbEg8dLccYYwKSKvkPvjtV9",
      "4000000000000"
    ],
    [
      "edpkuTXkJDGcFd5nh6VvMz8phXxU3Bi7h6hqgywNFi1vZTfQNnS1RV",
      "4000000000000"
    ],
    [
      "edpkuFrRoDSEbJYgxRtLx2ps82UdaYc1WwfS9sE11yhauZt5DgCHbU",
      "4000000000000"
    ],
    [
      "edpkv8EUUH68jmo3f7Um5PezmfGrRF24gnfLpH3sVNwJnV5bVCxL2n",
      "4000000000000"
    ]
  ],
  "preserved_cycles": 5,
  "blocks_per_cycle": 8192,
  "blocks_per_commitment": 64,
  "blocks_per_stake_snapshot": 512,
  "blocks_per_voting_period": 40960,
  "hard_gas_limit_per_operation": "1040000",
  "hard_gas_limit_per_block": "5200000",
  "proof_of_work_threshold": "70368744177663",
  "tokens_per_roll": "6000000000",
  "seed_nonce_revelation_tip": "125000",
  "origination_size": 257,
  "baking_reward_fixed_portion": "10000000",
  "baking_reward_bonus_per_slot": "4286",
  "endorsing_reward_per_slot": "2857",
  "cost_per_byte": "250",
  "hard_storage_limit_per_operation": "60000",
  "quorum_min": 2000,
  "quorum_max": 7000,
  "min_proposal_quorum": 500,
  "liquidity_baking_subsidy": "2500000",
  "liquidity_baking_sunset_level": 3063809,
  "liquidity_baking_escape_ema_threshold": 666667,
  "max_operations_time_to_live": 120,
  "minimal_block_delay": "30",
  "delay_increment_per_round": "15",
  "consensus_committee_size": 7000,
  "consensus_threshold": 4667,
  "minimal_participation_ratio": {
    "numerator": 2,
    "denominator": 3
  },
  "max_slashing_period": 2,
  "frozen_deposits_percentage": 10,
  "double_baking_punishment": "640000000",
  "ratio_of_frozen_deposits_slashed_per_double_endorsement": {
    "numerator": 1,
    "denominator": 2
  },
  "cache_script_size": 100000000,
  "cache_stake_distribution_cycles": 8,
  "cache_sampler_state_cycles": 8,
  "tx_rollup_enable": false,
  "tx_rollup_origination_size": 60000,
  "tx_rollup_hard_size_limit_per_inbox": 100000,
  "tx_rollup_hard_size_limit_per_message": 5000,
  "sc_rollup_enable": false,
  "sc_rollup_origination_size": 6314
}

Waiting to reach level 3
The benchmark has been started
Produced 10 block(s) in 302.40 seconds
BLrxseo8ndcFNX85gqKxhziWKe16fEVSfyY9SuuepMZWUaaQvcv -> 3178
BLGiKCr8aHAW4EjkxPLBz5mG8wxL5TdjcUr3vXogntZgjo3XyE3 -> 3088
BKqNngk3vzHF5PX2yi3CrvETyqzNvaXXFhH9wHgNV1fGCtfP94X -> 3067
BM1fqcsaQV7ogQNmfhft8jcpPij5VdPmNZxSZVC3XSP6qFUEjzQ -> 3030
BMLauzzPZxkUydAHLBcWZFEPAmtRKZvDPzg9YLq97FzZs9eJ75W -> 3088
BM3vcth7K82YP6Vt9tjZwRyjBWfHpF8tuuRkAPydRdAMpP8egLK -> 3025
BKiK572CzEFhM8Pi73rZwk4uZQumoGUduzLQZvr3ZgiVUzTTb7F -> 3044
BMYc944mCRx5cRPp6EiSZ1PDP14iBPqDktuHnznZyBaGQVfUJVN -> 3078
BM4DthifaA1W7RYRy5PAGgh5ijNy6jPgtLYizozhQVJuVZZ7AZD -> 3066
BLw9T5ULAt92VAMB4t29SpLVbSyYDBW8oSzjjey5rEKtGj76ZaH -> 3057
Total applied transactions: 30721
Total injected transactions: 31917
TPS of injection (target): 114
TPS of injection (de facto): 105.55
Empirical TPS: 101.59
[21:43:46.996] [SUCCESS] (1/1) tezos_tps_benchmark
```

## Automatic daily runs of the TPS benchmark

Every day the TPS benchmark is run and the following results are registered:

* Gas TPS
* Results of running the TPS benchmark with protocol limits:
  * De facto TPS of injection
  * Empirical TPS
* Results of running the TPS benchmark with protocol limits lifted:
  * De facto TPS of injection
  * Empirical TPS

Regressions for these values are detected and recorded using the same
framework as the one Tezt long tests use.

## Adding support for more smart contracts

Smart contracts jointly account for about 60% of all transactions on
Mainnet. It is imperative to support calling the most popular smart
contracts as part of the stress test command in order to produce a realistic
stream of transactions and hence, a credible TPS estimation.

To add support for a smart contract the following changes need to be
performed:

1. Add an origination sub-command to the collection of client stress test
   commands. This command should originate the smart contract and prepare
   the ground for calling it in the benchmark.

2. Add a new option to the stress test command that would allow us to
   control the probability of calling this smart contract.

3. Implement the logic for calling the smart contract. Every smart contract
   is different, so, unfortunately, no catch-all solution is possible.

4. Adjust the TPS benchmark code in order to call the origination command
   during the preparation stage (before level 3 is reached) and pass the
   correct probability for this smart contract to the stress test command
   invocation.

[unify-protocol-limits]: https://gitlab.com/tezos/tezos/-/issues/2089
[long-tezts-locally]: http://tezos.gitlab.io/developer/long-tezts.html#testing-your-benchmarks-locally
[issue-2414]: https://gitlab.com/tezos/tezos/-/issues/2414
