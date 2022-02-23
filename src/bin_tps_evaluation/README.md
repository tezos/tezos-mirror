# Tezos TPS evaluation tool

## Estimating the average block

The CLI tool can be used to extract statistics about transactions and originations using [Tezos
indexer](https://gitlab.com/nomadic-labs/tezos-indexer) snapshots.

Please refer to tezos-indexer's [SQL schema](https://gitlab.com/nomadic-labs/tezos-indexer/-/tree/master/src/db-schema) for more
information about the stored data.

### Prerequisites

In order to run `tps-evaluation` you'll need a running PostgreSQL instance with a snapshot (either
full or partial) from the Tezos indexer.

[Here](https://z.lamini.ca/v970.mainnet.1729000.tar.bz2) is a snapshot containing blocks 0 to 1.729M. And
[here](https://z.lamini.ca/recent_blocks_mainnet.2021-Oct-5.sql.bz2) is a partial snapshot between
2021-09-03 and 2021-10-05.

Once you have snapshot, you can dump it to the PostgreSQL instance:

```bash
bunzip2 -c v970.mainnet.1729000.tar.bz2 | psql -h <pg-instance> -U <pg-user> -a <your-db-name>
```

This might take a while depending on the hardware you use.

Once the above command is complete you'll be able to use `tps-evaluation` to query the database.

### Example usage

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

## Running the TPS benchmark

The TPS benchmark is run with the `benchmark-tps` command. It spawns a
network comprising a node, a baker, and a client. The network will use the
same constants and parameters as the mainnet. It will wait till level 3 is
reached and after that it will run the stress test client command with the
maximum TPS that the parameters of the system allow. This TPS (called **TPS
of injection** in the output of the command) is calculated based on the
total amount of gas per block and the cost of the transaction we use in the
benchmark. By default 10 blocks will be produced, but this can be changed by
supplying the `--blocks-total` command line option. The total number of
applied operations will be divided by the total time spent producing the
blocks and the resulting value will be presented as the **empirical TPS**.

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

### Making sense of the results

The goal of the TPS benchmark is to give a high-level estimate of the TPS
value that the system is capable of. It can be used to catch TPS
regressions, but not to find where exactly the bottlenecks are.

The empirical TPS is significantly affected by the hardware on which the
benchmark is run and other factors, such as the amount of logging that is
performed. For example, passing `-- -v` is likely to result in much lower
empirical TPS values.

If the empirical TPS is very close to the TPS of injection this means that
the system is constrained by the protocol parameters and constants. This is
the situation that we want to have. Otherwise, if the empirical TPS is
significantly lower than the TPS of injection, it means that there is room
for improvement.

Right now, the empirical TPS value is not to be trusted because it is only
an intermediate result. We need to address the following problems before we
can consider the result trustworthy:

* The client stress test command needs to be sped up as it cannot yet
  provide the target TPS of injection. This means that right now we are
  benchmarking the stress test command and not the rest of the system.
* Over 60% of transactions on mainnet are smart contract calls. Since we are
  not performing any smart contract calls right now as part of the stress
  test command we cannot hope to get a realistic TPS value.
* We should enable the mempool pre-check to be closer to the behavior of the
  system in the real world.
* The benchmark should be run on a dedicated machine with predictable
  performance, not on developer laptops.

### Example usage

```
./tezos-tps-evaluation benchmark-tps
Tezos TPS benchmark
Protocol: Alpha
Total number of accounts to use: 5
Blocks to bake: 10
Using the default average block description
Average transaction cost: 1600
Spinning up the network...
Using the parameter file: /run/user/1000/tezt-64501/1/parameters.json

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
  "liquidity_baking_sunset_level": 2244609,
  "liquidity_baking_escape_ema_threshold": 1000000,
  "max_operations_time_to_live": 120,
  "round_durations": [
    "30",
    "45"
  ],
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
  }
}

Waiting to reach level 3
The benchmark has been started
Produced 10 block(s) in 300.941928 seconds
BMHVzWR8VVp4i2QkSKrN6yH6UXdLPzcBbpEgnh7ZRKi3w5oVwBW -> 3000
BLRUeGrfGQyhqVsBka5M8f2zygbrhicBHSwC2uVynsofMDnA9mU -> 2885
BMTQThgQ5Aw83uTfoVDbdXzWq8jmvfSmRRnEBKz2P7kqHW2JMKh -> 2924
BLRFhLQAH1tPRZLWxdebkaY45gVssbbD7PMthLHkhsWXqjhHXmq -> 2950
BMFaCggLcByhGGrG1ZfA9gBsAr9uWzh2CiAof3VkHvTcicty3jp -> 2913
BLJjQrtGzMVaWruPbCnQ6UqZQVq7iiZJ9JJCY1ibbp3t1Ggc96r -> 2890
BKuNhvm43qtwiVL8u6cTp3F2J9PLqEeuErBR829nq25cjQB5Yi5 -> 2879
BLvASNNSkNGusJnS8Kai2qD1bH9J4qofa4YhZCa86sFrusQyydY -> 2892
BLgFNszJtSv9reoCmoCwGhVNKjTQLtH3m5NDwKgwGpDo7g2goB8 -> 2846
BLxtURpMp3jFDFgBW2DP9t5vZNBm3aEo7NLM4kWGxUwaCrr1CRA -> 2947
Total applied transactions: 29126
TPS of injection: 109
Empirical TPS: 96.782792
[11:04:38.729] [SUCCESS] (1/1) tezos_tps_benchmark
```

## Estimating TPS using gas

It is possible to get an estimation of the maximal possible TPS by using the
protocol parameters. Unlike the `benchmark-tps` command the result is
instantaneous.

```
/tezos-tps-evaluation gas-tps
Using the default average block description
Average transaction cost: 1600
Gas TPS: 109
```

This is the same TPS value that is termed “TPS of injection” in the previous
section.
