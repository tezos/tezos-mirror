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

In order to estimate contents of the average block you will need Docker and
`bunzip`. The TPS benchmark should be built and the `tezos-tps-evaluation`
executable should exist in the root of the Tezos project.

Go to the directory of the TPS benchmark `src/bin_tps_evaluation` and run:

```
$ ./perform-analysis.sh ~/Downloads/v99-2022-janfeb.sql.bz2 2022-01-01 2022-02-28
```

Where

* `~/Downloads/v99-2022-janfeb.sql.bz2` is the name of the file that
  contains the PostgreSQL dump from the Tezos indexer.
* `2022-01-01` is the start date for analysis (inclusive).
* `2022-02-28` is the end date for analysis (inclusive).

Replace these arguments as necessary.

Some remarks:

* Loading the snapshot may take a while depending on the hardware you use.
  Full snapshots are extremely big (at the moment of writing, a full snapshot
  is about 29 Gb compressed and 320 Gb uncompressed on disk) and take long
  time to load. It might be better to gain access to the live instance of
  the database used by the Tezos indexer. I was told that one should talk to
  Samuel Bourque and Corentin Méhat about that.

## Estimating TPS using gas

It is possible to get an estimation of the maximal possible TPS by using the
protocol parameters.

```
./tezos-tps-evaluation-gas-tps -a average-block=src/bin_tps_evaluation/average-block.json
[14:26:25.243] Starting test: tezos_tps_gas
[14:26:27.956] Reading description of the average block from src/bin_tps_evaluation/average-block.json
[14:26:28.061] Originating smart contracts
[14:26:28.285] Waiting to reach the next level
[14:26:57.514] Average transaction cost: 2900
[14:26:57.514] Gas TPS: 60
[14:26:57.536] [SUCCESS] (1/1) tezos_tps_gas
```

This estimation is obtained by dividing the hard gas limit per block by the
average transaction cost.

The `tezos-tps-evaluation-gas-tps` command will also register its result in
a database if `tezt_config.json` exists (see [these
instructions][long-tezts-locally]).

## Running the TPS benchmark

The TPS benchmark is run with the `tezos-tps-evaluation-benchmark-tps`
command. It spawns a network comprising a node, a baker, and a client. The
network will use the same constants and parameters as the Mainnet. It will
wait till level 3 is reached and after that it will run the stress test
client command.

The TPS parameter that is passed to the stress test command (we call this
**target TPS of injection**) depends on the presence of the
`-a lift-protocol-limits` flag:

* If `-a lift-protocol-limits` is passed, a high enough value is passed, so
  that stress test will inject as fast as possible.
* If no `-a lift-protocol-limits` is passed, TPS computed from gas (see the
  previous section for details) will be used as the target TPS of injection.

By default 10 blocks will be produced, but this can be changed by supplying
the `-a blocks-total` command line option. The total number of applied
operations will be divided by the total time spent producing the blocks and
the resulting value will be presented as the **empirical TPS**. The
benchmark is also capable of calculating **de facto TPS of injection** which
is useful in judging the results.

The `tezos-tps-evaluation-benchmark-tps` command will also register its
result in a database if `tezt_config.json` exists (see [these
instructions][long-tezts-locally]).

### Making sense of the results

The goal of the TPS benchmark is to give a high-level estimate of the TPS
value that the system (simple pair node/baker) is capable of. It can be used
to catch TPS regressions, but not to find where exactly the bottlenecks are.

The empirical TPS is significantly affected by the hardware on which the
benchmark is run and other factors, such as the amount of logging that is
performed. For example, passing `-v` is likely to result in much lower
empirical TPS values.

The empirical TPS should normally be very close to the de facto TPS of
injection. If it isn't, then it means that the system cannot keep up with
the injection rate of the stress test, i.e. the bottleneck is in the system.
Otherwise the bottleneck is in the stress test or, in the case of lifted
limits, possibly in the hardware that is used.

Right now, the empirical TPS value is an intermediate result. We need to
address the following problems before we can consider the result
trustworthy:

* There is usually a small gap between the target TPS of injection and the
  de facto TPS of injection (see [issue 2414][issue-2414]). It is worth
  investigating this and trying to close the gap.

### Example usage

```
./tezos-tps-evaluation-benchmark-tps -a average-block=src/bin_tps_evaluation/average-block.json -a lift-protocol-limits
[20:42:19.167] Starting test: tezos_tps_benchmark
[20:42:19.167] Gas TPS estimation
[20:42:21.761] Reading description of the average block from src/bin_tps_evaluation/average-block.json
[20:42:21.858] Originating smart contracts
[20:42:22.082] Waiting to reach the next level
[20:42:51.262] Average transaction cost: 2900
[20:42:51.262] Gas TPS: 60
[20:42:51.293] Tezos TPS benchmark
[20:42:51.293] Protocol: Alpha
[20:42:51.293] Blocks to bake: 10
[20:42:51.293] Accounts to use: 30000
[20:42:51.293] Spinning up the network...
[20:43:01.039] Originating smart contracts
[20:43:01.861] Waiting to reach the next level
[20:43:27.301] Using the parameter file: /run/user/1000/tezt-185969/1/parameters.json
[20:43:27.301] Waiting to reach level 3
[20:43:57.054] The benchmark has been started
[20:49:03.482] Produced 10 block(s) in 306.43 seconds
[20:49:04.091] BLSh3JB76aHp2Zg37zrv35kr6XNu8Nti9hiGELriEWNJXHoEQN9 -> 5699
[20:49:04.431] BM2MMEawEUvKZm25LoB81rGEm2MtQtdkaXzAeuJACsbcZcsuuD2 -> 5651
[20:49:04.759] BLRN2oxtgpep1D3oCiD6NGo9GPha6Z2S69LiTa8SPskHLdjN1Ag -> 5762
[20:49:05.095] BMD3U3mv8EM9ZKYsK6sxFob5ZHHCYRUZiSn6LJVgvnp8eMaV8bY -> 5822
[20:49:05.411] BLCZf4mHGaawCCuJZLF2yFXwhg1tn32BbBezYm4dnik1mfDQ4wK -> 5892
[20:49:05.684] BLpPYyppLmp4enYopGBWNHvhGLzBFuWaFSJmEqkR1QQK1stWbPt -> 3868
[20:49:05.996] BKmqoWrZgevEmmd7RXaEX5umsBQaW3cnHJRK4AJq5uzEXzaryeE -> 5721
[20:49:06.303] BMGpJV4HtYjzfeq5HLJmvN9LFpPoPeuBeEdGdwYsZ57rc7gyvnQ -> 5810
[20:49:06.610] BLYCC8jRAb4xAQAYzZixC4PSYLacFtFHAV2C6jWi43obZL9aXVw -> 5537
[20:49:06.918] BLAb72vxpN5tnGoq2gbLP1ce8XfAqEruWgbkheyKdiUmMC5LuDt -> 5417
[20:49:06.918] Total applied transactions: 55179
[20:49:06.918] Total injected transactions: 55452
[20:49:06.918] TPS of injection (target): 1000
[20:49:06.918] TPS of injection (de facto): 180.96
[20:49:06.918] Empirical TPS: 180.07
[20:49:06.962] [SUCCESS] (1/1) tezos_tps_benchmark
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

To add support for a smart contract edit the
`src/proto_alpha/lib_client_commands/client_proto_stresstest_contracts.ml`
file and include the new smart contract in the `all_contracts` list.
Normally the new smart contract should start working right away, but since
the functionality is fairly new, more efforts might be required in some
cases.

[unify-protocol-limits]: https://gitlab.com/tezos/tezos/-/issues/2089
[long-tezts-locally]: http://tezos.gitlab.io/developer/long-tezts.html#testing-your-benchmarks-locally
[issue-2414]: https://gitlab.com/tezos/tezos/-/issues/2414
