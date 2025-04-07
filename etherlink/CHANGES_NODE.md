# Changelog

## Unreleased

### Features

- RPC node uses a private injector to bypass the excessive validations of
  going through the public endpoint. Reads `--evm-node-private-endpoint` if
  specified or the RW config private rpc if not. (!16664)
- Supports native execution for the Calypso kernel. (!16728 !16729 !16734)

### Bug fixes

- `eth_getLogs` now accepts `null` as a valid value for all its filter
  parameters. (!16808)
- Fixes switching to a history mode with a smaller retention period.
  Previously, the command would succeed, but the node would keep using the
  previous retention period. (!16798)

### Internals

## Version 0.17 (2025-02-14)

This release addresses several bugs reported by partners, notably around the
transaction objects returned by the RPCs, notably for EIP-1559 transactions.

The node will apply one migration to its internal store (version 19), meaning
it is not possible to downgrade to the previous version.

### Features

- Adds `full` history mode, where the EVM node keeps all the blocks and transactions,
  but still prunes state data. (!16584)

### Bug Fixes

- Advertises the correct history mode on start-up when it is not explicitely
  set in the configuration file. (!16633)
- Fixes a performance regression in the RPCs of the new block storage backend.
  (!16659)
- Fixes transaction objects returned by the RPCs for non-legacy transactions
  (!16653 !16654 !16703 !16707)
- Improves call traces of `DELEGATECALL` and `CALLCODE` for Bifrost (requires
  native execution enabled with `--native-execution-policy rpcs_only` for
  instance). (!16588)

## Version 0.16 (2025-02-06)

This release notably stabilizes the new storage for blocks (which
significantly reduces the size of data directories), and the `rolling` history
mode (which enables deployment where only the most recent states are kept).

The node now uses this block storage by default for new deployments. Data
directories created before this release will keep using the deprecated, legacy
block storage. Migrating to the new block storage requires importing a
snapshot. To give an example, Etherlink Mainnet archive nodes using the legacy
storage require more than 250GBytes of disk, while the new block storage
requires around 180GBytes.

The `rolling` history mode can be enabled on new data directories by running
`octez-evm-node config init --history-mode rolling:N`, where `N` is the number
of days to keep, _before_ starting the node for the first time. For existing
data directories, it is required to run the command `octez-evm-node switch
history to rolling:N` when the node is stopped. At the time of releasing this
version, a data directory of a node configured to use the `rolling:14` history
mode requires around 13GBytes.

The node will apply two migrations to its internal store (version 18), meaning
it is not possible to downgrade to the previous version.

### Features

- Defaults to the new block storage, which significantly reduces the size of
- New command `switch history to` which is now the only way to change history
  mode for and already populated EVM node. (!16533)
- CLI command `list events`, allows listing events relative to the EVM node. (!16446)
- Reworks logs to be more compact and to respect other Octez logs format.
  (!16265 !16508 !16518 !16521)
- Added `elapsed_time` to the performance metrics, which exposes in seconds the
  time since the node started. (!16551)

### Bug fixes

#### RPCs

- `eth_estimateGas` now correctly interprets the block parameter. (!16423)
- `eth_getLogs` now properly supports the [EIP-234]
  introducing the `blockHash` parameter. (!16460)
- `eth_getLogs` now returns an empty array instead of an error when `fromBlock`
  is greater than `toBlock`.
- Improves reliability of `debug_traceTransaction` when dealing with non
  standard revert reasons. (!16415)
- Fixes the encoding of state overrides for `eth_call` to correctly parse
  `stateDiff`. Keeps the support for `state_diff` for now. (!16581)

[EIP-234]: https://eips.ethereum.org/EIPS/eip-234

#### UX

- Disables the performance metrics if `ps` does not support the necessary CLI
  arguments (typically, `ps` from BusyBox does not). (!16474)
- Hides the logs of `du` on failure (typically when a directory does not
  exist). (!16609)

### Internal

- Snapshot headers now include history mode to check they match on import and
  and the node now refuses to import archive snapshots with less
  history. (!16458)

## Version 0.15 (2025-01-28)

This release brings a number of quality of life improvements, including the
support of the [`debug_traceBlockByNumber`] RPC for the `callTracer` tracer,
the introducion of performance metrics which can be used to monitor the load
induced by a node to its host system, improved performance for the Bifröst
kernel when executed natively, as well as a number of bug fixes.

[`debug_traceBlockByNumber`]: https://www.quicknode.com/docs/ethereum/debug_traceBlockByNumber

This release will not apply any migration to the node’s store (version 16),
meaning it is possible to downgrade to the previous version.

### Features

#### RPCs

- Implements the RPC endpoint `debug_traceBlockByNumber`, with only the
  `callTracer` at the moment. (!16164)
- Improve performances of the kernel execution when the native execution is
  enabled (*e.g.*, `--native-execution-policy rpcs_only`). As a reminder, to
  this day only the [Bifröst kernel] can be executed natively by the EVM node.
  (!16335)

[Bifröst kernel]: https://medium.com/etherlink/announcing-bifr%C3%B6st-a-2nd-upgrade-proposal-for-etherlink-mainnet-ef1a7cf9715f

#### Metrics

- When the host running the node provides `ps`, `du` and `lsof`, the node will
  now exports performance metrics. (!16367)

#### Experimental

*No guarantees are provided regarding backward compatibility of experimental
features. They can be modified or removed without any deprecation notices. If
you start using them, you probably want to use `octez-evm-node check config
--config-file PATH` to assert your configuration file is still valid.*

- Websocket connections with clients are monitored by default by sending regular
  heartbeats. This feature can be disabled or tweaked by changing
  `experimental_features.monitor_websocket_heartbeat` in the
  configuration. (!16197)

- The observer mode option `--init-from-snapshot` can now take an
  snapshot provider url. the EVM node in will download and import a
  recent snapshot from it instead of the trusted source for fresh data
  directories. (!16138)

### Bug fixes

#### CLI

- Fix the initial kernel used by default for Etherlink Mainnet (`--network
  mainnet` on a newly created data directory). (!16409)

#### RPCs

- `eth_getLogs` now accepts `null` as a valid value for the `topics` parameter.
  (!16357)
- `eth_getLogs` now correctly interprets block numbers like `earliest`,
  `finalized`, etc. They were all defaulting to `latest` before. (!16372)

- New snapshot header for Sqlite3 block storage. Snapshots in the previous
  format are called legacy as reported by the `snapshot info` command. (!16435)

## Version 0.14 (2025-01-21)

This version contains UX improvements for displaying progress regarding snapshot
download/import with `--init-from-snapshot`, and fixes an issue where the `logs`
events of the JSON-RPC method `eth_subscribe` on WebSockets (whose support by
the EVM node remains experimental at this point) would not be notified.

This release will not apply any migration to the node’s store (version 16),
meaning it is possible to downgrade to the previous version.

### Features

- Periodically (every minute) reports the remaining number of bytes to download
  for snapshots. (!16198)
- Periodically (every minute) reports progress on the snapshot archive
  decompression and extraction. (!16288)

### Bug fixes

#### Experimental

*No guarantees are provided regarding backward compatibility of experimental
features. They can be modified or removed without any deprecation notices. If
you start using them, you probably want to use `octez-evm-node check config
--config-file PATH` to assert your configuration file is still valid.*

- Notify logs with `eth_subscribe` independently of block storage
  backend. (!16258)

## Version 0.13 (2025-01-09)

This new version notably reduces the complexity to set up a new node for
Etherlink Mainnet Beta and Etherlink Testnet. To give a concrete example, to
start a new node on a fresh data dir for the Testnet is now as simple as:

```
octez-evm-node run observer --network testnet \
    --dont-track-rollup-node \
    --init-from-snapshot \
    --data-dir foobar
```

If `foobar` does not exist, the Octez EVM Node will automatically download and
import a snapshot, before starting to track the chain through the public
endpoint. **For production deployment, you most likely need to configure
with more care**.

Additionally, the Octez EVM Node can now expose an **experimental** websocket
endpoint. Not only does this endpoint support the same methods the HTTP
endpoint, but it also implements the [`eth_subscribe`][eth-subscribe] method.

[eth-subscribe]: https://docs.alchemy.com/reference/eth-subscribe

To enable this feature, you need to modify the `config.json` file in the data
directory of your node to enable `experimental_features.enable_websocket`. For
instance, the following minimal `config.json` file can be used in conjunction
with the minimal command-line example to start a node for the Testnet.

```json
{ "experimental_features": { "enable_websocket": true } }
```

The node should advertises the availibility of the websocket endpoint on
startup.

```
Jan 08 17:33:39.911: the EVM node RPC server (resto) is listening to 127.0.0.1:8545
Jan 08 17:33:39.911:   (websockets enabled)
```

We plan to stabilize the feature in a near future release.

This release will apply one migration to the node’s store (version 16), meaning
it is not possible to downgrade to the previous version.

### Breaking Changes

- Add a default value (1024) for the RPC `eth_feeHistory` `block_count`
  parameters. Configuration can be `unlimited` if no maximum is wanted.
  (!16150)

### Features

- The event `evm_context_processed_l1_level` now contains the latest finalized
  blueprint in the event. (!15877)
- It is now possible to specify the network an EVM node in observer mode is
  expected to join (with the `--network` CLI argument)
    - If `--network` is set, new sanity checks are done at startup to ensure
      its data directory and upstream EVM node are consistent with the selected
      network. (!16073)
    - If `--network` is set, the EVM node will use default values for the upstream
      EVM node and the preimages endpoint if they have not been set either through
      the configuration file or a CLI argument (with `init config` and `run
      observer` commands). (!16074 !16075)
    - If `--network mainnet` is set, the EVM node no longer requires the initial
      kernel used to originate Etherlink on Tezos mainnet to initialize the
      data-dir of an observer node, though an initial kernel can still be provided.
      (!16076)
    - If both `--network` and `--init-from-snapshot` are set, the EVM node in
      observer mode will download and import a recent snapshot from a trusted
      source for fresh data directories. (!16093)
- The `snapshot import` command can now download a snapshot if provided with a
  URL instead of a path. (!16112)

#### Experimental

*No guarantees are provided regarding backward compatibility of experimental
features. They can be modified or removed without any deprecation notices. If
you start using them, you probably want to use `octez-evm-node check config
--config-file PATH` to assert your configuration file is still valid.*

- Experimental support for websockets on endpoints (`/ws` and `/private/ws`) for
  JSON-RPC requests with feature flag `experimental_features.enable_websocket =
  true`. (!15566, !16015)
    - Added support for the WebSocket event `newHeads`, allowing clients to
      receive real-time notifications of new blocks. (!15899)
    - Added support for the WebSocket event `newPendingTransactions`, enabling
      clients to receive real-time notifications of incoming pending
      transactions. (!15991)
    - Added support for the WebSocket event `logs`, enabling clients to receive
      real-time notifications for contract events filtered by address and
      topics. (!16011)
    - Configurable maximum websocket message length (defaults to 4MB). (!16070)
- History mode can now be selected with the parameter `history_mode`, `archive`
  and `rolling`. If the mode is `rolling` it will use the field
  `garbage_collect_parameters` to prune blocks, operations and states. (!16044)
- Experimental support of ordering blueprints by they level for the
  smart rollup node batcher. Activates the feature with the flag
  `exprimental_features.blueprints_publisher_order_enabled`.
  **Requires Octez Smart Rollup Node built from master.**. The
  necessary feature on the Octez Smart Rollup node is not yet
  released. (!15877)

### Bug fixes

- Observers will comply with the `keep_alive` configuration option when trying
  to fetch the rollup address from their upstream EVM node. Before, it
  would exit with an error even if `keep_alive` was set to `true`. (!16094)
- Fixes invalid pending nonce for fresh accounts with pending transactions
  in the transaction pool. (!16099)
- Fixes invalid error on RPC `eth_coinbase` when `"params": []` was provided. (!16109)

### Internals

- Removes an internal check about DA fees in the node, improving the
  performances of gas estimations. (!16055)

## Version 0.12 (2024-12-17)

This release notably addresses a bug introduced in the previous release,
leading the data directory of the node to grow in size more than necessary.

This release will apply one migration to the node’s store (version 15), meaning
it is not possible to downgrade to the previous version.

### Features

#### UX

- The sequencer can now handle when the delayed inbox is flushed to create
  blueprints containing overdue items, for kernels advertizing it **(which is
  not the case for Bifrost, currently activated on Etherlink Mainnet and
  Testnet)**. It clears all blocks produced on its invalidated branch, then
  starts a new one. (!15676)
- If an observer detects a divergence between its local state and the branch
  computed by the Rollup node it is connected to, it will reset its local state
  to its latest finalized block instead of exiting. (!15751)
- The node will no longer exit on invalid blueprint provided by its upstream
  EVM node endpoint, and will instead keep waiting for a valid blueprint to be
  provided. (!15751)

#### Performances

- The transaction's validation is now performed in native OCaml instead of
  using the kernel's simulation, which makes the validation of transactions
  on `eth_sendRawTransaction` faster. (!15958)

### Bug fixes

- Fixes the `store_move` host function implementation of the WASM Runtime host
  function, that would copy only instead of moving a subtree. The main impact
  of this bug was to needlessly increase the size of the node data directory.
  (!16009)

### Internals

- Private RPCs stateValue and stateSubkeys can now take the block parameter,
  e.g. `"params": ["/evm/chain_id", "finalized"]`. (!16010)

## Version 0.11 (2024-12-11)

In addition to several bug fixes and internal changes, this release introduces
several exciting new features, notably including support for executing the
Bifrost kernel natively, and an experimental alternative backend for the RPC
server which shows promising results in terms of raw performances.

This release will apply one migration to the node’s store (version 15), meaning
it is not possible to downgrade to the previous version.

### Features

#### Performances

- It is now possible to execute Bifrost (the kernel currently activated on
  Etherlink Mainnet and Testnet) natively, which leads to improved
  performances. For now, this features is disabled by default, but can be
  enabled by setting the `kernel_execution.native_execution_policy` in your
  configuration file, or with the `--native-execution-policy` CLI argument
  (*e.g.*, in `run observer`). For now, we recommend against using
  `--native-execution-policy always` in production. (!15736)

#### UX

- Gas estimation requires multiple kernel simulation. Every time a simulation
  was performed we would pick a new simulation timestamp, which could
  technically lead to a different execution. Changes the behavior to pick
  a single timestamp per simulation. (!15889)
- Internal error in the EVM context emits an event instead of silently
  failing. (!15824)

#### Experimental

*No guarantees are provided regarding backward compatibility of experimental
features. They can be modified or removed without any deprecation notices. If
you start using them, you probably want to use `octez-evm-node check config
--config-file PATH` to assert your configuration file is still valid.*

- The sequencer will use the `drop_duplicate` feature of the Rollup node when
  publishing blueprints if it is configured to do so (with
  `experimental_features.drop_duplicate_on_injection` is set to `true`).
  **Requires a Rollup Node from version 21.0 or higher.** (!15867)
- Experimental support for alternative RPC server backend
  [Dream](https://aantron.github.io/dream) with feature flag
  `experimental_features.rpc_server = "dream"`. (!15560)

### Bug fixes

#### RPCs

- Fix the RPC mode having an outdated view of the latest finalized block of the
  chain. (!15884)

#### Structured logs

- The `blueprint_applied` event now advertizes the correct processing time. It
  was significantly underestimated before. (!15948)

### Internals

- Gas estimation requires multiple kernel simulation. Every time a simulation
  was performed we would pick a new simulation timestamp, which could
  technically lead to a different execution. Changes the behavior to pick
  a single timestamp per simulation. (!15889)
- Internal error in the EVM context emits an event instead of silently
  failing. (!15824)
- Garbage collector experimental feature keeps a number of chunks
  rather than keeping "number of seconds" of history. For example, if
  `split_frequency_in_seconds` is set to 86_400 (1 day) and `number_of_chunks`
  is set to 7, the node will keep 7 days of history. (!15928)

## Verson 0.10 (2024-12-02)

This is a bug fixes release, fixing the memory leak of the node and improving
the interoperability with MetaMask and other wallets setting arbitrary high gas
limit on `eth_estimateGas` requests.

This release will not apply any migration to the node’s store (version 14),
meaning it is possible to downgrade to the previous version.

### Bug fixes

- Fix the memory leak observed in the EVM node, and improves performances for
  blocks application and RPC requests requiring to execute the kernel. This is
  achieved by switching to a dedicated WASM runtime, instead of using the one
  developed for the rollup node. (!15389)
- Do not fail on `eth_estimateGas` when the submitted gas is greater than 30M
  gas, to support some wallets setting a very high gas limit to estimate gas.

## Version 0.9 (2024-11-28)

This is a bug fixes release, notably improving the tracking of rollup node
events.

This release will apply one migration to the node’s store (version 14), meaning
it is not possible to downgrade to the previous version.

### Bug fixes

- Default rollup node port is 8932, not 8937. (!15701)
- Make the rollup node follower more resilient. (!15745)

### Internals

- Use a single RPC (if the rollup node supports it) when fetching EVM events.
  (!15629, !15703)
- Private RPC `produceBlock` can produce a block without delayed transactions,
  useful for testing purposes. (!15681)
- Keep a buffer of finalized states if the rollup node is in advance. When
  observer catches up, it checks provided blueprint against these finalized
  states. (!15748)

## Version 0.8 (2024-11-15)

This is a bug fixes release, notably improving verbosity of the evm-node on
communication errors with the rollup node.

This release will not apply any migration to the node’s store (version 13),
meaning it is possible to downgrade to the previous version.

### Features

#### Metrics

- Adds `l1_level` to expose the last processed L1 block. If the node is connected
  to a rollup node it should be close to the most recent finalized L1 block
  on the network. (!15623)

### Bug fixes

- Fixes a race condition where rollup node follower would process start
  processing a block before finishing the processing of its predecessor. (!15620)

### Internals

- Adds the duration of the blueprint application to the
  `blueprint_applied` event. (!15505)
- Adds the last processed L1 block in a new event `evm_context_processed_l1_level`. (!15623)
- The gas estimation exits earlier if the simulation needs more than the
  gas limit per transaction. It also supports custom maximum gas limit. (!15468)
- Modify the blueprints publisher worker to log error when it crash
  and make the node crash when it fails to add a blueprints into its
  queue. (!15600)
- Adds events for errors in evm events and rollup node
  followers. (!15622)

## Version 0.7 (2024-10-28)

This is a bug fixes release, notably improving how the observer mode deals with
many transactions coming from the same source when the finalized view is
enabled.

This release will not apply any migration to the node’s store (version 13),
meaning it is possible to downgrade to the previous version.

### Bug fixes

#### RPCs

- Forward transactions to the upstream EVM node without delay when running in
  observer mode with the finalized view option enabled. This improves the UX of
  the users of said node without sacrificing the security provided by the
  finalized view. (!15453)
- Fixes the `callTracer`’s output containing invalid UTF8 string on reverted
  calls. (!15421)
- The transaction pool has a limit of transactions per user, if the
  limit is reached, the submitted must replace an existing transaction, that is
  either a transaction with the same nonce, or replace the "largest" nonce
  found in the transaction pool. It fixes the issue where the limit is reached
  and the user cannot unlock the situation by itself as the node refuses
  all transactions. (!15465)

### Internals

- Adds more information to the `blueprint_applied` event, including block
  timestamp, number of transactions and gas used. (!15402)

## Version 0.6 (2024-10-21)

This release introduces a new, still experimental runtime to execute Etherlink
kernel. It will eventually replace the general-purpose WASM runtime initially
implemented for the Octez Rollup Node. This release also introduces a few
metrics, to help with monitoring.

The node will apply two migrations to its internal store (version 13), meaning
it is not possible to downgrade to the previous version.

### Features

- The sequencer forwards the kernel upgrade event as soon as possible so
  observers can download in advance the kernel. (!15276)

#### RPCs

- The experimental feature `node_transaction_validation` that enables validating
  transactions in the node instead of invoking the kernel is now activated by
  default. It does not change the notion of transaction validation but should
  provide faster validation. (!15062)

#### Metrics

- Adds `time_waiting` to expose the time spent by client waiting for their
  requests’ execution to start. (!15241)
- Adds `queue_size` to expose the number of clients waiting for their turn in
  the execution queue. (!15241)
- Add `gas_price` to expose the gas price of the latest block. (!15239)

#### Experimental

- The new WASM Runtime can be enabled by setting
  `experimental_features.next_wasm_runtime` to `true` in the  configuration
  file. It brings better performances (especially for short calls to the
  kernel), and fixes the memory leak that was affecting the node since its
  beginnings. (!15025 !15039 !15017 !15272 !15269)

### Bug fixes

- The node will no longer crash when catching-up if it cannot fetch a blueprint
  from its upstream EVM node. Instead, it will retry as many time as necessary.
  This is aligned with the behavior of the node when it is up-to-date with its
  upstream node. (!15232)
- The node will now create its daily logs file with Unix read permission
  granted to the group owning them. This makes them easier to collect by
  an external process like `promtail`. (!15227)
- Fix file descriptor leak in resto for connections with the rollup
  node. (!15322)

## Version 0.5 (2024-09-27)

This release brings two features of interest for users operating EVM nodes as
part of a dApp deployments. These features are not for general-purpose
deployments.

This release will not apply any migration to the node’s store (version 11),
meaning it is possible to downgrade to the previous version.

### Features

#### RPCs

- Generalized the support for the `--finalized-view` CLI argument. When
  enabled, the `latest` block parameter becomes a synonym for `finalized`.
  As a consequence, the `proxy.finalized_view` configuration parameter is
  **deprecated**, in favor of the newly introduced `finalized_view` toplevel
  configuration parameter. `proxy.finalized_view` will be removed in a future
  version. (!15080)
- **Experimental:** Introduce the experimental feature
  `ovewrite_simulation_tick_limit`. When enabled, the `eth_call` RPC is no
  longer subject to the kernel tick limit. This can be useful to execute calls
  that will not be injected in transactions (similarly to what the Uniswap V3
  frontend does to prepare swaps). However, it can lead to confusing UX for
  users, where eth_estimateGas fails when eth_call succeeded. (!15078)

## Version 0.4 (2024-09-18)

The main addition of this release is an initial support for the
`go-ethereum`-specific “state overrides” feature. `eth_call` now supports
overriding the balance, nonce, state (both with `state` and `stateDiff` keys)
and code of arbitrary address. It also contains a number of bug fixes and
miscellaneous improvements.

This version is compatible with every kernel deployed on Etherlink Mainnet
Beta.

The node will apply two migrations to its internal store (version 11), meaning
it is not possible to downgrade to the previous version.

### Features

#### CLI

- Support string interpolation in `--snapshot-file` (for command `snapshot
  export`): `%r` will be replaced by the short rollup address, `%R` by the full
  rollup address, and `%l` by the current level. (!14854 !14886)
- Add a flag `--no-sync` to the observer command. If the flag is set, the observer
  does not synchronize with the EVM node endpoint. (!14889)

#### RPCs

- Completes state override option of `eth_call` RPC, by adding `state` and
  `stateDiff`, similar to go-ethereum. (!14869, !14921, !14970)

#### Metrics

- Add the `smart_rollup_address` label to the `octez_evm_node_info` metrics. (!14906)

### Bug fixes

#### CLI

- Fix `init from rollup node` command failing to store items of the delayed
  inbox in its local state when `--omit-delayed-tx-events` is not provided.
  (!14855)
- Fix the issue that prevented importing snapshot in an already populated data
  directory. (!14856)
- Fix the issue that prevented exporting a compressed snapshot with a user
  provided name. (!14856)

#### Metrics

- Fix `octez_evm_node_level` and `octez_evm_node_confirmed_level` not being
  correctly initialized on startup. These values are now correctly set before
  the RPC servers are started. (!14956)

### Internals

- Delayed transactions are systematically attached to the blueprint that
  includes them, instead of the current head of the EVM node when they are
  inserted in the delayed inbox. This prevents the propagation of inconsistent
  blueprints when a sequencer does not include every item of the delayed inbox
  at once. (!14878, !14927)
- Nodes connected to an upstream EVM node will now use the content-type
  `octet-stream` (instead of JSON). (!14949)

## Version 0.3 (2024-09-10)

This release primilarly addresses two bugs uncovered in production: the broken
`octez_evm_node_head` metrics for the RPC mode, and the node hanging when
catching-up to the head of its upstream EVM node (in observer mode).

This release is compatible with every kernel deployed on Etherlink Mainnet
Beta.

The node will not apply any migration to its internal store (version 9),
meaning it is possible to downgrade to the previous release.

### Features

#### CLI

- Add the argument `--block-number N` to select the state to patch when using
  the `patch` commands. More precisely, patching the state `N` affects the
  replay of block `N+1`. (!14809)

#### RPCs

- Add state override option to `eth_call` RPC, similar to go-ethereum.
  Limited to fields `balance`, `nonce` and `code`. (!14708)

### Bug fixes

#### RPCs

- Hide `private_rpc` in the output of `GET /configuration`. (!14868)

#### Metrics

- Fix `octez_evm_node_head` for the RPC mode. (!14849)

#### Internals

- Prevent the node to hang when catching-up to the head of its upstream EVM
  node. (!14750)

## Version 0.2 (2024-09-05)

This release introduces a number of quality of life improvements for operators
running the Octez EVM Node in production. Most notably, a long awaited support
for snapshots (export and import) is introduced. Note that exporting a snapshot
requires to stop the node for now.

This version is compatible with every kernel deployed on Etherlink Mainnet
Beta.

The node will not apply any migration to its internal store (version 9),
meaning it is possible to downgrade to the previous version.

### Features

#### CLI

- Add a new command `check config [--data-dir <data-dir>] [--config-file
  <path>] [-p]` to assess the correctness of a configuration file (either
  `<data-dir>/config.json` or `path`). With `-p`, the parsed configuration file
  is printed in the standard output, which can be useful to review them
  carefully. (!14690 !14713)
- The command `replay` does not require to stop the node anymore. (!14727)
- Add a new command `snapshot export` to export a snapshot of the EVM node
  current state. (!14255 !14280 !14281 !14325 !14372 !14781)
- Add a new command `snapshot import` to bootstrap a node from a
  snapshot. (!14333 !14778)
- Add a new command `snapshot info` to display information about a snapshot
  without importing. (!14797)
- Add new commands `snapshot export` to export a snapshot
  of the EVM node current state  and `snapshot import` to bootstrap a node from
  a snapshot. (!14255 !14280 !14281 !14325 !14372 !14333 !14781 !14778)

#### RPCs

- Add `GET /configuration` that returns the loaded configuration. Some sensitive/internal
  fields are hidden from the output. (!13865)
- Add `GET /health_check` that fails with an internal server error if the EVM
  node is catching-up with its upstream EVM node, advertizing the RPC server
  can return outdated results. (!14769)
- The node in RPC mode now supports the `"finalized"` block parameter. (!14725)
- Transaction validation returns the transaction execution gas limit and the
  network's limit if the transaction has too much execution gas. (!todo)

#### Metrics

- Add `octez_evm_node_bootstrapping` which is set to 1.0 when the node is
  catching-up with its upstream EVM node endpoint, and 0.0 when it considers
  itself up-to-date. (!14751)

#### Internal

- When a kernel upgrade is detected from upstream (either a rollup
  node for a sequencer node or an EVM node for an observer node) the
  kernel is preemptively downloaded to reduce friction at the
  activation of the upgrade. (!14656)

## Version 0.1 (2024-08-28)

This release concludes two months of development since the freeze of the
Etherlink kernel `ec7c3b349624896b269e179384d0a45cf39e1145` on June 6, 2024. It
contains a number of breaking changes, making the upgrade potentially
challenging. This was deemed necessary to clean-up the UX of the node and
provide a better foundation for further changes.

This version is compatible with every kernel deployed on Etherlink Mainnet
Beta.

### Breaking changes

- Remove the deprecated `run <mode> with endpoint` commands, use
  `run <mode>` instead. (!14566)
- Remove the `time_between_blocks` field of the observer configuration
  (!14327).
- Remove the `preimages` and `preimages_endpoint` fields specific to each
  mode, in favor of a new `kernel_execution` top-level configuration.
  (!14332)
- Replace the fields related to the RPC servers configuration (`rpc-port`,
  `rpc-addr`, `max_active_connections`, `sequencer.private_rpc_port`, etc.) in
  favor of `publc_rpc` and `private_rpc`. (!14460 !14527)
- Use `_` consistently instead of `-` in every fields. (!14527)
- Remove the `--devmode` CLI argument. (!14602)

### Features

- Metrics register the number of calls to each RPC method, e.g. `eth_blockNumber`.
  (!13786)
- Spinner for `init from rollup node` command. (!13801)
- Log the errors trace when blueprint injection fails. (!13859)
- Add support for `max_blueprint_lookahead_in_seconds` in the kernel config helper
  command. (!13827)
- Support the RPC `eth_coinbase`. It returns the sequencer pool address if it
  exists, otherwise returns the zero address. (!todo)
- Add `--restricted-rpcs` to disable RPCs, following the given regular
  expression. (!13993)
- Add `--whitelisted-rpcs` to enable a subset of RPCs. (!14441)
- Add `--blacklisted-rpcs` to disable a subset of RPCs. (!14441)
- Add an **unsafe** command `patch kernel with [FILE]` to patch the kernel used
  by an EVM node. The main use case for this command can be used to fix bugs in
  RPCs partly implemented in the kernel (*e.g.*, `eth_call` for instance).
  (!14053)
- Add a private RPC "stateSubkeys", that allows to query the subkeys
  of a path in the storage. (!14129)
- The current timestamp is used in simulation (instead of previous block
  timestamp) if the kernel version is compatible. (!14186)
- Adds a `run sandbox` command runs the evm-node in a sequencer-like mode
  but do not require the sequencer's secret key and any connection to
  a rollup node. It is meant for debugging only. (!14258)
- Introduce `GET /evm/time_between_blocks` to retrieve the maximum time (in
  seconds) between two blocks created by the sequencer. It is used by the nodes
  running in observer and RPC modes. (!14327)
- Adds support for `callTracer` in `debug_traceTransaction` and
  `debug_traceCall`. (!14273)
- Add an **unsafe** command `patch state at [path] with [value]` to patch
  the kernel state used by an EVM node. The main use case for this command
  is to modify the state to debug, it is not mean to be used under normal
  circumstances.  (!14383)
- Support running an observer node without a companion rollup node with
  `--dont-track-rollup-node`. (!14428)
- Introduce a configuration parameter `rpc-batch-limit` and a CLI argument
  `--rpc-batch-limit` to limit the maximum size of JSON RPC API batches.
  (!14432)
- Allow a proxy node to forward incoming transactions to a companion EVM node.
  (!14540)
- Ensure the EVM node always uses the very last version of the WASM PVM.
  (!14553)
- The observer node now starts a private RPC server when `private_rpc_port` is
  set in the configuration file. (!14460)
- Support ignoring the block parameter in RPCs in proxy node, always defaulting
  to the latest block instead. (!14557)
- Add a debug command to print JSON schema of the configuration file. (!14531)
- The `callTracer` configuration is now enabled for `debug_traceTransaction` for all transactions created from this point onward. (!14636)

### Bug fixes

- `eth_sendTransaction` now returns "unsupported method" error
  instead of "the pattern matching is not exhaustive" one. (!13960)
- Simulation uses the `/evm/storage_version` to determine if the feature
  `with_da_fees` is activated. Enabling incorrectly the feature of past
  kernels results in errors in simulation. (!14127)
- Improves the error message in case of unknown block in RPCs. (!14150)
- Produces an empty block in case the produced one will trigger a
  kernel upgrade. (!14207)
- Currently decode transaction indices in RPC results of
  `eth_getTransactionReceipt` and `eth_getBlockByNumber`. (!14645 !14671)

### Experimental

- Introduce a new “RPC mode” which exposes a new RPC server for an existing
  data-dir. (!13208 !14348)

### Internal

- Limit the number of parallel execution of Etherlink’s kernel to answer RPC,
  to improve the node’s resilience to a large number of concurrent requests.
  (!13738)
- The simulation can be called by not taking into account DA fees in the gas
  computation. (!13729)
- Simulation supports mainnet after the security upgrade. (!14006)
- Speed up init from rollup node command. (!14173)
- Store finalized L2 level per L1 level. (!14193)
- Reduces the number of WASM PVM calls for gas estimation. In particular
  estimation of DA fees no longer require to run the kernel. (!14165)
- Add a private RPC method to by-pass node validation when injecting a
  transaction to the TX pool. (!13856).
- Add a debug command to print the SQLite schemas currently is use by the node.
  (!14509)

## Version for ec7c3b349624896b269e179384d0a45cf39e1145

### Features

- Deprecate the `--devmode` CLI flag. The EVM node aims to be compatible with
  three versions of the kernel: the latest one, the one deployed on Ghostnet
  and the one deployed on Mainnet. (!13522)
- Support daily log files for the observer mode (!13101).
- The RPC `eth_getBalance`, `eth_getCode`, `eth_getTransactionCount` and
  `eth_getStorageAt` support the default block parameter
  (https://ethereum.org/en/developers/docs/apis/json-rpc/#default-block).
  (!13039, !13056, !13058, !13124) For now, the `latest` block is used when
  `pending` is requested (except for `eth_getTransactionCount`). (!13298)
- Support partially https://eips.ethereum.org/EIPS/eip-1898, the
  `requireCanonical` field is not yet supported. (!12345)
- The RPC `eth_call` supports the default block parameter
  (https://ethereum.org/en/developers/docs/apis/json-rpc/#default-block). (!13110)
- The RPC `eth_call` supports `null` parameters. (!13067)
- Support the RPC `eth_maxPriorityFeePerGas`, which always returns 0 wei. (!13161)
- Support for specifying an expected time between blocks for the observer node
  in the configuration file, used to detect when the connection with the
  upstream EVM node endpoint is stalled. (!13265)
- Support for the `charset` specifier for `Content-Type: application/json`. (!13256)
- `txpool_content` support. (!12873, !13292)
- Support the RPC `eth_getBlockReceipts`. (!13370)
- `blueprints_publisher_config` and `verbose` are made optional in the
  configuration. (!13084)
- The EVM node has metrics available at `/metrics`. It gives metrics about
  progress of the chain, block production, node information, transaction pool
  and rpcs. (!13128, !13132, !13135)
- The observer mode has the `devmode` flag, similar to the sequencer mode. (!13163)
- Adds a `--read-only` flag to the proxy mode, if the flag is set, the transaction
  pool is not supported. (!13162)
- Support `debug_traceTransaction` with its default logger `structLogs`.
  (!13268, !13321, !13350, !13378)
- Return `baseFeePerGas` and `mixHash` field for `eth_getBlockBy*` RPCs. The
  former only when appropriate, the later with a default value. (!13159)
- Support for the `eth_feeHistory` RPC. (!13259)
- Support `FaDeposit` delayed message. (!13532)

### Experimental

- Support for write-ahead log journal mode for the EVM node’s store. (!13192)
- Add the field `drop_duplicate` for the batcher injection rpc input
  (rollup node version must support it). The field allows to reduce
  the cost of operating a rollup batcher node. (!13189)
- Support enabling or disabling the `eth_sendRawTransaction` method. (!13162 !13288)
- New threshold encryption sequencer mode of operation. (!12791)
  A new flag `--sequencer-sidecar-endpoint` has been added to the configuration
  command to specify the sequencer sidecar that will be used by the threshold
  encryption sequencer.
  You can run the threshold encryption sequencer with the command:
  - `run threshold encryption sequencer`
- Observer can run using a DSN node bundler as sidecar. (!12841)
  A new optional flag `--bundler-node-endpoint` can be used either
  when configuring the node, or when running the observer, to forward
  transactions to the DSN node bundler sidecar. The bundler is used
  for threshold encryption.
- Add a new experimental mode to run a JSON RPC API endpoint reusing the
  data-dir of a sequencer or observer EVM node. (!13208 !14426)

### Bug fixes

- The RPC `eth_getBlockByNumber` correctly interprets its block parameter
  argument. (!13201)
- Fix upgrade detection. (!13207)

### Breaking changes

### Internal

- Threshold encryption Sequencer connects to sequencer sidecar. (!3140)
- Decouple proposal submission and preblocks monitoring for threshold encryption sequencer. (!13181)
- Add a private rpc `replay_block`. (!13196)
- Open a read-only connection to the SQLite store in the context of the
  `replay` command. (!13456)
- Prevent the node to fallback to the WASM PVM to apply blueprints. (!13671)

## Version for b9f6c9138719220db83086f0548e49c5c4c8421f

### Features

- Add `run <mode>` command and deprecate other commands (!12789). Now
  the preferred way to run any mode are:
  - `run proxy`
  - `run sequencer`
  - `run observer`
- Add config for: (!12920)
  - max blueprints lag
  - max blueprints ahead
  - max blueprints catchup
  - catchup cooldown
  - log filter max number of blocks
  - log filter max number of logs
  - log filter chunk size
- Generalize 'keep-alive' option to all mode and rpc to the rollup
  node, not only when bootstrapping the proxy mode. 'keep-alive' retry
  rpcs to the rollup node when it fails with a connection
  error. (!12800)
- Add command "make kernel installer config <file>" that generates a
  configuration file filled with all the values given in
  argument. (!12764)
- Add a 'keep_alive' line in the configuration. (!12800)
- Add a `verbose` line for logs in the configuration. It sets the
  verbose level of the logs ( `debug`, `info`, `notice`, `warning`,
  `error`, `fatal`). Command line option `--verbose` set it `debug`,
  by default it's `notice`. (!12917, !12345)
- The transaction pool is now removing any transaction that was included more
  than a defined threshold ago (one hour by default). (!12741)
- The transaction pool imposes a limit of users permitted simultaneously
  (4000 by default). (!12749)
- The transaction pool has a threshold of simultaneous transactions allowed per user
  (16 by default). (!12749)
- The transaction pool will reject transactions that can not be contained within
  the size of a blueprint. (!12834)
- The new command `replay blueprint` allows to replay a given blueprint with
  arbitrary kernels. (!12850, !12854, !12855)
- The new command `init config` allows to initialize a new configuration file
  for an EVM node. (!12798)
- Observers now try to reconnect to their EVM endpoint if necessary. (!12772)
- The sequencer can read its smart rollup address from its store and check
  whether it's consistent with the rollup node endpoint. (!12345)

### Bug fixes

- The blueprints streaming RPC accepts to start from the next blueprint level.
  (!12753)
- The RPC `eth_getLogs` now fails when a request exceeds its limits (in blocks
  range or log numbers). (!12905)
- Observers do not stop when the rollup node they are connected to advertise a
  blueprint application for a level in the future. (!12753)
- The observer does not hang anymore when trying to reconnect to a sequencer that
  is still down. (!13024)
- Preimages default directory is now `<data_dir>/wasm_2_0_0` instead of
  `$HOME/.octez-evm-node/_evm_installer_preimages`. (!13070)

### Breaking changes

- The `mode` field of the configuration file has been removed, and replaced
  with `proxy`, `sequencer` and `observer` to configure each mode individually.
  (!12743)
- The EVM node will no longer modify its configuration file based on its
  CLI arguments. (!12799)
- Replace `observer.rollup_node_endpoint`,
  `proxy.rollup_node_endpoint` and `sequencer.rollup_node_endpoint`
  with `rollup_node_endpoint` in the configuration. (!12764)
- Remove `proxy` in the configuration. (!12764)
- `sequencer.sequencer` in the configuration now is the secret key
   instead of the public key hash (e.g. "unencrypted:<edsk...>",
   "encrypted:...", "tcp://...", ). (!12918)

### Internal

## Version for d517020b58afef0e15c768ee0b5acbda1786cdd8

### Features

- Observers now follows a rollup node. (!12547)
- Wait for rollup node to reconnect if it's not available. (!12561)
- Filter out irrelevant events in observer mode. (!12607)
- Forward delayed transactions to observer nodes. (!12606)
- Limit the size of blueprints. (!12666)

### Bug fixes

- Store last known level even if there are no events. (!12590)

### Breaking changes

### Internal

- Improve internal storage handling. (!12551,!12516, !12572, !12627)
- Merge publishable and executable blueprints. (!12571)
- Store delayed transactions in the EVM Context. (!12605)
- Reduce verbosity of events. (!12622)

## Version for 0a81ce76b3d4f57d8c5194bcb9418f9294fd2be1

### Features

- The private RPC server is no longer launched by default. You need to provide
  the parameter `--private-rpc-port` to launch it. (!12449)
- Delayed EVM transactions no longer pay data-availability fee. (!12401)
- Stop block production if the rollup is lagging behind. (!12482)
- Add a private RPC to access the storage. (!12504)
- The kernel logs are now stored under `<data-dir>/kernel_logs/` and events
  are emitted. (!12345)

### Bug fixes

- The transaction pool checks if a transaction can be prepayed before
  inclusion and injection. (!12342)

### Breaking changes

- Delayed Transactions use a dedicated encoding tag in the block in progress. (!12401)
- Record timestamps in executable blueprints. (!12487)

### Internal

- If an error occurs during transaction injection, the trace of errors is
  logged. (!12451)
- Improve resiliency to errors. (!12431)
- Better catchup of possibly missed events. (!12365)
- Improve upgrade detection. (!12459)
- Don't import the delayed inbox when initializing from rollup. (!12506)
- Forbid raw delayed transactions in blueprints sent to the rollup. (!12508)
- Add event for delayed transactions. (!12513)

## Version for 79509a69d01c38eeba38d6cc7a323b4d69c58b94

### Features

- Fetch WASM preimages from a remote endpoint. (!12060)
- When the sequencer evm node diverged from rollup node it fails with
  exit code 100. (!12214)
- Add a new RPC (tez_kernelRootHash) to retrieve the root hash used during the
  last upgrade (!12352)

### Bug fixes

### Breaking changes

### Internal

## Version for 624a144032d6dc6431697c39eb81790bccaacff9

### Features

- Detect when a sequencer upgrade evm event is seen in the rollup node
  kernel. The sequencer upgrade is applied to the sequencer local
  storage. (!12046)
- Version the sequencer store and supports migrations. (!12165)
- Revert message are now propagated in the `data` field of the error. (!11906)

### Bug fixes

### Breaking changes

### Internal

## Version for kernel 20ab639f09a8c7c76f982383c3d9e1f831f38088

### Features

- The sequencer node supports the latest format of the delayed inbox including
  a timeout in number of blocks. (!11811)

### Bug fixes

- Fix `address` parameter to `eth_getLogs` RPC service. (!11990)

### Breaking Changes

- The sequencer and observer nodes now store their locally applied blueprints
  in a sqlite database (as a consquence, the node now depends on `libsqlite3`).
  (!11948)


## Version for kernel c5969505b81b52a779270b69f48b6a66c84da429

### Features

- The sequencer node produces blueprints mentioning the expected context hash
  on which it should be applied. (!11644)
- The sequencer node features a “catch-up mechanism” where it tries sending
  its blueprints again when the difference between its local head and the one
  of the rollup node it is connected to becomes to large. (!11808)
- The sequencer node supports the new features of the delayed inbox, namely the
  timeout mechanism. (!11667)
- The observer node now applies the executable blueprints streamed by other EVM
  node endpoints locally. (!11803)
- The observer node now exposes the JSON RPC API endpoint and the EVM node
  specific services. (!11741)
- Improve the simulation endpoint (in particular to be more meaningful in case
  of errors). (!11816)
- Support parameterizing the limit on the number of active connections. (!11870)
- Support daily log files. (!11752)

### Breaking changes

- The sequencer node now stores its blueprint in `$data_dir/blueprint/execute`
  and `$data_dir/blueprint/publish` (previously, it was only storing the
  executable blueprint in `$data_dir/blueprint`). (!11878)

## Version for kernel 9978f3a5f8bee0be78686c5c568109d2e6148f13

### Features

- Stream the L2 blocks to give a faster and more consistent inclusion of
  transactions on the L1. (!11102)
- Add a keep alive argument that waits until the connection is made with the
  rollup node. (!11236)
- The chunker can also produce blueprints out of the given bytes. (!11497)

### Bug fixes

- Simulation errors are better propagated. (!11381)

### Breaking changes

- The node no longer show the RPC requests by default, you need to specify the
  `--verbose` flag to log the inputs and outputs of RPC. (!11475)
