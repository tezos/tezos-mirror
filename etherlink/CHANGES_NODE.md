# Changelog

## Unreleased

### Features

#### CLI

- Support string interpolation in `--snapshot-file` (for command `snapshot
  export`): `%r` will be replace by the short rollup address, `%R` by the full
  rollup address, and `%l` by the current level. (!14854 !14886)

### Bug fixes

- Fix `init from rollup node` command failing to store items of the delayed
  inbox in its local state when `--omit-delayed-tx-events` is not provided.
  (!14855)
- Delayed transactions are now stored on applied blueprint, instead of internal
  evm event. Otherwise it could result in incoherent blueprints. (!14878)

## Version 0.3 (2024-09-10)

This release primilarly addresses two bugs uncovered in production: the broken
`octez_evm_node_head` metrics for the RPC mode, and the node hanging when
catching-up to the head of its upstream EVM node (in observer mode).

This version is compatible with every kernel deployed on Etherlink Mainnet
Beta.

### Features

#### CLI

- Add the argument `--block-number N` to select the state to patch when using
  the `patch` commands. More precisely, patching the state `N` affects the
  replay of block `N+1`. (!14809)

### Bug fixes

#### RPCs

- Hide `private_rpc` in the output of `GET /configuration`. (!14868)

#### Metrics

- Fix `octez_evm_node_head` for the RPC mode. (!14849)

#### RPCs

- Add state override option to `eth_call` RPC, similar to go-ethereum.
  Limited to fields `balance`, `nonce` and `code`. (!14708)

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
