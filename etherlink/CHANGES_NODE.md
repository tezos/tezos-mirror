# Changelog

## Unreleased

### Breaking changes

### Configuration changes

- Deprecate now irrelevant `log_filter.chunk_size`. (!20332)

### RPCs changes

- Optimize `eth_getLogs` with up to 10x latency improvement when filtering on
  addresses or topics by implementing bloom filter checks in SQLite
  directly. (!20366, !20332)

### Metrics changes

- Add the attribute `etherlink.block.number` to OpenTelemetry traces related to
  instant preconfirmations. (!20369)
- Fix the status of traces for our workers’ handlers remaining unset. (!20379)

### Command-line interface changes

- Update the command that creates an upgrade payload with a delay, it
  can also takes a smart rollup alias or address as a argument.
  (!20302)
- Deprecate now irrelevant argument `--chunk-size`. (!20332)

### Execution changes

- Fix the kernel upgrade detection of the node’s execution runtime, which would
  prevent the node to progress correctly under certain edge cases. (!20135)
- Fix a race condition in the tx queue of the observer node, which could lead
  to transient inconsistent changes. (!20385)
- Remove kernel debug log file generation from trace RPC operations to
  prevent unnecessary disk usage. This affects `trace_transaction_*`
  and `replay_*` files in the `kernel_logs` directory. (!20388)

### Storage changes

### Documentation changes

### Experimental features changes

*No guarantees are provided regarding backward compatibility of experimental
features. They can be modified or removed without any deprecation notices. If
you start using them, you probably want to use `octez-evm-node check config
--config-file PATH` to assert your configuration file is still valid.*

## Version 0.50 (2025-12-19)

This release most notably adds support for [Etherlink 6.1] native execution. In
the event of a successful governance vote, this version will be required to
preserve RPC performances. As a consequence, we strongly advise node operators
to upgrade to this version ahead of the activation of Etherlink 6.1.

This release will not apply any migration to the node's store (version
22), meaning it is possible to downgrade to the previous version.

[Etherlink 6.1]: https://medium.com/@etherlink/announcing-etherlink-6-1-a-bugfix-proposal-for-fa-token-deposits-2cc08ffd6fad

### Command-line interface changes

- Add a collection of command to inspect the contents of an Etherlink instance
  durable storage: `shell cat PATH` to print the contents stored under the
  given `PATH`, `shell ls PATH` to list the subdirectories living under `PATH`,
  `shell tree` to list contents stored living under `PATH` directories in a
  tree-like format, and `shell` to start a REPL allowing users to use `ls` and
  `cat` interactively. See `man shell` for more information. (!20183 !20221)
- Adds support for `--network` to `download kernel` to fallback to default
  per-network preimages endpoint when `--preimages-endpoint` is omitted.
  (!20241)

### Execution changes

- Supports executing Farfadet-r1 natively. (!20285)

## Version 0.49 (2025-12-04)

This release of the EVM node makes it ready for the instant confirmation feature
(also known as sub-block latency) which is not activated by default. It also
improves performance of `eth_sendRawTransactionSync` and adds support for
executing the Farfadet kernel (Etherlink 6.0) natively.

### RPCs changes

- Fix issue when connecting to nodes behind a proxy that can rearrange
  chunks. (!20057)

- Enable concurrent waiting for transaction confirmation in batched
  `eth_sendRawTransactionSync` calls for improved throughput. (!20064)

### Command-line interface changes

- The `download kernel` command now allows to download the upcoming kernel
  Farfadet by its alias `farfadet`. (!20065)

### Execution changes

- Supports executing Farfadet natively. (!20065)

## Version 0.48 (2025-11-24)

This release of the EVM node adds a new JSON-RPC method
`eth_sendRawTransactionSync` as per
[EIP-7966](https://eips.ethereum.org/EIPS/eip-7966) and adds the ground work for
the future sub-block latency feature.

This release will not apply any migration to the node's store (version
22), meaning it is possible to downgrade to the previous version.

### Breaking changes

- Starting with kernel storage version 41, the proxy mode won't serve data of
  blocks older than the last one. (!19677, !19744)

### RPCs changes

- Add new `eth_sendRawTransactionSync` endpoint to our JSON-RPC.
  Defined in [EIP-7966](https://eips.ethereum.org/EIPS/eip-7966). (!19797)

### Experimental features changes

*No guarantees are provided regarding backward compatibility of experimental
features. They can be modified or removed without any deprecation notices. If
you start using them, you probably want to use `octez-evm-node check config
--config-file PATH` to assert your configuration file is still valid.*

- A new flag `experimental_features.preconfirmation_stream_enabled` is added and
  can be set to get sub-block latency for preconfirmations for
  transactions. Note: This feature is only available with a development
  kernel. (!19606)
- Add an optional `block` parameter to `eth_sendRawTransactionSync`.
  Supported values are: `latest` and `pending`. `latest` means waiting the
  block execution while `pending` use the preconfirmation system.
  Default value is `latest`. (!19837)
- Add new `eth_subscribe` subscription named `tez_newIncludedTransactions` that
  notifies transactions that have been chosen to be included in the next block
  (only available with experimental feature
  `preconfirmation_stream_enabled`). (!19810)
- Add new `eth_subscribe` subscription named `tez_newPreconfirmedReceipts`
  that notifies transactions preconfirmed receipts. (!19837)

### Execution changes

- Fix a regression introduced in v0.35 leading the EVM node to accumulate
  unused data in its context. (!19988)

## Version 0.47 (2025-10-31) 🎃

This release improves robustness of the EVM node by protecting all network calls
with a configurable timeout (also preventing transactions from getting stuck in
the queue). It also improves compatibility of RPC `eth_estimateGas` by allowing
state overrides and allows RPC nodes to receive notifications on websockets for
`eth_subscribe`.

This release will not apply any migration to the node's store (version
22), meaning it is possible to downgrade to the previous version.

### Configuration changes

- Every RPC made by the EVM node is now protected by a timeout which can be set
  in a new field `rpc_timeout` of the configuration file (default to
  30s). (!19718)

### RPCs changes

- Prevent transactions from getting stuck when they are being sent to an
  unresponsive node. (!19683, !19718)
- Add support for state overrides in `eth_estimateGas` for EIP-7702, and
  generalize block parameter of `eth_estimateGas` to support block
  hashes. (!19737)
- Websocket connections on an EVM node in RPC mode can now receive events
  with `eth_subscribe`. (!19743)

- Add new RPC endpoint eth_sendRawTransactionSync that
  wait for transaction's execution before returning the
  receipt (!19797). If you run an EVM node and
  an RPC node you need to update both in the same time.

### Monitoring changes

- Websocket client now emits traces when profiling with Opentelemetry is
  enabled. (!19763)

### Command-line interface changes

- New CLI option `--rpc-timeout <seconds>` to set the global timeout for RPCs
  made by the EVM node. (!19718)

### Execution changes

- Use the rollup node RPC `/global/monitor_finalized_blocks` when
  available. (!19569)

### Storage changes

- Shadownet snapshots available to use without warning. (!19726)

## Version 0.46 (2025-10-21)

This release is a hotfix release that addresses issues in the RPCs
`debug_traceTransaction` and `debug_traceBlockByNumber`.

This release will not apply any migration to the node's store (version
22), meaning it is possible to downgrade to the previous version.

### Configuration changes

- Add a `data_dir` field to the config. The field can be overridden by
  the command line or the environment variable `EVM_NODE_DATA_DIR`. (!19619)

### RPCs changes

- `debug_traceTransaction` and `debug_traceBlockByNumber` RPCs now correctly
  use native execution when the node is configured with the `rpcs_only` policy
  (which is the case by default). (!19665)
- Disable [EIP-3607](https://eips.ethereum.org/EIPS/eip-3607) checks during
  `debug_traceCall` simulations in Ebisu. (!19669)

### Experimental features changes

*No guarantees are provided regarding backward compatibility of experimental
features. They can be modified or removed without any deprecation notices. If
you start using them, you probably want to use `octez-evm-node check config
--config-file PATH` to assert your configuration file is still valid.*

- New experimental snapshot format using
  [desync](https://github.com/folbricht/desync) with support for incremental
  snapshots and deduplication, available through commands `experimental snapshot
  ...`. (!19395)

## Version 0.45 (2025-10-17)

This release is a bugfix release following the activation of the Ebisu kernel
(Etherlink 5.0). The fixes are for regressions introduced in the RPCs
`eth_call`, `eth_estimateGas` (_i.e._ simulations), and
`debug_traceBlockByNumber` with `callTracer`. (This fix is applied only on the
native kernel execution which is enabled by default for the RPC server.)

This release will not apply any migration to the node's store (version
22), meaning it is possible to downgrade to the previous version.

### RPCs changes (Ebisu)

- Disabled [EIP-3607](https://eips.ethereum.org/EIPS/eip-3607) checks during
  `eth_call` and `eth_estimateGas` simulations in Ebisu. (!19645)
- Fixed `callTracer` in Ebisu not tracing all transactions in a block if a user
  sent multiples transactions in that same block. (!19650)

## Version 0.44 (2025-10-14)

This release most notably adds support for the Ebisu kernel (Etherlink 5.0),
which was successfully voted through a governance vote by the Tezos bakers. We
strongly encourage node operators to upgrade to this version to maximize
performances of their deployment when the new kernel activates on October 15,
6:30 UTC.

This release will not apply any migration to the node's store (version
22), meaning it is possible to downgrade to the previous version.

### RPCs changes

- Add new GET `/mode` RPC endpoint to expose the current
  operating mode of the EVM node. This enables external monitoring
  tools to query and track node configuration. (!19486)
- Outputs of `txpool_content` and inputs of `injectTransaction` RPCs no longer
  downcast transactions to the legacy format; eip `2930`, `1559` and `7702` are
  now available. (!19494)

### Monitoring changes

- Update opentelemetry library to 0.12 which should fix the issue where a log
  protobuf encoding crashes the node when telemetry is activated. (!19516)

### Command-line interface changes

- The `download kernel` command now allows to download the upcoming kernel
  Ebisu by its alias. (!19461)
- Deprecates the `braeburn` network alias in favor of `shadownet`. (!19503)

### Execution changes

* Caps the number of native threads used by the node to prevent runtime errors.
  (!19479)
- Supports executing Ebisu natively. (!19563)
- Relaxes the condition for completing the EVM node's bootstrap phase to be
  more tolerant to network delays. (!19562)

## Version 0.43 (2025-09-30)

This release of the EVM node fixes an issue with the tx queue which introduced
latency in user requests for `eth_sendRawTransaction`,
`eth_getTransactionCount`, `eth_getTransactionByHash`.

This release will not apply any migration to the node's store (version
22), meaning it is possible to downgrade to the previous version.

### Configuration changes

- Use correct sequencer key from config (you can now define multiple and it will
  adapt). (!19267)

### RPCs changes

- `eth_subscribe newPendingTransactions` is only for transaction that
  are added to the tx_queue. (!19390)
- Fixes high-latency responses in `eth_sendRawTransaction`,
  `eth_getTransactionCount`, `eth_getTransactionByHash`. (!19433)


### Command-line interface changes

- Braeburn is now a supported network on the CLI command, through with argument
`--network braeburn` where applicable.  (!19419)

## Version 0.42 (2025-09-22)

This release of the EVM node is a bug fix release which addresses a regression
introduced by version 0.40 which accidentally removed support for state
overrides on RPCs `eth_call` and `eth_estimateGas`. It also enriches
Opentelemetry traces for better performance analysis.

### Configuration changes

- Add support for multiple sequencer keys in the CLI (only first lexicographic
  one used for now) (!19280, !19302).

### Monitoring changes

- Opentelemetry traces for host function calls can be activated in configuration
  with `"opentelemetry" : { "trace_host_functions" : true }` (!19099)
- Opentelemetry traces for the OCaml garbage collector can be activated in
  configuration with `"opentelemetry" : { "gc_telemetry" : { "enable" : true }
  }` and configured to trace only certain events (!19003)

### RPCs changes

- Fix regression in `eth_call` and `eth_estimateGas` to restore support for
  state overrides. (!19362)

## Version 0.41 (2025-09-15)

This release of the EVM node is a bug fix release which addresses a regression
introduced by version 0.40 on the RPC `eth_getCode`.

### Configuration changes

- Add support for multiple sequencer keys in the configuration
  (only one should be defined for now) (!19251).

### RPCs changes

- Fixes `eth_getCode` regression introduced in the previous version. (!19270)

## Version 0.40 (2025-09-11)

### RPCs changes

- Fixes `eth_getTransactionByHash` always returning a transaction object for
  transactions injected during a sequencer downtime. (!19247)

### Monitoring changes

- Adds two new attributes on every Opentelemetry spans: `service.version`
  (specific to each release of the node) and `deployment.environment` (defined
  in the configuration file or using the `DD_ENV` environment variable). See
  [Datadog configuration][config] for the semantics of these fields.
  (!19156)

[config]: https://docs.datadoghq.com/getting_started/tagging/unified_service_tagging

### Execution changes

- Prevents GCP KMS unresponsiveness to impact the block production of the
  sequencer by adding a timeout of 2s. (!19246)

## Version 0.39 (2025-09-02)

This release of the EVM nodes introduces a new prevalidator which improves
performances of the `eth_sendRawTransaction` RPC. It also offers a new
configuration option to improve performance at the cost of memory consumption
and removes the tx pool which has been replaced by the tx queue since version
0.21.

**There is a breaking change in the configuration file**, and node providers
who have configured the tx queue using the field
`experimental_features.enable_tx_queue` need to update their configuration.

Here is an example of a configuration change to address the breaking change.

```diff
-  "tx_pool_timeout_limit": "10",
-  "tx_pool_addr_limit": "10",
-  "tx_pool_tx_per_addr_limit": "10",
-  "experimental_features": {
-    "enable_tx_queue": {
-      "max_size": 100000,
-      "max_lifespan": 10,
-      "tx_per_addr_limit": "10000"
-    }
+  "tx_pool": {
+    "max_size": 100000,
+    "max_lifespan": 10,
+    "tx_per_addr_limit": "10000"
   },
```

This release will not apply any migration to the node's store (version
22), meaning it is possible to downgrade to the previous version.

### Configuration changes

- New field `"performance_profile"` which, when set to `"performance"`, changes
  the GC parameters of the node to improve responsiveness and latency of the
  node at the cost of a slight increase in memory consumption. (!19025)

### RPCs changes

- Improves performances of the `eth_sendRawTransaction` RPC, dividing the
  average time to process these requests by up to 5 times. (!18989)
- Delays broadcasting blueprints through the `/evm/messages` streamed RPC until
  after the blueprints have been committed to disk. (!18989)
- Remove fallback for `/evm/v2/blueprint/` and `/evm/v2/blueprints/range` making
  version 0.30 deprecated.
- The sequencer now performs full gas limit prevalidation in line with Ethereum
  standards.
  Transactions are rejected if the specified gas limit is insufficient to cover:
    * calldata cost,
    * potential access list cost, and
    * authorization list cost.
  Previously the prevalidation was only checking if the requirement to cover the
  minimum da fees and base intrisic gas cost were covered which was not enough. (!19149)

### Metrics changes

- Fixes the metrics related to the tx pool. Now
  if you activate `tx_queue` it show the size of the `tx_queue`. (!18996)
- The EVM node now exports its logs to Opentelemetry when enabled in the
  configuration file or with `--profiling true`. (!18910)

### Command-line interface changes

- Deprecates the fields `tx_pool_timeout_limit`, `tx_pool_addr_limit` and
  `tx_pool_tx_per_addr_limit` in favor of a top-level `tx_pool`
  top-level field. (!18994)

### Experimental features changes

*No guarantees are provided regarding backward compatibility of experimental
features. They can be modified or removed without any deprecation notices. If
you start using them, you probably want to use `octez-evm-node check config
--config-file PATH` to assert your configuration file is still valid.*

- Removes the `tx_queue` configuration parameters. (!18994)

## Version 0.38 (2025-08-13)

This is a quality of life release, most notably improving the performances of
the `eth_getLogs` and `eth_call` RPC requests. It also contains a bug fix for
the sequencer, which will improve its reliability.

This release will not apply any migration to the node's store (version
22), meaning it is possible to downgrade to the previous version.

## Breaking changes

- Improves performances of `eth_getLogs`. The semantics of the
  `log_filter.chunk_size` has changed in the process, and its default value has
  been bumped from 10 to 1,000 and node operators are encouraged to configure
  their nodes using a value of this order of magnitude. (!18950)

- Changes the default snapshot provider URL. It still the same
  provider but the snapshot is downloaded from another source.
  (!18969)

### RPCs changes

- To stay aligned with Ethereum standards, the sequencer will now refuse
  transactions with a gas limit below the intrinsic gas cost (21,000) plus the
  inclusion fees. (!18923)
- Adds support for type-4 transactions (see
  [EIP-7702](https://eips.ethereum.org/EIPS/eip-7702)) for the Etherlink
  networks with support for Prague. This is not the case for Mainnet and
  Testnet yet. (!18851)
- Encoding of responses for `eth_getLogs` are not blocking anymore, allowing the
  node to continue to process blueprints and handle concurrent RPCs. (!18975)

### Execution changes

- Fixes a race condition in the block production component which could trigger
  a crash of the sequencer. (!18924)
- Parallelize executions of EVM kernel, improving performance of `eth_call`
  RPC. (!18937)

## Version 0.37 (2025-08-04)

This is a bug fix release that improves system resilience and resource
management. It makes the sequencer more robust against GCP KMS
authentication issues and fixes a resource leak in the blueprint
follower where network connections were not properly closed.

This release will not apply any migration to the node's store (version
22), meaning it is possible to downgrade to the previous version.

### Execution changes

- Makes the sequencer resilient to GCP KMS authentication tokens
  invalidation. (!18901)
- Fixes a connection leak in the blueprint follower where the
  monitoring stream was not closed if an internal callback failed.
  This prevents resource exhaustion over time and improves the node's
  long-term stability. (!18908)

## Version 0.36 (2025-07-28)

This is a bug fix release that improves validation and reliability. It enforces
stricter checks on JSON configuration files by returning an error for any
extraneous content, fixes incorrect trace generation for HTTP requests to GCP
KMS, and resolves issues with GCP KMS token refresh.

This release will not apply any migration to the node’s store (version 22),
meaning it is possible to downgrade to the previous version.

### Breaking changes

- Enforces stricter validation for the JSON configuration file. Previously,
  the parser would silently ignore any content that appeared after the first
  valid JSON object. Now, any extraneous data will cause the function to return
  an error. (!18745)

### Metrics changes

- Fixes the traces generated for HTTP requests performed to a GCP KMS. (!18771)

### Execution changes

- Fixes GCP KMS token refresh. (!18795)

## Version 0.35 (2025-07-18)

This is a bug fix release that addresses two known issues in the latest
version.

First, the node now correctly handles receiving duplicated sequencer upgrade
events. There are several scenarios in which this can occur, so this fix is
necessary to ensure smooth transitions for the sequencer operator. Second, the
node will no longer format addresses in its RPC responses using the EIP-55
checksum format. This change, introduced in the previous version, was not
consistent with the behavior of Ethereum nodes and has therefore been reverted.

We strongly recommend that node providers upgrade to this version.

This release includes a single migration to the internal store (version 22),
which means it is not possible to downgrade to the previous version.

### Breaking changes

- Revert outputting EIP-55 encoded addresses in the RPC responses. This was not
  a standard behavior for nodes of EVM-compatible chains. (!18757)

### Execution changes

- Makes the node resilient to receiving a sequencer upgrade event twice.
  (!18713)
- Forces the installation of the full kernel on new data directory creation.
  (!18709)
- The sequencer now performs signing and applying the blueprints concurrently.
  (!18711 !18712)

## Version 0.34 (2025-07-15)

This release of the EVM node notably adds support for executing natively the
Etherlink 4.1 release (also known as Dionysus first revision). It is the
recommended version to use for networks running this version of the Etherlink
kernel, and we advise all node providers to upgrade ahead of Etherlink Mainnet
and Etherlink Testnet upgrades.

### RPCs changes

- `GET /health_check` will now fail if the database connections used by the EVM
  node are stalled. (!18667)
- RPC responses now output [EIP-55 compliant addresses][eip-55]. (!18676)

[eip-55]: https://eips.ethereum.org/EIPS/eip-55

### Execution changes

- Supports executing Dionysus R1 (Etherlink 4.1) natively. (!18683)

## Version 0.33 (2025-07-10)

This release of the EVM node fixes an issue with the configuration for sequencer
keys on GCP KMS and makes the sequencer stop block production ahead of a
sequencer update.

This release will not apply any migration to the node’s store (version 21),
meaning it is possible to downgrade to the previous version.

### Configuration changes

- Fix encodings for sequencer key when using a remote key on GCP KMS. (!18642)
- Add a new configuration option for the sequencer to stop block production
  ahead of a sequencer upgrade. See `describe config` or `man run sequencer`.
  (!18605)

### Execution changes

- Add support for GCP KMS `EC_SIGN_SECP256K1_SHA256` keys. (!18618)
- Add a new command `show gcp key` which can be used to retrieve Tezos-specific
  (b58-encoded public key, Tezos implicit account) and Ethereum-specific (EOA
  address) information about a key stored in a GCP KMS. (!18617 !18618)
- Halt blocks production five minutes before a planned upgrade of the sequencer
  operator. (!18605)

## Version 0.32 (2025-07-08)

This release of the EVM node adds support for the sequencer to sign blueprint
using a key stored on GCP KMS. It also allows to download the upcoming kernel
`dionysus-r1` by its alias and fixes an issue in the metrics introduced in
version 0.31.

This release will not apply any migration to the node’s store (version 21),
meaning it is possible to downgrade to the previous version.

### Breaking changes

- Remove CLI arguments and configuration options related to the threshold
  encryption mode. This mode was never used in production, so it should not
  affect existing deployment as long as they have not configured in their
  configuration file. You can run `check config` to make sure you are not
  concerned by the breaking change. (!18535)

### Configuration changes

- Adds `gcp_kms` top-level field to configure the interaction with a GCP KMS.
  See `describe config` for more information about the available options.
  (!18585)

### Metrics changes

- Fix the finalized number metrics for EVM nodes not tracking a rollup
  node. (!18603)

### Execution changes

- Support signing blueprints via a key stored in a GCP KMS. (!18527 !18586
  !18587)
- Support downloading Etherlink 4.1 kernel (Dionysus first revision,
  `dionysus-r1`) by name. (!18594)

## Version 0.31 (2025-06-26)

This release of the EVM node allows the `finalized` block to be available on
nodes which are not tracking a rollup node and adds new RPCs for future
sequencer upgrades. It is also now easier to enable profiling on the CLI or in
the configuration.

The node will apply one migration to its internal store (version 21), meaning
it is not possible to downgrade to the previous version.

### Configuration changes

- Sandbox mode can either generates automatically the used sequencer
  key, or uses the one given in argument. (!18385)
- Opentelemetry profiling is now enabled by default when then `"opentelemetry"`
  field of the config is present. `"opentelemetry" : true` also now enables
  Opentelemetry profiling with the default parameters. (!18411)
- New CLI argument/switch `--profiling` to enable or disable opentelemetry
  profiling. (!18411)

### RPCs changes

- Observers not tracking a rollup node now receive finalized L2 block
  levels through the EVM endpoint, enabling `eth_getBlockByNumber`
  with the `"finalized"` parameter to be handled correctly (!18355).
- Add new GET RPCs `/evm/v2/blueprint/`, `/evm/v2/blueprints/range`
  whose purpose is to fetch the contents of blueprints with a complete
  list of events, and notably the sequencer upgrade event. (!18367, !18471)

### Metrics changes

- Opentelemetry traces now have the namespace `emv_node` and include an instance
  id which can either be user provided (in the `opentelemetry` configuration) or
  is generated on the fly. (!18441)

### Execution changes

- Node correctly applies an sequencer upgrade EVM event when it sees
  one. (!18286)

## Version 0.30 (2025-06-18)

This release of the EVM node adds a new database connection pool implementation
for the store which should eliminate lock ups in the RPC server. The node now
always checks the gas limit to avoid sending invalid transactions, and
compatibility is improved with older tools and hardhat. An extensive profiling
instrumentation was added which will allow developers to precisely analyze and
improve performance.

This release will not apply any migration to the node’s store (version 20),
meaning it is possible to downgrade to the previous version.

### Configuration changes

- New field `"db"` which allows to configure some parameters for the database
  such as the connection pool size and the connection reuse limit. (!18235)
- New `"enable_revm"` config key feature flag to change the EVM execution's VM
  from Sputnik to REVM. (!18384)

### RPCs changes

- Add field `gasPrice` to EIP-1559 transactions for compatibility with older
  tools and hardhat in particular. (!18255)
- After the next kernel upgrade, the system will accept transactions
  submitted via `eth_sendRawTransaction` even when users specify gas
  limits that exceed the maximum allowed threshold. During execution,
  any gas limit above the maximum will be automatically capped at the
  maximum permitted value. (!18179)

### Metrics changes

- Extensive profiling with OpenTelemetry traces. Profiling is disabled by
  default and can be activated with the configuration field
  `opentelemetry`. (!18227 !18229 !18279 !18290 !18291
  !18312 !18315 !18328 !18331 !18337 !18347 !18348 !18368)

### Execution changes

- Node validation always checks that the gas limit are set correctly.
  (!18236)
- Gas limit validation changes. After the next kernel upgrade, the
  validation mechanism that automatically rejects transactions with
  over-estimated gas limits exceeding the maximum threshold will be
  disabled. (!18179)

### Storage changes

- New database connection pool which should be more efficient and not suffer
  from locks. (!18235)

### Experimental features changes

*No guarantees are provided regarding backward compatibility of experimental
features. They can be modified or removed without any deprecation notices. If
you start using them, you probably want to use `octez-evm-node check config
--config-file PATH` to assert your configuration file is still valid.*

## Version 0.29 (2025-06-06)

This release of the EVM node more accurately computes the inclusion fee gas
(e.g. in `eth_estimateGas`) which should sort out issues with transactions being
ignored by the sequencer. This release also fixes RPCs encoding of compressed
transactions to better follow the spec and fixes a regression with the RPC
server (which manifests as non responding RPCs) introduced by a dependency
upgrade in 0.28. Finally, this release adds a new RPC and metric to better track
the _gas consumed by execution_ of transactions and blocks (_i.e._ without
inclusion gas) which impacts gas prices.

This release will not apply any migration to the node’s store (version 20),
meaning it is possible to downgrade to the previous version.

### RPCs changes

- Aligns the validation logic of the node wrt. the execution gas limit with the
  one implemented in Dionysus. That is, the execution gas is now derived from
  the gas limit of a transaction by removing the inclusion fees paid with the
  minimum base fee per gas (not the current one).  (!18184)
- Fix encoding of signature fields `r` and `s` in transactions to use
  [quantities](https://ethereum.org/en/developers/docs/apis/json-rpc/#eth_gettransactionbyhash),
  _i.e._ compressed without leading zeros. (!18178)
- Adds the Etherlink-specific `tez_getTransactionGasInfo` RPC to determine how
  many units of gas were used to execute a given (included) transaction, and
  how many units of gas were paid for covering the inclusion fees. (!18212)

### Metrics changes

- Advertises the execution gas used on new blueprint application. (!18213)

### Execution changes

- `replay blueprint` and `replay blueprints` now log the execution gas, process
  time and whether or not the produced block diverged from the canonical one
  for each replayed blueprint. (!18215)

### Dependencies

- Downgrade [caqti](https://github.com/paurkedal/ocaml-caqti) from 2.2.4 to
  2.1.2 which fixes instabilities (deadlocks) in the RPC server. (!18226)

## Version 0.28 (2025-05-28)

This is a re-release of version 0.27 with the additional stabilized websockets
server (which can now be enabled simply with `--ws`). Binaries and images for
version 0.27 were not built due to an issue in the CI.

This release will not apply any migration to the node’s store (version 20),
meaning it is possible to downgrade to the previous version.

### Configuration changes

- Stabilize websocket server. The fields in `experimental_features` related to
  websockets are now deprecated in favor of a top level `websockets` field
  (which can contain additional configuration parameters for the websockets
  server). A new CLI parameter `--ws` allows to activate the websockets server
  without editing the configuration. (!18145)

## Version 0.27 (2025-05-26)

This is a hot fix release to address an issue in the encoding of transaction
type in JSON RPC responses.

This release will not apply any migration to the node’s store (version 20),
meaning it is possible to downgrade to the previous version.

### RPCs changes

- Fix transaction type encoding to use a compact hex encoding following the
  [standard](https://ethereum.org/en/developers/docs/transactions/#typed-transaction-envelope). (!18132)

## Version 0.26 (2025-05-20)

This release of the EVM node adds support for executing natively the Dionysus
kernel. This improves performance for computationally intensive RPCs like
`eth_call` when the update is activated on the network. This releases also
improves compatibility of the EVM node with Ethereum tooling like
anvil/cast. The docker images produced as part of this release _do not_ suffer
from [CVE-2025-4207](https://nvd.nist.gov/vuln/detail/CVE-2025-4207) and
[CVE-2025-29087](https://nvd.nist.gov/vuln/detail/CVE-2025-29087) anymore.

This release will not apply any migration to the node’s store (version 20),
meaning it is possible to downgrade to the previous version.

### RPCs changes

- `eth_call` does not returns the `data` field when it's empty if it
  fails during the execution. (!17893)
- RPCs that return transaction objects now include access lists when present,
  per EIP-1559 and EIP-2930. (!18062)

### Metrics changes

- Aggregate performance metrics for all child processes (including Irmin
  GC). (!17973)

### Execution changes

- Supports executing Dionysus natively. (!17975 !17976 !17977)

### Command-line interface changes

- Renames `--bootstrap-account` and `--bootstrap-balance` to
  `--eth-bootstrap-account` and `--eth-bootstrap-balance` for the `make kernel
  installer config` and `make l2 kernel installer config` commands. The old
  behavior is conserved (!17710)
- Adds support for downloading Dionysus kernel with`download kernel dionysus`.
  (!17974)

## Version 0.25 (2025-05-05)

This is a hot fix release to improve end-user experience with `eth_gasPrice` and
fix an issue with daily logs rotation.

This release will not apply any migration to the node’s store (version 20),
meaning it is possible to downgrade to the previous version.

If you used an EVM node v0.22 or later using the experimental RPC mode please
delete the log files prefixed with `rpc-<port>-` from
`<evm-node-datadir>/dailylogs/`.

### RPCs changes

- Changes `eth_gasPrice` to make it more resilient to gas price changes. The
  RPC will now anticipate gas price increases. (!17921)

### Command-line interface changes

- Adds `--kernel-verbosity` to `run sandbox` to control the level of logs
  emitted by the kernel. Note that printing kernel logs to the standard output
  still requires to set the `RUST_LOG` environment variable to
  `octez_evm_node_wasm_runtime::write_debug=trace`. (!17909)

### Experimental features changes

*No guarantees are provided regarding backward compatibility of experimental
features. They can be modified or removed without any deprecation notices. If
you start using them, you probably want to use `octez-evm-node check config
--config-file PATH` to assert your configuration file is still valid.*

- The event for the rpc mode when forwarding transaction was
  incorrectly named `forwarding_error`. It's now renamed
  `forward_transaction`. (!17910)
- The daily logs of the experimental RPC node are written in a
  different directory prefixed by `rpc-<port>`, and not in the same
  log directory as the main node. Since v0.23 the daily logs of the RPC
  node was written in the same directory, and the log files of the main node
  `dailylog-<date>.log` were incorrectly deleted on rotation. If you
  used an RPC EVM node v0.22 or later, please delete files prefixed
  with `rpc-<port>-` from `<evm-node-datadir>/dailylogs/`. (!17923)

## Version 0.24 (2025-04-30)

This is a hot fix release to address a regression in the `eth_getBlockBy*`
family of RPCs. Providers having updated to version 0.21 and higher are
encouraged to upgrade in order to restore full services.

This release will not apply any migration to the node’s store (version 20),
meaning it is possible to downgrade to the previous version.

### RPCs changes

- Fixes the `eth_getBlockBy*` family of RPCs being unable to return blocks
  recorded with a version older than 0.21. (!17899)

## Version 0.23 (2025-04-28)

This release of the EVM node brings improvements to the robustness, notably
with more graceful exits on connection issues which are experienced by end users
and partners. It also brings additional options to the sandbox mode which allows
the development team to better test and evaluate the features of the upcoming D
kernel.

This release will not apply any migration to the node’s store (version 20),
meaning it is possible to downgrade to the previous version.

This is the minimal required version to run when the Dionysus kernel is active.

### Breaking changes

- Replaces the command `download kernel with root hash` by `download kernel`.
  In addition to supporting root hash prefixed by `0x` (something the previous
  command was rejecting), `download kernel` also accept known kernel names
  (currently, `bifrost`, `calypso` and `calypso2`). (!17696)

### RPCs changes

- Blocks now include placeholders for (!17673):
  - `withdrawals` and `withdrawalsRoot` (EIP-4895)
  - `blobGasUsed` and `excessBlobGas` (EIP-4844)
  - `parentBeaconBlockRoot` (EIP-4788)

### Command-line interface changes

- Adds `--private-rpc-port` to `run observer` to enable the private RPC server
  from command-line. (!17762)
- Adds `--replicate` to `run sandbox` to provide an easy way to make local
  tests on a local endpoint with realistic traffic. (!17845)
- Adds `--disable-da-fees` to `run sandbox` to provide an easy way to disable
  DA fees in the local sandbox. (!17852)
- Previously when submitting the same transaction multiple times, if
  the first fails to be included by the sequencer then all following
  will fails. Now when submitting multiple time the same transaction,
  the first occurrence that could succeed does. (!17740)

### Execution changes

- Exits with 103 when the node fails to follow the head of its upstream node.
  (!17857)
- Fixes an issue where the node would hang when the upstream EVM node endpoint
  returned a 502 Bad Gateway error in observer or RPC mode. (!17856)

### Experimental features changes

*No guarantees are provided regarding backward compatibility of experimental
features. They can be modified or removed without any deprecation notices. If
you start using them, you probably want to use `octez-evm-node check config
--config-file PATH` to assert your configuration file is still valid.*

- The daily logs of the experimental RPC node are written in the same
  directory but in a different file. (!17836)
- RPC node now logs the transaction it forwards when using the private
  inject endpoint of its local node. (!17836)

## Version 0.22 (2025-04-14)

This release of the EVM node adds support for executing natively the Calypso2
kernel (the update which was activated on mainnet on block 10,453,254). This
will improve performance for computationally intensive RPCs like `eth_call`
(where the native execution is used by default).

This release will not apply any migration to the node’s store (version 20),
meaning it is possible to downgrade to the previous version.

### Execution changes

- `--profile` now has two different modes; `minimal` and `flamegraph`.
  `minimal` provides a file to which it streamlines tick and gas
  consumption, `flamegraph` creates a flamegraph indexed on tick
  consumption (!17608)
- Adds support for Calypso2 native execution. (!17693)

## Version 0.21 (2025-04-09)

This release finalizes the new experimental `tx_queue` which allows to send
multiple transactions to be included in a single block. It also improves the
bootstrapping process with snapshots and considerably speeds up the catch-up
phases.

This release will not apply any migration to the node’s store (version 20),
meaning it is possible to downgrade to the previous version.

Configuration files created with versions v0.17 and v0.18 might cause encoding
errors related to `l2_chain` or lead to `Mismatched_chain_family`
issues. Operators of EVM nodes should verify their configurations by running:

```
octez-evm-node check config
```

If issues persist after verification, remove the `l2_chains` entry from the `experimental_features` section of your configuration file.

By default, the configuration file (`config.json`) is located in your `data_dir`
directory.

### RPCs changes

- Fixes `mixHash` being renamed into `prevRandao` in the output of our JSON RPC
  API server. (!17394)
- Adds `GET /evm/blueprints/range?from_level=l&max_count=c` which returns a
  sequence of at most `c` consecutive blueprints starting from (and containing
  at least) level `l`. If the node does not have the blueprint for level `l` in
  its store, the RPC returns a 404 error instead of an empty list. `c` default
  to `1` if not specified and reduced to `500` if superior. (!17592)

### Execution changes

- `replay blueprint` can export tick and gas consumption with `--profile`. (!17441)
- `replay blueprint` can disable data availability fees with `--disable-da-fees`. (!17441)
- Adds `replay blueprints from <l1> to <l2>` to replay a range of blocks.
  (!17441 !17598)
- Fixes a bug (which negatively impacted performance) in the EVM events batch
  fetching in case the rollup node has ACL filtering for the required RPC.
  (!17572)
- Speeds up catch-up by fetching up to 500 blueprints at once when upstream EVM
  node allows it before applying them. (!17593)
- Recovers automatically when the remote EVM node streams unexpected blueprints
  from the future. (!17621)

### Storage changes

- Snapshots are downloaded and extracted simultaneously which uses a smaller
  disk footprint and is slightly more efficient. (!17407 !17420)
- Command `snapshot info` now supports remote snapshot URLs and can print the
  snapshot header without downloading the whole snapshot. (!17407)
- Allow to import snapshots from standard input with `-`. (!17463)
- Use `curl` (faster) to download snapshots when available. (!17477)

### Experimental features changes

*No guarantees are provided regarding backward compatibility of experimental
features. They can be modified or removed without any deprecation notices. If
you start using them, you probably want to use `octez-evm-node check config
--config-file PATH` to assert your configuration file is still valid.*

- Websocket server (with Resto backend) can be configured to rate limit messages
  with the field `experimental_features.websocket_rate_limit`. (!17510 !17523)
- Add support for the `tx_queue` in RPC mode. (!17254)
- Add support for the `tx_queue` in proxy mode. (!17246)

## Version 0.20 (2025-03-21)

This release contains general UX improvements with the configuration and command
line interface documentation. Configuration files can now be placed outside the
data directory and important options can be set up using environment variables,
both of which should facilitate cloud deployments.

The node will apply one migration to its internal store (version 20), meaning
it is not possible to downgrade to the previous version. This migration can take
a couple seconds depending on the machine and is necessary to better track
relationships between L2 blocks and L1 blocks (information which is exposed in
new RPCs).

EVM nodes with configuration produced by v0.18 or v0.17 may emit the
warning `the configuration for the l2_chains experimental feature was
ignored`. In this case, remove the field `experimental_features.l2_chains` from
your configuration file to be compatible with future versions.

### Configuration changes

- Fixes `check config` ignoring `--data-dir` when passed `--config-file`.
  (!17214)
- It is now possible to pass `--config-file` to the commands of the EVM node,
  to locate the configuration file outside of the data directory. (!17216)
- Supports selecting the data-directory using the `EVM_NODE_DATA_DIR`
  environment variable. If `--data-dir` is passed and `EVM_NODE_DATA_DIR` is
  set, `--data-dir` is selected. (!17282)
- Supports selecting the configuration file using the `EVM_NODE_CONFIG_FILE`
  environment variable. If `--config-file` is passed and `EVM_NODE_CONFIG_FILE`
  is set, `--config-file` is selected. (!17282)
- Supports selecting the network using the `EVM_NODE_NETWORK` environment
  variable. If `--network` is passed and `EVM_NODE_NETWORK` is set, `--network`
  is selected. (!17282)

### RPCs changes

- Added a `/describe` endpoint compatible with `octez-client`. (!17190)
- New Etherlink specific method for ``eth_subscribe``: ``tez_l1L2Levels``
  to be notified of L2/L1 levels associations when an L2 block is
  finalized. (!16923)
- New Etherlink specific JSONRPC ``tez_getFinalizedBlocksOfL1Level``
  to retrieve the finalized L2 levels for a given L1 level. (!16923)

### Execution changes

- Validates Ethereum addresses passed as arguments to existing CLI commands
  like `make kernel installer config`. (!17137)

### Storage changes

- Augment L1/L2 relationships with a new table to keep more accurate
  information. (!16848)

### Documentation changes

- Improves and cleans up the `man` command of the node. (!17289 !17311 !17282
  !17295 !17331 !17317 !17309)

### Experimental features changes

*No guarantees are provided regarding backward compatibility of experimental
features. They can be modified or removed without any deprecation notices. If
you start using them, you probably want to use `octez-evm-node check config
--config-file PATH` to assert your configuration file is still valid.*

- With the `tx_queue` feature enabled in an observer node, for the RPC
  `eth_getTransactionCount` at the `pending` block, it will return the
  next available nonce found in the `tx_queue`. It also works for
  transactions that have been already forwarded to the upstream node
  but not yet confirmed. (!!16829)
- `tx_queue` limits the number of transaction one user can submit.
  This limits is for pending transactions the node has seen.
  configurable with `tx_per_addr_limit`. (!16903)
- An sequencer EVM node can uses the tx_queue to speed the inclusion
  of transaction. (!17134 !17100 !17109 !17211)
- A proxy EVM node can now use the tx_queue. (!17246)
- A RPC EVM node can uses the tx_queue. (!17246)
- `tx_queue` now has a maximum number of transactions. (!17083)
- Observer nodes can now be run with `periodic_snapshot_path` defined in
  the configuration. It exports a snapshot to the given path every time they
  perform a garbage collection. (!17038)
- `Txpool_content` RPC now works for the `tx_queue` as well, it displays its
  information mapped into a `tx_pool` object to avoid breaking changes. (!17102)

## Version 0.19 (2025-03-10)

This release contains a number of quality of life improvements, notably related
to snapshots. The integration between the Octez EVM node and our [snapshot
service] is deepened, and it is now possible to initialize a new data directory
for arbitrary history mode using `run observer --network NETWORK --history
HISTORY --init-from-snapshot`. To a large extent, this release fully concludes
the stabilization of the new block storage feature initiated in version 0.16.

[snapshot service]: https://snapshotter-sandbox.nomadic-labs.eu/

This release will not apply any migration to the node’s store (version 19),
meaning it is possible to downgrade to the previous version.

### Breaking changes

- The default snapshot filename has been changed from
  `evm-snapshot-<rollup_address>-<head>` to
  `evm-<history_mode>-snapshot-<rollup_address>-<head>`. (!16946)
- When an explicit snapshot filename is omitted, `snapshot export` will append
  `.gz` to compressed snapshots and will not add any file extension to
  uncompressed snapshots. Previously, the node was not adding any extension to
  compressed snapshots, and was appending `.uncompressed` to uncompressed
  snapshots. (!16957)
- The node now advertises the level of each log by default. It can be disabled through
  the use of [environment variables][docs]. (!16976)
- Replaces `--initial-kernel` with `--kernel` in `run sandbox`, to patch the
  kernel on pre-existing data directories. (!17148)

[docs]: https://octez.tezos.com/docs/user/logging.html#environment-variables

### Configuration changes

- Commands emitting logs now systematically comply with the `verbosity`
  configuration option. (!16975)
- Fixes experimental features being set by `init config`. (!17078)
- Fixes `--private-rpc-port` being ignored by the `run observer` command.
  (!17078)

### RPCs changes

- `GET /health_check` now returns an error if the node does not keep up with
  its remote EVM node. More precisely, the node will fetch the current head of
  the upstream node every 60 seconds, and compare it with its own. If the
  difference is larger than a given `drift_threshold` (defaults to 100), then
  the RPC returns an error. To use a different threshold, it is possible to
  specify it in the URL, *e.g.*, `GET /health_check?drift_threshold=10` to set
  the threshold to 10. (!17025)

### Metrics changes

- Deduplicates metrics `block_process_time_histogram_bucket{le="0.100000"}`
  (!17035)

### Execution changes

- Adds `--network` and `--init-from-snapshot` to `run sandbox` to simplify
  starting a sandbox for a supported network. For instance, `run sandbox
  --network mainnet` will start a new sandbox using Mainnet’s initial kernel,
  while `run sandbox --network testnet --init-from-snapshot` will start a new
  sandbox from a `rolling:1` snapshot downloaded from the default snapshots
  provider. (!17098)
- Adds `--fund ADDRESS` to `run sandbox`, where `ADDRESS` is a wallet address
  in hexadecimal form that will be provisioned with 10,000 native tokens inside
  the sandbox. The parameter can be used multiple times. (!17104)

### Storage changes

- Adds support for the `%h` variable in the `snapshot export` and `run observer
  --init-from-snapshot` commands, to generalize over the current history mode
  of the node. (!16946)
- Ensures `snapshot export` does not leave temporary files behind when
  interrupted. (!16985)
- Observer `--init-from-snapshot` now also accepts a path to an existing
  snapshot. (!16963)
- Supports specifying an expected history mode in `run observer` with the
  `--history` argument. The node will refuse to start if its context is using a
  different history mode, but it can be used with `--init-from-snapshot` to
  download a snapshot of the expected mode. (!16979)
- Observer `--init-from-snapshot` now defaults to our new snapshot services.
  Snapshots are generated nightly (Europe time) for each history mode. (!17052)

### Experimental features changes

*No guarantees are provided regarding backward compatibility of experimental
features. They can be modified or removed without any deprecation notices. If
you start using them, you probably want to use `octez-evm-node check config
--config-file PATH` to assert your configuration file is still valid.*

- Adds a configuration for the `tx_queue`. (!16903)
- With the `tx_queue` feature enable in an observer node, the RPC
  `eth_getTransactionByhash` returns the transaction found in the `tx_queue`,
  it also works for transaction that have been already forwarded to the
  upstream node. (!16829)
- The `tx_queue` respect the configuration field `keep_alive` for it's RPC.
  (!16894)
- `tx_queue` clears itself when a delayed inbox flush has happened. (!17091)

## Version 0.18 (2025-02-24)

This releases notably includes support for executing the Calypso kernel
natively. Besides, the EVM node now uses native execution for RPCs by default
(was disabled before).

This release will not apply any migration to the node’s store (version 19),
meaning it is possible to downgrade to the previous version.

### Features

- Observer nodes now perform a lighter validation on submitted transactions.
  (!16713)
- Supports native execution for the Calypso kernel. (!16728 !16729 !16734)
- Native execution is now enabled by default for RPCs (can be disabled with
  `--native-execution-policy never` or with
  `kernel_execution.native_execution_policy: never` in the configuration file).
  (!16881)
- Adds the `octez_evm_node_pruning_history` metrics which is set to 1.0 when
  the node is pruning the history past its specified retention period, and 0.0
  otherwise. (!16908)

#### Experimental

*No guarantees are provided regarding backward compatibility of experimental
features. They can be modified or removed without any deprecation notices. If
you start using them, you probably want to use `octez-evm-node check config
--config-file PATH` to assert your configuration file is still valid.*

- RPC nodes now perform a lighter validation on submitted transactions.
  (!16713)
- RPC nodes can bypass transaction validation of their read/write node if it
  exposes a private RPC server. (!16664)
- Adds a new experimental feature `enable_tx_queue` to replace the tx pool by a
  tx queue to improve performance and simplify the code. (!16812)

### Bug fixes

- `eth_getLogs` now accepts `null` as a valid value for all its filter
  parameters. (!16808)
- Fixes switching to a history mode with a smaller retention period.
  Previously, the command would succeed, but the node would keep using the
  previous retention period. (!16798)
- Fixes exporting a snapshot using the `/tmp` directory to generate temporary
  files. (!16919)
- Fixes the node crashing when it cannot predownload a kernel ahead of an
  upgrade activation. It will now retry a minute later. (!16911)

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
  data directories.
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
- Disallow importing of snapshots in the legacy format, unless the flag
 `--force` is provided, or the data dir already contains block storage in the
 legacy format. (!16438)

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
