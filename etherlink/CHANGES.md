# Changelog

Note that the release for the kernel and the node are now separated, and both
have their respective changelogs (see [CHANGES_KERNEL.md](CHANGES_KERNEL.md) and
[CHANGES_NODE.md](CHANGES_NODE.md)).

## Version 43bbcea48eca22511d9bda0a739302985fab9d24

### EVM Kernel

- Change the L1 type of the rollup into an entrypoint style: `or (or (pair bytes
  (ticket (pair nat (option bytes)))) bytes) bytes`. (!10735)
- The L1 type now expects FA2.1-compatible tickets. (!10738)
- Add support for precompiled contract 0x01 (ecRecover) (!10926)

### EVM Node

- Add support for `eth_getLogs`. (!10624)
- Replaced the argument `--version <mode>` to a flag `--devmode` for dev, and
  mode production by default. (!10821)
- Add configuration file, automatically created on the first run of the EVM
  node. Subsequent executions of the node with different values for the
  arguments will update the configuration file accordingly. (!10790)
- The EVM Node now has a transaction pool that supports injection of transaction
  with nonce in the future and transaction replacement by fees. (!10289, !10290,
  !10291, !10625, !10627, !10628, !10629)

### Bug fixes

- Transaction's log indices correspond to the position in the block. (!10615)

### Breaking changes

- L1 Entrypoint of the rollup has changed and now has type `or (or (pair bytes (ticket (pair nat (option bytes)))) bytes) bytes`.

### Internal

- `evm-evaluation-assessor` binary was added to run `ethereum/tests`. (!10798)
- Conservative tick model for gas per opcodes. (!10715)
- Safe tick model for precompiled contracts. (!10748)

## Version 7ff44c30366c3d2b860413905a2253522454fe5e

### EVM Kernel

- Upgrade SputnikVM to version 0.39.1 and `primitive_types` to version 0.12.1 (!10043)
- Upgrade ethereum crate to version 0.14.0
- Store the `base_gas_per_fee` and use a default value of 21000. (!10234)
- Transaction can no longer be overwritten nor reincluded (implicit transaction size limit now
  becomes ~1200 chunks). (!10337)
- Produce outbox messages withdrawing funds. (!10063)
- EVM execution configuration was bumped from London to Shanghai. (!10591)

### EVM Node

- Remove `mockup` mode previously used for internal testing only. (!10406)
- RPCs related to blocks now return default values for the POW fields instead of
  mockup. (!10427)
- The binary `evm-proxy-server` is renamed to `evm-node`. (!10656)
- Renamed node's argument `mode` to `version`. (!10657)
- The node's mode is no longer `proxy` by default. The command becomes `run proxy ..` (!10658)

### Bug fixes

- The internal kernel version is now automatically updated at each upgrade. (!10321)
- Genesis parent hash is 0xff..ff, all parent hashes are now unique. (!10332)
- Fix the bug that nested contract calls returns `OutOfGas` (!10283)

### Breaking changes

### Internal

- Indexes of accounts are now correctly checked, they are no longer duplicated. (!10395)
- Remove POW related fields from L2 blocks. (!10421)
- L2 blocks' RLP encoding includes all the fields. (!10386)
- Block hash is no longer in Tx object, receipt and BIP. (!10520)
- Compute real block hashes. (!10442)
- EVM evaluation framework (testing for the EVM execution) is now available. (!10221)
- Compute {state,transactions,receipt}_root in block. (!10545)
- Upgrade nonce is no longer needed, only the preimage root hash. (!12345)

## Version fc06d63568b1be253088ff5d6e422fd80ed3d2c2

### EVM Kernel

- Fallback mechanism if stage zero fails. (!9732)
- Switch to `ExternalMessageFrame` protocol for external messages. (!9687)
- Support EIP-2930 transaction serialization format. (!9555)
- Support EIP-1559 transaction serialization format. (!9596)
- The kernel can no longer be administrated by a L2 dictator key, instead by a
  L1 smart contract. It will consider upgrades messages coming from a specific
  address defined in its storage. (!9927)
- Adds a new type of message in simulation mode, to verify that a transaction is valid by checking if the nonce is neither too low nor too high. (!9679)
- Simulate if a transaction has a correct chain id. (!9752)
- Fees are now paid by the sender. (!9480)
- Unused gas is refunded after succesful transactions. (!9915)
- Impose a maximum gas limit per transaction based on the tick model. (!10094)
- Transaction logs and bloom filter are returned in a transaction receipt. (!10019)
- Support EIP-3607: reject transactions from senders with deployed code. (!10292)

### EVM Node

- Switch to `ExternalMessageFrame` protocol for external messages. (!9687)
- Add arguments to enable CORS headers. (!9753)
- Add an optional `mode` argument to switch from the proxy on production
  to the one on development. (!9940)
- `eth_sendRawTransaction` checks that the nonce of the transaction is neither too low nor too high. (!9679)
- `eth_sendRawTransaction` checks if the chain id is correct. (!9752)
- Use different files to store kernel logs, differenciate the different kind of
  simulations. (!9664)
- Add support for `eth_getBlockByHash`. (!10061)
- Add support for `eth_getTransactionByBlockHashAndIndex` and `eth_getTransactionByBlockNumberAndIndex`. (!10068)
- Add an optional `--verbose` argument. Displays the responses to requests before
  returning them. (!10116)
- Add support for `eth_getBlockTransactionCountByHash` and `eth_getBlockTransactionCountByNumber`. (!10101)
- Add support for `web3_sha3`. (!10112)
- Add support for `eth_getUncleCountByBlockHash` and `eth_getUncleCountByBlockNumber`. (!10131)
- Add support for `eth_getUncleByBlockHashAndIndex` and `eth_getUncleByBlockNumberAndIndex`. (!10134)
- Add 2300 unit of gas for the RPC `eth_estimateGas` to account for the EIP2200. (!10176)
- Add support for `eth_gasPrice`. (!10195)
- Add support for `eth_getStorageAt`. (!10214)

### Bug fixes

### Breaking changes

- External Messages must now be framed using `ExternalMessageFrame` (adds an additional prefix byte). (!9687)
- The EVM rollup accepts tickets wrapping tez instead of ctez. (!9982)
- L2 blocks are now stored using their RLP encoding. (!9759)

### Internal

- The kernel reboots before reaching maximum number of ticks (!9369)
- One gasometer per transaction-level (AKA sub-context). (!9492)
- Refactor `EthereumTransactionCommon` and transactions' signatures modules. (!9590)
- Add data type fore representing withdrawals as they'll be produced through transactions. (!9902)
- Slightly refactor the tick model and reboot scheduling. (!10040)
- Add a reboot scheduling for the migration phase. (!10064)
- Add withdrawals to the transaction stack in the EVM handler. (!9905)
- Make it possible for any precompiled contract to generate withdrawals. (!9910)
- Nested transactions give unused gas back to parent transaction after completion. (!9877)
- Make it possible for any precompiled contract to generate withdrawals. (!9910)
- A precompiled contract for withdrawing funds at address 0x00...20. (!10022)
- Remove artificial minimal gas cost for simulation. (!10169)

## Version 4c111dcae061bea6c3616429a0ea1262ce6c174f

### EVM Kernel

- Kernel has an internal version. (!9579)
- Improve kernel's logging from the rollup node's perspective. (!9571)
- Adds a base gas cost of 21000 for every transaction. Note that the fees are
  still not paid by the sender. (!9477)
- Adds a storage migration stage (`stage 0`). (!9591)

### EVM Node

- The EVM node is in sync. with either the current or next kernel version. (!9579)
- Hash transactions with Keccak. (!9588)

### Bug fixes

- Increments the nonce of the transaction even if the transaction fails. (!9534)
- Transaction chunks are now more robust and shouldn't make the kernel
  panic. (!9654)

### Breaking changes

### Internal

- Introduce `anyhow` for error management. (!9377, !9443)
- Interrupt execution before reaching max number of ticks. (!9214)
- Upgrade to Kernel SDK 0.2.1 (!9417, !9526)
- Porting benchmark framework to the EVM Kernel. (!9529, !9524, !9545)
- Reorganized dependencies at the workspace level. (!9522)
