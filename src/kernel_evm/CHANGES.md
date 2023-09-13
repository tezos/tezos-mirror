# Changelog

## Version dev

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

### EVM Node

- Switch to `ExternalMessageFrame` protocol for external messages. (!9687)
- Add arguments to enable CORS headers. (!9753)
- Add an optional `mode` argument to switch from the proxy on production
  to the one on development. (!9940)
- `eth_sendRawTransaction` checks that the nonce of the transaction is neither too low nor too high. (!9679)
- `eth_sendRawTransaction` checks if the chain id is correct. (!9752)
- Use different files to store kernel logs, differenciate the different kind of
  simulations. (!9664)

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
- L2 Blocks are stored by hash instead of by number. (!10044)

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
