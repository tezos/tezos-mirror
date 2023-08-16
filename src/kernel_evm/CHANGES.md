# Changelog

## Version next

### EVM Kernel

- Fallback mechanism if stage zero fails. (!9732)
- Switch to `ExternalMessageFrame` protocol for external messages. (!9687)

### EVM Node

- Switch to `ExternalMessageFrame` protocol for external messages. (!9687)

### Bug fixes

### Breaking changes

- External Messages must now be framed using `ExternalMessageFrame` (adds an additional prefix byte). (!9687)

### Internal
- The kernel reboots before reaching maximum number of ticks (!9369)

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
- Refactor `TransactionCommon` and transactions' signatures modules. (!9590)

