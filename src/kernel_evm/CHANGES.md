# Changelog

## Version next

### EVM Kernel

- Kernel has an internal version. (!9579)

### EVM Node

- The EVM node is in sync. with either the current or next kernel version. (!9579)

### Bug fixes

- Increments the nonce of the transaction even if the transaction fails. (!9534)
- Transaction chunks are now more robust and shouldn't make the kernel
  panic. (!9654)

### Breaking changes

### Internal

- Adds a storage migration stage (`stage 0`). (!9591)
