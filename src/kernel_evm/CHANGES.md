# Changelog

## Version next

### EVM Kernel

### EVM Node

### Bug fixes

- Increments the nonce of the transaction even if the transaction fails. (!9534)
- Transaction chunks are now more robust and shouldn't make the kernel
  panic. (!9654)

### Breaking changes

### Internal

- Adds a storage migration stage (`stage 0`). (!9591)
