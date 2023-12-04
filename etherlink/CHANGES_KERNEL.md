# Changelog

## Version Next

### Features

- Fix contract code storage cost (!10356)
- Fix contract creation gas cost and transaction data cost. (!10349)

### Bug fixes

### Breaking changes

### Internal

## Version 32f957d52ace920916d54b9f02a2d32ee30e16b3

### Features

- Support precompiled contract `ecrecover`. (!10926)

### Bug fixes

- Fix the memory limit of the runtime, which is now of the maximum size
  addressable considering the limits of the WASM PVM (32bits, which means `2^32`
  bytes addressable). (!10988)
- Nested contract creation correctly limit gas according to EIP-150. (!10352)

### Breaking changes

### Internal

- Add a debug feature flag to the log crate for optional debug traces. (!10692)
- Blueprints include timestamp, instead of retrieving it at block finalization. (!10822)
