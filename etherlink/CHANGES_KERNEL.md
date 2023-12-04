# Changelog

## Version Next

### Features

### Bug fixes

### Breaking changes

### Internal

## Version 32f957d52ace920916d54b9f02a2d32ee30e16b3

### Features

### Bug fixes

- Fix the memory limit of the runtime, which is now of the maximum size
  addressable considering the limits of the WASM PVM (32bits, which means `2^32`
  bytes addressable).

### Breaking changes

### Internal

- Add a debug feature flag to the log crate for optional debug traces. (!10692)
- Blueprints include timestamp, instead of retrieving it at block finalization. (!10822)
