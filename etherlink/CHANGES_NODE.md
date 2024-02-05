# Changelog

## Notes

Currently supported kernel version: a41f30ddb8787e5ff5c461d949a9ae3f71e4eea9

## Version Next

### Features

### Bug fixes

### Breaking changes

### Internal

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

### Internal
