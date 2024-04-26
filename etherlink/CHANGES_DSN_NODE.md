# Changelog

## Version for NEXT

### Features
- Implement threshold encryption primitives in
  bin_dsn_nodes/crates/threshold_encryption (!12876)
- DSN node running in bundler mode proxies all requests received by EVM
  observer nodes to the EVM sequencer node  (!12841)
- DSN Node running in sequencer mode is able to receive proposals via a RPC
  POST request, compute preblocks from received proposals,  and stream
  preblocks via a RPC monitor endpoint (!12947)
- RPC functionalities for defining services that are compatible with RESTO
  have been isolated in a separate crate in bin_dsn_node/crates/rpc (!13080)

### Bug fixes

### Breaking changes

### Internal
- Initial DSN node binary. (!12633)
