# Changelog

## Version for NEXT

### Features
- DSN node running in bundler mode proxies all requests received by EVM
  observer nodes to the EVM sequencer node.  (!12841)
- DSN Node running in sequencer mode is able to receive proposals via a RPC
  POST request, compute preblocks from received proposals,  and stream
  preblocks via a RPC monitor endpoint. (!12947)

### Bug fixes
- Small fixes for threshold encryption. (!13197)
- Fix compatibility with EVM node. (!13174)

### Breaking changes

### Internal
- Initial DSN node binary. (!12633)
- Added ConfigProvider trait and DefaultConfigProvider implementation. (!13051)
- Added network interface abstraction. (!13077)
- Implement threshold encryption primitives in
  bin_dsn_nodes/crates/threshold_encryption. (!12876)
- RPC functionalities for defining services that are compatible with RESTO
  have been isolated in a separate crate in bin_dsn_node/crates/rpc. (!13080)
- Refactor as a project with multiple crate. (!12984)
- Config provider and default implementation for testing. (!13051)
- Create RPC create and refactor sources to make use of it. (!13060)
- Refactor node, integration tests. (!13157)
- Bundle multiple encrypted transactions. (!13176)
