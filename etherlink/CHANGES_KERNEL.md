# Changelog

## Version NEXT

### Features

#### Stable

- The kernel is able to trace calls for the `callTracer` configuration. (!14290)
- Gas cost of XTZ withdrawals is increased to prevent abuses. (!14016)

#### Experimental

##### FA Bridge

The FA Bridge complements the native bridge already available on both Mainnet
Beta and Testnet, and allows users to deposit and withdraw FA 2.1 tokens.

- Gas cost of FA withdrawals is increased to prevent abuses. (!14016)

### Internal

- Transactions root and receipts root from blocks no longer hash all
  transactions. It now hashes new transactions and the previous hash.
  It will allow to remove old transactions from the durable storage. (!14688)
- Tick model is updated with regards to FA deposits and withdrawals. (!14016)
- L1 proxy address is added as a field to the FA withdrawal event. (!14260)
- Preliminary support of larger delayed transactions. (!14467)
- Move block storage related functions to another file to improve the readability
  (!14685)
- EVM Execution no longer require an additional storage for block number to
  block hash. Uses the existing indexing table instead. (!14704)
- A generic storage crate was introduced so that every component of the kernel
  benefits from the same implementation to read/write primitives. (!14735)
- Adds a storage for account's code. Code are referenced by their code
  hash. (!14369)
- Enable LTO (Link Time Optimization) during compilation to reduce tick consumption
  and improve execution speed. (!14933)

### Bug fixes

- Fix gas cost of contract creation by fixing the implementation of init code
  cost recording. (!14892)
- Fix call trace reporting of error messages. (!15000)

## Version af7909023768bc4aad3120bec7bea4a64a576047

This kernel has been activated on Etherlink Testnet on block
[4,899,480][af90-activation-testnet]. It requires the 3rd revision of the WASM
PVM to work.

Its storage version is 14.

[af90-activation-testnet]: https://testnet.explorer.etherlink.com/block/0x2d80703e04542bd3e465d028674e19d3888e057b764a8482ed3218cc175b55d0

### Features

#### Stable

- Simulation now uses timestamp provided by the node. (!14186)

#### Experimental

##### FA Bridge

The FA Bridge complements the native bridge already available on both Mainnet
Beta and Testnet, and allows users to deposit and withdraw FA 2.1 tokens.

- FA withdrawals are applied if FA bridge feature is enabled. (!13958)
- Add FA withdrawal execution methods and FA bridge precompile. (!13941)
- FA bridge enabled in Ghostnet. (!14342)

##### DAL Integration

Tezos Data-Availibility Layer (DAL) offers an increased bandwidth compared to
L1 blocks, without sacrificing decentralization, and with reduced costs.

- The list of DAL slot indices on which the sequencer may publish DAL slots can
  be configured at path `/evm/dal_slots`. (!13717)
- A DAL feature flag is added to the configuration. If the path has a
  value in `/evm/feature_flags/enable_dal`, the kernel is allowed to import
  data from the DAL. (!13634)

### Bug fixes

- Simulation with parameter `from` no longer return `OutOfFunds` if the
  value is greater than zero. (!14150)
- Removes transactions from the delayed inbox only if they are applied
  in a block. They were previously optimistically removed from the delayed
  inbox, which could be an issue if an upgrade happen in the same block.
  (!14351)

### Internal

- Forbids in the code module to overwrite the code storage of an
  account. Execution forbid this. (!14317)
- Tick model is updated with regards to FA deposits and withdrawals. (!14016)
- Gas cost of XTZ/FA withdrawals is increased to prevent abuses. (!14016)

## Version 4f4457e2527cb227a90bb1c56d3a83f39c0f78fd

This kernel has been activated on Etherlink Testnet on block
[3,825,580][4f44-activation-testnet], and on Etherlink Mainnet Beta on block
[1,273,380][4f44-activation-mainnet].

Its storage version is 12.

**Note:** This commit is not part of the `master` branch of the Octez
repository, but is part of [`etherlink-mainnet-launch`][mainnet-branch] instead.

[mainnet-branch]: https://gitlab.com/tezos/tezos/-/tree/etherlink-mainnet-launch
[4f44-activation-testnet]: https://testnet-explorer.etherlink.com/block/0x54e37d7743b4a79e23640ab36fcd95cf9e54d9b95959cc6c7ed3abfaff93dc52
[4f44-activation-mainnet]: https://explorer.etherlink.com/block/0x18027e1703da079fed2d8bea3395a7f365957194b7ed35eaba36e901217e6c6f

### Features

- Blueprints provided by the sequencer must have increasing timestamps. (!13807)
- Blueprints provided by the sequencer are accepted by the rollup node only
  if they are smaller than current view of the L1 timestamp plus an margin
  error of 5 minutes. 5 minutes is the default value but can be configured by
  the installer. (!13827)
- FA deposits are applied if FA bridge feature is enabled (disabled on Mainnet
  Beta and Testnet). (!13835)
- Native token withdrawals emit event log in case of successful execution. (!14014)
- Native token deposits emit EVM event log. (!14012)

### Bug fixes

- Allow `eth_call` to return the contract bytecode on contract creation. (!13830)
- Stack depth for smart contracts are temporarily limited to 256, instead of
  the expected limit of 1024 on Shanghai. (!13850)
- The simulation now caps the gas limit for execution at the maximum allowed
  per transaction (30 million gas at most). (!13729)
- `block.number` and `block.timestamp` now return respectively the current
  number and timetamp of the processed block, instead of the last produced one.
  (!13901)
- Forced blueprints (in case of delayed inbox timeout) always use a timestamp
  equal or greater than the predecessor. (!13832)
- Withdrawal counter is now revertable. (!14389)

### Internal

- Add FA deposit structure and helper methods for its parsing and formatting. (!13720)
- Add FA deposit execution methods. (!13773)
- Add ticket table to account for FA deposits. (!12072)
- Refactor withdrawals handling to keep `OutboxMessage` in the `ExecutionOutcome`. (!13751)
- Compress h256 hash when encoding. Transaction encoded with `r` or
  `s` hash compressed were impacted. (!13654)
- Allows a `/__evm_node` flag in the kernel to have a different logic whether
  the kernel is run by the evm-node or the rollup node. (!13827)
- Add FA withdrawal structure and helper methods for parsing and encoding. (!13843)
- Rework the semantics of migrations in order to allow a network to skip frozen
  versions. (!13895)

## Version ec7c3b349624896b269e179384d0a45cf39e1145

This kernel has been deployed on Etherlink Testnet on block
[3,244,690][ec7c3-activation]. It has not been activated on Mainnet Beta.

Its storage version is 12.

[ec7c3-activation]: https://testnet-explorer.etherlink.com/block/0x5d16e74c2d5445df5944f79509c3e24dfe392f5f07556efe1b7c30ca477e7a28

### Features

- The kernel has now the ability to trace a transaction (via
  the EVM runtime). (!13185, !13502)
- Incorporate the gas limit in the block hash. (!13280)
- The block's miner and coinbase address are set to sequencer pool
  address if it exists. (!13414)
- Update Sequencer Pool address on Ghostnet. (!13614)

### Bug fixes

- Withdrawal reverts if the amount loses wei during the conversion to mutez,
  e.g. `1000000000001` would make caller lose 1 wei. (!13928)

### Breaking changes

- Validation returns the transaction object instead of the address of the sender. (!12873, !13292)
- Introduce new block encoding to include `baseFeePerGas` and `mixHash`.
  New blocks would contain `baseFeePerGas` and a default `mixHash`. (!13159)

### Internal

- Gas price is removed from internal structure block in progress. (!13220)
- Refactor block creation to properly add the gas limit. (!13278)
- Gas limit is no longer optional for blocks. (!13279)
- The kernel can limit the block production to at most one block per tezos level
  if the file `/__at_most_one_block` exists in the storage. (!13202)
- Stop emitting an `Info` log when running an Ethereum transaction. (!13630)
- A FA bridge feature flag is added to the configuration. If the path has a value
  in `/evm/feature_flags/enable_fa_bridge`, FA bridge related operations will be accepted. (!13535)
- Uses safe increment function for deposit transaction nonce (!13712)

## Version b9f6c9138719220db83086f0548e49c5c4c8421f

This is the kernel used to originate Etherlink Mainnet Beta.

Its storage version is 11.

### Features

- Set block gas limit to 2^50. (!13010)

### Security Upgrades

#### Version 4d98081699595b57512ffeff35bca320f281c806

**Note:** This commit is not part of the `master` branch of the Octez
repository, but is part of [`etherlink-security-upgrades`][su-1] instead.

[su-1]: https://gitlab.com/tezos/tezos/-/tree/etherlink-security-upgrade-1

This security upgrade addresses security vulnerabilities in the withdraw
precompiled contract, found by Spearbit as part as their external audit of
the Etherlink kernel. The security vulnerability was not exploited on
Mainnet Beta before the activation of the kernel on block 853,175.

- Forbid delegatecall, staticcall and callcode to the withdraw precompiled
  contract. (!13997)
- The withdrawal precompiled contract needs to be called with at least `10^12`
  Wei to make sure it can be transformed to a mutez.

## Older versions

For versions of the kernel pre-dating Etherlink Mainnet Beta, see
<CHANGES_KERNEL_PREMAINNET.md>.
