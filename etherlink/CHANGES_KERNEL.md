# Changelog

## Version NEXT

Its storage version is 47.

### Features

- Update the Layer 1 governance contracts to take into account protocol T L1
  block time that is reduced from 8s to 6s. (!20574)
    - [`KT1NM6cpM5BPTmYPszjv6LRDAMRZXPET9DmH`](https://better-call.dev/mainnet/KT1NM6cpM5BPTmYPszjv6LRDAMRZXPET9DmH) for the slow upgrade governance
    - [`KT1JGCdHEyvE3RkmzR7hRYK7vC42QF6zK34H`](https://better-call.dev/mainnet/KT1JGCdHEyvE3RkmzR7hRYK7vC42QF6zK34H) for the fast upgrade governance
    - [`KT1CSqkafD5ZCHFvmsozrCBeSy2XJQzutJRn`](https://better-call.dev/mainnet/KT1CSqkafD5ZCHFvmsozrCBeSy2XJQzutJRn) for the sequencer governance

### Bug fixes

- Cleanup remaining leftover block indexes from V41 migration. (!20187)
- EIP-7702 accounts and smart contracts will now have their code executed
  automatically when XTZ deposits are received via the L1 bridge.
  Note that the deposit event is now called via the XTZ Bridge
  `0xff00000000000000000000000000000000000001` and not the "system" address
  which was `0x0000000000000000000000000000000000000000`. (!20352)

### Internal

### Tezos X Experimental Features

## Version 6 (Farfadet)

### 6.1 (d748ae50)

This versions fixes two issues introduced in version 6.0 of Farfadet.

- Fix predeployed contract initialization to allow overwriting existing
  bytecode after kernel upgrade. (!20279)
- Re-inject a locked FA deposit (0x82f507bc5aba0f3f6088c087c2fcd87fc7b7f33c9445e331ec3d1fdf45e4be38)
  which was affected by the previous incorrect initialization. (!20280)

This kernel requires the Octez EVM node version 0.48 or higher.

Its storage version is 45.

### 6.0 (d2842ae4)

Important information: Rollup nodes configured to use the full history mode will require
up to twice the disk space they are currently using in the two weeks following
the activation of this kernel, due to how our storage backend (Irmin) performs
garbage collection. (!19744)

Furthermore, this kernel requires the Octez EVM node version 0.48 or higher.

Its storage version is 44.

### Features

- Optimize computation of receipt and transaction root. Receipt root is now
  Ethereum standard compatible (EIP 2718) (!19300)
- Etherlink's VM version was bumped from Prague to Osaka, it now supports (!19753):
  - [EIP-7951](https://eips.ethereum.org/EIPS/eip-7951) Precompile for secp256r1 Curve Support.
  - [EIP-7939](https://eips.ethereum.org/EIPS/eip-7939) Count leading zeros (CLZ) opcode.
  - [EIP-7883](https://eips.ethereum.org/EIPS/eip-7883) ModExp Gas Cost Increase.
  - [EIP-7823](https://eips.ethereum.org/EIPS/eip-7823) Set upper bounds for ModExp.
- Bump the capacity of Etherlink to 27MGas/s (meaning a target per second to
  13.5MGas/s). (!20024)

### Bug fixes

- `QueuedDeposit` event is now emitted by the FA bridge address: `0xff00000000000000000000000000000000000002` (!19239) (!19793)
- `QueuedDeposit` event first topic is now correctly computed as the keccak256 of its signature: `QueuedDeposit(uint256,address,uint256,address,uint256,uint256,uint256)`. Hex value is `0xb02d79c5657e344e23d91529b954c3087c60a974d598939583904a4f0b959614`. (!19239) (!19793)
- Disable [EIP-3607](https://eips.ethereum.org/EIPS/eip-3607) on simulation (`eth_call`). (!19643)
- Fixes `callTracer` not tracing all transactions in a block if a user
  sent multiples transactions in that same block. (!19649)
- Disable [EIP-3607](https://eips.ethereum.org/EIPS/eip-3607) checks during
  `debug_traceCall` simulations. (!19668)

### Internal

- Change the block storage to save only latest block. (!19744)
- Storage version for sub-block latency entrypoints is V42. Also used to check for incompatibility
  in the node at the time of executing the inclusion preconfirmation stream. (!19956)
- EVM node with storage V43 or above will no longer store inbox events. (!19986)

### Tezos X Experimental Features

- Adds a feature flag to control whether or not the Tezos runtime is enabled or
  not. (!19921)

## Etherlink 5 (Ebisu)

Its storage version is 38.

### Features

- The EVM now supports optional access lists.
  See [EIP-2930](https://eips.ethereum.org/EIPS/eip-2930). (!17766)
- Replaces SputnikVM with REVM (!18384 !18390 !18883):
  - Fixed the 1024-call stack depth limit as per EVM specification (previously
    256).
  - Improved smart contract execution performances.
  - Reduced overall tick consumption.
  - Better maintainability and alignment with upcoming Ethereum forks and EVM
    feature updates.
- Etherlink's VM version was bumped from Cancun to Prague, it now supports (!18851):
    - [EIP-7702](https://eips.ethereum.org/EIPS/eip-7702): Set Code for EOAs.
    - [EIP-2537](https://eips.ethereum.org/EIPS/eip-2537): Precompile for
      BLS12-381 curve operations.
    - [EIP-7623](https://eips.ethereum.org/EIPS/eip-7623): Increase calldata cost.
- Allows the sequencer operator to set a new public key to be used to verify
  the signature of its blueprint, through a call to a new precompile contract. (!19129)
- Bump the capacity of Etherlink to 14MGas/s (meaning a target per second to
  7MGas/s). (!18452)

### Bug fixes

- REVM precompile contract implementation fixed incoherent FA bridge events, locked behind the REVM feature flag (!18722):
  - Fast FA withdrawal
    - Previous signature : `FastFaWithdrawal(address,address,bytes22,bytes22,uint256,uint256,uint256,bytes)`
    - New signature : `FastFaWithdrawal(uint256,address,address,bytes22,bytes22,uint256,uint256,uint256,bytes)`
    - First topic changed : Keccak256 of new signature
    - Second topic unchanged : ticket hash
    - Emitter address unchanged : `0xff00000000000000000000000000000000000002`
    - Fixes topics incoherence, ticket hash was missing from the signature
  - Claimed deposit
    - Signature unchanged : `Deposit(uint256,address,address,uint256,uint256,uint256)`
    - First topic unchanged : Keccak256 of signature
    - Second topic unchanged : ticket hash
    - Previous emitter address : `0x0000000000000000000000000000000000000000`
    - New emitter address : `0xff00000000000000000000000000000000000002`
    - Fixes emitter incoherence, there is no reason to justify emitting this event from the system address

### Internal

- Optimize storage reads for fixed-size values (host functions). (!19268)
- Optimize storage writes for fixed-size values and new paths (host
  functions). (!19274)

## Etherlink 4 (Dionysus)

### 4.1 (6d73d48)

- The validation mechanism that automatically rejects transactions with
  over-estimated gas limits exceeding the maximum threshold will be disabled.
  During execution, any gas limit above the maximum will be automatically
  capped at the maximum permitted value. (!18179)
- Bump the capacity of Etherlink to 8MGas/s (meaning a target per second to
  4MGas/s). (!18452)
- Update the Layer 1 governance contracts to take into account alternative
  voting keys for bakers. (!18505)
    - [`KT1VZVNCNnhUp7s15d9RsdycP7C1iwYhAQ8r`](https://better-call.dev/mainnet/KT1VZVNCNnhUp7s15d9RsdycP7C1iwYhAQ8r) for the slow upgrade governance
    - [`KT1DxndcFitAbxLdJCN3C1pPivqbC3RJxD1R`](https://better-call.dev/mainnet/KT1DxndcFitAbxLdJCN3C1pPivqbC3RJxD1R) for the fast upgrade governance
    - [`KT1WckZ2uiLfHCfQyNp1mtqeRcC1X6Jg2Qzf`](https://better-call.dev/mainnet/KT1WckZ2uiLfHCfQyNp1mtqeRcC1X6Jg2Qzf) for the sequencer governance

### 4.0  (1f47de0)

This kernel has been activated on Etherlink Testnet on block
[19,307,965][1f47-activation-testnet], and on Etherlink Mainnet on block
[15,262,162][1f47-activation-mainnet].

[1f47-activation-testnet]: https://testnet.explorer.etherlink.com/block/0x905031212562def1cecd79e6472bf3b6588b9ea75563d0c8fc34c9c8b61871f0
[1f47-activation-mainnet]: https://explorer.etherlink.com/block/0x0f3872315951a148220bd3fe63ec9012c59ddcca49dfcdc40ec3ffbfec93ea63

Its storage version is 33.

### Features

- The EVM's configuration has been bumped to Cancun. (!16141)
  The following EIPs are now supported by Etherlink:
  * [EIP-1153](https://eips.ethereum.org/EIPS/eip-1153)
  * [EIP-5656](https://eips.ethereum.org/EIPS/eip-5656)
  * [EIP-6780](https://eips.ethereum.org/EIPS/eip-6780)
- A fast withdrawal entrypoint was added to the FA bridge precompiled contract
  under a feature flag. (!17114 !17494)
- Hot and cold accesses are now enabled to better align gas consumption
  with the actual execution on Etherlink. (!17308)
- Removes the `OutOfTicks` error when executing the EVM interpreter. We only
  use gas to decide when to reboot. (!15079)
- The flag to activate DAL has been enabled, slots from 0 to 7 can now be used
  from the kernel's perspective. (!17636)
- Execution gas is now used to compute the changes in gas price over time,
  instead of estimated ticks. (!17630)
- The EVM now uses a cache for contract code/hash/size which improves time
  performances on the execution side. (!17716)
- Updates the Layer 1 governance contracts to take into account Rio new cycle
  duration. (!17943)
    - [`KT1XdSAYGXrUDE1U5GNqUKKscLWrMhzyjNeh` for the regular upgrade governance][kgov]
    - [`KT1D1fRgZVdjTj5sUZKcSTPPnuR7LRxVYnDL` for the security upgrade governance][sgov]
    - [`KT1NnH9DCAoY1pfPNvb9cw9XPKQnHAFYFHXa` for the sequencer governance][sqgov]
- Computes the execution gas assuming a minimum base fee per gas instead of
  current gas price. The paid DA fees remain unchanged. (!17954)
- The EVM now supports optional access lists.
  See [EIP-2930](https://eips.ethereum.org/EIPS/eip-2930). (!17766)

### Bug fixes

- Improves call trace of `DELEGATECALL` and `CALLCODE`. (!16588)
- The EVM now complies to [EIP-3529](https://eips.ethereum.org/EIPS/eip-3529). (!16887, !17319)
- The EVM now complies to [EIP-3651](https://eips.ethereum.org/EIPS/eip-3651). (!16888)
- The EVM now properly record init code cost for `CREATE`/`CREATE2`
  opcodes, now fully complying with [EIP-3860](https://eips.ethereum.org/EIPS/eip-3860). (!16890)
- The EVM is now refunding accounts as expected during inter (layer)
  transactions. (!16973)
- Ported a security patch that fixed a bug causing proxy calls from FA deposits to
  not correctly revert state on failure. This also includes a fix for tracing inner calls. (!17704)

### Internal

- Rework block production to simplify data flow and remove unnecessary
  IO. (!16661 !16684 !16685 !16618)
- The EVM version is now written in the durable storage at `/evm/evm_version`. (!17202)
- Replaced the custom ABI encoder/decoder with `alloy`'s encoder/decoder
  for improved maintainability. (!16670)
- Keeps track of execution gas in addition to estimated ticks. (!17507)

## Etherlink 3 (Calypso)

### 3.1 (9ab8acd)

**Note:** This commit is not part of the `master` branch of the Octez
repository, but is part of [`etherlink-security-upgrades`][su-3] instead.

[su-3]: https://gitlab.com/tezos/tezos/-/tree/etherlink-security-upgrade-3

This security upgrade (named Calypso 2) addresses a severe bug found in the
deposit of FA tokens from Tezos to Etherlink. The bug was never triggered on
Mainnet before the activation of the kernel on block 10,453,254.

### 3.0 (6046630)

This kernel has been activated on Etherlink Testnet on block
[17,443,389][6046-activation-testnet], and on Etherlink Mainnet on block
[7,056,139][6046-activation-mainnet].

[6046-activation-testnet]: https://testnet.explorer.etherlink.com/block/0xe1df4a09691c11dc795dc230e5a3929932527f6f66e937bd97a25d709f3c4a01?tab=index
[6046-activation-mainnet]: https://explorer.etherlink.com/block/0xc7788ef7ccf24ad003757a73b6f8903c15d0d514bf9738011c6b3b583aad733f

Its storage version is 26.

### Features

- Once this kernel activates, only the latest block of the chain will
  be available in the rollup context, compared to the full history
  with previous version. This should sensibly reduces the disk
  consumption of full Rollup Node. The full history will remain
  available via Octez EVM Node deployments. (!15490)
- The VM is now relying on cache to make r/w access to storage indexes
  which improves time performances on the execution side. (!15812)
- The kernel uses events to make the sequencer aware of delayed transaction
  flush. This significantly reduces the risk of sequencer downtime due to
  unforseen flush event. (!15621 !15841)
- Updates the Layer 1 governance contracts to take into account Quebec new time
  between blocks. (!16592)
    - [`KT1FPG4NApqTJjwvmhWvqA14m5PJxu9qgpBK` for the regular upgrade governance][kgov]
    - [`KT1GRAN26ni19mgd6xpL6tsH52LNnhKSQzP2` for the security upgrade governance][sgov]
    - [`KT1UvCsnXpLAssgeJmrbQ6qr3eFkYXxsTG9U` for the sequencer governance][sqgov]
- A fast withdrawal entrypoint was added to the withdrawal precompiled contract
  under a feature flag. (!16417)
- Fast withdrawal entrypoint activation. (!16617)

[kgov]: https://better-call.dev/mainnet/KT1FPG4NApqTJjwvmhWvqA14m5PJxu9qgpBK/operations
[sgov]: https://better-call.dev/mainnet/KT1GRAN26ni19mgd6xpL6tsH52LNnhKSQzP2/operations
[sqgov]: https://better-call.dev/mainnet/KT1UvCsnXpLAssgeJmrbQ6qr3eFkYXxsTG9U/operations

### Bug fixes

- Standard withdrawals from the withdrawal precompiled contract now emit events
  from the appropriate address. (!16417)
- Fixed an issue in the FA bridge where withdrawals events were not
  being emitted from the appropriate address. (!16666)

### Internal

- Maximum gas per transaction is stored in the durable storage. (!15468 !15925)
- Minimum base fee per gas is stored in the durable storage. (!15475)
- Blueprints from the past are refused on parsing. (!15636)
- Clear blueprints on migration (!15637)
- Support for legacy untagged format for delayed transactions has been dropped. (!15366)
- Avoid writing gas price twice per block by adding it to block-in-progress. (!15574)
- Transaction validation support in simulation is dropped, the
  validation must be done outside of the kernel. (!15958)
- Parsing a DAL slot is now resumed after invalid inputs. (!15926)
- Current block's gas price is not stored on its own anymore but the
  information is still available as part of the current
  block. (!15609)
- The kernel always reboots between the application of
  blocks. (!16274)
- Use the timestamp provided by the `Info_per_level` internal message instead
  of trying to predict the current timestamp (notably in the context of the
  delayed inbox). It has been demonstrated that the prediction is
  over-approximated after a Layer 1 downtime, which is the most likely scenario
  triggering a flush of the delayed inbox. (!16425)
- An optional chain_id field has been added to the blueprints chunks
  sent by the sequencer. (!16366)
- The option to limit the block production to at most one block per
  Tezos level, which was added in version
  ec7c3b349624896b269e179384d0a45cf39e1145, has been removed. (!16302)
- Deposits can now specify a chain id, but it is currently ignored. (!16153)
- All blueprints in storage are deleted after a flush event. (!15673)

## Etherlink 2 (Bifrost)

### 2.0 (7386d0b)

This kernel has been activated on Etherlink Testnet on block
[10,758,937][7386-activation-testnet], and on Etherlink Mainnet on
block [4,162,673][7386-activation-mainnet].

Its storage version is 22.

[7386-activation-testnet]: https://testnet.explorer.etherlink.com/block/0x4ecefba952e877f9bc159f30d12544358abcceaabb0f4a721ab1044cae8ee09d
[7386-activation-mainnet]: https://explorer.etherlink.com/block/0xe29ef8db2227f495808fdc04d92b8ea7edcad32d1270fdddc9c5baed1542ccd9

### Features

#### Stable

- The kernel is able to trace calls for the `callTracer` configuration. (!14290)
- Gas cost of XTZ withdrawals is increased to prevent abuses. (!14016)
- Gas cost of FA withdrawals is increased to prevent abuses. (!14016)
- Support of larger delayed transactions (was limited at 4KBytes before). (!14467 !15353)
- Enable the FA bridge on Mainnet (!15355)

### Internal

- Transactions root and receipts root from blocks no longer hash all
  transactions. It now hashes new transactions and the previous hash.
  It will allow to remove old transactions from the durable storage. (!14688)
- Tick model is updated with regards to FA deposits and withdrawals. (!14016)
- L1 proxy address is added as a field to the FA withdrawal event. (!14260)
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
- The nonce of the zero account (0x00..00) is incremented, to avoid being cleaned
  by EIP-161 empty account rule. (!15197)
- Keep a null balance for the withdrawal precompiled address (i.e. 0xff00000000000000000000000000000000000001).

### Bug fixes

- Fix gas cost of contract creation by fixing the implementation of init code
  cost recording. (!14892)
- Fix call trace reporting of error messages. (!15000)
- Fix performance regression due to struct logger. (!15319)

## Etherlink 1

### 1.3 (af79090)

This kernel has been activated on Etherlink Testnet on block
[4,899,480][af90-activation-testnet]. It requires the 3rd revision of the WASM
PVM to work.

Its storage version is 14.

[af90-activation-testnet]: https://testnet.explorer.etherlink.com/block/0x2d80703e04542bd3e465d028674e19d3888e057b764a8482ed3218cc175b55d0

#### Features

##### Stable

- Simulation now uses timestamp provided by the node. (!14186)

##### Experimental

###### FA Bridge

The FA Bridge complements the native bridge already available on both Mainnet
Beta and Testnet, and allows users to deposit and withdraw FA 2.1 tokens.

- FA withdrawals are applied if FA bridge feature is enabled. (!13958)
- Add FA withdrawal execution methods and FA bridge precompile. (!13941)
- FA bridge enabled in Ghostnet. (!14342)

###### DAL Integration

Tezos Data-Availibility Layer (DAL) offers an increased bandwidth compared to
L1 blocks, without sacrificing decentralization, and with reduced costs.

- The list of DAL slot indices on which the sequencer may publish DAL slots can
  be configured at path `/evm/dal_slots`. (!13717)
- A DAL feature flag is added to the configuration. If the path has a
  value in `/evm/feature_flags/enable_dal`, the kernel is allowed to import
  data from the DAL. (!13634)

#### Bug fixes

- Simulation with parameter `from` no longer return `OutOfFunds` if the
  value is greater than zero. (!14150)
- Removes transactions from the delayed inbox only if they are applied
  in a block. They were previously optimistically removed from the delayed
  inbox, which could be an issue if an upgrade happen in the same block.
  (!14351)

#### Internal

- Forbids in the code module to overwrite the code storage of an
  account. Execution forbid this. (!14317)
- Tick model is updated with regards to FA deposits and withdrawals. (!14016)
- Gas cost of XTZ/FA withdrawals is increased to prevent abuses. (!14016)
- FA deposits disallowed to invoke XTZ/FA withdrawal precompiles. (!14545)

### 1.2 (ec7c3b3)

This kernel has been deployed on Etherlink Testnet on block
[3,244,690][ec7c3-activation]. It has not been activated on Mainnet Beta.

Its storage version is 12.

[ec7c3-activation]: https://testnet-explorer.etherlink.com/block/0x5d16e74c2d5445df5944f79509c3e24dfe392f5f07556efe1b7c30ca477e7a28

#### Features

- The kernel has now the ability to trace a transaction (via
  the EVM runtime). (!13185, !13502)
- Incorporate the gas limit in the block hash. (!13280)
- The block's miner and coinbase address are set to sequencer pool
  address if it exists. (!13414)
- Update Sequencer Pool address on Ghostnet. (!13614)

#### Bug fixes

- Withdrawal reverts if the amount loses wei during the conversion to mutez,
  e.g. `1000000000001` would make caller lose 1 wei. (!13928)

#### Breaking changes

- Validation returns the transaction object instead of the address of the sender. (!12873, !13292)
- Introduce new block encoding to include `baseFeePerGas` and `mixHash`.
  New blocks would contain `baseFeePerGas` and a default `mixHash`. (!13159)

#### Internal

- Gas price is removed from internal structure block in progress. (!13220)
- Refactor block creation to properly add the gas limit. (!13278)
- Gas limit is no longer optional for blocks. (!13279)
- The kernel can limit the block production to at most one block per tezos level
  if the file `/__at_most_one_block` exists in the storage. (!13202)
- Stop emitting an `Info` log when running an Ethereum transaction. (!13630)
- A FA bridge feature flag is added to the configuration. If the path has a value
  in `/evm/feature_flags/enable_fa_bridge`, FA bridge related operations will be accepted. (!13535)
- Uses safe increment function for deposit transaction nonce (!13712)

### 1.1 (ffb8a65)

**Note:** This commit is not part of the `master` branch of the Octez
repository, but is part of [`etherlink-security-upgrades`][su-2] instead.

[su-2]: https://gitlab.com/tezos/tezos/-/tree/etherlink-security-upgrade-2

This security upgrade addresses security vulnerabilities found in the underlying
implementation of a double linked list used for the delayed inbox. The security
vulnerability was not exploited on Mainnet before the activation of the kernel
on block 3,400,942.

The internal pointers of the double linked list were not updated correctly, which
could lead in weird scenarios to the application of a deposit without being
actually removed from the delayed inbox.

#### 1.0 (4f4457e)

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
- Circular calls to impure precompiles are forbidden. (!14390)

## Etherlink 0 (Genesis)

### 0.1 (4d98081)

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

### 0.0 (b9f6c91)

This is the kernel used to originate Etherlink Mainnet Beta.

Its storage version is 11.

#### Features

- Set block gas limit to 2^50. (!13010)

## Older versions

For versions of the kernel pre-dating Etherlink Mainnet Beta, see
<CHANGES_KERNEL_PREMAINNET.md>.
