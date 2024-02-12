# Changelog

## Version Next

### Features

### Bug fixes

- Fix bug where CALLCODE shouldn't send the balance to the "to" address. (!11907)
- Fix a bug where precompiled failures were considered as `Fatal`. (!11947)


### Breaking changes

### Internal

- `evm-evaluation-assessor` takes 'PREVRANDAO' in its computation. (!11907)
- Implement warm/cold access for state access opcodes. (!11580)

## Version a41f30ddb8787e5ff5c461d949a9ae3f71e4eea9

### Features

- Implement Call stipend for inner call with transfer. (!11587)
- Support 'modexp' precompiled contract. (!11732)
- Support 'ecAdd', 'ecMul' and 'ecPairing' precompiled contracts. (!11746)

### Bug fixes

- Fix minimum gas price used for charging fees: should be `base_fee_per_gas`, instead of `1 wei`. (!11509)
- Fix an overflow bug when prepaying transactions/repaying gas. (!11545)
- Fix a bug where creating a create a contract with not enough funds was allowed. (!11526)
- Hands the return to the current context when the creation of a contract fails. (!11546)
- Fix a bug where for non-existing address default hash was used rather than zero hash. (!11665)
- Fix a bug where nonce and code shouldn't be set when SELFDESTRUCT is called
  in initialisation code. (!11637)
- Fix a bug where SELFDESTRUCT deletes contracts immediately rather than at the end of the transaction. (!11658)
- `CallTooDeep` is treated as a simple error (not as fatal). (!11794)

### Breaking changes

- Prefix withdrawal precompiled contract by 'ff' to avoid any friction with upcoming Ethereum fork.
  Withdrawal contract address is now 'ff00000000000000000000000000000000000001'. (!11556)
  
### Internal

## Version 9978f3a5f8bee0be78686c5c568109d2e6148f13

### Features

- Fix contract code storage cost (!10356)
- Fix contract creation gas cost and transaction data cost. (!10349)
- Implementation of EIP-3541, new code starting with the 0xEF byte cannot be
deployed. (!11225)
- Implement EIP-684: Prevent create collision. Reject contract creation to non-empty address (!11150)
- Smart contract starts at nonce 1 following EIP-161. (!11276)
- Support signature of transactions pre EIP-155. (!11281)
- Prevent collision when creating a contract at the same level it was self-
  destructed. (!11474)

### Bug fixes

- Prevent fatal errors when an intermediate call/transaction runs out of gas during an execution. (!11290)
- Completely remove fatal error promotion between intermediate call/transactions. (!11334)
- Prevent fatal errors on transfers in connection with calls. (!11365)
- Prevent panics when BLOCKHASH opcode is used. (!11366)

### Breaking changes

### Internal

- Added support for multi-testing to the `evm-evaluation`. (!11223)
- Blueprints are now stored, the Queue is simplified. New storage version (3). (!11131)

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
