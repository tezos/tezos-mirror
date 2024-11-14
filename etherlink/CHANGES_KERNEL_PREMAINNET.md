# Changelog

## Version d517020b58afef0e15c768ee0b5acbda1786cdd8

### Features

- Limit the size of blueprints to 128 chunks. (!12631)

### Internal

- Improve tick model of the delayed inbox. (!12524)
- Update tick model. (!12631)

## Version 0a81ce76b3d4f57d8c5194bcb9418f9294fd2be1

### Features

- Delayed EVM transactions no longer pay data-availability fee. (!12401)
- Raise minimum gas price to **1 gwei**. (!12514)
- Set a delayed bridge for Ghostnet. (!12485)

### Bug fixes

- Fix `eth_getTransactionBy*`: report the gas & gasPrice as set by the sender. (!12419)

### Breaking changes

- Delayed EVM transactions use a dedicated encoding tag in blueprints. (!12401)

### Internal

- Make parsing dependent on the kernel mode. (!12356, !12363)
- Implements reboot scheduling for the stage one in sequencer mode. (!12400)

## Version d689a9dcfa16169aa0dbce1944e920093223c671t (Security Upgrade)

### Bug fixes

- The delayed inbox was blocked as deposits were stored in the wrong
  storage location. The delayed inbox is flushed to unblock it.

## Version 79509a69d01c38eeba38d6cc7a323b4d69c58b94

### Features

- Add detailed tick model for the `modexp` precompiled contract (!12278)
- Reduce the verbosity of logs (!12372)
- Store the root hash used for an upgrade in the durable storage (!12352)
- Update the governance contracts to a more up-to-date deployment (!12379)
- Support upgrade from a kernel security governance contract (!12359)
- Rename the sequencer admin contract into sequencer governance (!12380)

### Bug fixes

- Ticks for gas price adjustments are counted per block, rather than per run. (!12279)

### Breaking changes

## Version 5cc8ce8253e0fc4fb31b0d975d20351c25d22bab (Security Upgrade)

### Bug Fixes

- Disable elastic fee model
- Clean-up pending blueprints
- Patch timestamps of blocks created by the delayed inbox flushing mechanism

## Version 624a144032d6dc6431697c39eb81790bccaacff9

### Features

- Add an EVM event when a sequencer upgrade is seen by the
  kernel. (!12046)
- Da fee is sent to sequencer pool address. (!12113)
- Gas price adjusts itself to handle congestion. (!12167)

### Bug fixes

- `BLOCKHASH` opcode now returns the actual block hash instead of `0x00..0`. (!12130)
- Withdrawals are now compatible with sequencer. (!11733)

### Breaking changes

- Sequencer upgrade expect an activation timestamp and an etherlink
  pool address attached to the new public key. The new sequencer is
  activated when the rollup sees a l1 block with a timestamp greater
  than or equal to the activation timestamp. Blueprints in the storage
  are deleted during the sequencer upgrade as they can be trusted
  anymore. (!12038, !12046)
- Calling the withdrawals precompiled contract at `0xff00..0001` now costs
  880 gas units. (!12220)

### Internal

- Keys modified by block application are moved to `/evm/world_state`. (!12080, !12187)
- Remove error logs from storage. (!12180)

## Version 20ab639f09a8c7c76f982383c3d9e1f831f38088

### Features

- Implement EIP-3860. (!11831)
- Implement EIP-2681. (!11976)
- Flush every transaction in the delayed inbox in case one is overdue. (!11914)
- Require a minimal number of L1 blocks to have been backed before a delayed transaction is considered overdue. (!11811)
- Raise minimum base fee per gas to 0.05 gwei. (!12001)
- Raise da fee to 4 mutez/byte. (!12101)

### Bug fixes

- Fix selfdestruct behavior when target is itself. (!12081)
- Fix the nonce of the caller on contract creation. Callee's address remains hot even if creation fails. (!11634)
- Fix the nonce of a newly created contract. (!11767)
- Fix the encoding of the upgrade event emitted by the kernel. (!12003)
- Gas is now charged for inner create. (!11814)
- Add missing checks on data integrity and signature overflows for the `ecrecover` precompile contract. (!11997)
- Exit when there's a mod overflow when calling `modexp` precompiled contract. (!12005)
- Clear contract storage before execution on contract creation. (!12089)
- Record call stipend even if balance is not enough for inner call. (!12070)

## Version c5969505b81b52a779270b69f48b6a66c84da429

### Features

- Support forcing the execution of overdue transactions in the delayed inbox. (!11667)
- Allow to force upgrade if no blueprints are submitted for 24h. (!11918)

### Bug fixes

- Fix external messages being added to the delayed inbox. (!11779)
- EVM call stack is now the number of internal transaction layers. (!11719)
- Fix bug where CALLCODE shouldn't send the balance to the "to" address. (!11907)
- Fix a bug where precompiled failures were considered as `Fatal`. (!11947)
- Fix a bug where validation would succeed, for a transaction that failed to pay da-fee. (!11992)

### Internals

- Make the stage-1 more resilient to internals errors, to improve the robustness of the upgrade process. (!11874 !11887)
- Nonce is now bumped before executing the transaction. (!11998)

### Breaking changes

### Internal

- `evm-evaluation-assessor` takes 'PREVRANDAO' in its computation. (!11907)
- Implement warm/cold access for state access opcodes. (!11580)
- Remove invalid blueprints. (!11607)

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
- Nested contract creation correctly limits gas according to EIP-150. (!10352)

### Breaking changes

### Internal

- Add a debug feature flag to the log crate for optional debug traces. (!10692)
- Blueprints include timestamp, instead of retrieving it at block finalization. (!10822)
