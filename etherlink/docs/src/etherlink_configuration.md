# Understanding Etherlink configuration

The behavior of an Etherlink instance can be parameterized by the existence and
content of several key files in its durable storage.

These files can be created at origination time using the
`smart-rollup-installer` setup file (see [Originating a new instance of
Etherlink](./originating-etherlink.md), or when performing an upgrade through
the migration mechanism.

## Ticketer

If `/evm/ticketer` exists and contains a valid b58checked-encoded Tezos smart
contract address, then FA2.1 tickets from this smart contract are used to mint
the native token of an Etherlink instance.

See [Depositing on
Etherlink](./layer_1_interactions.html#depositing-on-etherlink) for more
information on the subject.

## Sequencer

The Etherlink kernel is meant to be configured to delegate block production
to a _sequencer_, but in the absence of `/evm/sequencer` in the context, it
will fallback to operating in proxy mode, where it fetches Ethereum
transactions from the Smart Rollups’ shared inbox and creates one
Ethereum-compatible block for every Tezos level following its origination.

If the file `/evm/sequencer` exists and if its contents is a b58checked encoded
valid Tezos implicit account public key (only  public keys for `tz1`, `tz2` and
`tz3` are supported, `tz4` are not), then the instance will operate in
sequencer mode.

Additionally, if the file `/evm/sequencer_pool_address` exists and contains a
valid binary Ethereum address, the so-called DA fees are transferred to this
address.

See [Etherlink sequencer](./etherlink_sequencer.md) for more information.

## Delayed Inbox

When an Etherlink instance is operating in sequencer mode, the delayed inbox is
the component preventing the sequencer to censor transactions. It can contain
two kind of items: deposits (of either native tokens or FA tokens) and
Ethereum-compatible transactions. This way, third-parties distrusting the
sequencer operator can still post their transactions directly onto the Layer 1,
without the need for third-party intermediaries.

### Delayed Bridge

To enable support for posting Ethereum-compatible transactions directly onto
the Layer 1, it is necessary to specify a smart contract acting as a delayed
bridge.

The file `/evm/delayed_bridge`  is expected to contain the b58checked encoded
address of the delayed bridge to enable injecting Ethereum-compatible
transactions through the Layer 1.

This smart contract is expected to implement countermeasures against spamming,
such as necessitating to pay an additional fee to be allowed to post a
transaction. See [Posting delayed transactions with a delayed
bridge](./layer_1_interactions.html#posting-delayed-transactions-with-a-delayed-bridge)
for more information.

### Delayed Inbox timeout

The delayed inbox acts as a temporary buffer hosting the deposits and
Ethereum-compatible transactions until they are included in a block by the
sequencer.

If the sequencer fails to include an item of the delayed inbox for too long,
it is considered “overdue.” If this happens, then Etherlink kernel will reject
its blueprints and create one block containing the content of the delayed
inbox instead. We refer to this mechanism as the delayed inbox timeout. A
delayed inbox timeout effectively invalidates the speculative history of the
sequencer.

The amount of time (in seconds) a sequencer is given to include a delayed inbox
item is defined by the `/evm/delayed_inbox_timeout` file, encoded as a little
endian unsigned 64-bit integer. It defaults to 12 hours.

Additionally, the sequencer is given a certain amount of Layer 1 blocks to
include delayed transactions. This additional requirement is here to prevent a
sequencer to see its speculative history invalidated because of a Layer 1
outage. The number of blocks to include a delayed transaction is defined by
the content of the file `/evm/delayed_inbox_min_levels`, encoded as a
little-endian unsigned 32-bit integer. It defaults to 720.

A timeout occurs if and only if too much time has passed _and_ too many l1
blocks have been produced.

## Governance contracts

### Kernel upgrades

An Etherlink instance can support receiving upgrade payloads from up to three
smart contracts, as specified by the contents (b58checked-encoded) of three
files: `/evm/kernel_governance`, `/evm/kernel_security_governance`, and
`/evm/admin`.


Although the names of these files are meant to describe the type of governance
implemented by the Tezos smart contracts they correspond to, the rollup does
not differentiate between the three addresses and interprets the payload it
receives from them in the same way.

Said payload is a RLP-encoded structure containing two fields: the root hash of
the kernel (that the current kernel will fetch using the reveal data channel),
and an activation timestamp.

See [Upgrading Etherlink](layer_1_interactions.html#upgrading-etherlink) for
more information.

### Sequencer upgrade

An Etherlink instance supports receiving sequencer upgrade payload from
one smart contract, as specified by the content (b58-checked encoded) of
the `/evm/sequencer_governance` file.

The payload expected from this contract is a RLP-encoded structure containing
three fields: the public key of the sequencer, the L2 address to be used to
reimbursed the DA fees, and an activation timestamp.

See [Upgrading Etherlink
sequencer](layer_1_interactions.html#upgrading-etherlink-sequencer) for more
information.

## Bootstrap Accounts

The native tokens balance of a given Ethereum-compatible address is specified
in the file `/evm/world_state/eth_accounts/<address>/balance`.

For instance, the balance of the address
`b53dc01974176e5dff2298c5a94343c2585e3c54` is stored at
`/evm/world_state/eth_accounts/b53dc01974176e5dff2298c5a94343c2585e3c54/balance`.

The balance is a little-endian 256-bit unsigned integer.

## Fees

Fees in Etherlink are controlled by two parameters: the minimum base fee per
gas unit and the DA fee per byte.

The minimum base fee per gas unit is defined by the contents of the file
`/evm/world_state/fees/minimum_base_fee_per_gas`. If the file does not exist,
the minimum base fee per gas unit defaults to \\( 10^9 \\) native tokens.

The DA fee per byte is defined by the contents of the file
`/evm/world_state/fees/da_fee_per_byte`. It is only used when Etherlink is
operated in sequencer mode. If the file does not exist, the DA fee per byte
defaults to \\( 4.10^12 \\) native tokens, because Etherlink official instance
uses tez as its native tokens and 4 mutez is enough to cover the cost for
the sequencer of creating a block containing only this transaction.

The speed limit of Etherlink is currently hard-coded in the file
`etherlink/kernel_latest/kernel/src/gas_price.rs`.

See [Etherlink fee model](fee_model.md) for more information.
