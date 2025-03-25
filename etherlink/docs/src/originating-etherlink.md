# Starting a new instance of Etherlink

Once a kernel is chosen and built [for
production](building_etherlink.html#for-production), starting a new instance of
Etherlink on a Tezos network consists in the following steps:

1. Originating the utility smart contracts necessary for this instance, like
   a ticketer allowed to mint native tokens, the governance contracts, etc.
2. Writing the setup file for the `smart-rollup-installer`, to initialize the
   [configuration of the instance](etherlink_configuration.html).
3. Originating the Smart Rollup using the `octez-client`.
4. Starting an Octez smart rollup node in operator mode, to commit the state of
   the rollup as it progresses and protects these commitments until they are
   cemented.
5. Starting an EVM node in proxy mode to inspect the state of the new chain as
   it is posted in the L1.
6. Optionally, starting an EVM node in sequencer mode to start producing blocks
   for the new chain.
7. Optionally, starting an EVM node in observer mode to inspect the speculative
   chain of the new chain.

## Originating the utility smart contracts

Each contract shown in this section can be found in [the directory
`etherlink/tezos_contracts` of the Tezos
repository](https://gitlab.com/tezos/tezos/-/tree/master/etherlink/tezos_contracts).

### The ticketer

The ticketer is a smart contract whose FA2.1-compatible tickets are used to
mint native tokens in the Etherlink instance.

If you are planning to use tez as your native tokens, you can reuse the `exchanger.mligo` contract.

```ocaml
{{#include ../../tezos_contracts/exchanger.mligo}}
```

with `ticket_type.mligo` being

```ocaml
{{#include ../../tezos_contracts/ticket_type.mligo}}
```

This contract creates FA2.1 compatible tokens when it receives tez, and gives
back tez when it receives its tickets.


Contrary to delayed Ethereum transactions, which have to come from a specific
address, deposits from any address are accepted by the Etherlink kernel as long
as the transmitted tickets have the correct ticketer address. It is thus
possible to originate intermediary smart contracts acting as bridge as well
(ideally with a companion UI for a better user experience).

See [Depositing on
Etherlink](./layer_1_interactions.html#depositing-on-etherlink) for an example
of such a bridge.
