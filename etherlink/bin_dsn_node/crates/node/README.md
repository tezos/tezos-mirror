# Distributed sequencer node

The DSN node is a binary written in rust that implements the components needed
to run a decentralised sequencer network with threshold encryption support.

The node provides three modes of operation:

 * Bundler: a sidecar for the EVM observer node, responsible for encrypting
   incoming transactions and forwarding them to the EVM sequencer node

 * Sequencer sidecar: a sidecar for the EVM sequencer node, responsible for
   aggregating transactions into proposals, collect attestations and decryption
   shares for encrypted transactions from keyholders, and constructing blueprint
   that will be applied by the EVM sequencer node

 * Keyholder: a standalone node responsible for monitoring proposals from the
   sequencer sidecar, attesting the order of transactions in the proposal and
   providing decryption shares for encrypted transactions.
