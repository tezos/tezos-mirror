# Etherlink sequencer

By default, the Etherlink kernel creates one Ethereum-compatible block for
every Tezos level following its origination. However, it is also possible to
configure it to delegate block production to a _sequencer_.

The sequencer is identified by a Tezos implicit account’s public key and a
Ethereum-compatible address. The sequencer creates block proposals (called
_blueprints_), and posts them to the Smart Rollups shared inbox in 4KB
chunks signed with its private key. To reimburse the sequencer for the fees it
has to pay on the Layer 1, Etherlink’s fee model features a “DA fee” (for data
availability fee). That is, part of the gas consumed by an Ethereum-compatible
transaction is dedicated to compensate the sequencer for posting on the Layer
1, and the associated fees are not burnt but credited to the sequencer L2
address (also called the sequencer pool address).
