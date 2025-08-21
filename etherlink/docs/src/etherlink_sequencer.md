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


## Sequencer upgrade

The sequencer upgrade goes through the layer 1 using a smart contract,
similarly to the kernel upgrade. The sequencer upgrade is applied in
the kernel on the first l1 level with a l1 timestamp inferior to the
activation timestamp.

In the EVM node as it's independent of the l1, it uses `0` (`epoch`)
for any given kernel run. When there is a pending sequencer upgrade,
the first blueprint applied that has a timestamp superior to the
upgrade activation timestamp triggers a special kernel run with an
empty inbox, using the blueprint timestamp as of the l1 timestamp in
the inbox, and with a state containing the upgrade.
