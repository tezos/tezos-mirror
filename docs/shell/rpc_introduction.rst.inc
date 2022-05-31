This page describes the RPCs built into the Octez shell, which are independent from a particular version of the Tezos protocol.

RPCs - Index
************

Note that the RPCs served under a given prefix can also be listed using the client, e.g.::

    tezos-client rpc list /chains/main/levels

Protocol
========

The RPCs implemented by the protocol currently active on Mainnet, served under the prefix ``/chains/<chain_id>/blocks/<block_id>/``, are described :doc:`in this other page <../active/rpc>`.
