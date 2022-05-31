This page describes the RPCs specific to a particular version of the Tezos protocol (specified below).

.. note::

    They are all served under the prefix ``/chains/<chain_id>/blocks/<block_id>/``.
    To make the RPC reference more readable, this prefix is not repeated every time below, but instead shortened as ``../``.

RPCs - Index
************

Note that the RPCs served under a given prefix can also be listed using the client, e.g.::

    tezos-client rpc list /chains/main/blocks/head/context/constants

Shell
=====

The protocol-independent RPCs are described :doc:`in this other page <../shell/rpc>`.
