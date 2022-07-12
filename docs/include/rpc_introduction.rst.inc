This page describes the RPCs specific to a particular version of the Tezos protocol (specified below).

.. note::

    They are all served under the prefix ``/chains/<chain_id>/blocks/<block_id>/``.
    To make the RPC reference more readable, this prefix is not repeated every time below, but instead shortened as ``../``.

.. warning::

    Some dynamic or plugin RPCs are not listed here due to a current limitation in the documentation generator. They are listed in the :doc:`../api/openapi` (search the ``.json`` files that are linked from that page).

RPCs - Index
************

Note that the RPCs served under a given prefix can also be listed using the client, e.g.::

    tezos-client rpc list /chains/main/blocks/head/context/constants

Shell
=====

The protocol-independent RPCs are described :doc:`in this other page <../shell/rpc>`.
