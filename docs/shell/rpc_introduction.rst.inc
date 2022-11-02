This page describes the RPCs built into the Octez shell, which are independent from a particular version of the Tezos protocol.

.. warning::

    This list could be missing RPC endpoints. The :doc:`OpenAPI
    specification <../api/openapi>` can be used to retrieve the
    complete list of protocol RPCs and their associated schemas
    (search the ``.json`` files that are linked from that page).

RPCs - Index
************

Note that the RPCs served under a given prefix can also be listed using the client, e.g.::

    tezos-client rpc list /chains/main/levels

Any RPC endpoint may also be described, using the ``describe`` RPC to
retrieve all JSON and binary schemas, e.g.::

    tezos-client rpc get /describe/chains/main/chain_id

Protocol
========

The RPCs implemented by the protocol currently active on Mainnet, served under the prefix ``/chains/<chain_id>/blocks/<block_id>/``, are described :doc:`in this other page <../active/rpc>`.
