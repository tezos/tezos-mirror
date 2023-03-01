This page describes the RPCs built into the Tezos shell, which are independent from a particular version of the Tezos protocol.
(The RPCs implemented by the protocol currently active on Mainnet, are described :doc:`in this other page <../active/rpc>`.)

.. warning::

    This list could be missing RPC endpoints. The :doc:`OpenAPI
    specification <../api/openapi>` can be used to retrieve the
    complete list of protocol RPCs and their associated schemas
    (search the ``.json`` files that are linked from that page).

Note that the RPCs served under a given prefix can also be listed using the client, e.g.::

    octez-client rpc list /chains/main/levels

Any RPC endpoint may also be described, using the ``describe`` RPC to
retrieve all JSON and binary schemas, e.g.::

    octez-client rpc get /describe/chains/main/chain_id
