This page describes the RPCs specific to the particular version of the Tezos protocol specified above.
(The protocol-independent RPCs are described :doc:`in this other page <../shell/rpc>`.)

.. warning::

    This list could be missing RPC endpoints. The :doc:`OpenAPI
    specification <../api/openapi>` may be used to retrieve the
    complete list of protocol RPCs and their associated schemas
    (search the ``.json`` files that are linked from that page).

Note that the RPCs served under a given prefix can also be listed using the client, e.g.::

    octez-client rpc list /chains/main/blocks/head/context/constants

Any RPC endpoint may also be described, using the ``describe`` RPC to
retrieve all JSON and binary schemas, e.g.::

    octez-client rpc get /describe/chains/main/blocks/head/context/constants

.. note::

    The protocol-specific RPCs are all served under the prefix ``/chains/<chain_id>/blocks/<block_id>/``.
    To make the RPC reference more readable, this prefix is not repeated every time below, but instead shortened as ``../``.
