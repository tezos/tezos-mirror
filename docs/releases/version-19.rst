Version 19.1
============

Version 19 contains a new version (V11) of the protocol environment,
which is the set of functions that a protocol can call.
This new version is used by the new version of :doc:`Oxford <../protocols/018_oxford>`,
protocol proposal for the successor of Nairobi.
This release contains the Oxford 2 protocol proposal itself, as well as its associated protocol-specific executable binaries (baker, accuser, etc).

.. _acl_fix:

Following the recent `security alert <https://forum.tezosagora.org/t/security-alert-recommendations-for-operators-of-public-rpc-nodes/6003>`_ for RPC nodes (and other public-facing infrastructure) operators, version 19.1 hardens the default RPC ACL whitelist when starting an RPC server. The same document provides further recommendations on operating securely public facing infra.

In addition, Octez v19.1 introduces a ``--max-active-rpc-connections <NUM>`` option, that limits the number
of active RPC connections *per server* to the provided argument. The
default limit is set to 100.

Finally, version 19.1 now shuts down the node gracefully when hitting an "unknown key" error raised by Irmin.
This prevents the node to run indefinitely in a failing state.

Rollup node
~~~~~~~~~~~

Starting from version 19, the rollup node is *protocol-agnostic* -- This change was also backported to v18.1.
This means that a single executable, ``octez-smart-rollup-node`` can be used with any Tezos protocols.
The old executable names have been kept as symbolic links, but will be removed in a future version.

The rollup client is not released anymore. Equivalent RPCs to the rollup node must be used instead of its commands.

.. code-block:: rst

    ==========================================  ====================================================
    Command                                     RPC
    ==========================================  ====================================================
    get smart rollup address                    [GET global/smart_rollup_address]
    ------------------------------------------  ----------------------------------------------------
    get state value for <key> [-B --block       [GET global/block/<block>/state]
    <block>]
    ------------------------------------------  ----------------------------------------------------
    get proof for message <index> of outbox     [GET /global/block/<block-id>/helpers/proofs/outbox/
    at level <level> transferring               <outbox_level>/messages] with message index in query
    <transactions>
    ------------------------------------------  ----------------------------------------------------
    get proof for message <index> of outbox     [GET /global/block/<block-id>/helpers/proofs/outbox/
    at level <level>                            <outbox_level>/messages] with message index in query
    ==========================================  ====================================================

The result of encode outbox message ``<transactions>`` can be achieved:
``octez-codec encode alpha.smart_rollup.outbox.message from <transactions>.``

The keys in the Smart Rollup client use the same format as the Octez client.
They can be imported with ``octez-client import secret key <sk_uri>``, or by merging the key files
between the ``octez-client`` base directory and the ``smart-rollup-client-<proto>`` base directory.

The Smart Rollup node now allows multiple :ref:`batcher keys <rollup_batcher>`. Setting multiple
keys for the batching purpose allows to inject multiple operations
of the same kind per block by the rollup node.

Version 19 introduces a :ref:`history-mode option <rollup_history_mode>` for the rollup node.
It can be either ``archive`` or ``full``.
The ``full`` mode integrates garbage collection that reduces the disk usage.
By default, the rollup node runs in ``archive`` mode, without the GC.

Version 19.1 fixes a critical bug that could happen on a ``full`` rollup node.
This bug leads to data loss when chain reorganizations happen while a GC is running.

In addition, it fixes the protocol migration on the rollup node. The constants are now fetched from a correct context, preventing failure in case the rollup node is stopped before processing the protocol migration.

.. warning::

   Due to the :ref:`new default ACL <acl_fix>`, the Octez node requires allowing some specific RPCs from the rollup node if it is running on a remote machine or a different network interface. Check :ref:`the rollup node documentation <smart_rollup_node_prerequisites>` for more details on the prerequisites.

Update Instructions
-------------------

To update from sources::

  git fetch
  git checkout v19.1
  make clean
  opam switch remove . # To be used if the next step fails
  make build-deps
  eval $(opam env)
  make

If you are using Docker instead, use the ``v19.1`` Docker images of Octez.

You can also install Octez using Opam by running ``opam install octez``.

It is now also possible to download experimental Debian and Redhat packages on the `release page <https://gitlab.com/tezos/tezos/-/releases/v19.1>`_  and in the `package registry <https://gitlab.com/tezos/tezos/-/packages>`_.

Changelog
---------

- `Version 19.1 <../CHANGES.html#version-19-1>`_
- `Version 19.0 <../CHANGES.html#version-19-0>`_
- `Version 19.0~rc1 <../CHANGES.html#version-19-0-rc1>`_
