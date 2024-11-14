Octez & Protocol versioning
===========================

Due to the fact that the Tezos blockchain is self-amending, versioning concerns both the Tezos protocol and several components of Octez. This page explains the various corresponding versioning schemes, which are mostly independent of each other. Though, some relations do exist (e.g. when a new protocol is proposed, a new Octez release is usually delivered embedding the new proposal, for convenience); this is explained in some versioning schemes below.

Octez versions
--------------

The whole Octez software is versioned by releases, see :doc:`../releases/releases`.

Protocol versions
-----------------

The Tezos economic protocol is versioned at a different pace, according to a :doc:`voting process <../active/voting>`, see :doc:`../protocols/naming`.

Protocol environment versions
-----------------------------

The economic protocol can interact with the rest of the Octez software through a sandboxed API called a protocol environment.
When new features are needed by a proposed protocol, or if the protocol API is changed, a new environment version is created,
see :ref:`Protocol environment versions <environment_versions>`.
The new environment is delivered as part of a new Octez release.

.. _rpc_versioning:

RPC Versioning
--------------

In Octez, RPCs can be versioned using a query parameter called
``version``. This query parameter exists only for RPCs which have at
least two different versions. For example:

::

   ./octez-client rpc get /chains/main/mempool/pending_operations?version=0

If the RPC is called with a bad version number (a negative or an
unsupported version) the call fails with an error message like:

::

   Fatal error:
   Command failed: The RPC was called with version number '2' which is not supported. Version numbers accepted are '0, 1'.

For technical reasons, the default version number of an RPC cannot be
retrieved easily yet.

New Version
~~~~~~~~~~~

Whenever a new version of an RPC is added (see
:ref:`RPC-versioning-dev-adding-an-rpc`), there is a corresponding
entry in the :doc:`changelog<../CHANGES>` in the release of Octez
which includes the new version.

For example::

   Added version 5 to RPC GET chains/main/mempool/pending_operations.
   It can be used by calling the RPC with the parameter ?version=5
   (default version is still 4).

New Default Version
~~~~~~~~~~~~~~~~~~~

Whenever an Octez release changes the default version of an RPC, there
is a corresponding entry in the :doc:`changelog<../CHANGES>`.

For example::

   The default version for RPC GET chains/main/mempool/pending_operations
   is now 5 (previously 4). You can still use the previous version
   by calling the RPC with the parameter ?version=4.

As a general rule (that we may break exceptionally), changing the
default version number of an RPC always follows a deprecation period.

Deprecated Version
~~~~~~~~~~~~~~~~~~

Whenever an Octez release deprecates an RPC version, there is a
corresponding entry in the :doc:`changelog<../CHANGES>`.

For example::

   The version 4 for RPC GET chains/main/mempool/pending_operations
   is deprecated and may be removed in the next major release of Octez.

Moreover, the following deprecation policy is applied.

Deprecation Policy
~~~~~~~~~~~~~~~~~~

As part of our ongoing efforts to improve and streamline our services,
we have established a deprecation policy for RPC endpoints.

Deprecation: RPC endpoints can be marked as ``DEPRECATED``, which serves as a
fair warning that these endpoints will soon be turned off. Developers are
actively encouraged to upgrade their code to use alternative endpoints at this stage.

Sunset: The sunset of an RPC endpoint indicates that it is no longer
available and supported. This can occur no earlier than ``6`` months
after the version in which the endpoint was deprecated. For example,
an RPC endpoint marked as deprecated in January (such as in Octez
version ``19.x``) could be removed no earlier than July (that could
correspond to an Octez version ``21.x`` -- depending on the release
pace).

The ``DEPRECATED`` flag is visible in the RPC endpoint description in the
:doc:`RPC reference <../shell/rpc>`.

Deprecated RPCs that have passed the sunset date will be deleted. The deleted
RPCs will be advertised as :doc:`breaking changes <../introduction/breaking_changes>`
and in the changelog of Octez versions.

Sunset Dates
""""""""""""

.. When RPCs are set as deprecated, use the following template to
   announce their sunset dates:
   `/path/to/rpc_name <../shell/rpc.html#get-path-to-rpc_name>`__
   <blank line>
   - Deprecated: vX.X, Month 20xx
   - Planned Sunset: vX.0 (not before Qx 20xx)
   - Comment: Use `/new/path/new_name <../shell/rpc.html#get-new-path-new_name>`__ instead..

Currently, the following RPCs are subject to the deprecation policy.

- None
