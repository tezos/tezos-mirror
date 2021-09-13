Versions
========

RPC Versioning
--------------

In Octez, RPCs can be versioned using a query parameter called
``version``. This query parameter exists only for RPCs which have at
least two different versions. For example:

::

   ./tezos-client rpc get /chains/main/mempool/pending_operations?version=0

If the RPC is called with a bad version number (a negative or an
unsupported version) the call fails with an error message like:

::

   Fatal error:
   Command failed: The RPC was called with version number '2' which is not supported. Version numbers accepted are '0, 1'.

For technical reasons, the default version number of an RPC cannot be
retrieved easily yet.

New Version
~~~~~~~~~~~

Whenever a new version of an RPC is added, there is a corresponding
entry in the file :src:`CHANGES.rst` in the release of Octez which
includes the new version.

For example::

   Added version 5 to RPC GET chains/main/mempool/pending_operations.
   It can be used by calling the RPC with the parameter ?version=5
   (default version is still 4).

New Default Version
~~~~~~~~~~~~~~~~~~~

Whenever an Octez release changes the default version of an RPC, there
is a corresponding entry in the file :src:`CHANGES.rst`.

For example::

   The default version for RPC GET chains/main/mempool/pending_operations
   is now 5 (previously 4). You can still use the previous version
   by calling the RPC with the parameter ?version=4.

As a general rule (that we may break exceptionnaly), changing the
default version number of an RPC always follows a deprecation period.

Deprecated Version
~~~~~~~~~~~~~~~~~~

Whenever an Octez release deprecates an RPC version, there is a
corresponding entry in the file :src:`CHANGES.rst`.

For example::

   The version 4 for RPC GET chains/main/mempool/pending_operations
   is deprecated and may be removed in the next major release of Octez.

