.. _version-9:

Version 9.0~rc1
===============

The first release candidate for version 9.0 contains a new version
(V2) of the protocol environment, which is the set of functions that
protocols can call.  This new version is used by Florence, which is
the current protocol proposal on Mainnet. The release candidate also
contains Florence itself as well as its daemons (baker, endorser and
accuser) so that you can test it easily.

This release candidate also contains the necessary configuration to
join the Florencenet test network, which runs Florence. To join
Florencenet, simply configure your node with ``tezos-node config
init --network florencenet``.

Update Instructions
-------------------

To update from sources::

  git fetch
  git checkout v9.0-rc1
  rm -rf _opam _build
  make build-deps
  eval $(opam env)
  make

If you are using Docker instead, use the ``v9.0`` Docker images of Tezos.

Changelog — Version 9.0~rc1
---------------------------

Node
~~~~

- Added Florence, the current protocol proposal on Mainnet.
  This is the version of Florence without baking accounts (``PsFLoren``).

- Added a new version of the protocol environment (v2).
  It is used by Florence.

- Added built-in network configurations for Edo2net (which runs Edo2,
  the current Mainnet protocol) and Florencenet (which runs Florence).
  Their corresponding aliases for ``--network`` are ``edo2net`` and ``florencenet``.

- Capped the number of expected connections to ``100`` on the command-line
  interface.

- Fixed a bug that caused the execution of the prevalidator when the node was not
  bootstrapped.

- Enforced loading of non-embedded protocols before starting the node
  to allow the prevalidator to start correctly.

- Optimized I/O and CPU usage by removing an unnecessary access to the
  context during block validation.

- Fixed a bug where any event would allocate more memory than needed
  when it was not to be printed.

- Added a new RPC for Alpha: ``helpers/scripts/normalize_type``.

- Replace Edonet by Edo2net in built-in network configuration.
  The alias to give to ``--network`` is now ``edo2net``.

- Removed the built-in configuration for Delphinet. You can no longer
  configure your node with ``--network delphinet``.

- The ``--network`` option now also accepts the name of a file
  containing the configuration for a custom network,
  or a URL from which such a file can be downloaded.

- Fixed JSON encoding of timestamps before epoch (1970).
  Pretty-printing and encoding of dates before epoch in human-readable form (as part
  of a JSON value) that failed in the past will now succeed. Binary
  form (used when nodes exchange data) was unaffected by the bug. This
  may impact some RPC representations of timestamps.

- Some RPCs now send their response in chunked transfer encoding.
  Additionally, the implementation allows for more concurrency internally: it
  allows RPC requests to be treated even if a request is currently being
  treated. This leads to some improved response times on some RPC requests.

Client
~~~~~~

- Fixed the return code of errors in the client calls to be non-zero.

- Added a new multisig command to change keys and threshold:
  ``set threshold of multisig contract ...``.

- Added a command to perform protocol migrations in persistent mockup mode:
  ``migrate mockup to <protocol_hash>``.

- Added the ``--version`` flag.

- Fixed commands ``--mode mockup config show`` and ``--mode mockup config init``
  which returned the default values rather than the actual ones.

- Replaced command ``check that <bytes> was signed by <pkh>`` by ``check that bytes
  <bytes> were signed by <pkh>`` to differentiate from new command ``check that
  message <string> was signed by <pkh>``.

- Added wallet support for PVSS keys.

- Added support for all protocol constants in Mockup mode.

Baker / Endorser / Accuser
~~~~~~~~~~~~~~~~~~~~~~~~~~

- Added the ``--version`` flag.

- Fixed the operation ordering in the baker so that the most
  profitable operations are applied first.

Protocol Compiler And Environment
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- Added the ``--version`` flag.

Codec
~~~~~

- Added the ``--version`` flag.

- Added support for some base encodings including arbitrary precision integers, n-bit
  sized integers, and floating point numbers.

Miscellaneous
~~~~~~~~~~~~~

- Sapling: fixed dummy address generator (the last 5 bits are now correctly set to 0
  instead of the first 5 bits).

- Fixed a bug that caused some file descriptors to be leaked to external processes.
