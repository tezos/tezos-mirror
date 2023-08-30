Development Changelog
'''''''''''''''''''''

**NB:** The changelog for releases can be found at: https://tezos.gitlab.io/CHANGES.html


This file lists the changes added to each version of octez-node,
octez-client, and the other Octez executables. The changes to the economic
protocol are documented in the ``docs/protocols/`` directory; in
particular in ``docs/protocols/alpha.rst``.

When you make a commit on master, you can add an item in one of the
following subsections (node, client, …) to document your commit or the
set of related commits. This will ensure that this change is not
forgotten in the final changelog, which can be found in ``docs/CHANGES.rst``.
By having your commits update this file you also make it easy to find the
commits which are related to your changes using ``git log -p -- CHANGES.rst``.
Relevant items are moved to ``docs/CHANGES.rst`` after each release.

Only describe changes which affect users (bug fixes and new features),
or which will affect users in the future (deprecated features),
not refactorings or tests. Changes to the documentation do not need to
be documented here either.

General
-------

Node
----

- Bump RPCs ``POST ../helpers/parse/operations``, ``POST
  ../helpers/scripts/run_operation`` and ``POST
  ../helpers/scripts/simulate_operation`` default version to version ``1``.
  Version ``0`` can still be used with ``?version=0`` argument. (MR :gl:`!9840`)

Client
------

- For the protocols that support it, added an
  ``operation_with_legacy_attestation_name`` and
  ``operation_with_legacy_attestation_name.unsigned`` registered encodings that
  support legacy ``endorsement`` kind instead of ``attestation``. (MR
  :gl:`!9832`)

- Fixed indentation of the stacks outputted by the ``normalize stack``
  command. (MR :gl:`!9944`)

Baker
-----

Accuser
-------

Proxy Server
------------

Protocol Compiler And Environment
---------------------------------

Codec
-----

Docker Images
-------------

Smart Rollup node
-----------------

Smart Rollup client
-------------------

Smart Rollup WASM Debugger
--------------------------

- Added flag ``--no-kernel-debug`` to deactivate the kernel debug messages. (MR
  :gl:`!9813`)

Data Availability Committee (DAC)
---------------------------------

Miscellaneous
-------------
