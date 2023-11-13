Breaking changes
================

This section presents the breaking changes that users can encounter between
different Protocols or Octez versions. It complements the "Breaking changes"
sections in the development changelogs by providing more context and/or less
fragmented mentions.

For each change, there may be a subsection ``deprecation`` and ``breaking
changes``. The first subsection will explain what changes can be made during a
deprecation phase to adapt smoothly to the new changes. The second subsection
will present the changes that can not be done by the deprecation mechanism and
that may be breaking.

Attestations
------------

Starting with the Oxford protocol proposal and the Octez
``v18`` the legacy attestation name ``endorsement`` is now deprecated and
``attestation`` should be used everywhere. Then, ``preendorsement`` is renamed
to ``preattestation``, ``double_preendorsement_evidence`` to
``double_preattestation_evidence``, and ``double_endorsement_evidence`` to
``double_attestation_evidence``. The same goes for operation receipts such as
``lost endorsing rewards``, which are renamed to ``lost attesting rewards``.

To allow a smooth transition we implemented a deprecation mechanism that will
start with Oxford and Octez ``v18`` and should end in two protocols and two
Octez releases. We were not able to version everything so some changes, detailed
below, are breaking.

Deprecation
~~~~~~~~~~~

For the Oxford and Octez ``v18`` we introduced a new :doc:`version argument
<../user/versioning>` ``?version=<n>`` for the following RPCs that can output
``attestation`` (and legacy ``endorsement``):

* ``POST /chains/<chain>/blocks/<block_id>/helpers/scripts/run_operation``
* ``POST /chains/<chain>/blocks/<block_id>/helpers/scripts/simulate_operation``
* ``POST /chains/<chain>/blocks/<block_id>/helpers/preapply/operations``
* ``POST /chains/<chain>/blocks/<block_id>/helpers/parse/operations``
* ``GET /chains/<chain>/blocks/<block_id>``
* ``GET /chains/<chain>/blocks/<block_id>/operations``
* ``GET /chains/<chain>/blocks/<block_id>/operations/<list_offset>``
* ``GET /chains/<chain>/blocks/<block_id>/operations/<list_offset>/<operation_offset>``
* ``GET /chains/<chain>/blocks/<block_id>/metadata``
* ``GET /chains/<chain>/mempool/monitor_operations``
* ``GET /chains/<chain>/mempool/pending_operations``

See :doc:`changelog<../CHANGES>` for more details.

For protocol ``O`` and version ``v18``, using the version ``0``, which is the
default value, will still output the legacy attestation name. Version ``1``
allows the RPCs to output ``attestation`` instead of the legacy name.

For a protocol upgrade proposal to succeed Oxford, i.e. for protocol ``P``, and
the next major release of Octez, v19.0, the default value of these RPCs will be
``1`` but the version ``0`` will still be usable.

Version ``0`` and support for legacy name ("endorsement") will be removed in the
subsequent protocol and major Octez versions -- that is, protocol upgrade
proposal ``Q`` and Octez v20.0

As an exception, for the ``GET /chains/<chain>/mempool/pending_operations`` RPC,
in protocol ``O`` and version ``v18``, due to previous versioning of this RPC,
the legacy version is already ``1`` (currently the default) and you should use
version ``2`` to output ``attestation``.

Breaking changes
~~~~~~~~~~~~~~~~

Starting with protocol Oxford, the protocol
parameters, storage fields and errors that were using the legacy attestation
name now use ``attestation``. The baker and accuser will no longer use the
legacy attestation name in their event messages and errors and will use
``attestation`` instead.
