Baker
=====

The Octez baker is an executable responsible for the creation of blocks and
consensus operations on the Tezos blockchain with respect to the Tenderbake
consensus algorithm.

The baker is designed as finite-state machine, with states, transitions and
:package-api:`events<octez-protocol-alpha-libs/Tezos_baking_alpha/Baking_state/index.html#type-event>`.

The machine is orchestrated by a scheduler, which operates in a never-ending
loop to listen for ongoing events, trigger transitions accordingly, and perform
side-effects.

It relies on two workers running alongside the main scheduling loop to manage
auxiliary tasks:

- the operation worker monitors consensus operations from the node via the ``/mempool/monitor_operations`` RPC, reporting ``Prequorum_reached`` and ``Quorum_reached`` events to the main scheduler.

- the forge worker is responsible for crafting blocks and operations. It receives crafting requests from the scheduler and notifies it back upon completion with ``New_forge_event (Block_ready block)``, ``New_forge_event (Preattestation_ready op)``, or ``New_forge_event (Attestation_ready op)`` events.

.. _baker_scheduling:

Scheduling
----------

:package-api:`Documentation<octez-protocol-alpha-libs/Tezos_baking_alpha/Baking_scheduling/index.html>`

.. _baker_states:

States
------

:package-api:`Documentation<octez-protocol-alpha-libs/Tezos_baking_alpha/Baking_state/index.html>`

.. _baker_transitions:

Transitions
-----------

:package-api:`Documentation<octez-protocol-alpha-libs/Tezos_baking_alpha/State_transitions/index.html>`

.. _baker_actions:

Actions
-------

:package-api:`Documentation<octez-protocol-alpha-libs/Tezos_baking_alpha/Baking_actions/index.html>`

.. _baker_operation_worker:

Operation worker
----------------

:package-api:`Documentation<octez-protocol-alpha-libs/Tezos_baking_alpha/Operation_worker/index.html>`

.. _baker_forge_worker:

Forge worker
------------

:package-api:`Documentation<octez-protocol-alpha-libs/Tezos_baking_alpha/Forge_worker/index.html>`

