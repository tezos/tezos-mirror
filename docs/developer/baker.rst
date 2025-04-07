Baker application's architecture
================================

The Octez baker is an executable responsible for the creation of blocks and
consensus operations on the Tezos blockchain according to the Tenderbake
consensus algorithm.

The baker is designed as finite-state machine, with states, transitions and
:package-api:`specific consensus-related events <octez-protocol-alpha-libs/Tezos_baking_alpha/Baking_state/index.html#type-event>`.

The machine is orchestrated by a scheduler, which operates in a never-ending
loop to listen for ongoing events, trigger transitions accordingly, and perform
side-effects.

It relies on two workers running alongside the main scheduling loop to manage
auxiliary tasks:

- The operation worker maintains a pool of fresh operations by listening to the node's mempool. It is responsible for tracking consensus and reports ``Prequorum_reached`` and ``Quorum_reached`` events to the main scheduler.

- The forge worker is responsible for crafting blocks and operations. It receives crafting requests from the scheduler and notifies completion with ``New_forge_event (Block_ready block)``, ``New_forge_event (Preattestation_ready op)``, or ``New_forge_event (Attestation_ready op)`` events.

.. _baker_scheduling:

Scheduling
----------

Alongside the operation and forge workers, the
:package-api:`scheduler<octez-protocol-alpha-libs/Tezos_baking_alpha/Baking_scheduling/index.html>`
tracks the following events itself:

- new blocks via the ``/monitor/heads`` and ``/monitor/validated_blocks`` RPCs, where new inputs are converted into ``New_head_proposal p`` and ``New_valid_proposal p`` events, respectively.

- end of rounds and baking times by spawning timers that trigger ``Timeout`` events.

Upon receiving a new event, the scheduler applies the corresponding transition
based on the current state and the event. The transition function returns a new
state and an action. These actions are side-effect requests, such as injecting
operations, forging blocks, or monitoring prequorums.

The scheduler then executes the current action and prepares for the next
loop iteration: it sets a timer to track the next timeout according to the
current state and then sleeps until the next event.

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

The :package-api:`operation
worker<octez-protocol-alpha-libs/Tezos_baking_alpha/Operation_worker/index.html>`
maintains an operations pool by listening for new operations incoming from the
node via the ``/mempool/monitor_operations`` streaming RPC. Every time the node
changes head, the worker flushes the operation pool, opens a new monitoring stream
and starts listening again for new incoming operations.

It carries a unique local proposal for which it monitors consensus. Said
proposal is set/overridden using ``monitor_preattestation_quorum`` and
``monitor_attestation_quorum`` functions. In practice, such functions are called
when the scheduler perfoms the actions ``Watch_prequorum`` and ``Watch_quorum``
respectively.

Once the consensus operations within the operation pool accumulate more voting
power than the proposal threshold, the operation worker informs the scheduler by
escalating ``Prequorum_reached`` or ``Quorum_reached`` events.

.. _baker_forge_worker:

Forge worker
------------

:package-api:`Documentation<octez-protocol-alpha-libs/Tezos_baking_alpha/Forge_worker/index.html>`

