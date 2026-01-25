Proof-of-stake
==============

Overview
--------

:doc:`The consensus algorithm <consensus>` in Tezos is based on the
*proof-of-stake* mechanism. Proof-of-stake means that participants
in the consensus algorithm are chosen in function of their stake (the
amount of tokens a participant has). The same mechanism is used in the
Tezos :doc:`governance <voting>`.

If one does not have enough stake to participate on its own or does not want to
set up the needed infrastructure, (s)he can use :ref:`delegation
<delegating_coins>`, possibly complemented with :ref:`staking
<staking_coins>`. Indeed, in Tezos, it is the :ref:`delegates<def_delegate_tallinn>`
that may participate in consensus.
However, at each level, not all delegates necessarily participate, and their participation weight may differ.
The selection of the delegates' participation rights at a level is done by running a
PRNG (pseudo-random number generator).
The PRNG's :ref:`seeds <random_seed_tallinn>` are obtained from random
data that are regularly produced and stored on the blockchain. Thus,
the procedure is deterministic in that delegates' rights are uniquely
determined from the seed; and it is random, in that its seed (and hence its results) cannot
be predicted too much in advance.


Delegation and Staking
----------------------

A *delegate* is any :ref:`user account <def_user_account_tallinn>` registered as
such. This is done by *self-delegating*, that is, emitting a delegation
operation (see below) in which the specified delegate is the same as the
operation emitter (its signer).

Any :ref:`account <def_account_tallinn>` (user account or smart contract) can specify a delegate
through a delegation operation.  Any non-delegate account can change or revoke its delegate
at any time, again through a delegation operation. However, the change only
becomes effective after ``CONSENSUS_RIGHTS_DELAY + 2`` :ref:`cycles <def_cycle_tallinn>`.  The
value ``CONSENSUS_RIGHTS_DELAY`` is a :ref:`protocol constant
<protocol_constants_tallinn>`. A delegate cannot stop self-delegating.

A delegate participates in consensus and in governance in proportion
to their *baking power* and *voting power* respectively.

- The voting power of a delegate is the total amount of tez owned by
  all the accounts that delegate to it (including the delegate
  itself), with no distinctions made between :doc:`staked<staking>`
  and non-staked tez.

- The baking power is similar, except that non-staked tez
  are weighted less than :doc:`staked<staking>` tez, and there are additional
  considerations such as overstaking and overdelegation. See the
  :doc:`Baking Power<baking_power>` page for more details.

Moreover, to participate in consensus and governance, the delegate
needs to be :ref:`active<active_delegate_tallinn>` and to meet
:ref:`minimal balance requirements<minimal_baking_power_tallinn>`.

.. _security_deposit:
.. _security_deposit_tallinn:

Delegates and delegators may :doc:`stake<staking>` their tez. Staked
tez are security deposits that may be forfeited in case the baker does
not follow (some particular rules of) the protocol. Besides, as
mentioned above, staked tez are weighted higher than non-staked tez
when computing the baking power.


.. _consensus_key:
.. _consensus_key_tallinn:

Consensus key
^^^^^^^^^^^^^

The key used by a delegate to sign blocks and consensus operations is called the
*consensus key*. By default, this is the delegate's private key, called its
*manager key*. However, a delegate may specify another, dedicated key for this
role.

Setting a new consensus key is accomplished via the
``Update_consensus_key`` operation. There is delay of
``CONSENSUS_KEY_ACTIVATION_DELAY + 1`` cycles before the new key
actually becomes the *active consensus key* that must be used to sign
blocks and consensus operations; until then, it is called a *pending
consensus key*. More precisely, the key becomes active after the cycle
containing the ``Update_consensus_key`` operation is over and then
another :ref:`CONSENSUS_KEY_ACTIVATION_DELAY<cs_constants_tallinn>` full
cycles have passed: if the update happens during cycle ``n``, then the
key becomes active at the beginning of cycle ``n +
CONSENSUS_KEY_ACTIVATION_DELAY + 1``.

There may be multiple pending consensus keys, set to activate in
different future cycles, and each one will replace the previously
active consensus key in turn. If multiple ``Update_consensus_key``
operations are performed within the same cycle ``n``, the last one
takes precedence for determining which key to activate at the start of
cycle ``n + CONSENSUS_KEY_ACTIVATION_DELAY + 1``.

Note that both the manager key and the consensus key give total
control over the delegate's spendable balance.
See :ref:`this page<consensus_key_details>` for further important details,
including client commands that are helpful for handling consensus keys.


Active delegates and deactivation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. _active_delegate:
.. _active_delegate_tallinn:

A delegate can be marked as active or not. An inactive delegate stops
receiving baking and attesting rights for future cycles.

A delegate is marked as active when it registers. An inactive delegate
may reactivate itself by :ref:`registering as a
delegate<DelegateRegistration>` again. Moreover, if an inactive baker
participates in the consensus (from leftover rights it had received
back when it was still active), then it gets automatically marked as
active again.

At the end of a cycle, a delegate gets deactivated if the chain has
not witnessed any consensus activity (baking, attesting) from it during the
past ``TOLERATED_INACTIVITY_PERIOD`` cycles, including the currently
ending cycle.

Note that there is an extra grace period of ``CONSENSUS_RIGHTS_DELAY``
cycles when a delegate has just registered or has just been
reactivated. This is to account for the fact that it will not receive
consensus rights yet for the first ``CONSENSUS_RIGHTS_DELAY``
cycles, so of course the chain cannot witness any activity from it
during that time.

Delegates' rights selection
---------------------------

Tezos being proof-of-stake, the delegates' rights are selected at
random based on their :doc:`baking power<baking_power>`. Let us detail
the selection mechanism used in Tezos.

.. _random_seed:
.. _random_seed_tallinn:

Random seed
^^^^^^^^^^^

To each cycle is associated a random number called the
seed. This seed is used within its cycle to generate pseudo-random
values in the protocol, in particular for selecting delegates to participate in consensus.

For more information on randomness generation, see :doc:`randomness-generation<randomness_generation>`.

.. _rights:
.. _rights_tallinn:
.. _slots:
.. _slots_tallinn:

Slot selection
^^^^^^^^^^^^^^

Delegates' rights to participate are determined using the `alias
method <https://en.wikipedia.org/wiki/Alias_method>`_, more precisely
using `Vose's algorithm
<https://web.archive.org/web/20131029203736/http://web.eecs.utk.edu/~vose/Publications/random.pdf>`_
(see also `this more pedagogic description
<https://www.keithschwarz.com/darts-dice-coins/>`_; the algorithm is the last one listed there).
This algorithm samples from a discrete probability distribution, which is given by
the :ref:`stakes<active_stake_tallinn>` of a specific cycle: the probability to sample a
particular delegate is its stake in the cycle over the total stake
in that cycle.

Concretely, the delegates' rights at a given level are expressed in terms of
the (quantity of) *slots* that the delegate owns at that level.
This quantity represents the delegate's weight in consensus.
We note that, in the long run (that is, on average over many levels), the number of slots is proportional to its stake.
The owner of a slot is obtained by sampling using the algorithm
mentioned above.
More precisely, given a level and a slot (which is just a non-negative integer),
the mentioned algorithm is invoked to assign a delegate to the given slot.
Its input is the probability distribution given by the stakes retained for the cycle to which the level belongs.
And whenever the algorithm needs to draw a random value, this is obtained using a
simple procedure which has as its initial state: the level, the
:ref:`random seed<random_seed_tallinn>` for the cycle to which the
level belongs, and the slot.


.. _ps_constants:
.. _ps_constants_tallinn:

Proof-of-stake parameters
-------------------------

.. list-table::
   :widths: 55 25
   :header-rows: 1

   * - Parameter name
     - Parameter value
   * - ``BLOCKS_PER_CYCLE``
     - 14400 blocks
   * - ``CONSENSUS_RIGHTS_DELAY``
     - 2 cycles
   * - ``MINIMAL_STAKE``
     - 6,000 ꜩ
   * - ``MINIMAL_FROZEN_STAKE``
     - 600 ꜩ
   * - ``TOLERATED_INACTIVITY_PERIOD``
     - 2 cycles

Further External Resources
--------------------------

The original design of the proof-of-stake mechanism in Tezos can be
found in the `whitepaper
<https://tezos.com/whitepaper.pdf>`_.


The adaptive issuance feature :ref:`documentation <adaptive_issuance_tallinn>`.

Other presentations of the Tezos' proof-of-stake mechanism can be
found in the
`Open Tezos entry <https://opentezos.com/tezos-basics/liquid-proof-of-stake/>`_.
