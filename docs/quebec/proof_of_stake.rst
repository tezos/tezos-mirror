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
<delegating_coins>`. Therefore, in Tezos, it is the :ref:`delegates<def_delegate_qena>`
that may participate in consensus.
However, at each level, not all delegates necessarily participate, and their participation weight may differ.
The selection of the delegates' participation rights at a level is done by running a
PRNG (pseudo-random number generator).
The PRNG's :ref:`seeds <random_seed_qena>` are obtained from random
data that are regularly produced and stored on the blockchain. Thus,
the procedure is deterministic in that delegates' rights are uniquely
determined from the seed; and it is random, in that its seed (and hence its results) cannot
be predicted too much in advance.


Delegation and Staking
----------------------

A *delegate* is any :ref:`user account <def_user_account_qena>` registered as
such. This is done by *self-delegating*, that is, emitting a delegation
operation (see below) in which the specified delegate is the same as the
operation emitter (its signer). Note that ``tz4`` accounts cannot be registered
as delegates.

Any :ref:`account <def_account_qena>` (user account or smart contract) can specify a delegate
through a delegation operation.  Any non-delegate account can change or revoke its delegate
at any time, again through a delegation operation. However, the change only
becomes effective after ``CONSENSUS_RIGHTS_DELAY + 2`` :ref:`cycles <def_cycle_qena>`.  The
value ``CONSENSUS_RIGHTS_DELAY`` is a :ref:`protocol constant
<protocol_constants_qena>`. A delegate cannot stop self-delegating.

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
needs to be :ref:`active<active_delegate_qena>` and to meet
:ref:`minimal balance requirements<minimal_baking_power_qena>`.

Delegates and delegators may :doc:`stake<staking>` their tez. Staked
tez are security deposits that may be forfeited in case the baker does
not follow (some particular rules of) the protocol. Besides, as
mentioned above, staked tez are weighted higher than non-staked tez
when computing the baking power.


Consensus key
^^^^^^^^^^^^^

The key used by a delegate to sign blocks and consensus operations is called the
*consensus key*. By default, this is the delegate's private key, called its
*manager key*. However, a delegate may specify another, dedicated key for this
role. See :ref:`this page<consensus_key>` for further important details. In particular,
both the delegate key and the consensus key give total control over the
delegate's funds: indeed, the consensus key may sign a *drain* operation to
transfer the delegate's free balance to an arbitrary account.  In :doc:`relevant RPCs<../api/openapi>`,
like ``/chains/main/blocks/head/helpers/baking_rights``, both the delegate's
manager and consensus keys are listed.

If the :ref:`adaptive issuance <adaptive_issuance_qena>`
feature is activated, it grants delegators the ability to become
'stakers' by placing security deposits. These deposits would contribute to their
delegate's stake and could be subject to slashing penalties if their delegate
misbehaves.  The staking power of funds placed by stakers and delegates is twice
that of delegated funds.

Active and passive delegates
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. _active_delegate_qena:

A delegate can be marked as either active or passive. A passive
delegate cannot participate in the consensus algorithm.

A delegate is marked as active at its registration.

A delegate becomes passive at the end of cycle ``n`` when it has
failed to participate in the consensus algorithm in
the past ``CONSENSUS_RIGHTS_DELAY + 1`` cycles. That is, in cycles ``n``, ``n-1``,
``n-2``, ..., ``n - CONSENSUS_RIGHTS_DELAY``.

Delegates' rights selection
---------------------------

Tezos being proof-of-stake, the delegates' rights are selected at
random based on their :doc:`baking power<baking_power>`. Let us detail
the selection mechanism used in Tezos.

.. _random_seed_qena:

Random seed
^^^^^^^^^^^

To each cycle is associated a random number called the
seed. This seed is used within its cycle to generate pseudo-random
values in the protocol, in particular for selecting delegates to participate in consensus.

For more information on randomness generation, see :doc:`randomness-generation<randomness_generation>`.

.. _rights_qena:
.. _slots_qena:

Slot selection
^^^^^^^^^^^^^^

Delegates' rights to participate are determined using the `alias
method <https://en.wikipedia.org/wiki/Alias_method>`_, more precisely
using `Vose's algorithm
<https://web.archive.org/web/20131029203736/http://web.eecs.utk.edu/~vose/Publications/random.pdf>`_
(see also `this more pedagogic description
<https://www.keithschwarz.com/darts-dice-coins/>`_; the algorithm is the last one listed there).
This algorithm samples from a discrete probability distribution, which is given by
the :ref:`stakes<active_stake_qena>` of a specific cycle: the probability to sample a
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
:ref:`random seed<random_seed_qena>` for the cycle to which the
level belongs, and the slot.


.. _ps_constants_qena:

Proof-of-stake parameters
-------------------------

.. list-table::
   :widths: 55 25
   :header-rows: 1

   * - Parameter name
     - Parameter value
   * - ``BLOCKS_PER_CYCLE``
     - 30720 blocks
   * - ``CONSENSUS_RIGHTS_DELAY``
     - 2 cycles
   * - ``MINIMAL_STAKE``
     - 6,000 ꜩ
   * - ``MINIMAL_FROZEN_STAKE``
     - 600 ꜩ

Further External Resources
--------------------------

The original design of the proof-of-stake mechanism in Tezos can be
found in the `whitepaper
<https://tezos.com/whitepaper.pdf>`_.


The adaptive issuance feature :ref:`documentation <adaptive_issuance_qena>`.

Other presentations of the Tezos' proof-of-stake mechanism can be
found in the
`Open Tezos entry <https://opentezos.com/tezos-basics/liquid-proof-of-stake/>`_.
