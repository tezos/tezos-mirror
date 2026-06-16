Baking Power
============

The :doc:`proof-of-stake<proof_of_stake>` mechanism used for the
:doc:`consensus algorithm<consensus>` assigns baking and attesting
:ref:`slots<slots_tallinn>`, called **baking rights**, to each
:ref:`delegate a.k.a. baker<def_delegate_tallinn>`. For this selection
process, each baker is weighted according to its **baking power** --
provided that it is :ref:`active<active_delegate_tallinn>` and meets the
:ref:`minimal power and own staked
requirements<minimal_baking_power_tallinn>`.

This page details how this baking power is determined from the
:doc:`staked<staking>` and non-staked funds owned by the baker itself
and all its delegators.

Note that the :doc:`amendment and voting process<voting>` is based on
each delegate's :ref:`voting power<voting_power_tallinn>` instead, which
is computed in a similar but simpler way.


.. _RPC_path_shortcut:
.. _RPC_path_shortcut_tallinn:

.. note::

  In this page, the prefix ``.../`` in all RPC paths is standing for
  ``/chain/<chain_id>/blocks/<block_id>/context/``

  Besides, many RPCs presented here used to be known under different
  names. They have been renamed in the Quebec protocol in order to
  disambiguate and normalize the baking power lexicon; the old names
  have been deprecated. See the
  :ref:`changelog<delegates_RPCs_normalization>` for more information.


.. _baking_power_overview:
.. _baking_power_overview_tallinn:

Overview
--------

At the end of :ref:`cycle<def_cycle_tallinn>` ``n`` (that is, the
beginning of cycle ``n + 1``), the protocol :doc:`randomly
generates<randomness_generation>` the baking rights for cycle ``n +
1 + CONSENSUS_RIGHTS_DELAY = n + 3``, using the **current baking
power** as the weight for each delegate that meets the
:ref:`requirements<minimal_baking_power_tallinn>`. (``CONSENSUS_RIGHTS_DELAY
= 2`` is a :ref:`protocol constant<cs_constants_tallinn>`.)

The ``.../delegates/<delegate_pkh>/baking_power`` RPC can be used to
retrieve the current baking power of a delegate, that is, its baking
power as of the end of the requested block ``<block_id>`` (see the
note above on RPC paths). Therefore, the baking power used for the
rights of cycle ``n + 3`` is the one returned by this RPC called on
the last block of cycle ``n``.

The baking power of a delegate is defined as:

.. code-block:: python

  baking_power = total_staked_after_limits + (total_delegated_after_limits / 3)

This page explains the relevant concepts and provides the detailed
computations of ``total_staked_after_limits`` and
``total_delegated_after_limits``.


Delegate, delegators, stakers
-----------------------------

A **delegate**, a.k.a. **baker**, is a :ref:`user
account<user_accounts_tallinn>` that has registered as a delegate by
emitting a self-``delegation`` :ref:`manager
operation<manager_operations_tallinn>`. The list of all registered
delegates is queried with the ``.../delegates`` RPC.

A **delegator** for a given baker is an :doc:`account<accounts>` that
has registered this baker as its delegate by emitting a ``delegation``
operation. This includes the baker itself. A delegator may be a user
account or a smart contract. The list of delegators for a given
delegate is queried with the
``.../delegates/<delegate_pkh>/delegators`` RPC.

A **staker** is a delegator that has :doc:`staked<staking>` tez by
emitting a :ref:`stake operation<staked_funds_management_tallinn>`. This
includes the delegate itself if it has staked funds. Note that stakers
are always user accounts, because smart contracts cannot emit
``stake`` operations. The list of a delegate's stakers and their
respective staked balances (see below) are queried with the
``.../delegates/<delegate_pkh>/stakers`` RPC.

An **external delegator** (resp. **external staker**) is a delegator
(resp. staker) that is not the delegate itself.


.. _total_staked:
.. _total_staked_tallinn:

Staked tez
----------

Delegates and delegators have the option to :doc:`stake<staking>`
their tez. **Staked tez** contribute to the baking power, but they
also function as a security deposit for baking, meaning that they may
be :ref:`slashed<slashing_tallinn>` if the delegate misbehaves. That's
why they are also known as **frozen deposits**.

The **staked balance** of an account is its amount of staked tez. It
can be queried with the ``.../contracts/<contract_id>/staked_balance``
RPC (in mutez). Note that if an account does not have a delegate, then
it cannot have any staked tez so its staked balance is zero.

For a given delegate, we define the following:

- ``own_staked`` is the staked balance of the delegate itself. It can
  be queried with either RPC
  ``.../contracts/<contract_id>/staked_balance`` or
  ``.../delegates/<delegate_pkh>/own_staked`` (in mutez).

- ``external_staked`` is the sum of the staked balances of the
  delegate's external stakers. It is queried with the
  ``.../delegates/<delegate_pkh>/external_staked`` RPC (in mutez).

- ``total_staked`` is the sum of the staked balances of all stakers,
  including the delegate itself. It is queried with the
  ``.../delegates/<delegate_pkh>/total_staked`` RPC (in mutez).

All three values are of course related:

.. code-block:: python

  total_staked = own_staked + external_staked


Delegated tez
-------------

Non-staked tez owned by delegates and delegators are called
**delegated tez**. They also contribute to the delegate's baking
power, without being subject to slashing. However, delegated tez
weigh a third as much as staked tez for the purpose of computing the
baking power.

Delegated tez of an account
^^^^^^^^^^^^^^^^^^^^^^^^^^^

The **delegated balance** of an account is its amount of delegated
tez. It is the sum of the following balances:

- The **spendable balance** is the amount of tez that the account can
  freely access and spend at the current time. It can be queried with
  RPC ``.../contracts/<contract_id>/spendable`` (in mutez).

- The **unstaked balance** is the sum of the tez amounts contained in
  all of the account's **unstake requests** (both unfinalizable and
  finalizable unstake requests). These tez have been removed from the
  staked balance via an ``unstake`` operation, but have not been added
  back to the spendable balance yet; see
  :ref:`staked_funds_management_tallinn`. Unstake requests can be
  queried with RPC ``.../contracts/<contract_id>/unstake_requests``
  (returns a detailed view with unfinalizable/finalizable status,
  delegate-at-creation-time, cycle, and amount in mutez).

- The **frozen bonds** are a deposit for :ref:`rollup
  commitments<commitments_tallinn>`. They can be queried with RPC
  ``.../contracts/<contract_id>/frozen_bonds`` (in mutez).

Together, the staked and delegated tez represent all the tez owned by
an account, called the **full balance**.

.. code-block:: python

  delegated = spendable + unstaked + frozen_bonds

  full_balance = staked + delegated


.. _total_delegated:
.. _total_delegated_tallinn:

Delegated tez to a baker
^^^^^^^^^^^^^^^^^^^^^^^^

Spendable tez and frozen bonds count as delegated to the account's
current delegate. However, the tez involved in an unstake request
count as delegated to the
account's **delegate at the time of the unstake request's creation**
(which is the account's current delegate in most cases, but might be a
former delegate instead).

To sum up:

.. code-block:: python

  delegated = (spendable + unstaked_from_current_delegate + frozen_bonds) + unstaked_from_former_delegates
  delegated =        delegated_to_current_delegate                        +  delegated_to_former_delegates

  delegated_to_current_delegate = delegated - delegated_to_former_delegates
  delegated_to_current_delegate = full_balance - staked - unstaked_from_former_delegates



For a given delegate, we define the following:

- ``own_delegated`` is the amount of delegated tez owned by the baker
  and counting as delegated to itself, that is, the
  ``delegated_to_current_delegate`` amount of the delegate's
  account. It corresponds to all non-staked tez owned by the baker
  (except for any tez involved in unstake requests created at a time
  when the baker was delegating to a different delegate, but it is
  rare for a delegate to still have such requests). It can be queried
  with RPC ``.../delegates/<delegate_pkh>/own_delegated`` (in mutez).

- ``external_delegated`` is the sum of tez that count as delegated to
  the baker but are not owned by the baker itself. In other words, it
  is the sum of ``delegated_to_current_delegate`` over all current
  external delegators, plus any tez involved in unstaked requests
  created by former external delegators when they were still
  delegating to the baker. It
  can be queried with RPC
  ``.../delegates/<delegate_pkh>/external_delegated`` (in mutez).

- ``total_delegated`` is the total amount that counts as delegated to
  this baker. It can be queried with RPC
  ``.../delegates/<delegate_pkh>/total_delegated`` (in mutez).

::

  total_delegated = own_delegated + external_delegated

.. _min_delegated:
.. _min_delegated_tallinn:

Min-delegated-in-current-cycle
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

When computing baking rights for cycle ``n + 3`` at the end of cycle
``n``, the ``total_delegated`` value used for each delegate is
actually the **minimum** of its ``total_delegated`` **over the whole
cycle** ``n``, called ``min_delegated_in_current_cycle``. The purpose
of this mechanism is to prevent any manipulation of baking rights
through short-duration transfers. (Note that such a mechanism is not
needed for staked tez because they are inherently :ref:`frozen for at
least four cycles<staked_funds_management_tallinn>`, so short-duration
staking is already not possible.)

Since the Paris protocol, the considered minimum is the minimum at any
point during block applications, which can be reached in the middle of
executing a transaction.

For example, if a baker transfers tez to one of its delegators, this
is internally treated as first removing the transferred amount from
the total amount delegated to this baker, then adding it back. In
between executing both updates, the total delegated amount is lower so
it might be the new minimum over the whole cycle. In other words, the
transferred tez risk not counting towards the baking rights that will
be computed at the end of the cycle, even though they have been owned
by an account delegating to this baker during the whole cycle.

Besides, when the minimum is reached in the middle of a block's
operations, the context for this minimum is not directly accessible
via RPC. In that case, in order to retrieve this exact context, one
needs to replay the block's balance updates on their own.

In the Quebec protocol, to solve these problems, only the total
delegated amounts **at the end of blocks** count when determining this
minimum. This is known as the **per-block min-delegated
feature**. This solution no longer penalizes baking rights when
funds are transferred between two accounts delegated to the same
baker. Moreover, it lets users easily retrieve via RPC the exact
context that the minimum comes from, since it is guaranteed to
correspond to the end of a block.

The min-delegated-in-current-cycle can be retrieved with RPC ``GET
/chains/<chain_id>/blocks/<block_id>/context/delegates/<delegate_pkh>/min_delegated_in_current_cycle``.
This RPC returns the minimal value of the baker’s ``total_delegated``
at the end of any block, from the first block of the current cycle up
to the current level, where "current cycle" and "current level" are
those of the ``<block_id>`` in the RPC path; it also returns the
earliest level at the end of which this minimum has been reached in
the current cycle. This means that calling the RPC on the last block
of cycle ``n`` returns the value actually used during the generation
of baking rights for cycle ``n + 3``.


Example
"""""""

At level ``150``, the baker receives ``50`` tez from a contract that
is not a delegator for this baker. Then, at level ``200``, the baker
transfers ``150`` tez to one of its delegators. Finally, at level
``205``, that delegator sends ``70`` tez to another contract that is
not a delegator for this baker.

The first transfer of ``50`` tez increases the baker’s ``total_delegated``
by ``50``. Then, the transfer of ``150`` tez is internally implemented
as removing ``150`` tez from the total delegated of the sender’s
delegate -- which is the baker itself, then adding ``150`` tez to the
``total_delegated`` of the destination’s delegate -- which is the same
baker in our case. Finally, the ``70``-tez transfer just removes
``70`` tez from the ``total_delegated`` of the same baker again.

Let's say that ``blocks_per_cycle = 128``, so the
first level of the current cycle is ``129``, and let's say that
``total_delegated`` was ``1000`` at the beginning of the cycle.

.. list-table::
   :widths: 14 16 14 14 14 14 14
   :header-rows: 1

   * -
     -
     - L129: first level of cycle
     - L150: add 50
     - L200: remove 150
     - L200: add 150
     - L205: remove 70
   * -
     - Baker's current ``total_delegated`` (tez)
     - 1000
     - 1050
     - 900
     - 1050
     - 980
   * - Quebec RPC
     - Returned min (tez)
     - 1000
     - 1000
     -
     - 1000
     - 980
   * - Quebec RPC
     - Returned level
     - 129
     - 129
     -
     - 129
     - 205
   * - Paris RPC
     - Returned min (tez)
     - 1000
     - 1000
     -
     - 900
     - 900
   * - Paris RPC
     - Returned level
     - None
     - 150
     -
     - 200
     - 200

Note there are empty cells in the table as RPCs cannot be called in
the middle of the block application. Also, the
``min_delegated_in_current_cycle`` RPC returns the value in mutez, but
here we use tez for simplicity.

* In Quebec:

  - At levels ``129`` and ``150``, the earliest level at the end of
    which the ``total_delegated`` is equal to the minimum ``1000`` is
    the first level of the cycle, that is, level ``129``.

  - At level ``200``, the ``900`` value happens in the middle of the
    block application so it is not considered. The new end-of-block
    value ``1050`` is not lower than the old minimum of ``1000``, so
    ``min_delegated_in_current_cycle`` stays at (min: ``1000``, level:
    ``129``).

  - At level ``205``, the new end-of-block value ``980`` is lower than
    the old minimum of ``1000``, so ``min_delegated_in_current_cycle``
    becomes (min: ``980``, level: ``205``).

* In Paris:

  - At level ``129``, the ``total_delegated`` has not changed since
    the start of the cycle. The Paris RPC returns level ``None`` in
    this case.

  - At level ``150``, the ``total_delegated`` has changed since the
    start of the cycle, but the minimum is actually the initial value
    it had at the start of the cycle. In this case, Paris RPC returns
    the earliest level at which the ``total_delegated`` has changed,
    that is, level ``150``.

  - At level ``200``, the ``total_delegated`` reaches a new minimum
    ``900``. Indeed, the Paris protocol does consider the values in
    the middle of the block application, so
    ``min_delegated_in_current_cycle`` becomes (min: ``900``, level:
    ``200``).

  - At level ``205``, the new value ``980`` is higher than the old
    minimum of ``900``, so ``min_delegated_in_current_cycle`` is still
    (min: ``900``, level: ``200``).


.. _overstaking:
.. _overstaking_tallinn:

Overstaking
-----------

The **limit_of_staking_over_baking** is a :ref:`configurable delegate
parameter<staking_policy_configuration_tallinn>` that limits how much
staked tez the external stakers can contribute to the baking power,
relative to the baker's own staked tez. It defaults to ``0``, meaning
no staked contribution from external stakers at all. It can be set to
any non-negative value (with a one millionth precision); however, the
``GLOBAL_LIMIT_OF_STAKING_OVER_BAKING`` constant, set to ``9``,
ensures that external stakers may never contribute more than nine times
as much staked tez as the baker itself, regardless of the delegate's
own limit.
If the amount of external staked
tez exceeds this quota, the baker is said to be **overstaked**, and we
also call **overstaked** the excess of external staked tez over the
allowed maximum. Any overstaked tez will count toward the baking power as
delegated instead of staked (provided that the baker is not
overdelegated too), so they will weigh a third as much.

.. code-block:: python

  global_limit_of_staking_over_baking = 9
  actual_limit_of_staking_over_baking = min(limit_of_staking_over_baking, global_limit_of_staking_over_baking)
  max_allowed_external_staked = own_staked * actual_limit_of_staking_over_baking
  external_staked_after_limits = min(external_staked, max_allowed_external_staked)

  total_staked_after_limits = own_staked + external_staked_after_limits

  overstaked = external_staked - external_staked_after_limits

The purpose of this feature is to ensure that the baker's
``own_staked``, that is, the part of the security deposit that belongs
to the baker itself, always represents a sizable portion of its
baking power. In other words, it guarantees that the baker always has
its own skin in the game. Besides, the global limit of ``9`` ensures
that a baker can never increase its own balance by denouncing its own
double baking or double attesting misbehavior; indeed, the reward that
would be given to the author of a denunciation is guaranteed to be
lower than the amount that would be slashed from the misbehaving
baker's own funds.

.. note::

  The ``limit_of_staking_over_baking`` has an additional effect when
  set to ``0``: it prevents external delegators from using ``stake``
  operations at all. This effect is completely removed when the limit
  is positive: external delegators can stake as much tez as they wish,
  even if this causes the baker to become overstaked or if the baker
  is already overstaked. If the limit is set to a positive value then
  back to ``0``, then external delegators are again prevented from
  staking new funds, but any previously staked tez remain staked
  (although they are now all considered overstaked, so they all count
  as delegated when computing baking the power).


Overdelegation
--------------

The amount that counts as *delegated* cannot exceed ``9`` times the
baker's ``own_staked``. Any excess tez are called **overdelegated**
and do not contribute to the baking power at all. This mechanism also
contributes to ensuring that all baking rights are covered by
appropriate security deposits.

Recall that the delegated amount used for baking rights is
:ref:`min_delegated_in_current_cycle<min_delegated_tallinn>`, and any
:ref:`overstaked<overstaking_tallinn>` tez count as delegated
too. Therefore:

.. code-block:: python

  total_delegated_after_limits = min(min_delegated_in_current_cycle + overstaked, own_staked * 9)

We finally have everything we need to compute the baking power
:ref:`as defined above<baking_power_overview_tallinn>`:

.. code-block:: python

  baking_power = total_staked_after_limits + (total_delegated_after_limits / 3)


.. _minimal_baking_power:
.. _minimal_baking_power_tallinn:

Minimal power and own staked requirements
-----------------------------------------

To receive baking rights, a delegate must meet the following
requirements:

- ``baking_power >= MINIMAL_STAKE``
- ``own_staked >= MINIMAL_FROZEN_STAKE``
- The delegate must be :ref:`active<active_delegate_tallinn>`

where ``MINIMAL_STAKE = 6,000ꜩ`` and ``MINIMAL_FROZEN_STAKE = 600ꜩ``
are :ref:`protocol constants<cs_constants_tallinn>`.

If any of these conditions is not met at the end of cycle ``n``, the delegate
still has a *baking power* as computed above, but receives no *baking
rights* at all for cycle ``n + 3``.
