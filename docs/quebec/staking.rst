.. note::

  For operational details about the staking mechanism and its configuration, see `a staking mechanism tutorial <https://docs.google.com/document/d/1-1WTG2Vuez9D8fROTJrs42twbIErR16xyknRRBrjr-A/edit?usp=sharing>`__.

=================
Staking mechanism
=================

Staking is an evolution of the existing Tezos :doc:`Liquid Proof-of-Stake
mechanism <proof_of_stake>`. It
introduces a new role for network participants, called **staker**,
complementary to the existing :ref:`delegate <def_delegate_qena>`
(also known as *baker*) and *delegator* roles. A staker must also be a
*delegator* – that is, they must first choose a delegate.

When stakers **stake** funds towards a delegate’s **staking**
**balance**, the associated **baking** and **voting powers** accrue to
that delegate. Similarly to how delegated funds work, staked funds
remain within the staker’s account at all times.

Staked and delegated funds **have different weights** in the computation
of delegates’ baking and voting powers: staked funds (both external
stakes by stakers and the delegate’s own) count **twice** as much as
delegated funds.

Unlike delegated funds, staked funds are considered to contribute to the
security deposit associated with their chosen delegate. Thus, they are
subject to :ref:`slashing <slashing_qena>` if
the delegate misbehaves by :ref:`double-signing <def_double_signing_qena>`
block proposals or consensus operations, and are subject to the same
withdrawal delays – colloquially, they are "frozen".

Stakers are slashed proportionally to their contribution to the
delegate’s staking balance. To simplify slashing, double-baking
penalties are now proportional to staked funds: instead of the previous
fixed sum of 640 tez they are now set to 5% of the delegate’s stake.
Moreover, denunciation rewards (both for double-baking and
double-attestations) are reduced from one half to one seventh of the
slashed funds. The chosen value prevents adversarial delegates from
abusing the slashing mechanism for profit at the expense of their
stakers.

:ref:`Participation rewards <adaptive_rewards_qena>` are automatically shared
between delegates and their stakers. Staker's rewards are proportional to their
participation in the delegate's total staked at the time the rewards are given.
This means that the staker gets rewards for staked tez as soon as they are staked,
and stops receiving rewards as soon as the tez are unstaked, disregarding the
fact that baking rights for the delegate are computed with some delays.
*Delegates* :ref:`configure their staking
policy <staking_policy_configuration_qena>` by setting staking parameters
which regulate whether they accept stakers (the default being to reject
them), and if so, up to which fraction of their total staking balance.
They can also configure which proportion of the staking rewards from other stakers is set
to accrue to their own staked balance instead, thereby collecting an *edge* from the
rewards attributable to their stakers.

Freezing and unfreezing of staked funds is controlled directly by delegates and
stakers.
This entails that staked funds are frozen until manually
unfrozen by stakers. This is a two step process which spans for at least
4 cycles (cf. :ref:`Staked funds management <staked_funds_management_qena>`).

A user interface is provided for delegates and stakers to interact
with the mechanism. It is based on four *pseudo-operations*: ``stake``,
``unstake``, ``finalize_unstake``, and ``set_delegate_parameters``.
Pseudo-operations are self-transfers: a transfer operation where the
destination matches the source – each involving a special entry-point of
the same name introduced for :ref:`user accounts <def_user_account_qena>`.
This approach was chosen to minimize the work required by wallets,
custodians, exchanges, and other parties to support the functionality.

**NB** In the current implementation, only *user accounts* can become
stakers. In other words, smart contracts cannot stake funds (they can
of course still delegate them).

.. _staking_policy_configuration_qena:

Staking policy configuration
----------------------------

*Delegates* can configure their staking policy by setting the following
parameters:

-  ``edge_of_baking_over_staking``: a ratio between 0 and 1, whose
   default value is 1. This parameter determines the fraction of the
   rewards that accrue to the delegate's frozen deposit – the
   remainder is shared among its stakers.
- ``limit_of_staking_over_baking``: a non-negative number, denoting
   the maximum portion of external stake by stakers over the
   delegate’s own staked funds. It defaults to 0 – which entails that
   delegates do not accept external stakes by default. It is moreover
   capped by a global constant, set to 5 starting in the Paris
   protocol, which ensures the baker controls a significant part of
   the stake.

Delegates can modify these staking parameters at all times, using the
``set_delegate_parameters`` pseudo-operation: that is, by transferring 0
tez to their own ``set_delegate_parameters`` entry-point. The chosen values for both
parameters need to be supplied. The new parameters are then applied
``DELEGATE_PARAMETERS_ACTIVATION_DELAY`` (currently 5) cycles later.

::

   octez-client transfer 0 from <delegate> to  <delegate> --entrypoint set_delegate_parameters --arg "Pair <limit as int value in millionth> (Pair <edge as int value in billionth> Unit)"

or more conveniently::

   octez-client set delegate parameters for  <delegate> --limit-of-staking-over-baking <value> --edge-of-baking-over-staking <value>

**On overstaking and overdelegation.** Note that if a delegate’s
``limit_of_staking_over_baking`` is exceeded (that is, the delegate is
*overstaked*), the exceeding stake is automatically considered as
*delegation* for the delegate’s baking and voting power calculation, but
it does remain slashable. The new mechanism does not alter
*overdelegation* (delegated funds beyond 9 times the delegate’s own
stake) nor its consequence on voting and baking powers. That is,
overdelegated funds are not counted towards a delegate baking power, but
they do increase their voting power.

.. _staked_funds_management_qena:

Staked funds management
-----------------------

Stakers (and delegates) can use the ``stake``, ``unstake``, and
``finalize_unstake`` pseudo-operations to control their stakes. Figure
1 illustrates their effect on a staker’s funds. Note that
while these pseudo-operations change the *state* of the involved funds,
they remain otherwise within the staker’s account at all times.

.. figure:: staked_funds_transitions.png

  Figure 1: staked funds management using pseudo-operations.

To *stake* funds, a delegator uses the ``stake`` pseudo-operation,
transferring the chosen amount of **spendable** tez to their own
``stake`` entry-point. The **staked** tez will then be frozen and
contribute to their chosen delegate’s staking balance. Note that the
``stake`` pseudo-operation will fail if the sender account is not
*delegated*.

::

   octez-client transfer <amount> from <staker> to <staker> --entrypoint stake

or more conveniently::

   octez-client stake <amount> for <staker>

To *unstake* funds, a staker first submits an unstake request with the
``unstake`` pseudo-operation. This is implemented by transferring the
chosen amount in tez to their ``unstake`` entry-point::

   octez-client transfer <amount> from <staker> to <staker> --entrypoint unstake

or more conveniently::

   octez-client unstake <amount|"everything"> for <staker>

The requested amount will be **unstaked** but will remain **frozen**,
a.k.a. **unfinalizable**.
After 4 cycles, unstaked frozen tokens are no longer considered at stake
nor slashable. They are said then to be both **unstaked** and
**finalizable**.

A staker can retrieve all unstaked and finalizable tokens at any time,
making them spendable again. This is done using the ``finalize_unstake``
entrypoint -– that is, by transferring 0 tez to their
``finalize_unstake`` entry-point::

   octez-client transfer 0 from <staker> to <staker> --entrypoint finalize_unstake

or more conveniently::

   octez-client finalize unstake for <staker>

In some circumstances, unstake and finalize can be done implicitly: any call
to ``stake`` or ``unstake`` will implicitly finalize all currently finalizable pending
unstake requests. Also, as we will see next, change of delegate triggers an
unstake operation.

Change of delegate
------------------

When a staker changes its delegate, the operation will trigger an implicit unstake
request for the full frozen deposit of the staker.

As long as the unstake request is not finalized, the frozen tokens will continue
to be delegated to the old delegate, however the spending
balance of the account is accounted in the new delegate's stake.
It will not be possible to stake with the new delegate as long as there are
unfinalizable unstake request for token staked with the old delegate.
