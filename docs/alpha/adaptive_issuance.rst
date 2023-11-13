:math:`\newcommand\F[2]{\mathit{#1}\left(#2\right)}`
:math:`\newcommand{\minR}{\mathit{min_r}}`
:math:`\newcommand{\maxR}{\mathit{max_r}}`
:math:`\newcommand{\tmult}{\cdot}`
:math:`\newcommand\static[1]{\F{static}{#1}}`
:math:`\newcommand{\sfr}{\frac{1}{1600}}` :math:`\newcommand\tc{\tau_c}`
:math:`\newcommand\tr{\tau_r}` :math:`\newcommand\grf{\gamma}`
:math:`\newcommand\dyn[1]{\F{dyn}{#1}}`
:math:`\newcommand\sgn[1]{\F{sign}{#1}}`
:math:`\newcommand\dist[1]{\F{distance}{#1}}`
:math:`\newcommand\DTF{{\Delta t}}`
:math:`\newcommand\IL[1]{\normalsize{#1}}`
:math:`\newcommand\MX[2]{\F{max}{#1,#2}}`
:math:`\newcommand\adr[1]{\F{adaptive}{#1}}`
:math:`\newcommand\clip[3]{\F{clip}{#1,#2,#3}}`
:math:`\newcommand\supply[1]{\F{supply}{#1}}`
:math:`\newcommand\iss[1]{\F{issuance}{#1}}`
:math:`\newcommand\isb[1]{\F{issuance_{block}}{#1}}`
:math:`\newcommand\tw{\Sigma_w}`
:math:`\newcommand\rw[2]{\F{reward_{#1}}{#2}}`
:math:`\newcommand\tip[2]{\F{tip_{#1}}{#2}}`
:math:`\newcommand\lbs[1]{\F{subsidy_{LB}}{#1}}`
:math:`\newcommand\exp[1]{\F{exp}{#1}}`
:math:`\newcommand{\vdf}{\mathit{VDF}}`


=============================
Adaptive Issuance and Staking
=============================

This document describes Adaptive Issuance and Staking, two new features experimented in the Alpha protocol (referred hereafter as the Adaptive-Issuance/Staking proposal), which together constitute a major evolution of Tezos’ :doc:`Proof-of-Stake mechanism <proof_of_stake>`.

.. note::

  For operational details about the new staking mechanism and its configuration, see `a new staking mechanism tutorial <https://medium.com/the-aleph/a-walkthrough-of-tezos-new-staking-mechanism-4ff0c50a57a8>`__.

.. _adaptive_issuance_alpha:

Adaptive Issuance
=================

Adaptive Issuance is a novel mechanism regulating tez issuance in Tezos.

The :doc:`Tezos economic protocol <./protocol>` issues new
tez via:

-  Participation rewards: incentives given to delegates for
   :doc:`participation in consensus <consensus>`
   and random seed generation.
-  The :doc:`Liquidity Baking (LB) subsidy <liquidity_baking>`.
-  Protocol "invoices": lump sums of tez issued and allocated during
   protocol migration.

Participation rewards and the LB subsidy are regularly issued by the
protocol, whereas the value and recipients of invoices are defined
discretionarily by the developers of a protocol proposal.
The values for participation rewards and
the LB subsidy, if any, are currently defined by the Tezos protocol using fixed
constants.

The Adaptive-Issuance/Staking proposal
introduces the possibility to activate Adaptive Issuance: a mechanism where the amount of
*regularly* issued tez (participation rewards and the LB subsidy, if
active) depends on the global **staked funds ratio** – that is, the
ratio of staked tez to the total supply. This lets issuance roughly
match the *actual* security budget the chain requires, the amount needed
to encourage participants to stake and produce blocks, but *no more*.

At the end of each blockchain :ref:`cycle <def_cycle_alpha>`, the
regular issuance is adjusted, to nudge the staked funds ratio towards a
protocol-defined target (set at 50% in the Adaptive-Issuance/Staking proposal). Participation rewards
and the LB subsidy are recomputed to match that budget. When the staked
funds ratio decreases and diverges from the target, emission rates
increase, incentivizing participants to stake funds to re-approach the
target. Conversely, incentives decrease as the ratio increases beyond
the target.

Adaptive issuance rate
----------------------

The :ref:`adaptive issuance rate <adaptive_rate_alpha>` determines, at the end
of cycle :math:`\IL{c}`, the issuance for cycle :math:`\IL{c + 5}`. The
adaptive issuance rate is the sum of a :ref:`static rate <static_rate_alpha>`
and a :ref:`dynamic rate <dynamic_rate_alpha>`. The final result is clipped to
ensure nominal emissions remain within :math:`\IL{[\minR,\ \maxR]}` (set
to [0.05%, 5%] in the Adaptive-Issuance/Staking proposal) of the total supply.

.. figure:: adaptive_rate.png

  Figure 1. Adaptive issuance rate as a function of the staked funds ratio f.

Figure 1 plots the nominal issuance rate and the static rate as a
function of the staked ratio :math:`\IL{f}`. In the graph above, we
picked the value 0.0075 (or 0.75%) for the dynamic rate, but this is
just an example and that number varies dynamically over time.

The **static rate** is a static mechanism, which approximates `a Dutch
auction <https://en.wikipedia.org/wiki/Dutch_auction>`__ to compute a
nominal issuance rate as a function of the staked funds ratio for a
given cycle. Its value decreases as the staked funds ratio increases,
and *vice versa*.

.. _static_rate_alpha:

\ **STATIC RATE**\     Let :math:`\IL{f}` be the staked funds ratio at
the end of cycle :math:`\IL{c}`. Then, the **static rate** is defined
as:

.. math::

  static(f)=\sfr \tmult \frac{1}{f^2}

The choice of :math:`\IL{\sfr}` as a scaling factor ensures that the
curve takes reasonable values for plausible staking ratios. Moreover,
assuming Adaptive Issuance is activated with a dynamic ratio of 0, and
at current staked funds ratio (that is, ~7.5% of the total supply), this
factor allows for a smooth transition from current issuance rate
(~4.6%).

The **dynamic reward rate** adjusts itself over time based on the
distance between the staked funds ratio :math:`\IL{f}` and the 50% (±2%)
target ratio (:math:`\IL{\tc}` and :math:`\IL{\tr}` parameters below),
increasing when :math:`\IL{f}` < 48% and decreasing when :math:`\IL{f}`
> 52%, provided the total issuance rate is not hitting its lower or
upper limit.

.. _dynamic_rate_alpha:

\ **DYNAMIC RATE**\     The **dynamic rate** :math:`\IL{\dyn{c}}` is
defined at the end of cycle :math:`\IL{c}` as:

.. math::

  & \dyn{c}  =\ \dyn{c -1} + \sgn{\tc - \F{f}{c}} \tmult \grf \tmult \dist{\F{f}{c}} \tmult \DTF \\
  & \dyn{c_0} =\ 0

:math:`\IL{\dyn{c}}` is then clipped to
:math:`\IL{\left[ 0, \maxR - \static{\F{f}{c}}\right]}`, ensuring that
:math:`\IL{\static{\F{f}{c}} + \dyn{c} \leq \maxR}`.

In this formula:

-  :math:`\IL{c_0}` is the first cycle where Adaptive Issuance is
   active.

-  Given a cycle :math:`\IL{c}`, :math:`\IL{\F{f}{c}}` denotes the
   **staked funds ratio** at the end of the cycle, and
   :math:`\IL{\dyn{c}}` the value of the dynamic rate computed in that
   cycle.

-  :math:`\IL{\tc}` = 0.5 and :math:`\IL{\tr}` = 0.02 denote,
   respectively, the **target staked funds ratio** and the **radius** of
   the interval centered on the target ratio.

-  :math:`\IL{\grf}` = 0.01, controls the speed at which the dynamic
   rate adjusts. The value is set so that a one percentage point
   deviation of the staked funds ratio changes the dynamic rate by 0.01
   percentage points per day.

-  :math:`\IL{\dist{\F{f}{c}} = \MX{0}{\left|\F{f}{c} - \tc \right| - \tr}}`
   denotes the (*absolute*) distance between the staked funds ratio
   :math:`\IL{\F{f}{c}}` and the interval
   :math:`\IL{\left[ \tc - \tr, \tc + \tr \right]}`.

-  :math:`\IL{\DTF = \frac{16384 \tmult 15}{86400} = 2.8\overline{444}}`,
   denotes the minimal duration (in days) of a Tezos cycle, assuming all
   16384 blocks in the cycle are produced at the minimal allowed time –
   that is, every 15 seconds.

-  :math:`\IL{\sgn{\tc - \F{f}{c}} = 1}` if
   :math:`\IL{\F{f}{c} \leq \tc}` and :math:`-1` otherwise, denotes the
   sign of the distance between the target ratio :math:`\IL{\tc}` and
   the staked funds ratio :math:`\IL{\F{f}{c}}`.

In a nutshell, :math:`\IL{\dyn{c}}` increases and decreases by an amount
proportional to the distance between the target rate and the interval
:math:`\IL{\left[ \tc - \tr, \tc + \tr \right]}`, while ensuring that
the adaptive issuance rate is kept within :math:`\IL{[\minR,\ \maxR]}`
bounds.

Finally, as mentioned before, the nominal adaptive issuance rate [1]_
for a cycle :math:`\IL{c + 5}` is defined as the sum of the static rate
and the dynamic rate, clipped to stay within 0.05% – 5% range.

.. _adaptive_rate_alpha:

\ **ADAPTIVE ISSUANCE RATE**\     Let :math:`\F{f}{c}` be the staked
funds ratio at the end of cycle :math:`\IL{c}`, the **adaptive issuance
rate** for cycle :math:`\IL{c+5}` is defined as:

.. math::

  \adr{c + 5} = \clip{\dyn{c} + \static{\F{f}{c}}}{\minR}{\maxR}

.. _adaptive_rewards_alpha:

Adaptive rewards
----------------

Before adaptive issuance activation, participation rewards and
the LB subsidy are fixed values defined by protocol constants. With the
proposed mechanism, the :ref:`adaptive issuance rate <adaptive_rate_alpha>`
provides instead a budget for the whole cycle, which gets allocated
equally to each block of the cycle and distributed between the various
rewards, in proportion to their relative :ref:`weights <reward_weights_alpha>`.

\ **ADAPTIVE ISSUANCE PER BLOCK**\     Let :math:`\supply{c}` be the
total supply at the end of cycle :math:`\IL{c}`, the **maximal issuance per
block** for cycle :math:`\IL{c+5}` is defined as:

.. math::

  \isb{c + 5} = \frac{\adr{c + 5}}{2102400} \tmult \supply{c}

Where 2102400 =
:math:`\IL{\frac{365 \tmult 24 \tmult 60 \tmult 60}{15}}` is the maximal
number of blocks produced in a year, given a minimal block time of 15
seconds.

.. _reward_weights_alpha:

\ **REWARD WEIGHTS**\     The Adaptive-Issuance/Staking proposal defines the weights for
participation rewards and the LB subsidy as:

-  Attestation (formerly, endorsing) rewards : 10,240.
-  Fixed baking reward: 5,120.
-  Bonus baking reward: 5,120.
-  LB subsidy: 1,280.
-  Nonce revelation tip: 1.
-  VDF tip: 1.

The total sum of all weights is :math:`\tw` = 21762. The total issuance
per block, :math:`\IL{\isb{c}}`, is distributed amongst the different
rewards in proportion to their weight.

**Consensus rewards.** Since the adoption of Tenderbake, Tezos protocols
before the Adaptive-Issuance/Staking proposal have rewarded delegates :doc:`for their participation in
consensus <consensus>`
with the following rewards per block:

-  A fixed **baking** reward, given to the delegate which produced the
   *payload* of the block (i.e. choosing transactions, and other
   non-consensus operations).
-  A variable, baking **bonus** reward given to the delegate which
   produced the block included in the chain. This bonus is given for
   including attestations, if their combined attesting power exceeds the
   minimal threshold (two thirds of total slots).
-  A *collective* **attestation** reward, for attesting block proposals,
   distributed at the end of the cycle to the delegates selected in the
   consensus committees for that cycle, proportionnaly to their expected
   participation.

We refer to :doc:`the consensus page <consensus>` for
further insight on the pre-requisites and distribution of these rewards.
Here, we derive the new formulas which compute their values *per block*
for a cycle :math:`\IL{c}`:

.. math::

  & \rw{baking}{c} = \rw{bonus}{c} = \frac{5120}{\tw} \tmult \isb{c}

  & \rw{attestation}{c} = \frac{10240}{\tw} \tmult \isb{c}

Note that these formulas change the value of available rewards, but not
why and how they are awarded. Hence, :math:`\IL{\rw{bonus}{c}}` still
denotes the maximal value for this reward: the actual reward issued
depends on the total number of attested slots in a block. Similarly,
:math:`\IL{\rw{attestation}{c}}` is also a maximal value per block,
as the basis for computing the share of selected delegate at the end of
the cycle, the actual allocation of the rewards
being subject to the existing participation conditions.

**Nonce and VDF revelation tips.** The rewards allocated to delegates
for contributing to :ref:`random seed generation <randomness_generation_alpha>`
(that is for, revealing nonce seeds and posting VDF proofs) are not paid
each block, but rather every 128 blocks. The adjusted formulas result:

.. math::

  \tip{\vdf}{c} = \tip{nr}{c} = 128 \tmult \frac{1}{\tw} \tmult \isb{c}

**Liquidity baking subsidy.** The :doc:`LB
subsidy <liquidity_baking>` per block is determined by the following formula:

.. math::

  \lbs{c} = \frac{1280}{\tw} \tmult \isb{c}

Note that while the subsidy is issued **only if** the feature is on, its
weight is always counted in the computation of :math:`\IL{\tw}`. In
other words, the budget for the LB subsidy is always allocated,
regardless of whether it is issued or not.

The Adaptive-Issuance/Staking proposal implements a new `RPC
endpoint <https://tezos.gitlab.io/alpha/rpc.html#get-block-id-context-issuance-expected-issuance>`__,
``/issuance/expected_issuance``, which reports the precomputed values of
all participation rewards and the LB subsidy, for the cycle
corresponding to the queried block level, and the next 4 cycles.

.. _new_staking_alpha:

New Staking mechanism
=====================

Staking is an evolution of the existing Tezos :doc:`Liquid Proof-of-Stake
mechanism <proof_of_stake>`. It
introduces a new role for network participants, called **staker**,
complementary to the existing :ref:`delegate <def_delegate_alpha>`
(also known as *baker*) and *delegator* roles. A staker must also be a
*delegator* – that is, they must first choose a delegate.

When stakers **stake**\ funds towards a delegate’s **staking**
**balance**, the associated **baking** and **voting powers** accrue to
that delegate. Similarly to how delegated funds work, staked funds
remain within the staker’s account at all times.

Staked and delegated funds **have different weights** in the computation
of delegates’ baking and voting powers: staked funds (both external
stakes by stakers and the delegate’s own) count **twice** as much as
delegated funds.

Unlike delegated funds, staked funds are considered to contribute to the
security deposit associated with their chosen delegate. Thus, they are
subject to :ref:`slashing <slashing_alpha>` if
the delegate misbehaves by :ref:`double-signing <def_double_signing_alpha>`
block proposals or consensus operations, and are subject to the same
withdrawal delays – colloquially, they are "frozen".

Stakers are slashed proportionally to their contribution to the
delegate’s staking balance. To simplify slashing, double-baking
penalties are now proportional to staked funds: instead of the previous
fixed sum of 640 tez they are now set to 10% of the delegate’s stake.
Moreover, denunciation rewards (both for double-baking and
double-attestations) are reduced from one half to one seventh of the
slashed funds. The chosen value prevents adversarial delegates from
abusing the slashing mechanism for profit at the expense of their
stakers.

*Delegates* :ref:`configure their staking
policy <staking_policy_configuration_alpha>` by setting staking parameters
which regulate whether they accept stakers (the default being to reject
them), and if so, up to which fraction of their total staking balance.
They can also configure which proportion of the staking rewards is set
to accrue to their own staked balance versus their unfrozen, spendable
balance. As :ref:`participation rewards <adaptive_rewards_alpha>` are paid to
the staked balance, and automatically shared between delegates and their
stakers, delegates can use this parameter to collect an *edge* from the
rewards attributable to their stakers.

If and when the Adaptive-Issuance/Staking proposal activates, freezing and unfreezing of staked funds
will be controlled directly by delegates and stakers, and will no longer
be automatic. This entails that staked funds are frozen until manually
unfrozen by stakers. This is a two step process which spans for at least
7 cycles (cf. :ref:`Staked funds management <staked_funds_management_alpha>`).

A new user interface is provided for delegates and stakers to interact
with the mechanism. It is based on four *pseudo-operations*: ``stake``,
``unstake``, ``finalize_unstake``, and ``set_delegate_parameters``.
Pseudo-operations are self-transfers: a transfer operation where the
destination matches the source – each involving a special entry-point of
the same name introduced for :ref:`implicit accounts <def_implicit_account_alpha>`.
This approach was chosen to minimize the work required by wallets,
custodians, exchanges, and other parties to support the functionality.

**NB** Until :ref:`feature
activation <feature_activation_alpha>`: only
*delegates* can stake funds and the relative weight of staked and
delegated funds remains unchanged. In the current implementation, only
*implicit accounts* can become stakers. In other words, smart contracts
cannot stake funds (they can of course still delegate them).

.. _staking_policy_configuration_alpha:

Staking policy configuration
----------------------------

*Delegates* can configure their staking policy by setting the following
parameters:

-  ``edge_of_baking_over_staking``: a ratio between 0 and 1, whose
   default value is 1. This parameter determines the fraction of the
   rewards that accrue to the delegate’s liquid spendable balance – the
   remainder accrues to frozen stakes.
-  ``limit_of_staking_over_baking``: a non-negative number, denoting the
   maximum portion of external stake by stakers over the delegate’s own
   staked funds. It defaults to 0 – which entails that delegates do not
   accept external stakes by default. It is moreover capped by a global
   constant, set to 5 in the Adaptive-Issuance/Staking proposal, which ensures the baker controls a
   significant part of the stake.

Delegates can modify these staking parameters at all times, using the
``set_delegate_parameters`` pseudo-operation: that is, by transferring 0
tez to their own ``set_delegate_parameters`` entry-point. The chosen values for both
parameters need to be supplied. The new parameters are then applied
``PRESERVED_CYCLES`` (currently 5) cycles later.

::

   octez-client transfer 0 from <delegate> to  <delegate> --entrypoint set_delegate_parameters --arg "Pair <limit as int value in millionth)> (Pair <edge as int value in billionth> Unit)"

or more conveniently::

   octez-client set delegate parameters for  <delegate> --limit-of-staking-over-baking <value> --edge-of-baking-over-staking <value>

**On overstaking and overdelegation.** Note that if a delegate’s
``limit_of_staking_over_baking`` is exceeded (that is, the delegate is
*overstaked*), the exceeding stake is automatically considered a
*delegation* for the delegate’s baking and voting power calculation, but
it does remain slashable. The new mechanism does not alter
*overdelegation* (delegated funds beyond 9 times the delegate’s own
stake) nor its consequence on voting and baking powers. That is,
overdelegated funds are not counted towards a delegate baking power, but
they do increase their voting power.

.. _staked_funds_management_alpha:

Staked funds management
-----------------------

Stakers (and delegates) can use the ``stake``, ``unstake``, and
``finalize_unstake`` pseudo-operations to control their stakes. Figure
2 illustrates their effect on a staker’s funds. Note that
while these pseudo-operations change the *state* of the involved funds,
they remain otherwise within the staker’s account at all times.

.. figure:: staked_funds_transitions.png

  Figure 2: staked funds management using pseudo-operations.

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

The requested amount will be **unstaked** but will remain **frozen**.
After 7 cycles, unstaked frozen tokens are no longer considered at stake
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

.. _feature_activation_alpha:

Feature activation vs protocol activation
=========================================

Should the Adaptive-Issuance/Staking proposal be accepted by the community, and
once the protocol becomes active on Tezos Mainnet, most of the features
described in this document will **not** be enabled by default, only
latent possibilities in the protocol, waiting for a separate activation.

In particular, the following changes will require additional approval
from delegates via separate feature activation vote mechanism:

-  Adaptive issuance – including notably the changes to the computation
   of consensus rewards and the LB subsidy.
-  Ability for *delegators* to become *stakers* – until feature
   activation delegates continue to be the only participants who can
   **stake** funds.
-  The changes in weight for staked and delegated funds towards the
   computation of baking and voting rights.

Other changes described earlier would be enabled from the Adaptive-Issuance/Staking proposal’s
activation:

-  The new interface for stake manipulation based on
   *pseudo-operations*. Note that this entails the deprecation of the
   ``set/unset deposits limit`` interface and also the end of automatic
   deposit freezing. On protocol activation, each delegate’s stake is
   derived from the frozen deposits at the end of the last cycle of
   Nairobi.
-  The changes in slashing penalties (double-baking penalties are set to
   10% of the staked funds) and denunciation rewards (they amount to one
   seventh of slashed funds).
-  Changes to protocol constants. Note that this entails calculating
   participation rewards and the LB subsidy using the weight-based
   formulas, but these are defined so that they match the previous
   values when :ref:`Adaptive Issuance <adaptive_issuance_alpha>` is not active.

Activation Vote
---------------

We highlight the following principles behind the feature activation vote
mechanism:

-  If and when the Adaptive-Issuance/Staking proposal activates, delegates can start voting for (**On**)
   or against (**Off**) the feature activation of the changes listed
   above in each block they bake. They can also abstain with a **Pass**
   vote.
-  These votes are cast by block-producing delegates, and are included
   in block headers.
-  Participation is not mandatory, defaulting to **Pass** in the absence
   of signaling.
-  The feature activation vote has two phases: a *Voting* phase and a
   subsequent *Adoption* phase.
-  The *Voting* phase is driven by an Exponential moving average (EMA)
   whose *half-life* is 2 weeks. That is, it takes two weeks for the EMA
   to raise from 0% to 50% assuming only **On**\ votes are cast.
-  The target threshold is a supermajority of 80% of **On** votes over
   **On plus Off** votes.
-  There is no time limit or fixed duration for the Voting phase. It
   continues as long as the threshold is not met. There is no *quorum*
   either, the lack of participation (reified as **Pass** votes) is not
   taken into account by the EMA, and hence only affects the time
   required to reach the threshold.
-  If the threshold is met, the Voting phase will complete at the end of
   the current cycle, and the Adoption phase will start at the beginning
   of the following cycle.
-  The Adoption phase lasts 7 cycles. The beginning of the cycle
   following the end of the Adoption phase activates the guarded
   features.
-  There is **no automatic deactivation** of the guarded features once
   in (and after) the Adoption phase – subsequent votes continue to be
   counted towards an updated EMA, but without any further effect.

**NB** In the implementation in the Adaptive-Issuance/Staking proposal, the issuance rate
is computed 5 cycles in advance. Thus, in the first 5 cycles where is
active, the protocol does not use the :ref:`adaptive reward
formula <adaptive_rewards_alpha>` and keeps using the current reward
values.

.. [1]
   Note that if the nominal annual issuance rate is :math:`r`, the
   annualized rate is close to :math:`\IL{\exp{r} - 1}` as it is
   compounded at every cycle.
