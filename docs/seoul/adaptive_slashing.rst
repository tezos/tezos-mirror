=================
Adaptive Slashing
=================


Overview
========

In Oxford2, each instance of a baker's misconduct results in the
slashing of a fixed percentage of their staked funds. However, this
approach does not distinguish between innocent mistakes and deliberate
attacks. The rule conservatively punishes every misconduct with strong
penalties. The rationale for this model is its simplicity and the
observation that the cost of avoiding these mistakes is low anyway.
Nonetheless, it overlooks the impact of double signing of attestations
on the entire block committee, treating all cases uniformly and
ignoring collusion among bakers.

To better reflect this distinction, protocols starting with Paris
adjusts the amount of slashing based on the fraction of
double-attestation in a single block. A low fraction of misconduct
incurs moderate penalties, while a high fraction of misconduct is
deemed to be critical and faces more serious repercussions.

This document presents the definition of the :ref:`adaptive slashing
function<adaptive_slashing_fn_seoul>` implementing this idea, as well as the
:ref:`new forbidden period<new_forbidden_period_seoul>`.

.. _adaptive_slashing_fn:
.. _adaptive_slashing_fn_seoul:

Adaptive Slashing Function
==========================

.. _adaptive_slashing_informal:
.. _adaptive_slashing_informal_seoul:

Informal presentation
---------------------

For a given block, we call “fraction of double-attestations” the ratio
between the total weight of attestations for which a valid
denunciation has been included and the total weight of possible
attestations in a block.

The shape of the slashing function for double attestations is the
following:

.. figure:: adaptive-slashing.jpeg


+------------------------------------------+----------------------------------------+
| f: fraction of double attestations       | S: slashing ratio                      |
+==========================================+========================================+
| 1.00%                                    | 0.09%                                  |
+------------------------------------------+----------------------------------------+
| 5.00%                                    | 2.25%                                  |
+------------------------------------------+----------------------------------------+
| 10.00%                                   | 9.00%                                  |
+------------------------------------------+----------------------------------------+
| 20.00%                                   | 36.00%                                 |
+------------------------------------------+----------------------------------------+
| 23.57%                                   | 50.00%                                 |
+------------------------------------------+----------------------------------------+
| 33.34%                                   | 100.00%                                |
+------------------------------------------+----------------------------------------+
| 100.00%                                  |                                        |
+------------------------------------------+----------------------------------------+

Instead of using a constant function as in Oxford2, we use
a convex function that saturates at 100% when a critical fraction of
doubled attestations are issued. Accidental double attestations are
unlikely to cause a large amount of slashing to be applied, but
concerted attacks result in severe penalties.

\ **Remark 1.** Even though the baker does not attest to a weight
exactly equivalent to its staked funds at each block, the slashing
function is applied to bakers’ total staked funds for simplicity.

\ **Remark 2.** As in Oxford2, slashing happens at the end of each
cycle. Since the denunciation period for a block ranges over its cycle
and the next one, a baker can be punished for misbehaving during the
previous cycle, not only the one that has just ended.

\ **Remark 3.** If a baker wants to get back their at-stake funds, it
takes more than 2 cycles to complete the unstaking process. This
ensures that the baker can't decrease their at-stake funds after being
denunciated and before facing penalties.

.. _formal_adaptive_slashing:
.. _formal_adaptive_slashing_seoul:

A formal definition of slashing function for double-attestations
----------------------------------------------------------------

* :math:`\mathcal{W}` denotes the maximal possible *weight* of
  attestations in a block, that is, the fixed number of available
  :ref:`slots<rights_seoul>` in any block. It is also known as
  :ref:`CONSENSUS_COMMITTEE_SIZE<tb_validator_seoul>`.

* :math:`f(B)` is the *fraction of double attestations* for block
  :math:`B`, that is, the ratio of the total weight of double
  attestations in :math:`B`, over :math:`\mathcal{W}`.

* :math:`T` is the *threshold* for the fraction of double attestations
  to be considered critical. A typical value for :math:`T` is
  :math:`{1 \over 3} \mathcal{W}`, which is the difference between
  :math:`\mathcal{W}` and the
  :ref:`CONSENSUS_THRESHOLD<tb_validator_seoul>` which is set to
  :math:`{2 \over 3} \mathcal{W}`.

We define :math:`S(B)` the percentage of slashed funds for all
misbehaving bakers at the block :math:`B` as follows:

:math:`S(B) = \text{min} (100\%, {1 \over T^2} \cdot f(B)^2 \cdot 100\%)`

Then, the percentage of slashed funds :math:`S(b,C)` for a baker
:math:`b` at the end of the cycle :math:`C` is defined as follows:

:math:`S(b, C) = \text{min} (100\%, \sum_{(b, B) \in C} S(B))`

where :math:`(b, B) \in C` means that:

* the double attestation by baker :math:`b` of block :math:`B` has
  been denounced before the end of cycle :math:`C`, and

* :math:`C` is the last cycle of the denunciation period for
  :math:`B`.

.. _new_forbidden_period:
.. _new_forbidden_period_seoul:

New definition for the forbidden period
=======================================

Given that slashing occurs with a delay, immediate action at
denunciation time is necessary upon clear evidence of a baker's
misbehavior to prevent further misconduct, or to protect the baker
against their own faulty setup. Any double-signing denunciation
immediately triggers the beginning of a **forbidden period** that
lasts at least 2 cycles, to make sure the slashing occurs before
accepting new attestations or blocks from the baker.

Note that it is still possible for one baker to commit multiple double
signings, but only if they all happen before any corresponding
denunciation gets included in a block.

This forbidding is lifted as soon as both following conditions are
met:

* all pending slashings for the delegate have occurred, and

* the current total frozen stake for the delegate (sum of the
  :ref:`staking balances<active_stake_seoul>` of the delegate itself
  and its stakers) is at least as high as the :ref:`active
  stake<active_stake_seoul>` that was used ``CONSENSUS_RIGHTS_DELAY``
  cycles ago to compute the consensus rights for the next cycle.

The second condition may be fulfilled when the delegate and/or stakers
stake additional funds so that the total frozen stake grows back to
its pre-slashing value, thus matching the rights computed before the
slashing. Or it may be fulfilled ``CONSENSUS_RIGHTS_DELAY`` cycles
after the slashing, when the rights for the next cycle are finally
based on the post-slashing stake.
