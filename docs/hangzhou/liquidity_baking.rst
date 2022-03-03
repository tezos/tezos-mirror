Liquidity Baking
================

Liquidity baking incentivizes large amounts of decentralized liquidity provision between tez and tzBTC by minting a small amount of tez every block and depositing it inside of a constant product market making smart-contract. It includes an escape hatch mechanism as a contingency.

Contracts
~~~~~~~~~

During activation of Granada protocol, a constant product market making (CPMM) Michelson contract has been deployed on the chain with address ``KT1TxqZ8QtKvLu3V3JH7Gx58n7Co8pgtpQU5`` as well as an associated liquidity token contract (LQT) with address ``KT1AafHA1C1vk959wvHWBispY9Y2f3fxBUUo``.

.. warning::

   while the CPMM and LQT contract originations provide an ``Origination_result``, the LQT contract contains two big maps not included in a `lazy_storage_diff` field. Indexers and other tooling may need manual updates to include these.

The CPMM maintains a balance of ``a`` tez and ``b`` `tzBTC <https://tzbtc.io/>`_, where tzBTC is the `FA1.2 token <https://gitlab.com/tezos/tzip/-/blob/master/proposals/tzip-7/tzip-7.md>`_  found at address ``KT1PWx2mnDueood7fEmfbBDKx1D9BAnnXitn``. The smart contract accepts deposits of ``da`` tez and returns ``db`` tzBTC (or vice versa) where the invariant ``(a + da * (1 - f - n)) * (b - db) = a b`` is preserved, and ``f`` and ``n`` are a fee and burn, set at 0.1% each. Calculations are done with precision of 1000, rounding down on division.

To implement this contract, we use a fork of the open source code base used by `version two <https://gitlab.com/dexter2tz/dexter2tz>`_ of the "Dexter" project. The implementation of this contract has been `formally verified <https://gitlab.com/dexter2tz/dexter2tz#audits-and-formal-verification-external-resources>`_ against its functional specification. The contract code is modified in the following way:

1. The fee is set to 0.1% only (the fee in Dexter v2 is set to 0.3%). Rationale: given the subsidy it is not necessary to charge a large fee and better to improve liquidity.
2. An additional 0.1% of every trade is burned by being transferred to the null implicit account. Rationale: this mechanism offsets inflation from the subsidy. The inflation is exactly balanced at a daily trade volume of 7.2 million tez.
3. The ability to set a delegate has been removed. Rationale: the subsidy means there is no need for a baker for that contract and having one would create an imbalance.
4. The ability to set a manager has been removed. Rationale: the only privilege of the Dexter manager is to set Dexter's delegate so this role is now unnecessary.

The LIGO and Michelson code for these contracts, as well as detailed documentation, can be found on `the liquidity baking branch of the Dexter 2 repository <https://gitlab.com/dexter2tz/dexter2tz/-/tree/liquidity_baking>`_.

Subsidy
~~~~~~~

At every block in the chain, a small amount of tez is minted and credited to the CPMM contract, and the CPMM's ``%default`` entrypoint is called to update the ``xtz_pool`` balance in its storage. The amount that is minted and sent to the CPMM contract is 1/16th of the rewards for a block of priority 0 with all endorsements; currently these rewards are 40 tez per block so the amount that is sent to the CPMM contract is 2.5 tez per block.

So the credits to the CPMM contract can be accounted for by indexers, they are included in block metadata as a balance update with a new constructor for ``update_origin``, ``Subsidy``.

As a safety precaution, the subsidy expires automatically at a given
level called the liquidity baking sunset level. The sunset level can
be renewed periodically by protocol amendment.

.. _esc_hatch:
.. _esc_hatch_hangzhou:

Escape hatch
~~~~~~~~~~~~

In addition to the sunset mechanism, an escape hatch is included. At every block, the baker producing the block can choose to include a flag that requests ending the subsidy. The context maintains an exponential moving average of that flag calculated as such with integer arithmetic:

``e[0] = 0``
``e[n+1] = (1999 * e[n] // 2000) + (1000 if flag[n] else 0)``

If at any block ``e[n] >= 1000000`` then it means that an exponential moving average with a window size on the order of two thousand blocks has had roughly a majority of blocks demanding the end of the subsidy. If that is the case, the subsidy is permanently halted (though it can be reactivated by a protocol upgrade).

For indicative purposes, if a fraction ``f`` of blocks start signalling the flag, the threshold is reached after roughly ``2*(log(1-1/(2f)) / log(0.999))`` blocks, about 1387 blocks if everyone signals, 1964 blocks if 80% do, 3590 blocks if 60% do, etc. Recall for comparison that assuming two blocks per minute there are 2880 blocks per day.

The escape hatch can be invoked through a JSON file containing a vote that is repeatedly submitted on each baked block, e.g. ``tezos-baker run with local node ~/.tezos-node alice --votefile "per_block_votes.json"`` where ``per_block_votes.json`` contains just ``{"liquidity_baking_escape_vote": true}``. See also the :ref:`baker man page<baker_manual_hangzhou>`.
