Liquidity Baking
================

Liquidity baking incentivizes large amounts of decentralized liquidity provision between tez and tzBTC by minting a small amount of tez every block and depositing it inside of a constant product market making smart-contract.

Contracts
~~~~~~~~~

During activation of Granada protocol, a constant product market making (CPMM) Michelson contract has been deployed on the chain with address ``KT1TxqZ8QtKvLu3V3JH7Gx58n7Co8pgtpQU5`` as well as an associated liquidity token contract (LQT) with address ``KT1AafHA1C1vk959wvHWBispY9Y2f3fxBUUo``.

.. warning::

   While the CPMM and LQT contract originations provide an ``Origination_result``, the LQT contract contains two big maps not included in a ``lazy_storage_diff`` field. Indexers and other tooling may need manual updates to include these.

The CPMM maintains a balance of ``a`` tez and ``b`` `tzBTC <https://tzbtc.io/>`_, where tzBTC is the `FA1.2 token <https://gitlab.com/tezos/tzip/-/blob/master/proposals/tzip-7/tzip-7.md>`_  found at address ``KT1PWx2mnDueood7fEmfbBDKx1D9BAnnXitn``. The smart contract accepts deposits of ``da`` tez and returns ``db`` tzBTC (or vice versa) where the invariant ``(a + da * (1 - f - n)) * (b - db) = a b`` is preserved, and ``f`` and ``n`` are a fee and burn, set at 0.1% each. Calculations are done with precision of 1000, rounding down on division.

To implement this contract, we use a fork of the open source code base used by `version two <https://gitlab.com/dexter2tz/dexter2tz>`_ of the "Dexter" project. The implementation of this contract has been `formally verified <https://gitlab.com/dexter2tz/dexter2tz#audits-and-formal-verification-external-resources>`_ against its functional specification. The contract code is modified in the following way:

1. The fee is set to 0.1% only (the fee in Dexter v2 is set to 0.3%). Rationale: given the subsidy it is not necessary to charge a large fee and better to improve liquidity.
2. An additional 0.1% of every trade is burned by being transferred to the null implicit account. Rationale: this mechanism offsets inflation from the subsidy. The inflation is exactly balanced at a daily trade volume of 7.2 million tez.
3. The ability to set a delegate has been removed. Rationale: the subsidy means there is no need for a baker for that contract and having one would create an imbalance.
4. The ability to set a manager has been removed. Rationale: the only privilege of the Dexter manager is to set Dexter's delegate so this role is now unnecessary.

The LIGO and Michelson code for these contracts, as well as detailed documentation, can be found on `the liquidity baking branch of the Dexter 2 repository <https://gitlab.com/dexter2tz/dexter2tz/-/tree/liquidity_baking>`_.

Subsidy
~~~~~~~

At every block in the chain, a small amount of tez is minted and credited to the
CPMM contract, and the CPMM's ``%default`` entrypoint is called to update the
``xtz_pool`` balance in its storage. The amount that is minted and sent to the
CPMM contract is 1/16th of the rewards for a block of round 0 with all
attestations; currently these rewards are 20 tez per block so the amount that is
sent to the CPMM contract is 1.25 tez per block.

So the credits to the CPMM contract can be accounted for by indexers, they are included in block metadata as a balance update with a new constructor for ``update_origin``, ``Subsidy``.

.. _toggle:
.. _toggle_oxford:

Toggle vote
~~~~~~~~~~~

The subsidy can be paused by a mechanism called the Liquidity Baking
Toggle Vote. At every block, the baker producing the block includes
a flag that requests ending the subsidy or on the contrary continuing
or restarting it. The context maintains an exponential moving average
of that flag. The baker has three options for this flag: ``Off`` to
request ending the subsidy, ``On`` to request continuing or restarting
the subsidy, and ``Pass`` to abstain.

``e[n+1] = e[n]`` if the flag is set to ``Pass``.
``e[n+1] = (1999 * e[n] // 2000) + 1_000_000`` if the flag is set to ``Off``.
``e[n+1] = (1999 * e[n] // 2000)`` if the flag is set to ``On``.
When computing ``e[n+1]``, the division is rounded toward ``1_000_000_000``.

If at any block ``e[n] >= 1_000_000_000`` then it means that an
exponential moving average with a window size on the order of two
thousand non-abstaining blocks has had roughly at least a half of the
blocks demanding the end of the subsidy. If that is the case, the
subsidy is halted but can be reactivated if for some later block
``e[n] < 1_000_000_000``.

For indicative purposes, if among the non-abstaining blocks a fraction
``f`` of blocks use it to request ending the subsidy, the threshold is
reached after roughly ``2*(log(1-1/(2f)) / log(0.999))``
non-abstaining blocks, about 1386 blocks if everyone signals, 1963
blocks if 80% do, 3583 blocks if 60% do etc. Recall for comparison
that assuming four blocks per minute there are 5760 blocks per day.

When producing blocks using Octez baking daemon ``octez-baker``, there
are two command-line options affecting toggle vote. The
``--liquidity-baking-toggle-vote <on|off|pass>`` option sets a static
value to be used in each block. Note that this option must be placed
**after** ``run`` on the command-line. Moreover, the path of a JSON
file can be given to the ``--votefile <path>`` option
e.g. ``octez-baker-<protocol codename> run with local node
~/.tezos-node alice --liquidity-baking-toggle-vote on --votefile
"per_block_votes.json"``, or placed in a default location:
``per_block_votes.json`` in the current working directory **or** in
the client data directory
(e.g. ``~/.tezos-client/per_block_votes.json``); the former location
takes precedence. The content of the JSON file will be repeatedly
submitted on each baked block, where ``per_block_votes.json`` contains
just ``{"liquidity_baking_toggle_vote": "pass"}`` (to abstain),
``{"liquidity_baking_toggle_vote": "off"}`` (to request ending the
subsidy), or ``{"liquidity_baking_toggle_vote": "on"}`` (to request
continuing the subsidy). When the ``--votefile`` option is present it
takes precedence over ``--liquidity-baking-toggle-vote``. If the JSON
file is deleted or becomes malformed while the baker is running, the
last valid value read is used. If neither a valid vote file is
provided nor a CLI value given, the baker will fail on the first block
after it was started. See also the :ref:`baker man
page<baker_manual_oxford>`.
