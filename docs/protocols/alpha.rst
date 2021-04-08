.. _alpha:

Protocol Alpha
==============

This page contains all the relevant information for protocol Alpha, a
development version of the Tezos protocol.

The code can be found in the ``src/proto_alpha`` directory of the
``master`` branch of Tezos.

This page documents the changes brought by protocol Alpha with respect
to Florence.

The main novelties in the Alpha protocol are:

- an upgrade of the consensus algorithm Emmy+ to Emmy*, which brings smaller block times and faster finality

- a 2.5 tez per block subsidy to a CPMM contract to generate liquidity between tez and tzBTC

.. contents:: Here is the complete list of changes:

Emmy*
-----

Emmy* updates Emmy+ by:

- a tweak in the definition of the minimal delay function, and
- an increase in the number of endorsement slots per block

Concretely, in Emmy* a block can be produced with a delay of 30 seconds with respect to the previous block if it has priority 0 and more than 60% of the total endorsing power per block, which has been increased to ``256`` (from ``32``) endorsement slots per block.

The baking and endorsing rewards are updated as follows:

- The reward producing a block at priority 0 is updated from ``e * 1.25`` tez to ``e * 0.078125`` tez, and for producing a block at priority 1 or higher, is updated from ``e * 0.1875`` tez to ``e * 0.011719`` tez, where ``e`` is the endorsing power of the endorsements contained in the block.
- The reward for endorsing a block of priority 0 is updated from ``1.25`` tez to ``0.078125`` tez per endorsement slot, and for endorsing a block at priority 1 or higher is updated from ``0.833333`` tez to ``0.052083`` tez per endorsement slot.

The values of the security deposits are updated from ``512`` tez to ``640`` tez for baking, and from ``64`` tez to ``2.5`` tez for endorsing.

The constant ``hard_gas_limit_per_block`` is updated from ``10,400,000`` to ``5,200,000`` gas units.

The Michelson `NOW` instruction keeps the same intuitive meaning,
namely it pushes the minimal injection time on the stack for the
current block. However, this minimal injection time is not 1 minute
after the previous block's timestamp as before, instead it is 30
seconds after.

The values of the ``BLOCKS_PER_*`` constants has doubled in order to
match the reduced block times, as follows: ``BLOCKS_PER_CYCLE =
8192``, ``BLOCKS_PER_COMMITMENT = 64``, ``BLOCKS_PER_ROLL_SNAPSHOT =
512``, and ``BLOCKS_PER_VOTING_PERIOD = 40960``. This partially solves issue: `tezos#1027 <https://gitlab.com/tezos/tezos/-/issues/1027>`__
 `tezos!2531 <https://gitlab.com/tezos/tezos/-/merge_requests/2531>`__

Liquidity Baking
----------------

2.5 tez per block is credited to a constant product market making (CPMM) contract, the contract's ``%default`` entrypoint is called to update its storage, and the credit is included in block metadata as a balance update with a new ``update_origin`` type, ``Subsidy``.

The liquidity baking subsidy shuts off automatically at six months from protocol activation if not renewed in a future upgrade. The sunset duration is included in constants and sunset level calculated during stitching.

At any time bakers can vote to shut off the liquidity baking subsidy by setting a boolean flag in protocol_data. An exponential moving average (ema) of this escape flag is calculated with a window size of 1000 blocks and the subsidy permanently shuts off if the ema is ever over a threshold included in constants (half the window size with precision of 1000 added for integer computation).

- `TZIP <https://gitlab.com/tzip/tzip/-/blob/master/drafts/current/draft-liquidity_baking.md>`_
- MR:
  :gl:`tezos!2765`

More detailed docs for liquidity baking can be found :ref:`here<liquidity_baking_alpha>`.

RPC changes
-----------

- Remove deprecated RPCs and deprecated fields in RPC answers related
  to voting periods. (MR `tezos!2763
  <https://gitlab.com/tezos/tezos/-/merge_requests/2763>`__, addresses
  issue `tezos#1204 <https://gitlab.com/tezos/tezos/-/issues/1204>__.)

- The RPC ``../<block_id>/required_endorsements`` has been removed.

- Replace `deposit` by `deposits` in `frozen_balance` RPC.

Minor changes
-------------

- Fix handling of potential integer overflow in `Time_repr` addition `Protocol/time_repr: check for potential overflow on addition <https://gitlab.com/tezos/tezos/-/merge_requests/2660>`_

- Increased the max operation time to live (`max_op_ttl`) from 60 to
  120

- Realign voting periods with cycles, solves issue `tezos#1151
  <https://gitlab.com/tezos/tezos/-/issues/1151>`__
  `<https://gitlab.com/tezos/tezos/-/merge_requests/2838>`__

- If gas remains for an operation after it gets executed, the remaining
  gas also gets consumed in the block;
  `tezos!2880 <https://gitlab.com/tezos/tezos/-/merge_requests/2880>`__
