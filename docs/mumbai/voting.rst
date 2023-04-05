The Amendment (and Voting) Process
==================================

In the Tezos blockchain, the *economic protocol* can be amended. Specifically,
there is an on-chain mechanism to propose changes to the economic protocol, to
vote for-or-against these proposed changes, and, depending on the result of the
vote, to activate these changes or not.

Note that the proposal, voting and activation processes are part of the economic
protocol itself. Consequently the amendment rules themselves are subject to
amendments.

The rest of this page gives more details about the amendment and voting process.

Periods
-------

The amendment process consists of five *periods*. Each period lasts for 5 cycles
(or approximately two weeks). The periods (listed below)
typically succeed one to another for a total duration of approximately 2 months and
a half, after which the whole amendment process starts again.

The five periods are as follows:

- *Proposal period*: During this period, delegates can

  - submit *protocol amendment proposals* (or, simply, *proposals*) using the
    ``Proposals`` operation (see below);
  - upvote one or several proposals, using the same ``Proposals`` operation.

  Each delegate can submit a maximum of 20 proposals. Duplicates count towards
  this total.

  At the end of a **proposal period**, if participation reaches a
  :ref:`proposal quorum <proposal_quorum>`, the proposal with most support is
  selected and we move to an **exploration period**. Note that support is
  measured in the cumulated staking power (expressed in mutez) that delegates supporting the
  proposal have. E.g., a proposal supported by a single delegate with 600,000 tz of staking power
  has more support than a proposal supported by two delegates with 100,000 tz
  each of staking power.

  If there are no proposals, or a tie between two or more proposals,
  or if participation did not reach the proposal quorum, the process
  moves back to a new **proposal period**.

- *Exploration period*: During this period delegates can cast one
  Yay, Nay, or Pass ballot on the selected proposal. They do so using the
  ``Ballot`` operation.

  If the voting participation reaches *quorum* and there is a *super-majority*
  of Yay, the process moves to the **cooldown period**. (See below for details
  on participation, quorum, and super-majority.)

  Otherwise the process moves back to the **proposal period**.

- *Cooldown period*: On-chain nothing specific happens during this period.
  Off-chain the delegates can read the proposal with more scrutiny, the
  community can discuss finer points of the proposal, the developers can
  perform additional tests, etc.

  At the end of this period, the process moves to the **promotion period**.

- *Promotion period*: During this period, delegates can cast a Yay, Nay, or Pass
  ballots using the ``Ballot`` operation.

  If the voting participation reaches *quorum* and there is a super-majority of
  Yay, the process moves to the **adoption period**.

  Otherwise the process moves back to the **proposal period**.

- *Adoption period*: On-chain nothing specific happens during this period except
  on the very last block (see below).

  Off-chain the developers release tools that include support for the
  soon-to-be activated protocol, other actors (bakers, indexers, etc.) update
  their infrastructure to support the newly released tools, smart-contract
  developers start working with soon-to-be-available features, etc.

  At the very end of the period, the proposal is *activated*. This means that
  the last block of the period is still interpreted by the current economic
  protocol, but the first block after the period is interpreted by the new
  economic protocol (the one that was voted in).

  And a new **proposal period** starts.


Activation
----------

After the activation step, the blocks added to the chain are interpreted in the
newly activated protocol. As a result gas costs may differ, new operations may
be available, contracts using new opcodes may be injected, etc.

Because the amendment process is also part of the economic protocol, the
amendment process now unfolds according to the rules of the newly activated
protocol. As a result the periods may be lengthened or shortened, a new period
might be introduced, a different selection mechanism may be used, the quorum
requirement might differ, etc.


Voting Power
------------

When supporting a proposal or casting a Yay, Nay, or Pass ballot, each delegate
has a voting power equal to its *stake*. More precisely, the voting power of a delegate during a voting period is its :ref:`staking balance <active_stake_mumbai>`, measured in *mutez*, sampled at the beginning of the period. (Note that this is opposed to validator selection for consensus, which is based on the active stake, and sampled at some stake snapshot during some previous cycle.)

Super-majority and Quorum
-------------------------

As mentioned above, during either of the **exploration** or **promotion** periods,
delegates can cast ballots using the ``Ballot`` operation (see below).
In both cases, delegates can cast a single Yay, Nay, or Pass ballot. A ballot
has a weight equal to the delegate's stake as detailed above.

For either of these two periods, the process continues to the next period if the
*vote participation* reaches *quorum* and there is a *super-majority* of
Yay.

The *vote participation* is the ratio of all the cumulated stake of cast ballots
(including Pass ballots) to the total stake.

For the first vote, the *quorum* started at 80% of stake. The quorum is
adjusted after each vote as detailed below. This adjustment is necessary to
ensure that the amendment process can continue even if some delegates stop
participating. After each vote the new quorum is updated based on the exponential moving average of the **vote participation**::

  new_participation_ema = 0.8 * old_participation_ema + 0.2 * participation

with the following formula::

  new_quorum = 0.2 + (0.7 - 0.2) * old_participation_ema

This formula avoids establishing quorums close to 100% that would be
very difficult to attain, or, conversely, low quorums close to 0% making
little participation chronic, ensuring that the quorums are lower- and upper-bounded by :ref:`quorum_caps` (0.2 and 0.7, respectively).

The *super-majority* is reached if the cumulated stake of Yay ballots is
greater than 8/10 of the cumulated stake of Yay and Nay ballots.

Note that Pass ballots do not count towards or against the super-majority;
they still counts towards participation and quorum.

More details can be found in the file
:src:`src/proto_016_PtMumbai/lib_protocol/amendment.ml`.


The Hash and the Protocol
-------------------------

On the one hand, the voting part of the process revolves around the
**hash of a protocol**. Specifically, a delegate submits a hash of a
protocol, and all the delegates cast ballots on the proposed hash.
The *hash of a protocol* is the hash of the files that constitute the source
code of the protocol.

On the other hand, the **protocol activation** (at the end of the
**adoption period**) revolves around the compiled sources of the protocol.

Basically, the voting process works on an identifier of the protocol whilst the
activation step works on the protocol itself. Consequently, if a protocol hash
is voted in and the protocol it identifies is invalid, the activation step
fails.

.. sidebar:: Checking a hash is of a valid protocol

   When a hash is proposed by a delegate, it is usually accompanied by some
   blogposts and forum threads on :ref:`community websites <tezos_community>`.
   These should include directions for testing the proposed protocols. If you
   cannot find such directions, do not hesitate to ask.

.. sidebar:: Localised failures

   It is possible that the activation step fails on a single node or a few nodes
   of the network, but succeed on the others. In this case the nodes with the
   failure are stuck, but the network as a whole continues.

   The most likely cause for this is nodes that have not been updated and do not
   include a new protocol environment version.

   If your node becomes stuck, you should start a fresh up-to-date node.

A protocol is *invalid* if its code cannot be compiled (e.g., if the code is not
valid source code), if its code uses functions not present in the
:doc:`protocol environment <../shell/protocol_environment>`, or if it
downgrades the protocol environment version.

If an invalid protocol is voted in, then the activation fails for all the nodes,
and then the chain becomes stuck. This is why it is important to vote for hashes
that designate valid protocols: ones with sources that are available and that
can be compiled.

Operations
----------

There are two operations used by the delegates: **proposals** and **ballot**.

A *proposals* operation can only be injected during a proposal period.

::

   Proposals : {
     source: Signature.Public_key_hash.t ;
     period: Voting_period_repr.t ;
     proposals: Protocol_hash.t list ; }

The ``source`` is the public key hash of the delegate, ``period`` is the unique
identifier of each voting period and ``proposals`` is a non-empty list of
maximum 20 protocol hashes.
The operation can be submitted more than once but only as long as the
cumulative length of the proposals lists is less than 20.
Duplicate proposals from the same delegate are accounted for in the
maximum number of proposals for that delegate.
However duplicates from the same delegate are not tallied at the end
of the proposal period.

For example, a delegate submits a *proposals* operation for protocols A
and B early in the proposal period, later a new protocol C is revealed
and the delegate submits another *proposals* operation for protocols B
and C.
The list of submissions that will be tallied is [A,B,C].

A *ballot* operation can only be submitted during periods where a vote happens
(e.g. exploration, promotion), and only once per period.

::

   Ballot : {
     source: Signature.Public_key_hash.t ;
     period: Voting_period_repr.t ;
     proposal: Protocol_hash.t ;
     ballot: Vote_repr.ballot ; }

The fields ``source`` and ``period`` are the same as above, while ``proposal``
is the currently selected proposal and ``ballot`` is one of ``Yay``, ``Nay`` or
``Pass``.
The ``Pass`` vote allows a delegate to contribute towards the quorum without
contributing towards the super-majority. This is important because, as detailed
above, the quorum is adaptive and that low participation would lower the
quorum of the next vote.

More details on the operations can be found in
:src:`src/proto_016_PtMumbai/lib_protocol/operation_repr.ml`.
The binary format is described by
``octez-client describe unsigned operation``.

Client Commands
---------------

The Octez client, ``octez-client``, provides commands for basic exploration and
interaction with the amendment and voting process.


Show
~~~~

The client provides a command to show the status of a voting period.
It displays different information for different kind of periods, as
in the following samples::

  $ octez-client show voting period
  Current period: "proposal"
  Blocks remaining until end of period: 59
  Current proposals:
  PsNa6jTtsRfbGaNSoYXNTNM5A7c3Lji22Yf2ZhpFUjQFC17iZVp 2,400,000 ꜩ

  $ octez-client show voting period
  Current period: "exploration"
  Blocks remaining until end of period: 63
  Current proposal: PsNa6jTtsRfbGaNSoYXNTNM5A7c3Lji22Yf2ZhpFUjQFC17iZVp
  Ballots:
    Yay: 2,400,000 ꜩ
    Nay: 0 ꜩ
    Pass: 0 ꜩ
  Current participation 20.00%, necessary quorum 80.00%
  Current in favor 2,400,000 ꜩ, needed supermajority 1,920,000 ꜩ

  $ octez-client show voting period
  Current period: "cooldown"
  Blocks remaining until end of period: 64
  Current proposal: PsNa6jTtsRfbGaNSoYXNTNM5A7c3Lji22Yf2ZhpFUjQFC17iZVp

It should be noted that the ballot number 2,400,000 ꜩ above is the stake counted in
mutez (displayed in tez).
The proposal has a total stake of 2,400,000 ꜩ, which may come from a single ballot
from a delegate having a staking balance of 2,400,000 ꜩ or it may come from multiple ballots from
delegates with a combined stake of 2,400,000 ꜩ.


Submit proposals
~~~~~~~~~~~~~~~~

During a proposal period, a list of proposals can be submitted with::

    octez-client submit proposals for <delegate> <proposal1> <proposal2> ...

Remember that each delegate can submit a maximum of 20 protocol
hashes and that duplicates count towards this total.
Moreover each proposal is accepted only if it meets one of the
following two conditions:

- the protocol hash was already proposed on the network. In this case
  we can submit an additional proposal that "upvotes" an existing one
  and our staking power are added to the ones already supporting the proposal.
- the protocol is known by the node. In particular the first proposer
  of a protocol should be able to successfully inject the protocol in
  its node which performs some checks, compiles and loads the
  protocol.

These are protection measures that the Octez client takes to prevent the
accidental injection of invalid protocols. As mentioned above, it is still
important to check the validity of the protocols that you vote for as they may
have been injected via different means.


Submit ballots
~~~~~~~~~~~~~~

During either of the **exploration** or **promotion** periods,
ballots can be submitted once with::

    octez-client submit ballot for <delegate> <proposal> <yay|nay|pass>

Further External Resources
--------------------------

Further details and explanations on the voting procedure can be found at:

- `Governance on-chain <https://opentezos.com/tezos-basics/governance-on-chain>`_ on Open Tezos
- `Tezos Governance <https://www.tezosagora.org/learn#an-introduction-to-tezos-governance>`_ on Tezos Agora.

For more details on the client commands refer to the manual at
:ref:`client_manual_mumbai`.

For vote related RPCs check the :doc:`rpc` under the prefix
``votes/``.

For Ledger support refer to Obsidian Systems' `documentation
<https://github.com/obsidiansystems/ledger-app-tezos#proposals-and-voting>`__.
