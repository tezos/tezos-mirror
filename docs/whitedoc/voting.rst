.. _voting:

The Voting Process
==================

The design of the Tezos Node allows the economic protocol to be
amended, that is replaced by another set of OCaml files which
implement the API of a valid protocol.

In the current protocol the amendment procedure is guided by a voting
procedure where delegates can propose, select and test a candidate
protocol before activating it.  Delegates take part in the amendment
procedure with an influence proportional to their stake. As for baking
and endorsing, only an active delegate with at least one roll can take
part in the amendment procedure. Furthermore, a delegate can cast a
"split vote", that is, it can partition its voting power to give a
confidence (or influence) level to its vote or to reflect the
preferences of its delegators.

The procedure consists of five periods, each of 8 cycles, that is, 32768 blocks
(roughly three weeks), for a total of approximately four months.

Other than this page, there is an excellent overview from `Jacob
Arluck on medium.
<https://medium.com/tezos/amending-tezos-b77949d97e1e>`_
(Note that concrete details there might be out of date.)

Periods
-------

The voting procedure works as follows:

- `Proposal period`: delegates can submit protocol amendment proposals using
  the `proposals` operation. At the end of a proposal period, the proposal with
  most supporters is selected and we move to a testing_vote period.
  If there are no proposals, or a tie between proposals, a new proposal
  period starts. Each delegate can submit a maximum of 20 proposals,
  including duplicates.
- `Testing_vote period`: delegates can cast one ballot to test or not the winning
  proposal using the `ballot` operation.
  At the end of a testing_vote period if participation reaches the quorum
  and the proposal has a super-majority in favor, we proceed to a testing
  period. Otherwise we go back to a proposal period.
- `Testing period`: a test chain is forked for 48 hours to test a
  correct migration of the context.
  At the end of a testing period we move to a promotion_vote period.
- `Promotion_vote period`: delegates can cast one ballot to promote or not the
  tested proposal using the `ballot` operation.
  At the end of a promotion_vote period if participation reaches the quorum
  and the tested proposal has a super-majority in favor, we proceed to
  adoption period. Otherwise we go back to a proposal period.
- `Adoption period`: at the end of the period the proposal is activated
  as the new protocol and we go back to a proposal period.

Each of the periods above are called a `voting period`. It is important to note
that the stake of each delegate is computed at the beginning of each voting
period, and if the delegate owns one roll or more, its stake in number of rolls is
stored in a list called the `voting listings`.

Super-majority and Quorum
-------------------------

Both voting periods work in the same way, only the subject of the vote
differs.  The protocol keeps track of the number of "Yay", "Nay", and
"Pass" votes which are indirectly cast during the voting period.

During a vote a delegate can cast a single ballot. A ballot consists
of three non-negative integers that add up to a given constant
``votes_per_roll``, currently set to 100. These three integers
represent partitions of the delegate's voting power that the delegate
is giving to the positive, negative, or respectively neutral part of
its vote.  The number of votes of a delegate for each of the three
categories is calculated by multiplying the corresponding partition by
the number of rolls owned by the delegate. For instance, if a delegate
has 3 rolls and casts a `50/30/20` ballot, then this ballot will add
150 Yay votes, 90 Nay votes, and 60 Pass votes to the total number of
Yays/Nays/Passes votes.

`Super-majority` means the Yay votes are more than 8/10 of Yay+Nay votes.  The
`participation` is the ratio of all received votes, including passes, with respect to the
number of possible votes. The `quorum` starts at 80% and at each vote it is updated using
the old quorum and the current participation with the following coefficients::

  newQ = oldQ * 8/10 + participation * 2/10

More details can be found in the file
``src/proto_alpha/lib_protocol/amendment.ml``.

Operations
----------

There are two operations used by the delegates: ``proposals`` and ``ballot``.
A proposal operation can only be submitted during a proposal period.

::

   Proposals : {
     source: Signature.Public_key_hash.t ;
     period: Voting_period_repr.t ;
     proposals: Protocol_hash.t list ; }

Source is the public key hash of the delegate, period is the unique
identifier of each voting period and proposals is a non-empty list of
maximum 20 protocol hashes.
The operation can be submitted more than once but only as long as the
cumulative length of the proposals lists is less than 20.
Duplicate proposals from the same delegate are accounted for in the
maximum number of proposals for that delegate.
However duplicates from the same delegate are not tallied at the end
of the proposal period.

For example, a delegate submits a proposals operation for protocol A
and B early in the proposal period, later a new protocol C is revealed
and the delegate submits another proposals operation for protocol B
and C.
The list of submissions that will be tallied is [A,B,C].

A ballot operation can only be submitted during one of the voting
periods, and only once per period.

::

   Ballot : {
     source: Signature.Public_key_hash.t ;
     period: Voting_period_repr.t ;
     proposal: Protocol_hash.t ;
     ballot: Vote_repr.ballot ; }

Source and period are the same as above, while proposal is the currently selected proposal
and ballot is a record that stores the fractions for ``Yay``, ``Nay`` and ``Pass``.

::

   Vote_repr.ballot : {
     yay_fraction : int32 ;
     nay_fraction: int32 ;
     pass_fraction : int32 ; }

Increasing the pass fraction over the other two fractions allows a delegate to diminish
its influence on a vote but still allowing it to reach quorum.

More details can be found, as for all operations, in
``src/proto_alpha/lib_protocol/operation_repr.ml``.
The binary format is described by ``tezos-client describe unsigned
operation``.

Client Commands
---------------

Tezos' client provides a command to show the status of a voting period.
It displays different informations for different kind of periods, as
in the following samples::

  $ tezos-client show voting period
  Current period: "proposal"
  Blocks remaining until end of period: 59
  Current proposals:
  PsNa6jTtsRfbGaNSoYXNTNM5A7c3Lji22Yf2ZhpFUjQFC17iZVp 400

  $ tezos-client show voting period
  Current period: "testing_vote"
  Blocks remaining until end of period: 63
  Current proposal: PsNa6jTtsRfbGaNSoYXNTNM5A7c3Lji22Yf2ZhpFUjQFC17iZVp
  Ballots: { "yay": 400, "nay": 0, "pass": 0 }
  Current participation 20.00%, necessary quorum 80.00%
  Current in favor 400, needed supermajority 320

  $ tezos-client show voting period
  Current period: "testing"
  Blocks remaining until end of period: 64
  Current proposal: PsNa6jTtsRfbGaNSoYXNTNM5A7c3Lji22Yf2ZhpFUjQFC17iZVp

It should be noted that the number 400 above is a number of rolls.
The proposal has a total of 400 rolls, which may come from several
delegates. The same applies for the ballots, there are 400 rolls in
favor of testing protocol PsNa6jTt.

Submit proposals
~~~~~~~~~~~~~~~~

During a proposal period, the list of proposals can be submitted with::

    tezos-client submit proposals for <delegate> <proposal1> <proposal2> ...

Remember that each delegate can submit a maximum of 20 protocol
hashes including duplicates.
Moreover each proposal is accepted only if it meets one of the
following two conditions:

- the protocol hash was already proposed on the network. In this case
  we can submit an additional proposal that "upvotes" an existing one
  and our rolls are added to the ones already supporting the proposal.
- the protocol is known by the node. In particular the first proposer
  of a protocol should be able to successfully inject the protocol in
  its node which performs some checks, compiles and loads the
  protocol.

Submit ballots
~~~~~~~~~~~~~~

During a voting period, being it a testing vote or a promotion vote,
ballots can be submitted once with::

    tezos-client submit ballot for <delegate> <proposal>
                 <yay_fraction> <nay_fraction> <pass_fraction>

Other resources
~~~~~~~~~~~~~~~

For more details on the client commands refer to the manual at
:ref:`client_manual`.

For vote related RPCs check the :ref:`rpc_index` under the prefix
``vote/``.

For Ledger support refer to Obsidian Systems' `documentation
<https://github.com/obsidiansystems/ledger-app-tezos#proposals-and-voting>`_.
