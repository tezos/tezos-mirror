(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(** Defines storage for Smart Contract Optimistic Rollups.

    {2 Commitments}

    [Commitment]s are stored directly in the L1 context. Commitments are
    immutable and content-addressed, and can be indexed by a [Commitment_hash].

    A commitment represents a claim about the state of a PVM.

    We also keep auxiliary state about each commitment, namely:

    {ul
      {li When it was first added.}
      {li Its current number of stakers.}
      }

    This auxiliary data is not part of the commitment itself. They represent
    information that the L1 knows about the claim, not the claim itself.

    {3 Predecessors and Boot state}
    Each commitment contains the hash of its {i predecessor}. Multiple
    commitments can have the same predecessor. Therefore, commitments form
    a Merkle tree.

    Conceptually the root of this tree is the [Commitment_hash.zero].  This
    commitment claims that the PVM (Proof-generating Virtual Machine) is in a
    pre-boot state and waiting to start booting by interpreting the boot sector with
    respect to the Machine semantics.

    {3 Cemented and Disputable commitments}
    Commitments accepted as true by the protocol are referred to as Cemented.

    {3 Stakers}
    The Stakers table maps Stakers (implicit accounts) to commitments.

    Let [Stakers(S)] mean "looking up the key S in [Stakers]".

    A staker [S] is directly staked on [C] if [Stakers(S) = C]. A staker [S]
    is indirectly staked on [C] if [C] is an ancestor of [Stakers(S)] in the commitment tree.

    {3 Dispute}
    Commitments that have at least one sibling are referred to as Disputed.
    More formally, a commitment C is disputed if at least one staker is not
    (directly or indirectly) staked on C.

    {3 Dispute resolution}
    The rollup protocol ensures that all disputes are resolved before cementing
    a commitment. Therefore, cemented commitments form a list rather than a tree.

    In the context we only store the Last Cemented Commitment (LCC), which is
    by definition a descendant of [zero]. We also store all Disputable
    commitments that have at least one Staker.

    For example, assuming the full set of commitments for a rollup
    looks like this:

    {[
                 LCC  staker1  staker2
                  |      |        |
                  |      V        |
                  V   --c3        |
      zero--c1 --c2--/            |
                     \            V
                      --c4------ c5
    ]}
    then commitments [c2..c5] will be stored in the context.

    {3 Conflicts}

    Let Commitments(S) be the set of commitments directly staked on by staker S.

    Two stakers A and B are:

    {ul
      {li In total agreement iff Commitments(A) = Commitments(B).}
      {li In partial agreement iff either Commitments(A) ⊂ Commitments(B), or
        Commitments(B) ⊂ Commitments(A).}
      {li In conflict iff they are neither in total or partial agreement.}}

    We can further refine a conflict to note what they are in conflict about,
    e.g. they may be in conflict about the inbox, about execution, or both. We
    can resolve conflicts by first resolving the conflict about inbox, then
    about execution (since execution is irrelevant if the inbox is not
    correct).
    *)

type error +=
  | (* `Temporary *)
      Sc_rollup_does_not_exist of Sc_rollup_repr.t
  | (* `Temporary *)
      Sc_rollup_already_staked
  | (* `Temporary *)
      Sc_rollup_not_staked_on_lcc
  | (* `Temporary *)
      Sc_rollup_staker_backtracked
  | (* `Temporary *)
      Sc_rollup_unknown_commitment of
      Sc_rollup_repr.Commitment_hash.t
  | (* `Temporary *)
      Sc_rollup_parent_not_lcc
  | (* `Temporary *)
      Sc_rollup_too_far_ahead
  | (* `Temporary *)
      Sc_rollup_too_recent
  | (* `Temporary *)
      Sc_rollup_no_stakers
  | (* `Temporary *)
      Sc_rollup_disputed
  | (* `Temporary *)
      Sc_rollup_no_conflict
  | (* `Temporary *)
      Sc_rollup_not_staked
  | (* `Temporary *)
      Sc_rollup_remove_lcc
  | (* `Temporary *)
      Sc_rollup_bad_inbox_level

(** [originate context ~kind ~boot_sector] produces an address [a] for
   a smart contract rollup using the origination nonce found in
   [context]. This function also initializes the storage with a new
   entry indexed by [a] to remember the [kind] of the rollup at
   address [a] and also to remember its [boot_sector].

   Also returns the number of allocated bytes.  *)
val originate :
  Raw_context.t ->
  kind:Sc_rollup_repr.Kind.t ->
  boot_sector:string ->
  (Sc_rollup_repr.Address.t * Z.t * Raw_context.t) tzresult Lwt.t

(** [kind context address] returns [Some kind] iff [address] is an
    existing rollup of some [kind]. Returns [None] if [address] is
    not the address of an existing rollup. *)
val kind :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  Sc_rollup_repr.Kind.t option tzresult Lwt.t

(** [add_message context rollup msg] adds [msg] to [rollup]'s inbox.

    This function returns the updated context as well as the size diff. *)
val add_messages :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  string list ->
  (Sc_rollup_inbox.t * Z.t * Raw_context.t) tzresult Lwt.t

(** [inbox context rollup] returns the current state of the inbox. *)
val inbox :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  (Sc_rollup_inbox.t * Raw_context.t) tzresult Lwt.t

(** [deposit_stake context rollup staker] stakes [staker] at the last
    cemented commitment, freezing [sc_rollup_deposit] from [staker]'s account
    balance.

    May fail with:
    {ul
      {li [Sc_rollup_does_not_exist] if [rollup] does not exist}
      {li [Sc_rollup_already_staked] if [staker] is already staked}
      {li [Sc_rollup_staker_funds_too_low] if [staker] does not have enough funds to cover the deposit}
    }

    This should usually be followed by [refine_stake] to stake on a
    specific commitment.

    This function does not authenticate the staker. *)
val deposit_stake :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  Sc_rollup_repr.Staker.t ->
  Raw_context.t tzresult Lwt.t

(** [withdraw_stake context rollup staker] removes [staker] and returns
    any deposit previously frozen by [deposit_stake].

    May fail with:
    {ul
      {li [Sc_rollup_does_not_exist] if [rollup] does not exist}
      {li [Sc_rollup_not_staked_on_lcc] if [staker] is not staked on the last
      cemented commitment}
    }

    Note that it is not possible to be staked on a Cemented commitment other
    than the Last, because of Cementation Rule #4. See [cement_commitment]
    for details.

    By design, the operation wrapping this should {i not} be authenticated,
    as it may be necessary for nodes on the honest branch to refund stakers on
    the LCC. They must do so by using [withdraw_stake] as they are implicitly
    staked on the LCC and can not dispute it. *)
val withdraw_stake :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  Sc_rollup_repr.Staker.t ->
  Raw_context.t tzresult Lwt.t

(** [refine_stake context rollup staker commitment] moves the stake of
    [staker] to [commitment]. Because we do not assume any form of coordination
    between validators, we do not distinguish between {i adding new}
    commitments and {i staking on existing commitments}.  The storage of
    commitments is content-addressable to minimize storage duplication.

    Subsequent calls to [refine_stake] and [cement_commitment] must use
    a [context] with greater level, or behavior is undefined.

    The first time a commitment hash is staked on, it is assigned a deadline,
    which is counted in Tezos blocks (levels). Further stakes on the block does
    not affect the deadline. The commitment can not be cemented before the
    deadline has expired. Note that if a commitment is removed due to disputes
    and then re-entered, a later deadline may be assigned. Assuming one honest
    staker is always available, this only affects invalid commitments.

    May fail with:
    {ul
      {li [Sc_rollup_does_not_exist] if [rollup] does not exist}
      {li [Sc_rollup_too_far_ahead] if [staker] would be more than
        [sc_rollup_max_future_commitments] ahead of the Last Cemented Commitment}
      {li [Sc_rollup_bad_inbox_level] if [commitment]'s predecessor is
        less than [sc_rollup_commitment_frequency] blocks ahead}
      {li [Sc_rollup_not_staked] if [staker] is not staked}
      {li [Sc_rollup_staker_backtracked] if [staker] is not staked on an ancestor of [commitment]}
      {li [Sc_rollup_unknown_commitment] if the parent of the given commitment does not exist}
    }

    Returns the hash of the given commitment.

    This function does not authenticate the staker. *)
val refine_stake :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  Sc_rollup_repr.Staker.t ->
  Sc_rollup_repr.Commitment.t ->
  (Sc_rollup_repr.Commitment_hash.t * Raw_context.t) tzresult Lwt.t

(** This is a wrapper around [deposit_stake] and [refine_stake] that
    deposits a stake and then refines it to the specified commitment,
    creating that commitment if necessary. Before calling
    [deposit_stake] it checks that the staker is not already staked, and
    if so will skip that step and go straight to calling [refine_stake].

    May fail with:
    {ul
      {li [Sc_rollup_does_not_exist] if [rollup] does not exist}
      {li [Sc_rollup_too_far_ahead] if [staker] would be more than
        [sc_rollup_max_future_commitments] ahead of the Last Cemented Commitment}
      {li [Sc_rollup_bad_inbox_level] if [commitment]'s predecessor is
        less than [sc_rollup_commitment_frequency] blocks ahead}
      {li [Sc_rollup_staker_backtracked] if [staker] is not staked on an ancestor
        of [commitment]}
      {li [Sc_rollup_unknown_commitment] if the parent of the given commitment
        does not exist}
      {li [Sc_rollup_staker_funds_too_low] if [staker] is not previously a staker, and does not have enough funds
        to cover the deposit}
    }

    Returns the hash of the given commitment.

    This function does not authenticate the staker. *)
val publish_commitment :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  Sc_rollup_repr.Staker.t ->
  Sc_rollup_repr.Commitment.t ->
  (Sc_rollup_repr.Commitment_hash.t * Raw_context.t) tzresult Lwt.t

(** [last_cemented_commitment context rollup] returns the last cemented
    commitment of the rollup.

    If no commitments have been cemented, the rollup is said to be in a
    pre-boot state, and [last_cemented_commitment = Commitment_hash.zero].

    May fail with:
    {ul
      {li [Sc_rollup_does_not_exist] if [rollup] does not exist}} *)
val last_cemented_commitment :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  (Sc_rollup_repr.Commitment_hash.t * Raw_context.t) tzresult Lwt.t

(** [cement_commitment context rollup commitment] cements the given
    commitment.

    Subsequent calls to [refine_stake] and [cement_commitment] must use
    a [context] with greater level, or behavior is undefined.

    For cementing to succeed, the following must hold:
    {ol
      {li The deadline for [commitment] must have passed.}
      {li The predecessor of [commitment] must be the Last Cemented Commitment.}
      {li There must be at least one staker.}
      {li All stakers must be indirectly staked on [commitment].}
    }

    If successful, [last_cemented_commitment] is set to the given [commitment] and
    the appropriate amount of inbox messages is consumed.

    May fail with:
    {ul
      {li [Sc_rollup_does_not_exist] if [rollup] does not exist}
      {li [Sc_rollup_unknown_commitment] if [commitment] does not exist}
      {li [Sc_rollup_parent_not_lcc] if [commitment] is not the child of the last cemented commitment}
      {li [Sc_rollup_too_recent] if [commitment] has not passed its deadline}
      {li [Sc_rollup_no_stakers] if there are zero stakers}
      {li [Sc_rollup_disputed] if at least one staker is not staked on [commitment]}
    } *)
val cement_commitment :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  Sc_rollup_repr.Commitment_hash.t ->
  Raw_context.t tzresult Lwt.t

type conflict_point =
  Sc_rollup_repr.Commitment_hash.t * Sc_rollup_repr.Commitment_hash.t

(** [get_conflict_point context rollup staker1 staker2] returns the first point
    of disagreement between the given stakers. The returned commitments are
    distinct, and have the same [parent] commitment.

    May fail with:
    {ul
      {li [Sc_rollup_does_not_exist] if [rollup] does not exist}
      {li [Sc_rollup_no_conflict] if [staker1] is staked on an ancestor of the commitment staked on by [staker2], or vice versa}
    } *)
val get_conflict_point :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  Sc_rollup_repr.Staker.t ->
  Sc_rollup_repr.Staker.t ->
  (conflict_point * Raw_context.t) tzresult Lwt.t

(** [get_commitment context rollup commitment_hash] returns the commitment with the given hash.

    May fail with:
    {ul
      {li [Sc_rollup_does_not_exist] if [rollup] does not exist}
      {li [Sc_rollup_unknown_commitment] if [commitment] does not exist}
    } *)
val get_commitment :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  Sc_rollup_repr.Commitment_hash.t ->
  (Sc_rollup_repr.Commitment.t * Raw_context.t) tzresult Lwt.t

(** [remove_staker context rollup staker] forcibly removes the given [staker]
    and confiscates their frozen deposits.

    Any commitments no longer staked on are removed and storage reclaimed by
    [remove_staker]. Because of this there is no need to explicitly reject
    commitments.

    May fail with:
    {ul
      {li [Sc_rollup_does_not_exist] if [rollup] does not exist}
      {li [Sc_rollup_not_staked] if [staker] is not staked}
      {li [Sc_rollup_remove_lcc] if [staker] is staked on a cemented commitment}
    } *)
val remove_staker :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  Sc_rollup_repr.Staker.t ->
  Raw_context.t tzresult Lwt.t

(* [list context] returns a list of all rollups that have been originated. *)
val list : Raw_context.t -> Sc_rollup_repr.t list tzresult Lwt.t

(* [initial_level ctxt sc_rollup] returns the level at which a [sc_rollup] was 
   originated. *)
val initial_level :
  Raw_context.t -> Sc_rollup_repr.t -> Raw_level_repr.t tzresult Lwt.t
