(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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
    A commitment that is not cemented is said to be disputable.

    {3 Stakers}
    The Stakers table maps Stakers (implicit accounts) to commitments hashes.

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

module Commitment = Sc_rollup_commitment_repr
module Commitment_hash = Commitment.Hash

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
  (Commitment_hash.t * Raw_context.t) tzresult Lwt.t

(** [last_cemented_commitment_hash_with_level ctxt sc_rollup] returns the hash
    and level of the last cemented commitment (lcc) for [sc_rollup]. If the
    rollup exists but no lcc exists, the initial commitment
    [Sc_rollup.Commitment.zero] together with the rollup origination level is
    returned.

    May fail with:
      {ul
        {li [Sc_rollup_does_not_exist] if [rollup] does not exist}}
*)
val last_cemented_commitment_hash_with_level :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  (Commitment_hash.t * Raw_level_repr.t * Raw_context.t) tzresult Lwt.t

(** [get_commitment context rollup commitment_hash] returns the commitment with
    the given hash.

    May fail with:
    {ul
      {li [Sc_rollup_does_not_exist] if [rollup] does not exist}
      {li [Sc_rollup_unknown_commitment] if [commitment] does not exist}
    }
*)
val get_commitment :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  Commitment_hash.t ->
  (Commitment.t * Raw_context.t) tzresult Lwt.t

(** [get_commitment_opt_unsafe context rollup commitment_hash] returns an
    [Option.t] which is either a defined value containing the commitment with
    the given hash, or `None` if such a commitment does not exist. This
    function *must* be called only after they have checked for the existence
    of the rollup, and therefore it is not necessary for it to check for the
    existence of the rollup again. Otherwise, use the safe function
    {!get_commitment}.
*)
val get_commitment_opt_unsafe :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  Commitment_hash.t ->
  (Commitment.t Option.t * Raw_context.t) tzresult Lwt.t

(** [get_commitment_unsafe context rollup commitment_hash] returns the commitment
    with the given hash.
    This function *must* be called only after they have checked for the existence
    of the rollup, and therefore it is not necessary for it to check for the
    existence of the rollup again. Otherwise, use the safe function
    {!get_commitment}.

    May fail with:
    {ul
      {li [Sc_rollup_unknown_commitment] if [commitment] does not exist}
    }
*)
val get_commitment_unsafe :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  Commitment_hash.t ->
  (Commitment.t * Raw_context.t) tzresult Lwt.t

(** [set_commitment_added ctxt rollup node current] sets the commitment
    addition time of [node] to [current] iff the commitment time was
    not previously set, and leaves it unchanged otherwise.
 *)
val set_commitment_added :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  Commitment_hash.t ->
  Raw_level_repr.t ->
  (int * Raw_level_repr.t * Raw_context.t) tzresult Lwt.t

(** [get_predecessor_opt_unsafe ctxt rollup commitment_hash] returns an
    [Option.t] value containing the [rollup] commitment predecessor of
    [commitment_hash] in the [ctxt], if any. It does not check for the
    existence of the [rollup]. *)
val get_predecessor_opt_unsafe :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  Commitment_hash.t ->
  (Commitment_hash.t Option.t * Raw_context.t) tzresult Lwt.t

(** [check_if_commitments_are_related ~descendant ~ancestor] checks whether a
    commitment with hash [~ancestor] exists as a predecessor of [~descendant],
    among the list of commitments stored for [rollup] in [ctxt]. *)
val check_if_commitments_are_related :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  descendant:Commitment_hash.t ->
  ancestor:Commitment_hash.t ->
  (bool * Raw_context.t) tzresult Lwt.t

(** Hash a commitment and account for gas spent. *)
val hash :
  Raw_context.t -> Commitment.t -> (Raw_context.t * Commitment_hash.t) tzresult

module Internal_for_tests : sig
  (** [get_cemented_commitments_with_levels ctxt rollup] returns a list of all
    cemented commitment hashes and corresponding inbox levels that are present
    in the storage, ordered by inbox level.

    May fail with:
    {ul
      {li [Sc_rollup_does_not_exist] if [rollup] does not exist}
    }
*)
  val get_cemented_commitments_with_levels :
    Raw_context.t ->
    Sc_rollup_repr.t ->
    ((Commitment_hash.t * Raw_level_repr.t) list * Raw_context.t) tzresult Lwt.t
end
