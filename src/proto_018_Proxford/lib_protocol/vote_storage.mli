(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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

(** Manages all the voting related storage in Storage.Vote.  *)

(** [get_delegate_proposal_count ctxt proposer] returns the number of
    proposals already made by [proposer] in the current voting cycle.

    This number of proposals, aka [count], has its own storage bucket.

    @return [0] if the [count] of the proposer was not initialized.

    @return [Error Storage_error] if the deserialization of [count]
    fails. *)
val get_delegate_proposal_count :
  Raw_context.t -> Signature.public_key_hash -> int tzresult Lwt.t

(** [set_delegate_proposal_count ctxt proposer count] sets
    [proposer]'s number of submitted proposals to [count].

    More precisely, the relevant storage bucket is allocated and
    initialized to [count] if it didn't exist; otherwise it is simply
    updated. *)
val set_delegate_proposal_count :
  Raw_context.t -> Signature.public_key_hash -> int -> Raw_context.t Lwt.t

(** [has_proposed ctxt proposer proposal] indicates whether the
    [proposer] has already proposed the [proposal]. *)
val has_proposed :
  Raw_context.t -> Signature.public_key_hash -> Protocol_hash.t -> bool Lwt.t

(** [add_proposal ctxt proposer proposal] records the submission of
    [proposal] by [proposer]. *)
val add_proposal :
  Raw_context.t ->
  Signature.public_key_hash ->
  Protocol_hash.t ->
  Raw_context.t Lwt.t

(** Computes for each proposal how many delegates proposed it. *)
val get_proposals : Raw_context.t -> int64 Protocol_hash.Map.t tzresult Lwt.t

val clear_proposals : Raw_context.t -> Raw_context.t Lwt.t

(** Counts of the votes *)
type ballots = {yay : int64; nay : int64; pass : int64}

(** All vote counts set to zero. *)
val ballots_zero : ballots

(** Encoding for {!ballots}. *)
val ballots_encoding : ballots Data_encoding.t

(** Equality check for {!ballots}. *)
val equal_ballots : ballots -> ballots -> bool

(** Pretty printer for {!ballots}. *)
val pp_ballots : Format.formatter -> ballots -> unit

val has_recorded_ballot :
  Raw_context.t -> Signature.Public_key_hash.t -> bool Lwt.t

(** Records a vote for a delegate, returns a
    [Error (Storage_error Existing_key)] if the vote was already registered *)
val record_ballot :
  Raw_context.t ->
  Signature.Public_key_hash.t ->
  Vote_repr.ballot ->
  Raw_context.t tzresult Lwt.t

(** Computes the sum of the current ballots weighted by stake. *)
val get_ballots : Raw_context.t -> ballots tzresult Lwt.t

val get_ballot_list :
  Raw_context.t -> (Signature.Public_key_hash.t * Vote_repr.ballot) list Lwt.t

val clear_ballots : Raw_context.t -> Raw_context.t Lwt.t

val listings_encoding :
  (Signature.Public_key_hash.t * int64) list Data_encoding.t

(** Populates [!Storage.Vote.Listings] using the currently existing
   staking power and sets `Voting_power_in_listings`. Inactive
   delegates or delegates without the minimal required stake are not
   included in the listings.
   If adaptive inflation is enabled, voting power accounts for
   {!Constants_parametric_repr.staking_over_delegation_edge}. *)
val update_listings : Raw_context.t -> Raw_context.t tzresult Lwt.t

(** Verifies the presence of a delegate in the listing. *)
val in_listings : Raw_context.t -> Signature.Public_key_hash.t -> bool Lwt.t

val get_listings :
  Raw_context.t -> (Signature.Public_key_hash.t * int64) list Lwt.t

type delegate_info = {
  voting_power : Int64.t option;
  current_ballot : Vote_repr.ballot option;
  current_proposals : Protocol_hash.t list;
  remaining_proposals : int;
}

val pp_delegate_info : Format.formatter -> delegate_info -> unit

val delegate_info_encoding : delegate_info Data_encoding.t

val get_delegate_info :
  Raw_context.t -> Signature.public_key_hash -> delegate_info tzresult Lwt.t

val get_voting_power_free :
  Raw_context.t -> Signature.public_key_hash -> int64 tzresult Lwt.t

val get_voting_power :
  Raw_context.t ->
  Signature.public_key_hash ->
  (Raw_context.t * int64) tzresult Lwt.t

(** Returns the sum of all voting power in the listings,
    without accounting for gas cost. *)
val get_total_voting_power_free : Raw_context.t -> int64 tzresult Lwt.t

(** Returns the sum of all voting power in the listings. *)
val get_total_voting_power :
  Raw_context.t -> (Raw_context.t * int64) tzresult Lwt.t

val get_current_quorum : Raw_context.t -> int32 tzresult Lwt.t

val get_participation_ema : Raw_context.t -> int32 tzresult Lwt.t

val set_participation_ema :
  Raw_context.t -> int32 -> Raw_context.t tzresult Lwt.t

(** Indicates whether there is a current proposal in the storage. *)
val current_proposal_exists : Raw_context.t -> bool Lwt.t

(** Retrieves the current proposal.

    @return [Error Storage_error] if there is no current proposal, or
    if the deserialization fails. *)
val get_current_proposal : Raw_context.t -> Protocol_hash.t tzresult Lwt.t

(** Retrieves the current proposal.

    @return [None] if there is no current proposal.

    @return [Error Storage_error] if the deserialization fails. *)
val find_current_proposal :
  Raw_context.t -> Protocol_hash.t option tzresult Lwt.t

(** Registers a current proposal.

    @return [Error (Storage_error Existing_key)] if there was already
    a current proposal. *)
val init_current_proposal :
  Raw_context.t -> Protocol_hash.t -> Raw_context.t tzresult Lwt.t

(** Removes the current proposal. Does nothing if there was no current
    proposal. *)
val clear_current_proposal : Raw_context.t -> Raw_context.t Lwt.t

(** Sets the initial quorum to 80% and period kind to proposal. *)
val init :
  Raw_context.t -> start_position:Int32.t -> Raw_context.t tzresult Lwt.t
