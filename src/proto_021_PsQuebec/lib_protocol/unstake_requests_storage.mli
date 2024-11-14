(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Simple abstraction from low-level storage to handle unstake requests.

    This module is responsible for maintaining the
    {!Storage.Contract.Unstake_requests} table. *)

type finalizable =
  (Signature.Public_key_hash.t * Cycle_repr.t * Tez_repr.t) list

type stored_requests = Storage.Unstake_request.t = {
  delegate : Signature.Public_key_hash.t;
  requests : (Cycle_repr.t * Tez_repr.t) list;
}

type prepared_finalize_unstake = {
  finalizable : finalizable;
  unfinalizable : stored_requests;
}

val prepared_finalize_unstake_encoding :
  prepared_finalize_unstake Data_encoding.encoding

(** [prepare_finalize_unstake ctxt ~for_next_cycle_use_only_after_slashing contract]
    preprocesses a [finalize_unstake] for [contract]. It returns a
    list of transfers [(d, c, a)] to do from delegate's [d] unstaked frozen
    deposits for cycle [c] of amount [a] in the [finalizable_field] as well as
    the remaining unfinalizable requests that should be kept in the storage in
    [unfinalizable].

    It returns [None] if there are no unstake requests.

    If [for_next_cycle_use_only_after_slashing] is true, the finalisation is
    done for the next cycle. It is meant to be used only at cycle end after the
    application of the slashing.

 *)
val prepare_finalize_unstake :
  Raw_context.t ->
  for_next_cycle_use_only_after_slashing:bool ->
  Contract_repr.t ->
  prepared_finalize_unstake option tzresult Lwt.t

(** [update ctxt contract requests] updates unstake requests for [contract]. *)
val update :
  Raw_context.t ->
  Contract_repr.t ->
  stored_requests ->
  Raw_context.t tzresult Lwt.t

type error +=
  | Cannot_unstake_with_unfinalizable_unstake_requests_to_another_delegate

(** [add ctxt ~contract ~delegate cycle amount] adds a request from [contract]
    to unstake [amount] from [delegate] at cycle [cycle].

    @raises Assert_failure if [contract] already has unstake requests from another
      delegate (broken invariant). *)
val add :
  Raw_context.t ->
  contract:Contract_repr.t ->
  delegate:Signature.Public_key_hash.t ->
  Cycle_repr.t ->
  Tez_repr.t ->
  Raw_context.t tzresult Lwt.t

(** Slow functions only used for RPCs *)
module For_RPC : sig
  (** Apply current slash history to unfinalizable unstake requests.
      [prepare_finalize_unstake] does not compute this value because it is never
      used internally. However, we need to apply slashes anyways when trying to
      compute the accurate balance of a staker *)
  val apply_slash_to_unstaked_unfinalizable :
    Raw_context.t ->
    stored_requests ->
    (Cycle_repr.t * Tez_repr.t) list tzresult Lwt.t
end
