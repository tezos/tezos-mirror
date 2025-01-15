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

(** Abstraction from low-level storage to handle unstake requests.

    This module is responsible for maintaining the
    {!Storage.Contract.Unstake_requests} table.

    Unstake requests added to this table are merged on a per-cycle basis,
    i.e. for a given contract we only retain, for each cycle, the total amount
    unstaked in the cycle.

    The table cannot contain more than
    {!Constants_storage.slashable_deposits_period} +
    {!Constants_repr.max_slashing_period} entries per contract, as one cannot
    add value without removing the finalizable ones.

    The table cannot contain a request of zero tez.
    Such a request would prevent a change of delegate while there are no
    unstaked tez.

    This module is responsible for applying slashing on unstake requests.

 *)

type finalizable =
  (Signature.Public_key_hash.t * Cycle_repr.t * Tez_repr.t) list

type transfer_result = Raw_context.t * Receipt_repr.balance_update_item list

(** [handle_finalizable_and_clear ctxt ~check_delegate_of_unfinalizable_requests ~handle_finalizable contract] will update the storage
    by removing all finalizable unstake request and calling [handle_finalizable]
    on each of them.

    This operation consumes the cost of the extraction of unstake_requests:
    {!Adaptive_issuance_costs.prepare_finalize_unstake_cost}.

    [check_delegate_of_unfinalizable_requests] can be used to interrupt the current
    finalisation by returning an error if it would be illegal to actually unstake funds from the given delegate.
*)
val handle_finalizable_and_clear :
  Raw_context.t ->
  Contract_repr.t ->
  check_delegate_of_unfinalizable_requests:
    (Signature.public_key_hash -> unit tzresult Lwt.t) ->
  handle_finalizable:
    (Raw_context.t -> finalizable -> transfer_result tzresult Lwt.t) ->
  transfer_result tzresult Lwt.t

(** [remove_from_unfinalizable_requests_and_finalize ctxt ~contract ~delegate
   ~check_delegate_of_unfinalizable_requests ~transfer_from_unstake ~handle_finalizable]
    allows to spend from unfinalizable unstake requests.

    This function ensures that the transfers from unstake request are licit.
    If not, it will only finalize the finalizable requests.

    Conditions to allow stake from unstake are the following:
    - the delegate of the unfinalizable requests is the staker,
    - the delegate has not been slashed in an unfinalizable cycle and has no
      pending denunciation.

    Transfers are done using the provided [transfer_from_unstake] and
    [handle_finalizable] functions successively.

    It returns the updated context, the balance updates and the part of the
    requested amount that could not be taken from the unfinalizable unstake
    requests.
*)
val remove_from_unfinalizable_requests_and_finalize :
  Raw_context.t ->
  contract:Contract_repr.t ->
  delegate:Signature.public_key_hash ->
  check_delegate_of_unfinalizable_requests:
    (Signature.public_key_hash -> unit tzresult Lwt.t) ->
  transfer_from_unstake:
    (Raw_context.t ->
    Cycle_repr.t ->
    Signature.public_key_hash ->
    Contract_repr.t ->
    Tez_repr.t ->
    transfer_result tzresult Lwt.t) ->
  handle_finalizable:
    (Raw_context.t -> finalizable -> transfer_result tzresult Lwt.t) ->
  Tez_repr.t ->
  (transfer_result * Tez_repr.t) tzresult Lwt.t

type error +=
  | Cannot_unstake_with_unfinalizable_unstake_requests_to_another_delegate

(** [finalize_and_add ctxt ~contract ~delegate ~handle_finalizable cycle amount] adds a request from [contract]
    to unstake [amount] from [delegate] at cycle [cycle].

    It also finalizes all finalizable unstake requests.

    @raises Assert_failure if [contract] already has unstake requests from another
      delegate (broken invariant). *)
val finalize_and_add :
  Raw_context.t ->
  contract:Contract_repr.t ->
  delegate:Signature.Public_key_hash.t ->
  handle_finalizable:
    (Raw_context.t -> finalizable -> transfer_result tzresult Lwt.t) ->
  Cycle_repr.t ->
  Tez_repr.t ->
  transfer_result tzresult Lwt.t

(** Slow functions only used for RPCs *)
module For_RPC : sig
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

  (** [prepare_finalize_unstake ctxt contract]
    preprocesses a [finalize_unstake] for [contract]. It returns a
    list of transfers [(d, c, a)] to do from delegate's [d] unstaked frozen
    deposits for cycle [c] of amount [a] in the [finalizable_field] as well as
    the remaining unfinalizable requests that should be kept in the storage in
    [unfinalizable].

    It returns [None] if there are no unstake requests.
  *)
  val prepare_finalize_unstake :
    Raw_context.t ->
    Contract_repr.t ->
    prepared_finalize_unstake option tzresult Lwt.t

  (** Apply current slash history to unfinalizable unstake requests.
      [prepare_finalize_unstake] does not compute this value because it is never
      used internally. However, we need to apply slashes anyways when trying to
      compute the accurate balance of a staker *)
  val apply_slash_to_unstaked_unfinalizable :
    Raw_context.t ->
    stored_requests ->
    (Cycle_repr.t * Tez_repr.t) list tzresult Lwt.t
end
