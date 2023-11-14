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

(** Simple abstraction from low-level storage to handle unstaked frozen deposits.

    This module is responsible for maintaining the
    {!Storage.Contract.Unstaked_frozen_deposits} table. *)

(** [balance ctxt delegate cycle] returns the amount of unstaked frozen deposits
    for [delegate] at [cycle].
    If [cycle] is an unslashable cycle, the returned amount is the squashed
    amount of all the unslashable cycles. *)
val balance :
  Raw_context.t ->
  Signature.Public_key_hash.t ->
  Cycle_repr.t ->
  Tez_repr.t tzresult Lwt.t

(** [get] acts like [balance] but returns both the initial amount and the
    current amount. *)
val get :
  Raw_context.t ->
  Signature.Public_key_hash.t ->
  Cycle_repr.t ->
  Deposits_repr.t tzresult Lwt.t

(** [credit_only_call_from_token ctxt staker cycle amount] credits the
    unstaked frozen deposits for [staker] at [cycle] by [amount].
    If [cycle] is an unslashable cycle, the credited cycle is the last
    unslashable cycle. *)
val credit_only_call_from_token :
  Raw_context.t ->
  Unstaked_frozen_staker_repr.t ->
  Cycle_repr.t ->
  Tez_repr.t ->
  Raw_context.t tzresult Lwt.t

(** [spend_only_call_from_token ctxt staker cycle amount] spends [amount]
    from the unstaked frozen deposits for [staker] at [cycle].
    If [cycle] is an unslashable cycle, the amount is spent from the last
    unslashable cycle.
    The function returns the error [Subtraction_underflow] if the balance is
    too low. *)
val spend_only_call_from_token :
  Raw_context.t ->
  Unstaked_frozen_staker_repr.t ->
  Cycle_repr.t ->
  Tez_repr.t ->
  Raw_context.t tzresult Lwt.t

(** [decrease_initial_amount_only_for_stake_from_unstake ctxt staker cycle amount]
    decreases [amount] from the unstaked frozen deposits for [staker] at [cycle].
    It is only called if the cycle hasn't been slashed, so the amount removed is
    the same as the amount spent in [current_amount].
*)
val decrease_initial_amount_only_for_stake_from_unstake :
  Raw_context.t ->
  Signature.public_key_hash ->
  Cycle_repr.t ->
  Tez_repr.t ->
  Raw_context.t tzresult Lwt.t
