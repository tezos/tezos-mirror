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

    Unstaked frozen deposits are tez coming from frozen deposits that have been
    unstaked, either:
    - manually with the "unstake" pseudo-operation,
    - automatically at cycle ends with the auto-staking mechanism, or
    - when a delegator changes delegate.
    The amounts are attached to a given cycle, the cycle at which the unstake
    happened, and are slashable for another
    [consensus_rights_delay + max_slashing_period - 1] cycles. After this, they can be
    finalized either with the "finalize_unstake" pseudo-operation, via
    auto-staking (for bakers only), or when staking or unstaking.

    Unstaked frozen deposits contain, for each cycle, a [current_amount] and an
    [initial_amount].
    Only unstaked frozen deposits for the current cycles can be increased, via
    unstaking.
    After a cycle has ended, the initial amount becomes the basis for
    forthcoming slashings. It can only be decreased by the
    "stake from unstake" mechanism.
    Slashings only affects the [current_amount] of the slashed cycles.

    Unstaked frozen deposits of finished cycles can be decreased by the
    "stake from unstake" mechanism, but only if the cycles haven't been slashed
    (to avoid shooting ourselves in the feet).

    To avoid the list of cycles growing unboundedly, amounts for finalizable
    cycles are squashed together, lazily, when the list needs to be updated,
    only.

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
