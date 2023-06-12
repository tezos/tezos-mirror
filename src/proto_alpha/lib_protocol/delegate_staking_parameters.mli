(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs                                           *)
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

val of_delegate :
  Raw_context.t ->
  Signature.Public_key_hash.t ->
  Staking_parameters_repr.t tzresult Lwt.t

val find :
  Raw_context.t ->
  Signature.Public_key_hash.t ->
  Staking_parameters_repr.t option tzresult Lwt.t

val of_delegate_for_cycle :
  Raw_context.t ->
  Signature.Public_key_hash.t ->
  Cycle_repr.t ->
  Staking_parameters_repr.t tzresult Lwt.t

val pending_updates :
  Raw_context.t ->
  Signature.Public_key_hash.t ->
  (Cycle_repr.t * Staking_parameters_repr.t) list Lwt.t

val register_update :
  Raw_context.t ->
  Signature.Public_key_hash.t ->
  Staking_parameters_repr.t ->
  Raw_context.t tzresult Lwt.t

(** Maintenance of staking parameters at the beginning of cycle [new_cycle].
    This function iterates on all registered delegates. *)
val activate :
  Raw_context.t -> new_cycle:Cycle_repr.t -> Raw_context.t tzresult Lwt.t

(** [pay_rewards ctxt ?active_stake source delegate] transfers the rewards to the
    [delegate] spendable balance and frozen balance.

    The distribution is based on the baker's staking parameters.

    If adaptive inflation is enabled, it also accounts for the repartition of the
    delegate's [active_stake] between delegated token and frozen deposits.
    If [active_stake] is not provided, it will be retrived from the context.
*)
val pay_rewards :
  Raw_context.t ->
  ?active_stake:Stake_repr.t ->
  source:[< Token.giver] ->
  delegate:Signature.public_key_hash ->
  Tez_repr.t ->
  (Raw_context.t * Receipt_repr.balance_updates) tzresult Lwt.t
