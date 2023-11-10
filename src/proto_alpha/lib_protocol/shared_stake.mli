(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

type shared = {baker_part : Tez_repr.t; stakers_part : Tez_repr.t}

(** [share ctxt delegate amount] shares [amount] between a [baker_part] and a
    [stakers_part] proportionally to their shares in [delegate]'s frozen
    deposits. *)
val share :
  Raw_context.t ->
  Signature.public_key_hash ->
  Tez_repr.t ->
  shared tzresult Lwt.t

(** [pay_rewards ctxt ?active_stake source delegate] transfers the rewards to the
    [delegate] spendable balance and frozen balance.

    The distribution is based on the baker's staking parameters.

    If adaptive issuance is enabled, it also accounts for the repartition of the
    delegate's [active_stake] between delegated token and frozen deposits.
    If [active_stake] is not provided, it will be retrieved from the context.
*)
val pay_rewards :
  Raw_context.t ->
  ?active_stake:Stake_repr.t ->
  source:[< Token.giver] ->
  delegate:Signature.public_key_hash ->
  Tez_repr.t ->
  (Raw_context.t * Receipt_repr.balance_updates) tzresult Lwt.t
