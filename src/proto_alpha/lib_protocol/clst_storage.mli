(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Abstraction for low level storage to handle CLST deposits, unstake requests
    and finalization.


    This module is responsible for maintaining the
    {!Storage.Clst.Deposits_balance} table.
*)

(** [increase_deposit_only_call_from_token ctxt amount] increases the deposited
    amount of tez in the CLST contract. Such amount is considered frozen and not
    spendable. *)
val increase_deposit_only_call_from_token :
  Raw_context.t -> Tez_repr.t -> Raw_context.t tzresult Lwt.t

(** [decrease_deposit_only_call_from_token ctxt amount] decreases the deposited
    amount of tez in the CLST contract. *)
val decrease_deposit_only_call_from_token :
  Raw_context.t -> Tez_repr.t -> Raw_context.t tzresult Lwt.t

(** [increase_redeemed_frozen_deposit_only_call_from_token ctxt cycle
    amount] increases the [amount] of [tez] redeemed for the given
    [cycle]. *)
val increase_redeemed_frozen_deposit_only_call_from_token :
  Raw_context.t -> Cycle_repr.t -> Tez_repr.t -> Raw_context.t tzresult Lwt.t

(** [decrease_redeemed_frozen_deposit_only_call_from_token ctxt cycle
    amount] decreases the [amount] of [tez] redeemed for the given
    [cycle]. *)
val decrease_redeemed_frozen_deposit_only_call_from_token :
  Raw_context.t -> Cycle_repr.t -> Tez_repr.t -> Raw_context.t tzresult Lwt.t

module For_RPC : sig
  val get_redeemed_balance :
    Raw_context.t -> Contract_repr.t -> Tez_repr.t option tzresult Lwt.t
end
