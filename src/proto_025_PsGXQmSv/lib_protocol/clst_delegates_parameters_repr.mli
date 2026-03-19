(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Parameters for a delegate registered on the CLST contract.

    - [edge_of_clst_staking_over_baking_millionth] is a ratio from 0 to 1
    denoting the fraction of the rewards that accrue to the delegate's frozen
    deposit, the remainder being shared to CLST frozen deposits.

    - [ratio_of_clst_staking_over_direct_staking_billionth] is a ratio from 0 to
    1 denoting the maximum stake portion allocated from the CLST contract.

*)
type t = private {
  edge_of_clst_staking_over_baking_millionth : int32;
  ratio_of_clst_staking_over_direct_staking_billionth : int32;
}

val default : t

(** Value used in the Pending_parameters storage, to distinguish between new
    parameters and unregistration. *)
type update = Update of t | Unregister

type error += Invalid_clst_delegates_parameters

val make :
  edge_of_clst_staking_over_baking_millionth:int32 ->
  ratio_of_clst_staking_over_direct_staking_billionth:int32 ->
  t tzresult

val encoding : t Data_encoding.t

(** [update_encoding] is the encoding for update instructions. Note that the
    binary encoding reuse the option encoding, while the JSON encoding aims to
    be more explicit as it is used as RPCs output. *)
val update_encoding : update Data_encoding.t
