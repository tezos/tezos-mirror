(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Kind of native contracts supported. *)
type t = CLST  (** CLST is the canonical liquid staking token contract. *)

(** Representation of native contracts in the Cache or as a result of fetching
    in the storage. This is equivalent to `Script.t` for native contracts, that
    represents both the contract code and its storage. *)
type with_storage = {kind : t; storage : Script_repr.lazy_expr}

module CLST_contract : sig
  val initial_storage : Script_repr.lazy_expr

  val with_initial_storage : with_storage
end

val encoding : t Data_encoding.t

val with_storage_encoding : with_storage Data_encoding.t

val rpc_arg : t RPC_arg.arg

val equal : t -> t -> bool
