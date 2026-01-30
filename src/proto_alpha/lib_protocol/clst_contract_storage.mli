(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Abstraction to handle high level interactions of the CLST enshrined contract.

    This module is responsible for maintaining the contract's storage, and
    handling the token transfers between the different containers via {!Token}
    and {!Clst_storage} (via the {!Alpha_context} abstraction).
*)

open Alpha_context
open Script_native_types
open Script_typed_ir

(** [token_id] is the token identifier of CLST, being 0 by default for FA2
    single asset contracts. *)
val token_id : CLST_types.nat

(** [get_storage ctxt] returns the storage retrieved and parsed from the
    context. It doesn't fail if the storage didn't exist in the context, i.e.
    the CLST contract has not been originated. *)
val get_storage :
  context -> (CLST_types.storage option * context) tzresult Lwt.t

(** [get_balance_from_storage ctxt storage address] returns the balance from the
    given address, extracted from the CLST contract's storage. Returns `zero_n`
    if no balance has been found, following FA2.1 specification. *)
val get_balance_from_storage :
  context ->
  CLST_types.storage ->
  address ->
  (CLST_types.nat * context) tzresult Lwt.t

(** [get_balance context contract] retrieves the balance of a given contract on
    the CLST contract. This is a combination of `get_storage` and
    `get_balance_from_storage`. *)
val get_balance :
  context -> Contract.t -> (CLST_types.nat * context) tzresult Lwt.t

(** [set_balance_from_storage ctxt storage address amount] updates the balance of the
    given address to [amount]. *)
val set_balance_from_storage :
  context ->
  CLST_types.storage ->
  address ->
  CLST_types.nat ->
  (CLST_types.storage * context) tzresult Lwt.t

(** [get_total_supply context] returns the total supply of CLST tokens
    in the CLST contract. *)
val get_total_supply : context -> (CLST_types.nat * context) tzresult Lwt.t

(** [deposit_to_clst_deposits context ~clst_contract_hash amount] deposits
    [amount] tez from [clst_contract_hash] balance to the CLST deposits
    container *)
val deposit_to_clst_deposits :
  context ->
  clst_contract_hash:Contract_hash.t ->
  Tez.t ->
  (context * Receipt.balance_updates) tzresult Lwt.t

(** [withdraw_from_clst_deposits context ~clst_contract_hash amount] withdraws
    [amount] tez from the CLST deposits container to [clst_contract_hash]
    balance. *)
val withdraw_from_clst_deposits :
  context ->
  clst_contract_hash:Contract_hash.t ->
  Tez.t ->
  (context * Receipt.balance_updates) tzresult Lwt.t
