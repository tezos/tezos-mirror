(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

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
