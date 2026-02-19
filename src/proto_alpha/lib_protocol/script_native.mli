(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Alpha_context
open Script_native_types
open Script_typed_ir

module CLST_contract : sig
  type error +=
    | Empty_transfer
    | Non_empty_transfer of Destination.t * Tez.t
    | Non_implicit_contract of Destination.t
    | Balance_too_low of Destination.t * CLST_types.nat * CLST_types.nat
    | Amount_too_large of Destination.t * CLST_types.nat
    | Only_owner_can_change_operator of Destination.t * Destination.t
end

val get_views :
  'arg 'storage.
  ('arg, 'storage) kind -> (Script_string.t, 'storage ex_view) map tzresult

(* [execute ctxt kind arg storage] executes the given native contract [kind]
   with [arg] and [storage], and returns the list of operations, the new storage
   and the context. *)
val execute :
  context * step_constants ->
  ('arg, 'storage) kind ->
  'arg ->
  'storage ->
  ((Script_typed_ir.operation Script_list.t
   * 'storage
   * Receipt.balance_updates)
  * context)
  tzresult
  Lwt.t
