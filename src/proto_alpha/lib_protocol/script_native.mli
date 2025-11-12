(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Alpha_context
open Script_native_types
open Script_typed_ir
open Script_typed

type ex_kind_and_types =
  | Ex_kind_and_types :
      (('arg, 'storage) kind * ('arg, _, 'storage, _) types)
      -> ex_kind_and_types

val get_typed_kind_and_types :
  Script_native_repr.t -> ex_kind_and_types tzresult

(* [execute ctxt kind arg storage] executes the given native contract [kind]
   with [arg] and [storage], and returns the list of operations, the new storage
   and the context. *)
val execute :
  context * step_constants ->
  ('arg, 'storage) kind ->
  'arg ->
  'storage ->
  ((Script_typed_ir.operation Script_list.t, 'storage) pair * context) tzresult
  Lwt.t

module Internal_for_tests : sig
  (* Types maintained internally to build the native contracts types and
     entrypoints. *)
  type 'a ty_node = {untyped : Script.node; typed : 'a ty_ex_c}

  type ('arg, 'storage) tys = 'arg ty_node * 'storage ty_node

  type ex_ty_node = Ex : ('arg, 'storage) tys -> ex_ty_node

  val types_of_kind : Script_native_repr.t -> ex_ty_node tzresult
end
