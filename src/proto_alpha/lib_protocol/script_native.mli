(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Alpha_context
open Script_native_types
open Script_typed_ir

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
