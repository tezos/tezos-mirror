(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2021 Trili Tech, <contact@trili.tech>                       *)
(*                                                                           *)
(*****************************************************************************)

(** Update a big map. See [Script_typed_ir.big_map_get_and_update] for details. *)
val update :
  'key ->
  'value option ->
  ('key, 'value) Protocol.Script_typed_ir.big_map ->
  Protocol.Alpha_context.t ->
  (('key, 'value) Protocol.Script_typed_ir.big_map * Protocol.Alpha_context.t)
  Environment.Error_monad.tzresult
  Lwt.t

(** Convert a list to a [Script_big_map]. If the list contains duplicate keys,
    the first occurence is used.
  *)
val of_list :
  'key Protocol.Script_typed_ir.comparable_ty ->
  ('value, _) Protocol.Script_typed_ir.ty ->
  ('key * 'value) list ->
  Protocol.Alpha_context.t ->
  (('key, 'value) Protocol.Script_typed_ir.big_map * Protocol.Alpha_context.t)
  Environment.Error_monad.tzresult
  Lwt.t
