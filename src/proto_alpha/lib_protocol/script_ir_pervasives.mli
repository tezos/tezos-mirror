(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Tocqueville Group, Inc. <contact@tezos.com>            *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(** This module contains basic operations on Michelson compound types: lists,
    sets, maps and big_maps. They are mainly used by the interpreter to execute
    scripts. *)

open Alpha_context

val list_empty : 'a Script_typed_ir.boxed_list

val list_cons :
  'a -> 'a Script_typed_ir.boxed_list -> 'a Script_typed_ir.boxed_list

val empty_set : 'a Script_typed_ir.comparable_ty -> 'a Script_typed_ir.set

val set_fold :
  ('elt -> 'acc -> 'acc) -> 'elt Script_typed_ir.set -> 'acc -> 'acc

val set_update : 'a -> bool -> 'a Script_typed_ir.set -> 'a Script_typed_ir.set

val set_mem : 'elt -> 'elt Script_typed_ir.set -> bool

val set_size : 'elt Script_typed_ir.set -> Script_int.n Script_int.num

val empty_map : 'a Script_typed_ir.comparable_ty -> ('a, 'b) Script_typed_ir.map

val map_fold :
  ('key -> 'value -> 'acc -> 'acc) ->
  ('key, 'value) Script_typed_ir.map ->
  'acc ->
  'acc

val map_update :
  'a ->
  'b option ->
  ('a, 'b) Script_typed_ir.map ->
  ('a, 'b) Script_typed_ir.map

val map_mem : 'key -> ('key, 'value) Script_typed_ir.map -> bool

val map_get : 'key -> ('key, 'value) Script_typed_ir.map -> 'value option

val map_key_ty :
  ('a, 'b) Script_typed_ir.map -> 'a Script_typed_ir.comparable_ty

val map_size : ('a, 'b) Script_typed_ir.map -> Script_int.n Script_int.num

val compare_comparable : 'a Script_typed_ir.comparable_ty -> 'a -> 'a -> int

val compare_address : Script_typed_ir.address -> Script_typed_ir.address -> int
