(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Trili Tech, <contact@trili.tech>                       *)
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
