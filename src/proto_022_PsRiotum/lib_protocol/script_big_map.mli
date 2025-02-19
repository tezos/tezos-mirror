(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2021-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2022 Trili Tech <contact@trili.tech>                        *)
(* Copyright (c) 2022 Marigold <team@marigold.dev>                           *)
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

open Alpha_context

(** [empty] is the big map with no bindings. *)
val empty :
  'a Script_typed_ir.comparable_ty ->
  ('b, _) Script_typed_ir.ty ->
  ('a, 'b) Script_typed_ir.big_map

(** [mem ctxt key big_map] returns [true] iff [key] is bound in the
    given [big_map].
    Consumes the cost of hashing the given key.
    Consumes as [Storage.Big_map.Contents.mem] if the key is not bound
    yet in the current overlay. *)
val mem :
  context ->
  'key ->
  ('key, 'value) Script_typed_ir.big_map ->
  (bool * context) tzresult Lwt.t

(** [get ctxt key big_map] returns the value bound by [key] in the
    given [big_map]. If the [key] is not bound, [None] is returned instead.
    Consumes cost of hashing the given key.
    Consumes cost as [Storage.Big_map.Contents.find] in case of the given key
    is absent in the current overlay.
    Consumes cost of parsing data if the value is readed from storage. *)
val get :
  context ->
  'key ->
  ('key, 'value) Script_typed_ir.big_map ->
  ('value option * context) tzresult Lwt.t

(** [update ctxt key new_value big_map] updates the value bound by [key]
    with [v] if the [new_value] is [Some v]. When the [new_value] is [None],
    delete the entire entry bound by [key] in the [big_map].
    Consumes cost for hashing the given key.
    See {!get_and_update} for details. *)
val update :
  context ->
  'key ->
  'value option ->
  ('key, 'value) Script_typed_ir.big_map ->
  (('key, 'value) Script_typed_ir.big_map * context) tzresult Lwt.t

(** [get_and_update ctxt key new_value big_map] works just like
    [update ctxt key new_value big_map] except it also returns
    the old value bound by [key].
    Consumes cost for hashing the given key.
    This does {i not} modify the underlying storage, only the diff table. *)
val get_and_update :
  context ->
  'key ->
  'value option ->
  ('key, 'value) Script_typed_ir.big_map ->
  (('value option * ('key, 'value) Script_typed_ir.big_map) * context) tzresult
  Lwt.t
