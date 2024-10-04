(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Functions to ease the manipulation of Michelson maps.

    A map in Michelson is a type-homegeneous, partial function of keys to
    values, along with the functions that operate on the structure (through a
    first-class module).
*)

open Script_typed_ir

val make :
  (module Boxed_map with type key = 'key and type value = 'value) ->
  ('key, 'value) map

val get_module :
  ('key, 'value) map ->
  (module Boxed_map with type key = 'key and type value = 'value)

(** [empty cmp_ty] creates a map module where keys have size
    [Gas_comparable_input_size.size_of_comparable_value cmp_ty] and are compared
    with [Script_comparable.compare_comparable cmp_ty] (used for sorting keys,
    which ensures a reasonable complexity of the map functions).
    The function returns an empty map packaged as a first-class map module. *)
val empty : 'a comparable_ty -> ('a, 'b) map

(** [empty_from map] creates an empty map module where the size of keys and the
    comparison function are those of [map]. *)
val empty_from : ('a, 'b) map -> ('a, 'c) map

val fold :
  ('key -> 'value -> 'acc -> 'acc) -> ('key, 'value) map -> 'acc -> 'acc

val fold_es :
  ('key -> 'value -> 'acc -> 'acc tzresult Lwt.t) ->
  ('key, 'value) map ->
  'acc ->
  'acc tzresult Lwt.t

(** [update k (Some v) map] associates [v] to [k] in [map] (overwriting the
    previous value, if any), and [update k None map] removes the potential
    association to [k] in [map]. *)
val update : 'a -> 'b option -> ('a, 'b) map -> ('a, 'b) map

val mem : 'key -> ('key, 'value) map -> bool

val get : 'key -> ('key, 'value) map -> 'value option

val size : ('a, 'b) map -> Script_int.n Script_int.num

val map_es_in_context :
  ('context -> 'key -> 'value1 -> ('value2 * 'context) tzresult Lwt.t) ->
  'context ->
  ('key, 'value1) map ->
  (('key, 'value2) map * 'context) tzresult Lwt.t
