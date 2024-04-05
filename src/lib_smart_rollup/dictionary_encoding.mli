(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Functori <contact@functori.com>              *)
(*                                                                           *)
(*****************************************************************************)

(** [dictionary_encoding keys string_of_key key_of_string
    value_encoding]: a json only encoding of a [(key, 'value)
    list]. JSON is of the form
{v
{ "k1" : v1,
  "k2" : v2,
  "k3" : v3 }
v} *)
val dictionary_encoding :
  keys:'k list ->
  string_of_key:('k -> string) ->
  key_of_string:(string -> 'k) ->
  value_encoding:('k -> 'v Data_encoding.t) ->
  ('k * 'v) list Data_encoding.t
