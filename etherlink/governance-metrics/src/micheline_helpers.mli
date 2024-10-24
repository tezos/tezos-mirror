(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Tezos_micheline

val unit : Micheline_parser.node

val is_unit : Micheline_parser.node -> bool

val decode_nat : Micheline_parser.node -> Z.t tzresult

(** [decode_string micheline] returns the Caml version of a Micheline.String node. *)
val decode_string : Micheline_parser.node -> string tzresult

(** [decode_bytes micheline] returns the Caml version of a Micheline.Bytes node. *)
val decode_bytes : Micheline_parser.node -> Bytes.t tzresult

(** [decode_pair f pair] destruct the [pair] micheline and apply the [f]
    function to the destruct result. *)
val decode_pair :
  (Micheline_parser.node list -> 'b tzresult) ->
  Micheline_parser.node ->
  'b tzresult

(** [decode_option f opt] returns an option, if the [opt] micheline is Some, it applies
    the [f] function on its argument.  *)
val decode_option :
  (Micheline_parser.node -> 'b tzresult) ->
  Micheline_parser.node ->
  'b option tzresult

(** [decode_set f set] decode [set] as a micheline set into a list and applies [f] to all
    elements of [set]. *)
val decode_set :
  (Micheline_parser.node -> 'b tzresult) ->
  Micheline_parser.node ->
  'b list tzresult

(** [decode_map decode_key decode_elt map] decode [map] as a micheline map into a list and applies
    [decode_key] to its keys and [decode_elt] for all its elements. *)
val decode_map :
  decode_key:(Micheline_parser.node -> 'key tzresult) ->
  decode_elt:(Micheline_parser.node -> 'value tzresult) ->
  Micheline_parser.node ->
  ('key * 'value) list tzresult
