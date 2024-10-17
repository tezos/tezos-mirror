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
