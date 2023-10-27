(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
(*****************************************************************************)

(** Purposes for operators, indicating their role and thus the kinds of
    operations that they sign. *)
type t = Operating | Batching | Cementing | Recovering | Executing_outbox

(** List of possible purposes for operator specialization. *)
val all : t list

module Map : Map.S with type key = t

type operators = Signature.Public_key_hash.t Map.t

type error +=
  | Missing_operator of t
  | Too_many_operators of {
      expected_purposes : t list;
      given_operators : operators;
    }

(** [to_string p] returns a string representation of purpose [p]. *)
val to_string : t -> string

(** [of_string s] parses a purpose from the given string [s]. *)
val of_string : string -> t option

(** [of_string s] parses a purpose from the given string [s]. *)
val of_string_exn : string -> t

val operators_encoding : operators Data_encoding.t

(** [make_operator ?default ~needed_purposes operators] constructs a
    purpose map from a list of bindings [operators], with a potential
    [default] key. If [operators] does not cover all purposes of
    [needed_purposes] and does not contains a [default] key then
    fails.*)
val make_operator :
  ?default_operator:Signature.public_key_hash ->
  needed_purposes:t list ->
  (t * Signature.public_key_hash) list ->
  operators tzresult

(** [replace_operator ?default_operator ~needed_purposes purposed_keys
    operators] replaces keys of [operators] by [default_operator] if
    it's set or by the purposed key in [purposed_keys]. It does
    nothing if [default_operator] is not set or [purposed_keys] is
    empty. Similar to {!make_operator} the returns [operators]
    contains only mapping of purpose from needed_purposes.*)
val replace_operator :
  ?default_operator:Signature.public_key_hash ->
  needed_purposes:t list ->
  (t * Signature.public_key_hash) list ->
  operators ->
  operators tzresult

(** For each purpose, it returns a list of associated operation
    kinds. *)
val operation_kind : t -> Operation_kind.t list

(** For a list of operation kind, it returns a list of associated
    purpose. *)
val of_operation_kind : Operation_kind.t list -> t list
