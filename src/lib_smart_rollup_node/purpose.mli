(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
(*****************************************************************************)

(** Purposes for operators, indicating their role and thus the kinds of
    operations that they sign. *)
type 'a t =
  | Operating : Signature.public_key_hash t
  | Batching : Signature.public_key_hash list t
  | Cementing : Signature.public_key_hash t
  | Recovering : Signature.public_key_hash t
  | Executing_outbox : Signature.public_key_hash t

(** existential type for purpose. *)
type ex_purpose = Purpose : 'a t -> ex_purpose

(** List of possible purposes for operator specialization. *)
val all : ex_purpose list

module Map : Map.S with type key = ex_purpose

(** Operator type. An operator is either a single key or a list of
   keys. *)
type 'a operator =
  | Single : Signature.public_key_hash -> Signature.public_key_hash operator
  | Multiple :
      Signature.public_key_hash list
      -> Signature.public_key_hash list operator

(** existential type for operator. *)
type ex_operator = Operator : 'a operator -> ex_operator

(** [operators] type is a map from purpose to operator. This type is
    private to control its creation and make sure [`kind t] and [`kind
    operator] are correctly related with [`kind] *)
type operators = private ex_operator Map.t

type error +=
  | Missing_operator of ex_purpose
  | Too_many_operators of {
      expected_purposes : ex_purpose list;
      given_operators : operators;
    }

(** [to_string_ex_purpose p] returns a string representation of purpose [p]. *)
val to_string_ex_purpose : ex_purpose -> string

val pp : Format.formatter -> 'a t -> unit

val pp_ex_purpose : Format.formatter -> ex_purpose -> unit

(** [of_string_ex_purpose s] parses a purpose from the given string
    [s]. *)
val of_string_ex_purpose : string -> ex_purpose option

(** [of_string_exn_ex_purpose s] parses a purpose from the given
    string [s]. *)
val of_string_exn_ex_purpose : string -> ex_purpose

val operators_encoding : operators Data_encoding.t

(** [make_operator ?default ~needed_purposes operators] constructs a
    purpose map from a list of bindings [operators], with a potential
    [default] key. If [operators] does not cover all purposes of
    [needed_purposes] and does not contains a [default] key then
    fails.*)
val make_operator :
  ?default_operator:Signature.public_key_hash ->
  needed_purposes:ex_purpose list ->
  (ex_purpose * Signature.public_key_hash) list ->
  operators tzresult

(** [replace_operator ?default_operator ~needed_purposes purposed_keys
    operators] replaces keys of [operators] by [default_operator] if
    it's set or by the purposed key in [purposed_keys]. It does
    nothing if [default_operator] is not set or [purposed_keys] is
    empty. Similar to {!make_operator} the returns [operators]
    contains only mapping of purpose from needed_purposes.*)
val replace_operator :
  ?default_operator:Signature.public_key_hash ->
  needed_purposes:ex_purpose list ->
  (ex_purpose * Signature.public_key_hash) list ->
  operators ->
  operators tzresult

(** For each purpose, it returns a list of associated operation
    kinds. *)
val operation_kind : ex_purpose -> Operation_kind.t list

(** For a list of operation kind, it returns a list of associated
    purpose. *)
val of_operation_kind : Operation_kind.t list -> ex_purpose list

(** [find_operator purpose operators] returns the {!operator} for the
    [purpose] from the [operators] list. The ['kind] type is to know
    if this purpose allows only one or multiple operators. *)
val find_operator : 'kind t -> operators -> 'kind operator option

(** [mem_operator operator operators] checks of [operator] is part of
    any purpose in [operators]. *)
val mem_operator : Signature.public_key_hash -> operators -> bool

(** function to bypass the private type. The opposite is not exposed
    so we don't risk corrupting the map with invalid ['kind1
    purpose_kind -> 'kind2 operator] mapping where ['kind1 <>
    'kind2] *)
val operators_bindings : operators -> (ex_purpose * ex_operator) list
