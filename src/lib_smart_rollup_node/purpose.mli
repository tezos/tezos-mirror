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

(** [to_string p] returns a string representation of purpose [p]. *)
val to_string : t -> string

(** [of_string s] parses a purpose from the given string [s]. *)
val of_string : string -> t option

(** [of_string s] parses a purpose from the given string [s]. *)
val of_string_exn : string -> t

val operators_encoding : operators Data_encoding.t

(** [make_map ?default purposes] constructs a purpose map from a list
    of bindings [purposes], with a potential [default] value. *)
val make_map : ?default:'a -> (t * 'a) list -> 'a Map.t

(** For each purpose, it returns a list of associated operation
    kinds. *)
val operation_kind : t -> Operation_kind.t list

(** For a list of operation kind, it returns a list of associated
    purpose. *)
val of_operation_kind : Operation_kind.t list -> t list
