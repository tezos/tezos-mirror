(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(*****************************************************************************)

(** The kind of operations that can be injected by the rollup node. *)
type t =
  | Publish
  | Add_messages
  | Cement
  | Timeout
  | Refute
  | Recover
  | Execute_outbox_message

(** List of possible operations kind for operator specialization. *)
val all : t list

module Map : Map.S with type key = t

type fee_parameters = Injector_common.fee_parameter Map.t

(** [to_string o] returns a string representation of operation_kind
    [o]. *)
val to_string : t -> string

(** [of_string s] parses an operation kind from the given string
    [s]. *)
val of_string : string -> t option

(** [of_string_exn s] parses an operation kind from the given string
    [s]. *)
val of_string_exn : string -> t

val encoding : t Data_encoding.t

val fee_parameters_encoding :
  default_fee_parameter:(t -> Injector_common.fee_parameter) ->
  Injector_common.fee_parameter Map.t Data_encoding.t
