(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2021-2024 Nomadic Labs <contact@nomadic-labs.com> *)
(* SPDX-FileCopyrightText: 2022-2024 TriliTech <contact@trili.tech>          *)
(* SPDX-FileCopyrightText: 2023-2024 Marigold <contact@marigold.dev>         *)
(* SPDX-FileCopyrightText: 2023-2024 Functori <contact@functori.com>         *)
(*                                                                           *)
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
  | Publish_dal_commitment

(** List of possible operations kind for operator specialization. *)
val all : t list

module Map : Map.S with type key = t

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

val map_encoding : (t -> 'value Data_encoding.t) -> 'value Map.t Data_encoding.t

(** [compare_priority] Comparison over tag for order of
    importance. Order is given by the list {!all}. it's the following:

   Timeout < Refute < [Publish; Publish_dal_commitment; Cement] <
   Recover < Add_messages < Execute_outbox_message.
*)
val compare_priority : t -> t -> int
