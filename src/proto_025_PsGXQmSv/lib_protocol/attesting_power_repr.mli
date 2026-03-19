(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs, <contact@nomadic-labs.com>     *)
(*                                                                           *)
(*****************************************************************************)

(** Internal representation of attestation power. *)

type t = {slots : int; baking_power : int64}

(* Only for operation receipts. See [apply_results.ml]. *)
type result

val encoding : t Data_encoding.t

val op_result_encoding : result Data_encoding.t

val to_result : abaab_activated:bool -> t -> result

val pp : Format.formatter -> t -> unit

val pp_result : Format.formatter -> result -> unit

val zero : t

val make : slots:int -> baking_power:int64 -> t

val add : t -> t -> t

val get_slots_from_result : result -> int

module Internal_for_tests : sig
  val get_from_result : result -> int64
end
