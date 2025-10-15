(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs, <contact@nomadic-labs.com>     *)
(*                                                                           *)
(*****************************************************************************)

(** Internal representation of attestation power. *)

type t = {slots : int; stake : int64}

val encoding : t Data_encoding.t

val pp : Format.formatter -> t -> unit

val zero : t

val make : slots:int -> stake:int64 -> t

val add : t -> t -> t
