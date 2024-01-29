(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** A chunked blueprint, ready to be executed localy. *)
type payload = [`External of string] list

type t = {number : Ethereum_types.quantity; payload : payload}

val encoding : t Data_encoding.t

val payload_encoding : payload Data_encoding.t
