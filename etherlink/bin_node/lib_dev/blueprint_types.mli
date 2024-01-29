(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** A chunked blueprint, ready to be executed localy. *)
type payload = [`External of string] list

val payload_encoding : payload Data_encoding.t
