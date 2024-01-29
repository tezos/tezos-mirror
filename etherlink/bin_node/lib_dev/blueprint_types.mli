(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** A chunked blueprint, ready to be executed localy. *)
type t = [`External of string] list

val encoding : t Data_encoding.t
