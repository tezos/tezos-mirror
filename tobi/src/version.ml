(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type t = Dev | Old of string

let parse = function "dev" -> Dev | v -> Old v

let show = function Dev -> "dev" | Old v -> v

let equal = (( = ) : t -> t -> bool)

let head = Old "HEAD"
