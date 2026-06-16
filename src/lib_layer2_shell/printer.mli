(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type t

val lterm_printer : LTerm.t -> t

val format_printer : Format.formatter -> t

val ln : t -> ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a

val errorln : t -> ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a
