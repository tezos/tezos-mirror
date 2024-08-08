(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type t =
  | Empty
  | Ident of string
  | String of string
  | List of Ppxlib.expression list
  | Apply of Ppxlib.expression * Parsetree.expression list
  | Other of Ppxlib.expression

val to_expression : Ppxlib.Location.t -> t -> Ppxlib.expression

val pp : Format.formatter -> t -> unit
