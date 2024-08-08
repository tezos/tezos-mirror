(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

val get_attribute_name : Ppxlib.attribute -> string

val get_loc : Ppxlib.expression -> Ppxlib.location
