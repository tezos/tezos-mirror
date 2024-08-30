(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

let get_attribute_name attribute = attribute.Ppxlib.attr_name.txt

let get_loc expr = expr.Ppxlib.pexp_loc
