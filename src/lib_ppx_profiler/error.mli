(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type error =
  | Invalid_action of string
  | Invalid_payload of Parsetree.payload
  | Invalid_aggregate of Key.t
  | Invalid_mark of Key.t
  | Invalid_record of Key.t
  | Invalid_span of Key.t
  | Invalid_stop of Key.t
  | Improper_field of (Longident.t Location.loc * Ppxlib.expression)
  | Improper_let_binding of Ppxlib.expression
  | Improper_record of (Ppxlib.Ast.longident_loc * Parsetree.expression) list
  | Malformed_attribute of Ppxlib.expression

(** Raise a located error *)
val error : Location.t -> error -> 'a
