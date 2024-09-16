(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type error =
  | Invalid_payload of Parsetree.payload
  | Invalid_aggregate of Key.t
  | Invalid_mark of Key.t
  | Invalid_record of Key.t
  | Invalid_span of Key.t
  | Invalid_stop of Key.t
  | Improper_let_binding of Ppxlib.expression
  | Malformed_attribute of Ppxlib.expression

(** Raise a located error *)
val error : Location.t -> error -> 'a
