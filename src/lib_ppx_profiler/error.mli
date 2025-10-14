(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type error =
  | Invalid_action of string
  | Invalid_payload of bool * Ppxlib.payload
  (* The [bool] represents the fact that the record is missing or not to have
     a better message display when encountering an invalid payload *)
  | Invalid_aggregate of Key.t
  | Invalid_mark of Key.t
  | Invalid_record of Key.t
  | Invalid_span of Key.t
  | Invalid_stop of Key.t
  | Invalid_type of string * string * (Ppxlib.longident_loc * Ppxlib.expression)
  | Invalid_wrap of Key.t
  | Invalid_list_of_driver_ids of Ppxlib.expression list
  | Improper_field of (Ppxlib.longident_loc * Ppxlib.expression)
  | Improper_list_field of (Ppxlib.longident_loc * Ppxlib.expression)
  | Improper_let_binding of Ppxlib.expression
  | Improper_record of (Ppxlib.longident_loc * Ppxlib.expression) list
  | Malformed_attribute of Ppxlib.expression
  | No_verbosity of Key.t

(** Raise a located error *)
val error : Location.t -> error -> 'a
