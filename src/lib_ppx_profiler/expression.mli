(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** [rewrite rewriters expr] sequentially interpretes the given [rewriters] in
    order to rewrite the given expression [expr]. *)
val rewrite : Rewriter.t list -> Ppxlib.expression -> Ppxlib_ast.Ast.expression

(** [remove_attributes expr] returns [expr] where the attributes handled
    by this ppx have been removed *)
val remove_attributes : Ppxlib.expression -> Ppxlib.expression
