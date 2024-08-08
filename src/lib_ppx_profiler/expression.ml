(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module Constants = Rewriter.Constants
module Rewriter = Rewriter.Rewriter

(** [add_wrapping_function expr name _ key] will create
      {[
        Profiler.name key @@ fun () -> (expr)
      ]}
  *)
let add_wrapping_function expr fun_name loc key =
  [%expr [%e fun_name] [%e Key.to_expression loc key] @@ fun () -> [%e expr]]

(** [add_unit_function expr name _ key] will create
      {[
        Profiler.name key;
        expr
      ]}
  *)
let add_unit_function expr fun_name loc key =
  [%expr
    [%e fun_name] [%e Key.to_expression loc key] ;
    [%e expr]]

let rewrite rewriters t =
  let loc = Ppxlib_helper.get_loc t in
  List.fold_left
    (fun expr rewriter ->
      match rewriter with
      | Rewriter.Aggregate_s content
      | Rewriter.Aggregate_f content
      | Rewriter.Record_f content
      | Rewriter.Record_s content
      | Rewriter.Span_s content ->
          add_wrapping_function
            expr
            (Rewriter.to_fully_qualified_lident_expr rewriter loc)
            loc
            (Rewriter.get_key content)
      | Rewriter.Mark content
      | Rewriter.Record content
      | Rewriter.Reset_block_section content
      | Rewriter.Stop content ->
          add_unit_function
            expr
            (Rewriter.to_fully_qualified_lident_expr rewriter loc)
            loc
            (Rewriter.get_key content))
    t
    rewriters

let remove_attributes t =
  {
    t with
    Ppxlib.pexp_attributes =
      Constants.filter_out_all_handled_attributes t.Ppxlib.pexp_attributes;
  }
