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
        Profiler.name ~lod:(Terse | Detailed | Verbose) key @@ fun () -> (expr)
      ]}
  *)
let add_wrapping_function expr fun_name loc key =
  [%expr
    [%e fun_name]
      ~lod:[%e Key.get_level_of_detail loc key]
      [%e Key.to_expression loc key]
    @@ fun () -> [%e expr]]

(** [add_unit_function ~lod expr name _ key] will create
      {[
        Profiler.name (if lod then ~lod:(Terse | Detailed | Verbose)) key;
        expr
      ]}
  *)
let add_unit_function ~lod expr fun_name loc key =
  match lod with
  | false ->
      [%expr
        [%e fun_name] [%e Key.to_expression loc key] ;
        [%e expr]]
  | true ->
      [%expr
        [%e fun_name]
          ~lod:[%e Key.get_level_of_detail loc key]
          [%e Key.to_expression loc key] ;
        [%e expr]]

let rewrite rewriters t =
  let loc = Ppxlib_helper.get_loc t in
  List.fold_left
    (fun expr rewriter ->
      match rewriter.Rewriter.action with
      | Rewriter.Aggregate | Rewriter.Aggregate_f | Rewriter.Aggregate_s
      | Rewriter.Record_f | Rewriter.Record_s | Rewriter.Span | Rewriter.Span_f
      | Rewriter.Span_s ->
          add_wrapping_function
            expr
            (Rewriter.to_fully_qualified_lident_expr rewriter loc)
            loc
            rewriter.key
      (* Functions that have a ~lod parameter *)
      | Rewriter.Mark | Rewriter.Record | Rewriter.Stamp ->
          add_unit_function
            ~lod:true
            expr
            (Rewriter.to_fully_qualified_lident_expr rewriter loc)
            loc
            rewriter.key
      (* Functions that don't have a ~lod parameter *)
      | Rewriter.Reset_block_section | Rewriter.Stop ->
          add_unit_function
            ~lod:false
            expr
            (Rewriter.to_fully_qualified_lident_expr rewriter loc)
            loc
            rewriter.key)
    t
    rewriters

let remove_attributes t =
  {
    t with
    Ppxlib.pexp_attributes =
      Constants.filter_out_all_handled_attributes t.Ppxlib.pexp_attributes;
  }
