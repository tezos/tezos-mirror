(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module Constants = Rewriter.Constants
module Rewriter = Rewriter.Rewriter

(** [with_metadata ~loc e metadata] turns [e] into a tuple [(e, metadata)],
    using empty list as metadata if none is provided.
    This is used by ppx in order to transform both [ {metadata} expr ] and
    [expr] into [(expr, metadata)], which is the type expected by profiling
    functions from {!Profiler} module. *)
let with_metadata ~loc e metadata =
  let metadata = Option.value ~default:[%expr []] metadata in
  Ppxlib.Ast_builder.Default.pexp_tuple ~loc [e; metadata]

(** [add_wrapping_function expr name _ key] will create
      {[
        Profiler.name
          ~verbosity:(Notice | Info | Debug)
          (key, metadata)
          (fun () -> expr)
      ]}
  *)
let add_wrapping_function expr fun_name loc key =
  let key_expr =
    with_metadata ~loc [%expr [%e Key.to_expression loc key]] key.Key.metadata
  in
  [%expr
    [%e fun_name] ~verbosity:[%e Key.get_level_of_detail loc key] [%e key_expr]
    @@ fun () -> [%e expr]]

(** [add_unit_function ~metadata ~verbosity expr name _ key] will create different
    code snippet depending on the options. The general shape is:

      {[
        Profiler.name VERBOSITY KEY;
        expr
      ]}

    [VERBOSITY] being present as [~verbosity:(Notice | Info | Debug)] only
    if [~verbosity] as been set to true.
    [KEY] being [(key, metadata)] if [~metadata] is set to true (which is
    default) and just [key] otherwise.

    Typically, [Profiler.stop ()] does not take metadata nor verbosity as argument,
    while [Profiler.stamp ?verbosity ("key", [])] does need metadata and can take
    an optionnal argument [verbosity]. Both of these constructions use the same  *)
let add_unit_function ?(metadata = true) ~verbosity expr fun_name loc key =
  let key_expr =
    let e = [%expr [%e Key.to_expression loc key]] in
    if metadata then with_metadata ~loc e key.Key.metadata else e
  in
  match verbosity with
  | false ->
      [%expr
        [%e fun_name] [%e key_expr] ;
        [%e expr]]
  | true ->
      [%expr
        [%e fun_name]
          ~verbosity:[%e Key.get_level_of_detail loc key]
          [%e key_expr] ;
        [%e expr]]

(** [add_custom_function_apply _ key] will create
      {[
        key
      ]}

    [key] is a function application *)
let add_custom_function_apply loc key = [%expr [%e Key.to_expression loc key]]

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
      (* Functions that have a ~verbosity parameter *)
      | Rewriter.Mark | Rewriter.Record | Rewriter.Stamp ->
          add_unit_function
            ~verbosity:true
            expr
            (Rewriter.to_fully_qualified_lident_expr rewriter loc)
            loc
            rewriter.key
      (* Functions that don't have a ~verbosity parameter *)
      | Rewriter.Reset_block_section ->
          add_unit_function
            ~verbosity:false
            expr
            (Rewriter.to_fully_qualified_lident_expr rewriter loc)
            loc
            rewriter.key
      (* Functions that don't have a ~verbosity nor ~metadata parameter *)
      | Rewriter.Stop ->
          add_unit_function
            ~metadata:false
            ~verbosity:false
            expr
            (Rewriter.to_fully_qualified_lident_expr rewriter loc)
            loc
            rewriter.key
          (* Custom functions *)
      | Rewriter.Custom -> add_custom_function_apply loc rewriter.key)
    t
    rewriters

let remove_attributes t =
  {
    t with
    Ppxlib.pexp_attributes =
      Constants.filter_out_all_handled_attributes t.Ppxlib.pexp_attributes;
  }
