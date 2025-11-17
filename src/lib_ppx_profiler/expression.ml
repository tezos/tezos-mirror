(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** [with_metadata ~loc e metadata] turns [e] into a tuple [(e, metadata)],
    using empty list as metadata if none is provided.
    This is used by ppx in order to transform both [ {metadata} expr ] and
    [expr] into [(expr, metadata)], which is the type expected by profiling
    functions from {!Profiler} module. *)
let with_metadata ~loc e metadata =
  let metadata = Option.value ~default:[%expr []] metadata in
  Ppxlib.Ast_builder.Default.pexp_tuple ~loc [e; metadata]

(** [get_verbosity _ key] will return the verbosity associated
    to key or raise an error otherwise *)
let get_verbosity loc key =
  match Key.get_verbosity loc key with
  | Some lod -> lod
  | None -> Error.error loc (No_verbosity key)

(** [get_cpu _ key] will return the cpu_profiling associated to key *)
let get_cpu loc key =
  match key.Key.cpu_profiling with
  | Some true -> [%expr Some true]
  | Some false -> [%expr Some false]
  | None -> [%expr None]

(** [add_wrapping_function expr name _ key] will create
      {[
        Profiler.name
          (Notice | Info | Debug)
          (key, metadata)
          (fun () -> expr)
      ]}
*)
let add_wrapping_function expr fun_name loc key =
  let key_expr =
    with_metadata ~loc [%expr [%e Key.to_expression loc key]] key.Key.metadata
  in
  [%expr
    [%e fun_name]
      ~cpu:[%e get_cpu loc key]
      [%e get_verbosity loc key]
      [%e key_expr]
    @@ fun () -> [%e expr]]

(** [add_unit_function_cpu ~metadata ~verbosity expr name _ key] will create different
    code snippet depending on the options. The general shape is:

      {[
        Profiler.name ~cpu:CPU VERBOSITY KEY;
        expr
      ]}

    [VERBOSITY] being present as [Notice | Info | Debug] only if [~verbosity] has
    been set to [true].
    [CPU] is [true] (default) if [Sys.time] should be computed
    [KEY] being [(key, metadata)] if [~metadata] is set to [true] (which is
    default) or just [key] otherwise.

    Typically, [Profiler.stop ()] does not take metadata nor verbosity as argument,
    while [Profiler.stamp verbosity ("key", [])] does need [metadata] and [verbosity]. *)
let add_unit_function_cpu ?(metadata = true) ~verbosity expr fun_name loc key =
  let key_expr =
    let e = [%expr [%e Key.to_expression loc key]] in
    if metadata then with_metadata ~loc e key.Key.metadata else e
  in
  match verbosity with
  | false ->
      [%expr
        [%e fun_name] ~cpu:[%e get_cpu loc key] [%e key_expr] ;
        [%e expr]]
  | true ->
      [%expr
        [%e fun_name]
          ~cpu:[%e get_cpu loc key]
          [%e get_verbosity loc key]
          [%e key_expr] ;
        [%e expr]]

(** [add_unit_function_no_cpu ~metadata expr name _ key] will create different
    code snippet depending on the options. The general shape is:

      {[
        Profiler.name KEY;
        expr
      ]}

    [KEY] being [(key, metadata)] if [~metadata] is set to [true] (which is
    default) or just [key] otherwise. *)
let add_unit_function_no_cpu ?(verbosity = true) ?(metadata = true) expr
    fun_name loc key =
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
        [%e fun_name] [%e get_verbosity loc key] [%e key_expr] ;
        [%e expr]]

(** [replace_expr_with _ key] will create
      {[
        key
      ]}

    [key] can be anything since this will replace the preprocessed expression *)
let replace_expr_with loc key = [%expr [%e Key.to_expression loc key]]

(** [add_wrapping_custom_function expr _ key] will create
      {[
        key (fun () -> expr)
      ]}

    [key] is a partial function application *)
let add_wrapping_custom_function expr loc key =
  [%expr [%e Key.to_expression loc key] @@ fun () -> [%e expr]]

let rewrite rewriters t =
  let loc = Ppxlib_helper.get_loc t in
  List.fold_left
    (fun expr rewriter ->
      match rewriter.Rewriter.action with
      | Rewriter.Aggregate_f | Rewriter.Aggregate_s | Rewriter.Record_f
      | Rewriter.Record_s | Rewriter.Span_f | Rewriter.Span_s ->
          add_wrapping_function
            expr
            (Rewriter.to_fully_qualified_lident_expr rewriter loc)
            loc
            rewriter.key
      (* Functions that have a ~verbosity parameter *)
      | Rewriter.Aggregate | Rewriter.Record | Rewriter.Span | Rewriter.Stamp ->
          add_unit_function_cpu
            ~verbosity:true
            expr
            (Rewriter.to_fully_qualified_lident_expr rewriter loc)
            loc
            rewriter.key
      | Rewriter.Mark ->
          add_unit_function_no_cpu
            expr
            (Rewriter.to_fully_qualified_lident_expr rewriter loc)
            loc
            rewriter.key
      (* Functions that don't have a ~verbosity nor ~metadata parameter *)
      | Rewriter.Stop ->
          add_unit_function_no_cpu
            ~verbosity:false
            ~metadata:false
            expr
            (Rewriter.to_fully_qualified_lident_expr rewriter loc)
            loc
            rewriter.key
          (* Custom functions *)
      | Rewriter.Overwrite -> replace_expr_with loc rewriter.key
      | Rewriter.Wrap_f | Rewriter.Wrap_s ->
          add_wrapping_custom_function expr loc rewriter.key)
    t
    rewriters

let remove_attributes t =
  {
    t with
    Ppxlib.pexp_attributes =
      Constants.filter_out_all_handled_attributes t.Ppxlib.pexp_attributes;
  }
