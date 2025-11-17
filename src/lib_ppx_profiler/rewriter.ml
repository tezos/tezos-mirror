(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type action =
  | Aggregate
  | Aggregate_f
  | Aggregate_s
  | Mark
  | Overwrite
  | Record
  | Record_f
  | Record_s
  | Span
  | Span_f
  | Span_s
  | Stamp
  | Stop
  | Wrap_f
  | Wrap_s

type t = {key : Key.t; action : action; location : Ppxlib.location}

let aggregate key location =
  match Key.content key with
  | Key.Apply _ | Key.Ident _ | Key.String _ -> Aggregate
  | _ -> Error.error location (Error.Invalid_aggregate key)

let aggregate_f key location =
  match Key.content key with
  | Key.Apply _ | Key.Ident _ | Key.String _ -> Aggregate_f
  | _ -> Error.error location (Error.Invalid_aggregate key)

let aggregate_s key location =
  match Key.content key with
  | Key.Apply _ | Key.Ident _ | Key.String _ -> Aggregate_s
  | _ -> Error.error location (Error.Invalid_aggregate key)

let mark key location =
  match Key.content key with
  | Key.Apply _ | Key.Ident _ | Key.List _ -> Mark
  | _ -> Error.error location (Error.Invalid_mark key)

let overwrite _key _location = Overwrite

let record key location =
  match Key.content key with
  | Key.Apply _ | Key.Ident _ | Key.String _ -> Record
  | _ -> Error.error location (Error.Invalid_record key)

let record_f key location =
  match Key.content key with
  | Key.Apply _ | Key.Ident _ | Key.String _ -> Record_f
  | _ -> Error.error location (Error.Invalid_record key)

let record_s key location =
  match Key.content key with
  | Key.Apply _ | Key.Ident _ | Key.String _ -> Record_s
  | _ -> Error.error location (Error.Invalid_record key)

let span key location =
  match Key.content key with
  | Key.Apply _ | Key.Ident _ | Key.List _ -> Span
  | _ -> Error.error location (Error.Invalid_span key)

let span_f key location =
  match Key.content key with
  | Key.Apply _ | Key.Ident _ | Key.List _ -> Span_f
  | _ -> Error.error location (Error.Invalid_span key)

let span_s key location =
  match Key.content key with
  | Key.Apply _ | Key.Ident _ | Key.List _ -> Span_s
  | _ -> Error.error location (Error.Invalid_span key)

let stamp key location =
  match Key.content key with
  | Key.Apply _ | Key.Ident _ | Key.String _ -> Stamp
  | _ -> Error.error location (Error.Invalid_span key)

let stop key location =
  match Key.content key with
  | Key.Empty -> Stop
  | _ -> Error.error location (Error.Invalid_stop key)

let wrap_f key location =
  match Key.content key with
  | Key.Apply _ | Key.Ident _ -> Wrap_f
  | _ -> Error.error location (Error.Invalid_wrap key)

let wrap_s key location =
  match Key.content key with
  | Key.Apply _ | Key.Ident _ -> Wrap_s
  | _ -> Error.error location (Error.Invalid_wrap key)

let get_location {location; _} = location

let to_constant {action; _} =
  match action with
  | Aggregate -> Constants.aggregate_constant
  | Aggregate_f -> Constants.aggregate_f_constant
  | Aggregate_s -> Constants.aggregate_s_constant
  | Mark -> Constants.mark_constant
  | Overwrite -> Constants.overwrite_constant
  | Record -> Constants.record_constant
  | Record_f -> Constants.record_f_constant
  | Record_s -> Constants.record_s_constant
  | Span -> Constants.span_constant
  | Span_f -> Constants.span_f_constant
  | Span_s -> Constants.span_s_constant
  | Stamp -> Constants.stamp_constant
  | Stop -> Constants.stop_constant
  | Wrap_f -> Constants.wrap_f_constant
  | Wrap_s -> Constants.wrap_s_constant

module StringMap = Map.Make (String)

(* Map associating to each constant (like "aggregate", "record" etc)
   a function that checks that the keys are consistent and, if they are,
   returns an action *)
let association_constant_action_maker =
  [
    (Constants.aggregate_constant, aggregate);
    (Constants.aggregate_f_constant, aggregate_f);
    (Constants.aggregate_s_constant, aggregate_s);
    (Constants.mark_constant, mark);
    (Constants.overwrite_constant, overwrite);
    (Constants.record_constant, record);
    (Constants.record_f_constant, record_f);
    (Constants.record_s_constant, record_s);
    (Constants.span_constant, span);
    (Constants.span_f_constant, span_f);
    (Constants.span_s_constant, span_s);
    (Constants.stamp_constant, stamp);
    (Constants.stop_constant, stop);
    (Constants.wrap_f_constant, wrap_f);
    (Constants.wrap_s_constant, wrap_s);
  ]
  |> List.map (fun (const, fn) -> (Constants.get_attribute const, fn))
  |> List.to_seq |> StringMap.of_seq

let get_action_maker loc attribute =
  match StringMap.find_opt attribute association_constant_action_maker with
  | Some res -> Some res
  | None ->
      (* Raise an Error if the ppx starts with [@profiler.action ...]
         but action is not handled by this ppx *)
      if String.starts_with ~prefix:"profiler." attribute then
        match String.split_on_char '.' attribute with
        | [_; action] -> Error.error loc Error.(Invalid_action action)
        | _ -> None
      else None

(** Transforms a rewriter in an OCaml function call:
    - [@profiler.aggregate_s ...] will create a proper
        Parsetree Lident representing Profiler.aggregate_s *)
let to_fully_qualified_lident_expr t loc =
  let lident =
    Ppxlib.Ldot
      ( Key.get_profiler_module t.key,
        match t.action with
        | Aggregate -> "aggregate"
        | Aggregate_f -> "aggregate_f"
        | Aggregate_s -> "aggregate_s"
        | Mark -> "mark"
        | Record -> "record"
        | Record_f -> "record_f"
        | Record_s -> "record_s"
        | Span -> "span"
        | Span_f -> "span_f"
        | Span_s -> "span_s"
        | Stamp -> "stamp"
        | Stop -> "stop"
        | Overwrite | Wrap_f | Wrap_s ->
            Stdlib.failwith
              "An overwrite or wrap section shouldn't be called with a leading \
               module" )
  in
  Ppxlib.Ast_helper.Exp.ident {txt = lident; loc}

(** [extract_list e] destruct [e] as [Some list] or returns [None] *)
let rec extract_list = function
  | [%expr [%e? h] :: [%e? r]] -> Option.map (List.cons h) (extract_list r)
  | [%expr []] -> Some []
  | _ -> None

let extract_content_from_structure structure =
  match structure with
  | [%expr []] ->
      (* [@ppx] *)
      Key.Empty
  | [%expr [%e? {pexp_desc = Pexp_constant (Pconst_string (label, _, _)); _}]]
    ->
      (* [@ppx "label"] *)
      Key.String label
  | [%expr [%e? {pexp_desc = Pexp_ident {txt = Lident ident; _}; _}]] ->
      (* [@ppx ident] *)
      Key.Ident ident
  | [%expr [%e? {pexp_desc = Pexp_apply (fun_name, fun_args); _}]] ->
      (* [@ppx fun_name fun_arg1 fun_arg2 ...] *)
      Key.Apply (fun_name, fun_args)
  | [%expr [%e? expr]] -> (
      (* [@ppx ["label1"; "label2"; ...]] *)
      match extract_list expr with
      | Some labels -> Key.List labels
      | None -> Key.Other expr)

let exists_field field string =
  let Ppxlib.{txt; _}, _ = field in
  txt = Ppxlib.Lident string

let extract_field_from_record record string =
  match List.find (fun field -> exists_field field string) record with
  | field -> Some field
  | exception Not_found -> None

(** [extract_list_from_record _ record field] checks that [field] exists
      and that the associated value is a list.
      If [field] is not present, returns [None] *)
let extract_list_from_record loc record string =
  Option.bind (extract_field_from_record record string) @@ function
  | (_, [%expr [%e? expr]]) as field -> (
      match extract_list expr with
      | Some labels -> Some labels
      | None ->
          (* This branch corresponds to the fact that the value
             associated to the field is not a list, which is an error *)
          Error.error loc Error.(Improper_list_field field))

(** [extract_enum_from_record _ record field] checks that [field] exists
      and that the associated value is an enum.
      If [field] is not present, returns [None] *)
let extract_enum_from_record loc record string =
  Option.bind (extract_field_from_record record string) @@ function
  | _, Ppxlib.{pexp_desc = Pexp_construct ({txt = Lident ident; _}, None); _} ->
      Some ident
  | field -> Error.error loc Error.(Improper_field field)

(** [extract_bool_from_record _ record field] checks that [field] exists
      and that the associated value is a boolean.
      If [field] is not present, returns [None] *)
let extract_bool_from_record loc record string =
  Option.bind (extract_field_from_record record string) @@ function
  | (_, Ppxlib.{pexp_desc = Pexp_construct ({txt = Lident ident; _}, None); _})
    as field -> (
      match bool_of_string ident with
      | b -> Some b
      | exception Invalid_argument _ ->
          Error.error loc Error.(Invalid_type ("bool", string, field)))
  | field -> Error.error loc Error.(Invalid_type ("bool", string, field))

let extract_from_record loc record =
  let verbosity = extract_enum_from_record loc record "verbosity" in
  let cpu_profiling = extract_bool_from_record loc record "cpu_profiling" in
  let profiler_module = extract_enum_from_record loc record "profiler_module" in
  let metadata =
    (* Metadata can be any valid expression so we return the
       expression associated to it as is *)
    Option.map snd (extract_field_from_record record "metadata")
  in
  let driver_ids =
    (* Transforms a list of constructs (that all have an associated
       labels) into a list of DriverKind.t *)
    (match extract_list_from_record loc record "driver_ids" with
    | None -> []
    | Some expr_list ->
        List.map
          (function
            | Ppxlib.
                {
                  pexp_desc =
                    ( Pexp_construct ({txt = Lident ident; _}, None)
                    | Pexp_ident {txt = Lident ident; _} );
                  _;
                } ->
                Handled_drivers.Driver_kind.of_string ident
            | _ -> Error.error loc Error.(Invalid_list_of_driver_ids expr_list))
          expr_list)
    |> Handled_drivers.of_list
  in
  (verbosity, cpu_profiling, profiler_module, metadata, driver_ids)

let extract_key_from_payload loc payload =
  match payload with
  | Ppxlib.PStr
      [
        [%stri
          [%e?
            {
              pexp_desc =
                Pexp_apply ({pexp_desc = Pexp_record (record, _); _}, item_list);
              _;
            }]];
      ] -> (
      match item_list with
      | [(Nolabel, structure)] ->
          (* [@ppx {<other infos>} ...] *)
          let verbosity, cpu_profiling, profiler_module, metadata, driver_ids =
            extract_from_record loc record
          in
          (match (verbosity, profiler_module, metadata) with
          | None, None, None when Handled_drivers.is_empty driver_ids ->
              Error.error loc Error.(Improper_record record)
          | _ -> ()) ;
          Key.
            {
              verbosity;
              cpu_profiling;
              profiler_module;
              metadata;
              driver_ids;
              content = extract_content_from_structure structure;
            }
      | _ -> Error.error loc (Invalid_payload (false, payload)))
  | Ppxlib.PStr [[%stri [%e? structure]]] ->
      (* [@ppx ...] *)
      Key.
        {
          verbosity = None;
          cpu_profiling = None;
          profiler_module = None;
          metadata = None;
          driver_ids = Handled_drivers.empty;
          content = extract_content_from_structure structure;
        }
  | Ppxlib.PStr [] ->
      (* [@ppx] *)
      Key.
        {
          verbosity = None;
          cpu_profiling = None;
          profiler_module = None;
          metadata = None;
          driver_ids = Handled_drivers.empty;
          content = Key.Empty;
        }
  | _ -> Error.error loc (Invalid_payload (true, payload))

let of_attribute handled_drivers
    ({Ppxlib.attr_payload; attr_loc; _} as attribute) =
  match
    Ppxlib_helper.get_attribute_name attribute |> get_action_maker attr_loc
  with
  | Some action_maker ->
      let key = extract_key_from_payload attr_loc attr_payload in
      if
        (* Preprocess this attribute if:
           - the driver_ids was not provided
           - one of the driver_ids provided is enabled by `TEZOS_PPX_PROFILER`
        *)
        Handled_drivers.is_empty key.driver_ids
        || Handled_drivers.exists
             (fun driver_id -> Handled_drivers.mem driver_id handled_drivers)
             key.driver_ids
      then
        let action = action_maker key attr_loc in
        Some {key; location = attr_loc; action}
      else None
  | None -> None

let extract_rewriters handled_drivers =
  List.filter_map (of_attribute handled_drivers)
