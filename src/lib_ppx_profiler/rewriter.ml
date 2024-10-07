(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module rec Constants : sig
  type t

  (** Constant representing [@profiler.aggregate] *)
  val aggregate_constant : t

  (** Constant representing [@profiler.aggregate_f] *)
  val aggregate_f_constant : t

  (** Constant representing [@profiler.aggregate_s] *)
  val aggregate_s_constant : t

  (** Constant representing [@profiler.custom] *)
  val custom_constant : t

  (** Constant representing [@profiler.mark] *)
  val mark_constant : t

  (** Constant representing [@profiler.record] *)
  val record_constant : t

  (** Constant representing [@profiler.record_f] *)
  val record_f_constant : t

  (** Constant representing [@profiler.record_s] *)
  val record_s_constant : t

  (** Constant representing [@profiler.reset_block_section] *)
  val reset_block_section_constant : t

  (** Constant representing [@profiler.span] *)
  val span_constant : t

  (** Constant representing [@profiler.span_f] *)
  val span_f_constant : t

  (** Constant representing [@profiler.span_s] *)
  val span_s_constant : t

  (** Constant representing [@profiler.stamp] *)
  val stamp_constant : t

  (** Constant representing [@profiler.stop] *)
  val stop_constant : t

  val of_rewriter : Rewriter.t -> t

  val get_attribute : t -> string

  val filter_out_all_handled_attributes :
    Parsetree.attribute list -> Parsetree.attribute list
end = struct
  (* This rewriter handles ppxes starting with profiler. *)
  let namespace = "profiler"

  type t = {attribute_name : string}

  let create_constant action =
    let attribute_name = namespace ^ "." ^ action in
    {attribute_name}

  (* [@profiler.aggregate] *)
  let aggregate_constant = create_constant "aggregate"

  (* [@profiler.aggregate_f] *)
  let aggregate_f_constant = create_constant "aggregate_f"

  (* [@profiler.aggregate_s] *)
  let aggregate_s_constant = create_constant "aggregate_s"

  (* [@profiler.custom] *)
  let custom_constant = create_constant "custom"

  (* [@profiler.mark] *)
  let mark_constant = create_constant "mark"

  (* [@profiler.record] *)
  let record_constant = create_constant "record"

  (* [@profiler.record_f] *)
  let record_f_constant = create_constant "record_f"

  (* [@profiler.record_s] *)
  let record_s_constant = create_constant "record_s"

  (* [@profiler.reset_block_section] *)
  let reset_block_section_constant = create_constant "reset_block_section"

  (* [@profiler.span] *)
  let span_constant = create_constant "span"

  (* [@profiler.span_s] *)
  let span_f_constant = create_constant "span_f"

  (* [@profiler.span_s] *)
  let span_s_constant = create_constant "span_s"

  (* [@profiler.stamp] *)
  let stamp_constant = create_constant "stamp"

  (* [@profiler.stop] *)
  let stop_constant = create_constant "stop"

  let get_attribute t = t.attribute_name

  let of_rewriter = Rewriter.to_constant

  (* This list should always be up to date by containing all the constants
     defined above *)
  let constants =
    [
      aggregate_constant;
      aggregate_f_constant;
      aggregate_s_constant;
      custom_constant;
      mark_constant;
      record_constant;
      record_f_constant;
      record_s_constant;
      reset_block_section_constant;
      span_constant;
      span_f_constant;
      span_s_constant;
      stop_constant;
    ]

  (** Attributes that are handled by this ppx *)
  let attribute_names = List.map get_attribute constants

  let has_attribute_name attribute name =
    String.equal (Ppxlib_helper.get_attribute_name attribute) name

  let filter_out_attributes attributes name =
    List.filter
      (fun attribute -> not (has_attribute_name attribute name))
      attributes

  let filter_out_all_handled_attributes attributes =
    List.fold_left
      (fun filtered_attributes handled_attribute_name ->
        filter_out_attributes filtered_attributes handled_attribute_name)
      attributes
      attribute_names
end

and Rewriter : sig
  type action =
    | Aggregate
    | Aggregate_f
    | Aggregate_s
    | Custom
    | Mark
    | Record
    | Record_f
    | Record_s
    | Reset_block_section
    | Span
    | Span_f
    | Span_s
    | Stamp
    | Stop

  type t = {key : Key.t; action : action; location : Ppxlib.location}

  val to_constant : t -> Constants.t

  val to_fully_qualified_lident_expr :
    t -> Warnings.loc -> Ppxlib.Parsetree.expression

  val get_location : t -> Ppxlib.location

  val extract_rewriters : Parsetree.attribute list -> t list
end = struct
  type action =
    | Aggregate
    | Aggregate_f
    | Aggregate_s
    | Custom
    | Mark
    | Record
    | Record_f
    | Record_s
    | Reset_block_section
    | Span
    | Span_f
    | Span_s
    | Stamp
    | Stop

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

  let custom key location =
    match Key.content key with
    | Key.Apply _ -> Custom
    | _ -> Error.error location (Error.Invalid_custom key)

  let mark key location =
    match Key.content key with
    | Key.Apply _ | Key.Ident _ | Key.List _ -> Mark
    | _ -> Error.error location (Error.Invalid_mark key)

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

  let reset_block_section key location =
    match Key.content key with
    | Key.Apply _ | Key.Ident _ | Key.String _ -> Reset_block_section
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

  let get_location {location; _} = location

  let to_constant {action; _} =
    match action with
    | Aggregate -> Constants.aggregate_constant
    | Aggregate_f -> Constants.aggregate_f_constant
    | Aggregate_s -> Constants.aggregate_s_constant
    | Custom -> Constants.custom_constant
    | Mark -> Constants.mark_constant
    | Record -> Constants.record_constant
    | Record_f -> Constants.record_f_constant
    | Record_s -> Constants.record_s_constant
    | Reset_block_section -> Constants.record_s_constant
    | Span -> Constants.span_constant
    | Span_f -> Constants.span_f_constant
    | Span_s -> Constants.span_s_constant
    | Stamp -> Constants.stamp_constant
    | Stop -> Constants.stop_constant

  module StringMap = Map.Make (String)

  (* Map associating to each constant (like "aggregate", "record" etc)
     a function that checks that the keys are consistent and, if they are,
     returns an action *)
  let association_constant_action_maker =
    [
      (Constants.aggregate_constant, aggregate);
      (Constants.aggregate_f_constant, aggregate_f);
      (Constants.aggregate_s_constant, aggregate_s);
      (Constants.custom_constant, custom);
      (Constants.mark_constant, mark);
      (Constants.record_constant, record);
      (Constants.record_f_constant, record_f);
      (Constants.record_s_constant, record_s);
      (Constants.reset_block_section_constant, reset_block_section);
      (Constants.span_constant, span);
      (Constants.span_f_constant, span_f);
      (Constants.span_s_constant, span_s);
      (Constants.stamp_constant, stamp);
      (Constants.stop_constant, stop);
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
          | Reset_block_section -> "reset_block_section"
          | Span -> "span"
          | Span_f -> "span_f"
          | Span_s -> "span_s"
          | Stamp -> "stamp"
          | Stop -> "stop"
          | Custom ->
              Stdlib.failwith
                "A custom function shouldn't be called with a leading module" )
    in
    Ppxlib.Ast_helper.Exp.ident {txt = lident; loc}

  (** [extract_list e] destruct [e] as [Some list] or returns [None] *)
  let rec extract_list = function
    | [%expr [%e? h] :: [%e? r]] -> Option.map (List.cons h) (extract_list r)
    | [%expr []] -> Some []
    | _ -> None

  let extract_content_from_structure loc structure =
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
        let fun_args = List.map snd fun_args in
        Key.Apply (fun_name, fun_args)
    | [%expr [%e? expr]] -> (
        (* [@ppx ["label1"; "label2"; ...]] *)
        match extract_list expr with
        | Some labels -> Key.List labels
        | None -> Error.error loc Error.(Malformed_attribute structure))

  let exists_field field string =
    let Ppxlib.{txt; _}, _ = field in
    txt = Ppxlib.Lident string

  let extract_field_from_record loc record string =
    match List.find (fun field -> exists_field field string) record with
    | _, Ppxlib.{pexp_desc = Pexp_construct ({txt = Lident ident; _}, None); _}
      ->
        Some ident
    | field -> Error.error loc Error.(Improper_field field)
    | exception Not_found -> None

  let extract_from_record loc record =
    let level_of_detail =
      extract_field_from_record loc record "level_of_detail"
    in
    let profiler_module =
      extract_field_from_record loc record "profiler_module"
    in
    (level_of_detail, profiler_module)

  let extract_key_from_payload loc payload =
    match payload with
    | Ppxlib.PStr
        [
          [%stri
            [%e?
              {
                pexp_desc =
                  Pexp_apply
                    ( {pexp_desc = Pexp_record (record, _); _},
                      [(Nolabel, structure)] );
                _;
              }]];
        ] ->
        (* [@ppx {<other infos>} ...] *)
        let level_of_detail, profiler_module = extract_from_record loc record in
        (match (level_of_detail, profiler_module) with
        | None, None -> Error.error loc Error.(Improper_record record)
        | _ -> ()) ;
        Key.
          {
            level_of_detail;
            profiler_module;
            content = extract_content_from_structure loc structure;
          }
    | Ppxlib.PStr [[%stri [%e? structure]]] ->
        (* [@ppx ...] *)
        Key.
          {
            level_of_detail = None;
            profiler_module = None;
            content = extract_content_from_structure loc structure;
          }
    | Ppxlib.PStr [] ->
        (* [@ppx] *)
        Key.
          {level_of_detail = None; profiler_module = None; content = Key.Empty}
    | _ -> Error.error loc (Invalid_payload payload)

  let of_attribute ({Ppxlib.attr_payload; attr_loc; _} as attribute) =
    match
      Ppxlib_helper.get_attribute_name attribute |> get_action_maker attr_loc
    with
    | Some action_maker ->
        let key = extract_key_from_payload attr_loc attr_payload in
        let action = action_maker key attr_loc in
        Some {key; location = attr_loc; action}
    | None -> None

  let extract_rewriters = List.filter_map of_attribute
end
