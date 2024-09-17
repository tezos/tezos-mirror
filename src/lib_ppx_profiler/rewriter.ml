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

  (** Constant representing [@profiler.stop] *)
  val stop_constant : t

  val of_rewriter : Rewriter.t -> t

  val get_attribute : t -> string

  val filter_out_all_handled_attributes :
    Parsetree.attribute list -> Parsetree.attribute list
end = struct
  (* This rewriter handles ppxes starting with profiler. *)
  let namespace = "profiler"

  type t = {action : string; attribute_name : string}

  let create_constant action =
    let attribute_name = namespace ^ "." ^ action in
    {action; attribute_name}

  (* [@profiler.aggregate] *)
  let aggregate_constant = create_constant "aggregate"

  (* [@profiler.aggregate_f] *)
  let aggregate_f_constant = create_constant "aggregate_f"

  (* [@profiler.aggregate_s] *)
  let aggregate_s_constant = create_constant "aggregate_s"

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
  type content = {key : Key.t; location : Ppxlib.location}

  val get_key : content -> Key.t

  type t =
    | Aggregate of content
    | Aggregate_f of content
    | Aggregate_s of content
    | Mark of content
    | Record of content
    | Record_f of content
    | Record_s of content
    | Reset_block_section of content
    | Span of content
    | Span_f of content
    | Span_s of content
    | Stop of content

  val to_constant : t -> Constants.t

  val to_fully_qualified_lident_expr :
    t -> Warnings.loc -> Ppxlib.Parsetree.expression

  val get_location : t -> Ppxlib.location

  val extract_rewriters : Parsetree.attribute list -> t list
end = struct
  type content = {key : Key.t; location : Ppxlib.location}

  let get_key content = content.key

  type t =
    | Aggregate of content
    | Aggregate_f of content
    | Aggregate_s of content
    | Mark of content
    | Record of content
    | Record_f of content
    | Record_s of content
    | Reset_block_section of content
    | Span of content
    | Span_f of content
    | Span_s of content
    | Stop of content

  let aggregate key location =
    match Key.content key with
    | Key.Apply _ | Key.Ident _ | Key.String _ -> Aggregate {key; location}
    | _ -> Error.error location (Error.Invalid_aggregate key)

  let aggregate_f key location =
    match Key.content key with
    | Key.Apply _ | Key.Ident _ | Key.String _ -> Aggregate_f {key; location}
    | _ -> Error.error location (Error.Invalid_aggregate key)

  let aggregate_s key location =
    match Key.content key with
    | Key.Apply _ | Key.Ident _ | Key.String _ -> Aggregate_s {key; location}
    | _ -> Error.error location (Error.Invalid_aggregate key)

  let mark key location =
    match Key.content key with
    | Key.Apply _ | Key.Ident _ | Key.List _ -> Mark {key; location}
    | _ -> Error.error location (Error.Invalid_mark key)

  let record key location =
    match Key.content key with
    | Key.Apply _ | Key.Ident _ | Key.String _ -> Record {key; location}
    | _ -> Error.error location (Error.Invalid_record key)

  let record_f key location =
    match Key.content key with
    | Key.Apply _ | Key.Ident _ | Key.String _ -> Record_f {key; location}
    | _ -> Error.error location (Error.Invalid_record key)

  let record_s key location =
    match Key.content key with
    | Key.Apply _ | Key.Ident _ | Key.String _ -> Record_s {key; location}
    | _ -> Error.error location (Error.Invalid_record key)

  let reset_block_section key location =
    match Key.content key with
    | Key.Apply _ | Key.Ident _ | Key.String _ ->
        Reset_block_section {key; location}
    | _ -> Error.error location (Error.Invalid_record key)

  let span key location =
    match Key.content key with
    | Key.Apply _ | Key.Ident _ | Key.List _ -> Span {key; location}
    | _ -> Error.error location (Error.Invalid_span key)

  let span_f key location =
    match Key.content key with
    | Key.Apply _ | Key.Ident _ | Key.List _ -> Span_f {key; location}
    | _ -> Error.error location (Error.Invalid_span key)

  let span_s key location =
    match Key.content key with
    | Key.Apply _ | Key.Ident _ | Key.List _ -> Span_s {key; location}
    | _ -> Error.error location (Error.Invalid_span key)

  let stop key location =
    match Key.content key with
    | Key.Empty -> Stop {key; location}
    | _ -> Error.error location (Error.Invalid_stop key)

  let get_location = function
    | Aggregate c
    | Aggregate_f c
    | Aggregate_s c
    | Mark c
    | Record c
    | Record_f c
    | Record_s c
    | Reset_block_section c
    | Span c
    | Span_f c
    | Span_s c
    | Stop c ->
        c.location

  let to_constant = function
    | Aggregate _ -> Constants.aggregate_constant
    | Aggregate_f _ -> Constants.aggregate_f_constant
    | Aggregate_s _ -> Constants.aggregate_s_constant
    | Mark _ -> Constants.mark_constant
    | Record _ -> Constants.record_constant
    | Record_f _ -> Constants.record_f_constant
    | Record_s _ -> Constants.record_s_constant
    | Reset_block_section _ -> Constants.record_s_constant
    | Span _ -> Constants.span_constant
    | Span_f _ -> Constants.span_f_constant
    | Span_s _ -> Constants.span_s_constant
    | Stop _ -> Constants.stop_constant

  let association_constant_rewriter =
    [
      (Constants.aggregate_constant, aggregate);
      (Constants.aggregate_f_constant, aggregate_f);
      (Constants.aggregate_s_constant, aggregate_s);
      (Constants.mark_constant, mark);
      (Constants.record_constant, record);
      (Constants.record_f_constant, record_f);
      (Constants.record_s_constant, record_s);
      (Constants.reset_block_section_constant, reset_block_section);
      (Constants.span_constant, span);
      (Constants.span_f_constant, span_f);
      (Constants.span_s_constant, span_s);
      (Constants.stop_constant, stop);
    ]
    |> List.map (fun (const, fn) -> (Constants.get_attribute const, fn))

  let of_string loc =
    let module StringMap = Map.Make (String) in
    let association_constant_rewriter =
      association_constant_rewriter |> List.to_seq |> StringMap.of_seq
    in
    fun attribute ->
      match StringMap.find_opt attribute association_constant_rewriter with
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
  let to_fully_qualified_lident_expr =
    let profiler_module = Ppxlib.Lident "Profiler" in
    fun t loc ->
      let lident =
        Ppxlib.Ldot
          ( profiler_module,
            match t with
            | Aggregate _ -> "aggregate"
            | Aggregate_f _ -> "aggregate_f"
            | Aggregate_s _ -> "aggregate_s"
            | Mark _ -> "mark"
            | Record _ -> "record"
            | Record_f _ -> "record_f"
            | Record_s _ -> "record_s"
            | Reset_block_section _ -> "reset_block_section"
            | Span _ -> "span"
            | Span_f _ -> "span_f"
            | Span_s _ -> "span_s"
            | Stop _ -> "stop" )
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

  let extract_key_from_payload loc payload =
    match payload with
    | Ppxlib.PStr
        [
          [%stri
            [%e?
              {
                pexp_desc =
                  Pexp_construct ({txt = Lident label; _}, Some structure);
                _;
              }]];
        ]
      when not (String.equal label "::") ->
        (* If the construct is "::" the payload attribute is a list but there's a trick:
           - [ [@profiler.mark Terse ["label"]] ] is a [Construct("Terse", Some expression)]
           - [ [@profiler.mark ["label"]] ] is a [Construct("::", Some expression)]
           The AST handles [Terse ...] and [ ["label"] ] in the same way so we
           need to make sure that we don't include the list case in this branch *)
        (* [@ppx Terse|Detailed|Verbose ...] *)
        Key.
          {
            label = Some label;
            content = extract_content_from_structure loc structure;
          }
    | Ppxlib.PStr [[%stri [%e? structure]]] ->
        (* [@ppx ...] *)
        Key.
          {label = None; content = extract_content_from_structure loc structure}
    | Ppxlib.PStr [] ->
        (* [@ppx] *)
        Key.{label = None; content = Key.Empty}
    | _ -> Error.error loc (Invalid_payload payload)

  let of_attribute ({Ppxlib.attr_payload; attr_loc; _} as attribute) =
    match Ppxlib_helper.get_attribute_name attribute |> of_string attr_loc with
    | Some rewriter ->
        let key = extract_key_from_payload attr_loc attr_payload in
        Some (rewriter key attr_loc)
    | None -> None

  let extract_rewriters = List.filter_map of_attribute
end
