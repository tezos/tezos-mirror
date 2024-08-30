(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module rec Constants : sig
  type t

  (** Constant representing [@profiler.aggregate_s] *)
  val aggregate_s_constant : t

  (** Constant representing [@profiler.aggregate_f] *)
  val aggregate_f_constant : t

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

  (** Constant representing [@profiler.span_s] *)
  val span_s_constant : t

  (** Constant representing [@profiler.stop] *)
  val stop_constant : t

  val of_rewriter : Rewriter.t -> t

  val get_attribute : t -> string

  val filter_out_all_handled_attributes :
    Parsetree.attribute list -> Parsetree.attribute list
end = struct
  (* This rewriter handles ppxes starting with profiling. *)
  let namespace = "profiler"

  type t = {action : string; attribute_name : string}

  let create_constant action =
    let attribute_name = namespace ^ "." ^ action in
    {action; attribute_name}

  (* [@profiler.aggregate_s] *)
  let aggregate_s_constant = create_constant "aggregate_s"

  (* [@profiler.aggregate_f] *)
  let aggregate_f_constant = create_constant "aggregate_f"

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
      aggregate_s_constant;
      aggregate_f_constant;
      mark_constant;
      record_constant;
      record_f_constant;
      record_s_constant;
      reset_block_section_constant;
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
    | Aggregate_s of content
    | Aggregate_f of content
    | Mark of content
    | Record of content
    | Record_f of content
    | Record_s of content
    | Reset_block_section of content
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
    | Aggregate_s of content
    | Aggregate_f of content
    | Mark of content
    | Record of content
    | Record_f of content
    | Record_s of content
    | Reset_block_section of content
    | Span_s of content
    | Stop of content

  let aggregate_s key location =
    match key with
    | Key.Ident _ | Key.String _ | Key.Apply _ -> Aggregate_s {key; location}
    | _ -> Error.error location (Error.Invalid_aggregate key)

  let aggregate_f key location =
    match key with
    | Key.Ident _ | Key.String _ | Key.Apply _ -> Aggregate_f {key; location}
    | _ -> Error.error location (Error.Invalid_aggregate key)

  let mark key location =
    match key with
    | Key.List _ -> Mark {key; location}
    | _ -> Error.error location (Error.Invalid_mark key)

  let record key location =
    match key with
    | Key.Ident _ | Key.String _ | Key.Apply _ -> Record {key; location}
    | _ -> Error.error location (Error.Invalid_record key)

  let record_f key location =
    match key with
    | Key.Ident _ | Key.String _ | Key.Apply _ -> Record_f {key; location}
    | _ -> Error.error location (Error.Invalid_record key)

  let record_s key location =
    match key with
    | Key.Ident _ | Key.String _ | Key.Apply _ -> Record_s {key; location}
    | _ -> Error.error location (Error.Invalid_record key)

  let reset_block_section key location =
    match key with
    | Key.Ident _ | Key.String _ | Key.Apply _ ->
        Reset_block_section {key; location}
    | _ -> Error.error location (Error.Invalid_record key)

  let span_s key location =
    match key with
    | Key.List _ -> Span_s {key; location}
    | Key.Apply _ -> Span_s {key; location}
    | _ -> Error.error location (Error.Invalid_span key)

  let stop key location =
    match key with
    | Key.Empty -> Stop {key; location}
    | _ -> Error.error location (Error.Invalid_stop key)

  let get_location = function
    | Aggregate_s c
    | Aggregate_f c
    | Mark c
    | Record c
    | Record_f c
    | Record_s c
    | Reset_block_section c
    | Span_s c
    | Stop c ->
        c.location

  let to_constant = function
    | Aggregate_f _ -> Constants.aggregate_f_constant
    | Aggregate_s _ -> Constants.aggregate_s_constant
    | Mark _ -> Constants.mark_constant
    | Record _ -> Constants.record_constant
    | Record_f _ -> Constants.record_f_constant
    | Record_s _ -> Constants.record_s_constant
    | Reset_block_section _ -> Constants.record_s_constant
    | Span_s _ -> Constants.span_s_constant
    | Stop _ -> Constants.stop_constant

  let association_constant_rewriter =
    [
      (Constants.aggregate_f_constant, aggregate_f);
      (Constants.aggregate_s_constant, aggregate_s);
      (Constants.mark_constant, mark);
      (Constants.record_constant, record);
      (Constants.record_f_constant, record_f);
      (Constants.record_s_constant, record_s);
      (Constants.reset_block_section_constant, reset_block_section);
      (Constants.span_s_constant, span_s);
      (Constants.stop_constant, stop);
    ]
    |> List.map (fun (const, fn) -> (Constants.get_attribute const, fn))

  let of_string =
    let module StringMap = Map.Make (String) in
    let association_constant_rewriter =
      association_constant_rewriter |> List.to_seq |> StringMap.of_seq
    in
    fun attribute -> StringMap.find_opt attribute association_constant_rewriter

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
            | Aggregate_f _ -> "aggregate_f"
            | Aggregate_s _ -> "aggregate_s"
            | Mark _ -> "mark"
            | Record _ -> "record"
            | Record_f _ -> "record_f"
            | Record_s _ -> "record_s"
            | Reset_block_section _ -> "reset_block_section"
            | Span_s _ -> "span_s"
            | Stop _ -> "stop" )
      in
      Ppxlib.Ast_helper.Exp.ident {txt = lident; loc}

  (** [extract_list e] destruct [e] as [Some list] or returns [None] *)
  let rec extract_list = function
    | [%expr [%e? h] :: [%e? r]] -> Option.map (List.cons h) (extract_list r)
    | [%expr []] -> Some []
    | _ -> None

  let extract_key_from_payload loc payload =
    match payload with
    | Ppxlib.PStr [] ->
        (* [@ppx] *)
        Key.Empty
    | Ppxlib.PStr
        [
          [%stri
            [%e? {pexp_desc = Pexp_constant (Pconst_string (label, _, _)); _}]];
        ] ->
        (* [@ppx "label"] *)
        Key.String label
    | Ppxlib.PStr
        [[%stri [%e? {pexp_desc = Pexp_ident {txt = Lident ident; _}; _}]]] ->
        (* [@ppx ident] *)
        Key.Ident ident
    | Ppxlib.PStr
        [[%stri [%e? {pexp_desc = Pexp_apply (fun_name, fun_args); _}]]] ->
        (* [@ppx fun_name fun_arg1 fun_arg2 ...] *)
        let fun_args = List.map snd fun_args in
        Key.Apply (fun_name, fun_args)
    | Ppxlib.PStr [[%stri [%e? expr]]] -> (
        (* [@ppx ["label1"; "label2"; ...]] *)
        match extract_list expr with
        | Some labels -> Key.List labels
        | None -> Error.error loc (Error.Invalid_mark (Key.Other expr)))
    | _ -> Error.error loc (Error.Invalid_payload payload)

  let of_attribute ({Ppxlib.attr_payload; attr_loc; _} as attribute) =
    match Ppxlib_helper.get_attribute_name attribute |> of_string with
    | Some rewriter ->
        let key = extract_key_from_payload attr_loc attr_payload in
        Some (rewriter key attr_loc)
    | None -> None

  let extract_rewriters = List.filter_map of_attribute
end
