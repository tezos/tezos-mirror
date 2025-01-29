(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Misc

(* First, we define pretty-printers for the types of the [opam-file-format] library.
   This helps when debugging, in particular when encountering a construct that we
   do not support yet. *)

let pp_opam_relop (op : OpamParserTypes.relop) : PP.t =
  match op with
  | `Eq -> Variant ("`Eq", [])
  | `Neq -> Variant ("`Neq", [])
  | `Geq -> Variant ("`Geq", [])
  | `Gt -> Variant ("`Gt", [])
  | `Leq -> Variant ("`Leq", [])
  | `Lt -> Variant ("`Lt", [])

let pp_opam_logop (op : OpamParserTypes.logop) : PP.t =
  match op with `And -> Variant ("`And", []) | `Or -> Variant ("`Or", [])

let rec pp_opam_value (value : OpamParserTypes.FullPos.value) : PP.t =
  match value.pelem with
  | Bool b -> Variant ("Bool", [Bool b])
  | Int i -> Variant ("Int", [Int i])
  | String s -> Variant ("String", [String s])
  | Relop (op, a, b) ->
      Variant
        ("Relop", [pp_opam_relop op.pelem; pp_opam_value a; pp_opam_value b])
  | Prefix_relop (op, a) ->
      Variant ("Prefix_relop", [pp_opam_relop op.pelem; pp_opam_value a])
  | Logop (op, a, b) ->
      Variant
        ("Logop", [pp_opam_logop op.pelem; pp_opam_value a; pp_opam_value b])
  | Pfxop _ -> Variant ("<Pfxop>", [])
  | Ident s -> Variant ("Ident", [String s])
  | List l -> Variant ("List", [List (List.map pp_opam_value l.pelem)])
  | Group _ -> Variant ("<Group>", [])
  | Option (v, l) ->
      Variant
        ("Option", [pp_opam_value v; List (List.map pp_opam_value l.pelem)])
  | Env_binding _ -> Variant ("<Env_binding>", [])

let rec pp_opam_item (item : OpamParserTypes.FullPos.opamfile_item) : PP.t =
  match item.pelem with
  | Section {section_kind; section_name; section_items} ->
      Variant
        ( "Section",
          [
            Record
              [
                ("section_kind", String section_kind.pelem);
                ( "section_name",
                  match section_name with
                  | None -> Variant ("None", [])
                  | Some name -> Variant ("Some", [String name.pelem]) );
                ( "section_items",
                  List (List.map pp_opam_item section_items.pelem) );
              ];
          ] )
  | Variable (name, value) ->
      Variant ("Variable", [String name.pelem; pp_opam_value value])

(* Then we define our own AST, which is higher-level than the one from opam-file-format.
   We also define pretty-printers for those, once again to help when debugging. *)

type version_term = Version_var | Version_value of string

let pp_version_term v : PP.t =
  match v with
  | Version_var -> Variant ("Version_var", [])
  | Version_value vv -> Variant ("Version_value", [String vv])

type comparison_operator = EQ | NEQ | GEQ | GT | LEQ | LT

let pp_comparison_operator op : PP.t =
  match op with
  | EQ -> Variant ("EQ", [])
  | NEQ -> Variant ("NEQ", [])
  | GEQ -> Variant ("GEQ", [])
  | GT -> Variant ("GT", [])
  | LEQ -> Variant ("LEQ", [])
  | LT -> Variant ("LT", [])

type dependency_condition =
  | True
  | With_test
  | Comparison of comparison_operator * version_term
  | And of dependency_condition * dependency_condition
  | Or of dependency_condition * dependency_condition

let rec pp_dependency_condition dc : PP.t =
  match dc with
  | True -> Variant ("True", [])
  | With_test -> Variant ("With_test", [])
  | Comparison (op, v) ->
      Variant ("Comparison", [pp_comparison_operator op; pp_version_term v])
  | And (a, b) ->
      Variant ("And", [pp_dependency_condition a; pp_dependency_condition b])
  | Or (a, b) ->
      Variant ("Or", [pp_dependency_condition a; pp_dependency_condition b])

type dependency = {name : string; condition : dependency_condition}

let pp_dependency {name; condition} : PP.t =
  Record
    [("name", String name); ("condition", pp_dependency_condition condition)]

type command_item = Const of string | Var of string

let pp_command_item item : PP.t =
  match item with
  | Const s -> Variant ("Const", [String s])
  | Var s -> Variant ("Var", [String s])

type build_condition = True | With_test

let pp_build_condition condition : PP.t =
  match condition with
  | True -> Variant ("True", [])
  | With_test -> Variant ("With_test", [])

type build_instruction = {
  command : command_item;
  arguments : command_item list;
  condition : build_condition;
}

let pp_build_instruction {command; arguments; condition} : PP.t =
  Record
    [
      ("command", pp_command_item command);
      ("arguments", List (List.map pp_command_item arguments));
      ("condition", pp_build_condition condition);
    ]

type t = {
  depends : dependency list;
  depopts : dependency list;
  conflicts : dependency list;
  build : build_instruction list;
}

let pp {depends; depopts; conflicts; build} : PP.t =
  Record
    [
      ("depends", List (List.map pp_dependency depends));
      ("depopts", List (List.map pp_dependency depopts));
      ("conflicts", List (List.map pp_dependency conflicts));
      ("build", List (List.map pp_build_instruction build));
    ]

(* This module does not implement all the opam file format.
   When something is not recognized, we raise [Unsupported].

   [value] is the value that is not supported, ready to be pretty-printed,
   to make it easier to add support for the missing feature.

   We use exceptions instead of the error monad to make it easier
   to write the conversion functions. But in the end the exception is converted
   to the error monad by the main parsing function [parse_file]. *)
exception Unsupported of {function_name : string; value : PP.t}

let unsupported function_name value = raise (Unsupported {function_name; value})

(* Finally we define conversion functions from the low-level AST of opam-file-format
   to our higher-level AST. *)

let dependency_condition_operator (op : OpamParserTypes.FullPos.relop) =
  match op.pelem with
  | `Eq -> EQ
  | `Neq -> NEQ
  | `Geq -> GEQ
  | `Gt -> GT
  | `Leq -> LEQ
  | `Lt -> LT

let as_version_term (value : OpamParserTypes.FullPos.value) =
  match value.pelem with
  | Ident "version" -> Version_var
  | String version -> Version_value version
  | _ -> unsupported "as_version_term" (pp_opam_value value)

let rec as_dependency_condition (value : OpamParserTypes.FullPos.value) :
    dependency_condition =
  match value.pelem with
  | Ident "with-test" -> With_test
  | Prefix_relop (relop, version) ->
      Comparison (dependency_condition_operator relop, as_version_term version)
  | Logop ({pelem = `And; _}, a, b) ->
      And (as_dependency_condition a, as_dependency_condition b)
  | Logop ({pelem = `Or; _}, a, b) ->
      Or (as_dependency_condition a, as_dependency_condition b)
  | _ -> unsupported "as_dependency_condition" (pp_opam_value value)

let as_dependency (value : OpamParserTypes.FullPos.value) =
  match value.pelem with
  | String name -> {name; condition = True}
  | Option ({pelem = String name; _}, {pelem = [condition]; _}) ->
      {name; condition = as_dependency_condition condition}
  | _ -> unsupported "as_dependency" (pp_opam_value value)

let as_build_instruction_item (value : OpamParserTypes.FullPos.value) =
  match value.pelem with
  | String s -> Const s
  | Ident s -> Var s
  | _ -> unsupported "as_build_instruction_item" (pp_opam_value value)

let as_condition
    (value :
      OpamParserTypes.FullPos.value list OpamParserTypes.FullPos.with_pos) =
  match value.pelem with
  | [{pelem = Ident "with-test"; _}] -> With_test
  | _ -> unsupported "as_condition" (List (List.map pp_opam_value value.pelem))

let as_build_instruction (value : OpamParserTypes.FullPos.value) =
  match value.pelem with
  | List {pelem = head :: tail; _} ->
      let command = as_build_instruction_item head in
      let arguments = List.map as_build_instruction_item tail in
      let condition = True in
      {command; arguments; condition}
  | Option ({pelem = List {pelem = head :: tail; _}; _}, filter) ->
      let command = as_build_instruction_item head in
      let arguments = List.map as_build_instruction_item tail in
      let condition = as_condition filter in
      {command; arguments; condition}
  | _ -> unsupported "as_build_instruction" (pp_opam_value value)

let as_list as_item (value : OpamParserTypes.FullPos.value option) =
  match value with
  | None -> []
  | Some value -> (
      match value.pelem with
      | List items -> List.map as_item items.pelem
      | _ -> unsupported "as_list" (pp_opam_value value))

let find_variable name (items : OpamParserTypes.FullPos.opamfile_item list) =
  Fun.flip List.find_map items @@ fun item ->
  match item.pelem with
  | Section _ -> unsupported "find_variable" (pp_opam_item item)
  | Variable (name', value) -> if name'.pelem = name then Some value else None

let parse_file filename =
  try
    if not (Sys.file_exists filename) then fail "file not found: %s" filename
    else
      (* Parse using the opam-file-format library. This gives a low-level AST. *)
      let* items =
        try Ok (OpamParser.FullPos.file filename).file_contents with
        | OpamLexer.Error msg -> fail "failed to parse %s: %s" filename msg
        | Parsing.Parse_error -> fail "failed to parse %s" filename
      in
      (* Give meaning to the low-level AST. *)
      let depends = find_variable "depends" items |> as_list as_dependency in
      let depopts = find_variable "depopts" items |> as_list as_dependency in
      let conflicts =
        find_variable "conflicts" items |> as_list as_dependency
      in
      let build = find_variable "build" items |> as_list as_build_instruction in
      Ok {depends; depopts; conflicts; build}
  with Unsupported {function_name; value} ->
    fail
      "failed to parse %s"
      filename
      ~reason:
        [sf "in Opam.%s: unsupported case:@.%a@?" function_name PP.pp value]
