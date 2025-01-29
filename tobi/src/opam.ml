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

module Install = struct
  type directory =
    | Lib
    | Lib_root
    | Libexec
    | Libexec_root
    | Bin
    | Sbin
    | Toplevel
    | Share
    | Share_root
    | Etc
    | Doc
    | Stublibs
    | Man
    | Misc

  type field_item = {source_path : string; target_path : string option}

  type field = {directory : directory; items : field_item list}

  type file = {
    directory : directory;
    source_path : string;
    target_path_relative_to_prefix : string;
    permissions : int;
  }

  (* [.install] files are lists of "fields", with a key denoting the directory
     where to install the files. But not exactly: each directory is treated
     slightly differently; for instance [lib] is not [lib/] but [lib/NAME]
     where [NAME] is the package name; [libexec] is the same as [lib] except
     that the execution flag must be set in the file's permissions; etc.
     So instead of storing directories as a string, we store them using a variant.
     [as_directory] converts into this variant. *)
  let as_directory = function
    | "lib" -> Lib
    | "lib_root" -> Lib_root
    | "libexec" -> Libexec
    | "libexec_root" -> Libexec_root
    | "bin" -> Bin
    | "sbin" -> Sbin
    | "toplevel" -> Toplevel
    | "share" -> Share
    | "share_root" -> Share_root
    | "etc" -> Etc
    | "doc" -> Doc
    | "stublibs" -> Stublibs
    | "man" -> Man
    | "misc" -> Misc
    | s -> unsupported "Install.as_directory" (String s)

  (* Field items are single files to install.
     They can be annotated with the target path where to install the file,
     or not, in which case a default target is used. *)
  let as_field_item (value : OpamParserTypes.FullPos.value) =
    match value.pelem with
    | String source_path -> {source_path; target_path = None}
    | Option
        ( {pelem = String source_path; _},
          {pelem = [{pelem = String target_path; _}]; _} ) ->
        {source_path; target_path = Some target_path}
    | _ -> unsupported "Install.as_field_item" (pp_opam_value value)

  (* Fields are keys (directories) associated with a list of files to install
     in the corresponding directory. *)
  let as_field (item : OpamParserTypes.FullPos.opamfile_item) =
    match item.pelem with
    | Variable (directory, {pelem = List items; _}) ->
        let directory = as_directory directory.pelem in
        let items = List.map as_field_item items.pelem in
        {directory; items}
    | _ -> unsupported "Install.as_field" (pp_opam_item item)

  let as_file ~package_name directory {source_path; target_path} =
    (* Give meaning to the [directory].
       The actual meaning depends on the [package_name]
       and, for man pages, on the file extension. *)
    let directory_path =
      match directory with
      | Lib | Libexec -> "lib" // package_name
      | Lib_root | Libexec_root -> "lib"
      | Bin -> "bin"
      | Sbin -> "sbin"
      | Toplevel -> "lib/toplevel"
      | Share -> "share" // package_name
      | Share_root -> "share"
      | Etc -> "etc" // package_name
      | Doc -> "doc" // package_name
      | Stublibs -> "lib/stublibs"
      | Man -> (
          match target_path with
          | Some _ -> "man"
          | None -> (
              match Filename.extension source_path with
              | ".1" -> "man/man1"
              | ".2" -> "man/man2"
              | ".3" | ".3o" -> "man/man3"
              | ".4" -> "man/man4"
              | ".5" -> "man/man5"
              | ".6" -> "man/man6"
              | ".7" -> "man/man7"
              | ".8" -> "man/man8"
              | ".9" -> "man/man9"
              | extension ->
                  (* There may be other subsections to support in the future,
                     but it's hard to predict which ones. *)
                  unsupported
                    "Install.list"
                    (Variant ("Man", [Record [("extension", String extension)]]))
              ))
      | Misc ->
          (* [Misc] seems dangerous because it allows to install files anywhere
             on the file system, not just in [_opam]. For now we don't need it. *)
          unsupported "Install.list" (Variant ("Misc", []))
    in
    (* Decide where the file will be installed. *)
    let target_path_relative_to_directory =
      match target_path with
      | None -> Filename.basename source_path
      | Some target_path -> target_path
    in
    let target_path_relative_to_prefix =
      directory_path // target_path_relative_to_directory
    in
    (* Decide whether to set the execution flag of the installed file. *)
    let permissions =
      match directory with
      | Lib | Lib_root | Toplevel | Share | Share_root | Etc | Doc | Man | Misc
        ->
          0o644
      | Libexec | Libexec_root | Bin | Sbin | Stublibs -> 0o755
    in
    {directory; source_path; target_path_relative_to_prefix; permissions}

  let parse_file ~package_name ~filename =
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
        let fields = List.map as_field items in
        let files =
          List.flatten @@ Fun.flip List.map fields
          @@ fun {directory; items} ->
          Fun.flip List.map items @@ fun item ->
          as_file ~package_name directory item
        in
        Ok files
    with Unsupported {function_name; value} ->
      fail
        "failed to parse %s"
        filename
        ~reason:
          [sf "in Opam.%s: unsupported case:@.%a@?" function_name PP.pp value]
end
