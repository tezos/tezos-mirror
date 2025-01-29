(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Misc

type dependency =
  | Internal of {name : string; version : Version.t; with_test : bool}
  | External of Opam.dependency

type t = {
  name : string;
  version : Version.t;
  paths : string * string list;
  dependencies : dependency list;
  build : Opam.build_instruction list;
}

(* Convert a tree of [And] nodes into a list.
   This allows to extract nodes like [With_test] more easily. *)
let rec flatten_opam_dependency_condition_conjunction
    (condition : Opam.dependency_condition) : Opam.dependency_condition list =
  match condition with
  | And (a, b) ->
      flatten_opam_dependency_condition_conjunction a
      @ flatten_opam_dependency_condition_conjunction b
  | x -> [x]

(* Convert an [Opam.dependency] into a [dependency].
   Mainly we want to extract version constraints of internal dependencies
   to make them easier to work with. *)
let convert_opam_dependency config dev_version (dependency : Opam.dependency) =
  match Config.find_component_by_name dependency.name config with
  | Error {code = `not_found; _} ->
      (* Not an internal component, we don't really care about those for now. *)
      Ok (External dependency)
  | Ok component ->
      (* Extract [with-test] annotations. *)
      let conjunction =
        flatten_opam_dependency_condition_conjunction dependency.condition
      in
      let with_test, conjunction =
        if List.mem (With_test : Opam.dependency_condition) conjunction then
          ( true,
            List.filter
              (( <> ) (With_test : Opam.dependency_condition))
              conjunction )
        else (false, conjunction)
      in
      (* The rest must either empty or a single term. *)
      let* condition =
        match conjunction with
        | [] -> Ok (True : Opam.dependency_condition)
        | [x] -> Ok x
        | _ :: _ :: _ ->
            fail
              "unsupported version constraint on internal dependency %S"
              dependency.name
      in
      (* The only supported version terms are of the form [= version] or [= "version"]. *)
      let* version =
        match condition with
        | True | Comparison (EQ, Version_var) -> Ok dev_version
        | Comparison (EQ, Version_value version) -> Ok (Version.Old version)
        | _ ->
            fail
              "unsupported version constraint on internal dependency %S"
              dependency.name
      in
      Ok (Internal {name = component.name; version; with_test})

let load_not_cached (name, version) =
  wrap_errors
    (sf "failed to load component %S version %S" name (Version.show version))
  @@
  (* Load the configuration so that we know which packages are internal components
     and which ones are external, for the requested version. *)
  let* config = Config.load version in
  (* Check that the requested component actually exists in this configuration. *)
  let* component =
    match Config.find_component_by_name name config with
    | Ok _ as x -> x
    | Error {code = `not_found; message} -> Error {code = `failed; message}
  in
  (* Parse the opam file of the component, for the requested version. *)
  let* opam =
    match version with
    | Dev -> Opam.parse_file component.opam
    | Old version ->
        Git.with_checkout_into_tmp
          ~git_reference:version
          ~path:component.opam
          (fun checkout -> Opam.parse_file checkout.tmp_path)
  in
  (* Extract dependencies and their version constraints from the opam file. *)
  let* dependencies =
    list_map_r opam.depends (convert_opam_dependency config version)
  in
  Ok {name; version; paths = component.paths; dependencies; build = opam.build}

(* Memoization makes it easy to load each component only once for a given version. *)
let load_cached = memoize load_not_cached

(* The [memoize] function is made for functions with one argument, not two.
   We just have to uncurry. *)
let load name version = load_cached (name, version)
