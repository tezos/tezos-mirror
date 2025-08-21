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

let relative_cache_dir {name; version; _} =
  match version with
  | Dev ->
      fail
        "failed to get cache directory for %s"
        name
        ~reason:["no cache directory for development versions"]
  | Old version ->
      let* commit_hash = Git.rev_parse version in
      Ok ("_tobi/cache" // name // commit_hash)

let available_in_cache component =
  let* cache_dir = relative_cache_dir component in
  Ok (Sys.file_exists (cache_dir // (component.name ^ ".install")))

let get_installed_commit_hash name =
  wrap_errors (sf "failed to figure out which version of %s is installed" name)
  @@
  (* Look into the cache directory for this component and
     iterate over all subdirectories that look like a commit hash. *)
  let cache_dir = "_tobi/cache" // name in
  match Unix.opendir cache_dir with
  | exception Unix.Unix_error (ENOENT, _, _) ->
      (* The component was never installed (or the cache was removed). *)
      Ok None
  | exception Unix.Unix_error (code, _, _) ->
      fail
        "failed to open directory: %s"
        cache_dir
        ~reason:[Unix.error_message code]
  | dir ->
      Fun.protect ~finally:(fun () -> closedir dir) @@ fun () ->
      let rec loop () =
        match Unix.readdir dir with
        | exception End_of_file -> Ok None
        | exception Unix.Unix_error (code, _, _) ->
            fail
              "failed to read directory: %s"
              cache_dir
              ~reason:[Unix.error_message code]
        | filename -> (
            let cache_dir = cache_dir // filename in
            if not (Git.is_a_commit_hash filename) then
              (* Not a commit hash, ignore. *)
              loop ()
            else
              let commit_hash = filename in
              (* An Opam [.install] file should be in the cache. Parse it. *)
              let* files =
                Opam.Install.parse_file
                  ~package_name:name
                  ~filename:(cache_dir // (name ^ ".install"))
              in
              (* Take the first file from the [.install] and check whether it exists
                 in [_opam] as a link to the expected target in Tobi's cache. *)
              match files with
              | [] ->
                  (* Empty component, weird.
                     Technically it is both installed and not installed. *)
                  Ok None
              | {source_path; target_path_relative_to_prefix; _} :: _ -> (
                  let expected_link_path =
                    "_opam" // target_path_relative_to_prefix
                  in
                  let expected_link_target_path =
                    Sys.getcwd () // cache_dir // source_path
                  in
                  match Unix.readlink expected_link_path with
                  | exception Unix.Unix_error (ENOENT, _, _) ->
                      (* Entry was removed since we started reading the directory. *)
                      loop ()
                  | exception Unix.Unix_error (EINVAL, _, _) ->
                      (* Not a link: this was not installed by Tobi. *)
                      loop ()
                  | exception Unix.Unix_error (code, _, _) ->
                      fail
                        "failed to read link: %s"
                        expected_link_path
                        ~reason:[Unix.error_message code]
                  | link_target_path ->
                      if link_target_path = expected_link_target_path then
                        Ok (Some commit_hash)
                      else loop ()))
      in
      loop ()
