(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Misc

type component = {name : string; paths : string * string list; opam : string}

type t = {pervasive_paths : string list; components : component list}

let pervasive_paths config = config.pervasive_paths

let components config = config.components

let find_component_by_name name config =
  match
    List.find_opt (fun component -> component.name = name) config.components
  with
  | None -> error `not_found "component not found in configuration"
  | Some component -> Ok component

(* Read and parse [filename]. *)
let load_file filename =
  match open_in filename with
  | exception Sys_error msg -> fail "failed to open configuration file: %s" msg
  | ch ->
      Fun.protect ~finally:(fun () -> close_in ch) @@ fun () ->
      (* Read all lines. *)
      let rec read_lines acc =
        match input_line ch with
        | exception End_of_file -> List.rev acc
        | line -> read_lines (line :: acc)
      in
      let lines = read_lines [] in
      (* Ignore comments and empty lines. *)
      let lines =
        Fun.flip List.filter lines @@ fun line ->
        let line = String.trim line in
        String.length line > 0 && line.[0] <> '#'
      in
      (* Split lines into key-value pairs separated by a colon,
         and values into lists of values separated by a comma. *)
      let entries =
        Fun.flip List.map lines @@ fun line ->
        match str_split_once line ':' with
        | None -> (String.trim line, [])
        | Some (key, value) ->
            let key = String.trim key in
            let values =
              value |> String.split_on_char ',' |> List.map String.trim
              |> List.filter (( <> ) "")
            in
            (key, values)
      in
      (* Merge multiple definitions of the same keys. *)
      let keys : String_set.t String_map.t ref = ref String_map.empty in
      ( Fun.flip List.iter entries @@ fun (key, values) ->
        let old_values =
          String_map.find_opt key !keys |> default String_set.empty
        in
        let new_values = String_set.of_list values in
        keys :=
          String_map.add key (String_set.union old_values new_values) !keys ) ;
      (* Extract __pervasive. *)
      let extract key =
        let values =
          String_map.find_opt key !keys
          |> default String_set.empty |> String_set.elements
        in
        keys := String_map.remove key !keys ;
        values
      in
      let pervasive_paths = extract "__pervasive" in
      (* Remaining entries are component definitions. *)
      let* components =
        list_map_r (String_map.bindings !keys) @@ fun (name, paths) ->
        match String_set.elements paths with
        | [] -> fail "in %s, component %s has no path" filename name
        | head :: tail ->
            (* Deduce the location of the .opam file from the name. *)
            let opam = "opam/" ^ name ^ ".opam" in
            Ok {name; paths = (head, tail); opam}
      in
      Ok {pervasive_paths; components}

let load_not_cached (version : Version.t) =
  match version with
  | Dev -> load_file "tobi/config"
  | Old version ->
      Git.with_checkout_into_tmp
        ~git_reference:version
        ~path:"tobi/config"
        (fun checkout -> load_file checkout.tmp_path)

(* Memoization makes it easy to load the configuration only once for a given version. *)
let load = memoize load_not_cached
