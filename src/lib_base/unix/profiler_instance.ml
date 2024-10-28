(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

open Profiler
module BackendMap = Map.Make (String)

let registered_backends :
    (directory:string -> name:string -> instance) BackendMap.t ref =
  ref BackendMap.empty

let max_verbosity =
  match Sys.getenv_opt "PROFILING" |> Option.map String.lowercase_ascii with
  | Some "debug" -> Debug
  | Some "info" -> Info
  | Some ("true" | "on" | "yes" | "notice") -> Notice
  | Some invalid_value ->
      Fmt.failwith
        "Invalid PROFILING value '%s', disabling profiling."
        invalid_value
  | None -> Notice

let register_backend env driver =
  match List.find (fun k -> BackendMap.mem k !registered_backends) env with
  | Some k ->
      Fmt.failwith "Profiler backend already registered for value \"%s\"" k
  | None ->
      registered_backends :=
        List.fold_left
          (fun acc k ->
            BackendMap.add
              k
              (fun ~directory ~name -> driver max_verbosity ~directory ~name)
              acc)
          !registered_backends
          env

let selected_backend () =
  let fail s =
    Fmt.failwith
      "@[<v 2>%s.@,\
       You can set a backend with PROFILING_BACKEND=backend.@,\
       @[<v 2>Available backends are:@,\
       %a@."
      s
      (Format.pp_print_list ~pp_sep:Format.pp_print_cut Format.pp_print_string)
      (BackendMap.fold
         (fun k _ acc -> (if k = "" then "\"\"" else k) :: acc)
         !registered_backends
         [])
  in
  match
    Sys.getenv_opt "PROFILING_BACKEND" |> Option.map String.lowercase_ascii
  with
  | None -> (
      match Sys.getenv_opt "PROFILING" with
      | None -> None
      | Some v ->
          Format.sprintf "No backend selected but verbosity is set to \"%s\"" v
          |> fail)
  | Some b -> (
      match BackendMap.find b !registered_backends with
      | Some _ as x -> x
      | None ->
          Format.sprintf "No backend registered for value \"%s\"" b |> fail)

(** Default profilers. *)

let profiler ?(suffix = "")
    (backend : (module DRIVER with type config = string * verbosity))
    max_verbosity ~directory ~name =
  let output_dir =
    (* If [PROFILING_OUTPUT_DIR] environment variable is set, it overwrites the
       directory provided by the application *)
    let output_dir =
      Sys.getenv_opt "PROFILING_OUTPUT_DIR" |> Option.value ~default:directory
    in
    match Sys.is_directory output_dir with
    | true -> output_dir
    | false ->
        Fmt.failwith
          "Error: Profiling output directory '%s' is not a directory."
          output_dir
    | exception Sys_error _ ->
        Tezos_stdlib_unix.Utils.create_dir ~perm:0o777 output_dir ;
        output_dir
  in
  Profiler.instance
    backend
    Filename.Infix.(output_dir // (name ^ suffix), max_verbosity)

let () =
  register_backend
    ["json+ext"]
    (profiler
       ~suffix:"_profiling.json"
       Simple_profiler.auto_write_as_json_to_file)

let () =
  register_backend
    ["json"]
    (profiler Simple_profiler.auto_write_as_json_to_file)

let () =
  register_backend
    ["text+ext"; "txt+ext"]
    (profiler
       ~suffix:"_profiling.txt"
       Simple_profiler.auto_write_as_txt_to_file)

let () =
  register_backend
    ["text"; "txt"]
    (profiler Simple_profiler.auto_write_as_txt_to_file)
