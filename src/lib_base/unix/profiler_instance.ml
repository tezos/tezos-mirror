(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

open Profiler
module BackendMap = Map.Make (String)
module VerbosityMap = Map.Make (String)

type instance_maker =
  verbosity:Profiler.verbosity ->
  directory:string ->
  name:string ->
  Profiler.instance

type 'config driver = (module Profiler.DRIVER with type config = 'config)

type backend_infos = {instance_maker : instance_maker}

let registered_backends : backend_infos BackendMap.t ref = ref BackendMap.empty

(** Get the verbosity map from the contents of the PROFILING environment
    variable. The contents are expected to be of the following form:

    {[
    PROFILING="<profiler_i>-><verbosity_i>; ..."
    ]}

    where [profiler_i] is the name of the profiler that we want to be reported
    about with level of detail given by [verbosity_i]. *)
let get_verbosity_map () : verbosity option VerbosityMap.t =
  let parse_rules =
    match Sys.getenv_opt "PROFILING" |> Option.map String.lowercase_ascii with
    | None -> []
    | Some rules -> (
        try Tezos_stdlib_unix.Level_config_rules.parse_rules rules
        with _exn ->
          Printf.ksprintf
            Stdlib.failwith
            "Incorrect profiling rules defined in PROFILING variable!")
  in
  let level_to_verbosity : Internal_event.level option -> verbosity option =
    function
    | Some Debug -> Some Debug
    | Some Info -> Some Info
    | Some Notice -> Some Notice
    | Some invalid_value ->
        Fmt.failwith
          "Invalid PROFILING value '%s', disabling profiling."
          (Internal_event.Level.to_string invalid_value)
    | None -> None
  in
  List.fold_left
    (fun acc (name, rule) ->
      VerbosityMap.add name (level_to_verbosity rule) acc)
    VerbosityMap.empty
    parse_rules

(** [get_profiler_verbosity ~name] returns the level of detail associated to the profiler
    given by [~name], as dictated by the PROFILING environment variable. *)
let get_profiler_verbosity =
  let profiler_verbosity_map = get_verbosity_map () in
  fun ~name ->
    match VerbosityMap.find name profiler_verbosity_map with
    | Some verbosity -> verbosity
    | None ->
        (* In the case that a profiler is not assigned any verbosity, we try to check whether
           the catch-all case, represented by "", has any verbosity assigned. *)
        Option.join @@ VerbosityMap.find "" profiler_verbosity_map

let register_backend :
    type config.
    string list -> (config driver -> instance_maker) -> config driver -> unit =
 fun env instance_maker driver ->
  let module Driver = (val driver : DRIVER with type config = config) in
  match List.find (fun k -> BackendMap.mem k !registered_backends) env with
  | Some k ->
      Fmt.failwith "Profiler backend already registered for value \"%s\"" k
  | None ->
      registered_backends :=
        List.fold_left
          (fun acc k ->
            BackendMap.add k {instance_maker = instance_maker driver} acc)
          !registered_backends
          env

let wrap_backend_verbosity instance_maker ~directory ~name =
  match get_profiler_verbosity ~name with
  | None -> None
  | Some verbosity -> Some (instance_maker ~verbosity ~directory ~name)

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
      | Some _ ->
          Format.sprintf
            "No backend selected but profilers were enabled in PROFILING."
          |> fail)
  | Some b -> (
      match BackendMap.find b !registered_backends with
      | Some {instance_maker; _} -> Some (wrap_backend_verbosity instance_maker)
      | None ->
          Format.sprintf "No backend registered for value \"%s\"" b |> fail)

(** Default profilers. *)

let profiler ?(suffix = "")
    (backend : (module DRIVER with type config = string * verbosity)) ~verbosity
    ~directory ~name =
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
    Filename.Infix.(output_dir // (name ^ "_profiling" ^ suffix), verbosity)

let () =
  register_backend
    ["json"]
    (profiler ~suffix:".json")
    Simple_profiler.auto_write_as_json_to_file

let () =
  register_backend
    ["text"; "txt"]
    (profiler ~suffix:".txt")
    Simple_profiler.auto_write_as_txt_to_file
