(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

type t =
  | Bootstrap
  | Baker of int
  | Producer of int
  | Observer of [`Indexes of int list | `Pkh of string]
  | Archiver of [`Indexes of int list]
  | Reverse_proxy
  | Etherlink_operator
  | Etherlink_dal_operator
  | Etherlink_dal_observer of {slot_index : int}
  | Etherlink_producer of int
  | Echo_rollup_operator of int
  | Echo_rollup_dal_observer of {operator : int; slot_index : int}
  | Stresstest of int

let rex_bootstrap = rex "bootstrap"

let rex_baker_index = rex "baker-(\\d+)"

let rex_producer_index = rex "dal-producer-(\\d+)"

let rex_observer_indexes = rex "dal-observer-([0-9-]+)"

let rex_observer_pkh = rex "dal-observer-([a-zA-Z0-9]+)"

let rex_archiver_indexes = rex "dal-archiver-([0-9-]+)"

let rex_reverse_proxy = rex "dal-reverse-proxy"

let rex_etherlink_operator = rex "etherlink-operator"

let rex_etherlink_dal_operator = rex "etherlink-dal-operator"

let rex_etherlink_dal_observer_index = rex "etherlink-dal-operator-(\\d+)"

let rex_etherlink_producer_index = rex "etherlink-producer-(\\d+)"

let rex_echo_rollup_operator_index = rex "echo-rollup-operator-(\\d+)"

let rex_echo_rollup_dal_observer_index operator =
  rex (Format.sprintf "echo-rollup-dal-node-operator-%d-slot-(\\d+)" operator)

let rex_stresstest_index = rex "stresstest-(\\d+)"

let rex_constant = show_rex

let rex_replace_string =
  let string = rex "\\(\\[a-zA-Z0-9\\]\\+\\)" in
  fun r by -> replace_string string ~by (show_rex r)

let rex_replace_index =
  let index = rex "\\(\\\\d\\+\\)" in
  fun r i -> replace_string index ~by:(string_of_int i) (show_rex r)

let rex_replace_indexes =
  let indexes = rex "\\(\\[0-9-\\]\\+\\)" in
  fun r l ->
    replace_string
      indexes
      ~by:
        Format.(
          asprintf
            "%a"
            (pp_print_list
               ~pp_sep:(fun ppf () -> pp_print_string ppf "-")
               pp_print_int)
            l)
      (show_rex r)

let name_of = function
  | Bootstrap -> rex_constant rex_bootstrap
  | Baker i -> rex_replace_index rex_baker_index i
  | Producer i -> rex_replace_index rex_producer_index i
  | Observer (`Indexes l) -> rex_replace_indexes rex_observer_indexes l
  | Observer (`Pkh pkh) ->
      rex_replace_string rex_observer_pkh (String.sub pkh 0 8)
  | Archiver (`Indexes l) -> rex_replace_indexes rex_archiver_indexes l
  | Reverse_proxy -> rex_constant rex_reverse_proxy
  | Etherlink_operator -> rex_constant rex_etherlink_operator
  | Etherlink_dal_operator -> rex_constant rex_etherlink_dal_operator
  | Etherlink_dal_observer {slot_index} ->
      rex_replace_index rex_etherlink_dal_operator slot_index
  | Etherlink_producer i -> rex_replace_index rex_etherlink_producer_index i
  | Echo_rollup_operator i -> rex_replace_index rex_echo_rollup_operator_index i
  | Echo_rollup_dal_observer {operator; slot_index} ->
      rex_replace_index (rex_echo_rollup_dal_observer_index operator) slot_index
  | Stresstest i -> rex_replace_index rex_stresstest_index i

type daemon =
  | Baker_l1_node of int
  | Baker_dal_node of int
  | Producer_l1_node of int
  | Producer_dal_node of int
  | Observer_l1_node of int
  | Observer_dal_node of int
  | Archiver_l1_node of int
  | Archiver_dal_node of int
  | Echo_rollup_node of string
  | Etherlink_sc_rollup_node of string
  | Etherlink_evm_node of string
  | Etherlink_producer_node of string

let name_of_daemon = function
  | Baker_l1_node i -> Format.asprintf "baker-node-%d" i
  | Baker_dal_node i -> Format.asprintf "baker-dal-node-%d" i
  | Producer_l1_node i -> Format.asprintf "producer-node-%i" i
  | Producer_dal_node i -> Format.asprintf "producer-dal-node-%i" i
  | Observer_l1_node i -> Format.asprintf "observer-node-%i" i
  | Observer_dal_node i -> Format.asprintf "observer-dal-node-%i" i
  | Archiver_l1_node i -> Format.asprintf "archiver-node-%i" i
  | Archiver_dal_node i -> Format.asprintf "archiver-dal-node-%i" i
  | Echo_rollup_node name -> Format.asprintf "%s-rollup-node" name
  | Etherlink_sc_rollup_node name ->
      Format.asprintf "etherlink-%s-rollup-node" name
  | Etherlink_evm_node name -> Format.asprintf "etherlink-%s-evm-node" name
  | Etherlink_producer_node name -> Format.asprintf "etherlink-%s-node" name

module Logs = struct
  let scp_profiling ~destination_root ~daemon_name agent =
    let agent_name = Agent.name agent in
    (* This is not compatible with the --proxy mode as the Agent's location of
       the proxy might differ from the localhost one. *)
    let tezt_root_path = Agent.temp_execution_path () in
    Log.info "Retrieving profiling from %s" daemon_name ;
    match Agent.runner agent with
    | None ->
        Log.warn
          "Cannot retrieve profiling for %s: no runner for agent"
          agent_name ;
        Lwt.return_unit
    | Some _runner ->
        let runner_local_path =
          Artifact_helpers.local_path
            [destination_root; agent_name; daemon_name]
        in
        let process =
          Agent.docker_run_command agent "ls" [tezt_root_path // daemon_name]
        in
        let* output = process |> Process.check_and_read_stdout in
        let files = String.split_on_char '\n' output in
        let profiling_files =
          List.filter
            (fun s ->
              Option.is_some
                (s =~* Tezt.Base.rex "^(.+)_profiling\\.(txt|json)$"))
            files
        in
        Lwt_list.iter_s
          (fun file ->
            let local_path =
              Artifact_helpers.local_path [runner_local_path; "profiling"]
            in
            Lwt.catch
              (fun () ->
                Agent.scp
                  agent
                  ~is_directory:false
                  ~source:(tezt_root_path // daemon_name // file)
                  ~destination:local_path
                  `FromRunner)
              (fun exn ->
                Log.warn
                  "Cannot retrieve profiling trace (%s) from %s: %s"
                  file
                  agent_name
                  (Printexc.to_string exn) ;
                Lwt.return_unit))
          profiling_files

  let scp_logs ?(log_dir_name = "daily_logs") ~destination_root ~daemon_name
      agent =
    let agent_name = Agent.name agent in
    (* This is not compatible with the --proxy mode as the Agent's location of
       the proxy might differ from the localhost one. *)
    let tezt_root_path = Agent.temp_execution_path () in
    Log.info "Retrieving logs from %s" daemon_name ;
    match Agent.runner agent with
    | None ->
        Log.warn "Cannot retrieve logs for %s: no runner for agent" agent_name ;
        Lwt.return_unit
    | Some _runner ->
        let local_path =
          Artifact_helpers.local_path
            [destination_root; agent_name; daemon_name]
        in
        Lwt.catch
          (fun () ->
            Agent.scp
              agent
              ~is_directory:true
              ~source:(tezt_root_path // daemon_name // log_dir_name)
              ~destination:(local_path // log_dir_name)
              `FromRunner)
          (fun exn ->
            Log.warn
              "Cannot retrieve log from %s: %s"
              agent_name
              (Printexc.to_string exn) ;
            Lwt.return_unit)
end
