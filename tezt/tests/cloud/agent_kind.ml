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
  | Observer of [`Index of int | `Pkh of string]
  | Reverse_proxy
  | Etherlink_operator
  | Etherlink_dal_operator
  | Etherlink_dal_observer of {slot_index : int}
  | Etherlink_producer of int
  | Echo_rollup_operator
  | Echo_rollup_dal_observer of {slot_index : int}

let name_of = function
  | Bootstrap -> "bootstrap"
  | Baker i -> Format.asprintf "attester-%d" i
  | Producer i -> Format.asprintf "dal-producer-%d" i
  | Observer (`Index i) -> Format.asprintf "dal-observer-%d" i
  | Observer (`Pkh pkh) ->
      (* Shorting the pkh enables to get better logs. *)
      Format.asprintf "dal-observer-%s" (String.sub pkh 0 8)
  | Reverse_proxy -> "dal-reverse-proxy"
  | Etherlink_operator -> "etherlink-operator"
  | Etherlink_dal_operator -> "etherlink-dal-operator"
  | Etherlink_dal_observer {slot_index} ->
      Format.asprintf "etherlink-dal-operator-%d" slot_index
  | Etherlink_producer i -> Format.asprintf "etherlink-producer-%d" i
  | Echo_rollup_operator -> "echo-rollup-operator"
  | Echo_rollup_dal_observer {slot_index} ->
      Format.sprintf "echo-rollup-dal-node-%d" slot_index

type daemon =
  | Baker_l1_node of int
  | Baker_dal_node of int
  | Producer_l1_node of int
  | Producer_dal_node of int
  | Observer_l1_node of int
  | Observer_dal_node of int
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
  | Echo_rollup_node name -> Format.asprintf "%s-rollup-node" name
  | Etherlink_sc_rollup_node name ->
      Format.asprintf "etherlink-%s-rollup-node" name
  | Etherlink_evm_node name -> Format.asprintf "etherlink-%s-evm-node" name
  | Etherlink_producer_node name -> Format.asprintf "etherlink-%s-node" name

module Logs = struct
  let local_path path =
    List.fold_left
      (fun prefix subdir ->
        let prefix = prefix // subdir in
        if not (Sys.file_exists prefix) then Sys.mkdir prefix 0o755 ;
        prefix)
      ""
      path

  let scp_logs ~destination_root ~daemon_name agent =
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
          local_path [destination_root; agent_name; daemon_name]
        in
        Lwt.catch
          (fun () ->
            Agent.scp
              agent
              ~is_directory:true
              ~source:(tezt_root_path // daemon_name // "daily_logs")
              ~destination:(local_path // "daily_logs")
              `FromRunner)
          (fun exn ->
            Log.warn
              "Cannot retrieve log from %s: %s"
              agent_name
              (Printexc.to_string exn) ;
            Lwt.return_unit)
end
