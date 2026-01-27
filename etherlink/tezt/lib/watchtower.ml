(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

type whitelist_entry = {proxy : string; ticket_hashes : string list option}

type rpc_config = {addr : string; port : int}

type config = {
  evm_node_endpoint : string;
  gas_limit : string option;
  max_fee_per_gas : string option;
  rpc : rpc_config option;
  secret_key : string option;
  whitelist : whitelist_entry list option;
}

module Parameters = struct
  type persistent_state = {
    data_dir : string;
    config_file : string;
    runner : Runner.t option;
    mutable pending_ready : unit option Lwt.u list;
  }

  type session_state = {mutable ready : bool}

  let base_default_name = "bridge_watchtower"

  let default_colors = Log.Color.[|FG.blue; FG.magenta; FG.yellow; FG.green|]
end

open Parameters
include Daemon.Make (Parameters)

let trigger_ready watchtower value =
  let pending = watchtower.persistent_state.pending_ready in
  watchtower.persistent_state.pending_ready <- [] ;
  List.iter (fun pending -> Lwt.wakeup_later pending value) pending

let set_ready watchtower =
  (match watchtower.status with
  | Not_running -> ()
  | Running status -> status.session_state.ready <- true) ;
  trigger_ready watchtower (Some ())

let event_ready_name = "bridge_watchtower_started.v0"

let handle_ready_event watchtower {name; value = _; timestamp = _} =
  if name = event_ready_name then set_ready watchtower else ()

let wait_for_ready ?timeout watchtower =
  match watchtower.status with
  | Running {session_state = {ready = true; _}; _} -> unit
  | Not_running | Running {session_state = {ready = false; _}; _} -> (
      let promise, resolver = Lwt.task () in
      watchtower.persistent_state.pending_ready <-
        resolver :: watchtower.persistent_state.pending_ready ;
      let* res =
        Lwt.pick
          [
            (let* result = promise in
             return result);
            (let* () = Lwt_unix.sleep (Option.value ~default:30. timeout) in
             return None);
          ]
      in
      match res with
      | Some v -> return v
      | None -> Test.fail "Timeout waiting for bridge watchtower to be ready")

module Config_file = struct
  let filename watchtower = watchtower.persistent_state.config_file

  let whitelist_entry_to_json {proxy; ticket_hashes} =
    `O
      ([("proxy", `String proxy)]
      @
      match ticket_hashes with
      | None -> []
      | Some hashes ->
          [("ticket_hashes", `A (List.map (fun h -> `String h) hashes))])

  let rpc_config_to_json {addr; port} =
    `O [("addr", `String addr); ("port", `Float (float_of_int port))]

  let config_to_json
      {
        evm_node_endpoint;
        gas_limit;
        max_fee_per_gas;
        rpc;
        secret_key;
        whitelist;
      } =
    JSON.annotate
      ~origin:"bridge_watchtower.config"
      (`O
         ([("evm_node_endpoint", `String evm_node_endpoint)]
         @ (match gas_limit with
           | None -> []
           | Some gl -> [("gas_limit", `String gl)])
         @ (match max_fee_per_gas with
           | None -> []
           | Some mfpg -> [("max_fee_per_gas", `String mfpg)])
         @ (match rpc with
           | None -> []
           | Some r -> [("rpc", rpc_config_to_json r)])
         @ (match secret_key with
           | None -> []
           | Some sk -> [("secret_key", `String sk)])
         @
         match whitelist with
         | None -> []
         | Some wl -> [("whitelist", `A (List.map whitelist_entry_to_json wl))]
         ))

  let write watchtower config =
    let json = config_to_json config in
    match watchtower.persistent_state.runner with
    | None -> JSON.encode_to_file (filename watchtower) json |> return
    | Some runner ->
        let content = JSON.encode json in
        let cmd =
          Runner.Shell.(
            redirect_stdout (cmd [] "echo" [content]) (filename watchtower))
        in
        let cmd, args = Runner.wrap_with_ssh runner cmd in
        Process.run cmd args

  let read watchtower =
    match watchtower.persistent_state.runner with
    | None -> Lwt.return (JSON.parse_file (filename watchtower))
    | Some runner ->
        let* content =
          Process.spawn ~runner "cat" [filename watchtower]
          |> Process.check_and_read_stdout
        in
        JSON.parse ~origin:"Bridge_watchtower.config_file.read" content
        |> Lwt.return
end

let create ?(path = "./fa-bridge-watchtower") ?name ?runner ?data_dir
    ?config_file () =
  let name = Option.value ~default:(fresh_name ()) name in
  let data_dir =
    match data_dir with
    | None -> Temp.dir ("bridge_watchtower_" ^ name)
    | Some dir -> dir
  in
  let config_file =
    match config_file with
    | None -> data_dir // "config.json"
    | Some file -> file
  in
  create ~path ~name {data_dir; config_file; runner; pending_ready = []}

let run ?(wait = true) ?(verbose = false) ?(debug = false) ?evm_node ?secret_key
    ?first_block watchtower =
  on_event watchtower (handle_ready_event watchtower) ;
  let on_terminate _status =
    trigger_ready watchtower None ;
    unit
  in
  let args =
    ["run"; "-d"; watchtower.persistent_state.data_dir]
    @ (if verbose then ["-v"] else [])
    @ (if debug then ["--debug"] else [])
    @ (match evm_node with
      | None -> []
      | Some endpoint -> ["--evm-node"; endpoint])
    @ (match secret_key with None -> [] | Some sk -> ["--secret-key"; sk])
    @
    match first_block with
    | None -> []
    | Some block -> ["--first-block"; string_of_int block]
  in
  let* () =
    run
      ?runner:watchtower.persistent_state.runner
      ~event_level:`Debug
      watchtower
      {ready = false}
      args
      ~on_terminate
  in
  if wait then wait_for_ready watchtower else unit

let init ?(wait = true) ?path ?name ?runner ?data_dir ?config ?verbose ?debug
    ?evm_node ?secret_key ?first_block () =
  let watchtower = create ?path ?name ?runner ?data_dir () in
  let* () =
    match config with
    | Some cfg -> Config_file.write watchtower cfg
    | None -> unit
  in
  let* () =
    run ~wait ?verbose ?debug ?evm_node ?secret_key ?first_block watchtower
  in
  return watchtower
