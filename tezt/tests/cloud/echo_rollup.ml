(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

open Agent_kind
open Scenarios_helpers
open Tezos
open Yes_crypto

type operator = {
  node : Node.t;
  client : Client.t;
  sc_rollup_node : Sc_rollup_node.t;
  sc_rollup_address : string;
  operator : Account.key;
  origination_level : int;
}

let fetch_echo_rollup_data ~echo_rollup ~dal_node_producers ~level =
  let slots = Hashtbl.create 32 in
  let read_slot echo_operator slot =
    let key = "/output/slot-" ^ string_of_int slot in
    let* value_written =
      Sc_rollup_node.RPC.call echo_operator.sc_rollup_node
      @@ Sc_rollup_rpc.get_global_block_durable_state_value
           ~pvm_kind:"wasm_2_0_0"
           ~operation:Sc_rollup_rpc.Value
           ~key
           ~block:(string_of_int level)
           ()
    in
    let size =
      match value_written with
      | None -> 0
      | Some bytes -> `Hex bytes |> Hex.to_string |> Z.of_bits |> Z.to_int
    in
    Hashtbl.add slots slot size ;
    unit
  in
  let* () =
    match echo_rollup with
    | None -> unit
    | Some echo_rollup ->
        if level < echo_rollup.origination_level then unit
        else
          let* _level =
            Sc_rollup_node.wait_for_level
              ~timeout:30.
              echo_rollup.sc_rollup_node
              level
          in
          Lwt_list.iter_s (read_slot echo_rollup) dal_node_producers
  in
  return slots

let init_echo_rollup cloud ~data_dir ~simulate_network ~external_rpc ~network
    ~snapshot ~ppx_profiling_verbosity ~ppx_profiling_backends ~memtrace
    ~node_p2p_endpoint ~dal_node_p2p_endpoint operator dal_slots next_agent
    index =
  let name = name_of (Echo_rollup_operator index) in
  let* agent = next_agent ~name in
  let data_dir = data_dir |> Option.map (fun data_dir -> data_dir // name) in
  let with_yes_crypto = should_enable_yes_crypto simulate_network in
  let* node =
    Node_helpers.init
      ?data_dir
      ~name
      ~arguments:
        [Peer node_p2p_endpoint; refutation_game_minimal_rolling_history_mode]
      ~rpc_external:external_rpc
      network
      ~snapshot
      ~with_yes_crypto
      ~ppx_profiling_verbosity
      ~ppx_profiling_backends
      cloud
      agent
  in
  let endpoint = Client.Node node in
  let* client = Client.Agent.create ~endpoint agent in
  let () = toplog "Init Echo rollup: importing the operator secret key" in
  let* () =
    Client.import_secret_key
      client
      ~endpoint
      operator.Account.secret_key
      ~alias:operator.Account.alias
  in

  let l = Node.get_last_seen_level node in
  let () = toplog "Init Echo rollup: revealing the operator account" in
  let*! () = Client.reveal client ~endpoint ~src:operator.Account.alias in
  let () = toplog "Init Echo rollup: waiting for level %d" (l + 2) in
  let* _ = Node.wait_for_level node (l + 2) in
  let () = toplog "Init Echo rollup: waiting for level %d: done" (l + 2) in

  let otel = Cloud.open_telemetry_endpoint cloud in
  let* dal_node =
    match dal_slots with
    | [] ->
        toplog "Echo rollup doesn't follow any slot" ;
        none
    | [slot_index] ->
        let name =
          name_of (Echo_rollup_dal_observer {operator = index; slot_index})
        in
        let* agent = next_agent ~name in
        let* dal_node = Dal_node.Agent.create ~name ~node cloud agent in
        let* () =
          Dal_node.init_config
            ~expected_pow:(Network.expected_pow network)
            ~observer_profiles:dal_slots
            ~peers:(Option.to_list dal_node_p2p_endpoint)
            dal_node
        in
        let* () =
          Dal_node.Agent.run
            ?otel
            ~ppx_profiling_verbosity
            ~ppx_profiling_backends
            dal_node
        in
        some dal_node
    | _ ->
        let* dal_reverse_proxy_with_observers =
          Dal_reverse_proxy.init_dal_reverse_proxy_observers
            ~external_rpc
            ~network
            ~snapshot
            ~ppx_profiling_verbosity
            ~ppx_profiling_backends
            ~memtrace
            ~simulate_network
            ~name_of:(fun slot_index ->
              name_of (Echo_rollup_dal_observer {operator = index; slot_index}))
            ~default_endpoint:None
            ~node_p2p_endpoint
            ~dal_node_p2p_endpoint
            ~dal_slots
            ~index
            ~next_agent
            ~otel
            ~cloud
        in
        some dal_reverse_proxy_with_observers
  in
  let* sc_rollup_node =
    Sc_rollup_node.Agent.create
      ~name:(name_of_daemon (Echo_rollup_node name))
      ~base_dir:(Client.base_dir client)
      ~kind:"wasm_2_0_0"
      ~default_operator:operator.alias
      ~operators:[(Sc_rollup_node.Operating, operator.Account.alias)]
      ?dal_node
      cloud
      agent
      Operator
      node
  in
  let preimages_dir =
    Filename.concat (Sc_rollup_node.data_dir sc_rollup_node) "wasm_2_0_0"
  in
  let slots_bitvector =
    List.fold_left
      (fun vec slot -> Z.logor vec (Z.of_int (1 lsl slot)))
      Z.zero
      dal_slots
  in
  let config =
    Format.sprintf
      {|instructions:
- set:
    value: %s
    to: /slots
      |}
      (Z.to_bits slots_bitvector |> Hex.of_string |> Hex.show)
  in
  let output_config = Temp.file "config.yaml" in
  write_file output_config ~contents:config ;
  let* remote_output_config = Agent.copy agent ~source:output_config in
  let* {output; _} =
    Sc_rollup_helpers.Agent.prepare_installer_kernel
      ~config:(`Path remote_output_config)
      ~preimages_dir
      Constant.WASM.dal_echo_kernel_bandwidth
      agent
  in
  let pvm_kind = "wasm_2_0_0" in
  let l = Node.get_last_seen_level node in
  let () = toplog "Init Echo rollup: originating the rollup" in
  let* sc_rollup_address =
    Sc_rollup_helpers.Agent.originate_sc_rollup
      ~kind:pvm_kind
      ~boot_sector:output
      ~parameters_ty:"unit"
      ~src:operator.alias
      client
  in
  let () = toplog "Init Echo rollup: waiting again, for level %d" (l + 2) in
  (* The origination level is an overapproximation, it prevents metrics to fail
     if they attempt to read the rollup storage before its origination level. *)
  let* _ = Node.wait_for_level node (l + 2) in
  let () =
    toplog "Init Echo rollup: waiting again, for level %d: done" (l + 2)
  in
  let () = toplog "Init Echo rollup: launching the rollup node" in
  let* () =
    Sc_rollup_node.run sc_rollup_node sc_rollup_address [Log_kernel_debug]
  in
  let* genesis_info =
    Client.RPC.call client
    @@ RPC.get_chain_block_context_smart_rollups_smart_rollup_genesis_info
         sc_rollup_address
  in
  let origination_level = JSON.(genesis_info |-> "level" |> as_int) in
  let () = toplog "Init Echo rollup: launching the rollup node: done" in
  let operator =
    {
      node;
      client;
      sc_rollup_node;
      operator;
      sc_rollup_address;
      origination_level;
    }
  in
  let* () =
    add_prometheus_source
      ?dal_node
      ~node
      ~sc_rollup_node
      cloud
      agent
      (Format.asprintf "echo-%s" name)
  in
  return operator

let init_echo_rollup cloud ~data_dir ~simulate_network ~external_rpc ~network
    ~snapshot ~ppx_profiling_verbosity ~ppx_profiling_backends ~memtrace
    ~node_p2p_endpoint ~dal_node_p2p_endpoint ~next_agent producers index
    echo_rollup_key =
  let dal_slots = List.map (fun p -> p.Dal_node_helpers.slot_index) producers in
  let* echo_rollup =
    init_echo_rollup
      cloud
      ~data_dir
      ~simulate_network
      ~external_rpc
      ~network
      ~snapshot
      ~ppx_profiling_verbosity
      ~ppx_profiling_backends
      ~memtrace
      ~node_p2p_endpoint
      ~dal_node_p2p_endpoint
      echo_rollup_key
      dal_slots
      next_agent
      index
  in
  let () = toplog "Init: Echo rollup has been initialized" in
  Lwt.return echo_rollup
