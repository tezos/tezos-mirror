(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(* This scenario runs a Tezlink sequencer in sandbox mode. No rollup
   is actually originated anywhere.
*)

module Cli = Scenarios_cli
open Scenarios_helpers
open Tezos

let init_tezlink_sequencer (cloud : Cloud.t) (name : string) agent =
  let chain_id = 1 in
  let () = toplog "Initializing the tezlink scenario" in
  let tezlink_config = Temp.file "l2-tezlink-config.yaml" in
  let tez_bootstrap_accounts = Account.Bootstrap.keys |> Array.to_list in
  let*! () =
    Evm_node.make_l2_kernel_installer_config
      ~chain_id
      ~chain_family:"Michelson"
      ~eth_bootstrap_accounts:[]
      ~tez_bootstrap_accounts
      ~output:tezlink_config
      ()
  in
  let* () = Process.spawn "cat" [tezlink_config] |> Process.check in
  let () = toplog "Configuring the kernel" in
  let* remote_tezlink_config = Agent.copy agent ~source:tezlink_config in
  let wallet_dir = Temp.dir "wallet" in
  let () = Account.write Constant.all_secret_keys ~base_dir:wallet_dir in
  let preimages_dir = Temp.dir "wasm_2_0_0" in
  let* {output; _} =
    Sc_rollup_helpers.Agent.prepare_installer_kernel_with_multiple_setup_file
      ~configs:[remote_tezlink_config]
      ~preimages_dir
      (Uses.path Constant.WASM.evm_kernel)
      agent
  in
  let private_rpc_port = Agent.next_available_port agent |> Option.some in
  let spawn_rpc = Agent.next_available_port agent in
  let time_between_blocks = Some (Evm_node.Time_between_blocks 10.) in
  let mode =
    Evm_node.Tezlink_sandbox
      {
        initial_kernel = output;
        funded_addresses = [];
        preimage_dir = Some preimages_dir;
        private_rpc_port;
        time_between_blocks;
        genesis_timestamp = None;
        max_number_of_chunks = None;
        wallet_dir = Some wallet_dir;
        tx_pool_timeout_limit = None;
        tx_pool_addr_limit = None;
        tx_pool_tx_per_addr_limit = None;
      }
  in
  let () = toplog "Launching the sandbox L2 node" in
  let* evm_node =
    Tezos.Evm_node.Agent.init
      ~patch_config:(fun json ->
        JSON.update
          "public_rpc"
          (fun json ->
            JSON.update
              "cors_headers"
              (fun _ ->
                JSON.annotate ~origin:"patch-config:cors_headers"
                @@ `A [`String "*"])
              json
            |> JSON.update "cors_origins" (fun _ ->
                   JSON.annotate ~origin:"patch-config:cors_origins"
                   @@ `A [`String "*"]))
          json
        |> Evm_node.patch_config_with_experimental_feature
             ~drop_duplicate_when_injection:true
             ~blueprints_publisher_order_enabled:true
             ~rpc_server:Resto
             ~spawn_rpc
             ~l2_chains:
               [
                 {
                   (Evm_node.default_l2_setup ~l2_chain_id:chain_id) with
                   l2_chain_family = "Michelson";
                   tez_bootstrap_accounts = Some tez_bootstrap_accounts;
                 };
               ]
             ())
      ~name:"tezlink-sandboxed-sequencer"
      ~mode
      "http://dummy_rollup_endpoint"
      cloud
      agent
  in
  let () = toplog "Launching the sandbox L2 node: done" in
  let* () = add_prometheus_source ~evm_node cloud agent name in
  unit

let rec loop n =
  let n = n + 1 in
  let () = toplog "Loop %d" n in
  let* () = Lwt_unix.sleep 1. in
  loop n

let register (module Cli : Scenarios_cli.Tezlink) =
  let () = toplog "Parsing CLI done" in
  let name = "tezlink-sequencer" in
  let vms = return [Agent.Configuration.make ~name ()] in
  Cloud.register
  (* docker images are pushed before executing the test in case binaries are modified locally. This way we always use the latest ones. *)
    ~vms
    ~proxy_files:[]
    ~proxy_args:[]
    ~__FILE__
    ~title:"Tezlink sandbox"
    ~tags:[]
    (fun cloud ->
      let () = toplog "Creating the agents" in
      let agents = Cloud.agents cloud in
      let next_agent ~name =
        let agent =
          match List.find_opt (fun agent -> Agent.name agent = name) agents with
          | None ->
              if Cli.proxy_localhost then List.hd agents
              else Test.fail ~__LOC__ "Agent not found: %s" name
          | Some agent -> agent
        in
        Lwt.return agent
      in
      let* tezlink_sequencer_agent = next_agent ~name in
      let* () = init_tezlink_sequencer cloud name tezlink_sequencer_agent in
      let () = toplog "Starting main loop" in
      loop 0)
