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

module Tzkt_process = struct
  module Parameters = struct
    type persistent_state = unit

    type session_state = unit

    let base_default_name = "tzkt"

    let default_colors = Log.Color.[|FG.green; FG.yellow; FG.cyan; FG.magenta|]
  end

  include Daemon.Make (Parameters)

  let run ?runner cmd args =
    let daemon = create ?runner ~path:cmd () in
    run ?runner daemon () args
end

let init_tzkt ~tzkt_api_port ~agent ~tezlink_sandbox_endpoint =
  (* Set of functions helpful for Tzkt setup *)
  let spawn_run ?name cmd args =
    Agent.docker_run_command ?name agent cmd args
  in
  let run ?name cmd args = spawn_run ?name cmd args |> Process.check in
  (* Run a psql command (a specific database can be set as a target) *)
  let psql ?db command =
    run
      "sudo"
      (["-u"; "postgres"; "--"; "psql"]
      @ (match db with None -> [] | Some db -> [db])
      @ ["-c"; command])
  in
  (* Compile and publish the Tzkt indexer along the API *)
  let compile_tzkt target dir =
    run "dotnet" ["publish"; sf "tzkt/%s" target; "-o"; dir]
  in
  (* Tzkt optional arg have a specific format.
     Argument name is separated with ":" and
     argument value is separated with ";". *)
  let tzkt_arg options values =
    let option = String.concat ":" options in
    let value = String.concat ";" values in
    sf "--%s=%S" option value
  in

  (* Constants for Tzkt initialization. Helpful to prevent
     error when setting indexer and API argument *)
  let tzkt_db = "tzkt_db" in
  let tzkt_db_user = "tzkt" in
  let tzkt_indexer_output = "/root/tzkt-sync" in
  let tzkt_api_output = "/root/tzkt-api" in

  (* Start and initialize TZKT's PGSQL database *)
  let* () = run "service" ["postgresql"; "start"] in
  (* For some reason, postgres is not immediatly available after the
     service start command. *)
  let* () = Lwt_unix.sleep 1. in

  (* Remove the tzkt repo if it already exists. Also drop
     everything related to the database if it already exists. *)
  let* () = run "rm" ["-rf"; "tzkt"] in
  let* () = psql (sf "DROP DATABASE IF EXISTS %s;" tzkt_db) in
  let* () = psql (sf "DROP USER IF EXISTS %s;" tzkt_db_user) in

  (* Setup the database for Tzkt indexer. *)
  let* () = psql (sf "CREATE DATABASE %s;" tzkt_db) in
  let* () =
    psql (sf "CREATE USER %s WITH ENCRYPTED PASSWORD 'qwerty';" tzkt_db_user)
  in
  let* () =
    psql (sf "GRANT ALL PRIVILEGES ON DATABASE %s TO %s;" tzkt_db tzkt_db_user)
  in
  let* () =
    psql ~db:tzkt_db (sf "GRANT ALL ON SCHEMA public TO %s;" tzkt_db_user)
  in

  (* Clone TZKT sources on `proto23` branch as it supports Seoul. *)
  let* () =
    run
      "git"
      ["clone"; "-b"; "proto23"; "https://github.com/baking-bad/tzkt"; "tzkt"]
  in
  (* Compile Tzkt indexer and API. The output of the compilation is sent
     to different directory to prevent collision. *)
  let* () = compile_tzkt "Tzkt.Sync" tzkt_indexer_output in
  let* () = compile_tzkt "Tzkt.Api" tzkt_api_output in
  (* Remove the tzkt repo as we don't need it anymore. *)
  let* () = run "rm" ["-rf"; "tzkt"] in

  (* Get available port for the API and indexer.
     (or given port for the API). *)
  let indexer_port = Agent.next_available_port agent in
  let api_port =
    match tzkt_api_port with
    | None -> Agent.next_available_port agent
    | Some api_port -> api_port
  in
  (* Print a log for the tezt-cloud user to retrieve the port of the indexer or API *)
  let () = toplog "Tzkt indexer will be available at port %d" indexer_port in
  let () = toplog "Tzkt API will be available at port %d" api_port in

  (* Prepare multiple argument for Tzkt indexer and API start. *)
  (* Set our endpoint for Tzkt instead of the default "https://rpc.tzkt.io/mainnet/".*)
  let endpoint_arg =
    tzkt_arg
      ["TezosNode"; "Endpoint"]
      [Client.string_of_endpoint tezlink_sandbox_endpoint]
  in
  (* Tell Tzkt that the database is available on localhost at port 5432. *)
  (* We can't set a specific argument in this command so we must set every values. *)
  let database_arg =
    tzkt_arg
      ["ConnectionStrings"; "DefaultConnection"]
      [
        "host=localhost";
        "port=5432";
        sf "database=%s" tzkt_db;
        sf "username=%s" tzkt_db_user;
        "password=qwerty";
        "command timeout=600";
      ]
  in
  (* Argument to deploy the indexer and API at the port we selected.
     Note that http://0.0.0.0/ instead of http://localhost/ is important
     to make them available from outside. *)
  let indexer_port_arg =
    tzkt_arg
      ["Kestrel"; "Endpoints"; "Http"; "Url"]
      [sf "http://0.0.0.0:%d" indexer_port]
  in
  let api_port_arg =
    tzkt_arg
      ["Kestrel"; "Endpoints"; "Http"; "Url"]
      [sf "http://0.0.0.0:%d" api_port]
  in

  (* Run the Tzkt indexer and Tzkt API *)
  let runner = Agent.runner agent in
  let* () =
    Tzkt_process.run
      ?runner
      "sh"
      [
        "-c";
        sf
          "cd %s && dotnet Tzkt.Sync.dll %s %s %s"
          tzkt_indexer_output
          endpoint_arg
          database_arg
          indexer_port_arg;
      ]
  in
  let* () =
    Tzkt_process.run
      ?runner
      "sh"
      [
        "-c";
        sf
          "cd %s && dotnet Tzkt.Api.dll %s %s %s"
          tzkt_api_output
          endpoint_arg
          database_arg
          api_port_arg;
      ]
  in
  unit

let init_tezlink_sequencer (cloud : Cloud.t) (name : string)
    (rpc_port : int option) agent =
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
      ?rpc_port
      "http://dummy_rollup_endpoint"
      cloud
      agent
  in
  let tezlink_sandbox_endpoint =
    Client.(
      Foreign_endpoint
        Endpoint.
          {(Evm_node.rpc_endpoint_record evm_node) with path = "/tezlink"})
  in
  let () = toplog "Launching the sandbox L2 node: done" in
  let* () = add_prometheus_source ~evm_node cloud agent name in
  return tezlink_sandbox_endpoint

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
      let* tezlink_sandbox_endpoint =
        init_tezlink_sequencer
          cloud
          name
          Cli.public_rpc_port
          tezlink_sequencer_agent
      in
      let* () =
        init_tzkt
          ~tzkt_api_port:Cli.tzkt_api_port
          ~agent:tezlink_sequencer_agent
          ~tezlink_sandbox_endpoint
      in
      let () = toplog "Starting main loop" in
      loop 0)
