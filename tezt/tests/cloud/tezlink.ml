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

    let default_colors = Evm_node.daemon_default_colors
  end

  include Daemon.Make (Parameters)

  let run ?runner ~suffix ~path ~dll ~endpoint ~db ~port ?(args = []) () =
    let process_name = sf "%s-%s" Parameters.base_default_name suffix in
    let daemon = create ?runner ~name:process_name ~path:"sh" () in
    let arguments = String.concat " " ([endpoint; db; port] @ args) in
    run
      ?runner
      daemon
      ()
      ["-c"; sf "cd %s && dotnet Tzkt.%s.dll %s" path dll arguments]
end

module Faucet_backend_process = struct
  module Parameters = struct
    type persistent_state = unit

    type session_state = unit

    let base_default_name = "faucet-backend"

    let default_colors = Evm_node.daemon_default_colors
  end

  include Daemon.Make (Parameters)

  let run ?runner ~path () =
    let daemon =
      create ?runner ~name:Parameters.base_default_name ~path:"sh" ()
    in
    run ?runner daemon () ["-c"; sf "cd %s && npm run start" path]
end

module Faucet_frontend_process = struct
  module Parameters = struct
    type persistent_state = unit

    type session_state = unit

    let base_default_name = "faucet-frontend"

    let default_colors = Evm_node.daemon_default_colors
  end

  include Daemon.Make (Parameters)

  let run ?runner ~path ~port () =
    let daemon =
      create ?runner ~name:Parameters.base_default_name ~path:"sh" ()
    in
    run
      ?runner
      daemon
      ()
      [
        "-c";
        sf
          "cd %s && node_modules/.bin/serve build --single -p %d \
           --no-port-switching"
          path
          port;
      ]
end

let polling_period = function
  | Evm_node.Nothing -> 500
  | Time_between_blocks t ->
      let in_ms = t *. 1000. |> int_of_float in
      in_ms / 2

let spawn_run agent ?name cmd args =
  Agent.docker_run_command ?name agent cmd args

let run agent ?name cmd args = spawn_run agent ?name cmd args |> Process.check

let run_cmd agent cmd = run agent "sh" ["-c"; cmd]

let git_clone agent ?branch repo_url directory =
  run
    agent
    "git"
    (["clone"]
    @ (match branch with None -> [] | Some branch -> ["-b"; branch])
    @ [repo_url; directory])

let build_endpoint ~runner port =
  Client.Foreign_endpoint
    (Endpoint.make ~host:(Runner.address runner) ~scheme:"http" ~port ())

let init_tzkt ~tzkt_api_port ~agent ~tezlink_sandbox_endpoint
    ~time_between_blocks =
  (* Set of functions helpful for Tzkt setup *)
  let run = run agent in
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

  (* Clone TZKT sources on head-streaming branch as it supports Seoul and fast
     indexing. *)
  let* () =
    git_clone
      agent
      ~branch:"head-streaming"
      "https://github.com/baking-bad/tzkt"
      "tzkt"
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
  let polling_args =
    [
      (* debounce=false means the indexer doesn't sleep for "min block time" *)
      tzkt_arg ["Observer"; "Debounce"] ["false"];
      (* delay between polling requests in milliseconds *)
      tzkt_arg
        ["Observer"; "Period"]
        [polling_period time_between_blocks |> string_of_int];
    ]
  in

  (* Run the Tzkt indexer and Tzkt API *)
  let runner = Agent.runner agent in
  let* () =
    Tzkt_process.run
      ?runner
      ~suffix:"indexer"
      ~path:tzkt_indexer_output
      ~dll:"Sync"
      ~endpoint:endpoint_arg
      ~db:database_arg
      ~port:indexer_port_arg
      ~args:polling_args
      ()
  in
  let* () =
    Tzkt_process.run
      ?runner
      ~suffix:"api"
      ~path:tzkt_api_output
      ~dll:"Api"
      ~endpoint:endpoint_arg
      ~db:database_arg
      ~port:api_port_arg
      ()
  in
  return (build_endpoint ~runner api_port)

let create_config_file ~agent destination format =
  let temp_file = Temp.file "config" in
  let ch = open_out temp_file in
  Format.kfprintf
    (fun _fmt ->
      let () = close_out ch in
      let* (_ : string) = Agent.copy agent ~source:temp_file ~destination in
      unit)
    (Format.formatter_of_out_channel ch)
    format

let init_faucet_backend ~agent ~tezlink_sandbox_endpoint ~faucet_private_key =
  let faucet_api_port = Agent.next_available_port agent in
  let faucet_backend_dir = "faucet-backend" in
  (* Clone faucet backend from personal fork because upstream depends on a RPC which we don't yet support (forge_operation). *)
  let* () =
    git_clone
      agent
      ~branch:"rafoo@local_forging"
      "https://github.com/rafoo/tezos-faucet-backend.git"
      faucet_backend_dir
  in
  let* () =
    create_config_file
      ~agent
      (sf "%s/.env" faucet_backend_dir)
      {|
API_PORT=%d
RPC_URL=%s
FAUCET_PRIVATE_KEY=%s

AUTHORIZED_HOST="*"
ENABLE_CAPTCHA=false
DISABLE_CHALLENGES=true
MAX_BALANCE=6000
MIN_TEZ=1
MAX_TEZ=6000
MIN_CHALLENGES=1
MAX_CHALLENGES=550
MAX_CHALLENGES_WITH_CAPTCHA=66
CHALLENGE_SIZE=2048
DIFFICULTY=4
|}
      faucet_api_port
      (Client.string_of_endpoint tezlink_sandbox_endpoint)
      faucet_private_key
  in
  let* () =
    run_cmd
      agent
      (sf "cd %s && npm install typescript && npm run build" faucet_backend_dir)
  in
  let runner = Agent.runner agent in
  let* () = Faucet_backend_process.run ?runner ~path:faucet_backend_dir () in
  return (build_endpoint ~runner faucet_api_port)

let init_faucet_frontend ~faucet_api ~agent ~tezlink_sandbox_endpoint
    ~faucet_pkh ~tzkt_api =
  let faucet_frontend_port = Agent.next_available_port agent in
  let faucet_frontend_dir = "faucet-frontend" in
  (* Clone faucet frontend from personal fork because upstream does
     not yet support using sandbox.tzkt.io as explorer. *)
  let* () =
    git_clone
      agent
      ~branch:"rafoo@hash_in_viewer"
      "https://github.com/rafoo/tezos-faucet.git"
      faucet_frontend_dir
  in
  let* () =
    create_config_file
      ~agent
      (sf "%s/public/config.json" faucet_frontend_dir)
      {|
{
    "application": {
      "name": "Tezlink Test Faucet",
      "googleCaptchaSiteKey": "6LefC8QmAAAAAIXuoOhyI-FtALkYXZm4JK0GBYfb",
      "backendUrl": %S,
      "githubRepo": "https://github.com/tacoinfra/tezos-faucet",
      "disableChallenges": true,
      "minTez": 1,
      "maxTez": 6000
    },
    "network": {
      "name": "Custom",
      "rpcUrl": %S,
      "faucetAddress": %S,
      "viewer": "http://sandbox.tzkt.io/<hash>?tzkt_api_url=%s"
    }
}
|}
      (Client.string_of_endpoint faucet_api)
      (Client.string_of_endpoint tezlink_sandbox_endpoint)
      faucet_pkh
      (Client.url_encoded_string_of_endpoint tzkt_api)
  in
  let* () =
    run_cmd
      agent
      (sf
         "cd %s && npm install && npm run build && npm run check-config && \
          mkdir -p build && cp public/config.json build/"
         faucet_frontend_dir)
  in
  let runner = Agent.runner agent in
  let* () =
    Faucet_frontend_process.run
      ?runner
      ~path:faucet_frontend_dir
      ~port:faucet_frontend_port
      ()
  in
  return
    Client.(
      Foreign_endpoint
        (Endpoint.make
           ~host:(Runner.address runner)
           ~scheme:"http"
           ~port:faucet_frontend_port
           ()))

let init_tezlink_sequencer (cloud : Cloud.t) (name : string)
    ?(rpc_port : int option) (verbose : bool)
    (time_between_blocks : Evm_node.time_between_blocks) agent =
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
  let mode =
    Evm_node.Tezlink_sandbox
      {
        initial_kernel = output;
        funded_addresses = [];
        preimage_dir = Some preimages_dir;
        private_rpc_port;
        time_between_blocks = Some time_between_blocks;
        genesis_timestamp = None;
        max_number_of_chunks = None;
        wallet_dir = Some wallet_dir;
        tx_queue_max_lifespan = None;
        tx_queue_max_size = None;
        tx_queue_tx_per_addr_limit = None;
        verbose;
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

let add_service cloud ~name ~url =
  let () = toplog "New service: %s: %s" name url in
  Cloud.add_service cloud ~name ~url

let register (module Cli : Scenarios_cli.Tezlink) =
  let () = toplog "Parsing CLI done" in
  let name = "tezlink-sequencer" in
  let vms () = return [Agent.Configuration.make ~name ()] in
  Cloud.register
  (* docker images are pushed before executing the test in case binaries are modified locally. This way we always use the latest ones. *)
    ~vms
    ~proxy_files:[]
    ~proxy_args:[]
    ~__FILE__
    ~title:"Tezlink sandbox"
    ~tags:[]
    (fun cloud ->
      Clap.close () ;
      let () = toplog "Creating the agents" in
      let agents = Cloud.agents cloud in
      let tezlink_sequencer_agent =
        match List.find_opt (fun agent -> Agent.name agent = name) agents with
        | None ->
            if Cli.proxy_localhost then List.hd agents
            else Test.fail ~__LOC__ "Agent not found: %s" name
        | Some agent -> agent
      in
      let public_rpc_port =
        match Cli.public_rpc_port with
        | None -> Agent.next_available_port tezlink_sequencer_agent
        | Some port -> port
      in
      let internal_port =
        match Tezt_cloud.Tezt_cloud_cli.dns_domains with
        | [] ->
            (* No DNS so no proxy, so we must use the public RPC port. *)
            Some public_rpc_port
        | _ :: _ ->
            (* We let the system choose a fresh internal RPC node port.
               Note that it will be publicy exposed, it's just that we don't need
               to share this one. *)
            None
      in
      let () = toplog "Starting Tezlink sequencer" in
      let* tezlink_sandbox_endpoint =
        init_tezlink_sequencer
          cloud
          name
          ?rpc_port:internal_port
          Cli.verbose
          Cli.time_between_blocks
          tezlink_sequencer_agent
      in
      let tezlink_proxy_endpoint, _ip =
        match tezlink_sandbox_endpoint with
        | Node _ ->
            failwith "Tezlink end-point should not be a full-fledged node."
        | Foreign_endpoint {host; scheme; port = _; path = _} ->
            let endpoint =
              Client.Foreign_endpoint
                (Endpoint.make ~host ~scheme ~port:public_rpc_port ())
            in
            (endpoint, host)
      in
      let* () =
        add_service
          cloud
          ~name:"Tezlink RPC endpoint"
          ~url:(Client.string_of_endpoint tezlink_proxy_endpoint)
      in
      let* () =
        add_service
          cloud
          ~name:"Check Tezlink RPC endpoint"
          ~url:
            (sf "%s/version" (Client.string_of_endpoint tezlink_proxy_endpoint))
      in
      let* () =
        if Cli.tzkt then
          let () = toplog "Starting TzKT" in
          let* tzkt_api =
            init_tzkt
              ~tzkt_api_port:Cli.tzkt_api_port
              ~agent:tezlink_sequencer_agent
              ~tezlink_sandbox_endpoint
              ~time_between_blocks:Cli.time_between_blocks
          in
          let* () =
            add_service
              cloud
              ~name:"TzKT API"
              ~url:(Client.string_of_endpoint tzkt_api)
          in
          let* () =
            add_service
              cloud
              ~name:"Check TzKT API"
              ~url:(sf "%s/v1/head" (Client.string_of_endpoint tzkt_api))
          in
          let* () =
            add_service
              cloud
              ~name:"TzKT Explorer"
              ~url:
                (sf
                   "http://sandbox.tzkt.io/blocks?tzkt_api_url=%s"
                   (Client.url_encoded_string_of_endpoint tzkt_api))
          in
          if Cli.faucet then
            let () = toplog "Starting faucet" in
            let faucet_account = Constant.bootstrap1 in
            let faucet_pkh = faucet_account.public_key_hash in
            let faucet_private_key =
              match Constant.bootstrap1.secret_key with
              | Unencrypted key -> key
              | _ -> assert false
            in
            let* faucet_api =
              init_faucet_backend
                ~agent:tezlink_sequencer_agent
                ~tezlink_sandbox_endpoint
                ~faucet_private_key
            in
            let* () =
              add_service
                cloud
                ~name:"Faucet API"
                ~url:(Client.string_of_endpoint faucet_api)
            in
            let* () =
              add_service
                cloud
                ~name:"Check Faucet API"
                ~url:(sf "%s/info" (Client.string_of_endpoint faucet_api))
            in
            let* faucet_frontend =
              init_faucet_frontend
                ~agent:tezlink_sequencer_agent
                ~faucet_api
                ~tezlink_sandbox_endpoint
                ~faucet_pkh
                ~tzkt_api
            in
            let* () =
              add_service
                cloud
                ~name:"Faucet"
                ~url:(Client.string_of_endpoint faucet_frontend)
            in
            unit
          else unit
        else unit
      in
      let* () =
        match Tezt_cloud.Tezt_cloud_cli.dns_domains with
        | full_name :: _ when Tezt_cloud.Tezt_cloud_cli.(proxy || not localhost)
          ->
            let* ssl = Ssl.generate tezlink_sequencer_agent full_name in
            let* () =
              let proxy_pass =
                (* The trailing / is mandatory, otherwise the reverse proxy
                   won't be able to serve services with a path. *)
                Client.string_of_endpoint tezlink_sandbox_endpoint ^ "/"
              in
              let rpc_nginx_node =
                Nginx_reverse_proxy.simple_ssl_node
                  ~server_name:full_name
                  ~port:public_rpc_port
                  ~location:"/"
                  ~proxy_pass
                  ~certificate:ssl.certificate
                  ~certificate_key:ssl.key
              in
              Nginx_reverse_proxy.init
                ~agent:tezlink_sequencer_agent
                ~site:"tezlink"
                rpc_nginx_node
            in
            let () =
              toplog "SSL certificate: %s, SSL key: %s" ssl.certificate ssl.key
            in
            unit
        | _ -> unit
      in
      let () = toplog "Starting main loop" in
      loop 0)
