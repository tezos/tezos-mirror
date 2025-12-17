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

(* This is a temporary fix to https://linear.app/tezos/issue/L2-730.
   It allows to make sure the error message is printed when it occurs (with
   `Log.error`).
   Remove this module when the issue is fixed. *)
module Test = struct
  include Test

  exception Failed of string

  let fail ?__LOC__ x =
    Format.kasprintf
      (fun message ->
        let message =
          match __LOC__ with
          | None ->
              Log.error "%s" message ;
              message
          | Some loc ->
              let message = sf "%s: %s" loc message in
              Log.error "%s" message ;
              message
        in
        raise (Failed message))
      x
end

let build_endpoint ?path ~runner ~dns_domain port =
  let host = Option.value ~default:(Runner.address runner) dns_domain in
  Client.Foreign_endpoint (Endpoint.make ?path ~host ~scheme:"http" ~port ())

(* The sequencer and the TzKT API may be proxified by a Nginx
   reverse-proxy to support HTTPS and, in the case of the sequencer,
   add a /tezlink path prefix. *)
type proxy_internal_info = {port : int; path : string option}

type proxy_info =
  | No_proxy of proxy_internal_info
  | Proxy of {
      dns_domain : string;
      external_port : int;
      internal_info : proxy_internal_info;
      activate_ssl : bool;
    }

(* There are several dual functions that include either [internal] or [external]
   in their name, and that are used by services to fetch the end-point of other
   services. Each service is responsible for understanding when to use which, but
   here are a general rules.
   * Referencing a local end-point should be preferred as it's faster.
   * Sometimes, the end-point is simply not local (for instance when pointing to
     an external sequencer.
   * Some exposed services like Umami requires to reference a HTTPS TzKT API
     node, which means its external end-point. *)

let proxy_internal_info = function
  | No_proxy internal_info | Proxy {internal_info; _} -> internal_info

let proxy_internal_port proxy_info = (proxy_internal_info proxy_info).port

let proxy_internal_endpoint proxy_info =
  let host = "127.0.0.1" in
  let scheme = "http" in
  let {path; port} = proxy_internal_info proxy_info in
  Client.Foreign_endpoint (Endpoint.make ?path ~host ~scheme ~port ())

let proxy_external_endpoint ~runner = function
  | No_proxy {port; path} -> build_endpoint ?path ~runner ~dns_domain:None port
  | Proxy {dns_domain; external_port; activate_ssl; internal_info = _} ->
      let scheme = if activate_ssl then "https" else "http" in
      Client.Foreign_endpoint
        (Endpoint.make ~host:dns_domain ~scheme ~port:external_port ())

let nginx_reverse_proxy_config ~agent ~proxy =
  match proxy with
  | No_proxy _ -> Lwt.return_nil
  | Proxy {dns_domain; external_port; activate_ssl; internal_info = _} ->
      let internal_endpoint = proxy_internal_endpoint proxy in
      let proxy_pass =
        (* The trailing / is mandatory, otherwise the reverse proxy
           won't be able to serve services with a path. *)
        Client.string_of_endpoint internal_endpoint ^ "/"
      in
      if activate_ssl then
        let () =
          if not (List.mem dns_domain Tezt_cloud_cli.dns_domains) then
            Test.fail "Please add --dns-domain %s" dns_domain
        in
        let* ssl = Ssl.generate agent dns_domain in
        return
        @@ Nginx_reverse_proxy.simple_ssl_node
             ~server_name:dns_domain
             ~port:external_port
             ~location:"/"
             ~proxy_pass
             ~certificate:ssl.certificate
             ~certificate_key:ssl.key
      else
        return
        @@ Nginx_reverse_proxy.make_simple_config
             ~server_name:dns_domain
             ~port:external_port
             ~location:"/"
             ~proxy_pass

let port_of_option agent = function
  | None -> Agent.next_available_port agent
  | Some port -> port

let make_proxy agent ~path ~dns_domain public_port activate_ssl =
  match dns_domain with
  | None ->
      (* No DNS so no proxy, so we must use the public port. *)
      let public_port = port_of_option agent public_port in
      No_proxy {port = public_port; path}
  | Some dns_domain when Option.is_some public_port ->
      Test.fail
        "Setting a public port is only allowed for non-proxy localhost mode, \
         but was specified for %s."
        dns_domain
  | Some dns_domain ->
      (* We let the system choose a fresh internal node port.
         Note that it will be publicy exposed, it's just that we don't need to
         share this one. *)
      let internal_port = Agent.next_available_port agent in
      let public_port = if activate_ssl then 443 else 80 in
      Proxy
        {
          internal_info = {port = internal_port; path};
          dns_domain;
          external_port = public_port;
          activate_ssl;
        }

type service_endpoint =
  | Internal of proxy_info
  | External of string (* the address of the end-point *)

(* The internal end-point if the sequencer node is local to the scenario.
   Otherwise, i.e. when the scenario relies on an external end-point, return the
   latter. *)
let service_internal_endpoint = function
  | Internal proxy_info ->
      proxy_internal_endpoint proxy_info |> Client.string_of_endpoint
  | External endpoint -> endpoint

(* The exposed end-point if the sequencer node is local to the scenario.
   Otherwise, i.e. when the scenario relies on an external end-point, return the
   latter. *)
let service_external_endpoint ~runner = function
  | Internal proxy_info ->
      proxy_external_endpoint ~runner proxy_info |> Client.string_of_endpoint
  | External endpoint -> endpoint

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

module Bridge_frontend_process = struct
  module Parameters = struct
    type persistent_state = unit

    type session_state = unit

    let base_default_name = "bridge-frontend"

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
        "-c"; sf "cd %s && npx vite preview --host 127.0.0.1 --port %d" path port;
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

let init_tzkt ~tzkt_proxy ~agent ~sequencer_endpoint ~time_between_blocks =
  (* Set of functions helpful for Tzkt setup *)
  let run = run agent in
  let tezlink_sandbox_endpoint = service_internal_endpoint sequencer_endpoint in
  let tzkt_api_port = proxy_internal_port tzkt_proxy in
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

  (* Clone TZKT sources. *)
  let* () =
    git_clone agent ~branch:"master" "https://github.com/baking-bad/tzkt" "tzkt"
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
  (* Print a log for the tezt-cloud user to retrieve the port of the indexer or API *)
  let () = toplog "Tzkt indexer will be available at port %d" indexer_port in
  let () = toplog "Tzkt API will be available at port %d" tzkt_api_port in

  (* Prepare multiple argument for Tzkt indexer and API start. *)
  (* Set our endpoint for Tzkt instead of the default "https://rpc.tzkt.io/mainnet/".*)
  let endpoint_arg =
    tzkt_arg ["TezosNode"; "Endpoint"] [tezlink_sandbox_endpoint]
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
      [sf "http://0.0.0.0:%d" tzkt_api_port]
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
  unit

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

let create_env_file ~agent destination env =
  let temp_file = Temp.file @@ Format.sprintf "config_%s" destination in
  let ch = open_out temp_file in
  let config_filtered =
    List.filter_map
      (function
        | param, Some value -> Some (Format.sprintf "%s=%s" param value)
        | _, None -> None)
      env
  in
  let config_str = String.concat "\n" config_filtered in
  Format.fprintf (Format.formatter_of_out_channel ch) "%s\n" config_str ;
  let () = close_out ch in
  let* (_ : string) = Agent.copy agent ~source:temp_file ~destination in
  unit

let init_faucet_backend ~agent ~sequencer_endpoint ~faucet_private_key
    ~faucet_api_proxy =
  let tezlink_sandbox_endpoint = service_internal_endpoint sequencer_endpoint in
  let faucet_api_port = proxy_internal_port faucet_api_proxy in
  let faucet_backend_dir = "faucet-backend" in
  (* Clone faucet backend from personal fork because upstream depends on a RPC which we don't yet support (forge_operation). *)
  let* () =
    git_clone
      agent
      ~branch:"rafoo@local_forging"
      "https://github.com/rafoo/tezos-faucet-backend.git"
      faucet_backend_dir
  in
  let config =
    [
      ("API_PORT", sf "%d" faucet_api_port);
      ("RPC_URL", tezlink_sandbox_endpoint);
      ("FAUCET_PRIVATE_KEY", faucet_private_key);
      ("AUTHORIZED_HOST", {|"*"|});
      ("ENABLE_CAPTCHA", "false");
      ("DISABLE_CHALLENGES", "true");
      ("MAX_BALANCE", "6000");
      ("MIN_TEZ", "1");
      ("MAX_TEZ", "6000");
      ("MIN_CHALLENGES", "1");
      ("MAX_CHALLENGES", "550");
      ("MAX_CHALLENGES_WITH_CAPTCHA", "66");
      ("CHALLENGE_SIZE", "2048");
      ("DIFFICULTY", "4");
    ]
  in
  let* () =
    create_env_file
      ~agent
      (sf "%s/.env" faucet_backend_dir)
      (List.map (fun (arg, param) -> (arg, Some param)) config)
  in
  let* () =
    run_cmd
      agent
      (sf "cd %s && npm install typescript && npm run build" faucet_backend_dir)
  in
  let runner = Agent.runner agent in
  Faucet_backend_process.run ?runner ~path:faucet_backend_dir ()

let init_bridge_frontend ~agent ~network ~l1_endpoint ~bridge_contract
    ~tzkt_api_url ~rollup ~bridge_proxy =
  let bridge_frontend_port = proxy_internal_port bridge_proxy in
  let bridge_dir = "bridge-tezlink" in
  (* Clone bridge frontend *)
  let* () =
    git_clone
      agent
      "https://github.com/luciano-fs/tezlink_deposit.git"
      bridge_dir
  in
  let* () =
    create_env_file
      ~agent
      (sf "%s/.env" bridge_dir)
      [
        ("VITE_ENDPOINT", l1_endpoint);
        ("VITE_CONTRACT", Some bridge_contract);
        ("VITE_TZKT", tzkt_api_url);
        ("VITE_ROLLUP", Some rollup);
        ("VITE_NETWORK", network);
      ]
  in
  let* () =
    run_cmd agent (sf "cd %s && npm install && npm run build" bridge_dir)
  in
  let runner = Agent.runner agent in
  Bridge_frontend_process.run
    ?runner
    ~path:bridge_dir
    ~port:bridge_frontend_port
    ()

let init_faucet_frontend ~faucet_api_proxy ~agent ~sequencer_endpoint
    ~faucet_pkh ~tzkt_api_endpoint ~tzkt_url ~faucet_frontend_proxy =
  let runner = Agent.runner agent in
  let faucet_api = proxy_external_endpoint ~runner faucet_api_proxy in
  let tezlink_sandbox_endpoint =
    service_external_endpoint ~runner sequencer_endpoint
  in
  let faucet_frontend_port = proxy_internal_port faucet_frontend_proxy in
  let viewer =
    let in_tzkt_sandbox api_url =
      sf "http://sandbox.tzkt.io/<hash>?tzkt_api_url=%s" api_url
    in
    match (tzkt_api_endpoint, tzkt_url) with
    | _, Some tzkt_url -> Filename.concat tzkt_url "<hash>"
    | Internal tzkt_api_proxy, None ->
        let tzkt_api = proxy_external_endpoint ~runner tzkt_api_proxy in
        let api_url = Client.url_encoded_string_of_endpoint tzkt_api in
        in_tzkt_sandbox api_url
    | External tzkt_api_url, None -> in_tzkt_sandbox tzkt_api_url
  in
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
      "viewer": "%s"
    }
}
|}
      (Client.string_of_endpoint faucet_api)
      tezlink_sandbox_endpoint
      faucet_pkh
      viewer
  in
  let* () =
    run_cmd
      agent
      (sf
         "cd %s && npm install && npm run build && npm run check-config && \
          mkdir -p build && cp public/config.json build/"
         faucet_frontend_dir)
  in
  Faucet_frontend_process.run
    ?runner
    ~path:faucet_frontend_dir
    ~port:faucet_frontend_port
    ()

(* It's not ideal, but the path to Umami is set in the dockerfile. We have to be
   careful if we ever update it. *)
let remote_umami_path = "/tmp/umami-v2"

module Umami_process = struct
  module Parameters = struct
    type persistent_state = unit

    type session_state = unit

    let base_default_name = "umami"

    let default_colors = Evm_node.daemon_default_colors
  end

  include Daemon.Make (Parameters)

  let run ?runner ~port () =
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
          "cd %s/apps/web && turbo preview -- --host 127.0.0.1 --port %d"
          remote_umami_path
          port;
      ]
end

let umami_patch ~rpc_url ~tzkt_api_url ~tzkt_url ~faucet_url =
  (* The Tezlink TzKT explorer requires some URL parameters suffixed to the
     different paths (block, contract, etc.), so it doesn't plug well into Umami
     for now. *)
  sf
    {|
diff --git a/packages/state/src/slices/networks.ts b/packages/state/src/slices/networks.ts
index 3453eabc..1967b769 100644
--- a/packages/state/src/slices/networks.ts
+++ b/packages/state/src/slices/networks.ts
@@ -1,5 +1,5 @@
 import { createSlice } from "@reduxjs/toolkit";
-import { DefaultNetworks, MAINNET, type Network, isDefault } from "@umami/tezos";
+import { DefaultNetworks, TEZLINK, type Network, isDefault } from "@umami/tezos";
 import { remove } from "lodash";
 
 type State = {
@@ -9,7 +9,7 @@ type State = {
 
 const initialState: State = {
   available: DefaultNetworks,
-  current: MAINNET,
+  current: TEZLINK,
 };
 
 export const networksSlice = createSlice({
diff --git a/packages/tezos/src/Network.ts b/packages/tezos/src/Network.ts
index 1d28850f..39a15d9b 100644
--- a/packages/tezos/src/Network.ts
+++ b/packages/tezos/src/Network.ts
@@ -16,6 +16,23 @@ export const GHOSTNET: Network = {
   buyTezUrl: "https://faucet.ghostnet.teztnets.com/",
 };
 
+export const TEZOS_SHADOWNET: Network = {
+  name: "shadownet",
+  rpcUrl: "https://shadownet.tezos.ecadinfra.com",
+  tzktApiUrl: "https://api.shadownet.tzkt.io",
+  tzktExplorerUrl: "https://shadownet.tzkt.io",
+  buyTezUrl: "https://faucet.shadownet.teztnets.com/",
+};
+
+
+export const TEZLINK: Network = {
+  name: "custom",
+  rpcUrl: "%s",
+  tzktApiUrl: "%s",
+  tzktExplorerUrl: "%s",
+  buyTezUrl: "%s",
+};
+
 export const isDefault = (network: Network) => !!DefaultNetworks.find(n => n.name === network.name);
 
-export const DefaultNetworks: Network[] = [MAINNET, GHOSTNET];
+export const DefaultNetworks: Network[] = [MAINNET, GHOSTNET, TEZOS_SHADOWNET, TEZLINK];
    |}
    rpc_url
    tzkt_api_url
    tzkt_url
    faucet_url

let init_umami agent ~sequencer_endpoint ~tzkt_api_endpoint ~tzkt_url
    ~umami_proxy ~faucet_proxy_opt =
  let runner = Agent.runner agent in
  let rpc_url = service_external_endpoint ~runner sequencer_endpoint in
  let tzkt_api_url = service_external_endpoint ~runner tzkt_api_endpoint in
  (* Umami can directly reference an explorer, and will concatenate the operation
     hash of a transaction. This means that it does not work with the sandbox
     scheme where the suffix is the TzKT API parameter. In this case, we don't
     point to an explorer and simply leave the value empty (`""`). *)
  let tzkt_url = Option.value ~default:"" tzkt_url in
  let faucet_url =
    Option.map
      (fun proxy ->
        proxy_external_endpoint ~runner proxy |> Client.string_of_endpoint)
      faucet_proxy_opt
    |> Option.value ~default:""
  in
  let patch = umami_patch ~rpc_url ~tzkt_api_url ~tzkt_url ~faucet_url in
  (* Create a local patch file with its contents. *)
  let patch_filename = Temp.file "umami.patch" in
  let out_chan = Stdlib.open_out patch_filename in
  let patch_ppf = Format.formatter_of_out_channel out_chan in
  Format.pp_print_string patch_ppf patch ;
  close_out out_chan ;
  (* Upload and delete the patch file. *)
  let patch_dst = "/tmp/umami.patch" in
  let* _ = Agent.copy ~destination:patch_dst ~source:patch_filename agent in
  let* () = Process.spawn "rm" [patch_filename] |> Process.check in
  (* Apply the patch. *)
  let* () =
    run_cmd agent (sf "cd %s && git apply %s" remote_umami_path patch_dst)
  in
  (* Run Umami. *)
  let port = proxy_internal_port umami_proxy in
  Umami_process.run ?runner ~port ()

let init_tezlink_sequencer (cloud : Cloud.t) (name : string)
    ~(sequencer_proxy : proxy_info) (verbose : bool)
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
  let rpc_port = proxy_internal_port sequencer_proxy in
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
      ~rpc_port
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

type faucet_proxys = {
  tzkt_api_endpoint : service_endpoint;
  faucet_api_proxy : proxy_info;
  faucet_frontend_proxy : proxy_info;
}

type umami_proxys = {
  tzkt_api_endpoint : service_endpoint;
  umami_proxy : proxy_info;
}

let add_service cloud ~name ~url =
  let () = toplog "New service: %s: %s" name url in
  Cloud.add_service cloud ~name ~url

let add_proxy_service cloud runner name ?(url = Fun.id) proxy =
  let endpoint =
    Client.string_of_endpoint (proxy_external_endpoint ~runner proxy)
  in
  add_service cloud ~name ~url:(url endpoint)

let add_services cloud runner ~sequencer_endpoint ~tzkt_api_proxy_opt
    ~faucet_proxys_opt ~umami_proxys_opt ~bridge_proxy_opt =
  let add_proxy_service = add_proxy_service cloud runner in
  let* () =
    let name_check = "Check Tezlink RPC endpoint" in
    let name_endpoint = "Tezlink RPC endpoint" in
    match sequencer_endpoint with
    | Internal sequencer_proxy ->
        let* () =
          add_proxy_service name_check ~url:(sf "%s/version") sequencer_proxy
        in
        add_proxy_service name_endpoint sequencer_proxy
    | External sequencer_endpoint ->
        let* () =
          add_service
            cloud
            ~name:name_check
            ~url:(sf "%s/version" sequencer_endpoint)
        in
        add_service cloud ~name:name_endpoint ~url:sequencer_endpoint
  in
  let* () =
    match tzkt_api_proxy_opt with
    | None -> unit
    | Some tzkt_proxy ->
        let* () = add_proxy_service "TzKT API" tzkt_proxy in
        let* () =
          add_proxy_service "Check TzKT API" ~url:(sf "%s/v1/head") tzkt_proxy
        in
        add_proxy_service
          "TzKT Explorer"
          ~url:(sf "http://sandbox.tzkt.io/blocks?tzkt_api_url=%s")
          tzkt_proxy
  in
  let* () =
    match faucet_proxys_opt with
    | None -> unit
    | Some {faucet_api_proxy; faucet_frontend_proxy; _} ->
        let* () = add_proxy_service "Faucet API" faucet_api_proxy in
        let* () =
          add_proxy_service
            "Check Faucet API"
            ~url:(sf "%s/info")
            faucet_api_proxy
        in
        add_proxy_service "Faucet" faucet_frontend_proxy
  in
  let* () =
    match umami_proxys_opt with
    | None -> unit
    | Some {umami_proxy; _} -> add_proxy_service "Umami" umami_proxy
  in
  let* () =
    match bridge_proxy_opt with
    | None -> unit
    | Some bridge_proxy -> add_proxy_service "Bridge" bridge_proxy
  in
  unit

type dns_domains = {
  sequencer_domain : string;
  tzkt_api_domain : string;
  faucet_domain : string;
  faucet_api_domain : string;
  umami_domain : string;
  bridge_domain : string;
}

let nginx_config_of_proxy_opt agent = function
  | None -> Lwt.return_nil
  | Some proxy -> nginx_reverse_proxy_config ~agent ~proxy

let nginx_config_of_service_endpoint agent = function
  | Internal proxy -> nginx_reverse_proxy_config ~agent ~proxy
  | External _ -> Lwt.return_nil

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
      let runner = Agent.runner tezlink_sequencer_agent in
      if
        Tezt_cloud.Tezt_cloud_cli.(
          (not proxy) && localhost && not (List.is_empty dns_domains))
      then
        Test.fail
          "Setting DNS domains in non-proxy localhost mode is unexpected, \
           please remove the `--dns-domain` option from the command line." ;
      let dns_domains =
        Option.map
          (fun parent_domain ->
            let make_domain domain = sf "%s.%s" domain parent_domain in
            {
              sequencer_domain = make_domain "node";
              tzkt_api_domain = make_domain "api.tzkt";
              faucet_domain = make_domain "faucet";
              faucet_api_domain = make_domain "faucet.api";
              umami_domain = make_domain "umami";
              bridge_domain = make_domain "bridge";
            })
          Cli.parent_dns_domain
      in
      let activate_ssl = Cli.activate_ssl in
      let sequencer_endpoint =
        match Cli.external_sequencer_endpoint with
        | None ->
            let sequencer_proxy =
              make_proxy
                tezlink_sequencer_agent
                ~path:(Some "/tezlink")
                ~dns_domain:
                  (Option.map (fun doms -> doms.sequencer_domain) dns_domains)
                Cli.public_rpc_port
                activate_ssl
            in
            Internal sequencer_proxy
        | Some endpoint -> External endpoint
      in
      let* tzkt_api_proxy_opt =
        if Cli.tzkt then
          let tzkt_proxy =
            make_proxy
              tezlink_sequencer_agent
              ~path:None
              ~dns_domain:
                (Option.map (fun doms -> doms.tzkt_api_domain) dns_domains)
              Cli.tzkt_api_port
              activate_ssl
          in
          some tzkt_proxy
        else none
      in
      let* tzkt_api_endpoint_opt =
        match Cli.external_tzkt_api with
        | Some _ when Cli.tzkt ->
            Test.fail
              ~__LOC__
              "TzKT has been specified to be run both locally (--tzkt) and \
               externally (--external-tzkt-api)"
        | Some external_tzkt_api -> some (External external_tzkt_api)
        | None when Cli.tzkt ->
            let tzkt_proxy =
              make_proxy
                tezlink_sequencer_agent
                ~path:None
                ~dns_domain:
                  (Option.map (fun doms -> doms.tzkt_api_domain) dns_domains)
                Cli.tzkt_api_port
                activate_ssl
            in
            some (Internal tzkt_proxy)
        | None -> none
      in
      let* faucet_proxys_opt =
        match (tzkt_api_endpoint_opt, Cli.faucet) with
        | Some tzkt_api_endpoint, true ->
            let faucet_api_proxy =
              make_proxy
                tezlink_sequencer_agent
                ~path:None
                ~dns_domain:
                  (Option.map (fun doms -> doms.faucet_api_domain) dns_domains)
                None
                activate_ssl
            in
            let faucet_frontend_proxy =
              make_proxy
                tezlink_sequencer_agent
                ~path:None
                ~dns_domain:
                  (Option.map (fun doms -> doms.faucet_domain) dns_domains)
                None
                activate_ssl
            in
            some {tzkt_api_endpoint; faucet_api_proxy; faucet_frontend_proxy}
        | None, true ->
            Test.fail
              "The faucet service relies on TzKT, but the latter is \
               deactivated (see the --tzkt option)."
        | (None | Some _), false -> none
      in
      let* umami_proxys_opt =
        match tzkt_api_endpoint_opt with
        | Some tzkt_api_endpoint when Cli.umami ->
            let umami_proxy =
              make_proxy
                tezlink_sequencer_agent
                ~path:None
                ~dns_domain:
                  (Option.map (fun doms -> doms.umami_domain) dns_domains)
                None
                activate_ssl
            in
            some {tzkt_api_endpoint; umami_proxy}
        | None when Cli.umami ->
            Test.fail
              "Umami relies on TzKT, but the latter is deactivated (see the \
               --tzkt option)."
        | _ -> none
      in
      let* bridge_proxy_opt =
        match (Cli.deposit_frontend, Cli.external_sequencer_endpoint) with
        | false, _ -> none
        | true, None ->
            Test.fail
              "Can't deploy the bridge frontend because \
               --external-sequencer-endpoint is missing. You can't do deposit \
               on a sandbox sequencer so the bridge would be useless."
        | true, Some _ ->
            let bridge_proxy =
              make_proxy
                tezlink_sequencer_agent
                ~path:None
                ~dns_domain:
                  (Option.map (fun doms -> doms.bridge_domain) dns_domains)
                None
                activate_ssl
            in
            some bridge_proxy
      in
      let* () =
        add_services
          cloud
          runner
          ~sequencer_endpoint
          ~tzkt_api_proxy_opt
          ~faucet_proxys_opt
          ~umami_proxys_opt
          ~bridge_proxy_opt
      in
      let () = toplog "Starting Tezlink sequencer" in
      let* () =
        match sequencer_endpoint with
        | Internal sequencer_proxy ->
            init_tezlink_sequencer
              cloud
              name
              ~sequencer_proxy
              Cli.verbose
              Cli.time_between_blocks
              tezlink_sequencer_agent
        | External _ -> unit
      and* () =
        match tzkt_api_endpoint_opt with
        | None | Some (External _) -> unit
        | Some (Internal tzkt_proxy) ->
            let () = toplog "Starting TzKT" in
            init_tzkt
              ~tzkt_proxy
              ~agent:tezlink_sequencer_agent
              ~sequencer_endpoint
              ~time_between_blocks:Cli.time_between_blocks
      and* () =
        match umami_proxys_opt with
        | None -> unit
        | Some {tzkt_api_endpoint; umami_proxy} ->
            init_umami
              tezlink_sequencer_agent
              ~sequencer_endpoint
              ~tzkt_api_endpoint
              ~tzkt_url:Cli.external_tzkt
              ~umami_proxy
              ~faucet_proxy_opt:
                (Option.map
                   (fun proxys -> proxys.faucet_frontend_proxy)
                   faucet_proxys_opt)
      and* () =
        match faucet_proxys_opt with
        | None when Cli.faucet_private_key <> None ->
            Test.fail
              "The private key for the faucet is set but the faucet option \
               itself is disabled."
        | None -> unit
        | Some {tzkt_api_endpoint; faucet_api_proxy; faucet_frontend_proxy} ->
            let () = toplog "Starting faucet" in
            let faucet_account = Constant.bootstrap1 in
            let faucet_pkh = faucet_account.public_key_hash in
            let default =
              match Constant.bootstrap1.secret_key with
              | Unencrypted key -> key
              | _ -> assert false
            in
            let faucet_private_key =
              Option.value Cli.faucet_private_key ~default
            in
            let* () =
              init_faucet_backend
                ~agent:tezlink_sequencer_agent
                ~sequencer_endpoint
                ~faucet_private_key
                ~faucet_api_proxy
            in
            let* () =
              init_faucet_frontend
                ~agent:tezlink_sequencer_agent
                ~faucet_api_proxy
                ~sequencer_endpoint
                ~faucet_pkh
                ~tzkt_api_endpoint
                ~tzkt_url:Cli.external_tzkt
                ~faucet_frontend_proxy
            in
            unit
      and* () =
        match bridge_proxy_opt with
        | None -> unit
        | Some bridge_proxy ->
            let () = toplog "Starting bridge frontend" in
            let bridge_contract, rollup =
              match (Cli.bridge_contract, Cli.rollup_address) with
              | Some bridge_contract, Some rollup_address ->
                  (bridge_contract, rollup_address)
              | _ ->
                  Test.fail
                    "Options --bridge-contract and --rollup are required to \
                     start the bridge frontend."
            in
            let _ =
              init_bridge_frontend
                ~agent:tezlink_sequencer_agent
                ~network:Cli.l1_network
                ~l1_endpoint:Cli.l1_endpoint
                ~bridge_contract
                ~tzkt_api_url:Cli.l1_tzkt_api
                ~rollup
                ~bridge_proxy
            in
            unit
      in
      let* () =
        let* rpc_nginx_config =
          nginx_config_of_service_endpoint
            tezlink_sequencer_agent
            sequencer_endpoint
        in
        let* tzkt_nginx_config =
          match tzkt_api_endpoint_opt with
          | None -> Lwt.return_nil
          | Some tzkt_api_endpoint ->
              nginx_config_of_service_endpoint
                tezlink_sequencer_agent
                tzkt_api_endpoint
        in
        let* faucet_api_nginx_config =
          let faucet_api_proxy =
            Option.map (fun proxys -> proxys.faucet_api_proxy) faucet_proxys_opt
          in
          nginx_config_of_proxy_opt tezlink_sequencer_agent faucet_api_proxy
        in
        let* faucet_nginx_config =
          let faucet_proxy =
            Option.map
              (fun proxys -> proxys.faucet_frontend_proxy)
              faucet_proxys_opt
          in
          nginx_config_of_proxy_opt tezlink_sequencer_agent faucet_proxy
        in
        let* umami_nginx_config =
          let umami_proxy =
            Option.map (fun proxys -> proxys.umami_proxy) umami_proxys_opt
          in
          nginx_config_of_proxy_opt tezlink_sequencer_agent umami_proxy
        in
        let* bridge_nginx_config =
          nginx_config_of_proxy_opt tezlink_sequencer_agent bridge_proxy_opt
        in
        match
          rpc_nginx_config @ tzkt_nginx_config @ faucet_api_nginx_config
          @ faucet_nginx_config @ umami_nginx_config @ bridge_nginx_config
        with
        | [] -> unit
        | nginx_configs ->
            Nginx_reverse_proxy.init
              ~agent:tezlink_sequencer_agent
              ~site:"tezlink"
              nginx_configs
      in
      let () = toplog "Starting main loop" in
      loop 0)
