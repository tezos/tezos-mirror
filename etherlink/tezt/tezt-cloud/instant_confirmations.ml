(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(**
   This scenario runs an Etherlink sandbox with preconfirmation enabled and
   benchmarks the eth_sendRawTransactionSync RPC latency.

   To run this tezt-cloud scenario, first setup your gcloud environment:
   {v
     gcloud auth application-default login
     gcloud config set project <your-project>
   v}

   The scenario can then be started with:
   {v
     dune exec etherlink/tezt/tezt-cloud/main.exe -- \
       --title "Instant confirmations benchmark" \
       --tezt-cloud ic-bench \
       --dockerfile-alias etherlink \
       --no-prometheus --no-grafana \
       --verbose --destroy [--macosx] [--localhost]
   v}

   The test will:
   - Set up a sandbox sequencer and observer with preconfirmation enabled
   - Run the benchmark script directly on the VM (to eliminate network latency)
   - Measure and report P80 and P99 latency percentiles
*)

open Rpc.Syntax

(* Evm_node wrapper for cloud compatibility *)
module Evm_node_cloud = struct
  include Evm_node

  module Agent = struct
    let create ?(group = "Etherlink") ?(copy_binary = true)
        ?(path = Uses.path Constant.octez_evm_node) ?name ?data_dir ~mode
        ?rpc_port ?websockets ?initial_kernel ?preimages_dir ?private_rpc_port
        ?tx_queue_tx_per_addr_limit cloud agent =
      let* path =
        if copy_binary then Agent.copy agent ~source:path
        else
          return Filename.(concat Tezt_cloud_cli.binaries_path (basename path))
      in
      let* () =
        Cloud.register_binary
          cloud
          ~agents:[agent]
          ~group
          ~name:(Filename.basename path)
          ()
      in
      let runner = Agent.runner agent in
      let rpc_port =
        match rpc_port with
        | None ->
            let port = Agent.next_available_port agent in
            port
        | Some port -> port
      in
      let node_setup =
        make_setup
          ~path
          ?name
          ?runner
          ?data_dir
          ~rpc_port
          ?websockets
          ?initial_kernel
          ?preimages_dir
          ?private_rpc_port
          ?tx_queue_tx_per_addr_limit
          ()
      in
      create ~node_setup ~mode () |> Lwt.return

    let init ?patch_config ?name ~mode ?websockets ?data_dir ?rpc_port ?wait
        ?extra_arguments ?copy_binary ?initial_kernel ?preimages_dir
        ?private_rpc_port ?tx_queue_tx_per_addr_limit cloud agent =
      let* evm_node =
        create
          ?name
          ?copy_binary
          ~mode
          ?rpc_port
          ?websockets
          ?data_dir
          ?initial_kernel
          ?preimages_dir
          ?private_rpc_port
          ?tx_queue_tx_per_addr_limit
          cloud
          agent
      in
      let* () = Process.check @@ spawn_init_config evm_node in
      let* () =
        match patch_config with
        | Some patch_config -> Config_file.update evm_node patch_config
        | None -> unit
      in
      let* () = run ?wait ?extra_arguments evm_node in
      return evm_node
  end
end

(* Send transactions to sequencer at a given TPS rate until cancelled *)
let send_transactions_at_tps ~tps ~sender evm_node =
  let* gas_price = Rpc.get_gas_price evm_node in
  let receiver = Eth_account.bootstrap_accounts.(2) in
  let delay = 1.0 /. float tps in
  Log.info "Sending transactions at %d TPS to sequencer" tps ;
  let rec loop nonce =
    let* raw_tx =
      Cast.craft_tx
        ~source_private_key:sender.Eth_account.private_key
        ~chain_id:1337
        ~nonce
        ~gas_price:(Int32.to_int gas_price)
        ~gas:21_000
        ~value:(Wei.of_eth_int 1)
        ~address:receiver.address
        ()
    in
    let* _hash = Rpc.send_raw_transaction ~raw_tx evm_node in
    let* () = Lwt_unix.sleep delay in
    loop (nonce + 1)
  in
  loop 0

(* Prepare kernel and wallet locally *)
let prepare_kernel () =
  let local_wallet_dir = Temp.dir "wallet" in
  let local_preimages_dir = Temp.dir "wasm_2_0_0" in
  let output_config = Temp.file "config.yaml" in
  let eth_bootstrap_accounts =
    Eth_account.bootstrap_accounts |> Array.to_list
    |> List.map (fun a -> a.Eth_account.address)
  in
  let*! () =
    Evm_node.make_kernel_installer_config
      ~output:output_config
      ~eth_bootstrap_accounts
      ~sequencer:Constant.bootstrap1.Account.public_key
      ()
  in
  let* {output = local_kernel; _} =
    Sc_rollup_helpers.prepare_installer_kernel
      ~preimages_dir:local_preimages_dir
      ~config:(`Path output_config)
      Constant.WASM.evm_kernel
  in
  let () = Account.write Constant.all_secret_keys ~base_dir:local_wallet_dir in
  return (local_kernel, local_preimages_dir, local_wallet_dir)

(* Setup sandbox sequencer with preconfirmation enabled *)
let setup_sandbox cloud agent ~local_kernel ~local_preimages_dir
    ~local_wallet_dir =
  let rpc_port = Agent.next_available_port agent in
  let private_rpc_port = Agent.next_available_port agent in
  Log.info
    "setup_sandbox: rpc_port=%d private_rpc_port=%d"
    rpc_port
    private_rpc_port ;
  (* Copy kernel, preimages, and wallet to the remote agent *)
  let* initial_kernel = Agent.copy agent ~source:local_kernel in
  let* preimages_dir =
    Agent.copy agent ~is_directory:true ~source:local_preimages_dir
  in
  let* remote_wallet_dir =
    Agent.copy agent ~is_directory:true ~source:local_wallet_dir
  in
  let mode =
    Evm_node.Sandbox
      {
        network = None;
        funded_addresses = [];
        sequencer_keys =
          [Account.uri_of_secret_key Constant.bootstrap1.Account.secret_key];
        sequencer_config =
          {
            time_between_blocks = Some (Time_between_blocks 0.5);
            genesis_timestamp = None;
            max_number_of_chunks = None;
            wallet_dir = Some remote_wallet_dir;
          };
      }
  in
  Evm_node_cloud.Agent.init
    ~patch_config:(fun json ->
      json
      |> Evm_node.patch_config_with_experimental_feature
           ~preconfirmation_stream_enabled:true
           ()
      |> JSON.update
           "private_rpc"
           (JSON.update "addr" (fun _ ->
                JSON.annotate
                  ~origin:"instant_confirmations"
                  (`String "0.0.0.0"))))
    ~mode
    ~rpc_port
    ~private_rpc_port
    ~initial_kernel
    ~preimages_dir
    ~tx_queue_tx_per_addr_limit:5000
    ~name:"sandbox"
    cloud
    agent

(* Setup observer connected to sandbox *)
let setup_observer cloud agent ~sandbox ~sandbox_agent ~local_kernel
    ~local_preimages_dir =
  let rpc_port = Agent.next_available_port agent in
  let private_rpc_port = Agent.next_available_port agent in
  Log.info
    "setup_observer: rpc_port=%d private_rpc_port=%d"
    rpc_port
    private_rpc_port ;
  (* Copy kernel and preimages to the observer agent *)
  let* initial_kernel = Agent.copy agent ~source:local_kernel in
  let* preimages_dir =
    Agent.copy agent ~is_directory:true ~source:local_preimages_dir
  in
  (* Get sandbox endpoint for cross-agent communication *)
  let sandbox_endpoint =
    let sandbox_host =
      if Tezt_cloud_cli.localhost then
        (* Local Docker mode: use container name for Docker DNS resolution *)
        Format.asprintf
          "teztcloud-%s-%s"
          Tezt_cloud_cli.tezt_cloud
          (Agent.name sandbox_agent)
      else
        (* GCP mode: use actual VM IP address *)
        let sandbox_runner = Agent.runner sandbox_agent |> Option.get in
        sandbox_runner.Runner.address
    in
    let sandbox_port = Evm_node.rpc_port sandbox in
    Format.asprintf "http://%s:%d" sandbox_host sandbox_port
  in
  let mode =
    Evm_node.Observer
      {rollup_node_endpoint = None; evm_node_endpoint = sandbox_endpoint}
  in
  Log.info "Observer connecting to sandbox at %s" sandbox_endpoint ;
  Evm_node_cloud.Agent.init
    ~patch_config:(fun json ->
      json
      |> Evm_node.patch_config_with_experimental_feature
           ~preconfirmation_stream_enabled:true
           ()
      |> JSON.update
           "private_rpc"
           (JSON.update "addr" (fun _ ->
                JSON.annotate
                  ~origin:"instant_confirmations"
                  (`String "0.0.0.0"))))
    ~extra_arguments:["--sandbox"]
    ~mode
    ~rpc_port
    ~private_rpc_port
    ~initial_kernel
    ~preimages_dir
    ~tx_queue_tx_per_addr_limit:5000
    ~name:"observer"
    cloud
    agent

(* Run the existing benchmark script on the remote VM *)
let run_remote_benchmark agent ~nb_transactions ~percentile ~private_key
    observer =
  let runner = Agent.runner agent |> Option.get in

  (* Path to the existing benchmark script *)
  let local_script = "etherlink/scripts/benchmark_instant_confirmations.sh" in
  let local_erc20_bin = "etherlink/scripts/contracts/bin/LatencyERC20.bin" in

  (* Copy script and ERC20 binary to VM *)
  let* remote_script = Agent.copy agent ~source:local_script in
  let* _remote_erc20_bin = Agent.copy agent ~source:local_erc20_bin in

  (* Get local endpoint URL (127.0.0.1 on the VM) *)
  let local_endpoint = Evm_node.rpc_endpoint ~local:true observer in
  Log.info "Running benchmark on VM with endpoint %s" local_endpoint ;

  (* Run the benchmark script on the VM *)
  let remote_output_dir = "/tmp/ic_benchmark" in
  let cmd =
    Printf.sprintf
      "NODE_URL=%s OUTPUT_DIR=%s %s %s %d %d"
      local_endpoint
      remote_output_dir
      remote_script
      private_key
      nb_transactions
      percentile
  in
  Log.info "Running benchmark command: %s" cmd ;
  let process = Process.spawn ~runner "sh" ["-c"; cmd] in
  (* Read stdout and stderr concurrently, then wait for the process *)
  let stdout_promise = Lwt_io.read (Process.stdout process) in
  let stderr_promise = Lwt_io.read (Process.stderr process) in
  let* stdout, stderr = Lwt.both stdout_promise stderr_promise in
  let* status = Process.wait process in
  (match status with
  | Unix.WEXITED 0 ->
      Log.info "Benchmark script completed successfully" ;
      if stdout <> "" then Log.info "Script stdout:\n%s" stdout ;
      if stderr <> "" then Log.warn "Script stderr:\n%s" stderr
  | Unix.WEXITED code | Unix.WSIGNALED code | Unix.WSTOPPED code ->
      Log.error "Script stdout:\n%s" stdout ;
      Log.error "Script stderr:\n%s" stderr ;
      Test.fail "Benchmark script failed with exit code %d" code) ;

  (* Copy results back *)
  let local_results_dir = Temp.dir "ic_benchmark_results" in
  let* () =
    Agent.scp
      agent
      ~is_directory:true
      ~source:remote_output_dir
      ~destination:local_results_dir
      `FromRunner
  in

  (* Parse results from CSV (index,latency_ms,tx_hash) *)
  let csv_file = local_results_dir // "ic_benchmark" // "results.csv" in
  let contents = Tezt.Base.read_file csv_file in
  let latencies =
    String.split_on_char '\n' contents
    |> List.filter (fun s ->
           String.length s > 0 && not (String.starts_with ~prefix:"index" s))
    |> List.filter_map (fun line ->
           match String.split_on_char ',' line with
           | [_index; latency_ms; _tx_hash] -> (
               try Some (float_of_string latency_ms) with _ -> None)
           | _ -> None)
  in
  return (latencies, remote_output_dir)

let register () =
  let name = "instant-confirmations" in
  Cloud.register
    ~vms:(fun () ->
      (* To mimic the colocated scenario we spawn 2 agents *)
      return
        [
          (* Sequencer: default configuration *)
          Agent.Configuration.make ();
          (* Observer: hyperdisk *)
          Agent.Configuration.make
            ~machine_type:"c4d-standard-8"
            ~disk_type:"hyperdisk-balanced"
            ~name
            ();
        ])
    ~__FILE__
    ~title:"Instant confirmations benchmark"
    ~tags:["instant_confirmations"; "benchmark"; "etherlink"]
  @@ fun cloud ->
  let sandbox_agent = List.nth (Cloud.agents cloud) 0 in
  let observer_agent = List.nth (Cloud.agents cloud) 1 in

  (* Prepare kernel locally *)
  let* local_kernel, local_preimages_dir, local_wallet_dir =
    prepare_kernel ()
  in

  (* Setup sandbox and observer *)
  let* sandbox =
    setup_sandbox
      cloud
      sandbox_agent
      ~local_kernel
      ~local_preimages_dir
      ~local_wallet_dir
  in
  let* () = Evm_node.wait_for_ready sandbox in
  let* observer =
    setup_observer
      cloud
      observer_agent
      ~sandbox
      ~sandbox_agent
      ~local_kernel
      ~local_preimages_dir
  in
  let* () = Evm_node.wait_for_ready observer in
  let* () = Evm_node.wait_for_blueprint_applied observer 0 in
  let*@ _ = Rpc.produce_block sandbox in
  let* () = Evm_node.wait_for_blueprint_applied observer 1 in

  (* Use different accounts to avoid nonce conflicts *)
  let sequencer_sender = Eth_account.bootstrap_accounts.(0) in
  let observer_sender = Eth_account.bootstrap_accounts.(1) in

  (* Run in parallel:
     - Sequencer: continuous transactions at TPS rate (stops when benchmark completes)
     - Observer: benchmark with latency measurement (waits for receipts) *)
  let* _latencies, remote_results_dir =
    Lwt.pick
      [
        send_transactions_at_tps ~tps:20 ~sender:sequencer_sender sandbox;
        run_remote_benchmark
          observer_agent
          ~nb_transactions:100
          ~percentile:99
          ~private_key:observer_sender.private_key
          observer;
      ]
  in

  (* SCP results from observer VM to local machine *)
  let local_output = "ic_benchmark_results" in
  let* () =
    Agent.scp
      observer_agent
      ~is_directory:true
      ~source:remote_results_dir
      ~destination:local_output
      `FromRunner
  in
  Log.info "Results saved to %s" local_output ;

  unit

let () = register ()
