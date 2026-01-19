(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(* SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>              *)
(*                                                                           *)
(*****************************************************************************)

(**
   This scenario runs an Etherlink sandbox and benchmarks the node capacity
   using the UniswapV2 contracts.

   To run this tezt-cloud scenario, first setup your gcloud environmenmt
   {v
     gcloud auth application-default login
     gcloud config set project tezlink
   v}

   If you run Docker on mac os, enable
   {{!https://docs.docker.com/engine/network/drivers/host/#docker-desktop}host
   networking} and add the flag [--macosx] to the command below.

   The scenario can then be started with
   {v
     dune exec tezt/tests/cloud/main.exe -- ETHERLINK \
       --tezt-cloud etherlink --destroy \
       --machine-type c2d-standard-8 \
       --docker-host-network \
       --network braeburn -i
   v}

   You can benchmark different combinations of network context and kernel using
   the [--network] and [--kernel] options. See the [--help] for more benchmark
   options to control the number of iterations, swap hops, accounts, etc.
*)

module Cli = Scenarios_cli
open Scenarios_helpers
open Tezt_etherlink
open Etherlink_benchmark_lib
open Benchmark_utils

let setup_sequencer_sandbox (cloud : Cloud.t) ~name network kernel
    (time_between_blocks : Evm_node.time_between_blocks) accounts agent =
  let rpc_port = Agent.next_available_port agent in
  let private_rpc_port = Agent.next_available_port agent |> Option.some in
  let custom_dest = "/kernel/evm_kernel.wasm" in
  let init_from_snapshot = Option.is_some network in
  let initial_kernel_source =
    match (network, kernel) with
    | Some _, `Mainnet -> None
    | _, `Mainnet ->
        Some
          "etherlink/kernel_latest/kernel/tests/resources/mainnet_kernel.wasm"
    | _, `Custom k -> Some k
  in
  let* initial_kernel =
    match initial_kernel_source with
    | None -> return None
    | Some source ->
        let* p = Agent.copy ~source ~destination:custom_dest agent in
        return (Some p)
  in
  let wallet_dir = Temp.dir "wallet" in
  let preimages_dir = Temp.dir "wasm_2_0_0" in
  let funded_addresses =
    Array.to_list accounts |> List.map (fun a -> a.Eth_account.address)
  in
  let sequencer_config : Evm_node.sequencer_config =
    {
      time_between_blocks = Some time_between_blocks;
      genesis_timestamp = None;
      max_number_of_chunks = None;
      wallet_dir = Some wallet_dir;
    }
  in
  let mode =
    Evm_node.Sandbox
      {sequencer_config; network; funded_addresses; sequencer_keys = []}
  in
  let () = toplog "Launching the sandbox L2 node" in
  let add_cors rpc =
    JSON.update rpc (fun json ->
        JSON.update
          "cors_headers"
          (fun _ ->
            JSON.annotate ~origin:"patch-config:cors_headers"
            @@ `A [`String "*"])
          json
        |> JSON.update "cors_origins" (fun _ ->
               JSON.annotate ~origin:"patch-config:cors_origins"
               @@ `A [`String "*"]))
  in
  let add_telemetry config =
    match Cloud.open_telemetry_endpoint cloud with
    | None -> config
    | Some base ->
        let j = JSON.annotate ~origin:"patch-config:opentelemetry" in
        config
        |> JSON.update "opentelemetry" @@ fun otel_config ->
           otel_config
           |> JSON.update "enable" (fun _ -> j (`Bool true))
           |> JSON.update "url_traces" (fun _ ->
                  j (`String (base ^ "/v1/traces")))
           |> JSON.update "url_logs" (fun _ -> j (`String (base ^ "/v1/logs")))
  in
  let extra_arguments =
    Cli_arg.optional_switch "init-from-snapshot" init_from_snapshot
  in
  let* evm_node =
    Tezos.Evm_node.Agent.init
      ~wait:false
      ~copy_binary:false
        (* We use the binary from the EVM release docker image *)
      ~patch_config:(fun json ->
        json |> add_cors "public_rpc" |> add_cors "private_rpc" |> add_telemetry
        |> Evm_node.patch_config_with_experimental_feature
             ~drop_duplicate_when_injection:true
             ~blueprints_publisher_order_enabled:true
             ~rpc_server:Resto
             ())
      ~name:"sandboxed-sequencer"
      ~mode
      ~rpc_port
      ?initial_kernel
      ~preimages_dir
      ?private_rpc_port
      ~extra_arguments
      ()
      cloud
      agent
  in
  let* () = Evm_node.wait_for_ready ~timeout:1200. evm_node in
  let () = toplog "Launching the sandbox L2 node: done" in
  let* () = add_prometheus_source ~evm_node cloud agent name in
  let () = toplog "Added prometheus source" in
  return evm_node

let benchmark_tags (module Cli : Scenarios_cli.Etherlink) =
  let env = "etherlink-capacity-benchmark" in
  let network = Option.value Cli.network ~default:"sandbox" in
  let kernel =
    match Cli.kernel with
    | `Mainnet -> network
    | `Custom ("evm_kernel.wasm" | "./evm_kernel.wasm") -> "master"
    | `Custom s -> String.map (function '/' -> '_' | c -> c) s
  in
  let evm_node_version =
    match Cli.evm_node_version with `Latest -> "latest" | `V v -> "v" ^ v
  in
  let version = String.concat "/" [evm_node_version; kernel] in
  let machine_tags =
    if Tezt_cloud_cli.localhost then [("machine", "local")]
    else
      [
        ("machine_type", Tezt_cloud_cli.machine_type);
        ("disk_type", Tezt_cloud_cli.disk_type |> Option.value ~default:"pd-ssd");
      ]
  in
  [
    ("environment", env);
    ("network", network);
    ("kernel", kernel);
    ("evm-node-version", evm_node_version);
    ("version", version);
    ("benchmark.type", "uniswapv2");
    ("benchmark.swap_hops", parameters.swap_hops |> string_of_int);
    ( "benchmark.tokens",
      parameters.contracts |> Option.value ~default:2 |> string_of_int );
    ( "benchmark.accounts",
      parameters.accounts |> Option.value ~default:150 |> string_of_int );
    ( "benchmark.time_between_blocks",
      let (`Auto t | `Manual t) = parameters.time_between_blocks in
      string_of_float t );
    ("benchmarks.iterations", parameters.iterations |> string_of_int);
  ]
  @ machine_tags
  |> List.map (fun (k, v) -> String.concat ":" [k; v])

let send_capacity_metric cli ~name ~value =
  let timestamp = Ptime_clock.now () in
  let tags = benchmark_tags cli in
  [
    {
      Datadog.metric = name;
      type_ = Gauge;
      metadata = None;
      points = [{timestamp; value}];
      resources = [];
      source_type_name = None;
      tags;
      unit = Some "MGas/s";
    };
  ]
  |> Datadog.send |> Runnable.run

let send_sampled_benchmark_metrics (module Cli : Scenarios_cli.Etherlink)
    gasometer =
  let rec sample acc data_points =
    match data_points with
    | [] ->
        List.rev_map
          (fun (count, total, timestamp) ->
            (* Average in sample period *)
            let value = total /. float_of_int count in
            let timestamp = Ptime.of_float_s timestamp |> Option.get in
            Datadog.{timestamp; value})
          acc
    | {timestamp; gas_per_sec; _} :: l -> (
        match acc with
        | (count, total, cur_timestamp) :: acc
          when timestamp < cur_timestamp +. Cli.datadog_sample_period ->
            sample ((count + 1, total +. gas_per_sec, cur_timestamp) :: acc) l
        | _ -> sample ((1, gas_per_sec, floor timestamp) :: acc) l)
  in
  let points = sample [] gasometer.datapoints in
  let tags = benchmark_tags (module Cli) in
  [
    {
      Datadog.metric = "etherlink_sampled_capacity";
      type_ = Gauge;
      metadata = None;
      points;
      resources = [];
      source_type_name = None;
      tags;
      unit = Some "MGas/s";
    };
  ]
  |> Datadog.send |> Runnable.run

let export_datadog_metrics (module Cli : Scenarios_cli.Etherlink)
    {median : float; p90 : float; wall : float; gasometer} =
  let* () =
    send_capacity_metric
      (module Cli)
      ~name:"etherlink_median_capacity"
      ~value:median
  and* () =
    send_capacity_metric (module Cli) ~name:"etherlink_p90_capacity" ~value:p90
  and* () =
    send_capacity_metric
      (module Cli)
      ~name:"etherlink_wall_clock_capacity"
      ~value:wall
  and* () = send_sampled_benchmark_metrics (module Cli) gasometer in
  toplog "Results exported to datadog." ;
  unit

(* Set default values for CLI arguments. *)
let () =
  parameters.time_between_blocks <- `Auto 0.5 ;
  parameters.iterations <- 1000 ;
  parameters.accounts <- Some 150 ;
  parameters.contracts <- Some 2 ;
  parameters.timeout <- 15. ;
  parameters.swap_hops <- 2 ;
  ()

let gen_report report_path network kernel {p90; gasometer; _} =
  let network = Option.value network ~default:"sandbox" in
  let kernel =
    match kernel with
    | `Mainnet -> network
    | `Custom ("evm_kernel.wasm" | "./evm_kernel.wasm") -> "master"
    | `Custom s -> String.map (function '/' -> '_' | c -> c) s
  in
  let csv_filename = sf "%s/p90_results_%s_%s.csv" report_path network kernel in
  let png_filename = sf "%s/p90_results_%s_%s.png" report_path network kernel in
  save_capacity ~csv_filename p90 ;
  let* () = plot_capacity ~csv_filename ~network ~kernel png_filename in
  Log.report "Capacity graph generated in %s" png_filename ;
  let graph_filename = sf "%s/graph_%s_%s.png" report_path network kernel in
  let* () = plot_gas_and_capacity ~gasometer ~output_filename:graph_filename in
  unit

let register (module Cli : Scenarios_cli.Etherlink) =
  let () = toplog "Parsing CLI done" in
  let name = "etherlink" in
  let docker_tag =
    match Cli.evm_node_version with `Latest -> "latest" | `V s -> "v" ^ s
  in
  let dockerbuild_args = [("EVM_NODE_VERSION", docker_tag)] in
  let vms () = return [Agent.Configuration.make ~name ~dockerbuild_args ()] in
  Cloud.register
    ~vms
    ~proxy_files:[]
    ~proxy_args:[]
    ~__FILE__
    ~title:"Etherlink benchmark"
    ~tags:[]
    ~dockerbuild_args
  @@ fun cloud ->
  Clap.close () ;
  let () = toplog "Creating the agents" in
  let agents = Cloud.agents cloud in
  let sequencer_agent = List.hd agents in
  let nb_accounts = Option.value parameters.accounts ~default:150 in
  let accounts = Eth_account.accounts nb_accounts in
  let nb_tokens = Option.value parameters.contracts ~default:2 in
  let () = toplog "Starting sequencer" in
  let time_between_empty_blocks = 6.0 in
  let time_between_blocks =
    match Cli.time_between_blocks with
    | `Auto _ -> Evm_node.Time_between_blocks time_between_empty_blocks
    | `Manual _ -> Nothing
  in
  let* sequencer =
    setup_sequencer_sandbox
      cloud
      ~name
      Cli.network
      Cli.kernel
      time_between_blocks
      accounts
      sequencer_agent
  in
  let* () =
    (* The durable storage is patched on startup to pre-fund addresses. If there
       is already a context, we need to wait for the next block for the patch to
       be effective. *)
    match Cli.kernel with
    | `Custom _ -> unit
    | `Mainnet -> Lwt_unix.sleep (time_between_empty_blocks *. 4.)
  in
  let () = toplog "Setup UniswapV2 benchmark" in
  let* env, shutdown =
    Uniswap.setup
      ~accounts
      ~nb_tokens
      ~nb_hops:parameters.swap_hops
      ~sequencer
      ~rpc_node:sequencer
  in
  let () = toplog "Setup UniswapV2 benchmark complete" in
  let* monitor_result =
    monitor_gasometer sequencer @@ fun () ->
    let* () =
      Lwt_list.iter_s (Uniswap.step env) (List.init parameters.iterations succ)
    in
    shutdown ()
  in
  let* () =
    if Cli.datadog_export then
      export_datadog_metrics (module Cli) monitor_result
    else unit
  in
  let total_confirmed = !(env.total_confirmed) in
  let total_sent = nb_accounts * parameters.iterations in
  if total_confirmed <= total_sent / 2 then
    Test.fail
      ~__LOC__
      "Less than half operations confirmed (%d/%d)"
      total_confirmed
      total_sent ;
  if Cli.gen_report then
    gen_report Cli.report_path Cli.network Cli.kernel monitor_result
  else unit
