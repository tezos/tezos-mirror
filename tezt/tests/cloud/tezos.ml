(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module Env = struct
  type t = string String_map.t

  let empty = String_map.empty

  let add = String_map.add

  let may_add cond var value env = if cond then add var value env else env

  let memtrace_env enable filename env = may_add enable "MEMTRACE" filename env

  let otel_env otel_endpoint service_name env =
    match otel_endpoint with
    | None -> env
    | Some endpoint ->
        add "OTEL" "true" env
        |> add "OTEL_SERVICE_NAME" service_name
        |> add "OTEL_EXPORTER_OTLP_ENDPOINT" endpoint

  let disable_shard_validation_env enable env =
    may_add
      enable
      Dal_node.disable_shard_validation_environment_variable
      "yes"
      env

  let allow_publication_regularly_env enable env =
    may_add
      enable
      Dal_node.allow_regular_publication_environment_variable
      "yes"
      env

  let ignore_topics_env ignore_pkhs env =
    match ignore_pkhs with
    | Some (_ :: _) -> add Dal_node.ignore_topics_environment_variable "yes" env
    | _ -> env

  let txt_profiler_backend = "txt"

  let json_profiler_backed = "json"

  let prometheus_profiler_backend = "prometheus"

  let opentelemetry_profiler_backend = "opentelemetry"

  let default_profiling_backends = [txt_profiler_backend; json_profiler_backed]

  let ppx_profiling_backends ?prometheus ?otel backends =
    let cons_if b v l = if b then v :: l else l in
    let is_selected backend = backends = [] || List.mem backend backends in
    let profiling_backends =
      []
      |> cons_if
           (Option.is_some otel && is_selected opentelemetry_profiler_backend)
           opentelemetry_profiler_backend
      |> cons_if
           (Option.is_some prometheus && is_selected prometheus_profiler_backend)
           prometheus_profiler_backend
      |> cons_if (is_selected txt_profiler_backend) txt_profiler_backend
      |> cons_if (is_selected json_profiler_backed) json_profiler_backed
    in
    "\"" ^ String.concat ";" profiling_backends ^ "\""

  let ppx_profiler_env ?prometheus ?otel verbosity backends env =
    match verbosity with
    | None | Some "" -> env
    | Some verbosity ->
        let backends = ppx_profiling_backends ?prometheus ?otel backends in
        env |> add "PROFILING" verbosity |> add "PROFILING_BACKENDS" backends

  let initialize_env ~memtrace ~memtrace_output_filename
      ~disable_shard_validation ~allow_publication_regularly ~prometheus
      ~otel_endpoint ~service_name ~ignore_pkhs
      ~(ppx_profiling_verbosity : string option)
      ~(ppx_profiling_backends : string list) =
    empty
    |> memtrace_env memtrace memtrace_output_filename
    |> otel_env otel_endpoint service_name
    |> disable_shard_validation_env disable_shard_validation
    |> allow_publication_regularly_env allow_publication_regularly
    |> ignore_topics_env ignore_pkhs
    |> ppx_profiler_env
         ?prometheus
         ?otel:otel_endpoint
         ppx_profiling_verbosity
         ppx_profiling_backends
end

let may_add_profiling_to_env ~ppx_profiling_verbosity:verbosity
    ~ppx_profiling_backends:backends = function
  | Some env -> Some (Env.ppx_profiler_env verbosity backends env)
  | None -> (
      match verbosity with
      | None | Some "" -> None
      | Some _ -> Some (Env.ppx_profiler_env verbosity backends Env.empty))

let create_dir ?runner dir =
  let* () = Process.spawn ?runner "rm" ["-rf"; dir] |> Process.check in
  let* () = Process.spawn ?runner "mkdir" ["-p"; dir] |> Process.check in
  Lwt.return_unit

(* See the FIXME below: this should belong to service_manager but
   requires splitting web in order to avoid dependency cycle *)
let service_manager_receiver notifier =
  let open Types in
  match notifier with
  | Notifier_null -> Alert.null_receiver
  | Notifier_slack {name; slack_bot_token; slack_channel_id} ->
      Alert.slack_bottoken_receiver
        ~name:(Format.asprintf "%s_service_manager" name)
        ~channel:slack_channel_id
        ~bot_token:slack_bot_token
        ~title:"service_manager: process crashed"
        ~text:
          {|{{ range .Alerts }}service_manager: process crashed: {{ .Labels.name }} on agent {{ .Labels.agent }}
            {{ .Annotations.summary }}
      {{ end }}|}
        ()

module Alerts = struct
  let service_manager_process_down ~(agent : string) executable receiver
      metric_name name =
    Alert.make
      ~name:"ServiceManagerProcessDown"
      ~description:
        {|This alert is raised when a process monitored by the service_manager is detected as being not running. This happens typically when the process pid is not found anymore in the process tree, or the pid has been recycled and does not correspond to the executable that was run initially|}
      ~summary:
        (Format.asprintf
           "'[%s.service_manager] the process [%s] is down'"
           agent
           executable)
      ~route:(Alert.route receiver)
      ~severity:Alert.Critical
      ~expr:(Format.asprintf {|%s{name="%s"} < 1|} metric_name name)
      ()

  let process_exporter_high_cpu ~agent_name ~appname ~groupname receiver =
    Alert.make
      ~name:"ProcessExporterProcessHighCPUUsage"
      ~description:
        "'The executable has been using more than 80% CPU for 5 minutes'"
      ~summary:
        (Format.asprintf
           "'[%s.process_exporter] high cpu usage of process [%s]'"
           agent_name
           groupname)
      ~for_:"5m"
      ~severity:Warning
      ~expr:
        (Format.asprintf
           {|'rate(namedprocess_namegroup_cpu_seconds_total{app="%s", groupname="%s"}[5m]) > 0.8'|}
           appname
           groupname)
      ~route:(Alert.route receiver)
      ()

  (* expected_mem is the expected *resident* memory usage in GiB, 2GiB if not specified *)
  let process_exporter_high_mem ~agent_name ~appname ?expected_mem ~groupname
      receiver =
    let expected_mem =
      match expected_mem with
      | None -> 2 * 1024 * 1024 * 1024 (* 2G by default *)
      | Some sz -> sz * 1024 * 1024 * 1024
    in
    Alert.make
      ~name:"ProcessExporterProcessHighMemory"
      ~description:"'The executable is using a large amount of memory'"
      ~summary:
        (Format.asprintf
           "'[%s.process_exporter] high memory usage of process [%s]'"
           agent_name
           groupname)
      ~for_:"5m"
      ~severity:Warning
      ~expr:
        (Format.asprintf
           {|'namedprocess_namegroup_memory_bytes{app="%s",groupname="%s", memtype="resident"} > %d'|}
           appname
           groupname
           expected_mem)
      ~route:(Alert.route receiver)
      ()

  let process_exporter_increase_mem ~agent_name ~appname ~groupname receiver =
    Alert.make
      ~name:"ProcessExporterProcessMemoryIncreaseFromOneDayAvg"
      ~expr:
        (Format.asprintf
           {|'namedprocess_namegroup_memory_bytes{app="%s",groupname="%s"} > avg_over_time(namedprocess_namegroup_memory_bytes{app="%s", groupname="%s"}[1d] offset 1d) * 1.5'|}
           appname
           groupname
           appname
           groupname)
      ~for_:"30m"
      ~severity:Warning
      ~summary:
        (Format.asprintf
           "'[%s.process_exporter] increase of %s memory usage'"
           agent_name
           groupname)
      ~description:
        {|'The process memory usage is more than 50% higher than the average from the previous 24h'|}
      ~route:(Alert.route receiver)
      ()

  let process_exporter_increase_mem_threshold ~agent_name ~appname ~groupname
      receiver =
    Alert.make
      ~name:"ProcessExporterMemoryIncreaseThreshold"
      ~expr:
        (Format.asprintf
           {|(
            namedprocess_namegroup_memory_bytes{app="%s", groupname="%s"} /
            avg_over_time(namedprocess_namegroup_memory_bytes{app="%s", groupname="%s"}[30m] offset 1h)
            - 1
          ) * 100 > 10
          and
          time() - process_start_time_seconds{app="%s", groupname="%s"} > 4 * 3600|}
           appname
           groupname
           appname
           groupname
           appname
           groupname)
      ~for_:"15m"
      ~severity:Warning
      ~summary:
        (Format.asprintf
           {|'[%s.process_exporter]: %s memory usage increased compared to reference'|}
           agent_name
           groupname)
      ~description:
        (Format.asprintf
           "The %s memory usage has increased compared to the reference period"
           groupname)
      ~route:(Alert.route receiver)
      ()

  let process_exporter_high_open_files ~agent_name ~appname ~groupname receiver
      =
    Alert.make
      ~name:"ProcessExporterProcessHighOpenFiles"
      ~description:
        "The executable is using more than 70% of its file descriptor quota"
      ~summary:
        (Format.asprintf
           "'[%s.process_exporter] high number of open files for process [%s]'"
           agent_name
           groupname)
      ~for_:"5m"
      ~route:(Alert.route receiver)
      ~severity:Alert.Warning
      ~expr:
        (Format.asprintf
           {|'namedprocess_namegroup_open_filedesc{app="%s", groupname="%s"} / process_max_fds{app="%s"} > 0.7'|}
           appname
           groupname
           appname)
      ()

  let add_process_exporter_alerts ~cloud ~agent_name ~appname ~groupname
      receiver =
    Log.report
      "Process_monitoring: adding alerts for %s on agent %s"
      groupname
      agent_name ;
    Lwt_list.iter_p
      (fun alert ->
        let alert = alert ~agent_name ~appname ~groupname receiver in
        let* () = Cloud.add_alert cloud ~alert in
        Lwt.return_unit)
      [
        process_exporter_high_cpu;
        process_exporter_high_mem ?expected_mem:None;
        process_exporter_increase_mem;
        process_exporter_increase_mem_threshold;
        process_exporter_high_open_files;
      ]
end

module Node = struct
  include Tezt_tezos.Node

  module Agent = struct
    let create ?(group = "L1") ?rpc_external ?(metadata_size_limit = true)
        ?(arguments = []) ?data_dir ?(path = Uses.path Constant.octez_node)
        ~name ?net_addr cloud agent =
      let* path = Agent.copy agent ~source:path in
      let binary_name = Filename.basename path in
      let* () =
        Cloud.register_binary cloud ~agents:[agent] ~group ~name:binary_name ()
      in
      let* () =
        Cloud.register_binary
          cloud
          ~agents:[agent]
          ~group
          ~name:"octez-validator"
          ()
      in
      let* () =
        if Option.is_some rpc_external then
          Cloud.register_binary
            cloud
            ~agents:[agent]
            ~group
            ~name:"octez-rpc-process"
            ()
        else Lwt.return_unit
      in
      let runner = Agent.runner agent in
      let rpc_port = Agent.next_available_port agent in
      let net_port = Agent.next_available_port agent in
      let metrics_port = Agent.next_available_port agent in
      let arguments =
        if metadata_size_limit then
          Metadata_size_limit (Some 10_000) :: arguments
        else arguments
      in
      let node =
        create
          ?data_dir
          ~name
          ~path
          ?runner
          ?rpc_external
          ?net_addr
          ~rpc_port
          ~net_port
          ~metrics_port
          arguments
      in
      let node_name = Node.name node in
      let executable = Node.path node in
      (* FIXME: The following metric and alert definition should belong to the
         service_manager module. Unfortunately, for the metrics to be registered,
         we need to define them in the web module, but this involves a mutual
         dependency between the service_manager, web, agent and cloud modules.
         A proper solution might be to split the web module into a backend
         that serves the metrics and a frontend (not a web frontend), that
         show information about the different tezt-cloud components *)
      let metric_name = "service_manager_process_alive" in
      let receiver = service_manager_receiver (Cloud.notifier cloud) in
      let alert =
        Alerts.service_manager_process_down
          ~agent:(Agent.name agent)
          executable
          receiver
          metric_name
          node_name
      in
      let* () = Cloud.add_alert cloud ~alert in
      let on_alive_callback ~alive =
        Cloud.push_metric
          cloud
          ~help:(Format.asprintf "'Process %s is alive'" node_name)
          ~typ:`Gauge
          ~name:metric_name
          ~labels:
            [
              ("agent", Agent.name agent);
              ("name", node_name);
              ("executable", executable);
              ("kind", "alive");
            ]
          (if alive then 1.0 else 0.0)
      in
      let* () =
        (* Prometheus *)
        let app_name =
          Format.asprintf "%s:prometheus-process-exporter" (Agent.name agent)
        in
        let target = Cloud.{agent; port = Node.metrics_port node; app_name} in
        let* () = Cloud.add_prometheus_source cloud ~name [target] in
        (* Prometheus process-exporter *)
        Alerts.add_process_exporter_alerts
          ~cloud
          ~agent_name:(Agent.name agent)
          ~appname:
            target.app_name (* reuse the app_name from prometheus source *)
          ~groupname:binary_name
            (* The label 'groupname' is filled by prometheus-process-exporter from the names
               given in command line. The alerts must match the same groupname *)
          receiver
      in
      let on_shutdown =
        match Agent.artifacts_dir agent with
        | Some destination_root ->
            (if Tezt_cloud_cli.retrieve_daily_logs then
               [
                 (fun () ->
                   Agent_kind.Logs.scp_logs
                     ~destination_root
                     ~daemon_name:name
                     agent);
               ]
             else [])
            @
            if Tezt_cloud_cli.retrieve_ppx_profiling_traces then
              [
                (fun () ->
                  Agent_kind.Logs.scp_profiling
                    ~destination_root
                    ~daemon_name:name
                    agent);
              ]
            else []
        | None -> []
      in
      Cloud.service_register
        ~name:node_name
        ~executable
        ~on_alive_callback
        ~on_shutdown
        agent ;
      Lwt.return node

    let init ?(group = "L1") ?rpc_external ?(metadata_size_limit = true)
        ?(arguments = []) ?data_dir ?(path = Uses.path Constant.octez_node)
        ?net_addr ?name cloud agent =
      let runner = Agent.runner agent in
      let* path = Agent.copy agent ~source:path in
      let* () =
        Cloud.register_binary
          cloud
          ~agents:[agent]
          ~group
          ~name:(Filename.basename path)
          ()
      in
      let* () =
        Cloud.register_binary
          cloud
          ~agents:[agent]
          ~group
          ~name:"octez-validator"
          ()
      in
      let* () =
        Cloud.register_binary
          cloud
          ~agents:[agent]
          ~group
          ~name:"octez-rpc-process"
          ()
      in
      let rpc_port = Agent.next_available_port agent in
      let net_port = Agent.next_available_port agent in
      let metrics_port = Agent.next_available_port agent in
      let arguments =
        if metadata_size_limit then
          Metadata_size_limit (Some 10_000) :: arguments
        else arguments
      in
      init
        ?name
        ?data_dir
        ~path
        ?runner
        ?rpc_external
        ~rpc_port
        ?net_addr
        ~net_port
        ~metrics_port
        ~event_level:`Notice
        arguments

    let run ?env ?patch_config ?on_terminate ?event_level ?event_sections_levels
        ~ppx_profiling_verbosity ~ppx_profiling_backends node args =
      let name = name node in
      let* () =
        run
          ?env:
            (may_add_profiling_to_env
               ~ppx_profiling_verbosity
               ~ppx_profiling_backends
               env)
          ?patch_config
          ?on_terminate
          ?event_level
          ?event_sections_levels
          node
          args
      in
      (* Notify service manager. Need to run before to get the pid *)
      let () =
        match Node.pid node with
        | None ->
            Log.error
              "Cannot update service %s: no pid. Is the program running ?"
              name
        | Some pid -> Cloud.notify_service_start ~name ~pid
      in
      Lwt.return_unit

    let terminate ?timeout node =
      let name = name node in
      (* Notify the Service manager. *)
      Cloud.notify_service_stop ~name ;
      terminate ?timeout node
  end
end

module Yes_wallet = struct
  include Tezt_tezos.Yes_wallet

  module Agent = struct
    let create ?(path = Uses.path Constant.yes_wallet) ?name agent =
      let* path = Agent.copy agent ~source:path in
      let runner = Agent.runner agent in
      Lwt.return (create ?runner ~path ?name ())
  end
end

module Dal_node = struct
  include Tezt_tezos.Dal_node

  module Agent = struct
    let create_from_endpoint ?(group = "DAL") ?net_port
        ?(path = Uses.path Constant.octez_dal_node) ~name ?rpc_port
        ?disable_shard_validation ?disable_amplification ?ignore_pkhs
        ?publish_slots_regularly ~l1_node_endpoint cloud agent =
      let* path = Agent.copy agent ~source:path in
      let binary_name = Filename.basename path in
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
        | None -> Agent.next_available_port agent
        | Some rpc_port -> rpc_port
      in
      let net_port =
        match net_port with
        | None -> Agent.next_available_port agent
        | Some port -> port
      in
      let metrics_port = Agent.next_available_port agent in
      let metrics_addr = Format.asprintf "0.0.0.0:%d" metrics_port in
      let listen_addr = Format.asprintf "0.0.0.0:%d" net_port in
      let node =
        create_from_endpoint
          ~name
          ~path
          ?runner
          ~rpc_port
          ~metrics_addr
          ~listen_addr
          ?disable_shard_validation
          ?disable_amplification
          ?ignore_pkhs
          ?publish_slots_regularly
          ~l1_node_endpoint
          ()
      in
      let node_name = Dal_node.name node in
      let executable = Dal_node.path node in
      let metric_name = "service_manager_process_alive" in
      let receiver = service_manager_receiver (Cloud.notifier cloud) in
      let on_alive_callback ~alive =
        Cloud.push_metric
          cloud
          ~help:(Format.asprintf "'Process %s is alive'" node_name)
          ~typ:`Gauge
          ~name:metric_name
          ~labels:
            [
              ("agent", Agent.name agent);
              ("name", node_name);
              ("executable", executable);
              ("kind", "alive");
            ]
          (if alive then 1.0 else 0.0)
      in
      let alert =
        Alert.make
          ~name:"ServiceManagerProcessDown"
          ~description:
            {|This alert is raised when a process monitored by the service_manager is detected as being not running. This happens typically when the process pid is not found anymore in the process tree, or the pid has been recycled and does not correspond to the executable that was run initially|}
          ~summary:
            (Format.asprintf
               "'[%s.service_manager] the process [%s] is down'"
               (Agent.name agent)
               executable)
          ~route:(Alert.route receiver)
          ~severity:Alert.Critical
          ~expr:(Format.asprintf {|%s{name="%s"} < 1|} metric_name name)
          ()
      in
      let* () = Cloud.add_alert cloud ~alert in
      let alert =
        Alerts.service_manager_process_down
          ~agent:(Agent.name agent)
          executable
          receiver
          metric_name
          node_name
      in
      let* () = Cloud.add_alert cloud ~alert in
      let* () =
        (* Prometheus source *)
        let app_name =
          Format.asprintf "%s:prometheus-process-exporter" (Agent.name agent)
        in
        let target =
          Cloud.{agent; port = Dal_node.metrics_port node; app_name}
        in
        let* () = Cloud.add_prometheus_source cloud ~name [target] in
        Alerts.add_process_exporter_alerts
          ~cloud
          ~agent_name:(Agent.name agent)
          ~appname:
            target.app_name (* reuse the app_name from prometheus source *)
          ~groupname:binary_name
          receiver
      in
      let on_shutdown =
        match Agent.artifacts_dir agent with
        | Some destination_root ->
            (if Tezt_cloud_cli.retrieve_daily_logs then
               [
                 (fun () ->
                   Agent_kind.Logs.scp_logs
                     ~destination_root
                     ~daemon_name:name
                     agent);
               ]
             else [])
            @
            if Tezt_cloud_cli.retrieve_ppx_profiling_traces then
              [
                (fun () ->
                  Agent_kind.Logs.scp_profiling
                    ~destination_root
                    ~daemon_name:name
                    agent);
              ]
            else []
        | None -> []
      in
      Cloud.service_register
        ~name
        ~executable
        ~on_alive_callback
        ~on_shutdown
        agent ;
      Lwt.return node

    let create ?net_port ?path ~name ?disable_shard_validation
        ?disable_amplification ?ignore_pkhs ?publish_slots_regularly ~node cloud
        agent =
      let l1_node_endpoint =
        Node.as_rpc_endpoint ~local:(Node.runner node = Agent.runner agent) node
      in
      create_from_endpoint
        ?net_port
        ?path
        ~name
        ?disable_shard_validation
        ?disable_amplification
        ?ignore_pkhs
        ?publish_slots_regularly
        ~l1_node_endpoint
        cloud
        agent

    let run ?prometheus ?otel ?(memtrace = false) ?wait_ready ?event_level
        ?(disable_shard_validation = false) ?ignore_pkhs
        ?(allow_publication_regularly = false) ~ppx_profiling_verbosity
        ~ppx_profiling_backends dal_node =
      let service_name = name dal_node in
      let memtrace_output_filename =
        Format.asprintf "%s/%s-trace.ctf" Path.tmp_dir service_name
      in
      let env =
        Env.initialize_env
          ~memtrace
          ~memtrace_output_filename
          ~disable_shard_validation
          ~allow_publication_regularly
          ~prometheus
          ~otel_endpoint:otel
          ~service_name
          ~ignore_pkhs
          ~ppx_profiling_verbosity
          ~ppx_profiling_backends
      in
      let* () = run ?wait_ready ~env ?event_level dal_node in
      (* Update the state in the service manager *)
      let () =
        match Dal_node.pid dal_node with
        | None ->
            (* Here, we simply log the error (in RED)
               Users of tezt-cloud are expected to read their logs.
               As a rule of thumb, if error arise in components of tezt-cloud,
               tezt-cloud SHALL NOT hinder the scenario and raise errors
               in deployment, unless errors are fatal *)
            Log.error
              "Cannot update service state %s: no pid. Is the program running ?"
              service_name
        | Some pid -> Cloud.notify_service_start ~name:service_name ~pid
      in
      Lwt.return_unit

    let terminate ?timeout dal_node =
      let name = Dal_node.name dal_node in
      Cloud.notify_service_stop ~name ;
      terminate ?timeout dal_node
  end
end

module Floodgate = struct
  include Tezt_etherlink.Floodgate

  module Agent = struct
    let run ?(group = "Etherlink") ?(path = "floodgate") ?scenario ~rpc_endpoint
        ~controller ?relay_endpoint ?max_active_eoa
        ?max_transaction_batch_length ?spawn_interval ?tick_interval
        ?base_fee_factor ?initial_balance cloud agent =
      let* path = Agent.copy agent ~source:path in
      let* () =
        Cloud.register_binary
          cloud
          ~agents:[agent]
          ~group
          ~name:(Filename.basename path)
          ()
      in
      let runner = Agent.runner agent in
      run
        ?runner
        ~path
        ?scenario
        ~rpc_endpoint
        ~controller
        ?relay_endpoint
        ?max_active_eoa
        ?max_transaction_batch_length
        ?spawn_interval
        ?tick_interval
        ?base_fee_factor
        ?initial_balance
        ()
  end
end

module Sc_rollup_node = struct
  include Tezt_tezos.Sc_rollup_node

  module Agent = struct
    let create ?(group = "Etherlink")
        ?(path = Uses.path Constant.octez_smart_rollup_node) ?name
        ?default_operator ?operators ?dal_node ~base_dir ~kind cloud agent mode
        l1_node =
      let* path = Agent.copy agent ~source:path in
      let binary_name = Filename.basename path in
      let* () =
        Cloud.register_binary cloud ~agents:[agent] ~group ~name:binary_name ()
      in
      let runner = Agent.runner agent in
      let rpc_port = Agent.next_available_port agent in
      let metrics_port = Agent.next_available_port agent in
      let metrics_addr = "0.0.0.0" in
      (* Prometheus *)
      let* () =
        let app_name =
          Format.asprintf "%s:prometheus-process-exporter" (Agent.name agent)
        in
        let target = Cloud.{agent; port = metrics_port; app_name} in
        let* () =
          Cloud.add_prometheus_source
            cloud
            ~name:(Option.value name ~default:path)
            [target]
        in
        (* Prometheus process exporter *)
        let receiver = service_manager_receiver (Cloud.notifier cloud) in
        Alerts.add_process_exporter_alerts
          ~cloud
          ~agent_name:(Agent.name agent)
          ~appname:
            target.app_name (* reuse the app_name from prometheus source *)
          ~groupname:binary_name
          receiver
      in
      create
        ?name
        ?default_operator
        ?operators
        ?dal_node
        ~path
        ?runner
        ~rpc_port
        ~metrics_addr
        ~metrics_port
        ~base_dir
        ~kind
        mode
        l1_node
      |> Lwt.return
  end
end

module Sc_rollup_helpers = struct
  include Sc_rollup_helpers

  module Agent = struct
    let prepare_installer_kernel_with_arbitrary_file ~preimages_dir ?config
        installee agent =
      let* installee = Agent.copy agent ~source:installee in
      let* smart_rollup_installer_path =
        Agent.copy agent ~source:(Uses.path Constant.smart_rollup_installer)
      in
      let runner = Agent.runner agent in
      prepare_installer_kernel_with_arbitrary_file
        ~smart_rollup_installer_path
        ?runner
        ~boot_sector:`Filename
        ~preimages_dir
        ?config
        installee

    let prepare_installer_kernel_with_multiple_setup_file ?output ~preimages_dir
        ?configs installee agent =
      let* installee = Agent.copy agent ~source:installee in
      let* smart_rollup_installer_path =
        Agent.copy agent ~source:(Uses.path Constant.smart_rollup_installer)
      in
      let runner = Agent.runner agent in
      prepare_installer_kernel_with_multiple_setup_file
        ?output
        ~smart_rollup_installer_path
        ?runner
        ~boot_sector:`Filename
        ~preimages_dir
        ?configs
        installee

    let prepare_installer_kernel ~preimages_dir ?config installee agent =
      prepare_installer_kernel_with_arbitrary_file
        ~preimages_dir
        ?config
        (Uses.path installee)
        agent

    let originate_sc_rollup ?hooks ?(burn_cap = Tez.(of_int 9999999)) ?whitelist
        ?(alias = "rollup") ?(src = Constant.bootstrap1.alias) ~kind
        ?(parameters_ty = "string") ~boot_sector client =
      let boot_sector =
        (* FIXME: This is not ideal. A better way would be to extend the
           originate command so that it takes a filename as input and read its
           content (actually maybe the feature already exists). *)
        "file:" ^ boot_sector
      in
      let* sc_rollup =
        Client.Sc_rollup.(
          originate
            ?hooks
            ~burn_cap
            ?whitelist
            ~alias
            ~src
            ~kind
            ~parameters_ty
            ~boot_sector
            client)
      in
      let* () = Lwt_unix.sleep 4. in
      return sc_rollup
  end
end

module Evm_node = struct
  include Tezt_etherlink.Evm_node

  module Agent = struct
    (* Use for compatibility with `tezt-cloud`. *)
    let create ?(group = "Etherlink") ?(copy_binary = true)
        ?(path = Uses.path Constant.octez_evm_node) ?name ?data_dir ?mode
        ?rpc_port ?websockets endpoint cloud agent =
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
        | None -> Agent.next_available_port agent
        | Some port -> port
      in
      create ?name ~path ?runner ?data_dir ~rpc_port ?mode ?websockets endpoint
      |> Lwt.return

    let init ?patch_config ?name ?mode ?websockets ?data_dir ?rpc_port ?wait
        ?extra_arguments ?copy_binary rollup_node cloud agent =
      let* evm_node =
        create
          ?name
          ?copy_binary
          ?mode
          ?rpc_port
          ?websockets
          ?data_dir
          rollup_node
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

module Client = struct
  include Client

  module Agent = struct
    let create ?(path = Uses.path Constant.octez_client) ?name ?endpoint agent =
      let* path = Agent.copy agent ~source:path in
      let runner = Agent.runner agent in
      create ?runner ?name ~path ?endpoint () |> Lwt.return
  end
end

module Agnostic_baker = struct
  include Agnostic_baker

  module Agent = struct
    let init ?(group = "L1") ?env ?name ?event_sections_levels ~delegates
        ?(path = Uses.path Constant.octez_agnostic_baker) ~client
        ?dal_node_rpc_endpoint ?dal_node_timeout_percentage
        ~ppx_profiling_verbosity ~ppx_profiling_backends
        ?allow_fixed_random_seed ?allow_signing_delay ?extra_arguments node
        cloud agent =
      let* path = Agent.copy agent ~source:path in
      let* () =
        Cloud.register_binary
          cloud
          ~agents:[agent]
          ~group
          ~name:(Filename.basename path)
          ()
      in
      let runner = Agent.runner agent in
      let* baker =
        init
          ?env:
            (may_add_profiling_to_env
               ~ppx_profiling_verbosity
               ~ppx_profiling_backends
               env)
          ?event_sections_levels
          ?name
          ~event_level:`Notice
          ?runner
          ~path
          ~delegates
          ?dal_node_rpc_endpoint
          ?dal_node_timeout_percentage
          ?allow_fixed_random_seed
          ?allow_signing_delay
          ?extra_arguments
          node
          client
      in
      let name =
        (* Baker logs are stored in its base-dir that is the client base-dir in
           teztcloud. *)
        Filename.basename (Client.base_dir client)
      in
      let on_shutdown =
        match Agent.artifacts_dir agent with
        | Some destination_root ->
            (if Tezt_cloud_cli.retrieve_daily_logs then
               [
                 (fun () ->
                   Agent_kind.Logs.scp_logs
                     ~log_dir_name:"logs"
                     ~destination_root
                     ~daemon_name:name
                     agent);
               ]
             else [])
            @
            if Tezt_cloud_cli.retrieve_ppx_profiling_traces then
              [
                (fun () ->
                  Agent_kind.Logs.scp_profiling
                    ~destination_root
                    ~daemon_name:name
                    agent);
              ]
            else []
        | None -> []
      in
      Cloud.service_register ~name ~executable:path ~on_shutdown agent ;
      Lwt.return baker
  end
end

module Accuser = struct
  include Accuser

  module Agent = struct
    let init ?(group = "L1") ?name ?(path = Uses.path Constant.octez_accuser)
        node cloud agent =
      let* path = Agent.copy agent ~source:path in
      let* () =
        Cloud.register_binary
          cloud
          ~agents:[agent]
          ~group
          ~name:(Filename.basename path)
          ()
      in
      let runner = Agent.runner agent in
      init ?name ~event_level:`Notice ?runner ~path node
  end
end

module Teztale = struct
  include Teztale

  module Server = struct
    include Teztale.Server

    let run ?(group = "teztale") cloud agent
        ?(path = Uses.path Constant.teztale_server) ?address ?name ?port ?users
        ?admin ?public_directory () =
      let runner = Agent.runner agent in
      let port =
        match port with
        | Some port -> port
        | None -> Agent.next_available_port agent
      in
      let* path = Agent.copy agent ~source:path in
      let* () =
        Cloud.register_binary
          cloud
          ~agents:[agent]
          ~group
          ~name:(Filename.basename path)
          ()
      in
      Teztale.Server.run
        ?runner
        ~path
        ?address
        ~port
        ?name
        ?users
        ?admin
        ?public_directory
        ()
  end

  module Archiver = struct
    include Teztale.Archiver

    let run ?(group = "teztale") cloud agent
        ?(path = Uses.path Constant.teztale_archiver) ?name ~node_port user feed
        =
      let runner = Agent.runner agent in
      let* path = Agent.copy agent ~source:path in
      let* () =
        Cloud.register_binary
          cloud
          ~agents:[agent]
          ~group
          ~name:(Filename.basename path)
          ()
      in
      Teztale.Archiver.run ?runner ~path ?name ~node_port user feed
  end

  type t = {
    agent : Agent.t;
    server : Server.t;
    address : string;
    artifact_dir : string option;
    mutable archivers : Archiver.t list;
  }

  let user ~agent_name ~node_name : user =
    let login = "teztale-archiver-" ^ agent_name ^ "-" ^ node_name in
    {login; password = login}

  let run_server
      ?(path =
        Uses.(path (make ~tag:"codec" ~path:"./octez-teztale-server" ())))
      ?artifact_dir cloud agent =
    let public_directory = "/tmp/teztale/public" in
    let* () = create_dir public_directory in
    let aliases_filename =
      Filename.concat public_directory "delegates-aliases.json"
    in
    write_file aliases_filename ~contents:"[]" ;
    let* _ =
      Agent.copy ~source:aliases_filename ~destination:aliases_filename agent
    in
    let* _ =
      (* Teztale dataviz is a single page app written with React.
         Copying the same main page (index.html) allow you to be able to share
         links directly to the right tab. Without this hack, going to e.g.
         http://your.teztale.server/Available directly would produce a 404 error. *)
      Lwt_list.map_s
        (fun dst ->
          Agent.copy
            ~source:"tezt/lib_cloud/teztale-visualisation/index.html"
            ~destination:(Format.asprintf "%s/%s" public_directory dst)
            agent)
        [
          "index.html";
          "Available";
          "CommitteeSizePerDelay";
          "CommitteeSizePerLevel";
          "DelayToConsensus";
          "DelegateStats";
          "LevelDiff";
          "Level";
          "Report";
        ]
    in
    let* _ =
      Agent.copy
        ~is_directory:true
        ~source:"tezt/lib_cloud/teztale-visualisation/assets"
        ~destination:public_directory
        agent
    in
    let* server = Server.run ~path ~public_directory cloud agent () in
    let address =
      match Agent.point agent with
      | None -> "127.0.0.1"
      | Some point -> fst point
    in
    let name = server.conf.name in
    let metric_name = "service_manager_process_alive" in
    let on_alive_callback ~alive =
      Cloud.push_metric
        cloud
        ~help:(Format.asprintf "'Process %s is alive'" name)
        ~typ:`Gauge
        ~name:metric_name
        ~labels:
          [
            ("agent", Agent.name agent);
            ("name", name);
            ("executable", path);
            ("kind", "alive");
          ]
        (if alive then 1.0 else 0.0)
    in
    let node = {agent; server; address; artifact_dir; archivers = []} in
    let on_shutdown =
      match artifact_dir with
      | None -> []
      | Some dir ->
          [
            (fun () ->
              Lwt_list.iter_s
                (fun _archiver ->
                  let db_destination =
                    Artifact_helpers.local_path
                      [dir; Filename.basename server.filenames.db_filename]
                  in
                  let* () =
                    Agent.scp
                      agent
                      ~is_directory:false
                      ~source:server.filenames.db_filename
                      ~destination:db_destination
                      `FromRunner
                  in
                  let conf_destination =
                    Artifact_helpers.local_path
                      [dir; Filename.basename server.filenames.conf_filename]
                  in
                  Agent.scp
                    agent
                    ~is_directory:false
                    ~source:server.filenames.conf_filename
                    ~destination:conf_destination
                    `FromRunner)
                node.archivers);
          ]
    in
    Cloud.service_register
      ~name
      ~executable:path
      ~on_alive_callback
      ~on_shutdown
      agent ;
    Lwt.return node

  let wait_server t = Server.wait_for_readiness t.server

  let add_archiver
      ?(path =
        Uses.(path (make ~tag:"codec" ~path:"./octez-teztale-archiver" ()))) t
      cloud agent ~node_name ~node_port =
    let user = user ~agent_name:(Agent.name agent) ~node_name in
    let feed : interface list =
      [{address = t.address; port = t.server.conf.interface.port}]
    in
    let* () =
      Lwt_result.get_exn
        (Server.add_user
           ?runner:(Agent.runner agent)
           ~public_address:t.address
           t.server
           user)
    in
    let* archiver = Archiver.run cloud agent ~path user feed ~node_port in
    t.archivers <- archiver :: t.archivers ;
    Lwt.return_unit

  let update_alias t ~address ~alias =
    let dir = Option.get t.server.conf.public_directory in
    let filename = Filename.concat dir "delegates-aliases.json" in
    let aliases =
      match JSON.parse_file filename |> JSON.unannotate with
      | exception _ -> []
      | `A aliases -> aliases
      | _ -> assert false
    in
    let update = `O [("alias", `String alias); ("address", `String address)] in
    let aliases = update :: aliases in
    JSON.encode_to_file_u filename (`A aliases) ;
    let* _ =
      Agent.copy ~refresh:true ~source:filename ~destination:filename t.agent
    in
    Lwt.return_unit
end
