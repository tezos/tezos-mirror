(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 Trili Tech, <contact@trili.tech>                       *)
(* Copyright (c) 2023 Functori, <contact@functori.com>                       *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

include Cli.Binary_dependent_args (struct
  let binary_name = "smart rollup node"
end)

let group =
  {
    Tezos_clic.name = "sc_rollup.node";
    title = "Commands related to the smart rollup node.";
  }

let config_init_command =
  let open Lwt_result_syntax in
  let open Tezos_clic in
  let open Cli in
  command
    ~group
    ~desc:"Configure the smart rollup node."
    (merge_options
       (args25
          force_switch
          data_dir_arg
          rpc_addr_arg
          rpc_port_arg
          acl_override_arg
          metrics_addr_arg
          enable_performance_metrics_arg
          loser_mode_arg
          reconnection_delay_arg
          dal_node_endpoint_arg
          dac_observer_endpoint_arg
          dac_timeout_arg
          pre_images_endpoint_arg
          injector_retention_period_arg
          injector_attempts_arg
          injection_ttl_arg
          index_buffer_size_arg
          index_buffer_size_arg
          log_kernel_debug_arg
          boot_sector_file_arg
          no_degraded_arg
          gc_frequency_arg
          history_mode_arg
          cors_allowed_origins_arg
          cors_allowed_headers_arg)
       (args1 bail_on_disagree_switch))
    (prefix "init" @@ mode_param
    @@ prefixes ["config"; "for"]
    @@ sc_rollup_address_param
    @@ prefixes ["with"; "operators"]
    @@ seq_of_param @@ operator_param)
    (fun ( ( force,
             data_dir,
             rpc_addr,
             rpc_port,
             acl_override,
             metrics_addr,
             enable_performance_metrics,
             loser_mode,
             reconnection_delay,
             dal_node_endpoint,
             dac_observer_endpoint,
             dac_timeout,
             pre_images_endpoint,
             injector_retention_period,
             injector_attempts,
             injection_ttl,
             index_buffer_size,
             irmin_cache_size,
             log_kernel_debug,
             boot_sector_file,
             no_degraded,
             gc_frequency,
             history_mode,
             allowed_origins,
             allowed_headers ),
           bail_on_disagree )
         mode
         sc_rollup_address
         operators
         cctxt ->
      let* config =
        Configuration.Cli.configuration_from_args
          ~rpc_addr
          ~rpc_port
          ~acl_override
          ~metrics_addr
          ~enable_performance_metrics
          ~loser_mode
          ~reconnection_delay
          ~dal_node_endpoint
          ~dac_observer_endpoint
          ~dac_timeout
          ~pre_images_endpoint
          ~injector_retention_period
          ~injector_attempts
          ~injection_ttl
          ~mode
          ~sc_rollup_address
          ~boot_sector_file
          ~operators
          ~index_buffer_size
          ~irmin_cache_size
          ~log_kernel_debug
          ~no_degraded
          ~gc_frequency
          ~history_mode
          ~allowed_origins
          ~allowed_headers
          ~apply_unsafe_patches:false
          ~bail_on_disagree
      in
      let* () = Configuration.save ~force ~data_dir config in
      let*! () =
        cctxt#message
          "Smart rollup node configuration written in %s"
          (Configuration.config_filename ~data_dir)
      in
      return_unit)

let legacy_run_command =
  let open Tezos_clic in
  let open Lwt_result_syntax in
  let open Cli in
  command
    ~group
    ~desc:"Run the rollup node daemon (deprecated)."
    (merge_options
       (args8
          data_dir_arg
          mode_arg
          sc_rollup_address_arg
          rpc_addr_arg
          rpc_port_arg
          acl_override_arg
          metrics_addr_arg
          enable_performance_metrics_arg)
       (args21
          loser_mode_arg
          reconnection_delay_arg
          dal_node_endpoint_arg
          dac_observer_endpoint_arg
          dac_timeout_arg
          pre_images_endpoint_arg
          injector_retention_period_arg
          injector_attempts_arg
          injection_ttl_arg
          index_buffer_size_arg
          index_buffer_size_arg
          log_kernel_debug_arg
          log_kernel_debug_file_arg
          boot_sector_file_arg
          no_degraded_arg
          gc_frequency_arg
          history_mode_arg
          cors_allowed_origins_arg
          cors_allowed_headers_arg
          apply_unsafe_patches_switch
          bail_on_disagree_switch))
    (prefixes ["run"] @@ stop)
    (fun ( ( data_dir,
             mode,
             sc_rollup_address,
             rpc_addr,
             rpc_port,
             acl_override,
             metrics_addr,
             enable_performance_metrics ),
           ( loser_mode,
             reconnection_delay,
             dal_node_endpoint,
             dac_observer_endpoint,
             dac_timeout,
             pre_images_endpoint,
             injector_retention_period,
             injector_attempts,
             injection_ttl,
             index_buffer_size,
             irmin_cache_size,
             log_kernel_debug,
             log_kernel_debug_file,
             boot_sector_file,
             no_degraded,
             gc_frequency,
             history_mode,
             allowed_origins,
             allowed_headers,
             apply_unsafe_patches,
             bail_on_disagree ) )
         cctxt ->
      let* configuration =
        Configuration.Cli.create_or_read_config
          ~data_dir
          ~rpc_addr
          ~rpc_port
          ~acl_override
          ~metrics_addr
          ~enable_performance_metrics
          ~loser_mode
          ~reconnection_delay
          ~dal_node_endpoint
          ~dac_observer_endpoint
          ~dac_timeout
          ~pre_images_endpoint
          ~injector_retention_period
          ~injector_attempts
          ~injection_ttl
          ~mode
          ~sc_rollup_address
          ~boot_sector_file
          ~operators:[]
          ~index_buffer_size
          ~irmin_cache_size
          ~log_kernel_debug
          ~no_degraded
          ~gc_frequency
          ~history_mode
          ~allowed_origins
          ~allowed_headers
          ~apply_unsafe_patches
          ~bail_on_disagree
      in
      Rollup_node_daemon.run
        ~data_dir
        ~irmin_cache_size:Configuration.default_irmin_cache_size
        ?log_kernel_debug_file
        configuration
        cctxt)

let run_command =
  let open Tezos_clic in
  let open Lwt_result_syntax in
  let open Cli in
  command
    ~group
    ~desc:
      "Run the rollup node daemon. Arguments overwrite values provided in the \
       configuration file."
    (merge_options
       (args12
          data_dir_arg
          rpc_addr_arg
          rpc_port_arg
          acl_override_arg
          metrics_addr_arg
          enable_performance_metrics_arg
          loser_mode_arg
          reconnection_delay_arg
          dal_node_endpoint_arg
          dac_observer_endpoint_arg
          dac_timeout_arg
          pre_images_endpoint_arg)
       (args15
          injector_retention_period_arg
          injector_attempts_arg
          injection_ttl_arg
          index_buffer_size_arg
          irmin_cache_size_arg
          log_kernel_debug_arg
          log_kernel_debug_file_arg
          boot_sector_file_arg
          no_degraded_arg
          gc_frequency_arg
          history_mode_arg
          cors_allowed_origins_arg
          cors_allowed_headers_arg
          apply_unsafe_patches_switch
          bail_on_disagree_switch))
    (prefixes ["run"] @@ mode_param @@ prefixes ["for"]
   @@ sc_rollup_address_param
    @@ prefixes ["with"; "operators"]
    @@ seq_of_param @@ operator_param)
    (fun ( ( data_dir,
             rpc_addr,
             rpc_port,
             acl_override,
             metrics_addr,
             enable_performance_metrics,
             loser_mode,
             reconnection_delay,
             dal_node_endpoint,
             dac_observer_endpoint,
             dac_timeout,
             pre_images_endpoint ),
           ( injector_retention_period,
             injector_attempts,
             injection_ttl,
             index_buffer_size,
             irmin_cache_size,
             log_kernel_debug,
             log_kernel_debug_file,
             boot_sector_file,
             no_degraded,
             gc_frequency,
             history_mode,
             allowed_origins,
             allowed_headers,
             apply_unsafe_patches,
             bail_on_disagree ) )
         mode
         sc_rollup_address
         operators
         cctxt ->
      let* configuration =
        Configuration.Cli.create_or_read_config
          ~data_dir
          ~rpc_addr
          ~rpc_port
          ~acl_override
          ~metrics_addr
          ~enable_performance_metrics
          ~loser_mode
          ~reconnection_delay
          ~dal_node_endpoint
          ~dac_observer_endpoint
          ~dac_timeout
          ~pre_images_endpoint
          ~injector_retention_period
          ~injector_attempts
          ~injection_ttl
          ~mode:(Some mode)
          ~sc_rollup_address:(Some sc_rollup_address)
          ~operators
          ~index_buffer_size
          ~irmin_cache_size
          ~log_kernel_debug
          ~boot_sector_file
          ~no_degraded
          ~gc_frequency
          ~history_mode
          ~allowed_origins
          ~allowed_headers
          ~apply_unsafe_patches
          ~bail_on_disagree
      in
      Rollup_node_daemon.run
        ~data_dir
        ~irmin_cache_size:Configuration.default_irmin_cache_size
        ?log_kernel_debug_file
        configuration
        cctxt)

let protocols_command =
  let open Tezos_clic in
  let open Lwt_result_syntax in
  command
    ~group
    ~desc:"Shows the protocols supported by this rollup node."
    no_options
    (prefixes ["show"; "supported"; "protocols"] @@ stop)
    (fun () (cctxt : #Client_context.full) ->
      let protocols = Protocol_plugins.registered_protocols () in
      let*! () =
        match protocols with
        | [] -> cctxt#error "No protocols supported by rollup node!"
        | _ ->
            cctxt#message
              "@[<v>%a@]"
              (Format.pp_print_list Protocol_hash.pp)
              protocols
      in
      return_unit)

(** Command to dump the rollup node metrics. *)
let dump_metrics =
  let open Tezos_clic in
  let open Lwt_result_syntax in
  command
    ~group
    ~desc:"dump the rollup node available metrics in CSV format."
    no_options
    (prefixes ["dump-metrics"] @@ stop)
    (fun () (cctxt : Client_context.full) ->
      let*! metrics =
        Prometheus.CollectorRegistry.collect Metrics.sc_rollup_node_registry
      in
      let*! () = cctxt#message "%a@." Metrics.print_csv_metrics metrics in
      return_unit)

let dump_durable_storage =
  let open Tezos_clic in
  command
    ~group
    ~desc:"dump the durable_storage."
    (args2 data_dir_arg (Tezos_client_base_unix.Client_config.block_arg ()))
    (prefixes ["dump"; "durable"; "storage"; "into"]
    @@ Cli.wasm_dump_file_param @@ stop)
    (fun (data_dir, block) file cctxt ->
      let open Lwt_result_syntax in
      let*! res =
        Wasm_2_0_0_utilities.dump_durable_storage ~block ~data_dir ~file
      in
      match res with
      | Ok () ->
          let*! () = cctxt#message "Dumped WASM PVM state to %s@." file in
          return_unit
      | Error errs -> cctxt#error "%a" pp_print_trace errs)

let patch_durable_storage =
  let open Tezos_clic in
  command
    ~group
    ~desc:
      "Patches the durable storage with an arbitrary value. This is an unsafe \
       command, it should be used for debugging only. Patched durable storage \
       is persisted and cannot be reverted."
    (args2
       data_dir_arg
       (switch
          ~long:"force"
          ~short:'f'
          ~doc:"Force patching the durable storage"
          ()))
    (prefixes ["patch"; "durable"; "storage"; "at"]
    @@ param ~name:"path" ~desc:"Durable storage path" Cli.string_parameter
    @@ prefixes ["with"]
    @@ param ~name:"value" ~desc:"Patched value" Cli.hex_parameter
    @@ stop)
    (fun (data_dir, force) key value cctxt ->
      if force then
        Wasm_2_0_0_utilities.patch_durable_storage ~data_dir ~key ~value
      else
        cctxt#error
          "You must add --force to your command-line to execute this command. \
           As a reminder, patching the state is an advanced and unsafe \
           procedure.")

let migrate_store =
  let open Tezos_clic in
  command
    ~group
    ~desc:"Migrate the rollup node store to the latest supported version."
    (args1 data_dir_arg)
    (prefixes ["migrate"; "store"] @@ stop)
    (fun data_dir _cctxt ->
      let open Lwt_result_syntax in
      let* metadata = Metadata.read_metadata_file ~dir:data_dir in
      let*? metadata =
        match metadata with
        | None ->
            error_with
              "No metadata for the rollup node in %s. Is the rollup node \
               initialized?"
              data_dir
        | Some m -> Ok m
      in
      let* _fd = Node_context_loader.lock ~data_dir in
      let* () =
        let open Octez_smart_rollup_node_store in
        Store_migration.maybe_run_migration metadata Store.version ~data_dir
      in
      return_unit)

let export_snapshot
    ( data_dir,
      dest,
      no_checks,
      compress_on_the_fly,
      uncompressed,
      compact,
      rollup_node_endpoint ) filename (cctxt : Client_context.full) =
  let open Lwt_result_syntax in
  let*! compression =
    match (compress_on_the_fly, uncompressed) with
    | true, true ->
        cctxt#error "Cannot have both --uncompressed and --compress-on-the-fly"
    | true, false -> Lwt.return Snapshots.On_the_fly
    | false, false -> Lwt.return Snapshots.After
    | false, true -> Lwt.return Snapshots.No
  in
  let* snapshot_file =
    if compact then
      Snapshots.export_compact
        cctxt
        ~no_checks
        ~compression
        ~data_dir
        ~dest
        ~filename
    else
      Snapshots.export
        ?rollup_node_endpoint
        cctxt
        ~no_checks
        ~compression
        ~data_dir
        ~dest
        ~filename
  in
  let*! () = cctxt#message "Snapshot exported to %s@." snapshot_file in
  return_unit

let export_snapshot_auto_name =
  let open Tezos_clic in
  command
    ~group
    ~desc:"Export a snapshot of the rollup node state."
    (args7
       data_dir_arg
       Cli.snapshot_dir_arg
       Cli.no_checks_arg
       Cli.compress_on_the_fly_arg
       Cli.uncompressed
       Cli.compact
       Cli.rollup_node_endpoint_arg)
    (prefixes ["snapshot"; "export"] @@ stop)
    (fun params cctxt -> export_snapshot params None cctxt)

let export_snapshot_named =
  let open Tezos_clic in
  command
    ~group
    ~desc:"Export a snapshot of the rollup node state to a given file."
    (args6
       data_dir_arg
       Cli.no_checks_arg
       Cli.compress_on_the_fly_arg
       Cli.uncompressed
       Cli.compact
       Cli.rollup_node_endpoint_arg)
    (prefixes ["snapshot"; "export"] @@ Cli.snapshot_file_param @@ stop)
    (fun ( data_dir,
           no_checks,
           compress_on_the_fly,
           uncompressed,
           compact,
           rollup_node_endpoint )
         filename
         cctxt ->
      export_snapshot
        ( data_dir,
          None,
          no_checks,
          compress_on_the_fly,
          uncompressed,
          compact,
          rollup_node_endpoint )
        (Some filename)
        cctxt)

let import_snapshot =
  let open Tezos_clic in
  command
    ~group
    ~desc:"Import a snapshot file in a rollup node."
    (args4
       data_dir_arg
       Cli.no_checks_arg
       Cli.import_force_switch
       Cli.apply_unsafe_patches_switch)
    (prefixes ["snapshot"; "import"] @@ Cli.snapshot_file_param @@ stop)
    (fun (data_dir, no_checks, force, apply_unsafe_patches) snapshot_file cctxt ->
      let open Lwt_result_syntax in
      let* () =
        Snapshots.import
          ~apply_unsafe_patches
          cctxt
          ~no_checks
          ~force
          ~data_dir
          ~snapshot_file
      in
      let*! () = cctxt#message "Snapshot successfully imported@." in
      return_unit)

let snapshot_info =
  let open Tezos_clic in
  command
    ~group
    ~desc:"Display information about a snapshot file."
    no_options
    (prefixes ["snapshot"; "info"] @@ Cli.snapshot_file_param @@ stop)
    (fun () snapshot_file cctxt ->
      let open Lwt_result_syntax in
      let ( Snapshots.Header.
              {version = _; history_mode; address; head_level; last_commitment},
            compressed ) =
        Snapshots.info ~snapshot_file
      in
      let*! () =
        cctxt#message
          "@[<v 0>Valid smart rollup node snapshot.@,\
           Format:          %scompressed@,\
           History mode:    %s@,\
           Rollup address:  %a@,\
           Head level:      %ld@,\
           Last commitment: %a@]"
          (match compressed with `Compressed -> "" | `Uncompressed -> "un")
          (Configuration.string_of_history_mode history_mode)
          Address.pp
          address
          head_level
          Commitment.Hash.pp
          last_commitment
      in
      return_unit)

let openapi_command =
  let open Tezos_clic in
  let open Lwt_result_syntax in
  command
    ~group
    ~desc:"Generate OpenAPI specification."
    (args1 Cli.protocol_hash_arg)
    (prefixes ["generate"; "openapi"] @@ stop)
    (fun protocol cctxt ->
      let* openapi_json = Rpc_directory.generate_openapi ?protocol cctxt in
      let openapi_json_str =
        Data_encoding.Json.to_string ~minify:true openapi_json
      in
      let*! () = cctxt#message "%s" openapi_json_str in
      return_unit)

let sc_rollup_commands () =
  [
    config_init_command;
    run_command;
    legacy_run_command;
    protocols_command;
    dump_metrics;
    dump_durable_storage;
    patch_durable_storage;
    migrate_store;
    export_snapshot_auto_name;
    export_snapshot_named;
    import_snapshot;
    snapshot_info;
    openapi_command;
  ]
  @ Repair.commands

let select_commands _ctxt _ = Lwt_result_syntax.return (sc_rollup_commands ())

let global_options () =
  let open Client_config in
  Tezos_clic.args11
    (base_dir_arg ())
    (no_base_dir_warnings_switch ())
    (timings_switch ())
    (log_requests_switch ())
    (better_errors ())
    (addr_arg ())
    (port_arg ())
    (tls_switch ())
    (endpoint_arg ())
    (remote_signer_arg ())
    (password_filename_arg ())

module Daemon_node_config = struct
  type t =
    string option
    * bool
    * bool
    * bool
    * bool
    * string option
    * int option
    * bool
    * Uri.t option
    * Uri.t option
    * string option

  let global_options = global_options

  let parse_config_args = Client_config.parse_config_args

  let default_chain = Client_config.default_chain

  let default_block = Client_config.default_block

  let default_base_dir = Client_config.default_base_dir

  let default_media_type = Daemon_config.default_media_type

  let other_registrations = None

  let default_daily_logs_path = None

  let logger = None

  let clic_commands ~base_dir:_ ~config_commands:_ ~builtin_commands:_
      ~other_commands ~require_auth:_ =
    other_commands
end

let () = Client_main_run.run (module Daemon_node_config) ~select_commands
