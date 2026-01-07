(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 TriliTech <contact@trili.tech>                         *)
(* Copyright (c) 2023 Functori, <contact@functori.com>                       *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
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

open Node_context

let lock ~data_dir =
  let open Lwt_result_syntax in
  let*! () = Event.acquiring_lock () in
  let*! () = Lwt_utils_unix.create_dir data_dir in
  let lockfile_path = global_lockfile_path ~data_dir in
  Lwt_lock_file.lock
    ~when_locked:
      (`Fail (Rollup_node_errors.Could_not_acquire_lock lockfile_path))
    ~filename:lockfile_path

let unlock {lockfile; _} = Lwt_lock_file.unlock lockfile

let update_metadata ({Metadata.rollup_address; _} as metadata) ~data_dir =
  let open Lwt_result_syntax in
  let* disk_metadata = Metadata.Versioned.read_metadata_file ~dir:data_dir in
  match disk_metadata with
  | Some (V1 {rollup_address = saved_address; context_version; _}) ->
      let*? () = Context.Version.check context_version in
      fail_unless Address.(rollup_address = saved_address)
      @@ Rollup_node_errors.Unexpected_rollup {rollup_address; saved_address}
  | Some (V0 {rollup_address = saved_address; context_version}) ->
      let*? () = Context.Version.check context_version in
      let*? () =
        error_unless Address.(rollup_address = saved_address)
        @@ Rollup_node_errors.Unexpected_rollup {rollup_address; saved_address}
      in
      Metadata.write_metadata_file ~dir:data_dir metadata
  | None -> Metadata.write_metadata_file ~dir:data_dir metadata

let create_sync_info () =
  {
    on_synchronized = Lwt_condition.create ();
    processed_level = 0l;
    sync_level_input = Lwt_watcher.create_input ();
  }

let init (cctxt : #Client_context.full) ~data_dir ~irmin_cache_size
    ?last_whitelist_update ~(store_access : 'store Access_mode.t)
    ~(context_access : 'context Access_mode.t) l1_ctxt genesis_info ~(lcc : lcc)
    ~lpc kind current_protocol
    Configuration.(
      {
        sc_rollup_address = rollup_address;
        dal_node_endpoint;
        log_kernel_debug_file;
        _;
      } as configuration) =
  let open Lwt_result_syntax in
  let* lockfile = lock ~data_dir in
  let metadata =
    {
      Metadata.rollup_address;
      context_version = Context.Version.version;
      kind;
      genesis_info;
    }
  in
  let* () = update_metadata metadata ~data_dir in
  let* store = Node_context.Node_store.init store_access ~data_dir in
  let dal_cctxt =
    Option.map
      Tezos_dal_node_lib.Dal_node_client.make_unix_cctxt
      dal_node_endpoint
  in
  let*? (module Plugin : Protocol_plugin_sig.S) =
    Protocol_plugins.proto_plugin_for_protocol current_protocol.hash
  in
  let (module C) = Plugin.Pvm.context kind in
  let* context =
    Context.load
      (module C)
      ~cache_size:irmin_cache_size
      context_access
      (Configuration.default_context_dir data_dir)
  in
  let* () =
    Node_context.Node_store.check_and_set_history_mode
      store_access
      store
      configuration.history_mode
  in
  let*! () = Event.rollup_exists ~addr:rollup_address ~kind in
  let*! () =
    if dal_cctxt = None && current_protocol.constants.dal.feature_enable then
      Event.warn_dal_enabled_no_node ()
    else Lwt.return_unit
  in
  let*! kernel_debug_logger, kernel_debug_finaliser =
    make_kernel_logger
      ~enable_tracing:true
      ?log_kernel_debug_file
      ~logs_dir:data_dir
      configuration
      Event.kernel_debug
  in
  let global_block_watcher = Lwt_watcher.create_input () in
  let finalized_block_watcher = Lwt_watcher.create_input () in
  let private_info =
    Option.map
      (fun (message_index, outbox_level) ->
        {
          last_whitelist_update =
            {outbox_level; message_index = Z.to_int message_index};
          last_outbox_level_searched = outbox_level;
        })
      last_whitelist_update
  in
  let*? unsafe_patches =
    Pvm_patches.make kind rollup_address configuration.unsafe_pvm_patches
  in
  let sync = create_sync_info () in
  Metrics.wrap (fun () ->
      Metrics.Info.set_lcc_level_l1 lcc.level ;
      Metrics.Info.set_lcc_level_local lcc.level ;
      Option.iter
        (fun {Commitment.inbox_level = l; _} ->
          Metrics.Info.set_lpc_level_l1 l ;
          Metrics.Info.set_lpc_level_local l)
        lpc) ;
  let node_ctxt =
    {
      config = configuration;
      cctxt :> Client_context.full;
      degraded = Reference.new_ false;
      dal_cctxt;
      data_dir;
      l1_ctxt;
      genesis_info;
      lcc = Reference.new_ lcc;
      lpc = Reference.new_ lpc;
      private_info = Reference.new_ private_info;
      kind;
      unsafe_patches;
      injector_retention_period = 0;
      block_finality_time = 2;
      lockfile;
      store;
      context;
      kernel_debug_logger;
      finaliser = kernel_debug_finaliser;
      current_protocol = Reference.new_ current_protocol;
      global_block_watcher;
      finalized_block_watcher;
      sync;
    }
  in
  let* l2_head = Node_context.last_processed_head_opt node_ctxt in
  let processed_level =
    match l2_head with Some {header = {level; _}; _} -> level | None -> 0l
  in
  sync.processed_level <- processed_level ;
  return node_ctxt

let close ({cctxt; store; context; l1_ctxt; finaliser; _} as node_ctxt) =
  let open Lwt_result_syntax in
  let message = cctxt#message in
  let*! () = message "Running finaliser@." in
  let*! () = finaliser () in
  let*! () = message "Shutting down L1@." in
  let*! () = Layer1.shutdown l1_ctxt in
  let*! () = message "Closing context@." in
  let*! () = Context.close context in
  let*! () = message "Closing store@." in
  let*! () = Node_context.Node_store.close store in
  let*! () = message "Releasing lock@." in
  let*! () = unlock node_ctxt in
  return_unit

module For_snapshots = struct
  let create_node_context cctxt current_protocol store context ~data_dir
      ~apply_unsafe_patches =
    let open Lwt_result_syntax in
    let loser_mode = Loser_mode.no_failures in
    let l1_blocks_cache_size = Configuration.default_l1_blocks_cache_size in
    let l2_blocks_cache_size = Configuration.default_l2_blocks_cache_size in
    let index_buffer_size = Configuration.default_index_buffer_size in
    let irmin_cache_size = Configuration.default_irmin_cache_size in
    let l1_rpc_timeout = Configuration.default_l1_rpc_timeout in
    let* metadata = Metadata.read_metadata_file ~dir:data_dir in
    let*? metadata =
      match metadata with
      | None -> error_with "Missing metadata file for snapshot node context"
      | Some m -> Ok m
    in
    let mode = Configuration.Observer in
    let* operators =
      Purpose.make_operators
        ~needed_purposes:(Configuration.purposes_of_mode mode)
        []
    in
    let config =
      Configuration.
        {
          sc_rollup_address = metadata.rollup_address;
          etherlink = Address.is_etherlink metadata.rollup_address;
          boot_sector_file = None;
          operators;
          rpc_addr = Configuration.default_rpc_addr;
          rpc_port = Configuration.default_rpc_port;
          acl = Configuration.default_acl;
          metrics_addr = None;
          performance_metrics = false;
          reconnection_delay = 1.;
          fee_parameters = Configuration.default_fee_parameters;
          mode;
          loser_mode;
          apply_unsafe_patches;
          unsafe_pvm_patches = [];
          unsafe_disable_wasm_kernel_checks = false;
          execute_outbox_messages_filter =
            Configuration.default_execute_outbox_filter;
          dal_node_endpoint = None;
          batcher = Configuration.default_batcher;
          injector = Configuration.default_injector;
          l1_blocks_cache_size;
          l2_blocks_cache_size;
          index_buffer_size = Some index_buffer_size;
          irmin_cache_size = Some irmin_cache_size;
          prefetch_blocks = None;
          l1_monitor_finalized = false;
          log_kernel_debug = false;
          log_kernel_debug_file = None;
          no_degraded = false;
          gc_parameters = Configuration.default_gc_parameters;
          history_mode = None;
          cors = Resto_cohttp.Cors.default;
          l1_rpc_timeout;
          loop_retry_delay = 10.;
          pre_images_endpoint = None;
          bail_on_disagree = false;
          opentelemetry = Octez_telemetry.Opentelemetry_config.default;
          dal_slot_status_max_fetch_attempts =
            Configuration.default_dal_slot_status_max_fetch_attempts;
        }
    in
    let*? l1_ctxt =
      Layer1.create
        ~name:"smart_rollup_node.snapshot"
        ~reconnection_delay:config.reconnection_delay
        ~l1_blocks_cache_size
        cctxt
    in
    let* lcc = Store.State.LCC.get store in
    let lcc =
      match lcc with
      | Some (commitment, level) -> {commitment; level}
      | None ->
          {
            commitment = metadata.genesis_info.commitment_hash;
            level = metadata.genesis_info.level;
          }
    in
    let* lpc = Store.Commitments.find_lpc store in
    let*! lockfile =
      Lwt_unix.openfile (Filename.temp_file "lock" "") [] 0o644
    in
    let global_block_watcher = Lwt_watcher.create_input () in
    let finalized_block_watcher = Lwt_watcher.create_input () in
    let sync = create_sync_info () in
    let*? unsafe_patches =
      Pvm_patches.make
        metadata.kind
        metadata.rollup_address
        config.unsafe_pvm_patches
    in
    return
      {
        config;
        cctxt :> Client_context.full;
        degraded = Reference.new_ false;
        dal_cctxt = None;
        data_dir;
        l1_ctxt;
        genesis_info = metadata.genesis_info;
        lcc = Reference.new_ lcc;
        lpc = Reference.new_ lpc;
        private_info = Reference.new_ None;
        kind = metadata.kind;
        unsafe_patches;
        injector_retention_period = 0;
        block_finality_time = 2;
        lockfile;
        store = Node_context.Node_store.of_store store;
        context;
        kernel_debug_logger = (fun _ -> Lwt.return_unit);
        finaliser = Lwt.return;
        current_protocol = Reference.new_ current_protocol;
        global_block_watcher;
        finalized_block_watcher;
        sync;
      }
end

module Internal_for_tests = struct
  let create_node_context cctxt (current_protocol : current_protocol) ~data_dir
      kind =
    let open Lwt_result_syntax in
    let rollup_address = Address.zero in
    let mode = Configuration.Observer in
    let* operators =
      Purpose.make_operators
        ~needed_purposes:(Configuration.purposes_of_mode mode)
        []
    in
    let loser_mode = Loser_mode.no_failures in
    let l1_blocks_cache_size = Configuration.default_l1_blocks_cache_size in
    let l2_blocks_cache_size = Configuration.default_l2_blocks_cache_size in
    let index_buffer_size = Configuration.default_index_buffer_size in
    let irmin_cache_size = Configuration.default_irmin_cache_size in
    let l1_rpc_timeout = Configuration.default_l1_rpc_timeout in
    let config =
      Configuration.
        {
          sc_rollup_address = rollup_address;
          etherlink = Address.is_etherlink rollup_address;
          boot_sector_file = None;
          operators;
          rpc_addr = Configuration.default_rpc_addr;
          rpc_port = Configuration.default_rpc_port;
          acl = Configuration.default_acl;
          metrics_addr = None;
          performance_metrics = false;
          reconnection_delay = 5.;
          fee_parameters = Configuration.default_fee_parameters;
          mode;
          loser_mode;
          apply_unsafe_patches = false;
          unsafe_pvm_patches = [];
          unsafe_disable_wasm_kernel_checks = false;
          execute_outbox_messages_filter =
            Configuration.default_execute_outbox_filter;
          dal_node_endpoint = None;
          pre_images_endpoint = None;
          batcher = Configuration.default_batcher;
          injector = Configuration.default_injector;
          l1_blocks_cache_size;
          l2_blocks_cache_size;
          index_buffer_size = Some index_buffer_size;
          irmin_cache_size = Some irmin_cache_size;
          prefetch_blocks = None;
          l1_monitor_finalized = false;
          l1_rpc_timeout;
          loop_retry_delay = 10.;
          log_kernel_debug = false;
          log_kernel_debug_file = None;
          no_degraded = false;
          gc_parameters = Configuration.default_gc_parameters;
          history_mode = None;
          cors = Resto_cohttp.Cors.default;
          bail_on_disagree = false;
          opentelemetry = Octez_telemetry.Opentelemetry_config.default;
          dal_slot_status_max_fetch_attempts =
            Configuration.default_dal_slot_status_max_fetch_attempts;
        }
    in
    let* lockfile = lock ~data_dir in
    let genesis_info = {level = 0l; commitment_hash = Commitment.Hash.zero} in
    let* store = Node_context.Node_store.init Read_write ~data_dir in
    let*? (module Plugin : Protocol_plugin_sig.S) =
      Protocol_plugins.proto_plugin_for_protocol current_protocol.hash
    in
    let (module C) = Plugin.Pvm.context kind in
    let* context =
      Context.load
        (module C)
        Read_write
        (Configuration.default_context_dir data_dir)
        ~cache_size:irmin_cache_size
    in
    let l1_ctxt = Layer1.Internal_for_tests.dummy cctxt in
    let lcc = Reference.new_ {commitment = Commitment.Hash.zero; level = 0l} in
    let lpc = Reference.new_ None in
    let* () =
      Node_context.Internal_for_tests.write_protocols_in_store
        store
        [
          Store.Protocols.
            {
              level = Activation_level 0l;
              proto_level = current_protocol.proto_level;
              protocol = current_protocol.hash;
            };
        ]
    in
    let global_block_watcher = Lwt_watcher.create_input () in
    let finalized_block_watcher = Lwt_watcher.create_input () in
    let sync = create_sync_info () in
    let*? unsafe_patches = Pvm_patches.make kind rollup_address [] in
    return
      {
        config;
        cctxt :> Client_context.full;
        degraded = Reference.new_ false;
        dal_cctxt = None;
        data_dir;
        l1_ctxt;
        genesis_info;
        lcc;
        lpc;
        private_info = Reference.new_ None;
        kind;
        unsafe_patches;
        injector_retention_period = 0;
        block_finality_time = 2;
        current_protocol = Reference.new_ current_protocol;
        lockfile;
        store;
        context;
        kernel_debug_logger = Event.kernel_debug;
        finaliser = (fun () -> Lwt.return_unit);
        global_block_watcher;
        finalized_block_watcher;
        sync;
      }

  let openapi_context cctxt protocol =
    let current_protocol =
      {
        hash = protocol;
        proto_level = 0;
        constants =
          Rollup_constants.
            {
              minimal_block_delay = 0L;
              delay_increment_per_round = 0L;
              sc_rollup =
                {
                  challenge_window_in_blocks = 0;
                  commitment_period_in_blocks = 0;
                  reveal_activation_level =
                    Some
                      {
                        blake2B = 0l;
                        metadata = 0l;
                        dal_page = 0l;
                        dal_parameters = 0l;
                        dal_attested_slots_validity_lag = Int32.max_int;
                      };
                  max_number_of_stored_cemented_commitments = 0;
                  max_active_outbox_levels = 0;
                };
              dal =
                {
                  feature_enable = false;
                  attestation_lag = 0;
                  attestation_lags = [];
                  dynamic_lag_enable = false;
                  number_of_slots = 0;
                  cryptobox_parameters =
                    {
                      redundancy_factor = 0;
                      page_size = 0;
                      slot_size = 0;
                      number_of_shards = 0;
                    };
                };
            };
      }
    in
    Lwt_utils_unix.with_tempdir "smart-rollup-node-openapi" @@ fun data_dir ->
    let open Lwt_result_syntax in
    let* node_ctxt =
      create_node_context cctxt current_protocol ~data_dir Wasm_2_0_0
    in
    let*! () = Context.close node_ctxt.context in
    let*! () = Node_context.Node_store.close node_ctxt.store in
    return node_ctxt
end
