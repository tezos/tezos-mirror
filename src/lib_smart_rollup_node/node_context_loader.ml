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
  let lockfile_path = global_lockfile_path ~data_dir in
  let lock_aux ~data_dir =
    let open Lwt_result_syntax in
    let*! () = Event.acquiring_lock () in
    let*! () = Lwt_utils_unix.create_dir data_dir in
    let* lockfile =
      protect @@ fun () ->
      Lwt_unix.openfile
        lockfile_path
        [Unix.O_CREAT; O_RDWR; O_CLOEXEC; O_SYNC]
        0o644
      |> Lwt_result.ok
    in
    let* () =
      protect ~on_error:(fun err ->
          let*! () = Lwt_unix.close lockfile in
          fail err)
      @@ fun () ->
      let*! () = Lwt_unix.lockf lockfile Unix.F_LOCK 0 in
      return_unit
    in
    return lockfile
  in
  trace (Rollup_node_errors.Could_not_acquire_lock lockfile_path)
  @@ lock_aux ~data_dir

let unlock {lockfile; _} =
  Lwt.finalize
    (fun () -> Lwt_unix.lockf lockfile Unix.F_ULOCK 0)
    (fun () -> Lwt_unix.close lockfile)

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

let init (cctxt : #Client_context.full) ~data_dir ~irmin_cache_size
    ~index_buffer_size ?log_kernel_debug_file ?last_whitelist_update mode
    l1_ctxt genesis_info ~lcc ~lpc kind current_protocol
    Configuration.(
      {
        sc_rollup_address = rollup_address;
        l2_blocks_cache_size;
        dal_node_endpoint;
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
  let* () =
    Store_migration.maybe_run_migration
      metadata
      ~storage_dir:(Configuration.default_storage_dir data_dir)
      ~index_buffer_size:Configuration.default_index_buffer_size
  in
  let dal_cctxt =
    Option.map Dal_node_client.make_unix_cctxt dal_node_endpoint
  in
  let* store =
    Node_context.Node_store.load
      mode
      ~index_buffer_size
      ~l2_blocks_cache_size
      Configuration.(default_storage_dir data_dir)
  in
  let*? (module Plugin : Protocol_plugin_sig.S) =
    Protocol_plugins.proto_plugin_for_protocol current_protocol.hash
  in
  let (module C) = Plugin.Pvm.context kind in
  let* context =
    Context.load
      (module C)
      ~cache_size:irmin_cache_size
      mode
      (Configuration.default_context_dir data_dir)
  in
  let* () =
    Node_context.Node_store.check_and_set_history_mode
      mode
      store
      configuration.history_mode
  in
  let*! () = Event.rollup_exists ~addr:rollup_address ~kind in
  let*! () =
    if dal_cctxt = None && current_protocol.constants.dal.feature_enable then
      Event.warn_dal_enabled_no_node ()
    else Lwt.return_unit
  in
  let* dac_client =
    Option.map_es
      (fun observer_endpoint ->
        Dac_observer_client.init
          {
            observer_endpoint;
            reveal_data_dir = Filename.concat data_dir (Kind.to_string kind);
            timeout_seconds = configuration.dac_timeout;
          })
      configuration.dac_observer_endpoint
  in
  let*! kernel_debug_logger, kernel_debug_finaliser =
    let open Lwt_syntax in
    if configuration.log_kernel_debug then
      make_kernel_logger Event.kernel_debug ?log_kernel_debug_file data_dir
    else return (Event.kernel_debug, fun () -> return_unit)
  in
  let global_block_watcher = Lwt_watcher.create_input () in
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
  return
    {
      config = configuration;
      cctxt = (cctxt :> Client_context.full);
      dal_cctxt;
      dac_client;
      data_dir;
      l1_ctxt;
      genesis_info;
      lcc = Reference.new_ lcc;
      lpc = Reference.new_ lpc;
      private_info = Reference.new_ private_info;
      kind;
      injector_retention_period = 0;
      block_finality_time = 2;
      lockfile;
      store;
      context;
      kernel_debug_logger;
      finaliser = kernel_debug_finaliser;
      current_protocol;
      global_block_watcher;
    }

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
  let* () = Node_context.Node_store.close store in
  let*! () = message "Releasing lock@." in
  let*! () = unlock node_ctxt in
  return_unit

module Internal_for_tests = struct
  let create_node_context cctxt (current_protocol : current_protocol) ~data_dir
      kind =
    let open Lwt_result_syntax in
    let rollup_address = Address.zero in
    let mode = Configuration.Observer in
    let*? operators =
      Purpose.make_operator
        ~needed_purposes:(Configuration.purposes_of_mode mode)
        []
    in
    let loser_mode = Loser_mode.no_failures in
    let l1_blocks_cache_size = Configuration.default_l1_blocks_cache_size in
    let l2_blocks_cache_size = Configuration.default_l2_blocks_cache_size in
    let index_buffer_size = Configuration.default_index_buffer_size in
    let irmin_cache_size = Configuration.default_irmin_cache_size in
    let config =
      Configuration.
        {
          sc_rollup_address = rollup_address;
          boot_sector_file = None;
          operators;
          rpc_addr = Configuration.default_rpc_addr;
          rpc_port = Configuration.default_rpc_port;
          metrics_addr = None;
          reconnection_delay = 5.;
          fee_parameters = Configuration.default_fee_parameters;
          mode;
          loser_mode;
          dal_node_endpoint = None;
          dac_observer_endpoint = None;
          dac_timeout = None;
          batcher = Configuration.default_batcher;
          injector = Configuration.default_injector;
          l1_blocks_cache_size;
          l2_blocks_cache_size;
          index_buffer_size = Some index_buffer_size;
          irmin_cache_size = Some irmin_cache_size;
          prefetch_blocks = None;
          log_kernel_debug = false;
          no_degraded = false;
          gc_parameters = Configuration.default_gc_parameters;
          history_mode = Configuration.default_history_mode;
          cors = Resto_cohttp.Cors.default;
        }
    in
    let* lockfile = lock ~data_dir in
    let* store =
      Node_context.Node_store.load
        Read_write
        ~index_buffer_size
        ~l2_blocks_cache_size
        Configuration.(default_storage_dir data_dir)
    in
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
    let genesis_info = {level = 0l; commitment_hash = Commitment.Hash.zero} in
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
    return
      {
        config;
        cctxt = (cctxt :> Client_context.full);
        dal_cctxt = None;
        dac_client = None;
        data_dir;
        l1_ctxt;
        genesis_info;
        lcc;
        lpc;
        private_info = Reference.new_ None;
        kind;
        injector_retention_period = 0;
        block_finality_time = 2;
        current_protocol;
        lockfile;
        store;
        context;
        kernel_debug_logger = Event.kernel_debug;
        finaliser = (fun () -> Lwt.return_unit);
        global_block_watcher;
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
                      };
                  max_number_of_stored_cemented_commitments = 0;
                };
              dal =
                {
                  feature_enable = false;
                  attestation_lag = 0;
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
    let* () = Node_context.Node_store.close node_ctxt.store in
    return node_ctxt
end
