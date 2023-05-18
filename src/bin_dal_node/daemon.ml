(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

let resolve_plugin
    (protocols : Tezos_shell_services.Chain_services.Blocks.protocols) =
  let open Lwt_syntax in
  let plugin_opt =
    Option.either
      (Dal_plugin.get protocols.current_protocol)
      (Dal_plugin.get protocols.next_protocol)
  in
  Option.map_s
    (fun dal_plugin ->
      let (module Dal_plugin : Dal_plugin.T) = dal_plugin in
      let* () = Event.emit_protocol_plugin_resolved Dal_plugin.Proto.hash in
      return dal_plugin)
    plugin_opt

type error +=
  | Cryptobox_initialisation_failed of string
  | Reveal_data_path_not_a_directory of string
  | Cannot_create_reveal_data_dir of string

let () =
  register_error_kind
    `Permanent
    ~id:"dal.node.cryptobox.initialisation_failed"
    ~title:"Cryptobox initialisation failed"
    ~description:"Unable to initialise the cryptobox parameters"
    ~pp:(fun ppf msg ->
      Format.fprintf
        ppf
        "Unable to initialise the cryptobox parameters. Reason: %s"
        msg)
    Data_encoding.(obj1 (req "error" string))
    (function Cryptobox_initialisation_failed str -> Some str | _ -> None)
    (fun str -> Cryptobox_initialisation_failed str)

let init_cryptobox unsafe_srs (proto_parameters : Dal_plugin.proto_parameters) =
  let open Lwt_result_syntax in
  let* () =
    let use_mock_srs_for_testing : Cryptobox.parameters option =
      if unsafe_srs then Some proto_parameters.cryptobox_parameters else None
    in
    let find_srs_files () = Tezos_base.Dal_srs.find_trusted_setup_files () in
    Cryptobox.Config.init_dal
      ~find_srs_files
      Cryptobox.Config.{activated = true; use_mock_srs_for_testing}
  in
  match Cryptobox.make proto_parameters.cryptobox_parameters with
  | Ok cryptobox -> return cryptobox
  | Error (`Fail msg) -> fail [Cryptobox_initialisation_failed msg]

module Handler = struct
  (** [make_stream_daemon handler streamed_call] calls [handler] on each newly
      received value from [streamed_call].

      It returns a couple [(p, stopper)] where [p] is a promise resolving when the
      stream closes and [stopper] a function closing the stream.
  *)
  let make_stream_daemon handle streamed_call =
    let open Lwt_result_syntax in
    let* stream, stopper = streamed_call in
    let rec go () =
      let*! tok = Lwt_stream.get stream in
      match tok with
      | None -> return_unit
      | Some element ->
          let*! r = handle stopper element in
          let*! () =
            match r with
            | Ok () -> Lwt.return_unit
            | Error trace ->
                let*! () = Event.(emit daemon_error) trace in
                Lwt.return_unit
          in
          go ()
    in
    return (go (), stopper)

  let resolve_plugin_and_set_ready config ctxt cctxt =
    (* Monitor heads and try resolve the DAL protocol plugin corresponding to
       the protocol of the targeted node. *)
    (* FIXME: https://gitlab.com/tezos/tezos/-/issues/3605
       Handle situtation where plugin is not found *)
    let open Lwt_result_syntax in
    let handler stopper
        (_block_hash, (_block_header : Tezos_base.Block_header.t)) =
      let* protocols =
        Tezos_shell_services.Chain_services.Blocks.protocols cctxt ()
      in
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/4627
         Register only one plugin according to mode of operation. *)
      let*! plugin_opt = resolve_plugin protocols in
      match plugin_opt with
      | Some plugin ->
          let (module Dal_plugin : Dal_plugin.T) = plugin in
          let* proto_parameters =
            Dal_plugin.get_constants cctxt#chain cctxt#block cctxt
          in
          let* cryptobox =
            init_cryptobox config.Configuration.use_unsafe_srs proto_parameters
          in
          Node_context.set_ready ctxt plugin cryptobox proto_parameters ;
          let*! () = Event.(emit node_is_ready ()) in
          stopper () ;
          return_unit
      | _ -> return_unit
    in
    let handler stopper el =
      match Node_context.get_status ctxt with
      | Starting -> handler stopper el
      | Ready _ -> return_unit
    in
    let*! () = Event.(emit layer1_node_tracking_started ()) in
    make_stream_daemon
      handler
      (Tezos_shell_services.Monitor_services.heads cctxt `Main)

  let new_head ctxt cctxt =
    (* Monitor heads and store published slot headers indexed by block hash. *)
    let open Lwt_result_syntax in
    let handler _stopper (block_hash, (header : Tezos_base.Block_header.t)) =
      match Node_context.get_status ctxt with
      | Starting -> return_unit
      | Ready {plugin = (module Dal_plugin); proto_parameters; _} ->
          let block_level = header.shell.level in
          let* block_info =
            Dal_plugin.block_info
              cctxt
              ~block:(`Hash (block_hash, 0))
              ~metadata:`Always
          in
          let* slot_headers =
            Dal_plugin.get_published_slot_headers block_info
          in
          let*! () =
            Slot_manager.store_slot_headers
              ~block_level
              ~block_hash
              slot_headers
              (Node_context.get_store ctxt)
          in
          let*? attested_slots =
            Dal_plugin.attested_slot_headers
              block_hash
              block_info
              ~number_of_slots:proto_parameters.number_of_slots
          in
          let*! () =
            Slot_manager.update_selected_slot_headers_statuses
              ~block_level
              ~attestation_lag:proto_parameters.attestation_lag
              ~number_of_slots:proto_parameters.number_of_slots
              attested_slots
              (Node_context.get_store ctxt)
          in
          let*! () =
            Event.(emit layer1_node_new_head (block_hash, block_level))
          in
          return_unit
    in
    let*! () = Event.(emit layer1_node_tracking_started ()) in
    (* FIXME: https://gitlab.com/tezos/tezos/-/issues/3517
        If the layer1 node reboots, the rpc stream breaks.*)
    make_stream_daemon
      handler
      (Tezos_shell_services.Monitor_services.heads cctxt `Main)

  let new_slot_header ctxt =
    (* Monitor neighbor DAL nodes and download published slots as shards. *)
    let open Lwt_result_syntax in
    let handler n_cctxt Node_context.{cryptobox; _} slot_header =
      let dal_parameters = Cryptobox.parameters cryptobox in
      let downloaded_shard_ids =
        0
        -- ((dal_parameters.number_of_shards / dal_parameters.redundancy_factor)
           - 1)
      in
      let* shards =
        RPC_server_legacy.shards_rpc n_cctxt slot_header downloaded_shard_ids
      in
      let shards = List.to_seq shards in
      let* () =
        Slot_manager.save_shards
          (Node_context.get_store ctxt)
          cryptobox
          slot_header
          shards
      in
      return_unit
    in
    let handler n_cctxt _stopper slot_header =
      match Node_context.get_status ctxt with
      | Starting -> return_unit
      | Ready ready_ctxt -> handler n_cctxt ready_ctxt slot_header
    in
    List.map
      (fun n_cctxt ->
        make_stream_daemon
          (handler n_cctxt)
          (RPC_server.monitor_shards_rpc n_cctxt))
      (Node_context.get_neighbors_cctxts ctxt)
end

let daemonize handlers =
  (* FIXME: https://gitlab.com/tezos/tezos/-/issues/3605
     Improve concurrent tasks by using workers *)
  let open Lwt_result_syntax in
  let* handlers = List.map_es (fun x -> x) handlers in
  let (_ : Lwt_exit.clean_up_callback_id) =
    (* close the stream when an exit signal is received *)
    Lwt_exit.register_clean_up_callback ~loc:__LOC__ (fun _exit_status ->
        List.iter (fun (_, stopper) -> stopper ()) handlers ;
        Lwt.return_unit)
  in
  (let* _ = all (List.map fst handlers) in
   return_unit)
  |> lwt_map_error (List.fold_left (fun acc errs -> errs @ acc) [])

let connect_gossipsub_with_p2p gs_worker transport_layer =
  Lwt.dont_wait
    (fun () ->
      Gossipsub.Transport_layer_hooks.activate gs_worker transport_layer)
    (fun exn ->
      "[dal_node] error in Daemon.connect_gossipsub_with_p2p: "
      ^ Printexc.to_string exn
      |> Stdlib.failwith)

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/3605
   Improve general architecture, handle L1 disconnection etc
*)
let run ~data_dir cctxt =
  let open Lwt_result_syntax in
  let*! () = Event.(emit starting_node) () in
  let* ({network_name; rpc_addr; rpc_port; peers; _} as config) =
    Configuration.load ~data_dir
  in
  let config = {config with data_dir} in
  (* Create and start a GS worker *)
  let gs_worker =
    let rng =
      let seed =
        Random.self_init () ;
        Random.bits ()
      in
      Random.State.make [|seed|]
    in
    let open Worker_parameters in
    Gossipsub.Worker.(
      make ~events_logging:Logging.event rng limits peer_filter_parameters
      |> start [])
  in
  (* Create a transport (P2P) layer instance. *)
  let* transport_layer =
    let open Transport_layer_parameters in
    let* p2p_config = p2p_config config in
    Gossipsub.Transport_layer.create p2p_config p2p_limits ~network_name
  in
  let* store = Store.init gs_worker config in
  let ctxt = Node_context.init config store gs_worker transport_layer cctxt in
  let* rpc_server = RPC_server.(start config ctxt) in
  (* activate the p2p instance. *)
  Gossipsub.Transport_layer.activate ~additional_points:peers transport_layer ;
  connect_gossipsub_with_p2p gs_worker transport_layer ;

  let _ = RPC_server.install_finalizer rpc_server in
  let*! () = Event.(emit rpc_server_is_ready (rpc_addr, rpc_port)) in
  (* Start daemon to resolve current protocol plugin *)
  let* () =
    daemonize [Handler.resolve_plugin_and_set_ready config ctxt cctxt]
  in
  (* Start never-ending monitoring daemons *)
  daemonize (Handler.new_head ctxt cctxt :: Handler.new_slot_header ctxt)
