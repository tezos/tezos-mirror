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

let resolve_plugin cctxt =
  let open Lwt_result_syntax in
  let* protocols =
    Tezos_shell_services.Chain_services.Blocks.protocols cctxt ()
  in
  return
  @@ Option.either
       (Dal_plugin.get protocols.current_protocol)
       (Dal_plugin.get protocols.next_protocol)

type error += Cryptobox_initialisation_failed of string

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

let init_cryptobox unsafe_srs cctxt (module Plugin : Dal_plugin.T) =
  let open Cryptobox in
  let open Lwt_result_syntax in
  let* parameters = Plugin.get_constants cctxt#chain cctxt#block cctxt in
  let* initialisation_parameters =
    if unsafe_srs then
      return
      @@ Cryptobox.Internal_for_tests.initialisation_parameters_from_slot_size
           ~slot_size:parameters.slot_size
    else
      let*? g1_path, g2_path = Tezos_base.Dal_srs.find_trusted_setup_files () in
      Cryptobox.initialisation_parameters_from_files ~g1_path ~g2_path
  in
  let*? () = Cryptobox.load_parameters initialisation_parameters in
  match Cryptobox.make parameters with
  | Ok cryptobox -> return (cryptobox, parameters)
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
          let*! r = handle element in
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

  let new_head config ctxt cctxt =
    let open Lwt_result_syntax in
    let handler (block_hash, (_block_header : Tezos_base.Block_header.t)) =
      (* Try to resolve the protocol plugin corresponding to the protocol of the
         targeted node. *)
      match Node_context.get_status ctxt with
      | Starting -> (
          let* plugin = resolve_plugin cctxt in
          match plugin with
          | Some plugin ->
              let (module Plugin : Dal_plugin.T) = plugin in
              let*! () =
                Event.emit_protocol_plugin_resolved Plugin.Proto.hash
              in
              let* dal_constants, dal_parameters =
                init_cryptobox config.Configuration.use_unsafe_srs cctxt plugin
              in
              let dir = RPC_server.register ctxt in
              let plugin_prefix = RPC_path.(open_root / "plugin") in
              let plugin_dir =
                RPC_directory.prefix plugin_prefix Plugin.RPC.rpc_services
              in
              let dir_with_plugin = RPC_server.merge dir plugin_dir in
              let* rpc_server = RPC_server.(start config dir_with_plugin) in
              let _ = RPC_server.install_finalizer rpc_server in
              let*! () =
                Event.(
                  emit rpc_server_is_ready (config.rpc_addr, config.rpc_port))
              in
              Node_context.set_ready
                ctxt
                (module Plugin)
                dal_constants
                dal_parameters ;
              let*! () = Event.(emit node_is_ready ()) in
              return_unit
          | None -> return_unit)
      | Ready {plugin = (module Plugin); _} ->
          let* slot_headers =
            Plugin.get_published_slot_headers (`Hash (block_hash, 0)) cctxt
          in
          let*! () =
            List.iter_s
              (fun (slot_index, slot_header) ->
                Slot_headers_store.add
                  (Node_context.get_store ctxt).slot_headers_store
                  ~primary_key:block_hash
                  ~secondary_key:slot_index
                  slot_header)
              slot_headers
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
    let open Lwt_result_syntax in
    let handler n_cctxt ready_ctxt slot_header =
      let params = ready_ctxt.Node_context.dal_parameters in
      let downloaded_shard_ids =
        0 -- ((params.number_of_shards / params.redundancy_factor) - 1)
      in
      let* shards =
        List.fold_left_es
          (fun acc shard_id ->
            let* shard = RPC_server.shard_rpc n_cctxt slot_header shard_id in
            return @@ Cryptobox.IntMap.add shard.index shard.share acc)
          Cryptobox.IntMap.empty
          downloaded_shard_ids
      in
      let* () =
        Slot_manager.save_shards
          (Node_context.get_store ctxt).slots_store
          (Node_context.get_store ctxt).slots_watcher
          ready_ctxt.dal_constants
          slot_header
          shards
      in
      return_unit
    in
    let handler n_cctxt slot_header =
      match Node_context.get_status ctxt with
      | Starting -> return_unit
      | Ready ready_ctxt -> handler n_cctxt ready_ctxt slot_header
    in
    List.map
      (fun n_cctxt ->
        make_stream_daemon
          (handler n_cctxt)
          (RPC_server.monitor_slot_headers_rpc n_cctxt))
      (Node_context.get_neighbors_cctxts ctxt)
end

let daemonize handlers =
  let open Lwt_result_syntax in
  let* handlers = List.map_es (fun x -> x) handlers in
  let (_ : Lwt_exit.clean_up_callback_id) =
    (* close the stream when an exit signal is received *)
    Lwt_exit.register_clean_up_callback ~loc:__LOC__ (fun _exit_status ->
        List.iter (fun (_, stopper) -> stopper ()) handlers ;
        Lwt.return_unit)
  in
  (* The no_cancel might not be necessary. Lwt_exit cancels the
     main promise upon receiving a signal or other form of interruption. The
     no_cancel renders this cancelation into a no-op.*)
  Lwt.no_cancel
    (let* _ = all (List.map fst handlers) in
     return_unit)
  |> lwt_map_error (List.fold_left (fun acc errs -> errs @ acc) [])

let get_dac_address_keys cctxt address =
  let open Lwt_result_syntax in
  let open Tezos_client_base.Client_keys in
  let* alias = Aggregate_alias.Public_key_hash.rev_find cctxt address in
  match alias with
  | None -> return_none
  | Some alias -> (
      let* keys_opt = alias_aggregate_keys cctxt alias in
      match keys_opt with
      | None -> return_none
      | Some (pkh, pk, sk_uri_opt) -> (
          match sk_uri_opt with
          | None -> return_none
          | Some sk_uri -> return_some (pkh, pk, sk_uri)))

let get_dac_keys cctxt {Configuration.dac = {addresses; threshold; _}; _} =
  let open Lwt_result_syntax in
  let* keys = List.map_es (get_dac_address_keys cctxt) addresses in
  let recovered_keys = List.length @@ List.filter Option.is_some keys in
  if recovered_keys < threshold then
    failwith
      "Only %d out of %d required aggregate keys can be used for aggregate \
       signatures"
      recovered_keys
      threshold
  else return keys

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/3605

   Improve general architecture, handle L1 disconnection etc
*)
let run ~data_dir cctxt =
  let open Lwt_result_syntax in
  let*! () = Event.(emit starting_node) () in
  let* config = Configuration.load ~data_dir in
  let config = {config with data_dir} in
  let* _dac_list = get_dac_keys cctxt config in
  let*! store = Store.init config in
  let ctxt = Node_context.init config store in
  daemonize (Handler.new_head config ctxt cctxt :: Handler.new_slot_header ctxt)
