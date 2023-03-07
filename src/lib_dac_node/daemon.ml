(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
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

type error += Mode_not_supported of string | Cannot_retrieve_node_keys of string

let () =
  register_error_kind
    `Permanent
    ~id:"dac.node.operating_mode_not_supported"
    ~title:"Operating mode not supported"
    ~description:"Operating mode not supported"
    ~pp:(fun ppf mode_string ->
      Format.fprintf ppf "DAC node cannot run in %s operating mode" mode_string)
    Data_encoding.(obj1 (req "mode" string))
    (function Mode_not_supported mode -> Some mode | _ -> None)
    (fun mode -> Mode_not_supported mode)

let () =
  register_error_kind
    `Permanent
    ~id:"dac.node.cannot_retrieve_node_keys"
    ~title:"Cannot retrieve node keys"
    ~description:"Cannot retrieve node keys"
    ~pp:(fun ppf public_key_hash ->
      Format.fprintf ppf "Cannot retrieve node keys from %s" public_key_hash)
    Data_encoding.(obj1 (req "public_key_hash" string))
    (function
      | Cannot_retrieve_node_keys public_key_hash -> Some public_key_hash
      | _ -> None)
    (fun public_key_hash -> Cannot_retrieve_node_keys public_key_hash)

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

  let resolve_plugin_and_set_ready ctxt cctxt =
    (* Monitor heads and try resolve the DAC protocol plugin corresponding to
       the protocol of the targeted node. *)
    (* FIXME: https://gitlab.com/tezos/tezos/-/issues/3605
       Handle situtation where plugin is not found *)
    let open Lwt_result_syntax in
    let handler stopper
        (_block_hash, (_block_header : Tezos_base.Block_header.t)) =
      let* protocols =
        Tezos_shell_services.Chain_services.Blocks.protocols cctxt ()
      in
      let*! dac_plugin = Dac_manager.resolve_plugin protocols in
      match dac_plugin with
      | Some dac_plugin ->
          Node_context.set_ready ctxt dac_plugin ;
          let*! () = Event.(emit node_is_ready ()) in
          stopper () ;
          return_unit
      | None -> return_unit
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
      | Ready _ ->
          let block_level = header.shell.level in
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

  (** This function will be called by [new_root_hash] 
      only when payload corresponding to provided [root_hash] 
      had been well dezerialized.
      Once it is done, call PUT /dac_member_signature to submit member signature *)
  let push_payload_signature dac_plugin ctxt coordinator_cctxt public_key_hash
      root_hash =
    let open Lwt_result_syntax in
    let wallet = Node_context.get_tezos_node_cctxt ctxt in
    let* keys_opt = Dac_manager.Keys.get_wallet_info wallet public_key_hash in
    match keys_opt with
    | Some keys ->
        let bytes_to_sign = Dac_plugin.hash_to_bytes root_hash in
        let* signature =
          Tezos_client_base.Client_keys.aggregate_sign
            wallet
            (Dac_manager.Keys.aggregate_sk_uris keys)
            bytes_to_sign
        in
        let signature_repr =
          Signature_repr.
            {
              root_hash;
              signature;
              signer_pkh = Dac_manager.Keys.public_key_hash keys;
            }
        in
        let* () =
          Dac_node_client.call
            coordinator_cctxt
            (RPC_services.store_dac_member_signature dac_plugin)
            ()
            ()
            signature_repr
        in
        let*! () = Event.emit_signature_pushed_to_coordinator signature in
        return_unit
    | None ->
        tzfail
        @@ Cannot_retrieve_node_keys
             (Tezos_crypto.Aggregate_signature.Public_key_hash.to_string
                public_key_hash)

  (** This handler will be invoked only when a [coordinator_cctxt] is specified
      in the DAC node configuration. The DAC node tries to subscribes to the
      stream of root hashes via the streamed GET /monitor/root_hashes RPC call
      to the dac node corresponding to [coordinator_cctxt]. *)
  let new_root_hash ctxt coordinator_cctxt committee_member_address_opt =
    let open Lwt_result_syntax in
    let handler dac_plugin remote_store _stopper root_hash =
      let*! () = Event.emit_new_root_hash_received dac_plugin root_hash in
      let*! payload_result =
        Pages_encoding.Merkle_tree.V0.Remote.deserialize_payload
          dac_plugin
          ~page_store:remote_store
          root_hash
      in
      match payload_result with
      | Ok _ -> (
          let*! () =
            Event.emit_received_root_hash_processed dac_plugin root_hash
          in
          (* If there is a [public_key_hash], it means the node is run as a member,
              so we must sign the payload and post the signature to the coordinator
             If there is no [public_key_hash] provided, it means the node is run as an observer
               then we simply [return ()] *)
          match committee_member_address_opt with
          | Some committee_member_address ->
              push_payload_signature
                dac_plugin
                ctxt
                coordinator_cctxt
                committee_member_address
                root_hash
          | None ->
              let*! () = Event.emit_no_committee_member_address () in
              return ())
      | Error errs ->
          (* TODO: https://gitlab.com/tezos/tezos/-/issues/4930.
             Improve handling of errors. *)
          let*! () =
            Event.emit_processing_root_hash_failed dac_plugin root_hash errs
          in
          return ()
    in
    let*? dac_plugin = Node_context.get_dac_plugin ctxt in
    let remote_store =
      Page_store.(
        Remote.init
          {
            cctxt = coordinator_cctxt;
            page_store = Node_context.get_page_store ctxt;
          })
    in
    let*! () = Event.(emit subscribed_to_root_hashes_stream ()) in
    make_stream_daemon
      (handler dac_plugin remote_store)
      (Monitor_services.root_hashes coordinator_cctxt dac_plugin)
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

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/3605
   Improve general architecture, handle L1 disconnection etc
*)
let run ~data_dir cctxt =
  let open Lwt_result_syntax in
  let*! () = Event.(emit starting_node) () in
  let* (Configuration.
          {rpc_address; rpc_port; reveal_data_dir; mode; data_dir = _} as
       config) =
    Configuration.load ~data_dir
  in
  let* () = Dac_manager.Storage.ensure_reveal_data_dir_exists reveal_data_dir in
  let* addresses, threshold, coordinator_cctxt_opt, committee_member_address_opt
      =
    match mode with
    | Legacy configuration ->
        let committee_members_addresses =
          Configuration.Legacy.committee_members_addresses configuration
        in
        let threshold = Configuration.Legacy.threshold configuration in
        let dac_cctxt_config =
          Configuration.Legacy.dac_cctxt_config configuration
        in
        let committee_member_address_opt =
          Configuration.Legacy.committee_member_address_opt configuration
        in
        return (committee_members_addresses, threshold, dac_cctxt_config, committee_member_address_opt)
    | Coordinator _ -> tzfail @@ Mode_not_supported "coordinator"
    | Committee_member _ -> tzfail @@ Mode_not_supported "committee_member"
    | Observer _ -> tzfail @@ Mode_not_supported "observer"
  in
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/4725
     Stop DAC node when in Legacy mode, if threshold is not reached. *)
  let* dac_accounts = Dac_manager.Keys.get_keys ~addresses ~threshold cctxt in
  let dac_pks_opt, dac_sk_uris =
    dac_accounts
    |> List.map (fun account_opt ->
           match account_opt with
           | Some account ->
               ( Dac_manager.Keys.public_key_opt account,
                 Some (Dac_manager.Keys.aggregate_sk_uris account) )
           | None -> (None, None))
    |> List.split
  in
  let coordinator_cctxt_opt =
    Option.map
      (fun Configuration.{host; port} ->
        Dac_node_client.make_unix_cctxt ~scheme:"http" ~host ~port)
      coordinator_cctxt_opt
  in
  let* ctxt = Node_context.init config cctxt coordinator_cctxt_opt in
  let* rpc_server =
    RPC_server.(
      start_legacy
        ~rpc_address
        ~rpc_port
        ~threshold
        cctxt
        ctxt
        dac_pks_opt
        dac_sk_uris)
  in
  let _ = RPC_server.install_finalizer rpc_server in
  let*! () = Event.(emit rpc_server_is_ready (rpc_address, rpc_port)) in
  (* Start daemon to resolve current protocol plugin *)
  let* () = daemonize [Handler.resolve_plugin_and_set_ready ctxt cctxt] in
  (* Start never-ending monitoring daemons. [coordinator_cctxt] is required to
     monitor new root hashes in legacy mode. *)
  match coordinator_cctxt_opt with
  | None -> daemonize [Handler.new_head ctxt cctxt]
  | Some coordinator_cctxt ->
      daemonize
        [
          Handler.new_head ctxt cctxt;
          Handler.new_root_hash
            ctxt
            coordinator_cctxt
            committee_member_address_opt;
        ]
