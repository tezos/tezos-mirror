(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Trili Tech, <contact@trili.tech>                       *)
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

type t = unit tzresult Lwt.t * Tezos_rpc__RPC_context.stopper

let resolve_plugin
    (protocols : Tezos_shell_services.Chain_services.Blocks.protocols) =
  let open Lwt_syntax in
  let current_protocol = protocols.current_protocol in
  let next_protocol = protocols.next_protocol in
  let plugin_opt =
    Option.either
      (Dac_plugin.get current_protocol)
      (Dac_plugin.get next_protocol)
  in
  match plugin_opt with
  | None ->
      let+ () =
        Event.emit_protocol_plugin_not_resolved current_protocol next_protocol
      in
      None
  | Some dac_plugin ->
      let (module Dac_plugin : Dac_plugin.T) = dac_plugin in
      let+ () = Event.emit_protocol_plugin_resolved Dac_plugin.Proto.hash in
      Some dac_plugin

(** [make_stream_daemon handler streamed_call] calls [handler] on each newly
      received value from [streamed_call].
      It returns a couple [(p, stopper)] where [p] is a promise resolving when
      the stream closes and [stopper] a function closing the stream.
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

(* TODO: https://gitlab.com/tezos/tezos/-/issues/5930
         Dac nodes operators should be able to configure
         [infinite_daemon_init_delay] from the command line. *)

(** [infinite_daemon_init_delay] represents a delay before trying to
    re-establish connection in case of streamed daemon disconnection. *)
let infinite_daemon_init_delay = 2.

(** [infinite_daemon_max_delay] represents a max delay before trying to
    re-establish connection in case of streamed daemon disconnection. *)
let infinite_daemon_max_delay = 128.

(** [make_infinite_stream_daemon ~on_disconnect ~on_failed_connection connect]
    creates an ever lasting streamed daemon, by restarting a daemon,
    every time connection is lost or connection fails to be established.

    In case of a lost connection, we first wait [infinite_daemon_init_delay]
    until trying to run the streamed daemon again. If connection is not
    established we duplicate the waiting time. The waiting time is bounded by
    [infinite_daemon_max_delay].

    - [connect] is a streamed daemon constructor.
    - [~on_disconnect] is used to emit event when the daemon disconnects.
    - [~on_failed_connection] is used to emit event when unable to re-establish
      connection. 

      TODO: https://gitlab.com/tezos/tezos/-/issues/5931
            We would want an upper bound in [max_retries] for this function.
            Both [max_retries] and [infinite_daemon_max_delay] would ideally
            be configurable. *)
let make_infinite_stream_daemon ~on_disconnect ~on_failed_connection connect =
  let rec loop ~delay ~count =
    let open Lwt_result_syntax in
    let*! daemon = connect () in
    match daemon with
    | Ok (daemon, stopper) ->
        (* [daemon] promise is resolved when underlying stream closes. E.g.
           this happens when rebooting Coordinator's node. *)
        let* () = daemon in
        let () = stopper () in
        let*! () = on_disconnect () in
        (* Before reconnecting we wait. *)
        let*! () = Lwt_unix.sleep delay in
        loop ~count:0 ~delay:infinite_daemon_init_delay
    | Error e ->
        let*! () = on_failed_connection ~count ~delay e in
        (* Before trying again we wait. *)
        let*! () = Lwt_unix.sleep delay in
        (* We duplicate the previous waiting time which is bounded by
           [infinite_daemon_max_delay]. *)
        let delay = Float.min (delay *. 2.0) infinite_daemon_max_delay in
        loop ~count:(count + 1) ~delay
  in
  loop ~count:0 ~delay:infinite_daemon_init_delay

let resolve_plugin_and_set_ready ctxt =
  (* Monitor heads and try resolve the DAC protocol plugin corresponding to
     the protocol of the targeted node. *)
  (* FIXME: https://gitlab.com/tezos/tezos/-/issues/3605
     Handle situtation where plugin is not found *)
  let open Lwt_result_syntax in
  let cctxt = Node_context.get_tezos_node_cctxt ctxt in
  let handler stopper (_block_hash, (_block_header : Tezos_base.Block_header.t))
      =
    let* protocols =
      Tezos_shell_services.Chain_services.Blocks.protocols cctxt ()
    in
    let*! dac_plugin = resolve_plugin protocols in
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

(** The [new_head] handler is shared by all operating modes.  This handler is
    responsible for tracking new heads from the Layer 1. *)
let new_head ctxt =
  let cctxt = Node_context.get_tezos_node_cctxt ctxt in
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

(** Handlers specific to a [Committee_member]. A [Committee_member] is
    responsible for
    {ul
      {li Monitoring root hashes from a [Coordinator],}
      {li Downloading the associated pages,}
      {li Validating the hash of each page,}
      {li Sign the final root hash with the public key of the
      committee member,}
      {li Send the signature back to the [Coordinaotor].}
    } *)
module Committee_member = struct
  let push_payload_signature coordinator_cctxt wallet_cctxt committee_member
      root_hash =
    let open Lwt_result_syntax in
    let signer_pkh =
      committee_member.Wallet_account.Committee_member.public_key_hash
    in
    let secret_key_uri = committee_member.secret_key_uri in
    let bytes_to_sign = Dac_plugin.hash_to_bytes root_hash in
    let* signature =
      Tezos_client_base.Client_keys.aggregate_sign
        wallet_cctxt
        secret_key_uri
        bytes_to_sign
    in
    let signature_repr =
      Signature_repr.make
        (Dac_plugin.hash_to_raw root_hash)
        signature
        signer_pkh
    in
    let* () =
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/5627
         Currently we have only one major DAC API version ([V0]). For this
         reason, we can always default to it. This should be revisited once we
         add another major version. *)
      Dac_node_client.V0.put_dac_member_signature
        coordinator_cctxt
        ~signature:signature_repr
    in
    let*! () = Event.emit_signature_pushed_to_coordinator signature in
    return_unit

  let new_root_hash ctxt wallet_cctxt dac_plugin page_store =
    let open Lwt_result_syntax in
    let coordinator_cctxt =
      ctxt.Node_context.Committee_member.coordinator_cctxt
    in
    let handler dac_plugin remote_store _stopper root_hash =
      let*? root_hash = Dac_plugin.raw_to_hash dac_plugin root_hash in
      let*! () = Event.emit_new_root_hash_received dac_plugin root_hash in
      let*! payload_result =
        Pages_encoding.Merkle_tree.V0.Remote.deserialize_payload
          dac_plugin
          ~page_store:remote_store
          root_hash
      in
      match payload_result with
      | Ok _ ->
          let*! () =
            Event.emit_received_root_hash_processed dac_plugin root_hash
          in
          let committee_member =
            ctxt.Node_context.Committee_member.committee_member
          in
          push_payload_signature
            coordinator_cctxt
            wallet_cctxt
            committee_member
            root_hash
      | Error errs ->
          (* TODO: https://gitlab.com/tezos/tezos/-/issues/4930.
              Improve handling of errors. *)
          let*! () =
            Event.emit_processing_root_hash_failed dac_plugin root_hash errs
          in
          return ()
    in
    let remote_store =
      Page_store.(Remote.init {cctxt = coordinator_cctxt; page_store})
    in
    let*! () = Event.(emit subscribed_to_root_hashes_stream ()) in
    make_infinite_stream_daemon
      ~on_disconnect:Event.emit_coordinators_connection_lost
      ~on_failed_connection:Event.emit_cannot_connect_to_coordinator
      (fun () ->
        make_stream_daemon
          (handler dac_plugin remote_store)
          (Monitor_services.V0.root_hashes coordinator_cctxt))
end

(** Handlers specific to an [Observer]. An [Observer] is responsible for
    {ul
      {li Monitoring root hashes from a [Coordinator],}
      {li Downloading the associated pages,}
      {li Validating the hash of each page.}
    } *)

module Observer = struct
  let new_root_hash ctxt dac_plugin page_store =
    let open Lwt_result_syntax in
    let coordinator_cctxt = ctxt.Node_context.Observer.coordinator_cctxt in
    let handler dac_plugin remote_store _stopper root_hash =
      let*? root_hash = Dac_plugin.raw_to_hash dac_plugin root_hash in
      let*! () = Event.emit_new_root_hash_received dac_plugin root_hash in
      let*! payload_result =
        Pages_encoding.Merkle_tree.V0.Remote.deserialize_payload
          dac_plugin
          ~page_store:remote_store
          root_hash
      in
      match payload_result with
      | Ok _ ->
          let*! () =
            Event.emit_received_root_hash_processed dac_plugin root_hash
          in
          return ()
      | Error errs ->
          (* TODO: https://gitlab.com/tezos/tezos/-/issues/4930.
             Improve handling of errors. *)
          let*! () =
            Event.emit_processing_root_hash_failed dac_plugin root_hash errs
          in
          return ()
    in
    let remote_store =
      Page_store.(Remote.init {cctxt = coordinator_cctxt; page_store})
    in
    let*! () = Event.(emit subscribed_to_root_hashes_stream ()) in
    make_infinite_stream_daemon
      ~on_disconnect:Event.emit_coordinators_connection_lost
      ~on_failed_connection:Event.emit_cannot_connect_to_coordinator
      (fun () ->
        make_stream_daemon
          (handler dac_plugin remote_store)
          (Monitor_services.V0.root_hashes coordinator_cctxt))
end

(** Handlers specific to a [Legacy] DAC node. If no
    [Dac_client.coordinator_cctxt] is specified for the [Legacy] DAC node,
    then its only handler will be the one responsible for tracking Layer 1
    heads. Otherwise, the [Legacy] DAC node will also be equipped with a
    handler for monitoring root hashes from the specified
    [Dac_client.coordinator_cctxt]. The responsibilities of this handler
    will be
    {ul
      {li Monitoring root hashes from the [Dac_client.coordinator_cctxt],}
      {li Downloading the associated pages,}
      {li Validating the hash of each page,}
      {li Furthermore, if a [committee_member] is specified
          in the [Legacy] node configuration, then this handler will also }
          {ul
            {li Sign the final root hash with the public key of the
                committee member,}
            {li Send the signature back to the [Coordinaotor] via the
                [Dac_client.coordinator_cctxt].}
          }
    } *)

module Legacy = struct
  let push_payload_signature coordinator_cctxt wallet_cctxt committee_member
      root_hash =
    let open Lwt_result_syntax in
    let secret_key_uri_opt =
      committee_member.Wallet_account.Legacy.secret_key_uri_opt
    in
    let signer_pkh = committee_member.public_key_hash in
    match secret_key_uri_opt with
    | Some secret_key_uri ->
        let bytes_to_sign = Dac_plugin.hash_to_bytes root_hash in
        let* signature =
          Tezos_client_base.Client_keys.aggregate_sign
            wallet_cctxt
            secret_key_uri
            bytes_to_sign
        in
        let signature_repr =
          Signature_repr.make
            (Dac_plugin.hash_to_raw root_hash)
            signature
            signer_pkh
        in
        let* () =
          (* TODO: https://gitlab.com/tezos/tezos/-/issues/5627
             Currently we have only one major DAC API version ([V0]). For this
             reason, we can always default to it. This should be revisited once
             we add another major version. *)
          Dac_node_client.V0.put_dac_member_signature
            coordinator_cctxt
            ~signature:signature_repr
        in
        let*! () = Event.emit_signature_pushed_to_coordinator signature in
        return_unit
    | None ->
        let*! () = Event.emit_cannot_retrieve_keys_from_address signer_pkh in
        return ()

  (** This handler will be invoked only when a [coordinator_cctxt] is specified
      in the DAC node configuration. The DAC node tries to subscribes to the
      stream of root hashes via the streamed GET v0/monitor/root_hashes RPC call
      to the dac node corresponding to [coordinator_cctxt]. *)
  let new_root_hash ctxt coordinator_cctxt wallet_cctxt dac_plugin page_store =
    let committee_member_opt = ctxt.Node_context.Legacy.committee_member_opt in
    let open Lwt_result_syntax in
    let handler dac_plugin remote_store _stopper root_hash =
      let*? root_hash = Dac_plugin.raw_to_hash dac_plugin root_hash in
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
          match committee_member_opt with
          | Some committee_member ->
              (* If there is a [committee_member_address], it means the node is run as a member,
                   so we must sign the payload and post the signature to the coordinator
                 If there is no [committee_member_address] provided, it means the node is run as an observer
                   then we simply [return ()] *)
              push_payload_signature
                coordinator_cctxt
                wallet_cctxt
                committee_member
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
    let remote_store =
      Page_store.(Remote.init {cctxt = coordinator_cctxt; page_store})
    in
    let*! () = Event.(emit subscribed_to_root_hashes_stream ()) in
    make_stream_daemon
      (handler dac_plugin remote_store)
      (Monitor_services.V0.root_hashes coordinator_cctxt)
end

let handlers node_ctxt =
  let open Lwt_result_syntax in
  let*? plugin = Node_context.get_dac_plugin node_ctxt in
  let page_store = Node_context.get_page_store node_ctxt in
  let wallet_cctxt = Node_context.get_tezos_node_cctxt node_ctxt in
  match Node_context.get_mode node_ctxt with
  | Coordinator _ -> return [new_head node_ctxt]
  | Committee_member ctxt ->
      return
        [
          new_head node_ctxt;
          Committee_member.new_root_hash ctxt wallet_cctxt plugin page_store;
        ]
  | Observer ctxt ->
      return [new_head node_ctxt; Observer.new_root_hash ctxt plugin page_store]
  | Legacy ctxt ->
      let coordinator_cctxt_opt = ctxt.Node_context.Legacy.coordinator_cctxt in
      let root_hash_handler =
        coordinator_cctxt_opt
        |> Option.map (fun coordinator_cctxt ->
               Legacy.new_root_hash
                 ctxt
                 coordinator_cctxt
                 wallet_cctxt
                 plugin
                 page_store)
        |> Option.to_list
      in
      return @@ [new_head node_ctxt] @ root_hash_handler
