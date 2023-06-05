(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022-2023 Trili Tech <contact@trili.tech>                   *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Marigold  <contact@marigold.dev>                       *)
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

open Tezos_rpc_http
open Tezos_rpc_http_server

let add_service registerer service handler directory =
  registerer directory service handler

let register_get_health_live cctxt directory =
  directory
  |> add_service
       Tezos_rpc.Directory.register0
       RPC_services.get_health_live
       (fun () () -> RPC_handlers.handle_get_health_live cctxt)

let register_get_health_ready cctxt directory =
  directory
  |> add_service
       Tezos_rpc.Directory.register0
       RPC_services.get_health_ready
       (fun () () -> RPC_handlers.handle_get_health_ready cctxt)

module V0 = struct
  let register_post_store_preimage ctx cctxt dac_sk_uris page_store
      hash_streamer directory =
    directory
    |> add_service
         Tezos_rpc.Directory.register0
         RPC_services.V0.post_store_preimage
         (fun () input ->
           RPC_handlers.V0.handle_post_store_preimage
             ctx
             cctxt
             dac_sk_uris
             page_store
             hash_streamer
             input)

  let register_get_verify_signature dac_plugin public_keys_opt directory =
    directory
    |> add_service
         Tezos_rpc.Directory.register0
         RPC_services.V0.get_verify_signature
         (fun external_message () ->
           RPC_handlers.V0.handle_get_verify_signature
             dac_plugin
             public_keys_opt
             external_message)

  let register_get_preimage dac_plugin page_store =
    add_service
      Tezos_rpc.Directory.register1
      RPC_services.V0.get_preimage
      (fun hash () () ->
        RPC_handlers.Shared_by_V0_and_V1.handle_get_page
          dac_plugin
          page_store
          hash)

  let register_monitor_root_hashes hash_streamer dir =
    Tezos_rpc.Directory.gen_register
      dir
      Monitor_services.V0.S.root_hashes
      (fun () () () -> RPC_handlers.V0.handle_monitor_root_hashes hash_streamer)

  let register_get_certificate node_store =
    add_service
      Tezos_rpc.Directory.register1
      RPC_services.V0.get_certificate
      (fun root_hash () () ->
        RPC_handlers.V0.handle_get_certificate node_store root_hash)

  let register_get_serialized_certificate node_store dac_plugin =
    add_service
      Tezos_rpc.Directory.register1
      RPC_services.V0.get_serialized_certificate
      (fun root_hash () () ->
        RPC_handlers.V0.handle_get_serialized_certificate
          dac_plugin
          node_store
          root_hash)

  module Coordinator = struct
    let register_monitor_certificate dac_plugin ro_node_store
        certificate_streamers committee_members dir =
      Tezos_rpc.Directory.gen_register
        dir
        Monitor_services.V0.S.certificate
        (fun ((), root_hash) () () ->
          let open Lwt_result_syntax in
          let*! handler =
            RPC_handlers.V0.Coordinator.handle_monitor_certificate
              dac_plugin
              ro_node_store
              certificate_streamers
              root_hash
              committee_members
          in
          match handler with
          | Ok (next, shutdown) ->
              Tezos_rpc.Answer.return_stream {next; shutdown}
          | Error e -> Tezos_rpc.Answer.fail e)

    let register_post_preimage dac_plugin hash_streamer page_store =
      add_service
        Tezos_rpc.Directory.register0
        RPC_services.V0.Coordinator.post_preimage
        (fun () payload ->
          RPC_handlers.V0.Coordinator.handle_post_preimage
            dac_plugin
            page_store
            hash_streamer
            payload)

    let register_put_dac_member_signature ctx dac_plugin rw_node_store
        page_store =
      add_service
        Tezos_rpc.Directory.register0
        RPC_services.V0.put_dac_member_signature
        (fun () dac_member_signature ->
          Signature_manager.Coordinator.handle_put_dac_member_signature
            ctx
            dac_plugin
            rw_node_store
            page_store
            dac_member_signature)

    let dynamic_rpc_dir dac_plugin rw_store page_store coordinator_node_ctxt =
      let hash_streamer =
        coordinator_node_ctxt.Node_context.Coordinator.hash_streamer
      in
      let certificate_streamers = coordinator_node_ctxt.certificate_streamers in
      let committee_members = coordinator_node_ctxt.committee_members in
      Tezos_rpc.Directory.empty
      |> register_post_preimage dac_plugin hash_streamer page_store
      |> register_get_preimage dac_plugin page_store
      |> register_monitor_root_hashes hash_streamer
      |> register_monitor_certificate
           dac_plugin
           rw_store
           certificate_streamers
           committee_members
      |> register_put_dac_member_signature
           coordinator_node_ctxt
           dac_plugin
           rw_store
           page_store
      |> register_get_serialized_certificate rw_store dac_plugin
      |> register_get_certificate rw_store
  end

  module Committee_member = struct
    let dynamic_rpc_dir dac_plugin page_store =
      Tezos_rpc.Directory.empty |> register_get_preimage dac_plugin page_store
  end

  module Observer = struct
    let register_get_missing_page dac_plugin page_store cctxts timeout =
      add_service
        Tezos_rpc.Directory.register1
        RPC_services.V0.get_missing_page
        (fun root_hash () () ->
          RPC_handlers.V0.Observer.handle_get_missing_page
            timeout
            cctxts
            page_store
            dac_plugin
            root_hash)

    let dynamic_rpc_dir dac_plugin committee_member_cctxts timeout page_store =
      Tezos_rpc.Directory.empty
      |> register_get_preimage dac_plugin page_store
      |> register_get_missing_page
           dac_plugin
           page_store
           committee_member_cctxts
           timeout
  end

  module Legacy = struct
    let register_put_dac_member_signature ctx dac_plugin rw_node_store
        page_store =
      add_service
        Tezos_rpc.Directory.register0
        RPC_services.V0.put_dac_member_signature
        (fun () dac_member_signature ->
          Signature_manager.Legacy.handle_put_dac_member_signature
            ctx
            dac_plugin
            rw_node_store
            page_store
            dac_member_signature)

    let dynamic_rpc_dir dac_plugin rw_store page_store cctxt legacy_node_ctxt =
      let hash_streamer = legacy_node_ctxt.Node_context.Legacy.hash_streamer in
      let public_keys_opt =
        Node_context.Legacy.public_keys_opt legacy_node_ctxt
      in
      let secret_key_uris_opt =
        Node_context.Legacy.secret_key_uris_opt legacy_node_ctxt
      in
      Tezos_rpc.Directory.empty
      |> register_post_store_preimage
           dac_plugin
           cctxt
           secret_key_uris_opt
           page_store
           hash_streamer
      |> register_get_verify_signature dac_plugin public_keys_opt
      |> register_get_preimage dac_plugin page_store
      |> register_monitor_root_hashes hash_streamer
      |> register_put_dac_member_signature
           legacy_node_ctxt
           dac_plugin
           rw_store
           page_store
      |> register_get_serialized_certificate rw_store dac_plugin
      |> register_get_certificate rw_store
  end
end

module V1 = struct
  let register_get_pages dac_plugin page_store =
    add_service
      Tezos_rpc.Directory.register1
      RPC_services.V1.get_pages
      (fun hash () () ->
        RPC_handlers.Shared_by_V0_and_V1.handle_get_page
          dac_plugin
          page_store
          hash)

  module Coordinator = struct
    let dynamic_rpc_dir dac_plugin page_store =
      Tezos_rpc.Directory.empty |> register_get_pages dac_plugin page_store
  end

  module Committee_member = struct
    let dynamic_rpc_dir dac_plugin page_store =
      Tezos_rpc.Directory.empty |> register_get_pages dac_plugin page_store
  end

  module Observer = struct
    let dynamic_rpc_dir dac_plugin page_store =
      Tezos_rpc.Directory.empty |> register_get_pages dac_plugin page_store
  end
end

let start ~rpc_address ~rpc_port ~allow_v1_api node_ctxt =
  let open Lwt_syntax in
  let rw_store = Node_context.get_node_store node_ctxt Store_sigs.Read_write in
  let page_store = Node_context.get_page_store node_ctxt in
  let cctxt = Node_context.get_tezos_node_cctxt node_ctxt in
  let register_v0_dynamic_rpc dac_plugin =
    match Node_context.get_mode node_ctxt with
    | Coordinator coordinator_node_ctxt ->
        V0.Coordinator.dynamic_rpc_dir
          dac_plugin
          rw_store
          page_store
          coordinator_node_ctxt
    | Committee_member _committee_member_node_ctxt ->
        V0.Committee_member.dynamic_rpc_dir dac_plugin page_store
    | Observer {committee_cctxts; timeout; _} ->
        V0.Observer.dynamic_rpc_dir
          dac_plugin
          committee_cctxts
          (Float.of_int timeout)
          page_store
    | Legacy legacy_node_ctxt ->
        V0.Legacy.dynamic_rpc_dir
          dac_plugin
          rw_store
          page_store
          cctxt
          legacy_node_ctxt
  in
  let register_v1_dynamic_rpc dac_plugin =
    match Node_context.get_mode node_ctxt with
    | Coordinator _coordinator_node_ctxt ->
        V1.Coordinator.dynamic_rpc_dir dac_plugin page_store
    | Committee_member _committee_member_node_ctxt ->
        V1.Committee_member.dynamic_rpc_dir dac_plugin page_store
    | Observer _observer_ctxt ->
        V1.Observer.dynamic_rpc_dir dac_plugin page_store
    | Legacy _legacy_node_ctxt -> Tezos_rpc.Directory.empty
  in
  let register_health_endpoints dir =
    dir
    |> register_get_health_ready node_ctxt
    |> register_get_health_live node_ctxt
  in
  let dir =
    Tezos_rpc.Directory.register_dynamic_directory
      Tezos_rpc.Directory.empty
      Tezos_rpc.Path.open_root
      (fun () ->
        match Node_context.get_status node_ctxt with
        | Ready {dac_plugin = (module Dac_plugin)} ->
            Lwt.return
              (Tezos_rpc.Directory.merge
                 (register_v0_dynamic_rpc (module Dac_plugin))
                 (if allow_v1_api then
                  register_v1_dynamic_rpc (module Dac_plugin)
                 else Tezos_rpc.Directory.empty)
              |> register_health_endpoints)
        | Starting ->
            Lwt.return (Tezos_rpc.Directory.empty |> register_health_endpoints))
  in
  let rpc_address = P2p_addr.of_string_exn rpc_address in
  let host = Ipaddr.V6.to_string rpc_address in
  let node = `TCP (`Port rpc_port) in
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/5613
     Check if proper ACL handling is needed. *)
  let acl = RPC_server.Acl.allow_all in
  let server =
    RPC_server.init_server dir ~acl ~media_types:Media_type.all_media_types
  in
  Lwt.catch
    (fun () ->
      let* () =
        RPC_server.launch
          ~host
          server
          ~callback:(RPC_server.resto_callback server)
          node
      in
      let* () = Event.emit_rpc_started () in
      return_ok server)
    fail_with_exn

let shutdown = RPC_server.shutdown

let install_finalizer rpc_server =
  let open Lwt_syntax in
  Lwt_exit.register_clean_up_callback ~loc:__LOC__ @@ fun exit_status ->
  let* () = shutdown rpc_server in
  let* () = Event.(emit shutdown_node exit_status) in
  Tezos_base_unix.Internal_event_unix.close ()
