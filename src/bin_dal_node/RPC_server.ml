(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

open Tezos_rpc_http
open Tezos_rpc_http_server

module Slots_handlers = struct
  let call_handler handler ctxt =
    let open Lwt_result_syntax in
    let*? {cryptobox; _} = Node_context.get_ready ctxt in
    let store = Node_context.get_store ctxt in
    handler store cryptobox

  let post_commitment ctxt () slot =
    call_handler (fun store -> Slot_manager.add_commitment store slot) ctxt

  let patch_commitment ctxt commitment () slot_id =
    call_handler
      (fun store cryptobox ->
        let open Lwt_result_syntax in
        let*! r =
          Slot_manager.associate_slot_id_with_commitment
            store
            cryptobox
            commitment
            slot_id
        in
        match r with Ok () -> return_some () | Error `Not_found -> return_none)
      ctxt

  let get_commitment_slot ctxt commitment () () =
    call_handler
      (fun store cryptobox ->
        let open Lwt_result_syntax in
        let*! r = Slot_manager.get_commitment_slot store cryptobox commitment in
        match r with Ok s -> return_some s | Error `Not_found -> return_none)
      ctxt

  let get_commitment_proof ctxt commitment () () =
    call_handler
      (fun store cryptobox ->
        let open Lwt_result_syntax in
        (* This handler may be costly: We need to recompute the
           polynomial and then compute the proof. *)
        let*! slot =
          Slot_manager.get_commitment_slot store cryptobox commitment
        in
        match slot with
        | Error `Not_found -> return_none
        | Ok slot -> (
            match Cryptobox.polynomial_from_slot cryptobox slot with
            | Error _ ->
                (* Storage consistency ensures we can always compute the
                   polynomial from the slot. *)
                assert false
            | Ok polynomial ->
                return_some (Cryptobox.prove_commitment cryptobox polynomial)))
      ctxt

  let get_commitment_by_published_level_and_index ctxt level slot_index () () =
    call_handler
      (fun store _cryptobox ->
        let open Lwt_result_syntax in
        let*! r =
          Slot_manager.get_commitment_by_published_level_and_index
            ~level
            ~slot_index
            store
        in
        match r with
        | Ok s -> return_some s
        | Error (Ok `Not_found) -> return_none
        | Error (Error e) -> fail e)
      ctxt

  let get_commitment_headers ctxt commitment (slot_level, slot_index) () =
    call_handler
      (fun store _cryptobox ->
        Slot_manager.get_commitment_headers
          commitment
          ?slot_level
          ?slot_index
          store)
      ctxt

  let get_published_level_headers ctxt published_level header_status () =
    call_handler
      (fun store _cryptobox ->
        Slot_manager.get_published_level_headers
          ~published_level
          ?header_status
          store)
      ctxt
end

module Profile_handlers = struct
  let patch_profile ctxt () profile =
    let store = Node_context.get_store ctxt in
    Profile_manager.add_profile store profile

  let get_profiles ctxt () () =
    let store = Node_context.get_store ctxt in
    Profile_manager.get_profiles store

  let get_assigned_shard_indices ctxt pkh level () () =
    Node_context.fetch_assigned_shard_indicies ctxt ~level ~pkh

  let get_attestable_slots ctxt pkh attested_level () () =
    let open Lwt_result_syntax in
    let*? {cryptobox; proto_parameters; _} = Node_context.get_ready ctxt in
    let store = Node_context.get_store ctxt in
    Profile_manager.get_attestable_slots
      ctxt
      store
      cryptobox
      proto_parameters
      pkh
      ~attested_level
end

let add_service registerer service handler directory =
  registerer directory service handler

let register_new :
    Node_context.t -> unit Tezos_rpc.Directory.t -> unit Tezos_rpc.Directory.t =
 fun ctxt directory ->
  directory
  |> add_service
       Tezos_rpc.Directory.register0
       Services.post_commitment
       (Slots_handlers.post_commitment ctxt)
  |> add_service
       Tezos_rpc.Directory.opt_register1
       Services.patch_commitment
       (Slots_handlers.patch_commitment ctxt)
  |> add_service
       Tezos_rpc.Directory.opt_register1
       Services.get_commitment_slot
       (Slots_handlers.get_commitment_slot ctxt)
  |> add_service
       Tezos_rpc.Directory.opt_register1
       Services.get_commitment_proof
       (Slots_handlers.get_commitment_proof ctxt)
  |> add_service
       Tezos_rpc.Directory.opt_register2
       Services.get_commitment_by_published_level_and_index
       (Slots_handlers.get_commitment_by_published_level_and_index ctxt)
  |> add_service
       Tezos_rpc.Directory.register0
       Services.patch_profile
       (Profile_handlers.patch_profile ctxt)
  |> add_service
       Tezos_rpc.Directory.register0
       Services.get_profiles
       (Profile_handlers.get_profiles ctxt)
  |> add_service
       Tezos_rpc.Directory.register1
       Services.get_commitment_headers
       (Slots_handlers.get_commitment_headers ctxt)
  |> add_service
       Tezos_rpc.Directory.register2
       Services.get_assigned_shard_indices
       (Profile_handlers.get_assigned_shard_indices ctxt)
  |> add_service
       Tezos_rpc.Directory.register1
       Services.get_published_level_headers
       (Slots_handlers.get_published_level_headers ctxt)
  |> add_service
       Tezos_rpc.Directory.register2
       Services.get_attestable_slots
       (Profile_handlers.get_attestable_slots ctxt)

let register_legacy ctxt =
  let open RPC_server_legacy in
  Tezos_rpc.Directory.empty |> register_split_slot ctxt
  |> register_show_slot ctxt |> register_shard ctxt |> register_shards ctxt
  |> register_show_slot_pages ctxt
  |> register_monitor_slot_headers ctxt

let register ctxt = register_new ctxt (register_legacy ctxt)

let merge dir plugin_dir = Tezos_rpc.Directory.merge dir plugin_dir

let start configuration cctxt ctxt dac_pks_opt dac_sk_uris =
  let open Lwt_syntax in
  let Configuration.
        {rpc_addr; rpc_port; dac = {reveal_data_dir; threshold; _}; _} =
    configuration
  in
  let dir = register ctxt in
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/4558
     Rename "plugin" prefix to "dac".
  *)
  let plugin_prefix = Tezos_rpc.Path.(open_root / "plugin") in
  let dir =
    Tezos_rpc.Directory.register_dynamic_directory dir plugin_prefix (fun () ->
        match Node_context.get_status ctxt with
        | Ready {dac_plugin = (module Dac_plugin); _} ->
            Lwt.return
              (Dac_plugin.RPC.rpc_services
                 ~reveal_data_dir
                 cctxt
                 dac_pks_opt
                 dac_sk_uris
                 threshold)
        | Starting -> Lwt.return Tezos_rpc.Directory.empty)
  in
  let rpc_addr = P2p_addr.of_string_exn rpc_addr in
  let host = Ipaddr.V6.to_string rpc_addr in
  let node = `TCP (`Port rpc_port) in
  let acl = RPC_server.Acl.default rpc_addr in
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
      return_ok server)
    fail_with_exn

let shutdown = RPC_server.shutdown

let install_finalizer rpc_server =
  let open Lwt_syntax in
  Lwt_exit.register_clean_up_callback ~loc:__LOC__ @@ fun exit_status ->
  let* () = shutdown rpc_server in
  let* () = Event.(emit shutdown_node exit_status) in
  Tezos_base_unix.Internal_event_unix.close ()
