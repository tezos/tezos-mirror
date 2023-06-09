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

let call_handler1 ctxt handler = handler (Node_context.get_store ctxt)

let call_handler2 ctxt handler =
  let open Lwt_result_syntax in
  let*? ready_ctxt = Node_context.get_ready ctxt in
  let store = Node_context.get_store ctxt in
  handler store ready_ctxt

module Slots_handlers = struct
  let to_option_tzresult r =
    Errors.to_option_tzresult
      ~none:(function `Not_found -> true | _ -> false)
      r

  let post_commitment ctxt () slot =
    call_handler2 ctxt (fun store {cryptobox; _} ->
        Slot_manager.add_commitment store slot cryptobox |> Errors.to_tzresult)

  let patch_commitment ctxt commitment () slot_id =
    call_handler2 ctxt (fun store {cryptobox; _} ->
        Slot_manager.associate_slot_id_with_commitment
          store
          cryptobox
          commitment
          slot_id
        |> to_option_tzresult)

  let get_commitment_slot ctxt commitment () () =
    call_handler2 ctxt (fun store {cryptobox; _} ->
        Slot_manager.get_commitment_slot store cryptobox commitment
        |> to_option_tzresult)

  let get_commitment_proof ctxt commitment () () =
    call_handler2 ctxt (fun store {cryptobox; _} ->
        let open Lwt_result_syntax in
        (* This handler may be costly: We need to recompute the
           polynomial and then compute the proof. *)
        let* slot =
          Slot_manager.get_commitment_slot store cryptobox commitment
          |> to_option_tzresult
        in
        match slot with
        | None -> return_none
        | Some slot -> (
            match Cryptobox.polynomial_from_slot cryptobox slot with
            | Error _ ->
                (* Storage consistency ensures we can always compute the
                   polynomial from the slot. *)
                assert false
            | Ok polynomial -> (
                match Cryptobox.prove_commitment cryptobox polynomial with
                (* [polynomial] was produced with the parameters from
                   [cryptobox], thus we can always compute the proof from
                   [polynomial]. *)
                | Error _ -> assert false
                | Ok proof -> return_some proof)))

  let put_commitment_shards ctxt commitment () Services.Types.{with_proof} =
    call_handler2
      ctxt
      (fun store {cryptobox; shards_proofs_precomputation; _} ->
        Slot_manager.add_commitment_shards
          ~shards_proofs_precomputation
          store
          cryptobox
          commitment
          ~with_proof
        |> Errors.to_option_tzresult)

  let get_commitment_by_published_level_and_index ctxt level slot_index () () =
    call_handler1 ctxt (fun store ->
        Slot_manager.get_commitment_by_published_level_and_index
          ~level
          ~slot_index
          store
        |> to_option_tzresult)

  let get_commitment_headers ctxt commitment (slot_level, slot_index) () =
    call_handler1 ctxt (fun store ->
        Slot_manager.get_commitment_headers
          commitment
          ?slot_level
          ?slot_index
          store
        |> Errors.to_tzresult)

  let get_published_level_headers ctxt published_level header_status () =
    call_handler1 ctxt (fun store ->
        Slot_manager.get_published_level_headers
          ~published_level
          ?header_status
          store
        |> Errors.to_tzresult)

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/4338

     Re-consider this implementation/interface when the issue above is
     tackeled. *)
  let monitor_shards ctxt () () () =
    call_handler1 ctxt (fun store ->
        let stream, stopper = Store.open_shards_stream store in
        let shutdown () = Lwt_watcher.shutdown stopper in
        let next () = Lwt_stream.get stream in
        Tezos_rpc.Answer.return_stream {next; shutdown})
end

module Profile_handlers = struct
  let patch_profile ctxt () profile =
    call_handler2 ctxt (fun store {proto_parameters; _} ->
        Profile_manager.add_profile proto_parameters store profile)

  let get_profiles ctxt () () =
    call_handler1 ctxt (fun store ->
        Profile_manager.get_profiles store |> Errors.to_tzresult)

  let get_assigned_shard_indices ctxt pkh level () () =
    Node_context.fetch_assigned_shard_indices ctxt ~level ~pkh

  let get_attestable_slots ctxt pkh attested_level () () =
    call_handler2 ctxt (fun store {proto_parameters; _} ->
        Profile_manager.get_attestable_slots
          ctxt
          store
          proto_parameters
          pkh
          ~attested_level
        |> Errors.to_tzresult)
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
       Tezos_rpc.Directory.opt_register1
       Services.put_commitment_shards
       (Slots_handlers.put_commitment_shards ctxt)
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
  |> add_service
       Tezos_rpc.Directory.gen_register
       Services.monitor_shards
       (Slots_handlers.monitor_shards ctxt)

let register_legacy ctxt =
  let open RPC_server_legacy in
  Tezos_rpc.Directory.empty |> register_shard ctxt |> register_shards ctxt
  |> register_show_slot_pages ctxt

let register ctxt = register_new ctxt (register_legacy ctxt)

let merge dir plugin_dir = Tezos_rpc.Directory.merge dir plugin_dir

let start configuration ctxt =
  let open Lwt_syntax in
  let Configuration.{rpc_addr; _} = configuration in
  let dir = register ctxt in
  let rpc_port = snd rpc_addr in
  let rpc_addr = fst rpc_addr in
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

let monitor_shards_rpc ctxt =
  Tezos_rpc.Context.make_streamed_call Services.monitor_shards ctxt () () ()
