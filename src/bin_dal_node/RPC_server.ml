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

type error +=
  | Cryptobox_error of string * string
  | Post_slot_too_large of {expected : int; got : int}

let () =
  register_error_kind
    `Permanent
    ~id:"cryptobox_error"
    ~title:"cryptobox error"
    ~description:"A wrapper around an error raised by the cryptobox of the DAL."
    ~pp:(fun fmt (f, msg) ->
      Format.fprintf
        fmt
        "The DAL cryptobox function '%s' failed with:@.'%s'"
        f
        msg)
    Data_encoding.(obj2 (req "function_name" string) (req "explanation" string))
    (function Cryptobox_error (f, msg) -> Some (f, msg) | _ -> None)
    (fun (f, msg) -> Cryptobox_error (f, msg)) ;
  register_error_kind
    `Permanent
    ~id:"post_slot_too_large"
    ~title:"Post slot too large"
    ~description:
      "The length of posted data exceeds the expected size of DAL slots."
    ~pp:(fun fmt (expected, got) ->
      Format.fprintf
        fmt
        "The RPC expects a slot_size of at most '%d'. Got: '%d' expected got"
        expected
        got)
    Data_encoding.(obj2 (req "expected" int31) (req "got" int31))
    (function
      | Post_slot_too_large {expected; got} -> Some (expected, got) | _ -> None)
    (fun (expected, got) -> Post_slot_too_large {expected; got})

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

  (* This function assumes the slot is valid since we already have
     computed a commitment for it. *)
  let commitment_proof_from_slot cryptobox slot =
    let open Result_syntax in
    match Cryptobox.polynomial_from_slot cryptobox slot with
    | Error (`Slot_wrong_size msg) ->
        (* Storage consistency ensures we can always compute the
           polynomial from the slot. But let's returne an errror to be defensive. *)
        tzfail (Cryptobox_error ("polynomial_from_slot", msg))
    | Ok polynomial -> (
        match Cryptobox.prove_commitment cryptobox polynomial with
        (* [polynomial] was produced with the parameters from
           [cryptobox], thus we can always compute the proof from
           [polynomial] except if an error happens with the loading of the SRS. *)
        | Error (`Invalid_degree_strictly_less_than_expected _) ->
            tzfail
              (Cryptobox_error
                 ( "prove_commitment",
                   "Unexpected error. Maybe an issue with the SRS from the DAL \
                    node." ))
        | Ok proof -> return proof)

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
        | Some slot ->
            let*? proof = commitment_proof_from_slot cryptobox slot in
            return_some proof)

  let put_commitment_shards ctxt commitment () Types.{with_proof} =
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

  let post_slot ctxt () query slot =
    call_handler2
      ctxt
      (fun store {cryptobox; shards_proofs_precomputation; proto_parameters; _}
      ->
        let open Lwt_result_syntax in
        let slot_size = proto_parameters.cryptobox_parameters.slot_size in
        let slot_length = String.length slot in
        let*? slot =
          if slot_length > slot_size then
            Result_syntax.tzfail
              (Post_slot_too_large {expected = slot_size; got = slot_length})
          else if slot_length = slot_size then Ok (Bytes.of_string slot)
          else
            let padding = String.make (slot_size - slot_length) query#padding in
            Ok (Bytes.of_string (slot ^ padding))
        in
        let* commitment =
          Slot_manager.add_commitment store slot cryptobox |> Errors.to_tzresult
        in
        let*? commitment_proof = commitment_proof_from_slot cryptobox slot in
        (* Cannot return None *)
        let* (_ : unit option) =
          Slot_manager.add_commitment_shards
            ~shards_proofs_precomputation
            store
            cryptobox
            commitment
            ~with_proof:true
          |> Errors.to_option_tzresult
        in
        return (commitment, commitment_proof))

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
  let patch_profiles ctxt () operator_profiles =
    let open Lwt_result_syntax in
    let gs_worker = Node_context.get_gs_worker ctxt in
    call_handler2 ctxt (fun _store {proto_parameters; _} ->
        match
          Profile_manager.add_operator_profiles
            (Node_context.get_profile_ctxt ctxt)
            proto_parameters
            gs_worker
            operator_profiles
        with
        | None -> fail Errors.[Profile_incompatibility]
        | Some pctxt -> return @@ Node_context.set_profile_ctxt ctxt pctxt)

  let get_profiles ctxt () () =
    let open Lwt_result_syntax in
    return @@ Profile_manager.get_profiles (Node_context.get_profile_ctxt ctxt)

  let get_assigned_shard_indices ctxt pkh level () () =
    Node_context.fetch_assigned_shard_indices ctxt ~level ~pkh

  let get_attestable_slots ctxt pkh attested_level () () =
    call_handler2 ctxt (fun store {proto_parameters; _} ->
        (* For retrieving the assigned shard indexes, we consider the committee
           at [attested_level - 1], because the (DAL) attestations in the blocks
           at level [attested_level] refer to the predecessor level. *)
        let attestation_level = Int32.pred attested_level in
        (let open Lwt_result_syntax in
        let* shard_indices =
          Node_context.fetch_assigned_shard_indices
            ctxt
            ~pkh
            ~level:attestation_level
          |> Errors.other_lwt_result
        in
        Profile_manager.get_attestable_slots
          ~shard_indices
          store
          proto_parameters
          ~attested_level)
        |> Errors.to_tzresult)
end

let version ctxt () () =
  let open Lwt_result_syntax in
  Node_context.version ctxt |> return

module P2P = struct
  let connect ctxt q point =
    Node_context.P2P.connect ctxt ?timeout:q#timeout point

  let disconnect_point ctxt point q () =
    let open Lwt_result_syntax in
    let*! () = Node_context.P2P.disconnect_point ctxt ~wait:q#wait point in
    return_unit

  let disconnect_peer ctxt peer q () =
    let open Lwt_result_syntax in
    let*! () = Node_context.P2P.disconnect_peer ctxt ~wait:q#wait peer in
    return_unit

  let get_points ctxt q () =
    Node_context.P2P.get_points ~connected:q#connected ctxt

  let get_points_info ctxt q () =
    Node_context.P2P.get_points_info ~connected:q#connected ctxt

  let get_point_info ctxt point () () =
    Node_context.P2P.get_point_info ctxt point

  let get_peers ctxt q () =
    Node_context.P2P.get_peers ~connected:q#connected ctxt

  let get_peers_info ctxt q () =
    Node_context.P2P.get_peers_info ~connected:q#connected ctxt

  let get_peer_info ctxt peer () () = Node_context.P2P.get_peer_info ctxt peer

  module Gossipsub = struct
    let get_topics ctxt () () =
      let open Lwt_result_syntax in
      return @@ Node_context.P2P.Gossipsub.get_topics ctxt

    let get_topics_peers ctxt q () =
      let open Lwt_result_syntax in
      return
      @@ Node_context.P2P.Gossipsub.get_topics_peers
           ~subscribed:q#subscribed
           ctxt

    let get_connections ctxt () () =
      let open Lwt_result_syntax in
      return @@ Node_context.P2P.Gossipsub.get_connections ctxt

    let get_scores ctxt () () =
      let open Lwt_result_syntax in
      return @@ Node_context.P2P.Gossipsub.get_scores ctxt

    let get_backoffs ctxt () () =
      let open Lwt_result_syntax in
      return @@ Node_context.P2P.Gossipsub.get_backoffs ctxt

    let get_message_cache ctxt () () =
      let open Lwt_result_syntax in
      return @@ Node_context.P2P.Gossipsub.get_message_cache ctxt
  end
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
       Services.patch_profiles
       (Profile_handlers.patch_profiles ctxt)
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
  |> add_service Tezos_rpc.Directory.register0 Services.version (version ctxt)
  |> add_service
       Tezos_rpc.Directory.register0
       Services.P2P.Gossipsub.get_topics
       (P2P.Gossipsub.get_topics ctxt)
  |> add_service
       Tezos_rpc.Directory.register0
       Services.P2P.Gossipsub.get_topics_peers
       (P2P.Gossipsub.get_topics_peers ctxt)
  |> add_service
       Tezos_rpc.Directory.register0
       Services.P2P.Gossipsub.get_connections
       (P2P.Gossipsub.get_connections ctxt)
  |> add_service
       Tezos_rpc.Directory.register0
       Services.P2P.Gossipsub.get_scores
       (P2P.Gossipsub.get_scores ctxt)
  |> add_service
       Tezos_rpc.Directory.register0
       Services.P2P.Gossipsub.get_backoffs
       (P2P.Gossipsub.get_backoffs ctxt)
  |> add_service
       Tezos_rpc.Directory.register0
       Services.P2P.Gossipsub.get_message_cache
       (P2P.Gossipsub.get_message_cache ctxt)
  |> add_service
       Tezos_rpc.Directory.register0
       Services.P2P.post_connect
       (P2P.connect ctxt)
  |> add_service
       Tezos_rpc.Directory.register1
       Services.P2P.delete_disconnect_point
       (P2P.disconnect_point ctxt)
  |> add_service
       Tezos_rpc.Directory.register1
       Services.P2P.delete_disconnect_peer
       (P2P.disconnect_peer ctxt)
  |> add_service
       Tezos_rpc.Directory.register0
       Services.P2P.get_points
       (P2P.get_points ctxt)
  |> add_service
       Tezos_rpc.Directory.register0
       Services.P2P.get_points_info
       (P2P.get_points_info ctxt)
  |> add_service
       Tezos_rpc.Directory.opt_register1
       Services.P2P.Points.get_point_info
       (P2P.get_point_info ctxt)
  |> add_service
       Tezos_rpc.Directory.register0
       Services.P2P.get_peers
       (P2P.get_peers ctxt)
  |> add_service
       Tezos_rpc.Directory.register0
       Services.P2P.get_peers_info
       (P2P.get_peers_info ctxt)
  |> add_service
       Tezos_rpc.Directory.opt_register1
       Services.P2P.Peers.get_peer_info
       (P2P.get_peer_info ctxt)

let register_legacy ctxt =
  let open RPC_server_legacy in
  Tezos_rpc.Directory.empty |> register_shard ctxt |> register_shards ctxt
  |> register_show_slot_pages ctxt

let register ctxt = register_new ctxt (register_legacy ctxt)

let merge dir plugin_dir = Tezos_rpc.Directory.merge dir plugin_dir

let start configuration ctxt =
  let open Lwt_syntax in
  let Configuration_file.{rpc_addr; _} = configuration in
  let dir = register ctxt in
  let dir =
    Tezos_rpc.Directory.register_describe_directory_service
      dir
      Tezos_rpc.Service.description_service
  in
  let rpc_port = snd rpc_addr in
  let rpc_addr = fst rpc_addr in
  let host = Ipaddr.V6.to_string rpc_addr in
  let node = `TCP (`Port rpc_port) in
  (* FIXME https://gitlab.com/tezos/tezos/-/issues/5918

     We should probably configure a better ACL policy.
  *)
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
