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

let call_handler1 handler = handler () |> Errors.to_option_tzresult

type error +=
  | Cryptobox_error of string * string
  | Cannot_publish_on_slot_index of Types.slot_index

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
    ~id:"cannot_publish_on_slot_index"
    ~title:"Cannot publish on requested slot index with current profiles"
    ~description:
      "The DAL node does not have a profile compatible with publication on the \
       requested slot index. Consider adding an operator or observer profile."
    Data_encoding.(obj1 (req "slot_index" uint8))
    (function
      | Cannot_publish_on_slot_index slot_index -> Some slot_index | _ -> None)
    (fun slot_index -> Cannot_publish_on_slot_index slot_index)

module Slots_handlers = struct
  let get_slot_content ctxt slot_level slot_index () () =
    call_handler1 (fun () ->
        let slot_id : Types.slot_id = {slot_level; slot_index} in
        Slot_manager.get_slot_content ~reconstruct_if_missing:true ctxt slot_id)

  let get_slot_page_proof ctxt slot_level slot_index page_index () () =
    call_handler1 (fun () ->
        let open Lwt_result_syntax in
        let slot_id : Types.slot_id = {slot_level; slot_index} in
        let* content =
          Slot_manager.get_slot_content
            ~reconstruct_if_missing:true
            ctxt
            slot_id
        in
        let*! proof =
          let cryptobox = Node_context.get_cryptobox ctxt in
          let*? polynomial = Cryptobox.polynomial_from_slot cryptobox content in
          let*? proof = Cryptobox.prove_page cryptobox polynomial page_index in
          return proof
        in
        match proof with
        | Ok proof -> return proof
        | Error e ->
            let msg =
              match e with
              | `Fail s -> "Fail " ^ s
              | `Page_index_out_of_range -> "Page_index_out_of_range"
              | `Slot_wrong_size s -> "Slot_wrong_size: " ^ s
              | ( `Invalid_degree_strictly_less_than_expected _
                | `Prover_SRS_not_loaded ) as commit_error ->
                  Cryptobox.string_of_commit_error commit_error
            in
            fail (Errors.other [Cryptobox_error ("get_slot_page_proof", msg)]))

  let error_pp ppf (slot : [`Not_found | `Other of error trace]) =
    match slot with
    | `Not_found -> Format.pp_print_string ppf "slot not found"
    | `Other errors -> pp_print_trace ppf errors

  let post_slot =
   fun ctxt query slot ->
    call_handler1 @@ fun () ->
    let open Lwt_result_syntax in
    let profile = Node_context.get_profile_ctxt ctxt in
    let* () =
      match query#slot_index with
      | Some slot_index
        when not (Profile_manager.can_publish_on_slot_index slot_index profile)
        ->
          fail (Errors.other [Cannot_publish_on_slot_index slot_index])
      | None | Some _ -> return_unit
    in
    (Slot_production.produce_commitment_and_proof
       ctxt
       query#padding
       slot
     [@profiler.wrap_f
       {driver_ids = [Opentelemetry]}
         (Opentelemetry_helpers.trace_slot_after_es
            ~name:"inject_slot"
            ?slot_index:query#slot_index
            ~error_pp
            ~commitment_of_result:fst)])

  let get_slot_commitment ctxt slot_level slot_index () () =
    call_handler1 (fun () ->
        let open Lwt_result_syntax in
        let slot_id : Types.slot_id = {slot_level; slot_index} in
        let* slot_header_opt =
          Slot_manager.try_get_slot_header_from_indexed_skip_list ctxt slot_id
          |> Errors.other_lwt_result
        in
        match slot_header_opt with
        | None -> fail `Not_found
        | Some slot_header -> return slot_header.Dal_plugin.commitment)

  let get_slot_status ctxt slot_level slot_index () () =
    call_handler1 (fun () ->
        let slot_id : Types.slot_id = {slot_level; slot_index} in
        Slot_manager.get_slot_status ~slot_id ctxt)

  let get_slot_shard ctxt slot_level slot_index shard_index () () =
    call_handler1 (fun () ->
        let store = Node_context.get_store ctxt in
        let slot_id : Types.slot_id = {slot_level; slot_index} in
        Slot_manager.get_slot_shard store slot_id shard_index)

  let get_slot_pages ctxt slot_level slot_index () () =
    call_handler1 (fun () ->
        let slot_id : Types.slot_id = {slot_level; slot_index} in
        Slot_manager.get_slot_pages ~reconstruct_if_missing:true ctxt slot_id)
end

module Node = struct
  let get_last_processed_level ctxt () () =
    Node_context.get_store ctxt
    |> Store.last_processed_level |> Store.Last_processed_level.load

  let get_protocol_parameters ctxt level () =
    let level = match level with None -> `Last_proto | Some l -> `Level l in
    Node_context.get_proto_parameters ~level ctxt |> Lwt.return
end

module Profile_handlers = struct
  let patch_profiles ctxt () controller_profiles =
    let open Lwt_result_syntax in
    let gs_worker = Node_context.get_gs_worker ctxt in
    call_handler1 (fun () ->
        let* () =
          Node_context.warn_if_attesters_not_delegates ctxt controller_profiles
          |> lwt_map_error (fun e -> `Other e)
        in
        let* proto_parameters =
          Node_context.get_proto_parameters ctxt ~level:`Last_proto
          |> Lwt.return
          |> lwt_map_error (fun e -> `Other e)
        in
        let number_of_slots = proto_parameters.Types.number_of_slots in
        match
          Profile_manager.add_and_register_controller_profiles
            (Node_context.get_profile_ctxt ctxt)
            ~number_of_slots
            gs_worker
            controller_profiles
        with
        | None -> fail @@ Errors.(other [Profile_incompatibility])
        | Some pctxt ->
            let*! () = Node_context.set_profile_ctxt ctxt pctxt in
            return_unit)

  let get_profiles ctxt () () =
    let open Lwt_result_syntax in
    return @@ Profile_manager.get_profiles (Node_context.get_profile_ctxt ctxt)

  let get_assigned_shard_indices ctxt pkh level () () =
    Node_context.fetch_assigned_shard_indices ctxt ~level ~pkh

  let warn_missing_shards ctxt attester published_level
      expected_number_of_shards number_of_stored_shards_per_slot =
    let open Lwt_syntax in
    let* problems =
      List.filter_map_s
        (fun (Types.Slot_id.{slot_index; _}, num_stored) ->
          if num_stored = expected_number_of_shards then
            Lwt.return_some (`Ok (slot_index, num_stored))
          else
            let* res =
              Slot_manager.get_slot_status
                ctxt
                ~slot_id:{slot_level = published_level; slot_index}
            in
            match res with
            | Error `Not_found ->
                let* () =
                  Event.emit_slot_header_status_not_found
                    ~published_level
                    ~slot_index
                in
                Lwt.return_none
            | Error (`Other error) ->
                let* () =
                  Event.emit_slot_header_status_storage_error
                    ~published_level
                    ~slot_index
                    ~error
                in
                Lwt.return_none
            | Ok res -> (
                match res with
                | `Waiting_attestation ->
                    Lwt.return_some (`Not_ok (slot_index, num_stored))
                | `Unpublished -> Lwt.return_none
                | (`Attested _ | `Unattested) as status ->
                    (* Most probably the RPC was called/handled too late. This
                       may mean that that DAL node is lagging. *)
                    let* () =
                      Event.emit_unexpected_slot_header_status
                        ~published_level
                        ~slot_index
                        ~expected_status:`Waiting_attestation
                        ~got_status:status
                    in
                    Lwt.return_none))
        number_of_stored_shards_per_slot
    in
    let ok, not_ok =
      List.partition (function `Ok _ -> true | `Not_ok _ -> false) problems
    in
    let* () =
      if List.is_empty ok then Lwt.return_unit
      else
        let slots_indices =
          List.filter_map
            (function
              | `Ok (slot_index, _num_stored) -> Some slot_index
              | `Not_ok _ -> None)
            ok
        in
        Event.emit_get_attestable_slots_ok_notice
          ~attester
          ~published_level
          ~slots_indices
    in
    let* () =
      if List.is_empty not_ok then Lwt.return_unit
      else
        let count_received_incomplete_shards_per_slot =
          List.filter_map
            (function
              | `Ok _ -> None
              | `Not_ok (idx, stored) ->
                  Some (idx, stored, expected_number_of_shards))
            not_ok
        in
        let indexes =
          List.map
            (fun (idx, _, _) -> idx)
            count_received_incomplete_shards_per_slot
        in
        Event.emit_get_attestable_slots_not_ok_warning
          ~attester
          ~published_level
          ~slots_indices:indexes
          ~slot_indexes_with_details:count_received_incomplete_shards_per_slot
    in
    Lwt.return_unit

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/7969

     We could reuse the internal L1 crawler's newly added status to implement this
     function. *)
  let warn_if_lagging ~last_finalized_level ~attestation_level =
    (* The L1 node's level is at least [last_finalized_level + 2], because the
       DAL node processes blocks with a delay of two levels, to be sure that
       processed blocks are final. *)
    let current_level = Int32.add last_finalized_level 2l in
    (* The baker's current level is the same as its L1 node and is the level
       of the latest seen proposal (ie block). The baker asks for slots'
       status when it has seen a proposal at [attestation_level - 1]. *)
    let current_baker_level = Int32.sub attestation_level 1l in
    (* We check that the baker is not in advance wrt the DAL node, which would
       mean that the DAL node is lagging. We allow a slack of 1 level. *)
    if Int32.succ current_level < current_baker_level then
      Event.emit_get_attestable_slots_future_level_warning
        ~current_level
        ~current_baker_level
    else Lwt.return_unit

  let get_attestable_slots ctxt pkh attested_level () () =
    let get_attestable_slots ~shard_indices store last_known_parameters
        ~attested_level =
      let open Lwt_result_syntax in
      let number_of_assigned_shards = List.length shard_indices in
      if number_of_assigned_shards = 0 then return Types.Not_in_committee
      else
        let published_level =
          Int32.(
            sub
              attested_level
              (of_int last_known_parameters.Types.attestation_lag))
        in
        if published_level <= 1l then
          let slots =
            Stdlib.List.init last_known_parameters.number_of_slots (fun _ ->
                false)
          in
          return (Types.Attestable_slots {slots; published_level})
        else
          let shards_store = Store.shards store in
          let number_of_shards_stored slot_index =
            let slot_id : Types.slot_id =
              {slot_level = published_level; slot_index}
            in
            let+ number_stored_shards =
              Store.Shards.number_of_shards_available
                shards_store
                slot_id
                shard_indices
              |> lwt_map_error (fun e -> `Other e)
            in
            (slot_id, number_stored_shards)
          in
          let* published_level_parameters =
            Node_context.get_proto_parameters
              ctxt
              ~level:(`Level published_level)
            |> Lwt.return
            |> lwt_map_error (fun e -> `Other e)
          in
          let all_slot_indexes =
            Utils.Infix.(0 -- (published_level_parameters.number_of_slots - 1))
          in
          let* number_of_stored_shards_per_slot =
            List.map_es number_of_shards_stored all_slot_indexes
          in
          let* flags =
            List.map_es
              (fun (slot_id, num_stored) ->
                let all_stored = num_stored = number_of_assigned_shards in
                if not last_known_parameters.incentives_enable then
                  return all_stored
                else if not all_stored then return false
                else
                  Attestable_slots.is_attestable_slot_with_traps
                    shards_store
                    last_known_parameters.traps_fraction
                    pkh
                    shard_indices
                    slot_id)
              number_of_stored_shards_per_slot
          in
          Lwt.dont_wait
            (fun () ->
              warn_missing_shards
                ctxt
                pkh
                published_level
                number_of_assigned_shards
                number_of_stored_shards_per_slot)
            (fun _exn -> ()) ;
          return (Types.Attestable_slots {slots = flags; published_level})
    in

    (* TODO: https://gitlab.com/tezos/tezos/-/issues/8064 *)
    let get_attestable_slots ~shard_indices store last_known_parameters
        ~attested_level =
      let open Lwt_result_syntax in
      let* should_drop_due_to_migration =
        Attestable_slots.attested_just_after_migration ctxt ~attested_level
        |> Lwt.return
        |> lwt_map_error (fun e -> `Other e)
      in
      if should_drop_due_to_migration then
        let*! () = Event.emit_skip_attesting_shards ~level:attested_level in

        let slots =
          Stdlib.List.init last_known_parameters.Types.number_of_slots (fun _ ->
              false)
        in
        return (Types.Attestable_slots {slots; published_level = 0l})
      else
        get_attestable_slots
          ~shard_indices
          store
          last_known_parameters
          ~attested_level
    in

    call_handler1 (fun () ->
        let open Lwt_result_syntax in
        let last_finalized_level = Node_context.get_last_finalized_level ctxt in
        let attestation_level = Int32.pred attested_level in
        let*! () = warn_if_lagging ~last_finalized_level ~attestation_level in
        (* For retrieving the assigned shard indexes, we consider the committee
           at [attestation_level], because the (DAL) attestations in the blocks
           at level [attested_level] refer to the predecessor level. *)
        let* shard_indices =
          Node_context.fetch_assigned_shard_indices
            ctxt
            ~pkh
            ~level:attestation_level
          |> Errors.other_lwt_result
        in
        let* last_known_parameters =
          Node_context.get_proto_parameters ctxt ~level:(`Level attested_level)
          |> Lwt.return
          |> lwt_map_error (fun e -> `Other e)
        in
        let store = Node_context.get_store ctxt in
        get_attestable_slots
          ~shard_indices
          store
          last_known_parameters
          ~attested_level)

  let monitor_attestable_slots ctxt pkh () () =
    let open Lwt_syntax in
    let* Resto_directory.Answer.{next; shutdown} =
      Attestable_slots.subscribe ctxt ~pkh
    in
    Tezos_rpc.Answer.return_stream {next; shutdown}
end

let version ctxt () () =
  let open Lwt_result_syntax in
  Node_context.version ctxt |> return

let get_traps ctxt published_level query () =
  let traps_store = Node_context.get_store ctxt |> Store.traps in
  let traps = Store.Traps.find traps_store ~level:published_level in
  Lwt_result_syntax.return
  @@
  match (query#delegate, query#slot_index) with
  | None, None -> traps
  | Some pkh, None ->
      List.filter
        (fun Types.{delegate; _} ->
          Signature.Public_key_hash.equal delegate pkh)
        traps
  | None, Some index ->
      List.filter (fun Types.{slot_index; _} -> index = slot_index) traps
  | Some pkh, Some index ->
      List.filter
        (fun Types.{delegate; slot_index; _} ->
          index = slot_index && Signature.Public_key_hash.equal delegate pkh)
        traps

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
    Lwt.return @@ Node_context.P2P.get_points ~connected:q#connected ctxt

  let get_points_info ctxt q () =
    Lwt.return @@ Node_context.P2P.get_points_info ~connected:q#connected ctxt

  let get_point_info ctxt point () () =
    Lwt.return @@ Node_context.P2P.get_point_info ctxt point

  let get_peers ctxt q () =
    Lwt.return @@ Node_context.P2P.get_peers ~connected:q#connected ctxt

  let get_peers_info ctxt q () =
    Lwt.return @@ Node_context.P2P.get_peers_info ~connected:q#connected ctxt

  let get_peer_info ctxt peer () () =
    Lwt.return @@ Node_context.P2P.get_peer_info ctxt peer

  let patch_peer ctxt peer () acl = Node_context.P2P.patch_peer ctxt peer acl

  module Gossipsub = struct
    let get_mesh ctxt q () =
      let open Lwt_result_syntax in
      return
      @@ Node_context.P2P.Gossipsub.get_mesh
           ?slot_index:q#slot_index
           ?delegate:q#delegate
           ctxt

    let get_topics ctxt () () =
      let open Lwt_result_syntax in
      return @@ Node_context.P2P.Gossipsub.get_topics ctxt

    let get_topics_peers ctxt q () =
      let open Lwt_result_syntax in
      return
      @@ Node_context.P2P.Gossipsub.get_topics_peers
           ~subscribed:(not q#all)
           ctxt

    let get_fanout ctxt () () =
      let open Lwt_result_syntax in
      return @@ Node_context.P2P.Gossipsub.get_fanout ctxt

    let get_slot_indexes_peers ctxt q () =
      let open Lwt_result_syntax in
      return
      @@ Node_context.P2P.Gossipsub.get_slot_indexes_peers
           ~subscribed:(not q#all)
           ctxt

    let get_pkhs_peers ctxt q () =
      let open Lwt_result_syntax in
      return
      @@ Node_context.P2P.Gossipsub.get_pkhs_peers ~subscribed:(not q#all) ctxt

    let get_connections ?ignore_bootstrap_topics ctxt () () =
      let open Lwt_result_syntax in
      return
      @@ Node_context.P2P.Gossipsub.get_connections
           ?ignore_bootstrap_topics
           ctxt

    let get_reconnection_delays ctxt () () =
      let open Lwt_result_syntax in
      return @@ Node_context.P2P.Gossipsub.get_reconnection_delays ctxt

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

module Health = struct
  let get_health ctxt () () =
    let open Lwt_result_syntax in
    let open Types.Health in
    let profiles = Node_context.get_profile_ctxt ctxt in
    let no_profile = Profile_manager.is_empty profiles in
    let*? points = Node_context.P2P.get_points ctxt in
    let topics = Node_context.P2P.Gossipsub.get_topics ctxt in
    let connections = Node_context.P2P.Gossipsub.get_connections ctxt in
    match (points, no_profile, topics, connections) with
    | [], _, _, _ ->
        let checks = [("p2p", Down)] in
        return {status = Down; checks}
    | _, true, _, _ ->
        let checks = [("p2p", Up); ("Has registered profiles", No)] in
        return {status = Degraded; checks}
    | _, _, [], _ ->
        let checks = [("p2p", Up); ("topics", Ko)] in
        return {status = Degraded; checks}
    | _, _, _, [] ->
        let checks = [("p2p", Up); ("topics", Ok); ("gossipsub", Down)] in
        return {status = Degraded; checks}
    | _ ->
        let checks = [("p2p", Up); ("topics", Ok); ("gossipsub", Up)] in
        return {status = Up; checks}
end

module Synchronized = struct
  let get_synchronized ctxt () () =
    Lwt_result_syntax.return @@ Node_context.get_l1_crawler_status ctxt

  let get_monitor_synchronized ctxt () () =
    let l1_crawler_status_input =
      Node_context.get_l1_crawler_status_input ctxt
    in
    let stream, stopper = Lwt_watcher.create_stream l1_crawler_status_input in
    let next () = Lwt_stream.get stream in
    let shutdown () = Lwt_watcher.shutdown stopper in
    Tezos_rpc.Answer.return_stream {next; shutdown}
end

let add_service registerer service handler directory =
  registerer directory service handler

let register :
    Node_context.t -> unit Tezos_rpc.Directory.t -> unit Tezos_rpc.Directory.t =
 fun ctxt directory ->
  directory
  |> add_service
       Tezos_rpc.Directory.opt_register0
       Services.post_slot
       (Slots_handlers.post_slot ctxt)
  |> add_service
       Tezos_rpc.Directory.opt_register2
       Services.get_slot_content
       (Slots_handlers.get_slot_content ctxt)
  |> add_service
       Tezos_rpc.Directory.opt_register3
       Services.get_slot_page_proof
       (Slots_handlers.get_slot_page_proof ctxt)
  |> add_service
       Tezos_rpc.Directory.opt_register2
       Services.get_slot_commitment
       (Slots_handlers.get_slot_commitment ctxt)
  |> add_service
       Tezos_rpc.Directory.opt_register0
       Services.patch_profiles
       (Profile_handlers.patch_profiles ctxt)
  |> add_service
       Tezos_rpc.Directory.register0
       Services.get_profiles
       (Profile_handlers.get_profiles ctxt)
  |> add_service
       Tezos_rpc.Directory.opt_register2
       Services.get_slot_status
       (Slots_handlers.get_slot_status ctxt)
  |> add_service
       Tezos_rpc.Directory.register2
       Services.get_assigned_shard_indices
       (Profile_handlers.get_assigned_shard_indices ctxt)
  |> add_service
       Tezos_rpc.Directory.opt_register2
       Services.get_attestable_slots
       (Profile_handlers.get_attestable_slots ctxt)
  |> add_service
       Tezos_rpc.Directory.gen_register1
       Services.monitor_attestable_slots
       (Profile_handlers.monitor_attestable_slots ctxt)
  |> add_service
       Tezos_rpc.Directory.register1
       Services.get_traps
       (get_traps ctxt)
  |> add_service
       Tezos_rpc.Directory.opt_register2
       Services.get_slot_pages
       (Slots_handlers.get_slot_pages ctxt)
  |> add_service Tezos_rpc.Directory.register0 Services.version (version ctxt)
  |> add_service
       Tezos_rpc.Directory.register0
       Services.P2P.Gossipsub.get_mesh
       (P2P.Gossipsub.get_mesh ctxt)
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
       Services.P2P.Gossipsub.get_fanout
       (P2P.Gossipsub.get_fanout ctxt)
  |> add_service
       Tezos_rpc.Directory.register0
       Services.P2P.Gossipsub.get_slot_indexes_peers
       (P2P.Gossipsub.get_slot_indexes_peers ctxt)
  |> add_service
       Tezos_rpc.Directory.register0
       Services.P2P.Gossipsub.get_pkhs_peers
       (P2P.Gossipsub.get_pkhs_peers ctxt)
  |> add_service
       Tezos_rpc.Directory.register0
       Services.P2P.Gossipsub.get_connections
       (P2P.Gossipsub.get_connections ~ignore_bootstrap_topics:true ctxt)
  |> add_service
       Tezos_rpc.Directory.register0
       Services.P2P.Gossipsub.get_reconnection_delays
       (P2P.Gossipsub.get_reconnection_delays ctxt)
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
       Services.P2P.Points.delete_disconnect_point
       (P2P.disconnect_point ctxt)
  |> add_service
       Tezos_rpc.Directory.register1
       Services.P2P.Peers.delete_disconnect_peer
       (P2P.disconnect_peer ctxt)
  |> add_service
       Tezos_rpc.Directory.register0
       Services.P2P.Points.get_points
       (P2P.get_points ctxt)
  |> add_service
       Tezos_rpc.Directory.register0
       Services.P2P.Points.get_points_info
       (P2P.get_points_info ctxt)
  |> add_service
       Tezos_rpc.Directory.opt_register1
       Services.P2P.Points.get_point_info
       (P2P.get_point_info ctxt)
  |> add_service
       Tezos_rpc.Directory.register0
       Services.P2P.Peers.get_peers
       (P2P.get_peers ctxt)
  |> add_service
       Tezos_rpc.Directory.register0
       Services.P2P.Peers.get_peers_info
       (P2P.get_peers_info ctxt)
  |> add_service
       Tezos_rpc.Directory.opt_register1
       Services.P2P.Peers.get_peer_info
       (P2P.get_peer_info ctxt)
  |> add_service
       Tezos_rpc.Directory.opt_register1
       Services.P2P.Peers.patch_peer
       (P2P.patch_peer ctxt)
  |> add_service
       Tezos_rpc.Directory.opt_register3
       Services.get_slot_shard
       (Slots_handlers.get_slot_shard ctxt)
  |> add_service
       Tezos_rpc.Directory.register0
       Services.health
       (Health.get_health ctxt)
  |> add_service
       Tezos_rpc.Directory.register0
       Services.synchronized
       (Synchronized.get_synchronized ctxt)
  |> add_service
       Tezos_rpc.Directory.gen_register0
       Services.monitor_synchronized
       (Synchronized.get_monitor_synchronized ctxt)
  |> add_service
       Tezos_rpc.Directory.opt_register0
       Services.get_last_processed_level
       (Node.get_last_processed_level ctxt)
  |> add_service
       Tezos_rpc.Directory.register0
       Services.get_protocol_parameters
       (Node.get_protocol_parameters ctxt)

let register_plugin node_ctxt =
  let open Lwt_syntax in
  Tezos_rpc.Directory.register_dynamic_directory
    Tezos_rpc.Directory.empty
    Tezos_rpc.Path.(open_root / "plugin")
    (fun () ->
      let store = Node_context.get_store node_ctxt in
      (* FIXME: https://gitlab.com/tezos/tezos/-/issues/7069

         DAL: handle protocol plugins change in dynamic proto-related RPCs.

         In case of protocol change where the type of cells and/or hashes
         change(s), we could register the wrong directory (the one with the
         current plugin while we want to request data encoded with the
         previous protocol). A fix would be try answering the RPCs with the
         current protocol plugin, then with the previous one in case of
         failure. *)
      let skip_list_cells_store = Store.skip_list_cells store in
      List.fold_left
        (fun dir (module Plugin : Dal_plugin.T) ->
          Tezos_rpc.Directory.merge
            dir
            (Plugin.RPC.directory skip_list_cells_store))
        Tezos_rpc.Directory.empty
        (Node_context.get_all_plugins node_ctxt)
      |> return)

let start configuration ctxt =
  let open Lwt_syntax in
  let Configuration_file.{rpc_addr; _} = configuration in
  let dir = register ctxt (register_plugin ctxt) in
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
  let* () = Event.emit_shutdown_node ~exit_status in
  Tezos_base_unix.Internal_event_unix.close ()
