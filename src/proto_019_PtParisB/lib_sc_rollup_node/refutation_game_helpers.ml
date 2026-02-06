(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 TriliTech <contact@trili.tech>                         *)
(* Copyright (c) 2023 Functori, <contact@functori.com>                       *)
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

open Protocol
open Alpha_context

(** When the PVM is waiting for a Dal page input, this function attempts to
      retrieve the page's content from the store, the data of its slot. Then it
      computes the proof that the page is part of the slot and returns the
      content along with the proof.

      If the PVM is not waiting for a Dal page input, or if the slot is known to
      be unconfirmed on L1, this function returns [None]. If the data of the
      slot are not saved to the store, the function returns a failure
      in the error monad. *)
let page_info_from_pvm_state constants (node_ctxt : _ Node_context.t)
    ~inbox_level (dal_params : Dal.parameters) start_state =
  let open Lwt_result_syntax in
  let is_reveal_enabled =
    match constants.Rollup_constants.sc_rollup.reveal_activation_level with
    | Some reveal_activation_level ->
        Sc_rollup.is_reveal_enabled_predicate
          (Sc_rollup_proto_types.Constants.reveal_activation_level_of_octez
             reveal_activation_level)
    | None ->
        (* For older protocol, constants don't have the notion of reveal
           activation level. *)
        fun ~current_block_level:_ _ -> true
  in
  let* dal_activation_level, dal_attested_slots_validity_lag =
    match constants.sc_rollup.reveal_activation_level with
    | Some reveal_activation_level when constants.dal.feature_enable ->
        let*? level =
          Raw_level.of_int32 reveal_activation_level.dal_parameters
          |> Environment.wrap_tzresult
        in
        return
          ( Some level,
            Int32.to_int reveal_activation_level.dal_attested_slots_validity_lag
          )
    | _ -> return (None, max_int)
  in
  let*! input_request =
    let open (val Pvm.of_kind node_ctxt.kind) in
    Mutable_state.is_input_state
      ~is_reveal_enabled
      (Ctxt_wrapper.of_node_pvmstate start_state)
  in
  match input_request with
  | Sc_rollup.(Needs_reveal (Request_dal_page page_id)) -> (
      let Dal.Page.{slot_id; page_index} = page_id in
      let* pages =
        Dal_pages_request.slot_pages
          constants.Rollup_constants.dal
          ~dal_activation_level
          ~dal_attested_slots_validity_lag
          ~inbox_level
          node_ctxt
          slot_id
      in
      match pages with
      | None -> return_none (* The slot is not confirmed. *)
      | Some pages -> (
          let pages_per_slot = dal_params.slot_size / dal_params.page_size in
          (* check invariant that pages' length is correct. *)
          (* FIXME/DAL: https://gitlab.com/tezos/tezos/-/issues/4031
             It's better to do the check when the slots are saved into disk. *)
          (* FIXME/DAL: https://gitlab.com/tezos/tezos/-/issues/3997
             This check is not resilient to dal parameters change. *)
          match List.nth_opt pages page_index with
          | Some content ->
              let* page_proof =
                let dal_cctxt =
                  WithExceptions.Option.get ~loc:__LOC__ node_ctxt.dal_cctxt
                in
                let Dal.Slot.Header.{published_level; index} = slot_id in
                Dal_node_client.get_slot_page_proof
                  dal_cctxt
                  {
                    slot_level = published_level |> Raw_level.to_int32;
                    slot_index = index |> Alpha_context.Dal.Slot_index.to_int;
                  }
                  page_index
              in
              return_some (content, page_proof)
          | None ->
              failwith
                "Page index %d too big or negative.\n\
                 Number of pages in a slot is %d."
                page_index
                pages_per_slot))
  | _ -> return_none

let metadata (node_ctxt : _ Node_context.t) =
  let address = node_ctxt.config.sc_rollup_address in
  let origination_level = Raw_level.of_int32_exn node_ctxt.genesis_info.level in
  Sc_rollup.Metadata.{address; origination_level}

let generate_proof (node_ctxt : _ Node_context.t)
    (game : Octez_smart_rollup.Game.t) (start_state : Context.pvmstate) =
  let open Lwt_result_syntax in
  let module PVM = (val Pvm.of_kind node_ctxt.kind) in
  let snapshot =
    Sc_rollup_proto_types.Inbox.history_proof_of_octez game.inbox_snapshot
  in
  (* NOTE: [snapshot_level_int32] below refers to the level of the snapshotted
     inbox (from the skip list) which also matches [game.start_level - 1]. *)
  let snapshot_level_int32 =
    (Octez_smart_rollup.Inbox.Skip_list.content game.inbox_snapshot).level
  in
  let* context =
    let* start_hash = Node_context.hash_of_level node_ctxt game.inbox_level in
    let+ context = Node_context.checkout_context node_ctxt start_hash in
    Context.index context
  in
  let dal_slots_history =
    (* Similarly to what's done for inbox snapshot above. *)
    Sc_rollup_proto_types.Dal.Slot_history.of_octez game.dal_snapshot
  in
  let get_cell_from_hash hash =
    (* each time a DAL skip list cell is requested by hash, we retrieve it from the DAL node. *)
    (* TODO: We may want to have a local cache here. *)
    let open Lwt_syntax in
    let dal_cctxt = node_ctxt.dal_cctxt in
    let dal_cctxt = WithExceptions.Option.get ~loc:__LOC__ dal_cctxt in
    let+ res =
      Dal_proto_client.get_commitments_history_hash_content dal_cctxt hash
    in
    Result.to_option res
  in
  (* We fetch the value of protocol constants at block snapshot level
     where the game started. *)
  let* constants =
    Protocol_plugins.get_constants_of_level node_ctxt snapshot_level_int32
  in
  let dal_l1_parameters = constants.dal in
  let dal_parameters = dal_l1_parameters.cryptobox_parameters in
  let dal_attestation_lag = dal_l1_parameters.attestation_lag in
  let dal_number_of_slots = dal_l1_parameters.number_of_slots in
  let* dal_activation_level, dal_attested_slots_validity_lag =
    match constants.sc_rollup.reveal_activation_level with
    | Some reveal_activation_level when dal_l1_parameters.feature_enable ->
        let*? level =
          Raw_level.of_int32 reveal_activation_level.dal_parameters
          |> Environment.wrap_tzresult
        in
        return
          ( Some level,
            Int32.to_int reveal_activation_level.dal_attested_slots_validity_lag
          )
    | _ -> return (None, max_int)
  in
  let* page_info =
    page_info_from_pvm_state
      constants
      ~inbox_level:game.inbox_level
      node_ctxt
      dal_parameters
      start_state
  in
  let module P = struct
    include PVM

    let context : context = Ctxt_wrapper.of_node_context context

    let state = PVM.Ctxt_wrapper.(to_imm @@ of_node_pvmstate start_state)

    let reveal hash =
      let open Lwt_syntax in
      let* res =
        Reveals.get
          ~pre_images_endpoint:node_ctxt.config.pre_images_endpoint
          ~data_dir:node_ctxt.data_dir
          ~pvm_kind:(Sc_rollup_proto_types.Kind.to_octez PVM.kind)
          hash
      in
      match res with Ok data -> return_some data | Error _ -> return_none

    module Inbox_with_history = struct
      let inbox = snapshot

      let get_history inbox_hash =
        let open Lwt_syntax in
        let+ inbox = Node_context.find_inbox node_ctxt inbox_hash in
        match inbox with
        | Error err ->
            Format.kasprintf
              Stdlib.failwith
              "Refutation game: Cannot get inbox history for %a, %a"
              Sc_rollup.Inbox.Hash.pp
              inbox_hash
              pp_print_trace
              err
        | Ok inbox ->
            Option.map
              (fun i ->
                Sc_rollup.Inbox.take_snapshot
                  (Sc_rollup_proto_types.Inbox.of_octez i))
              inbox

      let get_payloads_history witness =
        Lwt.map
          (WithExceptions.Result.to_exn_f
             ~error:(Format.kasprintf Stdlib.failwith "%a" pp_print_trace))
        @@
        let open Lwt_result_syntax in
        let* messages = Node_context.get_messages node_ctxt witness in
        let*? hist = Inbox.payloads_history_of_all_messages messages in
        return hist
    end

    module Dal_with_history = struct
      let confirmed_slots_history = dal_slots_history

      let get_history = get_cell_from_hash

      let dal_attestation_lag = dal_attestation_lag

      let dal_parameters = dal_parameters

      let dal_number_of_slots = dal_number_of_slots

      let page_info = page_info

      let dal_activation_level = dal_activation_level

      let dal_attested_slots_validity_lag = dal_attested_slots_validity_lag
    end
  end in
  let metadata = metadata node_ctxt in
  let*! start_tick =
    PVM.Mutable_state.get_tick (PVM.Ctxt_wrapper.of_node_pvmstate start_state)
  in
  let is_reveal_enabled =
    match constants.sc_rollup.reveal_activation_level with
    | Some reveal_activation_level ->
        Sc_rollup.is_reveal_enabled_predicate
          (Sc_rollup_proto_types.Constants.reveal_activation_level_of_octez
             reveal_activation_level)
    | None ->
        (* Constants for an older protocol, there is no notion of reveal
           activation level for those. *)
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/6247
           default value for is_reveal_enabled that returns true for all reveal
           supported in protocols <= 18. *)
        fun ~current_block_level:_ _ -> true
  in
  let* proof =
    trace
      (Sc_rollup_node_errors.Cannot_produce_proof
         {
           inbox_level = game.inbox_level;
           start_tick = Sc_rollup.Tick.to_z start_tick;
         })
    @@ let*! result =
         Sc_rollup.Proof.produce
           ~metadata
           (module P)
           (Raw_level.of_int32_exn game.inbox_level)
           ~is_reveal_enabled
       in
       Lwt.return @@ Environment.wrap_tzresult result
  in
  let*? pvm_step =
    Sc_rollup.Proof.unserialize_pvm_step ~pvm:(module PVM) proof.pvm_step
    |> Environment.wrap_tzresult
  in
  let unserialized_proof = {proof with pvm_step} in
  let*! result =
    Sc_rollup.Proof.valid
      ~metadata
      snapshot
      (Raw_level.of_int32_exn game.inbox_level)
      dal_slots_history
      dal_parameters
      ~dal_activation_level
      ~dal_attestation_lag
      ~dal_number_of_slots
      ~pvm:(module PVM)
      unserialized_proof
      ~is_reveal_enabled
      ~dal_attested_slots_validity_lag
  in
  let res = Environment.wrap_tzresult result in
  assert (Result.is_ok res) ;
  let proof =
    Data_encoding.Binary.to_string_exn Sc_rollup.Proof.encoding proof
  in
  return proof

let make_dissection plugin (node_ctxt : _ Node_context.t) state_cache
    ~start_state ~start_chunk ~our_stop_chunk ~default_number_of_sections
    ~commitment_period_tick_offset ~last_level =
  let open Lwt_result_syntax in
  let module PVM = (val Pvm.of_kind node_ctxt.kind) in
  let state_of_tick ?start_state tick =
    Interpreter.state_of_tick
      plugin
      node_ctxt
      state_cache
      ?start_state
      ~tick:(Z.add (Sc_rollup.Tick.to_z tick) commitment_period_tick_offset)
      last_level
  in
  let state_hash_of_eval_state s = s.Pvm_plugin_sig.info.state_hash in
  let start_chunk =
    Sc_rollup_proto_types.Game.dissection_chunk_of_octez start_chunk
  in
  let our_stop_chunk =
    Sc_rollup_proto_types.Game.dissection_chunk_of_octez our_stop_chunk
  in
  let+ dissection =
    Game_helpers.make_dissection
      ~state_of_tick
      ~state_hash_of_eval_state
      ?start_state
      ~start_chunk
      ~our_stop_chunk
    @@ PVM.new_dissection
         ~start_chunk
         ~our_stop_chunk
         ~default_number_of_sections
  in
  List.map Sc_rollup_proto_types.Game.dissection_chunk_to_octez dissection

let timeout_reached node_ctxt ~self ~opponent =
  let open Lwt_result_syntax in
  let Node_context.{config; cctxt; _} = node_ctxt in
  let*? self = Signature.Of_V_latest.get_public_key_hash self in
  let*? opponent = Signature.Of_V_latest.get_public_key_hash opponent in

  let+ game_result =
    Plugin.RPC.Sc_rollup.timeout_reached
      (new Protocol_client_context.wrap_full cctxt)
      (cctxt#chain, `Head 0)
      config.sc_rollup_address
      self
      opponent
  in
  let open Sc_rollup.Game in
  match game_result with
  | Some (Loser {loser; _}) ->
      let is_it_me = Signature.Public_key_hash.(self = loser) in
      not is_it_me
  | _ -> false

let timeout node_ctxt ~self ~opponent =
  let open Lwt_result_syntax in
  let Node_context.{config; cctxt; _} = node_ctxt in
  let*? self = Signature.Of_V_latest.get_public_key_hash self in
  let*? opponent = Signature.Of_V_latest.get_public_key_hash opponent in

  let+ timeout =
    Plugin.RPC.Sc_rollup.timeout
      (new Protocol_client_context.wrap_full cctxt)
      (cctxt#chain, `Head 0)
      config.sc_rollup_address
      self
      opponent
  in
  Option.map Sc_rollup_proto_types.Game.timeout_to_octez timeout

let get_conflicts cctxt rollup staker =
  let open Lwt_result_syntax in
  let cctxt = new Protocol_client_context.wrap_full cctxt in
  let*? staker = Signature.Of_V_latest.get_public_key_hash staker in

  let+ conflicts =
    Plugin.RPC.Sc_rollup.conflicts cctxt (cctxt#chain, `Head 0) rollup staker
  in
  List.map Sc_rollup_proto_types.Game.conflict_to_octez conflicts

let get_ongoing_games cctxt rollup staker =
  let open Lwt_result_syntax in
  let cctxt = new Protocol_client_context.wrap_full cctxt in
  let*? staker = Signature.Of_V_latest.get_public_key_hash staker in
  let+ games =
    Plugin.RPC.Sc_rollup.ongoing_refutation_games
      cctxt
      (cctxt#chain, `Head 0)
      rollup
      staker
  in
  List.map
    (fun (game, staker1, staker2) ->
      ( Sc_rollup_proto_types.Game.to_octez game,
        Tezos_crypto.Signature.Of_V1.public_key_hash staker1,
        Tezos_crypto.Signature.Of_V1.public_key_hash staker2 ))
    games
