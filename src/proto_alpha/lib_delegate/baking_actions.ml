(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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
open Baking_state
module Events = Baking_events.Actions

module Operations_source = struct
  type error +=
    | Failed_operations_fetch of {
        path : string;
        reason : string;
        details : Data_encoding.json option;
      }

  let operations_encoding =
    Data_encoding.(list (dynamic_size Operation.encoding))

  let retrieve =
    let open Lwt_result_syntax in
    function
    | None -> Lwt.return_none
    | Some operations -> (
        let fail reason details =
          let path =
            match operations with
            | Baking_configuration.Operations_source.Local {filename} ->
                filename
            | Baking_configuration.Operations_source.Remote {uri; _} ->
                Uri.to_string uri
          in
          tzfail (Failed_operations_fetch {path; reason; details})
        in
        let decode_operations json =
          protect
            ~on_error:(fun _ ->
              fail "cannot decode the received JSON into operations" (Some json))
            (fun () ->
              return (Data_encoding.Json.destruct operations_encoding json))
        in
        match operations with
        | Baking_configuration.Operations_source.Local {filename} ->
            if Sys.file_exists filename then
              let*! result =
                Tezos_stdlib_unix.Lwt_utils_unix.Json.read_file filename
              in
              match result with
              | Error _ ->
                  let*! () = Events.(emit invalid_json_file filename) in
                  Lwt.return_none
              | Ok json -> (
                  let*! operations = decode_operations json in
                  match operations with
                  | Ok operations -> Lwt.return_some operations
                  | Error errs ->
                      let*! () = Events.(emit cannot_fetch_operations errs) in
                      Lwt.return_none)
            else
              let*! () = Events.(emit no_operations_found_in_file filename) in
              Lwt.return_none
        | Baking_configuration.Operations_source.Remote {uri; http_headers} -> (
            let*! operations_opt =
              let* result =
                with_timeout
                  (Systime_os.sleep (Time.System.Span.of_seconds_exn 5.))
                  (fun _ ->
                    Tezos_rpc_http_client_unix.RPC_client_unix
                    .generic_media_type_call
                      ~accept:[Media_type.json]
                      ?headers:http_headers
                      `GET
                      uri)
              in
              let* rest =
                match result with
                | `Json json -> return json
                | _ -> fail "json not returned" None
              in
              let* json =
                match rest with
                | `Ok json -> return json
                | `Unauthorized json -> fail "unauthorized request" json
                | `Gone json -> fail "gone" json
                | `Error json -> fail "error" json
                | `Not_found json -> fail "not found" json
                | `Forbidden json -> fail "forbidden" json
                | `Conflict json -> fail "conflict" json
              in
              decode_operations json
            in
            match operations_opt with
            | Ok operations -> Lwt.return_some operations
            | Error errs ->
                let*! () = Events.(emit cannot_fetch_operations errs) in
                Lwt.return_none))
end

type action =
  | Do_nothing
  | Prepare_block of {block_to_bake : block_to_bake}
  | Prepare_preattestations of {preattestations : unsigned_consensus_vote_batch}
  | Prepare_attestations of {attestations : unsigned_consensus_vote_batch}
  | Prepare_consensus_votes of {
      preattestations : unsigned_consensus_vote_batch;
      attestations : unsigned_consensus_vote_batch;
    }
  | Inject_block of {
      prepared_block : prepared_block;
      force_injection : bool;
      asynchronous : bool;
    }
  | Inject_preattestation of {signed_preattestation : signed_consensus_vote}
  | Inject_attestation of {signed_attestation : signed_consensus_vote}
  | Inject_attestations of {signed_attestations : signed_consensus_vote_batch}
  | Update_to_level of level_update
  | Synchronize_round of round_update
  | Watch_prequorum
  | Watch_quorum

and level_update = {
  new_level_proposal : proposal;
  compute_new_state :
    current_round:Round.t ->
    delegate_slots:delegate_slots ->
    next_level_delegate_slots:delegate_slots ->
    (state * action) Lwt.t;
}

and round_update = {
  new_round_proposal : proposal;
  handle_proposal : state -> (state * action) Lwt.t;
}

type t = action

let pp_action fmt = function
  | Do_nothing -> Format.fprintf fmt "do nothing"
  | Prepare_block _ -> Format.fprintf fmt "prepare block"
  | Prepare_preattestations _ -> Format.fprintf fmt "prepare preattestations"
  | Prepare_attestations _ -> Format.fprintf fmt "prepare attestations"
  | Prepare_consensus_votes _ -> Format.fprintf fmt "prepare consensus votes"
  | Inject_block _ -> Format.fprintf fmt "inject block"
  | Inject_preattestation _ -> Format.fprintf fmt "inject preattestation"
  | Inject_attestation _ -> Format.fprintf fmt "inject attestation"
  | Inject_attestations _ -> Format.fprintf fmt "inject multiple attestations"
  | Update_to_level _ -> Format.fprintf fmt "update to level"
  | Synchronize_round _ -> Format.fprintf fmt "synchronize round"
  | Watch_prequorum -> Format.fprintf fmt "watch prequorum"
  | Watch_quorum -> Format.fprintf fmt "watch quorum"

let generate_seed_nonce_hash config delegate level =
  let open Lwt_result_syntax in
  if level.Level.expected_commitment then
    let* seed_nonce =
      Baking_nonces.generate_seed_nonce config delegate level.level
    in
    return_some seed_nonce
  else return_none

let sign_block_header global_state proposer unsigned_block_header =
  let open Lwt_result_syntax in
  let cctxt = global_state.cctxt in
  let chain_id = global_state.chain_id in
  let force = global_state.config.force in
  let {Block_header.shell; protocol_data = {contents; _}} =
    unsigned_block_header
  in
  let unsigned_header =
    Data_encoding.Binary.to_bytes_exn
      Alpha_context.Block_header.unsigned_encoding
      (shell, contents)
  in
  let level = shell.level in
  let*? round = Baking_state.round_of_shell_header shell in
  let open Baking_highwatermarks in
  let* result =
    cctxt#with_lock (fun () ->
        let block_location =
          Baking_files.resolve_location ~chain_id `Highwatermarks
        in
        let* may_sign =
          may_sign_block
            cctxt
            block_location
            ~delegate:proposer.public_key_hash
            ~level
            ~round
        in
        match may_sign with
        | true ->
            let* () =
              record_block
                cctxt
                block_location
                ~delegate:proposer.public_key_hash
                ~level
                ~round
            in
            return_true
        | false ->
            let*! () = Events.(emit potential_double_baking (level, round)) in
            return force)
  in
  match result with
  | false -> tzfail (Block_previously_baked {level; round})
  | true ->
      let* signature =
        Client_keys.sign
          cctxt
          proposer.secret_key_uri
          ~watermark:Block_header.(to_watermark (Block_header chain_id))
          unsigned_header
      in
      return {Block_header.shell; protocol_data = {contents; signature}}

let prepare_block (global_state : global_state) (block_to_bake : block_to_bake)
    =
  let open Lwt_result_syntax in
  let {
    predecessor;
    round;
    delegate = (consensus_key, _) as delegate;
    kind;
    force_apply;
  } =
    block_to_bake
  in
  let*! () =
    Events.(
      emit
        prepare_forging_block
        (Int32.succ predecessor.shell.level, round, delegate))
  in
  let cctxt = global_state.cctxt in
  let chain_id = global_state.chain_id in
  let simulation_mode = global_state.validation_mode in
  let round_durations = global_state.round_durations in
  let*? timestamp =
    Environment.wrap_tzresult
      (Round.timestamp_of_round
         round_durations
         ~predecessor_timestamp:predecessor.shell.timestamp
         ~predecessor_round:predecessor.round
         ~round)
  in
  let external_operation_source = global_state.config.extra_operations in
  let*! extern_ops = Operations_source.retrieve external_operation_source in
  let simulation_kind, payload_round =
    match kind with
    | Fresh pool ->
        let pool =
          let node_pool = Operation_pool.Prioritized.of_pool pool in
          match extern_ops with
          | None -> node_pool
          | Some ops ->
              Operation_pool.Prioritized.merge_external_operations node_pool ops
        in
        (Block_forge.Filter pool, round)
    | Reproposal {consensus_operations; payload_hash; payload_round; payload} ->
        ( Block_forge.Apply
            {
              ordered_pool =
                Operation_pool.ordered_pool_of_payload
                  ~consensus_operations
                  payload;
              payload_hash;
            },
          payload_round )
  in
  let*! () =
    Events.(
      emit forging_block (Int32.succ predecessor.shell.level, round, delegate))
  in
  let* injection_level =
    Plugin.RPC.current_level
      cctxt
      ~offset:1l
      (`Hash global_state.chain_id, `Hash (predecessor.hash, 0))
  in
  let* seed_nonce_opt =
    generate_seed_nonce_hash
      global_state.config.Baking_configuration.nonce
      consensus_key
      injection_level
  in
  let seed_nonce_hash = Option.map fst seed_nonce_opt in
  let user_activated_upgrades = global_state.config.user_activated_upgrades in
  (* Set liquidity_baking_toggle_vote for this block *)
  let {
    Baking_configuration.vote_file;
    liquidity_baking_vote;
    adaptive_issuance_vote;
  } =
    global_state.config.per_block_votes
  in
  (* Prioritize reading from the [vote_file] if it exists. *)
  let*! {liquidity_baking_vote; adaptive_issuance_vote} =
    let default =
      Protocol.Alpha_context.Per_block_votes.
        {liquidity_baking_vote; adaptive_issuance_vote}
    in
    match vote_file with
    | Some per_block_vote_file ->
        Per_block_vote_file.read_per_block_votes_no_fail
          ~default
          ~per_block_vote_file
    | None -> Lwt.return default
  in
  let*! () =
    Events.(emit vote_for_liquidity_baking_toggle) liquidity_baking_vote
  in
  let*! () = Events.(emit vote_for_adaptive_issuance) adaptive_issuance_vote in
  let chain = `Hash global_state.chain_id in
  let pred_block = `Hash (predecessor.hash, 0) in
  let* pred_resulting_context_hash =
    Shell_services.Blocks.resulting_context_hash
      cctxt
      ~chain
      ~block:pred_block
      ()
  in
  let* pred_live_blocks =
    Chain_services.Blocks.live_blocks cctxt ~chain ~block:pred_block ()
  in
  let* {unsigned_block_header; operations} =
    Block_forge.forge
      cctxt
      ~chain_id
      ~pred_info:predecessor
      ~pred_live_blocks
      ~pred_resulting_context_hash
      ~timestamp
      ~round
      ~seed_nonce_hash
      ~payload_round
      ~liquidity_baking_toggle_vote:liquidity_baking_vote
      ~adaptive_issuance_vote
      ~user_activated_upgrades
      ~force_apply
      global_state.config.fees
      simulation_mode
      simulation_kind
      global_state.constants.parametric
  in
  let* signed_block_header =
    sign_block_header global_state consensus_key unsigned_block_header
  in
  let* () =
    match seed_nonce_opt with
    | None ->
        (* Nothing to do *)
        return_unit
    | Some (_, nonce) ->
        let block_hash = Block_header.hash signed_block_header in
        Baking_nonces.register_nonce cctxt ~chain_id block_hash nonce
  in
  let baking_votes =
    {Per_block_votes.liquidity_baking_vote; adaptive_issuance_vote}
  in
  return {signed_block_header; round; delegate; operations; baking_votes}

let only_if_dal_feature_enabled =
  let no_dal_node_warning_counter = ref 0 in
  fun state ~default_value f ->
    let open Lwt_result_syntax in
    let open Constants in
    let Parametric.{dal = {feature_enable; _}; _} =
      state.global_state.constants.parametric
    in
    if feature_enable then
      match state.global_state.dal_node_rpc_ctxt with
      | None ->
          incr no_dal_node_warning_counter ;
          let*! () =
            if !no_dal_node_warning_counter mod 10 = 1 then
              Events.(emit no_dal_node ())
            else Lwt.return_unit
          in
          return default_value
      | Some ctxt -> f ctxt
    else return default_value

let may_get_dal_content state consensus_vote =
  let open Lwt_result_syntax in
  let {delegate = (consensus_key, _) as delegate; vote_consensus_content; _} =
    consensus_vote
  in
  let level, round =
    ( Raw_level.to_int32 vote_consensus_content.level,
      vote_consensus_content.round )
  in
  let compute_dal_rpc_timeout state =
    (* We set the timeout to a certain percent of the remaining time till the
       end of the round. *)
    let default = 0.2 in
    match compute_next_round_time state with
    | None -> (* corner case; we pick some small default value *) default
    | Some (timestamp, _next_round) -> (
        let ts = Time.System.of_protocol_opt timestamp in
        match ts with
        | None -> default
        | Some ts ->
            let now = Time.System.now () in
            let diff = Ptime.diff ts now |> Ptime.Span.to_float_s in
            if diff < 0. then
              (* We could output a warning, as this should not happen. *)
              default
            else diff *. 0.1)
  in
  only_if_dal_feature_enabled
    state
    ~default_value:None
    (fun dal_node_rpc_ctxt ->
      let timeout = compute_dal_rpc_timeout state in
      let*! result =
        Lwt.pick
          [
            (let*! () = Lwt_unix.sleep timeout in
             Lwt.return `RPC_timeout);
            (let*! tz_res =
               Node_rpc.get_attestable_slots
                 dal_node_rpc_ctxt
                 consensus_key.public_key_hash
                 ~attested_level:(Int32.succ level)
             in
             Lwt.return (`RPC_result tz_res));
          ]
      in
      match result with
      | `RPC_timeout ->
          let*! () =
            Events.(
              emit failed_to_get_dal_attestations_in_time (delegate, timeout))
          in
          return_none
      | `RPC_result (Error errs) ->
          let*! () =
            Events.(emit failed_to_get_dal_attestations (delegate, errs))
          in
          return_none
      | `RPC_result (Ok res) -> (
          match res with
          | Tezos_dal_node_services.Types.Not_in_committee -> return_none
          | Attestable_slots {slots = attestation_flags; published_level} ->
              let number_of_slots =
                state.global_state.constants.parametric.dal.number_of_slots
              in
              let dal_attestation =
                List.fold_left_i
                  (fun i acc flag ->
                    match Dal.Slot_index.of_int_opt ~number_of_slots i with
                    | Some index when flag -> Dal.Attestation.commit acc index
                    | None | Some _ -> acc)
                  Dal.Attestation.empty
                  attestation_flags
              in
              let*! () =
                let bitset_int = Bitset.to_z (dal_attestation :> Bitset.t) in
                Events.(
                  emit
                    attach_dal_attestation
                    (delegate, bitset_int, published_level, level, round))
              in
              return_some {attestation = dal_attestation}))

let is_authorized (global_state : global_state) highwatermarks consensus_vote =
  let {delegate = consensus_key, _; vote_consensus_content; _} =
    consensus_vote
  in
  let level, round =
    ( Raw_level.to_int32 vote_consensus_content.level,
      vote_consensus_content.round )
  in
  let may_sign =
    match consensus_vote.vote_kind with
    | Preattestation ->
        Baking_highwatermarks.may_sign_preattestation
          highwatermarks
          ~delegate:consensus_key.public_key_hash
          ~level
          ~round
    | Attestation ->
        Baking_highwatermarks.may_sign_attestation
          highwatermarks
          ~delegate:consensus_key.public_key_hash
          ~level
          ~round
  in
  may_sign || global_state.config.force

let authorized_consensus_votes global_state
    (unsigned_consensus_vote_batch : unsigned_consensus_vote_batch) =
  let open Lwt_result_syntax in
  (* Hypothesis: all consensus votes have the same round and level *)
  let {
    batch_kind;
    batch_content = ({level; round; _} : batch_content);
    batch_branch = _;
    unsigned_consensus_votes;
  } =
    unsigned_consensus_vote_batch
  in
  let level = Raw_level.to_int32 level in
  let cctxt = global_state.cctxt in
  let chain_id = global_state.chain_id in
  let block_location =
    Baking_files.resolve_location ~chain_id `Highwatermarks
  in
  (* Filter all operations that don't satisfy the highwatermark and
     record the ones that do. *)
  let* authorized_votes, unauthorized_votes =
    cctxt#with_lock (fun () ->
        let* highwatermarks = Baking_highwatermarks.load cctxt block_location in
        let authorized_votes, unauthorized_votes =
          List.partition
            (fun consensus_vote ->
              is_authorized global_state highwatermarks consensus_vote)
            unsigned_consensus_votes
        in
        (* Record all consensus votes new highwatermarks as one batch *)
        let delegates =
          List.map
            (fun {delegate = ck, _; _} -> ck.public_key_hash)
            authorized_votes
        in
        let record_all_consensus_vote =
          match batch_kind with
          | Preattestation -> Baking_highwatermarks.record_all_preattestations
          | Attestation -> Baking_highwatermarks.record_all_attestations
        in
        (* We exit the client's lock as soon as this function returns *)
        let* () =
          record_all_consensus_vote
            highwatermarks
            cctxt
            block_location
            ~delegates
            ~level
            ~round
        in
        return (authorized_votes, unauthorized_votes))
  in
  let*! () =
    List.iter_s
      (fun {vote_kind; delegate; _} ->
        let error =
          match vote_kind with
          | Preattestation ->
              Baking_highwatermarks.Block_previously_preattested {round; level}
          | Attestation ->
              Baking_highwatermarks.Block_previously_attested {round; level}
        in
        Events.(
          emit
            skipping_consensus_vote
            (vote_kind, delegate, level, round, [error])))
      unauthorized_votes
  in
  return authorized_votes

let forge_and_sign_consensus_vote global_state ~branch unsigned_consensus_vote :
    signed_consensus_vote tzresult Lwt.t =
  let open Lwt_result_syntax in
  let cctxt = global_state.cctxt in
  let chain_id = global_state.chain_id in
  let {vote_kind; vote_consensus_content; delegate = ck, _; dal_content} =
    unsigned_consensus_vote
  in
  let shell = {Tezos_base.Operation.branch} in
  let watermark =
    match vote_kind with
    | Preattestation -> Operation.(to_watermark (Preattestation chain_id))
    | Attestation -> Operation.(to_watermark (Attestation chain_id))
  in
  let (Contents_list contents) =
    match vote_kind with
    | Preattestation ->
        Contents_list (Single (Preattestation vote_consensus_content))
    | Attestation ->
        Contents_list
          (Single
             (Attestation
                {consensus_content = vote_consensus_content; dal_content}))
  in
  let unsigned_operation = (shell, Contents_list contents) in
  let unsigned_operation_bytes =
    Data_encoding.Binary.to_bytes_exn
      Operation.unsigned_encoding
      unsigned_operation
  in
  let sk_uri = ck.secret_key_uri in
  let* signature =
    Client_keys.sign cctxt ~watermark sk_uri unsigned_operation_bytes
  in
  let protocol_data = Operation_data {contents; signature = Some signature} in
  let signed_operation : Operation.packed = {shell; protocol_data} in
  return {unsigned_consensus_vote; signed_operation}

let sign_consensus_votes (global_state : global_state)
    ({batch_kind; batch_content; batch_branch; _} as
     unsigned_consensus_vote_batch :
      unsigned_consensus_vote_batch) =
  let open Lwt_result_syntax in
  let* authorized_consensus_votes =
    authorized_consensus_votes global_state unsigned_consensus_vote_batch
  in
  let* signed_consensus_votes =
    List.filter_map_es
      (fun ({delegate; vote_kind; vote_consensus_content; _} as
           unsigned_consensus_vote) ->
        let*! () = Events.(emit signing_consensus_vote (vote_kind, delegate)) in
        let*! signed_consensus_vote_r =
          forge_and_sign_consensus_vote
            global_state
            ~branch:batch_branch
            unsigned_consensus_vote
        in
        match signed_consensus_vote_r with
        | Error err ->
            let level, round =
              ( Raw_level.to_int32 vote_consensus_content.level,
                vote_consensus_content.round )
            in
            let*! () =
              Events.(
                emit
                  skipping_consensus_vote
                  (vote_kind, delegate, level, round, err))
            in
            return_none
        | Ok signed_consensus_vote -> return_some signed_consensus_vote)
      authorized_consensus_votes
  in
  let*? signed_consensus_vote_batch =
    make_signed_consensus_vote_batch
      batch_kind
      batch_content
      ~batch_branch
      signed_consensus_votes
  in
  return signed_consensus_vote_batch

let inject_consensus_vote state (signed_consensus_vote : signed_consensus_vote)
    =
  let open Lwt_result_syntax in
  let cctxt = state.global_state.cctxt in
  let chain_id = state.global_state.chain_id in
  let unsigned_consensus_vote = signed_consensus_vote.unsigned_consensus_vote in
  let delegate = unsigned_consensus_vote.delegate in
  let vote_consensus_content = unsigned_consensus_vote.vote_consensus_content in
  let level, round =
    ( Raw_level.to_int32 vote_consensus_content.level,
      vote_consensus_content.round )
  in
  protect
    ~on_error:(fun err ->
      let*! () =
        Events.(
          emit
            failed_to_inject_consensus_vote
            (unsigned_consensus_vote.vote_kind, delegate, err))
      in
      return_unit)
    (fun () ->
      let* oph =
        Node_rpc.inject_operation
          cctxt
          ~chain:(`Hash chain_id)
          signed_consensus_vote.signed_operation
      in
      let*! () =
        Events.(
          emit
            consensus_vote_injected
            (unsigned_consensus_vote.vote_kind, oph, delegate, level, round))
      in
      return_unit)

let inject_consensus_votes state signed_consensus_vote_batch =
  List.iter_ep
    (inject_consensus_vote state)
    signed_consensus_vote_batch.signed_consensus_votes

let inject_block ?(force_injection = false) ?(asynchronous = true) state
    prepared_block =
  let open Lwt_result_syntax in
  let {signed_block_header; round; delegate; operations; baking_votes} =
    prepared_block
  in
  (* Cache last per-block votes to use in case of vote file errors *)
  let new_state =
    {
      state with
      global_state =
        {
          state.global_state with
          config =
            {
              state.global_state.config with
              per_block_votes =
                {
                  state.global_state.config.per_block_votes with
                  liquidity_baking_vote = baking_votes.liquidity_baking_vote;
                  adaptive_issuance_vote = baking_votes.adaptive_issuance_vote;
                };
            };
        };
    }
  in
  let inject_block () =
    let*! () =
      Events.(
        emit injecting_block (signed_block_header.shell.level, round, delegate))
    in
    let* bh =
      Node_rpc.inject_block
        state.global_state.cctxt
        ~force:state.global_state.config.force
        ~chain:(`Hash state.global_state.chain_id)
        signed_block_header
        operations
    in
    let*! () =
      Events.(
        emit
          block_injected
          (bh, signed_block_header.shell.level, round, delegate))
    in
    return_unit
  in
  let now = Time.System.now () in
  let block_time =
    Time.System.of_protocol_exn signed_block_header.shell.timestamp
  in
  (* Blocks might be ready before their actual timestamp: when this
     happens, we wait asynchronously until our clock reaches the
     block's timestamp before injecting. *)
  let* () =
    let delay = Ptime.diff block_time now in
    if Ptime.Span.(compare delay zero < 0) || force_injection then
      inject_block ()
    else
      let*! () =
        Events.(
          emit
            delayed_block_injection
            (delay, signed_block_header.shell.level, round, delegate))
      in
      let t =
        let*! _ =
          protect
            ~on_error:(fun err ->
              let*! () =
                Events.(
                  emit
                    block_injection_failed
                    (Block_header.hash signed_block_header, err))
              in
              return_unit)
            (fun () ->
              let*! () = Lwt_unix.sleep (Ptime.Span.to_float_s delay) in
              inject_block ())
        in
        Lwt.return_unit
      in
      let*! () =
        if asynchronous then (
          Lwt.dont_wait (fun () -> t) (fun _exn -> ()) ;
          Lwt.return_unit)
        else t
      in
      return_unit
  in
  return new_state

let prepare_waiting_for_quorum state =
  let consensus_threshold =
    state.global_state.constants.parametric.consensus_threshold
  in
  let get_slot_voting_power ~slot =
    Delegate_slots.voting_power state.level_state.delegate_slots ~slot
  in
  let latest_proposal = state.level_state.latest_proposal.block in
  (* assert (latest_proposal.block.round = state.round_state.current_round) ; *)
  let candidate =
    {
      Operation_worker.hash = latest_proposal.hash;
      round_watched = latest_proposal.round;
      payload_hash_watched = latest_proposal.payload_hash;
    }
  in
  (consensus_threshold, get_slot_voting_power, candidate)

let start_waiting_for_preattestation_quorum state =
  let consensus_threshold, get_slot_voting_power, candidate =
    prepare_waiting_for_quorum state
  in
  let operation_worker = state.global_state.operation_worker in
  Operation_worker.monitor_preattestation_quorum
    operation_worker
    ~consensus_threshold
    ~get_slot_voting_power
    candidate

let start_waiting_for_attestation_quorum state =
  let consensus_threshold, get_slot_voting_power, candidate =
    prepare_waiting_for_quorum state
  in
  let operation_worker = state.global_state.operation_worker in
  Operation_worker.monitor_attestation_quorum
    operation_worker
    ~consensus_threshold
    ~get_slot_voting_power
    candidate

let compute_round proposal round_durations =
  let open Protocol in
  let open Baking_state in
  let timestamp = Time.System.now () |> Time.System.to_protocol in
  let predecessor_block = proposal.predecessor in
  Environment.wrap_tzresult
  @@ Alpha_context.Round.round_of_timestamp
       round_durations
       ~predecessor_timestamp:predecessor_block.shell.timestamp
       ~predecessor_round:predecessor_block.round
       ~timestamp

let update_to_level state level_update =
  let open Lwt_result_syntax in
  let {new_level_proposal; compute_new_state} = level_update in
  let cctxt = state.global_state.cctxt in
  let delegates = state.global_state.delegates in
  let new_level = new_level_proposal.block.shell.level in
  let chain = `Hash state.global_state.chain_id in
  (* Sync the context to clean-up potential GC artifacts *)
  let*! () =
    match state.global_state.validation_mode with
    | Node -> Lwt.return_unit
    | Local index -> index.sync_fun ()
  in
  let* delegate_slots =
    if Int32.(new_level = succ state.level_state.current_level) then
      return state.level_state.next_level_delegate_slots
    else
      Baking_state.compute_delegate_slots
        cctxt
        delegates
        ~level:new_level
        ~chain
  in
  let* next_level_delegate_slots =
    Baking_state.compute_delegate_slots
      cctxt
      delegates
      ~level:(Int32.succ new_level)
      ~chain
  in
  let round_durations = state.global_state.round_durations in
  let*? current_round = compute_round new_level_proposal round_durations in
  let*! new_state =
    compute_new_state ~current_round ~delegate_slots ~next_level_delegate_slots
  in
  return new_state

let synchronize_round state {new_round_proposal; handle_proposal} =
  let open Lwt_result_syntax in
  let*! () =
    Events.(emit synchronizing_round new_round_proposal.predecessor.hash)
  in
  let round_durations = state.global_state.round_durations in
  let*? current_round = compute_round new_round_proposal round_durations in
  if Round.(current_round < new_round_proposal.block.round) then
    (* impossible *)
    failwith
      "synchronize_round: current round (%a) is behind the new proposal's \
       round (%a)"
      Round.pp
      current_round
      Round.pp
      new_round_proposal.block.round
  else
    let new_round_state =
      {
        current_round;
        current_phase = Idle;
        delayed_quorum = None;
        early_attestations = [];
        awaiting_unlocking_pqc = false;
      }
    in
    let new_state = {state with round_state = new_round_state} in
    let*! new_state = handle_proposal new_state in
    return new_state

let prepare_block_request state block_to_bake =
  let open Lwt_result_syntax in
  let request = Forge_and_sign_block block_to_bake in
  state.global_state.forge_worker_hooks.push_request request ;
  return state

let prepare_preattestations_request state unsigned_preattestations =
  let open Lwt_result_syntax in
  let request = Forge_and_sign_preattestations {unsigned_preattestations} in
  state.global_state.forge_worker_hooks.push_request request ;
  return state

let prepare_attestations_request state unsigned_attestations =
  let open Lwt_result_syntax in
  let*! unsigned_attestations_with_dal =
    dal_content_map_p (may_get_dal_content state) unsigned_attestations
  in
  let request =
    Forge_and_sign_attestations
      {unsigned_attestations = unsigned_attestations_with_dal}
  in
  state.global_state.forge_worker_hooks.push_request request ;
  return state

(* TODO: https://gitlab.com/tezos/tezos/-/issues/4539
   Avoid updating the state here.
   (See also comment in {!State_transitions.step}.)

   TODO: https://gitlab.com/tezos/tezos/-/issues/4538
   Improve/clarify when the state is recorded.
*)
let rec perform_action state (action : action) =
  let open Lwt_result_syntax in
  match action with
  | Do_nothing -> return state
  | Prepare_block {block_to_bake} -> prepare_block_request state block_to_bake
  | Prepare_preattestations {preattestations} ->
      let* new_state = prepare_preattestations_request state preattestations in
      (* We wait for preattestations to trigger the [Prequorum_reached]
         event *)
      perform_action new_state Watch_prequorum
  | Prepare_attestations {attestations} ->
      let* new_state = prepare_attestations_request state attestations in
      (* We wait for attestations to trigger the [Quorum_reached]
         event *)
      perform_action new_state Watch_quorum
  | Prepare_consensus_votes {preattestations; attestations} ->
      let* state = prepare_preattestations_request state preattestations in
      let* state = prepare_attestations_request state attestations in
      (* We wait for preattestations to trigger the [Prequorum_reached]
         event *)
      perform_action state Watch_prequorum
  | Inject_block {prepared_block; force_injection; asynchronous} ->
      let* new_state =
        inject_block ~force_injection ~asynchronous state prepared_block
      in
      return new_state
  | Inject_preattestation {signed_preattestation} ->
      let* () = inject_consensus_vote state signed_preattestation in
      (* Here, we do not need to wait for the prequorum, it has
         already been triggered by the
         [Prepare_(preattestation|consensus_votes)] action *)
      return state
  | Inject_attestation {signed_attestation} ->
      let* () = inject_consensus_vote state signed_attestation in
      (* We wait for attestations to trigger the [Quorum_reached]
         event *)
      perform_action state Watch_quorum
  | Inject_attestations {signed_attestations} ->
      let* () = inject_consensus_votes state signed_attestations in
      (* We wait for attestations to trigger the [Quorum_reached]
         event *)
      perform_action state Watch_quorum
  | Update_to_level level_update ->
      let* new_state, new_action = update_to_level state level_update in
      perform_action new_state new_action
  | Synchronize_round round_update ->
      let* new_state, new_action = synchronize_round state round_update in
      perform_action new_state new_action
  | Watch_prequorum ->
      let*! () = start_waiting_for_preattestation_quorum state in
      return state
  | Watch_quorum ->
      let*! () = start_waiting_for_attestation_quorum state in
      return state
