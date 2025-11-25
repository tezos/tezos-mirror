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
open Baking_state_types
module Events = Baking_events.Actions

module Profiler = (val Profiler.wrap Baking_profiler.baker_profiler)

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
  | Inject_attestations of {signed_attestations : signed_consensus_vote_batch}
  | Update_to_level of level_update
  | Synchronize_round of round_update
  | Watch_prequorum
  | Watch_quorum

and level_update = {
  new_level_proposal : proposal;
  compute_new_state :
    current_round:Round.t ->
    delegate_infos:delegate_infos ->
    next_level_delegate_infos:delegate_infos ->
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
  | Inject_attestations _ -> Format.fprintf fmt "inject multiple attestations"
  | Update_to_level _ -> Format.fprintf fmt "update to level"
  | Synchronize_round _ -> Format.fprintf fmt "synchronize round"
  | Watch_prequorum -> Format.fprintf fmt "watch prequorum"
  | Watch_quorum -> Format.fprintf fmt "watch quorum"

let generate_seed_nonce_hash ?timeout config delegate level =
  let open Lwt_result_syntax in
  if level.Level.expected_commitment then
    let* seed_nonce =
      (Baking_nonces.generate_seed_nonce
         ?timeout
         config
         delegate
         level.level
       [@profiler.record_s {verbosity = Debug} "generate seed nonce"])
    in
    return_some seed_nonce
  else return_none

let round_of_shell_header shell_header =
  let open Result_syntax in
  let* fitness =
    Environment.wrap_tzresult
    @@ Fitness.from_raw shell_header.Tezos_base.Block_header.fitness
  in
  return (Fitness.round fitness)

let sign ?timeout ?watermark ~signing_request cctxt secret_key_uri msg =
  let open Lwt_result_syntax in
  let*! result =
    let sign () =
      match timeout with
      | None ->
          let*! res = Client_keys.sign cctxt secret_key_uri ?watermark msg in
          Lwt.return (`Signature_result res)
      | Some timeout ->
          Lwt.pick
            [
              (let*! () = Lwt_unix.sleep timeout in
               Lwt.return (`Signature_timeout timeout));
              (let*! signature =
                 Client_keys.sign cctxt secret_key_uri ?watermark msg
               in
               Lwt.return (`Signature_result signature));
            ]
    in
    Octez_baking_common.Signing_delay.sign_with_minimum_duration
      (module Profiler)
      sign
  in
  match result with
  | `Signature_timeout timeout ->
      let*! () = Events.(emit signature_timeout timeout) in
      tzfail (Baking_errors.Signature_timeout (timeout, signing_request))
  | `Signature_result (Error errs) ->
      let*! () = Events.(emit signature_error errs) in
      Lwt.return (Error errs)
  | `Signature_result (Ok res) -> Lwt.return (Ok res)

let sign_block_header round_duration global_state proposer unsigned_block_header
    =
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
  let*? round = round_of_shell_header shell in
  let open Baking_highwatermarks in
  let* result =
    cctxt#with_lock (fun () ->
        let block_location =
          Baking_files.resolve_location ~chain_id `Highwatermarks
        in
        let* may_sign =
          (may_sign_block
             cctxt
             block_location
             ~delegate:proposer.Key.id
             ~level
             ~round [@profiler.record_s {verbosity = Debug} "may sign"])
        in
        match may_sign with
        | true ->
            let* () =
              (record_block
                 cctxt
                 block_location
                 ~delegate:proposer.id
                 ~level
                 ~round [@profiler.record_s {verbosity = Debug} "record block"])
            in
            return_true
        | false ->
            let*! () = Events.(emit potential_double_baking (level, round)) in
            return force)
  in
  match result with
  | false -> tzfail (Block_previously_baked {level; round})
  | true ->
      let delay_between_stall_events =
        (* round_duration /. 2 is arbitrary and conservative *)
        round_duration /. 2.
      in
      let* signature =
        Utils.event_on_stalling_promise
          ~initial_delay:delay_between_stall_events
          ~event:(fun sum ->
            Events.(emit stalling_signature (`Block_header, level, round, sum)))
          (sign
             ?timeout:global_state.config.remote_calls_timeout
             ~signing_request:`Block_header
             cctxt
             proposer.secret_key_uri
             ~watermark:Block_header.(to_watermark (Block_header chain_id))
             unsigned_header
           [@profiler.record_s {verbosity = Debug} "sign : block header"])
      in
      return {Block_header.shell; protocol_data = {contents; signature}}

let prepare_block (global_state : global_state) (block_to_bake : block_to_bake)
    =
  let open Lwt_result_syntax in
  let {predecessor; round; delegate; kind; force_apply} = block_to_bake in
  let level = Int32.succ predecessor.shell.level in
  let*! () = Events.(emit prepare_forging_block (level, round, delegate)) in
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
    Events.(emit forging_block (level, round, delegate, force_apply))
  in
  let* injection_level =
    Node_rpc.current_level
      cctxt
      ~offset:1l
      ~chain:(`Hash global_state.chain_id)
      ~block:(`Hash (predecessor.hash, 0))
      ()
  in
  let* seed_nonce_opt =
    (generate_seed_nonce_hash
       ?timeout:global_state.config.remote_calls_timeout
       global_state.config.Baking_configuration.nonce
       delegate.consensus_key
       injection_level
     [@profiler.record_s {verbosity = Info} "generate seed nonce hash"])
  in
  let seed_nonce_hash = Option.map fst seed_nonce_opt in
  let user_activated_upgrades = global_state.config.user_activated_upgrades in
  (* Set liquidity_baking_toggle_vote for this block *)
  let {Baking_configuration.vote_file; liquidity_baking_vote} =
    global_state.config.per_block_votes
  in
  (* Prioritize reading from the [vote_file] if it exists. *)
  let*! {liquidity_baking_vote} =
    let of_protocol = function
      | Protocol.Alpha_context.Per_block_votes.Per_block_vote_on ->
          Octez_agnostic_baker.Per_block_votes.Per_block_vote_on
      | Protocol.Alpha_context.Per_block_votes.Per_block_vote_off ->
          Octez_agnostic_baker.Per_block_votes.Per_block_vote_off
      | Protocol.Alpha_context.Per_block_votes.Per_block_vote_pass ->
          Octez_agnostic_baker.Per_block_votes.Per_block_vote_pass
    in
    let to_protocol = function
      | Octez_agnostic_baker.Per_block_votes.Per_block_vote_on ->
          Protocol.Alpha_context.Per_block_votes.Per_block_vote_on
      | Octez_agnostic_baker.Per_block_votes.Per_block_vote_off ->
          Protocol.Alpha_context.Per_block_votes.Per_block_vote_off
      | Octez_agnostic_baker.Per_block_votes.Per_block_vote_pass ->
          Protocol.Alpha_context.Per_block_votes.Per_block_vote_pass
    in
    let default =
      Protocol.Alpha_context.Per_block_votes.{liquidity_baking_vote}
    in
    match vote_file with
    | Some per_block_vote_file ->
        let default =
          Octez_agnostic_baker.Per_block_votes.
            {liquidity_baking_vote = of_protocol liquidity_baking_vote}
        in
        let*! Octez_agnostic_baker.Per_block_votes.{liquidity_baking_vote} =
          (Per_block_vote_file.read_per_block_votes_no_fail
             ~default
             ~per_block_vote_file
           [@profiler.record_s {verbosity = Info} "read per block votes file"])
        in
        Lwt.return
          Protocol.Alpha_context.Per_block_votes.
            {liquidity_baking_vote = to_protocol liquidity_baking_vote}
    | None -> Lwt.return default
  in
  let*! () =
    Events.(emit vote_for_liquidity_baking_toggle) liquidity_baking_vote
  in
  let chain = `Hash global_state.chain_id in
  let pred_block = `Hash (predecessor.hash, 0) in
  let* pred_resulting_context_hash =
    (Node_rpc.block_resulting_context_hash
       cctxt
       ~chain
       ~block:pred_block
       () [@profiler.record_s {verbosity = Info} "pred resulting context hash"])
  in
  let* pred_live_blocks =
    (Node_rpc.live_blocks
       cctxt
       ~chain
       ~block:pred_block
       () [@profiler.record_s {verbosity = Info} "live blocks"])
  in
  let round_duration =
    Round.round_duration global_state.round_durations round
    |> Period.to_seconds |> Int64.to_float
  in
  let* {unsigned_block_header; operations; manager_operations_infos} =
    Utils.event_on_stalling_promise
      ~initial_delay:(round_duration /. 2.)
      ~event:(fun sum -> Events.(emit stalling_forge_block (level, round, sum)))
    @@ (Block_forge.forge
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
          ~user_activated_upgrades
          ~force_apply
          global_state.config.fees
          simulation_mode
          simulation_kind
          global_state.constants.parametric
        [@profiler.record_s {verbosity = Info} "forge block"])
  in
  let* signed_block_header =
    (sign_block_header
       round_duration
       global_state
       delegate.consensus_key
       unsigned_block_header
     [@profiler.record_s {verbosity = Info} "sign block header"])
  in
  let* () =
    match seed_nonce_opt with
    | None ->
        (* Nothing to do *)
        return_unit
    | Some (_, nonce) ->
        let block_hash = Block_header.hash signed_block_header in
        (Baking_nonces.register_nonce
           cctxt
           ~chain_id
           block_hash
           nonce
           ~cycle:injection_level.cycle
           ~level:injection_level.level
           ~round [@profiler.record_s {verbosity = Info} "register nonce"])
  in
  let baking_votes = {Per_block_votes.liquidity_baking_vote} in
  return
    {
      signed_block_header;
      round;
      delegate;
      operations;
      manager_operations_infos;
      baking_votes;
    }

let dal_checks_and_warnings state =
  let open Lwt_syntax in
  (* We print warning about DAL state only every 50 levels. *)
  let current_level = state.level_state.current_level in
  let level_with_warning = Int32.rem current_level 50l = 1l in
  if level_with_warning then
    match state.global_state.dal_node_rpc_ctxt with
    | None -> Events.(emit no_dal_node_provided) ()
    | Some ctxt -> (
        let* health = Node_rpc.get_dal_health ctxt in
        match health with
        | Ok health -> (
            match health.status with
            | Tezos_dal_node_services.Types.Health.Up -> return_unit
            | _ -> Events.(emit unhealthy_dal_node) (ctxt#base, health))
        | Error _ -> Events.(emit unreachable_dal_node) ctxt#base)
  else return_unit

let only_if_dal_feature_enabled state ~default_value f =
  let open Lwt_result_syntax in
  let open Constants in
  let Parametric.{dal = {feature_enable; _}; _} =
    state.global_state.constants.parametric
  in
  if feature_enable then
    let*! () = dal_checks_and_warnings state in
    Option.fold
      ~none:(return default_value)
      ~some:f
      state.global_state.dal_node_rpc_ctxt
  else return default_value

let process_dal_rpc_result state delegate level round =
  let open Lwt_result_syntax in
  function
  | None -> return_none
  | Some Tezos_dal_node_services.Types.Not_in_committee ->
      let*! () = Events.(emit not_in_dal_committee (delegate, level)) in
      return_none
  | Some (Attestable_slots {slots; published_level}) ->
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
          slots
      in
      let dal_content = {attestation = dal_attestation} in
      let*! () =
        Events.(
          emit
            attach_dal_attestation
            (delegate, dal_content, published_level, level, round))
      in
      return_some dal_content

let may_get_dal_content state consensus_vote =
  let open Lwt_result_syntax in
  let {delegate; vote_consensus_content; _} = consensus_vote in
  let level, round =
    ( Raw_level.to_int32 vote_consensus_content.level,
      vote_consensus_content.round )
  in
  let delegate_id = Delegate.delegate_id delegate in
  only_if_dal_feature_enabled
    state
    ~default_value:None
    (fun _dal_node_rpc_ctxt ->
      let*! dal_attestable_slots =
        (Dal_attestable_slots_worker.get_dal_attestable_slots
           state.global_state.dal_attestable_slots_worker
           ~delegate_id
           ~attestation_level:level
         [@profiler.record_s
           {verbosity = Debug}
             (Format.asprintf
                "get_dal_attestable_slots - delegate_id : %a"
                Delegate_id.pp
                delegate_id)])
      in
      process_dal_rpc_result state delegate_id level round dal_attestable_slots)

let is_authorized (global_state : global_state) highwatermarks consensus_vote =
  let {delegate; vote_consensus_content; _} = consensus_vote in
  let level, round =
    ( Raw_level.to_int32 vote_consensus_content.level,
      vote_consensus_content.round )
  in
  let may_sign =
    match consensus_vote.vote_kind with
    | Preattestation ->
        Baking_highwatermarks.may_sign_preattestation
          highwatermarks
          ~delegate:delegate.consensus_key.id
          ~level
          ~round
    | Attestation ->
        Baking_highwatermarks.may_sign_attestation
          highwatermarks
          ~delegate:delegate.consensus_key.id
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
            (fun ({delegate; _} : unsigned_consensus_vote) ->
              delegate.consensus_key.id)
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
      (fun unsigned_consensus_vote ->
        let error =
          match unsigned_consensus_vote.vote_kind with
          | Preattestation ->
              Baking_highwatermarks.Block_previously_preattested {round; level}
          | Attestation ->
              Baking_highwatermarks.Block_previously_attested {round; level}
        in
        Events.(emit skipping_consensus_vote (unsigned_consensus_vote, [error])))
      unauthorized_votes
  in
  return authorized_votes

let forge_and_sign_consensus_vote global_state ~branch unsigned_consensus_vote :
    signed_consensus_vote tzresult Lwt.t =
  let open Lwt_result_syntax in
  let cctxt = global_state.cctxt in
  let chain_id = global_state.chain_id in
  let {
    vote_kind;
    vote_consensus_content = {level; round; _} as vote_consensus_content;
    delegate;
    dal_content;
  } =
    unsigned_consensus_vote
  in
  let shell = {Tezos_base.Operation.branch} in
  let watermark =
    match vote_kind with
    | Preattestation -> Operation.(to_watermark (Preattestation chain_id))
    | Attestation -> Operation.(to_watermark (Attestation chain_id))
  in
  let bls_mode =
    Key.is_bls delegate.consensus_key
    && global_state.constants.parametric.aggregate_attestation
  in
  let* dal_content, companion_key_opt =
    match vote_kind with
    | Preattestation ->
        (* Preattestations cannot have a dal_content and do not use
           the companion key. *)
        return (None, None)
    | Attestation -> (
        if not bls_mode then
          (* If not in BLS mode, leave the dal_content unchanged and do
             not use the companion key. *)
          return (dal_content, None)
        else if Option.is_none dal_content then
          (* No dal_content: the companion key will not be used. *)
          return (dal_content, None)
        else
          match delegate.companion_key with
          | None ->
              (* There is an available dal_content, but the BLS
                 consensus key does not have an associated companion
                 key (whether because the delegate has not registered
                 any companion key, or because it has not been
                 provided to the baker): signing an attestation with
                 DAL is not possible. Set the dal_content to None and
                 issue a warning. *)
              let*! () =
                Events.(
                  emit
                    missing_companion_key_for_dal_with_bls
                    ( Delegate.delegate_id delegate,
                      Raw_level.to_int32 vote_consensus_content.level ))
              in
              return (None, None)
          | Some companion_key ->
              (* We have everything we need to issue a BLS attestation with DAL. *)
              return (dal_content, Some companion_key))
  in
  let (Contents_list contents as packed_contents_list) =
    match vote_kind with
    | Preattestation ->
        Contents_list (Single (Preattestation vote_consensus_content))
    | Attestation ->
        Contents_list
          (Single
             (Attestation
                {consensus_content = vote_consensus_content; dal_content}))
  in
  let unsigned_operation = (shell, packed_contents_list) in
  let encoding =
    if bls_mode then Operation.bls_mode_unsigned_encoding
    else Operation.unsigned_encoding
  in
  let unsigned_operation_bytes =
    Data_encoding.Binary.to_bytes_exn encoding unsigned_operation
  in
  let signing_request =
    match vote_kind with
    | Preattestation -> `Preattestation
    | Attestation -> `Attestation
  in
  let sk_consensus_uri = delegate.consensus_key.secret_key_uri in
  let delay_between_stall_events =
    let round_duration =
      Round.round_duration global_state.round_durations round
      |> Period.to_seconds |> Int64.to_float
    in
    (* round_duration /. 3 is arbitrary and conservative *)
    round_duration /. 3.
  in
  let* consensus_sig =
    Utils.event_on_stalling_promise
      ~initial_delay:delay_between_stall_events
      ~event:(fun sum ->
        Events.(
          emit
            stalling_signature
            (signing_request, Raw_level.to_int32 level, round, sum)))
    @@ sign
         ?timeout:global_state.config.remote_calls_timeout
         ~signing_request
         cctxt
         ~watermark
         sk_consensus_uri
         unsigned_operation_bytes
  in
  let* signature =
    match (dal_content, companion_key_opt) with
    | None, None -> return consensus_sig
    | None, Some _ ->
        (* should not be possible by construction of
           companion_key_opt *)
        return consensus_sig
    | Some _, None ->
        (* only possible in non-BLS mode *)
        return consensus_sig
    | Some {attestation = dal_attestation}, Some companion_key -> (
        let sk_companion_uri = companion_key.secret_key_uri in
        let* companion_sig =
          Utils.event_on_stalling_promise
            ~initial_delay:delay_between_stall_events
            ~event:(fun sum ->
              Events.(
                emit
                  stalling_signature
                  (signing_request, Raw_level.to_int32 level, round, sum)))
          @@ sign
               ?timeout:global_state.config.remote_calls_timeout
               ~signing_request
               cctxt
               ~watermark
               sk_companion_uri
               unsigned_operation_bytes
        in
        match
          ( consensus_sig,
            companion_sig,
            delegate.consensus_key.public_key,
            companion_key.public_key )
        with
        (* This if-else branch is for BLS mode so both signatures
           should be BLS signatures, and both public keys should be
           BLS public keys. *)
        | ( Signature.Bls consensus_sig,
            Signature.Bls companion_sig,
            Signature.Bls consensus_pk,
            Signature.Bls companion_pk ) -> (
            let dal_dependent_bls_sig_opt =
              Alpha_context.Dal.Attestation.Dal_dependent_signing.aggregate_sig
                ~subgroup_check:false
                ~consensus_pk
                ~companion_pk
                ~consensus_sig
                ~companion_sig
                ~op:unsigned_operation_bytes
                dal_attestation
            in
            match dal_dependent_bls_sig_opt with
            | None -> tzfail Baking_errors.Signature_aggregation_failure
            | Some dal_dependent_bls_sig ->
                return (Signature.Bls dal_dependent_bls_sig : Signature.t))
        | _, Signature.Bls _, Signature.Bls _, Signature.Bls _ ->
            tzfail (Baking_errors.Unexpected_signature_type consensus_sig)
        | _, _, Signature.Bls _, Signature.Bls _ ->
            tzfail (Baking_errors.Unexpected_signature_type companion_sig)
        | _, _, _, Signature.Bls _ ->
            tzfail
              (Baking_errors.Unexpected_public_key_type
                 delegate.consensus_key.public_key)
        | _, _, _, _ ->
            tzfail
              (Baking_errors.Unexpected_public_key_type companion_key.public_key)
        )
  in
  let protocol_data = Operation_data {contents; signature = Some signature} in
  let signed_operation : Operation.packed = {shell; protocol_data} in
  (* Also update unsigned_consensus_vote: dal_content may have been
     set to None. *)
  let unsigned_consensus_vote = {unsigned_consensus_vote with dal_content} in
  return {unsigned_consensus_vote; signed_operation}

let sign_consensus_votes (global_state : global_state)
    ({batch_kind; batch_content; batch_branch; _} as
     unsigned_consensus_vote_batch :
      unsigned_consensus_vote_batch) =
  let open Lwt_result_syntax in
  let* authorized_consensus_votes =
    (authorized_consensus_votes
       global_state
       unsigned_consensus_vote_batch
     [@profiler.record_s {verbosity = Info} "authorized consensus votes"])
  in
  let* signed_consensus_votes =
    List.filter_map_es
      (fun unsigned_consensus_vote ->
        let*! () = Events.(emit signing_consensus_op) unsigned_consensus_vote in
        let*! signed_consensus_vote_r =
          (forge_and_sign_consensus_vote
             global_state
             ~branch:batch_branch
             unsigned_consensus_vote
           [@profiler.record_s
             {verbosity = Info} "forge and sign consensus vote"])
        in
        match signed_consensus_vote_r with
        | Error err ->
            let*! () =
              Events.(
                emit skipping_consensus_vote (unsigned_consensus_vote, err))
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
  protect
    ~on_error:(fun err ->
      let*! () =
        Events.(
          emit failed_to_inject_consensus_vote (signed_consensus_vote, err))
      in
      return_unit)
    (fun () ->
      let* oph =
        (Node_rpc.inject_operation
           cctxt
           ~chain:(`Hash chain_id)
           signed_consensus_vote.signed_operation
         [@profiler.record_s
           {verbosity = Debug}
             (Format.sprintf
                "injecting consensus vote: %s"
                (match
                   signed_consensus_vote.unsigned_consensus_vote.vote_kind
                 with
                | Preattestation -> "preattestation"
                | Attestation -> "attestation"))])
      in
      let*! () =
        Events.(emit consensus_op_injected) (signed_consensus_vote, oph)
      in
      return_unit)

let inject_consensus_votes state signed_consensus_vote_batch =
  List.iter_ep
    (inject_consensus_vote state)
    signed_consensus_vote_batch.signed_consensus_votes

let inject_block ?(force_injection = false) ?(asynchronous = true) state
    prepared_block =
  let open Lwt_result_syntax in
  let {
    signed_block_header;
    round;
    delegate;
    operations;
    manager_operations_infos;
    baking_votes;
  } =
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
      (Node_rpc.inject_block
         state.global_state.cctxt
         ~force:state.global_state.config.force
         ~chain:(`Hash state.global_state.chain_id)
         signed_block_header
         operations [@profiler.record_s {verbosity = Info} "injecting block"])
    in
    let*! () =
      Events.(
        emit
          block_injected
          ( bh,
            signed_block_header.shell.level,
            round,
            delegate,
            manager_operations_infos ))
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
    Delegate_infos.consensus_threshold state.level_state.delegate_infos
  in
  let consensus_committee =
    Delegate_infos.consensus_committee state.level_state.delegate_infos
  in
  let get_slot_voting_power ~slot =
    Delegate_infos.voting_power state.level_state.delegate_infos ~slot
  in
  let latest_proposal = state.level_state.latest_proposal.block in
  (* assert (latest_proposal.block.round = state.round_state.current_round) ; *)
  let candidate =
    {
      Operation_worker.hash = latest_proposal.hash;
      level_watched = latest_proposal.shell.level;
      round_watched = latest_proposal.round;
      payload_hash_watched = latest_proposal.payload_hash;
      branch_watched =
        (if state.global_state.constants.parametric.aggregate_attestation then
           Some latest_proposal.grandparent
         else None);
    }
  in
  (consensus_threshold, consensus_committee, get_slot_voting_power, candidate)

let start_waiting_for_preattestation_quorum state =
  let consensus_threshold, consensus_committee, get_slot_voting_power, candidate
      =
    prepare_waiting_for_quorum state
  in
  let operation_worker = state.global_state.operation_worker in
  Operation_worker.monitor_preattestation_quorum
    operation_worker
    ~consensus_threshold
    ~consensus_committee
    ~get_slot_voting_power
    candidate

let start_waiting_for_attestation_quorum state =
  let consensus_threshold, consensus_committee, get_slot_voting_power, candidate
      =
    prepare_waiting_for_quorum state
  in
  let operation_worker = state.global_state.operation_worker in
  Operation_worker.monitor_attestation_quorum
    operation_worker
    ~consensus_threshold
    ~consensus_committee
    ~get_slot_voting_power
    candidate

let compute_round (proposal : proposal) round_durations =
  let open Protocol in
  let open Baking_state_types in
  let timestamp = Time.System.now () |> Time.System.to_protocol in
  let predecessor_block = proposal.predecessor in
  Environment.wrap_tzresult
  @@ Alpha_context.Round.round_of_timestamp
       round_durations
       ~predecessor_timestamp:predecessor_block.shell.timestamp
       ~predecessor_round:predecessor_block.round
       ~timestamp

let notice_delegates_without_slots all_delegates delegate_infos level =
  let delegates_without_slots =
    List.filter
      (fun {Baking_state_types.Key.id; _} ->
        not
        @@ List.exists
             (fun ({delegate = {consensus_key; _}; _} : delegate_info) ->
               id = consensus_key.id)
             (Baking_state.Delegate_infos.own_delegates delegate_infos))
      all_delegates
  in
  match delegates_without_slots with
  | [] -> Lwt.return_unit
  | delegates -> Events.(emit delegates_without_slots (delegates, level))

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
  let* delegate_infos =
    if Int32.(new_level = succ state.level_state.current_level) then
      return state.level_state.next_level_delegate_infos
    else
      Baking_state.compute_delegate_infos
        cctxt
        delegates
        ~level:new_level
        ~chain
      [@profiler.record_s
        {verbosity = Debug} "compute predecessor delegate slots"]
  in
  let* next_level_delegate_infos =
    (Baking_state.compute_delegate_infos
       cctxt
       delegates
       ~level:(Int32.succ new_level)
       ~chain
     [@profiler.record_s {verbosity = Debug} "compute current delegate slots"])
  in
  let*! () =
    notice_delegates_without_slots delegates delegate_infos new_level
  in
  let round_durations = state.global_state.round_durations in
  let*? current_round =
    (compute_round
       new_level_proposal
       round_durations [@profiler.record_f {verbosity = Debug} "compute round"])
  in
  let*! new_state, new_action =
    (compute_new_state
       ~current_round
       ~delegate_infos
       ~next_level_delegate_infos
     [@profiler.record_s {verbosity = Debug} "compute new state"])
  in
  let _promise =
    only_if_dal_feature_enabled
      new_state
      ~default_value:()
      (fun dal_node_rpc_ctxt ->
        let next_level_delegate_ids =
          Baking_state.Delegate_infos.own_delegate_ids next_level_delegate_infos
        in
        (* Refresh per-delegate subscriptions for the next level. Rights/committee
           can change at level boundaries (e.g. migrations, reorganisations, key/profile
           updates). Doing it here makes the streams ready before we build next-level
           attestations. *)
        let*! () =
          Dal_attestable_slots_worker.update_streams_subscriptions
            state.global_state.dal_attestable_slots_worker
            dal_node_rpc_ctxt
            ~delegate_ids:next_level_delegate_ids
        in
        return_unit)
  in
  return (new_state, new_action)

let synchronize_round state {new_round_proposal; handle_proposal} =
  let open Lwt_result_syntax in
  let*! () =
    Events.(emit synchronizing_round new_round_proposal.predecessor.hash)
  in
  let round_durations = state.global_state.round_durations in
  let*? current_round =
    (compute_round
       new_round_proposal
       round_durations [@profiler.record_f {verbosity = Debug} "compute round"])
  in
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
  | Prepare_block {block_to_bake} ->
      prepare_block_request
        state
        block_to_bake
      [@profiler.record_s {verbosity = Info} "action : prepare block"]
  | Prepare_preattestations {preattestations} ->
      let* new_state =
        (prepare_preattestations_request
           state
           preattestations
         [@profiler.record_s
           {verbosity = Info} "action : prepare preattestations"])
      in
      (* We wait for preattestations to trigger the [Prequorum_reached]
         event *)
      perform_action new_state Watch_prequorum
  | Prepare_attestations {attestations} ->
      let* new_state =
        (prepare_attestations_request
           state
           attestations
         [@profiler.record_s {verbosity = Info} "action : prepare attestations"])
      in
      (* We wait for attestations to trigger the [Quorum_reached]
         event *)
      perform_action new_state Watch_quorum
  | Prepare_consensus_votes {preattestations; attestations} ->
      let* state =
        (prepare_preattestations_request
           state
           preattestations
         [@profiler.record_s
           {verbosity = Info} "action : prepare preattestations"])
      in
      let* state =
        (prepare_attestations_request
           state
           attestations
         [@profiler.record_s {verbosity = Info} "action : prepare attestations"])
      in
      (* We wait for preattestations to trigger the [Prequorum_reached]
         event *)
      perform_action state Watch_prequorum
  | Inject_block {prepared_block; force_injection; asynchronous} ->
      let* new_state =
        (inject_block
           ~force_injection
           ~asynchronous
           state
           prepared_block
         [@profiler.record_s {verbosity = Info} "action : inject block"])
      in
      return new_state
  | Inject_preattestation {signed_preattestation} ->
      let* () =
        (inject_consensus_vote
           state
           signed_preattestation
         [@profiler.record_s
           {verbosity = Info} "action : inject preattestation"])
      in
      (* Here, we do not need to wait for the prequorum, it has
         already been triggered by the
         [Prepare_(preattestation|consensus_votes)] action *)
      return state
  | Inject_attestations {signed_attestations} ->
      let* () =
        (inject_consensus_votes
           state
           signed_attestations
         [@profiler.record_s {verbosity = Info} "action : inject attestations"])
      in
      (* We wait for attestations to trigger the [Quorum_reached]
         event *)
      perform_action state Watch_quorum
  | Update_to_level level_update ->
      let* new_state, new_action =
        (update_to_level
           state
           level_update
         [@profiler.record_s {verbosity = Info} "action : update to level"])
      in
      perform_action new_state new_action
  | Synchronize_round round_update ->
      let* new_state, new_action =
        (synchronize_round
           state
           round_update
         [@profiler.record_s {verbosity = Info} "action : synchronize round"])
      in
      perform_action new_state new_action
  | Watch_prequorum ->
      let*! () =
        (start_waiting_for_preattestation_quorum
           state
         [@profiler.record_s
           {verbosity = Info} "action : wait for preattestation quorum"])
      in
      return state
  | Watch_quorum ->
      let*! () =
        (start_waiting_for_attestation_quorum
           state
         [@profiler.record_s
           {verbosity = Info} "action : wait for attestation quorum"])
      in
      return state
