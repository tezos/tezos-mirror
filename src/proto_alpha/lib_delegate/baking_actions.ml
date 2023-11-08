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
    Data_encoding.(
      list (dynamic_size Operation.encoding_with_legacy_attestation_name))

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

type block_kind =
  | Fresh of Operation_pool.pool
  | Reproposal of {
      consensus_operations : packed_operation list;
      payload_hash : Block_payload_hash.t;
      payload_round : Round.t;
      payload : Operation_pool.payload;
    }

type block_to_bake = {
  predecessor : block_info;
  round : Round.t;
  delegate : Baking_state.consensus_key_and_delegate;
  kind : block_kind;
  force_apply : bool;
}

type action =
  | Do_nothing
  | Inject_block of {block_to_bake : block_to_bake; updated_state : state}
  | Inject_preattestations of {
      preattestations : (consensus_key_and_delegate * consensus_content) list;
    }
  | Inject_attestations of {
      attestations : (consensus_key_and_delegate * consensus_content) list;
    }
  | Update_to_level of level_update
  | Synchronize_round of round_update
  | Watch_proposal

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
  | Inject_block _ -> Format.fprintf fmt "inject block"
  | Inject_preattestations _ -> Format.fprintf fmt "inject preattestations"
  | Inject_attestations _ -> Format.fprintf fmt "inject attestations"
  | Update_to_level _ -> Format.fprintf fmt "update to level"
  | Synchronize_round _ -> Format.fprintf fmt "synchronize round"
  | Watch_proposal -> Format.fprintf fmt "watch proposal"

let generate_seed_nonce_hash config delegate level =
  let open Lwt_result_syntax in
  if level.Level.expected_commitment then
    let* seed_nonce =
      Baking_nonces.generate_seed_nonce config delegate level.level
    in
    return_some seed_nonce
  else return_none

let sign_block_header state proposer unsigned_block_header =
  let open Lwt_result_syntax in
  let cctxt = state.global_state.cctxt in
  let chain_id = state.global_state.chain_id in
  let force = state.global_state.config.force in
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

let inject_block ~state_recorder state block_to_bake ~updated_state =
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
  let cctxt = state.global_state.cctxt in
  let chain_id = state.global_state.chain_id in
  let simulation_mode = state.global_state.validation_mode in
  let round_durations = state.global_state.round_durations in
  let*? timestamp =
    Environment.wrap_tzresult
      (Round.timestamp_of_round
         round_durations
         ~predecessor_timestamp:predecessor.shell.timestamp
         ~predecessor_round:predecessor.round
         ~round)
  in
  let external_operation_source = state.global_state.config.extra_operations in
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
      (`Hash state.global_state.chain_id, `Hash (predecessor.hash, 0))
  in
  let* seed_nonce_opt =
    generate_seed_nonce_hash
      state.global_state.config.Baking_configuration.nonce
      consensus_key
      injection_level
  in
  let seed_nonce_hash = Option.map fst seed_nonce_opt in
  let user_activated_upgrades =
    state.global_state.config.user_activated_upgrades
  in
  (* Set liquidity_baking_toggle_vote for this block *)
  let {
    Baking_configuration.vote_file;
    liquidity_baking_vote;
    adaptive_issuance_vote;
  } =
    state.global_state.config.per_block_votes
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
  (* Cache last per-block votes to use in case of vote file errors *)
  let updated_state =
    {
      updated_state with
      global_state =
        {
          updated_state.global_state with
          config =
            {
              updated_state.global_state.config with
              per_block_votes =
                {
                  updated_state.global_state.config.per_block_votes with
                  liquidity_baking_vote;
                  adaptive_issuance_vote;
                };
            };
        };
    }
  in
  let*! () =
    Events.(emit vote_for_liquidity_baking_toggle) liquidity_baking_vote
  in
  let*! () = Events.(emit vote_for_adaptive_issuance) adaptive_issuance_vote in
  let chain = `Hash state.global_state.chain_id in
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
      state.global_state.config.fees
      simulation_mode
      simulation_kind
      state.global_state.constants.parametric
  in
  let* signed_block_header =
    sign_block_header state consensus_key unsigned_block_header
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
  let* () = state_recorder ~new_state:updated_state in
  let*! () =
    Events.(
      emit injecting_block (signed_block_header.shell.level, round, delegate))
  in
  let* bh =
    Node_rpc.inject_block
      cctxt
      ~force:state.global_state.config.force
      ~chain:(`Hash state.global_state.chain_id)
      signed_block_header
      operations
  in
  let*! () =
    Events.(
      emit block_injected (bh, signed_block_header.shell.level, round, delegate))
  in
  return updated_state

let sign_preattestations state ~preattestations =
  let open Lwt_result_syntax in
  let cctxt = state.global_state.cctxt in
  let chain_id = state.global_state.chain_id in
  (* N.b. signing a lot of operations may take some time *)
  (* Don't parallelize signatures: the signer might not be able to
     handle concurrent requests *)
  let block_location =
    Baking_files.resolve_location ~chain_id `Highwatermarks
  in
  List.filter_map_es
    (fun (((consensus_key, _) as delegate), consensus_content) ->
      let*! () = Events.(emit signing_preattestation delegate) in
      let shell =
        (* The branch is the latest finalized block. *)
        {
          Tezos_base.Operation.branch =
            state.level_state.latest_proposal.predecessor.shell.predecessor;
        }
      in
      let contents = Single (Preattestation consensus_content) in
      let level = Raw_level.to_int32 consensus_content.level in
      let round = consensus_content.round in
      let sk_uri = consensus_key.secret_key_uri in
      let* may_sign =
        cctxt#with_lock (fun () ->
            let* may_sign =
              Baking_highwatermarks.may_sign_preattestation
                cctxt
                block_location
                ~delegate:consensus_key.public_key_hash
                ~level
                ~round
            in
            match may_sign with
            | true ->
                let* () =
                  Baking_highwatermarks.record_preattestation
                    cctxt
                    block_location
                    ~delegate:consensus_key.public_key_hash
                    ~level
                    ~round
                in
                return_true
            | false -> return state.global_state.config.force)
      in
      let*! signature =
        if may_sign then
          let unsigned_operation = (shell, Contents_list contents) in
          let watermark = Operation.(to_watermark (Preattestation chain_id)) in
          let unsigned_operation_bytes =
            Data_encoding.Binary.to_bytes_exn
              Operation.unsigned_encoding_with_legacy_attestation_name
              unsigned_operation
          in
          Client_keys.sign cctxt ~watermark sk_uri unsigned_operation_bytes
        else
          tzfail
            (Baking_highwatermarks.Block_previously_preattested {round; level})
      in
      match signature with
      | Error err ->
          let*! () = Events.(emit skipping_preattestation (delegate, err)) in
          return_none
      | Ok signature ->
          let protocol_data =
            Operation_data {contents; signature = Some signature}
          in
          let operation : Operation.packed = {shell; protocol_data} in
          return_some (delegate, operation, level, round))
    preattestations

let inject_preattestations state ~preattestations =
  let open Lwt_result_syntax in
  let cctxt = state.global_state.cctxt in
  let chain_id = state.global_state.chain_id in
  let* signed_operations = sign_preattestations state ~preattestations in
  (* TODO: add a RPC to inject multiple operations *)
  List.iter_ep
    (fun (delegate, operation, level, round) ->
      protect
        ~on_error:(fun err ->
          let*! () =
            Events.(emit failed_to_inject_preattestation (delegate, err))
          in
          return_unit)
        (fun () ->
          let* oph =
            Node_rpc.inject_operation cctxt ~chain:(`Hash chain_id) operation
          in
          let*! () =
            Events.(emit preattestation_injected (oph, delegate, level, round))
          in
          return_unit))
    signed_operations

let sign_attestations state ~attestations =
  let open Lwt_result_syntax in
  let cctxt = state.global_state.cctxt in
  let chain_id = state.global_state.chain_id in
  (* N.b. signing a lot of operations may take some time *)
  (* Don't parallelize signatures: the signer might not be able to
     handle concurrent requests *)
  let block_location =
    Baking_files.resolve_location ~chain_id `Highwatermarks
  in
  List.filter_map_es
    (fun (((consensus_key, _) as delegate), consensus_content) ->
      let*! () = Events.(emit signing_attestation delegate) in
      let shell =
        (* The branch is the latest finalized block. *)
        {
          Tezos_base.Operation.branch =
            state.level_state.latest_proposal.predecessor.shell.predecessor;
        }
      in
      let contents =
        (* No preattestations are included *)
        Single (Attestation consensus_content)
      in
      let level = Raw_level.to_int32 consensus_content.level in
      let round = consensus_content.round in
      let sk_uri = consensus_key.secret_key_uri in
      let* may_sign =
        cctxt#with_lock (fun () ->
            let* may_sign =
              Baking_highwatermarks.may_sign_attestation
                cctxt
                block_location
                ~delegate:consensus_key.public_key_hash
                ~level
                ~round
            in
            match may_sign with
            | true ->
                let* () =
                  Baking_highwatermarks.record_attestation
                    cctxt
                    block_location
                    ~delegate:consensus_key.public_key_hash
                    ~level
                    ~round
                in
                return_true
            | false -> return state.global_state.config.force)
      in
      let*! signature =
        if may_sign then
          let watermark = Operation.(to_watermark (Attestation chain_id)) in
          let unsigned_operation = (shell, Contents_list contents) in
          let unsigned_operation_bytes =
            Data_encoding.Binary.to_bytes_exn
              Operation.unsigned_encoding_with_legacy_attestation_name
              unsigned_operation
          in
          Client_keys.sign cctxt ~watermark sk_uri unsigned_operation_bytes
        else
          tzfail
            (Baking_highwatermarks.Block_previously_attested {round; level})
      in
      match signature with
      | Error err ->
          let*! () = Events.(emit skipping_attestation (delegate, err)) in
          return_none
      | Ok signature ->
          let protocol_data =
            Operation_data {contents; signature = Some signature}
          in
          let operation : Operation.packed = {shell; protocol_data} in
          return_some (delegate, operation, level, round))
    attestations

let inject_attestations state ~attestations =
  let open Lwt_result_syntax in
  let cctxt = state.global_state.cctxt in
  let chain_id = state.global_state.chain_id in
  let* signed_operations = sign_attestations state ~attestations in
  (* TODO: add a RPC to inject multiple operations *)
  List.iter_ep
    (fun (delegate, operation, level, round) ->
      protect
        ~on_error:(fun err ->
          let*! () =
            Events.(emit failed_to_inject_attestation (delegate, err))
          in
          return_unit)
        (fun () ->
          let* oph =
            Node_rpc.inject_operation cctxt ~chain:(`Hash chain_id) operation
          in
          let*! () =
            Events.(emit attestation_injected (oph, delegate, level, round))
          in
          return_unit))
    signed_operations

let sign_dal_attestations state attestations =
  let open Lwt_result_syntax in
  let cctxt = state.global_state.cctxt in
  let chain_id = state.global_state.chain_id in
  (* N.b. signing a lot of operations may take some time *)
  (* Don't parallelize signatures: the signer might not be able to
     handle concurrent requests *)
  let shell =
    {
      Tezos_base.Operation.branch =
        state.level_state.latest_proposal.predecessor.hash;
    }
  in
  List.filter_map_es
    (fun (((consensus_key, _) as delegate), consensus_content, published_level) ->
      let watermark = Operation.(to_watermark (Dal_attestation chain_id)) in
      let contents = Single (Dal_attestation consensus_content) in
      let unsigned_operation = (shell, Contents_list contents) in
      let unsigned_operation_bytes =
        Data_encoding.Binary.to_bytes_exn
          Operation.unsigned_encoding_with_legacy_attestation_name
          unsigned_operation
      in
      let*! signature =
        Client_keys.sign
          cctxt
          ~watermark
          consensus_key.secret_key_uri
          unsigned_operation_bytes
      in
      match signature with
      | Error err ->
          let*! () = Events.(emit skipping_dal_attestation (delegate, err)) in
          return_none
      | Ok signature ->
          let protocol_data =
            Operation_data {contents; signature = Some signature}
          in
          let operation : Operation.packed = {shell; protocol_data} in
          return_some
            ( delegate,
              operation,
              consensus_content.Dal.Attestation.attestation,
              published_level ))
    attestations

let inject_dal_attestations state attestations =
  let open Lwt_result_syntax in
  let cctxt = state.global_state.cctxt in
  let chain_id = state.global_state.chain_id in
  let* signed_operations = sign_dal_attestations state attestations in
  List.iter_ep
    (fun ( delegate,
           signed_operation,
           (attestation : Dal.Attestation.t),
           published_level ) ->
      let encoded_op =
        Data_encoding.Binary.to_bytes_exn
          Operation.encoding_with_legacy_attestation_name
          signed_operation
      in
      let* oph =
        Shell_services.Injection.operation
          cctxt
          ~chain:(`Hash chain_id)
          encoded_op
      in
      let bitset_int = Bitset.to_z (attestation :> Bitset.t) in
      let attestation_level = state.level_state.current_level in
      let*! () =
        Events.(
          emit
            dal_attestation_injected
            (oph, delegate, bitset_int, published_level, attestation_level))
      in
      return_unit)
    signed_operations

let no_dal_node_warning_counter = ref 0

let only_if_dal_feature_enabled state ~default_value f =
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

let get_dal_attestations state =
  let open Lwt_result_syntax in
  only_if_dal_feature_enabled state ~default_value:[] (fun dal_node_rpc_ctxt ->
      let attestation_level = state.level_state.current_level in
      let attested_level = Int32.succ attestation_level in
      let delegates =
        List.map
          (fun delegate_slot ->
            (delegate_slot.consensus_key_and_delegate, delegate_slot.first_slot))
          (Delegate_slots.own_delegates state.level_state.delegate_slots)
      in
      let signing_key delegate = (fst delegate).public_key_hash in
      let* attestations =
        List.fold_left_es
          (fun acc (delegate, first_slot) ->
            let*! tz_res =
              Node_rpc.get_attestable_slots
                dal_node_rpc_ctxt
                (signing_key delegate)
                ~attested_level
            in
            match tz_res with
            | Error errs ->
                let*! () =
                  Events.(emit failed_to_get_dal_attestations (delegate, errs))
                in
                return acc
            | Ok res -> (
                match res with
                | Tezos_dal_node_services.Types.Not_in_committee -> return acc
                | Attestable_slots {slots = attestation; published_level} ->
                    if List.exists Fun.id attestation then
                      return
                        ((delegate, attestation, published_level, first_slot)
                        :: acc)
                    else
                      (* No slot is attested, no need to send an attestation, at least
                         for now. *)
                      let*! () =
                        Events.(
                          emit
                            dal_attestation_void
                            (delegate, attestation_level, published_level))
                      in
                      return acc))
          []
          delegates
      in
      let number_of_slots =
        state.global_state.constants.parametric.dal.number_of_slots
      in
      List.map
        (fun (delegate, attestation_flags, published_level, first_slot) ->
          let attestation =
            List.fold_left_i
              (fun i acc flag ->
                match Dal.Slot_index.of_int_opt ~number_of_slots i with
                | Some index when flag -> Dal.Attestation.commit acc index
                | None | Some _ -> acc)
              Dal.Attestation.empty
              attestation_flags
          in
          ( delegate,
            Dal.Attestation.
              {
                attestation;
                level = Raw_level.of_int32_exn attestation_level;
                slot = first_slot;
              },
            published_level ))
        attestations
      |> return)

let get_and_inject_dal_attestations state =
  let open Lwt_result_syntax in
  let* attestations = get_dal_attestations state in
  inject_dal_attestations state attestations

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
      {current_round; current_phase = Idle; delayed_prequorum = None}
    in
    let new_state = {state with round_state = new_round_state} in
    let*! new_state = handle_proposal new_state in
    return new_state

(* TODO: https://gitlab.com/tezos/tezos/-/issues/4539
   Avoid updating the state here.
   (See also comment in {!State_transitions.step}.)

   TODO: https://gitlab.com/tezos/tezos/-/issues/4538
   Improve/clarify when the state is recorded.
*)
let rec perform_action ~state_recorder state (action : action) =
  let open Lwt_result_syntax in
  match action with
  | Do_nothing ->
      let* () = state_recorder ~new_state:state in
      return state
  | Inject_block {block_to_bake; updated_state} ->
      inject_block state ~state_recorder block_to_bake ~updated_state
  | Inject_preattestations {preattestations} ->
      let* () = inject_preattestations state ~preattestations in
      perform_action ~state_recorder state Watch_proposal
  | Inject_attestations {attestations} ->
      let* () = state_recorder ~new_state:state in
      let* () = inject_attestations state ~attestations in
      (* We wait for attestations to trigger the [Quorum_reached]
         event *)
      let*! () = start_waiting_for_attestation_quorum state in
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/4667
         Also inject attestations for the migration block. *)
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/4671
         Don't inject multiple attestations? *)
      let* () = get_and_inject_dal_attestations state in
      return state
  | Update_to_level level_update ->
      let* new_state, new_action = update_to_level state level_update in
      perform_action ~state_recorder new_state new_action
  | Synchronize_round round_update ->
      let* new_state, new_action = synchronize_round state round_update in
      perform_action ~state_recorder new_state new_action
  | Watch_proposal ->
      (* We wait for preattestations to trigger the
           [Prequorum_reached] event *)
      let*! () = start_waiting_for_preattestation_quorum state in
      return state
