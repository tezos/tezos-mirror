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

  let retrieve = function
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
          fail (Failed_operations_fetch {path; reason; details})
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
              Tezos_stdlib_unix.Lwt_utils_unix.Json.read_file filename
              >>= function
              | Error _ ->
                  Events.(emit invalid_json_file filename) >>= fun () ->
                  Lwt.return_none
              | Ok json -> (
                  decode_operations json >>= function
                  | Ok operations -> Lwt.return_some operations
                  | Error errs ->
                      Events.(emit cannot_fetch_operations errs) >>= fun () ->
                      Lwt.return_none)
            else
              Events.(emit no_operations_found_in_file filename) >>= fun () ->
              Lwt.return_none
        | Baking_configuration.Operations_source.Remote {uri; http_headers} -> (
            ( ((with_timeout
                  (Systime_os.sleep (Time.System.Span.of_seconds_exn 5.))
                  (fun _ ->
                    Tezos_rpc_http_client_unix.RPC_client_unix
                    .generic_media_type_call
                      ~accept:[Media_type.json]
                      ?headers:http_headers
                      `GET
                      uri)
                >>=? function
                | `Json json -> return json
                | _ -> fail "json not returned" None)
               >>=? function
               | `Ok json -> return json
               | `Unauthorized json -> fail "unauthorized request" json
               | `Gone json -> fail "gone" json
               | `Error json -> fail "error" json
               | `Not_found json -> fail "not found" json
               | `Forbidden json -> fail "forbidden" json
               | `Conflict json -> fail "conflict" json)
            >>=? fun json -> decode_operations json )
            >>= function
            | Ok operations -> Lwt.return_some operations
            | Error errs ->
                Events.(emit cannot_fetch_operations errs) >>= fun () ->
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
  | Inject_preendorsements of {
      preendorsements : (consensus_key_and_delegate * consensus_content) list;
    }
  | Inject_endorsements of {
      endorsements : (consensus_key_and_delegate * consensus_content) list;
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
  | Inject_preendorsements _ -> Format.fprintf fmt "inject preendorsements"
  | Inject_endorsements _ -> Format.fprintf fmt "inject endorsements"
  | Update_to_level _ -> Format.fprintf fmt "update to level"
  | Synchronize_round _ -> Format.fprintf fmt "synchronize round"
  | Watch_proposal -> Format.fprintf fmt "watch proposal"

let generate_seed_nonce_hash config delegate level =
  if level.Level.expected_commitment then
    Baking_nonces.generate_seed_nonce config delegate level.level
    >>=? fun seed_nonce -> return_some seed_nonce
  else return_none

let sign_block_header state proposer unsigned_block_header =
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
  Baking_state.round_of_shell_header shell >>?= fun round ->
  let open Baking_highwatermarks in
  cctxt#with_lock (fun () ->
      let block_location =
        Baking_files.resolve_location ~chain_id `Highwatermarks
      in
      may_sign_block
        cctxt
        block_location
        ~delegate:proposer.public_key_hash
        ~level
        ~round
      >>=? function
      | true ->
          record_block
            cctxt
            block_location
            ~delegate:proposer.public_key_hash
            ~level
            ~round
          >>=? fun () -> return_true
      | false ->
          Events.(emit potential_double_baking (level, round)) >>= fun () ->
          return force)
  >>=? function
  | false -> fail (Block_previously_baked {level; round})
  | true ->
      Client_keys.sign
        cctxt
        proposer.secret_key_uri
        ~watermark:Block_header.(to_watermark (Block_header chain_id))
        unsigned_header
      >>=? fun signature ->
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
  Events.(
    emit
      prepare_forging_block
      (Int32.succ predecessor.shell.level, round, delegate))
  >>= fun () ->
  let cctxt = state.global_state.cctxt in
  let chain_id = state.global_state.chain_id in
  let simulation_mode = state.global_state.validation_mode in
  let round_durations = state.global_state.round_durations in
  Environment.wrap_tzresult
    (Round.timestamp_of_round
       round_durations
       ~predecessor_timestamp:predecessor.shell.timestamp
       ~predecessor_round:predecessor.round
       ~round)
  >>?= fun timestamp ->
  let external_operation_source = state.global_state.config.extra_operations in
  Operations_source.retrieve external_operation_source >>= fun extern_ops ->
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
  Events.(
    emit forging_block (Int32.succ predecessor.shell.level, round, delegate))
  >>= fun () ->
  Plugin.RPC.current_level
    cctxt
    ~offset:1l
    (`Hash state.global_state.chain_id, `Hash (predecessor.hash, 0))
  >>=? fun injection_level ->
  generate_seed_nonce_hash
    state.global_state.config.Baking_configuration.nonce
    consensus_key
    injection_level
  >>=? fun seed_nonce_opt ->
  let seed_nonce_hash = Option.map fst seed_nonce_opt in
  let user_activated_upgrades =
    state.global_state.config.user_activated_upgrades
  in
  (* Set liquidity_baking_toggle_vote for this block *)
  let {Baking_configuration.vote_file; liquidity_baking_vote} =
    state.global_state.config.liquidity_baking
  in
  (* Prioritize reading from the [vote_file] if it exists. *)
  (match vote_file with
  | Some per_block_vote_file ->
      Liquidity_baking_vote.read_liquidity_baking_toggle_vote_no_fail
        ~default_liquidity_baking_vote:liquidity_baking_vote
        ~per_block_vote_file
  | None -> Lwt.return liquidity_baking_vote)
  >>= fun liquidity_baking_toggle_vote ->
  (* Cache last toggle vote to use in case of vote file errors *)
  let updated_state =
    {
      updated_state with
      global_state =
        {
          updated_state.global_state with
          config =
            {
              updated_state.global_state.config with
              liquidity_baking =
                {
                  updated_state.global_state.config.liquidity_baking with
                  liquidity_baking_vote = liquidity_baking_toggle_vote;
                };
            };
        };
    }
  in
  Events.(emit vote_for_liquidity_baking_toggle) liquidity_baking_toggle_vote
  >>= fun () ->
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
    ~liquidity_baking_toggle_vote
    ~user_activated_upgrades
    ~force_apply
    state.global_state.config.fees
    simulation_mode
    simulation_kind
    state.global_state.constants.parametric
  >>=? fun {unsigned_block_header; operations} ->
  sign_block_header state consensus_key unsigned_block_header
  >>=? fun signed_block_header ->
  (match seed_nonce_opt with
  | None ->
      (* Nothing to do *)
      return_unit
  | Some (_, nonce) ->
      let block_hash = Block_header.hash signed_block_header in
      Baking_nonces.register_nonce cctxt ~chain_id block_hash nonce)
  >>=? fun () ->
  state_recorder ~new_state:updated_state >>=? fun () ->
  Events.(
    emit injecting_block (signed_block_header.shell.level, round, delegate))
  >>= fun () ->
  Node_rpc.inject_block
    cctxt
    ~force:state.global_state.config.force
    ~chain:(`Hash state.global_state.chain_id)
    signed_block_header
    operations
  >>=? fun bh ->
  Events.(
    emit block_injected (bh, signed_block_header.shell.level, round, delegate))
  >>= fun () -> return updated_state

let inject_preendorsements state ~preendorsements =
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
      Events.(emit signing_preendorsement delegate) >>= fun () ->
      let shell =
        (* The branch is the latest finalized block. *)
        {
          Tezos_base.Operation.branch =
            state.level_state.latest_proposal.predecessor.shell.predecessor;
        }
      in
      let contents = Single (Preendorsement consensus_content) in
      let level = Raw_level.to_int32 consensus_content.level in
      let round = consensus_content.round in
      let sk_uri = consensus_key.secret_key_uri in
      cctxt#with_lock (fun () ->
          Baking_highwatermarks.may_sign_preendorsement
            cctxt
            block_location
            ~delegate:consensus_key.public_key_hash
            ~level
            ~round
          >>=? function
          | true ->
              Baking_highwatermarks.record_preendorsement
                cctxt
                block_location
                ~delegate:consensus_key.public_key_hash
                ~level
                ~round
              >>=? fun () -> return_true
          | false -> return state.global_state.config.force)
      >>=? fun may_sign ->
      (if may_sign then
       let unsigned_operation = (shell, Contents_list contents) in
       let watermark = Operation.(to_watermark (Preendorsement chain_id)) in
       let unsigned_operation_bytes =
         Data_encoding.Binary.to_bytes_exn
           Operation.unsigned_encoding
           unsigned_operation
       in
       Client_keys.sign cctxt ~watermark sk_uri unsigned_operation_bytes
      else
        fail (Baking_highwatermarks.Block_previously_preendorsed {round; level}))
      >>= function
      | Error err ->
          Events.(emit skipping_preendorsement (delegate, err)) >>= fun () ->
          return_none
      | Ok signature ->
          let protocol_data =
            Operation_data {contents; signature = Some signature}
          in
          let operation : Operation.packed = {shell; protocol_data} in
          return_some (delegate, operation, level, round))
    preendorsements
  >>=? fun signed_operations ->
  (* TODO: add a RPC to inject multiple operations *)
  List.iter_ep
    (fun (delegate, operation, level, round) ->
      protect
        ~on_error:(fun err ->
          Events.(emit failed_to_inject_preendorsement (delegate, err))
          >>= fun () -> return_unit)
        (fun () ->
          Node_rpc.inject_operation cctxt ~chain:(`Hash chain_id) operation
          >>=? fun oph ->
          Events.(emit preendorsement_injected (oph, delegate, level, round))
          >>= fun () -> return_unit))
    signed_operations

let sign_endorsements state endorsements =
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
      Events.(emit signing_endorsement delegate) >>= fun () ->
      let shell =
        (* The branch is the latest finalized block. *)
        {
          Tezos_base.Operation.branch =
            state.level_state.latest_proposal.predecessor.shell.predecessor;
        }
      in
      let contents =
        (* No preendorsements are included *)
        Single (Endorsement consensus_content)
      in
      let level = Raw_level.to_int32 consensus_content.level in
      let round = consensus_content.round in
      let sk_uri = consensus_key.secret_key_uri in
      cctxt#with_lock (fun () ->
          Baking_highwatermarks.may_sign_endorsement
            cctxt
            block_location
            ~delegate:consensus_key.public_key_hash
            ~level
            ~round
          >>=? function
          | true ->
              Baking_highwatermarks.record_endorsement
                cctxt
                block_location
                ~delegate:consensus_key.public_key_hash
                ~level
                ~round
              >>=? fun () -> return_true
          | false -> return state.global_state.config.force)
      >>=? fun may_sign ->
      (if may_sign then
       let watermark = Operation.(to_watermark (Endorsement chain_id)) in
       let unsigned_operation = (shell, Contents_list contents) in
       let unsigned_operation_bytes =
         Data_encoding.Binary.to_bytes_exn
           Operation.unsigned_encoding
           unsigned_operation
       in
       Client_keys.sign cctxt ~watermark sk_uri unsigned_operation_bytes
      else fail (Baking_highwatermarks.Block_previously_endorsed {round; level}))
      >>= function
      | Error err ->
          Events.(emit skipping_endorsement (delegate, err)) >>= fun () ->
          return_none
      | Ok signature ->
          let protocol_data =
            Operation_data {contents; signature = Some signature}
          in
          let operation : Operation.packed = {shell; protocol_data} in
          return_some (delegate, operation, level, round))
    endorsements

let sign_dal_attestations state attestations =
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
    (fun (((consensus_key, _) as delegate), consensus_content) ->
      let watermark = Operation.(to_watermark (Dal_attestation chain_id)) in
      let contents = Single (Dal_attestation consensus_content) in
      let unsigned_operation = (shell, Contents_list contents) in
      let unsigned_operation_bytes =
        Data_encoding.Binary.to_bytes_exn
          Operation.unsigned_encoding
          unsigned_operation
      in
      Client_keys.sign
        cctxt
        ~watermark
        consensus_key.secret_key_uri
        unsigned_operation_bytes
      >>= function
      | Error err ->
          Events.(emit skipping_attestation (delegate, err)) >>= fun () ->
          return_none
      | Ok signature ->
          let protocol_data =
            Operation_data {contents; signature = Some signature}
          in
          let operation : Operation.packed = {shell; protocol_data} in
          return_some
            (delegate, operation, consensus_content.Dal.Attestation.attestation))
    attestations

let inject_endorsements state ~endorsements =
  let cctxt = state.global_state.cctxt in
  let chain_id = state.global_state.chain_id in
  sign_endorsements state endorsements >>=? fun signed_operations ->
  (* TODO: add a RPC to inject multiple operations *)
  List.iter_ep
    (fun (delegate, operation, level, round) ->
      protect
        ~on_error:(fun err ->
          Events.(emit failed_to_inject_endorsement (delegate, err))
          >>= fun () -> return_unit)
        (fun () ->
          Node_rpc.inject_operation cctxt ~chain:(`Hash chain_id) operation
          >>=? fun oph ->
          Events.(emit endorsement_injected (oph, delegate, level, round))
          >>= fun () -> return_unit))
    signed_operations

let inject_dal_attestations state attestations =
  let cctxt = state.global_state.cctxt in
  let chain_id = state.global_state.chain_id in
  sign_dal_attestations state attestations >>=? fun signed_operations ->
  List.iter_ep
    (fun (delegate, signed_operation, (attestation : Dal.Attestation.t)) ->
      let encoded_op =
        Data_encoding.Binary.to_bytes_exn Operation.encoding signed_operation
      in
      Shell_services.Injection.operation
        cctxt
        ~chain:(`Hash chain_id)
        encoded_op
      >>=? fun oph ->
      let bitset_int = Bitset.to_z (attestation :> Bitset.t) in
      Events.(emit attestation_injected (oph, delegate, bitset_int))
      >>= fun () -> return_unit)
    signed_operations

let no_dal_node_warning_counter = ref 0

let only_if_dal_feature_enabled state ~default_value f =
  let open Constants in
  let Parametric.{dal = {feature_enable; _}; _} =
    state.global_state.constants.parametric
  in
  if feature_enable then
    match state.global_state.dal_node_rpc_ctxt with
    | None ->
        incr no_dal_node_warning_counter ;
        (if !no_dal_node_warning_counter mod 10 = 1 then
         Events.(emit no_dal_node ())
        else Lwt.return_unit)
        >>= fun () -> return default_value
    | Some ctxt -> f ctxt
  else return default_value

let get_dal_attestations state ~level =
  only_if_dal_feature_enabled state ~default_value:[] (fun dal_node_rpc_ctxt ->
      let delegates =
        SlotMap.bindings state.level_state.delegate_slots.own_delegate_slots
        |> List.map (fun (_slot, (consensus_key_and_delegate, _slots)) ->
               consensus_key_and_delegate)
        |> List.sort_uniq compare
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/4666
           This sorting can be avoided. *)
      in
      let signing_key delegate = (fst delegate).public_key_hash in
      List.fold_left_es
        (fun acc delegate ->
          Node_rpc.get_attestable_slots
            dal_node_rpc_ctxt
            (signing_key delegate)
            ~level
          >>=? fun res ->
          match res with
          | Tezos_dal_node_services.Services.Types.Not_in_committee ->
              return acc
          | Attestable_slots attestation ->
              return ((delegate, attestation) :: acc))
        []
        delegates
      >>=? fun attestations ->
      List.map
        (fun (delegate, attestation_flags) ->
          let attestation =
            List.fold_left_i
              (fun i acc flag ->
                match Dal.Slot_index.of_int_opt i with
                | Some index when flag -> Dal.Attestation.commit acc index
                | None | Some _ -> acc)
              Dal.Attestation.empty
              attestation_flags
          in
          ( delegate,
            Dal.Attestation.
              {
                attestor = signing_key delegate;
                attestation;
                level = Raw_level.of_int32_exn level;
              } ))
        attestations
      |> return)

let get_and_inject_dal_attestations state =
  let level = Int32.succ state.level_state.current_level in
  get_dal_attestations state ~level >>=? fun attestations ->
  inject_dal_attestations state attestations

let prepare_waiting_for_quorum state =
  let consensus_threshold =
    state.global_state.constants.parametric.consensus_threshold
  in
  let get_slot_voting_power ~slot =
    match
      SlotMap.find slot state.level_state.delegate_slots.all_delegate_slots
    with
    | Some {endorsing_power; first_slot} when Slot.equal slot first_slot ->
        Some endorsing_power
    | Some _ | None (* cannot happen if the map is correctly populated *) ->
        None
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

let start_waiting_for_preendorsement_quorum state =
  let consensus_threshold, get_slot_voting_power, candidate =
    prepare_waiting_for_quorum state
  in
  let operation_worker = state.global_state.operation_worker in
  Operation_worker.monitor_preendorsement_quorum
    operation_worker
    ~consensus_threshold
    ~get_slot_voting_power
    candidate

let start_waiting_for_endorsement_quorum state =
  let consensus_threshold, get_slot_voting_power, candidate =
    prepare_waiting_for_quorum state
  in
  let operation_worker = state.global_state.operation_worker in
  Operation_worker.monitor_endorsement_quorum
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
  let {new_level_proposal; compute_new_state} = level_update in
  let cctxt = state.global_state.cctxt in
  let delegates = state.global_state.delegates in
  let new_level = new_level_proposal.block.shell.level in
  let chain = `Hash state.global_state.chain_id in
  (* Sync the context to clean-up potential GC artifacts *)
  (match state.global_state.validation_mode with
  | Node -> Lwt.return_unit
  | Local index -> index.sync_fun ())
  >>= fun () ->
  (if Int32.(new_level = succ state.level_state.current_level) then
   return state.level_state.next_level_delegate_slots
  else
    Baking_state.compute_delegate_slots cctxt delegates ~level:new_level ~chain)
  >>=? fun delegate_slots ->
  Baking_state.compute_delegate_slots
    cctxt
    delegates
    ~level:(Int32.succ new_level)
    ~chain
  >>=? fun next_level_delegate_slots ->
  let round_durations = state.global_state.round_durations in
  compute_round new_level_proposal round_durations >>?= fun current_round ->
  compute_new_state ~current_round ~delegate_slots ~next_level_delegate_slots
  >>= return

let synchronize_round state {new_round_proposal; handle_proposal} =
  Events.(emit synchronizing_round new_round_proposal.predecessor.hash)
  >>= fun () ->
  let round_durations = state.global_state.round_durations in
  compute_round new_round_proposal round_durations >>?= fun current_round ->
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
    handle_proposal new_state >>= return

(* TODO: https://gitlab.com/tezos/tezos/-/issues/4539
   Avoid updating the state here.
   (See also comment in {!State_transitions.step}.)

   TODO: https://gitlab.com/tezos/tezos/-/issues/4538
   Improve/clarify when the state is recorded.
*)
let rec perform_action ~state_recorder state (action : action) =
  match action with
  | Do_nothing -> state_recorder ~new_state:state >>=? fun () -> return state
  | Inject_block {block_to_bake; updated_state} ->
      inject_block state ~state_recorder block_to_bake ~updated_state
  | Inject_preendorsements {preendorsements} ->
      inject_preendorsements state ~preendorsements >>=? fun () ->
      perform_action ~state_recorder state Watch_proposal
  | Inject_endorsements {endorsements} ->
      state_recorder ~new_state:state >>=? fun () ->
      inject_endorsements state ~endorsements >>=? fun () ->
      (* We wait for endorsements to trigger the [Quorum_reached]
         event *)
      start_waiting_for_endorsement_quorum state >>= fun () ->
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/4667
         Also inject attestations for the migration block. *)
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/4671
         Don't inject multiple attestations? *)
      get_and_inject_dal_attestations state >>=? fun () -> return state
  | Update_to_level level_update ->
      update_to_level state level_update >>=? fun (new_state, new_action) ->
      perform_action ~state_recorder new_state new_action
  | Synchronize_round round_update ->
      synchronize_round state round_update >>=? fun (new_state, new_action) ->
      perform_action ~state_recorder new_state new_action
  | Watch_proposal ->
      (* We wait for preendorsements to trigger the
           [Prequorum_reached] event *)
      start_waiting_for_preendorsement_quorum state >>= fun () -> return state
