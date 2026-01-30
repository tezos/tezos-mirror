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
open Baking_errors
open Baking_state_types
module Profiler = Baking_profiler.Baker_profiler

type validation_mode = Node | Local of Abstract_context_index.t

type cache = {
  known_timestamps : Timestamp.time Baking_cache.Timestamp_of_round_cache.t;
}

let prequorum_encoding =
  let open Data_encoding in
  conv
    (fun {level; round; block_payload_hash; preattestations} ->
      (level, round, block_payload_hash, preattestations))
    (fun (level, round, block_payload_hash, preattestations) ->
      {level; round; block_payload_hash; preattestations})
    (obj4
       (req "level" int32)
       (req "round" Round.encoding)
       (req "block_payload_hash" Block_payload_hash.encoding)
       (req "preattestations" (list (dynamic_size Operation.encoding))))

let block_info_encoding =
  let open Data_encoding in
  conv
    (fun {
           hash;
           shell;
           payload_hash;
           payload_round;
           round;
           prequorum;
           quorum;
           payload;
           grandparent;
         }
       ->
      ( hash,
        shell,
        payload_hash,
        payload_round,
        round,
        prequorum,
        quorum,
        payload,
        grandparent ))
    (fun ( hash,
           shell,
           payload_hash,
           payload_round,
           round,
           prequorum,
           quorum,
           payload,
           grandparent )
       ->
      {
        hash;
        shell;
        grandparent;
        payload_hash;
        payload_round;
        round;
        prequorum;
        quorum;
        payload;
      })
    (obj9
       (req "hash" Block_hash.encoding)
       (req "shell" Block_header.shell_header_encoding)
       (req "payload_hash" Block_payload_hash.encoding)
       (req "payload_round" Round.encoding)
       (req "round" Round.encoding)
       (req "prequorum" (option prequorum_encoding))
       (req "quorum" (list (dynamic_size Operation.encoding)))
       (req "payload" Operation_pool.payload_encoding)
       (req "grandparent" Block_hash.encoding))

module SlotMap : Map.S with type key = Slot.t = Map.Make (Slot)

module RoundMap : Map.S with type key = Round.t = Map.Make (Round)

module Delegate_infos = struct
  type t = {
    own_delegates : delegate_info list;
    own_delegate_rounds : delegate_info RoundMap.t;
        (* This map cannot have as keys just the first round of delegates,
           because it is used in [round_proposer] for which we need all slots,
           as the round can be arbitrary. *)
    all_delegate_voting_power : int64 SlotMap.t;
        (* This is a map having as keys the attestation slot of all delegates, and as
           values their attesting power.
           Without all bakers attest, this map contains just the first slot for a delegate, because it is
           only used in [slot_voting_power] which is about (pre)attestations,
           not proposals. Indeed, only (pre)attestations that use the delegate's
           first slot are valid for inclusion in a block and count toward the
           (pre)quorum. Note that the baker might receive nominally valid
           non-first-slot operations from the mempool because this check is
           skipped in the mempool to increase its speed; the baker can and
           should ignore such operations. *)
    consensus_threshold : int64;
    consensus_committee : int64;
  }

  let own_delegates t = t.own_delegates

  let own_delegate_ids t =
    List.map
      (fun delegate_info ->
        Baking_state_types.Delegate.delegate_id delegate_info.delegate)
      t.own_delegates

  let own_round_owner t ~committee_size ~round =
    let open Result_syntax in
    let* round_int = Round.to_int round |> Environment.wrap_tzresult in
    let* round_rem =
      Round.of_int (round_int mod committee_size) |> Environment.wrap_tzresult
    in
    return @@ RoundMap.find round_rem t.own_delegate_rounds

  let find_first_round_from t ~round =
    RoundMap.find_first (fun s -> Round.(s >= round)) t.own_delegate_rounds

  let min_round t = RoundMap.min_binding t.own_delegate_rounds

  let voting_power t ~slot = SlotMap.find slot t.all_delegate_voting_power

  let consensus_threshold {consensus_threshold; _} = consensus_threshold

  let consensus_committee {consensus_committee; _} = consensus_committee
end

type delegate_infos = Delegate_infos.t

let proposal_encoding =
  let open Data_encoding in
  conv
    (fun {block; predecessor} -> (block, predecessor))
    (fun (block, predecessor) -> {block; predecessor})
    (obj2
       (req "block" block_info_encoding)
       (req "predecessor" block_info_encoding))

let is_first_block_in_protocol {block; predecessor; _} =
  Compare.Int.(block.shell.proto_level <> predecessor.shell.proto_level)

type locked_round = {payload_hash : Block_payload_hash.t; round : Round.t}

let locked_round_encoding =
  let open Data_encoding in
  conv
    (fun {payload_hash; round} -> (payload_hash, round))
    (fun (payload_hash, round) -> {payload_hash; round})
    (obj2
       (req "payload_hash" Block_payload_hash.encoding)
       (req "round" Round.encoding))

type attestable_payload = {proposal : proposal; prequorum : prequorum}

let attestable_payload_encoding =
  let open Data_encoding in
  conv
    (fun {proposal; prequorum} -> (proposal, prequorum))
    (fun (proposal, prequorum) -> {proposal; prequorum})
    (obj2
       (req "proposal" proposal_encoding)
       (req "prequorum" prequorum_encoding))

type elected_block = {
  proposal : proposal;
  attestation_qc : Kind.attestation Operation.t list;
}

type manager_operations_infos = {
  manager_operation_number : int;
  total_fees : Int64.t;
}

type prepared_block = {
  signed_block_header : block_header;
  round : Round.t;
  delegate : Delegate.t;
  operations : Tezos_base.Operation.t list list;
  manager_operations_infos : manager_operations_infos option;
  baking_votes : Per_block_votes_repr.per_block_votes;
}

(* The fields {current_level}, {delegate_infos}, {next_level_delegate_infos},
   {next_level_latest_forge_request} are updated only when we receive a block at a different
   level than {current_level}.  Note that this means that there is always a {latest_proposal},
   which may be our own baked block. *)
type level_state = {
  current_level : int32;
  latest_proposal : proposal;
  is_latest_proposal_applied : bool;
  (* Last proposal received where we injected an attestation (thus we
     have seen 2f+1 preattestations) *)
  locked_round : locked_round option;
  (* Latest payload where we've seen a proposal reach 2f+1 preattestations *)
  attestable_payload : attestable_payload option;
  (* Block for which we've seen 2f+1 attestations and that we may bake onto *)
  elected_block : elected_block option;
  delegate_infos : delegate_infos;
  next_level_delegate_infos : delegate_infos;
  next_level_latest_forge_request : Round.t option;
}

type phase =
  | Idle
  | Awaiting_preattestations
  | Awaiting_attestations
  | Awaiting_application

let phase_encoding =
  let open Data_encoding in
  union
    ~tag_size:`Uint8
    [
      case
        ~title:"Idle"
        (Tag 0)
        (constant "Idle")
        (function Idle -> Some () | _ -> None)
        (fun () -> Idle);
      case
        ~title:"Awaiting_preattestations"
        (Tag 1)
        (constant "Awaiting_preattestations")
        (function Awaiting_preattestations -> Some () | _ -> None)
        (fun () -> Awaiting_preattestations);
      case
        ~title:"Awaiting_application"
        (Tag 2)
        (constant "Awaiting_application")
        (function Awaiting_application -> Some () | _ -> None)
        (fun () -> Awaiting_application);
      case
        ~title:"Awaiting_attestationss"
        (Tag 3)
        (constant "Awaiting_attestationss")
        (function Awaiting_attestations -> Some () | _ -> None)
        (fun () -> Awaiting_attestations);
    ]

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
  delegate : Delegate.t;
  kind : block_kind;
  force_apply : bool;
}

type consensus_vote_kind = Attestation | Preattestation

let consensus_vote_kind_encoding =
  Data_encoding.string_enum
    [("preattestation", Preattestation); ("attestation", Attestation)]

let pp_consensus_vote_kind fmt = function
  | Attestation -> Format.fprintf fmt "attestation"
  | Preattestation -> Format.fprintf fmt "preattestation"

type unsigned_consensus_vote = {
  vote_kind : consensus_vote_kind;
  vote_consensus_content : consensus_content;
  delegate : Delegate.t;
  dal_content : dal_content option;
}

type batch_content = {
  level : Raw_level.t;
  round : Round.t;
  block_payload_hash : Block_payload_hash.t;
}

type unsigned_consensus_vote_batch = {
  batch_kind : consensus_vote_kind;
  batch_content : batch_content;
  batch_branch : Block_hash.t;
  unsigned_consensus_votes : unsigned_consensus_vote list;
}

let make_unsigned_consensus_vote_batch kind
    ({level; round; block_payload_hash} as batch_content) ~batch_branch
    delegates_and_slots =
  let unsigned_consensus_votes =
    List.map
      (fun (delegate, slot) ->
        let consensus_content = {level; round; slot; block_payload_hash} in
        {
          vote_kind = kind;
          vote_consensus_content = consensus_content;
          delegate;
          dal_content = None;
        })
      delegates_and_slots
  in
  {batch_kind = kind; batch_branch; batch_content; unsigned_consensus_votes}

let dal_content_map_p f unsigned_consensus_vote_batch =
  let open Lwt_syntax in
  let* patched_unsigned_consensus_votes =
    List.map_p
      (fun unsigned_consensus_vote ->
        let fallback_case =
          {
            unsigned_consensus_vote with
            dal_content = Some {attestation = Dal.Attestation.empty};
          }
        in
        Lwt.catch
          (fun () ->
            let* dal_content = f unsigned_consensus_vote in
            return {unsigned_consensus_vote with dal_content})
          (fun _exn -> return fallback_case))
      unsigned_consensus_vote_batch.unsigned_consensus_votes
  in
  return
    {
      unsigned_consensus_vote_batch with
      unsigned_consensus_votes = patched_unsigned_consensus_votes;
    }

type signed_consensus_vote = {
  unsigned_consensus_vote : unsigned_consensus_vote;
  signed_operation : packed_operation;
}

type signed_consensus_vote_batch = {
  batch_kind : consensus_vote_kind;
  batch_content : batch_content;
  batch_branch : Block_hash.t;
  signed_consensus_votes : signed_consensus_vote list;
}

type error += Mismatch_signed_consensus_vote_in_batch

let () =
  register_error_kind
    `Permanent
    ~id:"Baking_state.mismatch_signed_consensus_vote_in_batch"
    ~title:"Mismatch signed consensus vote in batch"
    ~description:"Consensus votes mismatch while creating a batch."
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "There are batched consensus votes which are not of the same kind or \
         do not have the same consensus content as the rest.")
    Data_encoding.unit
    (function Mismatch_signed_consensus_vote_in_batch -> Some () | _ -> None)
    (fun () -> Mismatch_signed_consensus_vote_in_batch)

type error += Unauthorised_dal_content_in_preattestation

let () =
  register_error_kind
    `Permanent
    ~id:"Baking_state.unauthorised_dal_content_in_preattestation"
    ~title:"Unauthorised dal content in preattestation"
    ~description:"Unauthorised dal content in preattestation."
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "There are batched consensus votes which are not of the same kind or \
         do not have the same consensus content as the rest.")
    Data_encoding.unit
    (function
      | Unauthorised_dal_content_in_preattestation -> Some () | _ -> None)
    (fun () -> Unauthorised_dal_content_in_preattestation)

let make_signed_consensus_vote_batch batch_kind (batch_content : batch_content)
    ~batch_branch signed_consensus_votes =
  let open Result_syntax in
  let* () =
    List.iter_e
      (fun {unsigned_consensus_vote; signed_operation = _} ->
        let* () =
          error_when
            (unsigned_consensus_vote.vote_kind <> batch_kind
            || Raw_level.(
                 unsigned_consensus_vote.vote_consensus_content.level
                 <> batch_content.level)
            || Round.(
                 unsigned_consensus_vote.vote_consensus_content.round
                 <> batch_content.round)
            || Block_payload_hash.(
                 unsigned_consensus_vote.vote_consensus_content
                   .block_payload_hash <> batch_content.block_payload_hash))
            Mismatch_signed_consensus_vote_in_batch
        in
        match batch_kind with
        | Preattestation ->
            error_when
              (Option.is_some unsigned_consensus_vote.dal_content)
              Unauthorised_dal_content_in_preattestation
        | Attestation -> return_unit)
      signed_consensus_votes
  in
  return {batch_kind; batch_content; batch_branch; signed_consensus_votes}

let make_singleton_consensus_vote_batch
    (signed_consensus_vote : signed_consensus_vote) =
  let {unsigned_consensus_vote; _} = signed_consensus_vote in
  let batch_content =
    {
      level = unsigned_consensus_vote.vote_consensus_content.level;
      round = unsigned_consensus_vote.vote_consensus_content.round;
      block_payload_hash =
        unsigned_consensus_vote.vote_consensus_content.block_payload_hash;
    }
  in
  {
    batch_kind = unsigned_consensus_vote.vote_kind;
    batch_content;
    batch_branch = signed_consensus_vote.signed_operation.shell.branch;
    signed_consensus_votes = [signed_consensus_vote];
  }

type round_state = {
  current_round : Round.t;
  current_phase : phase;
  delayed_quorum : Kind.attestation operation list option;
  early_attestations : signed_consensus_vote list;
  awaiting_unlocking_pqc : bool;
}

type forge_event =
  | Block_ready of prepared_block
  | Preattestation_ready of signed_consensus_vote
  | Attestation_ready of signed_consensus_vote

type forge_request =
  | Forge_and_sign_block of block_to_bake
  | Forge_and_sign_preattestations of {
      unsigned_preattestations : unsigned_consensus_vote_batch;
    }
  | Forge_and_sign_attestations of {
      unsigned_attestations : unsigned_consensus_vote_batch;
    }

type forge_worker_hooks = {
  push_request : forge_request -> unit;
  get_forge_event_stream : unit -> forge_event Lwt_stream.t;
  cancel_all_pending_tasks : unit -> unit;
}

type global_state = {
  (* client context *)
  cctxt : Protocol_client_context.full;
  (* chain id *)
  chain_id : Chain_id.t;
  (* baker configuration *)
  config : Baking_configuration.t;
  (* protocol constants *)
  constants : Constants.t;
  (* round durations *)
  round_durations : Round.round_durations;
  (* worker that monitors and aggregates new operations *)
  operation_worker : Operation_worker.t;
  (* worker that retrieves DAL attestable slots from the DAL node *)
  dal_attestable_slots_worker : Dal_attestable_slots_worker.t;
  (* hooks to the consensus and block forge worker *)
  mutable forge_worker_hooks : forge_worker_hooks;
  (* the validation mode used by the baker*)
  validation_mode : validation_mode;
  (* the delegates on behalf of which the baker is running *)
  delegates : Baking_state_types.Key.t list;
  cache : cache;
  dal_node_rpc_ctxt : Tezos_rpc.Context.generic option;
}

type state = {
  global_state : global_state;
  level_state : level_state;
  round_state : round_state;
}

type t = state

let update_current_phase state new_phase =
  {state with round_state = {state.round_state with current_phase = new_phase}}

type timeout_kind =
  | End_of_round of {ending_round : Round.t}
  | Time_to_prepare_next_level_block of {at_round : Round.t}

let timeout_kind_encoding =
  let open Data_encoding in
  union
    [
      case
        (Tag 0)
        ~title:"End_of_round"
        (obj2
           (req "kind" (constant "End_of_round"))
           (req "round" Round.encoding))
        (function
          | End_of_round {ending_round} -> Some ((), ending_round) | _ -> None)
        (fun ((), ending_round) -> End_of_round {ending_round});
      case
        (Tag 1)
        ~title:"Time_to_prepare_next_level_block"
        (obj2
           (req "kind" (constant "Time_to_prepare_next_level_block"))
           (req "round" Round.encoding))
        (function
          | Time_to_prepare_next_level_block {at_round} -> Some ((), at_round)
          | _ -> None)
        (fun ((), at_round) -> Time_to_prepare_next_level_block {at_round});
    ]

type event =
  | New_valid_proposal of proposal
  | New_head_proposal of proposal
  | Prequorum_reached of
      Operation_worker.candidate * Kind.preattestation operation list
  | Quorum_reached of
      Operation_worker.candidate * Kind.attestation operation list
  | New_forge_event of forge_event
  | Timeout of timeout_kind

let dal_content_encoding =
  Data_encoding.conv
    (fun {attestation} -> attestation)
    (fun attestation -> {attestation})
    Dal.Attestation.encoding

let unsigned_consensus_vote_encoding_for_logging__cannot_decode =
  let open Data_encoding in
  conv
    (fun {vote_kind; vote_consensus_content; delegate; dal_content} ->
      (vote_kind, vote_consensus_content, delegate, dal_content))
    (fun (vote_kind, vote_consensus_content, delegate, dal_content) ->
      {vote_kind; vote_consensus_content; delegate; dal_content})
    (obj4
       (req "operation_kind" consensus_vote_kind_encoding)
       (req "consensus_content" consensus_content_encoding)
       (req "delegate" Delegate.encoding_for_logging__cannot_decode)
       (opt "dal_content" dal_content_encoding))

let signed_consensus_vote_encoding_for_logging__cannot_decode =
  let open Data_encoding in
  conv
    (fun {unsigned_consensus_vote; signed_operation} ->
      (unsigned_consensus_vote, signed_operation))
    (fun (unsigned_consensus_vote, signed_operation) ->
      {unsigned_consensus_vote; signed_operation})
    (obj2
       (req
          "unsigned_consensus_vote"
          unsigned_consensus_vote_encoding_for_logging__cannot_decode)
       (req "signed_operation" (dynamic_size Operation.encoding)))

let manager_operations_infos_encoding =
  let open Data_encoding in
  conv
    (fun {manager_operation_number; total_fees} ->
      (manager_operation_number, total_fees))
    (fun (manager_operation_number, total_fees) ->
      {manager_operation_number; total_fees})
    (obj2 (req "manager_operation_number" int31) (req "total_fees" int64))

let forge_event_encoding_for_logging__cannot_decode =
  let open Data_encoding in
  let prepared_block_encoding =
    conv
      (fun {
             signed_block_header;
             round;
             delegate;
             operations;
             manager_operations_infos;
             baking_votes;
           }
         ->
        ( signed_block_header,
          round,
          delegate,
          operations,
          manager_operations_infos,
          baking_votes ))
      (fun ( signed_block_header,
             round,
             delegate,
             operations,
             manager_operations_infos,
             baking_votes )
         ->
        {
          signed_block_header;
          round;
          delegate;
          operations;
          manager_operations_infos;
          baking_votes;
        })
      (obj6
         (req "header" (dynamic_size Block_header.encoding))
         (req "round" Round.encoding)
         (req "delegate" Delegate.encoding_for_logging__cannot_decode)
         (req
            "operations"
            (list (list (dynamic_size Tezos_base.Operation.encoding))))
         (opt "operations_infos" manager_operations_infos_encoding)
         (req "baking_votes" Per_block_votes.per_block_votes_encoding))
  in
  union
    [
      case
        (Tag 0)
        ~title:"Block_ready"
        (obj1 (req "signed_block" prepared_block_encoding))
        (function
          | Block_ready prepared_block -> Some prepared_block | _ -> None)
        (fun prepared_block -> Block_ready prepared_block);
      case
        (Tag 1)
        ~title:"Preattestation_ready"
        (obj1
           (req
              "signed_preattestation"
              signed_consensus_vote_encoding_for_logging__cannot_decode))
        (function
          | Preattestation_ready signed_preattestation ->
              Some signed_preattestation
          | _ -> None)
        (fun signed_preattestation ->
          Preattestation_ready signed_preattestation);
      case
        (Tag 2)
        ~title:"Attestation_ready"
        (obj1
           (req
              "signed_attestation"
              signed_consensus_vote_encoding_for_logging__cannot_decode))
        (function
          | Attestation_ready signed_attestation -> Some signed_attestation
          | _ -> None)
        (fun signed_attestation -> Attestation_ready signed_attestation);
    ]

let event_encoding_for_logging__cannot_decode =
  let open Data_encoding in
  union
    [
      case
        (Tag 0)
        ~title:"New_valid_proposal"
        (tup2 (constant "New_valid_proposal") proposal_encoding)
        (function New_valid_proposal p -> Some ((), p) | _ -> None)
        (fun ((), p) -> New_valid_proposal p);
      case
        (Tag 1)
        ~title:"New_head_proposal"
        (tup2 (constant "New_head_proposal") proposal_encoding)
        (function New_head_proposal p -> Some ((), p) | _ -> None)
        (fun ((), p) -> New_head_proposal p);
      case
        (Tag 2)
        ~title:"Prequorum_reached"
        (tup3
           (constant "Prequorum_reached")
           Operation_worker.candidate_encoding
           (Data_encoding.list (dynamic_size Operation.encoding)))
        (function
          | Prequorum_reached (candidate, ops) ->
              Some ((), candidate, List.map Operation.pack ops)
          | _ -> None)
        (fun ((), candidate, ops) ->
          Prequorum_reached
            (candidate, Operation_pool.filter_preattestations ops));
      case
        (Tag 3)
        ~title:"Quorum_reached"
        (tup3
           (constant "Quorum_reached")
           Operation_worker.candidate_encoding
           (Data_encoding.list (dynamic_size Operation.encoding)))
        (function
          | Quorum_reached (candidate, ops) ->
              Some ((), candidate, List.map Operation.pack ops)
          | _ -> None)
        (fun ((), candidate, ops) ->
          Quorum_reached (candidate, Operation_pool.filter_attestations ops));
      case
        (Tag 4)
        ~title:"Timeout"
        (tup2 (constant "Timeout") timeout_kind_encoding)
        (function Timeout tk -> Some ((), tk) | _ -> None)
        (fun ((), tk) -> Timeout tk);
      case
        (Tag 5)
        ~title:"New_forge_event"
        (tup2
           (constant "New_forge_event")
           forge_event_encoding_for_logging__cannot_decode)
        (function New_forge_event event -> Some ((), event) | _ -> None)
        (fun ((), event) -> New_forge_event event);
    ]

(* Disk state *)

module Events = struct
  include Internal_event.Simple

  let section = [Protocol.name; "baker"; "disk"]

  let incompatible_stored_state =
    declare_0
      ~section
      ~name:"incompatible_stored_state"
      ~level:Warning
      ~msg:"found an outdated or corrupted baking state: discarding it"
      ()
end

type state_data = {
  level_data : int32;
  locked_round_data : locked_round option;
  attestable_payload_data : attestable_payload option;
}

let state_data_encoding =
  let open Data_encoding in
  conv
    (fun {level_data; locked_round_data; attestable_payload_data} ->
      (level_data, locked_round_data, attestable_payload_data))
    (fun (level_data, locked_round_data, attestable_payload_data) ->
      {level_data; locked_round_data; attestable_payload_data})
    (obj3
       (req "level" int32)
       (req "locked_round" (option locked_round_encoding))
       (req "attestable_payload" (option attestable_payload_encoding)))

let record_state (state : state) =
  let open Lwt_result_syntax in
  let cctxt = state.global_state.cctxt in
  let location =
    Baking_files.resolve_location ~chain_id:state.global_state.chain_id `State
  in
  let filename =
    Filename.Infix.(cctxt#get_base_dir // Baking_files.filename location)
  in
  protect @@ fun () ->
  cctxt#with_lock @@ fun () ->
  let level_data = state.level_state.current_level in
  let locked_round_data = state.level_state.locked_round in
  let attestable_payload_data = state.level_state.attestable_payload in
  let bytes =
    Data_encoding.Binary.to_bytes_exn
      state_data_encoding
      {level_data; locked_round_data; attestable_payload_data}
  in
  let filename_tmp = filename ^ "_tmp" in
  let*! () =
    Lwt_io.with_file
      ~flags:[Unix.O_CREAT; O_WRONLY; O_TRUNC; O_CLOEXEC; O_SYNC]
      ~mode:Output
      filename_tmp
      (fun channel ->
        Lwt_io.write_from_exactly channel bytes 0 (Bytes.length bytes))
  in
  let*! () = Lwt_unix.rename filename_tmp filename in
  return_unit

let may_record_new_state ~previous_state ~new_state =
  let open Lwt_result_syntax in
  if new_state.global_state.config.state_recorder = Baking_configuration.Memory
  then return_unit
  else
    let {
      current_level = previous_current_level;
      locked_round = previous_locked_round;
      attestable_payload = previous_attestable_payload;
      _;
    } =
      previous_state.level_state
    in
    let {
      current_level = new_current_level;
      locked_round = new_locked_round;
      attestable_payload = new_attestable_payload;
      _;
    } =
      new_state.level_state
    in
    let is_new_state_consistent =
      Compare.Int32.(new_current_level > previous_current_level)
      || new_current_level = previous_current_level
         &&
         if Compare.Int32.(new_current_level = previous_current_level) then
           let is_new_locked_round_consistent =
             match (new_locked_round, previous_locked_round) with
             | None, None -> true
             | Some _, None -> true
             | None, Some _ -> false
             | Some new_locked_round, Some previous_locked_round ->
                 Round.(new_locked_round.round >= previous_locked_round.round)
           in
           let is_new_attestable_payload_consistent =
             match (new_attestable_payload, previous_attestable_payload) with
             | None, None -> true
             | Some _, None -> true
             | None, Some _ -> false
             | Some new_attestable_payload, Some previous_attestable_payload ->
                 Round.(
                   new_attestable_payload.proposal.block.round
                   >= previous_attestable_payload.proposal.block.round)
           in
           is_new_locked_round_consistent
           && is_new_attestable_payload_consistent
         else true
    in
    let* () =
      fail_unless is_new_state_consistent Broken_locked_values_invariant
    in
    let has_not_changed =
      previous_state.level_state.current_level
      == new_state.level_state.current_level
      && previous_state.level_state.locked_round
         == new_state.level_state.locked_round
      && previous_state.level_state.attestable_payload
         == new_state.level_state.attestable_payload
    in
    if has_not_changed then return_unit else record_state new_state

let load_attestable_data cctxt location =
  let open Lwt_result_syntax in
  protect (fun () ->
      let filename =
        Filename.Infix.(cctxt#get_base_dir // Baking_files.filename location)
      in
      let*! exists = Lwt_unix.file_exists filename in
      match exists with
      | false -> return_none
      | true ->
          Lwt_io.with_file
            ~flags:[Unix.O_EXCL; O_RDONLY; O_CLOEXEC]
            ~mode:Input
            filename
            (fun channel ->
              let*! str = Lwt_io.read channel in
              match
                Data_encoding.Binary.of_string_opt state_data_encoding str
              with
              | Some state_data -> return_some state_data
              | None ->
                  (* The stored state format is incompatible: discard it. *)
                  let*! () = Events.(emit incompatible_stored_state ()) in
                  return_none))

let may_load_attestable_data state =
  let open Lwt_result_syntax in
  let cctxt = state.global_state.cctxt in
  let chain_id = state.global_state.chain_id in
  let location = Baking_files.resolve_location ~chain_id `State in
  protect ~on_error:(fun _ -> return state) @@ fun () ->
  cctxt#with_lock @@ fun () ->
  let* attestable_data_opt = load_attestable_data cctxt location in
  match attestable_data_opt with
  | None -> return state
  | Some {level_data; locked_round_data; attestable_payload_data} ->
      if Compare.Int32.(state.level_state.current_level = level_data) then
        let loaded_level_state =
          {
            state.level_state with
            locked_round = locked_round_data;
            attestable_payload = attestable_payload_data;
          }
        in
        return {state with level_state = loaded_level_state}
      else return state

(* Helpers *)

let delegate_infos attesting_rights delegates =
  let open Lwt_syntax in
  let known_keys = Key.Set.of_list delegates in
  match attesting_rights with
  | [] | _ :: _ :: _ -> assert false
  | [
   {
     Plugin.RPC.Validators.level = _;
     consensus_threshold;
     consensus_committee;
     abaab_activation_flag = _;
     delegates = attesting_rights;
   };
  ] ->
      let* ( own_delegate_first_slots,
             own_delegate_rounds,
             all_delegate_voting_power ) =
        Lwt_list.fold_left_s
          (fun (own_list, own_map, all_map) validator ->
            let {
              Plugin.RPC.Validators.rounds;
              attesting_power;
              attestation_slot;
              _;
            } =
              validator
            in
            let all_map =
              SlotMap.add attestation_slot attesting_power all_map
            in
            let* own_list, own_map =
              let* delegate_opt = Delegate.of_validator ~known_keys validator in
              match delegate_opt with
              | None -> return (own_list, own_map)
              | Some delegate ->
                  let attesting_slot =
                    {delegate; attestation_slot; attesting_power}
                  in
                  return
                    ( attesting_slot :: own_list,
                      List.fold_left
                        (fun own_map round ->
                          RoundMap.add round attesting_slot own_map)
                        own_map
                        rounds )
            in
            return (own_list, own_map, all_map))
          ([], RoundMap.empty, SlotMap.empty)
          attesting_rights
      in
      return
        {
          Delegate_infos.own_delegates = own_delegate_first_slots;
          own_delegate_rounds;
          all_delegate_voting_power;
          consensus_threshold;
          consensus_committee;
        }

let compute_delegate_infos (cctxt : Protocol_client_context.full)
    ?(block = `Head 0) ~level ~chain delegates =
  let open Lwt_result_syntax in
  let* attesting_rights =
    Node_rpc.get_validators cctxt ~chain ~block ~levels:[level] ()
  in
  let*! delegate_infos =
    (delegate_infos
       attesting_rights
       delegates [@profiler.record_f {verbosity = Debug} "delegate_infos"])
  in
  return delegate_infos

let round_proposer state ~level round =
  let slots =
    match level with
    | `Current -> state.level_state.delegate_infos
    | `Next -> state.level_state.next_level_delegate_infos
  in
  let committee_size =
    state.global_state.constants.parametric.consensus_committee_size
  in
  match Delegate_infos.own_round_owner slots ~round ~committee_size with
  | Error _ -> None
  | Ok owner -> owner

let cache_size_limit = 100

let create_cache () =
  let open Baking_cache in
  {known_timestamps = Timestamp_of_round_cache.create cache_size_limit}

(** Memoization wrapper for [Round.timestamp_of_round]. *)
let timestamp_of_round state ~predecessor_timestamp ~predecessor_round ~round =
  let open Result_syntax in
  let open Baking_cache in
  let known_timestamps = state.global_state.cache.known_timestamps in
  match
    Timestamp_of_round_cache.find_opt
      known_timestamps
      (predecessor_timestamp, predecessor_round, round)
  with
  (* Compute and register the timestamp if not already existing. *)
  | None ->
      let* ts =
        Environment.wrap_tzresult
        @@ Protocol.Alpha_context.Round.timestamp_of_round
             state.global_state.round_durations
             ~predecessor_timestamp
             ~predecessor_round
             ~round
      in
      Timestamp_of_round_cache.replace
        known_timestamps
        (predecessor_timestamp, predecessor_round, round)
        ts ;
      return ts
  (* If it already exists, just fetch from the memoization table. *)
  | Some ts -> return ts

let compute_next_round_time state =
  let proposal =
    match state.level_state.attestable_payload with
    | None -> state.level_state.latest_proposal
    | Some {proposal; _} -> proposal
  in
  if is_first_block_in_protocol proposal then None
  else
    let predecessor_timestamp = proposal.predecessor.shell.timestamp in
    let predecessor_round = proposal.predecessor.round in
    let next_round = Round.succ state.round_state.current_round in
    match
      timestamp_of_round
        state
        ~predecessor_timestamp
        ~predecessor_round
        ~round:next_round
    with
    | Ok timestamp -> Some (timestamp, next_round)
    | _ -> assert false

(* Pretty-printers *)

let pp_validation_mode fmt = function
  | Node -> Format.fprintf fmt "node"
  | Local _ -> Format.fprintf fmt "local"

let pp_global_state fmt {chain_id; config; validation_mode; delegates; _} =
  Format.fprintf
    fmt
    "@[<v 2>Global state:@ chain_id: %a@ @[<v 2>config:@ %a@]@ \
     validation_mode: %a@ @[<v 2>delegates:@ %a@]@]"
    Chain_id.pp
    chain_id
    Baking_configuration.pp
    config
    pp_validation_mode
    validation_mode
    Format.(pp_print_list Baking_state_types.Key.pp)
    delegates

let pp_option pp fmt = function
  | None -> Format.fprintf fmt "none"
  | Some v -> Format.fprintf fmt "%a" pp v

let pp_prequorum fmt {level; round; block_payload_hash; preattestations} =
  Format.fprintf
    fmt
    "level: %ld, round: %a, payload_hash: %a, preattestations: %d"
    level
    Round.pp
    round
    Block_payload_hash.pp_short
    block_payload_hash
    (List.length preattestations)

let pp_block_info fmt
    {
      hash;
      shell;
      grandparent = _;
      payload_hash;
      round;
      prequorum;
      quorum;
      payload;
      payload_round;
    } =
  Format.fprintf
    fmt
    "@[<v 2>Block:@ hash: %a@ payload_hash: %a@ level: %ld@ round: %a@ \
     prequorum: %a@ quorum: %d attestations@ payload: %a@ payload round: %a@]"
    Block_hash.pp
    hash
    Block_payload_hash.pp_short
    payload_hash
    shell.level
    Round.pp
    round
    (pp_option pp_prequorum)
    prequorum
    (List.length quorum)
    Operation_pool.pp_payload
    payload
    Round.pp
    payload_round

let pp_proposal fmt {block; _} = pp_block_info fmt block

let pp_locked_round fmt ({payload_hash; round} : locked_round) =
  Format.fprintf
    fmt
    "payload hash: %a, round: %a"
    Block_payload_hash.pp_short
    payload_hash
    Round.pp
    round

let pp_attestable_payload fmt {proposal; prequorum} =
  Format.fprintf
    fmt
    "proposal: %a, prequorum: %a"
    Block_hash.pp
    proposal.block.hash
    pp_prequorum
    prequorum

let pp_elected_block fmt {proposal; attestation_qc} =
  Format.fprintf
    fmt
    "@[<v 2>%a@ nb quorum attestations: %d@]"
    pp_block_info
    proposal.block
    (List.length attestation_qc)

let pp_delegate_info fmt {delegate; attestation_slot; attesting_power} =
  Format.fprintf
    fmt
    "slots: @[<h>attestation_slot: %a@],@ delegate: %a,@ attesting_power: %Ld"
    Slot.pp
    attestation_slot
    Delegate.pp
    delegate
    attesting_power

(* this type is only used below for pretty-printing *)
type delegate_infos_for_pp = {attester : Delegate.t; all_rounds : Round.t list}

let delegate_infos_for_pp delegate_info_map =
  RoundMap.fold
    (fun round {delegate; attestation_slot; attesting_power = _} acc ->
      match SlotMap.find attestation_slot acc with
      | None ->
          SlotMap.add
            attestation_slot
            {attester = delegate; all_rounds = [round]}
            acc
      | Some {attester; all_rounds} ->
          SlotMap.add
            attestation_slot
            {attester; all_rounds = round :: all_rounds}
            acc)
    delegate_info_map
    SlotMap.empty
  |> SlotMap.map (fun {attester; all_rounds} ->
         {attester; all_rounds = List.rev all_rounds})

let pp_delegate_infos fmt (Delegate_infos.{own_delegate_rounds; _} as t) =
  Format.fprintf
    fmt
    "@[<v>%a@]"
    Format.(
      pp_print_list
        ~pp_sep:pp_print_cut
        (fun fmt (first_slot, {attester; all_rounds}) ->
          Format.fprintf
            fmt
            "attester: %a, power: %Ld, first 10 slots: %a"
            Delegate.pp
            attester
            (Option.value ~default:0L
            @@ Delegate_infos.voting_power t ~slot:first_slot)
            (Format.pp_print_list
               ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ",")
               pp_print_int)
            (List.filteri (fun i _ -> i < 10) all_rounds
            |> List.map (fun x -> Round.to_int32 x |> Int32.to_int))))
    (SlotMap.bindings (delegate_infos_for_pp own_delegate_rounds))

let pp_prepared_block fmt {signed_block_header; delegate; _} =
  Format.fprintf
    fmt
    "predecessor block hash: %a, payload hash: %a, level: %ld, delegate: %a"
    Block_hash.pp
    signed_block_header.shell.predecessor
    Block_payload_hash.pp_short
    signed_block_header.protocol_data.contents.payload_hash
    signed_block_header.shell.level
    Delegate.pp
    delegate

let pp_level_state fmt
    {
      current_level;
      latest_proposal;
      is_latest_proposal_applied;
      locked_round;
      attestable_payload;
      elected_block;
      delegate_infos;
      next_level_delegate_infos;
      next_level_latest_forge_request;
    } =
  Format.fprintf
    fmt
    "@[<v 2>Level state:@ current level: %ld@ @[<v 2>proposal (applied:%b):@ \
     %a@]@ locked round: %a@ attestable payload: %a@ elected block: %a@ @[<v \
     2>own delegate infos:@ %a@]@ @[<v 2>next level own delegate infos:@ %a@]@ \
     next level proposed round: %a@]"
    current_level
    is_latest_proposal_applied
    pp_proposal
    latest_proposal
    (pp_option pp_locked_round)
    locked_round
    (pp_option pp_attestable_payload)
    attestable_payload
    (pp_option pp_elected_block)
    elected_block
    pp_delegate_infos
    delegate_infos
    pp_delegate_infos
    next_level_delegate_infos
    (pp_option Round.pp)
    next_level_latest_forge_request

let pp_phase fmt = function
  | Idle -> Format.fprintf fmt "idle"
  | Awaiting_preattestations -> Format.fprintf fmt "awaiting preattestations"
  | Awaiting_application -> Format.fprintf fmt "awaiting application"
  | Awaiting_attestations -> Format.fprintf fmt "awaiting attestations"

let pp_round_state fmt
    {
      current_round;
      current_phase;
      delayed_quorum;
      early_attestations;
      awaiting_unlocking_pqc;
    } =
  Format.fprintf
    fmt
    "@[<v 2>Round state:@ round: %a,@ phase: %a,@ delayed quorum: %a,@ early \
     attestations: %d,@ awaiting unlocking pqc: %b@]"
    Round.pp
    current_round
    pp_phase
    current_phase
    (pp_option Format.pp_print_int)
    (Option.map List.length delayed_quorum)
    (List.length early_attestations)
    awaiting_unlocking_pqc

let pp fmt {global_state; level_state; round_state} =
  Format.fprintf
    fmt
    "@[<v 2>State:@ %a@ %a@ %a@]"
    pp_global_state
    global_state
    pp_level_state
    level_state
    pp_round_state
    round_state

let pp_timeout_kind fmt = function
  | End_of_round {ending_round} ->
      Format.fprintf fmt "end of round %a" Round.pp ending_round
  | Time_to_prepare_next_level_block {at_round} ->
      Format.fprintf
        fmt
        "time to prepare next level block at round %a"
        Round.pp
        at_round

let pp_dal_content fmt {attestation} =
  Z.pp_print fmt (Environment.Bitset.to_z (attestation :> Environment.Bitset.t))

let pp_kind_and_dal fmt {vote_kind; dal_content; _} =
  match (vote_kind, dal_content) with
  | Preattestation, _ -> Format.fprintf fmt "preattestation"
  | Attestation, None -> Format.fprintf fmt "attestation (without DAL content)"
  | Attestation, Some dal_attestation ->
      Format.fprintf
        fmt
        "attestation (with DAL bitset %a)"
        pp_dal_content
        dal_attestation

let pp_unsigned_consensus_vote fmt
    ({
       vote_kind;
       vote_consensus_content = {level; round; _};
       delegate;
       dal_content;
     } as t) =
  let companion_key_is_relevant =
    match (vote_kind, dal_content) with
    | Attestation, Some _ -> Key.is_bls delegate.consensus_key
    | Attestation, None | Preattestation, _ -> false
  in
  Format.fprintf
    fmt
    "%a@ for level %a, round %a@ for delegate@ %a"
    pp_kind_and_dal
    t
    Protocol.Alpha_context.Raw_level.pp
    level
    Protocol.Alpha_context.Round.pp
    round
    (if companion_key_is_relevant then Delegate.pp
     else Delegate.pp_without_companion_key)
    delegate

let pp_signed_consensus_vote fmt {unsigned_consensus_vote; _} =
  pp_unsigned_consensus_vote fmt unsigned_consensus_vote

let pp_forge_event fmt = function
  | Block_ready {signed_block_header; round; delegate; _} ->
      Format.fprintf
        fmt
        "block ready@ at level %ld, round %a@ for@ delegate@ %a "
        signed_block_header.shell.level
        Round.pp
        round
        Delegate.pp_without_companion_key
        delegate
  | Preattestation_ready signed_op | Attestation_ready signed_op ->
      Format.fprintf
        fmt
        "operation ready:@ %a"
        pp_signed_consensus_vote
        signed_op

let pp_event fmt = function
  | New_valid_proposal proposal ->
      Format.fprintf
        fmt
        "new valid proposal received: %a"
        pp_block_info
        proposal.block
  | New_head_proposal proposal ->
      Format.fprintf
        fmt
        "new head proposal received: %a"
        pp_block_info
        proposal.block
  | Prequorum_reached (candidate, preattestations) ->
      Format.fprintf
        fmt
        "prequorum reached with %d preattestations for %a at round %a"
        (List.length preattestations)
        Block_hash.pp
        candidate.Operation_worker.hash
        Round.pp
        candidate.round_watched
  | Quorum_reached (candidate, attestations) ->
      Format.fprintf
        fmt
        "quorum reached with %d attestations for %a at round %a"
        (List.length attestations)
        Block_hash.pp
        candidate.Operation_worker.hash
        Round.pp
        candidate.round_watched
  | New_forge_event forge_event ->
      Format.fprintf fmt "new forge event: %a" pp_forge_event forge_event
  | Timeout kind ->
      Format.fprintf fmt "timeout reached: %a" pp_timeout_kind kind

let pp_short_event fmt =
  let open Format in
  function
  | New_valid_proposal _ -> fprintf fmt "new valid proposal"
  | New_head_proposal _ -> fprintf fmt "new head proposal"
  | Prequorum_reached (_, _) -> fprintf fmt "prequorum reached"
  | Quorum_reached (_, _) -> fprintf fmt "quorum reached"
  | Timeout (End_of_round _) -> fprintf fmt "end of round timeout"
  | Timeout (Time_to_prepare_next_level_block _) ->
      fprintf fmt "time to prepare next level block"
  | New_forge_event (Block_ready _) -> fprintf fmt "block ready"
  | New_forge_event (Preattestation_ready _) ->
      fprintf fmt "preattestation ready"
  | New_forge_event (Attestation_ready _) -> fprintf fmt "attestation ready"
