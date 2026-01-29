(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
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

open Validate_errors
open Alpha_context

type consensus_info = {
  predecessor_level : Raw_level.t;
  predecessor_round : Round.t;
  preattestation_slot_map : Consensus_key.power Slot.Map.t option;
  attestation_slot_map : Consensus_key.power Slot.Map.t option;
  consensus_slot_map : Consensus_key.power Slot.Map.t Level.Map.t option;
}

let init_consensus_info ctxt (predecessor_level, predecessor_round) =
  {
    predecessor_level;
    predecessor_round;
    preattestation_slot_map = Consensus.allowed_preattestations ctxt;
    attestation_slot_map = Consensus.allowed_attestations ctxt;
    consensus_slot_map = Consensus.allowed_consensus ctxt;
  }

(** Helper used to run all the pending checks after an operation validation. *)
let resolve_checks = List.iter_e (fun check -> check ())

(** Map used to detect consensus operation conflicts. Each delegate may
    (pre)attest at most once for each level and round, so two attestations
    (resp. two DAL attestations or two preattestations) conflict when they have
    the same slot, level, and round.

    Note that when validating the mempool, several (DAL/pre)attestations by the
    same delegate at the same level and round would not conflict if they have
    different Tenderbake slots.

    Note that when validating a block, all attestations (resp. all
    preattestations) must have the same level and round anyway, so only
    the slot is relevant. Taking the level and round into account is
    useful in mempool mode, because we want to be able to accept and
    propagate consensus operations for multiple close
    past/future/cousin blocks. *)
module Consensus_conflict_map = Map.Make (struct
  type t = Slot.t * Raw_level.t * Round.t

  let compare (slot1, level1, round1) (slot2, level2, round2) =
    Compare.or_else (Raw_level.compare level1 level2) @@ fun () ->
    Compare.or_else (Slot.compare slot1 slot2) @@ fun () ->
    Round.compare round1 round2
end)

type consensus_state = {
  preattestations_seen : Operation_hash.t Consensus_conflict_map.t;
  attestations_seen : Operation_hash.t Consensus_conflict_map.t;
  attestations_aggregate_seen : Operation_hash.t option;
  preattestations_aggregate_seen : Operation_hash.t option;
}

let consensus_conflict_map_encoding =
  let open Data_encoding in
  conv
    (fun map -> Consensus_conflict_map.bindings map)
    (fun l ->
      Consensus_conflict_map.(
        List.fold_left (fun m (k, v) -> add k v m) empty l))
    (list
       (tup2
          (tup3 Slot.encoding Raw_level.encoding Round.encoding)
          Operation_hash.encoding))

let consensus_state_encoding =
  let open Data_encoding in
  def "consensus_state"
  @@ conv
       (fun {
              preattestations_seen;
              attestations_seen;
              attestations_aggregate_seen;
              preattestations_aggregate_seen;
            }
          ->
         ( preattestations_seen,
           attestations_seen,
           attestations_aggregate_seen,
           preattestations_aggregate_seen ))
       (fun ( preattestations_seen,
              attestations_seen,
              attestations_aggregate_seen,
              preattestations_aggregate_seen )
          ->
         {
           preattestations_seen;
           attestations_seen;
           attestations_aggregate_seen;
           preattestations_aggregate_seen;
         })
       (obj4
          (req "preattestations_seen" consensus_conflict_map_encoding)
          (req "attestations_seen" consensus_conflict_map_encoding)
          (req "attestations_aggregate_seen" (option Operation_hash.encoding))
          (req
             "preattestations_aggregate_seen"
             (option Operation_hash.encoding)))

let empty_consensus_state =
  {
    preattestations_seen = Consensus_conflict_map.empty;
    attestations_seen = Consensus_conflict_map.empty;
    attestations_aggregate_seen = None;
    preattestations_aggregate_seen = None;
  }

type voting_state = {
  proposals_seen : Operation_hash.t Signature.Public_key_hash.Map.t;
      (** To each delegate that has submitted a Proposals operation in a
          previously validated operation, associates the hash of this
          operation. This includes Proposals from a potential Testnet
          Dictator. *)
  ballots_seen : Operation_hash.t Signature.Public_key_hash.Map.t;
      (** To each delegate that has submitted a ballot in a previously
          validated operation, associates the hash of this operation.  *)
}

let voting_state_encoding =
  let open Data_encoding in
  def "voting_state"
  @@ conv
       (fun {proposals_seen; ballots_seen} -> (proposals_seen, ballots_seen))
       (fun (proposals_seen, ballots_seen) -> {proposals_seen; ballots_seen})
       (obj2
          (req
             "proposals_seen"
             (Signature.Public_key_hash.Map.encoding Operation_hash.encoding))
          (req
             "ballots_seen"
             (Signature.Public_key_hash.Map.encoding Operation_hash.encoding)))

module Double_baking_evidence_map = struct
  include Map.Make (struct
    type t = Raw_level.t * Round.t

    let compare (l, r) (l', r') =
      Compare.or_else (Raw_level.compare l l') @@ fun () ->
      Compare.or_else (Round.compare r r') @@ fun () -> 0
  end)

  let encoding elt_encoding =
    Data_encoding.conv
      (fun map -> bindings map)
      (fun l -> List.fold_left (fun m (k, v) -> add k v m) empty l)
      Data_encoding.(
        list (tup2 (tup2 Raw_level.encoding Round.encoding) elt_encoding))
end

module Double_operation_evidence_map = struct
  include Map.Make (struct
    type t = Raw_level.t * Round.t * Slot.t * Misbehaviour.kind

    let compare (l, r, s, k) (l', r', s', k') =
      Compare.or_else (Raw_level.compare l l') @@ fun () ->
      Compare.or_else (Round.compare r r') @@ fun () ->
      Compare.or_else (Slot.compare s s') @@ fun () ->
      Misbehaviour.compare_kind k k'
  end)

  let encoding elt_encoding =
    Data_encoding.conv
      (fun map -> bindings map)
      (fun l -> List.fold_left (fun m (k, v) -> add k v m) empty l)
      Data_encoding.(
        list
          (tup2
             (tup4
                Raw_level.encoding
                Round.encoding
                Slot.encoding
                Misbehaviour.kind_encoding)
             elt_encoding))
end

module Dal_entrapment_map = struct
  include Map.Make (struct
    type t = Raw_level.t * Slot.t

    let compare (l1, s1) (l2, s2) =
      Compare.or_else (Raw_level.compare l1 l2) @@ fun () -> Slot.compare s1 s2
  end)

  let encoding elt_encoding =
    Data_encoding.conv
      (fun map -> bindings map)
      (fun l -> List.fold_left (fun m (k, v) -> add k v m) empty l)
      Data_encoding.(
        list (tup2 (tup2 Raw_level.encoding Slot.encoding) elt_encoding))
end

(** State used and modified when validating anonymous operations.
    These fields are used to enforce that we do not validate the same
    operation multiple times.

    Note that as part of {!state}, these maps live
    in memory. They are not explicitly bounded here, however:

    - In block validation mode, they are bounded by the number of
    anonymous operations allowed in the block.

    - In mempool mode, bounding the number of operations in this map
    is the responsability of the prevalidator on the shell side. *)
type anonymous_state = {
  activation_pkhs_seen : Operation_hash.t Ed25519.Public_key_hash.Map.t;
  double_baking_evidences_seen : Operation_hash.t Double_baking_evidence_map.t;
  double_consensus_operation_evidences_seen :
    Operation_hash.t Double_operation_evidence_map.t;
  dal_entrapments_seen : Operation_hash.t Dal_entrapment_map.t;
  seed_nonce_levels_seen : Operation_hash.t Raw_level.Map.t;
  vdf_solution_seen : Operation_hash.t option;
}

let raw_level_map_encoding elt_encoding =
  let open Data_encoding in
  conv
    (fun map -> Raw_level.Map.bindings map)
    (fun l ->
      Raw_level.Map.(List.fold_left (fun m (k, v) -> add k v m) empty l))
    (list (tup2 Raw_level.encoding elt_encoding))

let anonymous_state_encoding =
  let open Data_encoding in
  def "anonymous_state"
  @@ conv
       (fun {
              activation_pkhs_seen;
              double_baking_evidences_seen;
              double_consensus_operation_evidences_seen;
              dal_entrapments_seen;
              seed_nonce_levels_seen;
              vdf_solution_seen;
            }
          ->
         ( activation_pkhs_seen,
           double_baking_evidences_seen,
           double_consensus_operation_evidences_seen,
           dal_entrapments_seen,
           seed_nonce_levels_seen,
           vdf_solution_seen ))
       (fun ( activation_pkhs_seen,
              double_baking_evidences_seen,
              double_consensus_operation_evidences_seen,
              dal_entrapments_seen,
              seed_nonce_levels_seen,
              vdf_solution_seen )
          ->
         {
           activation_pkhs_seen;
           double_baking_evidences_seen;
           double_consensus_operation_evidences_seen;
           dal_entrapments_seen;
           seed_nonce_levels_seen;
           vdf_solution_seen;
         })
       (obj6
          (req
             "activation_pkhs_seen"
             (Ed25519.Public_key_hash.Map.encoding Operation_hash.encoding))
          (req
             "double_baking_evidences_seen"
             (Double_baking_evidence_map.encoding Operation_hash.encoding))
          (req
             "double_consensus_operation_evidences_seen"
             (Double_operation_evidence_map.encoding Operation_hash.encoding))
          (req
             "dal_entrapments_seen"
             (Dal_entrapment_map.encoding Operation_hash.encoding))
          (req
             "seed_nonce_levels_seen"
             (raw_level_map_encoding Operation_hash.encoding))
          (opt "vdf_solution_seen" Operation_hash.encoding))

let empty_anonymous_state =
  {
    activation_pkhs_seen = Ed25519.Public_key_hash.Map.empty;
    double_baking_evidences_seen = Double_baking_evidence_map.empty;
    double_consensus_operation_evidences_seen =
      Double_operation_evidence_map.empty;
    dal_entrapments_seen = Dal_entrapment_map.empty;
    seed_nonce_levels_seen = Raw_level.Map.empty;
    vdf_solution_seen = None;
  }

(** Static information used to validate manager operations. *)
type manager_info = {
  hard_storage_limit_per_operation : Z.t;
  hard_gas_limit_per_operation : Gas.Arith.integral;
}

let init_manager_info ctxt =
  {
    hard_storage_limit_per_operation =
      Constants.hard_storage_limit_per_operation ctxt;
    hard_gas_limit_per_operation = Constants.hard_gas_limit_per_operation ctxt;
  }

(** State used and modified when validating manager operations. *)
type manager_state = {
  managers_seen : Operation_hash.t Signature.Public_key_hash.Map.t;
      (** To enforce the one-operation-per manager-per-block restriction
          (1M). The operation hash lets us indicate the conflicting
          operation in the {!Manager_restriction} error.

          Note that as part of {!state}, this map
          lives in memory. It is not explicitly bounded here, however:

          - In block validation mode, it is bounded by the number of
            manager operations allowed in the block.

          - In mempool mode, bounding the number of operations in this
            map is the responsability of the mempool. (E.g. the plugin used
            by Octez has a [max_prechecked_manager_operations] parameter to
            ensure this.) *)
}

let manager_state_encoding =
  let open Data_encoding in
  def "manager_state"
  @@ conv
       (fun {managers_seen} -> managers_seen)
       (fun managers_seen -> {managers_seen})
       (obj1
          (req
             "managers_seen"
             (Signature.Public_key_hash.Map.encoding Operation_hash.encoding)))

let empty_manager_state = {managers_seen = Signature.Public_key_hash.Map.empty}

(** Information needed to validate consensus operations and/or to
    finalize the block in both modes that handle a preexisting block:
    [Application] and [Partial_validation]. *)
type block_info = {
  round : Round.t;
  locked_round : Round.t option;
  predecessor_hash : Block_hash.t;
  header_contents : Block_header.contents;
}

(** Information needed to validate consensus operations and/or to
    finalize the block in [Construction] mode. *)
type construction_info = {
  round : Round.t;
  predecessor_hash : Block_hash.t;
  header_contents : Block_header.contents;
}

(** Circumstances in which operations are validated, and corresponding
    specific information.

    If you add a new mode, please make sure that it has a way to bound
    the size of the maps in the {!operation_conflict_state}. *)
type mode =
  | Application of block_info
      (** [Application] is used for the validation of a preexisting block,
          often in preparation for its future application. *)
  | Partial_validation of block_info
      (** [Partial_validation] is used to quickly but partially validate a
          preexisting block, e.g. to quickly decide whether an alternate
          branch seems viable. In this mode, the initial {!type:context} may
          come from an ancestor block instead of the predecessor block. Only
          consensus operations are validated in this mode. *)
  | Construction of construction_info
      (** Used for the construction of a new block. *)
  | Mempool
      (** Used by the mempool ({!module:Mempool_validation}) and by the
          [Partial_construction] mode in {!module:Main}, which may itself be
          used by RPCs or by another mempool implementation. *)

(** {2 Definition and initialization of [info] and [state]} *)

type info = {
  ctxt : t;  (** The context at the beginning of the block or mempool. *)
  mode : mode;
  chain_id : Chain_id.t;  (** Needed for signature checks. *)
  current_level : Level.t;
  consensus_info : consensus_info option;
      (** Needed to validate consensus operations. This can be [None] during
          some RPC calls when some predecessor information is unavailable,
          in which case the validation of all consensus operations will
          systematically fail. *)
  manager_info : manager_info;
}

type operation_conflict_state = {
  consensus_state : consensus_state;
  voting_state : voting_state;
  anonymous_state : anonymous_state;
  manager_state : manager_state;
}

let operation_conflict_state_encoding =
  let open Data_encoding in
  def "operation_conflict_state"
  @@ conv
       (fun {consensus_state; voting_state; anonymous_state; manager_state} ->
         (consensus_state, voting_state, anonymous_state, manager_state))
       (fun (consensus_state, voting_state, anonymous_state, manager_state) ->
         {consensus_state; voting_state; anonymous_state; manager_state})
       (obj4
          (req "consensus_state" consensus_state_encoding)
          (req "voting_state" voting_state_encoding)
          (req "anonymous_state" anonymous_state_encoding)
          (req "manager_state" manager_state_encoding))

type block_state = {
  op_count : int;
  remaining_block_gas : Gas.Arith.fp;
  recorded_operations_rev : Operation_hash.t list;
  last_op_validation_pass : int option;
  locked_round_evidence : (Round.t * Attesting_power.t) option;
  attesting_power : Attesting_power.t;
}

type validation_state = {
  info : info;
  operation_state : operation_conflict_state;
  block_state : block_state;
}

let ok_unit = Result_syntax.return_unit

let result_error = Result_syntax.tzfail

let init_info ctxt mode chain_id ~predecessor_level_and_round =
  let consensus_info =
    Option.map (init_consensus_info ctxt) predecessor_level_and_round
  in
  {
    ctxt;
    mode;
    chain_id;
    current_level = Level.current ctxt;
    consensus_info;
    manager_info = init_manager_info ctxt;
  }

let empty_voting_state =
  {
    proposals_seen = Signature.Public_key_hash.Map.empty;
    ballots_seen = Signature.Public_key_hash.Map.empty;
  }

let empty_operation_conflict_state =
  {
    consensus_state = empty_consensus_state;
    voting_state = empty_voting_state;
    anonymous_state = empty_anonymous_state;
    manager_state = empty_manager_state;
  }

let init_block_state vi =
  {
    op_count = 0;
    remaining_block_gas =
      Gas.Arith.fp (Constants.hard_gas_limit_per_block vi.ctxt);
    recorded_operations_rev = [];
    last_op_validation_pass = None;
    locked_round_evidence = None;
    attesting_power = Attesting_power.zero;
  }

let get_initial_ctxt {info; _} = info.ctxt

(** Validation of consensus operations (validation pass [0]):
    preattestations and attestations. *)
module Consensus = struct
  open Validate_errors.Consensus

  let check_delegate_is_not_forbidden ctxt delegate_pkh =
    fail_when
      (Delegate.is_forbidden_delegate ctxt delegate_pkh)
      (Forbidden_delegate delegate_pkh)

  let get_delegate_details slot_map kind slot =
    let open Result_syntax in
    match slot_map with
    | None -> tzfail (Consensus.Slot_map_not_found {loc = __LOC__})
    | Some slot_map -> (
        match Slot.Map.find slot slot_map with
        | None -> tzfail (Wrong_slot_used_for_consensus_operation {kind})
        | Some x -> return x)

  (** When validating a block (ie. in [Application],
      [Partial_validation], and [Construction] modes), any
      preattestations must point to a round that is strictly before the
      block's round. *)
  let check_round_before_block ~block_round provided =
    error_unless
      Round.(provided < block_round)
      (Preattestation_round_too_high {block_round; provided})

  let check_level kind expected provided =
    (* We use [if] instead of [error_unless] to avoid computing the
       error when it is not needed. *)
    if Raw_level.equal expected provided then Result.return_unit
    else if Raw_level.(expected > provided) then
      result_error
        (Consensus_operation_for_old_level {kind; expected; provided})
    else
      result_error
        (Consensus_operation_for_future_level {kind; expected; provided})

  let check_round kind expected provided =
    (* We use [if] instead of [error_unless] to avoid computing the
       error when it is not needed. *)
    if Round.equal expected provided then ok_unit
    else if Round.(expected > provided) then
      result_error
        (Consensus_operation_for_old_round {kind; expected; provided})
    else
      result_error
        (Consensus_operation_for_future_round {kind; expected; provided})

  let check_payload_hash kind expected provided =
    error_unless
      (Block_payload_hash.equal expected provided)
      (Wrong_payload_hash_for_consensus_operation {kind; expected; provided})

  (** Preattestation checks for both [Application] and
      [Partial_validation] modes. *)
  let check_preexisting_block_preattestation vi block_info
      {level; round; block_payload_hash = bph; slot = _} =
    let open Lwt_result_syntax in
    let*? locked_round =
      match block_info.locked_round with
      | Some locked_round -> Ok locked_round
      | None ->
          (* A preexisting block whose fitness has no locked round
             should contain no preattestations. *)
          error Unexpected_preattestation_in_block
    in
    let kind = Preattestation in
    let*? () = check_round_before_block ~block_round:block_info.round round in
    let*? () = check_level kind vi.current_level.level level in
    let*? () = check_round kind locked_round round in
    let expected_payload_hash = block_info.header_contents.payload_hash in
    let*? () = check_payload_hash kind expected_payload_hash bph in
    return_unit

  (** Preattestation checks for Construction mode. *)
  let check_constructed_block_preattestation vi cons_info
      {level; round; block_payload_hash = bph; slot = _} =
    let open Lwt_result_syntax in
    let expected_payload_hash = cons_info.header_contents.payload_hash in
    let*? () =
      (* When the proposal is fresh, a fake [payload_hash] of [zero]
         has been provided. In this case, the block should not contain
         any preattestations. *)
      error_when
        Block_payload_hash.(expected_payload_hash = zero)
        Unexpected_preattestation_in_block
    in
    let kind = Preattestation in
    let*? () = check_round_before_block ~block_round:cons_info.round round in
    let*? () = check_level kind vi.current_level.level level in
    (* We cannot check the exact round here in construction mode, because
       there is no preexisting fitness to provide the locked_round. We do
       however check that all preattestations have the same round in
       [check_construction_preattestation_round_consistency] further below. *)
    let*? () = check_payload_hash kind expected_payload_hash bph in
    return_unit

  (** Preattestation/attestation checks for Mempool mode.

      We want this mode to be very permissive, to allow the mempool to
      accept and propagate consensus operations even if they point to a
      block which is not known to the mempool (e.g. because the block
      has just been validated and the mempool has not had time to
      switch its head to it yet, or because the block belongs to a
      cousin branch). Therefore, we do not check the round nor the
      payload, which may correspond to blocks that we do not know of
      yet. As to the level, we only require it to be the
      [predecessor_level] (aka the level of the mempool's head) plus or
      minus one, that is:
      [predecessor_level - 1 <= op_level <= predecessor_level + 1]
      (note that [predecessor_level + 1] is also known as [current_level]).

      The (pre)attested slot must be the lowest one assigned to its owner.

      Returns the corresponding delegate's consensus key and attesting power. *)
  let check_mempool_consensus vi consensus_info kind {level; slot; _} =
    let open Result_syntax in
    let* () =
      if Raw_level.(succ level < consensus_info.predecessor_level) then
        let expected = consensus_info.predecessor_level and provided = level in
        result_error
          (Consensus_operation_for_old_level {kind; expected; provided})
      else if Raw_level.(level > vi.current_level.level) then
        let expected = consensus_info.predecessor_level and provided = level in
        result_error
          (Consensus_operation_for_future_level {kind; expected; provided})
      else ok_unit
    in
    let level = Level.from_raw vi.ctxt level in
    match consensus_info.consensus_slot_map with
    | None -> tzfail (Consensus.Slot_map_not_found {loc = __LOC__})
    | Some level_map ->
        let slot_map = Level.Map.find level level_map in
        get_delegate_details slot_map kind slot

  (* We do not check that the frozen deposits are positive because this
     only needs to be true in the context of a block that actually
     contains the operation, which may not be the same as the current
     mempool's context. *)

  let check_preattestation vi ~check_signature
      (operation : Kind.preattestation operation) =
    let open Lwt_result_syntax in
    let*? consensus_info =
      Option.value_e
        ~error:(trace_of_error Consensus_operation_not_allowed)
        vi.consensus_info
    in
    let (Single (Preattestation consensus_content)) =
      operation.protocol_data.contents
    in
    let* {consensus_key; attesting_power; dal_power = _} =
      match vi.mode with
      | Application block_info | Partial_validation block_info ->
          let* () =
            check_preexisting_block_preattestation
              vi
              block_info
              consensus_content
          in
          Lwt.return
          @@ get_delegate_details
               consensus_info.preattestation_slot_map
               Preattestation
               consensus_content.slot
      | Construction construction_info ->
          let* () =
            check_constructed_block_preattestation
              vi
              construction_info
              consensus_content
          in
          Lwt.return
          @@ get_delegate_details
               consensus_info.preattestation_slot_map
               Preattestation
               consensus_content.slot
      | Mempool ->
          Lwt.return
          @@ check_mempool_consensus
               vi
               consensus_info
               Preattestation
               consensus_content
    in
    let* () =
      match vi.mode with
      | Application _ | Partial_validation _ | Construction _ -> (
          match consensus_key.consensus_pk with
          | Bls _ when Constants.aggregate_attestation vi.ctxt ->
              (* A preattestation with a BLS signature must be included in a
                 Preattestations_aggregate, even if there is only one in the
                 block. *)
              let hash = Operation.hash operation in
              tzfail
                (Unaggregated_eligible_operation {kind = Preattestation; hash})
          | _ -> return_unit)
      | Mempool -> return_unit
    in
    let* () = check_delegate_is_not_forbidden vi.ctxt consensus_key.delegate in
    let check_signature =
      if check_signature then
        [
          (fun () ->
            Operation.check_signature
              vi.ctxt
              consensus_key.consensus_pk
              vi.chain_id
              operation);
        ]
      else []
    in
    return (attesting_power, check_signature)

  let check_preattestation_conflict vs oph (op : Kind.preattestation operation)
      =
    let (Single (Preattestation {slot; level; round; _})) =
      op.protocol_data.contents
    in
    match
      Consensus_conflict_map.find_opt
        (slot, level, round)
        vs.consensus_state.preattestations_seen
    with
    | Some existing ->
        Error (Operation_conflict {existing; new_operation = oph})
    | None -> ok_unit

  let wrap_preattestation_conflict = function
    | Ok () -> ok_unit
    | Error conflict ->
        result_error
          Validate_errors.Consensus.(
            Conflicting_consensus_operation {kind = Preattestation; conflict})

  let add_preattestation vs oph (op : Kind.preattestation operation) =
    let (Single (Preattestation {slot; level; round; _})) =
      op.protocol_data.contents
    in
    let preattestations_seen =
      Consensus_conflict_map.add
        (slot, level, round)
        oph
        vs.consensus_state.preattestations_seen
    in
    {vs with consensus_state = {vs.consensus_state with preattestations_seen}}

  let may_update_locked_round_evidence block_state mode
      (consensus_content : consensus_content) attesting_power =
    let locked_round_evidence =
      match mode with
      | Mempool -> (* The block_state is not relevant in this mode. *) None
      | Application _ | Partial_validation _ | Construction _ -> (
          match block_state.locked_round_evidence with
          | None -> Some (consensus_content.round, attesting_power)
          | Some (_stored_round, total_attesting_power) ->
              (* [_stored_round] is always equal to [consensus_content.round].
                 Indeed, this is ensured by
                 {!check_preattestation_content_preexisting_block} in
                 application and partial validation modes, and by
                 {!check_construction_preattestation_round_consistency} in
                 construction mode. *)
              Some
                ( consensus_content.round,
                  Attesting_power.add total_attesting_power attesting_power ))
    in
    {block_state with locked_round_evidence}

  (* Hypothesis: this function will only be called in mempool mode *)
  let remove_preattestation vs (operation : Kind.preattestation operation) =
    (* As we are in mempool mode, we do not update
       [locked_round_evidence]. *)
    let (Single (Preattestation {slot; level; round; _})) =
      operation.protocol_data.contents
    in
    let preattestations_seen =
      Consensus_conflict_map.remove
        (slot, level, round)
        vs.consensus_state.preattestations_seen
    in
    {vs with consensus_state = {vs.consensus_state with preattestations_seen}}

  (** Attestation checks for all modes that involve a block:
      Application, Partial_validation, and Construction.
      Checks regarding the DAL content are done separately. *)
  let check_block_attestation vi consensus_info
      {level; round; block_payload_hash = bph; slot = _} =
    let open Lwt_result_syntax in
    let*? expected_payload_hash =
      match Consensus.attestation_branch vi.ctxt with
      | Some ((_branch : Block_hash.t), payload_hash) -> Ok payload_hash
      | None ->
          (* [Consensus.attestation_branch] only returns [None] when the
             predecessor is the block that activates the first protocol
             of the Tenderbake family; this block should not be
             attested. This can only happen in tests and test
             networks. *)
          result_error Unexpected_attestation_in_block
    in
    let kind = Attestation in
    let*? () = check_level kind consensus_info.predecessor_level level in
    let*? () = check_round kind consensus_info.predecessor_round round in
    let*? () = check_payload_hash kind expected_payload_hash bph in
    return_unit

  let check_dal_content vi level slot consensus_key = function
    | None -> return_unit
    | Some {attestation} ->
        Dal_apply.validate_attestation
          vi.ctxt
          level
          slot
          consensus_key
          attestation

  let check_attestation_signature vi consensus_key
      (operation : Kind.attestation operation) =
    let open Result_syntax in
    let (Single (Attestation {dal_content; _})) =
      operation.protocol_data.contents
    in
    let* dal_dependent_pk =
      match (consensus_key.Consensus_key.consensus_pk, dal_content) with
      | Bls consensus_bls_pk, Some {attestation = dal_attestation}
        when Constants.aggregate_attestation vi.ctxt -> (
          match consensus_key.companion_pk with
          | None ->
              tzfail
                (Missing_companion_key_for_bls_dal
                   (Consensus_key.pkh consensus_key))
          | Some companion_pk -> (
              let serialized_op =
                Operation.(
                  serialize_unsigned_operation
                    bls_mode_unsigned_encoding
                    operation)
              in
              let dal_dependent_bls_pk_opt =
                Dal.Attestation.Dal_dependent_signing.aggregate_pk
                  ~subgroup_check:false
                    (* We disable subgroup check (for better
                       performances) because the context only contains
                       valid consensus and companion keys. *)
                  ~consensus_pk:consensus_bls_pk
                  ~companion_pk
                  ~op:serialized_op
                  dal_attestation
              in
              match dal_dependent_bls_pk_opt with
              | None ->
                  tzfail
                    Validate_errors.Consensus.Public_key_aggregation_failure
              | Some dal_dependent_bls_pk ->
                  return
                    (Signature.Bls dal_dependent_bls_pk
                      : Signature.Public_key.t)))
      | _ ->
          (* When the feature flag is not set or the consensus key is
             non-BLS, we use the old behavior: the signed content
             (which includes the dal_content if any) is signed by the
             consensus key alone.

             When the dal_content is None (even with enabled feature
             flag and BLS consensus key), it is represented by
             [dal_dependent_pk = consensus_pk]. Notably, this allows a
             BLS consensus key without any associated companion key to
             still issue an attestation without DAL. *)
          return consensus_key.consensus_pk
    in
    Operation.check_signature vi.ctxt dal_dependent_pk vi.chain_id operation

  let check_attestation vi ~check_signature
      (operation : Kind.attestation operation) =
    let open Lwt_result_syntax in
    let*? consensus_info =
      Option.value_e
        ~error:(trace_of_error Consensus_operation_not_allowed)
        vi.consensus_info
    in
    let (Single (Attestation {consensus_content; dal_content})) =
      operation.protocol_data.contents
    in
    let* {consensus_key; attesting_power; dal_power = _} =
      match vi.mode with
      | Application _ | Partial_validation _ | Construction _ -> (
          let* () =
            check_block_attestation vi consensus_info consensus_content
          in
          let*? details =
            get_delegate_details
              consensus_info.attestation_slot_map
              Attestation
              consensus_content.slot
          in
          match details.consensus_key.consensus_pk with
          | Bls _ when Constants.aggregate_attestation vi.ctxt ->
              (* An attestation with a BLS signature must be included in an
                 Attestations_aggregate, even if there is only one in the block.
              *)
              let hash = Operation.hash operation in
              tzfail
                (Unaggregated_eligible_operation {kind = Attestation; hash})
          | _ -> return details)
      | Mempool ->
          Lwt.return
          @@ check_mempool_consensus
               vi
               consensus_info
               Attestation
               consensus_content
    in
    let* () = check_delegate_is_not_forbidden vi.ctxt consensus_key.delegate in
    let* () =
      check_dal_content
        vi
        consensus_content.level
        consensus_content.slot
        consensus_key
        dal_content
    in
    let check_signature =
      if check_signature then
        [(fun () -> check_attestation_signature vi consensus_key operation)]
      else []
    in
    return (attesting_power, check_signature)

  let check_attestation_conflict vs oph (operation : Kind.attestation operation)
      =
    let (Single
           (Attestation
              {consensus_content = {slot; level; round; _}; dal_content = _})) =
      operation.protocol_data.contents
    in
    match
      Consensus_conflict_map.find_opt
        (slot, level, round)
        vs.consensus_state.attestations_seen
    with
    | None -> ok_unit
    | Some existing ->
        Error (Operation_conflict {existing; new_operation = oph})

  let wrap_attestation_conflict = function
    | Ok () -> ok_unit
    | Error conflict ->
        result_error
          Validate_errors.Consensus.(
            Conflicting_consensus_operation {kind = Attestation; conflict})

  let add_attestation vs oph (op : Kind.attestation operation) =
    let (Single
           (Attestation
              {consensus_content = {slot; level; round; _}; dal_content = _})) =
      op.protocol_data.contents
    in
    let attestations_seen =
      Consensus_conflict_map.add
        (slot, level, round)
        oph
        vs.consensus_state.attestations_seen
    in
    {vs with consensus_state = {vs.consensus_state with attestations_seen}}

  let may_update_attesting_power vi block_state attesting_power =
    match vi.mode with
    | Mempool -> (* The block_state is not relevant. *) block_state
    | Application _ | Partial_validation _ | Construction _ ->
        {
          block_state with
          attesting_power =
            Attesting_power.add block_state.attesting_power attesting_power;
        }

  (* Hypothesis: this function will only be called in mempool mode *)
  let remove_attestation vs (operation : Kind.attestation operation) =
    (* We do not remove the attesting power because it is not
       relevant for the mempool mode. *)
    let (Single
           (Attestation
              {consensus_content = {slot; level; round; _}; dal_content = _})) =
      operation.protocol_data.contents
    in
    let attestations_seen =
      Consensus_conflict_map.remove
        (slot, level, round)
        vs.consensus_state.attestations_seen
    in
    {vs with consensus_state = {vs.consensus_state with attestations_seen}}

  (** In Construction mode, check that the preattestation has the same
      round as any previously validated preattestations.

      This check is not needed in other modes because
      {!check_preattestation} already checks that all preattestations
      have the same expected round (the locked_round in Application and
      Partial_validation modes when there is one (otherwise all
      preattestations are rejected so the point is moot), or the
      predecessor_round in Mempool mode). *)
  let check_construction_preattestation_round_consistency vi block_state
      (consensus_content : consensus_content) =
    let open Result_syntax in
    match vi.mode with
    | Construction _ -> (
        match block_state.locked_round_evidence with
        | None ->
            (* This is the first validated preattestation:
               there is nothing to check. *)
            return_unit
        | Some (expected, _power) ->
            (* Other preattestations have already been validated: we check
               that the current operation has the same round as them. *)
            check_round Preattestation expected consensus_content.round)
    | Application _ | Partial_validation _ | Mempool -> return_unit

  let validate_preattestation ~check_signature info operation_state block_state
      oph (operation : Kind.preattestation operation) =
    let open Lwt_result_syntax in
    let (Single (Preattestation consensus_content)) =
      operation.protocol_data.contents
    in
    let* attesting_power, pending_checks =
      check_preattestation info ~check_signature operation
    in
    let*? () = resolve_checks pending_checks in
    let*? () =
      check_construction_preattestation_round_consistency
        info
        block_state
        consensus_content
    in
    let*? () =
      check_preattestation_conflict operation_state oph operation
      |> wrap_preattestation_conflict
    in
    (* We need to update the block state *)
    let block_state =
      may_update_locked_round_evidence
        block_state
        info.mode
        consensus_content
        attesting_power
    in
    let operation_state = add_preattestation operation_state oph operation in
    return {info; operation_state; block_state}

  let validate_attestation ~check_signature info operation_state block_state oph
      operation =
    let open Lwt_result_syntax in
    let* power, pending_checks =
      check_attestation info ~check_signature operation
    in
    let*? () = resolve_checks pending_checks in
    let*? () =
      check_attestation_conflict operation_state oph operation
      |> wrap_attestation_conflict
    in
    let block_state = may_update_attesting_power info block_state power in
    let operation_state = add_attestation operation_state oph operation in
    return {info; operation_state; block_state}

  let check_attestations_aggregate_conflict vs oph =
    match vs.consensus_state.attestations_aggregate_seen with
    | None -> ok_unit
    | Some existing ->
        Error (Operation_conflict {existing; new_operation = oph})

  let wrap_attestations_aggregate_conflict = function
    | Ok () -> ok_unit
    | Error conflict ->
        result_error
          Validate_errors.Consensus.(
            Conflicting_consensus_operation
              {kind = Attestations_aggregate; conflict})

  let add_attestations_aggregate operation_state oph =
    {
      operation_state with
      consensus_state =
        {
          operation_state.consensus_state with
          attestations_aggregate_seen = Some oph;
        };
    }

  let check_attestations_aggregate_signature info pks weighted_pks
      ({shell; protocol_data = {contents = Single content; signature}} :
        Kind.attestations_aggregate Operation.t) =
    let open Result_syntax in
    let (Attestations_aggregate {consensus_content; _}) = content in
    let {level; round; block_payload_hash} : consensus_aggregate_content =
      consensus_content
    in
    let aggregated_pk_opt =
      (* We disable subgroup check (for better performances) since
         public keys are retreived from the context and assumed
         valid. *)
      let subgroup_check = false in
      if List.is_empty weighted_pks then
        Bls.aggregate_public_key_opt ~subgroup_check pks
      else
        Option.bind
          (Bls.aggregate_public_key_weighted_opt ~subgroup_check weighted_pks)
        @@ fun aggregated_weighted_pks ->
        Bls.aggregate_public_key_opt
          ~subgroup_check
          (aggregated_weighted_pks :: pks)
    in
    let* aggregated_pk =
      match aggregated_pk_opt with
      | Some aggregated_pk -> return aggregated_pk
      | None ->
          (* This is never supposed to happen since keys are assumed
             valid. *)
          tzfail Validate_errors.Consensus.Public_key_aggregation_failure
    in
    let attestation : Kind.attestation operation =
      (* Reconstructing an attestation to match the content signed by
         each delegate. Fields 'slot' and 'dal_content' are filled with
         dummy values as they are not part of the signed payload *)
      let consensus_content =
        {slot = Slot.zero; level; round; block_payload_hash}
      in
      let contents =
        Single (Attestation {consensus_content; dal_content = None})
      in
      {shell; protocol_data = {contents; signature}}
    in
    Operation.check_signature
      info.ctxt
      (Bls aggregated_pk)
      info.chain_id
      attestation

  let check_preattestations_aggregate_signature info public_keys
      ({shell; protocol_data = {contents = Single content; signature}} :
        Kind.preattestations_aggregate Operation.t) =
    let open Lwt_result_syntax in
    let (Preattestations_aggregate {consensus_content; _}) = content in
    let {level; round; block_payload_hash} : consensus_aggregate_content =
      consensus_content
    in
    (* We disable subgroup check (for better performances) since public keys are
       retreived from the context and assumed valid. *)
    match Bls.aggregate_public_key_opt ~subgroup_check:false public_keys with
    | None ->
        (* This is never supposed to happen since keys are assumed valid. *)
        tzfail Validate_errors.Consensus.Public_key_aggregation_failure
    | Some public_keys_aggregate ->
        (* Reconstructing an attestation to match the content signed by each
           delegate. The field slot is filled with a dummy value as its not part
           of the signed payload *)
        let consensus_content =
          {slot = Slot.zero; level; round; block_payload_hash}
        in
        let contents = Single (Preattestation consensus_content) in
        let preattestation : Kind.preattestation operation =
          {shell; protocol_data = {contents; signature}}
        in
        let*? () =
          Operation.check_signature
            info.ctxt
            (Bls public_keys_aggregate)
            info.chain_id
            preattestation
        in
        return_unit

  let handle_attestations_aggregate_conflicts
      {info; operation_state; block_state} oph
      ({shell; protocol_data = {contents = Single content; _}} :
        Kind.attestations_aggregate operation) =
    let open Lwt_result_syntax in
    (* Check that no other Attestations_aggregate operation was previously
       recorded in the operation state *)
    let*? () =
      check_attestations_aggregate_conflict operation_state oph
      |> wrap_attestations_aggregate_conflict
    in
    (* Record the aggregate in the operation state *)
    let operation_state = add_attestations_aggregate operation_state oph in
    (* Check for attestations conflicts and register each attestations in the
       operation state *)
    let (Attestations_aggregate {consensus_content; committee}) = content in
    let {level; round; block_payload_hash} : consensus_aggregate_content =
      consensus_content
    in
    let* operation_state =
      List.fold_left_es
        (fun operation_state slot ->
          let attestation : Kind.attestation operation =
            let consensus_content = {slot; level; round; block_payload_hash} in
            let contents =
              Single (Attestation {consensus_content; dal_content = None})
            in
            {shell; protocol_data = {contents; signature = None}}
          in
          let*? () =
            check_attestation_conflict operation_state oph attestation
            |> wrap_attestation_conflict
          in
          let operation_state =
            add_attestation operation_state oph attestation
          in
          return operation_state)
        operation_state
        (Operation.committee_slots committee)
    in
    return {info; operation_state; block_state}

  let check_preattestations_aggregate_conflict vs oph =
    match vs.consensus_state.preattestations_aggregate_seen with
    | None -> ok_unit
    | Some existing ->
        Error (Operation_conflict {existing; new_operation = oph})

  let wrap_preattestations_aggregate_conflict = function
    | Ok () -> ok_unit
    | Error conflict ->
        result_error
          Validate_errors.Consensus.(
            Conflicting_consensus_operation
              {kind = Preattestations_aggregate; conflict})

  let add_preattestations_aggregate operation_state oph =
    {
      operation_state with
      consensus_state =
        {
          operation_state.consensus_state with
          preattestations_aggregate_seen = Some oph;
        };
    }

  let handle_preattestations_aggregate_conflicts
      {info; operation_state; block_state} oph
      ({shell; protocol_data = {contents = Single content; _}} :
        Kind.preattestations_aggregate operation) =
    let open Lwt_result_syntax in
    (* Check that no other Preattestations_aggregate operation was previously
       recorded in the operation state *)
    let*? () =
      check_preattestations_aggregate_conflict operation_state oph
      |> wrap_preattestations_aggregate_conflict
    in
    (* Record the aggregate in the operation state *)
    let operation_state = add_preattestations_aggregate operation_state oph in
    (* Check for preattestations conflicts and register each operation in the
       operation state *)
    let (Preattestations_aggregate {consensus_content; committee}) = content in
    let {level; round; block_payload_hash} : consensus_aggregate_content =
      consensus_content
    in
    let* operation_state =
      List.fold_left_es
        (fun operation_state slot ->
          let preattestation : Kind.preattestation operation =
            let consensus_content = {slot; level; round; block_payload_hash} in
            let contents = Single (Preattestation consensus_content) in
            {shell; protocol_data = {contents; signature = None}}
          in
          let*? () =
            check_preattestation_conflict operation_state oph preattestation
            |> wrap_preattestation_conflict
          in
          let operation_state =
            add_preattestation operation_state oph preattestation
          in
          return operation_state)
        operation_state
        committee
    in
    return {info; operation_state; block_state}

  let validate_preattestations_aggregate ~check_signature info operation_state
      block_state oph
      ({protocol_data = {contents = Single content; _}; _} as op :
        Kind.preattestations_aggregate operation) =
    let open Lwt_result_syntax in
    match info.mode with
    | Mempool ->
        (* Aggregate operations are built at baking time and should not be
           propagated between mempools. *)
        tzfail Validate_errors.Consensus.Aggregate_in_mempool
    | Application _ | Partial_validation _ | Construction _ ->
        let*? () =
          (* Aggregates are currently under feature flag *)
          error_unless
            (Constants.aggregate_attestation info.ctxt)
            Validate_errors.Consensus.Aggregate_disabled
        in
        let*? consensus_info =
          Option.value_e
            ~error:(trace_of_error Consensus_operation_not_allowed)
            info.consensus_info
        in
        let (Preattestations_aggregate {consensus_content; committee}) =
          content
        in
        let {level; round; block_payload_hash} : consensus_aggregate_content =
          consensus_content
        in
        let* () =
          (* uses a dummy slot value in the following checks *)
          match info.mode with
          | Application block_info | Partial_validation block_info ->
              check_preexisting_block_preattestation
                info
                block_info
                {level; round; block_payload_hash; slot = Slot.zero}
          | Construction construction_info ->
              let* () =
                check_constructed_block_preattestation
                  info
                  construction_info
                  {level; round; block_payload_hash; slot = Slot.zero}
              in
              let*? () =
                check_construction_preattestation_round_consistency
                  info
                  block_state
                  {level; round; block_payload_hash; slot = Slot.zero}
              in
              return_unit
          | Mempool -> return_unit
        in
        (* Retrieve public keys and compute total attesting power *)
        let* public_keys, total_attesting_power =
          List.fold_left_es
            (fun (public_keys, total_attesting_power) slot ->
              (* Lookup the slot owner *)
              let*? {consensus_key; attesting_power; dal_power = _} =
                get_delegate_details
                  consensus_info.preattestation_slot_map
                  Preattestation
                  slot
              in
              let* () =
                check_delegate_is_not_forbidden info.ctxt consensus_key.delegate
              in
              match consensus_key.consensus_pk with
              | Bls pk ->
                  return
                    ( pk :: public_keys,
                      Attesting_power.add attesting_power total_attesting_power
                    )
              | _ -> tzfail Validate_errors.Consensus.Non_bls_key_in_aggregate)
            ([], Attesting_power.zero)
            committee
        in
        (* Fail on empty committee *)
        let*? () =
          error_when
            (List.is_empty public_keys)
            Validate_errors.Consensus.Empty_aggregation_committee
        in
        (* Check signature *)
        let* () =
          if check_signature then
            check_preattestations_aggregate_signature info public_keys op
          else return_unit
        in
        (* Check for conflicts and registers the preattestations in the
           validation state *)
        let* validation_state =
          handle_preattestations_aggregate_conflicts
            {info; operation_state; block_state}
            oph
            op
        in
        (* Update voting power (uses a dummy slot value) *)
        let block_state =
          may_update_locked_round_evidence
            block_state
            info.mode
            {level; round; block_payload_hash; slot = Slot.zero}
            total_attesting_power
        in
        return {validation_state with block_state}

  let validate_attestations_aggregate ~check_signature info operation_state
      block_state oph
      ({protocol_data = {contents = Single content; _}; shell} as op :
        Kind.attestations_aggregate operation) =
    let open Lwt_result_syntax in
    match info.mode with
    | Mempool ->
        (* Attestations_aggregate operations are built at baking time and
           should not be propagated between mempools. *)
        tzfail Validate_errors.Consensus.Aggregate_in_mempool
    | Application _ | Partial_validation _ | Construction _ ->
        let*? () =
          (* Attestations_aggregates are currently under feature flag *)
          error_unless
            (Constants.aggregate_attestation info.ctxt)
            Validate_errors.Consensus.Aggregate_disabled
        in
        let*? consensus_info =
          Option.value_e
            ~error:(trace_of_error Consensus_operation_not_allowed)
            info.consensus_info
        in
        let (Attestations_aggregate {consensus_content; committee}) = content in
        let {level; round; block_payload_hash} : consensus_aggregate_content =
          consensus_content
        in
        (* Check level, round and block_payload_hash
           (uses a dummy slot value) *)
        let* () =
          check_block_attestation
            info
            consensus_info
            {level; round; block_payload_hash; slot = Slot.zero}
        in
        let attestation : Kind.attestation operation =
          (* Reconstructing an attestation to match the content signed by
             each delegate. Fields 'slot' and 'dal_content' are filled with
             dummy values as they are not part of the signed payload *)
          let consensus_content =
            {slot = Slot.zero; level; round; block_payload_hash}
          in
          let contents =
            Single (Attestation {consensus_content; dal_content = None})
          in
          {shell; protocol_data = {contents; signature = None}}
        in
        let serialized_op =
          Operation.(
            serialize_unsigned_operation bls_mode_unsigned_encoding attestation)
        in
        (* Retrieve public keys that will later be aggregated (keys
           with weight 1 are put in [pks]; keys with a different
           weight are put in [weighted_pks]) and compute total attesting
           power. *)
        let* pks, weighted_pks, total_attesting_power =
          List.fold_left_es
            (fun (pks, weighted_pks, total_power) (slot, dal) ->
              (* Lookup the slot owner *)
              let*? {consensus_key; attesting_power; dal_power = _} =
                get_delegate_details
                  consensus_info.attestation_slot_map
                  Attestation
                  slot
              in
              let* () =
                check_delegate_is_not_forbidden info.ctxt consensus_key.delegate
              in
              let* () = check_dal_content info level slot consensus_key dal in
              let total_power =
                Attesting_power.add attesting_power total_power
              in
              match consensus_key.consensus_pk with
              | Bls consensus_pk -> (
                  let pks = consensus_pk :: pks in
                  match (dal, consensus_key.companion_pk) with
                  | None, _ -> return (pks, weighted_pks, total_power)
                  | Some {attestation = dal_attestation}, Some companion_pk ->
                      let weight =
                        Dal.Attestation.Dal_dependent_signing.weight
                          ~consensus_pk
                          ~companion_pk
                          ~op:serialized_op
                          dal_attestation
                      in
                      if Z.(equal weight one) then
                        return (companion_pk :: pks, weighted_pks, total_power)
                      else
                        let weighted_pks =
                          (weight, companion_pk) :: weighted_pks
                        in
                        return (pks, weighted_pks, total_power)
                  | Some _, None ->
                      tzfail
                        (Missing_companion_key_for_bls_dal
                           (Consensus_key.pkh consensus_key)))
              | _ -> tzfail Validate_errors.Consensus.Non_bls_key_in_aggregate)
            ([], [], Attesting_power.zero)
            committee
        in
        (* Fail on empty committee *)
        let*? () =
          error_when
            (List.is_empty pks)
            Validate_errors.Consensus.Empty_aggregation_committee
        in
        (* Check signature *)
        let*? () =
          let open Result_syntax in
          if check_signature then
            check_attestations_aggregate_signature info pks weighted_pks op
          else return_unit
        in
        (* Check for conflicts and register the aggregate and its underlying
           attestations in the validation state. *)
        let* validation_state =
          handle_attestations_aggregate_conflicts
            {info; operation_state; block_state}
            oph
            op
        in
        (* Increase block attesting power *)
        let block_state =
          may_update_attesting_power info block_state total_attesting_power
        in
        return {validation_state with block_state}
end

(** {2 Validation of voting operations}

    There are two kinds of voting operations:

    - Proposals: A delegate submits a list of protocol amendment
      proposals. This operation is only accepted during a Proposal period
      (see above).

    - Ballot: A delegate casts a vote for/against the current proposal
      (or pass). This operation is only accepted during an Exploration
      or Promotion period (see above). *)

module Voting = struct
  open Validate_errors.Voting

  let check_period_index ~expected period_index =
    error_unless
      Compare.Int32.(expected = period_index)
      (Wrong_voting_period_index {expected; provided = period_index})

  let check_proposals_source_is_registered ctxt source =
    let open Lwt_result_syntax in
    let*! is_registered = Delegate.registered ctxt source in
    fail_unless is_registered (Proposals_from_unregistered_delegate source)

  (** Check that the list of proposals is not empty and does not contain
      duplicates. *)
  let check_proposal_list_sanity proposals =
    let open Result_syntax in
    let* () =
      match proposals with [] -> tzfail Empty_proposals | _ :: _ -> ok_unit
    in
    let* (_ : Protocol_hash.Set.t) =
      List.fold_left_e
        (fun previous_elements proposal ->
          let* () =
            error_when
              (Protocol_hash.Set.mem proposal previous_elements)
              (Proposals_contain_duplicate {proposal})
          in
          return (Protocol_hash.Set.add proposal previous_elements))
        Protocol_hash.Set.empty
        proposals
    in
    return_unit

  let check_period_kind_for_proposals current_period =
    match current_period.Voting_period.kind with
    | Proposal -> ok_unit
    | (Exploration | Cooldown | Promotion | Adoption) as current ->
        result_error (Wrong_voting_period_kind {current; expected = [Proposal]})

  let check_in_listings ctxt source =
    let open Lwt_result_syntax in
    let*! in_listings = Vote.in_listings ctxt source in
    fail_unless in_listings Source_not_in_vote_listings

  let check_count ~count_in_ctxt ~proposals_length =
    (* The proposal count of the proposer in the context should never
       have been increased above [max_proposals_per_delegate]. *)
    assert (Compare.Int.(count_in_ctxt <= Constants.max_proposals_per_delegate)) ;
    error_unless
      Compare.Int.(
        count_in_ctxt + proposals_length <= Constants.max_proposals_per_delegate)
      (Too_many_proposals
         {previous_count = count_in_ctxt; operation_count = proposals_length})

  let check_already_proposed ctxt proposer proposals =
    let open Lwt_result_syntax in
    List.iter_es
      (fun proposal ->
        let*! already_proposed = Vote.has_proposed ctxt proposer proposal in
        fail_when already_proposed (Already_proposed {proposal}))
      proposals

  (** Check that the [apply_testnet_dictator_proposals] function in
      {!module:Amendment} will not fail.

      The current function is designed to be exclusively called by
      [check_proposals] right below.

      @return [Error Testnet_dictator_multiple_proposals] if
      [proposals] has more than one element. *)
  let check_testnet_dictator_proposals chain_id proposals =
    (* This assertion should be ensured by the fact that
       {!Amendment.is_testnet_dictator} cannot be [true] on mainnet
       (so the current function cannot be called there). However, we
       still double check it because of its criticality. *)
    assert (Chain_id.(chain_id <> Constants.mainnet_id)) ;
    match proposals with
    | [] | [_] ->
        (* In [Amendment.apply_testnet_dictator_proposals], the call to
           {!Vote.init_current_proposal} (in the singleton list case)
           cannot fail because {!Vote.clear_current_proposal} is called
           right before.

           The calls to
           {!Voting_period.Testnet_dictator.overwrite_current_kind} may
           usually fail when the voting period is not
           initialized. However, this cannot happen here because the
           current function is only called in [check_proposals] after a
           successful call to {!Voting_period.get_current}. *)
        ok_unit
    | _ :: _ :: _ -> result_error Testnet_dictator_multiple_proposals

  (** Check that a Proposals operation can be safely applied.

      @return [Error Wrong_voting_period_index] if the operation's
      period and the current period in the {!type:context} do not have
      the same index.

      @return [Error Proposals_from_unregistered_delegate] if the
      source is not a registered delegate.

      @return [Error Empty_proposals] if the list of proposals is empty.

      @return [Error Proposals_contain_duplicate] if the list of
      proposals contains a duplicate element.

      @return [Error Wrong_voting_period_kind] if the voting period is
      not of the Proposal kind.

      @return [Error Source_not_in_vote_listings] if the source is not
      in the vote listings.

      @return [Error Too_many_proposals] if the operation causes the
      source's total number of proposals during the current voting
      period to exceed {!Constants.max_proposals_per_delegate}.

      @return [Error Already_proposed] if one of the proposals has
      already been proposed by the source in the current voting period.

      @return [Error Testnet_dictator_multiple_proposals] if the
      source is a testnet dictator and the operation contains more than
      one proposal.

      @return [Error Operation.Missing_signature] or [Error
      Operation.Invalid_signature] if the operation is unsigned or
      incorrectly signed. *)
  let check_proposals vi ~check_signature (operation : Kind.proposals operation)
      =
    let open Lwt_result_syntax in
    let (Single (Proposals {source; period; proposals})) =
      operation.protocol_data.contents
    in
    let* current_period = Voting_period.get_current vi.ctxt in
    let*? () = check_period_index ~expected:current_period.index period in
    let* () =
      if Amendment.is_testnet_dictator vi.ctxt vi.chain_id source then
        let*? () = check_testnet_dictator_proposals vi.chain_id proposals in
        return_unit
      else
        let* () = check_proposals_source_is_registered vi.ctxt source in
        let*? () = check_proposal_list_sanity proposals in
        let*? () = check_period_kind_for_proposals current_period in
        let* () = check_in_listings vi.ctxt source in
        let* count_in_ctxt = Vote.get_delegate_proposal_count vi.ctxt source in
        let proposals_length = List.length proposals in
        let*? () = check_count ~count_in_ctxt ~proposals_length in
        check_already_proposed vi.ctxt source proposals
    in
    let*! pk =
      if check_signature then
        (* Retrieving the public key should not fail as it *should* be
           called after checking that the delegate is in the vote
           listings (or is a testnet dictator), which implies that it
           is a manager with a revealed key. *)
        Lwt.map Option.some (Contract.get_manager_key vi.ctxt source)
      else Lwt.return_none
    in
    let check_signature =
      match pk with
      | Some pk ->
          [
            (fun () ->
              Result.map_e
                (fun pk ->
                  Operation.check_signature vi.ctxt pk vi.chain_id operation)
                pk);
          ]
      | None -> []
    in
    return check_signature

  (** Check that a Proposals operation is compatible with previously
      validated operations in the current block/mempool.

      @return [Error Operation_conflict] if the current block/mempool
      already contains a Proposals operation from the same source
      (regardless of whether this source is a testnet dictator or an
      ordinary manager). *)
  let check_proposals_conflict vs oph (operation : Kind.proposals operation) =
    let open Result_syntax in
    let (Single (Proposals {source; _})) = operation.protocol_data.contents in
    match
      Signature.Public_key_hash.Map.find_opt
        source
        vs.voting_state.proposals_seen
    with
    | None -> return_unit
    | Some existing ->
        Error (Operation_conflict {existing; new_operation = oph})

  let wrap_proposals_conflict = function
    | Ok () -> ok_unit
    | Error conflict ->
        result_error Validate_errors.Voting.(Conflicting_proposals conflict)

  let add_proposals vs oph (operation : Kind.proposals operation) =
    let (Single (Proposals {source; _})) = operation.protocol_data.contents in
    let proposals_seen =
      Signature.Public_key_hash.Map.add
        source
        oph
        vs.voting_state.proposals_seen
    in
    let voting_state = {vs.voting_state with proposals_seen} in
    {vs with voting_state}

  let remove_proposals vs (operation : Kind.proposals operation) =
    let (Single (Proposals {source; _})) = operation.protocol_data.contents in
    let proposals_seen =
      Signature.Public_key_hash.Map.remove source vs.voting_state.proposals_seen
    in
    {vs with voting_state = {vs.voting_state with proposals_seen}}

  let check_ballot_source_is_registered ctxt source =
    let open Lwt_result_syntax in
    let*! is_registered = Delegate.registered ctxt source in
    fail_unless is_registered (Ballot_from_unregistered_delegate source)

  let check_period_kind_for_ballot current_period =
    match current_period.Voting_period.kind with
    | Exploration | Promotion -> ok_unit
    | (Cooldown | Proposal | Adoption) as current ->
        result_error
          (Wrong_voting_period_kind
             {current; expected = [Exploration; Promotion]})

  let check_current_proposal ctxt op_proposal =
    let open Lwt_result_syntax in
    let* current_proposal = Vote.get_current_proposal ctxt in
    fail_unless
      (Protocol_hash.equal op_proposal current_proposal)
      (Ballot_for_wrong_proposal
         {current = current_proposal; submitted = op_proposal})

  let check_source_has_not_already_voted ctxt source =
    let open Lwt_result_syntax in
    let*! has_ballot = Vote.has_recorded_ballot ctxt source in
    fail_when has_ballot Already_submitted_a_ballot

  (** Check that a Ballot operation can be safely applied.

      @return [Error Ballot_from_unregistered_delegate] if the source
      is not a registered delegate.

      @return [Error Wrong_voting_period_index] if the operation's
      period and the current period in the {!type:context} do not have
      the same index.

      @return [Error Wrong_voting_period_kind] if the voting period is
      not of the Exploration or Promotion kind.

      @return [Error Ballot_for_wrong_proposal] if the operation's
      proposal is different from the current proposal in the context.

      @return [Error Already_submitted_a_ballot] if the source has
      already voted during the current voting period.

      @return [Error Source_not_in_vote_listings] if the source is not
      in the vote listings.

      @return [Error Operation.Missing_signature] or [Error
      Operation.Invalid_signature] if the operation is unsigned or
      incorrectly signed. *)
  let check_ballot vi ~check_signature (operation : Kind.ballot operation) =
    let open Lwt_result_syntax in
    let (Single (Ballot {source; period; proposal; ballot = _})) =
      operation.protocol_data.contents
    in
    let* () = check_ballot_source_is_registered vi.ctxt source in
    let* current_period = Voting_period.get_current vi.ctxt in
    let*? () = check_period_index ~expected:current_period.index period in
    let*? () = check_period_kind_for_ballot current_period in
    let* () = check_current_proposal vi.ctxt proposal in
    let* () = check_source_has_not_already_voted vi.ctxt source in
    let* () = check_in_listings vi.ctxt source in
    let*! pk =
      if check_signature then
        (* Retrieving the public key should not fail as it *should* be
           called after checking that the delegate is in the vote
           listings (or is a testnet dictator), which implies that it
           is a manager with a revealed key. *)
        Lwt.map Option.some (Contract.get_manager_key vi.ctxt source)
      else Lwt.return_none
    in
    let check_signature =
      match pk with
      | Some pk ->
          [
            (fun () ->
              Result.map_e
                (fun pk ->
                  Operation.check_signature vi.ctxt pk vi.chain_id operation)
                pk);
          ]
      | None -> []
    in
    return check_signature

  (** Check that a Ballot operation is compatible with previously
      validated operations in the current block/mempool.

      @return [Error Operation_conflict] if the current block/mempool
      already contains a Ballot operation from the same source. *)
  let check_ballot_conflict vs oph (operation : Kind.ballot operation) =
    let (Single (Ballot {source; _})) = operation.protocol_data.contents in
    match
      Signature.Public_key_hash.Map.find_opt source vs.voting_state.ballots_seen
    with
    | None -> ok_unit
    | Some existing ->
        Error (Operation_conflict {existing; new_operation = oph})

  let wrap_ballot_conflict = function
    | Ok () -> ok_unit
    | Error conflict -> result_error (Conflicting_ballot conflict)

  let add_ballot vs oph (operation : Kind.ballot operation) =
    let (Single (Ballot {source; _})) = operation.protocol_data.contents in
    let ballots_seen =
      Signature.Public_key_hash.Map.add source oph vs.voting_state.ballots_seen
    in
    let voting_state = {vs.voting_state with ballots_seen} in
    {vs with voting_state}

  let remove_ballot vs (operation : Kind.ballot operation) =
    let (Single (Ballot {source; _})) = operation.protocol_data.contents in
    let ballots_seen =
      Signature.Public_key_hash.Map.remove source vs.voting_state.ballots_seen
    in
    {vs with voting_state = {vs.voting_state with ballots_seen}}
end

module Anonymous = struct
  open Validate_errors.Anonymous

  let check_activate_account vi (operation : Kind.activate_account operation) =
    let open Lwt_result_syntax in
    let (Single (Activate_account {id = edpkh; activation_code})) =
      operation.protocol_data.contents
    in
    let blinded_pkh =
      Blinded_public_key_hash.of_ed25519_pkh activation_code edpkh
    in
    let*! exists = Commitment.exists vi.ctxt blinded_pkh in
    let*? () = error_unless exists (Invalid_activation {pkh = edpkh}) in
    return_unit

  let check_activate_account_conflict vs oph
      (operation : Kind.activate_account operation) =
    let (Single (Activate_account {id = edpkh; _})) =
      operation.protocol_data.contents
    in
    match
      Ed25519.Public_key_hash.Map.find_opt
        edpkh
        vs.anonymous_state.activation_pkhs_seen
    with
    | None -> ok_unit
    | Some existing ->
        Error (Operation_conflict {existing; new_operation = oph})

  let wrap_activate_account_conflict
      (operation : Kind.activate_account operation) = function
    | Ok () -> ok_unit
    | Error conflict ->
        let (Single (Activate_account {id = edpkh; _})) =
          operation.protocol_data.contents
        in
        result_error (Conflicting_activation {edpkh; conflict})

  let add_activate_account vs oph (operation : Kind.activate_account operation)
      =
    let (Single (Activate_account {id = edpkh; _})) =
      operation.protocol_data.contents
    in
    let activation_pkhs_seen =
      Ed25519.Public_key_hash.Map.add
        edpkh
        oph
        vs.anonymous_state.activation_pkhs_seen
    in
    {vs with anonymous_state = {vs.anonymous_state with activation_pkhs_seen}}

  let remove_activate_account vs (operation : Kind.activate_account operation) =
    let (Single (Activate_account {id = edpkh; _})) =
      operation.protocol_data.contents
    in
    let activation_pkhs_seen =
      Ed25519.Public_key_hash.Map.remove
        edpkh
        vs.anonymous_state.activation_pkhs_seen
    in
    {vs with anonymous_state = {vs.anonymous_state with activation_pkhs_seen}}

  let check_denunciation_age vi kind given_level =
    let open Result_syntax in
    let current_cycle = vi.current_level.cycle in
    let given_cycle = (Level.from_raw vi.ctxt given_level).cycle in
    let last_denunciable_cycle =
      Cycle.add given_cycle Constants.denunciation_period
    in
    let* () =
      error_unless
        Cycle.(given_cycle <= current_cycle)
        (match kind with
        | `Consensus_denounciation kind ->
            Too_early_denunciation
              {kind; level = given_level; current = vi.current_level.level}
        | `Dal_denounciation ->
            Too_early_dal_denunciation
              {level = given_level; current = vi.current_level.level})
    in
    error_unless
      Cycle.(last_denunciable_cycle >= current_cycle)
      (match kind with
      | `Consensus_denounciation kind ->
          Outdated_denunciation
            {kind; level = given_level; last_cycle = last_denunciable_cycle}
      | `Dal_denounciation ->
          Outdated_dal_denunciation
            {level = given_level; last_cycle = last_denunciable_cycle})

  (** Checks the signature of the given consensus operation [op]. To
      be called on denounced operations.

      [consensus_key] is the consensus key of the denounced delegate:
      when [op] is a standalone (pre)attestation, it assumed to be the
      key associated with the operation's [slot]. When [op] is an
      aggregate, [consensus_key] is not used since the signature
      depends on the whole committee. *)
  let check_consensus_operation_signature (type a) vi consensus_key
      (op : a Kind.consensus Operation.t) =
    let open Lwt_result_syntax in
    match op.protocol_data.contents with
    | Single (Preattestation _) ->
        Operation.check_signature
          vi.ctxt
          consensus_key.Consensus_key.consensus_pk
          vi.chain_id
          op
        |> Lwt.return
    | Single (Attestation _) ->
        Consensus.check_attestation_signature vi consensus_key op |> Lwt.return
    | Single
        (Preattestations_aggregate {consensus_content = {level; _}; committee})
      ->
        let attested_level = Level.from_raw vi.ctxt level in
        let* _ctxt, public_keys =
          (* [ctxt] is part of the accumulator so that attesting
             rights data for [level] are loaded at most once. *)
          List.fold_left_es
            (fun (ctxt, public_keys) slot ->
              let* ctxt, consensus_key =
                Stake_distribution.attestation_slot_owner
                  ctxt
                  ~attested_level
                  slot
              in
              match consensus_key.consensus_pk with
              | Bls pk -> return (ctxt, pk :: public_keys)
              | _ -> tzfail Validate_errors.Consensus.Non_bls_key_in_aggregate)
            (vi.ctxt, [])
            committee
        in
        Consensus.check_preattestations_aggregate_signature vi public_keys op
    | Single
        (Attestations_aggregate
           {consensus_content = {level; round; block_payload_hash}; committee})
      ->
        let serialized_op =
          let attestation : Kind.attestation operation =
            (* Reconstructing an attestation to match the content signed by
               each delegate. Fields 'slot' and 'dal_content' are filled with
               dummy values as they are not part of the signed payload *)
            let consensus_content =
              {slot = Slot.zero; level; round; block_payload_hash}
            in
            let contents =
              Single (Attestation {consensus_content; dal_content = None})
            in
            {shell = op.shell; protocol_data = {contents; signature = None}}
          in
          Operation.(
            serialize_unsigned_operation bls_mode_unsigned_encoding attestation)
        in
        let* _ctxt, pks, weighted_pks =
          List.fold_left_es
            (fun (ctxt, pks, weighted_pks) (slot, dal) ->
              let* ctxt, consensus_key =
                Stake_distribution.attestation_slot_owner
                  ctxt
                  ~attested_level:(Level.from_raw vi.ctxt level)
                  slot
              in
              match consensus_key.consensus_pk with
              | Bls consensus_pk -> (
                  let pks = consensus_pk :: pks in
                  match (dal, consensus_key.companion_pk) with
                  | None, _ -> return (ctxt, pks, weighted_pks)
                  | Some {attestation = dal_attestation}, Some companion_pk ->
                      let weight =
                        Dal.Attestation.Dal_dependent_signing.weight
                          ~consensus_pk
                          ~companion_pk
                          ~op:serialized_op
                          dal_attestation
                      in
                      let weighted_pks =
                        (weight, companion_pk) :: weighted_pks
                      in
                      return (ctxt, pks, weighted_pks)
                  | Some _, None ->
                      tzfail
                        (Validate_errors.Consensus
                         .Missing_companion_key_for_bls_dal
                           (Consensus_key.pkh consensus_key)))
              | _ -> tzfail Validate_errors.Consensus.Non_bls_key_in_aggregate)
            (vi.ctxt, [], [])
            committee
        in
        Consensus.check_attestations_aggregate_signature vi pks weighted_pks op
        |> Lwt.return

  let misbehaviour_kind (type a) (op : a Kind.consensus operation) =
    match op.protocol_data.contents with
    | Single (Preattestation _ | Preattestations_aggregate _) ->
        Misbehaviour.Double_preattesting
    | Single (Attestation _ | Attestations_aggregate _) ->
        Misbehaviour.Double_attesting

  let consensus_aggregate_content (type a) (op : a Kind.consensus operation) =
    match op.protocol_data.contents with
    | Single
        ( Preattestation {level; round; block_payload_hash; _}
        | Attestation
            {consensus_content = {level; round; block_payload_hash; _}; _} ) ->
        {level; round; block_payload_hash}
    | Single
        ( Preattestations_aggregate {consensus_content; _}
        | Attestations_aggregate {consensus_content; _} ) ->
        consensus_content

  let check_no_duplicates_in_committee_preattestation committee
      error_if_duplicate =
    let open Result_syntax in
    let* (_ : Slot.Set.t) =
      List.fold_left_e
        (fun seen_slots slot ->
          let* () =
            error_when (Slot.Set.mem slot seen_slots) error_if_duplicate
          in
          return (Slot.Set.add slot seen_slots))
        Slot.Set.empty
        committee
    in
    return_unit

  let check_no_duplicates_in_committee_attestation committee error_if_duplicate
      =
    let open Result_syntax in
    let* (_ : Slot.Set.t) =
      List.fold_left_e
        (fun seen_slots (slot, (_ : dal_content option)) ->
          let* () =
            error_when (Slot.Set.mem slot seen_slots) error_if_duplicate
          in
          return (Slot.Set.add slot seen_slots))
        Slot.Set.empty
        committee
    in
    return_unit

  let check_double_consensus_operation_evidence vi
      (operation : Kind.double_consensus_operation_evidence operation) =
    let open Lwt_result_syntax in
    let (Single (Double_consensus_operation_evidence {slot; op1; op2})) =
      operation.protocol_data.contents
    in
    let kind = misbehaviour_kind op1 in
    let ({level; round; block_payload_hash} : consensus_aggregate_content) =
      consensus_aggregate_content op1
    in
    let ({level = level2; round = round2; block_payload_hash = bph2}
          : consensus_aggregate_content) =
      consensus_aggregate_content op2
    in
    let*? () =
      (* Both denounced operations must be of the same kind and point
         to the same level and round. *)
      error_unless
        (Misbehaviour.equal_kind kind (misbehaviour_kind op2)
        && Raw_level.(level = level2)
        && Round.(round = round2))
        (Invalid_denunciation kind)
    in
    let misbehaviour = {Misbehaviour.level; round; kind} in
    let*? () =
      (* Both denounced operations must involve [slot]. That is,
         they must be either a standalone (pre)attestation for [slot],
         or an aggregate whose committee includes [slot]. *)
      let open Result_syntax in
      let check_slot_and_committee (type a) (op : a Kind.consensus Operation.t)
          =
        match op.protocol_data.contents with
        | Single (Preattestation consensus_content)
        | Single (Attestation {consensus_content; _}) ->
            error_unless
              Slot.(consensus_content.slot = slot)
              (Invalid_denunciation kind)
        | Single (Preattestations_aggregate {committee; _}) ->
            let* () =
              error_unless
                (List.mem ~equal:Slot.equal slot committee)
                (Invalid_denunciation kind)
            in
            check_no_duplicates_in_committee_preattestation
              committee
              (Invalid_denunciation kind)
        | Single (Attestations_aggregate {committee; _}) ->
            let* () =
              error_unless
                (List.mem_assoc ~equal:Slot.equal slot committee)
                (Invalid_denunciation kind)
            in
            check_no_duplicates_in_committee_attestation
              committee
              (Invalid_denunciation kind)
      in
      let* () = check_slot_and_committee op1 in
      check_slot_and_committee op2
    in
    let*? () =
      (* For the double operations to be punishable, they must have at
         least one difference that is considered the delegate's fault:
         different payloads or different branches. Note that different
         payloads would endanger the consensus process, while
         different branches could be used to spam mempools with a lot
         of valid operations.

         Indeed, we cannot punish a pair of operations just because
         they have different hashes: the hash also depends on the
         signature, and using signature malleability, anyone could
         inject a consensus operation with all the same data as the
         delegate's original operation but a different signature. *)
      error_unless
        (Block_payload_hash.(block_payload_hash <> bph2)
        || Block_hash.(op1.shell.branch <> op2.shell.branch))
        (Invalid_denunciation kind)
    in
    let*? () =
      (* We require an order on hashes so that the denunciation
         constructed from two given operations is unique. *)
      error_unless
        Operation_hash.(Operation.hash op1 < Operation.hash op2)
        (Invalid_denunciation kind)
    in
    let level = Level.from_raw vi.ctxt level in
    let*? () =
      check_denunciation_age vi (`Consensus_denounciation kind) level.level
    in
    let* ctxt, consensus_key =
      Stake_distribution.attestation_slot_owner
        vi.ctxt
        ~attested_level:level
        slot
    in
    let delegate = consensus_key.delegate in
    let* already_slashed =
      Delegate.already_denounced ctxt delegate misbehaviour
    in
    let*? () =
      error_unless
        (not already_slashed)
        (Already_denounced {kind; delegate; level})
    in
    let* () = check_consensus_operation_signature vi consensus_key op1 in
    let* () = check_consensus_operation_signature vi consensus_key op2 in
    return_unit

  let check_double_consensus_operation_evidence_conflict vs oph key =
    match
      Double_operation_evidence_map.find
        key
        vs.anonymous_state.double_consensus_operation_evidences_seen
    with
    | None -> ok_unit
    | Some existing ->
        Error (Operation_conflict {existing; new_operation = oph})

  let conflict_key_of_double_consensus_operation
      (operation : Kind.double_consensus_operation_evidence operation) =
    let (Single (Double_consensus_operation_evidence {slot; op1; _})) =
      operation.protocol_data.contents
    in
    let ({level; round; _} : consensus_aggregate_content) =
      consensus_aggregate_content op1
    in
    (level, round, slot, misbehaviour_kind op1)

  let check_double_consensus_operation_evidence_conflict vs oph
      (operation : Kind.double_consensus_operation_evidence operation) =
    let key = conflict_key_of_double_consensus_operation operation in
    check_double_consensus_operation_evidence_conflict vs oph key

  let wrap_denunciation_conflict kind = function
    | Ok () -> ok_unit
    | Error conflict -> result_error (Conflicting_denunciation {kind; conflict})

  let add_double_consensus_operation_evidence vs oph key =
    let double_consensus_operation_evidences_seen =
      Double_operation_evidence_map.add
        key
        oph
        vs.anonymous_state.double_consensus_operation_evidences_seen
    in
    {
      vs with
      anonymous_state =
        {vs.anonymous_state with double_consensus_operation_evidences_seen};
    }

  let add_double_consensus_operation_evidence vs oph
      (operation : Kind.double_consensus_operation_evidence operation) =
    let key = conflict_key_of_double_consensus_operation operation in
    add_double_consensus_operation_evidence vs oph key

  let remove_double_consensus_operation_evidence vs
      (operation : Kind.double_consensus_operation_evidence operation) =
    let key = conflict_key_of_double_consensus_operation operation in
    let double_consensus_operation_evidences_seen =
      Double_operation_evidence_map.remove
        key
        vs.anonymous_state.double_consensus_operation_evidences_seen
    in
    let anonymous_state =
      {vs.anonymous_state with double_consensus_operation_evidences_seen}
    in
    {vs with anonymous_state}

  let check_double_baking_evidence vi
      (operation : Kind.double_baking_evidence operation) =
    let open Lwt_result_syntax in
    let (Single (Double_baking_evidence {bh1; bh2})) =
      operation.protocol_data.contents
    in
    let hash1 = Block_header.hash bh1 in
    let hash2 = Block_header.hash bh2 in
    let*? bh1_fitness = Fitness.from_raw bh1.shell.fitness in
    let round1 = Fitness.round bh1_fitness in
    let*? bh2_fitness = Fitness.from_raw bh2.shell.fitness in
    let round2 = Fitness.round bh2_fitness in
    let*? level1 = Raw_level.of_int32 bh1.shell.level in
    let*? level2 = Raw_level.of_int32 bh2.shell.level in
    let*? () =
      error_unless
        (Raw_level.(level1 = level2)
        && Round.(round1 = round2)
        &&
        (* we require an order on hashes to avoid the existence of
              equivalent evidences *)
        Block_hash.(hash1 < hash2))
        (Invalid_double_baking_evidence
           {hash1; level1; round1; hash2; level2; round2})
    in
    let misbehaviour =
      {Misbehaviour.level = level1; round = round1; kind = Double_baking}
    in
    let*? () =
      check_denunciation_age
        vi
        (`Consensus_denounciation Misbehaviour.Double_baking)
        level1
    in
    let level = Level.from_raw vi.ctxt level1 in
    let* ctxt, _, consensus_key1 =
      Stake_distribution.baking_rights_owner vi.ctxt level ~round:round1
    in
    let* ctxt, _, consensus_key2 =
      Stake_distribution.baking_rights_owner ctxt level ~round:round2
    in
    let delegate1, delegate2 =
      (consensus_key1.delegate, consensus_key2.delegate)
    in
    let*? () =
      error_unless
        Signature.Public_key_hash.(delegate1 = delegate2)
        (Inconsistent_denunciation {kind = Double_baking; delegate1; delegate2})
    in
    let delegate_pk, delegate = (consensus_key1.consensus_pk, delegate1) in
    let* already_slashed =
      Delegate.already_denounced ctxt delegate misbehaviour
    in
    let*? () =
      error_unless
        (not already_slashed)
        (Already_denounced {kind = Double_baking; delegate; level})
    in
    let*? () = Block_header.check_signature bh1 vi.chain_id delegate_pk in
    let*? () = Block_header.check_signature bh2 vi.chain_id delegate_pk in
    return_unit

  let check_double_baking_evidence_conflict vs oph
      (operation : Kind.double_baking_evidence operation) =
    let (Single (Double_baking_evidence {bh1; _})) =
      operation.protocol_data.contents
    in
    let bh1_fitness =
      Fitness.from_raw bh1.shell.fitness |> function
      | Ok f -> f
      | Error _ ->
          (* We assume the operation valid, it cannot fail anymore *)
          assert false
    in
    let round = Fitness.round bh1_fitness in
    let level = Fitness.level bh1_fitness in
    match
      Double_baking_evidence_map.find
        (level, round)
        vs.anonymous_state.double_baking_evidences_seen
    with
    | None -> ok_unit
    | Some existing ->
        Error (Operation_conflict {existing; new_operation = oph})

  let add_double_baking_evidence vs oph
      (operation : Kind.double_baking_evidence operation) =
    let (Single (Double_baking_evidence {bh1; _})) =
      operation.protocol_data.contents
    in
    let bh1_fitness =
      Fitness.from_raw bh1.shell.fitness |> function
      | Ok f -> f
      | Error _ -> assert false
    in
    let round = Fitness.round bh1_fitness in
    let level = Fitness.level bh1_fitness in
    let double_baking_evidences_seen =
      Double_baking_evidence_map.add
        (level, round)
        oph
        vs.anonymous_state.double_baking_evidences_seen
    in
    {
      vs with
      anonymous_state = {vs.anonymous_state with double_baking_evidences_seen};
    }

  let remove_double_baking_evidence vs
      (operation : Kind.double_baking_evidence operation) =
    let (Single (Double_baking_evidence {bh1; _})) =
      operation.protocol_data.contents
    in
    let bh1_fitness, level =
      match
        (Fitness.from_raw bh1.shell.fitness, Raw_level.of_int32 bh1.shell.level)
      with
      | Ok v, Ok v' -> (v, v')
      | _ ->
          (* The operation is valid therefore decoding cannot fail *)
          assert false
    in
    let round = Fitness.round bh1_fitness in
    let double_baking_evidences_seen =
      Double_baking_evidence_map.remove
        (level, round)
        vs.anonymous_state.double_baking_evidences_seen
    in
    let anonymous_state =
      {vs.anonymous_state with double_baking_evidences_seen}
    in
    {vs with anonymous_state}

  let check_shard_index_is_in_range ~number_of_shards shard_index =
    error_unless
      Compare.Int.(shard_index >= 0 && shard_index < number_of_shards)
      (Invalid_shard_index
         {given = shard_index; min = 0; max = number_of_shards - 1})

  (* This function validates an entrapment evidence by doing the following checks:
     - the included attestation is either a standalone attestation for
       the included consensus_slot, or an attestations_aggregate whose
       committee contains the included consensus_slot
     - the included attestation contains a dal_content for
       consensus_slot, in which the included slot index is attested
     - the included slot index and shard index are within bounds
     - the level of the faulty attestation is within the slashing period
     - the included shard is a trap
     - the delegate has not already been denounced for the same level and slot index
     - the delegate that signed the included attestation is assigned to the included shard
     - the signature verification of the included attestation succeeds
  *)
  let check_dal_entrapment_evidence vi
      (operation : Kind.dal_entrapment_evidence operation) =
    let open Lwt_result_syntax in
    let (Single
           (Dal_entrapment_evidence
              {attestation; consensus_slot; slot_index; shard_with_proof})) =
      operation.protocol_data.contents
    in
    let*? raw_level, dal_content =
      let open Result_syntax in
      match attestation.protocol_data.contents with
      | Single (Attestation {consensus_content = {slot; level; _}; dal_content})
        ->
          let* () =
            error_unless
              Slot.(slot = consensus_slot)
              Invalid_accusation_inconsistent_consensus_slot
          in
          return (level, dal_content)
      | Single
          (Attestations_aggregate {consensus_content = {level; _}; committee})
        -> (
          match List.assoc ~equal:Slot.equal consensus_slot committee with
          | None -> tzfail Invalid_accusation_inconsistent_consensus_slot
          | Some dal_content -> return (level, dal_content))
      | Single (Preattestation _ | Preattestations_aggregate _) ->
          tzfail Invalid_accusation_of_preattestation
    in
    match dal_content with
    | None ->
        tzfail
          (Invalid_accusation_no_dal_content
             {tb_slot = consensus_slot; level = raw_level; slot_index})
    | Some dal_content -> (
        let* dal_params = Dal.Past_parameters.parameters vi.ctxt raw_level in
        let attestation_lag = dal_params.attestation_lag in
        let*? () = check_denunciation_age vi `Dal_denounciation raw_level in
        let level = Level.from_raw vi.ctxt raw_level in
        let* ctxt, consensus_key =
          Stake_distribution.attestation_slot_owner
            vi.ctxt
            ~attested_level:level
            consensus_slot
        in
        let delegate = consensus_key.delegate in
        let* published_level =
          match Raw_level.(sub (succ raw_level) attestation_lag) with
          | None ->
              (* The slot couldn't have been published in this case *)
              tzfail
                (Invalid_accusation_slot_not_published
                   {delegate; level = raw_level; slot_index})
          | Some level -> return level
        in
        let* dal_params =
          Dal.Past_parameters.parameters vi.ctxt published_level
        in
        let number_of_slots = dal_params.number_of_slots in
        let*? () =
          Dal.Slot_index.check_is_in_range ~number_of_slots slot_index
        in
        let number_of_shards =
          dal_params.cryptobox_parameters.number_of_shards
        in
        let shard_index = shard_with_proof.shard.index in
        let*? () =
          check_shard_index_is_in_range ~number_of_shards shard_index
        in
        let*? () =
          error_unless
            (Dal.Attestation.is_attested dal_content.attestation slot_index)
            (Invalid_accusation_slot_not_attested
               {tb_slot = consensus_slot; level = raw_level; slot_index})
        in
        let*! already_denounced =
          Dal.Delegate.is_already_denounced ctxt delegate level slot_index
        in
        let*? () =
          error_unless
            (not already_denounced)
            (Dal_already_denounced {delegate; level = level.level})
        in
        let traps_fraction = dal_params.traps_fraction in
        let*? is_trap =
          Dal.Shard_with_proof.share_is_trap
            delegate
            shard_with_proof.shard.share
            ~traps_fraction
        in
        let*? () =
          error_unless
            is_trap
            (Invalid_accusation_shard_is_not_trap
               {
                 delegate;
                 level = level.level;
                 slot_index;
                 shard_index = shard_with_proof.shard.index;
               })
        in
        let* _ctxt, _, shard_owner =
          let*? tb_round = Round.of_int shard_index in
          Stake_distribution.baking_rights_owner vi.ctxt level ~round:tb_round
        in
        let*? () =
          error_unless
            (Signature.Public_key_hash.equal delegate shard_owner.delegate)
            (Invalid_accusation_wrong_shard_owner
               {
                 delegate;
                 level = level.level;
                 slot_index;
                 shard_index = shard_with_proof.shard.index;
                 shard_owner = shard_owner.delegate;
               })
        in
        let* slot_headers_opt =
          Dal.Slot.find_slot_headers ctxt published_level
        in
        match slot_headers_opt with
        | None ->
            (* It should not happen if 1) the denunciation age is correct, and 2) \
               the storage is updated correctly *)
            tzfail
              (Accusation_validity_error_cannot_get_slot_headers
                 {delegate; level = level.level; slot_index})
        | Some headers -> (
            let slot_header_opt =
              List.find
                (fun (Dal.Slot.Header.{id; _}, _publisher) ->
                  Dal.Slot_index.equal id.index slot_index)
                headers
            in
            match slot_header_opt with
            | Some (header, _publisher)
              when Raw_level.equal header.id.published_level published_level ->
                let*? cryptobox =
                  let open Result_syntax in
                  if
                    Dal.Parameters.equal
                      (Constants.parametric ctxt).dal.cryptobox_parameters
                      dal_params.cryptobox_parameters
                  then
                    let* _ctxt, cryptobox = Dal.make ctxt in
                    return cryptobox
                  else Dal.make_cryptobox dal_params.cryptobox_parameters
                in
                let*? () =
                  Dal.Shard_with_proof.verify
                    cryptobox
                    header.commitment
                    shard_with_proof
                in
                check_consensus_operation_signature vi consensus_key attestation
            | Some (header, _publisher) ->
                (* mismatch between published levels in \
                   storage versus in evidence; it should not happen *)
                tzfail
                  (Accusation_validity_error_levels_mismatch
                     {
                       delegate;
                       level = level.level;
                       slot_index;
                       accusation_published_level = published_level;
                       store_published_level = header.id.published_level;
                     })
            | None ->
                tzfail
                  (Invalid_accusation_slot_not_published
                     {delegate; level = level.level; slot_index})))

  let check_dal_entrapment_evidence vi
      (operation : Kind.dal_entrapment_evidence operation) =
    if (Constants.parametric vi.ctxt).dal.incentives_enable then
      check_dal_entrapment_evidence vi operation
    else tzfail Dal_errors.Dal_incentives_disabled

  let dal_entrapment_evidence_info
      (operation : Kind.dal_entrapment_evidence operation) =
    let (Single (Dal_entrapment_evidence {attestation; consensus_slot; _})) =
      operation.protocol_data.contents
    in
    let (Single
           ( Preattestation {level; _}
           | Attestation {consensus_content = {level; _}; _}
           | Preattestations_aggregate {consensus_content = {level; _}; _}
           | Attestations_aggregate {consensus_content = {level; _}; _} )) =
      attestation.protocol_data.contents
    in
    (level, consensus_slot)

  let check_dal_entrapment_evidence_conflict vs oph
      (operation : Kind.dal_entrapment_evidence operation) =
    let level, tb_slot = dal_entrapment_evidence_info operation in
    match
      Dal_entrapment_map.find
        (level, tb_slot)
        vs.anonymous_state.dal_entrapments_seen
    with
    | None -> ok_unit
    | Some existing ->
        Error (Operation_conflict {existing; new_operation = oph})

  let wrap_dal_entrapment_evidence_conflict = function
    | Ok () -> ok_unit
    | Error conflict -> result_error (Conflicting_dal_entrapment conflict)

  let add_dal_entrapment_evidence vs oph
      (operation : Kind.dal_entrapment_evidence operation) =
    let level, tb_slot = dal_entrapment_evidence_info operation in
    let dal_entrapments_seen =
      Dal_entrapment_map.add
        (level, tb_slot)
        oph
        vs.anonymous_state.dal_entrapments_seen
    in
    {vs with anonymous_state = {vs.anonymous_state with dal_entrapments_seen}}

  let remove_dal_entrapment_evidence vs
      (operation : Kind.dal_entrapment_evidence operation) =
    let level, tb_slot = dal_entrapment_evidence_info operation in
    let dal_entrapments_seen =
      Dal_entrapment_map.remove
        (level, tb_slot)
        vs.anonymous_state.dal_entrapments_seen
    in
    {vs with anonymous_state = {vs.anonymous_state with dal_entrapments_seen}}

  let check_drain_delegate info ~check_signature
      (operation : Kind.drain_delegate Operation.t) =
    let open Lwt_result_syntax in
    let (Single (Drain_delegate {delegate; destination; consensus_key})) =
      operation.protocol_data.contents
    in
    let*! is_registered = Delegate.registered info.ctxt delegate in
    let* () =
      fail_unless
        is_registered
        (Drain_delegate_on_unregistered_delegate delegate)
    in
    let* active_pk = Delegate.Consensus_key.active_pubkey info.ctxt delegate in
    let* () =
      fail_unless
        (Signature.Public_key_hash.equal active_pk.consensus_pkh consensus_key)
        (Invalid_drain_delegate_inactive_key
           {
             delegate;
             consensus_key;
             active_consensus_key = active_pk.consensus_pkh;
           })
    in
    let* () =
      fail_when
        (Signature.Public_key_hash.equal active_pk.consensus_pkh delegate)
        (Invalid_drain_delegate_no_consensus_key delegate)
    in
    let* () =
      fail_when
        (Signature.Public_key_hash.equal destination delegate)
        (Invalid_drain_delegate_noop delegate)
    in
    let*! is_destination_allocated =
      Contract.allocated info.ctxt (Contract.Implicit destination)
    in
    let* balance =
      Contract.get_balance info.ctxt (Contract.Implicit delegate)
    in
    let*? origination_burn =
      if is_destination_allocated then Ok Tez.zero
      else
        let cost_per_byte = Constants.cost_per_byte info.ctxt in
        let origination_size = Constants.origination_size info.ctxt in
        Tez.(cost_per_byte *? Int64.of_int origination_size)
    in
    let* drain_fees =
      let*? one_percent = Tez.(balance /? 100L) in
      return Tez.(max one one_percent)
    in
    let*? min_amount = Tez.(origination_burn +? drain_fees) in
    let* () =
      fail_when
        Tez.(balance < min_amount)
        (Invalid_drain_delegate_insufficient_funds_for_burn_or_fees
           {delegate; destination; min_amount})
    in
    let check_signature =
      if check_signature then
        [
          (fun () ->
            Operation.check_signature
              info.ctxt
              active_pk.consensus_pk
              info.chain_id
              operation);
        ]
      else []
    in
    return check_signature

  let check_drain_delegate_conflict state oph
      (operation : Kind.drain_delegate Operation.t) =
    let (Single (Drain_delegate {delegate; _})) =
      operation.protocol_data.contents
    in
    match
      Signature.Public_key_hash.Map.find_opt
        delegate
        state.manager_state.managers_seen
    with
    | None -> ok_unit
    | Some existing ->
        Error (Operation_conflict {existing; new_operation = oph})

  let wrap_drain_delegate_conflict (operation : Kind.drain_delegate Operation.t)
      =
    let (Single (Drain_delegate {delegate; _})) =
      operation.protocol_data.contents
    in
    function
    | Ok () -> ok_unit
    | Error conflict ->
        result_error (Conflicting_drain_delegate {delegate; conflict})

  let add_drain_delegate state oph (operation : Kind.drain_delegate Operation.t)
      =
    let (Single (Drain_delegate {delegate; _})) =
      operation.protocol_data.contents
    in
    let managers_seen =
      Signature.Public_key_hash.Map.add
        delegate
        oph
        state.manager_state.managers_seen
    in
    {state with manager_state = {managers_seen}}

  let remove_drain_delegate state (operation : Kind.drain_delegate Operation.t)
      =
    let (Single (Drain_delegate {delegate; _})) =
      operation.protocol_data.contents
    in
    let managers_seen =
      Signature.Public_key_hash.Map.remove
        delegate
        state.manager_state.managers_seen
    in
    {state with manager_state = {managers_seen}}

  let check_seed_nonce_revelation vi
      (operation : Kind.seed_nonce_revelation operation) =
    let open Lwt_result_syntax in
    let (Single (Seed_nonce_revelation {level = commitment_raw_level; nonce})) =
      operation.protocol_data.contents
    in
    let commitment_level = Level.from_raw vi.ctxt commitment_raw_level in
    let* () = Nonce.check_unrevealed vi.ctxt commitment_level nonce in
    return_unit

  let check_seed_nonce_revelation_conflict vs oph
      (operation : Kind.seed_nonce_revelation operation) =
    let (Single (Seed_nonce_revelation {level = commitment_raw_level; _})) =
      operation.protocol_data.contents
    in
    match
      Raw_level.Map.find_opt
        commitment_raw_level
        vs.anonymous_state.seed_nonce_levels_seen
    with
    | None -> ok_unit
    | Some existing ->
        Error (Operation_conflict {existing; new_operation = oph})

  let wrap_seed_nonce_revelation_conflict = function
    | Ok () -> ok_unit
    | Error conflict -> result_error (Conflicting_nonce_revelation conflict)

  let add_seed_nonce_revelation vs oph
      (operation : Kind.seed_nonce_revelation operation) =
    let (Single (Seed_nonce_revelation {level = commitment_raw_level; _})) =
      operation.protocol_data.contents
    in
    let seed_nonce_levels_seen =
      Raw_level.Map.add
        commitment_raw_level
        oph
        vs.anonymous_state.seed_nonce_levels_seen
    in
    let anonymous_state = {vs.anonymous_state with seed_nonce_levels_seen} in
    {vs with anonymous_state}

  let remove_seed_nonce_revelation vs
      (operation : Kind.seed_nonce_revelation operation) =
    let (Single (Seed_nonce_revelation {level = commitment_raw_level; _})) =
      operation.protocol_data.contents
    in
    let seed_nonce_levels_seen =
      Raw_level.Map.remove
        commitment_raw_level
        vs.anonymous_state.seed_nonce_levels_seen
    in
    let anonymous_state = {vs.anonymous_state with seed_nonce_levels_seen} in
    {vs with anonymous_state}

  let check_vdf_revelation vi (operation : Kind.vdf_revelation operation) =
    let open Lwt_result_syntax in
    let (Single (Vdf_revelation {solution})) =
      operation.protocol_data.contents
    in
    let* () = Seed.check_vdf vi.ctxt solution in
    return_unit

  let check_vdf_revelation_conflict vs oph =
    match vs.anonymous_state.vdf_solution_seen with
    | None -> ok_unit
    | Some existing ->
        Error (Operation_conflict {existing; new_operation = oph})

  let wrap_vdf_revelation_conflict = function
    | Ok () -> ok_unit
    | Error conflict -> result_error (Conflicting_vdf_revelation conflict)

  let add_vdf_revelation vs oph =
    {
      vs with
      anonymous_state = {vs.anonymous_state with vdf_solution_seen = Some oph};
    }

  let remove_vdf_revelation vs =
    let anonymous_state = {vs.anonymous_state with vdf_solution_seen = None} in
    {vs with anonymous_state}
end

module Manager = struct
  open Validate_errors.Manager

  (** State that simulates changes from individual operations that have
      an effect on future operations inside the same batch. *)
  type batch_state = {
    balance : Tez.t;
        (** Remaining balance in the contract, used to simulate the
            payment of fees by each operation in the batch. *)
    is_allocated : bool;
        (** Track whether the contract is still allocated. Indeed,
            previous operations' fee payment may empty the contract and
            this may deallocate the contract.

            TODO: https://gitlab.com/tezos/tezos/-/issues/3209 Change
            empty account cleanup mechanism to avoid the need for this
            field. *)
    total_gas_used : Gas.Arith.fp;
  }

  type batch_elt = Elt : 'kind Kind.manager contents -> batch_elt

  let rec batch_fold_left_e : type kind.
      f:('a -> batch_elt -> 'a tzresult) ->
      init:'a ->
      batch:kind Kind.manager contents_list ->
      'a tzresult =
   fun ~f ~init ~batch ->
    let open Result_syntax in
    match batch with
    | Single contents -> f init (Elt contents)
    | Cons (contents, tail) ->
        let* acc = f init (Elt contents) in
        batch_fold_left_e ~f ~init:acc ~batch:tail

  let check_source ~expected_source ~source =
    Option.iter_e
      (fun expected_source ->
        error_unless
          (Signature.Public_key_hash.equal source expected_source)
          (Inconsistent_sources {expected_source; source}))
      expected_source

  let check_counter ~previous_counter ~counter ~source =
    Option.iter_e
      (fun previous_counter ->
        error_unless
          Manager_counter.(succ previous_counter = counter)
          (Inconsistent_counters {source; previous_counter; counter}))
      previous_counter

  (** Used in [check_consistency] below. *)
  type consistency_state = {
    source : public_key_hash option;
        (** [None] while checking the first operation in the batch,
            then the source of the first operation in the batch. *)
    previous_counter : Manager_counter.t option;
        (** [None] while checking the first operation in the batch,
            then the counter of the last encountered operation. *)
  }

  (** Checks that a batch of manager operations is well-formed.

      The consistency checks from this function are universal
      requirements for all batches, regardless of the context. E.g. we
      check here that counters are internally consistent, but not that
      the first counter is the next expected counter for its source in
      the context.

      Accordingly, all potential errors here should be [`Permanent].

      {b Source consistency}

      All operations in the batch must have the same source.

      {b Counter consistency}

      The counters must be increasing and consecutive.

      {b Reveal position}

      A [Reveal] operation is only allowed if it is the very first
      operation in the batch.

      ---

      This function can return the following errors:

      - [Inconsistent_sources] when the batch contains multiple
      sources.

      - [Inconsistent_counters] when the counters are not increasing and consecutive.

      - [Incorrect_reveal_position] when a [Reveal] operation is not
      the first operation of the whole batch.  *)
  let check_consistency batch =
    let open Result_syntax in
    let initial_state = {source = None; previous_counter = None} in
    let check_elt {source = expected_source; previous_counter}
        (Elt (Manager_operation {source; counter; operation; _})) =
      (* Checks an individual operation of the batch. Relies on the
         given {!consistency_state} (see the type definition for
         invariants), and returns the appropriate {!consistency_state}
         for the next operation in the batch. *)
      let* () = check_source ~expected_source ~source in
      let* () = check_counter ~previous_counter ~counter ~source in
      let* () =
        match operation with
        | Reveal _ -> (
            (* A [Reveal] operation is only allowed if it is
               the first operation of the whole batch. The
               invariants of {!consistency_state} guarantee
               that [expected_source] is [None] if and only if
               we are currently checking the first
               operation. *)
            match expected_source with
            | None -> (* First op in the whole batch: OK *) return_unit
            | Some _ -> tzfail Incorrect_reveal_position)
        | _ -> return_unit
      in
      return {source = Some source; previous_counter = Some counter}
    in
    batch_fold_left_e ~f:check_elt ~init:initial_state ~batch

  (** Check a few simple properties of the batch, and return the
      initial {!batch_state} and the contract public key.

      Invariants checked:

      - The batch is well formed: see {!check_consistency}.

      - The source's contract is allocated.

      - The first counter is the source's next expected counter.

      - The source's public key has been revealed (either before the
        considered batch, or during its first operation).

      Note that currently, the [op] batch contains only one signature,
      so all operations in the batch are required to originate from the
      same manager. This may change in the future, in order to allow
      several managers to group-sign a sequence of operations. *)
  let check_sanity_and_find_public_key vi (type kind)
      (contents_list : kind Kind.manager contents_list) =
    let open Lwt_result_syntax in
    let*? (_ : consistency_state) = check_consistency contents_list in
    let source, first_counter =
      match contents_list with
      | Single (Manager_operation {source; counter; _})
      | Cons (Manager_operation {source; counter; _}, _) ->
          (source, counter)
    in
    let* balance = Contract.check_allocated_and_get_balance vi.ctxt source in
    let* () = Contract.check_counter_increment vi.ctxt source first_counter in
    let revealed_key =
      match contents_list with
      | Single
          (Manager_operation {operation = Reveal {public_key; proof = _}; _})
      | Cons
          (Manager_operation {operation = Reveal {public_key; proof = _}; _}, _)
        ->
          Some public_key
      | _ -> None
    in
    let* pk =
      (* Note that it is important to always retrieve the public
         key. This includes the case where the key ends up not being
         used because the signature check is skipped in
         {!validate_manager_operation} called with
         [~check_signature:false]. Indeed, the mempool may use
         this argument when it has already checked the signature of
         the operation in the past; but if there has been a branch
         reorganization since then, the key might not be revealed in
         the new branch anymore, in which case
         {!Contract.get_manager_key} will return an error. *)
      match revealed_key with
      | Some pk -> return pk
      | None -> Contract.get_manager_key vi.ctxt source
    in
    let initial_batch_state =
      {
        balance;
        (* Initial contract allocation is ensured by the success of
           the call to {!Contract.check_allocated_and_get_balance}
           above. *)
        is_allocated = true;
        total_gas_used = Gas.Arith.zero;
      }
    in
    return (initial_batch_state, pk)

  let check_gas_limit info ~gas_limit =
    Gas.check_gas_limit
      ~hard_gas_limit_per_operation:
        info.manager_info.hard_gas_limit_per_operation
      ~gas_limit

  let check_storage_limit vi storage_limit =
    error_unless
      Compare.Z.(
        storage_limit <= vi.manager_info.hard_storage_limit_per_operation
        && storage_limit >= Z.zero)
      Fees.Storage_limit_too_high

  let assert_pvm_kind_enabled vi kind =
    let open Result_syntax in
    let* () =
      error_when
        ((not (Constants.sc_rollup_arith_pvm_enable vi.ctxt))
        && Sc_rollup.Kind.(equal kind Example_arith))
        Sc_rollup_arith_pvm_disabled
    in
    error_when
      ((not (Constants.sc_rollup_riscv_pvm_enable vi.ctxt))
      && Sc_rollup.Kind.(equal kind Riscv))
      Sc_rollup_riscv_pvm_disabled

  let assert_not_zero_messages messages =
    match messages with
    | [] -> result_error Sc_rollup_errors.Sc_rollup_add_zero_messages
    | _ -> ok_unit

  let assert_zk_rollup_feature_enabled vi =
    error_unless (Constants.zk_rollup_enable vi.ctxt) Zk_rollup_feature_disabled

  let consume_decoding_gas remaining_gas lexpr =
    record_trace Gas_quota_exceeded_init_deserialize
    @@
    (* Fail early if the operation does not have enough gas to
          cover the deserialization cost. We always consider the full
          deserialization cost, independently from the internal state
          of the lazy_expr. Otherwise we might risk getting different
          results if the operation has already been deserialized
          before (e.g. when retrieved in JSON format). Note that the
          lazy_expr is not actually decoded here; its deserialization
          cost is estimated from the size of its bytes. *)
    Script.consume_decoding_gas remaining_gas lexpr

  let may_trace_gas_limit_too_high info =
    match info.mode with
    | Application _ | Partial_validation _ | Construction _ -> fun x -> x
    | Mempool ->
        (* [Gas.check_limit] will only
           raise a "temporary" error, however when
           {!validate_operation} is called on a batch in isolation
           (like e.g. in the mempool) it must "refuse" operations
           whose total gas limit (the sum of the [gas_limit]s of each
           operation) is already above the block limit. We add the
           "permanent" error [Gas.Gas_limit_too_high] on top of the
           trace to this effect. *)
        record_trace Gas.Gas_limit_too_high

  let check_bls_proof_for_manager_pk remaining_gas source
      (public_key : Signature.Public_key.t) proof =
    let open Result_syntax in
    match (public_key, proof) with
    | Bls _bls_public_key, None ->
        result_error
          (Validate_errors.Manager.Missing_bls_proof
             {kind = Manager_pk; source; public_key})
    | Bls bls_public_key, Some _ ->
        (* Compute the gas cost to encode the manager public key and
           check the proof. *)
        let gas_cost_for_sig_check =
          let open Saturation_repr.Syntax in
          let size = Bls.Public_key.size bls_public_key in
          Operation_costs.serialization_cost size
          + Michelson_v1_gas.Cost_of.Interpreter.check_signature_on_algo
              Bls
              size
        in
        let* (_ : Gas.Arith.fp) =
          record_trace
            Insufficient_gas_for_manager
            (Gas.consume_from
               (Gas.Arith.fp remaining_gas)
               gas_cost_for_sig_check)
        in
        return_unit
    | (Ed25519 _ | Secp256k1 _ | P256 _ | Mldsa44 _), Some _proof ->
        result_error
          (Validate_errors.Manager.Unused_bls_proof
             {kind = Manager_pk; source; public_key})
    | (Ed25519 _ | Secp256k1 _ | P256 _ | Mldsa44 _), None -> return_unit

  let check_update_consensus_key vi remaining_gas source
      (public_key : Signature.Public_key.t) proof
      (kind : Operation_repr.consensus_key_kind) =
    let open Result_syntax in
    let* () = Delegate.Consensus_key.check_not_tz5 kind public_key in
    if Constants.allow_tz4_delegate_enable vi.ctxt then
      match (public_key, proof, kind) with
      | Bls _bls_public_key, None, kind ->
          result_error
            (Validate_errors.Manager.Missing_bls_proof
               {
                 kind = Operation_repr.consensus_to_public_key_kind kind;
                 source;
                 public_key;
               })
      | Bls bls_public_key, Some _, _kind ->
          (* Compute the gas cost to encode the consensus public key and
             check the proof. *)
          let gas_cost_for_sig_check =
            let open Saturation_repr.Syntax in
            let size = Bls.Public_key.size bls_public_key in
            Operation_costs.serialization_cost size
            + Michelson_v1_gas.Cost_of.Interpreter.check_signature_on_algo
                Bls
                size
          in
          let* (_ : Gas.Arith.fp) =
            record_trace
              Insufficient_gas_for_manager
              (Gas.consume_from
                 (Gas.Arith.fp remaining_gas)
                 gas_cost_for_sig_check)
          in
          return_unit
      | ( (Ed25519 _ | Secp256k1 _ | P256 _ | Mldsa44 _),
          _,
          Operation_repr.Companion ) ->
          result_error
            (Validate_errors.Manager.Update_companion_key_not_tz4
               {source; public_key})
      | (Ed25519 _ | Secp256k1 _ | P256 _ | Mldsa44 _), Some _proof, Consensus
        ->
          result_error
            (Validate_errors.Manager.Unused_bls_proof
               {kind = Consensus_pk; source; public_key})
      | (Ed25519 _ | Secp256k1 _ | P256 _ | Mldsa44 _), None, Consensus ->
          return_unit
    else
      let* () = Delegate.Consensus_key.check_not_tz4 kind public_key in
      match kind with
      | Companion ->
          result_error
            (Validate_errors.Manager.Update_companion_key_not_tz4
               {source; public_key})
      | Consensus ->
          if Option.is_some proof then
            result_error
              (Validate_errors.Manager.Unused_bls_proof
                 {kind = Consensus_pk; source; public_key})
          else return_unit

  let check_kind_specific_content (type kind)
      (contents : kind Kind.manager contents) remaining_gas vi =
    let open Result_syntax in
    let (Manager_operation
           {
             source;
             fee = _;
             counter = _;
             operation;
             gas_limit = _;
             storage_limit = _;
           }) =
      contents
    in
    match operation with
    | Reveal {public_key; proof} ->
        let* () = Contract.check_public_key public_key source in
        check_bls_proof_for_manager_pk remaining_gas source public_key proof
    | Transaction {parameters; _} ->
        let* (_ : Gas.Arith.fp) =
          consume_decoding_gas remaining_gas parameters
        in
        return_unit
    | Origination {script; _} ->
        let* remaining_gas = consume_decoding_gas remaining_gas script.code in
        let* (_ : Gas.Arith.fp) =
          consume_decoding_gas remaining_gas script.storage
        in
        return_unit
    | Register_global_constant {value} ->
        let* (_ : Gas.Arith.fp) = consume_decoding_gas remaining_gas value in
        return_unit
    | Delegation (Some pkh) ->
        let* () = Delegate.check_not_tz5 pkh in
        if Constants.allow_tz4_delegate_enable vi.ctxt then return_unit
        else Delegate.check_not_tz4 pkh
    | Update_consensus_key {public_key; proof; kind} ->
        check_update_consensus_key vi remaining_gas source public_key proof kind
    | Delegation None | Set_deposits_limit _ | Increase_paid_storage _ ->
        return_unit
    | Transfer_ticket {contents; ty; _} ->
        let* remaining_gas = consume_decoding_gas remaining_gas contents in
        let* (_ : Gas.Arith.fp) = consume_decoding_gas remaining_gas ty in
        return_unit
    | Sc_rollup_originate {kind; _} -> assert_pvm_kind_enabled vi kind
    | Sc_rollup_add_messages {messages; _} -> assert_not_zero_messages messages
    | Sc_rollup_cement _ | Sc_rollup_publish _ | Sc_rollup_refute _
    | Sc_rollup_timeout _ | Sc_rollup_execute_outbox_message _
    | Sc_rollup_recover_bond _ ->
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/3063
           Should we successfully precheck Sc_rollup_recover_bond and any
           (simple) Sc rollup operation, or should we add some some checks to make
           the operations Branch_delayed if they cannot be successfully
           prechecked? *)
        return_unit
    | Dal_publish_commitment slot_header ->
        Dal_apply.validate_publish_commitment vi.ctxt slot_header
    | Zk_rollup_origination _ | Zk_rollup_publish _ | Zk_rollup_update _ ->
        assert_zk_rollup_feature_enabled vi

  let check_contents (type kind) vi batch_state
      (contents : kind Kind.manager contents) ~consume_gas_for_sig_check
      remaining_block_gas =
    let open Lwt_result_syntax in
    let (Manager_operation
           {source; fee; counter = _; operation = _; gas_limit; storage_limit})
        =
      contents
    in
    let*? () = check_gas_limit vi ~gas_limit in
    let total_gas_used =
      Gas.Arith.(add batch_state.total_gas_used (fp gas_limit))
    in
    let*? () =
      may_trace_gas_limit_too_high vi
      @@ error_unless
           Gas.Arith.(fp total_gas_used <= remaining_block_gas)
           Gas.Block_quota_exceeded
    in
    (* Part of the gas cost of the operation which is independent of
       the contents of the operation. It is
       Michelson_v1_gas.Cost_of.manager_operation constant plus the
       cost of checking the signature if the operation is the first of
       the batch. *)
    let fixed_gas_cost =
      let manager_op_cost = Michelson_v1_gas.Cost_of.manager_operation in
      match consume_gas_for_sig_check with
      | None -> manager_op_cost
      | Some gas_for_sig_check -> Gas.(manager_op_cost +@ gas_for_sig_check)
    in
    let*? remaining_gas =
      record_trace
        Insufficient_gas_for_manager
        (Gas.consume_from (Gas.Arith.fp gas_limit) fixed_gas_cost)
    in
    let*? () = check_storage_limit vi storage_limit in
    let*? () =
      (* {!Contract.must_be_allocated} has already been called while
         initializing [batch_state]. This checks that the contract has
         not been emptied by spending fees for previous operations in
         the batch. *)
      error_unless
        batch_state.is_allocated
        (Contract_storage.Empty_implicit_contract source)
    in
    let*? () = check_kind_specific_content contents remaining_gas vi in
    (* Gas should no longer be consumed below this point, because it
       would not take into account any gas consumed by
       {!check_kind_specific_content}. If you really need to consume gas here, then you
       must make {!check_kind_specific_content} return the [remaining_gas].*)
    let* balance, is_allocated =
      Contract.simulate_spending
        vi.ctxt
        ~balance:batch_state.balance
        ~amount:fee
        source
    in
    return {total_gas_used; balance; is_allocated}

  (** This would be [fold_left_es (check_contents vi) batch_state
     contents_list] if [contents_list] were an ordinary [list].  The
     [consume_gas_for_sig_check] arg indicates whether or not gas for
     checking the signature of the batch should be consumed; it is
     [None] if the cost has already been consumed and [Some cost] if
     the cost to be consumed is [cost] and remains to be
     consumed. This cost is consumed in the first operation of the
     batch. *)
  let rec check_contents_list : type kind.
      info ->
      batch_state ->
      kind Kind.manager contents_list ->
      consume_gas_for_sig_check:Gas.cost option ->
      Gas.Arith.fp ->
      Gas.Arith.fp tzresult Lwt.t =
   fun vi batch_state contents_list ~consume_gas_for_sig_check remaining_gas ->
    let open Lwt_result_syntax in
    match contents_list with
    | Single contents ->
        let* batch_state =
          check_contents
            vi
            batch_state
            contents
            ~consume_gas_for_sig_check
            remaining_gas
        in
        return batch_state.total_gas_used
    | Cons (contents, tail) ->
        let* batch_state =
          check_contents
            vi
            batch_state
            contents
            ~consume_gas_for_sig_check
            remaining_gas
        in
        check_contents_list
          vi
          batch_state
          tail
          ~consume_gas_for_sig_check:None
          remaining_gas

  let check_manager_operation vi ~check_signature
      (operation : _ Kind.manager operation) remaining_block_gas =
    let open Lwt_result_syntax in
    let contents_list = operation.protocol_data.contents in
    let* batch_state, source_pk =
      check_sanity_and_find_public_key vi contents_list
    in
    let signature_checking_gas_cost =
      Operation_costs.check_signature_cost
        (Michelson_v1_gas.Cost_of.Interpreter.algo_of_public_key source_pk)
        operation
    in
    let* gas_used =
      check_contents_list
        vi
        batch_state
        contents_list
        ~consume_gas_for_sig_check:(Some signature_checking_gas_cost)
        remaining_block_gas
    in
    let check_signature =
      if check_signature then
        [
          (fun () ->
            Operation.check_signature vi.ctxt source_pk vi.chain_id operation);
        ]
      else []
    in
    return (gas_used, check_signature)

  let check_manager_operation_conflict (type kind) vs oph
      (operation : kind Kind.manager operation) =
    let source =
      match operation.protocol_data.contents with
      | Single (Manager_operation {source; _})
      | Cons (Manager_operation {source; _}, _) ->
          source
    in
    (* One-operation-per-manager-per-block restriction (1M) *)
    match
      Signature.Public_key_hash.Map.find_opt
        source
        vs.manager_state.managers_seen
    with
    | None -> ok_unit
    | Some existing ->
        Error (Operation_conflict {existing; new_operation = oph})

  let wrap_check_manager_operation_conflict (type kind)
      (operation : kind Kind.manager operation) =
    let source =
      match operation.protocol_data.contents with
      | Single (Manager_operation {source; _})
      | Cons (Manager_operation {source; _}, _) ->
          source
    in
    function
    | Ok () -> ok_unit
    | Error conflict -> result_error (Manager_restriction {source; conflict})

  let add_manager_operation (type kind) vs oph
      (operation : kind Kind.manager operation) =
    let source =
      match operation.protocol_data.contents with
      | Single (Manager_operation {source; _})
      | Cons (Manager_operation {source; _}, _) ->
          source
    in
    let managers_seen =
      Signature.Public_key_hash.Map.add
        source
        oph
        vs.manager_state.managers_seen
    in
    {vs with manager_state = {managers_seen}}

  (* Return the new [block_state] with the updated remaining gas used:
     - In non-mempool modes, this value is
       [block_state.remaining_block_gas], in which the gas from the
       validated operation has been subtracted.

     - In [Mempool] mode, the [block_state] should remain
       unchanged. Indeed, we only want each batch to not exceed the
       block limit individually, without taking other operations
       into account. *)
  let may_update_remaining_gas_used mode (block_state : block_state)
      operation_gas_used =
    match mode with
    | Application _ | Partial_validation _ | Construction _ ->
        let remaining_block_gas =
          Gas.Arith.(sub block_state.remaining_block_gas operation_gas_used)
        in
        {block_state with remaining_block_gas}
    | Mempool -> block_state

  let remove_manager_operation (type kind) vs
      (operation : kind Kind.manager operation) =
    let source =
      match operation.protocol_data.contents with
      | Single (Manager_operation {source; _})
      | Cons (Manager_operation {source; _}, _) ->
          source
    in
    let managers_seen =
      Signature.Public_key_hash.Map.remove source vs.manager_state.managers_seen
    in
    {vs with manager_state = {managers_seen}}

  let validate_manager_operation ~check_signature info operation_state
      block_state oph operation =
    let open Lwt_result_syntax in
    let* gas_used, pending_checks =
      check_manager_operation
        info
        ~check_signature
        operation
        block_state.remaining_block_gas
    in
    let*? () = resolve_checks pending_checks in
    let*? () =
      check_manager_operation_conflict operation_state oph operation
      |> wrap_check_manager_operation_conflict operation
    in
    let operation_state = add_manager_operation operation_state oph operation in
    let block_state =
      may_update_remaining_gas_used info.mode block_state gas_used
    in
    return {info; operation_state; block_state}
end

let init_validation_state ctxt mode chain_id ~predecessor_level_and_round =
  let info = init_info ctxt mode chain_id ~predecessor_level_and_round in
  let operation_state = empty_operation_conflict_state in
  let block_state = init_block_state info in
  {info; operation_state; block_state}

(* Pre-condition: Shell block headers' checks have already been done.
   These checks must ensure that:
   - the block header level is the succ of the predecessor block level
   - the timestamp of the predecessor is lower than the current block's
   - the fitness of the block is greater than its predecessor's
   - the number of operations by validation passes does not exceed the quota
     established by the protocol
   - the size of an operation does not exceed [max_operation_data_length]
*)
let begin_any_application ctxt chain_id ~predecessor_level
    ~predecessor_timestamp (block_header : Block_header.t) fitness ~is_partial =
  let open Lwt_result_syntax in
  let predecessor_round = Fitness.predecessor_round fitness in
  let round = Fitness.round fitness in
  let current_level = Level.current ctxt in
  let* ctxt, _slot, block_producer =
    Stake_distribution.baking_rights_owner ctxt current_level ~round
  in
  let*? () =
    Block_header.begin_validate_block_header
      ~block_header
      ~chain_id
      ~predecessor_timestamp
      ~predecessor_round
      ~fitness
      ~timestamp:block_header.shell.timestamp
      ~delegate_pk:block_producer.consensus_pk
      ~round_durations:(Constants.round_durations ctxt)
      ~proof_of_work_threshold:(Constants.proof_of_work_threshold ctxt)
      ~expected_commitment:current_level.expected_commitment
  in
  let* () =
    Consensus.check_delegate_is_not_forbidden ctxt block_producer.delegate
  in
  let* ctxt, _slot, _payload_producer =
    (* We just make sure that this call will not fail in apply.ml *)
    Stake_distribution.baking_rights_owner
      ctxt
      current_level
      ~round:block_header.protocol_data.contents.payload_round
  in
  let predecessor_hash = block_header.shell.predecessor in
  let block_info =
    {
      round;
      locked_round = Fitness.locked_round fitness;
      predecessor_hash;
      header_contents = block_header.protocol_data.contents;
    }
  in
  let mode =
    if is_partial then Partial_validation block_info else Application block_info
  in
  let validation_state =
    init_validation_state
      ctxt
      mode
      chain_id
      ~predecessor_level_and_round:
        (Some (predecessor_level.Level.level, predecessor_round))
  in
  return validation_state

let begin_partial_validation ctxt chain_id ~predecessor_level
    ~predecessor_timestamp block_header fitness =
  begin_any_application
    ctxt
    chain_id
    ~predecessor_level
    ~predecessor_timestamp
    block_header
    fitness
    ~is_partial:true

let begin_application ctxt chain_id ~predecessor_level ~predecessor_timestamp
    block_header fitness =
  begin_any_application
    ctxt
    chain_id
    ~predecessor_level
    ~predecessor_timestamp
    block_header
    fitness
    ~is_partial:false

let begin_full_construction ctxt chain_id ~predecessor_level ~predecessor_round
    ~predecessor_timestamp ~predecessor_hash round
    (header_contents : Block_header.contents) =
  let open Lwt_result_syntax in
  let round_durations = Constants.round_durations ctxt in
  let timestamp = Timestamp.current ctxt in
  let*? () =
    Block_header.check_timestamp
      round_durations
      ~timestamp
      ~round
      ~predecessor_timestamp
      ~predecessor_round
  in
  let current_level = Level.current ctxt in
  let* ctxt, _slot, block_producer =
    Stake_distribution.baking_rights_owner ctxt current_level ~round
  in
  let* () =
    Consensus.check_delegate_is_not_forbidden ctxt block_producer.delegate
  in
  let* ctxt, _slot, _payload_producer =
    (* We just make sure that this call will not fail in apply.ml *)
    Stake_distribution.baking_rights_owner
      ctxt
      current_level
      ~round:header_contents.payload_round
  in
  let validation_state =
    init_validation_state
      ctxt
      (Construction {round; predecessor_hash; header_contents})
      chain_id
      ~predecessor_level_and_round:
        (Some (predecessor_level.Level.level, predecessor_round))
  in
  return validation_state

let begin_partial_construction ctxt chain_id ~predecessor_level
    ~predecessor_round =
  let validation_state =
    init_validation_state
      ctxt
      Mempool
      chain_id
      ~predecessor_level_and_round:
        (Some (predecessor_level.Level.level, predecessor_round))
  in
  validation_state

let begin_no_predecessor_info ctxt chain_id =
  init_validation_state ctxt Mempool chain_id ~predecessor_level_and_round:None

let check_operation ?(check_signature = true) info (type kind)
    (operation : kind operation) : (unit -> unit tzresult) list tzresult Lwt.t =
  let open Lwt_result_syntax in
  let no_pending_checks :
      (unit, error trace) result Lwt.t ->
      ((unit -> unit tzresult) list, error trace) result Lwt.t =
    Lwt.map @@ Result.map @@ fun () -> []
  in
  match operation.protocol_data.contents with
  | Single (Preattestation _) ->
      Lwt.map
        (Result.map snd)
        (Consensus.check_preattestation info ~check_signature operation)
  | Single (Attestation _) ->
      Lwt.map
        (Result.map snd)
        (Consensus.check_attestation info ~check_signature operation)
  | Single (Preattestations_aggregate _) ->
      tzfail Validate_errors.Consensus.Aggregate_in_mempool
  | Single (Attestations_aggregate _) ->
      tzfail Validate_errors.Consensus.Aggregate_in_mempool
  | Single (Proposals _) ->
      Voting.check_proposals info ~check_signature operation
  | Single (Ballot _) -> Voting.check_ballot info ~check_signature operation
  | Single (Activate_account _) ->
      Anonymous.check_activate_account info operation |> no_pending_checks
  | Single (Double_consensus_operation_evidence _) ->
      Anonymous.check_double_consensus_operation_evidence info operation
      |> no_pending_checks
  | Single (Double_baking_evidence _) ->
      Anonymous.check_double_baking_evidence info operation |> no_pending_checks
  | Single (Dal_entrapment_evidence _) ->
      Anonymous.check_dal_entrapment_evidence info operation
      |> no_pending_checks
  | Single (Drain_delegate _) ->
      Anonymous.check_drain_delegate info ~check_signature operation
  | Single (Seed_nonce_revelation _) ->
      Anonymous.check_seed_nonce_revelation info operation |> no_pending_checks
  | Single (Vdf_revelation _) ->
      Anonymous.check_vdf_revelation info operation |> no_pending_checks
  | Single (Manager_operation _) ->
      let remaining_gas =
        Gas.Arith.fp (Constants.hard_gas_limit_per_block info.ctxt)
      in
      Lwt.map
        (Result.map snd)
        (Manager.check_manager_operation
           info
           ~check_signature
           operation
           remaining_gas)
  | Cons (Manager_operation _, _) ->
      let remaining_gas =
        Gas.Arith.fp (Constants.hard_gas_limit_per_block info.ctxt)
      in
      Lwt.map
        (Result.map snd)
        (Manager.check_manager_operation
           info
           ~check_signature
           operation
           remaining_gas)
  | Single (Failing_noop _) -> tzfail Validate_errors.Failing_noop_error

let check_operation_conflict (type kind) operation_conflict_state oph
    (operation : kind operation) =
  match operation.protocol_data.contents with
  | Single (Preattestation _) ->
      Consensus.check_preattestation_conflict
        operation_conflict_state
        oph
        operation
  | Single (Attestation _) ->
      Consensus.check_attestation_conflict
        operation_conflict_state
        oph
        operation
  | Single (Preattestations_aggregate _) | Single (Attestations_aggregate _) ->
      (* This case is unreachable because the operation is assumed to be valid,
         and aggregates are never valid in mempools. *)
      assert false
  | Single (Proposals _) ->
      Voting.check_proposals_conflict operation_conflict_state oph operation
  | Single (Ballot _) ->
      Voting.check_ballot_conflict operation_conflict_state oph operation
  | Single (Activate_account _) ->
      Anonymous.check_activate_account_conflict
        operation_conflict_state
        oph
        operation
  | Single (Double_consensus_operation_evidence _) ->
      Anonymous.check_double_consensus_operation_evidence_conflict
        operation_conflict_state
        oph
        operation
  | Single (Double_baking_evidence _) ->
      Anonymous.check_double_baking_evidence_conflict
        operation_conflict_state
        oph
        operation
  | Single (Dal_entrapment_evidence _) ->
      Anonymous.check_dal_entrapment_evidence_conflict
        operation_conflict_state
        oph
        operation
  | Single (Drain_delegate _) ->
      Anonymous.check_drain_delegate_conflict
        operation_conflict_state
        oph
        operation
  | Single (Seed_nonce_revelation _) ->
      Anonymous.check_seed_nonce_revelation_conflict
        operation_conflict_state
        oph
        operation
  | Single (Vdf_revelation _) ->
      Anonymous.check_vdf_revelation_conflict operation_conflict_state oph
  | Single (Manager_operation _) ->
      Manager.check_manager_operation_conflict
        operation_conflict_state
        oph
        operation
  | Cons (Manager_operation _, _) ->
      Manager.check_manager_operation_conflict
        operation_conflict_state
        oph
        operation
  | Single (Failing_noop _) -> (* Nothing to do *) ok_unit

let add_valid_operation operation_conflict_state oph (type kind)
    (operation : kind operation) =
  match operation.protocol_data.contents with
  | Single (Preattestation _) ->
      Consensus.add_preattestation operation_conflict_state oph operation
  | Single (Attestation _) ->
      Consensus.add_attestation operation_conflict_state oph operation
  | Single (Preattestations_aggregate _) | Single (Attestations_aggregate _) ->
      (* This case is unreachable because the operation is assumed to be valid,
         and aggregates are never valid in mempools. *)
      assert false
  | Single (Proposals _) ->
      Voting.add_proposals operation_conflict_state oph operation
  | Single (Ballot _) ->
      Voting.add_ballot operation_conflict_state oph operation
  | Single (Activate_account _) ->
      Anonymous.add_activate_account operation_conflict_state oph operation
  | Single (Double_consensus_operation_evidence _) ->
      Anonymous.add_double_consensus_operation_evidence
        operation_conflict_state
        oph
        operation
  | Single (Double_baking_evidence _) ->
      Anonymous.add_double_baking_evidence
        operation_conflict_state
        oph
        operation
  | Single (Dal_entrapment_evidence _) ->
      Anonymous.add_dal_entrapment_evidence
        operation_conflict_state
        oph
        operation
  | Single (Drain_delegate _) ->
      Anonymous.add_drain_delegate operation_conflict_state oph operation
  | Single (Seed_nonce_revelation _) ->
      Anonymous.add_seed_nonce_revelation operation_conflict_state oph operation
  | Single (Vdf_revelation _) ->
      Anonymous.add_vdf_revelation operation_conflict_state oph
  | Single (Manager_operation _) ->
      Manager.add_manager_operation operation_conflict_state oph operation
  | Cons (Manager_operation _, _) ->
      Manager.add_manager_operation operation_conflict_state oph operation
  | Single (Failing_noop _) -> (* Nothing to do *) operation_conflict_state

(* Hypothesis:
   - the [operation] has been validated and is present in [vs];
   - this function is only valid for the mempool mode. *)
let remove_operation operation_conflict_state (type kind)
    (operation : kind operation) =
  match operation.protocol_data.contents with
  | Single (Preattestation _) ->
      Consensus.remove_preattestation operation_conflict_state operation
  | Single (Attestation _) ->
      Consensus.remove_attestation operation_conflict_state operation
  | Single (Preattestations_aggregate _) | Single (Attestations_aggregate _) ->
      (* This case is unreachable because the operation is assumed to be valid,
         and aggregates are never valid in mempools. *)
      assert false
  | Single (Proposals _) ->
      Voting.remove_proposals operation_conflict_state operation
  | Single (Ballot _) -> Voting.remove_ballot operation_conflict_state operation
  | Single (Activate_account _) ->
      Anonymous.remove_activate_account operation_conflict_state operation
  | Single (Double_consensus_operation_evidence _) ->
      Anonymous.remove_double_consensus_operation_evidence
        operation_conflict_state
        operation
  | Single (Double_baking_evidence _) ->
      Anonymous.remove_double_baking_evidence operation_conflict_state operation
  | Single (Dal_entrapment_evidence _) ->
      Anonymous.remove_dal_entrapment_evidence
        operation_conflict_state
        operation
  | Single (Drain_delegate _) ->
      Anonymous.remove_drain_delegate operation_conflict_state operation
  | Single (Seed_nonce_revelation _) ->
      Anonymous.remove_seed_nonce_revelation operation_conflict_state operation
  | Single (Vdf_revelation _) ->
      Anonymous.remove_vdf_revelation operation_conflict_state
  | Single (Manager_operation _) ->
      Manager.remove_manager_operation operation_conflict_state operation
  | Cons (Manager_operation _, _) ->
      Manager.remove_manager_operation operation_conflict_state operation
  | Single (Failing_noop _) -> (* Nothing to do *) operation_conflict_state

let check_validation_pass_consistency vi vs validation_pass =
  let open Lwt_result_syntax in
  match vi.mode with
  | Mempool | Construction _ -> return vs
  | Application _ | Partial_validation _ -> (
      match (vs.last_op_validation_pass, validation_pass) with
      | None, validation_pass ->
          return {vs with last_op_validation_pass = validation_pass}
      | Some previous_vp, Some validation_pass ->
          let* () =
            fail_unless
              Compare.Int.(previous_vp <= validation_pass)
              (Validate_errors.Block.Inconsistent_validation_passes_in_block
                 {expected = previous_vp; provided = validation_pass})
          in
          return {vs with last_op_validation_pass = Some validation_pass}
      | Some _, None -> tzfail Validate_errors.Failing_noop_error)

(** Increment [vs.op_count] for all operations, and record
    non-consensus operation hashes in [vs.recorded_operations_rev]. *)
let record_operation vs ophash validation_pass_opt =
  let op_count = vs.op_count + 1 in
  match validation_pass_opt with
  | Some n when Compare.Int.(n = Operation_repr.consensus_pass) ->
      {vs with op_count}
  | _ ->
      {
        vs with
        op_count;
        recorded_operations_rev = ophash :: vs.recorded_operations_rev;
      }

let validate_operation ?(check_signature = true)
    {info; operation_state; block_state} oph
    (packed_operation : packed_operation) =
  let open Lwt_result_syntax in
  let {shell; protocol_data = Operation_data protocol_data} =
    packed_operation
  in
  let validation_pass_opt = Operation.acceptable_pass packed_operation in
  let* block_state =
    check_validation_pass_consistency info block_state validation_pass_opt
  in
  let block_state = record_operation block_state oph validation_pass_opt in
  let operation : _ operation = {shell; protocol_data} in
  match (info.mode, validation_pass_opt) with
  | Partial_validation _, Some n
    when Compare.Int.(n <> Operation_repr.consensus_pass) ->
      (* Do not validate non-consensus operations in
         [Partial_validation] mode. *)
      return {info; operation_state; block_state}
  | (Application _ | Partial_validation _ | Construction _ | Mempool), _ -> (
      match operation.protocol_data.contents with
      | Single (Preattestation _) ->
          Consensus.validate_preattestation
            ~check_signature
            info
            operation_state
            block_state
            oph
            operation
      | Single (Attestation _) ->
          Consensus.validate_attestation
            ~check_signature
            info
            operation_state
            block_state
            oph
            operation
      | Single (Preattestations_aggregate _) ->
          Consensus.validate_preattestations_aggregate
            ~check_signature
            info
            operation_state
            block_state
            oph
            operation
      | Single (Attestations_aggregate _) ->
          Consensus.validate_attestations_aggregate
            ~check_signature
            info
            operation_state
            block_state
            oph
            operation
      | Single (Proposals _) ->
          let open Voting in
          let* pending_checks =
            check_proposals info ~check_signature operation
          in
          let*? () = resolve_checks pending_checks in
          let*? () =
            check_proposals_conflict operation_state oph operation
            |> wrap_proposals_conflict
          in
          let operation_state = add_proposals operation_state oph operation in
          return {info; operation_state; block_state}
      | Single (Ballot _) ->
          let open Voting in
          let* pending_checks = check_ballot info ~check_signature operation in
          let*? () = resolve_checks pending_checks in
          let*? () =
            check_ballot_conflict operation_state oph operation
            |> wrap_ballot_conflict
          in
          let operation_state = add_ballot operation_state oph operation in
          return {info; operation_state; block_state}
      | Single (Activate_account _) ->
          let open Anonymous in
          let* () = check_activate_account info operation in
          let*? () =
            check_activate_account_conflict operation_state oph operation
            |> wrap_activate_account_conflict operation
          in
          let operation_state =
            add_activate_account operation_state oph operation
          in
          return {info; operation_state; block_state}
      | Single (Double_consensus_operation_evidence {op1; _}) ->
          let open Anonymous in
          let* () = check_double_consensus_operation_evidence info operation in
          let*? () =
            check_double_consensus_operation_evidence_conflict
              operation_state
              oph
              operation
            |> wrap_denunciation_conflict (misbehaviour_kind op1)
          in
          let operation_state =
            add_double_consensus_operation_evidence
              operation_state
              oph
              operation
          in
          return {info; operation_state; block_state}
      | Single (Double_baking_evidence _) ->
          let open Anonymous in
          let* () = check_double_baking_evidence info operation in
          let*? () =
            check_double_baking_evidence_conflict operation_state oph operation
            |> wrap_denunciation_conflict Double_baking
          in
          let operation_state =
            add_double_baking_evidence operation_state oph operation
          in
          return {info; operation_state; block_state}
      | Single (Dal_entrapment_evidence _) ->
          let open Anonymous in
          let* () = check_dal_entrapment_evidence info operation in
          let*? () =
            check_dal_entrapment_evidence_conflict operation_state oph operation
            |> wrap_dal_entrapment_evidence_conflict
          in
          let operation_state =
            add_dal_entrapment_evidence operation_state oph operation
          in
          return {info; operation_state; block_state}
      | Single (Drain_delegate _) ->
          let open Anonymous in
          let* pending_checks =
            check_drain_delegate info ~check_signature operation
          in
          let*? () = resolve_checks pending_checks in
          let*? () =
            check_drain_delegate_conflict operation_state oph operation
            |> wrap_drain_delegate_conflict operation
          in
          let operation_state =
            add_drain_delegate operation_state oph operation
          in
          return {info; operation_state; block_state}
      | Single (Seed_nonce_revelation _) ->
          let open Anonymous in
          let* () = check_seed_nonce_revelation info operation in
          let*? () =
            check_seed_nonce_revelation_conflict operation_state oph operation
            |> wrap_seed_nonce_revelation_conflict
          in
          let operation_state =
            add_seed_nonce_revelation operation_state oph operation
          in
          return {info; operation_state; block_state}
      | Single (Vdf_revelation _) ->
          let open Anonymous in
          let* () = check_vdf_revelation info operation in
          let*? () =
            check_vdf_revelation_conflict operation_state oph
            |> wrap_vdf_revelation_conflict
          in
          let operation_state = add_vdf_revelation operation_state oph in
          return {info; operation_state; block_state}
      | Single (Manager_operation _) ->
          Manager.validate_manager_operation
            ~check_signature
            info
            operation_state
            block_state
            oph
            operation
      | Cons (Manager_operation _, _) ->
          Manager.validate_manager_operation
            ~check_signature
            info
            operation_state
            block_state
            oph
            operation
      | Single (Failing_noop _) -> tzfail Validate_errors.Failing_noop_error)

(** Block finalization *)

open Validate_errors.Block

let check_attesting_power vi bs =
  let open Lwt_result_syntax in
  let* are_attestations_required =
    (* The migration block (whose level is [first_level_of_protocol])
       is always considered final, and is not attested. Therefore, the
       block at the next level does not need to contain attestations.
       (Note that the migration block itself is validated by the
       previous protocol, so the returned value for it does not matter.) *)
    let* first_level_of_protocol = Protocol_activation_level.get vi.ctxt in
    let level_position_in_protocol =
      Raw_level.diff vi.current_level.level first_level_of_protocol
    in
    return Compare.Int32.(level_position_in_protocol > 1l)
  in
  if are_attestations_required then
    (* The attested level is the predecessor of the block's level. *)
    match Level.pred vi.ctxt vi.current_level with
    | None ->
        (* This cannot happen because [required_attestations = true]
           ensures that [vi.current_level >= 2]. *)
        assert false
    | Some attested_level ->
        (* We can safely drop the context: it is only updated for the
           cache of the stake info for a given level, which should
           already be cached at this time anyways. *)
        let* _ctxt, required =
          Attesting_power.consensus_threshold vi.ctxt ~attested_level
        in
        let provided =
          Attesting_power.get vi.ctxt ~attested_level bs.attesting_power
        in
        fail_unless
          Compare.Int64.(provided >= required)
          (Not_enough_attestations {required; provided})
  else return_unit

(** Check that the locked round in the fitness and the locked round
    observed in the preattestations are the same.

    This check is not called in construction mode because there is
    no provided fitness (meaning that we do not know whether the block
    should contain any preattestations).

    When the observed locked round is [Some _], we actually already
    know that it is identical to the fitness locked round, otherwise
    {!Consensus.check_preexisting_block_preattestation} would have
    rejected the preattestations. But this check is needed to reject
    blocks where the fitness locked round has a value yet there are no
    preattestations (ie. the observed locked round is [None]). *)
let check_fitness_locked_round bs fitness_locked_round =
  let observed_locked_round = Option.map fst bs.locked_round_evidence in
  error_unless
    (Option.equal Round.equal observed_locked_round fitness_locked_round)
    Fitness.Wrong_fitness

(** When there are preattestations, check that they point to a round
    before the block's round, and that their total power is high enough.

    Note that this function does not check whether the block actually
    contains preattestations when they are mandatory. This is checked by
    {!check_fitness_locked_round} instead. *)
let check_preattestation_round_and_power vi vs round =
  let open Lwt_result_syntax in
  match vs.locked_round_evidence with
  | None -> return_unit
  | Some (preattestation_round, total_attesting_power) ->
      let*? () =
        (* Actually, this check should never fail, because we have
           already called {!Consensus.check_round_before_block} for
           all preattestations in a block. Nevertheless, it does not
           cost much to check again here. *)
        error_when
          Round.(preattestation_round >= round)
          (Locked_round_after_block_round
             {locked_round = preattestation_round; round})
      in
      (* The preattestations' level is the same as the block's level. *)
      let attested_level = vi.current_level in
      (* We can safely drop the context: it is only updated for the cache of
         the stake info for a given level, which should already be cached
         at this time anyways. *)
      let* _ctxt, consensus_threshold =
        Attesting_power.consensus_threshold vi.ctxt ~attested_level
      in
      let total_attesting_power =
        Attesting_power.get vi.ctxt ~attested_level total_attesting_power
      in
      let*? () =
        error_when
          Compare.Int64.(total_attesting_power < consensus_threshold)
          (Insufficient_locked_round_evidence
             {total_attesting_power; consensus_threshold})
      in
      return_unit

let check_payload_hash block_state ~predecessor_hash
    (block_header_contents : Block_header.contents) =
  let expected =
    Block_payload.hash
      ~predecessor_hash
      ~payload_round:block_header_contents.payload_round
      (List.rev block_state.recorded_operations_rev)
  in
  let provided = block_header_contents.payload_hash in
  error_unless
    (Block_payload_hash.equal expected provided)
    (Invalid_payload_hash {expected; provided})

let finalize_block {info; block_state; _} =
  let open Lwt_result_syntax in
  match info.mode with
  | Application {round; locked_round; predecessor_hash; header_contents} ->
      let* () = check_attesting_power info block_state in
      let*? () = check_fitness_locked_round block_state locked_round in
      let* () = check_preattestation_round_and_power info block_state round in
      let*? () =
        check_payload_hash block_state ~predecessor_hash header_contents
      in
      return_unit
  | Partial_validation {round; locked_round; _} ->
      let* () = check_attesting_power info block_state in
      let*? () = check_fitness_locked_round block_state locked_round in
      let* () = check_preattestation_round_and_power info block_state round in
      return_unit
  | Construction {round; predecessor_hash; header_contents} ->
      let* () = check_attesting_power info block_state in
      let* () = check_preattestation_round_and_power info block_state round in
      let*? () =
        match block_state.locked_round_evidence with
        | Some _ ->
            check_payload_hash block_state ~predecessor_hash header_contents
        | None ->
            (* In construction mode, when there is no locked round
               evidence (ie. no preattestations), the baker cannot know
               the payload hash before selecting the operations.
               Therefore, we do not check the initially given payload
               hash. The baker will have to patch the resulting block
               header with the actual payload hash afterwards. *)
            ok_unit
      in
      return_unit
  | Mempool ->
      (* There is no block to finalize in mempool mode. *)
      return_unit
