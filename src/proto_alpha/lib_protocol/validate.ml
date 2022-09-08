(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Alpha_context

(** {2 Definition and initialization of [info] and [state]}

    These live in memory during the validation of a block, or until a
    change of head block; they are never put in the storage. *)

(** Since the expected features of preendorsement and endorsement are
    the same for all operations in the considered block, we compute
    them once and for all at the begining of the block.

    See [expected_features_for_block_validation],
    [expected_features_for_block_construction], and
    [expected_features_for_mempool] in the [Consensus] module below. *)

type expected_features = {
  level : Raw_level.t;
  round : Round.t option;
      (** This always contains a value, except for the case of
          preendorsements during block construction. See
          [Consensus.check_round_equal] below for its usage. *)
  branch : Block_hash.t;
  payload_hash : Block_payload_hash.t;
}

type expected_preendorsement =
  | Expected_preendorsement of {
      expected_features : expected_features;
      block_round : Round.t option;
          (** During block validation or construction, we must also check
              that the preendorsement round is lower than the block
              round. In mempool mode, this field is [None]. *)
    }
  | No_locked_round_for_block_validation_preendorsement
      (** A preexisting block whose fitness indicates no locked round
          should contain no preendorsements. *)
  | Fresh_proposal_for_block_construction_preendorsement
      (** A constructed block with a fresh proposal should contain no
          preendorsements. *)
  | No_expected_branch_for_mempool_preendorsement of {
      expected_level : Raw_level.t;
    }  (** See [No_expected_branch_for_mempool_endorsement] below. *)
  | No_predecessor_info_cannot_validate_preendorsement
      (** We do not have access to predecessor level, round, etc. so any
          preendorsement validation will fail. *)

type expected_endorsement =
  | Expected_endorsement of {expected_features : expected_features}
  | No_expected_branch_for_block_endorsement
      (** The context contains no branch: this happens to the first block
          that uses the Tenderbake consensus algorithm. This block contains
          no endorsements. *)
  | No_expected_branch_for_mempool_endorsement of {expected_level : Raw_level.t}
      (** Same as [No_expected_branch_for_block_endorsement]. This has a
          separate constructor because the error raised is distinct: in
          mempool mode, we simply assume that we have received a
          preendorsement for a future block to which we have not switched
          yet. *)
  | No_predecessor_info_cannot_validate_endorsement
      (** We do not have access to predecessor level, round, etc. so any
          endorsement validation will fail. *)

type all_expected_consensus_features = {
  expected_preendorsement : expected_preendorsement;
  expected_endorsement : expected_endorsement;
  expected_grandparent_endorsement_for_mempool : expected_features option;
      (** This only has a value in Mempool mode and when the [ctxt] has a
          [grand_parent_branch]; it is [None] in all other cases. *)
}

type consensus_info = {
  all_expected_features : all_expected_consensus_features;
  preendorsement_slot_map :
    (Signature.public_key * Signature.public_key_hash * int) Slot.Map.t;
  endorsement_slot_map :
    (Signature.public_key * Signature.public_key_hash * int) Slot.Map.t;
}

let init_consensus_info ctxt all_expected_features =
  {
    all_expected_features;
    preendorsement_slot_map = Consensus.allowed_preendorsements ctxt;
    endorsement_slot_map = Consensus.allowed_endorsements ctxt;
  }

type consensus_state = {
  preendorsements_seen : Slot.Set.t;
  endorsements_seen : Slot.Set.t;
  endorsement_power : int;
  grandparent_endorsements_seen : Signature.Public_key_hash.Set.t;
  locked_round_evidence : (Round.t * int) option;
  dal_slot_availability_seen : Signature.Public_key_hash.Set.t;
}

let empty_consensus_state =
  {
    preendorsements_seen = Slot.Set.empty;
    endorsements_seen = Slot.Set.empty;
    endorsement_power = 0;
    grandparent_endorsements_seen = Signature.Public_key_hash.Set.empty;
    locked_round_evidence = None;
    dal_slot_availability_seen = Signature.Public_key_hash.Set.empty;
  }

(** Summary of previously validated Proposals operations by a given
    proposer in the current block/mempool. *)
type proposer_history = {
  count : int;
      (** Total number of protocols submitted by the proposer in
          previously validated operations. *)
  operations : Operation_hash.t list;
      (** Hashes of the previously validated Proposals operations from
          the proposer. *)
  proposed : Operation_hash.t Protocol_hash.Map.t;
      (** A map indexed by the protocols that have been submitted by the
          proposer in previously validated operations. Each protocol
          points to the operation in which it was proposed. *)
}

type voting_state = {
  proposals_validated : proposer_history Signature.Public_key_hash.Map.t;
      (** Summary of all Proposals operations validated in the current
          block/mempool, indexed by the operation's source aka
          proposer. *)
  dictator_proposals_validated : Operation_hash.t option;
      (** If a testnet dictator Proposals operation has been validated
          in the current block/mempool, then its hash is recorded
          here. Since such an operation can change the voting period
          kind, it is mutually exclusive with any other voting operation
          in a single block (otherwise we would loose the commutativity
          of validated operation application: see
          {!Validate_operation}). *)
  ballots_validated : Operation_hash.t Signature.Public_key_hash.Map.t;
      (** To each delegate that has submitted a ballot in a previously
          validated operation, associates the hash of this operation.  *)
}

let empty_voting_state =
  {
    proposals_validated = Signature.Public_key_hash.Map.empty;
    dictator_proposals_validated = None;
    ballots_validated = Signature.Public_key_hash.Map.empty;
  }

module Double_evidence = Map.Make (struct
  type t = Signature.Public_key_hash.t * Level.t

  let compare (d1, l1) (d2, l2) =
    let res = Signature.Public_key_hash.compare d1 d2 in
    if Compare.Int.equal res 0 then Level.compare l1 l2 else res
end)

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
  blinded_pkhs_seen : Operation_hash.t Blinded_public_key_hash.Map.t;
  double_baking_evidences_seen : Operation_hash.t Double_evidence.t;
  double_consensus_evidences_seen : Operation_hash.t Double_evidence.t;
  seed_nonce_levels_seen : Raw_level.Set.t;
  vdf_solution_seen : bool;
}

let empty_anonymous_state =
  {
    blinded_pkhs_seen = Blinded_public_key_hash.Map.empty;
    double_baking_evidences_seen = Double_evidence.empty;
    double_consensus_evidences_seen = Double_evidence.empty;
    seed_nonce_levels_seen = Raw_level.Set.empty;
    vdf_solution_seen = false;
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
  remaining_block_gas : Gas.Arith.fp;
}

let init_manager_state ctxt =
  {
    managers_seen = Signature.Public_key_hash.Map.empty;
    remaining_block_gas = Gas.Arith.fp (Constants.hard_gas_limit_per_block ctxt);
  }

(** Mode-dependent information needed in final checks. *)
type application_info = {
  fitness : Fitness.t;
  block_producer : public_key_hash;
  payload_producer : public_key_hash;
  predecessor_hash : Block_hash.t;
  block_data_contents : Block_header.contents;
}

(** Circumstances in which operations are validated:

    - [Application] is used for the validation of preexisting block.
   Corresponds to [Application] of {!Main.validation_mode}.

    - [Partial_application] is used to partially validate preexisting
   block. Corresponds to [Partial_application] of
   {!Main.validation_mode}.

    - [Construction] is used for the construction of a new block.
   Corresponds to [Full_construction] of {!Main.validation_mode}.

    - [Mempool] is used by the mempool (either directly or through the
   plugin). Corresponds to [Partial_construction] of
   {!Main.validation_mode}.

    If you add a new mode, please make sure that it has a way to bound
   the size of the map {!recfield:managers_seen}. *)
type mode =
  | Application of application_info
  | Partial_application of application_info
  | Construction of {
      predecessor_round : Round.t;
      predecessor_hash : Block_hash.t;
      round : Round.t;
      block_data_contents : Block_header.contents;
      block_producer : public_key_hash;
      payload_producer : public_key_hash;
    }
  | Mempool

(** {2 Definition and initialization of [info] and
    [state]} *)

type info = {
  ctxt : t;  (** The context at the beginning of the block or mempool. *)
  mode : mode;
  chain_id : Chain_id.t;  (** Needed for signature checks. *)
  current_level : Level.t;
  consensus_info : consensus_info;
  manager_info : manager_info;
}

type state = {
  consensus_state : consensus_state;
  voting_state : voting_state;
  anonymous_state : anonymous_state;
  manager_state : manager_state;
  op_count : int;
  recorded_operations_rev : Operation_hash.t list;
  last_op_validation_pass : int option;
}

type validation_state = {info : info; state : state}

let init_info ctxt mode chain_id all_expected_consensus_characteristics =
  {
    ctxt;
    mode;
    chain_id;
    current_level = Level.current ctxt;
    consensus_info =
      init_consensus_info ctxt all_expected_consensus_characteristics;
    manager_info = init_manager_info ctxt;
  }

let init_info ctxt mode chain_id all_expected_consensus_characteristics =
  init_info ctxt mode chain_id all_expected_consensus_characteristics

let init_state ctxt =
  {
    consensus_state = empty_consensus_state;
    voting_state = empty_voting_state;
    anonymous_state = empty_anonymous_state;
    manager_state = init_manager_state ctxt;
    op_count = 0;
    recorded_operations_rev = [];
    last_op_validation_pass = None;
  }

(** Validation of consensus operations (validation pass [0]):
    preendorsement, endorsement, and dal_slot_availability. *)
module Consensus = struct
  let expected_endorsement_features ~predecessor_level ~predecessor_round branch
      payload_hash =
    {
      level = predecessor_level.Level.level;
      round = Some predecessor_round;
      branch;
      payload_hash;
    }

  let expected_endorsement_for_block ctxt ~predecessor_level ~predecessor_round
      : expected_endorsement =
    match Consensus.endorsement_branch ctxt with
    | None -> No_expected_branch_for_block_endorsement
    | Some (branch, payload_hash) ->
        let expected_features =
          expected_endorsement_features
            ~predecessor_level
            ~predecessor_round
            branch
            payload_hash
        in
        Expected_endorsement {expected_features}

  let expected_features_for_block_validation ctxt fitness payload_hash
      ~predecessor_level ~predecessor_round ~predecessor_hash =
    let expected_preendorsement =
      match Fitness.locked_round fitness with
      | None -> No_locked_round_for_block_validation_preendorsement
      | Some locked_round ->
          let expected_features =
            {
              level = (Level.current ctxt).level;
              round = Some locked_round;
              branch = predecessor_hash;
              payload_hash;
            }
          in
          let block_round = Some (Fitness.round fitness) in
          Expected_preendorsement {expected_features; block_round}
    in
    let expected_endorsement =
      expected_endorsement_for_block ctxt ~predecessor_level ~predecessor_round
    in
    {
      expected_preendorsement;
      expected_endorsement;
      expected_grandparent_endorsement_for_mempool = None;
    }

  let expected_features_for_block_construction ctxt round payload_hash
      ~predecessor_level ~predecessor_round ~predecessor_hash =
    let expected_preendorsement =
      if Block_payload_hash.(payload_hash = zero) then
        (* When the proposal is fresh, a fake [payload_hash] of [zero]
           has been provided. In this case, the block should not
           contain any preendorsements. *)
        Fresh_proposal_for_block_construction_preendorsement
      else
        let expected_features =
          {
            level = (Level.current ctxt).level;
            round = None;
            branch = predecessor_hash;
            payload_hash;
          }
        in
        Expected_preendorsement {expected_features; block_round = Some round}
    in
    let expected_endorsement =
      expected_endorsement_for_block ctxt ~predecessor_level ~predecessor_round
    in
    {
      expected_preendorsement;
      expected_endorsement;
      expected_grandparent_endorsement_for_mempool = None;
    }

  let expected_features_for_mempool ctxt ~predecessor_level ~predecessor_round
      ~grandparent_round =
    let expected_preendorsement, expected_endorsement =
      match Consensus.endorsement_branch ctxt with
      | None ->
          let expected_level = predecessor_level.Level.level in
          ( No_expected_branch_for_mempool_preendorsement {expected_level},
            No_expected_branch_for_mempool_endorsement {expected_level} )
      | Some (branch, payload_hash) ->
          let expected_features =
            expected_endorsement_features
              ~predecessor_level
              ~predecessor_round
              branch
              payload_hash
          in
          ( Expected_preendorsement {expected_features; block_round = None},
            Expected_endorsement {expected_features} )
    in
    let expected_grandparent_endorsement_for_mempool =
      match
        ( Consensus.grand_parent_branch ctxt,
          Raw_level.pred predecessor_level.level )
      with
      | None, _ | _, None -> None
      | Some (branch, payload_hash), Some level ->
          Some {level; round = Some grandparent_round; branch; payload_hash}
    in
    {
      expected_preendorsement;
      expected_endorsement;
      expected_grandparent_endorsement_for_mempool;
    }

  open Validate_errors.Consensus

  let check_frozen_deposits_are_positive ctxt delegate_pkh =
    let open Lwt_result_syntax in
    let* frozen_deposits = Delegate.frozen_deposits ctxt delegate_pkh in
    fail_unless
      Tez.(frozen_deposits.current_amount > zero)
      (Zero_frozen_deposits delegate_pkh)

  let check_level_equal kind expected_features
      (consensus_content : consensus_content) =
    let expected = expected_features.level in
    let provided = consensus_content.level in
    error_unless
      (Raw_level.equal expected provided)
      (if Raw_level.(expected > provided) then
       Consensus_operation_for_old_level {kind; expected; provided}
      else Consensus_operation_for_future_level {kind; expected; provided})

  let check_round_equal vs kind expected_features
      (consensus_content : consensus_content) =
    let check expected =
      let provided = consensus_content.round in
      error_unless
        (Round.equal expected provided)
        (if Round.(expected > provided) then
         Consensus_operation_for_old_round {kind; expected; provided}
        else Consensus_operation_for_future_round {kind; expected; provided})
    in
    match expected_features.round with
    | Some expected -> check expected
    | None -> (
        (* For preendorsements in block construction mode,
           [expected_features.round] has been set to [None] because we
           could not know yet whether there is a locked round. *)
        match vs.consensus_state.locked_round_evidence with
        | None ->
            (* This is the first validated preendorsement in
               construction mode: there is nothing to check. *)
            ok ()
        | Some (expected, _power) ->
            (* Other preendorsements have already been validated: we
               check that the current operation has the same round as
               them. *)
            check expected)

  let check_branch_equal kind expected_features (operation : 'a operation) =
    let expected = expected_features.branch in
    let provided = operation.shell.branch in
    error_unless
      (Block_hash.equal expected provided)
      (Wrong_consensus_operation_branch {kind; expected; provided})

  let check_payload_hash_equal kind expected_features
      (consensus_content : consensus_content) =
    let expected = expected_features.payload_hash in
    let provided = consensus_content.block_payload_hash in
    error_unless
      (Block_payload_hash.equal expected provided)
      (Wrong_payload_hash_for_consensus_operation {kind; expected; provided})

  let check_consensus_features vs kind (expected : expected_features)
      (consensus_content : consensus_content) (operation : 'a operation) =
    let open Result_syntax in
    let* () = check_level_equal kind expected consensus_content in
    let* () = check_round_equal vs kind expected consensus_content in
    let* () = check_branch_equal kind expected operation in
    check_payload_hash_equal kind expected consensus_content

  let ensure_conflict_free_preendorsement vs slot =
    error_unless
      (not (Slot.Set.mem slot vs.consensus_state.preendorsements_seen))
      (Conflicting_consensus_operation {kind = Preendorsement})

  let update_validity_state_preendorsement vs slot round voting_power =
    let locked_round_evidence =
      match vs.consensus_state.locked_round_evidence with
      | None -> Some (round, voting_power)
      | Some (_stored_round, evidences) -> Some (round, evidences + voting_power)
      (* In mempool mode, round and stored_round can be different when
         one of them corresponds to a grandparent preendorsement; this
         doesn't matter because quorum certificates are not used in
         mempool mode. For other cases, {!check_round_equal} ensures
         that all preendorsements have the same round. Indeed, during
         block validation, they are all checked to be the same
         {!recfield:expected_features.round}; and during block
         construction, the round of the first validated preendorsement
         is stored in [locked_round_evidence] then all subsequent
         preendorsements are checked to have the same round in
         {!check_round_equal}. *)
    in
    let preendorsements_seen =
      Slot.Set.add slot vs.consensus_state.preendorsements_seen
    in
    {
      vs with
      consensus_state =
        {vs.consensus_state with locked_round_evidence; preendorsements_seen};
    }

  let get_expected_preendorsements_features consensus_info consensus_content =
    match consensus_info.all_expected_features.expected_preendorsement with
    | Expected_preendorsement {expected_features; block_round} ->
        ok (expected_features, block_round)
    | No_locked_round_for_block_validation_preendorsement
    | Fresh_proposal_for_block_construction_preendorsement ->
        error Unexpected_preendorsement_in_block
    | No_expected_branch_for_mempool_preendorsement {expected_level} ->
        error
          (Consensus_operation_for_future_level
             {
               kind = Preendorsement;
               expected = expected_level;
               provided = consensus_content.Alpha_context.level;
             })
    | No_predecessor_info_cannot_validate_preendorsement ->
        error Consensus_operation_not_allowed

  let check_round_not_too_high ~block_round ~provided =
    match block_round with
    | None -> ok ()
    | Some block_round ->
        error_unless
          Round.(provided < block_round)
          (Preendorsement_round_too_high {block_round; provided})

  let get_delegate_details slot_map kind consensus_content =
    Result.of_option
      (Slot.Map.find consensus_content.slot slot_map)
      ~error:(trace_of_error (Wrong_slot_used_for_consensus_operation {kind}))

  let validate_preendorsement vi vs ~should_check_signature
      (operation : Kind.preendorsement operation) =
    let open Lwt_result_syntax in
    let (Single (Preendorsement consensus_content)) =
      operation.protocol_data.contents
    in
    let kind = Preendorsement in
    let*? () = ensure_conflict_free_preendorsement vs consensus_content.slot in
    let*? expected_features, block_round =
      get_expected_preendorsements_features vi.consensus_info consensus_content
    in
    let*? () =
      check_round_not_too_high ~block_round ~provided:consensus_content.round
    in
    let*? () =
      check_consensus_features
        vs
        kind
        expected_features
        consensus_content
        operation
    in
    let*? delegate_pk, delegate_pkh, voting_power =
      get_delegate_details
        vi.consensus_info.preendorsement_slot_map
        kind
        consensus_content
    in
    let* () = check_frozen_deposits_are_positive vi.ctxt delegate_pkh in
    let*? () =
      if should_check_signature then
        Operation.check_signature delegate_pk vi.chain_id operation
      else ok ()
    in
    return
      (update_validity_state_preendorsement
         vs
         consensus_content.slot
         consensus_content.round
         voting_power)

  let ensure_conflict_free_grandparent_endorsement vs delegate =
    error_unless
      (not
         (Signature.Public_key_hash.Set.mem
            delegate
            vs.consensus_state.grandparent_endorsements_seen))
      (Conflicting_consensus_operation {kind = Grandparent_endorsement})

  let update_validity_state_grandparent_endorsement vs delegate =
    {
      vs with
      consensus_state =
        {
          vs.consensus_state with
          grandparent_endorsements_seen =
            Signature.Public_key_hash.Set.add
              delegate
              vs.consensus_state.grandparent_endorsements_seen;
        };
    }

  (** Validate an endorsement pointing to the grandparent block. This
      function will only be called in [Mempool] mode. *)
  let validate_grandparent_endorsement vi vs ~should_check_signature expected
      (consensus_content : consensus_content) (operation : 'kind operation) =
    let open Lwt_result_syntax in
    let kind = Grandparent_endorsement in
    let level = Level.from_raw vi.ctxt consensus_content.level in
    let* _ctxt, (delegate_pk, delegate_pkh) =
      Stake_distribution.slot_owner vi.ctxt level consensus_content.slot
    in
    let*? () = ensure_conflict_free_grandparent_endorsement vs delegate_pkh in
    let*? () =
      check_consensus_features vs kind expected consensus_content operation
    in
    let*? () =
      if should_check_signature then
        Operation.check_signature delegate_pk vi.chain_id operation
      else ok ()
    in
    return (update_validity_state_grandparent_endorsement vs delegate_pkh)

  let ensure_conflict_free_endorsement vs slot =
    error_unless
      (not (Slot.Set.mem slot vs.consensus_state.endorsements_seen))
      (Conflicting_consensus_operation {kind = Endorsement})

  let update_validity_state_endorsement vs slot voting_power =
    {
      vs with
      consensus_state =
        {
          vs.consensus_state with
          endorsements_seen =
            Slot.Set.add slot vs.consensus_state.endorsements_seen;
          endorsement_power =
            vs.consensus_state.endorsement_power + voting_power;
        };
    }

  let get_expected_endorsements_features consensus_info consensus_content =
    match consensus_info.all_expected_features.expected_endorsement with
    | Expected_endorsement {expected_features} -> ok expected_features
    | No_expected_branch_for_block_endorsement ->
        error Unexpected_endorsement_in_block
    | No_expected_branch_for_mempool_endorsement {expected_level} ->
        error
          (Consensus_operation_for_future_level
             {
               kind = Endorsement;
               expected = expected_level;
               provided = consensus_content.Alpha_context.level;
             })
    | No_predecessor_info_cannot_validate_endorsement ->
        error Consensus_operation_not_allowed

  (** Validate an endorsement pointing to the predecessor, aka a
      "normal" endorsement. Only this kind of endorsement may be found
      during block validation or construction. *)
  let validate_normal_endorsement vi vs ~should_check_signature
      (consensus_content : consensus_content) (operation : 'kind operation) =
    let open Lwt_result_syntax in
    let kind = Endorsement in
    let*? () = ensure_conflict_free_endorsement vs consensus_content.slot in
    let*? expected_features =
      get_expected_endorsements_features vi.consensus_info consensus_content
    in
    let*? () =
      check_consensus_features
        vs
        kind
        expected_features
        consensus_content
        operation
    in
    let*? delegate_pk, delegate_pkh, voting_power =
      get_delegate_details
        vi.consensus_info.endorsement_slot_map
        kind
        consensus_content
    in
    let* () = check_frozen_deposits_are_positive vi.ctxt delegate_pkh in
    let*? () =
      if should_check_signature then
        Operation.check_signature delegate_pk vi.chain_id operation
      else ok ()
    in
    return
      (update_validity_state_endorsement vs consensus_content.slot voting_power)

  let validate_endorsement vi vs ~should_check_signature
      (operation : Kind.endorsement operation) =
    let (Single (Endorsement consensus_content)) =
      operation.protocol_data.contents
    in
    match
      vi.consensus_info.all_expected_features
        .expected_grandparent_endorsement_for_mempool
    with
    | Some expected_grandparent_endorsement
      when Raw_level.(
             consensus_content.level = expected_grandparent_endorsement.level)
      ->
        validate_grandparent_endorsement
          vi
          vs
          ~should_check_signature
          expected_grandparent_endorsement
          consensus_content
          operation
    | _ ->
        validate_normal_endorsement
          vi
          vs
          ~should_check_signature
          consensus_content
          operation

  let ensure_conflict_free_dal_slot_availability vs endorser =
    error_unless
      (not
         (Signature.Public_key_hash.Set.mem
            endorser
            vs.consensus_state.dal_slot_availability_seen))
      (Conflicting_dal_slot_availability {endorser})

  let update_validity_state_dal_slot_availabitiy vs endorser =
    {
      vs with
      consensus_state =
        {
          vs.consensus_state with
          dal_slot_availability_seen =
            Signature.Public_key_hash.Set.add
              endorser
              vs.consensus_state.dal_slot_availability_seen;
        };
    }

  let validate_dal_slot_availability vi vs ~should_check_signature:_
      (operation : Kind.dal_slot_availability operation) =
    (* DAL/FIXME https://gitlab.com/tezos/tezos/-/issues/3115

       This is a temporary operation. Some checks are missing for the
       moment. In particular, the signature is not
       checked. Consequently, it is really important to ensure this
       operation cannot be included into a block when the feature flag
       is not set. This is done in order to avoid modifying the
       endorsement encoding. However, once the DAL is ready, this
       operation should be merged with an endorsement or at least
       refined. *)
    let open Lwt_result_syntax in
    let (Single (Dal_slot_availability (endorser, slot_availability))) =
      operation.protocol_data.contents
    in
    let*? () = ensure_conflict_free_dal_slot_availability vs endorser in
    let*? () =
      (* Note that this function checks the dal feature flag. *)
      Dal_apply.validate_data_availability vi.ctxt slot_availability
    in
    return (update_validity_state_dal_slot_availabitiy vs endorser)
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

  let check_count_conflict ~count_previous_blocks ~count_operation
      proposer_history =
    let max_allowed = Constants.max_proposals_per_delegate in
    let count_before_op = count_previous_blocks + proposer_history.count in
    (* [count_before_op] should never have been increased above
       [max_proposals_per_delegate]. *)
    assert (Compare.Int.(count_before_op <= max_allowed)) ;
    error_unless
      Compare.Int.(count_before_op + count_operation <= max_allowed)
      (Conflict_too_many_proposals
         {
           max_allowed;
           count_previous_blocks;
           count_current_block = proposer_history.count;
           count_operation;
           conflicting_operations = proposer_history.operations;
         })

  (** Check that a regular (ie. non-dictator) Proposals operation is
      compatible with previously validated voting operations in the
      current block/mempool, and update the [state] with this
      operation.

      @return [Error Conflict_too_many_proposals] if the total number
      of proposals by [proposer] in previously applied operations in
      [ctxt], in previously validated operations in the current
      block/mempool, and in the operation to validate, exceeds
      {!Constants.max_proposals_per_delegate}.

      @return [Error Conflict_already_proposed] if one of the
      operation's [proposals] has already been submitted by [proposer]
      in the current block/mempool.

      @return [Error Conflicting_dictator_proposals] if the current
      block/mempool already contains a testnet dictator Proposals
      operation (see {!recfield:dictator_proposals_validated}).

      Note that this function is designed to be called in addition to
      {!check_proposal_list_sanity} and {!check_count} further below,
      not instead of them: that's why nothing is done when the
      [proposer] is not in {!recfield:proposals_validated}. More
      precisely, this function should be called {e after} the
      aforementioned functions, whose potential errors
      e.g. [Proposals_contain_duplicate] or [Too_many_proposals] should
      take precedence because they are independent from the validation
      [state]. *)
  let check_proposals_conflicts_and_update_state state oph proposer proposals
      ~count_in_ctxt ~proposals_length =
    let open Tzresult_syntax in
    let* new_proposer_history =
      match
        Signature.Public_key_hash.Map.find proposer state.proposals_validated
      with
      | None ->
          let proposed =
            List.fold_left
              (fun acc proposal -> Protocol_hash.Map.add proposal oph acc)
              Protocol_hash.Map.empty
              proposals
          in
          return {count = proposals_length; operations = [oph]; proposed}
      | Some proposer_history ->
          let* () =
            check_count_conflict
              ~count_previous_blocks:count_in_ctxt
              ~count_operation:proposals_length
              proposer_history
          in
          let add_proposal proposed_map proposal =
            match Protocol_hash.Map.find proposal proposer_history.proposed with
            | Some conflicting_operation ->
                error
                  (Conflict_already_proposed {proposal; conflicting_operation})
            | None -> ok (Protocol_hash.Map.add proposal oph proposed_map)
          in
          let* proposed =
            List.fold_left_e add_proposal proposer_history.proposed proposals
          in
          return
            {
              count = proposer_history.count + proposals_length;
              operations = oph :: proposer_history.operations;
              proposed;
            }
    in
    let* () =
      match state.dictator_proposals_validated with
      | None -> ok ()
      | Some dictator_oph -> error (Conflicting_dictator_proposals dictator_oph)
    in
    let proposals_validated =
      Signature.Public_key_hash.Map.add
        proposer
        new_proposer_history
        state.proposals_validated
    in
    return {state with proposals_validated}

  (** Check that a Proposals operation from a testnet dictator is
      compatible with previously validated voting operations in the
      current block/mempool (ie. that no other voting operation has
      been validated), and update the [state] with this operation.

      @return [Error Testnet_dictator_conflicting_operation] if the
      current block or mempool already contains any validated voting
      operation. *)
  let check_dictator_proposals_conflicts_and_update_state state oph =
    let open Tzresult_syntax in
    let* () =
      error_unless
        (Signature.Public_key_hash.Map.is_empty state.proposals_validated
        && Option.is_none state.dictator_proposals_validated
        && Signature.Public_key_hash.Map.is_empty state.ballots_validated)
        Testnet_dictator_conflicting_operation
    in
    return {state with dictator_proposals_validated = Some oph}

  (** Check that a Ballot operation is compatible with previously
      validated voting operations in the current block/mempool.

      @return [Error Conflicting_ballot] if the [delegate] has already
      submitted a ballot in the current block/mempool.

      @return [Error Conflicting_dictator_proposals] if the current
      block/mempool already contains a testnet dictator Proposals
      operation (see {!recfield:dictator_proposals_validated}). *)
  let check_ballot_conflicts state voter =
    let open Tzresult_syntax in
    let* () =
      match
        Signature.Public_key_hash.Map.find voter state.ballots_validated
      with
      | None -> ok ()
      | Some conflicting_operation ->
          error (Conflicting_ballot {conflicting_operation})
    in
    match state.dictator_proposals_validated with
    | None -> ok ()
    | Some dictator_oph -> error (Conflicting_dictator_proposals dictator_oph)

  (** Update the [state] when a Ballot operation is validated. *)
  let update_state_on_ballot state oph voter =
    let ballots_validated =
      Signature.Public_key_hash.Map.add voter oph state.ballots_validated
    in
    {state with ballots_validated}

  (** Check that [record_proposals] below will not fail.

      This function is designed to be exclusively called by
      [validate_proposals] further down this file.

      @return [Error Multiple_proposals] if [proposals] has more than
      one element. *)
  let check_testnet_dictator_proposals chain_id proposals =
    (* This assertion should be ensured by the fact that
       {!is_testnet_dictator} cannot be [true] on mainnet, but we
       double check it because it is critical. *)
    assert (Chain_id.(chain_id <> Constants.mainnet_id)) ;
    match proposals with
    | [] | [_] ->
        (* In [record_proposals] below, the call to
           {!Vote.init_current_proposal} (in the singleton list case)
           cannot fail because {!Vote.clear_current_proposal} is called
           right before.

           The calls to
           {!Voting_period.Testnet_dictator.overwrite_current_kind} may
           usually fail when the voting period is not
           initialized. However, this cannot happen because the current
           function is only called in [validate_proposals] after a
           successful call to {!Voting_period.get_current}. *)
        ok ()
    | _ :: _ :: _ -> error Testnet_dictator_multiple_proposals

  let check_period_index ~expected period_index =
    error_unless
      Compare.Int32.(expected = period_index)
      (Wrong_voting_period_index {expected; provided = period_index})

  let check_proposals_source_is_registered ctxt source =
    let open Lwt_tzresult_syntax in
    let*! is_registered = Delegate.registered ctxt source in
    fail_unless is_registered (Proposals_from_unregistered_delegate source)

  (** Check that the list of proposals is not empty and does not contain
    duplicates. *)
  let check_proposal_list_sanity proposals =
    let open Tzresult_syntax in
    let* () =
      match proposals with [] -> error Empty_proposals | _ :: _ -> ok ()
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
    | Proposal -> ok ()
    | (Exploration | Cooldown | Promotion | Adoption) as current ->
        error (Wrong_voting_period_kind {current; expected = [Proposal]})

  let check_in_listings ctxt source =
    let open Lwt_tzresult_syntax in
    let*! in_listings = Vote.in_listings ctxt source in
    fail_unless in_listings Source_not_in_vote_listings

  let check_count ~count_in_ctxt ~proposals_length =
    (* The proposal count of the proposer in the context should never
       have been increased above [max_proposals_per_delegate]. *)
    assert (Compare.Int.(count_in_ctxt <= Constants.max_proposals_per_delegate)) ;
    error_unless
      Compare.Int.(
        count_in_ctxt + proposals_length <= Constants.max_proposals_per_delegate)
      Too_many_proposals

  let check_already_proposed ctxt proposer proposals =
    let open Lwt_tzresult_syntax in
    List.iter_es
      (fun proposal ->
        let*! already_proposed = Vote.has_proposed ctxt proposer proposal in
        fail_when already_proposed (Already_proposed {proposal}))
      proposals

  let check_period_kind_for_ballot current_period =
    match current_period.Voting_period.kind with
    | Exploration | Promotion -> ok ()
    | (Cooldown | Proposal | Adoption) as current ->
        error
          (Wrong_voting_period_kind
             {current; expected = [Exploration; Promotion]})

  let check_current_proposal ctxt op_proposal =
    let open Lwt_tzresult_syntax in
    let* current_proposal = Vote.get_current_proposal ctxt in
    fail_unless
      (Protocol_hash.equal op_proposal current_proposal)
      (Ballot_for_wrong_proposal
         {current = current_proposal; submitted = op_proposal})

  let check_source_has_not_already_voted ctxt source =
    let open Lwt_tzresult_syntax in
    let*! has_ballot = Vote.has_recorded_ballot ctxt source in
    fail_when has_ballot Already_submitted_a_ballot

  let check_ballot_source_is_registered ctxt source =
    let open Lwt_tzresult_syntax in
    let*! is_registered = Delegate.registered ctxt source in
    fail_unless is_registered (Ballot_from_unregistered_delegate source)

  (** Check that a Proposals operation can be safely applied.

    @return [Error Wrong_voting_period_index] if the operation's
    period and the [context]'s current period do not have the same
    index.

    @return [Error Proposals_from_unregistered_delegate] if the
    source is not a registered delegate.

    @return [Error Empty_proposals] if the list of proposals is empty.

    @return [Error Proposals_contain_duplicate] if the list of
    proposals contains a duplicate element.

    @return [Error Wrong_voting_period_kind] if the voting period is
    not of the Proposal kind.

    @return [Error Source_not_in_vote_listings] if the source is not
    in the vote listings.

    @return [Error Too_many_proposals] if the operation would make the
    source's total number of proposals exceed
    {!Constants.recorded_proposal_count_for_delegate}.

    @return [Error Already_proposed] if one of the proposals has
    already been proposed by the source.

    @return [Error Conflict_too_many_proposals] if the total count of
    proposals submitted by the source in previous blocks, in previously
    validated operations of the current block/mempool, and in the
    operation to validate, exceeds
    {!Constants.max_proposals_per_delegate}.

    @return [Error Conflict_already_proposed] if one of the
    operation's proposals has already been submitted by the source in
    the current block/mempool.

    @return [Error Conflicting_dictator_proposals] if a testnet
    dictator Proposals operation has already been validated in the
    current block/mempool.

    @return [Error Testnet_dictator_multiple_proposals] if the source
    is a testnet dictator and the operation contains more than one
    proposal.

    @return [Error Testnet_dictator_conflicting_operation] if the
    source is a testnet dictator and the current block or mempool
    already contains any validated voting operation.

    @return [Error Operation.Missing_signature] or [Error
    Operation.Invalid_signature] if the operation is unsigned or
    incorrectly signed. *)
  let validate_proposals vi vs ~should_check_signature oph
      (operation : Kind.proposals operation) =
    let open Lwt_tzresult_syntax in
    let (Single (Proposals {source; period; proposals})) =
      operation.protocol_data.contents
    in
    let* current_period = Voting_period.get_current vi.ctxt in
    let*? () = check_period_index ~expected:current_period.index period in
    let* voting_state =
      if Amendment.is_testnet_dictator vi.ctxt vi.chain_id source then
        let*? () = check_testnet_dictator_proposals vi.chain_id proposals in
        Lwt.return
          (check_dictator_proposals_conflicts_and_update_state
             vs.voting_state
             oph)
      else
        let* () = check_proposals_source_is_registered vi.ctxt source in
        let*? () = check_proposal_list_sanity proposals in
        let*? () = check_period_kind_for_proposals current_period in
        let* () = check_in_listings vi.ctxt source in
        let* count_in_ctxt = Vote.get_delegate_proposal_count vi.ctxt source in
        let proposals_length = List.length proposals in
        let*? () = check_count ~count_in_ctxt ~proposals_length in
        let* () = check_already_proposed vi.ctxt source proposals in
        Lwt.return
          (check_proposals_conflicts_and_update_state
             vs.voting_state
             oph
             source
             proposals
             ~count_in_ctxt
             ~proposals_length)
    in
    (* The signature check is done last because it is more costly than
       most checks. *)
    let* () =
      when_ should_check_signature (fun () ->
          (* Retrieving the public key cannot fail. Indeed, we have
             already checked that the delegate is in the vote listings
             (or is a testnet dictator), which implies that it is a
             manager with a revealed key. *)
          let* public_key = Contract.get_manager_key vi.ctxt source in
          Lwt.return
            (Operation.check_signature public_key vi.chain_id operation))
    in
    return {vs with voting_state}

  (** Check that a Ballot operation can be safely applied.

    @return [Error Ballot_from_unregistered_delegate] if the
    source is not a registered delegate.

    @return [Error Conflicting_ballot] if the source has already
    submitted a ballot in the current block/mempool.

    @return [Error Conflicting_dictator_proposals] if the current
    block/mempool already contains a validated testnet dictator
    Proposals operation.

    @return [Error Wrong_voting_period_index] if the operation's
    period and the [context]'s current period do not have the same
    index.

    @return [Error Wrong_voting_period_kind] if the voting period is
    not of the Exploration or Promotion kind.

    @return [Error Ballot_for_wrong_proposal] if the operation's
    proposal is different from the [context]'s current proposal.

    @return [Error Already_submitted_a_ballot] if the source has
    already voted.

    @return [Error Source_not_in_vote_listings] if the source is not
    in the vote listings.

    @return [Error Operation.Missing_signature] or [Error
    Operation.Invalid_signature] if the operation is unsigned or
    incorrectly signed. *)
  let validate_ballot vi vs ~should_check_signature oph
      (operation : Kind.ballot operation) =
    let open Lwt_tzresult_syntax in
    let (Single (Ballot {source; period; proposal; ballot = _})) =
      operation.protocol_data.contents
    in
    let* () = check_ballot_source_is_registered vi.ctxt source in
    let*? () = check_ballot_conflicts vs.voting_state source in
    let* current_period = Voting_period.get_current vi.ctxt in
    let*? () = check_period_index ~expected:current_period.index period in
    let*? () = check_period_kind_for_ballot current_period in
    let* () = check_current_proposal vi.ctxt proposal in
    let* () = check_source_has_not_already_voted vi.ctxt source in
    let* () = check_in_listings vi.ctxt source in
    (* The signature check is done last because it is more costly than
       most checks. *)
    let* () =
      when_ should_check_signature (fun () ->
          (* Retrieving the public key cannot fail. Indeed, we have
             already checked that the delegate is in the vote listings,
             which implies that it is a manager with a revealed key. *)
          let* public_key = Contract.get_manager_key vi.ctxt source in
          Lwt.return
            (Operation.check_signature public_key vi.chain_id operation))
    in
    let voting_state = update_state_on_ballot vs.voting_state oph source in
    return {vs with voting_state}
end

module Anonymous = struct
  open Validate_errors.Anonymous

  let validate_activate_account vi vs oph
      (Activate_account {id = edpkh; activation_code}) =
    let open Lwt_result_syntax in
    let blinded_pkh =
      Blinded_public_key_hash.of_ed25519_pkh activation_code edpkh
    in
    let*? () =
      match
        Blinded_public_key_hash.Map.find
          blinded_pkh
          vs.anonymous_state.blinded_pkhs_seen
      with
      | None -> ok ()
      | Some oph' -> error (Conflicting_activation (edpkh, oph'))
    in
    let*! exists = Commitment.exists vi.ctxt blinded_pkh in
    let*? () = error_unless exists (Invalid_activation {pkh = edpkh}) in
    let blinded_pkhs_seen =
      Blinded_public_key_hash.Map.add
        blinded_pkh
        oph
        vs.anonymous_state.blinded_pkhs_seen
    in
    return
      {vs with anonymous_state = {vs.anonymous_state with blinded_pkhs_seen}}

  let check_denunciation_age vi kind given_level =
    let open Result_syntax in
    let current_cycle = vi.current_level.cycle in
    let given_cycle = (Level.from_raw vi.ctxt given_level).cycle in
    let max_slashing_period = Constants.max_slashing_period vi.ctxt in
    let last_slashable_cycle = Cycle.add given_cycle max_slashing_period in
    let* () =
      error_unless
        Cycle.(given_cycle <= current_cycle)
        (Too_early_denunciation
           {kind; level = given_level; current = vi.current_level.level})
    in
    error_unless
      Cycle.(last_slashable_cycle > current_cycle)
      (Outdated_denunciation
         {kind; level = given_level; last_cycle = last_slashable_cycle})

  let validate_double_consensus (type kind)
      ~consensus_operation:denunciation_kind vi vs oph
      (op1 : kind Kind.consensus Operation.t)
      (op2 : kind Kind.consensus Operation.t) =
    let open Lwt_result_syntax in
    match (op1.protocol_data.contents, op2.protocol_data.contents) with
    | Single (Preendorsement e1), Single (Preendorsement e2)
    | Single (Endorsement e1), Single (Endorsement e2) ->
        let op1_hash = Operation.hash op1 in
        let op2_hash = Operation.hash op2 in
        let*? () =
          error_unless
            (Raw_level.(e1.level = e2.level)
            && Round.(e1.round = e2.round)
            && (not
                  (Block_payload_hash.equal
                     e1.block_payload_hash
                     e2.block_payload_hash))
            && (* we require an order on hashes to avoid the existence of
                  equivalent evidences *)
            Operation_hash.(op1_hash < op2_hash))
            (Invalid_denunciation denunciation_kind)
        in
        (* Disambiguate: levels are equal *)
        let level = Level.from_raw vi.ctxt e1.level in
        let*? () = check_denunciation_age vi denunciation_kind level.level in
        let* ctxt, (delegate1_pk, delegate1) =
          Stake_distribution.slot_owner vi.ctxt level e1.slot
        in
        let* ctxt, (_delegate2_pk, delegate2) =
          Stake_distribution.slot_owner ctxt level e2.slot
        in
        let*? () =
          error_unless
            (Signature.Public_key_hash.equal delegate1 delegate2)
            (Inconsistent_denunciation
               {kind = denunciation_kind; delegate1; delegate2})
        in
        let delegate_pk, delegate = (delegate1_pk, delegate1) in
        let*? () =
          match
            Double_evidence.find
              (delegate, level)
              vs.anonymous_state.double_consensus_evidences_seen
          with
          | None -> ok ()
          | Some oph' ->
              error
                (Conflicting_denunciation
                   {kind = denunciation_kind; delegate; level; hash = oph'})
        in
        let* already_slashed =
          Delegate.already_slashed_for_double_endorsing ctxt delegate level
        in
        let*? () =
          error_unless
            (not already_slashed)
            (Already_denounced {kind = denunciation_kind; delegate; level})
        in
        let*? () = Operation.check_signature delegate_pk vi.chain_id op1 in
        let*? () = Operation.check_signature delegate_pk vi.chain_id op2 in
        let double_consensus_evidences_seen =
          Double_evidence.add
            (delegate, level)
            oph
            vs.anonymous_state.double_consensus_evidences_seen
        in
        return
          {
            vs with
            anonymous_state =
              {vs.anonymous_state with double_consensus_evidences_seen};
          }

  let validate_double_preendorsement_evidence vi vs oph
      (Double_preendorsement_evidence {op1; op2}) =
    validate_double_consensus
      ~consensus_operation:Preendorsement
      vi
      vs
      oph
      op1
      op2

  let validate_double_endorsement_evidence vi vs oph
      (Double_endorsement_evidence {op1; op2}) =
    validate_double_consensus ~consensus_operation:Endorsement vi vs oph op1 op2

  let validate_double_baking_evidence vi vs oph
      (Double_baking_evidence {bh1; bh2}) =
    let open Lwt_result_syntax in
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
        && (* we require an order on hashes to avoid the existence of
              equivalent evidences *)
        Block_hash.(hash1 < hash2))
        (Invalid_double_baking_evidence
           {hash1; level1; round1; hash2; level2; round2})
    in
    let*? () = check_denunciation_age vi Block level1 in
    let level = Level.from_raw vi.ctxt level1 in
    let committee_size = Constants.consensus_committee_size vi.ctxt in
    let*? slot1 = Round.to_slot round1 ~committee_size in
    let* ctxt, (delegate1_pk, delegate1) =
      Stake_distribution.slot_owner vi.ctxt level slot1
    in
    let*? slot2 = Round.to_slot round2 ~committee_size in
    let* ctxt, (_delegate2_pk, delegate2) =
      Stake_distribution.slot_owner ctxt level slot2
    in
    let*? () =
      error_unless
        Signature.Public_key_hash.(delegate1 = delegate2)
        (Inconsistent_denunciation {kind = Block; delegate1; delegate2})
    in
    let delegate_pk, delegate = (delegate1_pk, delegate1) in
    let*? () =
      match
        Double_evidence.find
          (delegate, level)
          vs.anonymous_state.double_baking_evidences_seen
      with
      | None -> ok ()
      | Some oph' ->
          error
            (Conflicting_denunciation
               {kind = Block; delegate; level; hash = oph'})
    in
    let* already_slashed =
      Delegate.already_slashed_for_double_baking ctxt delegate level
    in
    let*? () =
      error_unless
        (not already_slashed)
        (Already_denounced {kind = Block; delegate; level})
    in
    let*? () = Block_header.check_signature bh1 vi.chain_id delegate_pk in
    let*? () = Block_header.check_signature bh2 vi.chain_id delegate_pk in
    let double_baking_evidences_seen =
      Double_evidence.add
        (delegate, level)
        oph
        vs.anonymous_state.double_baking_evidences_seen
    in
    return
      {
        vs with
        anonymous_state = {vs.anonymous_state with double_baking_evidences_seen};
      }

  let validate_seed_nonce_revelation vi vs
      (Seed_nonce_revelation {level = commitment_raw_level; nonce}) =
    let open Lwt_result_syntax in
    let commitment_level = Level.from_raw vi.ctxt commitment_raw_level in
    let*? () =
      error_unless
        (not
           (Raw_level.Set.mem
              commitment_raw_level
              vs.anonymous_state.seed_nonce_levels_seen))
        Conflicting_nonce_revelation
    in
    let* () = Nonce.check_unrevealed vi.ctxt commitment_level nonce in
    let seed_nonce_levels_seen =
      Raw_level.Set.add
        commitment_raw_level
        vs.anonymous_state.seed_nonce_levels_seen
    in
    let new_vs =
      {
        vs with
        anonymous_state = {vs.anonymous_state with seed_nonce_levels_seen};
      }
    in
    return new_vs

  let validate_vdf_revelation vi vs (Vdf_revelation {solution}) =
    let open Lwt_result_syntax in
    let*? () =
      error_unless
        (not vs.anonymous_state.vdf_solution_seen)
        Seed_storage.Already_accepted
    in
    let* () = Seed.check_vdf vi.ctxt solution in
    return
      {
        vs with
        anonymous_state = {vs.anonymous_state with vdf_solution_seen = true};
      }
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
    remaining_block_gas : Gas.Arith.fp;
        (** In Block_validation mode, this is what remains of the block gas
            quota after subtracting the gas_limit of all previously
            validated operations in the block. In Mempool mode, only
            previous gas for previous operations in the same batch has been
            subtracted from the block quota. Cf
            {!maybe_update_remaining_block_gas}:
            [vs.manager_state.remaining_block_gas] is updated only in
            Block_validation mode. *)
  }

  (** Check a few simple properties of the batch, and return the
      initial {!batch_state} and the contract public key.

      Invariants checked:

      - All operations in a batch have the same source.

      - The source's contract is allocated.

      - The counters in a batch are successive, and the first of them
        is the source's next expected counter.

      - A batch contains at most one Reveal operation that must occur
        in first position.

      - The source's public key has been revealed (either before the
        considered batch, or during its first operation).

      Note that currently, the [op] batch contains only one signature,
      so all operations in the batch are required to originate from the
      same manager. This may change in the future, in order to allow
      several managers to group-sign a sequence of operations. *)
  let check_sanity_and_find_public_key vi vs
      (contents_list : _ Kind.manager contents_list) =
    let open Result_syntax in
    let check_source_and_counter ~expected_source ~source ~previous_counter
        ~counter =
      let* () =
        error_unless
          (Signature.Public_key_hash.equal expected_source source)
          Inconsistent_sources
      in
      error_unless
        Compare.Z.(Z.succ previous_counter = counter)
        Inconsistent_counters
    in
    let rec check_batch_tail_sanity :
        type kind.
        public_key_hash ->
        counter ->
        kind Kind.manager contents_list ->
        unit tzresult =
     fun expected_source previous_counter -> function
      | Single (Manager_operation {operation = Reveal _key; _}) ->
          error Incorrect_reveal_position
      | Cons (Manager_operation {operation = Reveal _key; _}, _res) ->
          error Incorrect_reveal_position
      | Single (Manager_operation {source; counter; _}) ->
          check_source_and_counter
            ~expected_source
            ~source
            ~previous_counter
            ~counter
      | Cons (Manager_operation {source; counter; _}, rest) ->
          let open Result_syntax in
          let* () =
            check_source_and_counter
              ~expected_source
              ~source
              ~previous_counter
              ~counter
          in
          check_batch_tail_sanity source counter rest
    in
    let check_batch :
        type kind.
        kind Kind.manager contents_list ->
        (public_key_hash * public_key option * counter) tzresult =
     fun contents_list ->
      match contents_list with
      | Single (Manager_operation {source; operation = Reveal key; counter; _})
        ->
          ok (source, Some key, counter)
      | Single (Manager_operation {source; counter; _}) ->
          ok (source, None, counter)
      | Cons
          (Manager_operation {source; operation = Reveal key; counter; _}, rest)
        ->
          check_batch_tail_sanity source counter rest >>? fun () ->
          ok (source, Some key, counter)
      | Cons (Manager_operation {source; counter; _}, rest) ->
          check_batch_tail_sanity source counter rest >>? fun () ->
          ok (source, None, counter)
    in
    let open Lwt_result_syntax in
    let*? source, revealed_key, first_counter = check_batch contents_list in
    let* balance = Contract.check_allocated_and_get_balance vi.ctxt source in
    let* () = Contract.check_counter_increment vi.ctxt source first_counter in
    let* pk =
      (* Note that it is important to always retrieve the public
         key. This includes the case where the key ends up not being
         used because the signature check is skipped in
         {!validate_manager_operation} called with
         [~should_check_signature:false]. Indeed, the mempool may use
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
        remaining_block_gas = vs.manager_state.remaining_block_gas;
      }
    in
    return (initial_batch_state, pk)

  let check_gas_limit_and_consume_from_block_gas vi ~remaining_block_gas
      ~gas_limit =
    (match vi.mode with
    | Application _ | Partial_application _ | Construction _ -> fun res -> res
    | Mempool ->
        (* [Gas.check_limit_and_consume_from_block_gas] will only
           raise a "temporary" error, however when
           {!validate_operation} is called on a batch in isolation
           (like e.g. in the mempool) it must "refuse" operations
           whose total gas limit (the sum of the [gas_limit]s of each
           operation) is already above the block limit. We add the
           "permanent" error [Gas.Gas_limit_too_high] on top of the
           trace to this effect. *)
        record_trace Gas.Gas_limit_too_high)
      (Gas.check_limit_and_consume_from_block_gas
         ~hard_gas_limit_per_operation:
           vi.manager_info.hard_gas_limit_per_operation
         ~remaining_block_gas
         ~gas_limit)

  let check_storage_limit vi storage_limit =
    error_unless
      Compare.Z.(
        storage_limit <= vi.manager_info.hard_storage_limit_per_operation
        && storage_limit >= Z.zero)
      Fees.Storage_limit_too_high

  let assert_tx_rollup_feature_enabled vi =
    let open Result_syntax in
    let* sunset =
      Raw_level.of_int32 (Constants.tx_rollup_sunset_level vi.ctxt)
    in
    error_unless
      (Constants.tx_rollup_enable vi.ctxt
      && Raw_level.(vi.current_level.level < sunset))
      Tx_rollup_feature_disabled

  let assert_sc_rollup_feature_enabled vi =
    error_unless (Constants.sc_rollup_enable vi.ctxt) Sc_rollup_feature_disabled

  let assert_dal_feature_enabled vi =
    error_unless (Constants.dal_enable vi.ctxt) Dal_errors.Dal_feature_disabled

  let assert_not_zero_messages messages =
    match messages with
    | [] -> error Sc_rollup_errors.Sc_rollup_add_zero_messages
    | _ -> ok ()

  let assert_zk_rollup_feature_enabled vi =
    error_unless (Constants.zk_rollup_enable vi.ctxt) Zk_rollup_feature_disabled

  let consume_decoding_gas ctxt lexpr =
    record_trace Gas_quota_exceeded_init_deserialize
    @@ (* Fail early if the operation does not have enough gas to
          cover the deserialization cost. We always consider the full
          deserialization cost, independently from the internal state
          of the lazy_expr. Otherwise we might risk getting different
          results if the operation has already been deserialized
          before (e.g. when retrieved in JSON format). Note that the
          lazy_expr is not actually decoded here; its deserialization
          cost is estimated from the size of its bytes. *)
    Script.consume_decoding_gas ctxt lexpr

  let validate_tx_rollup_submit_batch vi remaining_gas content =
    let open Result_syntax in
    let* () = assert_tx_rollup_feature_enabled vi in
    let size_limit = Constants.tx_rollup_hard_size_limit_per_message vi.ctxt in
    let _message, message_size = Tx_rollup_message.make_batch content in
    let* cost = Tx_rollup_gas.hash_cost message_size in
    let* remaining_gas = Gas.consume_from remaining_gas cost in
    let* () =
      error_unless
        Compare.Int.(message_size <= size_limit)
        Tx_rollup_errors.Message_size_exceeds_limit
    in
    return remaining_gas

  let validate_tx_rollup_dispatch_tickets vi remaining_gas operation =
    let open Result_syntax in
    let* () = assert_tx_rollup_feature_enabled vi in
    let (Tx_rollup_dispatch_tickets {tickets_info; message_result_path; _}) =
      operation
    in
    let Constants.Parametric.
          {max_messages_per_inbox; max_withdrawals_per_batch; _} =
      Constants.tx_rollup vi.ctxt
    in
    let* () =
      Tx_rollup_errors.check_path_depth
        `Commitment
        (Tx_rollup_commitment.Merkle.path_depth message_result_path)
        ~count_limit:max_messages_per_inbox
    in
    let* () =
      error_when
        Compare.List_length_with.(tickets_info = 0)
        Tx_rollup_errors.No_withdrawals_to_dispatch
    in
    let* () =
      error_when
        Compare.List_length_with.(tickets_info > max_withdrawals_per_batch)
        Tx_rollup_errors.Too_many_withdrawals
    in
    record_trace
      Gas_quota_exceeded_init_deserialize
      (List.fold_left_e
         (fun remaining_gas Tx_rollup_reveal.{contents; ty; _} ->
           let* remaining_gas =
             Script.consume_decoding_gas remaining_gas contents
           in
           Script.consume_decoding_gas remaining_gas ty)
         remaining_gas
         tickets_info)

  let validate_tx_rollup_rejection vi operation =
    let open Result_syntax in
    let* () = assert_tx_rollup_feature_enabled vi in
    let (Tx_rollup_rejection
          {message_path; message_result_path; previous_message_result_path; _})
        =
      operation
    in
    let Constants.Parametric.{max_messages_per_inbox; _} =
      Constants.tx_rollup vi.ctxt
    in
    let* () =
      Tx_rollup_errors.check_path_depth
        `Inbox
        (Tx_rollup_inbox.Merkle.path_depth message_path)
        ~count_limit:max_messages_per_inbox
    in
    let* () =
      Tx_rollup_errors.check_path_depth
        `Commitment
        (Tx_rollup_commitment.Merkle.path_depth message_result_path)
        ~count_limit:max_messages_per_inbox
    in
    Tx_rollup_errors.check_path_depth
      `Commitment
      (Tx_rollup_commitment.Merkle.path_depth previous_message_result_path)
      ~count_limit:max_messages_per_inbox

  let validate_contents (type kind) vi batch_state
      (contents : kind Kind.manager contents) =
    let open Lwt_result_syntax in
    let (Manager_operation
          {source; fee; counter = _; operation; gas_limit; storage_limit}) =
      contents
    in
    let*? remaining_block_gas =
      check_gas_limit_and_consume_from_block_gas
        vi
        ~remaining_block_gas:batch_state.remaining_block_gas
        ~gas_limit
    in
    let*? remaining_gas =
      record_trace
        Insufficient_gas_for_manager
        (Gas.consume_from
           (Gas.Arith.fp gas_limit)
           Michelson_v1_gas.Cost_of.manager_operation)
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
    let*? (_remaining_gas : Gas.Arith.fp) =
      let open Result_syntax in
      match operation with
      | Reveal pk ->
          let* () = Contract.check_public_key pk source in
          return remaining_gas
      | Transaction {parameters; _} ->
          consume_decoding_gas remaining_gas parameters
      | Origination {script; _} ->
          let* remaining_gas = consume_decoding_gas remaining_gas script.code in
          consume_decoding_gas remaining_gas script.storage
      | Register_global_constant {value} ->
          consume_decoding_gas remaining_gas value
      | Delegation _ | Set_deposits_limit _ | Increase_paid_storage _ ->
          return remaining_gas
      | Tx_rollup_origination ->
          let* () = assert_tx_rollup_feature_enabled vi in
          return remaining_gas
      | Tx_rollup_submit_batch {content; _} ->
          validate_tx_rollup_submit_batch vi remaining_gas content
      | Tx_rollup_commit _ | Tx_rollup_return_bond _
      | Tx_rollup_finalize_commitment _ | Tx_rollup_remove_commitment _ ->
          let* () = assert_tx_rollup_feature_enabled vi in
          return remaining_gas
      | Transfer_ticket {contents; ty; _} ->
          let* () = assert_tx_rollup_feature_enabled vi in
          let* remaining_gas = consume_decoding_gas remaining_gas contents in
          consume_decoding_gas remaining_gas ty
      | Tx_rollup_dispatch_tickets _ ->
          validate_tx_rollup_dispatch_tickets vi remaining_gas operation
      | Tx_rollup_rejection _ ->
          let* () = validate_tx_rollup_rejection vi operation in
          return remaining_gas
      | Sc_rollup_originate _ | Sc_rollup_cement _ | Sc_rollup_publish _
      | Sc_rollup_refute _ | Sc_rollup_timeout _
      | Sc_rollup_execute_outbox_message _ ->
          let* () = assert_sc_rollup_feature_enabled vi in
          return remaining_gas
      | Sc_rollup_add_messages {messages; _} ->
          let* () = assert_sc_rollup_feature_enabled vi in
          let* () = assert_not_zero_messages messages in
          return remaining_gas
      | Sc_rollup_recover_bond _ ->
          (* TODO: https://gitlab.com/tezos/tezos/-/issues/3063
             Should we successfully precheck Sc_rollup_recover_bond and any
             (simple) Sc rollup operation, or should we add some some checks to make
             the operations Branch_delayed if they cannot be successfully
             prechecked? *)
          let* () = assert_sc_rollup_feature_enabled vi in
          return remaining_gas
      | Sc_rollup_dal_slot_subscribe _ ->
          let* () = assert_sc_rollup_feature_enabled vi in
          let* () = assert_dal_feature_enabled vi in
          return remaining_gas
      | Dal_publish_slot_header {slot} ->
          let* () = Dal_apply.validate_publish_slot_header vi.ctxt slot in
          return remaining_gas
      | Zk_rollup_origination _ ->
          let* () = assert_zk_rollup_feature_enabled vi in
          return remaining_gas
    in
    let* balance, is_allocated =
      Contract.simulate_spending
        vi.ctxt
        ~balance:batch_state.balance
        ~amount:fee
        source
    in
    return {remaining_block_gas; balance; is_allocated}

  (** This would be [fold_left_es (validate_contents vi) batch_state
      contents_list] if [contents_list] were an ordinary [list]. *)
  let rec validate_contents_list :
      type kind.
      info ->
      batch_state ->
      kind Kind.manager contents_list ->
      batch_state tzresult Lwt.t =
   fun vi batch_state contents_list ->
    let open Lwt_result_syntax in
    match contents_list with
    | Single contents -> validate_contents vi batch_state contents
    | Cons (contents, tail) ->
        let* batch_state = validate_contents vi batch_state contents in
        validate_contents_list vi batch_state tail

  (** Return the new value that [remaining_block_gas] should have in
      [state] after the validation of a manager
      operation:

      - In [Block] (ie. block validation or block full construction)
        mode, this value is [batch_state.remaining_block_gas], in which
        the gas from the validated operation has been subtracted.

      - In [Mempool] mode, the [remaining_block_gas] in
        [state] should remain unchanged. Indeed, we
        only want each batch to not exceed the block limit individually,
        without taking other operations into account. *)
  let maybe_update_remaining_block_gas vi vs batch_state =
    match vi.mode with
    | Application _ | Partial_application _ | Construction _ ->
        batch_state.remaining_block_gas
    | Mempool -> vs.manager_state.remaining_block_gas

  let validate_manager_operation vi vs ~should_check_signature source oph
      (type kind) (operation : kind Kind.manager operation) =
    let open Lwt_result_syntax in
    let*? () =
      (* One-operation-per-manager-per-block restriction (1M).

         We want to check 1M before we call
         {!Contract.check_counter_increment} in
         {!check_batch_sanity_and_find_public_key}. Indeed, if a block
         contains two operations from the same manager, it is more
         relevant to fail the second one with {!Manager_restriction}
         than with {!Contract_storage.Counter_in_the_future}. *)
      match
        Signature.Public_key_hash.Map.find source vs.manager_state.managers_seen
      with
      | None -> Result.return_unit
      | Some conflicting_oph ->
          error (Manager_restriction (source, conflicting_oph))
    in
    let contents_list = operation.protocol_data.contents in
    let* batch_state, source_pk =
      check_sanity_and_find_public_key vi vs contents_list
    in
    let* batch_state = validate_contents_list vi batch_state contents_list in
    let*? () =
      if should_check_signature then
        Operation.check_signature source_pk vi.chain_id operation
      else ok ()
    in
    let managers_seen =
      Signature.Public_key_hash.Map.add
        source
        oph
        vs.manager_state.managers_seen
    in
    let remaining_block_gas =
      maybe_update_remaining_block_gas vi vs batch_state
    in
    return {vs with manager_state = {managers_seen; remaining_block_gas}}

  let rec sum_batch_gas_limit :
      type kind.
      Gas.Arith.integral ->
      kind Kind.manager contents_list ->
      Gas.Arith.integral =
   fun acc contents_list ->
    match contents_list with
    | Single (Manager_operation {gas_limit; _}) -> Gas.Arith.add gas_limit acc
    | Cons (Manager_operation {gas_limit; _}, tail) ->
        sum_batch_gas_limit (Gas.Arith.add gas_limit acc) tail

  let remove_manager_operation (type manager_kind) vi vs
      (operation : manager_kind Kind.manager operation) =
    let source =
      match operation.protocol_data.contents with
      | Single (Manager_operation {source; _})
      | Cons (Manager_operation {source; _}, _) ->
          source
    in
    match
      Signature.Public_key_hash.Map.find_opt
        source
        vs.manager_state.managers_seen
    with
    | None -> (* Nothing to do *) vs
    | Some _oph ->
        let managers_seen =
          Signature.Public_key_hash.Map.remove
            source
            vs.manager_state.managers_seen
        in
        let remaining_block_gas =
          match vi.mode with
          | Application _ | Partial_application _ | Construction _ ->
              let gas_limit =
                sum_batch_gas_limit
                  Gas.Arith.zero
                  operation.protocol_data.contents
              in
              Gas.Arith.(
                sub vs.manager_state.remaining_block_gas (fp gas_limit))
          | Mempool ->
              (* The remaining block gas is never updated in [Mempool]
                 mode anyway (see {!maybe_update_remaining_block_gas}). *)
              vs.manager_state.remaining_block_gas
        in
        {vs with manager_state = {managers_seen; remaining_block_gas}}
end

let init_info_and_state ctxt mode chain_id all_expected_consensus_features =
  let info = init_info ctxt mode chain_id all_expected_consensus_features in
  let state = init_state ctxt in
  {info; state}

(* Pre-condition: Shell block headers' checks have already been done.
   These checks must ensure that:
   - the block header level is the succ of the predecessor block level
   - the timestamp of the predecessor is lower than the current block's
   - the fitness of the block is greater than its predecessor's
   - the number of operations by validation passes does not exceed the quota
     established by the protocol
   - the size of an operation does not exceed [max_operation_data_length]
*)
let begin_application ctxt chain_id ~predecessor_level ~predecessor_timestamp
    (block_header : Block_header.t) fitness ~is_partial =
  let open Lwt_result_syntax in
  let predecessor_round = Fitness.predecessor_round fitness in
  let round = Fitness.round fitness in
  let current_level = Level.current ctxt in
  let* ctxt, _slot, (block_producer_pk, block_producer) =
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
      ~delegate_pk:block_producer_pk
      ~round_durations:(Constants.round_durations ctxt)
      ~proof_of_work_threshold:(Constants.proof_of_work_threshold ctxt)
      ~expected_commitment:current_level.expected_commitment
  in
  let* () = Consensus.check_frozen_deposits_are_positive ctxt block_producer in
  let* ctxt, _slot, (_payload_producer_pk, payload_producer) =
    Stake_distribution.baking_rights_owner
      ctxt
      current_level
      ~round:block_header.protocol_data.contents.payload_round
  in
  let payload_hash = block_header.protocol_data.contents.payload_hash in
  let predecessor_hash = block_header.shell.predecessor in
  let application_info =
    {
      fitness;
      block_producer;
      payload_producer;
      predecessor_hash;
      block_data_contents = block_header.protocol_data.contents;
    }
  in
  let mode =
    if is_partial then Partial_application application_info
    else Application application_info
  in
  let all_expected_consensus_features =
    Consensus.expected_features_for_block_validation
      ctxt
      fitness
      payload_hash
      ~predecessor_level
      ~predecessor_round
      ~predecessor_hash
  in
  return
    (init_info_and_state ctxt mode chain_id all_expected_consensus_features)

let begin_partial_application ~ancestor_context chain_id ~predecessor_level
    ~predecessor_timestamp (block_header : Block_header.t) fitness =
  begin_application
    ancestor_context
    chain_id
    ~predecessor_level
    ~predecessor_timestamp
    block_header
    fitness
    ~is_partial:true

let begin_application ctxt chain_id ~predecessor_level ~predecessor_timestamp
    (block_header : Block_header.t) fitness =
  begin_application
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
  let* ctxt, _slot, (_block_producer_pk, block_producer) =
    Stake_distribution.baking_rights_owner ctxt current_level ~round
  in
  let* () = Consensus.check_frozen_deposits_are_positive ctxt block_producer in
  let* ctxt, _slot, (_payload_producer_pk, payload_producer) =
    Stake_distribution.baking_rights_owner
      ctxt
      current_level
      ~round:header_contents.payload_round
  in
  let all_expected_consensus_features =
    Consensus.expected_features_for_block_construction
      ctxt
      round
      header_contents.payload_hash
      ~predecessor_level
      ~predecessor_round
      ~predecessor_hash
  in
  let validation_state =
    init_info_and_state
      ctxt
      (Construction
         {
           predecessor_round;
           predecessor_hash;
           round;
           block_data_contents = header_contents;
           block_producer;
           payload_producer;
         })
      chain_id
      all_expected_consensus_features
  in
  return validation_state

let begin_partial_construction ctxt chain_id ~predecessor_level
    ~predecessor_round ~predecessor_hash:_ ~grandparent_round =
  let open Lwt_result_syntax in
  let all_expected_consensus_features =
    Consensus.expected_features_for_mempool
      ctxt
      ~predecessor_level
      ~predecessor_round
      ~grandparent_round
  in
  let validation_state =
    init_info_and_state ctxt Mempool chain_id all_expected_consensus_features
  in
  return validation_state

let begin_no_predecessor_info ctxt chain_id =
  let all_expected_consensus_features =
    {
      expected_preendorsement =
        No_predecessor_info_cannot_validate_preendorsement;
      expected_endorsement = No_predecessor_info_cannot_validate_endorsement;
      expected_grandparent_endorsement_for_mempool = None;
    }
  in
  init_info_and_state ctxt Mempool chain_id all_expected_consensus_features

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

let check_validation_pass_consistency vi vs validation_pass =
  let open Lwt_tzresult_syntax in
  match vi.mode with
  | Mempool | Construction _ -> return vs
  | Application _ | Partial_application _ -> (
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
      | Some _, None -> fail Validate_errors.Failing_noop_error)

let validate_operation {info; state} ?(should_check_signature = true) oph
    (packed_operation : packed_operation) =
  let open Lwt_tzresult_syntax in
  let validation_pass_opt =
    Alpha_context.Operation.acceptable_pass packed_operation
  in
  let {shell; protocol_data = Operation_data protocol_data} =
    packed_operation
  in
  let* state =
    check_validation_pass_consistency info state validation_pass_opt
  in
  let state = record_operation state oph validation_pass_opt in
  let operation : _ Alpha_context.operation = {shell; protocol_data} in
  let* state =
    match (info.mode, validation_pass_opt) with
    | Partial_application _, Some n
      when Compare.Int.(n <> Operation_repr.consensus_pass) ->
        (* Do not validate non consensus operation in [Partial_application] mode *)
        return state
    | Partial_application _, _
    | Mempool, _
    | Construction _, _
    | Application _, _ -> (
        match operation.protocol_data.contents with
        | Single (Preendorsement _) ->
            Consensus.validate_preendorsement
              info
              state
              ~should_check_signature
              operation
        | Single (Endorsement _) ->
            Consensus.validate_endorsement
              info
              state
              ~should_check_signature
              operation
        | Single (Dal_slot_availability _) ->
            Consensus.validate_dal_slot_availability
              info
              state
              ~should_check_signature
              operation
        | Single (Proposals _) ->
            Voting.validate_proposals
              info
              state
              ~should_check_signature
              oph
              operation
        | Single (Ballot _) ->
            Voting.validate_ballot
              info
              state
              ~should_check_signature
              oph
              operation
        | Single (Activate_account _ as contents) ->
            Anonymous.validate_activate_account info state oph contents
        | Single (Double_preendorsement_evidence _ as contents) ->
            Anonymous.validate_double_preendorsement_evidence
              info
              state
              oph
              contents
        | Single (Double_endorsement_evidence _ as contents) ->
            Anonymous.validate_double_endorsement_evidence
              info
              state
              oph
              contents
        | Single (Double_baking_evidence _ as contents) ->
            Anonymous.validate_double_baking_evidence info state oph contents
        | Single (Seed_nonce_revelation _ as contents) ->
            Anonymous.validate_seed_nonce_revelation info state contents
        | Single (Vdf_revelation _ as contents) ->
            Anonymous.validate_vdf_revelation info state contents
        | Single (Manager_operation {source; _}) ->
            Manager.validate_manager_operation
              info
              state
              ~should_check_signature
              source
              oph
              operation
        | Cons (Manager_operation {source; _}, _) ->
            Manager.validate_manager_operation
              info
              state
              ~should_check_signature
              source
              oph
              operation
        | Single (Failing_noop _) -> fail Validate_errors.Failing_noop_error)
  in
  return state

let are_endorsements_required vi =
  let open Lwt_result_syntax in
  let+ first_level = First_level_of_protocol.get vi.ctxt in
  (* [Comment from Legacy_apply] NB: the first level is the level
     of the migration block. There are no endorsements for this
     block. Therefore the block at the next level cannot contain
     endorsements. *)
  let level_position_in_protocol =
    Raw_level.diff vi.current_level.level first_level
  in
  Compare.Int32.(level_position_in_protocol > 1l)

let check_endorsement_power vi vs =
  let required = Constants.consensus_threshold vi.ctxt
  and provided = vs.consensus_state.endorsement_power in
  error_unless
    Compare.Int.(provided >= required)
    (Validate_errors.Block.Not_enough_endorsements {required; provided})

let finalize_validate_block_header vi vs checkable_payload_hash
    (block_header_contents : Alpha_context.Block_header.contents) round fitness
    =
  let locked_round_evidence =
    Option.map
      (fun (preendorsement_round, preendorsement_count) ->
        Block_header.{preendorsement_round; preendorsement_count})
      vs.consensus_state.locked_round_evidence
  in
  Block_header.finalize_validate_block_header
    ~block_header_contents
    ~round
    ~fitness
    ~checkable_payload_hash
    ~locked_round_evidence
    ~consensus_threshold:(Constants.consensus_threshold vi.ctxt)

let compute_payload_hash (vs : state)
    (block_header_contents : Alpha_context.Block_header.contents) predecessor =
  let operations_hash =
    Operation_list_hash.compute (List.rev vs.recorded_operations_rev)
  in
  Block_payload.hash
    ~predecessor
    block_header_contents.payload_round
    operations_hash

let finalize_block {info; state} =
  let open Lwt_tzresult_syntax in
  match info.mode with
  | Application {fitness; predecessor_hash; block_data_contents; _} ->
      let* are_endorsements_required = are_endorsements_required info in
      let*? () =
        if are_endorsements_required then check_endorsement_power info state
        else ok ()
      in
      let block_payload_hash =
        compute_payload_hash state block_data_contents predecessor_hash
      in
      let round = Fitness.round fitness in
      let*? () =
        finalize_validate_block_header
          info
          state
          (Block_header.Expected_payload_hash block_payload_hash)
          block_data_contents
          round
          fitness
      in
      return_unit
  | Partial_application _ ->
      let* are_endorsements_required = are_endorsements_required info in
      let*? () =
        if are_endorsements_required then check_endorsement_power info state
        else ok ()
      in
      return_unit
  | Construction
      {predecessor_round; predecessor_hash; round; block_data_contents; _} ->
      let block_payload_hash =
        compute_payload_hash state block_data_contents predecessor_hash
      in
      let locked_round_evidence = state.consensus_state.locked_round_evidence in
      let checkable_payload_hash =
        match locked_round_evidence with
        | Some _ -> Block_header.Expected_payload_hash block_payload_hash
        | None ->
            (* In full construction, when there is no locked round
               evidence (and thus no preendorsements), the baker cannot
               know the payload hash before selecting the operations. We
               may dismiss checking the initially given
               payload_hash. However, to be valid, the baker must patch
               the resulting block header with the actual payload
               hash. *)
            Block_header.No_check
      in
      let* are_endorsements_required = are_endorsements_required info in
      let*? () =
        if are_endorsements_required then check_endorsement_power info state
        else ok ()
      in
      let* fitness =
        let locked_round =
          match locked_round_evidence with
          | None -> None
          | Some (preendorsement_round, _power) -> Some preendorsement_round
        in
        let level = (Level.current info.ctxt).level in
        let*? fitness =
          Fitness.create ~level ~round ~predecessor_round ~locked_round
        in
        return fitness
      in
      let*? () =
        finalize_validate_block_header
          info
          state
          checkable_payload_hash
          block_data_contents
          round
          fitness
      in
      return_unit
  | Mempool ->
      (* Nothing to do for the mempool mode*)
      return_unit

(* This function will be replaced with a generic remove_operation in
   the future. *)
let remove_manager_operation {info; state} =
  Manager.remove_manager_operation info state
