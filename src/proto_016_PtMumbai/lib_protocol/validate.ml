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

open Validate_errors
open Alpha_context

(** Since the expected features of preendorsement and endorsement are
    the same for all operations in the considered block, we compute
    them once and for all at the begining of the block.

    See [expected_features_for_application],
    [expected_features_for_construction], and
    [expected_features_for_partial_construction] in the [Consensus]
    module below. *)
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
          (** During block validation or construction, we must also check that
              the preendorsement round is lower than the block round. In
              mempool mode, this field is [None]. *)
    }
  | No_locked_round_for_block_validation_preendorsement
      (** A preexisting block whose fitness indicates no locked round
          should contain no preendorsements. *)
  | Fresh_proposal_for_construction_preendorsement
      (** A constructed block with a fresh proposal should contain no
          preendorsements. *)
  | No_expected_branch_for_partial_construction_preendorsement of {
      expected_level : Raw_level.t;
    }
      (** See [No_expected_branch_for_partial_construction_endorsement] below. *)
  | No_predecessor_info_cannot_validate_preendorsement
      (** We do not have access to predecessor level, round, etc. so any
          preendorsement validation will fail. *)

type expected_endorsement =
  | Expected_endorsement of {expected_features : expected_features}
  | No_expected_branch_for_block_endorsement
      (** The context contains no branch: this happens to the first block
          that uses the Tenderbake consensus algorithm. This block contains
          no endorsements. *)
  | No_expected_branch_for_partial_construction_endorsement of {
      expected_level : Raw_level.t;
    }
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
  expected_grandparent_endorsement_for_partial_construction :
    expected_features option;
      (** This only has a value in Mempool mode and when the [ctxt] has a
          [grandparent_branch]; it is [None] in all other cases. *)
}

type consensus_info = {
  all_expected_features : all_expected_consensus_features;
  preendorsement_slot_map : (Consensus_key.pk * int) Slot.Map.t;
  endorsement_slot_map : (Consensus_key.pk * int) Slot.Map.t;
}

let init_consensus_info ctxt all_expected_features =
  {
    all_expected_features;
    preendorsement_slot_map = Consensus.allowed_preendorsements ctxt;
    endorsement_slot_map = Consensus.allowed_endorsements ctxt;
  }

module Consensus_content_map = Map.Make (struct
  type t = consensus_content

  let compare {slot; level; round; block_payload_hash}
      {
        slot = slot';
        level = level';
        round = round';
        block_payload_hash = block_payload_hash';
      } =
    Compare.or_else (Raw_level.compare level level') @@ fun () ->
    Compare.or_else (Slot.compare slot slot') @@ fun () ->
    Compare.or_else (Round.compare round round') @@ fun () ->
    Compare.or_else
      (Block_payload_hash.compare block_payload_hash block_payload_hash')
    @@ fun () -> 0
end)

type consensus_state = {
  predecessor_level : Raw_level.t;
  preendorsements_seen : Operation_hash.t Slot.Map.t;
  endorsements_seen : Operation_hash.t Slot.Map.t;
  grandparent_endorsements_seen : Operation_hash.t Slot.Map.t;
  dal_attestation_seen : Operation_hash.t Signature.Public_key_hash.Map.t;
}

let slot_map_encoding element_encoding =
  let open Data_encoding in
  conv
    (fun slot_map -> Slot.Map.bindings slot_map)
    (fun l -> Slot.Map.(List.fold_left (fun m (k, v) -> add k v m) empty l))
    (list (tup2 Slot.encoding element_encoding))

let consensus_state_encoding =
  let open Data_encoding in
  def "consensus_state"
  @@ conv
       (fun {
              predecessor_level;
              preendorsements_seen;
              endorsements_seen;
              grandparent_endorsements_seen;
              dal_attestation_seen;
            } ->
         ( predecessor_level,
           preendorsements_seen,
           endorsements_seen,
           grandparent_endorsements_seen,
           dal_attestation_seen ))
       (fun ( predecessor_level,
              preendorsements_seen,
              endorsements_seen,
              grandparent_endorsements_seen,
              dal_attestation_seen ) ->
         {
           predecessor_level;
           preendorsements_seen;
           endorsements_seen;
           grandparent_endorsements_seen;
           dal_attestation_seen;
         })
       (obj5
          (req "predecessor_level" Raw_level.encoding)
          (req
             "preendorsements_seen"
             (slot_map_encoding Operation_hash.encoding))
          (req "endorsements_seen" (slot_map_encoding Operation_hash.encoding))
          (req
             "grandparent_endorsements_seen"
             (slot_map_encoding Operation_hash.encoding))
          (req
             "dal_attestation_seen"
             (Signature.Public_key_hash.Map.encoding Operation_hash.encoding)))

let init_consensus_state ~predecessor_level =
  {
    predecessor_level;
    preendorsements_seen = Slot.Map.empty;
    endorsements_seen = Slot.Map.empty;
    grandparent_endorsements_seen = Slot.Map.empty;
    dal_attestation_seen = Signature.Public_key_hash.Map.empty;
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

module Double_endorsing_evidence_map = struct
  include Map.Make (struct
    type t = Raw_level.t * Round.t * Slot.t

    let compare (l, r, s) (l', r', s') =
      Compare.or_else (Raw_level.compare l l') @@ fun () ->
      Compare.or_else (Round.compare r r') @@ fun () ->
      Compare.or_else (Slot.compare s s') @@ fun () -> 0
  end)

  let encoding elt_encoding =
    Data_encoding.conv
      (fun map -> bindings map)
      (fun l -> List.fold_left (fun m (k, v) -> add k v m) empty l)
      Data_encoding.(
        list
          (tup2
             (tup3 Raw_level.encoding Round.encoding Slot.encoding)
             elt_encoding))
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
  double_endorsing_evidences_seen :
    Operation_hash.t Double_endorsing_evidence_map.t;
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
              double_endorsing_evidences_seen;
              seed_nonce_levels_seen;
              vdf_solution_seen;
            } ->
         ( activation_pkhs_seen,
           double_baking_evidences_seen,
           double_endorsing_evidences_seen,
           seed_nonce_levels_seen,
           vdf_solution_seen ))
       (fun ( activation_pkhs_seen,
              double_baking_evidences_seen,
              double_endorsing_evidences_seen,
              seed_nonce_levels_seen,
              vdf_solution_seen ) ->
         {
           activation_pkhs_seen;
           double_baking_evidences_seen;
           double_endorsing_evidences_seen;
           seed_nonce_levels_seen;
           vdf_solution_seen;
         })
       (obj5
          (req
             "activation_pkhs_seen"
             (Ed25519.Public_key_hash.Map.encoding Operation_hash.encoding))
          (req
             "double_baking_evidences_seen"
             (Double_baking_evidence_map.encoding Operation_hash.encoding))
          (req
             "double_endorsing_evidences_seen"
             (Double_endorsing_evidence_map.encoding Operation_hash.encoding))
          (req
             "seed_nonce_levels_seen"
             (raw_level_map_encoding Operation_hash.encoding))
          (opt "vdf_solution_seen" Operation_hash.encoding))

let empty_anonymous_state =
  {
    activation_pkhs_seen = Ed25519.Public_key_hash.Map.empty;
    double_baking_evidences_seen = Double_baking_evidence_map.empty;
    double_endorsing_evidences_seen = Double_endorsing_evidence_map.empty;
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

(** Mode-dependent information needed in final checks. *)
type block_finalization_info = {
  fitness : Fitness.t;
  block_producer : Consensus_key.pk;
  payload_producer : Consensus_key.pk;
  predecessor_hash : Block_hash.t;
  block_data_contents : Block_header.contents;
}

(** Circumstances in which operations are validated, and corresponding
    information.

    - [Application] is used for the validation of a preexisting block,
      often in preparation for its future application.

    - [Partial_validation] is used to quickly but partially validate a
      preexisting block, e.g. to quickly decide whether an alternate
      branch seems viable. In this mode, the initial {!type:context} may
      come from an ancestor block instead of the predecessor block. Only
      consensus operations are validated in this mode.

    - [Construction] is used for the construction of a new block.

    - [Mempool] is used by the {!module:Mempool} and by the
      [Partial_construction] mode in {!module:Main}, which may itself be
      used by RPCs or by another mempool implementation. (The [Mempool]
      mode is also used by the plugin.)

    If you add a new mode, please make sure that it has a way to bound
    the size of the map {!recfield:manager_state.managers_seen}. *)
type mode =
  | Application of block_finalization_info
  | Partial_validation of block_finalization_info
  | Construction of {
      predecessor_round : Round.t;
      predecessor_hash : Block_hash.t;
      round : Round.t;
      block_data_contents : Block_header.contents;
      block_producer : Consensus_key.pk;
      payload_producer : Consensus_key.pk;
    }
  | Mempool

(** {2 Definition and initialization of [info] and [state]} *)

type info = {
  ctxt : t;  (** The context at the beginning of the block or mempool. *)
  mode : mode;
  chain_id : Chain_id.t;  (** Needed for signature checks. *)
  current_level : Level.t;
  consensus_info : consensus_info;
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
  locked_round_evidence : (Round.t * int) option;
  endorsement_power : int;
}

type validation_state = {
  info : info;
  operation_state : operation_conflict_state;
  block_state : block_state;
}

let ok_unit = Result_syntax.return_unit

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

let empty_voting_state =
  {
    proposals_seen = Signature.Public_key_hash.Map.empty;
    ballots_seen = Signature.Public_key_hash.Map.empty;
  }

let init_operation_conflict_state ~predecessor_level =
  {
    consensus_state = init_consensus_state ~predecessor_level;
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
    endorsement_power = 0;
  }

let get_initial_ctxt {info; _} = info.ctxt

(** Validation of consensus operations (validation pass [0]):
    preendorsement, endorsement, and dal_attestation. *)
module Consensus = struct
  let expected_endorsement_features ~predecessor_level ~predecessor_round branch
      payload_hash =
    {
      level = predecessor_level.Level.level;
      round = Some predecessor_round;
      branch;
      payload_hash;
    }

  (** Expected endorsement features for all modes in which a block is
      considered: application, partial validation, and construction. *)
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

  (** Retrieve the expected consensus features for both application and
      partial validation modes. *)
  let expected_features_for_application ctxt fitness payload_hash
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
      expected_grandparent_endorsement_for_partial_construction = None;
    }

  let expected_features_for_construction ctxt round payload_hash
      ~predecessor_level ~predecessor_round ~predecessor_hash =
    let expected_preendorsement =
      if Block_payload_hash.(payload_hash = zero) then
        (* When the proposal is fresh, a fake [payload_hash] of [zero]
           has been provided. In this case, the block should not
           contain any preendorsements. *)
        Fresh_proposal_for_construction_preendorsement
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
      expected_grandparent_endorsement_for_partial_construction = None;
    }

  let expected_features_for_partial_construction ctxt ~predecessor_level
      ~predecessor_round ~grandparent_round =
    let expected_preendorsement, expected_endorsement =
      match Consensus.endorsement_branch ctxt with
      | None ->
          let expected_level = predecessor_level.Level.level in
          ( No_expected_branch_for_partial_construction_preendorsement
              {expected_level},
            No_expected_branch_for_partial_construction_endorsement
              {expected_level} )
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
    let expected_grandparent_endorsement_for_partial_construction =
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
      expected_grandparent_endorsement_for_partial_construction;
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

  let check_round kind expected (consensus_content : consensus_content) =
    let provided = consensus_content.round in
    error_unless
      (Round.equal expected provided)
      (if Round.(expected > provided) then
       Consensus_operation_for_old_round {kind; expected; provided}
      else Consensus_operation_for_future_round {kind; expected; provided})

  let check_round_equal kind expected_features
      (consensus_content : consensus_content) =
    match expected_features.round with
    | Some expected -> check_round kind expected consensus_content
    | None -> ok_unit

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

  let check_consensus_features kind (expected : expected_features)
      (consensus_content : consensus_content) (operation : 'a operation) =
    let open Result_syntax in
    let* () = check_level_equal kind expected consensus_content in
    let* () = check_round_equal kind expected consensus_content in
    let* () = check_branch_equal kind expected operation in
    check_payload_hash_equal kind expected consensus_content

  let get_expected_preendorsements_features consensus_info consensus_content =
    match consensus_info.all_expected_features.expected_preendorsement with
    | Expected_preendorsement {expected_features; block_round} ->
        ok (expected_features, block_round)
    | No_locked_round_for_block_validation_preendorsement
    | Fresh_proposal_for_construction_preendorsement ->
        error Unexpected_preendorsement_in_block
    | No_expected_branch_for_partial_construction_preendorsement
        {expected_level} ->
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
    | None -> ok_unit
    | Some block_round ->
        error_unless
          Round.(provided < block_round)
          (Preendorsement_round_too_high {block_round; provided})

  let get_delegate_details slot_map kind consensus_content =
    Result.of_option
      (Slot.Map.find consensus_content.slot slot_map)
      ~error:(trace_of_error (Wrong_slot_used_for_consensus_operation {kind}))

  let check_preendorsement vi ~check_signature
      (operation : Kind.preendorsement operation) =
    let open Lwt_result_syntax in
    let (Single (Preendorsement consensus_content)) =
      operation.protocol_data.contents
    in
    let kind = Preendorsement in
    let*? expected_features, block_round =
      get_expected_preendorsements_features vi.consensus_info consensus_content
    in
    let*? () =
      check_round_not_too_high ~block_round ~provided:consensus_content.round
    in
    let*? () =
      check_consensus_features
        kind
        expected_features
        consensus_content
        operation
    in
    let*? consensus_key, voting_power =
      get_delegate_details
        vi.consensus_info.preendorsement_slot_map
        kind
        consensus_content
    in
    let* () =
      check_frozen_deposits_are_positive vi.ctxt consensus_key.delegate
    in
    let*? () =
      if check_signature then
        Operation.check_signature
          consensus_key.consensus_pk
          vi.chain_id
          operation
      else ok_unit
    in
    return voting_power

  let check_preendorsement_conflict vs oph (op : Kind.preendorsement operation)
      =
    let (Single (Preendorsement consensus_content)) =
      op.protocol_data.contents
    in
    match
      Slot.Map.find_opt
        consensus_content.slot
        vs.consensus_state.preendorsements_seen
    with
    | Some existing ->
        Error (Operation_conflict {existing; new_operation = oph})
    | None -> ok_unit

  let wrap_preendorsement_conflict = function
    | Ok () -> ok_unit
    | Error conflict ->
        error
          Validate_errors.Consensus.(
            Conflicting_consensus_operation {kind = Preendorsement; conflict})

  let add_preendorsement vs oph (op : Kind.preendorsement operation) =
    let (Single (Preendorsement consensus_content)) =
      op.protocol_data.contents
    in
    let preendorsements_seen =
      Slot.Map.add
        consensus_content.slot
        oph
        vs.consensus_state.preendorsements_seen
    in
    {vs with consensus_state = {vs.consensus_state with preendorsements_seen}}

  let may_update_locked_round_evidence block_state mode
      (consensus_content : consensus_content) voting_power =
    let locked_round_evidence =
      match mode with
      | Mempool -> None
      | Application _ | Partial_validation _ | Construction _ -> (
          match block_state.locked_round_evidence with
          | None -> Some (consensus_content.round, voting_power)
          | Some (_stored_round, evidences) ->
              (* [_stored_round] is always equal to [consensus_content.round].
                 Indeed, this is ensured by {!check_round_equal} in
                 application and partial validation modes, and by
                 {!check_construction_preendorsement_round_consistency} in
                 construction mode. *)
              Some (consensus_content.round, evidences + voting_power))
    in
    {block_state with locked_round_evidence}

  (* Hypothesis: this function will only be called in mempool mode *)
  let remove_preendorsement vs (operation : Kind.preendorsement operation) =
    (* As we are in mempool mode, we do not update
       [locked_round_evidence]. *)
    let (Single (Preendorsement consensus_content)) =
      operation.protocol_data.contents
    in
    let preendorsements_seen =
      Slot.Map.remove
        consensus_content.slot
        vs.consensus_state.preendorsements_seen
    in
    {vs with consensus_state = {vs.consensus_state with preendorsements_seen}}

  (** Validate an endorsement pointing to the grandparent block. This
      function will only be called in [Partial_construction] mode. *)
  let check_grandparent_endorsement vi ~check_signature expected operation
      (consensus_content : consensus_content) =
    let open Lwt_result_syntax in
    let kind = Grandparent_endorsement in
    let level = Level.from_raw vi.ctxt consensus_content.level in
    let* (_ctxt : t), consensus_key =
      Stake_distribution.slot_owner vi.ctxt level consensus_content.slot
    in
    let*? () =
      check_consensus_features kind expected consensus_content operation
    in
    let*? () =
      if check_signature then
        Operation.check_signature
          consensus_key.consensus_pk
          vi.chain_id
          operation
      else ok_unit
    in
    return_unit

  let add_grandparent_endorsement vs oph (consensus_content : consensus_content)
      =
    {
      vs with
      consensus_state =
        {
          vs.consensus_state with
          grandparent_endorsements_seen =
            Slot.Map.add
              consensus_content.slot
              oph
              vs.consensus_state.grandparent_endorsements_seen;
        };
    }

  let check_grandparent_endorsement_conflict vs oph
      (consensus_content : consensus_content) =
    match
      Slot.Map.find_opt
        consensus_content.slot
        vs.consensus_state.grandparent_endorsements_seen
    with
    | None -> ok_unit
    | Some existing ->
        Error (Operation_conflict {existing; new_operation = oph})

  let remove_grandparent_endorsement vs (consensus_content : consensus_content)
      =
    let grandparent_endorsements_seen =
      Slot.Map.remove
        consensus_content.slot
        vs.consensus_state.grandparent_endorsements_seen
    in
    {
      vs with
      consensus_state = {vs.consensus_state with grandparent_endorsements_seen};
    }

  let get_expected_endorsements_features consensus_info consensus_content =
    match consensus_info.all_expected_features.expected_endorsement with
    | Expected_endorsement {expected_features} -> ok expected_features
    | No_expected_branch_for_block_endorsement ->
        error Unexpected_endorsement_in_block
    | No_expected_branch_for_partial_construction_endorsement {expected_level}
      ->
        error
          (Consensus_operation_for_future_level
             {
               kind = Endorsement;
               expected = expected_level;
               provided = consensus_content.Alpha_context.level;
             })
    | No_predecessor_info_cannot_validate_endorsement ->
        error Consensus_operation_not_allowed

  type endorsement_kind = Grandparent_endorsement | Normal_endorsement of int

  (** Validate an endorsement pointing to the predecessor, aka a
      "normal" endorsement. Only this kind of endorsement may be found
      during block validation or construction. *)
  let check_normal_endorsement vi ~check_signature
      (operation : Kind.endorsement operation) =
    let open Lwt_result_syntax in
    let (Single (Endorsement consensus_content)) =
      operation.protocol_data.contents
    in
    let kind = Endorsement in
    let*? expected_features =
      get_expected_endorsements_features vi.consensus_info consensus_content
    in
    let*? () =
      check_consensus_features
        kind
        expected_features
        consensus_content
        operation
    in
    let*? consensus_key, voting_power =
      get_delegate_details
        vi.consensus_info.endorsement_slot_map
        kind
        consensus_content
    in
    let* () =
      check_frozen_deposits_are_positive vi.ctxt consensus_key.delegate
    in
    let*? () =
      if check_signature then
        Operation.check_signature
          consensus_key.consensus_pk
          vi.chain_id
          operation
      else ok_unit
    in
    return voting_power

  let check_normal_endorsement_conflict vs oph
      (consensus_content : consensus_content) =
    match
      Slot.Map.find_opt
        consensus_content.slot
        vs.consensus_state.endorsements_seen
    with
    | None -> ok_unit
    | Some existing ->
        Error (Operation_conflict {existing; new_operation = oph})

  let add_normal_endorsement vs oph (consensus_content : consensus_content) =
    {
      vs with
      consensus_state =
        {
          vs.consensus_state with
          endorsements_seen =
            Slot.Map.add
              consensus_content.slot
              oph
              vs.consensus_state.endorsements_seen;
        };
    }

  (* Hypothesis: this function will only be called in mempool mode *)
  let remove_normal_endorsement vs (consensus_content : consensus_content) =
    (* We do not remove the endorsement power because it is not
       relevant for the mempool mode. *)
    let endorsements_seen =
      Slot.Map.remove
        consensus_content.slot
        vs.consensus_state.endorsements_seen
    in
    {vs with consensus_state = {vs.consensus_state with endorsements_seen}}

  let check_endorsement vi ~check_signature
      (operation : Kind.endorsement operation) =
    let open Lwt_result_syntax in
    let (Single (Endorsement consensus_content)) =
      operation.protocol_data.contents
    in
    match
      vi.consensus_info.all_expected_features
        .expected_grandparent_endorsement_for_partial_construction
    with
    | Some expected_grandparent_endorsement
      when Raw_level.(
             consensus_content.level = expected_grandparent_endorsement.level)
      ->
        let* () =
          check_grandparent_endorsement
            vi
            ~check_signature
            expected_grandparent_endorsement
            operation
            (consensus_content : consensus_content)
        in
        return Grandparent_endorsement
    | _ ->
        let* voting_power =
          check_normal_endorsement vi ~check_signature operation
        in
        return (Normal_endorsement voting_power)

  let is_normal_endorsement_assuming_valid vs
      (consensus_content : consensus_content) =
    Raw_level.equal vs.consensus_state.predecessor_level consensus_content.level

  let check_endorsement_conflict vs oph (operation : Kind.endorsement operation)
      =
    let (Single (Endorsement consensus_content)) =
      operation.protocol_data.contents
    in
    if is_normal_endorsement_assuming_valid vs consensus_content then
      check_normal_endorsement_conflict vs oph consensus_content
    else check_grandparent_endorsement_conflict vs oph consensus_content

  let wrap_endorsement_conflict = function
    | Ok () -> ok_unit
    | Error conflict ->
        error
          Validate_errors.Consensus.(
            Conflicting_consensus_operation {kind = Endorsement; conflict})

  let add_endorsement vs oph (op : Kind.endorsement operation) endorsement_kind
      =
    let (Single (Endorsement consensus_content)) = op.protocol_data.contents in
    match endorsement_kind with
    | Grandparent_endorsement ->
        add_grandparent_endorsement vs oph consensus_content
    | Normal_endorsement _voting_power ->
        add_normal_endorsement vs oph consensus_content

  let may_update_endorsement_power block_state = function
    | Grandparent_endorsement -> block_state
    | Normal_endorsement voting_power ->
        {
          block_state with
          endorsement_power = block_state.endorsement_power + voting_power;
        }

  let remove_endorsement vs (op : Kind.endorsement operation) =
    let (Single (Endorsement consensus_content)) = op.protocol_data.contents in
    if is_normal_endorsement_assuming_valid vs consensus_content then
      remove_normal_endorsement vs consensus_content
    else remove_grandparent_endorsement vs consensus_content

  let check_dal_attestation vi (operation : Kind.dal_attestation operation) =
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
    let (Single (Dal_attestation op)) = operation.protocol_data.contents in
    let*? () =
      (* Note that this function checks the dal feature flag. *)
      Dal_apply.validate_attestation vi.ctxt op
    in
    return_unit

  let check_dal_attestation_conflict vs oph
      (operation : Kind.dal_attestation operation) =
    let (Single (Dal_attestation {attestor; attestation = _; level = _})) =
      operation.protocol_data.contents
    in
    match
      Signature.Public_key_hash.Map.find_opt
        attestor
        vs.consensus_state.dal_attestation_seen
    with
    | None -> ok_unit
    | Some existing ->
        Error (Operation_conflict {existing; new_operation = oph})

  let wrap_dal_attestation_conflict = function
    | Ok () -> ok_unit
    | Error conflict ->
        error
          Validate_errors.Consensus.(
            Conflicting_consensus_operation {kind = Dal_attestation; conflict})

  let add_dal_attestation vs oph (operation : Kind.dal_attestation operation) =
    let (Single (Dal_attestation {attestor; attestation = _; level = _})) =
      operation.protocol_data.contents
    in
    {
      vs with
      consensus_state =
        {
          vs.consensus_state with
          dal_attestation_seen =
            Signature.Public_key_hash.Map.add
              attestor
              oph
              vs.consensus_state.dal_attestation_seen;
        };
    }

  let remove_dal_attestation vs (operation : Kind.dal_attestation operation) =
    let (Single (Dal_attestation {attestor; attestation = _; level = _})) =
      operation.protocol_data.contents
    in
    let dal_attestation_seen =
      Signature.Public_key_hash.Map.remove
        attestor
        vs.consensus_state.dal_attestation_seen
    in
    {vs with consensus_state = {vs.consensus_state with dal_attestation_seen}}

  let check_construction_preendorsement_round_consistency vi block_state kind
      (consensus_content : consensus_content) =
    let open Result_syntax in
    let* expected_features, _block_round =
      get_expected_preendorsements_features vi.consensus_info consensus_content
    in
    match expected_features.round with
    | Some _ ->
        (* When [expected_features.round] has a value (ie. in
           application and partial validation modes when the block
           fitness has a [locked_round], and always in mempool mode),
           [check_preendorsement] already checks that all
           preendorsements have this expected round, so checking
           anything here would be redundant. Also note that when the
           fitness contains no [locked_round], this code is
           unreachable because [get_expected_preendorsements_features]
           returns an error. *)
        return_unit
    | None -> (
        (* For preendorsements in block construction mode,
           [expected_features.round] has been set to [None] because we
           could not know yet whether there is a locked round. *)
        match block_state.locked_round_evidence with
        | None ->
            (* This is the first validated preendorsement in
               construction mode: there is nothing to check. *)
            return_unit
        | Some (expected, _power) ->
            (* Other preendorsements have already been validated: we
               check that the current operation has the same round as
               them. *)
            check_round kind expected consensus_content)

  let validate_preendorsement ~check_signature info operation_state block_state
      oph (operation : Kind.preendorsement operation) =
    let open Lwt_result_syntax in
    let (Single (Preendorsement consensus_content)) =
      operation.protocol_data.contents
    in
    let* voting_power = check_preendorsement info ~check_signature operation in
    let*? () =
      check_construction_preendorsement_round_consistency
        info
        block_state
        Preendorsement
        consensus_content
    in
    let*? () =
      check_preendorsement_conflict operation_state oph operation
      |> wrap_preendorsement_conflict
    in
    (* We need to update the block state *)
    let block_state =
      may_update_locked_round_evidence
        block_state
        info.mode
        consensus_content
        voting_power
    in
    let operation_state = add_preendorsement operation_state oph operation in
    return {info; operation_state; block_state}

  let validate_endorsement ~check_signature info operation_state block_state oph
      operation =
    let open Lwt_result_syntax in
    let* kind = check_endorsement info ~check_signature operation in
    let*? () =
      check_endorsement_conflict operation_state oph operation
      |> wrap_endorsement_conflict
    in
    let block_state = may_update_endorsement_power block_state kind in
    let operation_state = add_endorsement operation_state oph operation kind in
    return {info; operation_state; block_state}
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
      match proposals with [] -> error Empty_proposals | _ :: _ -> ok_unit
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
        error (Wrong_voting_period_kind {current; expected = [Proposal]})

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
    | _ :: _ :: _ -> error Testnet_dictator_multiple_proposals

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
    if check_signature then
      (* Retrieving the public key should not fail as it *should* be
         called after checking that the delegate is in the vote
         listings (or is a testnet dictator), which implies that it
         is a manager with a revealed key. *)
      let* public_key = Contract.get_manager_key vi.ctxt source in
      Lwt.return (Operation.check_signature public_key vi.chain_id operation)
    else return_unit

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
        error Validate_errors.Voting.(Conflicting_proposals conflict)

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
        error
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
    when_ check_signature (fun () ->
        (* Retrieving the public key cannot fail. Indeed, we have
           already checked that the delegate is in the vote listings,
           which implies that it is a manager with a revealed key. *)
        let* public_key = Contract.get_manager_key vi.ctxt source in
        Lwt.return (Operation.check_signature public_key vi.chain_id operation))

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
    | Error conflict -> error (Conflicting_ballot conflict)

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
    let (Single (Activate_account {id = edpkh; activation_code})) =
      operation.protocol_data.contents
    in
    let open Lwt_result_syntax in
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
        error (Conflicting_activation {edpkh; conflict})

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

  let check_double_endorsing_evidence (type kind)
      ~consensus_operation:denunciation_kind vi
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
        let* ctxt, consensus_key1 =
          Stake_distribution.slot_owner vi.ctxt level e1.slot
        in
        let* ctxt, consensus_key2 =
          Stake_distribution.slot_owner ctxt level e2.slot
        in
        let delegate1, delegate2 =
          (consensus_key1.delegate, consensus_key2.delegate)
        in
        let*? () =
          error_unless
            (Signature.Public_key_hash.equal delegate1 delegate2)
            (Inconsistent_denunciation
               {kind = denunciation_kind; delegate1; delegate2})
        in
        let delegate_pk, delegate = (consensus_key1.consensus_pk, delegate1) in
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
        return_unit

  let check_double_preendorsement_evidence vi
      (operation : Kind.double_preendorsement_evidence operation) =
    let (Single (Double_preendorsement_evidence {op1; op2})) =
      operation.protocol_data.contents
    in
    check_double_endorsing_evidence
      ~consensus_operation:Preendorsement
      vi
      op1
      op2

  let check_double_endorsement_evidence vi
      (operation : Kind.double_endorsement_evidence operation) =
    let (Single (Double_endorsement_evidence {op1; op2})) =
      operation.protocol_data.contents
    in
    check_double_endorsing_evidence ~consensus_operation:Endorsement vi op1 op2

  let check_double_endorsing_evidence_conflict (type kind) vs oph
      (op1 : kind Kind.consensus Operation.t) =
    match op1.protocol_data.contents with
    | Single (Preendorsement e1) | Single (Endorsement e1) -> (
        match
          Double_endorsing_evidence_map.find
            (e1.level, e1.round, e1.slot)
            vs.anonymous_state.double_endorsing_evidences_seen
        with
        | None -> ok_unit
        | Some existing ->
            Error (Operation_conflict {existing; new_operation = oph}))

  let check_double_preendorsement_evidence_conflict vs oph
      (operation : Kind.double_preendorsement_evidence operation) =
    let (Single (Double_preendorsement_evidence {op1; _})) =
      operation.protocol_data.contents
    in
    check_double_endorsing_evidence_conflict vs oph op1

  let check_double_endorsement_evidence_conflict vs oph
      (operation : Kind.double_endorsement_evidence operation) =
    let (Single (Double_endorsement_evidence {op1; _})) =
      operation.protocol_data.contents
    in
    check_double_endorsing_evidence_conflict vs oph op1

  let wrap_denunciation_conflict kind = function
    | Ok () -> ok_unit
    | Error conflict -> error (Conflicting_denunciation {kind; conflict})

  let add_double_endorsing_evidence (type kind) vs oph
      (op1 : kind Kind.consensus Operation.t) =
    match op1.protocol_data.contents with
    | Single (Preendorsement e1) | Single (Endorsement e1) ->
        let double_endorsing_evidences_seen =
          Double_endorsing_evidence_map.add
            (e1.level, e1.round, e1.slot)
            oph
            vs.anonymous_state.double_endorsing_evidences_seen
        in
        {
          vs with
          anonymous_state =
            {vs.anonymous_state with double_endorsing_evidences_seen};
        }

  let add_double_endorsement_evidence vs oph
      (operation : Kind.double_endorsement_evidence operation) =
    let (Single (Double_endorsement_evidence {op1; _})) =
      operation.protocol_data.contents
    in
    add_double_endorsing_evidence vs oph op1

  let add_double_preendorsement_evidence vs oph
      (operation : Kind.double_preendorsement_evidence operation) =
    let (Single (Double_preendorsement_evidence {op1; _})) =
      operation.protocol_data.contents
    in
    add_double_endorsing_evidence vs oph op1

  let remove_double_endorsing_evidence (type kind) vs
      (op : kind Kind.consensus Operation.t) =
    match op.protocol_data.contents with
    | Single (Endorsement e) | Single (Preendorsement e) ->
        let double_endorsing_evidences_seen =
          Double_endorsing_evidence_map.remove
            (e.level, e.round, e.slot)
            vs.anonymous_state.double_endorsing_evidences_seen
        in
        let anonymous_state =
          {vs.anonymous_state with double_endorsing_evidences_seen}
        in
        {vs with anonymous_state}

  let remove_double_preendorsement_evidence vs
      (operation : Kind.double_preendorsement_evidence operation) =
    let (Single (Double_preendorsement_evidence {op1; _})) =
      operation.protocol_data.contents
    in
    remove_double_endorsing_evidence vs op1

  let remove_double_endorsement_evidence vs
      (operation : Kind.double_endorsement_evidence operation) =
    let (Single (Double_endorsement_evidence {op1; _})) =
      operation.protocol_data.contents
    in
    remove_double_endorsing_evidence vs op1

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
    let* ctxt, consensus_key1 =
      Stake_distribution.slot_owner vi.ctxt level slot1
    in
    let*? slot2 = Round.to_slot round2 ~committee_size in
    let* ctxt, consensus_key2 =
      Stake_distribution.slot_owner ctxt level slot2
    in
    let delegate1, delegate2 =
      (consensus_key1.delegate, consensus_key2.delegate)
    in
    let*? () =
      error_unless
        Signature.Public_key_hash.(delegate1 = delegate2)
        (Inconsistent_denunciation {kind = Block; delegate1; delegate2})
    in
    let delegate_pk, delegate = (consensus_key1.consensus_pk, delegate1) in
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
      if is_destination_allocated then ok Tez.zero
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
    let*? () =
      if check_signature then
        Operation.check_signature active_pk.consensus_pk info.chain_id operation
      else ok_unit
    in
    return_unit

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
    | Error conflict -> error (Conflicting_drain_delegate {delegate; conflict})

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
    | Error conflict -> error (Conflicting_nonce_revelation conflict)

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
    | Error conflict -> error (Conflicting_vdf_revelation conflict)

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
  let check_sanity_and_find_public_key vi
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
        Manager_counter.(succ previous_counter = counter)
        Inconsistent_counters
    in
    let rec check_batch_tail_sanity :
        type kind.
        public_key_hash ->
        Manager_counter.t ->
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
        (public_key_hash * public_key option * Manager_counter.t) tzresult =
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

  let assert_pvm_kind_enabled vi kind =
    error_when
      ((not (Constants.sc_rollup_arith_pvm_enable vi.ctxt))
      && Sc_rollup.Kind.(equal kind Example_arith))
      Sc_rollup_arith_pvm_disabled

  let assert_not_zero_messages messages =
    match messages with
    | [] -> error Sc_rollup_errors.Sc_rollup_add_zero_messages
    | _ -> ok_unit

  let assert_zk_rollup_feature_enabled vi =
    error_unless (Constants.zk_rollup_enable vi.ctxt) Zk_rollup_feature_disabled

  let consume_decoding_gas remaining_gas lexpr =
    record_trace Gas_quota_exceeded_init_deserialize
    @@ (* Fail early if the operation does not have enough gas to
          cover the deserialization cost. We always consider the full
          deserialization cost, independently from the internal state
          of the lazy_expr. Otherwise we might risk getting different
          results if the operation has already been deserialized
          before (e.g. when retrieved in JSON format). Note that the
          lazy_expr is not actually decoded here; its deserialization
          cost is estimated from the size of its bytes. *)
    Script.consume_decoding_gas remaining_gas lexpr

  let validate_tx_rollup_submit_batch vi remaining_gas content =
    let open Result_syntax in
    let* () = assert_tx_rollup_feature_enabled vi in
    let _message, message_size = Tx_rollup_message.make_batch content in
    let* cost = Tx_rollup_gas.hash_cost message_size in
    let size_limit = Constants.tx_rollup_hard_size_limit_per_message vi.ctxt in
    let* (_ : Gas.Arith.fp) = Gas.consume_from remaining_gas cost in
    error_unless
      Compare.Int.(message_size <= size_limit)
      Tx_rollup_errors.Message_size_exceeds_limit

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
    let* (_ : Gas.Arith.fp) =
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
    in
    return_unit

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

  let check_contents (type kind) vi batch_state
      (contents : kind Kind.manager contents) remaining_block_gas =
    let open Lwt_result_syntax in
    let (Manager_operation
          {source; fee; counter = _; operation; gas_limit; storage_limit}) =
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
    let*? () =
      let open Result_syntax in
      match operation with
      | Reveal pk -> Contract.check_public_key pk source
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
      | Delegation (Some pkh) -> Delegate.check_not_tz4 pkh
      | Update_consensus_key pk -> Delegate.Consensus_key.check_not_tz4 pk
      | Delegation None | Set_deposits_limit _ | Increase_paid_storage _ ->
          return_unit
      | Tx_rollup_origination -> assert_tx_rollup_feature_enabled vi
      | Tx_rollup_submit_batch {content; _} ->
          validate_tx_rollup_submit_batch vi remaining_gas content
      | Tx_rollup_commit _ | Tx_rollup_return_bond _
      | Tx_rollup_finalize_commitment _ | Tx_rollup_remove_commitment _ ->
          assert_tx_rollup_feature_enabled vi
      | Transfer_ticket {contents; ty; _} ->
          let* remaining_gas = consume_decoding_gas remaining_gas contents in
          let* (_ : Gas.Arith.fp) = consume_decoding_gas remaining_gas ty in
          return_unit
      | Tx_rollup_dispatch_tickets _ ->
          validate_tx_rollup_dispatch_tickets vi remaining_gas operation
      | Tx_rollup_rejection _ -> validate_tx_rollup_rejection vi operation
      | Sc_rollup_originate {kind; _} ->
          let* () = assert_sc_rollup_feature_enabled vi in
          assert_pvm_kind_enabled vi kind
      | Sc_rollup_cement _ | Sc_rollup_publish _ | Sc_rollup_refute _
      | Sc_rollup_timeout _ | Sc_rollup_execute_outbox_message _ ->
          assert_sc_rollup_feature_enabled vi
      | Sc_rollup_add_messages {messages; _} ->
          let* () = assert_sc_rollup_feature_enabled vi in
          assert_not_zero_messages messages
      | Sc_rollup_recover_bond _ ->
          (* TODO: https://gitlab.com/tezos/tezos/-/issues/3063
             Should we successfully precheck Sc_rollup_recover_bond and any
             (simple) Sc rollup operation, or should we add some some checks to make
             the operations Branch_delayed if they cannot be successfully
             prechecked? *)
          assert_sc_rollup_feature_enabled vi
      | Dal_publish_slot_header slot_header ->
          Dal_apply.validate_publish_slot_header vi.ctxt slot_header
      | Zk_rollup_origination _ | Zk_rollup_publish _ | Zk_rollup_update _ ->
          assert_zk_rollup_feature_enabled vi
    in
    (* Gas should no longer be consumed below this point, because it
       would not take into account any gas consumed during the pattern
       matching right above. If you really need to consume gas here, then you
       must make this pattern matching return the [remaining_gas].*)
    let* balance, is_allocated =
      Contract.simulate_spending
        vi.ctxt
        ~balance:batch_state.balance
        ~amount:fee
        source
    in
    return {total_gas_used; balance; is_allocated}

  (** This would be [fold_left_es (check_contents vi) batch_state
      contents_list] if [contents_list] were an ordinary [list]. *)
  let rec check_contents_list :
      type kind.
      info ->
      batch_state ->
      kind Kind.manager contents_list ->
      Gas.Arith.fp ->
      Gas.Arith.fp tzresult Lwt.t =
   fun vi batch_state contents_list remaining_gas ->
    let open Lwt_result_syntax in
    match contents_list with
    | Single contents ->
        let* batch_state =
          check_contents vi batch_state contents remaining_gas
        in
        return batch_state.total_gas_used
    | Cons (contents, tail) ->
        let* batch_state =
          check_contents vi batch_state contents remaining_gas
        in
        check_contents_list vi batch_state tail remaining_gas

  let check_manager_operation vi ~check_signature
      (operation : _ Kind.manager operation) remaining_block_gas =
    let open Lwt_result_syntax in
    let contents_list = operation.protocol_data.contents in
    let* batch_state, source_pk =
      check_sanity_and_find_public_key vi contents_list
    in
    let* gas_used =
      check_contents_list vi batch_state contents_list remaining_block_gas
    in
    let*? () =
      if check_signature then
        Operation.check_signature source_pk vi.chain_id operation
      else ok_unit
    in
    return gas_used

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
    | Error conflict -> error (Manager_restriction {source; conflict})

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
    let* gas_used =
      check_manager_operation
        info
        ~check_signature
        operation
        block_state.remaining_block_gas
    in
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

let init_validation_state ctxt mode chain_id all_expected_consensus_features
    ~predecessor_level =
  let info = init_info ctxt mode chain_id all_expected_consensus_features in
  let operation_state = init_operation_conflict_state ~predecessor_level in
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
    Consensus.check_frozen_deposits_are_positive ctxt block_producer.delegate
  in
  let* ctxt, _slot, payload_producer =
    Stake_distribution.baking_rights_owner
      ctxt
      current_level
      ~round:block_header.protocol_data.contents.payload_round
  in
  let payload_hash = block_header.protocol_data.contents.payload_hash in
  let predecessor_hash = block_header.shell.predecessor in
  let block_finalization_info =
    {
      fitness;
      block_producer;
      payload_producer;
      predecessor_hash;
      block_data_contents = block_header.protocol_data.contents;
    }
  in
  let mode =
    if is_partial then Partial_validation block_finalization_info
    else Application block_finalization_info
  in
  let all_expected_consensus_features =
    Consensus.expected_features_for_application
      ctxt
      fitness
      payload_hash
      ~predecessor_level
      ~predecessor_round
      ~predecessor_hash
  in
  let predecessor_level = predecessor_level.level in
  return
    (init_validation_state
       ctxt
       mode
       chain_id
       all_expected_consensus_features
       ~predecessor_level)

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
    Consensus.check_frozen_deposits_are_positive ctxt block_producer.delegate
  in
  let* ctxt, _slot, payload_producer =
    Stake_distribution.baking_rights_owner
      ctxt
      current_level
      ~round:header_contents.payload_round
  in
  let all_expected_consensus_features =
    Consensus.expected_features_for_construction
      ctxt
      round
      header_contents.payload_hash
      ~predecessor_level
      ~predecessor_round
      ~predecessor_hash
  in
  let predecessor_level = predecessor_level.level in
  let validation_state =
    init_validation_state
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
      ~predecessor_level
  in
  return validation_state

let begin_partial_construction ctxt chain_id ~predecessor_level
    ~predecessor_round ~grandparent_round =
  let all_expected_consensus_features =
    Consensus.expected_features_for_partial_construction
      ctxt
      ~predecessor_level
      ~predecessor_round
      ~grandparent_round
  in
  let predecessor_level = predecessor_level.level in
  let validation_state =
    init_validation_state
      ctxt
      Mempool
      chain_id
      all_expected_consensus_features
      ~predecessor_level
  in
  validation_state

let begin_no_predecessor_info ctxt chain_id =
  let all_expected_consensus_features =
    {
      expected_preendorsement =
        No_predecessor_info_cannot_validate_preendorsement;
      expected_endorsement = No_predecessor_info_cannot_validate_endorsement;
      expected_grandparent_endorsement_for_partial_construction = None;
    }
  in
  let current_level = Level.current ctxt in
  let predecessor_level =
    match Raw_level.pred current_level.level with
    | None -> current_level.level
    | Some level -> level
  in
  init_validation_state
    ctxt
    Mempool
    chain_id
    all_expected_consensus_features
    ~predecessor_level

let check_operation ?(check_signature = true) info (type kind)
    (operation : kind operation) : unit tzresult Lwt.t =
  let open Lwt_result_syntax in
  match operation.protocol_data.contents with
  | Single (Preendorsement _) ->
      let* (_voting_power : int) =
        Consensus.check_preendorsement info ~check_signature operation
      in
      return_unit
  | Single (Endorsement _) ->
      let* (_kind : Consensus.endorsement_kind) =
        Consensus.check_endorsement info ~check_signature operation
      in
      return_unit
  | Single (Dal_attestation _) -> Consensus.check_dal_attestation info operation
  | Single (Proposals _) ->
      Voting.check_proposals info ~check_signature operation
  | Single (Ballot _) -> Voting.check_ballot info ~check_signature operation
  | Single (Activate_account _) ->
      Anonymous.check_activate_account info operation
  | Single (Double_preendorsement_evidence _) ->
      Anonymous.check_double_preendorsement_evidence info operation
  | Single (Double_endorsement_evidence _) ->
      Anonymous.check_double_endorsement_evidence info operation
  | Single (Double_baking_evidence _) ->
      Anonymous.check_double_baking_evidence info operation
  | Single (Drain_delegate _) ->
      Anonymous.check_drain_delegate info ~check_signature operation
  | Single (Seed_nonce_revelation _) ->
      Anonymous.check_seed_nonce_revelation info operation
  | Single (Vdf_revelation _) -> Anonymous.check_vdf_revelation info operation
  | Single (Manager_operation _) ->
      let remaining_gas =
        Gas.Arith.fp (Constants.hard_gas_limit_per_block info.ctxt)
      in
      let* (_remaining_gas : Gas.Arith.fp) =
        Manager.check_manager_operation
          info
          ~check_signature
          operation
          remaining_gas
      in
      return_unit
  | Cons (Manager_operation _, _) ->
      let remaining_gas =
        Gas.Arith.fp (Constants.hard_gas_limit_per_block info.ctxt)
      in
      let* (_remaining_gas : Gas.Arith.fp) =
        Manager.check_manager_operation
          info
          ~check_signature
          operation
          remaining_gas
      in
      return_unit
  | Single (Failing_noop _) -> tzfail Validate_errors.Failing_noop_error

let check_operation_conflict (type kind) operation_conflict_state oph
    (operation : kind operation) =
  match operation.protocol_data.contents with
  | Single (Preendorsement _) ->
      Consensus.check_preendorsement_conflict
        operation_conflict_state
        oph
        operation
  | Single (Endorsement _) ->
      Consensus.check_endorsement_conflict
        operation_conflict_state
        oph
        operation
  | Single (Dal_attestation _) ->
      Consensus.check_dal_attestation_conflict
        operation_conflict_state
        oph
        operation
  | Single (Proposals _) ->
      Voting.check_proposals_conflict operation_conflict_state oph operation
  | Single (Ballot _) ->
      Voting.check_ballot_conflict operation_conflict_state oph operation
  | Single (Activate_account _) ->
      Anonymous.check_activate_account_conflict
        operation_conflict_state
        oph
        operation
  | Single (Double_preendorsement_evidence _) ->
      Anonymous.check_double_preendorsement_evidence_conflict
        operation_conflict_state
        oph
        operation
  | Single (Double_endorsement_evidence _) ->
      Anonymous.check_double_endorsement_evidence_conflict
        operation_conflict_state
        oph
        operation
  | Single (Double_baking_evidence _) ->
      Anonymous.check_double_baking_evidence_conflict
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
  | Single (Preendorsement _) ->
      Consensus.add_preendorsement operation_conflict_state oph operation
  | Single (Endorsement consensus_content) ->
      let endorsement_kind =
        if
          Consensus.is_normal_endorsement_assuming_valid
            operation_conflict_state
            consensus_content
        then Consensus.Normal_endorsement 0
        else Grandparent_endorsement
      in
      Consensus.add_endorsement
        operation_conflict_state
        oph
        operation
        endorsement_kind
  | Single (Dal_attestation _) ->
      Consensus.add_dal_attestation operation_conflict_state oph operation
  | Single (Proposals _) ->
      Voting.add_proposals operation_conflict_state oph operation
  | Single (Ballot _) ->
      Voting.add_ballot operation_conflict_state oph operation
  | Single (Activate_account _) ->
      Anonymous.add_activate_account operation_conflict_state oph operation
  | Single (Double_preendorsement_evidence _) ->
      Anonymous.add_double_preendorsement_evidence
        operation_conflict_state
        oph
        operation
  | Single (Double_endorsement_evidence _) ->
      Anonymous.add_double_endorsement_evidence
        operation_conflict_state
        oph
        operation
  | Single (Double_baking_evidence _) ->
      Anonymous.add_double_baking_evidence
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
  | Single (Preendorsement _) ->
      Consensus.remove_preendorsement operation_conflict_state operation
  | Single (Endorsement _) ->
      Consensus.remove_endorsement operation_conflict_state operation
  | Single (Dal_attestation _) ->
      Consensus.remove_dal_attestation operation_conflict_state operation
  | Single (Proposals _) ->
      Voting.remove_proposals operation_conflict_state operation
  | Single (Ballot _) -> Voting.remove_ballot operation_conflict_state operation
  | Single (Activate_account _) ->
      Anonymous.remove_activate_account operation_conflict_state operation
  | Single (Double_preendorsement_evidence _) ->
      Anonymous.remove_double_preendorsement_evidence
        operation_conflict_state
        operation
  | Single (Double_endorsement_evidence _) ->
      Anonymous.remove_double_endorsement_evidence
        operation_conflict_state
        operation
  | Single (Double_baking_evidence _) ->
      Anonymous.remove_double_baking_evidence operation_conflict_state operation
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
      (* Do not validate non-consensus operation in [Partial_validation] mode *)
      return {info; operation_state; block_state}
  | Partial_validation _, _ | Mempool, _ | Construction _, _ | Application _, _
    -> (
      match operation.protocol_data.contents with
      | Single (Preendorsement _) ->
          Consensus.validate_preendorsement
            ~check_signature
            info
            operation_state
            block_state
            oph
            operation
      | Single (Endorsement _) ->
          Consensus.validate_endorsement
            ~check_signature
            info
            operation_state
            block_state
            oph
            operation
      | Single (Dal_attestation _) ->
          let open Consensus in
          let* () = check_dal_attestation info operation in
          let*? () =
            check_dal_attestation_conflict operation_state oph operation
            |> wrap_dal_attestation_conflict
          in
          let operation_state =
            add_dal_attestation operation_state oph operation
          in
          return {info; operation_state; block_state}
      | Single (Proposals _) ->
          let open Voting in
          let* () = check_proposals info ~check_signature operation in
          let*? () =
            check_proposals_conflict operation_state oph operation
            |> wrap_proposals_conflict
          in
          let operation_state = add_proposals operation_state oph operation in
          return {info; operation_state; block_state}
      | Single (Ballot _) ->
          let open Voting in
          let* () = check_ballot info ~check_signature operation in
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
      | Single (Double_preendorsement_evidence _) ->
          let open Anonymous in
          let* () = check_double_preendorsement_evidence info operation in
          let*? () =
            check_double_preendorsement_evidence_conflict
              operation_state
              oph
              operation
            |> wrap_denunciation_conflict Preendorsement
          in
          let operation_state =
            add_double_preendorsement_evidence operation_state oph operation
          in
          return {info; operation_state; block_state}
      | Single (Double_endorsement_evidence _) ->
          let open Anonymous in
          let* () = check_double_endorsement_evidence info operation in
          let*? () =
            check_double_endorsement_evidence_conflict
              operation_state
              oph
              operation
            |> wrap_denunciation_conflict Endorsement
          in
          let operation_state =
            add_double_endorsement_evidence operation_state oph operation
          in
          return {info; operation_state; block_state}
      | Single (Double_baking_evidence _) ->
          let open Anonymous in
          let* () = check_double_baking_evidence info operation in
          let*? () =
            check_double_baking_evidence_conflict operation_state oph operation
            |> wrap_denunciation_conflict Block
          in
          let operation_state =
            add_double_baking_evidence operation_state oph operation
          in
          return {info; operation_state; block_state}
      | Single (Drain_delegate _) ->
          let open Anonymous in
          let* () = check_drain_delegate info ~check_signature operation in
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

let check_endorsement_power vi bs =
  let required = Constants.consensus_threshold vi.ctxt in
  let provided = bs.endorsement_power in
  error_unless
    Compare.Int.(provided >= required)
    (Validate_errors.Block.Not_enough_endorsements {required; provided})

let finalize_validate_block_header vi vs checkable_payload_hash
    (block_header_contents : Block_header.contents) round fitness =
  let locked_round_evidence =
    Option.map
      (fun (preendorsement_round, preendorsement_count) ->
        Block_header.{preendorsement_round; preendorsement_count})
      vs.locked_round_evidence
  in
  Block_header.finalize_validate_block_header
    ~block_header_contents
    ~round
    ~fitness
    ~checkable_payload_hash
    ~locked_round_evidence
    ~consensus_threshold:(Constants.consensus_threshold vi.ctxt)

let compute_payload_hash block_state
    (block_header_contents : Block_header.contents) ~predecessor_hash =
  Block_payload.hash
    ~predecessor_hash
    ~payload_round:block_header_contents.payload_round
    (List.rev block_state.recorded_operations_rev)

let finalize_block {info; block_state; _} =
  let open Lwt_result_syntax in
  match info.mode with
  | Application {fitness; predecessor_hash; block_data_contents; _} ->
      let* are_endorsements_required = are_endorsements_required info in
      let*? () =
        if are_endorsements_required then
          check_endorsement_power info block_state
        else ok_unit
      in
      let block_payload_hash =
        compute_payload_hash block_state block_data_contents ~predecessor_hash
      in
      let round = Fitness.round fitness in
      let*? () =
        finalize_validate_block_header
          info
          block_state
          (Block_header.Expected_payload_hash block_payload_hash)
          block_data_contents
          round
          fitness
      in
      return_unit
  | Partial_validation _ ->
      let* are_endorsements_required = are_endorsements_required info in
      let*? () =
        if are_endorsements_required then
          check_endorsement_power info block_state
        else ok_unit
      in
      return_unit
  | Construction
      {predecessor_round; predecessor_hash; round; block_data_contents; _} ->
      let block_payload_hash =
        compute_payload_hash block_state block_data_contents ~predecessor_hash
      in
      let locked_round_evidence = block_state.locked_round_evidence in
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
        if are_endorsements_required then
          check_endorsement_power info block_state
        else ok_unit
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
          block_state
          checkable_payload_hash
          block_data_contents
          round
          fitness
      in
      return_unit
  | Mempool ->
      (* Nothing to do for the mempool mode*)
      return_unit
