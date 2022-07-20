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

module Consensus = struct
  type error += Zero_frozen_deposits of Signature.Public_key_hash.t

  let () =
    register_error_kind
      `Permanent
      ~id:"validate.zero_frozen_deposits"
      ~title:"Zero frozen deposits"
      ~description:"The delegate has zero frozen deposits."
      ~pp:(fun ppf delegate ->
        Format.fprintf
          ppf
          "Delegate %a has zero frozen deposits; it is not allowed to \
           bake/preendorse/endorse."
          Signature.Public_key_hash.pp
          delegate)
      Data_encoding.(obj1 (req "delegate" Signature.Public_key_hash.encoding))
      (function Zero_frozen_deposits delegate -> Some delegate | _ -> None)
      (fun delegate -> Zero_frozen_deposits delegate)

  (** This type is only used in consensus operation errors to make
      them more informative. *)
  type consensus_operation_kind =
    | Preendorsement
    | Endorsement
    | Grandparent_endorsement

  let consensus_operation_kind_encoding =
    Data_encoding.string_enum
      [
        ("Preendorsement", Preendorsement);
        ("Endorsement", Endorsement);
        ("Grandparent_endorsement", Grandparent_endorsement);
      ]

  let consensus_operation_kind_pp fmt = function
    | Preendorsement -> Format.fprintf fmt "Preendorsement"
    | Endorsement -> Format.fprintf fmt "Endorsement"
    | Grandparent_endorsement -> Format.fprintf fmt "Grandparent endorsement"

  (** Errors for preendorsements and endorsements. *)
  type error +=
    | Consensus_operation_for_old_level of {
        kind : consensus_operation_kind;
        expected : Raw_level.t;
        provided : Raw_level.t;
      }
    | Consensus_operation_for_future_level of {
        kind : consensus_operation_kind;
        expected : Raw_level.t;
        provided : Raw_level.t;
      }
    | Consensus_operation_for_old_round of {
        kind : consensus_operation_kind;
        expected : Round.t;
        provided : Round.t;
      }
    | Consensus_operation_for_future_round of {
        kind : consensus_operation_kind;
        expected : Round.t;
        provided : Round.t;
      }
    | Wrong_consensus_operation_branch of {
        kind : consensus_operation_kind;
        expected : Block_hash.t;
        provided : Block_hash.t;
      }
    | Wrong_payload_hash_for_consensus_operation of {
        kind : consensus_operation_kind;
        expected : Block_payload_hash.t;
        provided : Block_payload_hash.t;
      }
    | Unexpected_preendorsement_in_block
    | Unexpected_endorsement_in_block
    | Preendorsement_round_too_high of {
        block_round : Round.t;
        provided : Round.t;
      }
    | Wrong_slot_used_for_consensus_operation of {
        kind : consensus_operation_kind;
      }
    | Conflicting_consensus_operation of {kind : consensus_operation_kind}
    | Consensus_operation_not_allowed

  let () =
    register_error_kind
      `Outdated
      ~id:"validate.consensus_operation_for_old_level"
      ~title:"Consensus operation for old level"
      ~description:"Consensus operation for old level."
      ~pp:(fun ppf (kind, expected, provided) ->
        Format.fprintf
          ppf
          "%a for old level (expected: %a, provided: %a)."
          consensus_operation_kind_pp
          kind
          Raw_level.pp
          expected
          Raw_level.pp
          provided)
      Data_encoding.(
        obj3
          (req "kind" consensus_operation_kind_encoding)
          (req "expected" Raw_level.encoding)
          (req "provided" Raw_level.encoding))
      (function
        | Consensus_operation_for_old_level {kind; expected; provided} ->
            Some (kind, expected, provided)
        | _ -> None)
      (fun (kind, expected, provided) ->
        Consensus_operation_for_old_level {kind; expected; provided}) ;
    register_error_kind
      `Temporary
      ~id:"validate.consensus_operation_for_future_level"
      ~title:"Consensus operation for future level"
      ~description:"Consensus operation for future level."
      ~pp:(fun ppf (kind, expected, provided) ->
        Format.fprintf
          ppf
          "%a for future level (expected: %a, provided: %a)."
          consensus_operation_kind_pp
          kind
          Raw_level.pp
          expected
          Raw_level.pp
          provided)
      Data_encoding.(
        obj3
          (req "kind" consensus_operation_kind_encoding)
          (req "expected" Raw_level.encoding)
          (req "provided" Raw_level.encoding))
      (function
        | Consensus_operation_for_future_level {kind; expected; provided} ->
            Some (kind, expected, provided)
        | _ -> None)
      (fun (kind, expected, provided) ->
        Consensus_operation_for_future_level {kind; expected; provided}) ;
    register_error_kind
      `Branch
      ~id:"validate.consensus_operation_for_old_round"
      ~title:"Consensus operation for old round"
      ~description:"Consensus operation for old round."
      ~pp:(fun ppf (kind, expected, provided) ->
        Format.fprintf
          ppf
          "%a for old round (expected_min: %a, provided: %a)."
          consensus_operation_kind_pp
          kind
          Round.pp
          expected
          Round.pp
          provided)
      Data_encoding.(
        obj3
          (req "kind" consensus_operation_kind_encoding)
          (req "expected_min" Round.encoding)
          (req "provided" Round.encoding))
      (function
        | Consensus_operation_for_old_round {kind; expected; provided} ->
            Some (kind, expected, provided)
        | _ -> None)
      (fun (kind, expected, provided) ->
        Consensus_operation_for_old_round {kind; expected; provided}) ;
    register_error_kind
      `Temporary
      ~id:"validate.consensus_operation_for_future_round"
      ~title:"Consensus operation for future round"
      ~description:"Consensus operation for future round."
      ~pp:(fun ppf (kind, expected, provided) ->
        Format.fprintf
          ppf
          "%a for future round (expected: %a, provided: %a)."
          consensus_operation_kind_pp
          kind
          Round.pp
          expected
          Round.pp
          provided)
      Data_encoding.(
        obj3
          (req "kind" consensus_operation_kind_encoding)
          (req "expected_max" Round.encoding)
          (req "provided" Round.encoding))
      (function
        | Consensus_operation_for_future_round {kind; expected; provided} ->
            Some (kind, expected, provided)
        | _ -> None)
      (fun (kind, expected, provided) ->
        Consensus_operation_for_future_round {kind; expected; provided}) ;
    register_error_kind
      `Temporary
      ~id:"validate.wrong_consensus_operation_branch"
      ~title:"Wrong consensus operation branch"
      ~description:
        "Trying to include an endorsement or preendorsement which points to \
         the wrong block. It should be the predecessor for preendorsements and \
         the grandfather for endorsements."
      ~pp:(fun ppf (kind, expected, provided) ->
        Format.fprintf
          ppf
          "%a with wrong branch (expected: %a, provided: %a)."
          consensus_operation_kind_pp
          kind
          Block_hash.pp
          expected
          Block_hash.pp
          provided)
      Data_encoding.(
        obj3
          (req "kind" consensus_operation_kind_encoding)
          (req "expected" Block_hash.encoding)
          (req "provided" Block_hash.encoding))
      (function
        | Wrong_consensus_operation_branch {kind; expected; provided} ->
            Some (kind, expected, provided)
        | _ -> None)
      (fun (kind, expected, provided) ->
        Wrong_consensus_operation_branch {kind; expected; provided}) ;
    register_error_kind
      (* Note: in Mempool mode this used to be
         Consensus_operation_on_competing_proposal (which was
         [`Branch] so we kept this classification). *)
      `Branch
      ~id:"validate.wrong_payload_hash_for_consensus_operation"
      ~title:"Wrong payload hash for consensus operation"
      ~description:"Wrong payload hash for consensus operation."
      ~pp:(fun ppf (kind, expected, provided) ->
        Format.fprintf
          ppf
          "%a with wrong payload hash (expected: %a, provided: %a)."
          consensus_operation_kind_pp
          kind
          Block_payload_hash.pp_short
          expected
          Block_payload_hash.pp_short
          provided)
      Data_encoding.(
        obj3
          (req "kind" consensus_operation_kind_encoding)
          (req "expected" Block_payload_hash.encoding)
          (req "provided" Block_payload_hash.encoding))
      (function
        | Wrong_payload_hash_for_consensus_operation {kind; expected; provided}
          ->
            Some (kind, expected, provided)
        | _ -> None)
      (fun (kind, expected, provided) ->
        Wrong_payload_hash_for_consensus_operation {kind; expected; provided}) ;
    register_error_kind
      `Permanent
      ~id:"validate.unexpected_preendorsement_in_block"
      ~title:"Unexpected preendorsement in block"
      ~description:"Unexpected preendorsement in block."
      ~pp:(fun ppf () ->
        Format.fprintf ppf "Unexpected preendorsement in block.")
      Data_encoding.empty
      (function Unexpected_preendorsement_in_block -> Some () | _ -> None)
      (fun () -> Unexpected_preendorsement_in_block) ;
    register_error_kind
      `Permanent
      ~id:"validate.unexpected_endorsement_in_block"
      ~title:"Unexpected endorsement in block"
      ~description:"Unexpected endorsement in block."
      ~pp:(fun ppf () -> Format.fprintf ppf "Unexpected endorsement in block.")
      Data_encoding.empty
      (function Unexpected_endorsement_in_block -> Some () | _ -> None)
      (fun () -> Unexpected_endorsement_in_block) ;
    register_error_kind
      `Permanent
      ~id:"validate.preendorsement_round_too_high"
      ~title:"Preendorsement round too high"
      ~description:"Preendorsement round too high."
      ~pp:(fun ppf (block_round, provided) ->
        Format.fprintf
          ppf
          "Preendorsement round too high (block_round: %a, provided: %a)."
          Round.pp
          block_round
          Round.pp
          provided)
      Data_encoding.(
        obj2 (req "block_round" Round.encoding) (req "provided" Round.encoding))
      (function
        | Preendorsement_round_too_high {block_round; provided} ->
            Some (block_round, provided)
        | _ -> None)
      (fun (block_round, provided) ->
        Preendorsement_round_too_high {block_round; provided}) ;
    register_error_kind
      `Permanent
      ~id:"validate.wrong_slot_for_consensus_operation"
      ~title:"Wrong slot for consensus operation"
      ~description:"Wrong slot used for a preendorsement or endorsement."
      ~pp:(fun ppf kind ->
        Format.fprintf
          ppf
          "Wrong slot used for a %a."
          consensus_operation_kind_pp
          kind)
      Data_encoding.(obj1 (req "kind" consensus_operation_kind_encoding))
      (function
        | Wrong_slot_used_for_consensus_operation {kind} -> Some kind
        | _ -> None)
      (fun kind -> Wrong_slot_used_for_consensus_operation {kind}) ;
    register_error_kind
      `Branch
      ~id:"validate.double_inclusion_of_consensus_operation"
      ~title:"Double inclusion of consensus operation"
      ~description:"Double inclusion of consensus operation."
      ~pp:(fun ppf kind ->
        Format.fprintf
          ppf
          "Double inclusion of %a operation"
          consensus_operation_kind_pp
          kind)
      Data_encoding.(obj1 (req "kind" consensus_operation_kind_encoding))
      (function
        | Conflicting_consensus_operation {kind} -> Some kind | _ -> None)
      (fun kind -> Conflicting_consensus_operation {kind}) ;
    register_error_kind
      `Branch
      ~id:"validate.consensus_operation_not_allowed"
      ~title:"Consensus operation not allowed"
      ~description:"Consensus operation not allowed."
      ~pp:(fun ppf () ->
        Format.fprintf ppf "Validation of consensus operation if forbidden ")
      Data_encoding.empty
      (function Consensus_operation_not_allowed -> Some () | _ -> None)
      (fun () -> Consensus_operation_not_allowed)
end

module Anonymous = struct
  type error +=
    | Invalid_activation of {pkh : Ed25519.Public_key_hash.t}
    | Conflicting_activation of Ed25519.Public_key_hash.t * Operation_hash.t

  let () =
    register_error_kind
      `Permanent
      ~id:"validate_operation.invalid_activation"
      ~title:"Invalid activation"
      ~description:
        "The given key and secret do not correspond to any existing \
         preallocated contract."
      ~pp:(fun ppf pkh ->
        Format.fprintf
          ppf
          "Invalid activation. The public key %a and accompanying secret do \
           not match any commitment."
          Ed25519.Public_key_hash.pp
          pkh)
      Data_encoding.(obj1 (req "pkh" Ed25519.Public_key_hash.encoding))
      (function Invalid_activation {pkh} -> Some pkh | _ -> None)
      (fun pkh -> Invalid_activation {pkh}) ;
    register_error_kind
      `Branch
      ~id:"validate_operation.conflicting_activation"
      ~title:"Account already activated in current validation_state"
      ~description:
        "The account has already been activated by a previous operation in the \
         current validation state."
      ~pp:(fun ppf (edpkh, oph) ->
        Format.fprintf
          ppf
          "Invalid activation: the account %a has already been activated in \
           the current validation state by operation %a."
          Ed25519.Public_key_hash.pp
          edpkh
          Operation_hash.pp
          oph)
      Data_encoding.(
        obj2
          (req "account_edpkh" Ed25519.Public_key_hash.encoding)
          (req "conflicting_op_hash" Operation_hash.encoding))
      (function
        | Conflicting_activation (edpkh, oph) -> Some (edpkh, oph) | _ -> None)
      (fun (edpkh, oph) -> Conflicting_activation (edpkh, oph))

  type denunciation_kind = Preendorsement | Endorsement | Block

  let denunciation_kind_encoding =
    let open Data_encoding in
    string_enum
      [
        ("preendorsement", Preendorsement);
        ("endorsement", Endorsement);
        ("block", Block);
      ]

  let pp_denunciation_kind fmt : denunciation_kind -> unit = function
    | Preendorsement -> Format.fprintf fmt "preendorsement"
    | Endorsement -> Format.fprintf fmt "endorsement"
    | Block -> Format.fprintf fmt "block"

  type error +=
    | Invalid_double_baking_evidence of {
        hash1 : Block_hash.t;
        level1 : Raw_level.t;
        round1 : Round.t;
        hash2 : Block_hash.t;
        level2 : Raw_level.t;
        round2 : Round.t;
      }
    | Invalid_denunciation of denunciation_kind
    | Inconsistent_denunciation of {
        kind : denunciation_kind;
        delegate1 : Signature.Public_key_hash.t;
        delegate2 : Signature.Public_key_hash.t;
      }
    | Already_denounced of {
        kind : denunciation_kind;
        delegate : Signature.Public_key_hash.t;
        level : Level.t;
      }
    | Conflicting_denunciation of {
        kind : denunciation_kind;
        delegate : Signature.Public_key_hash.t;
        level : Level.t;
        hash : Operation_hash.t;
      }
    | Too_early_denunciation of {
        kind : denunciation_kind;
        level : Raw_level.t;
        current : Raw_level.t;
      }
    | Outdated_denunciation of {
        kind : denunciation_kind;
        level : Raw_level.t;
        last_cycle : Cycle.t;
      }

  let () =
    register_error_kind
      `Permanent
      ~id:"validate.block.invalid_double_baking_evidence"
      ~title:"Invalid double baking evidence"
      ~description:
        "A double-baking evidence is inconsistent (two distinct levels)"
      ~pp:(fun ppf (hash1, level1, round1, hash2, level2, round2) ->
        Format.fprintf
          ppf
          "Invalid double-baking evidence (hash: %a and %a, levels/rounds: \
           (%ld,%ld) and (%ld,%ld))"
          Block_hash.pp
          hash1
          Block_hash.pp
          hash2
          (Raw_level.to_int32 level1)
          (Round.to_int32 round1)
          (Raw_level.to_int32 level2)
          (Round.to_int32 round2))
      Data_encoding.(
        obj6
          (req "hash1" Block_hash.encoding)
          (req "level1" Raw_level.encoding)
          (req "round1" Round.encoding)
          (req "hash2" Block_hash.encoding)
          (req "level2" Raw_level.encoding)
          (req "round2" Round.encoding))
      (function
        | Invalid_double_baking_evidence
            {hash1; level1; round1; hash2; level2; round2} ->
            Some (hash1, level1, round1, hash2, level2, round2)
        | _ -> None)
      (fun (hash1, level1, round1, hash2, level2, round2) ->
        Invalid_double_baking_evidence
          {hash1; level1; round1; hash2; level2; round2}) ;
    register_error_kind
      `Permanent
      ~id:"validate_operation.block.invalid_denunciation"
      ~title:"Invalid denunciation"
      ~description:"A denunciation is malformed"
      ~pp:(fun ppf kind ->
        Format.fprintf
          ppf
          "Malformed double-%a evidence"
          pp_denunciation_kind
          kind)
      Data_encoding.(obj1 (req "kind" denunciation_kind_encoding))
      (function Invalid_denunciation kind -> Some kind | _ -> None)
      (fun kind -> Invalid_denunciation kind) ;
    register_error_kind
      `Permanent
      ~id:"validate_operation.block.inconsistent_denunciation"
      ~title:"Inconsistent denunciation"
      ~description:
        "A denunciation operation is inconsistent (two distinct delegates)"
      ~pp:(fun ppf (kind, delegate1, delegate2) ->
        Format.fprintf
          ppf
          "Inconsistent double-%a evidence (distinct delegate: %a and %a)"
          pp_denunciation_kind
          kind
          Signature.Public_key_hash.pp_short
          delegate1
          Signature.Public_key_hash.pp_short
          delegate2)
      Data_encoding.(
        obj3
          (req "kind" denunciation_kind_encoding)
          (req "delegate1" Signature.Public_key_hash.encoding)
          (req "delegate2" Signature.Public_key_hash.encoding))
      (function
        | Inconsistent_denunciation {kind; delegate1; delegate2} ->
            Some (kind, delegate1, delegate2)
        | _ -> None)
      (fun (kind, delegate1, delegate2) ->
        Inconsistent_denunciation {kind; delegate1; delegate2}) ;
    register_error_kind
      `Branch
      ~id:"validate_operation.already_denounced"
      ~title:"Already denounced"
      ~description:"The same denunciation has already been validated."
      ~pp:(fun ppf (kind, delegate, level) ->
        Format.fprintf
          ppf
          "Delegate %a at level %a has already been denounced for a double %a."
          pp_denunciation_kind
          kind
          Signature.Public_key_hash.pp
          delegate
          Level.pp
          level)
      Data_encoding.(
        obj3
          (req "denunciation_kind" denunciation_kind_encoding)
          (req "delegate" Signature.Public_key_hash.encoding)
          (req "level" Level.encoding))
      (function
        | Already_denounced {kind; delegate; level} ->
            Some (kind, delegate, level)
        | _ -> None)
      (fun (kind, delegate, level) -> Already_denounced {kind; delegate; level}) ;
    register_error_kind
      `Branch
      ~id:"validate_operation.conflicting_denunciation"
      ~title:"Conflicting denunciation in current validation state"
      ~description:
        "The same denunciation has already been validated in the current \
         validation state."
      ~pp:(fun ppf (kind, delegate, level, hash) ->
        Format.fprintf
          ppf
          "Double %a evidence for the delegate %a at level %a already exists \
           in the current validation state as operation %a."
          pp_denunciation_kind
          kind
          Signature.Public_key_hash.pp
          delegate
          Level.pp
          level
          Operation_hash.pp
          hash)
      Data_encoding.(
        obj4
          (req "denunciation_kind" denunciation_kind_encoding)
          (req "delegate" Signature.Public_key_hash.encoding)
          (req "level" Level.encoding)
          (req "hash" Operation_hash.encoding))
      (function
        | Conflicting_denunciation {kind; delegate; level; hash} ->
            Some (kind, delegate, level, hash)
        | _ -> None)
      (fun (kind, delegate, level, hash) ->
        Conflicting_denunciation {kind; delegate; level; hash}) ;
    register_error_kind
      `Temporary
      ~id:"validate_operation.block.too_early_denunciation"
      ~title:"Too early denunciation"
      ~description:"A denunciation is too far in the future"
      ~pp:(fun ppf (kind, level, current) ->
        Format.fprintf
          ppf
          "A double-%a denunciation is too far in the future (current level: \
           %a, given level: %a)"
          pp_denunciation_kind
          kind
          Raw_level.pp
          current
          Raw_level.pp
          level)
      Data_encoding.(
        obj3
          (req "kind" denunciation_kind_encoding)
          (req "level" Raw_level.encoding)
          (req "current" Raw_level.encoding))
      (function
        | Too_early_denunciation {kind; level; current} ->
            Some (kind, level, current)
        | _ -> None)
      (fun (kind, level, current) ->
        Too_early_denunciation {kind; level; current}) ;
    register_error_kind
      `Permanent
      ~id:"validate_operation.block.outdated_denunciation"
      ~title:"Outdated denunciation"
      ~description:"A denunciation is outdated."
      ~pp:(fun ppf (kind, level, last_cycle) ->
        Format.fprintf
          ppf
          "A double-%a denunciation is outdated (last acceptable cycle: %a, \
           given level: %a)."
          pp_denunciation_kind
          kind
          Cycle.pp
          last_cycle
          Raw_level.pp
          level)
      Data_encoding.(
        obj3
          (req "kind" denunciation_kind_encoding)
          (req "level" Raw_level.encoding)
          (req "last" Cycle.encoding))
      (function
        | Outdated_denunciation {kind; level; last_cycle} ->
            Some (kind, level, last_cycle)
        | _ -> None)
      (fun (kind, level, last_cycle) ->
        Outdated_denunciation {kind; level; last_cycle})

  type error += Conflicting_nonce_revelation

  let () =
    register_error_kind
      `Branch
      ~id:"validate_operation.conflicting_nonce_revelation"
      ~title:"Conflicting nonce revelation in the current validation state)."
      ~description:
        "A revelation for the same nonce has already been validated for the \
         current validation state."
      ~pp:(fun ppf () ->
        Format.fprintf
          ppf
          "This nonce was previously revealed in the current block")
      Data_encoding.unit
      (function Conflicting_nonce_revelation -> Some () | _ -> None)
      (fun () -> Conflicting_nonce_revelation)
end

module Manager = struct
  type error +=
    | Manager_restriction of Signature.Public_key_hash.t * Operation_hash.t
    | Inconsistent_sources
    | Inconsistent_counters
    | Incorrect_reveal_position
    | Insufficient_gas_for_manager
    | Gas_quota_exceeded_init_deserialize
    | Tx_rollup_feature_disabled
    | Sc_rollup_feature_disabled

  let () =
    register_error_kind
      `Temporary
      ~id:"validate_operation.manager_restriction"
      ~title:"Manager restriction"
      ~description:
        "An operation with the same manager has already been validated in the \
         current block."
      ~pp:(fun ppf (d, hash) ->
        Format.fprintf
          ppf
          "Manager %a already has the operation %a in the current block."
          Signature.Public_key_hash.pp
          d
          Operation_hash.pp
          hash)
      Data_encoding.(
        obj2
          (req "manager" Signature.Public_key_hash.encoding)
          (req "hash" Operation_hash.encoding))
      (function
        | Manager_restriction (manager, hash) -> Some (manager, hash)
        | _ -> None)
      (fun (manager, hash) -> Manager_restriction (manager, hash)) ;
    let inconsistent_sources_description =
      "The operation batch includes operations from different sources."
    in
    register_error_kind
      `Permanent
      ~id:"validate_operation.inconsistent_sources"
      ~title:"Inconsistent sources in operation batch"
      ~description:inconsistent_sources_description
      ~pp:(fun ppf () ->
        Format.fprintf ppf "%s" inconsistent_sources_description)
      Data_encoding.empty
      (function Inconsistent_sources -> Some () | _ -> None)
      (fun () -> Inconsistent_sources) ;
    let inconsistent_counters_description =
      "Inconsistent counters in operation. Counters of an operation must be \
       successive."
    in
    register_error_kind
      `Permanent
      ~id:"validate_operation.inconsistent_counters"
      ~title:"Inconsistent counters in operation"
      ~description:inconsistent_counters_description
      ~pp:(fun ppf () ->
        Format.fprintf ppf "%s" inconsistent_counters_description)
      Data_encoding.empty
      (function Inconsistent_counters -> Some () | _ -> None)
      (fun () -> Inconsistent_counters) ;
    let incorrect_reveal_description =
      "Incorrect reveal operation position in batch: only allowed in first \
       position."
    in
    register_error_kind
      `Permanent
      ~id:"validate_operation.incorrect_reveal_position"
      ~title:"Incorrect reveal position"
      ~description:incorrect_reveal_description
      ~pp:(fun ppf () -> Format.fprintf ppf "%s" incorrect_reveal_description)
      Data_encoding.empty
      (function Incorrect_reveal_position -> Some () | _ -> None)
      (fun () -> Incorrect_reveal_position) ;
    register_error_kind
      `Permanent
      ~id:"validate_operation.insufficient_gas_for_manager"
      ~title:"Not enough gas for initial manager cost"
      ~description:
        (Format.asprintf
           "Gas limit is too low to cover the initial cost of manager \
            operations: at least %a gas required."
           Gas.pp_cost
           Michelson_v1_gas.Cost_of.manager_operation)
      Data_encoding.empty
      (function Insufficient_gas_for_manager -> Some () | _ -> None)
      (fun () -> Insufficient_gas_for_manager) ;
    let gas_deserialize_description =
      "Gas limit was not high enough to deserialize the transaction parameters \
       or origination script code or initial storage etc., making the \
       operation impossible to parse within the provided gas bounds."
    in
    register_error_kind
      `Permanent
      ~id:"validate_operation.gas_quota_exceeded_init_deserialize"
      ~title:"Not enough gas for initial deserialization of script expressions"
      ~description:gas_deserialize_description
      ~pp:(fun ppf () -> Format.fprintf ppf "%s" gas_deserialize_description)
      Data_encoding.empty
      (function Gas_quota_exceeded_init_deserialize -> Some () | _ -> None)
      (fun () -> Gas_quota_exceeded_init_deserialize) ;
    register_error_kind
      `Permanent
      ~id:"validate_operation.tx_rollup_is_disabled"
      ~title:"Tx rollup is disabled"
      ~description:"Cannot originate a tx rollup as it is disabled."
      ~pp:(fun ppf () ->
        Format.fprintf
          ppf
          "Cannot apply a tx rollup operation as it is disabled. This feature \
           will be enabled in a future proposal")
      Data_encoding.unit
      (function Tx_rollup_feature_disabled -> Some () | _ -> None)
      (fun () -> Tx_rollup_feature_disabled) ;
    let scoru_disabled_description =
      "Smart contract rollups will be enabled in a future proposal."
    in
    register_error_kind
      `Permanent
      ~id:"validate_operation.sc_rollup_disabled"
      ~title:"Smart contract rollups are disabled"
      ~description:scoru_disabled_description
      ~pp:(fun ppf () -> Format.fprintf ppf "%s" scoru_disabled_description)
      Data_encoding.unit
      (function Sc_rollup_feature_disabled -> Some () | _ -> None)
      (fun () -> Sc_rollup_feature_disabled)
end
