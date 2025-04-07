(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

type lost_result = Draw | Timeout | Conflict_resolved

let lost_result_to_string = function
  | Draw -> "draw"
  | Timeout -> "timeout"
  | Conflict_resolved -> "conflict resolved"

let lost_result_encoding =
  Data_encoding.string_enum
    (List.map
       (fun r -> (lost_result_to_string r, r))
       [Draw; Timeout; Conflict_resolved])

type error +=
  | Cannot_produce_proof of {inbox_level : int32; start_tick : Z.t}
  | Bad_minimal_fees of string
  | Disagree_with_cemented of {
      inbox_level : int32;
      ours : Commitment.Hash.t option;
      on_l1 : Commitment.Hash.t;
    }
  | Unreliable_tezos_node_returning_inconsistent_game
  | Wrong_initial_pvm_state of {
      initial_state_hash : State_hash.t;
      expected_state_hash : State_hash.t;
    }
  | Inconsistent_inbox of {
      layer1_inbox : Octez_smart_rollup.Inbox.t;
      inbox : Octez_smart_rollup.Inbox.t;
    }
  | Missing_PVM_state of Block_hash.t * Int32.t
  | Cannot_checkout_context of Block_hash.t * Smart_rollup_context_hash.t option
  | Cannot_checkout_l2_header
  | No_batcher
  | No_dal_injector
  | No_publisher
  | Refutation_player_failed_to_start
  | No_refutation_coordinator
  | Could_not_acquire_lock of string
  | Patch_durable_storage_on_commitment of int32
  | Dal_message_too_big of {slot_size : int; message_size : int}

type error += PVM_eval_too_many_ticks of {max_given : int64; executed : int64}

type error +=
  | Could_not_open_preimage_file of String.t
  | Could_not_encode_raw_data

type error +=
  | Lost_game of lost_result
  | Unparsable_boot_sector of {path : string}
  | Invalid_genesis_state of {
      expected : Commitment.Hash.t;
      actual : Commitment.Hash.t;
      actual_state_hash : State_hash.t;
    }

type error += Operator_not_in_whitelist

type error +=
  | Cannot_patch_pvm_of_public_rollup
  | Needs_apply_unsafe_flag of Pvm_patches.unsafe_patch list

type error += Operator_has_no_staked

type error += Exit_bond_recovered_bailout_mode

type error +=
  | Access_below_first_available_level of {
      first_available_level : int32;
      accessed_level : int32;
    }

type error +=
  | Unexpected_rollup of {
      rollup_address : Octez_smart_rollup.Address.t;
      saved_address : Octez_smart_rollup.Address.t;
    }

type error +=
  | Disagree_with_commitment of {
      our_commitment : Octez_smart_rollup.Commitment.t;
      their_commitment : Octez_smart_rollup.Commitment.t;
    }

type error +=
  | Outbox_level_not_cemented of {outbox_level : int32; lcc_level : int32}

let () =
  register_error_kind
    `Permanent
    ~id:"bad_minimal_fees_arg"
    ~title:"Bad -minimal-fees arg"
    ~description:"invalid fee threshold in -fee-threshold"
    ~pp:(fun ppf literal ->
      Format.fprintf ppf "invalid minimal fees '%s'" literal)
    Data_encoding.(obj1 (req "parameter" string))
    (function Bad_minimal_fees parameter -> Some parameter | _ -> None)
    (fun parameter -> Bad_minimal_fees parameter) ;

  register_error_kind
    `Permanent
    ~id:"internal.node_disagrees_with_cemented"
    ~title:"Internal error: The node disagrees with a cemented commitment on L1"
    ~description:
      "Internal error: The node disagrees with a cemented commitment on L1"
    ~pp:(fun ppf (inbox_level, ours, on_l1) ->
      Format.fprintf
        ppf
        "Internal error: The node has commitment %a for inbox level %ld but \
         this level is cemented on L1 with commitment %a"
        (Format.pp_print_option
           ~none:(fun ppf () -> Format.pp_print_string ppf "[None]")
           Commitment.Hash.pp)
        ours
        inbox_level
        Commitment.Hash.pp
        on_l1)
    Data_encoding.(
      obj3
        (req "inbox_level" int32)
        (req "ours" (option Commitment.Hash.encoding))
        (req "on_l1" Commitment.Hash.encoding))
    (function
      | Disagree_with_cemented {inbox_level; ours; on_l1} ->
          Some (inbox_level, ours, on_l1)
      | _ -> None)
    (fun (inbox_level, ours, on_l1) ->
      Disagree_with_cemented {inbox_level; ours; on_l1}) ;

  let description =
    "Internal error: The game invariant states that the dissection from the \
     opponent must contain a tick we disagree with. If the retrieved game does \
     not respect this, we cannot trust the Tezos node we are connected to and \
     prefer to stop here."
  in
  register_error_kind
    `Permanent
    ~id:"internal.unreliable_tezos_node"
    ~title:"Internal error: Tezos node seems unreliable"
    ~description
    ~pp:(fun ppf () ->
      Format.fprintf ppf "Unreliable Tezos node. %s" description)
    Data_encoding.unit
    (function
      | Unreliable_tezos_node_returning_inconsistent_game -> Some () | _ -> None)
    (fun () -> Unreliable_tezos_node_returning_inconsistent_game) ;

  register_error_kind
    `Permanent
    ~id:"internal.cannot_produce_proof"
    ~title:"Internal error: rollup node cannot produce refutation proof"
    ~description:
      "The rollup node is in a state that prevents it from producing \
       refutation proofs."
    ~pp:(fun ppf (inbox_level, start_tick) ->
      Format.fprintf
        ppf
        "cannot produce proof for inbox level %ld starting at tick %a"
        inbox_level
        Z.pp_print
        start_tick)
    Data_encoding.(obj2 (req "inbox_level" int32) (req "start_tick" z))
    (function
      | Cannot_produce_proof {inbox_level; start_tick} ->
          Some (inbox_level, start_tick)
      | _ -> None)
    (fun (inbox_level, start_tick) ->
      Cannot_produce_proof {inbox_level; start_tick}) ;

  register_error_kind
    ~id:"sc_rollup.node.Wrong_initial_pvm_state"
    ~title:"Initial state produced by PVM is incorrect"
    ~description:"Initial state produced by PVM is incorrect."
    ~pp:(fun ppf (actual, expected) ->
      Format.fprintf
        ppf
        "The initial state hash produced by the PVM %a is not consistent\n\
        \     with the one expected by the Layer 1 PVM implementation %a"
        State_hash.pp
        actual
        State_hash.pp
        expected)
    `Permanent
    Data_encoding.(
      obj2
        (req "initial_state_hash" State_hash.encoding)
        (req "expected_state_hash" State_hash.encoding))
    (function
      | Wrong_initial_pvm_state {initial_state_hash; expected_state_hash} ->
          Some (initial_state_hash, expected_state_hash)
      | _ -> None)
    (fun (initial_state_hash, expected_state_hash) ->
      Wrong_initial_pvm_state {initial_state_hash; expected_state_hash}) ;

  register_error_kind
    ~id:"internal.inconsistent_inbox"
    ~title:"Internal error: Rollup node has an inconsistent inbox"
    ~description:
      "The rollup node inbox should be the same as the layer 1 inbox."
    ~pp:(fun ppf (layer1_inbox, inbox) ->
      Format.fprintf
        ppf
        "@[Rollup inbox:@;%a@]@;should be equal to @[Layer1 inbox:@;%a@]"
        Octez_smart_rollup.Inbox.pp
        inbox
        Octez_smart_rollup.Inbox.pp
        layer1_inbox)
    `Permanent
    Data_encoding.(
      obj2
        (req "layer1_inbox" Octez_smart_rollup.Inbox.encoding)
        (req "inbox" Octez_smart_rollup.Inbox.encoding))
    (function
      | Inconsistent_inbox {layer1_inbox; inbox} -> Some (layer1_inbox, inbox)
      | _ -> None)
    (fun (layer1_inbox, inbox) -> Inconsistent_inbox {layer1_inbox; inbox}) ;

  register_error_kind
    `Permanent
    ~id:"internal.missing_pvm_state"
    ~title:"Internal error: Missing PVM state"
    ~description:"The rollup node cannot retrieve the state of the PVM."
    ~pp:(fun ppf (block, level) ->
      Format.fprintf
        ppf
        "Cannot retrieve PVM state for block %a at level %ld"
        Block_hash.pp
        block
        level)
    Data_encoding.(obj2 (req "block" Block_hash.encoding) (req "level" int32))
    (function
      | Missing_PVM_state (block, level) -> Some (block, level) | _ -> None)
    (fun (block, level) -> Missing_PVM_state (block, level)) ;

  register_error_kind
    `Permanent
    ~id:"internal.cannot_checkout_context"
    ~title:"Internal error: Cannot checkout context"
    ~description:
      "The rollup node cannot checkout the context registered for the block."
    ~pp:(fun ppf (block, context_hash) ->
      Format.fprintf
        ppf
        "The context %sfor block %a cannot be checkouted"
        (Option.fold
           ~none:""
           ~some:Smart_rollup_context_hash.to_b58check
           context_hash)
        Block_hash.pp
        block)
    Data_encoding.(
      obj2
        (req "block" Block_hash.encoding)
        (opt "context" Smart_rollup_context_hash.encoding))
    (function
      | Cannot_checkout_context (block, context) -> Some (block, context)
      | _ -> None)
    (fun (block, context) -> Cannot_checkout_context (block, context)) ;

  register_error_kind
    `Permanent
    ~id:"internal.cannot_checkout_l2_header"
    ~title:"Internal error: Cannot checkout L2 header"
    ~description:
      "The rollup node cannot checkout the l2 header registered for the block."
    ~pp:(fun ppf () -> Format.fprintf ppf "The l2 header cannot be checkouted")
    Data_encoding.unit
    (function Cannot_checkout_l2_header -> Some () | _ -> None)
    (fun () -> Cannot_checkout_l2_header) ;

  register_error_kind
    `Permanent
    ~id:"sc_rollup.node.lost_game"
    ~title:"Lost refutation game"
    ~description:"The rollup node lost a refutation game."
    ~pp:(fun ppf result ->
      Format.fprintf
        ppf
        "The rollup node lost the refutation game (%s)"
        (lost_result_to_string result))
    Data_encoding.(obj1 (req "result" lost_result_encoding))
    (function Lost_game result -> Some result | _ -> None)
    (fun result -> Lost_game result) ;

  register_error_kind
    `Permanent
    ~id:"sc_rollup.node.unparsable_boot_sector"
    ~title:"Unparsable boot sector"
    ~description:"The boot sector provided is not parsable by the PVM."
    ~pp:(fun ppf path ->
      Format.fprintf ppf "The boot sector at path %S is unparsable" path)
    Data_encoding.(obj1 (req "path" string))
    (function Unparsable_boot_sector {path} -> Some path | _ -> None)
    (fun path -> Unparsable_boot_sector {path}) ;

  register_error_kind
    `Permanent
    ~id:"sc_rollup.node.invalid_genesis_state"
    ~title:"Invalid genesis state"
    ~description:
      "The rollup node computed an invalid genesis state, it cannot continue."
    ~pp:(fun ppf (expected, actual, actual_state_hash) ->
      Format.fprintf
        ppf
        "Computed genesis commitment hash %a is not equal to the rollup \
         genesis commitment hash %a which commits state hash %a. The rollup \
         node cannot continue. If you used the argument `--boot-sector-file` \
         you probably provided the wrong boot sector. If not, please report \
         the bug."
        Commitment.Hash.pp
        expected
        Commitment.Hash.pp
        actual
        State_hash.pp
        actual_state_hash)
    Data_encoding.(
      obj3
        (req "expected" Commitment.Hash.encoding)
        (req "actual" Commitment.Hash.encoding)
        (req "actual_state_hash" State_hash.encoding))
    (function
      | Invalid_genesis_state {expected; actual; actual_state_hash} ->
          Some (expected, actual, actual_state_hash)
      | _ -> None)
    (fun (expected, actual, actual_state_hash) ->
      Invalid_genesis_state {expected; actual; actual_state_hash}) ;

  register_error_kind
    ~id:"sc_rollup.node.no_batcher"
    ~title:"No batcher for this node"
    ~description:"This node does not have a batcher"
    ~pp:(fun ppf () ->
      Format.fprintf ppf "This rollup node does not have batcher.")
    `Permanent
    Data_encoding.unit
    (function No_batcher -> Some () | _ -> None)
    (fun () -> No_batcher) ;

  register_error_kind
    ~id:"sc_rollup.node.no_dal_injector"
    ~title:"No dal injector for this node"
    ~description:"This node does not have a ADL injector"
    ~pp:(fun ppf () ->
      Format.fprintf ppf "This rollup node does not have DAL injector.")
    `Permanent
    Data_encoding.unit
    (function No_dal_injector -> Some () | _ -> None)
    (fun () -> No_dal_injector) ;

  register_error_kind
    ~id:"sc_rollup.node.no_publisher"
    ~title:"No publisher for this node"
    ~description:"This node does not have an operator to publish commitments"
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "This rollup node does not have an operator to publish commitments.")
    `Permanent
    Data_encoding.unit
    (function No_publisher -> Some () | _ -> None)
    (fun () -> No_publisher) ;

  register_error_kind
    ~id:"sc_rollup.node.no_refutation_coordinator"
    ~title:"No refutation coordinator for this node"
    ~description:"This node does not have a refutation game coordinator"
    ~pp:(fun ppf () ->
      Format.fprintf ppf "This node does not have a refutation game coordinator")
    `Permanent
    Data_encoding.unit
    (function No_refutation_coordinator -> Some () | _ -> None)
    (fun () -> No_refutation_coordinator) ;

  register_error_kind
    ~id:"sc_rollup.node.no_refutation_player"
    ~title:"A refutation player failed to start"
    ~description:"A refutation player failed to start"
    ~pp:(fun ppf () ->
      Format.fprintf ppf "A refutation player failed to start.")
    `Permanent
    Data_encoding.unit
    (function Refutation_player_failed_to_start -> Some () | _ -> None)
    (fun () -> Refutation_player_failed_to_start) ;

  register_error_kind
    `Permanent
    ~id:"could_not_acquire_lock"
    ~title:"Could not acquire lock on data dir"
    ~description:"Could not acquire lock on data dir."
    ~pp:(fun ppf f ->
      Format.fprintf
        ppf
        "Could not acquire lock on data directory, another rollup node may \
         already be running with this data. If this is not the case, consider \
         removing manually the file %S"
        f)
    Data_encoding.(obj1 (req "lock_file" string))
    (function Could_not_acquire_lock f -> Some f | _ -> None)
    (fun f -> Could_not_acquire_lock f) ;

  register_error_kind
    ~id:"sc_rollup.node.could_not_open_reveal_preimage_file"
    ~title:"Could not open reveal preimage file"
    ~description:"Could not open reveal preimage file."
    ~pp:(fun ppf hash ->
      Format.fprintf
        ppf
        "Could not open file containing preimage of reveal hash %s"
        hash)
    `Permanent
    Data_encoding.(obj1 (req "hash" string))
    (function
      | Could_not_open_preimage_file filename -> Some filename | _ -> None)
    (fun filename -> Could_not_open_preimage_file filename) ;

  register_error_kind
    ~id:"sc_rollup.node.could_not_encode_raw_data"
    ~title:"Could not encode raw data to reveal"
    ~description:"Could not encode raw data to reveal."
    ~pp:(fun ppf () ->
      Format.pp_print_string
        ppf
        "Could not encode raw data to reveal with the expected protocol \
         encoding")
    `Permanent
    Data_encoding.unit
    (function Could_not_encode_raw_data -> Some () | _ -> None)
    (fun () -> Could_not_encode_raw_data) ;

  register_error_kind
    ~id:"sc_rollup.node.operator_not_in_whitelist"
    ~title:"The operator is not in the whitelist"
    ~description:"The operator is not in the whitelist."
    ~pp:(fun ppf () ->
      Format.pp_print_string
        ppf
        "The operator is not in the whitelist. Please restart the rollup node \
         in bailout mode if you still have stakes.")
    `Permanent
    Data_encoding.unit
    (function Operator_not_in_whitelist -> Some () | _ -> None)
    (fun () -> Operator_not_in_whitelist) ;

  register_error_kind
    ~id:"sc_rollup.node.cannot_patch_pvm_of_public_rollup"
    ~title:"Cannot patch PVM of public rollup"
    ~description:"Unsafe PVM patches can only be applied in private rollups."
    ~pp:(fun ppf () ->
      Format.pp_print_string
        ppf
        "Unsafe PVM patches can only be applied in private rollups, i.e. in \
         non publicly refutable settings.")
    `Permanent
    Data_encoding.unit
    (function Cannot_patch_pvm_of_public_rollup -> Some () | _ -> None)
    (fun () -> Cannot_patch_pvm_of_public_rollup) ;

  register_error_kind
    ~id:"sc_rollup.node.needs_apply_unsafe_flag"
    ~title:"Needs --apply-unsafe-patches flag for this rollup"
    ~description:"Needs --apply-unsafe-patches flag for this rollup."
    ~pp:(fun ppf patches ->
      Format.fprintf
        ppf
        "This rollup requires the application of the following unsafe PVM \
         patches: @[<v 2>%a.@,\
         @]The rollup node must be started with option --apply-unsafe-patches \
         to allow the application of these patches."
        (Format.pp_print_list Pvm_patches.pp_unsafe_patch)
        patches)
    `Permanent
    Data_encoding.(
      obj1 (req "patches" (list Pvm_patches.unsafe_patch_encoding)))
    (function Needs_apply_unsafe_flag p -> Some p | _ -> None)
    (fun p -> Needs_apply_unsafe_flag p) ;

  register_error_kind
    ~id:"sc_rollup.node.operator_has_no_staked"
    ~title:"The operator does not has any stake"
    ~description:"The operator does not has any stake."
    ~pp:(fun ppf () ->
      Format.pp_print_string ppf "The operator does not has any stake.")
    `Permanent
    Data_encoding.unit
    (function Operator_has_no_staked -> Some () | _ -> None)
    (fun () -> Operator_has_no_staked) ;

  register_error_kind
    ~id:"sc_rollup.node.exiting_bailout_mode"
    ~title:"The rollup node is exiting."
    ~description:
      "The rollup node is exiting after recovering the bond of the operator."
    ~pp:(fun ppf () ->
      Format.pp_print_string
        ppf
        "The rollup node is exiting after bailout mode.")
    `Permanent
    Data_encoding.unit
    (function Exit_bond_recovered_bailout_mode -> Some () | _ -> None)
    (fun () -> Exit_bond_recovered_bailout_mode) ;

  register_error_kind
    `Permanent
    ~id:"sc_rollup.node.access_below_first_available_level"
    ~title:"Rollup node access data that is garbage collected"
    ~description:
      "The rollup node attempts to access data that is garbage collected."
    ~pp:(fun ppf (first, access) ->
      Format.fprintf
        ppf
        "Attempting to access data for level %ld, which is before the first \
         available level %ld"
        access
        first)
    Data_encoding.(
      obj2 (req "first_available_level" int32) (req "accessed_level" int32))
    (function
      | Access_below_first_available_level
          {first_available_level; accessed_level} ->
          Some (first_available_level, accessed_level)
      | _ -> None)
    (fun (first_available_level, accessed_level) ->
      Access_below_first_available_level {first_available_level; accessed_level}) ;

  register_error_kind
    ~id:"sc_rollup.node.unexpected_rollup"
    ~title:"Unexpected rollup for rollup node"
    ~description:"This rollup node is already set up for another rollup."
    ~pp:(fun ppf (rollup_address, saved_address) ->
      Format.fprintf
        ppf
        "This rollup node was already set up for rollup %a, it cannot be run \
         for a different rollup %a."
        Address.pp
        saved_address
        Address.pp
        rollup_address)
    `Permanent
    Data_encoding.(
      obj2
        (req "rollup_address" Address.encoding)
        (req "saved_address" Address.encoding))
    (function
      | Unexpected_rollup {rollup_address; saved_address} ->
          Some (rollup_address, saved_address)
      | _ -> None)
    (fun (rollup_address, saved_address) ->
      Unexpected_rollup {rollup_address; saved_address}) ;

  register_error_kind
    ~id:"sc_rollup.node.disagree_with_commitment"
    ~title:"Rollup node disagrees with commitment"
    ~description:
      "The rollup node disagrees with a commitment but cannot refute it."
    ~pp:(fun ppf (our_commitment, their_commitment) ->
      Format.fprintf
        ppf
        "The rollup node has computed commitment %a, but it disagrees with the \
         commitment %a which it cannot refute."
        Commitment.pp
        our_commitment
        Commitment.pp
        their_commitment)
    `Permanent
    Data_encoding.(
      obj2
        (req "our_commitment" Commitment.encoding)
        (req "their_commitment" Commitment.encoding))
    (function
      | Disagree_with_commitment {our_commitment; their_commitment} ->
          Some (our_commitment, their_commitment)
      | _ -> None)
    (fun (our_commitment, their_commitment) ->
      Disagree_with_commitment {our_commitment; their_commitment}) ;

  register_error_kind
    ~id:"sc_rollup.node.patch_durable_storage_on_commitment"
    ~title:"Patch durable storage on commitment"
    ~description:
      "The command patch durable storage was run on a level with a commitment."
    ~pp:(fun ppf level ->
      Format.fprintf
        ppf
        "The command patch durable storage cannot be run on a level with a \
         commitment, current level %ld has one. Please try in the next block."
        level)
    `Permanent
    Data_encoding.(obj1 (req "level" int32))
    (function
      | Patch_durable_storage_on_commitment level -> Some level | _ -> None)
    (fun level -> Patch_durable_storage_on_commitment level) ;

  register_error_kind
    ~id:"sc_rollup.node.dal_message_too_big"
    ~title:"Dal message too big"
    ~description:
      "The DAL injection worker received a message whose length exceeds slot \
       size."
    ~pp:(fun ppf (slot_size, message_size) ->
      Format.fprintf
        ppf
        "The DAL injection worker received a %d bytes message length, but a \
         DAL slot size is %d bytes."
        message_size
        slot_size)
    `Permanent
    Data_encoding.(obj2 (req "slot_size" int31) (req "message_size" int31))
    (function
      | Dal_message_too_big {slot_size; message_size} ->
          Some (slot_size, message_size)
      | _ -> None)
    (fun (slot_size, message_size) ->
      Dal_message_too_big {slot_size; message_size}) ;

  register_error_kind
    ~id:"sc_rollup.node.pvm_eval_too_many_ticks"
    ~title:"PVM advanced too many ticks"
    ~description:
      "The rollup node expects the PVM to advance at most a given amount of \
       ticks. \n\
       NOTE: This is a potential security issue. Please email \
       security@tezos.com to have the problem investigated."
    ~pp:(fun ppf (maximum, executed) ->
      Format.fprintf
        ppf
        "The rollup node expected the PVM to advance at most %Ld ticks, but \
         the PVM executed %Ld ticks \n\
         NOTE: This is a potential security issue. Please email \
         security@tezos.com to have the problem investigated."
        maximum
        executed)
    `Permanent
    Data_encoding.(
      obj2 (req "maximum ticks" int64) (req "executed ticks" int64))
    (function
      | PVM_eval_too_many_ticks {max_given; executed} ->
          Some (max_given, executed)
      | _ -> None)
    (fun (max_given, executed) -> PVM_eval_too_many_ticks {max_given; executed}) ;

  register_error_kind
    ~id:"sc_rollup.node.outbox_level_not_cemented"
    ~title:"Outbox level not cemented"
    ~description:"The outbox level is not yet cemented."
    ~pp:(fun ppf (outbox_level, lcc_level) ->
      Format.fprintf
        ppf
        "The outbox level %ld is not cemented yet (LCC is %ld)."
        outbox_level
        lcc_level)
    `Temporary
    Data_encoding.(obj2 (req "outbox_level" int32) (req "lcc_level" int32))
    (function
      | Outbox_level_not_cemented {outbox_level; lcc_level} ->
          Some (outbox_level, lcc_level)
      | _ -> None)
    (fun (outbox_level, lcc_level) ->
      Outbox_level_not_cemented {outbox_level; lcc_level})
