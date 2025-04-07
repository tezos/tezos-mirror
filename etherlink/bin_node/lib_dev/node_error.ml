(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let exit_code_when_diverge = 100

let exit_code_when_out_of_sync = 101

let exit_code_when_flushed_blueprint = 102

type error +=
  | Diverged of {
      level : Z.t;
      expected_block_hash : Ethereum_types.block_hash;
      found_block_hash : Ethereum_types.block_hash option;
      must_exit : bool;
    }
  | Out_of_sync of {level_expected : int32; level_received : int32}
  | Cannot_handle_flushed_blueprint of Ethereum_types.quantity

let () =
  register_error_kind
    `Permanent
    ~id:"evm_node.dev.evm_event_follower.rollup_diverged"
    ~title:"Sequencer diverged from rollup node."
    ~description:"Sequencer diverged from rollup node."
    ~pp:(fun ppf (level, expected_hash, found_hash, must_exit) ->
      Format.fprintf
        ppf
        "Evm node sequencer diverged from rollup node at blueprint %a, \
         expected hash %a%a.%S"
        Z.pp_print
        level
        Ethereum_types.pp_block_hash
        expected_hash
        Format.(
          pp_print_option (fun fmt hash ->
              fprintf fmt " (found hash: %a)" Ethereum_types.pp_block_hash hash))
        found_hash
        (if must_exit then " The node must exit." else ""))
    Data_encoding.(
      obj4
        (req "blueprint_level" z)
        (req "expected_block_hash" Ethereum_types.block_hash_encoding)
        (opt "found_block_hash" Ethereum_types.block_hash_encoding)
        (req "must_exit" Data_encoding.bool))
    (function
      | Diverged {level; expected_block_hash; found_block_hash; must_exit} ->
          Some (level, expected_block_hash, found_block_hash, must_exit)
      | _ -> None)
    (fun (level, expected_block_hash, found_block_hash, must_exit) ->
      Diverged {level; expected_block_hash; found_block_hash; must_exit}) ;
  register_error_kind
    `Permanent
    ~id:"evm_node.dev.evm_event_follower.rollup_out_of_sync"
    ~title:"Evm node out of sync with rollup node."
    ~description:"Sequencer and the rollup node are out of sync."
    ~pp:(fun ppf (expected, received) ->
      Format.fprintf
        ppf
        "Evm node received finalized level %ld but was expected %ld."
        expected
        received)
    Data_encoding.(obj2 (req "expected" int32) (req "received" int32))
    (function
      | Out_of_sync {level_expected; level_received} ->
          Some (level_expected, level_received)
      | _ -> None)
    (fun (level_expected, level_received) ->
      Out_of_sync {level_expected; level_received}) ;
  register_error_kind
    `Permanent
    ~id:"evm_node.dev.evm_event_follower.flushed_blueprint"
    ~title:"Sequencer cannot handle flushed blueprint."
    ~description:"The sequencer cannot handle a flushed blueprint."
    ~pp:(fun ppf level ->
      Format.fprintf
        ppf
        "The sequencer cannot handle a flushed blueprint at level %a"
        Ethereum_types.pp_quantity
        level)
    Data_encoding.(obj1 (req "level" Ethereum_types.quantity_encoding))
    (function Cannot_handle_flushed_blueprint level -> Some level | _ -> None)
    (fun level -> Cannot_handle_flushed_blueprint level)
