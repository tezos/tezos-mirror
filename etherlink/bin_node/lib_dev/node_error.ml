(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let exit_code_when_diverge = 100

let exit_code_when_out_of_sync = 101

type error +=
  | Diverged of
      (Z.t * Ethereum_types.block_hash * Ethereum_types.block_hash option)
  | Out_of_sync of {level_expected : int32; level_received : int32}

let () =
  register_error_kind
    `Permanent
    ~id:"evm_node.dev.evm_event_follower.rollup_diverged"
    ~title:"Sequencer diverged from rollup node."
    ~description:"Sequencer diverged from rollup node."
    ~pp:(fun ppf (level, expected_hash, found_hash) ->
      Format.fprintf
        ppf
        "Evm node sequencer diverged from rollup node at blueprint %a, \
         expected hash %a%a."
        Z.pp_print
        level
        Ethereum_types.pp_block_hash
        expected_hash
        Format.(
          pp_print_option (fun fmt hash ->
              fprintf fmt " (found hash: %a)" Ethereum_types.pp_block_hash hash))
        found_hash)
    Data_encoding.(
      obj3
        (req "blueprint_level" z)
        (req "expected_hash" Ethereum_types.block_hash_encoding)
        (opt "found_hash" Ethereum_types.block_hash_encoding))
    (function
      | Diverged (level, expected_hash, found_hash) ->
          Some (level, expected_hash, found_hash)
      | _ -> None)
    (fun (level, expected_hash, found_hash) ->
      Diverged (level, expected_hash, found_hash)) ;
  register_error_kind
    `Permanent
    ~id:"evm_node.dev.evm_event_follower.rollup_out_of_sync"
    ~title:"Sequencer out of sync with rollup node."
    ~description:"Sequencer and the rollup node are out of sync."
    ~pp:(fun ppf (expected, received) ->
      Format.fprintf
        ppf
        "Evm node sequencer received finalized level %ld but was expected %ld."
        expected
        received)
    Data_encoding.(obj2 (req "expected" int32) (req "received" int32))
    (function
      | Out_of_sync {level_expected; level_received} ->
          Some (level_expected, level_received)
      | _ -> None)
    (fun (level_expected, level_received) ->
      Out_of_sync {level_expected; level_received})
