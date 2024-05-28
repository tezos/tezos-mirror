(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let exit_code_when_diverge = 100

type error +=
  | Diverged of
      (Z.t * Ethereum_types.block_hash * Ethereum_types.block_hash option)

let () =
  register_error_kind
    `Permanent
    ~id:"evm_node.prod.evm_event_follower.rollup_diverged"
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
      Diverged (level, expected_hash, found_hash))
