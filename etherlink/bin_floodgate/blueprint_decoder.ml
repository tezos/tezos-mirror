(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let from_rlp =
  let open Result_syntax in
  let open Rlp in
  function
  | List
      [
        Value _parent_hash;
        List _delayed_transactions;
        List transactions;
        Value _timestamp;
      ] ->
      let* transaction_hashes =
        List.map_e
          (function
            | Value bytes ->
                Ok (Ethereum_types.hash_raw_tx (Bytes.unsafe_to_string bytes))
            | _ -> Error [Sequencer_blueprint.Not_a_blueprint])
          transactions
      in
      return transaction_hashes
  | rlp ->
      Format.printf "invalid rlp: %a\n%!" pp rlp ;
      fail [Sequencer_blueprint.Not_a_blueprint]

let transaction_hashes Blueprint_types.{blueprint = {payload; _}; _} =
  let open Result_syntax in
  let* bytes =
    List.map_e
      (fun chunk ->
        let+ chunk = Sequencer_blueprint.chunk_of_external_message chunk in
        (Sequencer_blueprint.unsafe_drop_signature chunk).value)
      payload
  in
  let* rlp_content = Rlp.decode Bytes.(concat Bytes.empty bytes) in
  from_rlp rlp_content
