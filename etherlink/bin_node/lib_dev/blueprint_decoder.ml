(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let from_rlp rlp =
  let open Result_syntax in
  let open Rlp in
  let* delayed_transactions, transactions =
    match rlp with
    | List
        [
          Value _parent_hash;
          List delayed_transactions;
          List transactions;
          Value _timestamp;
        ] ->
        return (delayed_transactions, transactions)
    | _rlp -> tzfail Sequencer_blueprint.Not_a_blueprint
  in
  let* delayed_transaction_hashes =
    List.map_e
      (function
        | Value hash_tx ->
            let (`Hex hex) = Hex.of_bytes hash_tx in
            return (Ethereum_types.Hash (Hex hex), None)
        | _ -> tzfail Sequencer_blueprint.Not_a_blueprint)
      delayed_transactions
  in
  let* transaction_hashes =
    List.map_e
      (function
        | Value bytes ->
            let raw_tx = Bytes.to_string bytes in
            return (Ethereum_types.hash_raw_tx raw_tx, Some raw_tx)
        | _ -> tzfail Sequencer_blueprint.Not_a_blueprint)
      transactions
  in
  return (delayed_transaction_hashes @ transaction_hashes)

let transactions payload =
  let open Result_syntax in
  let* rlp_content = Sequencer_blueprint.to_rlp payload in
  from_rlp rlp_content
