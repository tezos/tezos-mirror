(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let from_rlp rlp =
  let open Result_syntax in
  let open Rlp in
  let* version, delayed_transactions, transactions =
    match rlp with
    | List
        [
          Value _parent_hash;
          List delayed_transactions;
          List transactions;
          Value _timestamp;
        ] ->
        return (Sequencer_blueprint.Legacy, delayed_transactions, transactions)
    | List
        [
          version;
          Value _parent_hash;
          List delayed_transactions;
          List transactions;
          Value _timestamp;
        ] ->
        let* version =
          match version with
          | Value version
            when Bytes.length version = 1 && Char.code (Bytes.get version 0) = 1
            ->
              return Sequencer_blueprint.V1
          | _ -> tzfail Sequencer_blueprint.Not_a_blueprint
        in
        return (version, delayed_transactions, transactions)
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
            let* raw_tx, common_tx =
              match version with
              | Legacy ->
                  let raw_tx = Bytes.to_string bytes in
                  return (raw_tx, Broadcast.Evm raw_tx)
              | V1 ->
                  let length = Bytes.length bytes in
                  if length = 0 then tzfail Sequencer_blueprint.Not_a_blueprint
                  else
                    let tag = Bytes.get bytes 0 in
                    let raw_tx =
                      String.sub (Bytes.to_string bytes) 1 (length - 1)
                    in
                    let tag_string = String.make 1 tag in
                    if tag_string = Sequencer_blueprint.evm_runtime_id then
                      return (raw_tx, Broadcast.Evm raw_tx)
                    else if
                      tag_string = Sequencer_blueprint.michelson_runtime_id
                    then return (raw_tx, Broadcast.Michelson raw_tx)
                    else tzfail Sequencer_blueprint.Not_a_blueprint
            in
            return (Ethereum_types.hash_raw_tx raw_tx, Some common_tx)
        | _ -> tzfail Sequencer_blueprint.Not_a_blueprint)
      transactions
  in
  return (delayed_transaction_hashes @ transaction_hashes)

let transactions payload =
  let open Result_syntax in
  let* rlp_content = Sequencer_blueprint.to_rlp payload in
  from_rlp rlp_content
