(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type error += Not_a_blueprint

let () =
  register_error_kind
    `Permanent
    ~id:"evm_node_not_a_blueprint"
    ~title:"Not a blueprint"
    ~description:"Tried to decode a payload that is not a valid blueprint"
    Data_encoding.empty
    (function Not_a_blueprint -> Some () | _ -> None)
    (fun () -> Not_a_blueprint)

let chunk_content chunk =
  let open Result_syntax in
  let rlp_chunk = String.sub chunk 22 (String.length chunk - 22) in
  let* decoded = Rlp.decode (Bytes.unsafe_of_string rlp_chunk) in
  match decoded with
  | List [Value value; _; _; _; _] -> return value
  | _ -> tzfail Not_a_blueprint

let from_rlp =
  let open Result_syntax in
  let open Rlp in
  function
  | List
      [
        Value _parent_hash;
        List delayed_transactions;
        List transactions;
        Value _timestamp;
      ] ->
      let* delayed_transaction_hashes =
        List.map_e
          (function
            | Value hash_tx ->
                let (`Hex hex) = Hex.of_bytes hash_tx in
                return (Ethereum_types.Hash (Hex hex), None)
            | _ -> tzfail Not_a_blueprint)
          delayed_transactions
      in

      let* transaction_hashes =
        List.map_e
          (function
            | Value bytes ->
                let raw_tx = Bytes.to_string bytes in
                return (Ethereum_types.hash_raw_tx raw_tx, Some raw_tx)
            | _ -> tzfail Not_a_blueprint)
          transactions
      in
      return (delayed_transaction_hashes @ transaction_hashes)
  | _rlp -> tzfail Not_a_blueprint

let transactions payload =
  let open Result_syntax in
  let* bytes =
    List.map_e (function `External chunk -> chunk_content chunk) payload
  in
  let* rlp_content = Rlp.decode Bytes.(concat Bytes.empty bytes) in
  from_rlp rlp_content
