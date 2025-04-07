(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type error += Not_a_blueprint

let chunk_content chunk =
  let open Result_syntax in
  let rlp_chunk = String.sub chunk 22 (String.length chunk - 22) in
  let* decoded = Rlp.decode (Bytes.unsafe_of_string rlp_chunk) in
  match decoded with
  | List [Value value; _; _; _; _] -> return value
  | _ -> fail [Not_a_blueprint]

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
            | _ -> Error [Not_a_blueprint])
          transactions
      in
      return transaction_hashes
  | rlp ->
      Format.printf "invalid rlp: %a\n%!" pp rlp ;
      fail [Not_a_blueprint]

let transaction_hashes Blueprint_types.{blueprint = {payload; _}; _} =
  let open Result_syntax in
  let* bytes =
    List.map_e (function `External chunk -> chunk_content chunk) payload
  in
  let* rlp_content = Rlp.decode Bytes.(concat Bytes.empty bytes) in
  from_rlp rlp_content
