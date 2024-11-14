(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 TriliTech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(** Ocaml definitions and encoding of the datatypes used for threshold
   encryption. They mirror the Rust definition from
   `etherlink/bin_dsn_node/crates/core/src/types.rs`.
   Compatibility of the encodings between the Ocaml and Rust types
   is enforced only for (de)serialization in json format.
*)

module Transaction = struct
  type t = string

  let of_string tx = tx

  let to_string tx = tx

  let encoding = Data_encoding.(string' Hex)
end

type proposal = {
  transactions : Transaction.t list;
  delayed_transaction_hashes : Ethereum_types.hash list;
  previous_block_hash : Ethereum_types.block_hash;
  timestamp : Time.Protocol.t;
  (* TODO: Encoding is not consistent with the rust counterpart. *)
  current_blueprint_number : Ethereum_types.quantity;
}

let proposal_encoding =
  let open Data_encoding in
  conv
    (fun {
           transactions;
           delayed_transaction_hashes;
           previous_block_hash;
           timestamp;
           current_blueprint_number;
         } ->
      ( transactions,
        delayed_transaction_hashes,
        previous_block_hash,
        timestamp,
        current_blueprint_number ))
    (fun ( transactions,
           delayed_transaction_hashes,
           previous_block_hash,
           timestamp,
           current_blueprint_number ) ->
      {
        transactions;
        delayed_transaction_hashes;
        previous_block_hash;
        timestamp;
        current_blueprint_number;
      })
    (obj5
       (req "transactions" @@ list Transaction.encoding)
       (req "delayed_transaction_hashes" @@ list Ethereum_types.hash_encoding)
       (req "previous_block_hash" Ethereum_types.block_hash_encoding)
       (req "timestamp" Time.Protocol.rfc_encoding)
       (req "current_blueprint_number" Ethereum_types.quantity_encoding))

(* https://gitlab.com/tezos/tezos/-/issues/7246
   Distinguish between proposal request and preblock timestamps. *)
type preblock = proposal

let preblock_encoding = proposal_encoding

type proposal_submission_outcome =
  | Tx_pool_is_locked
  | Proposal_is_early
  | Proposal_submitted
