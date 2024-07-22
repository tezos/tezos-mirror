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

(** Module defining the transaction datatype and
    functions that operate on transactions. *)
module Transaction : sig
  (** The type of a transaction. *)
  type t

  (** [of_string s] constructs a transaction from [s].*)
  val of_string : string -> t

  (** [to_string t] converts [t] to a string. *)
  val to_string : t -> string

  (** Data encoding for transactions. *)
  val encoding : t Data_encoding.t
end

(** The type of a proposal. *)
type proposal = {
  transactions : Transaction.t list;
      (** The transactions included in the proposal. *)
  delayed_transaction_hashes : Ethereum_types.hash list;
      (** The delayed transaction hashes included in the proposal. *)
  previous_block_hash : Ethereum_types.block_hash;
      (** The block hash of the last block. *)
  timestamp : Time.Protocol.t;  (** The time when the proposal was crafted. *)
  current_blueprint_number : Ethereum_types.quantity;
      (** The latest  blueprint number known to the EVM node. *)
}

(** Encoding for proposals. *)
val proposal_encoding : proposal Data_encoding.t

(* TODO: https://gitlab.com/tezos/tezos/-/issues/7209
   Revisit the terminology for preblock. *)
(* TODO: https://gitlab.com/tezos/tezos/-/issues/7199
   Avoid exposing this type equality. *)

(** The type of a preblock. *)
type preblock = proposal

(** Encoding for preblocks. *)
val preblock_encoding : preblock Data_encoding.t

(** Possible outcomes when submitting a proposal to the dsn node. *)
type proposal_submission_outcome =
  | Tx_pool_is_locked
      (** The proposal was not submitted because the [Tx_pool] is locked. *)
  | Proposal_is_early
      (** The proposal was not submitted because it did not contain any
      transaction, and not enough time has passed since when a proposal was
      submitted last. *)
  | Proposal_submitted  (** The proposal was submitted to the DSN node. *)
