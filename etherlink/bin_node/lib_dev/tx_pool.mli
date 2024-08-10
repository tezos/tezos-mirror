(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

type mode = Proxy | Sequencer | Relay

type parameters = {
  rollup_node : (module Services_backend_sig.S);  (** The backend RPC module. *)
  smart_rollup_address : string;  (** The address of the smart rollup. *)
  mode : mode;
  tx_timeout_limit : int64;  (** TTL of a transaction inside the pool. *)
  tx_pool_addr_limit : int;  (** Maximum allowed addresses inside the pool. *)
  tx_pool_tx_per_addr_limit : int;
      (** Maximum allowed transactions per address inside the pool. *)
  max_number_of_chunks : int option;
      (** Maximum allowed number of chunks to be sent (relevant for the
          sequencer). *)
}

(** [start parameters] starts the tx-pool *)
val start : parameters -> unit tzresult Lwt.t

(** [shutdown ()] stops the tx-pool, waiting for the ongoing request
    to be processed. *)
val shutdown : unit -> unit tzresult Lwt.t

(** [add transaction_object raw_tx] adds a eth transaction and its raw contents
    to the tx-pool.

    The consistency between [transaction_object] and [raw_tx] is assumed by
    [add]. It is the responsibility of the caller to enforce it. *)
val add :
  Ethereum_types.transaction_object ->
  string ->
  (Ethereum_types.hash, string) result tzresult Lwt.t

(** [nonce address] returns the nonce of the user
    Returns the first gap in the tx-pool, or the nonce stored on the rollup
    if no transactions are in the pool. *)
val nonce : Ethereum_types.Address.t -> Ethereum_types.quantity tzresult Lwt.t

(** [pop_transactions maximum_cumulative_size] pops as much valid transactions
    as possible from the pool, until their cumulative size exceeds
    `maximum_cumulative_size`. Returns no transactions if the pool is locked. *)
val pop_transactions :
  maximum_cumulative_size:int ->
  (string * Ethereum_types.transaction_object) list tzresult Lwt.t

(** [pop_and_inject_transactions ()] pops the valid transactions from
    the pool using {!pop_transactions} and injects them using
    [inject_raw_transactions] provided by {!parameters.rollup_node}. *)
val pop_and_inject_transactions : unit -> unit tzresult Lwt.t

(** [pop_and_inject_transactions_lazy ()] same as
    [pop_and_inject_transactions] but don't wait for the request to
    complete *)
val pop_and_inject_transactions_lazy : unit -> unit tzresult Lwt.t

(** [lock_transactions] locks the transactions in the pool, new transactions
    can be added but nothing can be retrieved with {!pop_transactions}. *)
val lock_transactions : unit -> unit tzresult Lwt.t

(** [unlock_transactions] unlocks the transactions if it was locked by
    {!lock_transactions}. *)
val unlock_transactions : unit -> unit tzresult Lwt.t

(** [is_locked] checks if the pools is locked. *)
val is_locked : unit -> bool tzresult Lwt.t

type size_info = {number_of_addresses : int; number_of_transactions : int}

val size_info : unit -> size_info tzresult Lwt.t

val get_tx_pool_content : unit -> Ethereum_types.txpool tzresult Lwt.t

(** [find tx_hash] look into the tx pool if a transaction with hash
    [tx_hash] exists and returns it's corresponding
    {!Ethereum_types.transaction_object}. *)
val find :
  Ethereum_types.hash -> Ethereum_types.transaction_object option tzresult Lwt.t

val clear_popped_transactions : unit -> unit tzresult Lwt.t
