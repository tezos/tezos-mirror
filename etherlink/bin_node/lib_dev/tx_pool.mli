(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(*                                                                           *)
(*****************************************************************************)

type mode = Proxy of {rollup_node_endpoint : Uri.t} | Sequencer | Observer

type parameters = {
  rollup_node : (module Services_backend_sig.S);  (** The backend RPC module. *)
  smart_rollup_address : string;  (** The address of the smart rollup. *)
  mode : mode;
}

type popped_transactions = Locked | Transactions of string list

(** [start parameters] starts the tx-pool *)
val start : parameters -> unit tzresult Lwt.t

(** [shutdown ()] stops the tx-pool, waiting for the ongoing request
    to be processed. *)
val shutdown : unit -> unit tzresult Lwt.t

(** [add raw_tx] adds a raw eth transaction to the tx-pool. *)
val add : string -> (Ethereum_types.hash, string) result tzresult Lwt.t

(** [nonce address] returns the nonce of the user
    Returns the first gap in the tx-pool, or the nonce stored on the rollup
    if no transactions are in the pool. *)
val nonce : Ethereum_types.Address.t -> Ethereum_types.quantity tzresult Lwt.t

(** [pop_transactions maximum_cumulative_size] pops as much valid transactions
    as possible from the pool, until their cumulative size exceeds
    `maximum_cumulative_size`. *)
val pop_transactions : maximum_cumulative_size:int -> string list tzresult Lwt.t

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
