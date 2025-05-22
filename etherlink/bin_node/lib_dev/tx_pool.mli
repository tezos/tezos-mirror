(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

type mode =
  | Proxy
  | Sequencer
  | Relay
      (** Relays the transactions when they are valid w.r.t. the local state. *)

type parameters = {
  backend : (module Services_backend_sig.S);  (** The backend RPC module. *)
  smart_rollup_address : string;  (** The address of the smart rollup. *)
  mode : mode;
  tx_timeout_limit : int64;  (** TTL of a transaction inside the pool. *)
  tx_pool_addr_limit : int;  (** Maximum allowed addresses inside the pool. *)
  tx_pool_tx_per_addr_limit : int;
      (** Maximum allowed transactions per address inside the pool. *)
  chain_family : L2_types.chain_family;
}

(** [start parameters] starts the tx-pool *)
val start : parameters -> unit tzresult Lwt.t

(** [pop_transactions chain_family maximum_cumulative_size] pops as much
    valid transactions as possible from the pool, until their cumulative
    size exceeds `maximum_cumulative_size`. If the pool is locked or node
    in tezlink mode, returns no transactions. *)
val pop_transactions :
  maximum_cumulative_size:int ->
  (string * Ethereum_types.legacy_transaction_object) list tzresult Lwt.t

(** [pop_and_inject_transactions ()] pops the valid transactions from
    the pool using {!pop_transactions} and injects them using
    [inject_raw_transactions] provided by {!parameters.backend}. *)
val pop_and_inject_transactions : unit -> unit tzresult Lwt.t

(** [pop_and_inject_transactions_lazy ()] same as
    [pop_and_inject_transactions] but don't wait for the request to
    complete *)
val pop_and_inject_transactions_lazy : unit -> unit tzresult Lwt.t

val size_info : unit -> Metrics.Tx_pool.size_info tzresult Lwt.t

val clear_popped_transactions : unit -> unit tzresult Lwt.t

(** [mode] retrieves the current pool mode *)
val mode : unit -> mode tzresult Lwt.t

(** wrapper of the Tx_pool to be compatible with the Tx_container
    signature for the services. *)
module Tx_container : Services_backend_sig.Tx_container
