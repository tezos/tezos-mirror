(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(** [is_ready chain_id base_fee_per_gas] advertises that Floodgate has started
    and is ready to spam. *)
val is_ready : L2_types.chain_id -> Z.t -> unit Lwt.t

(** [tx_queue_is_ready ()] advertises that the [Tx_queue] is ready to receive
    transactions. *)
val tx_queue_is_ready : unit -> unit Lwt.t

(** [mainnet_experiment ()] advertises that an experiment is about to start on
    Etherlink Mainnet. That is, real funds will be spent. *)
val mainnet_experiment : unit -> unit Lwt.t

(** [received_blueprint number] advertises that Floodgate has received
    the blueprint of height [number] from its relay endpoint. *)
val received_blueprint : Ethereum_types.quantity -> unit Lwt.t

(** [spam_started address] advertises that Floodgate is now using [address] to
    spam the Etherlink network. *)
val spam_started : Ethereum_types.address -> unit Lwt.t

(** [transaction_confirmed account duration] advertises that a transaction from
    [account] has been seen in a blueprint, [duration] seconds after it has
    been accepted by the relay endpoint. *)
val transaction_confirmed : Account.t -> Ptime.Span.t -> unit Lwt.t

(** [transaction_refused account] advertises that a transaction from [account]
    has been refused by the relay endpoint. *)
val transaction_refused : Account.t -> unit Lwt.t

(** [transaction_dropped account] advertises that it is likely that a
    transaction from [account] has been dropped by the relay endpoint. *)
val transaction_dropped : Account.t -> unit Lwt.t

(** [transaction_retried_confirmed account attempt time] advertises
    that a transaction from [account] that was previously dropped have
    been confirmed after [attempt] on [time] sec. *)
val transaction_retried_confirmed : Account.t -> int -> Ptime.span -> unit Lwt.t

(** [transaction_retried_failed account attempt] advertises that a
    transaction from [account] that was retried [attempt] failed to be
    confirmed.. *)
val transaction_retried_failed : Account.t -> int -> unit Lwt.t

(** [reimbursed_controller account] advertises that the transaction reimbursing
    the controller used to fund [account] has been accepted by the relay
    endpoint. *)
val reimbursed_controller : Account.t -> unit Lwt.t

(** [setup_completed ()] advertises that Floodgate ramp-up is done. Every EOA
    has been funded and is now used to spam the Etherlink network. *)
val setup_completed : unit -> unit Lwt.t

(** [injecting_transactions nb] advertises [nb] transactions are about to be
    injected to the relay endpoint with a batch of [eth_sendRawTransaction]. *)
val injecting_transactions : int -> unit Lwt.t

(** [deploy_erc20 address] advertises an ERC20 contract as been deployed at
    address [address]. *)
val deploy_erc20 : string -> unit Lwt.t

(** [rpc_error error] advertises an RPC produced the error [error]. *)
val rpc_error : Rpc_encodings.JSONRPC.error -> unit Lwt.t

(** [measured_tps transactions_count duration] advertises that Floodgate has
    been able to inject [transaction_counts] transactions over the given
    [duration]. *)
val measured_tps : int -> Ptime.span -> unit Lwt.t

(** [measured_dps dummy_data_size duration] advertises that Floodgate
    has been able to send [data_size]KB of dummy data over the given
    [duration]. *)
val measured_dps : int -> Ptime.span -> unit Lwt.t
