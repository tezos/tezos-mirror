(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(*                                                                           *)
(*****************************************************************************)

(** [start config] starts the tx-pool. The [config] represents the 
    Rollup_node rpc module and the address of the smart rollup. *)
val start : (module Services_backend_sig.S) * string -> unit tzresult Lwt.t

(** [shutdown ()] stops the tx-pool, waiting for the ongoing request
    to be processed. *)
val shutdown : unit -> unit Lwt.t

(** [add raw_tx] adds a raw eth transaction to the tx-pool. *)
val add : string -> (Ethereum_types.hash, string) result tzresult Lwt.t

(** [nonce address] returns the nonce of the user
    Returns the first gap in the tx-pool, or the nonce stored on the rollup 
    if no transactions are in the pool. *)
val nonce : Ethereum_types.Address.t -> Ethereum_types.quantity tzresult Lwt.t
