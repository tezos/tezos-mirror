(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(*                                                                           *)
(*****************************************************************************)

(* TODO: https://gitlab.com/tezos/tezos/-/issues/6672
   It should be created by the configuration, or at least using values of
   the configuration. *)
type mode = Proxy of {rollup_node_endpoint : Uri.t} | Sequencer

type parameters = {
  rollup_node : (module Services_backend_sig.S);  (** The backend RPC module. *)
  smart_rollup_address : string;  (** The address of the smart rollup. *)
  mode : mode;
}

(** [start parameters] starts the tx-pool *)
val start : parameters -> unit tzresult Lwt.t

(** [shutdown ()] stops the tx-pool, waiting for the ongoing request
    to be processed. *)
val shutdown : unit -> unit Lwt.t

(** [add raw_tx] adds a raw eth transaction to the tx-pool. *)
val add : string -> (Ethereum_types.hash, string) result tzresult Lwt.t

(** [nonce address] returns the nonce of the user
    Returns the first gap in the tx-pool, or the nonce stored on the rollup 
    if no transactions are in the pool. *)
val nonce : Ethereum_types.Address.t -> Ethereum_types.quantity tzresult Lwt.t

(** [produce_block ~timestamp] takes the transactions in the tx pool
    and produces a block from it. *)
val produce_block : timestamp:Time.Protocol.t -> unit tzresult Lwt.t
