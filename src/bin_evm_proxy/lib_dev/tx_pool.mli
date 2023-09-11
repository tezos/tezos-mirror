(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(*                                                                           *)
(*****************************************************************************)

(** [start config] starts the tx-pool. The [config] represents the 
    Rollup_node rpc module and the address of the smart rollup. *)
val start : (module Rollup_node.S) * string -> unit tzresult Lwt.t

(** [shutdown ()] stops the tx-pool, waiting for the ongoing request
    to be processed. *)
val shutdown : unit -> unit Lwt.t

(** [add raw_tx] adds a raw eth transaction to the tx-pool. *)
val add :
  Ethereum_types.hex -> (Ethereum_types.hash, string) result tzresult Lwt.t
