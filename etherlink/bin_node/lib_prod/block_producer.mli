(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type parameters = {
  ctxt : Evm_context.t;
  cctxt : Client_context.wallet;
  smart_rollup_address : string;
  sequencer_key : Client_keys.sk_uri;
}

(** [start parameters] starts the events follower. *)
val start : parameters -> unit tzresult Lwt.t

(** [shutdown ()] stops the events follower. *)
val shutdown : unit -> unit Lwt.t

(** [produce_block  ~force ~timestamp] takes the transactions
    in the tx pool and produces a block from it, returns the number of
    transaction in the block. The block is not produced if the list of
    transactions is empty and [force] is set to [false]. *)
val produce_block :
  force:bool -> timestamp:Time.Protocol.t -> int tzresult Lwt.t
