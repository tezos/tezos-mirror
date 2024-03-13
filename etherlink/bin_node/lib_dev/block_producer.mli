(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [produce_block backend ~force ~timestamp] takes the transactions
    in the tx pool and produces a block from it, returns the number of
    transaction in the block. The block is not produced if the list of
    transactions is empty and [force] is set to [false]. *)
val produce_block :
  (module Services_backend_sig.S) ->
  force:bool ->
  timestamp:Time.Protocol.t ->
  int tzresult Lwt.t
