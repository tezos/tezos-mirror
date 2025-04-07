(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [is_tx_valid backend_rpc tx_raw] validates the transaction
    [tx_raw]. *)
val is_tx_valid :
  (module Services_backend_sig.S) ->
  string ->
  (Ethereum_types.legacy_transaction_object, string) result tzresult Lwt.t
