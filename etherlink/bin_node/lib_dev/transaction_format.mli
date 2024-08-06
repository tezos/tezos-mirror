(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [make_encoded_messages ~smart_rollup_address raw_tx] returns the
    hash of the transaction, and a list of transactions to include in
    the inbox.

    - [smart_rollup_address] is encoded on 20 bytes
    - [raw_tx] is an ethereum transaction in hex format (without the 0x prefix).

    All messages go through the same encoding, but will only be chunked if
    necessary. *)
val make_encoded_messages :
  smart_rollup_address:string ->
  string ->
  (Ethereum_types.hash * string list) tzresult
