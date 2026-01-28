(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Helpers for tx_kernel tests *)

(** [replace_variables string] sanitizes non-deterministic values in tx_kernel
    debug output (raw rollup address bytes, Ed25519 signatures, ticket IDs,
    operation byte arrays), then applies standard smart rollup sanitizations. *)
let replace_variables string =
  string
  |> replace_string
       ~all:true
       (rex "raw_rollup_address: \\[[0-9, ]+\\]")
       ~by:"raw_rollup_address: [SMART_ROLLUP_ADDRESS_BYTES]"
  |> replace_string
       ~all:true
       (rex "Ed25519Signature\\(\"edsig[a-zA-Z0-9]+\"\\)")
       ~by:"Ed25519Signature(\"[SIGNATURE]\")"
  |> replace_string
       ~all:true
       (rex "TicketId\\([0-9a-f]+\\)")
       ~by:"TicketId([TICKET_HASH])"
  |> replace_string
       ~all:true
       (rex "parsed: \\[[0-9, ]+\\]")
       ~by:"parsed: [OPERATION_BYTES]"
  |> Sc_rollup_helpers.replace_variables

let hooks = Tezos_regression.hooks_custom ~replace_variables ()
