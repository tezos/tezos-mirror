(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

type transaction_summary = {
  destination : string;
  entrypoint : string;
  parameters : string Tezos_micheline.Micheline.canonical;
  parameters_ty : string Tezos_micheline.Micheline.canonical option;
}

(** Simplified view of outbox messages for filtering and logging. This type must
    cover the values from all protocols. *)
type summary =
  | Whitelist_update of Signature.Public_key_hash.t list option
  | Transaction_batch of transaction_summary list

val summary_encoding : summary Data_encoding.t

val pp_summary : Format.formatter -> summary -> unit
