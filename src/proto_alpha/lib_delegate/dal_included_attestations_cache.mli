(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

open Protocol.Alpha_context

type t

val create : attestation_lags:int list -> number_of_slots:int -> t

(** [set_committee t ~level lookup_fn] stores the committee lookup function
    for the given level. The lookup function maps attestation slots to delegate
    public key hashes and is used when extracting attestations from operations
    without receipts. *)
val set_committee :
  t -> level:int32 -> (Slot.t -> Signature.Public_key_hash.t option) -> unit
