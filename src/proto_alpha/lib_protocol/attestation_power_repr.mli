(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs, <contact@nomadic-labs.com>     *)
(*                                                                           *)
(*****************************************************************************)

(** Internal representation of attestation power.

    If the consensus is distributed randomly for 7000 slots,
    the (relative) power is the number of slots of the given delegate,
    noted in [slots]. Otherwise, if all bakers attest, then its power is its stake
    for the cycle in which the rights were distributed, noted in [stake]. *)

type t = {slots : int; stake : int64}

val encoding : t Data_encoding.t

val pp : Format.formatter -> t -> unit

val zero : t

val add : t -> t -> t
