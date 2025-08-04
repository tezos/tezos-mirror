(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs, <contact@nomadic-labs.com>     *)
(*                                                                           *)
(*****************************************************************************)

(** If the consensus is distributed randomly for 7000 slots,
    the (relative) power is the number of slots of the given delegate,
    noted in [slots]. Otherwise, if all bakers attest, then its power is its stake
    for the cycle in which the rights were distributed, noted in [stake]. *)
type t = {slots : int; stake : int64}

let encoding =
  let open Data_encoding in
  conv
    (fun {slots; stake} -> (slots, stake))
    (fun (slots, stake) -> {slots; stake})
    (obj2 (req "slots" int31) (req "stake" int64))

let pp ppf {slots; stake} =
  Format.fprintf ppf "(slots:%d, stake:%Ld)" slots stake

let zero = {slots = 0; stake = 0L}

let make ~slots ~stake = {slots; stake}

let add a b = {slots = a.slots + b.slots; stake = Int64.add a.stake b.stake}
