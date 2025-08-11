(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs, <contact@nomadic-labs.com>     *)
(*                                                                           *)
(*****************************************************************************)

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

let add a b = {slots = a.slots + b.slots; stake = Int64.add a.stake b.stake}
