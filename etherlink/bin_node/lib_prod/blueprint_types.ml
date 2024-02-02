(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type payload = [`External of string] list

type t = {number : Ethereum_types.quantity; payload : payload}

let payload_encoding =
  let open Data_encoding in
  list
    (conv
       (function `External str -> str)
       (fun str -> `External str)
       (string' Hex))

let encoding =
  let open Data_encoding in
  conv
    (fun {number = Qty n; payload} -> (n, payload))
    (fun (n, payload) -> {number = Qty n; payload})
    (obj2 (req "number" n) (req "payload" payload_encoding))
