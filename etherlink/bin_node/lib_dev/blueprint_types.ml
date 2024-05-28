(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type payload = [`External of string] list

type t = {
  number : Ethereum_types.quantity;
  timestamp : Time.Protocol.t;
  payload : payload;
}

type with_events = {
  delayed_transactions : Ethereum_types.Delayed_transaction.t list;
  blueprint : t;
}

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
    (fun {number = Qty n; timestamp; payload} -> (n, timestamp, payload))
    (fun (n, timestamp, payload) -> {number = Qty n; timestamp; payload})
    (obj3
       (req "number" n)
       (req "timestamp" Time.Protocol.encoding)
       (req "payload" payload_encoding))

let with_events_encoding =
  let open Data_encoding in
  conv
    (fun {delayed_transactions; blueprint} -> (delayed_transactions, blueprint))
    (fun (delayed_transactions, blueprint) -> {delayed_transactions; blueprint})
    (obj2
       (req
          "delayed_transactions"
          (list Ethereum_types.Delayed_transaction.encoding))
       (req "blueprint" encoding))
