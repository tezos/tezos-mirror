(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type chunk = [`External of string]

type payload = chunk list

type t = {
  number : Ethereum_types.quantity;
  timestamp : Time.Protocol.t;
  payload : payload;
}

type with_events = {
  delayed_transactions : Evm_events.Delayed_transaction.t list;
  kernel_upgrade : Evm_events.Upgrade.t option;
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
    (fun {delayed_transactions; kernel_upgrade; blueprint} ->
      (delayed_transactions, kernel_upgrade, blueprint))
    (fun (delayed_transactions, kernel_upgrade, blueprint) ->
      {delayed_transactions; kernel_upgrade; blueprint})
    (obj3
       (req
          "delayed_transactions"
          (list Evm_events.Delayed_transaction.encoding))
       (opt "kernel_upgrade" Evm_events.Upgrade.encoding)
       (req "blueprint" encoding))
