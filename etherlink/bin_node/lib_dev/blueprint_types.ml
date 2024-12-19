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

let with_events_equal x y =
  let hash_bytes v =
    Data_encoding.Binary.to_bytes_exn with_events_encoding v
    |> Tezos_crypto.Hacl.Hash.Keccak_256.digest
  in
  hash_bytes x = hash_bytes y

let events_of_blueprint_with_events with_events =
  Evm_events.of_parts
    with_events.delayed_transactions
    with_events.kernel_upgrade
