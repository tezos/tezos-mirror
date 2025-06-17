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

type with_events = {
  delayed_transactions : Evm_events.Delayed_transaction.t list;
  kernel_upgrade : Evm_events.Upgrade.t option;
  sequencer_upgrade : Evm_events.Sequencer_upgrade.t option;
  blueprint : t;
}

let with_events_encoding =
  let open Data_encoding in
  conv
    (fun {delayed_transactions; kernel_upgrade; sequencer_upgrade; blueprint} ->
      (delayed_transactions, kernel_upgrade, sequencer_upgrade, blueprint))
    (fun (delayed_transactions, kernel_upgrade, sequencer_upgrade, blueprint) ->
      {delayed_transactions; kernel_upgrade; sequencer_upgrade; blueprint})
    (obj4
       (req
          "delayed_transactions"
          (list Evm_events.Delayed_transaction.encoding))
       (opt "kernel_upgrade" Evm_events.Upgrade.encoding)
       (opt "sequencer_upgrade" Evm_events.Sequencer_upgrade.encoding)
       (req "blueprint" encoding))

let with_events_equal x y =
  let hash_bytes v =
    Data_encoding.Binary.to_bytes_exn with_events_encoding v
    |> Tezos_crypto.Hacl.Hash.Keccak_256.digest
  in
  hash_bytes x = hash_bytes y

let events_of_blueprint_with_events with_events =
  Evm_events.of_parts
    ~delayed_transactions:with_events.delayed_transactions
    ~kernel_upgrade:with_events.kernel_upgrade
    ~sequencer_upgrade:with_events.sequencer_upgrade

module Legacy = struct
  type with_events = {
    delayed_transactions : Evm_events.Delayed_transaction.t list;
    kernel_upgrade : Evm_events.Upgrade.t option;
    blueprint : t;
  }

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
end

let make_legacy
    ({delayed_transactions; kernel_upgrade; blueprint; sequencer_upgrade = _} :
      with_events) : Legacy.with_events =
  {delayed_transactions; kernel_upgrade; blueprint}

(* temporary *)
let of_legacy
    ({delayed_transactions; kernel_upgrade; blueprint} : Legacy.with_events) :
    with_events =
  {delayed_transactions; kernel_upgrade; blueprint; sequencer_upgrade = None}
