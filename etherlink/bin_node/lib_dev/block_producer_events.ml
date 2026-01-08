(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Event = struct
  open Internal_event.Simple

  let section = Events.section

  let transaction_selected =
    declare_1
      ~section
      ~name:"block_producer_transaction_injected"
      ~msg:"transaction {transaction} has been selected for a block"
      ~level:Info
      ("transaction", Ethereum_types.hash_encoding)
      ~pp1:Ethereum_types.pp_hash

  let started =
    declare_0
      ~section
      ~name:"block_producer_started"
      ~msg:"block producer has been started"
      ~level:Notice
      ()

  let shutdown =
    declare_0
      ~section
      ~name:"shutting_down_block_producer"
      ~msg:"stopping the block producer follower"
      ~level:Notice
      ()

  let production_locked =
    declare_0
      ~section
      ~name:"block_producer_locked"
      ~msg:"transaction pool and block production are locked"
      ~level:Error
      ()

  let transaction_rejected =
    declare_2
      ~section
      ~name:"block_producer_transaction_rejected"
      ~msg:"transaction {tx_hash} is not valid with current state: {error}"
      ~level:Debug
      ~pp1:(fun fmt Ethereum_types.(Hash (Hex h)) ->
        Format.fprintf fmt "%10s" h)
      ("tx_hash", Ethereum_types.hash_encoding)
      ("error", Data_encoding.string)

  let operation_rejected =
    declare_2
      ~section
      ~name:"block_producer_operation_rejected"
      ~msg:"operation {op_hash} is not valid with current state: {error}"
      ~level:Debug
      ~pp1:(fun fmt hash -> Format.fprintf fmt "%a" Operation_hash.pp hash)
      ("op_hash", Operation_hash.encoding)
      ("error", Data_encoding.string)

  let sunset =
    declare_0
      ~section
      ~name:"block_production_sunset"
      ~msg:"block production is sunset ahead of the change of sequencer"
      ~level:Notice
      ()
end

let transaction_selected ~hash =
  Internal_event.Simple.emit Event.transaction_selected hash

let started () = Internal_event.Simple.emit Event.started ()

let shutdown () = Internal_event.Simple.emit Event.shutdown ()

let production_locked () = Internal_event.Simple.emit Event.production_locked ()

let transaction_rejected tx_hash error =
  Internal_event.Simple.emit Event.transaction_rejected (tx_hash, error)

let operation_rejected op_hash error =
  Internal_event.Simple.emit Event.operation_rejected (op_hash, error)

let sunset () = Internal_event.Simple.emit Event.sunset ()
