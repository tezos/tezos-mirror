(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Event = struct
  open Internal_event.Simple

  let section = ["evm_node"; "dev"; "tx_pool"]

  let started =
    declare_0
      ~section
      ~name:"evm_node_dev_tx_pool_started"
      ~msg:"Tx-pool has been started"
      ~level:Notice
      ()

  let add_transaction =
    declare_1
      ~section
      ~name:"evm_node_dev_tx_pool_add_transaction"
      ~msg:"Add transaction {transaction} to the tx-pool"
      ~level:Info
      ("transaction", Data_encoding.string)
      ~pp1:Format.pp_print_string

  let invalid_transaction =
    declare_1
      ~section
      ~name:"evm_node_dev_tx_pool_invalid_transaction"
      ~msg:"Transaction {transaction} is not valid"
      ~level:Info
      ("transaction", Data_encoding.string)
      ~pp1:Format.pp_print_string

  let transaction_injected =
    declare_1
      ~section
      ~name:"evm_node_dev_tx_pool_transaction_injected"
      ~msg:"Transaction {transaction} has been injected"
      ~level:Info
      ("transaction", Ethereum_types.hash_encoding)
      ~pp1:Ethereum_types.pp_hash

  let transaction_injection_failed =
    declare_0
      ~section
      ~name:"evm_node_dev_tx_pool_transaction_injection_failed"
      ~msg:"Injection of transactions failed"
      ~level:Error
      ()

  let connection_lost =
    declare_0
      ~section
      ~name:"evm_node_dev_tx_pool_connection_lost"
      ~msg:"Connection with the rollup node has been lost, retrying..."
      ~level:Error
      ()

  let stopped =
    declare_0
      ~section
      ~name:"evm_node_dev_tx_pool_stopped"
      ~msg:"Tx-pool has been stopped"
      ~level:Notice
      ()

  let shutdown =
    declare_0
      ~section
      ~name:"evm_node_dev_shutting_down_tx_pool"
      ~msg:"Stopping the tx-pool"
      ~level:Notice
      ()
end

let started = Internal_event.Simple.emit Event.started

let add_transaction ~transaction =
  Internal_event.Simple.emit Event.add_transaction transaction

let invalid_transaction ~transaction =
  Internal_event.Simple.emit Event.invalid_transaction transaction

let transaction_injection_failed =
  Internal_event.Simple.emit Event.transaction_injection_failed

let transaction_injected ~hash =
  Internal_event.Simple.emit Event.transaction_injected hash

let connection_lost = Internal_event.Simple.emit Event.connection_lost

let stopped = Internal_event.Simple.emit Event.stopped

let shutdown = Internal_event.Simple.emit Event.shutdown
