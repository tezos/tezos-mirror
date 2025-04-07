(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Event = struct
  open Internal_event.Simple

  let section = Events.section

  let started =
    declare_0
      ~section
      ~name:"tx_pool_started"
      ~msg:"tx-pool has been started"
      ~level:Notice
      ()

  let add_transaction =
    declare_1
      ~section
      ~name:"tx_pool_add_transaction"
      ~msg:"add transaction {transaction} to the tx-pool"
      ~level:Info
      ("transaction", Data_encoding.string)
      ~pp1:Format.pp_print_string

  let invalid_transaction =
    declare_1
      ~section
      ~name:"tx_pool_invalid_transaction"
      ~msg:"transaction {transaction} is not valid"
      ~level:Info
      ("transaction", Data_encoding.string)
      ~pp1:Format.pp_print_string

  let users_threshold_reached =
    declare_0
      ~section
      ~name:"tx_pool_users_threshold_reached"
      ~msg:
        "the transaction pool has reached its maximum threshold for user \
         transactions"
      ~level:Info
      ()

  let txs_per_user_threshold_reached =
    declare_1
      ~section
      ~name:"txs_per_user_threshold_reached"
      ~msg:"user {address} has reached the maximum threshold for transactions"
      ~level:Info
      ("address", Data_encoding.string)
      ~pp1:Format.pp_print_string

  let tx_data_size_limit_reached =
    declare_0
      ~section
      ~name:"tx_data_size_limit_reached"
      ~msg:"the transaction data size is beyond the allowed threshold"
      ~level:Info
      ()

  let transaction_injected =
    declare_1
      ~section
      ~name:"tx_pool_transaction_injected"
      ~msg:"transaction {transaction} has been injected"
      ~level:Info
      ("transaction", Ethereum_types.hash_encoding)
      ~pp1:Ethereum_types.pp_hash

  let transaction_injection_failed =
    declare_1
      ~section
      ~name:"tx_pool_transaction_injection_failed"
      ~msg:"injection of transactions failed"
      ~level:Error
      ("trace", Events.trace_encoding)

  let connection_lost =
    declare_0
      ~section
      ~name:"tx_pool_connection_lost"
      ~msg:"connection with the rollup node has been lost, retrying..."
      ~level:Error
      ()

  let stopped =
    declare_0
      ~section
      ~name:"tx_pool_stopped"
      ~msg:"tx-pool has been stopped"
      ~level:Notice
      ()

  let shutdown =
    declare_0
      ~section
      ~name:"shutting_down_tx_pool"
      ~msg:"stopping the tx-pool"
      ~level:Notice
      ()
end

let started = Internal_event.Simple.emit Event.started

let add_transaction ~transaction =
  Internal_event.Simple.emit Event.add_transaction transaction

let invalid_transaction ~transaction =
  let Ethereum_types.(Hex tx_str) = transaction in
  Internal_event.Simple.emit Event.invalid_transaction tx_str

let users_threshold_reached =
  Internal_event.Simple.emit Event.users_threshold_reached

let txs_per_user_threshold_reached ~address =
  Internal_event.Simple.emit Event.txs_per_user_threshold_reached address

let tx_data_size_limit_reached =
  Internal_event.Simple.emit Event.tx_data_size_limit_reached

let transaction_injection_failed trace =
  Internal_event.Simple.emit Event.transaction_injection_failed trace

let transaction_injected ~hash =
  Internal_event.Simple.emit Event.transaction_injected hash

let connection_lost = Internal_event.Simple.emit Event.connection_lost

let stopped = Internal_event.Simple.emit Event.stopped

let shutdown = Internal_event.Simple.emit Event.shutdown
