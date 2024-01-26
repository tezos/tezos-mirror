(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Event = struct
  open Internal_event.Simple

  let section = ["evm_node"; "dev"; "delayed-inbox"]

  let started =
    declare_0
      ~section
      ~name:"evm_node_dev_delayed_inbox_started"
      ~msg:"Delayed inbox has been started"
      ~level:Notice
      ()

  let add_transaction =
    declare_1
      ~section
      ~name:"evm_node_dev_delayed_inbox_add_transaction"
      ~msg:"Add delayed transaction {transaction} to the tx-pool"
      ~level:Notice
      ("transaction", Ethereum_types.Delayed_transaction.encoding)
      ~pp1:Ethereum_types.Delayed_transaction.pp_short

  let pp_int32 fmt i = Format.fprintf fmt "%ld" i

  let fetch_succeeded =
    declare_2
      ~section
      ~name:"evm_node_dev_delayed_inbox_fetch_succeeded"
      ~msg:
        "Fetching delayed inbox for level {level} succeeded, {nb} new \
         transactions fetched"
      ~level:Notice
      ("level", Data_encoding.int32)
      ~pp1:pp_int32
      ("nb", Data_encoding.int31)
      ~pp2:Format.pp_print_int

  let fetch_failed =
    declare_1
      ~section
      ~name:"evm_node_dev_delayed_inbox_fetch_failed"
      ~msg:"Fetching delayed inbox for level {level} failed"
      ~level:Error
      ("level", Data_encoding.int32)
      ~pp1:pp_int32

  let transaction_fetch_failed =
    declare_2
      ~section
      ~name:"evm_node_dev_delayed_inbox_transaction_fetch_failed"
      ~msg:
        "Fetching transaction {tx_hash} from delayed inbox for level {level} \
         failed"
      ~level:Error
      ("tx_hash", Ethereum_types.hash_encoding)
      ~pp1:Ethereum_types.pp_hash
      ("level", Data_encoding.int32)
      ~pp2:pp_int32

  let connection_lost =
    declare_0
      ~section
      ~name:"evm_node_dev_delayed_inbox_connection_lost"
      ~msg:"Connection with the rollup node has been lost"
      ~level:Error
      ()

  let stopped =
    declare_0
      ~section
      ~name:"evm_node_dev_delayed_inbox_stopped"
      ~msg:"Delayed inbox has been stopped"
      ~level:Notice
      ()

  let shutdown =
    declare_0
      ~section
      ~name:"evm_node_dev_shutting_down_delayed_inbox"
      ~msg:"Stopping the delayed inbox"
      ~level:Notice
      ()
end

let started = Internal_event.Simple.emit Event.started

let add_transaction ~delayed_transaction =
  Internal_event.Simple.emit Event.add_transaction delayed_transaction

let fetch_succeeded ~level ~nb_txs =
  Internal_event.Simple.emit Event.fetch_succeeded (level, nb_txs)

let fetch_failed ~level = Internal_event.Simple.emit Event.fetch_failed level

let transaction_fetch_failed ~tx_hash ~level =
  Internal_event.Simple.emit Event.transaction_fetch_failed (tx_hash, level)

let connection_lost = Internal_event.Simple.emit Event.connection_lost

let stopped = Internal_event.Simple.emit Event.stopped

let shutdown = Internal_event.Simple.emit Event.shutdown
