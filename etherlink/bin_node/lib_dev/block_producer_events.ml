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
      ~msg:"Transaction {transaction} has been selected for a block"
      ~level:Info
      ("transaction", Ethereum_types.hash_encoding)
      ~pp1:Ethereum_types.pp_hash

  let started =
    declare_0
      ~section
      ~name:"block_producer_started"
      ~msg:"Block producer has been started"
      ~level:Notice
      ()

  let shutdown =
    declare_0
      ~section
      ~name:"shutting_down_block_producer"
      ~msg:"Stopping the block producer follower"
      ~level:Notice
      ()

  let production_locked =
    declare_0
      ~section
      ~name:"block_producer_locked"
      ~msg:
        "Transaction pool is locked, block production as well. The sequencer \
         is progressing too far in advance in comparison to the rollup node."
      ~level:Error
      ()
end

let transaction_selected ~hash =
  Internal_event.Simple.emit Event.transaction_selected hash

let started () = Internal_event.Simple.emit Event.started ()

let shutdown () = Internal_event.Simple.emit Event.shutdown ()

let production_locked () = Internal_event.Simple.emit Event.production_locked ()
