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
end

let transaction_selected ~hash =
  Internal_event.Simple.emit Event.transaction_selected hash
