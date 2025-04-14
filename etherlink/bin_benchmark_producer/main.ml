(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let () =
  Producer.register
    ~title:"general_contracts"
    ~registered:Primitives.registered_general_contracts
    ~tx_per_call:1
    ~max_gas_per_tx:5_000_000 ;
  Test.run ()
